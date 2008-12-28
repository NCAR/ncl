/*
 *	$Id: talkto.c,v 1.31 2008-12-28 13:30:29 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	talkto.c
 *
 *	Author		John Clyne
 *
 *	Date		Thu Sep 13 12:48:22 MDT 1990
 *
 *	This file handles all communication between idt and ictrans
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/time.h>
#include "text.h"
#include <sys/param.h>

#ifdef	BSD4_4
#include <sys/select.h>
#endif

#ifdef	Solaris
#include <sys/select.h>
#endif

#ifdef RS6000
#define NBBY    8       /* xlc compiler isn't config'ed properly        */
#include <sys/select.h>
#endif


#ifndef	CRAY
#include <sys/wait.h>
#include <sys/resource.h>
#endif

#include <ncarg/c.h>

#include "display.h"
#include "talkto.h"

/*
 * a structure containing information about running translators
 */
static	struct	{
	int	pid;		/* the pid of the translator process	*/
	int	wfd;		/* a fd for writing to the process	*/
	int	rfd;		/* fd for reading responses from trans	*/
	int	r_err_fd;	/* standard error from the process	*/
	FILE	*rfp;		/* file pointer for rfd			*/
	FILE	*r_err_fp;	/* file pointer for r_err_fd		*/
	boolean	pending;	/* outstanding commands with no acks	*/
	} Translators[MAX_DISPLAYS];	

static	int	hFD = -1;	/* history file file descriptor	*/

#ifdef	CRAY
static	void	reaper(sig)
	int	sig;
{
	int	pid, status;

	sighold(SIGCLD);	/* hold signals	*/

	if ((pid = wait(&status)) == -1) {
		perror("wait");
		exit(1);
	}

	close_trans_pid(pid);	/* close connection to dead kid	*/

	sigrelse(SIGCLD);
}
#endif

/*
 *	close_trans_pid
 *	[internal]
 *
 *	given the pid of a translator close communications with that translator
 *	This is simple an interface to CloseTranslator that excepts a pid 
 *	rather then a connection id.
 */
static	close_trans_pid(pid)
	int	pid;
{
	int	i;

	for (i = 0; i < MAX_DISPLAYS; i++) {
		if (Translators[i].pid == pid) {
			CloseTranslator(i);
		}
	}
}
/*
 *	Get a message from the translator. All translator messages must
 *	be terminated with an ack defined by the manifest constant PROMPT.
 *	The message returned is stripped of this prompt and any trailing
 *	newline. 
 *
 * on entry
 *	*fp		: file pointer to read message from
 *	size		: max number of characters that may be written to
 *			  'buf' including the null terminator, '\0'.
 * on exit
 *	buf		: contains a NULL terminated string.
 *	return		: strlen(buf) if ok, else -1.
 */
static	int	get_trans_msg(fp, buf, size)
	FILE	*fp;
	char	*buf;
	int	size;
{
	int	c;
	char	*prompt = PROMPT;
	char	*s = prompt;
	int	i;
	boolean	done;
	int	match;

	for (i=0, s=prompt, done=FALSE, match=0;! done; i++) {
		c = getc(fp);

		if (c == EOF) return (-1);

		if (*s == c) {
			match++;
			s++;
		}
		else {
			s = prompt;
			match = 0;
		}

		if (i < size) {
			buf[i] = (char) c;
		}

		if (match == strlen(PROMPT)) {
			done = TRUE;
		}
	}

	i -= strlen(PROMPT);	/* back over prompt	*/

	/*
	 * null-terminate the string sans the prompt
	 */
	if (i >= size) i = size - 1;
	buf[i] = '\0';

	/*SUPPRESS 624*/
	if (s = strrchr(buf, '\n')) *s = '\0';

	return(strlen(buf));
}

/*
 *	Get an error message from the translator. Unlike get_trans_msg()
 *	this function does no massaging to the string returned on the
 *	stream 'fp'. Any and all bytes available on fp at the time
 *	get_trans_err() is called are returned as a null terminated string.
 *	if no bytes are available get_trans_err() returns immediately.
 *
 * on entry
 *	*fp		: file pointer to read message from
 *	size		: max number of characters that may be written to
 *			  'buf' including the null terminator, '\0'.
 * on exit
 *	buf		: contains a NULL terminated string.
 *	return		: strlen(buf) if ok, else -1.
 */
static	int	get_trans_err(fp, buf, size)
	FILE	*fp;
	char	*buf;
	int	size;
{
	int		fd = fileno(fp);
	int		width = fd + 1;
	fd_set		readfs;
	int		n, rc;
	struct timeval	tv;


	FD_ZERO(&readfs);
	FD_SET(fd, &readfs);
	tv.tv_sec = tv.tv_usec = 0;	/* poll descriptor	*/

	rc = select( width, &readfs, (fd_set *) NULL, (fd_set *) NULL, &tv);
	if (rc < 0) {
		return(-1);
	}
	else if (rc == 0) {
		buf[0] = '\0';
		return(0);
	}

	if ((n = read(fd, buf, size)) < 0) {
		return(-1);
	}

	buf[n-1] = '\0';
	return(strlen(buf));
}
	

/*
 *	OpenTranslator
 *
 *	Fork a translator to process a metafile. Set up communications between
 *	idt and the translator process. It is assumed the translator will
 *	write all error messages. Status messages will ge terminated with
 *	a prompt defined by PROMPT. There will be a single PROMPT terminated
 *	status message returned by the translator for each command sent to
 *	the translator.
 * on entry
 *	channel		: Communication channel. All future communication with
 *			  this process will use this unique channel id. channel
 *			  is an int in the range [0,MAX_DISPLAYS). 
 *	argv		: process to exec with all its args
 *	hfd		: history file fd, if -1 not open
 * on exit
 *	return		: < 0 => failure, else ok
 */
int	OpenTranslator(channel, argv, hfd)
	int	channel;
	char	**argv;
	int	hfd;
{

	int	i;
	int	pid;
	int	to_child[2],		/* pipe descriptors to/from process */
		to_parent[2],
		stderr_to_parent[2];

	char	fd_ascii[10];
	char	**argptr;

	static	short first	= 1;

	if (first) {	/* one time initialization		*/
		for (i = 0; i < MAX_DISPLAYS; i++)
			Translators[i].pid = -1;

#ifdef	CRAY
		/*
		 * no wait3() in unicos, use signals
		 */
		if (sigctl(SCTL_REG, SIGCLD, reaper) < 0) {
			perror("sigctl");
			exit(1);
		}
#endif
		first = 0;
	}

	hFD = hfd;			/* the history file fd		*/

	/*
	 * set up pipes to communicate with child process. 
	 */
	if (pipe(to_child) < 0) {
		perror("pipe");
		return(-1);
	}

	if (pipe(to_parent) < 0) {
		(void) close(to_child[0]); (void) close(to_child[1]);
		perror("pipe");
		return(-1);
	}

	if (pipe(stderr_to_parent) < 0) {
		(void) close(to_child[0]); (void) close(to_child[1]);
		(void) close(to_parent[0]); (void) close(to_parent[1]);
		perror("pipe");
		return(-1);
	}
		

	pid = fork();
	switch	(pid)	{
	case	-1:
		perror("fork");
		return(-1);

	case	0:	/* the child	*/
		(void) close(0);	/* redirect stdin	*/
		(void) dup(to_child[0]);
		(void) close(2);	/* redirect stderr	*/
		(void) dup(stderr_to_parent[1]);

		setbuf(stderr, (char *) NULL);	/* no buffered output	*/

		(void) close(to_child[0]); 
		(void) close(to_child[1]);
		(void) close(stderr_to_parent[0]);
		(void) close(stderr_to_parent[1]);
		(void) close(to_parent[0]);

		(void) sprintf(fd_ascii, "%d", to_parent[1]);
		/*SUPPRESS 570*/
		for (argptr = argv; *argptr; argptr++);

		*argptr++ = "-fdn";
		*argptr++ = fd_ascii;
		*argptr = NULL;
		
		execvp(argv[0], argv);
		perror("execvp");	/* should never get here	*/
		_exit(127);

		break;
	default:	/* the parent		*/
		Translators[channel].pid = pid;
		Translators[channel].wfd = to_child[1];
		Translators[channel].rfd = to_parent[0];
		Translators[channel].r_err_fd = stderr_to_parent[0];
		Translators[channel].pending = TRUE;

		(void) close(to_child[0]); 
		(void) close(to_parent[1]);
		(void) close(stderr_to_parent[1]);

		Translators[channel].rfp = fdopen(
			Translators[channel].rfd,"r"
		);
		Translators[channel].r_err_fp = fdopen(
			Translators[channel].r_err_fd,"r"
		);

	}

	return(1);
}

/*
 *	CloseTranslator
 *	[exported]
 *
 *	Terminate communications with translator process
 * on entry
 *	channel		: ids process we are talking to
 */
void	CloseTranslator(channel)
	int	channel;
{
		/*
		 * close pipes
		 */
		(void) close(Translators[channel].wfd); 
		(void) fclose(Translators[channel].rfp);
		(void) fclose(Translators[channel].r_err_fp);
		Translators[channel].pid = -1;

}
/*
 *	TalkTo
 *	[exported]
 *	
 *	Send a *single* syntactically correct ictrans command string to 
 *	the translator
 *	associated with the specified connection channel id.
 *
 * on entry
 *	id		: a connection id created by OpenDisplay
 *	*command_string	: A *single* ictrans command
 *	mode		: one of (ASYNC, SYNC). If mode is ASYNC issue command
 *			  and return. If mode is SYNC issue command and wait
 *			  for a responce from the translator. Return 
 *			  translators responce in a string.
 *			  
 * on exit
 *	return		: if mode == ASYNC => empty string. if mode == SYNC => 
 *			  translator responce string. Return NULL if 
 *			  communication with translator is no longer possible.
 */
char	*TalkTo(id, command_string, mode)
	int	id;
	char	*command_string;
	int	mode;
{
	static	char	buf[8192];
	int	pid;

	/*
	 * see if anybody died
	 */
#ifndef	CRAY
	if ((pid = waitpid(0, (int *) NULL, WNOHANG)) > 0) {
		close_trans_pid(pid);
	}
#endif

	if (Translators[id].pid == -1) return (NULL);

	/*
	 * See if we're expecting any response from the translator. We
	 * won't have any messages pending if in the last call to 
	 * TalkTo() mode was set to 'SYNC'.
	 */
	if (Translators[id].pending) {

		if (get_trans_msg(Translators[id].rfp, buf, sizeof(buf)) < 0) {
			perror("reading stdout of translator");
			return(NULL);
		}

		Translators[id].pending = FALSE;

		/*
		 * Post any old messages
		 */
		if (strlen(buf)) Message(id, buf);
	}

	/*
	 * see if there were any error messages
	 */
	if (get_trans_err(Translators[id].r_err_fp, buf, sizeof(buf)) < 0) {
		perror("reading stderr of translator");
		return(NULL);
	}
	/*
	 * Post any old messages
	 */
	if (strlen(buf)) ErrorMessage(id, buf);

	/*
	 * Send the command to the desired translator
	 */
	if (write(Translators[id].wfd,command_string,strlen(command_string))<0){
		perror("writing to translator");
		return(NULL);
	}

	if (hFD != -1) {
		(void) write(hFD, command_string, strlen(command_string));
	}
	Translators[id].pending = TRUE;	/* we have an outstanding cmd	*/

	/*
 	 * if mode is 'ASYNC' we don't wait for the translator to respond
	 */
	if (mode == ASYNC) {
		return("");	/* no msg from translator, yet	*/
	}

	/*
	 * read from translator until we see a translator prompt
	 * signifying the end of the message or the translator
	 * dies.
	 */
	if (get_trans_msg(Translators[id].rfp, buf, sizeof(buf)) < 0) {
		perror("reading from translator");
		return(NULL);
	}

	Translators[id].pending = FALSE;

	/*
	 * Post any old messages
	 */
	if (strlen(buf)) Message(id, buf);


	/*
	 * return the message sans the prompt
	 */
	return (buf);
}


/*
 *	SignalTo
 *	[exported]
 *
 *	Send a UNIX signal to a translator
 *
 * on entry
 *	id		: id of the recipient translator
 *	signal		: signal to send
 */
void	SignalTo(id, signal)
	int	id;
	int	signal;
{
	(void) kill (Translators[id].pid, signal);
}


/*
 *	Message
 *	[exported]
 *
 *	Send a message to the idt message handler. Prepend the id of the 
 *	translator generating the message
 * on entry
 *	id		: id of the translator
 *	*s		: message to send
 */
void	Message(id, s)
	int	id;
	char	*s;
{
	char	buf[132];
	char	msg[132];
	char	*format = "Display(%1d): %s\n";
	int	max_len = sizeof(buf) - strlen(format) - 1;
	char	*t;


	s = strncpy(msg, s, sizeof(buf)-1);
	/*
	 * remove any newlines.
	 */
	/*SUPPRESS 570*/
	for(t=s; *t && *t != '\n' && (t-s <= max_len); t++);
	*t = '\0';

	(void) sprintf(buf, format, id, s);

	AppendText(buf);
}

/*
 *	ErrorMessage
 *	[exported]
 *
 *	Send a *short* message to the idt message handler. Prepend the 
 *	id of the translator generating the message. 
 * on entry
 *	id		: id of the translator
 *	*s		: message to send
 */
void	ErrorMessage(id, s)
	int	id;
	char	*s;
{
	char	buf[132];
	char	msg[132];
	char	*format = "Display(%1d): Error - %s\n";
	int	max_len = sizeof(buf) - strlen(format) - 1;
	char	*t;

	s = strncpy(msg, s, sizeof(buf)-1);
	/*
	 * remove any newlines.
	 */
	/*SUPPRESS 570*/
	for(t=s; *t && *t != '\n' && (t-s <= max_len); t++);
	*t = '\0';

	(void) sprintf(buf, format, id, s);

	AppendText(buf);

}
