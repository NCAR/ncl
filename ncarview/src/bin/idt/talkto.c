/*
 *	$Id: talkto.c,v 1.10 1992-04-03 23:21:09 clyne Exp $
 */
/*
 *	talkto.c
 *
 *	Author		John Clyne
 *
 *	Date		Thu Sep 13 12:48:22 MDT 1990
 *
 *	This file handles all communication between idt and ictrans
 */

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>

#ifndef	CRAY
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef	SYSV
#include <string.h>
#else
#include <strings.h>
#endif

#ifndef	sun
#define	FORK	fork
#else
#include <vfork.h>
#define	FORK	vfork
#endif

#include <ncarv.h>

#include "display.h"
#include "talkto.h"

/*
 *	brain damaged ultrix sets wrong error code when read from non-blocking
 *	pipe should block
 */
#ifdef	ultrix
#define	EWOULDBLOCK_	EAGAIN
#else
#define	EWOULDBLOCK_	EWOULDBLOCK
#endif

/*
 * a structure containing information about running translators
 */
static	struct	{
	int	pid;		/* the pid of the translator process	*/
	int	wfd;		/* a fd for writing to the process	*/
	int	rfd;		/* fd for reading responses from trans	*/
	int	r_err_fd;	/* standard error from the process	*/
	int	pending;	/* outstanding commands with no acks	*/
	} Translators[MAX_DISPLAYS];	

static	int	hFD = -1;	/* history file file descriptor	*/

#ifdef	CRAY
static	void	reaper()
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
 *	OpenTranslator
 *
 *	Fork a translator to process a metafile. Set up communications between
 *	idt and the translator process. All translation from the translator
 *	comes from stderr. All comunication to the translator goes to stdout.
 * on entry
 *	channel		: Communication channel. All future communication with
 *			  this process will use this unique channel id. channel
 *			  is an int in the range [0,MAX_DISPLAYS). 
 *	argv		: process to exec with all its args
 *	hfd		: history file fd, if -1 not open
 * on exit
 *	return		: < 0 => failure, else ok
 */
OpenTranslator(channel, argv, hfd)
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
		perror((char *) NULL);
		return(-1);
	}

	if (pipe(to_parent) < 0) {
		(void) close(to_child[0]); (void) close(to_child[1]);
		perror((char *) NULL);
		return(-1);
	}

	if (pipe(stderr_to_parent) < 0) {
		(void) close(to_child[0]); (void) close(to_child[1]);
		(void) close(to_parent[0]); (void) close(to_parent[1]);
		perror((char *) NULL);
		return(-1);
	}
		

	pid = FORK();
	switch	(pid)	{
	case	-1:
		perror((char *) NULL);
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
		for (argptr = argv; *argptr; argptr++)
			;

		*argptr++ = "-fdn";
		*argptr++ = fd_ascii;
		*argptr = NULL;
		

		execvp(argv[0], argv);
		perror((char *) NULL);	/* should never get here	*/
		_exit(127);

		break;
	default:	/* the parent		*/
		Translators[channel].pid = pid;
		Translators[channel].wfd = to_child[1];
		Translators[channel].rfd = to_parent[0];
		Translators[channel].r_err_fd = stderr_to_parent[0];
		Translators[channel].pending = 1;

		(void) close(to_child[0]); 
		(void) close(to_parent[1]);
		(void) close(stderr_to_parent[1]);

		/*
		 * do non blocking reads from the translator
		 */
		(void) fcntl(Translators[channel].r_err_fd, F_SETFL, O_NDELAY);
		(void) fcntl(Translators[channel].rfd, F_SETFL, O_NDELAY);

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
		(void) close(Translators[channel].rfd);
		(void) close(Translators[channel].r_err_fd);
		Translators[channel].pid = -1;

}
/*
 *	TalkTo
 *	[exported]
 *	
 *	Send a syntactically correct ictrans command string to the translator
 *	associated with the specified connection channel id.
 *
 * on entry
 *	id		: a connection id created by OpenDisplay
 *	*command_string	: the ictrans command
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
	static	char	buf[1024];
	char	*b, *s;
	int	pid;

	extern	char *strrchr();

	/*
	 * see if anybody died
	 */
#ifndef	CRAY
	if ((pid = wait3((union wait *) NULL, 
				WNOHANG, (struct rusage *) NULL)) > 0) {
		close_trans_pid(pid);
	}
#endif

	if (Translators[id].pid == -1) return (NULL);

	do {

		if (nreads(Translators[id].rfd, buf, sizeof(buf)) < 0) {
			perror(NULL);
			return(NULL);
		}

		/* 
		 * remove translator prompt from message
		 */
		Translators[id].pending -= strip_prompt(buf);

		if (s = strrchr(buf, '\n')) *s = '\0'; 

		/*
		 * Post any old messages
		 */
		if (strlen(buf)) Message(id, buf);

	} while (mode == SYNC && Translators[id].pending > 0);

	/*
	 * see if there were any error messages
	 */
	if (nreads(Translators[id].r_err_fd, buf, sizeof(buf)) < 0) {
		perror(NULL);
		return(NULL);
	}

	/*
	 * Post any old messages
	 */
	if (strlen(buf)) ErrorMessage(id, buf);

	/*
	 * Send the command to the desired translator
	 */
	(void)write(Translators[id].wfd, command_string,strlen(command_string));

	if (hFD != -1) {
		(void) write(hFD, command_string, strlen(command_string));
	}
	Translators[id].pending += 1;

	if (mode == ASYNC) {	/* don't wait for response	*/
		return("");
	}
	else {	/* wait for translator responce.	 */
		b = buf;

		/*
		 * read from translator until we see a translator prompt
		 * signifying the end of the message or the translator
		 * dies.
		 */
		while(1) {
			/*
			 * see if anybody died
			 */
#ifndef	CRAY
			if ((pid = wait3((union wait *) NULL, 
					WNOHANG, (struct rusage *) NULL)) > 0) {

				close_trans_pid(pid);
			}
#endif
			if (Translators[id].pid == -1) return (NULL);

			if (nreads(Translators[id].rfd, buf, sizeof(buf)) < 0) {
				perror(NULL);
				return(NULL);
			}

			if (strip_prompt(buf)) {
				Translators[id].pending -= 1;
				break;	/* get a prompt yet? */
			}

		}
		
		if (s = strrchr(buf, '\n')) *s = '\0'; 
		/*
		 * Post the message sans the prompt
		 */
		if (strlen(buf)) Message(id, buf);

		/*
		 * return the message sans the prompt
		 */
		return (buf);
	}
}

/*
 *	strip_prompt
 *	[internal]
 *
 *	remove the translator prompt from a string
 * on entry
 *	*s		: a C string possibly containing one or more translator
 *			  prompts
 */
static	strip_prompt(s)
	char	*s;
{

	char	*t1, *t2;
	int	match = 0;
	int	prompt_len	= strlen(PROMPT);

	while (*s) {
		if (strncmp(s, PROMPT, prompt_len) == 0) {
			t2 = s + prompt_len;
			t1 = s;
			match++;
			while(*t1++ = *t2++)
				;
		}
		else
			s++;
	}
	return (match);
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
SignalTo(id, signal)
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
Message(id, s)
	int	id;
	char	*s;
{
	char	buf[132];
	char	*format = "Display(%1d): %s\n";
	int	max_len = sizeof(buf) - strlen(format) - 1;
	char	*t;

	void	AppendText();

	/*
	 * remove any newlines.
	 */
	for(t=s; *t && *t != '\n' && (t-s <= max_len); t++)
		;
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
ErrorMessage(id, s)
	int	id;
	char	*s;
{
	char	buf[132];
	char	*format = "Display(%1d) Error: %s\n";
	int	max_len = sizeof(buf) - strlen(format) - 1;
	char	*t;

	void	AppendText();

	/*
	 * remove any newlines.
	 */
	for(t=s; *t && *t != '\n' && (t-s <= max_len); t++)
		;
	*t = '\0';

	(void) sprintf(buf, format, id, s);

	AppendText(buf);

}
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
 * read from non-blocking descriptor 'fd' until 'n'-1 bytes are read
 * or read would block. Terminate 'buf' with null character
 */
static	nreads(fd, buf, n)
	int	fd;
	char	*buf;
	int	n;
{
	char	c;
	int	count;
	int	status;

	count = 0;
	while ((status = read(fd, &c, 1)) == 1) {
                /*
                 * if there is room in buffer stuff the response in it
                 */
                if (count < n-1) {
                        buf[count++] = c;
                }
	}
	buf[count] = '\0';

	if (status < 0) {
		if (errno != EWOULDBLOCK_ ) {
			perror((char *) NULL);
			return(-1);
		}
	}

	return(1);
}
