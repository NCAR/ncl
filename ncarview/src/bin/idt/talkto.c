/*
 *	$Id: talkto.c,v 1.4 1991-02-06 15:10:22 clyne Exp $
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
 * a structure containing information about running translators
 */
static	struct	{
	int	pid;		/* the pid of the translator process	*/
	int	wfd;		/* a fd for writing to the process	*/
	int	r_err_fd;	/* standard error from the process	*/
	} Translators[MAX_DISPLAYS];	

static	int	hFD;	/* history file file descriptor	*/

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
 *	*device		: device arg for translator
 *	*font		: font arg for translator
 *	*metafile	: name of metafile to translate
 *	hfd		: history file fd, if -1 not open
 * on exit
 *	return		: < 0 => failure, else ok
 */
OpenTranslator(channel, device, font, metafile, hfd)
	int	channel;
	char	*device;
	char	*font;
	char	*metafile;
	int	hfd;
{

	int	i;
	int	pid;
	int	to_child[2],		/* pipe descriptors to/from process */
		stderr_to_parent[2];

	static	char	*translator;

	char	*argv[10];

	static	short first	= 1;

	if (first) {	/* one time initialization		*/
		for (i = 0; i < MAX_DISPLAYS; i++)
			Translators[i].pid = -1;
			
		/*
		 * build path to the translator if we know it. Else we
		 * hope its on the user's search path.
		 */
#ifdef	BINDIR
		translator = icMalloc((unsigned) 
				(strlen(BINDIR) + strlen(TRANSLATOR) + 2));

		(void) strcpy(translator, BINDIR);
		(void) strcat(translator, "/");
		(void) strcat(translator, TRANSLATOR);
#else
		translator = icMalloc(strlen(TRANSLATOR) + 1);
		(void) strcpy(translator, TRANSLATOR);
#endif

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

	argv[0] = translator;
	argv[1] = "-d";			/* device specifier arg	*/
	argv[2] = device;		/* device to translate to	*/
	argv[3] = "-f";			/* font specifier arg	*/
	argv[4] = font;			/* font for text in translation	*/
	argv[5] = "-bell";		/* turn off the bloody bell	*/
	argv[6] = metafile;		/* metafile to translate	*/
	argv[7] = NULL;

		
	hFD = hfd;			/* the history file fd		*/

	/*
	 * set up pipes to communicate with child process. 
	 */
	if (pipe(to_child) < 0) {
		perror((char *) NULL);
		return(-1);
	}

	if (pipe(stderr_to_parent) < 0) {
		(void) close(to_child[0]); (void) close(to_child[1]);
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

		execvp(argv[0], argv);
		perror((char *) NULL);	/* should never get here	*/
		_exit(127);

		break;
	default:	/* the parent		*/
		Translators[channel].pid = pid;
		Translators[channel].wfd = to_child[1];
		Translators[channel].r_err_fd = stderr_to_parent[0];

		(void) close(to_child[0]); 
		(void) close(stderr_to_parent[1]);

		/*
		 * do non blocking reads from the translator
		 */
		(void) fcntl(Translators[channel].r_err_fd, F_SETFL, O_NDELAY);

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
	int	n, l, r;
	static	char	buf[1024];
	char	*b, *s;
	int	pid;

	extern	char *strrchr();

	r = 1024;

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

	/*
	 * read any previous messages from the translator that may still
	 * be waiting in the pipe
	 */
	b = buf;
	for(n=0; n < r; n+=l) {
		if ((l = read(Translators[id].r_err_fd, b + n, r - n)) < 0) {
#ifndef	ultrix
			if (errno != EWOULDBLOCK ) {
#else
			/*
			 * ultrix returns wrong error code when pipe empty
			 */
			if (errno != EWOULDBLOCK && errno != EAGAIN) {
#endif
				perror((char *) NULL);
				return(NULL);
			}
			else break;
		}
		else if (l == 0) {
			break;
		}
	}
	buf[n] = '\0';
	
	(void) strip_prompt(buf);/* remove translator prompt from message */

	if (s = strrchr(buf, '\n')) *s = '\0'; 

	/*
	 * Post any old messages
	 */
	if (strlen(buf)) Message(id, buf);

	/*
	 * Send the command to the desired translator
	 */
	(void)write(Translators[id].wfd, command_string,strlen(command_string));
	if (hFD != -1) {
		(void) write(hFD, command_string, strlen(command_string));
	}

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

			for(n=0; n < r; n+=l) {
				if ((l = read(Translators[id].r_err_fd, 
					b + n, r - n)) < 0) {

#ifndef	ultrix
					if (errno != EWOULDBLOCK ) {
#else
					if (errno != EWOULDBLOCK && 
							errno != EAGAIN) {
#endif
						perror((char *) NULL);
						return(NULL);
					}
					else break;
				}
				else if (l == 0) {
					break;
				}
			}
			buf[n] = '\0';
			if (strip_prompt(buf)) break;	/* get a prompt yet? */

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
			match = 1;
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
	char	*cptr;
	char	buf[20];

	void	AppendText();

	(void) sprintf(buf, "Translator[%d]: ", id);

	cptr = icMalloc((unsigned) (strlen(buf) + strlen(s) + strlen("\n")+ 1));
	(void) strcpy(cptr, buf);
	(void) strcat(cptr, s); 
	(void) strcat(cptr, "\n"); 

	AppendText(cptr);

	cfree(cptr);
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
			

