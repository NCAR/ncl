/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/

/*
 *	glob.c
 *
 *	Author		John Clyne
 *
 *	Date		Mon Apr 23 13:07:50 MDT 1990
 *
 *	perform filname expansion on a string using the shell
 */
#include <stdio.h>
#include <ctype.h>
#include <ncarv.h>

extern	char	*strcpy();
extern	char	*strcat();

static int	to_child[2],
		to_parent[2];	/* pipes for talking to spawned process	*/

#define	MAX_LINE_LEN	80

#define	ACK	"echo ''\n"

/*
 *	glob
 *	[exported]
 *
 *	perform filename expansion on a string. glob allocates memory as
 *	necessary and returns a pointer to that memory. glob uses the command
 *	specified by the enviroment variable "SHELL" to do expansion. If 
 *	SHELL is not set glob uses /bin/sh by default.
 * on entry
 *	*s		: the string
 * on exit
 *	***r_argv	: a list of files expanded by the shell
 *	*r_argc		: size of r_argv
 */
glob(s, r_argv, r_argc)
	char	*s;
	char	***r_argv;
	int	*r_argc;
{

	static	short	is_init = 0;
	static	char	**argv;
	static	int	argc;
	static	int	args;	/* memory alloced to argv	*/
	static	char	inBuf[2*BUFSIZ];

	int	i;
	char	outbuf[MAX_LINE_LEN];
	char	*cptr;
	int	nbytes;
	char	*shell;
	extern	char	*getenv();

	*r_argv = NULL;
	*r_argc = argc = 0;

	/*
	 * perform one time initialization
	 */
	if (!is_init) {

		/*
		 * try and find out what shell the user like so we can spawn
		 * it to parse it to do globbing.
		 */
		if ((shell = getenv ("SHELL")) == NULL) {
			shell = "/bin/sh";
		}

		talkto(shell);		/* spawn shell to talk to	*/
		is_init = 1;


		argv = (char **) icMalloc(SMALL_MALLOC_BLOCK * sizeof(char **));
		args = SMALL_MALLOC_BLOCK;
	}

	if ((strlen(outbuf) + strlen(s) + 1) >= MAX_LINE_LEN) {
		(void) fprintf(stderr, "Line too long: %s\n", s);
		return;
	}

	/*
	 * build command to send to the shell. 
	 */
	(void) strcpy(outbuf, "echo ");
	(void) strcat(outbuf, s);
	(void) strcat(outbuf, "\n");

	/*
	 * send "echo whatever" to shell. Also send a  so we get an
	 * ack back. We need that ack in case the string send doen't 
	 * generate a responce to stdout. i.e. a shell error
	 */
	(void) write(to_child[1], outbuf, strlen(outbuf));
	(void) write(to_child[1], ACK, strlen(ACK));

	/*
	 * read in output from shell
	 */
	nbytes = 0;
	while (1) {	/* read until receive ack or buffer is full	*/
		cptr = inBuf + nbytes;
		nbytes += read(to_parent[0], cptr, 2*BUFSIZ - nbytes);
		if ((inBuf[nbytes - 2] == '') || nbytes == 2*BUFSIZ) break; 
	}


	if (inBuf[0] == '') return;	/* shell syntax error probably	*/

	/*
	 * replace terminating newline with a null terminator
	 */
	for(i = 0; i<nbytes; i++) {
		if (inBuf[i] == '\n')
			inBuf[i] = '\0';
	}
	inBuf[nbytes] = '\0';

	/*
	 * null terminate and assigne a poiner to each arg in inBuf 
	 */
	cptr = inBuf;
	argv[argc++] = cptr;	/* point to first arg	*/
	while(*cptr) {
		if (isspace(*cptr)) {
			*cptr = '\0';
			if (argc >= args) {	/* enough memory ?	*/
				args += SMALL_MALLOC_BLOCK;
				argv = (char **) icRealloc ((char *) argv,
					args * sizeof (char **));
			}
			argv[argc++] = cptr+1;
		}
		cptr++;
	}

	*r_argv = argv;
	*r_argc = argc;
}



/*
 *	talkto
 *	[internal]
 *	
 *	set up communictions between invoking process and the desired
 *	command;
 * on entry
 *	*cmd		: name of command to talk to
 * on exit
 *	to_child[1]	: fd for writing to spawned process
 *	to_parent[0]	: fd for reading from spawned process
 */
static	talkto(cmd) 
	char	*cmd;
{
	int	pid;

	if (pipe(to_child) < 0) {
		perror((char *) NULL);
		exit(1);
	}
	if (pipe(to_parent) < 0) {
		perror((char *) NULL);
		exit(1);
	}

	if ((pid = fork()) == 0) {	/* the child process		*/
		(void) close(fileno(stdin));	/* close child's stdin	*/
		(void) dup(to_child[0]);	/* redirect stdin from pipe*/
		(void) close(fileno(stdout));	/* close child's stdout	*/
		(void) dup(to_parent[1]);	/* redirect stdout to pipe*/

		(void) close(to_child[0]);	/* close the pipes	*/
		(void) close(to_child[1]);
		(void) close(to_parent[0]);
		(void) close(to_parent[1]);

		/* 
		 * exec the command to talk to	
		 */
		if (execlp(cmd, cmd, NULL) < 0)	 {
			perror((char *) NULL);
			exit(1);
		}

	}
	else if (pid > 0) {		/* we're the parent		*/

	}

	else {	/* error	*/
		perror((char *) NULL);
		exit(1);
	}
}
