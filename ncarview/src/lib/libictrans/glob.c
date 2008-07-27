/*
 *	$Id: glob.c,v 1.13 2008-07-27 03:18:45 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

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
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <ncarg/c.h>
#include "ictrans.h"
#include "glob.h"

static int	to_child[2],
		to_parent[2];	/* pipes for talking to spawned process	*/

static	char	ackString[80];
const	char	Magic[] = "NCARG_GRAPHICS_MAGIC_COOKIE";
const	int	magicLen = (sizeof(Magic) / sizeof(Magic[0]));

/*
 *	talkto
 *	[internal]
 *	
 *	set up communictions between invoking process and the desired
 *	command; stderr of command is sent to /dev/null
 * on entry
 *	**argv		: name of command to talk to
 * on exit
 *	to_child[1]	: fd for writing to spawned process
 *	to_parent[0]	: fd for reading from spawned process
 */
static	talkto(argv) 
	char	**argv;
{
	int	pid;
	FILE	*fp;

	if (pipe(to_child) < 0) {
		perror((char *) NULL);
		exit(1);
	}
	if (pipe(to_parent) < 0) {
		perror((char *) NULL);
		exit(1);
	}


	if ((pid = fork()) == 0) {	/* the child process		*/
		fp = fopen("/dev/null", "a");
		(void) close(fileno(stdin));	/* close child's stdin	*/
		(void) dup(to_child[0]);	/* redirect stdin from pipe*/
		(void) close(fileno(stdout));	/* close child's stdout	*/
		(void) dup(to_parent[1]);	/* redirect stdout to pipe*/
		(void) close(fileno(stderr));	/* close child's stderr	*/
		(void) dup(fileno(fp));	/* redirect stderr to bit-buck*/


		(void) close(to_child[0]);	/* close the pipes	*/
		(void) close(to_child[1]);
		(void) close(to_parent[0]);
		(void) close(to_parent[1]);
		(void) fclose(fp);

		/* 
		 * exec the command to talk to	
		 */
		(void) execvp(argv[0], argv);

		perror((char *) NULL);	/* shouldn't get here	*/
		exit(1);

	}
	else if (pid > 0) {		/* we're the parent		*/

	}

	else {	/* error	*/
		perror((char *) NULL);
		(void) exit(1);
	}
}
/*
 *	glob
 *	[exported]
 *
 *	perform filename expansion on a string. glob allocates memory as
 *	necessary and returns a pointer to that memory. glob uses the command
 *	specified by the enviroment variable "SHELL" to do expansion. If 
 *	SHELL is not set glob uses /bin/sh by default.
 * on entry
 *	*string		: the string
 * on exit
 *	***r_argv	: a list of files expanded by the shell
 *	*r_argc		: size of r_argv
 */
void	glob(string, r_argv, r_argc)
	const char	*string;
	char	***r_argv;
	int	*r_argc;
{

	static	short	is_init = 0;
	static	char	**argv;
	static	int	argc;
	static	int	args;	/* memory alloced to argv	*/
	static	char	inBuf[4*BUFSIZ];

	int	i;
	char	outbuf[1024];
	char	*cptr;
	int	nbytes;
	char	*shell_argv[3];
	char	*t, *s;

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
		if ((shell_argv[0] = getenv ("SHELL")) == NULL) {
			shell_argv[0] = "/bin/sh";	/* default	*/
		}
		shell_argv[1] = NULL;

		/*
		 * if using csh then use csh with the fast option, '-f'
		 */
        	t = (t = strrchr(shell_argv[0], '/')) ? ++t : shell_argv[0];
		if ((strcmp(t, "csh") == 0) || (strcmp(t, "tcsh") == 0)) {
			shell_argv[1] = "-f";
			shell_argv[2] = NULL;
		}
		else if (strcmp(t, "ksh") == 0) {
			shell_argv[1] = "-p";
			shell_argv[2] = NULL;
		}


		talkto(shell_argv);	/* spawn shell to talk to	*/
		is_init = 1;


		argv = (char **) malloc(SMALL_MALLOC_BLOCK * sizeof(char **));
		if (! argv) {
			perror("malloc()");
			return;
		}
		args = SMALL_MALLOC_BLOCK;

		sprintf(ackString, "/bin/echo %s\n", Magic);
	}

	if ((strlen(outbuf) + strlen(string) + 1) >= sizeof(outbuf)) {
		(void) fprintf(stderr, "Line too long: %s\n", string);
		return;
	}

	/*
	 * build command to send to the shell. 
	 */
	(void) strcpy(outbuf, "/bin/echo ");
	(void) strcat(outbuf, string);
	(void) strcat(outbuf, "\n");

	/*
	 * send "echo whatever" to shell. Also send a \001 so we get an
	 * ack back. We need that ack in case the string send doen't 
	 * generate a responce to stdout. i.e. a shell error
	 */
	(void) write(to_child[1], outbuf, strlen(outbuf));
	(void) write(to_child[1], ackString, strlen(ackString));

	/*
	 * read in output from shell
	 */
	nbytes = 0;
	while (1) {	/* read until receive ack or buffer is full	*/
		cptr = inBuf + nbytes;
		nbytes += read(to_parent[0], cptr, sizeof(inBuf) - nbytes);
		if ((s = strstr(inBuf, Magic)) || nbytes == sizeof(inBuf)) {
			*s = '\0';
			break; 
		}
	}

	if (nbytes == sizeof(inBuf)) {
		inBuf[nbytes-1] = '\0';
	}


	if (strlen(inBuf) == 0)  {
		return;	/* no match	*/
	}

	/*
	 * replace terminating newline with a null terminator
	 */
	if (s = strchr(inBuf, '\n')) {
		*s = '\0';
	}

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
				argv = (char **) realloc ((char *) argv,
					(unsigned) (args * sizeof (char **)));
			}
			argv[argc++] = cptr+1;
		}
		cptr++;
	}

	*r_argv = argv;
	*r_argc = argc;
}



