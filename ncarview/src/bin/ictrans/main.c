/*
 *	$Id: main.c,v 1.5 1992-09-01 23:38:27 clyne Exp $
 */
#include <stdio.h>
#include <signal.h>
/*
 *	main.c
 *
 *	Author		John Clyne
 *
 *	Date		Fri Apr 13 13:34:33 MDT 1990
 *
 *	This is the program driver for ictrans
 */
main(argc, argv) 
	int	argc;
	char	**argv;
{


	(void) signal(SIGINT, SIG_IGN);
	if (ICTrans(argc, argv, NULL) < 0) {
		exit(1);
	}

	exit(0);

}
