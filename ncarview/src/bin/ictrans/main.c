/*
 *	$Id: main.c,v 1.4 1992-07-31 21:07:12 clyne Exp $
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
