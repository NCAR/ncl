/*
 *	$Id: main.c,v 1.2 1991-01-09 10:56:21 clyne Exp $
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
	ICTrans(argc, argv, NULL);

}
