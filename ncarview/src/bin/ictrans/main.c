/*
 *	$Id: main.c,v 1.3 1992-06-24 21:01:19 clyne Exp $
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
