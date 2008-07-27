/*
 *	$Id: main.c,v 1.10 2008-07-27 03:18:38 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdio.h>
#include <sys/types.h>
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
