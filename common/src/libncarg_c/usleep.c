/*
 *      $Id: usleep.c,v 1.9 2008-12-28 13:25:36 haley Exp $
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
 *	File:		usleep
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:35:12 MDT 1992
 *
 *	Description:	Suspend execution for interval in microseconds
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef RS6000
#define	NBBY	8	/* xlc compiler isn't config'ed properly	*/
#include <sys/select.h>
#endif

/* BSD systems need this for usleep(3) */
#include <sys/param.h>  
#include <unistd.h>

void	USleep(usec)
	unsigned	usec;
{
#if (defined(SUN) || defined(BSD4_3))
	(void) usleep(usec);
#else
	struct timeval	tv;
	tv.tv_usec = usec % 1000000;
	tv.tv_sec = usec / 1000000;
	(void) select(1, (fd_set *) NULL, (fd_set *) NULL, (fd_set *) NULL,&tv);
#endif
}
