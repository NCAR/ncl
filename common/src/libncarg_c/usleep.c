
/*
 *      $Id: usleep.c,v 1.2 1992-09-09 17:38:23 clyne Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
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


void	USleep(usec)
	unsigned	usec;
{
#ifdef	sun
	(void) usleep(usec);
#else
	struct timeval	tv;
	tv.tv_usec = usec % 1000000;
	tv.tv_sec = usec / 1000000;
	(void) select(1, (fd_set *) NULL, (fd_set *) NULL, (fd_set *) NULL, tv);
#endif
}
