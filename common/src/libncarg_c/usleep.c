/*
 *      $Id: usleep.c,v 1.7 2000-08-22 04:03:33 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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

void	USleep(usec)
	unsigned	usec;
{
#ifdef	SUN
	(void) usleep(usec);
#else
	struct timeval	tv;
	tv.tv_usec = usec % 1000000;
	tv.tv_sec = usec / 1000000;
	(void) select(1, (fd_set *) NULL, (fd_set *) NULL, (fd_set *) NULL,&tv);
#endif
}
