/*
 *	$Id: x.h,v 1.4 2000-07-12 16:52:00 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *      File:		x.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed May  1 17:49:30 MDT 1991
 *
 *      Description:	Some common defines for the x device driver
 *
 */
#ifndef	_x_h_
#define	_x_h_

#define	DEFAULT_WIDTH	512
#define	DEFAULT_HEIGHT	512

#define	MIN_WIDTH	10
#define	MIN_HEIGHT	10

/*
 * maximum X11 color intensity
 */
#define	MAX_INTENSITY	(65535)
/*
 * max distance in RGB space ie.
 *	SQRT(MAX_INTENSITY^2 + MAX_INTENSITY^2 + MAX_INTENSITY^2)
 */
#define	MAX_INTEN_DIST	(113509)

#define	MAX_DPY_LEN	(80)

typedef	unsigned long	Pixeltype;

enum XWorkType_ { XREG = 8, XUSRWIN = 7 };

typedef enum XWorkType_ XWorkType;

typedef enum XColModel_{
	CM_UNDEFINED = -1,
	CM_SHARED = 0,
	CM_PRIVATE = 1,
	CM_MIXED = 2
} XColModel;

#endif	/*	_x_h_	*/
