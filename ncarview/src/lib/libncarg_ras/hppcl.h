/*
 *	$Id: hppcl.h,v 1.3 2000-07-12 18:01:35 haley Exp $
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

#ifndef	_hppcl_
#define	_hppcl_

#define	HPPCL_RESET	"E"
#define	HPPCL_PORTRAIT	"&l0O"
#define	HPPCL_LANDSCAPE	"&l1O"
#define	HPPCL_ENCODING	""
#define	HPPCL_START	"1A"
#define	HPPCL_END	"B"
#define	HPPCL_EJECT	"&l0H"
#define	HPPCL_POSITION	"%dx%dY"
#define	HPPCL_TRANSFER	"%dW"
#define	HPPCL_RESOLUTION	"%dR"


#ifdef	DEAD
#define	HPPCL_PAPER_WIDTH	8.5
#define	HPPCL_PAPER_HEIGHT	11.0
#endif

#define	HPPCL_PAPER_WIDTH	8.0
#define	HPPCL_PAPER_HEIGHT	10.0

#define	HPPCL_MAX_RES		300	/* default resolution 300dpi	*/

typedef	struct	HPPCL_Info	{
	int		do_compress;
	int		orientation;		
	int		dpi;
	char		*trans_data;
	int		row_size;
	int		image_size;
	int		start_x;
	int		start_y;
	} HPPCL_Info;

#endif	/* _hppcl_ */
