/*
 *	$Id: rast.h,v 1.8 2000-08-22 03:30:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1994                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
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

#ifndef	_color_
#define	_color_

#define	MAX_COLOR	256
#define	MAX_COLOR_INDEX	256

typedef	struct	{
	unsigned char	red,
			green,
			blue;
	} Rgb;

/*
 *	we use this struct to maintain a default and a current colormap.
 */	
typedef	struct	{
	Rgb	rgb[MAX_COLOR_INDEX];
	} RasColrTab;

#define	RAS_PUT_PIX(ras, x,y, index, table, direct) \
	if (direct) { \
		DIRECT_RED((ras),(x),(y)) = (table).rgb[(index)].red; \
		DIRECT_GREEN((ras),(x),(y)) = (table).rgb[(index)].green; \
		DIRECT_BLUE((ras),(x),(y)) = (table).rgb[(index)].blue; \
	} \
	else { \
		INDEXED_PIXEL((ras),(x),(y)) = (index); \
	} 
#endif

