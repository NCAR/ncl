/*
 *	$Id: soft_fill.h,v 1.5 2000-07-12 18:00:51 haley Exp $
 */
/************************************************************************
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

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#ifndef	soft_fill_
#define	soft_fill_

typedef	short DCtype;	/* Device Coordinate 	*/ 

typedef	struct { 
	DCtype	x,y; 
	} DCPoint;		/* Device Coordinate Pair (a point)	*/ 

extern	int		ySoftFillOffset;
#define	XC_INDEX(y)	(y - ySoftFillOffset)

/*
 * a table to represent the coordinate pairs in the polyline outlining
 * the polygon 
 */
typedef	struct	{ 
	DCtype	**x_coord;	/* a list of x coords for each y coord  */
	DCtype	y_first,	/* index in x_coord of first coord	*/ 
		y_last;		/* index in x_coord of last coord	*/
	int	*x_count;	/* num x coords in each *x_coord	*/
	int	x_coord_size;	/* mem allocated to each *x_coord	*/
	} FillTable;




extern	int	initSoftSim(
#ifdef	NeedFuncProto
	DCtype	minx,
	DCtype	maxx,
	DCtype	miny,
	DCtype	maxy
#endif
);


extern	FillTable	*buildFillTable(
#ifdef	NeedFuncProto
	Ptype		*point_list,
	unsigned	count
#endif
);

#endif

