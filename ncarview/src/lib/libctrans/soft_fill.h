/*
 *	$Id: soft_fill.h,v 1.7 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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

