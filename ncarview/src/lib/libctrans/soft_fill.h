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

/*
 * a table to represent the coordinate pairs in the polyline outlining
 * the polygon 
 */
typedef	struct	{ 
	DCtype	**x_coord;	/* a list of x coords for each y coord 
				 * (0 - max_y) 
				 */ 
	DCtype	y_first,	/* index in x_coord of first coord	*/ 
		y_last;		/* index in x_coord of last coord	*/
	int	*x_count;	/* num x coords in each *x_coord	*/
	int	x_extent, 
		height;		/* current overall dimensions of 
				 * x_coord (memory allocated
				 */
	} FillTable;
#endif

