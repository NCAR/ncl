/*
 *	$Id: soft_fill.c,v 1.2 1991-01-09 11:11:31 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include	<stdio.h>
#include	"soft_fill.h"
#include	"cgmc.h"


/*
 *	soft_fill.c
 *	
 *	Author		John Clyne
 *
 *	Date		Fri Sep 29 15:54:43 MDT 1989
 *
 *		This file contains a set of routines to aid in software
 *	simulation of filled polygons. Currenty only solid fill is supported
 *	but extensions to hatch fills should be fairly simple. Soft_fill
 *	creates a table of (x,y) coordinates that map out the boundry
 *	of a polygon so that parity filling can be done. This table
 *	is of type FillTable and is returned indirectly from 
 *	buildFillTable(). Prior to invoking buildFillTable() and whenever
 *	the dimensions of the display change initSoftSim() must be invoked
 *
 * rev 1.1 clyne 2/20/90	: 'isInit' not set correctly causing erronoeus
 *				  plots
 */
	
extern	char	*malloc();

static	FillTable	fillTable =  { /* the fill table		*/
				NULL, 0, 0, NULL, 0, 0
			};

static	unsigned	Height;		/* height of display (dev coord)*/
static	unsigned	xExtent;	/* X extent of display (dev coord*/
static	unsigned	isInit = 0;	/* boolean is initializec	*/

/*
 *	initSoftSim
 *	[exported]
 *
 *		initialize the software fill simulation module. Sets the
 *	dispay dimensions. Must be called whenever the display changes 
 *	dimensions (this usually only 	happens in a window environment)
 * on entry
 *	height		: height of display in device coordinates
 *	x_extent	: maximum valid device coord in x direction
 */
initSoftSim(height, x_extent)
	DCtype	height,
		x_extent;
{
	int	i;

	/*
	 * if alread initialized free resources and realocate
	 */
	if (isInit) {
		free_fill_table();
		cfree ((char *) fillTable.x_coord); 
		cfree((char *) fillTable.x_count);
	}

	fillTable.x_coord = (DCtype **) malloc ((unsigned) 
			(2 * height * sizeof(DCtype *)));
	fillTable.x_count = (int *) malloc ((unsigned) 
			(2 * height * sizeof (int)));
	Height = height;	/* height of the display */
	xExtent = x_extent;	/* width of the display		*/

	/*
	 * malloc enough memmory for twice display height. This kludge
	 * allows for coordinates that extend past view surface
	 */
	for (i = 0; i < (2 * Height); i++) {
		fillTable.x_coord[i] = NULL;
		fillTable.x_count[i] = 0;
	}

	fillTable.x_extent = 0;		/* mem allocated to x_coord[i]	*/
	fillTable.height = height;	/* mem allocated to x_coord	*/
	fillTable.y_first = fillTable.y_last = 0;

	isInit = 1;
}


/*
 *	free_fill_table
 *	[internal]
 *
 *		free memory alloced to fill table
 *
 * on entry		: initSoftSim has been called
 * on exit
 *	fillTable	: is returned to initial state.
 */
static	free_fill_table()
{
	int	i;

	if (!isInit) return;

	for (i = 0; i < Height; i++) {
		if (fillTable.x_coord[i]) cfree((char *) fillTable.x_coord[i]);
		fillTable.x_coord[i] = NULL;
	}
}

/* 
 *	buildFillTable
 *	[exported]
 *		
 *		construct a table containing all the pixel coordinates
 *	necessary to render a polyLINE in a format that can be easily
 *	used to perform software filling of that polyline. The format 
 *	of the table is as follows: Each coodinate x,y pair is hashed
 *	by the y value in to a list of x coordinates. Each list
 *	of x coordinates is ordered. 
 *
 *	buildFillTable assumes horizontal filling is done and will not
 *	work if the fill lines are drawn in any other direction
 * 
 * on entry
 *	point_list	: list of n coord. pairs describing a polygon
 *			  point_list[0] != point_list[n-1]
 *	count		: num elements in point_list, n
 */
FillTable	*buildFillTable(point_list, count)
	Ptype		*point_list;
	unsigned	count;
{

	int	i;
	DCtype	xmax = 0;
	DCtype	ymax = 0; 
	DCtype	xmin = xExtent;
	DCtype	ymin = Height;
	DCtype	xwidth = 0;	/* how wide a table we need	*/

	DCtype	*hit_list;	/* which X points are used in point_list */

	if (count < 2)
		return((FillTable *) NULL);

	if ((hit_list = (DCtype *) malloc 
		((unsigned) (xExtent * sizeof (DCtype)))) == NULL) {

		perror("");
		exit(1);
	}
	bzero((char *) hit_list, (int) xExtent * sizeof(DCtype));

	/*
	 * find max x and y and min x and y and record all X values used
	 */
	for (i = 0; i < count; i++) {
		if (xmax < point_list[i].x) xmax = (DCtype) point_list[i].x;
		if (ymax < point_list[i].y) ymax = (DCtype) point_list[i].y;
		if (xmin > point_list[i].x) xmin = (DCtype) point_list[i].x;
		if (ymin > point_list[i].y) ymin = (DCtype) point_list[i].y;

		hit_list[point_list[i].x] = 1; 
	}


	/*
	 *	find out how wide our table needs to be based on how many
	 *	x values are used. This is conservative but is guaranteed
	 *	to give us enough memory
	 */
	for (i = 0, xwidth = 0; i < xExtent; i++) {
		if (hit_list[i]) xwidth++;
	}
	cfree ((char *) hit_list);

	/*
	 * see if filltable is wide enough for widest data. 
	 */
	if (fillTable.x_extent < xwidth) {
		free_fill_table();

		fillTable.x_extent = xwidth;
	}

	fillTable.y_first = ymin;	/* first and last y coordinate	*/
	fillTable.y_last = ymax;

	/*
	 * malloc memory for the x coordinates. This is a little wastefull
	 * since for all y coordinates we give the maximum memory needed
	 * for the largest set of x coords at a given y. But is fast
	 */
	for (i = ymin; i < (ymax + 1); i++) {
		if (!(fillTable.x_coord[i])) {
			fillTable.x_coord[i] = (DCtype *) malloc 
				((unsigned) fillTable.x_extent *sizeof(DCtype));
		}

		if (fillTable.x_coord[i] == NULL) 
			exit(1);

		fillTable.x_count[i] = 0;	/* no x coordinates	*/
	}

	/*
	 * make sure first and last points are not equal
	 */ 
	if (point_list[0].x == point_list[count-1].x
		&& point_list[0].y == point_list[count-1].y) {

		count--;
	}
	/*
	 * add the pixels coords to the fillTable
	 */
	for (i = 0; i < count - 1; i++) {
		add_line((DCtype) point_list[i].x, 
			(DCtype) point_list[i].y,
			(DCtype) point_list[i+1].x, 
			(DCtype) point_list[i+1].y);
	}
	/*
	 * close the polygon
	 */
	add_line((DCtype) point_list[count-1].x, 
		(DCtype) point_list[count-1].y,
		(DCtype) point_list[0].x, 
		(DCtype) point_list[0].y);

	/*
	 * add_line does not supply coorinates for the polygon verticies
	 * This needs to be done when the entire table is constructed because
	 * vertices that form local extrema need to be left out for the
	 * parity fill algorithm
	 */
	add_end_points(point_list, count);

	/*
	 *	sort the table so x coordinates are in ascending order
	 */
	sort_table();

#ifdef	DEBUG
	print_fill_table ();
#endif
	return(&fillTable);
}

#define	MAX(X,Y)	(((X) > (Y)) ? (X) : (Y))
#define	ABS(X)		((X) < 0 ? -(X) : (X))
/*
 *	add_line
 *	[internal]
 *
 *		Add a single line to the fillTable. This algorithm is 
 *	based on Bresenham's algo. All the coordinates for the line are
 *	not included. Specifically the end points are left out and
 *	each horizontal segment is treated like an end point. This is 
 *	necessary for the fill algorithm.
 */
static	add_line(x1, y1, x2, y2)
	DCtype	x1, y1, x2, y2;
{

	DCtype	dx, dy;		/* difference between endpoints	*/
	int	ix, iy;		/* absolute value of (dx, dy)	*/
	int	i, inc;		/* the largest of ix, iy	*/
	DCtype	x,y;
	DCtype	plotx, ploty;	/* possible candiated for ploting	*/
	DCtype	lasty;	/* previous value of plot{x,y}		*/
	unsigned short	plot;	/* boolean				*/

	dx = x2 - x1;
	dy = y2 - y1;

	ix = ABS(dx);
	iy = ABS(dy);
	inc = MAX(ix, iy);

	plotx = x1;
	ploty = lasty = y1;
	x = y = 0;

	for (i = 0; i <= inc; ++i) {
		x += ix;
		y += iy;

		plot = 0;	/* false	*/

		if (x > inc) {
			plot = 1;
			x -= inc;
			plotx = (dx > 0 
				? plotx + 1 : (plotx == 0 ? plotx : plotx - 1));

		}

		if (y > inc) {
			plot = 1;
			y -= inc;
			ploty = (dy > 0 
				? ploty + 1 : (ploty == 0 ? ploty : ploty - 1));
		}


		if (plot) {
			/*
			 * only insert a point if it has different y coord
			 * from the last point inserted and is not the 
			 * end point (the last y value)
			 */
			if (ploty != lasty && ploty != y2) {
				/* insert	*/
			fillTable.x_coord[ploty][fillTable.x_count[ploty]] = 
									plotx;
			fillTable.x_count[ploty]++;
			}

			lasty = ploty;	/* keep track of previous point	*/
		}
	}

}

/*
 *	add_end_points
 *	[internal]
 * 
 *		The add_line routine does not include line end points when
 *	it builds the fill table. This because end points require special
 *	care. If an end point is at a local extrema in the polygon we don't
 *	want it in the fill table. (it screws up the parity for the fill
 *	algorithm). Polygon segments that are tangental (horizontal) to the 
 *	fill also cause problems. They may be considered as a single point.
 *	Again, if they are located at extrema we don't want them in the
 *	fill table. If they are not extrema we only want one point to represent
 *	them. Thus this function adds end points (and single points for 
 *	horizontal lines) to the fill Table that are NOT extrema and were not 
 *	inserted by add_line().
 */
static	add_end_points(point_list, count)
	Ptype		*point_list;
	unsigned	count;
{
	int	i;
	DCtype	y;

	/*
	 * check for extrema at point i+1 for all points except
	 * first and last.
	 */
	for (i = 0; i < count - 2; i++) {

		/*
		 *	check for maxima
		 */
		y = (DCtype) point_list[i+1].y;
		if (!(point_list[i].y < y && point_list[i+2].y < y)) {

			/*
			 * no maxima, make sure no minima
			 */
			if (!(point_list[i].y >= y && point_list[i+2].y >= y)){
				
				/*
				 * no extrema
				 */
				fillTable.x_coord[y][fillTable.x_count[y]] =
					(DCtype) point_list[i+1].x;

				fillTable.x_count[y]++;
			}
		}
	}

	/*
	 * now check first and last point
	 */
	y = (DCtype) point_list[0].y;
	if (!(point_list[count-1].y < y && point_list[1].y < y)) {

		if (!(point_list[count-1].y >= y && point_list[1].y >= y)){
			
			fillTable.x_coord[y][fillTable.x_count[y]] =
				(DCtype) point_list[0].x;

			fillTable.x_count[y]++;
		}
	}

	y = (DCtype) point_list[count-1].y;
	if (!(point_list[count-2].y < y && point_list[0].y < y)) {

		if (!(point_list[count-2].y >= y && point_list[0].y >= y)){
			
			fillTable.x_coord[y][fillTable.x_count[y]] =
				(DCtype) point_list[count-1].x;

			fillTable.x_count[y]++;
		}
	}
}


				
/*
 *	sort_table
 *	[internal]
 *
 *		sort each x coordinate list into ascending order. Duplicate
 *	entries are allowed.
 */
static	sort_table()
{

	int	i, j, k, gap;
	int	n;	/* number of items in the list to sort		*/
	DCtype	*ptr;	/* pointer to the current list of x coords	*/
	DCtype	temp;

	for (i = fillTable.y_first; i < (fillTable.y_last + 1); i++) {

		/*
		 * sort fillTable.xcoord[i] 
		 */
		n = fillTable.x_count[i];
		ptr = fillTable.x_coord[i];
		for (gap = n/2; gap > 0; gap /=2) {
			for (k = gap; k < n; k++) {
				for (j = k - gap; j >= 0; j -= gap) {
					if (ptr[j] <= ptr[j+gap]) {
						break;
					}

					temp = ptr[j];
					ptr[j] = ptr[j + gap];
					ptr[j + gap] = temp;
				}
			}
		}
	}
}

#ifdef	DEBUG
static	print_fill_table ()
{
	
	int	i,j;

	for (i = fillTable.y_first; i < (fillTable.y_last + 1); i++) {
		(void) fprintf(stderr, "\nY = %d, X are ...", i);
		for (j = 0; j < fillTable.x_count[i]; j++) {

			(void) fprintf(stderr, " %d", fillTable.x_coord[i][j]);
		}
	}

}
#endif	DEBUG

