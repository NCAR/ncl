/*
 *	$Id: soft_fill.c,v 1.7 1992-04-03 20:58:22 clyne Exp $
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
	
#include	<stdio.h>
#include	<ncarv.h>
#include	"soft_fill.h"
#include	"ctrandef.h"
#include	"cgmc.h"

/*
 * becuase device coordinates may not begin at zero, or worse yet even
 * be negative, the x_coord field of FillTable is indexed by adding
 * 'ySoftFillOffset' to the devices 'y' coordinate via the XC_INDEX()
 * macro. This way the x_coord array can always begin at zero regardless
 * of what coord system the devices uses
 */
int	ySoftFillOffset;

/*
 * the fill table
 */
static	FillTable	fillTable;


/*
 * device extents
 */
static	DCtype		minX, maxX, minY, maxY;
/*
 * magnitude of device extents
 */
static	unsigned	xExtent, yExtent;

static	int		*tableWidths;

static	unsigned	isInit = 0;	/* boolean is initializec	*/

/*
 *	initSoftSim
 *	[exported]
 *
 *		initialize the software fill simulation module. Sets the
 *	dispay dimensions. Must be called whenever the display changes 
 *	dimensions (this usually only 	happens in a window environment)
 *
 * on entry
 *	minx		: minimum x extent in device coords
 *	maxx		: maximum x extent in device coords
 *	minx		: minimum y extent in device coords
 *	maxx		: maximum y extent in device coords
 */
initSoftSim(minx, maxx, miny, maxy)
	DCtype	minx, maxx, miny, maxy;
{

	/*
	 * if alread initialized free resources and realocate
	 */
	if (isInit) {
		free_fill_table();
	}

	maxX = maxx;
	minX = minx;
	maxY = maxy;
	minY = miny;
	xExtent = ABS(maxx - minx) + 1;
	yExtent = ABS(maxy - miny) + 1;

	/*
	 * make an initial memory allocation that should handle any
	 * convex polygons.
	 */
	alloc_fill_table(yExtent, 3);

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

	for (i = 0; i < yExtent; i++) {
		if (fillTable.x_coord[i]) free((char *) fillTable.x_coord[i]);
	}
	free((char *) fillTable.x_coord);
	free((char *) fillTable.x_count);
	free((char *) tableWidths);
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
 *
 * on exit
 *	return		: NULL => count < 3, else the address of the
 *			  fill table is returned.
 */
FillTable	*buildFillTable(point_list, count)
	Ptype		*point_list;
	unsigned	count;
{

	int	i,j;
	DCtype	ymax = 0; 
	DCtype	ymin = maxY;
	int	xwidth;		/* how wide a table we need	*/
	int	inc;

	if (count < 2) return((FillTable *) NULL);

	bzero((char *) fillTable.x_count, yExtent * sizeof(int));

	/*
	 * find max y and min y.
	 */
	for (i = 0; i < count; i++) {
		if (ymax < point_list[i].y) ymax = (DCtype) point_list[i].y;
		if (ymin > point_list[i].y) ymin = (DCtype) point_list[i].y;

	}
	/*
	 * clip to device extents
	 */
	ymax = ymax > maxY ? maxY : ymax;
	ymin = ymin < minY ? minY : ymin;

	ySoftFillOffset = ymin;		/* offset for XC_INDEX()	*/

	
	/*
	 * calculate the number of x intercepts for all the polygon
	 * edges
	 */
	bzero((char *) tableWidths, (int) yExtent * sizeof(int));
	for(i=1; i<count; i++) {
		inc = point_list[i-1].y < point_list[i].y ? 1 : -1;
		for(j=point_list[i-1].y; j!=point_list[i].y; j+=inc) {
			if (j >= ymin && j <= ymax) {
				tableWidths[j-ymin]++;
			}
		}
	}


	/*
	 * find "widest" run of X values
	 */
	xwidth = max_array_val(tableWidths, yExtent) + 1; 
	

	/*
	 * see if filltable is wide enough for widest data. 
	 */
	if (fillTable.x_coord_size < xwidth) {
		free_fill_table();
		alloc_fill_table(yExtent, xwidth);
	}

	fillTable.y_first = ymin;	/* first and last y coordinate	*/
	fillTable.y_last = ymax;

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
			(DCtype) point_list[i+1].y,
			ySoftFillOffset);
	}

	/*
	 * close the polygon
	 */
	add_line((DCtype) point_list[count-1].x, 
		(DCtype) point_list[count-1].y,
		(DCtype) point_list[0].x, 
		(DCtype) point_list[0].y,
		ySoftFillOffset);

	/*
	 * add_line does not supply coorinates for the polygon verticies
	 * This needs to be done when the entire table is constructed because
	 * vertices that form local extrema need to be left out for the
	 * parity fill algorithm
	 */
	add_end_points(point_list, count, ySoftFillOffset);


	/*
	 *	sort the table so x coordinates are in ascending order
	 */
	sort_table(ySoftFillOffset);

#ifdef	DEBUG
	print_fill_table ();
#endif
	return(&fillTable);
}

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
static	add_line(x1, y1, x2, y2, yoff)
	DCtype	x1, y1, x2, y2;
	int	yoff;
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
			if (ploty != lasty && ploty != y2 &&
				ploty >= minY && ploty <= maxY &&
				plotx >= minX && plotx <= maxX) {

				/* insert	*/
			fillTable.x_coord[ploty-yoff][fillTable.x_count[ploty-yoff]] = 
									plotx;
			fillTable.x_count[ploty-yoff]++;
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
static	add_end_points(point_list, count, yoff)
	Ptype		*point_list;
	unsigned	count;
	int		yoff;
{
	int	i;
	DCtype	y, x;

	/*
	 * check for extrema at point i+1 for all points except
	 * first and last.
	 */
	for (i = 0; i < count - 2; i++) {

		/*
		 *	check for maxima
		 */
		y = (DCtype) point_list[i+1].y;
		x = (DCtype) point_list[i+1].x;
		if (x < minX || x > maxX || y < minY || y > maxY) {
			continue;	/* clipped	*/
		}

		if (!(point_list[i].y < y && point_list[i+2].y < y)) {

			/*
			 * no maxima, make sure no minima
			 */
			if (!(point_list[i].y >= y && point_list[i+2].y >= y)){
				
				/*
				 * no extrema
				 */
				fillTable.x_coord[y-yoff][fillTable.x_count[y-yoff]] = x;
				fillTable.x_count[y-yoff]++;
			}
		}
	}

	/*
	 * now check first and last point
	 */
	y = (DCtype) point_list[0].y;
	x = (DCtype) point_list[0].x;
	if (x >= minX && x <= maxX && y >= minY && y <= maxY) {
	if (!(point_list[count-1].y < y && point_list[1].y < y)) {

		if (!(point_list[count-1].y >= y && point_list[1].y >= y)){
			
			fillTable.x_coord[y-yoff][fillTable.x_count[y-yoff]] =x;
			fillTable.x_count[y-yoff]++;
		}
	}
	}

	y = (DCtype) point_list[count-1].y;
	x = (DCtype) point_list[count-1].x;
	if (x >= minX && x <= maxX && y >= minY && y <= maxY) {
	if (!(point_list[count-2].y < y && point_list[0].y < y)) {

		if (!(point_list[count-2].y >= y && point_list[0].y >= y)){
			
			fillTable.x_coord[y-yoff][fillTable.x_count[y-yoff]] =x;
			fillTable.x_count[y-yoff]++;
		}
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
static	sort_table(yoff)
	int	yoff;
{

	int	i, j, k, gap;
	int	n;	/* number of items in the list to sort		*/
	DCtype	*ptr;	/* pointer to the current list of x coords	*/
	DCtype	temp;

	for (i = fillTable.y_first; i < (fillTable.y_last + 1); i++) {

		/*
		 * sort fillTable.xcoord[i] 
		 */
		n = fillTable.x_count[i-yoff];
		ptr = fillTable.x_coord[i-yoff];
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

static	int	max_array_val(a, n)
	int	*a;
	int	n;
{
	int	i, max;

	max = a[0];
	for(i=1; i<n; i++) {
		max = max < a[i] ? a[i] : max;
	}
	return(max);
}

static	alloc_fill_table(ys, xs)
	int	ys, xs;
{
	int	i;

	fillTable.x_coord = (DCtype **) icMalloc ((unsigned) 
			(ys * sizeof(DCtype *)));
	fillTable.x_count = (int *) icMalloc ((unsigned) 
			(ys * sizeof (int)));

	fillTable.x_coord_size = xs;
	for (i = 0; i < ys; i++) {
		fillTable.x_coord[i] = (DCtype *) icMalloc(xs * sizeof(DCtype));
		fillTable.x_count[i] = 0;
	}
	tableWidths = (int *) icMalloc(sizeof(int) * ys);

}

#ifdef	DEBUG
static	print_fill_table ()
{
	
	int	i,j;

	for (i = fillTable.y_first; i < (fillTable.y_last + 1); i++) {
		(void) fprintf(stderr, "\nY = %d, X are ...", i);
		for (j = 0; j < fillTable.x_count[i-ySoftFillOffset]; j++) {

			(void) fprintf(stderr, 
				" %d", fillTable.x_coord[i-ySoftFillOffset][j]);
		}
	}
}
#endif	DEBUG

