/*
 *	$Id: hatch.c,v 1.2 1991-01-09 11:10:37 clyne Exp $
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
#include <stdio.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarv.h>
#include	"cgmc.h"
#include	"default.h"
#include	"Xdefs.h"
#include	"ctrandef.h"
#include	<math.h>



#define	DEG2RAD(THETA)	((M_PI) * THETA / 180.0)	
#define	SPACEING	10


extern	char	*program_name;
extern	FILE	*errfile;
extern	int	VDC2Xcoord();
extern	double	Xoffs, Yoffs, Yvdc2d, Xvdc2d;
extern	long	Xvdcsw, Yvdcsw;

extern	Display	*dpy;
extern	Window	win;
extern	GC	lineGC, markerGC, polygonGC, cellGC;
static	XGCValues	gcv;

struct	{			/* struct to hold a polygon	*/
	XPoint	P[50];
	int	num_edge;	/* number of edges of polygon	*/
	} poly;

	/* struct to hold slope and offset in
	 * line equation of the form y = mx + c
	 */
typedef	struct {		
	float	slope;		/* slope of the line		*/
	int	C;		/* offset C	C	*/
	} eq;

typedef	struct	{
		int	index;
		float	dist;
		} distlist;



	eq	edges[50];	/* list of equations for all lines making
				 * up the edges in the polygon
				 */
	eq	hatch_origin_eq;/* equation of a hatch line at origin	*/



/*	simple:
 *
 *		this routine breaks up a complex polygon to simpleones
 */
int	simple(c)
	CGMC	*c;
{
	static	int	index;	/* index into cgmc		*/

	int	i;

	for (i=0;i<c->Pnum;i++) {
		poly.P[i].x = XConvert(c->p[i].x);
		poly.P[i].y = YConvert(c->p[i].y);
	}
		
	/* make sure last point and first point are the same. if not fix it */
	if ((poly.P[i-1].x != poly.P[0].x) 
			|| (poly.P[i-1].x != poly.P[0].y)) {

		poly.P[i].x = poly.P[0].x;
		poly.P[i].y = poly.P[0].y;
		poly.num_edge = i;
	}

	else
		poly.num_edge = i-1;


	return(1);
}

/*	edge_eq_calc:
 *
 *		calculate array representing the linear equation for each 
 *		edge
 */
edge_eq_calc()
{

	int	i;

	for (i=0; i<poly.num_edge;i++) {
		edges[i].slope = (float) (poly.P[i+1].x - poly.P[i].x) 
				/ (float) (poly.P[i+1].y - poly.P[i].y);

		edges[i].C = poly.P[i].y 
				- (edges[i].slope * poly.P[i].x);
	}
}


float	extreme(near_i, far_i)
	int	*near_i, 
		*far_i;		/* index into edge_eg of nearest and furthest
				 * edge
				 */	
{

	float	dist;
	float	near_dist = 0.0;
	float	far_dist = 0.0;
	float	m = hatch_origin_eq.slope;
	float	base = sqrt ((double) ((m*m) + 1));

	int	i;


	*near_i = *far_i = 0.0;

	for (i=0;i<=poly.num_edge;i++) {
		if ((dist = ((m*poly.P[i].x)+poly.P[i].y) / base) < near_dist) {
			*near_i = i;
			near_dist = dist;
		}
		else {
			if (dist > far_dist) {
				*far_i = i;
				far_dist = dist;
			}
		}
	}

	return(far_dist - near_dist);
}
			

intersect(hatchline, i, point)
	eq	hatchline;
	int	i;
	XPoint	*point;
{
	XPoint	P;	/* intersection point of hatchline and line edges[i] */

	/* check to see if lines are parallel	*/
	if (hatchline.slope == edges[i].slope)
		return(0);

	P.x = (int) ((edges[i].C - hatchline.C) 
			/ (hatchline.slope - edges[i].slope)); 

	P.y = (hatchline.slope * P.x) + hatchline.C;

	
	if (poly.P[i].x > poly.P[i+1].x) {
		if ((P.x <= poly.P[i].x) && (P.x >= poly.P[i+1].x)) {
			point->x = P.x;
			point->y = P.y;
			return(1);
		}
	}
	else {
		if ((P.x >= poly.P[i].x) && (P.x <= poly.P[i+1].x)) {
			point->x = P.x;
			point->y = P.y;
			return(1);
		}
	}

	return(0);
}


draw (hitlist, hit, start_eq)
	XPoint	hitlist[];
	int	hit;
	eq	start_eq;
{
	int	sort[25];

	distlist	dt[10];

	int	x,i;


	x = (int) -start_eq.C / start_eq.slope;

	for (i=0;i<hit;i++)  {
		dt[i].index = i;
		dt[i].dist = sqrt((double) 
				(SQR(hitlist->y)+SQR(hitlist->x - x)));
	}

	d_sort(dt, hit);

	
	hit = hit/2;
	for (i=0;i<hit;i+=2) {
		XDrawPoint(dpy,win,polygonGC,hitlist[i].x, hitlist[i].y, 
			hitlist[i+1].x, hitlist[i+1].y);
	}
}

d_sort(dt, hit)
	distlist	dt[];
	int	hit;
{

	int	gap, i, j;
	float	temp;

	for(gap = hit/2; gap > 0; gap/= 2)
		for(i = gap; i<hit; i++)
			for (j=i-gap; j>=0 && dt[dt[j].index].dist >
					dt[dt[j+gap].index].dist; j -= gap) {

				temp = dt[j].index;
				dt[j].index = dt[j+gap].index;
				dt[j+gap].index = temp;
			}
}
	





hatch_sim(c, angle)
	CGMC	*c;
	float	angle;
{


	int	num_hatch;

	int	near_i,	
		far_i;	/* index into poly of nearest and furthest point to
			 * hatch line at origin
			 */

	XPoint	hitlist[25];	/* list of indexes of edges intersected
				 *  by a hatch line
				 */
	int	hit;
	int	i,j;

	float	space;

	eq	start_eq;	/* equation for first hatch line drawn	*/

	float	cosine,
		sine;		/* sin and cosin of hatch angle		*/

	/* convert from degrees to radians	*/
	angle = DEG2RAD(angle);

	cosine = (float) cos ((double) angle);
	sine = (float) sin ((double) angle);

	/*
	 *	calculate equation for hatch line through the  origin
	 */
	hatch_origin_eq.slope = sine/cosine;
	hatch_origin_eq.C = 0;

	/* directed spacing between hatch lines	*/
	space = SPACEING / sin((double) (M_PI_2) - angle);


	/*
	 *	break complex polygons into simple ones one at a time
	 */
	while(simple(c,  &poly)) {
		
		/*
		 *	calculate equations for line makeing up each edge
		 *	in a simple polygon
		 */
		edge_eq_calc(poly, edges); 

		/*
		 *	determine the number of hatch lines to be drawn as
		 *	well as an index into poly[] of nearest and furtherest
		 *	points in poly to hatch_origin
		 */
		num_hatch = extreme(&near_i, &far_i) / SPACEING;

		/* 
		 *	calculate equation for first hatch line. This line
		 * 	is determined by the "nearest" point to hatch_origin
		 */
		start_eq.slope = hatch_origin_eq.slope;
		start_eq.C = poly.P[near_i].y - 
			(start_eq.slope * poly.P[near_i].x); 

		/*
		 *	draw hatch lines where necessary
		 */
		for (i=0;i < num_hatch; i++) {

			hit = 0;
			for (j=0; j<poly.num_edge; j++) {

				/* see where a hatch line intercects an edge */
				if (intersect(start_eq, j, &hitlist[hit])) { 
					hit++;
				}
			}

			/* 	draw portions of hatch lines that are bounded
			 *	by an appropriate polygon edge
			 */
			draw(hitlist,hit, start_eq);
		}
	}
}






		
