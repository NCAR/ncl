/*
 * $Id: c_cardb1.c,v 1.1 1994-07-15 21:36:11 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define NPTS   101
#define MAPSIZ 5000
#define IDSIZE 1
#define MCS    1000

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float x, y, angle, d2r;
	float x1[NPTS], y1[NPTS], x2[NPTS], y2[NPTS], x3[11], y3[11];
	float xc[MCS], yc[MCS];
	int i, map[MAPSIZ], nvert;
	int areaid[IDSIZE], grpid[IDSIZE];
      
	extern int fill();
	extern void color(), star();
      
	d2r =.017453292519943;
/*
 * Draw circles of radius .9 and .85 centered on the origin
 */
	for( i = 0; i < NPTS; i++ ) {
		angle = d2r*3.6*(float)i;
		x = cos(angle);
		y = sin(angle);
		x1[i]= 0.90*x;
		y1[i]= 0.90*y;
		x2[i]= 0.85*x;
		y2[i]= 0.85*y;
	}
/*
 * Get data to draw a pentagram inside inner circle.
 */
	nvert = 6;
	star(x3,y3,nvert,d2r);
/*
 *  Open gks, open and activate a workstation.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Define color table
 */
	color();
/*
 * Define window from -1. to 1.
 */
	c_set(0.,1.,0.,1.,-1.,1.,-1.,1.,1);
	gset_char_ht (.2);
/* 
 * initialize areas
 */
	c_arinam(map, MAPSIZ);
	c_arseti("db - debug plots",1);
/*
 * Add edges to area map
 */
	c_aredam(map, x1, y1, NPTS, 1, 1, 0);
	c_aredam(map, x2, y2, NPTS, 1, 2, 1);
	c_aredam(map, x3, y3, nvert, 1, 3, 2);
/*
 * Fill regions according to instructions
 */
	c_arscam(map, xc, yc, MCS, areaid, grpid, IDSIZE, fill);
/*
 * Advance frame
 */
	c_frame();
/*
 * Deactivate and close workstation, close gks.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void color()
{
	Gcolr_rep rgb;
/*
 * Define color table
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,3,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,4,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,5,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,6,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,7,&rgb);
					
	return;
}
      
int fill (xc, yc, pts, areaid, grpid, idsize)
float *xc, *yc;
int *pts, *areaid, *grpid, *idsize;
{	 
	Gpoint_list fill_area;
	int i;
/*
 * Fill area map
 *
 * In this case, we have only one group, so we know that
 * areaid(IDSIZE) is a unique area identifier.
 *
 * If the area is the ring between circles solid fill red
 */
	fill_area.num_points = *pts;
	fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
	if( !fill_area.points ) {
		fprintf( stderr, "fill: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
	for( i = 0; i < *pts; i++ ) {
		fill_area.points[i].x = xc[i];
		fill_area.points[i].y = yc[i];
	}
	if (areaid[*idsize-1] == 1) {
		gset_fill_int_style(GSTYLE_SOLID);
		gset_fill_colr_ind(1);
		gfill_area (&fill_area);
		free(fill_area.points);
	}
/*
 * If the area is between the ring and the star, hatch fill in yellow
 */
	else if (areaid[*idsize-1] == 2) {
		gset_fill_int_style(GSTYLE_HATCH);
		gset_fill_style_ind(6);
		gset_fill_colr_ind(3);
		gfill_area (&fill_area);
		free(fill_area.points);
	}
/*
 * If the area is inside the star, solid fill in aqua
 */
	else if (areaid[*idsize-1] == 3) {
		gset_fill_int_style(GSTYLE_SOLID);
		gset_fill_colr_ind(6);
		gfill_area (&fill_area);
		free(fill_area.points);
	}

	return(0);
}
      
void star (x3,y3,nvert,d2r)
float *x3, *y3, d2r;
int nvert;
{      
	float dist;
      
	dist = (1.0 - 0.835*cos(d2r*36.));
      
	x3[0] = 0.85*cos(d2r* 18.);
	y3[0] = 0.85*sin(d2r* 18.);
	x3[3] = 0.00;
	y3[3] = 0.85;
	x3[1] = 0.85*cos(d2r*162.);
	y3[1] = 0.85*sin(d2r*162.);
	x3[4] = 0.85*cos(d2r*234.);
	y3[4] = 0.85*sin(d2r*234.);
	x3[2] = 0.85*cos(d2r*306.);
	y3[2] = 0.85*sin(d2r*306.);
	x3[5] = x3[0];
	y3[5] = y3[0];

	return;
}
