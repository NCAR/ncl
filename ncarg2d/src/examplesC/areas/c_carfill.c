/*
 * $Id: c_carfill.c,v 1.1 1994-07-15 21:36:12 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define  LMAP   150000
#define  NMAP   43
#define  NPTS   50
#define  NGRPS  2
#define  NWRK   1000

#define WSTYPE SED_WSTYPE
#define WKID   1

float xgeo[NMAP] = {.63, .12, .05, .07, .10, .04, .19, .31, .31, .41,
.39, .47, .64, .63, .70, .66, .67, .69, .76, .92, .95, .69, .64, .53,
.53, .60, .63, .63, .72, .74, .79, .75, .75, .80, .75, .70, .68, .64,
.63, .55, .55, .63, .63};

float ygeo[NMAP] = {.94, .95, .92, .85, .83, .78, .84, .75, .69, .58,
.64, .55, .47, .37, .30, .05, .03, .05, .13, .26, .38, .52, .50, .57,
.63, .63, .59, .64, .72, .71, .75, .75, .77, .78, .85, .83, .86, .86,
.77, .80, .86, .90, .94};

float x[12] = {.10, .22, .25, .25, .25, .50, .30, .47, .50, .77, .75,
.68};

float y[12] = {.98, .70, .55, .38, .18, .18, .90, .85, .70, .35, .18,
.05};

float xperim[5] = {0.0, 1.0, 1.0, 0.0, 0.0};
float yperim[5] = {0.0, 0.0, 1.0, 1.0, 0.0};

main()
{
	int i, j, map[LMAP], iarea[NGRPS], igrp[NGRPS], nai, ispace;
	float dist;
	float xcntr[NPTS], ycntr1[NPTS], ycntr2[NPTS], ycntr3[NPTS];
	float ycntr4[NPTS], ycntr5[NPTS];
	float xwrk[NWRK], ywrk[NWRK];
	char string[20];
	extern void color();
	extern int fill();
      
	xcntr[1] = 0.0;
	ycntr1[1] = 0.25;
	ycntr2[1] = 0.40;
	ycntr3[1] = 0.60;
	ycntr4[1] = 0.80;
	ycntr5[1] = 0.95;
	for( j = 1; j < NPTS; j++ ) {
		dist = (float)(j+1)/(float)NPTS;
		xcntr[j] = dist;
		ycntr1[j] = .1*cos((float)(4*3.14*dist))+.15;
		ycntr2[j] = .1*cos((float)(4*3.14*dist))+.30;
		ycntr3[j] = .1*cos((float)(4*3.14*dist))+.50;
		ycntr4[j] = .1*cos((float)(4*3.14*dist))+.70;
		ycntr5[j] = .1*cos((float)(4*3.14*dist))+.85;
	}
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
 * set the fill area option to solid
 */
	gset_fill_int_style(GSTYLE_SOLID);
/*
 * Initialize areas
 */
	c_arinam (map,LMAP);
/*
 * Add continents to area map in group 1.
 */
	c_aredam (map, xgeo, ygeo, NMAP, 1, 2, 1);
/*
 * Add contours and perimeter to area map in group 3.
 */
	c_aredam (map, xcntr, ycntr1, NPTS, 3, 2, 1);
	c_aredam (map, xcntr, ycntr2, NPTS, 3, 3, 2);
	c_aredam (map, xcntr, ycntr3, NPTS, 3, 4, 3);
	c_aredam (map, xcntr, ycntr4, NPTS, 3, 5, 4);
	c_aredam (map, xcntr, ycntr5, NPTS, 3, 6, 5);
/*
 * Fill water blue and contour lines according to area id.
 */
	c_arscam(map,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,fill);
/*
 * Write out area and group identifiers for each area, using red for
 * geographic identifiers, and green for contour identifiers.
 */
	c_sflush();
	gset_line_colr_ind(0);
	for( i = 0; i < 12; i++ ) {
		c_argtai(map, x[i], y[i], iarea, igrp, NGRPS, &nai, 0);
		for( j = 0; j < 2; j++ ) {
            if ((i <= 5) || (i == 11)) gset_line_colr_ind(3);
            if (i == 6) gset_line_colr_ind(0);
            if (i == 7) gset_line_colr_ind(3);
            if (i == 8) gset_line_colr_ind(6);
            if (i == 9) gset_line_colr_ind(0);
            if (i == 10) gset_line_colr_ind(3);
            sprintf( string, "A(%d)=%d G(%d)=%d", j+1,iarea[j],j+1,igrp[j]);
            if (igrp[j] == 1) {
				c_plchhq (x[i], y[i], string, .01, 0., 0.);
			}
            if (igrp[j] == 3) {
				c_plchhq (x[i], y[i]-.02, string, .01, 0., 0.);
			}
		}
	}
/*
 * Draw contours and perimeter in white
 */
	gset_line_colr_ind (6);
	c_curve (xcntr,ycntr1,NPTS);
	c_curve (xcntr,ycntr2,NPTS);
	c_curve (xcntr,ycntr3,NPTS);
	c_curve (xcntr,ycntr4,NPTS);
	c_curve (xcntr,ycntr5,NPTS);
	c_curve (xperim,yperim,5);
      
	c_frame();
/*
 * Deactivate and close workstation, close gks.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
/*
 * Write out amount of space needed for the area map
 */
	ispace = map[0] - map[5] + map[4];
	printf( "area map space used: %d\n",ispace );
}
      
void color()
{
	Gcolr_rep rgb;
/*
 * Define color table
 *
 * Background is black
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,0,&rgb);
/*
 * Red
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
/*
 * Yellow
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,3,&rgb);
/*
 * Blue
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,4,&rgb);
/*
 * Magenta
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,5,&rgb);
/*
 * White
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,6,&rgb);
/*
 * Aqua
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,7,&rgb);

	return;
}

int fill(xwrk,ywrk,nwrk,iarea,igrp,ngrps)
float *xwrk, *ywrk;
int *nwrk, *iarea, *igrp, *ngrps;
{
	int i, idgeo, idcntr;
	Gpoint_list fill_area;

	idgeo = -1;
	idcntr = -1;
/*
 * Retrieve geographic and contour area identifier info
 */
	  for( i = 0; i < *ngrps; i++ ) {
		  if (igrp[i] == 1) idgeo=iarea[i];
		  if (igrp[i] == 3) idcntr=iarea[i];
	  }
/*
 * If the contour segment is over water, then fill it in aquamarine
 */
	if (idgeo  ==  1 || ((idgeo  ==  2) && (idcntr >= 1))) {
		fill_area.num_points = *nwrk;
		fill_area.points = (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));
		if( !fill_area.points ) {
			fprintf( stderr, "fill: Not enough memory to create fill area structure\n" );
			gemergency_close_gks();
			exit(1);
		}
		for( i = 0; i < *nwrk; i++ ) {
			fill_area.points[i].x = xwrk[i];
			fill_area.points[i].y = ywrk[i];
		}
	}
	if (idgeo  ==  1 ) {
		gset_fill_colr_ind(7);
		gfill_area(&fill_area);
		free(fill_area.points);
	}
/*
 * If the contour band is over land, then fill it according to it's 
 * area identifier info.
 */
	else if ((idgeo  ==  2) && (idcntr >= 1)) {
		gset_fill_colr_ind(idcntr);
		gfill_area(&fill_area);
		free(fill_area.points);
	}
	return(0);
}
