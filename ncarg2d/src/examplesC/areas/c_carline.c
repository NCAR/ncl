/*
 * $Id: c_carline.c,v 1.1 1994-07-15 21:36:12 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define LMAP     150000
#define NGEO     43
#define NPTS     50
#define NGRPS     2
#define NWRK     1000

float xgeo[NGEO] = {.63, .12, .05, .07, .10, .04, .19, .31, .31, .41,
.39, .47, .64, .63, .70, .66, .67, .69, .76, .92, .95, .69, .64, .53,
.53, .60, .63, .63, .72, .74, .79, .75, .75, .80, .75, .70, .68, .64,
.63, .55, .55, .63, .63};

float ygeo[NGEO] = {.94, .95, .92, .85, .83, .78, .84, .75, .69, .58,
.64, .55, .47, .37, .30, .05, .03, .05, .13, .26, .38, .52, .50, .57,
.63, .63, .59, .64, .72, .71, .75, .75, .77, .78, .85, .83, .86, .86,
.77, .80, .86, .90, .94}; 

float x[12] = {.10, .22, .25, .25, .25, .50, .30, .47, .50, .77, .75,
.68};

float y[12] = {.98, .70, .55, .38, .18, .18, .90, .85, .70, .35, .18,
.05};

main()
{

	int map[LMAP], iarea[NGRPS], igrp[NGRPS];
	float xcntr[NPTS], ycntr1[NPTS], ycntr2[NPTS], ycntr3[NPTS];
	float ycntr4[NPTS], ycntr5[NPTS];
	float xwrk[NWRK], ywrk[NWRK], dist;
	char string[20];
	int i, j, nai;
	extern void color();
	extern int mask();
/*     
 * Generate Contour lines
 */
	for( j = 0; j < NPTS; j++ ) {
		dist = (float)j/(float)(NPTS-1);
		xcntr [j] = dist;
		ycntr1[j] = .1*cos(4.*3.14*dist)+.15;
		ycntr2[j] = .1*cos(4.*3.14*dist)+.30;
		ycntr3[j] = .1*cos(4.*3.14*dist)+.50;
		ycntr4[j] = .1*cos(4.*3.14*dist)+.70;
		ycntr5[j] = .1*cos(4.*3.14*dist)+.85;
	}
/*
 *  Open gks, open and activate a workstation.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);
/*
 * Set up color table and set Dash pattern to solid
 */
	color ();
	i = 65535;
	c_dashdb(&i);
/*
 * Outline continents in red
 */
	gset_line_colr_ind (1);
	c_curve (xgeo,ygeo,NGEO);
/*
 * Initialize Areas
 */
	c_arinam (map,LMAP);
/*
 * Add continents to area map in group 1.
 */
	c_aredam (map, xgeo, ygeo, NGEO, 1, 2, 1);
/*
 * Add contours to area map in group 3.
 */
	c_aredam (map, xcntr, ycntr1, NPTS, 3, 2, 1);
	c_aredam (map, xcntr, ycntr2, NPTS, 3, 3, 2);
	c_aredam (map, xcntr, ycntr3, NPTS, 3, 4, 3);
	c_aredam (map, xcntr, ycntr4, NPTS, 3, 5, 4);
	c_aredam (map, xcntr, ycntr5, NPTS, 3, 6, 5);
/*
 * Write out area and group identifiers for each area, using red for
 * geographic identifiers, and green for contour identifiers.
 */
	for( i = 0; i < 12; i++ ) {
		c_argtai(map, x[i], y[i], iarea, igrp, NGRPS, &nai, 1);
		for( j = 0; j < 2; j++ ) {
            sprintf( string, "A(%d)=%d G(%d)=%d", j+1,iarea[j],j+1,igrp[j]);
            if (igrp[j] == 1) {
				gset_line_colr_ind(1);
				c_plchhq (x[i], y[i], string, .01, 0., 0.);
			}
            if (igrp[j] == 3) {
				gset_line_colr_ind(6);
				c_plchhq (x[i], y[i]-.018, string, .01, 0., 0.);
			}
		}
	}
/*
 * Draw contour lines only over water
 */
	gset_line_colr_ind(7);
	c_ardrln(map,xcntr,ycntr1,NPTS,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,mask);
	c_ardrln(map,xcntr,ycntr2,NPTS,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,mask);
	gset_linewidth(2.);
	gset_line_colr_ind(2);
	c_ardrln(map,xcntr,ycntr3,NPTS,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,mask);
	gset_linewidth(1.);
	gset_line_colr_ind(7);
	c_ardrln(map,xcntr,ycntr4,NPTS,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,mask);
	c_ardrln(map,xcntr,ycntr5,NPTS,xwrk,ywrk,NWRK,iarea,igrp,NGRPS,mask);
      
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
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
 *
 * Background is white to show up on both paper and terminal
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&rgb);
/*
 * Red
 */
	rgb.rgb.red = .7; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.; rgb.rgb.green = .7; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
/*
 * Yellow
 */
	rgb.rgb.red = .7; rgb.rgb.green = .7; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,3,&rgb);
/*
 * Blue
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = .7;
	gset_colr_rep(WKID,4,&rgb);
/*
 * Magenta
 */
	rgb.rgb.red = .7; rgb.rgb.green = 0.; rgb.rgb.blue = .7;
	gset_colr_rep(WKID,5,&rgb);
/*
 * Aqua
 */
	rgb.rgb.red = 0.; rgb.rgb.green = .7; rgb.rgb.blue = .7;
	gset_colr_rep(WKID,6,&rgb);
/*
 * Black
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,7,&rgb);

	return;
}
      
#ifdef __STDC__
int mask(
    float *xc,
    float *yc,
    int *mcs,
    int *iarea,
    int *igrp,
    int *nai
)
#else
int mask(xc,yc,mcs,iarea,igrp,nai)
    float *xc;
    float *yc;
    int *mcs;
    int *iarea;
    int *igrp;
    int *nai;
#endif
{
	int i, idgeo;
	idgeo = -1;
/*
 * Retrieve geographic area identifier
 */
	for( i = 0; i < *nai; i++ ) {
		if (igrp[i] == 1) idgeo=iarea[i];
	}
/*
 * If the contour segment is over water, then draw it
 */
	if (idgeo == 1) {
		c_curved(xc,yc,*mcs);
	}
/*
 * Otherwise, don't draw the line - mask it.
 */
	return(0);
}
