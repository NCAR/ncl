/*
 * $Id: c_ccpfil.c,v 1.2 1994-06-21 14:59:20 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define MREG  50
#define NREG  50
#define LRWK  5000
#define LIWK  5000
#define LMAP  50000
#define NWRK  5000
#define NOGRPS  5
#define NRAN  30

#define WSTYPE SED_WSTYPE
#define WKID   1

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	float xreg[MREG],yreg[NREG],zreg[NREG][MREG];
	extern void getdat();
	extern void ccpfil();
/*
 * Get data array
 */
	getdat(xreg,yreg,zreg);
/*
 * Open GKS
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Call Conpack color fill routine
 */
	ccpfil(zreg,-15);
/*
 * Close frame and close GKS
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void ccpfil(zreg,ncl)
float zreg[NREG][MREG];
int ncl;
{
	int iwrk[LIWK];
	float rwrk[LRWK], xwrk[NWRK], ywrk[NWRK];
	int map[LMAP],iarea[NOGRPS],igrp[NOGRPS];
	extern void color();
	extern int fill();
/*
 * Set up color table
 */
	color();
/*
 * Initialize Areas
 */
	c_arinam(map, LMAP);
/*
 * Set number of contour levels and initialize Conpack
 */
	c_cpseti("CLS - CONTOUR LEVEL SELECTION FLAG",ncl);
	c_cprect((float *)zreg, MREG, MREG, NREG, rwrk, LRWK, iwrk, LIWK);
/*
 * Add contours to area map
 */
	c_cpclam((float *)zreg, rwrk, iwrk, map);
/*
 * Set fill style to solid, and fill contours
 */
	gset_fill_int_style(GSTYLE_SOLID);
	c_arscam(map, xwrk, ywrk, NWRK, iarea, igrp, NOGRPS, fill);
/*
 * Draw Perimeter
 */
	c_cpback((float *)zreg, rwrk, iwrk);
/*
 * Draw Labels
 */
	c_cplbdr((float *)zreg,rwrk,iwrk);
/*
 * Draw Contours
 */
	c_cpcldr((float *)zreg,rwrk,iwrk);

	return;
}

int fill(
    float *xwrk,
    float *ywrk,
    int *nwrk,
    int *iarea,
    int *igrp,
    int *ngrps
)
{
	Gpoint_list area;
	int i, iarea3;

	for( i=0; i < *ngrps; i++ ) {
		if (igrp[i] == 3) iarea3=iarea[i];
	}
/*
 * If the area is defined by 3 or more points, fill it
 */
	if (iarea3 > 0) {
/*
 * set up struct for fill area
 */
        area.num_points = *nwrk-1;
        area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
        if( !area.points ) {
            fprintf( stderr, "fill:  Not enough memory to create fill area array\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *nwrk-1; i++ ) {
            area.points[i].x = xwrk[i];
            area.points[i].y = ywrk[i];
        }
		gset_fill_colr_ind(iarea3+1);
        gfill_area(&area);
        free(area.points);
	}
/*
 * Otherwise, do nothing
 */
	return(0);
}

float xran[NRAN] = {12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
                    19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
                    18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19.};
float yran[NRAN] = { 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
                     1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
                    29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67.};
float zran[NRAN] = {1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
                    1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
                    1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0};

void getdat(xreg,yreg,zreg)
float *xreg, *yreg, zreg[NREG][MREG];
{
	float rwrk[LRWK];
	int i, iwrk[LIWK];
	float xmin, xmax, ymin, ymax;
/*
 *  Set the min and max data values.
 */
	xmin = 0.0;
	xmax = 65.0;
	ymin =  0.0;
	ymax = 68.0;
/*
 * Choose the X and Y coordinates for interpolation points on the 
 * regular grid.
 */
	for( i = 0; i < MREG; i++ ) {
        xreg[i] = xmin + (xmax - xmin)* (float)i/MREG;
	}
	for( i = 0; i < NREG; i++ ) {
        yreg[i] = ymin + (ymax - ymin)* (float)i/NREG;
	}
/*
 * Interpolate data onto a regular grid
 */
	c_idsfft (1,NRAN,xran,yran,zran,MREG,NREG,MREG,xreg,yreg,(float *)zreg,iwrk,rwrk);
	return;
}

void color()
{
	Gcolr_rep rgb[18];
/*
 *     BACKGROUND COLOR
 *     BLACK
 */
	rgb[0].rgb.red = 0.; rgb[0].rgb.green = 0.; rgb[0].rgb.blue = 0.;
/*
 *     FORGROUND COLORS
 * White
 */
	rgb[1].rgb.red = 1.0; rgb[1].rgb.green =  1.0; rgb[1].rgb.blue =  1.0;
/*
 * Orchid 
 */
	rgb[2].rgb.red = 0.85; rgb[2].rgb.green =  0.45; rgb[2].rgb.blue =  0.8;
/*
 * Red
 */
	rgb[3].rgb.red = 0.9; rgb[3].rgb.green =  0.25; rgb[3].rgb.blue =  0.0;
/*
 * OrangeRed
 */
	rgb[4].rgb.red = 1.0; rgb[4].rgb.green =  0.0; rgb[4].rgb.blue =  0.2;
/*
 * Orange
 */
	rgb[5].rgb.red = 1.0; rgb[5].rgb.green =  0.65; rgb[5].rgb.blue =  0.0;
/*
 * Gold
 */
	rgb[6].rgb.red = 1.0; rgb[6].rgb.green =  0.85; rgb[6].rgb.blue =  0.0;
/*
 * Yellow
 */
	rgb[7].rgb.red = 1.0; rgb[7].rgb.green =  1.0; rgb[7].rgb.blue =  0.0;
/*
 * GreenYellow
 */
	rgb[8].rgb.red = 0.7; rgb[8].rgb.green =  1.0; rgb[8].rgb.blue =  0.2;
/*
 * Chartreuse
 */
	rgb[9].rgb.red = 0.5; rgb[9].rgb.green =  1.0; rgb[9].rgb.blue =  0.0;
/*
 * Celeste
 */
	rgb[10].rgb.red = 0.2; rgb[10].rgb.green =  1.0; rgb[10].rgb.blue =  0.5;
/*
 * Green
 */
	rgb[11].rgb.red = 0.2; rgb[11].rgb.green =  0.8; rgb[11].rgb.blue =  0.2;
/*
 * Aqua
 */
	rgb[12].rgb.red = 0.0; rgb[12].rgb.green =  0.9; rgb[12].rgb.blue =  1.0;
/*
 * DeepSkyBlue
 */
	rgb[13].rgb.red = 0.0; rgb[13].rgb.green =  0.75; rgb[13].rgb.blue =  1.0;
/*
 * RoyalBlue
 */
	rgb[14].rgb.red = 0.25; rgb[14].rgb.green =  0.45; rgb[14].rgb.blue =  0.95;
/*
 * SlateBlue
 */
	rgb[15].rgb.red = 0.4; rgb[15].rgb.green =  0.35; rgb[15].rgb.blue =  0.8;
/*
 * DarkViolet
 */
	rgb[16].rgb.red = 0.6; rgb[16].rgb.green =  0.0; rgb[16].rgb.blue =  0.8;
/*
 * Lavender
 */
	rgb[17].rgb.red = 0.8; rgb[17].rgb.green =  0.8; rgb[17].rgb.blue =  1.0;
	for( i = 0; i <= 17; i++ ) {
		gset_colr_rep(WKID, i, &rgb[i]);
	}
	return;
}
