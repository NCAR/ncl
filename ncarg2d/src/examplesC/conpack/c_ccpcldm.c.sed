/*
 * $Id: c_ccpcldm.c.sed,v 1.3 1994-06-21 14:59:13 haley Exp $
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

main()
{
	float xreg[MREG],yreg[NREG],zreg[NREG][MREG];
	extern void ccpldm();
	extern void getdat();
/*
 * Get data array
 */
	getdat(xreg,yreg,zreg);
/*
 * Open GKS
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Call Conpack color fill routine
 */
	ccpldm(zreg);
/*
 * Close frame and close GKS
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void ccpldm(zreg)
float zreg[NREG][MREG];
{
	int iwrk[LIWK], ncl;
	float rwrk[LRWK], xwrk[NWRK], ywrk[NWRK];
	int map[LMAP],iarea[NOGRPS],igrp[NOGRPS];

	extern void color();
	extern int fill();
	extern int cpdrpl_();
/*
 * Set fill style to solid
 */
	gset_fill_int_style(GSTYLE_SOLID);
/*
 * Use regular or penalty labeling scheme so that contour labels can be
 * boxed.
 */
	c_cpseti("LLP - LINE LABEL POSITIONING FLAG",2);
/*
 * Initialize Conpack
 */
	c_cprect((float *)zreg, MREG, MREG, NREG, rwrk, LRWK, iwrk, LIWK);
/*
 * Set up color table
 */
	c_cppkcl ((float *)zreg, rwrk, iwrk);
	c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&ncl);
	color(ncl+1);
/*
 * Draw Perimeter
 */
	c_cpback((float *)zreg, rwrk, iwrk);
/*
 * Initialize Areas
 */
	c_arinam(map, LMAP);
/*
 * Add label boxes to area map
 */
	c_cplbam((float *)zreg, rwrk, iwrk, map);
/*
 * Draw Labels
 */
	c_cplbdr((float *)zreg, rwrk, iwrk);
/*
 * Add contours to area map
 */
	c_cpclam((float *)zreg, rwrk, iwrk, map);
/*
 * Fill contours
 */
	c_arscam(map, xwrk, ywrk, NWRK, iarea, igrp, NOGRPS, fill);
/*
 * Draw contours, masking label boxes
 */
	c_cpcldm((float *)zreg, rwrk, iwrk, map, cpdrpl_);
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
		gset_fill_colr_ind(iarea3+2);
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

void color(n)
int n;
{
	int i,icnt, lap;
	float xhue, hues,redln, red, green, blue;
	Gcolr_rep rgb;
/*
 * BACKGROUND COLOR
 * BLACK
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,0,&rgb);
/*
 * First foreground color is white
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,1,&rgb);
/*
 * Second foreground color is gray
 */
	rgb.rgb.red = .75; rgb.rgb.green = .75; rgb.rgb.blue = .75;
	gset_colr_rep(WKID,2,&rgb);
/*
 * Choose other foreground colors spaced equally around the spectrum
 */
	icnt=0;
	hues=360./n;
/*
 * REDLN is intended to be the line between red and violet values
 */
	redln=36.0;
	lap=(int)(redln/hues);
	for( i = 1; i <= n; i++ ) {
		xhue=i*hues;
		c_hlsrgb(xhue,60.,75.,&rgb.rgb.red,&rgb.rgb.green,&rgb.rgb.blue);
/*
 * Sort colors so that the redest is first, and violetest is last
 */
		if (xhue <= redln) {
            gset_colr_rep(WKID,(n+2)-(lap-i),&rgb);
            icnt=icnt+1;
		}
		else {
            gset_colr_rep(WKID,i-icnt+2,&rgb);
		}
	}
	return;
}

