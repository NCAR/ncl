/*
 *	$Id: c_fsfsgfa.c,v 1.1 1994-07-27 19:10:04 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/* 
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define MREG  50
#define NREG 50
#define LRWK  5000
#define LIWK  5000
#define LMAP  50000
#define NWRK  5000
#define NOGRPS  5
#define NRAN  30

float zreg[NREG][MREG];

main()
{
	float xreg[MREG],yreg[NREG];
	extern void color(), csfill(), getdat();
/*
 * Get data array
 */
	getdat(xreg,yreg,zreg);
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Call Conpack color fill routine
 */
	csfill(-15);
/*
 * Close frame and close GKS
 */
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void csfill(ncl)
int ncl;
{
    float rwrk[LRWK], xwrk[NWRK], ywrk[NWRK];
    int iwrk[LIWK];
    int map[LMAP],iarea[NOGRPS],igrp[NOGRPS];
    extern int sfill();
	extern void color();
/*
 * Set up color table
 */
    color();
/*
 * First draw B&W plot to left
 */
    c_cpsetr("VPL - VIEWPORT LEFT",0.);
    c_cpsetr("VPR - VIEWPORT RIGHT",.49);
/*
 * Set number of contour levels and initialize Conpack
 */
    c_cpseti("CLS - CONTOUR LEVEL SELECTION FLAG",ncl);
    c_cprect((float *)zreg, MREG, MREG, NREG, rwrk, LRWK, iwrk, LIWK);
/*
 * Set up B&W fill options
 */
    c_sfseti("TY - TYPE OF FILL",-4);
/*
 * Draw Perimeter
 */
    c_cpback((float *)zreg, rwrk, iwrk);
/*
 * Initialize Areas
 */
    c_arinam(map, LMAP);
/*
 * Add contours to area map
 */
    c_cpclam((float *)zreg, rwrk, iwrk, map);
/*
 * Fill contours
 */
    c_arscam(map, xwrk, ywrk, NWRK, iarea, igrp, NOGRPS, sfill);
/*
 * Draw contours, masking label boxes
 */
    gset_line_colr_ind(0);
    c_cpcldr((float *)zreg, rwrk, iwrk);
    gset_line_colr_ind(1);
/*
 * Second draw color plot to left
 */
    c_cpsetr("VPL - VIEWPORT LEFT",.51);
    c_cpsetr("VPR - VIEWPORT RIGHT",1.);
    c_cprect((float *)zreg, MREG, MREG, NREG, rwrk, LRWK, iwrk, LIWK);
/*
 * Set up color fill options
 */
    gset_fill_int_style(GSTYLE_SOLID);
    c_sfseti("TY - TYPE OF FILL",0);
/*
 * Draw Perimeter
 */
    c_cpback((float *)zreg, rwrk, iwrk);
/*
 * Initialize Areas
 */
    c_arinam(map, LMAP);
/*
 * Add contours to area map
 */
    c_cpclam((float *)zreg, rwrk, iwrk, map);
/*
 * Fill contours
 */
    c_arscam(map, xwrk, ywrk, NWRK, iarea, igrp, NOGRPS, sfill);
/*     
 * Draw contours, masking label boxes
 */
    gset_line_colr_ind(0);
    c_cpcldr((float *)zreg, rwrk, iwrk);
    return;
}
      
int sfill (xwrk,ywrk,nwrk,iarea,igrp,ngrps)
float *xwrk, *ywrk;
int *nwrk, *iarea, *igrp, *ngrps;
{
    int i, iscr[5000], iarea3;
    float rscr[5000];

    for( i = 0; i < *ngrps; i++ ) {
        if (igrp[i] == 3) iarea3=iarea[i];
    }

    if (iarea3 >= 1) {
/*
 * If the area is defined by 3 or more points, fill it
 */
        c_sfsgfa(xwrk,ywrk,*nwrk,rscr,5000,iscr,5000,iarea3+1);
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
	Gcolr_rep rgb;
/*
 * BACKGROUND COLOR
 * BLACK
 */
	rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,0,&rgb);
/*
 * FORGROUND COLORS
 * White
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID,  1, &rgb );

	rgb.rgb.red = 0.85; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.4;
	gset_colr_rep(WKID,  2, &rgb );
/*
 * Red
 */
	rgb.rgb.red = 0.9; rgb.rgb.green =  0.25; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  3, &rgb );
/*
 * OrangeRed
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID,  4, &rgb );
/*
 * Orange
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.65; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  5, &rgb );
/*
 * Gold
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.85; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  6, &rgb );
/*
 * Yellow
 */
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  7, &rgb );
/*
 * GreenYellow
 */
	rgb.rgb.red = 0.7; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID,  8, &rgb );
/*
 * Chartreuse
 */
	rgb.rgb.red = 0.5; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID,  9, &rgb );
/*
 * Green
 */
	rgb.rgb.red = 0.2; rgb.rgb.green =  0.8; rgb.rgb.blue =  0.2;
	gset_colr_rep(WKID, 10, &rgb );
/*
 * Celeste
 */
	rgb.rgb.red = 0.2; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.7;
	gset_colr_rep(WKID, 11, &rgb );
/*
 * Aqua
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.9; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID, 12, &rgb );
/*
 * DeepSkyBlue
 */
	rgb.rgb.red = 0.0; rgb.rgb.green =  0.75; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID, 13, &rgb );
/*
 * RoyalBlue
 */
	rgb.rgb.red = 0.25; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.95;
	gset_colr_rep(WKID, 14, &rgb );
/*
 * SlateBlue
 */
	rgb.rgb.red = 0.4; rgb.rgb.green =  0.35; rgb.rgb.blue =  0.8;
	gset_colr_rep(WKID, 15, &rgb );
/*
 * DarkViolet
 */
	rgb.rgb.red = 0.6; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.8;
	gset_colr_rep(WKID, 16, &rgb );
/*
 * Lavender
 */
	rgb.rgb.red = 0.8; rgb.rgb.green =  0.8; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID, 17, &rgb );
/*
 * Sienna
 */
	rgb.rgb.red = 0.63; rgb.rgb.green =  0.32; rgb.rgb.blue =  0.18;
	gset_colr_rep(WKID, 18, &rgb );
/*
 * Done.
 */
	return;
}
