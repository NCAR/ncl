/*
 * $Id: c_ccpscam.c,v 1.1 1994-10-31 02:26:25 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define MREG   50
#define NREG   50
#define NRAN   30
#define LRWK   5000
#define LIWK   5000
#define LMAP   60000
#define NWRK   5000
#define NOGRPS   5

main()
{
	float xreg[MREG],yreg[NREG],zreg[NREG][MREG];
	extern void getdat(), ccpscm();
/*
 * Get data array
 */
	getdat(xreg,yreg,zreg);
/*
 * Open GKS and turn off clipping
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);

	gset_clip_ind(GIND_NO_CLIP);
/*
 * Call contour B&W fill routine
 */
	ccpscm(zreg);
/*
 * Close frame and close GKS
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void ccpscm(zreg)
float zreg[NREG][MREG];
{
	float rwrk[LRWK], xwrk[NWRK], ywrk[NWRK];
	int iwrk[LIWK];
	int map[LMAP],iarea[NOGRPS],igrp[NOGRPS];
	int ncl, i, irwu, iwu;

	extern int sfill();
	extern int NGCALLF(cpdrpl,CPDRPL)();
/*
 * Use regular or penalty labeling scheme so that contour labels can be
 * boxed, and draw boxes.
 */
	c_cpseti("LLP - LINE LABEL POSITIONING FLAG",2);
	c_cpseti("LLB - LINE LABEL BOX FLAG",1);
	c_cpseti("HLB - HIGH/LOW LABEL BOX FLAG",1);
/*
 * Set number of contour levels and initialize Conpack
 */
	c_cprect (&zreg[0][0], MREG, MREG, NREG, rwrk, LRWK, iwrk, LIWK);
	c_cppkcl (&zreg[0][0], rwrk, iwrk);
/*
 * Turn on line labeling and turn off area identifiers for all levels
 */
	c_cpgeti("NCL - NUMBER OF CONTOUR LEVELS",&ncl);
	for( i = 1; i <= ncl; i++ ) {
		c_cpseti("PAI - PARAMETER ARRAY INDEX",i);
		c_cpseti("CLU - CONTOUR LEVEL USE FLAG",3);
		c_cpseti("AIA - AREA IDENTIFIER ABOVE",0);
		c_cpseti("AIB - AREA IDENTIFIER BELOW",0);
	}
/*
 * Add contour levels at 1.25 and 1.5, and set area ids so that 
 * you can fill between them
 */
	c_cpseti("NCL - NUMBER OF CONTOUR LEVELS",ncl+2);
	c_cpseti("PAI - PARAMETER ARRAY INDEX",ncl+1);
	c_cpsetr("CLV - CONTOUR LEVEL VALUE",1.25);
	c_cpseti("CLU - CONTOUR LEVEL USE FLAG",3);
	c_cpseti("AIA - AREA IDENTIFIER ABOVE",1);
	c_cpseti("AIB - AREA IDENTIFIER BELOW",2);
	c_cpseti("PAI - PARAMETER ARRAY INDEX",ncl+2);
	c_cpsetr("CLV - CONTOUR LEVEL VALUE",1.5);
	c_cpseti("CLU - CONTOUR LEVEL USE FLAG",3);
	c_cpseti("AIA - AREA IDENTIFIER ABOVE",3);
	c_cpseti("AIB - AREA IDENTIFIER BELOW",1);
/*
 * Draw Perimeter
 */
	c_cpback(&zreg[0][0], rwrk, iwrk);
/*
 * Initialize Areas
 */
	c_arinam(map, LMAP);
/*
 * Add contours to area map
 */
	c_cpclam(&zreg[0][0], rwrk, iwrk, map);
/*
 * Add label boxes to area map
 */
	c_cplbam(&zreg[0][0], rwrk, iwrk, map);
/*
 * Fill contours
 */
	c_arscam(map, xwrk, ywrk, NWRK, iarea, igrp, NOGRPS, sfill);
/*
 * Draw contours, masking label boxes
 */
	c_cpcldm(&zreg[0][0], rwrk, iwrk, map, NGCALLF(cpdrpl,CPDRPL));
/*
 * Draw Labels
 */
	c_cplbdr(&zreg[0][0], rwrk, iwrk);
/*
 * Write out the amount of space used in the area map
 */
	c_cpgeti("RWU - REAL WORKSPACE USED",&irwu);
	c_cpgeti("IWU - INTEGER WORKSPACE USED",&iwu);
	printf( "Area map used %d words.\n",map[0]-map[5]+map[4] );
	printf("Real workspace used %d words.\n",irwu );
	printf("Integer workspace used %d words.\n",iwu );
	return;
}

int sfill (xwrk,ywrk,nwrk,iarea,igrp,ngrps)
float *xwrk, *ywrk;
int *nwrk, *iarea, *igrp, *ngrps;
{
	float rscr[5000];
	int i, iscr[5000], iarea3;

	for( i = 0; i < *ngrps; i++ ) {
		if (igrp[i] == 3) iarea3 = iarea[i];
	}

	if (iarea3 == 1) {
/*
 * If the area is defined by 3 or more points, fill it
 */
		c_sfsetr("SPACING",.006);
		c_sfnorm(xwrk,ywrk,*nwrk,rscr,5000,iscr,5000);
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
