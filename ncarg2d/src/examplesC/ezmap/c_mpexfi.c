/*
 *	$Id: c_mpexfi.c,v 1.2 1994-06-21 15:00:16 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{

/*
 * Define a data array.
 */
    int i, j, ifst,ierr;
    float xycd[224];
    float plm1[2], plm2[2], plm3[2], plm4[2];
    extern void bndary();
/*
 * Define the centers and the expansion/shrinkage factors for
 * various copies of the curve to be drawn.
 */
    float flac[4],floc[4],fmul[4], flon, flat;

    floc[0] = -38.1;
    flac[0] = 32.0;
    fmul[0] = 1.7;
    floc[1] = -37.9;
    flac[1] = 32.0;
    fmul[1] = 1.7;
    floc[2] = -38.0;
    flac[2] = 31.9;
    fmul[2] = 1.7;
    floc[3] = -38.0;
    flac[3] = 32.1;
    fmul[3] = 1.7;
/*
 * Open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Fill the data array.
 */
    for( i = 0; i < 224; i++ ) {
        fscanf( stdin, "%g", &xycd[i] );
    }
/*
 * Define the altitude of the satellite.
 */
    c_mapstr ("SA",2.);
/*
 * Draw a map of the North Atlantic, as seen by a satellite.
 */
    plm1[0] = plm2[0] = plm3[0] = plm4[0] = 0.;
    c_supmap (7,32.,-38.,20.,plm1,plm2,plm3,plm4,1,-1000,5,0,&ierr);
/*
 * Force MAPIT to draw dotted lines.
 */
    c_mapsti ("DL",1);
    c_mapsti ("DD",3);
/*
 * Draw some curves.
 */
    for( i = 0; i < 4; i++ ) {
        ifst=0;
        for( j = 0; j < 112; j++ ) {
            if (xycd[2*j] == 0.) {
                ifst=0;
            }
            else {
                flon=floc[i]+fmul[i]*(xycd[2*j  ]-15.);
                flat=flac[i]+fmul[i]*(xycd[2*j+1]-15.);
                c_mapit (flat,flon,ifst);
                ifst=1;
            }
        }

    }
/*
 * Dump MAPIT"s buffers.
 */
    c_mapiq();
/*
 * Draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void bndary()
{
/*
 * Routine to draw the plotter-frame edge.
 */
    c_plotit(    0,    0,0);
    c_plotit(32767,    0,1);
    c_plotit(32767,32767,1);
    c_plotit(    0,32767,1);
    c_plotit(    0,    0,1);
}
