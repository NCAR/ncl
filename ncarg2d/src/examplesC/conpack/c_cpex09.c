/*
 *	$Id: c_cpex09.c,v 1.1 1994-05-13 14:25:59 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
/*
 * Define an array for the data.
 */
    float zdat[40][40],zdat2[40][40];
    float p1[2],p2[2],p3[2],p4[2];
    int i, j;
/*
 * Generate dummy data.
 */
    gendat (zdat,40,40,40,14,14,-145.6,389.7);
/*
 * Open GKS.
 */
    c_opngks();
/*
 * Turn off clipping by GKS.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Force the use of medium-quality characters by the package PLOTCHAR.
 */
    c_pcseti ("QU - QUALITY OF CHARACTERS",1);
/*
 * Put a label at the top of the first plot.  (The SET call is not
 * needed for CPEZCT, but for the labelling routine.)
 */
    c_set (.05,.95,.05,.95,0.,1.,0.,1.,1);
    labtop ("EXAMPLE 9-1",.017);
/*
 * Put a boundary line at the edge of the plotter frame.
 */
    bndary();
/*
 * Contour the data, using the EZCNTR simulator.
 */
    c_cpezct (&zdat[0][0],40,40);
/*
 * Contour a subset of the data, forcing a contour interval of 20, using
 * the CONREC simulator.
 */
    for( j = 0; j < 32; j++ ) {
        for( i = 0; i < 24; i++ ) {
            zdat2[i][j] = zdat[8+i][6+j];
        }
    }
    c_cpcnrc (&zdat2[0][0],40,32,24,0.,0.,20.,3,0,-682);
/*
 * Put a boundary line at the edge of the plotter frame, label the plot,
 * and advance the frame.
 */
    bndary();
    labtop ("EXAMPLE 9-2",.017);
    c_frame();
/*
 * Switch to the "penalty scheme" for positioning contour-line labels
 * and change one of the constants which are used by it.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",3);
    c_cpsetr ("PC1 - PENALTY SCHEME CONSTANT 1",1.5);
/*
 * Turn on the smoother, with a relatively high tension.
 */
    c_cpseti ("T2D - TENSION ON THE 2D SMOOTHER",4);
/*
 * Force the labelling of every other contour line.  (This will only
 * work when the contour interval is forced, as it will be in the next
 * call to CPCNRC.)
 */
    c_cpseti ("LIS - LABEL INTERVAL SPECIFIER",2);
/*
 * Repeat the last plot, forcing a contour interval of 10.
 */
    for( i = 0; i < 24; i++ ) {
        for( j = 0; j < 32; j++ ) {
            zdat2[i][j] = zdat[8+i][6+j];
        }
    }
    c_cpcnrc (&zdat2[0][0],40,32,24,0.,0.,10.,3,0,-682);
/*
 * Put a boundary line at the edge of the plotter frame, label the plot,
 * and advance the frame.
 */
    bndary();
    labtop ("EXAMPLE 9-3",.017);
    c_frame();
/*
 * Create an EZMAP background.
 */
    p1[0] = p2[0] = p3[0] = p4[0] = 0.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mapsti ("PE - PERIMETER",0);
    c_mapsti ("GR - GRID",0);
    c_mapstc ("OU - OUTLINE DATASET","PS");
    c_mapsti ("DO - DOTTING OF OUTLINES",1);
    c_mapstr ("SA - SATELLITE HEIGHT",1.13);
    c_maproj ("SV - SATELLITE-VIEW",40.,-95.,0.);
    c_mapset ("MA - MAXIMAL AREA",p1,p2,p3,p4);
    c_mapdrw();
/*
 * Arrange for output from CPCNRC to be placed on the EZMAP background.
 */
    c_cpsetr ("XC1 - X COORDINATE AT I = 1",-130.);
    c_cpsetr ("XCM - X COORDINATE AT I = M",-60.);
    c_cpsetr ("YC1 - Y COORDINATE AT J = 1",10.);
    c_cpsetr ("YCN - Y COORDINATE AT J = N",70.);
    c_cpseti ("MAP - MAPPING FLAG",1);
    c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.E12);
/*
 * define some special values and arrange for the special-value area to;
 * be outlined.
 */
    zdat[12][14]=1.e36;
    zdat[12][15]=1.e36;
    zdat[13][14]=1.e36;
    c_cpsetr ("SPV - OUT-OF-RANGE VALUE",1.E36);
    c_cpseti ("PAI - PARAMETER ARRAY INDEX",-2);
    c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
/*
 * Specify that high/low labels are to be written at an angle of 30
 * degrees, using relatively small characters.  (These parameters will
 * really be used for point-value labels.)
 */
    c_cpsetr ("HLA - HIGH/LOW LABEL ANGLE",30.);
    c_cpsetr ("HLS - HIGH/LOW LABEL CHARACTER SIZE",.008);
/*
 * Turn off line labelling.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",0);
/*
 * Use the CONREC simulator to plot a subset of the data on the EZMAP
 * background, labelling each data point.
 */
    for( i = 0; i < 20; i++ ) {
        for( j = 0; j < 20; j++ ) {
            zdat2[i][j] = zdat[6+i][8+j];
        }
    }
    c_cpcnrc (&zdat2[0][0],40,20,20,0.,0.,10.,4,1,-682);
/*
 * Put a boundary line at the edge of the plotter frame, label the plot,
 * and advance the frame.
 */
    bndary();
    labtop ("EXAMPLE 9-4",.017);
    c_frame();
/*
 * Close GKS.
 */
    c_clsgks();
}
