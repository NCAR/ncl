/*
 *	$Id: c_tthree.c,v 1.1 1994-05-13 14:29:27 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
    int idum, ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws (1);
/*
 * INVOKE DEMO DRIVER
 */
    tthree(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

tthree(ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of THREED.
 *
 * USAGE                  CALL TTHREE (IERROR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERROR
 *                          An integer variable
 *                          = 0, if the test was successful,
 *                          = 1, the test was not successful.
 *
 * I/O                    If the test is successful, the message
 *
 *               THREED TEST EXECUTED--SEE PLOT TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 1
 *                        frame is produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        the plot.
 *
 * PRECISION              Single
 *
 * LANGUAGE               FORTRAN 77
 *
 * REQUIRED ROUTINES      THREED
 *
 * REQUIRED GKS LEVEL     0A
 *
 * ALGORITHM              Routine TTHREE calls SET3 to establish a
 *                        mapping between the plotter addresses and
 *                        the user's volume, and to indicate the
 *                        coordinates of the eye position from which
 *                        the lines to be drawn are viewed.  Next,
 *                        the volume perimeters and associated tick
 *                        marks are drawn by calls to PERIM3.  The
 *                        selected latitude and longitude lines of
 *                        a sphere are then drawn.
 *
 * HISTORY                THREED was originally written in November
 *                        1976 and converted to FORTRAN 77 and GKS
 *                        in July 1984.
 */
    float eye[3],x[31],y[31],z[31];
    float rxa, rxb, rya, ryb, uc, ud, vc, vd, wc, wd, tx, ty, pi;
    float theta, ct, st, phi, sp, tueta, cp;
    int i, j, k;
/*
 * Specify the arguments to be used by routine SET3 on a plot
 * grid in the address range of 0. to 1.  In each coordinate direction,
 * the values  RXA, RXB, RYA, and RYB define the portion of the address
 * space to be used in making the plot.  UC, UD, VC, VD, WC, and WD
 * define a volume in user coordinates which is to be mapped onto the
 * portion of the viewing surface as specified by RXA, RXB, RYA, and RYB.
 */
    rxa = 0.097656;
    rxb = 0.90236;
    rya = 0.097656;
    ryb = 0.90236;
    uc = -1.;
    ud = 1.;
    vc = -1.;
    vd = 1.;
    wc = -1.;
    wd = 1.;
    eye[0] = 10.;
    eye[1] = 6.;
    eye[2] = 3.;
    tx = 0.4374;
    ty = 0.9570;
    pi = 3.1415926535898;
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran (0);
/*
 * Call SET3 to establish a mapping between the plotter addresses
 * and the user's volume, and to indicate the coordinates of the
 * eye position from which the lines to be drawn are viewed.
 */
    c_set3(rxa,rxb,rya,ryb,uc,ud,vc,vd,wc,wd,eye);
/*
 * Call PERIM3 to draw perimeter lines and tick marks.
 */
    c_perim3(2,5,1,10,1,-1.);
    c_perim3(4,2,1,1,2,-1.);
    c_perim3(2,10,4,5,3,-1.);
/*
 * Define and draw latitudinal lines on the sphere of radius one
 * having its center at (0.,0.,0.)
 */
    for( j = 1; j <= 18; j++ ) {
        theta = (float)(j)*pi/9.;
        ct = cos(theta);
        st = sin(theta);
        for( k = 0; k < 31; k++ ) {
            phi = (float)(k-15)*pi/30.;
            z[k] = sin(phi);
            cp = cos(phi);
            x[k] = ct*cp;
            y[k] = st*cp;
        }
        c_curve3(x,y,z,31);
    }
/*
 * Define and draw longitudinal lines on the sphere of radius one
 * having its center at (0.,0.,0.)
 */
    for( k=1; k <= 5; k++ ) {
        phi = (float)(k-3)*pi/6.;
        sp = sin(phi);
        cp = cos(phi);
        for( j = 0; j < 31; j++ ) {
            tueta = (float)(j)*pi/15.;
            x[j] = cos(tueta)*cp;
            y[j] = sin(tueta)*cp;
            z[j] = sp;
        }
        c_curve3(x,y,z,31);
    }
/*
 * Add a plot title.
 */
    c_plchlq(tx,ty,"DEMONSTRATION PLOT FOR ROUTINE THREED",16.,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();

    *ierror = 0;
    printf(" THREED TEST EXECUTED--SEE PLOT TO CERTIFY\n");
    return(1);
}

