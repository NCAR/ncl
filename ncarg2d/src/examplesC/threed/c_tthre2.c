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
    gopen_gks("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws(1);
/*
 * INVOKE DEMO DRIVER
 */
    tthree(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws (1);
    gclose_ws (1);
    gclose_gks();
}

tthree(ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of THREED.
 *
 * USAGE                  tthree (&ierror)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              ierror
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
    float eye[3],x[31],y[31],z[31],ex,why,zee;
    float rxa, rxb, rya, ryb, uc, ud, vc, vd, wc, wd, tx, ty, pi;
    float theta, ct, st, phi, sp, tueta, cp,rise,run;
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
    c_tick43(5,10,5,10,5,10);
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
            z[k] = sin(phi)/2.;
           zee =z[k];
            cp = cos(phi);
            x[k] = ct*cp;
           ex = x[k];
            y[k] = st*cp;
           why = y[k];
            c_point3(ex,why,zee);
        }
/*   c_fence3(x,y,z,31,1,-1.);*/
 }  


/*
 * Add a plot title.
 */
    c_plchlq(tx,ty,"TEST FOR THREED - c_point3,c_tick43",16.,0.,0.);
/*
 * Make a new frame 
 */
    c_frame();

    for( k=1; k < 3; k++ ) {
        phi = (float)(-k)*pi/6.;
        sp = sin(phi);
        cp = cos(phi);
        for( j = 0; j < 31; j++ ) {
            tueta = (float)(j)*pi/15.;
            x[j] = cos(tueta)*cp;
            y[j] = sin(tueta)*cp;
            z[j] =  (sp - .5);
        }
        c_fence3(x,y,z,31,3,-1.);
    }

        k = 1;
        phi = (float)(k-3)*pi/6.;
        sp = sin(phi);
        cp = cos(phi);
        for( j = 0; j < 31; j++ ) {
            tueta = (float)(j)*pi/15.;
            x[j] = cos(tueta)*cp;
            y[j] = sin(tueta)*cp;
            z[j] = 1.;
        c_frst3(0.,0.,-.75);
        c_vect3(x[j],y[j],z[j]);
        }


/*
 * Add a plot title.
 */
    c_plchlq(tx,ty,"TEST FOR THREED - c_frst3,c_vect3",16.,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();

    *ierror = 0;
    printf(" THREED TEST EXECUTED--SEE PLOT TO CERTIFY\n");
}


