/*
 *	$Id: c_tpwrzt.c,v 1.1 1994-05-13 14:29:23 haley Exp $
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
    tc_pwrzt(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

tc_pwrzt (ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of
 *                        entry PWRZT with the THREED utility.
 *
 * USAGE                  CALL TPWRZT (IERROR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERROR
 *                          An integer variable
 *                          = 0, if the test was successful,
 *                          = 1, otherwise
 *
 * I/O                    If the test is successful, the message
 *
 *               PWRZT TEST EXECUTED--SEE PLOTS TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 1
 *                        frame is produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        the plot.
 *
 * PRECISION              Single
 *
 * REQUIRED ROUTINES      PWRZT, THREED
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN 77
 *
 * ALGORITHM              The THREED package is called to establish a
 *                        3-D projection onto 2 space, and to draw the
 *                        axis lines.  PWRZT is then called to label
 *                        the axes for a 3 space plot.
 *
 * EYE contains the (U,V,Z) coordinate of the eye position.
 */
    float eye[3];
    int line, itop, isize, icnt;

    eye[0] = 3.5;
    eye[1] = 3.0;
    eye[2] = 5.0;
/*
 * Initialize the error parameter.
 */
    *ierror = 1;
/*
 * Select normalization transformation number 0.
 */
    gsel_norm_tran (0);
/*
 * A call to SET3 establishes the mapping of 3 space coordinates onto
 * the coordinate system of the graphics device.
 */
    c_set3 (.1,.9,.1,.9,0.,1.,0.,1.,0.,1.,eye);
/*
 * draw the 3 space axes.
 */
    c_line3 (0.,0.,0.,0.,0.,1.);
    c_line3 (0.,0.,0.,0.,1.,0.);
    c_line3 (0.,0.,0.,1.,0.,0.);
/*
 * PWRZT is used to label each of the axes and the plot
 */
    icnt = 0;
    isize = 30;
    line = 2;
    itop = 3;
    c_pwrzt (0.,.5,.1,"V-AXIS",6,isize,line,itop,icnt);

    line = -1;
    itop = 3;
    c_pwrzt (.5,0.,.1,"U-AXIS",6,isize,line,itop,icnt);

    line = 3;
    itop = -2;
    c_pwrzt (0.,.1,.5,"Z-AXIS",6,isize,line,itop,icnt);

    line = 2;
    itop = -1;
    isize = 30;
    icnt = -1;
    c_pwrzt (.5,.2,0.,"DEMONSTRATION OF PWRZT WITH THREED",34,isize,line,itop,icnt);
    c_frame();

    *ierror = 0;
    printf(" PWRZT TEST EXECUTED--SEE PLOT TO CERTIFY\n");
    return(1);
}

