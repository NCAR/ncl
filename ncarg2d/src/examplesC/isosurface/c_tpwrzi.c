/*
 *	$Id: c_tpwrzi.c,v 1.1 1994-05-13 14:28:22 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

float t[19][31][21],slab[33][33];

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
    tc_pwrzi(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

tc_pwrzi (ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of
 *                        entry PWRZI with the ISOSRF utility.
 *
 * USAGE                  CALL TPWRZI (IERROR)
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
 *               PWRZI TEST EXECUTED--SEE PLOTS TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 1
 *                        frame is produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        the plot.
 *
 * PRECISION              Single
 *
 * REQUIRED ROUTINES      PWRZI, ISOSRF
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN 77
 *
 * ALGORITHM              A function of 3 variables is defined and the
 *                        values of the function on a 3-D rectangular
 *                        grid are stored in an array.  This test routine
 *                        then calls ISOSRF to draw an iso-valued surface
 *                        plot of the function.  PWRZI is then called 3
 *                        times to label the front, side, and back of
 *                        the picture.
 */
    float eye[3], tx, ty;
    float rbig1, rbig2, rsml1, rsml2, tiso, fimid, fkmid;
    float f1, f2, fip1, fip2, fjmid1, fjmid2, fkp1, fkp2, fjp1, fjp2;
    float xmin1, xmin2;
    int nu, nv, nw, i, j, k, l, muvwp2, iflag, mu, mv, mw;
    int jcent1, jcent2, isize;
/*
 * Define the center of a plot title string on a square grid of size
 * 0. to 1.
 */
    tx = 0.4375;
    ty = 0.9667;

    nu = 21;
    nv = 31;
    nw = 19;
    rbig1 = 6.;
    rbig2 = 6.;
    rsml1 = 2.;
    rsml2 = 2.;
    tiso = 0.;
    muvwp2 = 33;
    iflag = -7;
/*
 * Initialize the error parameter.
 */
    *ierror = 1;
/*
 * Fill the 3-D array to be plotted.
 */
    jcent1 = (float)(nv)*.5-rbig1*.5;
    jcent2 = (float)(nv)*.5+rbig2*.5;
    mu = nu/2;
    mv = nv/2;
    mw = nw/2;
    for( i = 0; i < nu; i++ ) {
        fimid = (i+1)-nu/2;
        for( j = 0; j < nv; j++ ) {
            fjmid1 = (j+1)-jcent1;
            fjmid2 = (j+1)-jcent2;
            for( k = 0; k < nw; k++ ) {
                fkmid = (k+1)-nw/2;
                f1 = sqrt(rbig1*rbig1/(fjmid1*fjmid1+fkmid*fkmid+.1));
                f2 = sqrt(rbig2*rbig2/(fimid*fimid+fjmid2*fjmid2+.1));
                fip1 = (1.-f1)*fimid;
                fip2 = (1.-f2)*fimid;
                fjp1 = (1.-f1)*fjmid1;
                fjp2 = (1.-f2)*fjmid2;
                fkp1 = (1.-f1)*fkmid;
                fkp2 = (1.-f2)*fkmid;
                xmin1 = fimid*fimid+fjp1*fjp1+fkp1*fkp1-rsml1*rsml1;
                xmin2 = fkmid*fkmid+fip2*fip2+fjp2*fjp2-rsml2*rsml2;
                t[k][j][i] = xmin1 < xmin2 ? xmin1 : xmin2;
            }
        }
    }
/*
 * Define the eye position.
 */
    eye[0] = 100.;
    eye[1] = 150.;
    eye[2] = 125.;
/*
 * Select normalization transformation number 0.
 */
    gsel_norm_tran (0);
/*
 * Label the plot.
 */
    c_plchlq (tx,ty,"DEMONSTRATION PLOT FOR PWRZI",16.,0.,0.);
/*
 * Test ISOSRF with subarray T.
 */
    muvwp2 = mu > mv ? mu : mv;
    muvwp2 = mw > muvwp2 ? mw : muvwp2;
    muvwp2 += 2;
    c_isosrf(&t[mw-1][mv-1][mu-1],nu,mu,nv,mv,mw,eye,muvwp2,&slab[0][0],tiso,iflag);
    isize = 35;
    c_pwrzi (5.,16.,.5,"FRONT",5,isize,-1,3,0);
    c_pwrzi (11.,7.5,.5,"SIDE",4,isize,2,-1,0);
    c_pwrzi (5.,1.,5.," BACK BACK BACK BACK BACK",25,isize,-1,3,0);
    c_frame();

    *ierror = 0;
    printf(" PWRZI TEST EXECUTED--SEE PLOT TO CERTIFY\n");
    return(1);
}
