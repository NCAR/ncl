/*
 *	$Id: c_tisosr.c,v 1.6 1995-06-19 00:09:15 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

float t[19][31][21],slab[33][33];

#define IWTYPE 1
#define WKID    1

main()
{
    int idum, ierr;
/*
 * Open GKS, open workstation, activate workstation
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws (WKID);
/*
 * invoke demo driver
 */
    tisosr(&ierr);
/*
 *     Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
tisosr (ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of ISOSRF.
 *
 * USAGE                  CALL TISOSR (IERROR)
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
 *               ISOSRF TEST EXECUTED--SEE PLOTS TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 2
 *                        frames are produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        the plots.
 *
 * PRECISION              Single
 *
 * LANGUAGE               FORTRAN 77
 *
 * REQUIRED ROUTINES      ISOSRF
 *
 * REQUIRED GKS LEVEL     0A
 *
 * ALGORITHM              Values of a function on a 3-D rectangular grid
 *                        are stored in array T.  Entries EZISOS and
 *                        ISOSRF are called to draw iso-valued surface
 *                        plots of the function.
 */
    float eye[3];
    float rbig1, rbig2, rsml1, rsml2, tiso, fimid, fkmid;
    float f1, f2, fip1, fip2, fjmid1, fjmid2, fkp1, fkp2, fjp1, fjp2;
    float xmin1, xmin2;
    float ix, iy;
    int nu, nv, nw, i, j, k, muvwp2, iflag, mu, mv, mw;
    int jcent1, jcent2, isize;
/*
 * Specify coordinates for plot titles.
 */
    for( i = 0; i < 33; i++ ) {
        for( j = 0; j < 33; j++ ) {
            slab[i][j] = 0.;
        }
    }
    ix = 0.44;
    iy = 0.95;
    nu = 21;
    nv = 31;
    nw = 19;
    rbig1 = 6.;
    rbig2 = 6.;
    rsml1 = 2.;
    rsml2  = 2.;
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
 *     Frame 1 -- The EZISOS entry.
 */
/*
 * Select normalization transformation 0.
 */
   gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(ix,iy,"DEMONSTRATION PLOT FOR ENTRY EZISOS OF ISOSRF",16.,0.,0.);
    c_ezisos (&t[0][0][0],nu,nv,nw,eye,&slab[0][0],tiso);
/*
 *     frame 2 -- The ISOSRF entry.
 *
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq(ix,iy,"DEMONSTRATION PLOT FOR ENTRY ISOSRF OF ISOSRF",16.,0.,0.);
/*
 * Test ISOSRF with a subarray of T.
 */
    mu = nu/2;
    mv = nv/2;
    mw = nw/2;
    muvwp2 = mu > mv ? mu : mv;
    muvwp2 = mw > muvwp2 ? mw : muvwp2;
    muvwp2 += 2;
    c_isosrf(&t[mw-1][mv-1][mu-1],nu,mu,nv,mv,mw,eye,muvwp2,&slab[0][0],tiso,iflag);
    c_frame();

    *ierror = 0;
    printf( "ISOSRF TEST EXECUTED--SEE PLOTS TO CERTIFY\n");
    return(1);
}
