/*
 *	$Id: c_tpwrzs.c,v 1.1 1994-10-31 04:09:46 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define pow2(x)    ((x)*(x))

#if !defined(cray)
extern struct common1
#else
struct common1
#endif
{
    int ifr, istp, irots, idrx, idry, idrz, iupper, iskirt, ncla, ispval;
    float theta, hskirt, chi, clo, cinc;
} NGCALLF(srfip1,SRFIP1);

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    int idum, ierr;
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws (WKID);
/*
 * INVOKE DEMO DRIVER
 */
	tc_pwrzs(ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

tc_pwrzs (ierror)
int ierror;
{
    float z[30][20], x[20], y[30], s[6];
    int i, j, mm[2][20][30], m, n, isize;
    float tx, ty;
/*
 * Define the center of a plot title string on a square grid of size
 * 0. to 1.
 */
    tx = 0.4375;
    ty = 0.9667;
/*
 * Specify grid loop indices and a line of sight.
 */
    m = 20;
    n = 30;
    s[0] = 4.;
    s[1] = 5.;
    s[2] = 3.;
    s[3] = s[4] = s[5] = 0.;
/*
 * Initial the error parameter.
 */
    ierror = 1;
/*
 * Define the function values and store them in the Z array.
 */
    for( i = 0; i < m; i++ ) {
        x[i] = -1.+(float)i/(float)(m-1)*2.;
    }
    for( j = 0; j < n; j++ ) {
        y[j] = -1.+(float)j/(float)(n-1)*2.;
    }
    for( j = 0; j < n; j++ ) {
        for( i = 0; i < m; i++ ) {
            z[j][i] = exp(-2.*sqrt(pow2(x[i])+pow2(y[j])));
        }
    }
/*
 * Set SRFACE parameters to supress the FRAME call and draw contours.
 */
    NGCALLF(srfip1,SRFIP1).ifr = 0;
    NGCALLF(srfip1,SRFIP1).idrz = 1;
/*
 * Select normalization trans number 0.
 */
    gsel_norm_tran (0);
/*
 * Label the plot.
 */
    c_plchlq (tx,ty,"DEMONSTRATION PLOT FOR PWRZS",16.,0.,0.);
/*
 * Draw the surface plot.
 */
    c_srface (x,y,&z[0][0],&mm[0][0][0],m,m,n,s,0.);
/*
 * Put the PWRZS labels on the picture.
 */
    isize = 35;
    c_pwrzs (0.,1.1,0.,"FRONT",5,isize,-1,3,0);
    c_pwrzs (1.1,0.,0.,"SIDE",4,isize,2,-1,0);
    c_pwrzs (0.,-1.1,.2," BACK BACK BACK BACK BACK",25,isize,-1,3,0);
    c_frame();

    ierror = 0;
/*
 * Restore the SRFACE parameters to their default values.
 */
    NGCALLF(srfip1,SRFIP1).ifr = 1;
    NGCALLF(srfip1,SRFIP1).idrz = 0;
    printf( "PWRZS TEST EXECUTED--SEE PLOT TO CERTIFY\n");
    return(1);
}
