/*
 *	$Id: c_fsrpwrzs.c,v 1.1 1994-10-31 04:09:45 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define pow2(x)   ((x)*(x))

#if !defined(cray)
extern struct common1
#else
struct common1
#endif
{
    int ifr, istp, irots, idrx, idry, idrz, iupper, iskirt, ncla, ispval;
    float theta, hskirt, chi, clo, cinc;
} NGCALLF(srfip1,SRFIP1);

main()
{
	int ierr;
	extern void tpwrzs();
/*
 * Open gks, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	tpwrzs(&ierr);
/*
 * Deactivate and close workstation, close gks.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void tpwrzs(ierror)
int *ierror;
{
	Gcolr_rep rgb;
	float tx, ty;
	int i, j, m, n, isize;
/*
 * PURPOSE                To provide a simple demonstration of
 *                        entry PWRZS with the SRFACE utility.
 *
 * USAGE                  CALL TPWRZS (IERROR)
 *
 * ARGUMENTS
 *
 * ON INPUT               WKID
 *                          A workstation id number
 *
 * ON OUTPUT              IERROR
 *                          An integer variable
 *                          = 0, if the test was successful,
 *                          = 1, otherwise
 *
 * I/O                    If the test is successful, the message
 *
 *               PWRZS TEST EXECUTED--SEE PLOTS TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 1
 *                        frame is produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        the plot.
 *
 * PRECISION              Single
 *
 * REQUIRED ROUTINES      PWRZS, SRFACE
 *
 * REQUIRED GKS LEVEL     0A
 *
 * LANGUAGE               FORTRAN 77
 *
 * ALGORITHM              A function of 2 variables is defined and the
 *                        values of the function on a 2-D rectangular
 *                        grid are stored in an array.  This routine
 *                        calls SRFACE to draw a surface representation
 *                        of the array values.  PWRZS is then called 3
 *                        times to label the front, side, and back of
 *                        the picture.
 */
	float z[30][20],x[20],y[30],s[6];
	int mm[2][20][30];
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
    *ierror = 1;
/*
 * Set up a color table
 *
 * White background
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,0,&rgb);
/*
 * Black foreground
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,1,&rgb);
/*
 * Red
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,2,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,3,&rgb);
/*
 * Blue
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,4,&rgb);
/*
 * Define the function values and store them in the Z array.
 */
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
 *
 * Set the label color
 */
	gset_line_colr_ind(2);
    isize = 35;
    c_pwrzs (0.,1.1,0.,"FRONT",5,isize,-1,3,0);
    c_pwrzs (1.1,0.,0.,"SIDE",4,isize,2,-1,0);
    c_pwrzs (0.,-1.1,.2," BACK BACK BACK BACK BACK",25,isize,-1,3,0);
    c_frame();
	*ierror = 0;
    printf( "PWRZS TEST EXECUTED--SEE PLOT TO CERTIFY\n");
/*
 * Restore the SRFACE parameters to their default values.
 */
    NGCALLF(srfip1,SRFIP1).ifr = 1;
    NGCALLF(srfip1,SRFIP1).idrz = 0;
	return;
}
