/*
 *	$Id: c_fsfwrld.c,v 1.1 1994-07-27 19:10:04 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/* 
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NPTS  101
#define LRWK 1000
#define LIWK 1000

main()
{
	int i, iwrk[LIWK];
	float x1[NPTS],y1[NPTS],x2[NPTS],y2[NPTS],x3[NPTS],y3[NPTS];
	float rwrk[LRWK], x, y, ang;
/*
 * Convert from degrees to radians.
 */
	float d2r = .017453292519943;
/*
 *  Demonstrate the use of SFWRLD.
 *
 *  Generate three intersecting circles of radius 1.
 */
    for( i = 0; i < NPTS; i++ ) {
        ang=d2r*3.6*(float)i;
        x=cos(ang);
        y=sin(ang);
        x1[i] = x - .5;
        x2[i] = x + .5;
        x3[i] = x;
        y1[i] = y + .5;
        y2[i] = y + .5;
        y3[i] = y - .5;
    }
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 *  Define the entire Viewport and a Window from -2. to 2 with linear scaling.
 */
	c_set(0.,1.,0.,1.,-2.,2.,-2.,2.,1);
/*
 *  Process the area definitions (regions) and fill according to instructions
 */
	c_sfsetr ("SP",0.006);
	c_sfsetr ("AN",0.);
	c_sfwrld (x1,y1,NPTS,rwrk,LRWK,iwrk,LIWK);
	c_sfsetr ("AN",45.);
	c_sfwrld (x2,y2,NPTS,rwrk,LRWK,iwrk,LIWK);
	c_sfsetr ("AN",90.);
	c_sfwrld (x3,y3,NPTS,rwrk,LRWK,iwrk,LIWK);
/*
 *  Do all the rest necesary to display the picture and end the plot
 */
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}
