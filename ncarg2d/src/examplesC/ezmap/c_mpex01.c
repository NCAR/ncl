/*
 *	$Id: c_mpex01.c,v 1.2 1994-06-21 15:00:05 haley Exp $
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
    char plbl[38];
    float p1[2],p2[2],p3[2],p4[2];
    extern void bndary();

    strcpy( plbl,"THE U.S. ON A LAMBERT CONFORMAL CONIC" );

	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);

    p1[0] = 22.6;
    p2[0] = -120.;
    p3[0] = 46.9;
    p4[0] = -64.2;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mpsetc("OU","US");
    c_maproj("LC",30.,-100.,45.);
    c_mapset("CO",p1,p2,p3,p4);
    c_mapdrw();
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pwrit(.5,.925,plbl,37,2,0,0);
    bndary();
    c_frame();

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
