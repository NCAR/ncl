/*
 *	$Id: c_fagezmy.c,v 1.1 1994-07-18 15:25:04 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NPTS  200
#define NCURVE  4

main()
{
	float ydra[NCURVE][NPTS];
	int i, j;

	for( i = 1; i <= NPTS; i++ ) {
        for( j = 1; j <= NCURVE; j++ ) {
            ydra[j-1][i-1] = sin(i*0.1+0.2*j)*exp(-0.01*i*.1*j*j);
		}
	}
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);

	c_ezmy (&ydra[0][0],NPTS,NCURVE,NPTS,"EZMY$");
/*
 *  Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}
