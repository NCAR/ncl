/*
 *	$Id: c_fagezxy.c,v 1.1 1994-07-18 15:25:05 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NPTS  200

main()
{
	float ydra[NPTS],xdra[NPTS];
	int i;

	for( i = 1; i <= NPTS; i++ ) {
		xdra[i-1] = i*0.1;
		ydra[i-1] = sin(xdra[i-1]+0.2)*exp(-0.01*xdra[i-1]*4);
	}
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);

	c_ezxy (xdra,ydra,NPTS,"EZXY$");
/*
 *  Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}
