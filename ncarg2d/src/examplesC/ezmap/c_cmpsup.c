/*
 *	$Id: c_cmpsup.c,v 1.1 1994-07-20 17:06:03 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	int ierr;
	float plim1[2],plim2[2],plim3[2],plim4[2];
	plim1[0] = 0.0; plim1[1] =  0.0;
	plim2[0] = 0.0; plim2[1] =  0.0;
	plim3[0] = 0.0; plim3[1] =  0.0;
	plim4[0] = 0.0; plim4[1] =  0.0;
/*
 * Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
      
	c_supmap(7,0.,0.,0.,plim1,plim2,plim3,plim4,1,5,0,0,&ierr);
	c_frame();
/*
 * Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}
