/*
 *	$Id: c_cmpou.c,v 1.1 1994-07-20 17:06:00 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	extern void cmpou();
/*
 * Open GKS, Turn Clipping off
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	cmpou();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void cmpou()
{
	float plim1[2], plim2[2], plim3[2], plim4[2];

	plim1[0] = 30.; plim1[1] = 0.;
	plim2[0] = -15.; plim2[1] = 0.;
	plim3[0] = 60.; plim3[1] = 0.;
	plim4[0] = 30.; plim4[1] = 0.;
/*
 * CMPOU demonstrates political boundaries in the Maps utility.
 *
 * Set up Maps.
 */
	c_mapstc ("OU - OUTLINE DATASET SELECTOR","PO");
	c_maproj ("ME",0.,0.,0.);
	c_mapset ("CO",plim1,plim2,plim3,plim4);
/*
 * Initialize Maps.
 */
	c_mapint();
/*
 * Draw a perimeter and outline all the countries.
 */
	c_mapsti("LA - LABEL FLAG",0);
	c_maplbl();
	c_maplot();
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Done.
 */
	return;
}
