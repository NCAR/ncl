/*
 *	$Id: c_cezmap1.c,v 1.1 1994-07-20 17:05:45 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	extern void cezmap();
/*
 * Open GKS, Turn Clipping off
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	cezmap("SV",40.,-50.,0.,"PO");
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void cezmap(proj,plat,plon,rota)
char *proj;
float plat, plon, rota;
{
	float plim1[2], plim2[2], plim3[2], plim4[2];

	plim1[0] =  0.; plim1[1] = 0.;
	plim2[0] =  0.; plim2[1] = 0.;
	plim3[0] =  0.; plim3[1] = 0.;
	plim4[0] =  0.; plim4[1] = 0.;
/*
 * CMPLOT demonstrates MAPLOT drawing continental and political outlines
 *
 * Set up Maps.
 *
 *
 * Draw Continental, political outlines 
 */
	c_mapstc ("OU - OUTLINE DATASET SELECTOR","PO");
/*
 * Set up projection
 */
	c_maproj (proj,plat,plon,rota);
/*
 * If it's a satellite projection, choose a satellite distance
 */
	if (!strcmp(proj,"SV")) c_mapstr ("SA - SATELLITE DISTANCE",5.);
/*
 * Set limits of map
 */
	c_mapset ("MA",plim1,plim2,plim3,plim4);
/*
 * Draw map
 */
	c_mapdrw();
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Done.
 */
	return;
}
