/*
 *	$Id: c_cmpmsk.c,v 1.1 1994-07-20 17:05:59 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1
#define LMAP   150000
#define NWRK   1000
#define ISIZ   5

main()
{
	float plim1[2], plim2[2], plim3[2], plim4[2];
	extern void cmpmsk();

	plim1[0] =  0.; plim1[1] = 0.;
	plim2[0] =  0.; plim2[1] = 0.;
	plim3[0] =  0.; plim3[1] = 0.;
	plim4[0] =  0.; plim4[1] = 0.;
/*
 * Open GKS, Turn Clipping off
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	cmpmsk("SV",40.,-50.,0.,"PO","MA", plim1,plim2,plim3,plim4,10.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}  

void cmpmsk(proj, plat, plon, rota, outln, jlim, plim1, plim2, plim3, plim4, grd)
char *proj, *outln, *jlim;
float plat, plon, rota, grd;
float *plim1, *plim2, *plim3, *plim4;
{
	extern int mask();
      
	int i, map[LMAP], iarea[ISIZ], igrp[ISIZ];
	float xwrk[NWRK], ywrk[NWRK];
/*
 * CMPLOT demonstrates MAPLOT drawing continental and political outlines
 *
 * Use solid lines for grid
 */
	i = 65535;
	c_dashdb(&i);
/*
 * Draw Continental, political outlines 
 */
	c_mapstc ("OU - OUTLINE DATASET SELECTOR",outln);
/*
 * Set grid spacing
 */
	c_mapstr ("GR - GRID SPACING",grd);
/*
 * Set up projection
 */
	c_maproj (proj,plat,plon,rota);
/*
 * If it's a satellite projection, choose a satellite distance
 */
      if (!strcmp(proj,"SV")) c_mapstr ("SA - SATELLITE DISTANCE",7.);
/*
 * Set limits of map
 */
	c_mapset (jlim,plim1,plim2,plim3,plim4);
/*
 * Initialize Maps and Areas
 */
	c_mapint();
	c_arinam (map,LMAP);
	c_mapbla (map);
/*
 * Draw Masked Grid Lines
 */
	c_mapgrm (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, mask);
/*
 * Draw Continental Outlines and Elliptical Perimeter
 */
	c_mapsti("LA - LABEL FLAG",0);
	c_mapsti("EL - ELLIPTICAL-PERIMETER SELECTOR",1);
	c_maplbl();
	c_maplot();
/*
 * Done.
 */
	return;
}

int mask(xc,yc,mcs,areaid,grpid,idsize)
float *xc, *yc;
int *mcs, *areaid, *grpid, *idsize;
{
	int id, i;
/*
 * Retrieve area id for geographical area
 */
	for( i = 0; i < *idsize; i++ ) {
		if (grpid[i] == 1) id=areaid[i];
	}
/*
 * If the line is over water, and has 2 or more points draw it.
 */
	if ((c_mapaci(id) == 1) && (*mcs >= 2)) {
		c_curved(xc,yc,*mcs);
	}
/*   
 * Otherwise, don't draw the line - mask it.
 */
	return(0);
}
