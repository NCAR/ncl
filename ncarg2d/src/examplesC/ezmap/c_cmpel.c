/*
 *	$Id: c_cmpel.c,v 1.1 1994-07-20 17:05:51 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	extern void cmpel();
/*
 * Open GKS and turn off clipping.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Call the routine which does all the work
 */
	cmpel();
/*
 * Close GKS, and end program
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void cmpel()
{
/*
 * Satellite-view.
 *
 * Do this plot in white with Softfill over the water and no lat/lon
 * lines
 */
	c_maproj ("SV",40.,10.,0.);
	c_mapstr ("SA - SATELLITE DISTANCE",5.);
	c_mapstc ("OU - OUTLINE DATASET SELECTOR","PO");
	c_mapsti ("PE - PERIMETER FLAG", 0);
	c_mapsti ("EL - ELLIPTICAL-PERIMETER SELECTOR", 1);
	c_mapint();
	c_maplbl();
	c_maplot();
	c_frame();

	return;
}

