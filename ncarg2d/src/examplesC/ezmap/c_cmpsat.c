/*
 *	$Id: c_cmpsat.c,v 1.1 1994-07-20 17:06:02 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn off the clipping indicator.
 */
	gset_clip_ind(GIND_NO_CLIP);
/*
 * International outlines
 */
	c_mapstc ("OU - OUTLINE DATASET SELECTOR","PO");
/*
 * Satellite-view.
 */
	c_maproj ("SV",40.,10.,0.);
	c_mapstr ("SA - SATELLITE DISTANCE",2.);
	c_mapstr ("S1 - SATELLITE ANGLE 1",10.);
	c_mapstr ("S2 - SATELLITE ANGLE 2",15.);
	c_mapdrw();

	c_frame();
/*
 * Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}
      
