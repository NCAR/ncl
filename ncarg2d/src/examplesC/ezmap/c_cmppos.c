/*
 *	$Id: c_cmppos.c,v 1.1 1994-07-20 17:06:01 haley Exp $
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
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Draw map in lower lefthand corner of the window
 */
	c_mappos (0.5, 1.0, 0.0, 0.5);
	c_mapdrw();
/*
 * Draw a perimeter around the viewport
 */
	gsel_norm_tran(0);
	c_plotif (0.0,0.0,0);
	c_plotif (1.0,0.0,1);
	c_plotif (1.0,1.0,1);
	c_plotif (0.0,1.0,1);
	c_plotif (0.0,0.0,1);

	c_frame();
/*
 * Deactivate and close the workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}
