/*
 *	$Id: X11_class3.c,v 1.10 2008-07-27 03:18:42 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*	X11_class3.c
 *
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *
 *	This file contain the functions that implement class 3 
 *	CGM elements. The supported elements are VDC INTEGER PRECISION,
 *	CLIP RECTANGLE and CLIP INDICATOR. Most elements of class 3
 *	type merely set defaults and are thus handled by the generic
 *	default table in "default.c" 
 */
/*LINTLIBRARY*/



#include <stdio.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <ncarg/c.h>
#include "default.h"
#include "cgmc.h"
#include "Xdefs.h"
#include "ctrandef.h"
#include "translate.h"

static	XRectangle	devExtentRectangle = {0,0,0,0};

/* Class 3 */




/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_AuxColr(c)
CGMC *c;
{

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*
 *	GCSetClipExtent
 *	[exported]
 *
 *	Inform the clipping module of the extent of the device coordinate
 *	system. GCSetClipExtent() should be called whenever the device
 *	extent changes. It must be called *before* any calls to GCsetclipping
 *	are made.
 *
 * on entry;
 *	ulx		: upper left x coordinate of device.
 *	uly		: upper left y coordinate of device.
 *	lrx		: lower right x coordinate of device.
 *	lry		: lower right y coordinate of device.
 */
GCSetClipExtent(ulx, uly, lrx, lry)
	int	ulx, uly, lrx, lry;
{
	devExtentRectangle.x = ulx;
	devExtentRectangle.y = uly;
	devExtentRectangle.width = lrx - ulx + 1;
	devExtentRectangle.height = lry - uly + 1;
	
	GCsetclipping();
}


/*
 *	GCSetClipping
 *	[exported]
 *
 *	Set the clipping rectangle for subsequent drawing. The clip rectangle
 *	is defined by the current metafile
 */
GCsetclipping()
{
	int	ulx, uly, lrx, lry;		/* origin of rectangle	*/

	XRectangle rectangle;

	rectangle = devExtentRectangle;

	if (CLIPFLAG) {
		int r_lrx, r_lry;

		/*
		 * CGM's coodinate origin is the lower left corner, X's
		 * is the upper left corner
		 */
		ulx = XConvert(CLIPXMIN);
		uly = YConvert(CLIPYMAX);
		lrx = XConvert(CLIPXMAX);
		lry = YConvert(CLIPYMIN);

		/*
		 * clipped rectangle should be intersection of device
		 * extent (corrected for aspect ratio) and desired clipping
		 * rectangle
		 */
		r_lrx = rectangle.x + rectangle.width - 1;
		r_lry = rectangle.y + rectangle.height - 1;

		rectangle.x = rectangle.x < ulx ? ulx : rectangle.x;
		rectangle.y = rectangle.y < uly ? uly : rectangle.y;
		r_lrx = r_lrx > lrx ? lrx : r_lrx;
		r_lry = r_lry > lry ? lry : r_lry;

		rectangle.width = r_lrx - rectangle.x + 1;
		rectangle.height = r_lry - rectangle.y + 1;
	}

	XSetClipRectangles(dpy, lineGC,0,0,&rectangle, 1, Unsorted);
	XSetClipRectangles(dpy, markerGC,0,0,&rectangle, 1, Unsorted);
	XSetClipRectangles(dpy, polygonGC,0,0,&rectangle, 1, Unsorted);
	XSetClipRectangles(dpy, cellGC, 0, 0, &rectangle, 1, Unsorted);

}
