/*
 *	$Id: xddi.h,v 1.1 1994-03-30 02:11:40 fred Exp $
 */
/*
 *      File:		xddi.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed May  1 17:49:30 MDT 1991
 *
 *      Description:	This file defines the device dependent structure for
 *			gksc.ddp field for an X11 device
 */

#ifndef	_xddi_
#define	_xddi_

#include "x.h"
#include "gks.h"
#include "common.h"
#include "transform.h"

typedef	struct	Xddi_	{
	Display	*dpy;
	Window	win;
	int	width, height;
	unsigned 	dim;
	Transform2D	transform;
	GC	line_gc,
		marker_gc,
		text_gc,
		fill_gc,
		cell_gc,
		bg_gc;
	Boolean	color_ava;
	Pixeltype	color_pal[MAX_COLORS];
	Colormap	cmap;
	int	marker_type,
		marker_size;
	int	fill_style,
		hatch_index;
	TransSystem	tsystem;
	} Xddp;

#endif	/*	_xddi_	*/
