/*
 *	$Id: xddi.h,v 1.2 1994-06-08 16:58:03 boote Exp $
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

typedef	struct Xddi_ColorStatus_ {
	int		ref_count;
	unsigned short	red,green,blue;
	Pixeltype	xpixnum;
} XddpColorStatus;

typedef	struct	Xddi_	{
	XWorkType	xwtype;
	Display		*dpy;
	Screen		*scr;
	Visual		*vis;
	Window		win;
	unsigned int	depth;
	int		width, height;
	unsigned 	dim;
	Transform2D	transform;
	GC		line_gc,
			marker_gc,
			text_gc,
			fill_gc,
			cell_gc,
			bg_gc;
	Boolean		color_ava;
	Pixeltype	color_pal[MAX_COLORS];
	int		color_info[MAX_COLORS];
	XddpColorStatus	color_status[MAX_COLORS];
	Colormap	cmap;
	Boolean		cmap_ro;
	Boolean		mycmap;
	int		mycmap_cells;
	int		marker_type,
			marker_size;
	int		fill_style,
			hatch_index;
	TransSystem	tsystem;
	int		percent_colerr;
	float		pcerr_sqr;
} Xddp;

#endif	/*	_xddi_	*/
