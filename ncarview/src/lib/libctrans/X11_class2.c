/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*	X11_class2.c
 *
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *
 *	This file contain the functions that implement class 2 
 *	CGM elements. The supported elements are COLOUR SELECTION MODE,
 *	VDC EXTENT and BACKROUND. These elements are primarily concerened
 *	with providing information necessary to interpret the data in the
 *	CGM. These elements have no direct effect but merely set defaults
 *	and are thus handeled by the generic default table in "default.c"
 */
/*LINTLIBRARY*/


#include <stdio.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarv.h>
#include	"cgmc.h"
#include	<cterror.h>
#include	"Xcrm.h"
#include	"Xdefs.h"
#include	"devices.h"

extern	struct	device	devices[];
extern	int	currdev;

Ct_err	set_background_colr(colr)
	CDtype	colr;
{
	Pixeltype	pixel;
	Pixeltype	planedummy[1];
	Pixeltype	pixel_return[1];
	extern	boolean	startedDrawing;
	XColor color;

	extern	Colormap	Cmap;
	extern	boolean		Colordef[];
	extern	Pixeltype	Colortab[];
	
	if (startedDrawing) {
		ct_error(NT_NULL,"Background color changes ignored after drawing has begun");
		return (SICK);
	}

	/*
	 *	convert CGM rgb values to X rgb values
	 */
	CGMrgb_2_Xrgb(colr, &color);

	/*
	 * see if color model is writeable
	 */
	if (visual->class == DirectColor || visual->class == PseudoColor 
		|| visual->class == GrayScale) {

		/*
		 * if the background color has not been set yet we need
		 * to allocate a cell for it.
		 */
		if (! Colordef[0]) {
			/*
			 * try and alloc a new cell in the color map
			 */
			if (XAllocColorCells(dpy,Cmap,FALSE, planedummy,
					0, pixel_return, 1) == 0) {

				/* error allocating color cell	*/
				ct_error(NT_CAE,"");
				return (SICK);
			}

			/* 
			 *	record pixel in the colortable
			 */
			Colortab[0] = pixel_return[0];
			Colordef[0] = TRUE;
		}

		/* 
		 *	set cell index in the colour map
		 */
		color.pixel = Colortab[0];

		/*
		 *	store the new background color in the cell
		 */
		XStoreColor(dpy, Cmap, &color);
	}
	else {	/* color model is read only	*/

		if (!XAllocColor(dpy, Cmap, &color)) {

			/* error allocating color cell  */
			ct_error(NT_CAE,"");
			return (pre_err);
		}
		Colortab[0] = color.pixel;
		Colortab[0] = TRUE;
	}

	/*
	 * background color needs to be handled differently for
	 * windows and pixmaps
	 */
	pixel = Colortab[0];
	if (strcmp("X11", devices[currdev].name) == 0) {
		XSetWindowBackground(dpy, win, pixel);
		XClearWindow(dpy, win);
	}

	return (OK);
}
