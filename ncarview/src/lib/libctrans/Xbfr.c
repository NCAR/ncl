/*
 *	$Id: Xbfr.c,v 1.3 1991-03-12 17:34:37 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*	Xbrf.c:
 *
 *	Author		John Clyne
 *
 *	Date		Fri Oct 13 15:17:43 MDT 1989
 *
 *		This file contains the X11 pixmap driver. These routines
 *	are used to create a pixmap, as opposed to drawing in a X11 window,
 *	and dump it to standard out
 *	
 */
/*LINTLIBRARY*/

#define	APP_DEF		"/usr/lib/X11/app-defaults/"

#include <stdio.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include        <X11/Xresource.h>
#include	<ncarv.h>
#include	<cterror.h>
#include	"cgmc.h"
#include	"default.h"
#include	"Xdefs.h"
#include	"ctrandef.h"
#include	"translate.h"
#define BORDER		1
#define BORDERWIDTH	0
#define	LOGO	"NCAR Graphics"

extern	char	*strcpy();
extern	char	*strcat();
extern	char	*strncpy();
extern	char	*getenv();
extern	boolean	stand_Alone;
extern	boolean	deviceIsInit;
extern	boolean	Batch;
extern	char	*program_name;
extern	Colormap	Cmap;
extern	boolean	*softFill;
extern	boolean	*bellOff;

extern	Ct_err	init_color();
extern	Ct_err	init_polygon();
extern	Ct_err	set_background_colr();

static	Pixeltype fg, bg, bd;		/* Pixel values 		*/
static  XSizeHints  xsh;                /* geometry for the pixmap	*/
static	Pixmap	pixmap;			/* the pixmap			*/
static	GC	bgGC;			/* the background color gc	*/


static  CoordModifier   dev_coord_mod = {0,0,1.0,1.0};


static	struct  {
	StringType_     Geometry;
	} commLineOpt;

static  Option  options[] =  {
	{"geometry", StringType,
		(unsigned long) &commLineOpt.Geometry, sizeof (StringType_ )},
	{NULL},
};

extern	int	lineWidthScale;

/*
 *	The class 0 CGM element functions for xbfr
 */
/*ARGSUSED*/
Ct_err	xbfr_BegMF(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_BegMF\n");
#endif DEBUG

	char	*dpy_name;

	CoordRect       dev_extent;

	Ct_err	X11_EndMF();

	if (deviceIsInit) {
		(void) X11_EndMF(c);
	}

	/*
	 *	in stand_alone mode the X driver is responcible for
	 *	making the connection to the X server and opening a pixmap.
	 *	Otherwise, the interface must provide these things. "dpy"
	 *	and "win" must be set by the interface in this case
	 */
	if (stand_Alone) {

		/*
		 *      parse X11 specific command line args
		 *      (currently only geometry accepted       )
		 */
		getOptions((caddr_t) 0, options);



		/*
		 *	establish connection to sever
		 */
		if ((dpy_name = getenv("DISPLAY")) == NULL) {
			ct_error(T_X11DEVNS,"");
			return(DIE);
		}


		if ((dpy = XOpenDisplay(dpy_name)) == NULL) {
			ct_error(T_CNOD, dpy_name);
			return (DIE);
		}
	}




	/*
	 * init the color module
	 */
	(void) init_color(&fg, &bg, &bd);


	if (stand_Alone) {

		/*
		 *      Initialize the Resource manager. See 10.11
		 */
		XrmInitialize();

		/*
		 *      provide pixmap with size. Fill out
		 */
		do_geometry(commLineOpt.Geometry, &xsh);

	}	/* if stand_Alone	*/


	/*
	 * Create the pixmap
	 */
	if (!(drawable = XCreatePixmap(dpy, RootWindow(dpy,DefaultScreen(dpy)), 
				xsh.width, xsh.height, DspDepth))){

		ct_error(T_MALLOC, "more memory, FEED ME SEYMORE");
		return(DIE);
	}
	pixmap = drawable;

	/* 
	 * 	create default graphics contexts for the pixmap. 
	 *	See Section 5.3.	
	 * 	Use defaults except for backround, backround and line width.
	 */

	gcv.background = bg;
	gcv.foreground = fg;

	lineGC = XCreateGC(dpy, drawable,
			(GCForeground | GCBackground ), &gcv);

	markerGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);

	polygonGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);

	cellGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);



	/*	tile GC has foreground and backround reversed	*/
	gcv.background = fg;
	gcv.foreground = bg;
	tileGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);
	/*
	 * a gc for drawing in the background since X does not support
	 * a clear window function or set background function for pixmaps
	 */
	bgGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);


	/*
	 *	intitialize polygon fill stuff
	 */
	if (init_polygon() != OK) {
		return(pre_err);
	}

	/*
	 * initialize the virtual device coordinate translation macros
	 */
	dev_extent.llx = dev_extent.ury = 0;
	dev_extent.lly = xsh.height - 1;
	dev_extent.urx = xsh.width - 1;

	transinit(&dev_extent, dev_coord_mod, TRUE);
	GCSetClipExtent((short) dev_extent.llx, (short) dev_extent.ury,
			(short) dev_extent.urx, (short) dev_extent.lly);



	/*
	 * if software simulation of polygon filling is desired
	 * initialize soft sim module with new height and width
	 */
	if (*softFill) {
		initSoftSim(xsh.height, xsh.width);
	}

	/*
	 * this is a hack to draw fatter lines when the resolution is
	 * real high so we can see them.
	 */
	lineWidthScale = (xsh.width / 1024) + 1;

	deviceIsInit = TRUE;
	LINE_WIDTH_DAMAGE = TRUE;

	return(OK);
}

/*ARGSUSED*/
Ct_err	xbfr_BegPic(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_BegPic\n");
#endif DEBUG

	extern	boolean	startedDrawing;

	startedDrawing = FALSE;


	/*
	 *	copy default table to working default table	
	 *	most of the CGM elements contain output attribute or
	 *	input processing information. This data is stored in a 
	 *	table in "default.h". SetInPic keeps the data up to date
	 *	for each new frame
	 */
	SetInPic((boolean) TRUE);

	return(OK);
}

/*ARGSUSED*/
Ct_err	xbfr_BegPicBody(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_BegPicBody\n");
#endif DEBUG

	COtype	back_color;

	/*	
	 *	check to see if any CGM picture descriptor elements 
	 *	were changed/received
	 */

	extern	Ct_err	GCsetcolor();

	/*
	 * this needs to be fixed to test and make sure that the 
	 * color has been allocated for index 0
	 */
	back_color.index = 0;
	(void) GCsetcolor(back_color, bgGC);

	/*
	 *	clear the pixmap by drawing a filled rectangle the size of 
	 *	the pixmap in the background colour
	 */
	XFillRectangle(dpy, drawable, bgGC, 0, 0, xsh.width+1, xsh.height+1);
	return (OK);
}

/*ARGSUSED*/
Ct_err	xbfr_EndPic(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_EndPic\n");
#endif DEBUG

	/*
	 *	clear default table
	 */
	(void) SetInPic((boolean) False);

	/*
	 *	if not interactive don't perform any user interaction
	 */
	if (Batch) {
		XFlush(dpy);
		return(OK);
	}

	/* 
	 *	dump the pixmap to stdout
	 */
	(void) pixmap_dump(dpy, drawable, 0, 0, 
			(unsigned) xsh.width, (unsigned) xsh.height, 
			Cmap, visual, ZPixmap, stdout);

	if (! *bellOff) XBell(dpy, 0);
	return(OK);

}
