/*
 *	$Id: X11_class5.c,v 1.14 1993-02-04 19:32:45 clyne Exp $
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
/*	X11_class5.c
 *
 *
 *		Author		John Clyne	
 *
 *	This file contain the functions that implement class 5 
 *	CGM elements. The supported elements are: LINE BUNDLE INDEX, 
 *	LINE TYPE, LINE WIDTH, LINE COLOUR, MARKER BUNDLE INDEX, 
 *	MARKER TYPE, MARKER SIZE, MARKER COLOUR, TEXT BUNDLE INDEX, 
 *	TEXT FONT INDEX, TEXT PRECISION, CHARACTER EXPANSION FACTOR,
 *	CHARACTER SPACING, TEXT COLOUR, CHARACTER HEIGHT, CHARACTER 
 *	ORIENTATION, TEXT PATH TEXT ALIGNMENT, FILL BUNDLE INDEX,
 *	INTERIOR STYLE, FILL COLOUR, HATCH INDEX, PATTERN INDEX,
 *	FILL REFERENCE POINT, COLOUR TABLE, ASPECT SOURCE FLAGS. 
 *	These elements are primarily concerened
 *	with providing information necessary to plot the output primitives. 
 */
/*LINTLIBRARY*/



#include 	<stdio.h>
#include 	<stdlib.h>
#include 	<errno.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarg/c.h>
#include	"Xdefs.h"
#include	"default.h"
#include	"cgmc.h"
#include	"Xcrm.h"


Pixeltype	Colortab[MAX_COLOR_SIZE]; 	/* index into color map	*/
boolean		Colordef[MAX_COLOR_SIZE];	/* true if an index in Colortab
						 * has been allocated
						 */
boolean		Color_ava = FALSE;		/* true if device has color*/


Pixeltype	max_colour;			/* maximum r or g or b value
						 * specifiable in the CGM
						 */


/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_PatTable(c)
	CGMC *c;
{

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}



/*ARGSUSED*/
int	X11_ASF(c)
	CGMC *c;
{

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}


static	Visual	*get_best_8bit_visual(depth, dpy)
	int	*depth;
	Display	*dpy;
{
	XVisualInfo	vinfo;
	int	screen = DefaultScreen(dpy);


	/*
	 * find best 8-bit depth visual
	 */
	if (XMatchVisualInfo(dpy, screen, 8, PseudoColor, &vinfo)) {
		*depth = vinfo.depth;
		return(vinfo.visual);
	}
	else if (XMatchVisualInfo(dpy, screen, 8, StaticColor, &vinfo)) {
		*depth = vinfo.depth;
		return(vinfo.visual);
	}
	else if (XMatchVisualInfo(dpy, screen, 8, GrayScale, &vinfo)) {
		*depth = vinfo.depth;
		return(vinfo.visual);
	}
	else if (XMatchVisualInfo(dpy, screen, 8, StaticGray, &vinfo)) {
		*depth = vinfo.depth;
		return(vinfo.visual);
	}

	/*
	 * yuck, can't find anything. return the default
	 */
	*depth = DefaultDepth(dpy, screen);
	return (DefaultVisual(dpy, screen));
}

/*	init_color: 	
 *
 *		intialize the color table and allocate default colours
 * on entry
 *	*foreground	: Default foreground color name
 *	*background	: Default background color name
 *	do_pcmap	: If true create private colormap else use default.
 *	best_vis	: If true try to get the best visual, else use default.
 *
 * on exit
 *	Cmap		: contains the color map
 *	bestVisual	: the visual to use.
 *	DspDepth	: the display depth for this visual
 *	Color_ava	: true if have a color display
 *	fg, bg, bd	: set to default colours as described in name
 */


int	init_color(foreground, background, do_pcmap, reverse, best_vis,fg,bg,bd)
	char		*foreground,
			*background;
	boolean		do_pcmap;
	boolean		reverse;
	boolean		best_vis;
	Pixeltype	*fg, *bg, *bd;
{

	int	i;
	char	*name[2];
	int	col_2_alloc;
	int	status = 0;

	static	XColor	color = {
		0,0,0,0,(DoRed | DoGreen | DoBlue), '\0'
		};

	int	ColrTable();

	col_2_alloc = 0;

	if (background) name[col_2_alloc++] = background;
	if (foreground) name[col_2_alloc++] = foreground;

	


	/*
	 * get the visual
	 */
	if (best_vis) {
		bestVisual = get_best_8bit_visual(&DspDepth, dpy);
	}
	else {
		bestVisual = DefaultVisual(dpy, DefaultScreen(dpy));
		DspDepth = DefaultDepth(dpy, DefaultScreen(dpy));
	}

	if (DspDepth == 1) {

		/* one plane monochrome display	*/

		if (! reverse) {	/* if not reverse video	*/
			*fg = WhitePixel(dpy, DefaultScreen(dpy));
			*bd = WhitePixel(dpy, DefaultScreen(dpy));
			*bg = BlackPixel(dpy, DefaultScreen(dpy));
		}
		else {	/* reverse video	*/
			*fg = BlackPixel(dpy, DefaultScreen(dpy));
			*bd = BlackPixel(dpy, DefaultScreen(dpy));
			*bg = WhitePixel(dpy, DefaultScreen(dpy));
		}

		return (0);
	}


	/* 
	 *			colour display
	 *
	 *	We should be able to allocate "basic"  colours on any kind 
	 *	of colour system (visual type). So say Oriely and Associates.
	 */



	/*
	 * all output primitives will use Color_ava to see 
	 * if they have a colour display
	 */
	Color_ava = TRUE;

	/*
	 * if we are requested to create a new color map or we are not
	 * using the default visual we need to create our own color map
	 */
	if (do_pcmap || bestVisual != DefaultVisual(dpy, DefaultScreen(dpy))) {
		Cmap = XCreateColormap(
			dpy, RootWindow(dpy, DefaultScreen(dpy)), 
			bestVisual, AllocAll
		);
		privateCmap = TRUE;
	}
	else {
		Cmap = DefaultColormap(dpy, DefaultScreen(dpy));
		privateCmap = FALSE;
	}

	/* 
	 * find max direct colour, DCP is direct colour precision in the CGM
	 */
	max_colour = (1 << DCP) - 1;

	/* 
	 * 	initialize the color table to empty 
	 *	and mark all indexes as not defined
	 */
	for (i=0;i<MAX_COLOR_SIZE;i++) { 
		Colordef[i] = FALSE;
	}

	/*
	 * if the user requested that the default foreground and/or background
	 * colors be overriden do so now
	 */
	if (col_2_alloc) {
		CGMC	cgmc;
		CItype	ci_array[1];
		CDtype	cd_array[2];

		cgmc.ci = &ci_array[0];
		cgmc.cd = &cd_array[0];

		for (i=0; i < col_2_alloc; i++) {

			if (!XParseColor(dpy, Cmap, name[i], &color))  {
				/* color name s not in database	*/
				ESprintf(E_UNKNOWN,"XParseColor(,,%s,)",name[i]);
				status = -1;
			}
			cgmc.cd[i].red = color.red / X_MAX_RGB * max_colour;
			cgmc.cd[i].green = color.green / X_MAX_RGB* max_colour;
			cgmc.cd[i].blue = color.blue / X_MAX_RGB * max_colour;
		}
		cgmc.CDnum = col_2_alloc;

		if (background) {
			cgmc.ci[0] = 0;
		}
		else {
			cgmc.ci[0] = 1;
		}
		cgmc.CInum = 1;

		(void) ColrTable(&cgmc);
	}

	/*
	 * load the default colors
	 */
	if (X11_UpdateColorTable_() < 0) status = -1;
	COLOUR_TABLE_DAMAGE = FALSE;

	/*
	 * set default foreground, background and border colour
	 */
	*bg = Colortab[0];
	*fg = *bd =  Colortab[1];

	return (status);
}
