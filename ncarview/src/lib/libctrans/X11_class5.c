/*
 *	$Id: X11_class5.c,v 1.9 1992-04-03 20:40:18 clyne Exp $
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
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarv.h>
#include	"Xdefs.h"
#include	<cterror.h>
#include	"default.h"
#include	"cgmc.h"
#include	"Xcrm.h"


Colormap	Cmap;		/* current color map on screen	*/

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
Ct_err	X11_PatTable(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_PatTable\n");
#endif DEBUG

	return (OK);
}



/*ARGSUSED*/
Ct_err	X11_ASF(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_ASF\n");
#endif DEBUG

	return (OK);
}


/*	init_color: 	
 *
 *		intialize the color table and allocate default colours
 *
 *	on exit
 *		Cmap	: contains the color map
 *		Color_ava	: true if have a color display
 *		fg, bg, bd	: set to default colours as described in name
 */


Ct_err	init_color(foreground, background, reverse, fg, bg, bd)
	char		*foreground,
			*background;
	boolean		reverse;
	Pixeltype	*fg, *bg, *bd;
{

	int	i;
	char	*name[2];
	int	col_2_alloc;

	static	XColor	color = {
		0,0,0,0,(DoRed | DoGreen | DoBlue), '\0'
		};

	Ct_err	ColrTable();

	col_2_alloc = 0;

	if (foreground) name[col_2_alloc++] = foreground;
	if (background) name[col_2_alloc++] = background;

	


	/*
	 *	get default visual that describes colourmap
 	 *	See Section 3.1.
	 */
	visual = DefaultVisual(dpy, DefaultScreen(dpy));	
	DspDepth = DisplayPlanes(dpy, DefaultScreen(dpy));

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

		return (OK);
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


	Cmap = DefaultColormap(dpy, DefaultScreen(dpy));


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
		cgmc.ci = (CItype *) icMalloc(sizeof(CItype));
		cgmc.cd = (CDtype *) icMalloc(col_2_alloc * sizeof(CDtype));

		for (i=0; i < col_2_alloc; i++) {

			if (!XParseColor(dpy, Cmap, name[i], &color))  {
				/* color name s not in database	*/
				ct_error(NT_CAE,name[i]);
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
		cgmc.CInum = 1;;

		ColrTable(&cgmc);
		free ((char *) cgmc.ci);
		free ((char *) cgmc.cd);
	}

	/*
	 * load the default colors
	 */
	X11_UpdateColorTable_();
	COLOUR_TABLE_DAMAGE = FALSE;

	/*
	 * set default foreground, background and border colour
	 */
	*bg = Colortab[0];
	*fg = *bd =  Colortab[1];

	return (OK);
}
