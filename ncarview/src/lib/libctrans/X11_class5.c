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

static	XColor	color = {
		0,0,0,0,(DoRed | DoGreen | DoBlue), '\0'
		};

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
Ct_err	X11_ColrTable(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_ColrTable\n");
#endif DEBUG

	int	i,
		index;
	Pixeltype	planedummy[1];		/* not used	*/
	Pixeltype	pixel_return[1];	/* device index	*/
	Pixeltype	pixel;
	long		dummy = 0;

	Ct_err	BackColr();

	/* see if device supports colour	*/
	if (!Color_ava)
		return (OK);		/* punt!	*/


	/*
	 * any time we change the colour table we "damage" the colour
	 * attributes
	 */
	FILL_COLOUR_DAMAGE = TRUE;
	MARKER_COLOUR_DAMAGE = TRUE;
	LINE_COLOUR_DAMAGE = TRUE;

	/* 
	 *	see what type of visual we have (read only or read/write)
	 */

	if ((visual->class == TrueColor) || (visual->class == StaticColor)
		|| (visual->class == StaticGray)) {


		/*	load the colours from the cgmc into the colour map */
		for(i=0,index=c->ci[0];index<(c->ci[0]+c->CDnum);i++,index++) {


		/* convert CGM rgb values to X rgb values	*/
		CGMrgb_2_Xrgb(c->cd[i], &color);
		
                if (!XAllocColor(dpy, Cmap, &color)) {

			/* error allocating color cell	*/
			ct_error(NT_CAE,"");
                        return (pre_err);
		}

		Colortab[index] = color.pixel;
		Colordef[index] = TRUE;

		}

		/*
		 * This is a hack to ensure background color gets set correctly
		 * in the case that colr table index 0 is changed *and* no
		 * coresponding CGM BACKGROUND COLOUR is received
		 */
		if (index == 0) (void) BackColr(c);
		return (OK);
	}
 
	/*	
	 * load the colours from the cgmc into the colour map 
	 */
	for (index=c->ci[0], i = 0 ;index< (c->ci[0] + c->CDnum); index++,i++) {
		/*
		 * if this index has not had a cell allocated to it previously
		 * we need to do it now.
		 */
		if (! Colordef[index]) {
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
			Colortab[index] = pixel_return[0];
			Colordef[index] = TRUE;
		}

		/* 
		 *	set cell index in the colour map
		 */
		color.pixel = Colortab[index];

		/* 
		 *	convert CGM rgb values to X rgb values
		 */
		CGMrgb_2_Xrgb(c->cd[i], &color);
 
		/* 
		 *	store the colour in the map
		 */
		XStoreColor(dpy, Cmap, &color);
		if (index == 0) (void) BackColr(c);
	}
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

#define		COL_2_ALLOC	5

Ct_err	init_color(fg, bg, bd)
	Pixeltype	*fg, *bg, *bd;
{

	int	i;
	Pixeltype	planedummy[1];		/* not used	*/
	Pixeltype	pixel_return[1];	/* device index	*/
	static	char	*name[] = {"black", "white", "red", "green", "blue"};



	/*
	 *	get default visual that describes colourmap
 	 *	See Section 3.1.
	 */
	visual = DefaultVisual(dpy, DefaultScreen(dpy));	
	DspDepth = DisplayPlanes(dpy, DefaultScreen(dpy));

	if (DspDepth == 1) {

		/* one plane monochrome display	*/

		*fg = WhitePixel(dpy, DefaultScreen(dpy));
		*bd = WhitePixel(dpy, DefaultScreen(dpy));
		*bg = BlackPixel(dpy, DefaultScreen(dpy));

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
	 *	allocate some default colours	
	 */
	for (i=0; i < COL_2_ALLOC; i++) {

		if (!XParseColor(dpy, Cmap, name[i], &color))  {
			/* color name s not in database	*/
			ct_error(NT_CAE,name[i]);
		}
		else {

			/*
			 * see if have read/write color model
			 */
			if ((visual->class == DirectColor)
				|| (visual->class == PseudoColor)
				|| (visual->class == GrayScale)) {
				
				if (XAllocColorCells(dpy,Cmap,FALSE, planedummy,
                                        0, pixel_return, 1) == 0) {

					/* 
					 * Failed. try read only color model
					 */
					if (visual->class == DirectColor)
						visual->class = TrueColor;
					else if (visual->class == PseudoColor)
						visual->class = StaticColor;
					else if (visual->class == GrayScale)
						visual->class = StaticGray;
				}
				else {
					color.pixel = pixel_return[0];
					/*
					 *      store the colour in the map
					 */
					XStoreColor(dpy, Cmap, &color);

				}

			}
			/*
			 * read only color model
			 */
			if ((visual->class == TrueColor)
				|| (visual->class == StaticColor)
				|| (visual->class == StaticGray)) {



				if (!XAllocColor(dpy, Cmap, &color)) {
					ct_error(NT_CAE,"");
					return(SICK);
				}

			}

			/* 
			 * record colour in the colour table
			 */
			Colortab[i] = color.pixel;
			Colordef[i] = TRUE;
		}
	}

	/*
	 * set default foreground, background and border colour
	 */
	*bg = Colortab[0];
	*fg = *bd =  Colortab[1];

	return (OK);
}



