/*
 *	$Id: Xcrm.c,v 1.8 1992-02-07 17:38:44 clyne Exp $
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
/*	Xcrm.c:
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *				9/22/88
 *
 *		
 *	This is the X colour resource manager file. It contains an abstract 
 *	data type that maintains a list of colors found in a cgmc. The list 
 *	is used to prevent the redundant allocation of the same colors into 
 *	the color map. 
 */



#include 	<stdio.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<cterror.h>
#include	<ncarv.h>
#include	"cgmc.h"
#include	"devices.h"
#include	"Xdefs.h"
#define		XCRM
#include	"Xcrm.h"
#include	"default.h"

extern	Pixeltype 	max_colour;
extern	Display		*dpy;
extern	Colormap	Cmap;
extern	boolean		Colordef[];
extern	Pixeltype	Colortab[];
extern	boolean		Color_ava;

free_colors()
{
	
	Pixeltype	*free_list = NULL;
	int	i;
	int	count = 0;


	free_list = (Pixeltype *) icMalloc 
			((unsigned) (MAX_COLOR_SIZE * sizeof(Pixeltype)));


	for (i = 0; i < MAX_COLOR_SIZE; i++) {
		if (Colordef[i]) {
			Colordef[i] = FALSE;
			free_list[count] = Colortab[i];
			Colortab[i] = 0;
			count++;
		}
	}	

	XFreeColors(dpy, Cmap, free_list, count, (unsigned long) 0);

	if (free_list) cfree((char *) free_list);
}

/*	rgb_2_Xrgb
 *	
 *		convert a CGM specified r,g,b to their coresponding
 *	X r,g,b values
 */
static	rgb_2_Xrgb(red, green, blue, Xcolor)
	unsigned char	red,
			green,
			blue;
	XColor	*Xcolor;

{
		Xcolor->red = (unsigned short)	
			(((float) red / max_colour) * X_MAX_RGB);

		Xcolor->green = (unsigned short)	
			(((float) green / max_colour) * X_MAX_RGB);

		Xcolor->blue = (unsigned short)	
			(((float) blue / max_colour) * X_MAX_RGB);
}


X11_UpdateColorTable_()
{

	Pixeltype	planedummy[1];		/* not used	*/
	Pixeltype	pixel_return[1];	/* device index	*/
	int		i;

	static	XColor	color = {
		0,0,0,0,(DoRed | DoGreen | DoBlue), '\0'
		};

	void	back_color();

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
	 * This is a hack to ensure background color gets set correctly
	 * in the case that colr table index 0 is changed *and* no
	 * coresponding CGM BACKGROUND COLOUR is received
	 */
	if (COLOUR_INDEX_DAMAGE(0)) {
		rgb_2_Xrgb(
			COLOUR_INDEX_RED(0),
			COLOUR_INDEX_GREEN(0),
			COLOUR_INDEX_BLUE(0),
			&color
			);
		(void) back_color(color);
		
		COLOUR_INDEX_DAMAGE(0) = FALSE;
		COLOUR_TOTAL_DAMAGE--;
	}

	/* 
	 *	see what type of visual we have (read only or read/write)
	 */
	if ((visual->class == TrueColor) || (visual->class == StaticColor)
		|| (visual->class == StaticGray)) {


		/*	
		 * load the colours from the ctrans color map into the X
		 * server's color map
		 */
		for(i=1; COLOUR_TOTAL_DAMAGE > 0 && i<MAX_C_I; i++) {

		if (COLOUR_INDEX_DAMAGE(i)) {


			/*
			 * convert ctrans rgb values to X rgb values
			 */
		
			rgb_2_Xrgb(
				COLOUR_INDEX_RED(i),
				COLOUR_INDEX_GREEN(i),
				COLOUR_INDEX_BLUE(i),
				&color
				);
		
			if (!XAllocColor(dpy, Cmap, &color)) {

				/* error allocating color cell	*/
				ct_error(NT_CAE,"Too many colors allocated");
				return (pre_err);
			}

			Colortab[i] = color.pixel;
			Colordef[i] = TRUE;

			COLOUR_TOTAL_DAMAGE--;
			COLOUR_INDEX_DAMAGE(i) = FALSE;

		}	/* if	*/
		}	/* for	*/

		return (OK);
	}
 
	/*	
	 * load the colours from the cgmc into the colour map 
	 */
	for(i=1; COLOUR_TOTAL_DAMAGE > 0 && i<MAX_C_I; i++) {

		if (COLOUR_INDEX_DAMAGE(i)) {
			/*
			 * if this index has not had a cell allocated 
			 * to it previously  we need to do it now.
			 */
			if (! Colordef[i]) {
				/*
				 * try and alloc a new cell in the color map
				 */
				if (XAllocColorCells(dpy,Cmap,FALSE, planedummy,
					0, pixel_return, 1) == 0) {

					/* error allocating color cell	*/
					ct_error(
						NT_CAE,
						"Too many colors allocated"
						);
					return (SICK);
				}

				/* 
				 *	record pixel in the colortable
				 */
				Colortab[i] = pixel_return[0];
				Colordef[i] = TRUE;
			}

			/* 
			 *	set cell index in the colour map
			 */
			color.pixel = Colortab[i];

			/* 
			 *	convert CGM rgb values to X rgb values
			 */
			rgb_2_Xrgb(
				COLOUR_INDEX_RED(i),
				COLOUR_INDEX_GREEN(i),
				COLOUR_INDEX_BLUE(i),
				&color
				);
 
			/* 
			 *	store the colour in the map
			 */
			XStoreColor(dpy, Cmap, &color);
			COLOUR_TOTAL_DAMAGE--;
			COLOUR_INDEX_DAMAGE(i) = FALSE;
		}
	}	/* for	*/
	return (OK);
}


static	void	back_color(color)
	XColor	color;
	
{
	Pixeltype	pixel;
	Pixeltype	planedummy[1];
	Pixeltype	pixel_return[1];
	extern	boolean	startedDrawing;

	extern	struct	device	devices[];
	extern	int	currdev;

	if (! Color_ava) return;

	if (startedDrawing) {
		ct_error(NT_NULL,"Background color changes ignored after drawing has begun");
		return;
	}


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
				return;
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
			return;
		}
		Colortab[0] = color.pixel;
		Colordef[0] = TRUE;
	}

	/*
	 * background color needs to be handled differently for
	 * windows and pixmaps
	 */
	pixel = Colortab[0];
	if (strcmp("X11", devices[currdev].name) == 0) {
		XSetWindowBackground(dpy, win, pixel);
#ifdef	DEAD
		XClearWindow(dpy, win);
#endif
	}
}
