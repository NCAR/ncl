/*
 *	$Id: w_icon.c,v 1.7 2008-07-27 03:18:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	w_icon.c
 *
 *	Author		John Clyne
 *
 *	Date		Tue Jul 23 16:28:25 MDT 1991
 *
 *	Create a icon pixmap and install it on the toplevel widget if the
 *	user hasn't already requested a icon pixmap. We imbed the code for
 *	the bitmap so we can avoid having to install bitmaps non standard
 *	bitmap directory
 */
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>


/*
 *	bitmap for default icon
 */
#include "earth.xbm"
static  struct  {
	int     width,
	height;
	unsigned char    *bits;
	Pixmap  pmap;
	} icon = {earth_width, earth_height, earth_bits, 0};

SetIconResource(toplevel)
	Widget	toplevel;
{

	Arg		args[5];
	Cardinal	n;
	/*
	 * Get icon pixmap if its been set.
	 */
	n = 0;
	XtSetArg(args[n], XtNiconPixmap, &icon.pmap);   n++;
	XtGetValues(toplevel, args, n);

	/*
	 * don't know if this is portable but there doesn't seem to be any
	 * other way
	 */
	if (icon.pmap == 0) {
		/*
		 * user hasn't specified a icon pixmap resource so use
		 * the default
	 	 */
		icon.pmap = XCreateBitmapFromData(
			XtDisplay(toplevel),
			RootWindowOfScreen(XtScreen(toplevel)),
			(char *) icon.bits, icon.width, icon.height
		);

		n = 0;
		XtSetArg(args[n], XtNiconPixmap, icon.pmap);    n++;
		XtSetValues(toplevel, args, n);
	}


}
