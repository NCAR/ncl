/*
 *	$Id: Xcrm.c,v 1.2 1991-01-09 11:08:07 clyne Exp $
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
#include	"cgmc.h"
#include	"Xdefs.h"
#define		XCRM
#include	"Xcrm.h"

extern	Pixeltype 	max_colour;
extern	Display		*dpy;
extern	Colormap	Cmap;
extern	char		*malloc();

free_colors()
{
	
	Pixeltype	*free_list = NULL;
	int	i;
	int	count = 0;

	extern	Pixeltype	Colortab[];
	extern	boolean		Colordef[];

	free_list = (Pixeltype *) malloc 
			((unsigned) MAX_COLOR_SIZE * sizeof(Pixeltype));


	for (i = 0; i < MAX_COLOR_SIZE; i++) {
		if (Colordef[i]) {
			free_list[i] = Colortab[i];
			count++;
		}
	}	

	XFreeColors(dpy, Cmap, free_list, count, (unsigned long) 0);

	if (free_list) cfree((char *) free_list);
}

/*	CGMrgb_2_Xrgb
 *	
 *		convert a CGM specified r,g,b to their coresponding
 *	X r,g,b values
 */

CGMrgb_2_Xrgb(CGMcolor, Xcolor)
	CDtype	CGMcolor;
	XColor	*Xcolor;

{
		Xcolor->red = (unsigned short)	
			(((float) CGMcolor.red / max_colour) 
			* X_MAX_RGB);

		Xcolor->green = (unsigned short)	
			(((float) CGMcolor.green / max_colour) 
			* X_MAX_RGB);

		Xcolor->blue = (unsigned short)	
			(((float) CGMcolor.blue / max_colour) 
			* X_MAX_RGB);
}
