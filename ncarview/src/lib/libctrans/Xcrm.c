/*
 *	$Id: Xcrm.c,v 1.18 1996-01-18 14:48:33 boote Exp $
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
#include 	<stdlib.h>
#include 	<math.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarg/c.h>
#include	"cgmc.h"
#include	"devices.h"
#include	"Xdefs.h"
#include	"Xcrm.h"
#include	"default.h"
#include	"ctrandef.h"

extern	Pixeltype 	max_colour;
extern	boolean		Color_ava;
extern	boolean		startedDrawing;

Pixeltype		Colortab[MAX_COLOR_SIZE];
int			Colordef[MAX_COLOR_SIZE];

static X11_ColorStatus	ColorStatus[MAX_COLOR_SIZE];
static boolean		XIndexes[MAX_COLOR_SIZE];

void X11_initColorTable()
{
	int	i;

	for(i=0;i<MAX_COLOR_SIZE;i++){
		Colordef[i] = -1;
		ColorStatus[i].ref_count = 0;
		XIndexes[i] = False;
	}
	return;
}

static void
free_colors()
{
	
	Pixeltype	free_list[MAX_COLOR_SIZE];
	int		i;
	int		count = 0;

	for(i=0;i<MAX_COLOR_SIZE;i++)
		if(ColorStatus[i].ref_count > 0)
			free_list[count++] = ColorStatus[i].xpixnum;

	XFreeColors(dpy, Cmap, free_list, count, (unsigned long) 0);
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

/*
 * On return - sindx == -1 means we don't own any colors - shouldn't ever happen
 *	       sindx == -2 means we allocated a new xindex for the color
 *	       sindx > -1 means use the ColorStatus record indexed by sindx
 *	       color is filled with the correct x pixel number, and if
 *		a new color is allocated, it also has the red,green,blue
 *		members filled.
 */
static int
AllocColor(color,sindx)
	XColor	*color;
	int	*sindx;
{
	int	i,j;
	XColor	colors[MAX_COLOR_SIZE];
	float	color_error = 0.0;
	int	status = 0;
	int	minindx = -1;
	float	minval = 0;
	float	curval;
	float	tfloat;

	if(!RoCmap){
		/*
		 * Find unused X pixel - start from top of map, hopefully
		 * this will help minimize flashing...
		 */
		for(i=MAX_COLOR_SIZE-1;i>=0;i--)
			if(!XIndexes[i])
				break;

		color->pixel = i;
		XStoreColor(dpy,Cmap,color);
	}
	else if(XAllocColor(dpy,Cmap,color)){
		;
	}
	else if(!MyCmap && ColorModel == CM_MIXED){
		switch(bestVisual->class){
			/*
			 * RO
			 */
			case TrueColor:
			case StaticColor:
			case StaticGray:

				Cmap = XCopyColormapAndFree(dpy,Cmap);
				RoCmap = True;

				break;

			/*
			 * RW
			 */
			default:

				for(i=0;i<MAX_COLOR_SIZE;i++){
					colors[i].pixel = i;
					colors[i].flags = DoRed|DoBlue|DoGreen;
					colors[i].pad = '\0';
				}

				XQueryColors(dpy,Cmap,colors,MAX_COLOR_SIZE);
				free_colors();
				Cmap = XCreateColormap(dpy,
					RootWindow(dpy,DefaultScreen(dpy)),
					bestVisual,AllocAll);
				XStoreColors(dpy,Cmap,colors,MAX_COLOR_SIZE);
				RoCmap = False;

				break;
		}
		MyCmap = True;

		if(win != 0)
			XSetWindowColormap(dpy,win,Cmap);
		return AllocColor(color,sindx);
	}
	else{
		/*
		 * unable to allocate a new color cell.
		 * Use closest one of our current colors.
		 */

		for(j=0;j<MAX_COLOR_SIZE;j++){
			if(ColorStatus[j].ref_count > 0){
				tfloat = color->red;
				tfloat -= ColorStatus[j].red;
				curval = (tfloat * tfloat);
				tfloat = color->green;
				tfloat -= ColorStatus[j].green;
				curval += (tfloat * tfloat);
				tfloat = color->blue;
				tfloat -= ColorStatus[j].blue;
				curval += (tfloat * tfloat);

				/*SUPPRESS766*/
				if((minindx == -1)||(curval < minval)){
					minval = curval;
					minindx = j;
				}
			}
		}

		if(minindx > -1){
			/*
			 * This is the color to use.
			 */
			color->pixel = ColorStatus[minindx].xpixnum;
			*sindx = minindx;
			color_error = minval;
		}
		else{
			/*
			 * We don't own any colors?!?!
			 */
			color->pixel = WhitePixel(dpy,DefaultScreen(dpy));
			*sindx = -1;
			tfloat = color->red;
			tfloat -= X_MAX_RGB;
			color_error = (tfloat * tfloat);
			tfloat = color->green;
			tfloat -= X_MAX_RGB;
			color_error += (tfloat * tfloat);
			tfloat = color->blue;
			tfloat -= X_MAX_RGB;
			color_error += (tfloat * tfloat);
		}
	}

	if(ColorErr > 0.0 && color_error > ColorErr){
		tfloat = (float)sqrt(color_error);
		ESprintf(E_UNKNOWN,
		"Using a color that is %%%d different than requested",
			(int)(tfloat*(float)100/(float)X_MAX_INTEN_DIST));
		status = -1;
	}

	return status;
}

static int
DoColor(i)
	int	i;
{
	static	XColor	color = {
		0,0,0,0,(DoRed | DoGreen | DoBlue), '\0'
		};
	int	sindx = -2;
	int	status = 0;
	int	j;

	/*
	 * convert ctrans rgb values to X rgb values
	 */

	rgb_2_Xrgb(COLOUR_INDEX_RED(i),
		COLOUR_INDEX_GREEN(i),
		COLOUR_INDEX_BLUE(i),
		&color
		);

	/*
	 * Free this color if we already have it allocated.
	 */
	if(Colordef[i] > -1){
		if(color.red == ColorStatus[Colordef[i]].red &&
			color.green == ColorStatus[Colordef[i]].green &&
			color.blue == ColorStatus[Colordef[i]].blue){
			return status;
		}

		ColorStatus[Colordef[i]].ref_count--;

		if(ColorStatus[Colordef[i]].ref_count < 1){
			if(RoCmap){
			XFreeColors(dpy,Cmap,
			&ColorStatus[Colordef[i]].xpixnum,1,0);
			}
			XIndexes[ColorStatus[Colordef[i]].xpixnum]--;
		}
		Colordef[i] = -1;
	}

	status = AllocColor(&color,&sindx);
	if(sindx == -2){
		/*
		 * Find an empty "status" record.
		 */
		for(j=0;j<MAX_COLOR_SIZE;j++)
			if(ColorStatus[j].ref_count == 0)
				break;
		sindx = j;
		/*
		 * Colordef points to the "status" record used for this cgm
		 * cmap index
		 * Colortab points to just the xpixnum value for this cgm cmap
		 * index
		 * XIndexes is used to keep track of allocated X cmap indices
		 */
		ColorStatus[sindx].ref_count = 1;
		ColorStatus[j].red = color.red;
		ColorStatus[j].green = color.green;
		ColorStatus[j].blue = color.blue;
		ColorStatus[j].xpixnum = color.pixel;
		XIndexes[color.pixel]++;
	}
	else if(sindx > -1){
		ColorStatus[sindx].ref_count++;
	}
	Colordef[i] = sindx;
	Colortab[i] = color.pixel;

	return status;
}

int	X11_UpdateColorTable_()
{

	Pixeltype	pixel;
	int		i;
	int		status = 0;
	int		lstatus = 0;

	/* see if device supports colour	*/
	if (!Color_ava)
		return (status);		/* punt!	*/

	/*
	 * any time we change the colour table we "damage" the colour
	 * attributes
	 */
	FILL_COLOUR_DAMAGE = TRUE;
	MARKER_COLOUR_DAMAGE = TRUE;
	LINE_COLOUR_DAMAGE = TRUE;

	/*	
	 * load the colours from the ctrans color map into the X
	 * server's color map
	 */

	/*
	 * background is a special case...
	 */
	if(COLOUR_INDEX_DAMAGE(0)){

		if(!ignoreBGChanges){
			if (startedDrawing) {
				ESprintf(E_UNKNOWN,
		"Background color changes ignored after drawing has begun");
			}
			else{
				status = DoColor(0);
				if(win != 0){
				XSetWindowBackground(dpy,win,Colortab[0]);
				XClearWindow(dpy,win);
				}
			}
		}
		COLOUR_TOTAL_DAMAGE--;
		COLOUR_INDEX_DAMAGE(0) = FALSE;
	}
	for(i=1; COLOUR_TOTAL_DAMAGE > 0 && i<=MAX_C_I; i++) {
		if(COLOUR_INDEX_DAMAGE(i)) {

			lstatus = DoColor(i);
			status = MIN(lstatus,status);
			COLOUR_TOTAL_DAMAGE--;
			COLOUR_INDEX_DAMAGE(i) = FALSE;

		}	/* if	*/ }	/* for	*/

	return (status);
}
