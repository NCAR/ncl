/*
 *	$Id: Xcrm.c,v 1.25 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

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
#include 	<errno.h>
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

extern	boolean		startedDrawing;


boolean			Color_ava = FALSE;	/* true if device has color*/
Pixeltype		max_colour;		/* maximum r or g or b value
						 * specifiable in the CGM
						 */
Pixeltype		Colortab[MAX_COLOR_SIZE];
int			Colordef[MAX_COLOR_SIZE];

static X11_ColorStatus	ColorStatus[MAX_COLOR_SIZE];
static int		*XIndexes = NULL;
static int		MaxXCol;
static boolean		XRefCount;

void X11_initColorTable()
{
	int	i;

	MaxXCol = 2;
	for(i=1;i<DspDepth;i++){
		MaxXCol*=2;
	}

	switch(bestVisual->class){
		case TrueColor:
		case StaticColor:
		case StaticGray:
		case DirectColor:
			XRefCount = False;
			break;
		default:
			XRefCount = True;
	}

	if(XRefCount){
		XIndexes = malloc(sizeof(int)*MaxXCol); 
		if(XIndexes)
			for(i=0;i<MaxXCol;i++)
				XIndexes[i] = 0;
		else{
			ESprintf(errno, "malloc(%d)", sizeof(int)*MaxXCol);
			XRefCount = False;
		}
	}

	for(i=0;i<MAX_COLOR_SIZE;i++){
		Colordef[i] = -1;
		ColorStatus[i].ref_count = 0;
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
		Xcolor->flags = DoRed|DoGreen|DoBlue;
		Xcolor->pad = '\0';
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
	XColor	*colors;
	float	color_error = 0.0;
	int	status = 0;
	int	minindx = -1;
	float	minval = 0;
	float	curval;
	float	tfloat;
	Colormap	new;

	if(!RoCmap && XRefCount){
		boolean	colordone = False;
		/*
		 * Find unused X pixel - start from top of map, hopefully
		 * this will help minimize flashing...
		 */
		for(i=MaxXCol-1;i>=0;i--){
			if(!XIndexes[i]){
				color->pixel = i;
				XStoreColor(dpy,Cmap,color);
				colordone = True;
				break;
			}
		}
		/*
		 * This goto will only happen in the most desperate situations.
		 */
		if(!colordone) goto COLORBUST;

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
			case DirectColor:

				Cmap = XCopyColormapAndFree(dpy,Cmap);
				RoCmap = True;

				break;

			/*
			 * RW
			 */
			default:

				new = XCreateColormap(dpy,
					RootWindow(dpy,DefaultScreen(dpy)),
					bestVisual,AllocAll);
				if(colors = malloc(sizeof(XColor)*MaxXCol)){
					for(i=0;i<MaxXCol;i++)
						colors[i].pixel = i;
					XQueryColors(dpy,Cmap,colors,MaxXCol);
					XStoreColors(dpy,new,colors,MaxXCol);
					free(colors);
				}
				/*
				free_colors();
				*/
				XFreeColormap(dpy,Cmap);
				Cmap = new;
				RoCmap = False;

				break;
		}
		MyCmap = True;

		if(win != None)
			XSetWindowColormap(dpy,win,Cmap);
		return AllocColor(color,sindx);
	}
	else{
COLORBUST:
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
			if(XRefCount)
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
		if(XRefCount)
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

/*	init_color: 	
 *
 *		intialize the color table and allocate default colours
 * on entry
 *	*foreground	: Default foreground color name
 *	*background	: Default background color name
 *
 * on exit
 *	Cmap		: contains the color map
 *	bestVisual	: the visual to use.
 *	DspDepth	: the display depth for this visual
 *	Color_ava	: true if have a color display
 *	fg, bg, bd	: set to default colours as described in name
 */


int	init_color(foreground,background,reverse,fg,bg,bd,vis_id)
	char		*foreground,
			*background;
	boolean		reverse;
	Pixeltype	*fg, *bg, *bd;
	unsigned long	vis_id;
{

	int		i;
	char		*name[2];
	int		col_2_alloc;
	int		status = 0;
	XColor		*colors;
	Colormap	def_cmap = DefaultColormap(dpy, DefaultScreen(dpy));
	Visual		*def_vis = DefaultVisual(dpy, DefaultScreen(dpy));
	int		def_depth = DefaultDepth(dpy, DefaultScreen(dpy));
	int		osize,nsize;

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
	if(vis_id != None){
		XVisualInfo	vinfo;
		XVisualInfo	*vret;
		int		num_vret;

		vinfo.visualid = vis_id;
		vret = XGetVisualInfo(dpy,VisualIDMask,&vinfo,&num_vret);
		if(vret && num_vret == 1){
			bestVisual = vret->visual;
			DspDepth = vret->depth;
		}
		if(vret)
			XFree(vret);
	}
	if(!bestVisual){
		bestVisual = def_vis;
		DspDepth = def_depth;
	}
	if(bestVisual != def_vis)
		ColorModel = CM_PRIVATE;

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
	 * all output primitives will use Color_ava to see 
	 * if they have a colour display
	 */
	Color_ava = TRUE;

	/*
	 * if we are requested to create a new color map or we are not
	 * using the default visual we need to create our own color map
	 */
	if (ColorModel == CM_PRIVATE) {
		MyCmap = TRUE;
		switch(bestVisual->class){
			/*
			 * RO model
			 */
			case TrueColor:
			case StaticColor:
			case StaticGray:
			case DirectColor:
				RoCmap = True;
				if(bestVisual == def_vis)
					Cmap = XCopyColormapAndFree(dpy,
								def_cmap);
				else{
					Cmap = XCreateColormap(dpy,
					RootWindow(dpy,DefaultScreen(dpy)), 
					bestVisual,AllocNone);
					XFreeColormap(dpy,def_cmap);
				}
				break;

			/*
			 * RW model - copy current table to minimize flashing.
			 */
			default:
				RoCmap = False;

				for(i=1,osize=2;i<def_depth;i++) osize*=2;
				for(i=1,nsize=2;i<DspDepth;i++) nsize*=2;
				Cmap = XCreateColormap(dpy,
					RootWindow(dpy,DefaultScreen(dpy)), 
					bestVisual,AllocAll);
				if(colors = malloc(sizeof(XColor)*osize)){
					for(i=0;i<osize;i++)
						colors[i].pixel = i;
					XQueryColors(dpy,def_cmap,colors,osize);
					XStoreColors(dpy,Cmap,colors,
						(osize<nsize)?osize:nsize);
					free(colors);
				}
				XFreeColormap(dpy,def_cmap);
				break;
		}
	}
	else {
		MyCmap = FALSE;
		RoCmap = True;
		Cmap = def_cmap;
	}

	/* 
	 * find max direct colour, DCP is direct colour precision in the CGM
	 */
	max_colour = (1 << DCP) - 1;

	/* 
	 * 	initialize the color table to empty 
	 *	and mark all indexes as not defined
	 */
	X11_initColorTable();

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
			cgmc.cd[i].red = 0;
			cgmc.cd[i].green = 0;
			cgmc.cd[i].blue = 0;
			if(color.flags & DoRed)
				cgmc.cd[i].red = color.red /
							X_MAX_RGB * max_colour;
			if(color.flags & DoGreen)
				cgmc.cd[i].green = color.green /
							X_MAX_RGB* max_colour;
			if(color.flags & DoBlue)
				cgmc.cd[i].blue = color.blue /
							X_MAX_RGB * max_colour;
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
