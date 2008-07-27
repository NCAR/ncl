/*
 *	$Id: rasdraw.c,v 1.23 2008-07-27 03:18:41 haley Exp $
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
 *	rasdraw.c
 *
 *	Author		John Clyne (clyne@ncar.ucar.edu)
 *
 *	Date		Fri Mar  8 10:01:17 MST 1991
 *
 *	This file contains routines or displaying an image store in a
 *	Raster* structure to an X window. Currently only 8-bit indexed
 *	color images are supported. Similarly only pseudo-color displays
 *	are supported.
 */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>     /* Get standard string definations. */
#include <X11/Shell.h>
#include <ncarg/c.h>
#include <ncarg/ncarg_ras.h>
#include "rasdraw.h"

/*LINTLIBRARY*/

static	void	load_static_24bit_image_(ras, context)
	Raster	*ras;
	Context	*context;
{
	unsigned char	*s, *t;
	int	i,j;

	s = ras->data;
	t = (unsigned char *) context->ximage->data;
	if (! context->image_info.lsbFirst) t++;

	for(i=0; i<ras->ny; i++)
	for(j=0; j<ras->nx; j++, s+=3, t+=4) {
		memmove((void *) t, (const void *) s, 3);
	}

	XPutImage(context->dpy, XtWindow(context->canvas), context->gc,
			context->ximage, 0,0,0,0, ras->nx, ras->ny);
}

static	void	load_24bit_image_(ras, context)
	Raster	*ras;
	Context	*context;
{
	fprintf(stderr, "Unsupported visual\n");
}

/*ARGSUSED*/
static	void	load_24bit_image(ras, context)
	Raster	*ras;
	Context	*context;
{
	if (context->cmap_info.writeable) {
		load_24bit_image_(ras, context);
	}
	else {
		load_static_24bit_image_(ras, context);
	}
}

static	void	load_8bit_image_(ras, context)
	Raster	*ras;
	Context	*context;
{
	if (context->dsp_depth == 8) {
		context->ximage->data = (char *) ras->data;

		XPutImage(context->dpy, XtWindow(context->canvas), context->gc,
			context->ximage, 0,0,0,0, ras->nx, ras->ny);
	}
	else {
		(void) fprintf(stderr, "Unsupported display depth\n");
	}
}

/*ARGSUSED*/
static	void	load_static_8bit_image_(ras, context)
	Raster	*ras;
	Context	*context;
{

#ifdef	DEAD
	int	i;
	long	*static_pal = context->cmap_info.static_pal;
	int	bytes_per_pixel = context->image_info.bytes_per_pixel;
	unsigned char	*rdata	= ras->data;
	char		*xidata = context->ximage->data;

	if (! context->image_info.lsbFirst) {
		for (i = 0; i < ras->nx * ras->ny; i++) {
			memmove(xidata, ((const void *) (&static_pal[*rdata]) + 
				sizeof(long) - bytes_per_pixel),
				bytes_per_pixel);

			rdata++;
			xidata += bytes_per_pixel;
		}
	}
	else {
		for (i = 0; i < ras->nx * ras->ny; i++) {
			memmove(xidata,(const void *) &static_pal[*rdata],
				bytes_per_pixel);

			rdata++;
			xidata += bytes_per_pixel;
		}

	}

	XPutImage(context->dpy, XtWindow(context->canvas), context->gc,
		context->ximage, 0,0,0,0, ras->nx, ras->ny);
#endif

	(void) fprintf(stderr, "Unsupported color model\n");
}
/*
 *	load an image from ras into an XImage structure
 */
static	void	load_8bit_image(ras, context)
	Raster	*ras;
	Context	*context;
{

	if (context->cmap_info.writeable) {
		load_8bit_image_(ras, context);
	}
	else {
		load_static_8bit_image_(ras, context);
	}
}

static	int	load_static_pal_(red, green, blue, ncolor, context)
	unsigned char	*red, *green, *blue;
	unsigned int	ncolor;
	Context	*context;
{
	Display		*dpy = context->dpy;
	Colormap 	cmap = context->cmap;
	long		*static_pal = context->cmap_info.static_pal;
	XColor		xcolor;
	char		flags = (DoRed | DoGreen | DoBlue);
	int		i;
	/*
	 * one time allocation of memory for static palette
	 */
	if (! static_pal) {
		static_pal = (long *) malloc(256 * sizeof(long));

		if (! static_pal) {
			ESprintf(errno,"malloc(%d)", 256 * sizeof(long));
			return(-1);
		}
	}
	context->cmap_info.static_pal = static_pal;

	for (i=0; i < ncolor; i++) {
		xcolor.red = (unsigned short)
			((float) red[i] / 255.0 * 65535);
		xcolor.green = (unsigned short)
			((float) green[i] / 255.0 * 65535);
		xcolor.blue = (unsigned short)
			((float) blue[i] / 255.0 * 65535);

		xcolor.flags = flags;
		xcolor.pad = '\0';

		if (! XAllocColor(dpy, cmap, &xcolor)) {
			ESprintf(E_UNKNOWN,"XAllocColor(,,)");
			return(-1);
		}
		static_pal[i] = xcolor.pixel;
	}

	context->cmap_info.static_pal = static_pal;
	return(1);
}

static	int	load_palette_(red, green, blue, ncolor, context)
	unsigned char	*red, *green, *blue;
	unsigned int	ncolor;
	Context	*context;
{
	int	i;
	int	num_cols;
	Display	*dpy = context->dpy;
	char	flags = (DoRed | DoGreen | DoBlue);

	num_cols = MIN(context->cmap_info.max_colors, ncolor);

	if (num_cols < ncolor) {
		(void) fprintf(stderr, 
			"Warning: only %d of %d colors can be displayed\n",
			num_cols, ncolor);
	}

	/*
	 * alloc more memory for color list if necessary
	 */
	if (context->xcolor_size < num_cols) {
		if (context->xcolors) free((char *) context->xcolors);

		context->xcolors = (XColor *) 
				malloc((unsigned) num_cols * sizeof(XColor));

		if (! context->xcolors) {
			ESprintf(errno,"malloc(%d)", num_cols * sizeof(XColor));
			return(-1);
		}
		context->xcolor_size = num_cols;
	}

	for (i=0; i<num_cols; i++) {
		context->xcolors[i].pixel = (unsigned long) i;

		context->xcolors[i].red = (unsigned short)
			((float) red[i] / 255.0 * 65535);
		context->xcolors[i].green = (unsigned short)
			((float) green[i] / 255.0 * 65535);
		context->xcolors[i].blue = (unsigned short)
			((float) blue[i] / 255.0 * 65535);

		context->xcolors[i].flags = flags;
		context->xcolors[i].pad = '\0';
	}

	XStoreColors(dpy, context->cmap, context->xcolors, num_cols);

	return(1);
}



/*
 *	load our colormap with the appropriate palette. this only works
 *	with pseudo-color displays.
 */
static	int	load_palette(ras, context)
	Raster	*ras;
	Context	*context;
{
	static	Boolean	first = True;
	Display		*dpy = context->dpy;
	Colormap 	cmap = context->cmap;

	/*
	 * is colormap writeable
	 */
	if (context->cmap_info.writeable) {

		/*
		 * we wait to last possible moment to load new colormap
		 * so user doesn't get a black screen while trying to
		 * place the image window
		 */
		if (first) {
			/*
			 * change default colormap. Window Manager is 
			 * responsible  for swapping it in when sprite 
			 * is in our window
			 */
			XSetWindowColormap(dpy, XtWindow(context->toplevel), 
								cmap);
			first = False;
		}
		/*
		 * if load_pal is true load palette from ras else try and 
		 * load default palette.
		 */
		if (context->load_pal && ras->red && ras->green &&ras->blue) {
			(void) load_palette_(ras->red, ras->green, ras->blue, 
						(unsigned) ras->ncolor,context);
		}
		else {
			if (! context->pal_loaded) {	/* already loaded? */
				(void) load_palette_(context->default_pal.red, 
					context->default_pal.green, 
					context->default_pal.blue,
					context->default_pal.ncolor, context);

				context->pal_loaded = True;
			}
		}
	}
	else {
		if (context->load_pal) {
			(void)load_static_pal_(ras->red, ras->green, ras->blue, 
						(unsigned) ras->ncolor,context);
		}
		else {
			if (! context->pal_loaded) {	/* already loaded? */

				(void) load_static_pal_(
					context->default_pal.red, 
					context->default_pal.green, 
					context->default_pal.blue,
					context->default_pal.ncolor, context
				);

				context->pal_loaded = True;
			}
		}

	}
}



/*
 *	create a colormap 
 */
static	Colormap	create_colormap(context)
	Context	*context;
{
	Display	*dpy = context->dpy;
	Visual	*visual = context->visual;
	int	screen = DefaultScreen(dpy);
	int	dsp_depth;
	Colormap	cmap;
	long	max_colors;
	Boolean	writeable;

#ifdef	NOT_ICCCM
	void	InstallCMap(), UnInstallCMap();
#endif

	dsp_depth =  context->dsp_depth;
	max_colors = 1 << dsp_depth;

	if ((visual->class == DirectColor)
		|| (visual->class == PseudoColor)
		|| (visual->class == GrayScale)) {

		cmap = XCreateColormap(dpy, RootWindow(dpy, screen),
					visual, AllocAll);

#ifdef	DEAD
		/*
		 * change default colormap. Window Manager is responsible
		 * for swapping it in when sprite is in our window
		 */
		XSetWindowColormap(dpy, XtWindow(context->toplevel), cmap);
#endif

		writeable = True;

	}
	else {
		cmap = XCreateColormap(
			dpy, RootWindow(dpy, screen), visual, AllocNone
		);
		writeable = False;
		
	}

	context->cmap_info.max_colors = max_colors;
	context->cmap_info.writeable = writeable;
	context->cmap_info.static_pal = (long *) NULL;

	return(cmap);
}


/*
 * create and setup the ximage structure
 */
static	XImage	*create_ximage(dpy, depth, visual, nx, ny, context)
	Display	*dpy;
	int	depth;
	Visual	*visual;
	int	nx, ny;
	Context	*context;
{
	long	image_size;
	int	bytes_per_pixel;
	XImage	*ximage;
	Boolean	use_xputpixel;
	static	char	*image_buf = NULL;
	unsigned long   swaptest = 1;   /* used to test if client byte swaped*/
	int	bytes_per_line;

	bytes_per_line = 0;	/* let X figure it out	*/

	if ((ximage = XCreateImage(dpy, visual,
		depth, ZPixmap, 0, NULL, nx, ny, 8, bytes_per_line)) == NULL) {

		ESprintf(E_UNKNOWN,"XCreateImage(,,,,,,,,,)");
		return(NULL);
	}

	switch (ximage->bits_per_pixel)
	{
	case 8:
	    use_xputpixel = False;
	    bytes_per_pixel = 1;
	    break;
	case 16:
	    use_xputpixel = True;
	    bytes_per_pixel = 2;
	    break;
	case 24:
	    use_xputpixel = False;
	    bytes_per_pixel = 3;
	    break;
	case 32:
	    use_xputpixel = True;
	    bytes_per_pixel = 4;
	    break;
	case 64:
	    use_xputpixel = True;
	    bytes_per_pixel = 8;
	    break;
	default:
	    use_xputpixel = True;
	    break;
	}

	image_size = ximage->width * ximage->height * bytes_per_pixel;


	if (depth == 8) {
		if ((*(char *) &swaptest)) ximage->byte_order = LSBFirst;
		else ximage->byte_order = MSBFirst;
	}
	else {
/*
**	Wish I new what I was doing here. Seems to fix the problem of
**	having red & blue swapped on SGI IR systems.
*/
#ifdef	DEAD
		/*
		 * raw data format is fixed for direct color: red, green, blue
		 */
		if (ImageByteOrder(dpy) == MSBFirst) {
			ximage->byte_order = LSBFirst;
		}
		else {
			ximage->byte_order = MSBFirst;
		}
#else
		if (
			ximage->red_mask == 0xff0000 && 
			ximage->blue_mask == 0xff) {

			ximage->byte_order = MSBFirst;
		}
		else if (
			ximage->red_mask == 0xff && 
			ximage->blue_mask == 0xff0000) {

			ximage->byte_order = LSBFirst;
		}
		else {
			ESprintf(E_UNKNOWN,"Unsupported color mask");
			return(NULL);
		}
#endif
	}

	/*
	 * if bytes_per_pixel is eight we cheet and use memory allocated to 
	 * the Raster* structure, else we have to allocate our own memory
	 */
	if (bytes_per_pixel != 1) {
		image_buf = (char *) malloc ((unsigned) image_size);
		if (! image_buf) {
			ESprintf(errno,"malloc(%d)", image_size);
			return(NULL);
		}
	}

	context->image_info.use_xputpixel = use_xputpixel;
	context->image_info.lsbFirst = ximage->byte_order == LSBFirst;
	context->image_info.bytes_per_pixel = bytes_per_pixel;
	context->image_info.image_size = image_size;

	ximage->data = image_buf;

	return(ximage);
}

static	void	destroy_ximage(context)
	Context	*context;
{
	if (! context->ximage) {
		return;
	}

	if (context->image_info.bytes_per_pixel != 1) {
		if (context->ximage->data) free((char *) context->ximage->data);
	}
	/*
	 * XDestroyImage frees  context->image_info.data
	 */
	context->ximage->data = NULL;
	XDestroyImage(context->ximage);
}


/*
 * create a drawing canvas 
 */
/*ARGSUSED*/
static	Widget	create_graphics_canvas(parent, nx, ny)
	Widget	parent;
	int	nx, ny;
{
	Widget		canvas;
	Cardinal	n;
	Arg		args[10];

	extern	WidgetClass	widgetClass;


	/*
	 *      create drawing canvas
	 */
	n = 0;
	XtSetArg(args[n], XtNwidth, nx);	n++;
	XtSetArg(args[n], XtNheight, ny);	n++;
	canvas = XtCreateManagedWidget("canvas", widgetClass, parent, args, n);

	return(canvas);
}

static	Visual	*get_best_visual(depth, encoding_hint, dpy)
	int	*depth;
	int	encoding_hint;
	Display	*dpy;
{
	XVisualInfo	vinfo;
	int	screen = DefaultScreen(dpy);

	/*
	 * if want direct color (24-bit encoded) look for a DirectColor
	 * followed by TrueColor visual with depth of 24 bits
	 */
	if (encoding_hint == RAS_DIRECT) {
#ifdef	DEAD
		if (XMatchVisualInfo(dpy, screen, 24, DirectColor, &vinfo)) {
			*depth = vinfo.depth;
			return(vinfo.visual);
		}
		else 
#endif
		if (XMatchVisualInfo(dpy, screen, 24, TrueColor, &vinfo)) {
			*depth = vinfo.depth;
			return(vinfo.visual);
		}
	}

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

#ifdef	NOT_ICCCM
void	InstallCMap(w,client_data,call_data)
	Widget w;
	caddr_t client_data;
	caddr_t call_data;
{
	Colormap cmap	= (Colormap) client_data;
	
	XInstallColormap(XtDisplay(w),cmap);
	XSetWindowColormap(XtDisplay(w), XtWindow(w), cmap);
}

void	UnInstallCMap(w,client_data,call_data)
	Widget w;
	caddr_t client_data;
	caddr_t call_data;
{
	Colormap cmap	= (Colormap) client_data;
	
	XUninstallColormap(XtDisplay(w),cmap);
}
#endif

/*ARGSUSED*/
void PassGo(w,client_data,call_data)
	Widget w;
	caddr_t client_data;
	caddr_t call_data;
{
	int	*pass_go = (int *) client_data;

	*pass_go = 1;
}

/*
 *	RasDrawOpen
 *	[exported]
 *
 *	Initialize DrawRas for displaying images. Images are assumed to be
 *	either 8-bit indexed or 24-bit direct. It is not guaranteed that
 *	a X visual will exactly match one of these types. 
 *
 *	RasDrawOpen() fills in the following fields of the 'Context' struct
 *	that is returned on success:
 *
 *		app_con		The Xt application context
 *		encoding	Bit mask of available visual types
 *		dpy		The display
 *		batch		Run in batch mode
 *		load_pal	get color palette from Raster struct
 *		app_name	the application name
 *		app_class	X application class name
 *		
 *		
 *	The 'encoding' field is a bit mask which specifies the available
 *	color models - (RASDRAW_8BIT, RASDRAW_24BIT)
 *
 *	When 'batch' is false rasview waits for a mouse click at the end
 *	of each frame.
 *
 * on entry
 *	argc		: len of argv
 *	**argv		: null terminated argument list passed to XtInitialize
 *			  The application class passed to XtInitialize is 
 *			  obtained from argv[0]
 *	batch		: set/unset batch mode
 * on exit
 *	return		: A context pointer to be used with DrawRas and Close
 *			  else NULL if an error occured.
 */
#ifdef	NeedFuncProto
Context	*RasDrawOpen(int *argc, char **argv, Boolean batch)
#else
Context	*RasDrawOpen(argc, argv, batch)
	int	*argc;
	char	**argv;
	Boolean	batch;
#endif
{
	Context		*context;
	String		app_class;

	Visual		*visual;
	char		*prog_name;
	Arg		args[5];
	Cardinal	n;
	Display		*dpy;
	XVisualInfo	vinfo;
	int		screen;
	
	/*
	 * remove path to argv[0]
	 */
	prog_name = (prog_name = strrchr(*argv, '/')) ? ++prog_name : *argv;
	argv[0] = prog_name;

	/*
	 * use argv[0] as application class after converting argv[0][0]
	 * to uppercase.
	 */
	if (! (app_class = (String) malloc((unsigned) strlen(argv[0] + 1)))) {
		ESprintf(errno,"malloc(%d)", strlen(argv[0]) + 1);
		return ((Context *) NULL);
	}

	app_class = strcpy(app_class, argv[0]);
	*app_class = toupper(*app_class);

	/*
	 * create context for this instance
	 */
	if ((context = (Context *) malloc(sizeof(Context))) == NULL) {
		ESprintf(errno,"malloc(%d)", sizeof(Context));
		return ((Context *) NULL);
	}

	/*
	 * initialize X. We can't use XtAppInitialize() because we
	 * need to create the top level shell with a visual that is 
	 * appropriate for the image we are displaying. We won't
	 * know the visual until we've opened the raster file.
	 */
	XtToolkitInitialize();
	context->app_con = XtCreateApplicationContext();
	dpy = XtOpenDisplay(
		context->app_con, NULL, NULL, app_class, 
		(XrmOptionDescRec *) NULL, (Cardinal) 0, argc, argv
	);
	if (! dpy) {
		ESprintf(E_UNKNOWN,"XtOpenDisplay(,,,,,,,)");
		return(NULL);
	}

#ifdef  XSYNC
        XSynchronize(dpy, 1);
#endif


	screen = DefaultScreen(dpy);
	context->encoding = RASDRAW_0BIT;
	if (XMatchVisualInfo(dpy, screen, 24, DirectColor, &vinfo) ||
		XMatchVisualInfo(dpy, screen, 24, TrueColor, &vinfo)) {

		context->encoding |= RASDRAW_24BIT;
	}
	if (XMatchVisualInfo(dpy, screen, 8, PseudoColor, &vinfo) ||
		XMatchVisualInfo(dpy, screen, 8, StaticColor, &vinfo) ||
		XMatchVisualInfo(dpy, screen, 8, GrayScale, &vinfo) ||
		XMatchVisualInfo(dpy, screen, 8, StaticGray, &vinfo)) {

		context->encoding |= RASDRAW_8BIT;
	}
	
	context->dpy = dpy;
	context->batch = batch;
	context->load_pal = True;
	context->xcolors = (XColor *) NULL;
	context->xcolor_size = 0;
	context->default_pal.red = (unsigned char *) NULL;
	context->default_pal.green = (unsigned char *) NULL;
	context->default_pal.blue = (unsigned char *) NULL;
	context->app_name = argv[0];
	context->app_class = app_class;
	context->ras = (Raster *) NULL;

	return(context);
}

/*
**	Expose event handler. Redraw image (if any)
*/
/*ARGSUSED*/
void	exposeEH(
	Widget		w,
	XtPointer	client_data,
	XEvent		*event,
	Boolean		*dispatch
) {
	Context	*context =  (Context *) client_data;

	if (context->ras) {
		RasDraw(context->ras, context);
	}
}

/*
 * finish initialzation of context. We can't do this inside of
 * RasDrawOpen() because we don't know dimensions or type of image
 *
 * ras_draw_init() fills in the following Context fields:
 *
 *	visual		: The X visual used by the drawing canvas
 *	vis_class	: class of 'visual'
 *	cmap		: the color map installed in the drawing canvas
 *	toplevel	: top level widget
 *	canvas		: drawing canvas widget
 *	gc		: GC for canvas
 *	dsp_depth	: depth of display
 *	ximage		: XImage structure for transfering image to canvas
 */
ras_draw_init(context, nx, ny, encoding_hint)
	Context	*context;
	int	nx, ny;
	int	encoding_hint;
{
	XGCValues	xgcvalues;
	XSetWindowAttributes xswa;
	XEvent		event;
	Arg		args[10];
	Cardinal	n;


	/*
	 * find the visual that best matches 'encoding_hint'
	 */
	context->visual = get_best_visual(
		&context->dsp_depth, encoding_hint, context->dpy
	);
	context->vis_class = context->visual->class;

	context->cmap = create_colormap(context);

	/*
	 * create toplevel widget. We do this here because the drawing
	 * canvas needs to inherit the colormap and visual from the
	 * toplevel widget
	 */
	n = 0;
	XtSetArg(args[n], XtNcolormap, context->cmap);		n++;
	XtSetArg(args[n], XtNvisual, context->visual);		n++;
	XtSetArg(args[n], XtNdepth, context->dsp_depth);	n++;
	XtSetArg(args[n], XtNinput, True);			n++;
	context->toplevel = XtAppCreateShell(
		NULL, context->app_class, applicationShellWidgetClass, 
		context->dpy, args, n
	);

	context->canvas = create_graphics_canvas(context->toplevel, nx, ny);


	XtAddEventHandler(
		context->canvas, ExposureMask, False, exposeEH, 
		(XtPointer) context
	);

	XtRealizeWidget(context->toplevel);

	context->gc = XtGetGC(context->canvas, 0L, &xgcvalues); 

#ifdef	DEAD
	/*
	 * turn on backing store
	 */
	xswa.backing_store = WhenMapped;
	XChangeWindowAttributes(context->dpy, XtWindow(context->canvas), 
						CWBackingStore, &xswa);

#endif


	context->ximage = create_ximage(context->dpy, 
		context->dsp_depth, context->visual, nx, ny, context);

	/*
	 * loop until window becomes exposed
	 */
	for(;;) {
		XtAppNextEvent(context->app_con, &event);
		XtDispatchEvent(&event);

		/* 
		 * window is exposed when we get a map notify
		 */
		if (event.type == MapNotify) break;
	}

}

/*
 *	RasDraw
 *	[exported]
 *
 *	Put an 8-bit indexed or 24-bit true color image on the screen. 
 *	The 'type' field of the Raster struct must be one of RAS_INDEXED
 *	or RAS_DIRECT. The type of raster file must also be supported as
 *	indicated by the encoding field of 'context'.
 *	
 * on entry
 *	ras		: contains an image 
 *	*context	: context returned by OpenRasDraw
 *
 * on exit
 *	return		: -1 => error, else OK
 */
int	RasDraw(ras, context)
	Raster	*ras;
	Context	*context;
{
	XEvent	event;
	int	class;

	static	Boolean	first	= True;

	/*
	**	Save the raster for expose events if needed
	*/
	context->ras = ras;

	if (first) {
		/*
		 * finish initialization of context now that we know
		 * dimensions of image
		 */
		ras_draw_init(context, ras->nx, ras->ny, ras->type);
		first = False;
	}
	class = context->vis_class;

	/*
	 * see if raster changed size
	 */
	if (context->ximage->width != ras->nx || 
				context->ximage->height != ras->ny) {

		destroy_ximage(context);

		context->ximage = create_ximage(context->dpy, 
			context->dsp_depth, context->visual, 
			ras->nx, ras->ny, context);

	}

	if (class == DirectColor || class == TrueColor) {
		if (ras->type != RAS_DIRECT) {
			ESprintf(E_UNKNOWN, "Expected RAS_DIRECT raster type");
			return(-1);	/* bad encoding	*/
		}

		if (context->dsp_depth != 24) {
			ESprintf(E_UNKNOWN, "Expected 24-bit display depth");
			return(-1);	/* bad encoding	*/
		}

		load_24bit_image(ras, context);
	}
	else {
		if (ras->type != RAS_INDEXED) {
			ESprintf(E_UNKNOWN, "Expected RAS_INDEXED raster type");
			return(-1);	/* bad encoding	*/
		}
		/*
		 * load colors from image into our own private colormap
		 */
		load_palette(ras, context);

		/*
		 * load the image from ras into a XImage structure
		 */
		load_8bit_image(ras, context);

	}


	/*
	 * if we're not in batch mode wait for a mouse click between frames
	 */
	if (! context->batch) {
		int	pass_go = 0;

		XtAddEventHandler(context->canvas, ButtonPressMask,
				False, (XtEventHandler) PassGo, (caddr_t) &pass_go);
		/*
		 * loop until we get a button press
		 */
		for(;;) {

			XtAppNextEvent(context->app_con, &event);
			XtDispatchEvent(&event);

			if (pass_go) break;
		}
		XtRemoveEventHandler(context->canvas, ButtonPressMask,
				False, (XtEventHandler) PassGo, NULL);
	} 

	/*
	 * clean up any remaining events
	 */
	while(XtAppPending(context->app_con)) {
		XtAppNextEvent(context->app_con, &event);
		XtDispatchEvent(&event);
	}
	return(1);		
}

/*
 *	RasDrawSetPalette
 *	[exported]
 *
 *	Set the default color map to be loaded for each RasDraw(). Subsequent
 *	invocations of RasDraw() will ignore color map information in the
 *	Raster* structure. RasDrawSetPalette() may be called any time after
 *	RasDrawOpen() and before RasDrawClose().
 *
 *	RasDrawSetPalette() only works with 8-bit indexed imagery. 
 *
 * on entry
 *	*context	: context returned by RasDrawOpen()
 *	*red, *green, *blue	: the color palette to use
 *	ncolor		: num elements in each of red, green, blue
 *	
 */
int	RasDrawSetPalette(context, red, green, blue, ncolor)
	Context	*context;
	unsigned char	*red, *green, *blue;
	unsigned int	ncolor;

{
	unsigned char 	*ucptr;

	if (context->default_pal.red) 
		free((char *) context->default_pal.red);
	if (context->default_pal.green) 
		free((char *)context->default_pal.green);
	if (context->default_pal.blue) 
		free((char *)context->default_pal.blue);

	/*
	 * store the default color palette in the context structure for
	 * later use
	 */
	if ( ! (ucptr = (unsigned char *) malloc(ncolor))) {
		ESprintf(errno,"malloc(%d)", ncolor);
		return(-1);
	}
	memmove((void *) ucptr, (const void *) red, (size_t) ncolor);
	context->default_pal.red = ucptr;

	if ( ! (ucptr = (unsigned char *) malloc(ncolor))) {
		ESprintf(errno,"malloc(%d)", ncolor);
		return(-1);
	}
	memmove((void *) ucptr, (const void *) green, (size_t) ncolor);
	context->default_pal.green = ucptr;

	if ( ! (ucptr = (unsigned char *) malloc(ncolor))) {
		ESprintf(errno,"malloc(%d)", ncolor);
		return(-1);
	}
	memmove((void *) ucptr, (const void *) blue, (size_t) ncolor);
	context->default_pal.blue = ucptr;

	context->default_pal.ncolor = ncolor;
	context->load_pal = False;
	context->pal_loaded = False;

	return(1);
}

/*
 *	RasDrawUnSetPalette
 *	[exported]
 *
 *	After calling RasDrawUnSetPalett subsequent invocations of RasDraw() 
 *	will attempt to use colormap info stored in the Raster* structure.
 *
 * on entry
 *	*context	: context returned by RasDrawOpen()
 */
void	RasDrawUnSetPalette(context)
	Context	*context;
{
	context->load_pal = True;
}

/*
 *	RasDrawClose
 *	[exported]
 *
 *	clean up context created with RasDrawOpen()
 */
void	RasDrawClose(context)
	Context	*context;
{
	if (! context) return;
	XtDestroyApplicationContext(context->app_con);

	if (context->xcolors) free((char *) context->xcolors);

	if (context->default_pal.red) 
		free((char *) context->default_pal.red);
	if (context->default_pal.green) 
		free((char *) context->default_pal.green);
	if (context->default_pal.blue) 
		free((char *) context->default_pal.blue);
	if (context->cmap_info.static_pal)
		free((char *) context->cmap_info.static_pal);

	destroy_ximage(context);

	free((char *) context);
}

