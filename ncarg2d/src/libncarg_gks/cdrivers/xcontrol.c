/*
 *	$Id: xcontrol.c,v 1.2 1994-05-28 00:44:56 fred Exp $
 */
/*
 *      File:		xcontrol.c
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Thu May 16 15:44:37 MDT 1991
 *
 *      Description:	This file contains routines for handling gks control
 *			functions for the x device driver
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <ncarg/c.h>
#include "common.h"
#include "gksc.h"
#include "gks.h"
#include "x.h"
#include "x_device.h"
#include "xddi.h"

/*	init_color
 *
 *		intialize the color table and allocate default colours
 *
 * on entry
 *	palette_size	: size of the color palette
 * 
 * on exit
 *	Cmap	: contains the color map
 *	Color_ava	: true if have a color display
 *	fg, bg, bd	: set to default colours as described in name
 */
static	Colormap	init_color(
				dpy, fg, bg, bd, color_ava, 
				color_palette, palette_size
			)
	Display	*dpy;
	Pixeltype	*fg, *bg, *bd;
	Boolean		*color_ava;
	Pixeltype	color_palette[];
	unsigned	palette_size;
{

	static	char	*name[] = {
		"black", "white", "red", "green", "blue", "yellow", "magenta",
		"cyan", "gray"
	};

	static  int num_cols = sizeof (name) / sizeof (char *);

	static  XColor  color = {
		0,0,0,0,(DoRed | DoGreen | DoBlue), '\0'
	};


	int	i;
	Pixeltype	planedummy[1];		/* not used	*/
	Pixeltype	pixel_return[1];	/* device index	*/
	Visual		*visual;
	Colormap	cmap;
	int		dsp_depth;


	/*
	 *	get default visual that describes colourmap
	 */
	visual = DefaultVisual(dpy, DefaultScreen(dpy));	
	dsp_depth = DisplayPlanes(dpy, DefaultScreen(dpy));
	cmap = DefaultColormap(dpy, DefaultScreen(dpy));


	*fg = WhitePixel(dpy, DefaultScreen(dpy));
	*bd = WhitePixel(dpy, DefaultScreen(dpy));
	*bg = BlackPixel(dpy, DefaultScreen(dpy));

	/*
	 * initialize the color palette to the foreground color initialy.
	 * Thus any reference to a undefined color palette entry will get
	 * the default foreground color
	 */
	for (i=0; i<palette_size; i++) {
		color_palette[i] = *fg;
	}
		

	if (dsp_depth == 1) {
		/* one plane monochrome display	*/
		color_ava = FALSE;
		return(cmap);
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
	*color_ava = TRUE;


	/* 
	 *	allocate some default colours	
	 */
	for (i=0; i < num_cols; i++) {

		if (XParseColor(dpy, cmap, name[i], &color))  {

			/*
			 * see if have read/write color model
			 */
			if ((visual->class == DirectColor)
				|| (visual->class == PseudoColor)
				|| (visual->class == GrayScale)) {
				
				if (XAllocColorCells(dpy,cmap,FALSE, planedummy,
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
					XStoreColor(dpy, cmap, &color);

				}

			}
			/*
			 * read only color model
			 */
			if ((visual->class == TrueColor)
				|| (visual->class == StaticColor)
				|| (visual->class == StaticGray)) {

				if (!XAllocColor(dpy, cmap, &color)) {
					if (Verbose) {
						(void) fprintf(stderr, 
						"Warning - color \"%s\" not allocated\n", 
						name[i]);
					}
					continue;
				}

			}

			color_palette[i] = color.pixel;
		}
		else {
			if (Verbose) {
				(void) fprintf(stderr, 
				"Warning - color \"%s\" is not in database\n", 
				name[i]);
			}
		}
	}

	return(cmap);
}

static	void	pause(dpy)
	Display	*dpy;
{
	XEvent	event;

	/*
	 * discard all button press events that a impatient user
	 * may have aquired while waiting for a plot to finnish
	 */
	while(XCheckTypedEvent(dpy, ButtonPress, &event))
	;

	for (;;) {

		XNextEvent(dpy, &event);

		switch (event.type) {

		case	ButtonPress:
		case	KeyPress:
			return;	/* exit event loop	*/

		default:
			break;
		}
	}
}

/*
 *	do_geometry
 *	[internal]
 *
 *	Use the X resource manager to determine user preferances for
 *	geometry
 *
 * on entry
 *	*dpy		: the display
 *	*res_name	: the application resource name
 *	border_width	: width of window border
 *	*geometry	: geoemtry string. If Null geometry will be taken
 *			  from the resourc manager
 *
 * on exit
 *	return		: size hints structure with geometry information
 */
static	XSizeHints	*do_geometry(dpy, res_name, border_width, geometry)
	Display		*dpy;
	char		*res_name;
	unsigned long	border_width;
	char		*geometry;
{

#ifdef	DEAD
	static	XSizeHints  xsh = {	/* Size hints for window manager*/
		(PMinSize | PAspect),
		0,0,			/* obsolete ????		*/
		DEFAULT_WIDTH,		/* obsolete ????		*/
		DEFAULT_HEIGHT,		/* obsolete ????		*/
		MIN_WIDTH, MIN_HEIGHT,	/* minimum usefull win dim	*/	
		0,0,			/* max dim (not used)		*/
		0,0,			/* not used			*/
		{1,1},			/* min x/y aspect ratio		*/
		{1,1},			/* max x/y aspect ratio		*/
		0,
		0,			/* dimensions of window		*/
		0
	};
#else
	static	XSizeHints  xsh = {	/* Size hints for window manager*/
		(PMinSize),
		0,0,			/* obsolete ????		*/
		DEFAULT_WIDTH,		/* obsolete ????		*/
		DEFAULT_HEIGHT,		/* obsolete ????		*/
		MIN_WIDTH, MIN_HEIGHT,	/* minimum usefull win dim	*/	
		0,0,			/* max dim (not used)		*/
		0,0,			/* not used			*/
		{0,0},			/* not used			*/
		{0,0},			/* not used			*/
		0,
		0,			/* dimensions of window		*/
		0
	};
#endif
	int	geom_mask = 0;

	/*
	 * get the geometry resource string from the resource manager
	 */
	if (!geometry) geometry = XGetDefault (dpy, res_name, "geometry");
	if (!geometry) geometry = XGetDefault (dpy, res_name, "Geometry");

	if (geometry) {
		geom_mask = XParseGeometry (geometry, &xsh.x, &xsh.y,
				(unsigned int *)&xsh.width,
				(unsigned int *)&xsh.height);
	}

	/*
	 * see if user specified a window position. 
	 */
	if ((geom_mask & XValue) || (geom_mask & YValue)) {
		xsh.flags |= USPosition;
	}

	/*
	 * deal with negative position
	 */
	if ((geom_mask & XValue) && (geom_mask & XNegative)) {
		xsh.x = DisplayWidth (dpy, DefaultScreen(dpy)) + xsh.x -
		xsh.width - border_width * 2;
	}

	if ((geom_mask & YValue) && (geom_mask & YNegative)) {
		xsh.y = DisplayWidth (dpy, DefaultScreen(dpy)) + xsh.y -
		xsh.height - border_width * 2;
	}


	/*
	 * see if user specified a dimension, else we use program defaults
	 */
	if ((geom_mask & WidthValue) || (geom_mask & HeightValue)) {
		xsh.flags |= USSize;
	}
	else {
		xsh.flags |= PSize;
	}

	return(&xsh);
}

/*ARGSUSED*/
X11_OpenWorkstation(gksc)
	GKSC	*gksc;
{
        int             *iptr = (int *) gksc->i.list;

	static	XWMHints	xwmh = {
		(InputHint | StateHint ),/* flags 			*/
		True,			/* input 			*/
		NormalState,		/* initial_state 		*/
		0,			/* icon pixmap 			*/
		0,			/* icon window 			*/
		0, 0,			/* icon location 		*/
		0,			/* icon mask 			*/
		0			/* Window group 		*/
	};
	
	static	XClassHint	xch = {
		"xgks",			/* resource name		*/
		"Xgks"			/* class name			*/
	};


	Pixeltype fg, bg, bd;		/* Pixel values 		*/
	XSetWindowAttributes xswa;	/* Set Window Attribute struct 	*/
	XTextProperty	window_name,
			icon_name;
	unsigned long bw = 0;		/* Border width 		*/
	Drawable	drawable;
	XGCValues	gcv;		/* struc for manipulating a GC	*/
	XEvent      event;		/* Event received 		*/
	XWindowAttributes	xwa;	/* Get window attributes	*/
	CoordSpace	square_screen;

	GC	line_gc, mark_gc, fill_gc,
		cell_gc, text_gc, bg_gc;


	Display	*dpy;
	Window	win;

	Xddp	*xi;

	char	*dpy_name;		/* display name			*/
	XSizeHints	*xshptr;

	if ((xi = (Xddp *) malloc (sizeof (Xddp))) == (Xddp *) NULL) {
		ESprintf(ERR_DTABLE_MEMORY, "malloc(%d)", sizeof(Xddp));
		return(ERR_DTABLE_MEMORY);
	}

	gksc->ddp = (GKSC_Ptr) xi;

	/*
	 *	establish connection to sever
	 */
	if ((dpy_name = getenv("DISPLAY")) == NULL) {
		ESprintf(ERR_NO_DISPLAY, 
			"X11 \"DISPLAY\" env. variable not set");
		return(ERR_NO_DISPLAY);
	}

	if ((dpy = XOpenDisplay(dpy_name)) == NULL) {
		ESprintf(ERR_OPN_DISPLAY, "  Error on opening X display (%s)", 
			dpy_name);
		return(ERR_OPN_DISPLAY);
	}

	/*
	 * initialize color
	 */
	xi->cmap = init_color(
		dpy, &fg, &bg, &bd, &(xi->color_ava), xi->color_pal, MAX_COLORS
	);

	/*
	 * create our own window or use one given to us?
	 */
	/*
	 * get user preferances for window geometry
	 */
	xshptr = do_geometry(dpy, xch.res_name, bw, NULL);

	/* 
	 * Create the Window with the information in the XSizeHints, the
	 * border width, and the border & background pixels.
	 */
	drawable = XCreateSimpleWindow(
		dpy, RootWindow(dpy,DefaultScreen(dpy)),
		xshptr->x, xshptr->y, xshptr->width, xshptr->height,
		bw, bd, bg
	);
	win = drawable;

	/*
	 * Set the standard properties for the window managers. 
	 */
	window_name.encoding = XA_STRING;
	window_name.format = 8;
	window_name.value = (unsigned char *) "NCAR Xgks";
	window_name.nitems = strlen (window_name.value);
	icon_name.encoding = XA_STRING;
	icon_name.format = 8;
	icon_name.value = (unsigned char *) "xgks";
	icon_name.nitems = strlen (icon_name.value);

	XSetWMProperties(dpy, win, &window_name, &icon_name, 
				NULL, 0, xshptr, &xwmh, &xch);

	/*
	 * Ensure that the window's colormap field points to the default
	 * colormap,  so that the window manager knows the correct 
	 * colormap to use for the window.  
	 */
	xswa.colormap = xi->cmap;
	xswa.bit_gravity = CenterGravity;
	xswa.backing_store = WhenMapped;
	XChangeWindowAttributes(dpy, win, (CWColormap 
			| CWBitGravity 
			| CWBackingStore), &xswa);


	/* 
	 * Select notification of Expose event that is generated when
	 * the window is first mapped (becomes visible) to the screen.
	 */
	XSelectInput(dpy, win, ExposureMask);

	/*
	 * Map the window to make it visible.
	 */
	XMapWindow(dpy, win);

	/*
	 *	get expose event as window becomes visible. we can't
	 *	draw until after this 
	 */
	while(1) {
		/* get next event	*/
		XNextEvent(dpy, &event);

		/* 
		 * find the last expose event on the event queue.
		 */
		if (event.type == Expose && event.xexpose.count == 0) {

			/*
			 * Remove any other pending Expose events from 
			 * the queue to avoid multiple repaints. 
			 */
			while (XCheckTypedEvent(dpy, Expose, &event))
				;
		
			break;
		}
	}
	/* 
	 * 	create default graphics contexts for the win.
	 * 	Use defaults except for backround, backround and line width.
	 */
	gcv.background = bg;
	gcv.foreground = fg;

	line_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);
	fill_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);
	mark_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);
	cell_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);
	text_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);

	/*
	 * create a background gc (gc for drawing in background color)
	 */
	gcv.background = fg;
	gcv.foreground = bg;
	bg_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);

	/*
 	 *      Select events for which we want future notification.
	 */
	XSelectInput(dpy,win, (ButtonPressMask | KeyPressMask ));

	TransformSetWindow(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);
	TransformSetViewport(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);
	TransformSetNDScreenSpace(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);

	/*
	 *	find out how big window is. calculate
	 *	coordinate translation macros
	 */
	if (XGetWindowAttributes(dpy, win, &xwa) == 0) {
		ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
		return(ERR_WIN_ATTRIB);
	}


	square_screen = ComputeLargestSquare(
		(double) 0.0, (double) (xwa.height - 1),
		(double) (xwa.width - 1), (double) 0.0
	);
	TransformSetScreenSpace(
		&xi->tsystem, square_screen.llx, square_screen.lly, 
		square_screen.urx, square_screen.ury
	);

	xi->transform = TransformGetTransform(&xi->tsystem);

	xi->width = xwa.width;
	xi->height = xwa.height;
	xi->dim = xwa.width;


	xi->dpy = dpy;
	xi->win = win;
	xi->line_gc = line_gc;
	xi->marker_gc = mark_gc;
	xi->fill_gc = fill_gc;
	xi->cell_gc = cell_gc;
	xi->text_gc = text_gc;
	xi->bg_gc = bg_gc;

	xi->marker_size = 1.0;

	return(0);
}


/*
 *
 *	N.B.
 *	This version of OpenWorkstation doesn't create its own rendering
 *	window. Instead it uses the window id passed to it in 
 *	gksc->i.list[0]. Other than that it is identical to 
 *	X11_OpenWorkstation().
 */

/*ARGSUSED*/
X11P_OpenWorkstation(gksc)
	GKSC	*gksc;
{
        int             *iptr = (int *) gksc->i.list;

	static	XWMHints	xwmh = {
		(InputHint | StateHint ),/* flags 			*/
		True,			/* input 			*/
		NormalState,		/* initial_state 		*/
		0,			/* icon pixmap 			*/
		0,			/* icon window 			*/
		0, 0,			/* icon location 		*/
		0,			/* icon mask 			*/
		0			/* Window group 		*/
	};
	
	static	XClassHint	xch = {
		"xgks",			/* resource name		*/
		"Xgks"			/* class name			*/
	};


	Pixeltype fg, bg, bd;		/* Pixel values 		*/
	XSetWindowAttributes xswa;	/* Set Window Attribute struct 	*/
	XTextProperty	window_name,
			icon_name;
	unsigned long bw = 0;		/* Border width 		*/
	Drawable	drawable;
	XGCValues	gcv;		/* struc for manipulating a GC	*/
	XEvent      event;		/* Event received 		*/
	XWindowAttributes	xwa;	/* Get window attributes	*/
	CoordSpace	square_screen;

	GC	line_gc, mark_gc, fill_gc,
		cell_gc, text_gc, bg_gc;



	Display	*dpy;
	Window	win;

	Xddp	*xi;

	char	*dpy_name;		/* display name			*/
	XSizeHints	*xshptr;

	/*
	 * the window id is the first element in iptr
	 */
	drawable =  (Drawable) iptr[0];
	win = drawable;

	if ((xi = (Xddp *) malloc (sizeof (Xddp))) == (Xddp *) NULL) {
		ESprintf(ERR_DTABLE_MEMORY, "malloc(%d)", sizeof(Xddp));
		return(ERR_DTABLE_MEMORY);
	}

	gksc->ddp = (GKSC_Ptr) xi;

	/*
	 *	establish connection to sever
	 */
	if ((dpy_name = getenv("DISPLAY")) == NULL) {
		ESprintf(ERR_NO_DISPLAY, 
			"X11 \"DISPLAY\" env. variable not set");
		return(ERR_NO_DISPLAY);
	}

	if ((dpy = XOpenDisplay(dpy_name)) == NULL) {
		ESprintf(ERR_OPN_DISPLAY, "  Error on opening X display (%s)", 
			dpy_name);
		return(ERR_OPN_DISPLAY);
	}

	/*
	 * initialize color
	 */
	xi->cmap = init_color(
		dpy, &fg, &bg, &bd, &(xi->color_ava), xi->color_pal, MAX_COLORS
	);


	/* 
	 * 	create default graphics contexts for the win.
	 * 	Use defaults except for backround, backround and line width.
	 */
	gcv.background = bg;
	gcv.foreground = fg;

	line_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);
	fill_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);
	mark_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);
	cell_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);
	text_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);

	/*
	 * create a background gc (gc for drawing in background color)
	 */
	gcv.background = fg;
	gcv.foreground = bg;
	bg_gc = XCreateGC(dpy, drawable, (GCForeground | GCBackground ),&gcv);

#ifdef	DEAD
	/*
 	 *      Select events for which we want future notification.
	 */
	XSelectInput(dpy,win, (ButtonPressMask | KeyPressMask ));
#endif

	TransformSetWindow(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);
	TransformSetViewport(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);
	TransformSetNDScreenSpace(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);

	/*
	 *	find out how big window is. calculate
	 *	coordinate translation macros
	 */
	if (XGetWindowAttributes(dpy, win, &xwa) == 0) {
		ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
		return(ERR_WIN_ATTRIB);
	}


	square_screen = ComputeLargestSquare(
		(double) 0.0, (double) (xwa.height - 1),
		(double) (xwa.width - 1), (double) 0.0
	);
	TransformSetScreenSpace(
		&xi->tsystem, square_screen.llx, square_screen.lly, 
		square_screen.urx, square_screen.ury
	);

	xi->transform = TransformGetTransform(&xi->tsystem);

	xi->width = xwa.width;
	xi->height = xwa.height;
	xi->dim = xwa.width;


	xi->dpy = dpy;
	xi->win = win;
	xi->line_gc = line_gc;
	xi->marker_gc = mark_gc;
	xi->fill_gc = fill_gc;
	xi->cell_gc = cell_gc;
	xi->text_gc = text_gc;
	xi->bg_gc = bg_gc;

	xi->marker_size = 1.0;

	return(0);
}

/*ARGSUSED*/
X11_ActivateWorkstation(gksc)
	GKSC	*gksc;
{
	Xddp	*xi = (Xddp *) gksc->ddp;
	Display	*dpy = xi->dpy;
	Window	win = xi->win;

	XWindowAttributes	xwa;	/* Get window attributes	*/
	CoordSpace	square_screen;


	/*
	 *	Find out how big the window is; calculate the
	 *	coordinate translation macros.
	 */
	if (XGetWindowAttributes(dpy, win, &xwa) == 0) {
		ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
		return(ERR_WIN_ATTRIB);
	}

	square_screen = ComputeLargestSquare(
		(double) 0.0, (double) (xwa.height - 1),
		(double) (xwa.width - 1), (double) 0.0
	);
	TransformSetScreenSpace(
		&xi->tsystem, square_screen.llx, square_screen.lly, 
		square_screen.urx, square_screen.ury
	);

	xi->transform = TransformGetTransform(&xi->tsystem);

	xi->width = xwa.width;
	xi->height = xwa.height;
	xi->dim = xwa.width;

	return(0);
}

/*ARGSUSED*/
X11_DeactivateWorkstation(gksc)
	GKSC	*gksc;
{
	return(0);
}

/*ARGSUSED*/
X11_UpdateWorkstation(gksc)
	GKSC	*gksc;
{
	Xddp	*xi = (Xddp *) gksc->ddp;
	Display	*dpy = xi->dpy;
	Window	win = xi->win;

	XSync(dpy, False);
	return(0);
}


/*ARGSUSED*/
X11_CloseWorkstation(gksc)
	GKSC	*gksc;
{
	Xddp	*xi = (Xddp *) gksc->ddp;
	Display	*dpy = xi->dpy;
	Window	win = xi->win;

	XCloseDisplay(dpy);
	free((char *) xi);

	return(0);
}

/*ARGSUSED*/
X11_ClearWorkstation(gksc)
	GKSC	*gksc;
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;
        Window  win = xi->win;

	XWindowAttributes	xwa;	/* Get window attributes	*/
	CoordSpace	square_screen;

        /* 
	 *	clear the screen
	 */
        XClearWindow(dpy,win);

	/*
	 *	find out how big window is. calculate coordinate translation 
	 *	macros. (The user may have resized the window between frames).
	 */
	if (XGetWindowAttributes(dpy, win, &xwa) == 0) {
		ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
		return(ERR_WIN_ATTRIB);
	}

	square_screen = ComputeLargestSquare(
		(double) 0.0, (double) (xwa.height - 1),
		(double) (xwa.width - 1), (double) 0.0
	);
	TransformSetScreenSpace(
		&xi->tsystem, square_screen.llx, square_screen.lly, 
		square_screen.urx, square_screen.ury
	);

	xi->transform = TransformGetTransform(&xi->tsystem);

	xi->width = xwa.width;
	xi->height = xwa.height;
	xi->dim = xwa.width;


	return(0);
}



/*ARGSUSED*/
X11_Esc(gksc)
	GKSC	*gksc;
{
	Xddp	*xi = (Xddp *) gksc->ddp;
	Display	*dpy = xi->dpy;
	Window	win = xi->win;

	char	*sptr = (char *) gksc->s.list;
	int	*iptr = (int *) gksc->i.list;

	int	flag = iptr[0];

	switch (flag) {
	case	ESCAPE_PAUSE:
		pause(dpy);
		break;

	case	ESCAPE_CMAP:
		xi->cmap = atoi(sptr);
		break;

	default:
		return(ERR_INV_ESCAPE);
	}

	return(0);
}

/*ARGSUSED*/
X11P_Esc(gksc)
	GKSC	*gksc;
{
	Xddp	*xi = (Xddp *) gksc->ddp;
	Display	*dpy = xi->dpy;
	Window	win = xi->win;

	char	*sptr = (char *) gksc->s.list;
	int	*iptr = (int *) gksc->i.list;

	int	flag = iptr[0];

	switch (flag) {

	/*
	 * N.B. No pause for the X11P device since we cannot select the 
	 * events we want to receive since we do not own the drawing canvas
	 * window.
	 */
#ifdef	DEAD
	case	ESCAPE_PAUSE:
		pause(dpy);
		break;
#endif

	case	ESCAPE_CMAP:
		xi->cmap = atoi(sptr);
		break;

	default:
		return(ERR_INV_ESCAPE);
	}

	return(0);
}
