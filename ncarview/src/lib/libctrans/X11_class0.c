/*
 *	$Id: X11_class0.c,v 1.21 1992-12-12 00:22:04 clyne Exp $
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
/*	X11_class0.c:
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *				8/19/88
 *
 *	This file contains the functions that implement the class 0
 *	CGM elements under X. The class 0 elements are: BEGIN METAFILE,
 *	END METAFILE, BEGIN PICTURE, BEGIN PICTURE BODY and END PICTURE.
 *	These functions are concerned mostly with initializtation, shutdown
 *	and user interaction with the X server.
 */
/*LINTLIBRARY*/

#define	X11_class0
#define	APP_DEF		"/usr/lib/X11/app-defaults/"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <ncarg/c.h>
#include "cgmc.h"
#include "default.h"
#include "Xdefs.h"
#include "ctrandef.h"
#include "translate.h"
#include "Xcrm.h"
#define BORDER		1
#define BORDERWIDTH	0
#define	LOGO	"NCAR Graphics"

#define	FONT	"8x13bold"

extern	boolean	stand_Alone;
extern	boolean	deviceIsInit;
extern	boolean	Batch;
extern	char	**Argv;
extern	int	Argc;
extern	int	optionDesc;
extern	boolean	Color_ava;
extern	boolean	*softFill;
extern	boolean	*doBell;
extern	boolean	startedDrawing;

extern	int	init_color();
extern	int	init_polygon();

static	struct	{
	char	*Geometry;
	char	*window;
	char	*viewport;
	char	*foreground;
	char	*background;
	boolean	reverse;
	int	wid;
	} x11_opts;

static	Option	options[] =  {
	{
	"geometry", NCARGCvtToString, 
		(Voidptr) &x11_opts.Geometry, sizeof (x11_opts.Geometry )
	},
	{
	 "window", NCARGCvtToString, 
		(Voidptr) &x11_opts.window, sizeof (x11_opts.window )
	},
	{
	"viewport", NCARGCvtToString, 
		(Voidptr) &x11_opts.viewport, sizeof (x11_opts.viewport )
	},
	{
	"foreground", NCARGCvtToString, 
		(Voidptr) &x11_opts.foreground, sizeof (x11_opts.foreground )
	},
	{
	"background", NCARGCvtToString, 
		(Voidptr) &x11_opts.background, sizeof (x11_opts.background )
	},
	{
	"reverse", NCARGCvtToBoolean, 
		(Voidptr) &x11_opts.reverse, sizeof (x11_opts.reverse )
	},
	{
	"wid", NCARGCvtToInt, 
		(Voidptr) &x11_opts.wid, sizeof (x11_opts.wid )
	},
	{
	NULL
	},
};

#include	"ncaricon.bit"
static	struct {
	int	width,
		height;
	Pixmap	pmap;
	} icon = { ctrans_width, ctrans_height, 0};


/*
 * This structure forms the WM_HINTS property of the window,
 * letting the window manager know how to handle this window.
 * See Section 9.1 of the Xlib manual.
 */
static	XWMHints	xwmh = {
	(InputHint
	| StateHint 
	),	/* flags 			*/
	False,			/* input 			*/
	NormalState,		/* initial_state 		*/
	0,			/* icon pixmap 			*/
	0,			/* icon window 			*/
	0, 0,			/* icon location 		*/
	0,			/* icon mask 			*/
	0,			/* Window group 		*/
};

static	XFontStruct	*fontstruct = NULL;/* Font descriptor for title bar*/
static	Pixeltype fg, bg, bd;		/* Pixel values 		*/
static	XEvent      event;		/* Event received 		*/
static	XSizeHints  xsh;		/* Size hints for window manager*/
static	XSetWindowAttributes xswa;	/* Set Window Attribute struct 	*/
static	unsigned long bw = BORDERWIDTH;	/* Border width 		*/
static	GC	titleGC;	/* GC for title bar containing NCAR logo*/



static	struct {		/* current info about the device*/	
	int	height,
		width;
	} dev; 		

static	struct {		/* possistion and height of title bar	*/
	int	x, y,
		height;
	} title = {0,0,0}; 

static	CoordModifier	dev_coord_mod = {0,0,1.0,1.0};
/*
 *	The class 0 CGM element functions
 */


/*ARGSUSED*/
int	X11_BegMF(c)
CGMC *c;
{

	char	*dpy_name;

	XButtonPressedEvent	*button_event;
	XKeyEvent		*key_event;
	int			len;
	char			keybuffer[8];
	char			*dpy_env = "DISPLAY";
	int			status = 0;
	char			*prog_name;

	if (deviceIsInit) {
		(void) X11_EndMF(c);
	}



	/*
	 *      parse X11 specific command line args
	 *      (currently only geometry accepted       )
	 */
	if (GetOptions(optionDesc, options) < 0) {
		ESprintf(
			E_UNKNOWN,"GetOptions(%d,) [ %s ]",
			optionDesc, ErrGetMsg()
		);
		return(-1);
	}

	/*
	 *	establish connection to sever
	 */
	if ((dpy_name = getenv(dpy_env)) == NULL) {
		ESprintf(E_UNKNOWN,"%s env. variable not set", dpy_env);
		return(-1);
	}


	if ((dpy = XOpenDisplay(dpy_name)) == NULL) {
		ESprintf(E_UNKNOWN,"XOpenDisplay(%s)", dpy_name);
		return (-1);
	}


	/*
	 *	load the font to use. See section 10.2 & 6.5.1
	 */
	if (!Batch)
		fontstruct = XLoadQueryFont(dpy, FONT);

	/*
	 * set device viewport specification
	 */
	if (x11_opts.viewport) {
		int	llx, lly, urx, ury;

		if (CoordStringToInt(x11_opts.viewport,&llx,&lly,&urx,&ury)<0){
			ESprintf(
				E_UNKNOWN,
				"Invalid viewport format [ %s ]", ErrGetMsg()
			);
			status = -1;
		}
		else {
			SetDevViewport(
				(long) llx, (long) lly,(long) urx,(long) ury
			);
		}
	}

	/*
	 * set device window specification
	 */
	if (x11_opts.window) {
		int	llx, lly, urx, ury;

		if (CoordStringToInt(x11_opts.window,&llx,&lly,&urx,&ury)<0){
			ESprintf(
				E_UNKNOWN,
				"Invalid window format [ %s ]", ErrGetMsg()
			);
			status = -1;
		}
		else {
			SetDevWin((long) llx, (long) lly,(long) urx,(long) ury);
		}
	}






	/*
	 * Create the Window with the information in the XSizeHints, the
	 * border width, and the border & background pixels. See 
	 * Section 3.3.
	 */
	if (x11_opts.wid == -1) {
		/*
		 *      Initialize the Resource manager. See 10.11
		 */
		XrmInitialize();
		/*
		 *      provide window with intial size and possition. Fill out
		 *      XSizeHints struct to inform window manager. See section
		 *      9.1.6 and 10.3
		 */
		do_geometry(x11_opts.Geometry, &xsh);
		drawable = XCreateSimpleWindow(dpy, 
			RootWindow(dpy,DefaultScreen(dpy)),
			xsh.x, xsh.y, xsh.width, xsh.height,
			bw, bd, bg);

		/*
		 *	read in pixmap for icon
		 */
	       icon.pmap = XCreateBitmapFromData(
			dpy, drawable, (const char *) ctrans_bits,
			icon.width, icon.height
		);
  
		/*
		 * Set the standard properties for the window managers. 
		 * See Section  9.1.
		 */
		prog_name = (
			(prog_name = strrchr(*Argv,'/')) ? ++prog_name : *Argv
			);

		XSetStandardProperties(dpy, drawable, prog_name,
			prog_name, icon.pmap, Argv, Argc, &xsh);

		xwmh.icon_pixmap = icon.pmap;
		xwmh.flags |= IconPixmapHint;
		XSetWMHints(dpy, drawable, &xwmh);

	}
	else {	/* else use window provided on command line	*/
		drawable = x11_opts.wid;
	}
	win = drawable;


	/*
	 *		intitialize color map if available
	 *	select default colours for border, background and foreground 
	 *	of window. These colours will be used if the user doesn't 
	 *	supply his own colour table through the CGM.
	 */
	startedDrawing = FALSE;
	(void) init_color(x11_opts.foreground, x11_opts.background, 
			(boolean) x11_opts.reverse, &fg, &bg, &bd);

	/*
	 * make sure we have a contrasting window background by default.
	 * this will most likely get overiden by the CGM.
	 */
	XSetWindowBackground(dpy, win, bg);
	XSetWindowBorder(dpy, win, bd);

	/*
	 * Ensure that the window's colormap field points to the default
	 * colormap,  so that the window manager knows the correct 
	 * colormap to use for the window.  See Section 3.2.9. Also,  
	 * set the window's Bit Gravity to reduce Expose events.
	 */
	xswa.colormap = DefaultColormap(dpy, DefaultScreen(dpy));
	xswa.bit_gravity = CenterGravity;
	xswa.backing_store = WhenMapped;
	XChangeWindowAttributes(dpy, win, (CWColormap 
				| CWBitGravity 
				| CWBackingStore), &xswa);

	/* 
	 * 	create default graphics contexts for the win. See Section 5.3.	
	 * 	Use defaults except for backround, backround and line width.
	 */

	gcv.background = bg;
	gcv.foreground = fg;

	lineGC = XCreateGC(dpy, drawable,
			(GCForeground | GCBackground ), &gcv);

	markerGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);

	polygonGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);

	cellGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);

	if (fontstruct != NULL) {
		gcv.font = fontstruct->fid;
		titleGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground | GCFont), &gcv);
	}


	/*	tile GC has foreground and backround reversed	*/
	gcv.background = fg;
	gcv.foreground = bg;
	tileGC = XCreateGC(dpy, drawable, 
			(GCForeground | GCBackground ), &gcv);


	/*
	 *	intitialize polygon fill stuff
	 */
	if (init_polygon() != 0) {
		return(-1);
	}

	deviceIsInit = TRUE;

	/*
	 * if we created out own window we need to map it and wait for 
	 * it to become exposed
	 */
	if (x11_opts.wid == -1) {
		/* 
		 * Select notification of Expose event that is generated when
		 * the window is first mapped (becomes visible) to the screen.
		 */
		 XSelectInput(dpy, win, ExposureMask);

		/*
		 * Map the window to make it visible.  See Section 3.5.
		 */
		XMapWindow(dpy, win);

		/*
		 *	get expose event as window becomes visible. we can't
		 *	draw until after this 
		 */

		while(1) {
			/* get next event	*/
			XNextEvent(dpy, &event);

			/* find the last expose event on the event queue.
			 * See Section 8.4.5.1.
			 */
			if (event.type == Expose && event.xexpose.count == 0) {

				/*
				 * Remove any other pending Expose events from 
				 * the queue to avoid multiple repaints. 
				 * See Section 8.7.
				 */
				while (XCheckTypedEvent(dpy, Expose, &event))
				;
			
				break;
			}
		}
	}


	if (stand_Alone) {

		/*
		 *	Select events for which we want future notification.
		 *	These apply for the remainder of the program. 
		 *	See section 8.
		 */
		XSelectInput(dpy,win, (ButtonPressMask | KeyPressMask ));

		button_event = (XButtonPressedEvent *) &event;
		key_event = (XKeyEvent *) &event;

		if (!Batch) {
		while (TRUE) {

			XNextEvent(dpy, &event);

			switch (event.type) {

			case ButtonPress:

			/*	go on to next frame	*/
				if (button_event->button == Button1)
					return (status);

				/* leave event loop	*/
				break;

			case KeyPress:
			/* press q, Q, or ^C to abort further translation */
				len = XLookupString(key_event, 
					keybuffer, sizeof(keybuffer), 
					(KeySym *) NULL, 
					(XComposeStatus *) NULL);

				if (len == 1 && (keybuffer[0] == 'q' 
					|| keybuffer[0] == 'Q'
					|| keybuffer[0] == '\003')) {

					ESprintf(E_UNKNOWN,"Interruped");
					return(-1);

				}

			/* press space or return for next frame	*/
				if (len == 1 && (keybuffer[0] == ' '
					|| keybuffer[0] == ''))
				
					return (status);
				break;

			default:

				break;
			}	/* end switch */
		}	/* while	*/
		}

	}	/* if stand_Alone	*/
	

	return(status);
}

/*ARGSUSED*/
int	X11_EndMF(c)
CGMC *c;
{

	if (!deviceIsInit)
		return(0);

	/*
	 * free any color resources allocated (only try freeing if 
	 * color map is writable)
	 */
	if (Color_ava && 
		((visual->class == DirectColor)
		|| (visual->class == PseudoColor)
		|| (visual->class == GrayScale))) {

		free_colors();
	}

	XCloseDisplay(dpy);

	deviceIsInit = FALSE;
	return (0);
}

/*ARGSUSED*/
int	X11_BegPic(c)
CGMC *c;
{
	return (0);
}

/*ARGSUSED*/
int	X11_BegPicBody(c)
CGMC *c;
{
	XWindowAttributes	xwa;

	extern	short	devWinChange;



	CoordRect	dev_extent;

	startedDrawing = FALSE;
	/*
	 *	copy default table to working default table	
	 *	most of the CGM elements contain output attribute or
	 *	input processing information. This data is stored in a 
	 *	table in "default.h". SetInPic keeps the data up to date
	 *	for each new frame
	 */
	SetInPic((boolean) TRUE);

	/*
	 *	find out how big window is. if its changed size recalculate
	 *	coordinate translation macros
	 */
	if (XGetWindowAttributes(dpy, win, &xwa) == 0) {
		ESprintf(E_UNKNOWN, "XGetWindowAttributes(,,)");
		return(-1);
	}

	if (dev.height != xwa.height || dev.width != dev.width 
						|| DevWinChanged()){

		dev.height = xwa.height;
		dev.width = xwa.width;

		/*
		 * if software simulation of polygon filling is desired
		 * initialize soft sim module with new height and width
		 */
		if (*softFill) {
			if (initSoftSim(0, dev.width-1, 0, dev.height-1) < 0) {
				return(-1);
			}
		}

		if (fontstruct != NULL && !Batch) {

			/*
			 *	calculate possistion and heigth of title bar
			 *	See section 6.5
			 */
			title.x = (dev.width- XTextWidth(fontstruct, 
					LOGO, strlen(LOGO))) / 2;

			/* a hack to keep the title bar from
			 * being drawn if the window is too small
			 */
			if (title.x < 0)
				title.x = dev.width;

			title.y = dev.height - fontstruct->max_bounds.descent;
			title.height = fontstruct->max_bounds.ascent +
					fontstruct->max_bounds.descent + 2;

		}

		/*
		 *	calculate X device coordinate transfer macro
		 *	leave room for title bar
		 */
		dev_extent.llx = dev_extent.ury = 0;
		dev_extent.lly = dev.height - title.height - 1;
		dev_extent.urx = dev.width - 1;

		transinit(&dev_extent, dev_coord_mod, TRUE);
		GCSetClipExtent((short) dev_extent.llx, (short) dev_extent.ury,
				(short) dev_extent.urx, (short) dev_extent.lly);
	}
	return(0);
}


/*ARGSUSED*/
int	X11_EndPic(c)
CGMC *c;
{
	XButtonPressedEvent	*button_event;
	XKeyEvent		*key_event;
	int			len;
	char			keybuffer[8];
	boolean			loop;


	if (fontstruct != NULL) 
		/*
		 *	draw title bar
		 */
		XDrawString(dpy,drawable, titleGC, title.x, 
			title.y, LOGO, strlen(LOGO));

	
	/*
	 *	clear default table
	 */
	(void) SetInPic((boolean) False);


	/*
	 *	if not interactive don't perform any user interaction
	 */
	if (*doBell) XBell(dpy, 0);

	if (Batch) {
		XFlush(dpy);
		free_colors();
		return(0);
	}

	/*
	 * if not stand alone do not interact with user. Let interface do it
	 */
	if (! stand_Alone) {
		XFlush(dpy);
		free_colors();
		return(0);
	}

	/*
	 * discard all button press events that a impatient user
	 * may have aquired while waiting for a plot to finnish
	 */
	while(XCheckTypedEvent(dpy, ButtonPress, &event))
	;

	/* 
	 *	Beep & wait for the user to press mouse button 1,
	 *	space bar, return or quit.
	 */

	button_event = (XButtonPressedEvent *) &event;
	key_event = (XKeyEvent *) &event;

	loop = TRUE;
	while (loop) {

		XNextEvent(dpy, &event);

		switch (event.type) {

		case ButtonPress:
			/*
			 * go on to next frame
			 */
			if (button_event->button == Button1) {
				loop = FALSE;
			}
			break;

		case KeyPress:
			/* 
			 * press q, Q, or ^C to abort further translation
			 */
			len = XLookupString(key_event, 
				keybuffer, sizeof(keybuffer), 
				(KeySym *) NULL, 
				(XComposeStatus *) NULL);

			if (len == 1 && (keybuffer[0] == 'q' 
				|| keybuffer[0] == 'Q'
				|| keybuffer[0] == '\003')) {

				ESprintf(E_UNKNOWN, "Interruped");
				return(-1);

			}

			/*
			 * press space or return for next frame
			 */
			if (len == 1 && (keybuffer[0] == ' '
				|| keybuffer[0] == '')) {

				loop = FALSE;
			}
			break;

		default:
			break;
		} /* end switch */

	} /* end while */

	free_colors();
	return(0);
}

/*ARGSUSED*/
int	X11_ClearDevice(c)
CGMC *c;
{
	/* 
	 * clear the screen
	 */
	XClearWindow(dpy,win);
	return(0);
}




/* 
 *	do_geometry
 *	[internal]
 *
 *		parse command line arg for geometry string. Build complete
 *	geometry specification via command line arg, the resouce manager
 *	and system application default file
 *
 * on entry
 *	Geometry	: command line supplied geometry string
 * on exit
 *	xsh		: contains the geometry specification
 */
do_geometry(Geometry, xsh)
	char	*Geometry;
	XSizeHints  *xsh;		/* Size hints for window manager*/
{
	char	*name;
	char	Geostr[20];		/* default geometry string	*/
	char	*str_type[20];
	XrmDatabase	applicationDB;	/* pointer to application file	*/
	XrmValue	value;

	/*
	 *	create full path name to application default file for 
	 *	ctrans
	 */
	name = (char *) malloc ((unsigned) 
		(strlen(APP_DEF) + strlen("ctrans") + 1));

	(void) strcpy(name, APP_DEF);
	(void) strcat(name, "ctrans");

	/*
	 *	get user default if no geometry specified
	 */
	if (!Geometry) {
		Geometry = XGetDefault(dpy, "ctrans", "geometry");
	}

	/*
	 *	get application default file if any
	 */
	if ((applicationDB = XrmGetFileDatabase(name)) != NULL) {

		/* get the program default geometry string regardless of
		 * whether it has been specified on commandl line or in
		 * resource database, because those specifications may be
		 * partial. We're going to use XGeometry to fill in the 
		 * gaps
		 */
		if (XrmGetResource(applicationDB, "ctrans.geometry",
			"Ctrans.Geometry", str_type, &value) == True) {

			(void) strncpy(Geostr, value.addr, (int) value.size);
		}
		else
			Geostr[0] = '\0';
		
	}
	else {	/* no default applications file, bummer => hardwire	*/
		(void) strcpy(Geostr, "=512x512-0-0");
	}

	xsh->flags = XGeometry(dpy, DefaultScreen(dpy), Geometry, Geostr, 
		BORDERWIDTH, 1,1,0,0, 
		&(xsh->x), &(xsh->y), &(xsh->width), &(xsh->height));

}
