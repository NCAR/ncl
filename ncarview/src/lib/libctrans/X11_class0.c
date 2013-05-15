/*
 *	$Id: X11_class0.c,v 1.43 2008-07-27 03:18:42 haley Exp $
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
#include <X11/Xatom.h>
#include <ncarg/c.h>
#include "cgmc.h"
#include "default.h"
#include "Xdefs.h"
#include "ctrandef.h"
#include "translate.h"
#include "Xcrm.h"
#include "xresources.h"

#define BORDER		1
#define BORDERWIDTH	0
#define	LOGO	"NCAR Graphics"

#define	DEF_FONT	"8x13bold"

extern	boolean	deviceIsInit;
extern	boolean	Batch;
extern	char	**Argv;
extern	int	Argc;
extern	int	optionDesc;
extern	boolean	Color_ava;
extern	boolean	*softFill;
extern	boolean	*doBell;
extern	boolean	startedDrawing;

extern	int	init_polygon();

/*
 * convert an decimal or  hexadecimal string to its integer value
 * if the string is preceded by "0x" or "0X" it is in hexadecimal form.
 */
int     hex_to_xid(from, to)
        const char      *from;  /* the string   */
        Voidptr to;
{
        unsigned long	*ulptr   = (unsigned long *) to;
	char		*ptr;

        if (! from) {
                *ulptr = None;
        }
	else{
		*ulptr = strtoul(from,&ptr,0);
		if(*ulptr == 0 && from == ptr){
		ESprintf(E_UNKNOWN,"Convert(%s) to unsigned long failed", from);
			return(-1);
		}

	}
        return(1);
}


static	struct	{
	char		*Geometry;
	char		*window;
	char		*viewport;
	char		*foreground;
	char		*background;
	boolean		reverse;
	unsigned long	wid;
	boolean		ignorebg;
	boolean		pcmap;
	boolean		scmap;
	int		colerr;
	unsigned long	visual_id;
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
	"wid", hex_to_xid, 
		(Voidptr) &x11_opts.wid, sizeof (x11_opts.wid )
	},
	{
	"ignorebg", NCARGCvtToBoolean, 
		(Voidptr) &x11_opts.ignorebg, sizeof (x11_opts.ignorebg )
	},
	{
	"pcmap", NCARGCvtToBoolean, 
		(Voidptr) &x11_opts.pcmap, sizeof (x11_opts.pcmap )
	},
	{
	"scmap", NCARGCvtToBoolean, 
		(Voidptr) &x11_opts.scmap, sizeof (x11_opts.scmap )
	},
	{
	"colerr", NCARGCvtToInt, 
		(Voidptr) &x11_opts.colerr, sizeof (x11_opts.colerr )
	},
	{
	"visual", hex_to_xid, 
		(Voidptr) &x11_opts.visual_id, sizeof (x11_opts.visual_id )
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
static	XSetWindowAttributes xswa;	/* Set Window Attribute struct 	*/
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


Window	create_window(dpy, geometry, cmap, depth, visual)
	Display		*dpy;
	char		*geometry;
	Colormap	cmap;
	int		depth;
	Visual		*visual;
{
	XSetWindowAttributes	xswa;
	unsigned long		mask = 0;
	Window			win;
	XSizeHints  		xsh;	/* Size hints for window manager*/


	xsh.flags = 0L;
	xsh.flags = XWMGeometry(
		dpy, DefaultScreen(dpy), geometry, "512x512", BORDERWIDTH, 
		&xsh, &(xsh.x), &(xsh.y), &(xsh.base_width), &(xsh.base_height),
		&(xsh.win_gravity)
	);
	xsh.flags &= ~(PMinSize | PMaxSize);

	xswa.background_pixel = bg;		mask |= CWBackPixel;
	xswa.border_pixel = bd;			mask |= CWBorderPixel;
	xswa.colormap = cmap;			mask |= CWColormap;
	xswa.bit_gravity = CenterGravity;	mask |= CWBitGravity;
	xswa.backing_store = WhenMapped;	mask |= CWBackingStore;

	win = XCreateWindow(
		dpy, RootWindow(dpy,DefaultScreen(dpy)),
		xsh.x, xsh.y, xsh.base_width, xsh.base_height,
		BORDERWIDTH, depth, InputOutput, visual, mask, &xswa
	);

	XSetWMNormalHints(dpy, win, &xsh);

	return(win);
}





/*ARGSUSED*/
int	X11_BegMF(c)
CGMC *c;
{

	char	*dpy_name;

	XButtonPressedEvent	*button_event;
	XKeyEvent		*key_event;
	XTextProperty		textprop;
	XClassHint 		class_hint;
	XrmDatabase		xrm;
	int			len;
	char			keybuffer[8];
	char			*dpy_env = "DISPLAY";
	int			status = 0;
	char			*prog_name;
	char			class_name[80];
	char			*geometry;
	char			geom_string[80];
	int			depth;		/* window depth	*/
	char			*logo_font;

	if (deviceIsInit) {
		(void) X11_EndMF(c);
	}

	/*
	 *      parse X11 specific command line args
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

#ifdef	XSYNC
	XSynchronize(dpy, 1);
#endif


	if (x11_opts.ignorebg) {
		ignoreBGChanges = TRUE;
	}

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
	 *		intitialize color map if available
	 *	select default colours for border, background and foreground 
	 *	of window. These colours will be used if the user doesn't 
	 *	supply his own colour table through the CGM. init_color()
	 * 	sets bestVisual, Cmap, and DspDepth as a side effect.
	 *
	 * 	If we are creating our own window then we tell init_color
	 *	to find the best visual (idealy 8-bit, PseudoColor). If
	 *	we are using another application's window we expect that
	 *	window to have been created with the default visual, depth
	 *	and colormap;
	 */
	if(x11_opts.scmap || x11_opts.wid != None)
		ColorModel = CM_SHARED;
	else if(x11_opts.pcmap)
		ColorModel = CM_PRIVATE;
	else
		ColorModel = CM_MIXED;

	if(x11_opts.colerr < 0 || x11_opts.colerr > 100){
		ESprintf(E_UNKNOWN,"Invalid color error [%d]",x11_opts.colerr);
		status = -1;
		x11_opts.colerr = 10;
	}
	else if(x11_opts.colerr == 100)
		x11_opts.colerr = 0;
	ColorErr =(float)x11_opts.colerr*((float)(X_MAX_INTEN_DIST)/(float)100);
	ColorErr *= ColorErr;

	startedDrawing = FALSE;
	(void) init_color(
		x11_opts.foreground, x11_opts.background,
		(boolean) x11_opts.reverse,&fg, &bg, &bd,
		(x11_opts.wid != None)?None:x11_opts.visual_id
	);

	/*
	 * Create the Window with the information in the XSizeHints, the
	 * border width, and the border & background pixels. See 
	 * Section 3.3.
	 */
	if (x11_opts.wid == None) {

		prog_name = (
			(prog_name = strrchr(*Argv,'/')) ? ++prog_name : *Argv
			);
		strncpy(class_name, prog_name, sizeof(class_name) -1);
		class_name[0] = toupper(class_name[0]);

		/*
		 *      Initialize the Resource manager. 
		 */
		xrm = _CtOpenResources(
			dpy, prog_name, class_name, 0, NULL, 
			(XrmOptionDescRec *) NULL, 0
		);

		if (x11_opts.Geometry) {
			geometry = x11_opts.Geometry;
		}
		else {
			geometry = _CtGetResource(
				xrm, prog_name, class_name, "geometry", 
				"Geometry", "512x512"
			);
		}
		strncpy(geom_string, geometry, sizeof(geom_string) -1);
		if (!x11_opts.Geometry)
			free(geometry);	

		/*
		 *	load the font to use. See section 10.2 & 6.5.1
		 */
		if (!Batch) {
			logo_font = _CtGetResource(
					xrm, prog_name, class_name, "font",
					"Font", DEF_FONT
				);

			fontstruct = XLoadQueryFont(dpy, logo_font);
			free(logo_font);
		}

		drawable = create_window(
			dpy, geom_string, Cmap, DspDepth, bestVisual
		);

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

		xwmh.icon_pixmap = icon.pmap; xwmh.flags |= IconPixmapHint;
		textprop.value = (unsigned char *) prog_name;
		textprop.encoding = XA_STRING;
		textprop.format = 8;
		textprop.nitems = strlen(prog_name);
		class_hint.res_name = (char *)NULL;
		class_hint.res_class = class_name;

		XSetWMProperties(
			dpy, (Window) drawable, &textprop, 
			(XTextProperty *) NULL, 
			Argv, Argc, (XSizeHints *) NULL,
			&xwmh, &class_hint
		);

		/*
		 * change default colormap. Window Manager is responsible
		 * for swapping it in when sprite is in our window. We
		 * can only use a private colormap if ctrans creates
		 * the drawing window. Otherwise we rely on the externaly
		 * provided window to be created with the DefaultColormap
		 */
		if (x11_opts.pcmap) {
			XSetWindowColormap(dpy, drawable, Cmap);
		}

		_CtCloseResources(xrm);


	}
	else {	/* else use window provided on command line	*/
		drawable = x11_opts.wid;

		/*
		 * since we don't create the window we need to make sure
		 * that it has a contrasting background color in case
		 * metafile doesn't explicity set the background (This 
		 * may only be necessary for monochrome displays.)
		 */
		XSetWindowBackground(dpy, drawable, bg);
		XClearWindow(dpy, drawable);

	}
	win = drawable;


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
	if (x11_opts.wid == None) {
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



	if (!Batch && x11_opts.wid == None) {
		/*
		 *	Select events for which we want future notification.
		 *	These apply for the remainder of the program. 
		 *	See section 8.
		 */
		XSelectInput(dpy,win, (ButtonPressMask | KeyPressMask ));

		button_event = (XButtonPressedEvent *) &event;
		key_event = (XKeyEvent *) &event;

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

					ESprintf(E_UNKNOWN,"Interrupted");
					return(-1);

				}

			/* press space or return for next frame	*/
				if (len == 1 && (keybuffer[0] == ' '
					|| keybuffer[0] == 0x0D))
				
					return (status);
				break;

			default:

				break;
			}	/* end switch */
		}	/* while	*/

	}	/* if ! Batch	*/
	

	return(status);
}

/*ARGSUSED*/
int	X11_EndMF(c)
CGMC *c;
{
		

	if (!deviceIsInit)
		return(0);

	/* 
	 * hack to inform init_color that we no longer have a valid
	 * window
	 */
	Cmap = win = 0;	
	bestVisual = NULL;
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

	if (dev.height != xwa.height || 
		dev.width != dev.width || 
		DevWinChanged() ||
		VDC_EXTENT_DAMAGE) {

		dev.height = xwa.height;
		dev.width = xwa.width;

		if(fontstruct != NULL && !Batch && x11_opts.wid ==None){

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

		/*
		 * if software simulation of polygon filling is desired
		 * initialize soft sim module with new height and width
		 */
		if (*softFill) {
			if (initSoftSim(
				dev_extent.llx, dev_extent.urx,
				dev_extent.lly, dev_extent.ury
				) < 0) {

				return(-1);
			}
		}

		GCSetClipExtent(dev_extent.llx, dev_extent.ury,
				dev_extent.urx, dev_extent.lly);
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

	if (Batch || x11_opts.wid != None) {
		XSync(dpy, True);
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

				ESprintf(E_UNKNOWN, "Interrupted");
				return(-1);

			}

			/*
			 * press space or return for next frame
			 */
			if (len == 1 && (keybuffer[0] == ' '
				|| keybuffer[0] == 0x0D)) {

				loop = FALSE;
			}
			break;

		default:
			break;
		} /* end switch */

	} /* end while */

	return(0);
}

/*ARGSUSED*/
int	X11_ClearDevice(c)
CGMC *c;
{
	/* 
	 * clear the screen
	 */
	if (! ignoreBGChanges) {
		XClearWindow(dpy,win);
	}
	return(0);
}

