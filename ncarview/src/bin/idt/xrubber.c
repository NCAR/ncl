/*
 *	$Id: xrubber.c,v 1.10 1992-12-14 22:05:44 clyne Exp $
 */
/*
 *	xrubber.c
 *	
 *	Author		John Clyne
 *
 *	Date		Fri Sep 21 08:10:23 MDT 1990
 *
 *	This file contains code used by the ZOOM command for doing 
 *	rubberbanding on a window.
 */
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Xmu/WinUtil.h>
#include "xrubber.h"

typedef	unsigned long	Pixel;


static	GC	rubberGC;
static	Cursor	rubberStartCursor;
static	Cursor	rubberDragCursor;


/*
 *	MoveOutline
 *	[internal]
 *	
 *	This code was taken from Tom's Window Manager. It undraws the old
 *	rubber band and redraws the new one.
 *
 * on entry
 *	*dpy		: the display
 *	root		: the root window
 *	x,y		: upper left coordinates of rubber band
 *	width, height	: width and height of band relative to x and y
 */
static	void MoveOutline(dpy, root, x, y, width, height)
	Display	*dpy;
	Window root;
	int x, y, width, height;
{
	static int	lastx = 0;
	static int	lasty = 0;
	static int	lastWidth = 0;
	static int	lastHeight = 0;
	static int	lastBW = 0;
	static int	lastTH = 0;
	int		xl, xr, yt, yb, xinnerl, xinnerr, yinnert, yinnerb;
	int		xthird, ythird;
	XSegment	outline[18];
	register XSegment	*r;

	if ((x == lastx) && (y == lasty) && 
			(width == lastWidth) && (height == lastHeight)) {
		return;
	}

	r = outline;

#define DRAWIT() \
	if (lastWidth || lastHeight)			\
	{						\
	xl = lastx;					\
	xr = lastx + lastWidth - 1;			\
	yt = lasty;					\
	yb = lasty + lastHeight - 1;			\
	xinnerl = xl + lastBW;				\
	xinnerr = xr - lastBW;				\
	yinnert = yt + lastTH + lastBW;			\
	yinnerb = yb - lastBW;				\
	xthird = (xinnerr - xinnerl) / 3;		\
	ythird = (yinnerb - yinnert) / 3;		\
				\
	r->x1 = xl;					\
	r->y1 = yt;					\
	r->x2 = xr;					\
	r->y2 = yt;					\
	r++;						\
				\
	r->x1 = xl;					\
	r->y1 = yb;					\
	r->x2 = xr;					\
	r->y2 = yb;					\
	r++;						\
				\
	r->x1 = xl;					\
	r->y1 = yt;					\
	r->x2 = xl;					\
	r->y2 = yb;					\
	r++;						\
				\
	r->x1 = xr;					\
	r->y1 = yt;					\
	r->x2 = xr;					\
	r->y2 = yb;					\
	r++;						\
				\
	r->x1 = xinnerl + xthird;			\
	r->y1 = yinnert;				\
	r->x2 = r->x1;					\
	r->y2 = yinnerb;				\
	r++;						\
				\
	r->x1 = xinnerl + (2 * xthird);			\
	r->y1 = yinnert;				\
	r->x2 = r->x1;					\
	r->y2 = yinnerb;				\
	r++;						\
				\
	r->x1 = xinnerl;				\
	r->y1 = yinnert + ythird;			\
	r->x2 = xinnerr;				\
	r->y2 = r->y1;					\
	r++;						\
				\
	r->x1 = xinnerl;				\
	r->y1 = yinnert + (2 * ythird);			\
	r->x2 = xinnerr;				\
	r->y2 = r->y1;					\
	r++;						\
				\
	if (lastTH != 0) {				\
	r->x1 = xl;					\
	r->y1 = yt + lastTH;			\
	r->x2 = xr;					\
	r->y2 = r->y1;				\
	r++;					\
	}						\
	}

	/* undraw the old one, if any */
	DRAWIT ();

	lastx = x;
	lasty = y;
	lastWidth = width;
	lastHeight = height;

	/* draw the new one, if any */
	DRAWIT ();

#undef DRAWIT


	if (r != outline) {
		XDrawSegments(dpy, root, rubberGC, outline, r - outline);
	}
}



static	void	init_rubber(dpy, win, fg)
	Display	*dpy;
	Window	win;
	Pixel	fg;
{
	unsigned	long	gcm;
	XGCValues	gcv;

	gcm = 0;
	gcm |= GCFunction;      gcv.function = GXxor;
	gcm |= GCLineWidth;     gcv.line_width = 0;
	gcm |= GCForeground;    gcv.foreground = fg;
	gcm |= GCSubwindowMode; gcv.subwindow_mode = IncludeInferiors;

	rubberGC = XCreateGC(dpy, win, gcm, &gcv);

	rubberStartCursor = XCreateFontCursor(dpy, XC_fleur);
	rubberDragCursor = XCreateFontCursor(dpy, XC_crosshair);
}


/*
 *	rubber_band_window
 *	[internal]
 *
 *	This routine allows a user to create a rubberband and resize it with
 *	the mouse. Upon invocation the server and mouse are grabbed and the
 *	sprite is changed to inform the user that the operation has begun. A
 *	button-down event will start the resize. A button-up event terminates
 *	the rubberbanding and causes rubber_band_window() to return with the
 *	coordinates and dimensions of the rubber band with respect to the
 *	window it was created in.
 * on entry
 *	*dpy		: the display
 *	root		: the root window for this display	
 *	fg		: forground pixel for rubberband
 * on exit
 *	*x,*y		: the x and y coordinates of one corner of the 
 *			  rubber band the user selected. Which corner this is
 *			  can be determined from band_width and band_height
 *	*band_width,
 *	*band_height	: width and height of the rubberband with respect to
 *			  (x,y). i.e. the values can be negative
 *	win_width,
 *	win_height	: width and height of the window that contained the
 *			  rubberband
 *	return		: -1 => failure, else ok
 */
static	rubber_band_window(dpy, root, fg, x, y, 
		band_width, band_height, win_width, win_height)

	Display	*dpy;
	Window	root;
	Pixel	fg;
	int	*x, *y, 
		*band_width, *band_height;
	unsigned	*win_width, *win_height;
{

	int 		orig_x;
	int 		orig_y;/* coords of fixed corner of the band	*/

	unsigned	width,
			height;/* width and height of window band in	*/

	static	short	firstTime = 1;	/* 0 after first invocation	*/
	Window	rubber_win;		/* the window to be rubbered	*/
	XEvent	event;

	Window		dummy_win;
	unsigned	dummy_un;
	int		dummy_int;

	int	root_x,root_y;		/* coords of fixed point of rubber */

	int	new_x, new_y,		/* track the rubber band coords	*/
		last_x, last_y;


	if (firstTime) {	/* do onetime initialization	*/
		init_rubber(dpy, root, fg);
		firstTime = 0;
	}

	/*
	 * grab the serve and pointer and change the sprite to the 
	 * selection font
	 */
	XGrabServer(dpy);
	if (XGrabPointer(dpy, root, True,
		ButtonPressMask | ButtonReleaseMask,
		GrabModeAsync, GrabModeAsync,
		root, rubberStartCursor, CurrentTime) != GrabSuccess) {

		(void) fprintf(stderr, "Can't grab the mouse\n");
		return(-1);
	}

	/*
	 * wait for user to start resize with a button-down in a window
	 */
	while (1) {
		if (XCheckMaskEvent(dpy, ButtonPressMask, &event)) {

#ifdef	DEAD
			root_x = event.xbutton.x;	/* position of sprite */
			root_y = event.xbutton.y;
			rubber_win= event.xbutton.subwindow;
#else
			root_x = event.xbutton.x_root;	/* position of sprite */
			root_y = event.xbutton.y_root;
			rubber_win = event.xbutton.window;
#endif


			/*
			 * won't work if window is root
			 */
			if (rubber_win == 0) {
				XBell(dpy, 100);
				continue;
			}

			/*
			 * get the window without the title bar
			 */
			rubber_win = XmuClientWindow(dpy, rubber_win);

			/*
			 * get the width and height of the window
			 */
			XGetGeometry(dpy, (Drawable) rubber_win, &dummy_win,
				&dummy_int, &dummy_int, &width, &height,
				&dummy_un, &dummy_un);

			*win_width = (unsigned) width;
			*win_height = (unsigned) height;

			/*
			 * get the coordinates of the pointer relative
			 * to the selected window (rubber_win)
			 */
			XTranslateCoordinates(dpy, root, rubber_win, 
				root_x, root_y, &orig_x, &orig_y, &dummy_win); 

			/*
			 * change the shape of the sprite to the drag pointer
			 * and confine it to the selection window. Do we
			 * realy need to ungrab the pointer
			 */
			XUngrabPointer(dpy, CurrentTime);
			XGrabPointer(dpy, root, True,
				ButtonPressMask | ButtonReleaseMask,
				GrabModeAsync, GrabModeAsync,
				rubber_win, rubberDragCursor, CurrentTime);

			break;

		}
	}

	/*
	 * move and resize rubberband as necessary until a button-release
	 */
	last_x = last_y = -99999;
	while (1) {
		XQueryPointer(dpy, rubber_win, &dummy_win, &dummy_win,
			&dummy_int, &dummy_int, &new_x, &new_y,
			(unsigned int *) &dummy_int);

		if ((last_x != new_x) || (last_y != new_y)) {

			last_y = new_y;
			last_x = new_x;

			/*
			 * draw the rubber band 
			 */
			MoveOutline(dpy, rubber_win, orig_x, orig_y,
				new_x - orig_x,
				new_y - orig_y);
		}

		if (XCheckMaskEvent(dpy, ButtonReleaseMask, &event)) {
			break;
		}
	}

	/*
	 * undraw the last rubber band
	 */
	MoveOutline(dpy, rubber_win, 0, 0, 0, 0);

	/*
	 * the return values
	 */
	*x = orig_x;
	*y = orig_y;
	*band_width = new_x - orig_x;
	*band_height = new_y - orig_y;

	XUngrabPointer(dpy, CurrentTime);
	XUngrabServer(dpy);

	return(1);
}

/*
 *	ZoomCoords
 *	[exported]
 *
 *	Get the user's selection for coordinates for the ZOOM command
 * on entry
 *	*dpy		: the display
 *	*root		: the root window
 * on exit
 *	*llx		: lower left x if non-null on entry.
 *	*lly		: lower left y if non-null on entry.
 *	*urx		: upper right x if non-null on entry.
 *	*ury		: upper right y if non-null on entry.
 *	return		: NULL => failure, else a string containing the user's
 *			  selection for lower left and upper right coords 
 *			  for the ictrans "zoom" command
 */	
char	*ZoomCoords(dpy, root, llx, lly, urx, ury)
	Display	*dpy;
	Window	root;
	float	*llx, *lly, *urx, *ury;
{

	int		x1, y1, x2, y2;
	float		norm_x1, norm_y1, norm_x2, norm_y2;
	int		tmp;
	int		band_width, band_height;
	unsigned	win_width, win_height;

	int		x_aspect_correct,
			y_aspect_correct;

	unsigned	dimension;	/* length of a side of window corrected
					 * for non-square aspect ratio	
					 */

	static	char	buf[80];

	/*
	 * User makes selection with a rubberband
	 */
	if (rubber_band_window(dpy, root, (Pixel) 1, &x1, &y1, 
			&band_width, &band_height, 
			&win_width, &win_height) < 0) {

		return ((char *) NULL);
	}

#ifdef	DBX
	XSync(dpy, True);
#endif

	if ((band_width == 0) || (band_height == 0)) {
		return ((char *) NULL);
	}

	/*
	 * correct for window border. Don't know why we have to do this
	 */
	if (x1 < 0) x1 = 0;	if (x1 > win_width) x1 = win_width;
	if (y1 < 0) y1 = 0;	if (y1 > win_height) y1 = win_height;

	x2 = x1 + band_width;
	y2 = y1 + band_height;

	if (x2 < 0) x2 = 0;	if (x2 > win_width) x2 = win_width;
	if (y2 < 0) y2 = 0;	if (y2 > win_height) y2 = win_height;


	/*
	 * The window containing the image may not be square. We assume
	 * that the translator corrects for this and maintains a square
	 * aspect ratio. ie. the entire window is NOT used. This code 
	 * constrains the bounds of the rubberband to the largest square
	 * that will fit in the window
	 */
	x_aspect_correct = y_aspect_correct = 0;
	if (win_width > win_height) {
		dimension = win_height;
		x_aspect_correct = (win_width - win_height) / 2;

		if (x1 < x_aspect_correct) 
			x1 = x_aspect_correct;

		if (x1 > x_aspect_correct + dimension) 
			x1 = x_aspect_correct + dimension;

		if (x2 < x_aspect_correct) 
			x2 = x_aspect_correct;

		if (x2 > x_aspect_correct + dimension) 
			x2 = x_aspect_correct + dimension;

		x1 -= x_aspect_correct;
		x2 -= x_aspect_correct;
	}
	else {
		dimension = win_width;
		y_aspect_correct = (win_height - win_width) / 2;

		if (y1 < y_aspect_correct) 
			y1 = y_aspect_correct;

		if (y1 > y_aspect_correct + dimension) 
			y1 = y_aspect_correct + dimension;

		if (y2 < y_aspect_correct) 
			y2 = y_aspect_correct;

		if (y2 > y_aspect_correct + dimension) 
			y2 = y_aspect_correct + dimension;

		y1 -= y_aspect_correct;
		y2 -= y_aspect_correct;
	}

	/*
	 * origin for translator is at lower left. For X its at upper left. We 
	 * want (x1,y1) to be lower left and  (x2, y2) to be upper right
	 */
	if (x1 > x2) {	/*	swap	*/
		tmp = x2;
		x2 = x1;
		x1 = tmp;
	}
	if (y1 < y2) {	/*	swap	*/
		tmp = y2;
		y2 = y1;
		y1 = tmp;
	}

	/*
	 * normalize the coordinates on [0.0 , 1.0]
	 */
	norm_x1 = (float) x1 / (float) dimension;
	norm_y1 = (float) y1 / (float) dimension;
	norm_x2 = (float) x2 / (float) dimension;
	norm_y2 = (float) y2 / (float) dimension;

	/*
	 * swap top and bottom
	 */
	norm_y1 = 1 - norm_y1;
	norm_y2 = 1 - norm_y2;

	/*
	 * create the data string
	 */
	(void) sprintf(buf, 
		"%6.4f %6.4f %6.4f %6.4f",norm_x1,norm_y1,norm_x2,norm_y2);

	if (llx) *llx = norm_x1;
	if (lly) *lly = norm_y1;
	if (urx) *urx = norm_x2;
	if (ury) *ury = norm_y2;

	return(buf);
}
