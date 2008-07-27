/*
 *	$Id: xrubber.c,v 1.18 2008-07-27 03:18:39 haley Exp $
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
#include <assert.h>
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
 *	*band_height	: width and height of the rubberband 
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
	int	*x, *y;
	unsigned	*band_width, *band_height;
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

		XUngrabServer(dpy);
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
	 * make sure width and heigh of rubber band are positive
	 */
	if (orig_x > new_x) {
		int	tmp = orig_x;
		orig_x = new_x;
		new_x = tmp;
	}
	if (orig_y > new_y) {
		int	tmp = orig_y;
		orig_y = new_y;
		new_y = tmp;
	}
		

	/*
	 * the return values
	 */
	*x = orig_x;
	*y = orig_y;
	*band_width = (unsigned) (new_x - orig_x + 1);
	*band_height = (unsigned) (new_y - orig_y + 1);

	XUngrabPointer(dpy, CurrentTime);
	XUngrabServer(dpy);

	return(1);
}

/*
 *	return the coordinates of largest rectangle which maintains the 
 *	given aspect ratio, $ar, and fits in the rectangle given by 
 *	$x, $y, $width, $height. The rectangle is centered in the 
 *	original rectangle.
 */
void	get_largest_rect(ar, x, y, width, height)
	double		ar;
	int		*x, *y;
	unsigned	*width, *height;
{
	double	ar2;

	assert (*width != 0);	/* should never happen	*/

	ar2 = (double) *width / (double) *height;

	if (ar < ar2) {	/* height is correct	*/
		int	width_;
		width_ = (int) ((ar * (double) *height) + 0.5);
		*x +=  (int) (((double) (*width - width_) / 2) + 0.5);
		*width = width_;
	}
	else {		/* width is correct	*/
		int	height_;
		height_ = (int) (((double) *width / ar) + 0.5);
		*y +=  (int) (((double) (*height - height_) / 2) + 0.5);
		*height = height_;
	}
}



/*
 *	ZoomCoords
 *	[exported]
 *
 *	Get the user's selection for coordinates for the ZOOM command
 * on entry
 *	*dpy		: the display
 *	*root		: the root window
 *	ar		: aspect ratio to maintain.
 * on exit
 *	*llx		: lower left x if non-null on entry.
 *	*lly		: lower left y if non-null on entry.
 *	*urx		: upper right x if non-null on entry.
 *	*ury		: upper right y if non-null on entry.
 *	return		: NULL => failure, else a string containing the user's
 *			  selection for lower left and upper right coords 
 *			  for the ictrans "zoom" command
 */	

char	*ZoomCoords(
#ifdef	__STDC__
		Display *dpy, Window root, float ar, float *llx, 
			float *lly, float *urx, float *ury)
#else
		dpy, root, ar, llx, lly, urx, ury)

		Display *dpy;
		Window root;
		float ar;
		float *llx;
		float *lly;
		float *urx;
		float *ury;

		
#endif
{

	int		wx, wy;		/* window origin	*/
	int		bx, by;		/* band origin		*/
	float		norm_bx, norm_by;	/* normalized band origin */
	float		norm_band_width,
			norm_band_height;	/* normalized band height */
	float		a, b;
	float		llx_, lly_, urx_, ury_;
	unsigned	band_width, band_height;
	unsigned	win_width, win_height;

	static	char	buf[80];

	/*
	 * User makes selection with a rubberband
	 */
	if (rubber_band_window(dpy, root, (Pixel) 1, &bx, &by, 
			&band_width, &band_height, 
			&win_width, &win_height) < 0) {

		return ((char *) NULL);
	}

#ifdef	DBX
	XSync(dpy, True);
#endif



	/*
	 * adjust the source window coordinates to maintain the given
	 * aspect ratio. i.e. find the largest rectangle within the 
	 * window which maintains the given aspect ratio. The rectangle is
	 * centered.
	 */
	wx = wy = 0;
	get_largest_rect(ar, &wx, &wy, &win_width, &win_height); 
	
	/*
	** clip the rubber band to the largest allowable window
	*/
	if (bx < wx) {
		band_width -= (wx - bx);
		bx = wx;
	}
	if (by < wy) {
		band_height -= (wy - by);
		by = wy;
	}
	if ((bx + band_width) > (wx + win_width)) {
		band_width -= ((bx + band_width) - (wx + win_width));
	}
	if ((by + band_height) > (wy + win_height)) {
		band_height -= ((by + band_height) - (wy + win_height));
	}

	if ((band_width <= 0) || (band_height <= 0)) {
		return ((char *) NULL);
	}

	/*
	 * convert the coordinates of the rubber band into normalized coords
	 * with respect to the above rectantle
	 */
	norm_band_width = (float) band_width / (float) win_width;
	norm_band_height = (float) band_height / (float) win_height;

	/*
	 * find the mapping for the coords
	 */
	a = 1.0 / (double) (win_width - 1);
	b = - ((double) wx * a);
	norm_bx = ((double) bx * a) + b;

	a = 1.0 / (double) (win_height - 1);
	b = - ((double) wy * a);
	norm_by = ((double) by * a) + b;


	/*
	 * map the rectangle in terms of its lower left and upper right
	 * coords. We assume an origin of lower left. (X origin is upper left)
	 */
	llx_ = norm_bx;
	urx_ = llx_ + norm_band_width;
	lly_ = 1.0 - (norm_by + norm_band_height);
	ury_ = 1.0 - norm_by;
	
	/*
	 * create the data string
	 */
	(void) sprintf(buf, 
		"%6.4f %6.4f %6.4f %6.4f",llx_,lly_,urx_,ury_);

	if (llx) *llx = llx_;
	if (lly) *lly = lly_;
	if (urx) *urx = urx_;
	if (ury) *ury = ury_;

	return(buf);
}
