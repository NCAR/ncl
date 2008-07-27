/*
 *      $Id: animate.c,v 1.9 2008-07-27 03:18:38 haley Exp $
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
 *	File:		animate.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Aug 14 09:31:11 MDT 1992
 *
 *	Description:	Animate a sequence of XImages. This file provides
 *			an interface for animating a sequence of XImage
 *			structures in an X window.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <ncarg/c.h>
#include "animate.h"

/*
 * initialize an AnimatePixmaps struct.
 */
static	int	ap_construct(ap, dpy, canvas, size, src_x, src_y, dst_x, 
						dst_y, width, height)
	AnimatePixmaps	*ap;
	Display		*dpy;
	Widget		canvas;
	int		size;
	int		src_x,
			src_y,
			dst_x,
			dst_y;

	unsigned int	width,
			height;
{
	AnimatePixmap_	*ap_;
	int		i;

	ap_ = (AnimatePixmap_ *) malloc (sizeof(AnimatePixmap_) * size);
	if (! ap_) {
		ESprintf(errno, "malloc(%d)", sizeof(AnimatePixmap_) * size);
		return(-1);
	}

	/*
	 * create a circular linked list of AnimatePixmap_ structs for 
	 * easy access.
	 */
	ap_[0].prev = &ap_[size-1];
	ap_[size-1].next = &ap_[0];
	for(i=0; i<(size-1); i++) {
		ap_[i].next = &ap_[i+1];
		ap_[i+1].prev = &ap_[i];
		ap_[i].pixmap_created = False;
		ap_[i].whoami = i;
	}
	ap_[i].pixmap_created = False;
	ap_[i].whoami = i;


	ap->ap_ = ap_;
	ap->current = ap_;	/* current AnimatePixmap_ member	*/
	ap->size = size;
	ap->dpy = dpy;
	ap->src_x = src_x;
	ap->src_y = src_y;
	ap->dst_x = dst_x;
	ap->dst_y = dst_y;
	ap->width = width;
	ap->height = height;
	ap->canvas = canvas;
	ap->win = XtWindow(canvas);

	return(1);
}

/*
 *	free an AnimatePixmaps struct
 */
static	int	ap_destruct(ap)
	AnimatePixmaps	*ap;
{
	int	i;

	for (i=0; i<ap->size; i++) {
		if (ap->ap_[i].pixmap_created) {
			XFreePixmap(ap->dpy, ap->ap_[i].pixmap);
			ap->ap_[i].pixmap_created = False;
	
		}
	}

	free((Voidptr) ap->ap_);
}

/*
 *	load a sequence of images into pixmaps.
 */
static	int	ap_load(ap, gc, ximage, index)
	AnimatePixmaps	*ap;
	GC		gc;
	XImage		*ximage;
	unsigned	index;
{
	Pixmap		pixmap;
	int		screen = XDefaultScreen(ap->dpy);
	unsigned int	depth = DisplayPlanes(ap->dpy, screen);

	if (index >= ap->size) {
		ESprintf(E_UNKNOWN, "Invalid image index(%d)", index);
		return(-1);
	}

	pixmap = XCreatePixmap(ap->dpy, ap->win,ap->width,ap->height,depth);

	XPutImage(
		ap->dpy, pixmap, gc, ximage, 
		ap->src_x, ap->src_y, ap->dst_x, ap->dst_y, 
		ap->width, ap->height
	);

	if (ap->ap_[index].pixmap_created) {
		XFreePixmap(ap->dpy, ap->ap_[index].pixmap);
	}

	ap->ap_[index].pixmap = pixmap;
	ap->ap_[index].pixmap_created = True;

	return(0);
}

/*
 *	display a specific image in the X window
 */
static	int	ap_display(ap, image_num) 
	AnimatePixmaps	*ap;
	unsigned int	image_num;
{
	if (image_num >= ap->size) {
		return(-1);
	}

	ap->current = &ap->ap_[image_num];
	XSetWindowBackgroundPixmap(ap->dpy, ap->win, ap->ap_[image_num].pixmap);
	XClearArea(
		ap->dpy, ap->win, ap->dst_x, ap->dst_y, 
		ap->width, ap->height, False
	);
	XSync(ap->dpy,False);

	return(1);
}

/*
 *	display the next image in the sequence. The image sequence wraps
 *	around at the end. Update the current image.
 */
static	void	ap_display_next(ap)
	AnimatePixmaps	*ap;
{
	int	image_num;

	ap->current = ap->current->next;
	image_num = ap->current->whoami;
	(void) ap_display(ap, image_num);
}

/*
 *	display the previous image in the sequence. The image sequence wraps
 *	around at the beginning. Update the current image.
 */
static	void	ap_display_prev(ap) 
	AnimatePixmaps	*ap;
{
	int	image_num;

	ap->current = ap->current->prev;
	image_num = ap->current->whoami;
	(void) ap_display(ap, image_num);
}

/*
 * return the current image identifier. images are numbered from zero.
 */
static	int	ap_current_image(ap) 
	AnimatePixmaps	*ap;
{
	return(ap->current->whoami);
}

/*
**
**	A N I M A T E   S P I
**
*/

/*
 * Function:	AnimateOpen()
 *
 * Description:		Initialize the animation module. Return an Animation
 *			handle to be used for future Animate*() calls.
 *
 * In Args:
 *	*dpy		the display
 *	canvas		widget whose window in which the animation 
 *			will be displayed.
 *	n		Number of images which will be animated
 *	src_x		src offset in X from left edge of image.
 *	src_y		src offset in Y from top edge of image.
 *	width		src width of subimage within image
 *	height		src height of subimage within image
 *	dst_x		dst X of image in window to place image.
 *	dst_y		dst y of image in window to place image.
 *
 * Out Args:
 *
 * Return Values:
 *	NULL		=> failure, ESprintf() is invoked
 *	Animate*	=> OK
 *
 * Side Effects:
 *
 */
AnimateType	*AnimateOpen(dpy, canvas, n, 
					src_x, src_y, dst_x, 
					dst_y, width, height)
	Display		*dpy;
	Widget		canvas;
	int		n;
        int     	src_x,
                	src_y,
                	dst_x,
                	dst_y;
	unsigned int	width,
                	height;


{
	AnimateType	*a;


	if (n < 2) {
		ESprintf(E_UNKNOWN, "Animation requires at least 2 frames");
		return((AnimateType *) NULL);
	}

	if (! (a = (AnimateType *) malloc(sizeof(AnimateType)))) {
		ESprintf(errno, "malloc(%d)", sizeof(AnimateType));		
		return((AnimateType *) NULL);
	}

	/*
	 * load function pointers
	 */
	a->ap.ap = ap_construct;
	a->ap.not_ap = ap_destruct;
	a->ap.load = ap_load;
	a->ap.display = ap_display;
	a->ap.display_next = ap_display_next;
	a->ap.display_prev = ap_display_prev;
	a->ap.current_image = ap_current_image;

	/*
	 * call the AnimatePixmap constructor
	 */
	if (a->ap.ap(&a->ap, dpy, canvas, n, src_x, src_y, 
					dst_x, dst_y, width, height) < 0) {
		return((AnimateType *) NULL);
	}



	a->num_frames = n;
	a->do_loop = False;	/* looping not requested	*/
	a->do_stop = False;	/* stop not requested		*/
	a->is_animate = False;	/* not currently animating	*/
	a->delay = 0;		/* no delay between frames	*/
	a->app = XtWidgetToApplicationContext(canvas);

	return(a);
}

/*
 * Function:	AnimateLoad()
 *
 * Description: Load a single image for subsequent animation. The image $ximage
 *		will be the $index image in the sequence where 0 <= $index < n,
 *		where 'n' is the number of images specified in AnimateOpen().
 *
 *		AnimateLoad() shall be called at least 'n' times after 
 *		Animateopen(), once for each of the n images to
 *		be loaded. After all 'n' images have been loaded any of the
 *		other Animate*() functions may be called.
 *
 *		N.B. At first glance it might appear that a more reasonable
 *		interface would be to combine AnimateLoad() with AnimateOpen().
 *		AnimateOpen() would then accept an array of images instead
 *		of loading them one call at a time. Such an interface
 *		was considered and discarded because of the memory that would
 *		be imposed by the image array; memory requirements would
 *		be double what they currently are for animation.
 *		
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *	*ximage		XImage structure containing a single image. All
 *			successive calls to a AnimateLoad() between an
 *			AnimateOpen()/AnimateClose() pair must have identical
 *			XImage structures, save for the actual image.
 *	index		index of this image in the sequence.
 *
 * Out Args:
 *
 * Return Values:	
 *	< 0		error, else OK
 *
 * Side Effects:
 */
int	AnimateLoad(a, ximage, index)
	AnimateType	*a;
	XImage		*ximage;
	unsigned	index;
{
	GC		gc;	/* gc required by AnimatePixmap::load	*/
	XtGCMask	gc_mask = 0;	/* read-only gc			*/
	XGCValues	gcv;

        /*
         * need a gc for the Animation module
         */
        gc = XtGetGC(a->ap.canvas, gc_mask, &gcv);

	/*
	 * load the image array onto the server.
	 */
	if (a->ap.load(&a->ap, gc, ximage, index) < 0) {
		return(-1);
	}
	return(0);
}

/*
 * Function:		AnimateDisplayImage
 *
 * Description:		Display the specified image
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *	number		number of the image to display. Addressing starts
 *			from zero
 *
 * Out Args:
 *
 * Return Values:
 *	< 0		error, else OK
 *	
 * Side Effects:
 */
int	AnimateDisplayImage(a, number)
	AnimateType	*a;
	unsigned int	number;
{
	if (a->ap.display(&a->ap, number) < 0) {
		ESprintf(EINVAL, "invalid frame number(%d)", number);
		return(-1);
	}
	return(1);
}

/*
 * Function:		AnimateDisplayNext
 *
 * Description:		Display the "next" sequential image. If the 
 *			current image is the last image in the seqence the
 *			"next" image will be the first image in the sequence.
 *			i.e. the image sequence wraps around. The current image
 *			becomes the "next" image.
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *
 * Out Args:
 *
 * Return Values:
 *	
 * Side Effects:
 */
void	AnimateDisplayNext(a)
	AnimateType	*a;
{
	a->ap.display_next(&a->ap);
}


/*
 * Function:		AnimateDisplayPrev
 *
 * Description:		Display the "previous" sequential image. If the 
 *			current image is the first image in the seqence the
 *			"previous" image will be the last image in the sequence.
 *			i.e. the image sequence wraps around. The current image
 *			becomes the "previous" image.
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *
 * Out Args:
 *
 * Return Values:
 *	
 * Side Effects:
 */
void	AnimateDisplayPrev(a)
	AnimateType	*a;
{
	a->ap.display_prev(&a->ap);
}

/*
 * Function:		AnimateDispContForward
 *
 * Description:		Animate continuously forward. If looping has been
 *			set with AnimateLoop() the animation will loop as
 *			if the image sequence is a continous loop. Else the 
 *			animation will 	stop after the last image is displayed.
 *
 *			As a convenience this function dispatches
 *			X events via XtAppNextEvent()/XtDispatchEvent() in
 *			between the display of each image.
 *
 *			If the function pointer update_func() is non-null
 *			update_func is called after each image is displayed.
 *			update_func is passed $data as its first argument and
 *			the currently displayed image number as its second
 *			argument. update_func() is defined:
 *
 *			void	update_func(Voidptr data, int num); 
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *	*update_func	update function if not NULL.
 *	*data		caller data passed to update_func().
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *
 *			This function may call other functions via X
 *			callbacks
 */
void	AnimateDispContForward(a, update_func, data)
	AnimateType	*a;
#ifdef	__STDC__
	void		(*update_func)(Voidptr, int);	
#else
	void		(*update_func)();	
#endif
	Voidptr		data;
{
	int	i;
	XEvent	xevent;

	a->do_stop = False;	/* don't stop yet		*/
	a->is_animate = True;	/* we are animating		*/

	if (a->do_loop) {
		while (! a->do_stop) {
			AnimateDisplayNext(a);
			while(XtAppPending(a->app)) {
				XtAppNextEvent(a->app, &xevent);
				XtDispatchEvent(&xevent);
			}

			if (update_func) {
				update_func(data, a->ap.current_image(&a->ap));
			}
			if (a->delay) {	/*	delay in 1/100 sec*/
				USleep((a->delay)*10000); 
			}

		}
	}
	else {
		for(i=(a->ap.current_image(&a->ap) + 1); i<a->num_frames; i++) {
			AnimateDisplayNext(a);
			while(XtAppPending(a->app)) {
				XtAppNextEvent(a->app, &xevent);
				XtDispatchEvent(&xevent);
			}
			if (update_func) {
				update_func(data, a->ap.current_image(&a->ap));
			}
			if (a->delay) {	/*	delay in 1/100 sec*/
				USleep((a->delay)*10000); 
			}
		}
	}

	a->is_animate = False;	/* no longer animating	*/
}



/*
 * Function:		AnimateDispContReverse
 *
 * Description:		Animate continuously backwards. If looping has been
 *			set with AnimateLoop() the animation will loop as
 *			if the image sequence is a continous loop. Else the 
 *			animation will 	stop after the first image in the 
 *			animation sequence is displayed.
 *
 *			As a convenience this function calls 
 *			X events via XtAppNextEvent()/XtDispatchEvent() in
 *			between the display of each image.
 *
 *			If the function pointer update_func() is non-null
 *			update_func is called after each image is displayed.
 *			update_func is passed $data as its first argument and
 *			the currently displayed image number as its second
 *			argument. update_func() is defined:
 *
 *			void	update_func(Voidptr data, int num); 
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *			This function may call other functions via X
 *			callbacks
 */
void	AnimateDispContReverse(a, update_func, data)
	AnimateType	*a;
#ifdef	__STDC__
	void		(*update_func)(Voidptr, int);	
#else
	void		(*update_func)();	
#endif
	Voidptr		data;
{
	int	i;
	XEvent	xevent;

	a->do_stop = False;
	a->is_animate = True;

	if (a->do_loop) {
		while (! a->do_stop) {
			AnimateDisplayPrev(a);
			while(XtAppPending(a->app)) {
				XtAppNextEvent(a->app, &xevent);
				XtDispatchEvent(&xevent);
			}

			if (update_func) {
				update_func(data, a->ap.current_image(&a->ap));
			}
			if (a->delay) {	/*	delay in 1/100 sec*/
				USleep((a->delay)*10000); 
			}
		}
	}
	else {
		for(i=a->ap.current_image(&a->ap); i>0; i--) {
			AnimateDisplayPrev(a);
			while(XtAppPending(a->app)) {
				XtAppNextEvent(a->app, &xevent);
				XtDispatchEvent(&xevent);
			}
			if (update_func) {
				update_func(data, a->ap.current_image(&a->ap));
			}
			if (a->delay) {	/*	delay in 1/100 sec*/
				USleep((a->delay)*10000); 
			}
		}
	}

	a->is_animate = False;
}

/*
 * Function:		AnimateStop
 *
 * Description:		Stop an animation initiated with AnimateDispContForward
 *			or AnimateDispContReverse. 
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
void	AnimateStop(a)
	AnimateType	*a;
{
	a->do_stop = True;
}

/*
 * Function:		AnimateLoop
 *
 * Description:		Toggle looping mode. By default looping is turned
 *			off. If turned on subsequent calls to 
 *			AnimateDispContForward or AnimateDispContReverse
 *			will result in an infinite looping animation that
 *			may only be stopped with a call to AnimateStop().
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
void	AnimateLoop(a)
	AnimateType	*a;
{
	a->do_loop = ! a->do_loop;
}

/*
 * Function:		AnimateGetImageNum
 *
 * Description:		Return the current image number. Images are numbered
 *			starting from zero.
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *
 * Out Args:
 *
 * Return Values:
 *			return current image number.
 *
 * Side Effects:
 */
int	AnimateGetImageNum(a)
	AnimateType	*a;
{
	return(a->ap.current_image(&a->ap));
}

/*
 * Function:		AnimateSetDelay
 *
 * Description:		Set time delay in 1/100 sec for continous animation
 *			By default there is no delay.
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *	time		time in 1/100 second.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
void	AnimateSetDelay(a, time)
	AnimateType	*a;
	unsigned int	time;
{
	a->delay = time;
}

/*
 * Function:		AnimateClose
 *
 * Description:		Terminate animation session. WARNING AnimateClose()
 *			Should not be called from within AnimateDispContForward
 *			or AnimateDispContReverse by way of a callback. i.e.
 *			callbacks which invoke AnimateClose should be disabled
 *			when AnimateDispCont... is called.
 *
 * In Args:
 *	*a		Pointer an Animate struct created by AnimateOpen()
 *
 * Out Args:
 *	-1		Error, AnimateClose called from AnimateDispCont...().
 *
 * Side Effects:
 */
int	AnimateClose(a)
	AnimateType	*a;
{
	if (a->is_animate) {
		return(-1);	/* can't call from within AnimateDispCont..*/
	}
	a->ap.not_ap(&a->ap);
	free((Voidptr) a);
	return(1);
}
