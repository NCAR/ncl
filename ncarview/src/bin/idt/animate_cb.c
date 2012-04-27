/*
 *      $Id: animate_cb.c,v 1.8 2008-07-27 03:18:38 haley Exp $
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
 *	File:		animate_cb.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 17 12:20:15 MDT 1992
 *
 *	Description:	Handle the ANIMATE call back.
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <ncarg/c.h>
#include "animate.h"
#include "talkto.h"
#include "display.h"

/*
 * call the translator to plot an image in the drawing canvas
 */
static	int	plotit(id, frame) 
	int	id;
	int	frame;
{
	char	cmd[80];
	sprintf(cmd, "%d plot\n", frame);

	if (! TalkTo(id, cmd, SYNC)) {
		return(-1);
	}
	else {
		return(1);
	}
}


/*
 * initialize the animation module
 */
static	int	animate_on(wd)
	WidgetData	*wd;
{
	int		frame;		/* current frame number	*/
	int		skip = wd->pcv.skip + 1;
	int		start = wd->pcv.start_segment;
	int		stop = wd->pcv.stop_segment;
	int		num = ((stop - start) / skip) + 1;	/* num frames */
	XImage		*ximage;	/* XImage describing images in $images*/
	AnimateType	*a;
	int		i;
	char		buf[80];
	XWindowAttributes	xwa;
	Arg			args[5];

	sprintf(buf, "Loading %d images for animation\n", num);
	Message(wd->id, buf);

	if (num < 2) {
		ErrorMessage(wd->id,"Need at least 2 images to animate");
		return(-1);
	}

	wd->a = (AnimateType *) NULL;

	/*
	 * turn off buttons that don't have any function when in animation
 	 * mode.
	 */
	XtSetArg(args[0], XtNsensitive, False);
	XtSetValues(wd->dup, args, 1);
	XtSetValues(wd->zoom, args, 1);
	XtSetValues(wd->unzoom, args, 1);
	XtSetValues(wd->skip, args, 1);
	XtSetValues(wd->start_segment, args, 1);
	XtSetValues(wd->stop_segment, args, 1);
	XtSetValues(wd->set_window, args, 1);
	XtSetValues(wd->current_frame, args, 1);
	XtSetValues(wd->save, args, 1);
	XtSetValues(wd->print, args, 1);

	/*
	 * turn on delay button
	 */
	XtSetArg(args[0], XtNsensitive, True);
	XtSetValues(wd->delay, args, 1);

	/*
	 * find out how big the window is where the animation will be
	 * displayed.
	 */
	if (! XGetWindowAttributes(wd->dpy, wd->win, &xwa)) {
		ErrorMessage(wd->id,"Animation failed in XGetWindowAttributes");
		return(-1);
	}

	if (! (a = AnimateOpen(
		wd->dpy, wd->canvas, num, 
		0, 0, 0, 0, xwa.width, xwa.height))) {

		ErrorMessage(wd->id,"Animation failed in AnimateOpen()");
		return(-1);

	}

	/*
	 * plot each picture in the list, grab the resulting image.
	 */

	XSynchronize(wd->dpy,True);
	for(i=0, frame=start; i<num; frame+=skip, i++) {

		/*
		 * plot the image to the window. We have to plot the image
		 * in order to get it into raster form.
		 */
		if (plotit(wd->id, frame) < 0) {
			ErrorMessage(wd->id, "Animation failed");
			XSynchronize(wd->dpy,False);
			return(-1);
		}
			
		/*
		 *int usleep(useconds_t usec);

		 *DESCRIPTION The usleep() function suspends execution
		 *of the calling process for (at least) usec microseconds.
		 *The sleep may be lengthened slightly by any system
		 *activity or by the time spent processing the call
		 *or by the granularity of system timers.

		 *The reason we put usleep(...) here is that:
		 *On Mac OSX 10.7.x, the idt animate will abort with message like:
		 *
		 *X Error of failed request: BadMatch (invalid parameter attributes)
		 *  Major opcode of failed request: 73 (X_GetImage)
		 *  Serial number of failed request: 398
		 *  Current serial number in output stream: 398 
		 *
		 * and usleep(...) fixed this issue. We also want to put a little bigger
		 * number to be safer.
		 *
		 * Wei Huang, 04/24/2012, JIRA: 1344
		 */

		usleep(10000);

		/*
		 * snarf the image from the window
		 */
		ximage = XGetImage(
			wd->dpy, wd->win, 0,0, xwa.width, xwa.height, 
			AllPlanes, ZPixmap
		);

		if (! ximage) {
			ErrorMessage(wd->id, "Animation failed in XGetImage");
			XSynchronize(wd->dpy,False);
			return(-1);
		}

		if (AnimateLoad(a, ximage, (unsigned) i) < 0) {
			ErrorMessage(wd->id, "Animation failed in loading images");
			XSynchronize(wd->dpy,False);
			return(-1);
		}

		XDestroyImage(ximage);

		if (((i+1) % 10) == 0) {
			sprintf(buf, "Read %d images", i+1);
			Message(wd->id, buf);
		}
	}
	XSynchronize(wd->dpy,False);
	sprintf(buf, "Done loading %d images", num);
	Message(wd->id, buf);

	if (! a) {
		ErrorMessage(wd->id, "Animation failed in AnimateOpen()");
		return(-1);
	}
	wd->a = a;
		
	return(1);
}

/*
 *	terminate an animation session
 */
static	int	animate_off(wd)
	WidgetData	*wd;
{
	Arg			args[5];

	Message(wd->id, "Terminating animation mode\n");

	if (wd->a) AnimateClose(wd->a);

	/*
	 * restore buttons that only make sense when not animating
	 */
	XtSetArg(args[0], XtNsensitive, True);
	XtSetValues(wd->dup, args, 1);
	XtSetValues(wd->zoom, args, 1);
	XtSetValues(wd->unzoom, args, 1);
	XtSetValues(wd->skip, args, 1);
	XtSetValues(wd->start_segment, args, 1);
	XtSetValues(wd->stop_segment, args, 1);
	XtSetValues(wd->set_window, args, 1);
	XtSetValues(wd->current_frame, args, 1);
	XtSetValues(wd->save, args, 1);
	XtSetValues(wd->print, args, 1);

	XtSetArg(args[0], XtNsensitive, False);
	XtSetValues(wd->delay, args, 1);

	wd->a = (AnimateType *) NULL;
	return(1);
}

/*
 *	The Animate Callback - Toggle animation on and off. The
 *	first time Animate() is called animation is toggled on.
 */
/*ARGSUSED*/
void	Animate(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data, 
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	wd->do_animate = ! wd->do_animate;	/* toggle do_animate	*/

	if (wd->do_animate) {	/* are we animating	*/
		if (animate_on(wd) < 0) {
			wd->do_animate = False;
		}
	}
	else {
		(void) animate_off(wd);
	}
}
