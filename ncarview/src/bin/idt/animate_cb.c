
/*
 *      $Id: animate_cb.c,v 1.2 1992-08-25 20:24:01 clyne Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
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
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
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
	char		**images;	/* array of images to animate	*/
	AnimateType	*a;
	int		i;
	char		buf[80];
	XWindowAttributes	xwa;
	Arg			args[5];

	sprintf(buf, "Loading %d images for animation\n", num);
	Message(wd->id, buf);

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

	if (! (images = (char **) malloc (sizeof(char *) * num))) {
		ErrorMessage(wd->id, "Animation failed in malloc()");
		return(-1);
	}

	/*
	 * find out how big the window is where the animation will be
	 * displayed.
	 */
	if (! XGetWindowAttributes(wd->dpy, wd->win, &xwa)) {
		ErrorMessage(wd->id, "Animation failed in XGetWindowAttributes");
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

		images[i] = ximage->data;

		/*
		 * free all but the last ximage structures. We need one
		 * ximage structure for AnimateOpen().
		 */
		if (i != (num-1)) {
			ximage->data = NULL;	/* don't free the data	*/
			XDestroyImage(ximage);
		}

		if (((i+1) % 10) == 0) {
			sprintf(buf, "Read %d images", i+1);
			Message(wd->id, buf);
		}
	}
	XSynchronize(wd->dpy,False);
	sprintf(buf, "Done loading %d images", num);
	Message(wd->id, buf);

	/*
	 * pass the list of images to the animate module
	 */
	a = AnimateOpen(
		wd->dpy, wd->canvas, ximage, images, num, 
		0, 0, 0, 0, xwa.width, xwa.height
	);

	/*
	 * free all the image junk 
	 */
	for (i=0; i<num; i++) {
		free((char *) images[i]);
	}
	free((char *) images);
	ximage->data = NULL;
	XDestroyImage(ximage);

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
	int	i;
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
