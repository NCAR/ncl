/*
 *	$Id: w_display.c,v 1.25 2008-07-27 03:18:38 haley Exp $
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
 *	w_display.c
 *
 *	Author		John Clyne
 *
 *	Date		Mon Sep 17 14:02:33 MDT 1990
 *
 *	This file contains widget code necessary to dynamically create the 
 *	'display' panels for idt. A maximum of MAX_DISPLAYS may be 
 *	created simultaneously.
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Scrollbar.h>

#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SmeBSB.h>

#include <ncarg/c.h>
#include "idt.h"
#include "w_dialog.h"
#include "display.h"
#include "talkto.h"
#include "bits.h"
#include "commands.h"
#include "scroll.h"
#include "xrubber.h"

extern	void	Animate(
#ifdef	NeedFuncProto
	Widget		widget,
	XtPointer	client_data, 
	XtPointer	call_data
#endif
);

/*
 *	simple_command
 *	[internal]
 *
 * on entry
 *	id		: connection of translator 
 *	command		: command this box represents
 */
static	void	simple_command(wd, format, command)
	WidgetData	*wd;
	char		*format;
	DisplayCommands	command;

{
	Command(wd->id, format, NULL);
}

static	void	update_frame_by_num(data, num)
	Voidptr		*data;
	int		num;
{
	WidgetData	*wd = (WidgetData *) data;
	char		frame_string[10];

	sprintf(frame_string, "%d", num);
	UpdateFrameLabel(wd, frame_string);
}

static	void	update_frame_by_num_cb(data, num)
	Voidptr		*data;
	int		num;
{
	update_frame_by_num(data, num + 1);
}

/*
 *	The Callbacks
 */
/*ARGSUSED*/
static  void    Playback(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	wd->do_play = True;

	if (wd->do_animate) {
		Arg		args[2];
		XtSetArg(args[0], XtNsensitive, False);
		XtSetValues(wd->animate, args, 1);
		XtSetValues(wd->done, args, 1);
#ifdef	DEAD
		AnimateDispContReverse(wd->a, (int (*)()) NULL, (Voidptr) NULL);
#endif
		AnimateDispContReverse(
			wd->a, update_frame_by_num_cb, (Voidptr) wd
		);
		XtSetArg(args[0], XtNsensitive, True);
		XtSetValues(wd->done, args, 1);
		XtSetArg(args[1], XtNstate, True);
		XtSetValues(wd->animate, args, 2);
	}
	else {
		simple_command(wd, PLAYBACK_STRING, PLAYBACK);
		UpdateFrameLabel(wd, "");
	}
}

/*ARGSUSED*/
static  void    Jogback(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	if (wd->do_animate) {
		(void) AnimateDisplayPrev(wd->a);
		update_frame_by_num((Voidptr) wd, AnimateGetImageNum(wd->a)+1);
	}
	else {
		simple_command(wd, JOGBACK_STRING, JOGBACK);
		UpdateFrameLabel(wd, "");
	}

}

/*ARGSUSED*/
static  void    Stop(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	if (wd->do_animate) {
#ifdef	DEAD
		wd->do_play = False;
#endif
		AnimateStop(wd->a);
	}
	else {
		if (wd->do_play) {
			SignalTo(wd->id, STOP_SIGNAL);
			wd->do_play = False;
		}
		else {
			simple_command(wd, REDRAW_STRING, REDRAW);
		}
	}

}

/*ARGSUSED*/
static  void    Jog(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	if (wd->do_animate) {
		(void) AnimateDisplayNext(wd->a);
		update_frame_by_num((Voidptr) wd, AnimateGetImageNum(wd->a)+1);
	}
	else {
		simple_command(wd, JOG_STRING, JOG);
		UpdateFrameLabel(wd, "");
	}

}

/*ARGSUSED*/
static  void    Play(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	wd->do_play = True;

	if (wd->do_animate) {
		Arg		args[2];
		XtSetArg(args[0], XtNsensitive, False);
		XtSetValues(wd->animate, args, 1);
		XtSetValues(wd->done, args, 1);
#ifdef	DEAD
		AnimateDispContForward(wd->a, (int (*)()) NULL, (Voidptr) NULL);
#endif
		AnimateDispContForward(
			wd->a, update_frame_by_num_cb, (Voidptr) wd
		);
		XtSetArg(args[0], XtNsensitive, True);
		XtSetValues(wd->done, args, 1);
		XtSetArg(args[1], XtNstate, True);
		XtSetValues(wd->animate, args, 2);
	}
	else {
		simple_command(wd, PLAY_STRING, PLAY);
		UpdateFrameLabel(wd, "");
	}
}

/*ARGSUSED*/
static  void    Loop(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	if (wd->do_animate) {
		AnimateLoop(wd->a);
	}
	else {
		simple_command(wd, LOOP_STRING, LOOP);
	}
}

void	DupSelect(data, value)
	Voidptr	data;
	char	*value;
{
	WidgetData	*wd = (WidgetData *) data;

	Command(wd->id, DUP_STRING, value);
	wd->pcv.dup = atoi(value);
}
/*ARGSUSED*/
static  void    Dup(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	char		buf[MAX_DATA_LEN];

	sprintf(buf, "%d", wd->pcv.dup);
	CreateSimpleDialogPopup(widget, "dup:", DupSelect, (Voidptr) wd, buf);

}
/*ARGSUSED*/
static  void    Scroll(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id		*/
			call_data;	/* percent scrolled	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	float		percent = *(float *) call_data;

	ScrollTo(wd, percent);

}

void	GotoSelect(data, value)
	Voidptr	data;
	char	*value;
{
	WidgetData	*wd = (WidgetData *) data;

	if (wd->do_animate) {
		int	frame = atoi(value);
		AnimateDisplayImage(wd->a, frame-1);
		update_frame_by_num((Voidptr) wd, AnimateGetImageNum(wd->a)+1);
	}
	else {
		Command(wd->id, GOTO_STRING, value);
	}
	wd->pcv.goto_ = atoi(value);
}
/*ARGSUSED*/
static  void    Goto_(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	char		buf[MAX_DATA_LEN];

	sprintf(buf, "%d", wd->pcv.goto_);
	CreateSimpleDialogPopup(widget, "goto:", GotoSelect, (Voidptr) wd, buf);

}

void	SkipSelect(data, value)
	Voidptr	data;
	char	*value;
{
	WidgetData	*wd = (WidgetData *) data;

	Command(wd->id, SKIP_STRING, value);
	wd->pcv.skip = atoi(value);
}
/*ARGSUSED*/
static  void    Skip(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	char		buf[MAX_DATA_LEN];

	sprintf(buf, "%d", wd->pcv.skip);
	CreateSimpleDialogPopup(widget, "skip:", SkipSelect, (Voidptr) wd, buf);

}

void	DelaySelect(data, value)
	Voidptr	data;
	char	*value;
{
	WidgetData	*wd = (WidgetData *) data;

	wd->pcv.delay = atoi(value);
	AnimateSetDelay(wd->a, wd->pcv.delay);
}

/*ARGSUSED*/
static  void    Delay(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	char		buf[MAX_DATA_LEN];

	sprintf(buf, "%d", wd->pcv.delay);
	CreateSimpleDialogPopup(
		widget, "delay in 1/100 sec:", DelaySelect, (Voidptr) wd, buf
	);
}

void	StartSegmentSelect(data, value)
	Voidptr	data;
	char	*value;
{
	WidgetData	*wd = (WidgetData *) data;

	Command(wd->id, START_SEGMENT_STRING, value);
	wd->pcv.start_segment = atoi(value);
}
/*ARGSUSED*/
static  void    Start_Segment(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	char		buf[MAX_DATA_LEN];

	sprintf(buf, "%d", wd->pcv.start_segment);
	CreateSimpleDialogPopup(
		widget, "start segment:", StartSegmentSelect, (Voidptr) wd, buf
	);

}

void	StopSegmentSelect(data, value)
	Voidptr	data;
	char	*value;
{
	WidgetData	*wd = (WidgetData *) data;

	Command(wd->id, STOP_SEGMENT_STRING, value);
	wd->pcv.stop_segment = atoi(value);
}
/*ARGSUSED*/
static  void    Stop_Segment(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	char		buf[MAX_DATA_LEN];

	sprintf(buf, "%d", wd->pcv.stop_segment);
	CreateSimpleDialogPopup(
		widget, "stop segment:", StopSegmentSelect, (Voidptr) wd, buf
	);

}

void	SetWindowSelect(data, value)
	Voidptr	data;
	char	*value;
{
	WidgetData	*wd = (WidgetData *) data;

	Command(wd->id, SET_WINDOW_STRING, value);
	strncpy(wd->pcv.set_window, value, sizeof(wd->pcv.set_window));

	/*
	 * SetWindow should really update wd->ar, wd->llx, etc in the
	 * same manner as  Zoom() does. Some day...
	 */
}
/*ARGSUSED*/
static  void    Set_Window(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	CreateSimpleDialogPopup(
		widget, "set device window coordinates:", 
		SetWindowSelect, (Voidptr) wd, wd->pcv.set_window
	);

}

/*
 *	
 */
/*ARGSUSED*/
static  void    Done(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{

	WidgetData	*wd = (WidgetData *) client_data;

	if (wd->do_animate) {
		/*
		 * turn animation off by calling animation callback.
		 * The animation button is a toggle
		 */
		Animate(widget, (XtPointer) wd, (XtPointer) NULL);
	}

	simple_command(wd, DONE_STRING, DONE);

	CloseDisplay(wd->id);
	XtDestroyWidget(wd->popup);
}

/*
 *	
 */
/*ARGSUSED*/
static  void    CurrentFrame(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	char	*current;


	current = TalkTo(wd->id, "current\n", SYNC);
	
	wd->pcv.current_frame = atoi(current);
	UpdateFrameLabel(wd, current);

}

/*ARGSUSED*/
static  void    PrintSelect(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
	char	*spooler = XtName(widget);

	Command(wd->id, PRINT_STRING, spooler);
}


/*
 *	
 */
void	SaveSelect(data, value)
	Voidptr	data;
	char	*value;
{
	WidgetData	*wd = (WidgetData *) data;

	Command(wd->id, SAVE_STRING, value);
	strncpy(wd->pcv.save, value, sizeof(wd->pcv.save));
}
/*ARGSUSED*/
static  void    Save(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	CreateSimpleDialogPopup(
		widget, "Please enter file name:", 
		SaveSelect, (Voidptr) wd, wd->pcv.save
	);

}

/*
 *	
 */
/*ARGSUSED*/
static  void    Zoom(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;
        Window  root;
	float	llx, lly, urx, ury;	/* coords of rubber band	*/
	float	new_llx, new_lly,	/* zoom coords			*/
		new_urx, new_ury;
	float	ax, bx, ay, by;
	char	buf[80];

	root = RootWindowOfScreen(XtScreen(widget));

	/*
	 * get the new mapping
	 */
	(void) TalkTo(wd->id, NOOP_STRING, SYNC);
        if (ZoomCoords(
		XtDisplay(widget), root, wd->ar, &llx,&lly,&urx,&ury) == NULL){

                (void) fprintf(stderr, "Zoom failed\n");
		return;
	}


	/*
	 * map the new mapping into the old
	 */
	bx = wd->llx;
	ax = wd->urx - bx;
	by = wd->lly;
	ay = wd->ury - by;

	new_llx = (ax * llx) + bx;
	new_lly = (ay * lly) + by;
	new_urx = (ax * urx) + bx;
	new_ury = (ay * ury) + by;

	/*
	 * remember the new coords and aspect ratio
	 */
	wd->ar = (new_urx - new_llx) / (new_ury - new_lly);
	wd->llx = new_llx;
	wd->lly = new_lly;
	wd->urx = new_urx;
	wd->ury = new_ury;


	sprintf(buf, "%6.4f %6.4f %6.4f %6.4f",new_llx,new_lly,new_urx,new_ury);
	
	Command(wd->id, ZOOM_STRING, buf);

	/*
	 * remember the zoom coordinates as if the SET_WINDOW command
	 * was called
	 */
	strcpy(wd->pcv.set_window, buf);
}

/*ARGSUSED*/
static  void    UnZoom(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	WidgetData	*wd = (WidgetData *) client_data;

	wd->ar = 1.0;
	wd->llx = wd->lly = 0.0;
	wd->urx = wd->ury = 1.0;
	simple_command(wd, UNZOOM_STRING, UNZOOM);
}


UpdateFrameLabel(wd, frame_string)
	WidgetData	*wd;
	char		*frame_string;
{
	Arg		args[10];
	char		buf[80];

	sprintf(buf, "%s %s", FRAME_LABEL_DISPLAY, frame_string);
	
	XtSetArg(args[0], XtNlabel, buf);
	XtSetValues(wd->frame_label, args, 1);
}

void	create_tip_top_panel(paned, wd)
	Widget		paned;
	WidgetData	*wd;
{
	Widget	form;

	Cardinal	n;
	Arg		args[10];

        n = 0;
	XtSetArg(args[n], XtNskipAdjust, True);	n++;
	form = XtCreateManagedWidget("form", formWidgetClass,paned, args,n);

        n = 0;
        wd->scrollbar = XtCreateManagedWidget("scrollbar",
			scrollbarWidgetClass,form, (ArgList) args,n);

	XtAddCallback(wd->scrollbar, XtNjumpProc, Scroll, (XtPointer) wd);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->scrollbar);	n++;
	XtSetArg(args[n], XtNborderColor, XtDefaultBackground);	n++;
        wd->frame_label = XtCreateManagedWidget(FRAME_LABEL_DISPLAY,
			labelWidgetClass,form,args,n);
}

/*
 * 	The top panel consists of the scrollbar; "playback", "jogback", "stop",
 *	"jog", and "play" buttons.
 */
void	create_top_panel(paned, wd)
	Widget		paned;
	WidgetData	*wd;
{
	Widget	form;

	Pixmap	pixmap;

	Cardinal	n;
	Arg		args[10];

        n = 0;
	XtSetArg(args[n], XtNskipAdjust, True);	n++;
	form = XtCreateManagedWidget("form", formWidgetClass,paned, args,n);

	/*
	 * create a pixmap for the playback button
	 */
	pixmap = XCreateBitmapFromData(
		XtDisplay(paned), RootWindowOfScreen(XtScreen(paned)),
		 (const char *) playback_bits, playback_width,playback_height
	);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
        wd->playback = XtCreateManagedWidget("playback",
			commandWidgetClass,form,args,n);

	XtAddCallback(wd->playback, XtNcallback, Playback, (XtPointer) wd);

	/*
	 * the jogback button
	 */
	pixmap = XCreateBitmapFromData(
		XtDisplay(paned), RootWindowOfScreen(XtScreen(paned)),
		(const char *) jogback_bits,jogback_width,jogback_height
	);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
	XtSetArg(args[n], XtNfromHoriz, wd->playback);	n++;
        wd->jogback = XtCreateManagedWidget("jogback",
			commandWidgetClass,form,args,n);

	XtAddCallback(wd->jogback, XtNcallback, Jogback, (XtPointer) wd);

	/*
	 * the stop button
	 */
	pixmap = XCreateBitmapFromData(
		XtDisplay(paned), RootWindowOfScreen(XtScreen(paned)),
		 (const char *) stop_bits,stop_width,stop_height
	);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
	XtSetArg(args[n], XtNfromHoriz, wd->jogback);	n++;
        wd->stop = XtCreateManagedWidget("stop",commandWidgetClass,form,args,n);
	XtAddCallback(wd->stop, XtNcallback, Stop, (XtPointer) wd);

	/*
	 * the jog button
	 */
	pixmap = XCreateBitmapFromData(
		XtDisplay(paned), RootWindowOfScreen(XtScreen(paned)),
		(const char *) jog_bits,jog_width,jog_height
	);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
	XtSetArg(args[n], XtNfromHoriz, wd->stop);	n++;
        wd->jog = XtCreateManagedWidget("jog",commandWidgetClass,form,args,n);
	XtAddCallback(wd->jog, XtNcallback, Jog, (XtPointer) wd);

	/*
	 * the play button
	 */
	pixmap = XCreateBitmapFromData(
		XtDisplay(paned), RootWindowOfScreen(XtScreen(paned)),
		(const char *) play_bits,play_width,play_height
	);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
	XtSetArg(args[n], XtNfromHoriz, wd->jog);	n++;
        wd->play = XtCreateManagedWidget(
		"play", commandWidgetClass,form,args,n
	);
	XtAddCallback(wd->play, XtNcallback, Play, (XtPointer) wd);

}

/*
 * The middle panel consists of the "loop", "goto", "dup", "skip",
 * "start segment" and the "stop segment" buttons.
 */
static	void	create_middle_panel(paned, wd)
	Widget		paned;
	WidgetData	*wd;
{
	Widget	form;

	Cardinal	n;
	Arg		args[10];

        n = 0;
	XtSetArg(args[n], XtNskipAdjust, True);	n++;
	form = XtCreateManagedWidget("form", formWidgetClass,paned, args,n);

        n = 0;
        wd->loop = XtCreateManagedWidget("loop", toggleWidgetClass,form,args,n);
	XtAddCallback(wd->loop, XtNcallback, Loop, (XtPointer) wd);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->loop);	n++;
        wd->dup = XtCreateManagedWidget("dup",commandWidgetClass,form,args,n);
	XtAddCallback(wd->dup, XtNcallback, Dup, (XtPointer) wd);
	wd->pcv.dup = DEFAULT_DUP;

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->dup);	n++;
        wd->goto_ = XtCreateManagedWidget(
		"goto",commandWidgetClass,form,args,n
	);
	XtAddCallback(wd->goto_, XtNcallback, Goto_, (XtPointer) wd);
	wd->pcv.goto_ = DEFAULT_GOTO;

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->goto_);	n++;
        wd->skip = XtCreateManagedWidget("skip",commandWidgetClass,form,args,n);
	XtAddCallback(wd->skip, XtNcallback, Skip, (XtPointer) wd);
	wd->pcv.skip = DEFAULT_SKIP;

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->skip);	n++;
	XtSetArg(args[n], XtNsensitive, False);		n++;
        wd->delay = XtCreateManagedWidget(
		"delay",commandWidgetClass,form,args,n
	);
	XtAddCallback(wd->delay, XtNcallback, Delay, (XtPointer) wd);
	wd->pcv.delay = DEFAULT_DELAY;

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->delay);	n++;
        wd->start_segment = XtCreateManagedWidget("start segment",
		commandWidgetClass,form,args,n);
	XtAddCallback(
		wd->start_segment, XtNcallback, Start_Segment,(XtPointer) wd
	);
	wd->pcv.start_segment = DEFAULT_START;

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->start_segment);	n++;
        wd->stop_segment = XtCreateManagedWidget("stop segment",
		commandWidgetClass,form,args,n);
	XtAddCallback(wd->stop_segment,XtNcallback,Stop_Segment,(XtPointer) wd);
	wd->pcv.stop_segment = DEFAULT_STOP;

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->stop_segment);	n++;
        wd->set_window = XtCreateManagedWidget("set window",
		commandWidgetClass,form,args,n);
	XtAddCallback(wd->set_window,XtNcallback,Set_Window,(XtPointer) wd);
	strcpy(wd->pcv.set_window, DEFAULT_SET_WINDOW);
}

static	Widget	create_bottom_panel(paned, wd) 
	Widget		paned;
	WidgetData	*wd;
{
	Arg		args[10];
	Widget	form;
	Cardinal	n;

        n = 0;
	XtSetArg(args[n], XtNskipAdjust, True);	n++;
	form = XtCreateManagedWidget("form", formWidgetClass,paned, args,n);

        n = 0;
        wd->done = XtCreateManagedWidget("done",commandWidgetClass,form,args,n);
	XtAddCallback(wd->done, XtNcallback, Done, (XtPointer) wd);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->done);	n++;
        wd->current_frame = XtCreateManagedWidget("current frame",
				commandWidgetClass,form,args,n);
	XtAddCallback(wd->current_frame,XtNcallback,CurrentFrame,(XtPointer)wd);
	wd->pcv.current_frame = DEFAULT_CURRENT;

	n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->current_frame);	n++;
	wd->print = XtCreateManagedWidget("print",menuButtonWidgetClass,
								form,args,n);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->print);	n++;
        wd->save = XtCreateManagedWidget("save",commandWidgetClass,form,args,n);
	XtAddCallback(wd->save, XtNcallback, Save, (XtPointer) wd);
	strcpy(wd->pcv.save, DEFAULT_SAVE);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->save);	n++;
        wd->zoom = XtCreateManagedWidget("zoom",commandWidgetClass,form,args,n);
	XtAddCallback(wd->zoom, XtNcallback, Zoom, (XtPointer) wd);
	wd->ar = 1.0;
	wd->llx = wd->lly = 0.0;
	wd->urx = wd->ury = 1.0;

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->zoom);	n++;
        wd->unzoom = XtCreateManagedWidget(
		"unzoom",commandWidgetClass,form,args,n
	);
	XtAddCallback(wd->unzoom, XtNcallback, UnZoom, (XtPointer) wd);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, wd->unzoom);	n++;
        wd->animate = XtCreateManagedWidget(
		"animate",toggleWidgetClass,form,args,n
	);
	XtAddCallback(wd->animate, XtNcallback, Animate, (XtPointer) wd);
	wd->do_animate = False;

	return(wd->print);
}

/*
 *	dyanmically create a print menu by polling the translator to see
 *	what printing devices are available
 */
void	create_print_menu(print, wd)
	Widget		print;
	WidgetData	*wd;
{

	Widget		menu, entry;
	Arg		args[10];
	char		*alias_list,
			**spooler_list,
			**ptr;

	extern	char	**SpoolerList();

	alias_list = TalkTo(wd->id, "alias\n", SYNC);
	spooler_list = SpoolerList(alias_list);
	if (*spooler_list) {

		menu = XtCreatePopupShell("menu", 
				simpleMenuWidgetClass, print, (ArgList) NULL,0);

		for (ptr = spooler_list; *ptr; ptr++) {
			entry = XtCreateManagedWidget(*ptr, smeBSBObjectClass,
					menu, (ArgList) NULL, 0);

			XtAddCallback(entry, XtNcallback, 
					PrintSelect, (XtPointer) wd);
		}
	}
	else {
		Message(wd->id, "Can't find  any spooled devices for printing");
		XtSetArg(args[0], XtNsensitive, False);
		XtSetValues(print, args, 1);
	}
}

#ifdef	DEAD

static	Visual	*get_best_8bit_visual(depth, dpy)
	int	*depth;
	Display	*dpy;
{
	XVisualInfo	vinfo;
	int	screen = DefaultScreen(dpy);


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

#endif /* DEAD */


/*
 *	CreateDisplayPopup
 *	[exported]
 *
 *	Dynamically create a popup 'display'. This popup is located with its
 *	top left corner in the center of the calling widget.
 *
 * on entry
 *	button		: widget used to position the display
 *	*metafile	: name of the metafile to translate
 */
void	CreateDisplayPopup(button, metafile)
	Widget	button;
	char	*metafile;
{

	Widget	paned;		/* constraint widget		*/
	Widget	canvas;		/* the drawing canvas		*/
	Widget	popup;		/* top-level popup		*/
	Widget	print;		/* "print" button widget	*/
	Window	win;		/* drawing canvas window id	*/
	WidgetData	*wd;
	char		*s;

#ifdef	DEAD
	Arg		args[10];
	Cardinal	n;
	Visual		*visual;
	int		dsp_depth;
	Colormap	cmap;
#endif	/* DEAD */

	if (!(wd = (WidgetData *) malloc(sizeof(WidgetData)))) {
		(void) fprintf(stderr, "Malloc failed\n");
		return;
	}

	wd->dpy = XtDisplay(button);

	/* init some fields */
	wd->current_frame_num = -1; /* none initially */

#ifdef	DEAD
	visual = get_best_8bit_visual(&dsp_depth, wd->dpy);
	if (visual == DefaultVisual(wd->dpy, DefaultScreen(wd->dpy))) {
		cmap = DefaultColormap(wd->dpy, DefaultScreen(wd->dpy));
		dsp_depth = DefaultDepth(wd->dpy, DefaultScreen(wd->dpy));
	}
	else {
		cmap = XCreateColormap(
			wd->dpy, RootWindow(wd->dpy, DefaultScreen(wd->dpy)),
			visual, AllocAll
		);
	}


        n = 0;
	XtSetArg(args[n], XtNcolormap, cmap);		n++;
        XtSetArg(args[n], XtNvisual, visual);		n++;
        XtSetArg(args[n], XtNdepth, dsp_depth);		n++;
	popup = XtCreatePopupShell(metafile, topLevelShellWidgetClass, 
				button, args, n);
#endif
	popup = XtCreatePopupShell(metafile, topLevelShellWidgetClass, 
				button, (ArgList) NULL, 0);

	paned = XtCreateManagedWidget("paned",
				panedWidgetClass,popup, (ArgList) NULL,0);

	if (! App_Data.oldidt) {
		canvas = XtCreateManagedWidget("canvas",
				widgetClass,paned, (ArgList) NULL,0);
		wd->canvas = canvas;
	}

	/*
	 * open a connection to a translator
	 */
	if ((wd->id = OpenDisplay())< 0) {
		(void) fprintf(stderr, "Translator aborted\n");
		return;
	}
	wd->popup = popup;
	wd->app_context = XtWidgetToApplicationContext(popup);

	/*
	 * The main display is made up of four sub-panels
	 */
	create_tip_top_panel(paned, wd);

	create_top_panel(paned, wd);

	create_middle_panel(paned, wd);

	print = create_bottom_panel(paned, wd);


	XtPopup(wd->popup, XtGrabNone);

	if (! App_Data.oldidt) {
                unsigned long mask = CWBackingStore;
                XSetWindowAttributes xswa;

		/*
		 * enable button click events in the drawing canvas so the 
		 * rubber-banding code in ZoomCoords() can get events from
		 * the drawing window, not the top-level window
		 */
		win = (Window) XtWindow(canvas);
		XSelectInput(XtDisplay(canvas), win, ButtonPressMask);

		/*
		 * turn on backing store for drawing canvas. Wish we
		 * could do this from an Xt resource.
		 */
                xswa.backing_store = WhenMapped;
                XChangeWindowAttributes(wd->dpy, win, mask, &xswa);

	}
	else {
		win = (Window) -1;
	}
	wd->win = win;

	if (App_Data.debug) {
		fprintf(stderr, "Canvas window id(%d)\n", win);
	}


	/*
	 * now that our drawing window has been mapped we can spawn
	 * the translator and request it to drawn in the idt canvas
	 */
	if (StartTranslator(wd->id, metafile, win)< 0) {
		(void) fprintf(stderr, "Translator aborted\n");
		return;
	}

	/*
	 * now that the translator is up and running we can poll it to
	 * find out what printing devices are available so we can create
	 * our print menu dynamically
	 */
	create_print_menu(print, wd);

	/*
	 * find out how many frames are in the metafile now that the
	 * translator is up
	 */
	s = TalkTo(wd->id, "stop\n", SYNC);
	wd->pcv.stop_segment = atoi(s);
}
