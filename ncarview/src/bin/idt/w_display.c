/*
 *	$Id: w_display.c,v 1.11 1992-08-12 21:42:05 clyne Exp $
 */
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

#include <ncarv.h>
#include "idt.h"
#include "display.h"
#include "talkto.h"
#include "bits.h"
#include "commands.h"


/*
 * callbacks
 */
static	void	Scroll();
static	void	Done(), CurrentFrame(), NoPrint(), PrintSelect(), 
		Save(), Zoom(), UnZoom();
static	void	Loop(), Dup(), Goto_(), Skip(), 
		Start_Segment(), Stop_Segment(), Set_Window();
static	void	Playback(), Jogback(), Stop(), Jog(), Play();

/*
 *	top level widget of a display. Up to MAX_DISPLAYS may be active
 *	simultaneously.
 */
static	Widget	popUp[MAX_DISPLAYS];

static	Command_Id	command_Id;

static	Widget	frameLabel[MAX_DISPLAYS];

static	short	playMode = False;	/* True after Playback or Play is
					 * selected. False after Stop is
					 * selected
					 */

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

	Arg		args[10];
	Widget	paned;		/* constraint widget		*/
	Widget	canvas;		/* the drawing canvas		*/
	Widget	popup;		/* top-level popup		*/
	Widget	print;		/* "print" button widget	*/
	Cardinal	n;
	Window	win;		/* drawing canvas window id	*/

	int	id;	/* translator connection id	*/

	void	create_tip_top_panel(), create_top_panel(), 
		create_middle_panel(), create_print_menu();
	Widget	create_bottom_panel();

	popup = XtCreatePopupShell(metafile, topLevelShellWidgetClass, 
				button, (ArgList) NULL, 0);

	paned = XtCreateManagedWidget("paned",
				panedWidgetClass,popup, (ArgList) NULL,0);

	if (! App_Data.oldidt) {
		canvas = XtCreateManagedWidget("canvas",
				widgetClass,paned, (ArgList) NULL,0);
	}

	/*
	 * open a connection to a translator
	 */
	if ((id = OpenDisplay())< 0) {
		(void) fprintf(stderr, "Translator aborted\n");
		return;
	}
	popUp[id] = popup;

	/*
	 * The main display is made up of four sub-panels
	 */
	create_tip_top_panel(paned, id);

	create_top_panel(paned, id);

	create_middle_panel(paned, id);

	print = create_bottom_panel(paned, id);


	XtPopup(popUp[id], XtGrabNone);

	if (! App_Data.oldidt) {
		/*
		 * enable button click events in the drawing canvas so the 
		 * rubber-banding code in ZoomCoords() can get events from
		 * the drawing window, not the top-level window
		 */
		win = (Window) XtWindow(canvas);
		XSelectInput(XtDisplay(canvas), win, ButtonPressMask);
	}
	else {
		win = (Window) -1;
	}


	/*
	 * now that our drawing window has been mapped we can spawn
	 * the translator and request it to drawn in the idt canvas
	 */
	if (StartTranslator(id, metafile, win)< 0) {
		(void) fprintf(stderr, "Translator aborted\n");
		return;
	}

	/*
	 * now that the translator is up and running we can poll it to
	 * find out what printing devices are available so we can create
	 * our print menu dynamically
	 */
	create_print_menu(print, id);
}


void	create_tip_top_panel(paned, id)
	Widget	paned;
	int	id;
{
	Widget	form;
	Widget	scrollbar;

	Cardinal	n;
	Arg		args[10];

        n = 0;
	XtSetArg(args[n], XtNskipAdjust, True);	n++;
	form = XtCreateManagedWidget("form", formWidgetClass,paned, args,n);

        n = 0;
        scrollbar = XtCreateManagedWidget("scrollbar",
			scrollbarWidgetClass,form, (ArgList) args,n);

	XtAddCallback(scrollbar, XtNjumpProc, Scroll, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, scrollbar);	n++;
	XtSetArg(args[n], XtNborderColor, XtDefaultBackground);	n++;
        frameLabel[id] = XtCreateManagedWidget(FRAME_LABEL_DISPLAY,
			labelWidgetClass,form,args,n);
}

/*
 * 	The top panel consists of the scrollbar; "playback", "jogback", "stop",
 *	"jog", and "play" buttons.
 */
void	create_top_panel(paned, id)
	Widget	paned;
	int	id;
{
	Widget	form;
	Widget	playback, jogback, stop, play, jog;

	Pixmap	pixmap;

	Cardinal	n;
	Arg		args[10];

        n = 0;
	XtSetArg(args[n], XtNskipAdjust, True);	n++;
	form = XtCreateManagedWidget("form", formWidgetClass,paned, args,n);

	/*
	 * create a pixmap for the playback button
	 */
	pixmap = XCreateBitmapFromData(XtDisplay(paned),
                                 RootWindowOfScreen(XtScreen(paned)),
                                 playback_bits,playback_width,playback_height);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
        playback = XtCreateManagedWidget("playback",
			commandWidgetClass,form,args,n);

	XtAddCallback(playback, XtNcallback, Playback, (XtPointer) id);

	/*
	 * the jogback button
	 */
	pixmap = XCreateBitmapFromData(XtDisplay(paned),
                                 RootWindowOfScreen(XtScreen(paned)),
                                 jogback_bits,jogback_width,jogback_height);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
	XtSetArg(args[n], XtNfromHoriz, playback);	n++;
        jogback = XtCreateManagedWidget("jogback",
			commandWidgetClass,form,args,n);

	XtAddCallback(jogback, XtNcallback, Jogback, (XtPointer) id);

	/*
	 * the stop button
	 */
	pixmap = XCreateBitmapFromData(XtDisplay(paned),
                                 RootWindowOfScreen(XtScreen(paned)),
                                 stop_bits,stop_width,stop_height);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
	XtSetArg(args[n], XtNfromHoriz, jogback);	n++;
        stop = XtCreateManagedWidget("stop",commandWidgetClass,form,args,n);
	XtAddCallback(stop, XtNcallback, Stop, (XtPointer) id);

	/*
	 * the jog button
	 */
	pixmap = XCreateBitmapFromData(XtDisplay(paned),
                                 RootWindowOfScreen(XtScreen(paned)),
                                 jog_bits,jog_width,jog_height);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
	XtSetArg(args[n], XtNfromHoriz, stop);	n++;
        jog = XtCreateManagedWidget("jog",commandWidgetClass,form,args,n);
	XtAddCallback(jog, XtNcallback, Jog, (XtPointer) id);

	/*
	 * the play button
	 */
	pixmap = XCreateBitmapFromData(XtDisplay(paned),
                                 RootWindowOfScreen(XtScreen(paned)),
                                 play_bits,play_width,play_height);

        n = 0;
	XtSetArg(args[n], XtNbitmap, pixmap);	n++;
	XtSetArg(args[n], XtNfromHoriz, jog);	n++;
        play = XtCreateManagedWidget("play", commandWidgetClass,form,args,n);
	XtAddCallback(play, XtNcallback, Play, (XtPointer) id);

}

/*
 * The middle panel consists of the "loop", "goto", "dup", "skip",
 * "start segment" and the "stop segment" buttons.
 */
static	void	create_middle_panel(paned, id)
	Widget	paned;
	int	id;
{
	Widget	form;
	Widget	loop, goto_, dup, skip, start_segment, stop_segment, set_window;

	Cardinal	n;
	Arg		args[10];

        n = 0;
	XtSetArg(args[n], XtNskipAdjust, True);	n++;
	form = XtCreateManagedWidget("form", formWidgetClass,paned, args,n);

        n = 0;
        loop = XtCreateManagedWidget("loop", toggleWidgetClass,form,args,n);
	XtAddCallback(loop, XtNcallback, Loop, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, loop);	n++;
        dup = XtCreateManagedWidget("dup",commandWidgetClass,form,args,n);
	XtAddCallback(dup, XtNcallback, Dup, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, dup);	n++;
        goto_ = XtCreateManagedWidget("goto",commandWidgetClass,form,args,n);
	XtAddCallback(goto_, XtNcallback, Goto_, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, goto_);	n++;
        skip = XtCreateManagedWidget("skip",commandWidgetClass,form,args,n);
	XtAddCallback(skip, XtNcallback, Skip, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, skip);	n++;
        start_segment = XtCreateManagedWidget("start segment",
		commandWidgetClass,form,args,n);
	XtAddCallback(start_segment,XtNcallback,Start_Segment,(XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, start_segment);	n++;
        stop_segment = XtCreateManagedWidget("stop segment",
		commandWidgetClass,form,args,n);
	XtAddCallback(stop_segment,XtNcallback,Stop_Segment,(XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, stop_segment);	n++;
        set_window = XtCreateManagedWidget("set window",
		commandWidgetClass,form,args,n);
	XtAddCallback(set_window,XtNcallback,Set_Window,(XtPointer) id);
}

static	Widget	create_bottom_panel(paned, id) 
	Widget	paned;
	int	id;
{
	Arg		args[10];
	Widget	form;
	Widget	save, current_frame, zoom, unzoom, done;
	Widget	print;
	Cardinal	n;

        n = 0;
	XtSetArg(args[n], XtNskipAdjust, True);	n++;
	form = XtCreateManagedWidget("form", formWidgetClass,paned, args,n);

        n = 0;
        done = XtCreateManagedWidget("done",commandWidgetClass,form,args,n);
	XtAddCallback(done, XtNcallback, Done, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, done);	n++;
        current_frame = XtCreateManagedWidget("current frame",
				commandWidgetClass,form,args,n);
	XtAddCallback(current_frame,XtNcallback,CurrentFrame,(XtPointer) id);

	n = 0;
	XtSetArg(args[n], XtNfromHoriz, current_frame);	n++;
	print = XtCreateManagedWidget("print",menuButtonWidgetClass,
								form,args,n);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, print);	n++;
        save = XtCreateManagedWidget("save",commandWidgetClass,form,args,n);
	XtAddCallback(save, XtNcallback, Save, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, save);	n++;
        zoom = XtCreateManagedWidget("zoom",commandWidgetClass,form,args,n);
	XtAddCallback(zoom, XtNcallback, Zoom, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, zoom);	n++;
        unzoom = XtCreateManagedWidget("unzoom",commandWidgetClass,form,args,n);
	XtAddCallback(unzoom, XtNcallback, UnZoom, (XtPointer) id);

	return(print);
}

/*
 *	dyanmically create a print menu by polling the translator to see
 *	what printing devices are available
 */
void	create_print_menu(print, id)
	Widget	print;
	int	id;
{

	Widget		menu, entry;
	Arg		args[10];
	char		*alias_list,
			**spooler_list,
			**ptr;

	extern	char	*TalkTo();
	extern	char	**SpoolerList();

	alias_list = TalkTo(id, "alias\n", SYNC);
	spooler_list = SpoolerList(alias_list);
	if (*spooler_list) {

		menu = XtCreatePopupShell("menu", 
				simpleMenuWidgetClass, print, (ArgList) NULL,0);

		for (ptr = spooler_list; *ptr; ptr++) {
			entry = XtCreateManagedWidget(*ptr, smeBSBObjectClass,
					menu, (ArgList) NULL, 0);

			XtAddCallback(entry, XtNcallback, 
					PrintSelect, (XtPointer) id);
		}
	}
	else {
		Message(id, "Can't find  any spooled devices for printing");
		XtSetArg(args[0], XtNsensitive, False);
		XtSetValues(print, args, 1);
	}
}

/*
 *	simple_command
 *	[internal]
 *
 * on entry
 *	id		: connection of translator 
 *	command		: command this box represents
 */
static	void	simple_command(id, format, command)
	int		id;
	char		*format;
	DisplayCommands	command;

{

	command_Id.id = id;
	command_Id.command = command;

	Command((caddr_t) &command_Id, format, NULL);
}

/*
 *	create_simple_dialog_popup
 *	[internal]
 *
 *	create a dialog popup on the fly
 * on entry
 * 	widget		: the calling widget, used to position the popup
 *	id		: connection of translator 
 *	command_name	: string used to label the box
 *	command		: command this box represents
 *	cmd_format	: command format string
 */
static	void	create_simple_dialog_popup(widget, id, command_name, command,
		cmd_format
		)
	Widget	widget;
	int	id;
	char	*command_name;
	DisplayCommands	command;
	char	*cmd_format;

{
	char	*value;

	void	CreateSimpleDialogPopup();
	char	*GetValue();

	value = GetValue(id, command);

	command_Id.id = id;
	command_Id.command = command;

	CreateSimpleDialogPopup(
		widget, command_name, Command, 
		(caddr_t) &command_Id,cmd_format, value
	);
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
	int	id = (int) client_data;

	playMode = True;
	simple_command(id, PLAYBACK_STRING, PLAYBACK);
	UpdateFrameLabel(id, "");

}

/*ARGSUSED*/
static  void    Jogback(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	simple_command(id, JOGBACK_STRING, JOGBACK);
	UpdateFrameLabel(id, "");

}

/*ARGSUSED*/
static  void    Stop(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	if (playMode) {
		SignalTo(id, STOP_SIGNAL);
		playMode = False;
	}
	else {
		simple_command(id, REDRAW_STRING, REDRAW);
	}

}

/*ARGSUSED*/
static  void    Jog(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	simple_command(id, JOG_STRING, JOG);
	UpdateFrameLabel(id, "");

}

/*ARGSUSED*/
static  void    Play(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	playMode = True;
	simple_command(id, PLAY_STRING, PLAY);
	UpdateFrameLabel(id, "");

}

/*ARGSUSED*/
static  void    Loop(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	simple_command(id, LOOP_STRING, LOOP);

}
/*ARGSUSED*/
static  void    Dup(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	create_simple_dialog_popup(widget, id, "dup:", DUP, DUP_STRING);

}
/*ARGSUSED*/
static  void    Scroll(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id		*/
			call_data;	/* percent scrolled	*/
{
	int		id = (int) client_data;
	float		percent = *(float *) call_data;

	extern	void	ScrollTo();

	ScrollTo(id, percent);

}

/*ARGSUSED*/
static  void    Goto_(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int		id = (int) client_data;

	create_simple_dialog_popup(widget, id, "goto:", GOTO, GOTO_STRING);

}
/*ARGSUSED*/
static  void    Skip(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	create_simple_dialog_popup(widget, id, "skip:", SKIP, SKIP_STRING);

}
/*ARGSUSED*/
static  void    Start_Segment(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	create_simple_dialog_popup(
		widget, id, "start segment:", START_SEGMENT,
		START_SEGMENT_STRING
	);

}
/*ARGSUSED*/
static  void    Stop_Segment(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	create_simple_dialog_popup(
		widget, id, "stop segment:", STOP_SEGMENT,
		STOP_SEGMENT_STRING
	);

}
/*ARGSUSED*/
static  void    Set_Window(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	create_simple_dialog_popup(
		widget, id, "set device window coordinates:",
		SET_WINDOW, SET_WINDOW_STRING
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

	int	id = (int) client_data;

	void	CloseDisplay();

	simple_command(id, DONE_STRING, DONE);

	CloseDisplay(id);
	XtDestroyWidget(popUp[id]);
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
	int	id = (int) client_data;
	char	*current;

	extern	char	*TalkTo();

	command_Id.id = id;
	command_Id.command = LIST;


	current = TalkTo(id, "current\n", SYNC);

	if (current)
		UpdateFrameLabel(id, current);

}

/*ARGSUSED*/
static  void    PrintSelect(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	char	*spooler = XtName(widget);

	command_Id.id = id;
	command_Id.command = PRINT;

	Command((caddr_t) &command_Id, PRINT_STRING, spooler);
}


/*
 *	
 */
/*ARGSUSED*/
static  void    Save(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int		id = (int) client_data;

	create_simple_dialog_popup(
		widget, id, "Pleas enter file name:", SAVE, SAVE_STRING
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
	int	id = (int) client_data;
        Window  root;
	char	*zoom_str;
	float	llx, lly, urx, ury;
	float	llx_, lly_, urx_, ury_;
	float	new_llx, new_lly,
		new_urx, new_ury;
	float	ax, bx, ay, by;
	char	buf[80];

	extern	char	*ZoomCoords();

	root = RootWindowOfScreen(XtScreen(widget));

	/*
	 * get the new mapping
	 */
        if (ZoomCoords(XtDisplay(widget), root, &llx,&lly,&urx,&ury) == NULL){
                (void) fprintf(stderr, "Zoom failed\n");
		return;
        }

	/*
	 * grab the old mapping
	 */
	zoom_str = TalkTo(id, "zoom\n", SYNC);
	if (sscanf(zoom_str, "llx = %f, lly = %f, urx = %f, ury = %f", 
					&llx_, &lly_, &urx_, &ury_) != 4) {

		(void) fprintf(stderr, "Zoom failed\n");
		return;
	}

	/*
	 * map the new mapping into the old
	 */
	bx = llx_;
	ax = urx_ - bx;
	by = lly_;
	ay = ury_ - by;

	new_llx = (ax * llx) + bx;
	new_lly = (ay * lly) + by;
	new_urx = (ax * urx) + bx;
	new_ury = (ay * ury) + by;

	sprintf(buf, "%6.4f %6.4f %6.4f %6.4f",new_llx,new_lly,new_urx,new_ury);
	

	command_Id.id = id;
	command_Id.command = ZOOM;

	Command((caddr_t) &command_Id, ZOOM_STRING, buf);

	/*
	 * remember the zoom coordinates as if the SET_WINDOW command
	 * was called
	 */
	SetValues(id, SET_WINDOW, buf);
}

/*ARGSUSED*/
static  void    UnZoom(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	simple_command(id, UNZOOM_STRING, UNZOOM);
}


UpdateFrameLabel(id, frame_string)
	int	id;
	char	*frame_string;
{
	Arg		args[10];
	char		*s;
	
	s = icMalloc((unsigned) 
		(strlen(FRAME_LABEL_DISPLAY) + strlen(frame_string) + 1));

	(void) strcpy(s, FRAME_LABEL_DISPLAY);
	(void) strcat(s, frame_string);
	
	XtSetArg(args[0], XtNlabel, s);
	XtSetValues(frameLabel[id], args, 1);

	cfree(s);
}
