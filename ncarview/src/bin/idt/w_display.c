/*
 *	$Id: w_display.c,v 1.8 1992-04-03 23:21:15 clyne Exp $
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

extern	void	Command1(), Command2(), Command3();

/*
 * callbacks
 */
static	void	Scroll();
static	void	Done(), CurrentFrame(), NoPrint(), PrintSelect(), 
		Save(), Zoom();
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
	Widget	paned;
	Cardinal	n;

	int	id;	/* translator connection id	*/

	void	create_tip_top_panel(), create_top_panel(), 
		create_middle_panel(), create_bottom_panel();

	/*
	 * open a connection to a translator
	 */
	if ((id = OpenDisplay(metafile)) < 0) {
		(void) fprintf(stderr, "Translator aborted\n");
		return;
	}

	n = 0;
	popUp[id] = XtCreatePopupShell(metafile, topLevelShellWidgetClass, 
		button, args, n);

	paned = XtCreateManagedWidget("paned",
				panedWidgetClass,popUp[id], (ArgList) NULL,0);

	/*
	 * The main display is made up of three sub-panels
	 */
	create_tip_top_panel(paned, id);

	create_top_panel(paned, id);

	create_middle_panel(paned, id);

	create_bottom_panel(paned, id);


	XtPopup(popUp[id], XtGrabNone);
}


void	create_tip_top_panel(paned, id)
	Widget	paned;
	int	id;
{
	Widget	form;
	Widget	scrollbar;

	Cardinal	n;
	Arg		args[10];

	form = XtCreateManagedWidget("form",
				formWidgetClass,paned, (ArgList) NULL,0);

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

	form = XtCreateManagedWidget("form",
				formWidgetClass,paned, (ArgList) NULL,0);

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

	form = XtCreateManagedWidget("form",
				formWidgetClass,paned, (ArgList) NULL,0);

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

static	void	create_bottom_panel(paned, id) 
	Widget	paned;
	int	id;
{
	Arg		args[10];
	Widget	form;
	Widget	save, current_frame, zoom, done;
	Widget	print, menu, entry;
	Cardinal	n;
	char	*alias_list,
		**spooler_list,
		**ptr;

	extern	char	*TalkTo();
	extern	char	**SpoolerList();

	form = XtCreateManagedWidget("form",
				formWidgetClass,paned, (ArgList) NULL,0);

        n = 0;
        done = XtCreateManagedWidget("done",commandWidgetClass,form,args,n);
	XtAddCallback(done, XtNcallback, Done, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, done);	n++;
        current_frame = XtCreateManagedWidget("current frame",
				commandWidgetClass,form,args,n);
	XtAddCallback(current_frame,XtNcallback,CurrentFrame,(XtPointer) id);


	alias_list = TalkTo(id, "alias\n", SYNC);
	spooler_list = SpoolerList(alias_list);
	if (*spooler_list) {
		n = 0;
		XtSetArg(args[n], XtNfromHoriz, current_frame);	n++;
		print = XtCreateManagedWidget("print",menuButtonWidgetClass,
								form,args,n);

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
		n = 0;
		XtSetArg(args[n], XtNfromHoriz, current_frame);	n++;
		print = XtCreateManagedWidget("print",commandWidgetClass,
								form,args,n);

		XtAddCallback(print, XtNcallback, NoPrint, (XtPointer) id);
	}

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, print);	n++;
        save = XtCreateManagedWidget("save",commandWidgetClass,form,args,n);
	XtAddCallback(save, XtNcallback, Save, (XtPointer) id);

        n = 0;
	XtSetArg(args[n], XtNfromHoriz, save);	n++;
        zoom = XtCreateManagedWidget("zoom",commandWidgetClass,form,args,n);
	XtAddCallback(zoom, XtNcallback, Zoom, (XtPointer) id);
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
	void	simple_command();

	playMode = True;
	simple_command(id, PLAYBACK);
	UpdateFrameLabel(id, "");

}

/*ARGSUSED*/
static  void    Jogback(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	simple_command();

	simple_command(id, JOGBACK);
	UpdateFrameLabel(id, "");

}

/*ARGSUSED*/
static  void    Stop(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	simple_command();

	if (playMode) {
		simple_command(id, STOP);
		playMode = False;
	}
	else {
		simple_command(id, REDRAW);
	}

}

/*ARGSUSED*/
static  void    Jog(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	simple_command();

	simple_command(id, JOG);
	UpdateFrameLabel(id, "");

}

/*ARGSUSED*/
static  void    Play(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	simple_command();

	playMode = True;
	simple_command(id, PLAY);
	UpdateFrameLabel(id, "");

}

/*ARGSUSED*/
static  void    Loop(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	simple_command();

	simple_command(id, LOOP);

}
/*ARGSUSED*/
static  void    Dup(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	create_simple_dialog_popup();

	create_simple_dialog_popup(widget, id, "dup:", DUP);

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
	void	create_simple_dialog_popup();

	create_simple_dialog_popup(widget, id, "goto:", GOTO);

}
/*ARGSUSED*/
static  void    Skip(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	create_simple_dialog_popup();

	create_simple_dialog_popup(widget, id, "skip:", SKIP);

}
/*ARGSUSED*/
static  void    Start_Segment(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	create_simple_dialog_popup();

	create_simple_dialog_popup(widget, id, "start segment:", START_SEGMENT);

}
/*ARGSUSED*/
static  void    Stop_Segment(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	create_simple_dialog_popup();

	create_simple_dialog_popup(widget, id, "stop segment:", STOP_SEGMENT);

}
/*ARGSUSED*/
static  void    Set_Window(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;
	void	create_simple_dialog_popup();

	create_simple_dialog_popup(widget, id, 
		"set device window coordinates:",SET_WINDOW);

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

	void	simple_command();
	void	CloseDisplay();

	simple_command(id, DONE);

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

	Command3((caddr_t) &command_Id, spooler);
}

/*ARGSUSED*/
static  void    NoPrint(widget, client_data, call_data)
	Widget  widget;
	XtPointer       client_data,	/* display id	*/
			call_data;	/* not used	*/
{
	int	id = (int) client_data;

	Message(id, "No spooled devices found");

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
	void	create_simple_dialog_popup();

	create_simple_dialog_popup(widget, id, "Pleas enter file name:", SAVE);

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
	char	*s;
        Window  root;

	extern	char	*ZoomCoords();

	root = RootWindowOfScreen(XtScreen(widget));

        s = ZoomCoords(XtDisplay(widget), root);

        if (s == NULL) {
                (void) fprintf(stderr, "error in zoom\n");
        }
        else {
		command_Id.id = id;
		command_Id.command = ZOOM;

		Command2((caddr_t) &command_Id, s);
        }
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
 */
static	void	create_simple_dialog_popup(widget, id, command_name, command)
	Widget	widget;
	int	id;
	char	*command_name;
	DisplayCommands	command;

{
	char	*value;

	void	CreateSimpleDialogPopup();
	char	*GetValue();

	value = GetValue(id, command);

	command_Id.id = id;
	command_Id.command = command;

	CreateSimpleDialogPopup(widget, command_name,
				Command2, (caddr_t) &command_Id,value);
}

/*
 *	simple_command
 *	[internal]
 *
 * on entry
 *	id		: connection of translator 
 *	command		: command this box represents
 */
static	void	simple_command(id, command)
	int	id;
	DisplayCommands	command;

{

	command_Id.id = id;
	command_Id.command = command;

	Command1((caddr_t) &command_Id);
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
