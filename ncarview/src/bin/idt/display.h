#ifndef	_display_
#define	_display_

#include <X11/Intrinsic.h>
#include "animate.h"

#define	MAX_DATA_LEN	80
typedef	struct	{
	int	dup,
		goto_,
		skip,
		delay,
		start_segment,
		stop_segment,
		current_frame;
	char	set_window[MAX_DATA_LEN],
		save[MAX_DATA_LEN];
	} PlotCommandValues;

#define	DEFAULT_DUP		1
#define	DEFAULT_GOTO		1
#define	DEFAULT_SKIP		0
#define	DEFAULT_DELAY		0
#define	DEFAULT_START		1
#define	DEFAULT_STOP		-1
#define	DEFAULT_CURRENT		1
#define	DEFAULT_SET_WINDOW	"0.0 0.0 1.0 1.0"
#define	DEFAULT_SAVE		"new.ncgm"

typedef	struct	WidgetDataStruct {
	Display			*dpy;
	XtAppContext		app_context;
	Widget			popup;	/* toplevel popup display widget */
	Widget			frame_label;	/* display current frame */
	Widget			canvas;	/* drawing canvas		*/
	Window			win;	/* drawing canvas window	*/
	int			id;	/* communication id for display	*/
	int			current_frame_num;
	PlotCommandValues	pcv;	/* translator command values	*/
	Bool			do_play;	/* in play mode?	*/
	Bool			do_animate;	/* in animate mode?	*/
	AnimateType		*a;

	/*
	 * command buttons
	 */
	Widget	scrollbar,
		playback, 
		jogback,
		stop,
		play,
		jog,
		loop,
		goto_,
		dup,
		skip,
		delay,
		start_segment,
		stop_segment,
		set_window,
		save, 
		done,
		current_frame,
		print,
		zoom,
		unzoom,
		animate;
	} WidgetData;


typedef	enum	{
	LOOP, DUP, SKIP, GOTO, START_SEGMENT, STOP_SEGMENT,
	PLAYBACK, JOGBACK, STOP, JOG, PLAY, LIST, SAVE, ZOOM, UNZOOM, 
	DONE, PRINT, REDRAW, SET_WINDOW
	} DisplayCommands;

#define	MAX_DISPLAYS	10

#define	FRAME_LABEL_DISPLAY	"Scrolled to Frame -> "


#endif	/* _display_	*/
