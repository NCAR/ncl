/* 
 * $Id: display.h,v 1.8 2000-07-12 18:13:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

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
	float			ar;	/* current zoom aspect ratio	*/
	float			llx,
				lly,
				urx,
				ury;	/* current zoom coords		*/
				

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





extern	int	OpenDisplay();

extern	int	StartTranslator(
#ifdef	NeedFuncProto
	int	id,
	char	*metafile,
	int	wid
#endif
);

extern	void	CloseDisplay(
#ifdef	NeedFuncProto
	int	id
#endif
);

extern	void	InitDisplayModule(
#ifdef	NeedFuncProto
	char	*program_name,
	char	**targv,
	int	targc,
	short	history
#endif
);

extern	void	CloseDisplayModule();






#endif	/* _display_	*/
