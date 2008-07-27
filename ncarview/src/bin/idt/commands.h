/*
 * $Id: commands.h,v 1.9 2008-07-27 03:22:37 haley Exp $
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

#ifndef	_commands_
#define _commands_

#include <signal.h>

#define	DUP_STRING		"dup %s\n"
#define	GOTO_STRING		"%s plot\n"
#define	SKIP_STRING		"skip %s\n"
#define	START_SEGMENT_STRING	"%s start\n"
#define	STOP_SEGMENT_STRING	"%s stop\n"
#define	SET_WINDOW_STRING	"zoom %s\n"
#define	DONE_STRING		"quit\n"

#define	LOOP_STRING		"loop\n"
#define	PLAYBACK_STRING		".,1 plot\n"
#define	JOGBACK_STRING		".-1 plot\n"
#define	JOG_STRING		"plot\n"
#define	REDRAW_STRING		". plot\n"
#define	PLAY_STRING		".,$ plot\n"

#define	LIST_STRING		"%s list\n"
#define	SAVE_STRING		". Save %s\n"
#define	ZOOM_STRING		"zoom %s\n. plot\n"
#define	UNZOOM_STRING		"zoom 0.0 0.0 1.0 1.0\n. plot\n"
#define	PRINT_STRING		"spooler %s\n. Print\n"
#define	NOOP_STRING		"Noop\n"

#define	STOP_SIGNAL		SIGINT

extern void Command();

#endif
