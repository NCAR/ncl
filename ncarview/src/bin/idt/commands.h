/*
 * $Id: commands.h,v 1.8 2000-08-22 03:30:16 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
