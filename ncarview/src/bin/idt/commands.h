#ifndef	_commands_
#define _commands_

#define	DUP_STRING		"dup"
#define	GOTO_STRING		"plot"
#define	SKIP_STRING		"skip"
#define	START_SEGMENT_STRING	"start"
#define	STOP_SEGMENT_STRING	"stop"
#define	SET_WINDOW_STRING	"zoom"
#define	DONE_STRING		"quit"

#define	LOOP_STRING		"loop"
#define	PLAYBACK_STRING		".,1 plot"
#define	JOGBACK_STRING		".-1 plot"
#define	JOG_STRING		"plot"
#define	REDRAW_STRING		". plot"
#define	PLAY_STRING		".,$ plot"

#define	LIST_STRING		"list"
#define	SAVE_STRING		"save"
#define	ZOOM_STRING1		"zoom"
#define	ZOOM_STRING2		"\n. plot"
#define	UNZOOM_STRING		"zoom 0.0 0.0 1.0 1.0\n. plot"
#define	PRINT_STRING1		"spooler"
#define	PRINT_STRING2		"\n. Print"

#define	STOP_SIGNAL		SIGINT
#endif
