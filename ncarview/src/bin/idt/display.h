
#define	MAX_DATA_LEN	30
typedef	struct	{
	char	loop[MAX_DATA_LEN],
		dup[MAX_DATA_LEN],
		goto_[MAX_DATA_LEN],
		skip[MAX_DATA_LEN],
		start_segment[MAX_DATA_LEN],
		stop_segment[MAX_DATA_LEN],
		set_window[MAX_DATA_LEN],
		save[MAX_DATA_LEN];
	} PlotCommandValues;


typedef	enum	{
	LOOP, DUP, SKIP, GOTO, START_SEGMENT, STOP_SEGMENT,
	PLAYBACK, JOGBACK, STOP, JOG, PLAY, LIST, SAVE, ZOOM, UNZOOM, 
	DONE, PRINT, REDRAW, SET_WINDOW
	} DisplayCommands;

typedef	struct	{
	DisplayCommands	command;
	int	id;
	} Command_Id;	/* a command and the id of translator its to go to */

#define	MAX_DISPLAYS	10

#define	FRAME_LABEL_DISPLAY	"Scrolled to Frame -> "

