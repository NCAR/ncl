
typedef	struct	{
	int	loop,
		dup,
		goto_,
		skip,
		start_segment,
		stop_segment;
	} PlotCommandValues;


typedef	enum	{
	LOOP, DUP, SKIP, GOTO, START_SEGMENT, STOP_SEGMENT,
	PLAYBACK, JOGBACK, STOP, JOG, PLAY, LIST, SAVE, ZOOM, DONE, PRINT,
	REDRAW
	} DisplayCommands;

typedef	struct	{
	DisplayCommands	command;
	int	id;
	} Command_Id;	/* a command and the id of translator its to go to */

#define	MAX_DISPLAYS	10

#define	FRAME_LABEL_DISPLAY	"Scrolled to Frame -> "

