/*
 *	$Id: display.c,v 1.5 1991-08-15 17:14:35 clyne Exp $
 */
/*
 *	Display.c
 *
 *	Author		John Clyne
 *
 *	Date		Mon Sep 17 13:36:04 MDT 1990
 *
 *	This module contains widget indepent code associated with the
 *	'display' panel.
 */
#include <stdio.h>
#include <fcntl.h>

#ifdef	SYSV
#include <string.h>
#else
#include <strings.h>
#endif

#include <ncarv.h>
#include "display.h"
#include "talkto.h"

char	*programName;

static	int	hFD = -1;	/* history file fd	*/

static	unsigned long	usedMask = 0;
static	int		numUsed = 0;

static	char	**tArgv;	/* translator command line	*/
static	int	tArgc;

static	PlotCommandValues	pcvs[MAX_DISPLAYS];	

/*
 *	OpenDisplay
 *	[exported]
 *
 *	This routine is called to spawn a translator to process a file. 
 *	OpenDisplay returns a unique integer id that is associated with
 *	the particular instance of the translator spawned. This id is used
 *	by other routines to communicate with the appropriate translator.
 *	OpenDisplay also sets some of the default values for idt commands
 * on entry
 *	*metafile	: name of metafile to translate
 *
 * on exit
 *	return		: -1 => error spawning translator; else connection id 
 */
int	OpenDisplay(metafile)
	char	*metafile;
{

	int	id;		/* connection id		*/
	char	*s;

	extern	char	*TalkTo();

	if (numUsed >= MAX_DISPLAYS) {
		return (-1);	/* too many connections		*/
	}

        /*
	 * find a free id
	 */
        for(id = 0; id < MAX_DISPLAYS && ((usedMask >> id) & 1); id++);

	/*
	 * append the metafile name to the end of the translator command
	 * line
	 */
	tArgv[tArgc - 1] = metafile;

	if (OpenTranslator(id, tArgv, hFD) < 0) {
		return(-1);	/* can't get a translator for this metafile*/
	}

	/*
	 * put translator in movie mode
	 */
	if (TalkTo(id, "movie 0\n", ASYNC) == NULL) return (-1);

	/*
	 * find out how many frames there are
	 */
	if ((s = TalkTo(id, "count\n", SYNC)) == NULL) return (-1);

	/*
	 * set some default data values for idt commands associated with 
	 * this connection
	 */
	(void) strncpy(pcvs[id].loop, "0", MAX_DATA_LEN - 1);
	(void) strncpy(pcvs[id].dup, "1", MAX_DATA_LEN - 1);
	(void) strncpy(pcvs[id].goto_, "1", MAX_DATA_LEN - 1);
	(void) strncpy(pcvs[id].skip ,"0", MAX_DATA_LEN - 1);
	(void) strncpy(pcvs[id].start_segment, "1", MAX_DATA_LEN - 1);
	(void) strncpy(pcvs[id].stop_segment,  s ? s : "1", MAX_DATA_LEN - 1);
	(void) strncpy(pcvs[id].set_window, "0.0 0.0 1.0 1.0", MAX_DATA_LEN - 1);


	numUsed++;
	usedMask |= (1 << id); /* update bitmap to include new addition*/
	return(id);			/* return users file descriptor	*/
}

/*
 *	CloseDisplay
 *	[exported]
 *
 *	Free a connection id and break communications with the associated 
 *	translator. The semantics for calling CloseDisplay more then once
 *	for the same id are undefined.
 * on entry
 *	id		: valid connection id return by OpenDisplay
 */
void	CloseDisplay(id)
	int	id;
{
	void	CloseTranslator();

	CloseTranslator(id);

	usedMask &= (~(1 << id));
	numUsed--;
}

/*
 *	InitDisplayModule
 *	[exported]
 *
 *	Initialize this module. This routine must be called prior to any 
 *	other functions defined in this file. The semantics of calling
 *	InitDisplayModule() more then once are undefined
 *
 * on entry
 *	*program_name	: the name of the program, used for generating error
 *			  messages
 *	targv		: the translator command line
 *	targc		: size of targv.
 *	history		: if true record all commands sent to translator to
 *			  a history file
 */
void	InitDisplayModule(program_name, targv, targc, history)
	char	*program_name;
	char	**targv;
	int	targc;
	short	history;
{

	programName = icMalloc((unsigned) (strlen(program_name) + 1));
	(void) strcpy(programName, program_name);

	if (history) {
		hFD = open (HISTORY_FILE, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		if (hFD == -1) {
			perror(programName);
		}
	}

	/*
	 * record translator command line
	 */
	tArgv = targv;
	tArgc = targc;
}

/*
 *	CloseDisplayModule()
 *	[exported]
 *
 *	For proper termination of idt this routine must be invoked before 
 *	exiting. The semantics of calling this routine more then once or before
 *	InitDisplayModule() are undefined
 */
void	CloseDisplayModule()
{
	if (hFD != -1) {
		(void) close(hFD);
	}
}

/*
 *	GetValue
 *	[exported]
 *
 *	Get a command value for a particular command and connection id
 * on entry
 *	id		: the connection id
 *	command		: the command
 *
 * on exit
 *	return		: NULL => error; else the data is returned.
 */
char	*GetValue(id, command)
	int		id;
	DisplayCommands	command;
{

	switch ((int) command) {
	case	LOOP:
		return (pcvs[id].loop);

	case	DUP:
		return (pcvs[id].dup);

	case	SKIP:
		return (pcvs[id].skip);

	case	GOTO:
		return (pcvs[id].goto_);

	case	START_SEGMENT:
		return (pcvs[id].start_segment);

	case	STOP_SEGMENT:
		return (pcvs[id].stop_segment);

	case	SET_WINDOW:
		return (pcvs[id].set_window);

	case	SAVE:
		return (pcvs[id].save);

	default:
		(void) fprintf(stderr, "Illegal command\n");
		return (NULL);

	}
}

/*
 *	SetValue
 *	[exported]
 *
 *	Set a command value for a particular command and connection id
 * on entry
 *	id		: the connection id
 *	command		: the command
 *	value		: the data
 */
void	SetValues(id, command, value)
	int		id;
	DisplayCommands	command;
	char		*value;
{

	switch ((int) command) {
	case	LOOP:
		(void) strncpy(pcvs[id].loop, value, MAX_DATA_LEN - 1);
		break;

	case	DUP:
		(void) strncpy(pcvs[id].dup, value, MAX_DATA_LEN - 1);
		break;

	case	SKIP:
		(void) strncpy(pcvs[id].skip, value, MAX_DATA_LEN - 1);
		break;

	case	GOTO:
		(void) strncpy(pcvs[id].goto_, value, MAX_DATA_LEN - 1);
		break;

	case	START_SEGMENT:
		(void) strncpy(pcvs[id].start_segment, value, MAX_DATA_LEN - 1);
		break;

	case	STOP_SEGMENT:
		(void) strncpy(pcvs[id].stop_segment, value, MAX_DATA_LEN - 1);
		break;

	case	SET_WINDOW:
		(void) strncpy(pcvs[id].set_window, value, MAX_DATA_LEN - 1);
		break;

	case	SAVE:
		(void) strncpy(pcvs[id].save, value, MAX_DATA_LEN - 1);
		break;

	default:
		(void) fprintf(stderr, "Illegal command\n");
		break;

	}
}
