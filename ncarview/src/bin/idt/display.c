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
#include <ncarv.h>
#include "display.h"
#include "talkto.h"

char	*programName;

static	int	hFD = -1;	/* history file fd	*/

static	unsigned long	usedMask = 0;
static	int		numUsed = 0;

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
 *	*device		: device option for translator
 *	*font		: font option for translator
 *	*metafile	: name of metafile to translate
 *
 * on exit
 *	return		: -1 => error spawning translator; else connection id 
 */
int	OpenDisplay(device, font, metafile)
	char	*device,
		*font;
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

	if (OpenTranslator(id, device, font, metafile, hFD) < 0) {
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
	pcvs[id].loop = 0;
	pcvs[id].dup = 1;
	pcvs[id].goto_ = 1;
	pcvs[id].skip = 0;
	pcvs[id].start_segment = 1;
	pcvs[id].stop_segment = s ? atoi(s) : 1;


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

	(void)	CloseTranslator(id);

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
 *	history		: if true record all commands sent to translator to
 *			  a history file
 */
void	InitDisplayModule(program_name, history)
	char	*program_name;
	short	history;
{

	programName = icMalloc(strlen(program_name) + 1);
	(void) strcpy(programName, program_name);

	if (history) {
		hFD = open (HISTORY_FILE, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		if (hFD == -1) {
			perror(programName);
		}
	}
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
 *	return		: -1 => error; else the data is returned.
 */
int	GetValue(id, command)
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

	default:
		fprintf(stderr, "Illegal command\n");
		return (-1);

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
	int		value;
{

	switch ((int) command) {
	case	LOOP:
		pcvs[id].loop = value;
		break;

	case	DUP:
		pcvs[id].dup = value;
		break;

	case	SKIP:
		pcvs[id].skip = value;
		break;

	case	GOTO:
		pcvs[id].goto_ = value;
		break;

	case	START_SEGMENT:
		pcvs[id].start_segment = value;
		break;

	case	STOP_SEGMENT:
		pcvs[id].stop_segment = value;
		break;

	default:
		fprintf(stderr, "Illegal command\n");
		break;

	}
}
