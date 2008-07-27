/*
 *	$Id: display.c,v 1.16 2008-07-27 03:18:38 haley Exp $
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
#include <string.h>
#include <ncarg/c.h>
#include "display.h"
#include "talkto.h"

static	int	hFD = -1;	/* history file fd	*/

static	unsigned long	usedMask = 0;
static	int		numUsed = 0;

static	char	**tArgv;	/* translator command line	*/
static	int	tArgc;

/*
 *	OpenDisplay
 *	[exported]
 *
 *	This routine is called to spawn a translator to process a file. 
 *	OpenDisplay returns a unique integer id that is associated with
 *	the particular instance of the translator spawned. This id is used
 *	by other routines to communicate with the appropriate translator.
 *
 * on entry
 *	*metafile	: name of metafile to translate
 *	wid		: window id for transator to display in
 *
 * on exit
 *	return		: -1 => error spawning translator; else connection id 
 */
int	OpenDisplay()
{
	int	id;		/* connection id		*/


	if (numUsed >= MAX_DISPLAYS) {
		return (-1);	/* too many connections		*/
	}
        /*
	 * find a free id
	 */
	/*SUPPRESS 570*/
        for(id = 0; id < MAX_DISPLAYS && ((usedMask >> id) & 1); id++);

	numUsed++;
	usedMask |= (1 << id); /* update bitmap to include new addition*/
	return(id);			/* return users file descriptor	*/
}

/*
 *	spawn the actual translator associated with $id.
 *	$metafile is the name of the metafile to process. $wid is the 
 *	X11 window id of the window in which the translator is to draw.
 */
int	StartTranslator(id, metafile, wid)
	int	id;
	char	*metafile;
	int	wid;
{
	char	widbuf[10];


	/*
	 * append the metafile name to the end of the translator command
	 * line
	 */
	tArgv[tArgc - 1] = metafile;

	/*
	 * stuff the window id in argv[2];
	 */
	sprintf(widbuf, "%d", wid);
	tArgv[2] = widbuf;

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
	if (TalkTo(id, "count\n", SYNC) == NULL) return (-1);

	return(1);
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
 *			  targv[0] = translator name
 *			  targv[2] is not used
 *	targc		: size of targv.
 *	history		: if true record all commands sent to translator to
 *			  a history file
 */
void	InitDisplayModule(
#ifdef	__STDC__
	char *program_name, char **targv, int targc, short history
	)
#else
	program_name, targv, targc, history
	)
	char *program_name; 
	char **targv; 
	int targc; 
	short history;
#endif
{

	if (history) {
		hFD = open (HISTORY_FILE, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		if (hFD == -1) {
			perror(program_name);
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
