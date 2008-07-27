/*
 *	$Id: cmd_tab.c,v 1.8 2008-07-27 03:18:45 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/

/*
 *	cmd_tab.c
 *	
 *	Author		John Clyne 
 *
 */
#include	<stdio.h>
#include	"ictrans.h"

int	iCHelp(), iCFile(), iCPlot(), Noop(), iCMovie(), iCDevice(),
	iCFont(), iCQuit(), iCNextfile(), iCList(), iCAlias(), iCSpooler(),
	iCPrint(), iCShell(), iCSave(), iCZoom(), iCCount(), iCCurrent(),
	iCDup(), iCLoop(), iCStartSegment(), iCStopSegment(), iCSkip(),
	iCMerge();

char	helphelp[] =	
	"Provide description of all commands or usage for a single command";
char	filehelp[] =	"Translate a new file";
char	plothelp[] =	"Plot the addressed frames to the selected device";
char	moviehelp[] =	
	"Toggle movie mode. If set wait for return before plotting advancing";
char	devicehelp[] =	"Set the output device for subsequent plots";
char	fonthelp[] =	"Set the stroked-text font for subsequent plots";
char	quithelp[] =	"Terminate the session";
char	noophelp[] =	"Do nothing";
char	nextfilehelp[] =	"Process the next file in the file list";
char	listhelp[] =	"List the addressed frames from the file";
char	aliashelp[] =	"Assign a spooler definition to an alias name";
char	duphelp[] =	"Set duplication number for subsequent plots";
char	loophelp[] =	"Toggle looping off or on";
char	starthelp[] =	"Define the starting frame in a segment";
char	stophelp[] =	"Define the last frame in a segment";
char	skiphelp[] =	"Set frame increment value";
char	counthelp[] =	"Report the number of frames in the current file";
char	currenthelp[] =	"Report the current frame number";
char	spoolerhelp[] =	"Set or get current spooler name";
char	Printhelp[] =	"Send frames to a print spooler";
char	shellhelp[] =	"Escape to the shell";
char	savehelp[] =	"Save frames to a file";
char	Savehelp[] =	"Force a save to a file";
char	zoomhelp[] =	"Change window coordinates to facilitate zooming";
char	mergehelp[] =	"Overlay one frame on top of another frame";

char	helpusage[]	= "help [ command ]";
char	fileusage[]	= "file [ filename ]";
char	plotusage[]	= "[ frame list ] plot";
char	movieusage[]	= "movie [ time ]";
char	deviceusage[]	= "device [ device_name ]";
char	fontusage[]	= "font [ font_name ]";
char	quitusage[]	= "quit";
char	nextfileusage[]	= "nextfile ";
char	listusage[]	= "[ frame list ] list";
char	aliasusage[]	= "alias [name [ :trans args:[ | (args)+ ]* [(>|>>) file]] ]";
char	dupusage[]	= "dup [ num frames ]";
char	loopusage[]	= "loop ";
char	startusage[]	= "[ start frame ] start ";
char	stopusage[]	= "[ last frame ] stop ";
char	skipusage[]	= "skip [ value ] ";
char	countusage[]	= "count ";
char	currentusage[]	= "current ";
char	spoolerusage[]	= "spooler [ alias ]";
char	Printusage[]	= "[ frame list ] Print";
char	shellusage[]	= "! shell_command";
char	saveusage[]	= "[ frame list ] save [ filename ]";
char	Saveusage[]	= "[ frame list ] Save [ filename ]";
char	zoomusage[]	= "zoom [ llx [ lly [ urx [ ury ]]] ]";
char	mergeusage[]	= "< frame1 > < frame2 > merge";

/*
 */
CmdOp	cmdOptab[] = {
	{"plot",	plothelp,	plotusage,	1,0,	iCPlot },
	{"alias",	aliashelp,	aliasusage,	0,1,	iCAlias },
	{"count",	counthelp,	countusage,	0,0,	iCCount },
	{"current",	currenthelp,	currentusage,	0,0,	iCCurrent },
	{"device",	devicehelp,	deviceusage,	0,1,	iCDevice },
	{"dup",		duphelp,	dupusage,	0,1,	iCDup },
	{"file",	filehelp,	fileusage,	0,1,	iCFile },
	{"font",	fonthelp,	fontusage,	0,1,	iCFont },
	{"help",	helphelp,	helpusage,	0,1,	iCHelp },
	{"list",	listhelp,	listusage,	1,0,	iCList },
	{"loop",	loophelp,	loopusage,	0,0,	iCLoop },
	{"movie",	moviehelp,	movieusage,	0,1,	iCMovie },
	{"nextfile",	nextfilehelp,	nextfileusage,	0,1,	iCNextfile },
	{"Print",	Printhelp,	Printusage,	1,0,	iCPrint },
	{"quit",	quithelp,	quitusage,	0,0,	iCQuit },
	{"save",	savehelp,	saveusage,	1,1,	iCSave },
	{"Save",	Savehelp,	Saveusage,	1,1,	iCSave },
	{"skip",	skiphelp,	skipusage,	0,1,	iCSkip },
	{"spooler",	spoolerhelp,	spoolerusage,	0,1,	iCSpooler },
	{"start",	starthelp,	startusage,	1,0,	iCStartSegment},
	{"stop",	stophelp,	stopusage,	1,0,	iCStopSegment },
	{"zoom",	zoomhelp,	zoomusage,	0,1,	iCZoom },
	{"merge",	mergehelp,	mergeusage,	1,0,	iCMerge },
	{"!",		shellhelp,	shellusage,	0,1,	iCShell },
	{"Noop",	noophelp,	"",		0,0,	Noop },
	{NULL}
	};

int	NUM_CMDS = (sizeof (cmdOptab) / sizeof (CmdOp)) - 1;




/*
 *	getcmdOp
 *	[exported]
 *	
 *	get a command object from the command table using its name for a
 *	lookup. Command names may be abreviated.
 * on entry
 *	*name		: name to lookup
 * on exit
 *	return		: if found return command obj ptr. If name is ambiguous
 *			  return -1. If not found return NULL.
 */
CmdOp	*getcmdOp (name)
	char	*name;
{
	char *p, *q;
	CmdOp *c, *found;
	int nmatches, longest;

	longest = 0;
	nmatches = 0;
	found = NULL;

	for (c = cmdOptab; p = c->c_name; c++) {
		for (q = name; *q == *p++; q++) {
			if (*q == 0)            /* exact match? */
				return (c);
		}
		if (!*q) {                      /* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)	/* ambiguous	*/
		return ((CmdOp *)-1);
	return (found);
}


