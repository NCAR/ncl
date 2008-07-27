/*
 *	$Id: cmd_tab.c,v 1.7 2008-07-27 03:18:39 haley Exp $
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
 *	cmd_tab.c
 *	
 *	Author		John Clyne 
 *
 *	Date		Fri Jan 26 16:43:39 MST 1990
 *
 *	This file contains the user command table used to reach the 
 *	user functions.
 */
#include	<stdio.h>
#include	"med.h"

void	medCopy(), medDelete(), medEdit(), medLabel(), medHelp(), medMerge(), 
	medMove(), medPrint(), medQuit(), medRead(), medWrite(), medAppend(),
	medShell(), medSplit();

char	appendhelp[]	= "append contents of buffer to an existing NCAR CGM";
char	copyhelp[]	= "copy addressed frames to target address";
char	deletehelp[]	= "delete frames from buffer";
char	edithelp[]	= "edit a new file";
char	labelhelp[]	= "give a ascii name to a frame";
char	helphelp[]	= "help you";
char	mergehelp[]	= "merge two frames together";
char	movehelp[]	= "move frames within buffer";
char	printhelp[]	= "show buffer contents";
char	quithelp[]	= "exit med";
char	readhelp[]	= "read a file into current buffer";
char	writehelp[]	= "write buffer contents to a file";
char	splithelp[]	= "split the current file into N files";
char	shellhelp[]	= "escape to the shell";

char	appendusage[]	= "[ address [, address] ]  append [ file name ]";
char	copyusage[]	= "[ address [, address] ] copy [ address ]";
char	deleteusage[]	= "[ address [, address] ] delete"; 
char	editusage[]	= "edit [!] [file name]";
char	labelusage[]	= "[ address [, address] ] label < string >"; 
char	helpusage[]	= "help [ command ]"; 
char	mergeusage[]	= "< address1 , address2 > merge"; 
char	moveusage[]	= "[ address [, address] ] move [ address ]";
char	printusage[]	= "[ address [, address] ] print"; 
char	quitusage[]	= "quit [!]";
char	readusage[]	= "[ address ] read < file name >";
char	writeusage[]	= "[ address [, address] ]  write [ file name ]";
char	splitusage[]	= "[ address , address ] split <number> [file prefix]";
char	shellusage[]	= "! < shell command >";

/*
 * the command table. This table is used to access a user function by name
 */
Cmd	cmdtab[] = {
	{ "append",	appendhelp,	appendusage,	medAppend },
	{ "copy",	copyhelp,	copyusage,	medCopy },
	{ "delete",	deletehelp,	deleteusage,	medDelete },
	{ "edit",	edithelp,	editusage,	medEdit },
	{ "label",	labelhelp,	labelusage,	medLabel },
	{ "help",	helphelp,	helpusage,	medHelp },
	{ "merge",	mergehelp,	mergeusage,	medMerge },
	{ "move",	movehelp,	moveusage,	medMove },
	{ "print",	printhelp,	printusage,	medPrint },
	{ "quit",	quithelp,	quitusage,	medQuit },
	{ "read",	readhelp,	readusage,	medRead },
	{ "split",	splithelp,	splitusage,	medSplit },
	{ "write",	writehelp,	writeusage,	medWrite },
	{ "!",		shellhelp,	shellusage,	medShell },
	{ NULL}
	};

int	NUM_CMDS = (sizeof (cmdtab) / sizeof (Cmd)) - 1;
