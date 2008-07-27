/*
 *	$Id: file.c,v 1.15 2008-07-27 03:18:38 haley Exp $
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
 *	file.c
 *
 *	Author		John Clyne
 *
 *	Date		Tue Jul 31 18:12:46 MDT 1990
 *
 *	This file together with 'w_file.c' define a file selection box. 
 *	Nonwidget-dependent code is located in this module. Widget-dependent
 *	code is in 'w_file.c'
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncarg/c.h>
#include "glob.h"
#include "idt.h"

static	char	*currentFileSelection = NULL;	/* user's current selection */
static	char	*currentPath = NULL;		/* path to the current file */

/*
 *	GetFiles
 *	[ exported to w_file.c ]
 *
 *	Get a list of files from the shell using file name substitution. 
 *	The memory allocated is available until the next invocation GetFiles.
 *	The path to the files in the file list is stored internaly.
 *
 * on entry
 *	*file_filter	: filter passed to the shell to match file
 * on exit
 *	*longest	: length of longest file found	
 *	return		: a null terminated, newline separated list of files;
 */
char	*GetFiles(file_filter, longest)
	char	*file_filter;
	int	*longest;

{
	char	**files;
	char	*s;
	int	file_count;
	int	len = 0;
	int	total_len = 0;
	int	i;

	static	char	*buf = NULL;	/* storage for list of files	*/
	static	char	*pathBuf = NULL;/* storage for path to files	*/
	

	if (buf) {
		free((Voidptr) buf);
		buf = NULL;
	}
	if (pathBuf) {
		free((Voidptr) pathBuf);
		pathBuf = NULL;
	}
	*longest = 10;

	/* 
	 * perform globing using the file filter
	 */
	glob(file_filter, &files, &file_count);

	if (file_count == 0) return ("");

	/*
	 * get the path to the file(s)
	 */
	s = strrchr(files[0], '/');
	if (s) {
		s++;
		if ( !(pathBuf = malloc((unsigned) (s - files[0] + 1)))) {
			perror("malloc()");
			return(NULL);
		}
		(void) strncpy(pathBuf, files[0], s - files[0]);
		pathBuf[s - files[0]] = '\0';
	}
	else {	/* default path is "./"	*/
		if ( !(pathBuf = malloc((unsigned) (strlen("./") + 1)))) {
			perror("malloc()");
			return(NULL);
		}
		(void) strcpy(pathBuf, "./");
	}
	currentPath = pathBuf;
		
	
	/*
	 * find out how much memory we need and what the longest file name is
	 * and stip off path of file
	 */
	for (i = 0, *longest = 0; i < file_count; i++) {
		/*SUPPRESS 624*/
		s = (s = strrchr(files[i], '/')) ? ++s : files[i];
		files[i] = s;
		len = strlen(files[i]);
		*longest = *longest < len ? len : *longest;
		total_len += len;
	}

	if ( !(buf = malloc( (unsigned) (total_len + file_count + 1 )))) {
		perror("malloc()");
		return(NULL);
	}
	buf[0] = '\0';

	/*
	 * create a newline separated list of files
	 */
	for (i = 0; i < file_count; i++) {
		(void) strcat(buf, files[i]);
		(void) strcat(buf, "\n");
	}

	return (buf);
}

/*
 *	SetFileSelection
 *	[ exported to w_file.c ]
 *
 *	Record the users file selection
 * on entry
 *	*file		: the selection
 */
void
SetFileSelection(file, select_action)
	char	*file;
	FuncPtrPasser	select_action;
{

	if (currentFileSelection) free ((Voidptr) currentFileSelection);

	if (! *file) {
		currentFileSelection = NULL;
		return;
	}

	/*
	 * if file name already contains a path name or currentPath is NULL
	 * don't cat on the current path name
	 */
	if ((*file == '.' && (*(file+1) == '.' || *(file+1) == '/')) 
			|| *file == '/' || *file == '~' || (! currentPath)) {

		currentFileSelection = malloc((unsigned) (strlen (file) +1));
		if (! currentFileSelection) {
			perror("malloc()");
			return;
		}
		(void) strcpy(currentFileSelection, file);
		if (select_action.func) {
			select_action.func();
		}
		return;
	}

	/*
	 * build the path to the file and store it
	 */
	currentFileSelection = malloc((unsigned) 
				(strlen(currentPath) + strlen (file) +1));
	if (! currentFileSelection) {
		perror("malloc()");
		return;
	}

	(void) strcpy(currentFileSelection, currentPath);
	(void) strcat(currentFileSelection, file);

	if (select_action.func) {
		select_action.func();
	}
}
	
/*
 *	GetFileSelection
 *	[exported]
 *
 *	Retrieve the user's selection
 */
char	*GetFileSelection()
{
	return (currentFileSelection);
}
