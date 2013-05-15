/*
 *      $Id: xresources.c,v 1.1 1993-04-01 23:04:54 clyne Exp $
 */
/*
 *
 * Copyright (c) 1988-91 by Patrick J. Naughton.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation.
 *
 * This file is provided AS IS with no warranties of any kind.  The author
 * shall have no liability with respect to the infringement of copyrights,
 * trade secrets or any patents by this file or any part thereof.  In no
 * event will the author be liable for any lost revenue or profits or
 * other special, indirect and consequential damages.
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xresources.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 1 13:29:49 MST 1993
 *
 *	Description:	Provide an interface for querying X resources that
 *			may be set in a application file, resource defaults
 *			file, or on the X server. 
 *
 *			Most of the code in here was shamelessly stolen
 *			from the original author, Patrick J. Naughton.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include "xresources.h"


#ifndef DEF_FILESEARCHPATH
#define DEF_FILESEARCHPATH "/usr/lib/X11/%T/%N%S"
#endif


/*
 *	Search for a application resource file and return its database
 */
static	XrmDatabase parsefilepath(xfilesearchpath, TypeName, ClassName)
	char       *xfilesearchpath;
	char       *TypeName;
	char       *ClassName;
{
	XrmDatabase database = NULL;
	char        appdefaults[1024];
	char       *src;
	char       *dst;

	src = xfilesearchpath;
	appdefaults[0] = '\0';
	dst = appdefaults;
	while (1) {
		if (*src == '%') {
			src++;
			switch (*src) {
			case '%':
			case ':':
				*dst++ = *src++;
				*dst = '\0';
				break;
			case 'T':
				(void) strcat(dst, TypeName);
				src++;
				dst += strlen(TypeName);
				break;
			case 'N':
				(void) strcat(dst, ClassName);
				src++;
				dst += strlen(ClassName);
				break;
			case 'S':
				src++;
				break;
			default:
				src++;
				break;
			}
		} 
		else if (*src == ':') {
			database = XrmGetFileDatabase(appdefaults);
			if (database == NULL) {
				dst = appdefaults;
				src++;
			} else {
				break;
			}
		} 
		else if (*src == '\0') {
			database = XrmGetFileDatabase(appdefaults);
			break;
		} 
		else {
			*dst++ = *src++;
			*dst = '\0';
		}
	}
	return (database);
}


/*
 * Function:	_CtOpenResources()
 *
 * Description:	Create a resource database for a given application which
 *		is the merger of the application, user, server and command
 *		line data base for a given application.
 *
 * In Args:
 *	*dpy		The display
 *	*prog_name	Name of this program.
 *	*class_name	Application class name.
 *	*argc		pointer to command line argument vector count
 *	**argv		command line args
 *	xodr		Command line options to parse
 *	xodr_size	size of xodr.
 *
 * Out Args:
 *	argv		arguments recoginized are removed from argv.
 *	argc		size of argv on out.		
 *
 * Return Values:
 *			Return a XrmDatabase for subsequent queries via
 *			_CtGetResources().
 *
 * Side Effects:
 */
XrmDatabase	_CtOpenResources(
	dpy, prog_name, class_name, argc, argv,xodr,xodr_size
	)

	Display			*dpy;
	char			*prog_name;
	char			*class_name;
	int			*argc;
	char			**argv;
	XrmOptionDescRec	*xodr;
	int			xodr_size;
{
	XrmDatabase	RDB = NULL;
	XrmDatabase	cmdlineDB = NULL;
	XrmDatabase	applicationDB = NULL;
	XrmDatabase	serverDB = NULL;
	XrmDatabase	userDB = NULL;
	char		userfile[1024];
	char		homeenv[80];
	char		*userpath;
	char		*env;
	char		*server_string;
	int		i;

	XrmInitialize();

	if (! (env = getenv("HOME"))) {
		homeenv[0] = '\0';
	}
	else {
		strncpy(homeenv, env, sizeof(homeenv)-1);
	}

	env = getenv("XFILESEARCHPATH");
	applicationDB = parsefilepath(env ? env : DEF_FILESEARCHPATH,
				  "app-defaults", class_name);

	/*
	 * parse the command line options into the cmdline Data Base.
	 */
	if (xodr_size) {
		XrmParseCommand(
			&cmdlineDB, xodr, xodr_size, prog_name, argc, argv
		);
	}

	if (! (userpath = getenv("XUSERFILESEARCHPATH"))) {
		env = getenv("XAPPLRESDIR");
		if (env)
			sprintf(userfile, "%s/%%N:%s/%%N", env, homeenv);
		else
			sprintf(userfile, "%s/%%N", homeenv);
		userpath = userfile;
	}
	userDB = parsefilepath(userpath, "app-defaults", class_name);


	server_string = XResourceManagerString(dpy);
	if (server_string) {
		serverDB = XrmGetStringDatabase(server_string);
	} else {
		char        buf[1024];
		sprintf(buf, "%s/.Xdefaults", homeenv);
		serverDB = XrmGetFileDatabase(buf);
	}

	if (applicationDB) (void) XrmMergeDatabases(applicationDB, &RDB);
	if (userDB) (void) XrmMergeDatabases(userDB, &RDB);
	if (serverDB) (void) XrmMergeDatabases(serverDB, &RDB);
	if (cmdlineDB) (void) XrmMergeDatabases(cmdlineDB, &RDB);

	return(RDB);
}

/*
 * Function:	_CtCloseResources()
 *
 * Description:	Free a resource database created with _CtOpenResources().
 *
 * In Args:
 *	xrd		database to be freed.
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
void	_CtCloseResources(xrd)
	XrmDatabase	xrd;
{
	if (xrd) {
		(void) XrmDestroyDatabase(xrd);
	}
}


/*
 * Function:	_CtGetResource()
 *
 * Description:	Query the given database, $database, for a given resource
 *		and return its value string. Subsequent calls to 
 *		_CtGetResource may overwrite previously returned values.
 *
 * In Args:
 *	database	XrmDatabase created with _CtOpenResources()
 *	*parentname	parent of resource.
 *	*parentclass	parent class of resource.
 *	*name		name of resource to query
 *	*class		class of resource to query
 *	*def		default resource value to return if resource is not 
 *			found.
 *	
 *
 * Out Args:
 *
 * Return Values:
 *			Return resource value.
 *
 * Side Effects:
 */
char	*_CtGetResource( database, parentname, parentclass, name, class, def )
	XrmDatabase	database;
	char		*parentname;
	char		*parentclass;
	char		*name;
	char		*class;
	char		*def;
{
	char		*type;	
	XrmValue	value;
	char		*string;
	char		*buffer;
	char		fullname[1024];
	char		fullclass[1024];
	int		len;

	sprintf(fullname, "%s.%s", parentname, name);
	sprintf(fullclass, "%s.%s", parentclass, class);
	if (XrmGetResource(database, fullname, fullclass, &type, &value)) {
		string = value.addr;
		len = value.size;
	} 
	else {
		string = def;
		len = strlen(string);
	}
        buffer = (char*) malloc(len + 1);
	(void) strncpy(buffer, string, len);
	buffer[len] = '\0';

	return(buffer);
}


