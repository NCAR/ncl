/*
 *      $Id: xt_env.c,v 1.11 2008-07-27 03:18:39 haley Exp $
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
 *	File:		xt_env.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 30 15:45:32 MDT 1991
 *
 *	Description:	This file contains routines that muck around with the
 *			X environment.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ncarg/c.h>
#include <string.h>

#include "xt_env.h"

/*
 *	XAppDirPath
 *
 *	This routine attempts to ensure that the correct X application 
 *	resource file is used for the application. If the user's environment
 *	doesn't have XUSERFILESEARCHPATH or XAPPLRESDIR environment variables
 *	set which point to the localtion of the app resoure file then  the
 *	XUSERFILESEARCHPATH env variable is set to where we think the resource
 *	file lives. We use the NCAR G param file to get our information.
 */
void	XAppDirPath()
{
	const char	*xapp_path;
	char		*xapp_env = "XAPPLRESDIR";

	static	char	*bufptr = NULL;

#ifdef	DEAD
	/*
	 * if either the XUSERFILESEARCHPATH or XAPPLRESDIR environment
	 * variables are set do nothing.
	 */
	if (getenv(xufsp_env)) return;
	if (getenv(xapp_env)) return;
#endif

	if ( !(xapp_path = GetNCARGPath(XAPPDIR))) {
		(void) fprintf(
			stderr, "NCARG xapp dir path not found [ %s ]\n",
			ErrGetMsg()
		);
		return;
	}

	if (bufptr) free ((Voidptr) bufptr);
	
	bufptr = malloc ((unsigned) 
		(strlen(xapp_env) + strlen("=") + strlen(xapp_path) + 1));

	if ( !bufptr) {
		perror("malloc()");
		return;
	}

	(void) strcpy(bufptr, xapp_env);
	(void) strcat(bufptr, "=");
	(void) strcat(bufptr, xapp_path);

	(void) putenv (bufptr);
}
