/*
 *      $Id: xt_env.c,v 1.10 2000-08-22 15:11:11 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
