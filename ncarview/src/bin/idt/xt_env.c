/*
 *      $Id: xt_env.c,v 1.2 1991-08-15 17:16:44 clyne Exp $
 */
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
#include <ncarv.h>

#ifdef	SYSV
#include <string.h>
#else
#include <strings.h>
#endif

extern	char	*getenv();

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
	char	*xapp_path;
	char	*xufsp_env	= "XUSERFILESEARCHPATH";
	char	*xapp_env	= "XAPPLRESDIR";

	static	char	*bufptr = NULL;

	/*
	 * if either the XUSERFILESEARCHPATH or XAPPLRESDIR environment
	 * variables are set do nothing.
	 */
	if (getenv(xufsp_env)) return;
	if (getenv(xapp_env)) return;

	xapp_path = GetNCARGPath("XAPPDIR");

	/*
	 * if param file bombed use the default path that we were
	 * compiled with.
	 */
	xapp_path = xapp_path ? xapp_path : XAPPDIR_DEFAULT;

	if (bufptr) free (bufptr);
	
	bufptr = icMalloc ((unsigned) 
		(strlen(xapp_env) + strlen("=") + strlen(xapp_path) + 1));

	(void) strcpy(bufptr, xapp_env);
	(void) strcat(bufptr, "=");
	(void) strcat(bufptr, xapp_path);

	(void) putenv (bufptr);
}
