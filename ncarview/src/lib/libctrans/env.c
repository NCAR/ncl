/*
 *	$Id: env.c,v 1.8 2000-08-22 15:11:46 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1992                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
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

		

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <ncarg/c.h>

/*	env.c
 *
 *	Authors:	John Clyne
 *			Don Middleton
 *                      Dave Kennison (modifications, July, 1991)
 *		
 *	This file contains procedures that return information
 *	about runtime environments, such as the type of graphics
 *	terminal you are using.
 *
 *	$GRAPHCAP
 *		Names the graphic display device. This
 *		variable may be set to an absolute or
 *		relative pathname, if so desired. Otherwise,
 *		the name is searched for in $NCARG/graphcaps.
 *
 *	$FONTCAP
 *              Just like GRAPHCAP, but the default location
 *		is in $NCARG/fontcaps.
 *
 *
 *	11/88	Added support for install-time paths and
 *		absolute pathnames - Don Middleton 11/88
 *
 *
 *      07/91   Code revised to use information from the "NCAR Graphics
 *              parameter file".  Threw out unused stuff, including the
 *              header file "env.h".
 */ 



/*      getFcapname
 *
 *		get path to fontcap
 *	on entry:
 *		device : 	name of fontcap
 *	on exit
 *		return() = 	path to fontcap
 */
char *  getFcapname( device )
	const char	*device;
{
	const char	*path;
	char	*fcap;

	if ( device == (char *) NULL )
	{
		if ( (device = getenv("FONTCAP")) == (char *) NULL)
		{
			ESprintf(E_UNKNOWN, "No font specified");
			return( (char *) NULL);
		}
	}

	if ( *device == '/' || *device == '.' ) /* absolute path */
	{
		return( (char * ) device );
	}

	if ( (path = GetNCARGPath(FONTCAPDIR)) == NULL )
	{
#ifdef	DEAD
		/*
		 * can't find path so assume current directory
		 */
		path = ".";
#endif
		ESprintf(
			E_UNKNOWN, "NCARG font directory not found [ %s ]",
			ErrGetMsg()
		);
		return(NULL);
	}


	fcap = (char *)malloc(
		(unsigned) strlen(path) +
		(unsigned) strlen(device) + 2
	);
	if (! fcap) {
		ESprintf(errno, "malloc()");
		return(NULL);
	}
	fcap = strcpy(fcap,path);
	fcap = strcat(fcap,"/");
	fcap = strcat(fcap,device);
	return(fcap);
}


/*      getGcapname
 *
 *		get path to graphcap
 *	on entry:
 *		device : 	name of graphcap
 *	on exit
 *		return() = 	path to graphcap
 */
char *  getGcapname( device )
	const char	*device;
{
	const char	*path;
	char	*gcap;

	if ( device == (char *) NULL )
	{
		if ( (device = getenv("GRAPHCAP")) == (char *) NULL)
		{
			ESprintf(E_UNKNOWN, "No device specified");
			return(NULL);
		}
	}

	if ( *device == '/' || *device == '.' ) /* absolute path */
	{
		return( (char * ) device );
	}
	if ( (path = GetNCARGPath(GRAPHCAPDIR)) == NULL )
	{
#ifdef	DEAD
		/*
		 * can't find path so assume current directory
		 */
		path = ".";
#endif
		ESprintf(
			E_UNKNOWN, "NCARG device directory not found [ %s ]",
			ErrGetMsg()
		);
		return(NULL);
	}

	gcap = (char *)malloc(
		(unsigned)strlen(path)+
		(unsigned)strlen(device) + 2
	);
	if (! gcap) {
		ESprintf(errno, "malloc()");
		return(NULL);
	}
	gcap = strcpy(gcap,path);
	gcap = strcat(gcap,"/");
	gcap = strcat(gcap,device);
	return(gcap);
}
