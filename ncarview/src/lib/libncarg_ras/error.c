/*
 *	$Id: error.c,v 1.7 2000-08-22 15:12:10 haley Exp $
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

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	error.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	3/7/91
 *
 *	Description:
 *		RasterSetError(error_number) is generally
 *		used by the library to set a particular error
 *		state.
 *
 *		RasterPrintError() is generally used by the
 *		application to print an error when a library
 *		function returns either a NULL pointer, or
 *		the token RAS_ERROR. If its a system error
 *		RasterPrintError() simply uses perror() to
 *		print the message.
 *
 *		RasterGetError() returns a pointer to the
 *		error message string, which would generally
 *		be used by an application, particularly
 *		one with a GUI interface where you don't
 *		want messages coming out on stderr but instead
 *		need to grab the message and display it in
 *		the GUI.
 */
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "ncarg_ras.h"
#include "error.h"

/*LINTLIBRARY*/

static int	ras_nerr = sizeof(ras_errlist)/sizeof(char *);
static int	hdf_nerr = sizeof(hdf_errlist)/sizeof(char *);

extern char	*NrtProgramName;

int
RasterInitError()
{
	(void) ErrorList(RAS_ERROR_START, ras_nerr, ras_errlist);
	(void) ErrorList(HDF_ERROR_START, hdf_nerr, hdf_errlist);
	return(RAS_OK);
}

int
RasterSetError(error_number)
	int	error_number;
{
	(void) ESprintf(error_number, "");
	return(RAS_OK);
}

const char *
RasterGetError()
{
	return(ErrorGetMessage());
}

int
RasterEsprintfError()
{
	(void) fprintf(stderr, "%s: %s\n", NrtProgramName, ErrorGetMessage());
	return(RAS_OK);
}

int
RasterPrintError()
{
	(void) fprintf(stderr, "%s: %s\n", NrtProgramName, ErrorGetMessage());
	return(RAS_OK);
}
