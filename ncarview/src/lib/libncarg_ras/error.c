/*
 *	$Id: error.c,v 1.8 2008-07-27 03:18:46 haley Exp $
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
