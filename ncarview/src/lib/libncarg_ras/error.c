/*
 *	$Id: error.c,v 1.2 1991-08-16 11:08:37 clyne Exp $
 */
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
 *		state. If the problem was a system error,
 *		ras_errno is set to RAS_E_SYSTEM which is zero.
 *
 *		RasterPrintError() is generally used by the
 *		application to print an error when a library
 *		function returns either a NULL pointer, or
 *		the token RAS_ERROR. If its a system error
 *		RasterPrintError() simply uses perror() to
 *		print the message.

 *		RasterGetError() returns a pointer to the
 *		error message string, which would generally
 *		be used by an application, particularly
 *		one with a GUI interface where you don't
 *		want messages coming out on stderr but instead
 *		need to grab the message and display it in
 *		the GUI.
 */
#include <stdio.h>
#include <varargs.h>
#include "ncarg_ras.h"

/*LINTLIBRARY*/

extern	char	*strcpy(), *strcat();
extern int	errno;
extern char	*sys_errlist[];

char	*ras_errlist[] = {
	"System Error",
	"Internal programming",
	"Only 8-bit pixels supported",
	"Only 8-bit intensities supported",
	"Only 8-bit run lengths supported",
	"Image not in correct format",
	"Unsupported image encoding",
	"Improper colormap load",
	"Colormap too big",
	"Image size changed",
	"No format specified",
	"Cannot use stdin for HDF format",
	"NULL name provided",
	"Unknown format",
	"Invalid colormap entry",
	"Bad option",
	"Unsupported resolution",
	"Bogus raster structure",
	"Unsupported function",
	"Too many dither bits",
	"Sun RLE encoding not supported"
};

int	ras_nerr = sizeof(ras_errlist)/sizeof(char *);
int	ras_errno = 0;

static char	msgbuf[256];

#ifdef STANDALONE
char	*ProgramName;

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	status;

	ProgramName = argv[0];
	
	status = wooga();
	if (status == RAS_ERROR) RasterError();

	status = booga();
	if (status == RAS_ERROR) RasterError();

	status = snooga();
	if (status == RAS_ERROR) RasterError();
}

wooga()
{
	int	fd;
	fd = open("snooga", 0);
	if (fd == -1) (void) RasterSetError(RAS_E_SYSTEM);
	return(RAS_ERROR);
}

booga()
{
	(void) RasterSetError(RAS_E_8BIT_PIXELS_ONLY);
	return(RAS_ERROR);
}

snooga()
{
	(void) RasterSetError(RAS_E_8BIT_INTENSITIES_ONLY);
	return(RAS_ERROR);
}
#else

extern char	*ProgramName;
#endif STANDALONE

int
RasterSetError(errno)
	int	errno;
{
	if (errno > (ras_nerr-1)) {
		(void) fprintf(stderr, 
		"Now you're in trouble! The error routine has an error\n");
		return(RAS_ERROR);
	}

	ras_errno = errno;
	return(RAS_OK);
}

char *
RasterGetError()
{
	if (ras_errno == RAS_E_SYSTEM) {
		return(sys_errlist[errno]);
	}

	return(ras_errlist[ras_errno]);
}

int
RasterPrintError(msg)
	char	*msg;
{
	if (ras_errno == RAS_E_SYSTEM) {
		(void) strcpy(msgbuf, ProgramName);
		if (msg != (char *) NULL) {
			(void) strcat(msgbuf, " - ");
			(void) strcat(msgbuf, msg);
		}
		perror(msgbuf);
	}
	else {
		if (msg != (char *) NULL) {
			(void) fprintf(stderr,"%s - %s: %s\n",
				ProgramName, msg,
				ras_errlist[ras_errno]);
		}
		else {
			(void) fprintf(stderr,"%s: %s\n",
				ProgramName,
				ras_errlist[ras_errno]);
		}
	}

	return(RAS_OK);
}
