/*
 *      $Id: error.c,v 1.3 1992-02-11 14:59:12 clyne Exp $
 */
/*
 *	File:		error.c	
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 29 12:25:08 MDT 1991
 *
 *	Description:	This file defines a simplified error module for 
 *			the api defined by ctrans_api.c. 
 *
 */

#include <errno.h>
#include "ctrandef.h"

extern	int	sys_nerr;
extern	char	*sys_errlist[];

/*
 *	the current error number. errorNumber is used as an index into
 *	errorMessages structure.
 */
static	int	errorNumber  = 0;

static	char	*errorMessages[] = {
	"",
	"No such device",
	"Can't opening device",
	"No such font",
	"Error opening font",
	"Invalid state for operation",
	"Can't open metafile",
	"Error in metafile encoding - metafile can't be parsed",
	"Illegal or invalid metafile element", 
	"Invalid argument"
	};

static	int	errorTableSize = sizeof (errorMessages) / sizeof (char **);

/*
 *	CtransSetError_
 *
 *	Set the current error number. Error codes are defined in error.h
 *
 * on entry
 *	err_code	: an error code defined in error.h. If negative then
 *			  the unix global variable 'err_code' is ABS(err_code).	
 * on exit
 *	return		: < 0 => invalid error code, else ok.
 */
CtransSetError_(err_code)
	int	err_code;
{
	if ((err_code >= 0) &&  (err_code >= errorTableSize)) {
		/*
		 * invalid error number
		 */
		return(-1);
	}
	if ((err_code < 0) &&  (ABS(err_code) >= sys_nerr)) {
		/*
		 * invalid error number
		 */
		return(-1);
	}

	errorNumber = err_code;
	return(1);
}

/*
 *	CtransGetErrorNumber_
 * 
 *	returns the current number. If negative then the unix global 
 *	variable 'err_code' is ABS(err_code).	
 */
CtransGetErrorNumber_()
{
	return(errorNumber);
}

/*
 *	CtransGetErrorMessage_
 *
 *	returns an error message associated with the current error  number
 */
char	*CtransGetErrorMessage_()
{
	if (errorNumber >= 0) return(errorMessages[errorNumber]);

	return(sys_errlist[ABS(errorNumber)]);
}
