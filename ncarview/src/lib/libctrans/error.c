
/*
 *      $Id: error.c,v 1.1 1991-08-16 10:55:11 clyne Exp $
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
 *	errno		: an error code defined in error.h
 * on exit
 *	return		: < 0 => invalid error code, else ok.
 */
CtransSetError_(errno)
	int	errno;
{
	if ((errno > 0) && (errno < errorTableSize)) {
		errorNumber = errno;
		return(1);
	}

	/*
	 * invalid error number
	 */
	return(-1);
}

/*
 *	CtransGetErrorNumber_
 * 
 *	returns the current number
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
	return(errorMessages[errorNumber]);
}
