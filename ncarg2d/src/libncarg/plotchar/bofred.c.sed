/*
 *	$Id: bofred.c.sed,v 1.3 1992-10-02 21:57:44 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR GRAPHICS V3.01                         *
*                                                                      *
***********************************************************************/

#include <stdio.h>
#include <ncarg/ncarg_loc.h>

/* Procedure bofred_(unit, fnum, ios, status)
 *
 *	bofred_ creates a fontname using the index "fnum" and opens
 *	the file in either the default library location, or locally.
 *	The file descriptor is returned to the FORTRAN caller in
 *	"unit", which simply passes it to other
 *	underlying C routines so FORTRAN never has cause to become
 *	upset because it is not actually a Logical Unit. The return
 *	code, "status", is not 0 if an error occurred, and 0 if
 *	all went well. The I/O status, "ios", is not used, but
 *	it is set equal to "status" so that it is used and
 *	compilers don't complain.
 */
bofred_(unit, fnum, ios, status)
	long		*unit, *fnum, *ios, *status;
{
	int             fd;
	char		fontname[32];
	char		*pathname;

	/* Get the path name to the fontcap file using the font number */

	(void) sprintf(fontname, "font%d", (int) *fnum);

	if ( (pathname = getFcapname(fontname)) == (char *) NULL)
	{
		(void) fprintf(stderr, 
		"Error in bofred_(): Could not find <%s>\n", fontname);
		*status = 1L;
		*unit = -1L;
	}
	else
		if ( (fd = open(pathname, 0)) == -1)
			*status = 1L;
		else
			*status = 0L;

	*unit = (long) fd;
	*ios = *status;
}
