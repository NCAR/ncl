/*
 * $Id: bofred.c.sed,v 1.5 1994-03-01 21:45:21 haley Exp $
 */

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
	int		*unit, *fnum, *ios, *status;
{
	int		fd;
	char	fontname[32];
	char	*pathname;

	/* Get the path name to the fontcap file using the font number */

	(void) sprintf(fontname, "font%d", *fnum);

	if ( (pathname = getFcapname(fontname)) == (char *) NULL)
	{
		(void) fprintf(stderr, 
		"Error in bofred_(): Could not find <%s>\n", fontname);
		*status = 1;
		*unit = -1;
	}
	else
		if ( (fd = open(pathname, 0)) == -1)
			*status = 1;
		else
			*status = 0;

	*unit = fd;
	*ios = *status;
}
