/*
 * $Id: bofred.c,v 1.7 2008-07-23 16:16:58 haley Exp $
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


#include <stdio.h>
#include <ncarg/c.h>

/* Procedure bofred_(unit, fnum, ios, status)
 *
 *	bofred_ creates a fontname using the index "fnum" and opens
 *	the file in either the default library location, or locally.
 *	The file descriptor is returned to the FORTRAN caller in
 *	"unit", which simply passes it to other
 *	underlying C routines so FORTRAN never has cause to become
 *	upset because it is not actually a Logical Unit. The return
 *	code, "status", is not 0 if an error occurred, and 0 if
 *	all went well. The returned value of the "I/O status" ("ios")
 *	is always equal to that of "status"; "ios" is set only to keep
 *	certain "C" compilers from issuing warning messages.
 */
void
NGCALLF(bofred,BOFRED)(unit, fnum, ios, status)
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
	free(pathname);
}
