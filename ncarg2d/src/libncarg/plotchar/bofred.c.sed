/*
 * $Id: bofred.c.sed,v 1.7 2000-08-22 15:05:20 haley Exp $
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
