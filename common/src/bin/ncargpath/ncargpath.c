/*
 *      $Id: ncargpath.c,v 1.4 2000-08-22 04:04:15 haley Exp $
 */
/************************************************************************
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
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/************************************************************************
*                                                                       *
*                Copyright (C)  1992                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *	File:		ncargpath.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Sep 4 14:07:57 MDT 1992
 *
 *	Description:	Return the full path name to a given NCAR G
 *			directory.
 */

#include <stdio.h>
#include <string.h>
#include <ncarg/c.h>

void	usage(progname)
	char	*progname;
{
	(void) fprintf(stderr, "Usage: %s <directory>\n", progname);
}

main(argc, argv)
	int	argc;
	char	**argv;
{
	char	*progname;
	const char	*path;

	progname = (progname = strrchr(argv[0],'/')) ? ++progname : *argv;

	if (argc != 2) {
		usage(progname);
		exit(1);
	}

	if (! (path = GetNCARGPath(argv[1]))) {
		(void) fprintf(
			stderr, "Path to directory(%s) not found [ %s ]\n",
			argv[1], ErrGetMsg()
		);
		exit(1);
	}

	(void) fprintf(stdout, "%s\n", path);
		
	exit(0);
}
