/*
 *      $Id: ncargpath.c,v 1.5 2008-07-27 12:22:58 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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
