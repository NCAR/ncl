
/*
 *      $Id: version.c,v 1.1 1992-02-13 14:02:57 clyne Exp $
 */
/*
 *	File:		version.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Feb 13 13:49:10 MST 1992
 *
 *	Description:	NCAR View version number utilities.
 */

#include <stdio.h>

#ifndef	VERSION
#define	VERSION	"unknown version"
#endif

void	PrintVersion(header)
	char	*header;
{
	FILE	*fp;
	if ((fp = fopen("/dev/tty", "w")) == NULL) {
		fp = stderr;
	}

	if (header) (void) fprintf(fp, "%s - ", header);
	(void) fprintf(fp, "%s\n", VERSION);
	(void) fflush(fp);
}
