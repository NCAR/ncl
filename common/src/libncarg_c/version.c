
/*
 *      $Id: version.c,v 1.5 1992-09-01 23:47:32 clyne Exp $
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
#include "c.h"

#ifndef	VERSION
#define	VERSION	"unknown version"
#endif

void	PrintVersion(header)
	const char	*header;
{
	FILE	*fp;
	if ((fp = fopen("/dev/tty", "w")) == NULL) {
		fp = stderr;
	}

	if (header) (void) fprintf(fp, "%s - ", header);
	(void) fprintf(fp, "Version %s\n", VERSION);

	if (fp != stderr) fclose(fp);
}
