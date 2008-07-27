/*
 *      $Id: version.c,v 1.10 2008-07-27 12:23:46 haley Exp $
 */
/************************************************************************
*                                                                       *
*			     Copyright (C)  2000	                        		*
*	     University Corporation for Atmospheric Research		        *
*			     All Rights Reserved			                        *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

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

#ifndef	NGVERSION
#define	NGVERSION	"unknown version"
#endif

#ifndef	NCLVERSION
#define	NCLVERSION	"unknown version"
#endif

void	PrintVersion(header)
	const char	*header;
{
	FILE	*fp;
	if ((fp = fopen("/dev/tty", "w")) == NULL) {
		fp = stderr;
	}

	if (header) (void) fprintf(fp, "%s - ", header);
	(void) fprintf(fp, "Version %s\n", NGVERSION);

	if (fp != stderr) fclose(fp);
}

const char *GetNCARGVersion(
#ifdef	NeedFuncProto
	void				 
#endif
)
{
	static char ngversion[] = NGVERSION; 

	return ngversion;
}
const char *GetNCLVersion(
#ifdef	NeedFuncProto
	void				 
#endif
)
{
	static char nclversion[] = NCLVERSION; 

	return nclversion;
}
