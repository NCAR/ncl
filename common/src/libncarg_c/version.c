/*
 *      $Id: version.c,v 1.8 2000-08-22 04:03:33 haley Exp $
 */
/************************************************************************
*                                                                       *
*			     Copyright (C)  2000	                        		*
*	     University Corporation for Atmospheric Research		        *
*			     All Rights Reserved			                        *
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

const char *GetNCARGVersion(
#ifdef	NeedFuncProto
	void				 
#endif
)
{
	static char version[] = VERSION; 

	return version;
}
