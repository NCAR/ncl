/*
 *      $Id: generic.c,v 1.3 2000-08-22 15:12:10 haley Exp $
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

/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		generic.
 *
 *	Author:		Don Middleton
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sat Jan 16 23:27:26 MST 1993
 *
 *	Description:	This file contains functions that parallel
 *			those of the various raster drivers, with
 *			the exception that these are generic.
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ncarg/c.h>
#include "ncarg_ras.h"

int
GenericClose(ras)
	Raster	*ras;
{
	int		status;
	char		*errmsg = "GenericClose(\"%s\")";

	if (ras == (Raster *) NULL) {
		ESprintf(RAS_E_PROGRAMMING, errmsg, ras->name);
		return(RAS_ERROR);
	}

	if (ras->fp != (FILE *) NULL) {
		if(ras->fp != stdin && ras->fp != stdout) {
			status = fclose(ras->fp);
			if (status != 0) {
				ESprintf(errno, errmsg, ras->name);
				return(RAS_ERROR);
			}
		}
	}
	else {
		if (ras->fd != fileno(stdin) && ras->fd != fileno(stdout) &&
		ras->fd != -1) {
			status = close(ras->fd);
			if (status != 0) {
				ESprintf(errno, errmsg, ras->name);
				return(RAS_ERROR);
			}
		}
	}

	if (ras->dep   !=  (char *) NULL) {
		ras_free((char *)ras->dep);
		ras->dep = (char *) NULL;
	}
	if (ras->name  !=  (char *) NULL) {
		ras_free((char *)ras->name);
		ras->name = (char *) NULL;
	}
	if (ras->format!=  (char *) NULL) {
		ras_free((char *)ras->format);
		ras->format = (char *) NULL;
	}
	if (ras->text  !=  (char *) NULL) {
		ras_free((char *)ras->text);
		ras->text = (char *) NULL;
	}
	if (ras->data  != (unsigned char *) NULL) {
		ras_free((char *)ras->data);
		ras->data = (unsigned char *) NULL;
	}
	if (ras->red   != (unsigned char *) NULL) {
		ras_free((char *)ras->red);
		ras->red = (unsigned char *) NULL;
	}
	if (ras->green != (unsigned char *) NULL) {
		ras_free((char *)ras->green);
		ras->green = (unsigned char *) NULL;
	}
	if (ras->blue  != (unsigned char *) NULL) {
		ras_free((char *)ras->blue);
		ras->blue = (unsigned char *) NULL;
	}

	ras_free((void *) ras);

	return(RAS_OK);
}

int
GenericDestroy(ras)
	Raster	*ras;
{
	if (ras != (Raster *) NULL) {
		(void) ras_free((Voidptr) ras);
	}
	return(RAS_OK);
}
