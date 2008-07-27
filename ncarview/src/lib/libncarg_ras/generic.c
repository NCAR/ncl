/*
 *      $Id: generic.c,v 1.4 2008-07-27 03:18:46 haley Exp $
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
