/*
 *	$Id: cleartext.c,v 1.9 2008-07-27 03:18:46 haley Exp $
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

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	cleartext.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	1/31/91
 *
 *	Description:
 *		This file contains a collection of functions
 *		which provides access to a raster sequence
 *		using a general abstraction.
 *
 *		This particular set of routines provides
 *		a set of clear text functions that simply
 *		print calling information on stderr.
 *		
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include "ncarg_ras.h"

static char	*FormatName = "cleartext";

Raster *
ClearTextOpen(name)
	char	*name;
{
	Raster	*ras;

	(void) fprintf(stderr, "ClearTextOpen(%s)\n", name);

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "ClearTextOpen(%s)", name);
		return( (Raster *) NULL );
	}

	ras->name = (char *) ras_calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned)(strlen(FormatName) + 1),1);
	(void) strcpy(ras->format, FormatName);

	(void) ClearTextSetFunctions(ras);

	return(ras);
}

/*ARGSUSED*/
Raster *
ClearTextOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	int		encoding;
{
	Raster		*ras;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME,"ClearTextOpenWrite(%s)", name);
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "ClearTextOpenWrite(%s)", name);
		return( (Raster *) NULL );
	}

	ras->dep = (char *) NULL;

	ras->name = (char *) ras_calloc((unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) (strlen(FormatName)+1), 1);
	(void) strcpy(ras->format, FormatName);

	if (encoding == RAS_INDEXED) {
		ras->type	= RAS_INDEXED;
		ras->nx		= nx;
		ras->ny		= ny;
		ras->length	= ras->nx * ras->ny;
		ras->ncolor	= 256;
		ras->red=(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->green=(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->blue=(unsigned char *)ras_calloc((unsigned) ras->ncolor,1);
		ras->data=(unsigned char *)ras_calloc((unsigned) ras->length,1);
	}
	else if (encoding == RAS_DIRECT) {
		ras->type	= RAS_DIRECT;
		ras->nx		= nx;
		ras->ny		= ny;
		ras->length	= ras->nx * ras->ny * 3;
		ras->ncolor	= 0;
		ras->red	= (unsigned char *) NULL;
		ras->green	= (unsigned char *) NULL;
		ras->blue	= (unsigned char *) NULL;
		ras->data       = (unsigned char *)
				  ras_calloc((unsigned) ras->length, 1);
	}
	else {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
			"ClearTextOpenWrite(encoding=%d)", encoding);
		return( (Raster *) NULL );
	}

	(void) ClearTextSetFunctions(ras);

	return(ras);
}

int
ClearTextWrite(ras)
	Raster	*ras;
{
	(void) fprintf(stderr, "ClearTextPrintInfo(%s)\n", ras->name);
	return(RAS_OK);
}

int
ClearTextPrintInfo(ras)
	Raster		*ras;
{
	(void) fprintf(stderr, "ClearTextPrintInfo(%s)\n", ras->name);
	return(RAS_OK);
}

int
ClearTextRead(ras)
	Raster	*ras;
{
	(void) fprintf(stderr, "ClearTextRead(%s)\n", ras->name);
	return(RAS_OK);
}

int
ClearTextClose(ras)
	Raster	*ras;
{
	int	status;

	(void) fprintf(stderr, "ClearTextClose(%s)\n", ras->name);
	status = GenericClose(ras);
	return(status);
}

int
ClearTextSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	(void) fprintf(stderr, "ClearTextSetFunctions(%s)\n", ras->name);
	ras->Open      = ClearTextOpen;
	ras->OpenWrite = ClearTextOpenWrite;
	ras->Read      = ClearTextRead;
	ras->Write     = ClearTextWrite;
	ras->Close     = ClearTextClose;
	ras->PrintInfo = ClearTextPrintInfo;
	ras->ImageCount = ImageCount_;
	return(RAS_OK);
}
