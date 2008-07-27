/*
 *	$Id: abekas.c,v 1.7 2008-07-27 03:18:45 haley Exp $
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
/*	File:	abekas.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	8-92
 *
 *	Description:
 *		This file contains a collection of functions
 *		which provides access to a raster sequence
 *		using a general abstraction.
 *
 *		This particular set of routines provides
 *		basic file access functions for the rasterfile
 *		format supported by the Abekas A60 digital
 *		recorder, and also the Quantel Paintbox.
 *
 *		The Abekas A60 has a fixed resolution of 720x486
 *		and images come in RGB and YUV flavors.
 *		
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include "ncarg_ras.h"
#include "options.h"
#include "abekas.h"

/*LINTLIBRARY*/

static char	*FormatName = "a60";

Raster *
AbekasOpen(name)
	char	*name;
{
	char		*errmsg = "AbekasOpen(\"%s\")";
	Raster		*ras;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(AbekasInfo),1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) ESprintf(errno, errmsg, name);
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, errmsg, name);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) ras_calloc( (unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) (strlen(FormatName)+1),1);
	(void) strcpy(ras->format, FormatName);

	AbekasSetFunctions(ras);

	return(ras);
}

int
AbekasRead(ras)
	Raster	*ras;
{
	char			*errmsg = "AbekasRead(\"%s\")";
	int			status;

	/*
	Initialize per-file information on first read. This is
	done for consistency, but there are no headers and
	there is an implicit assumption of a fixed resolution and
	encoding. Nothing can really change from one frame
	to another.
	*/

	if (ras->read == False) {
		ras->read      = True;
		ras->file_nx   = RAS_ABEKAS_NX;
		ras->file_ny   = RAS_ABEKAS_NY;
		ras->file_type = RAS_DIRECT;
	}

	ras->nx     = RAS_ABEKAS_NX;
	ras->ny     = RAS_ABEKAS_NY;
	ras->type   = RAS_DIRECT;
	ras->length = RAS_ABEKAS_NX * RAS_ABEKAS_NY * 3;

	if (ras->data == (unsigned char *) NULL) {
		ras->data=(unsigned char *)ras_calloc((unsigned)ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, errmsg, ras->name);
			return(RAS_ERROR);
		}
	}

	status = fread((char *)ras->data, 1, ras->length, ras->fp);
	if (status != ras->length) {
		(void) ESprintf(errno, errmsg, ras->name);
		return(RAS_EOF);
	}

	return(RAS_OK);
}

/*ARGSUSED*/
Raster *
AbekasOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	char		*errmsg = "AbekasOpenWrite(\"%s\")";
	Raster		*ras;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(AbekasInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
	}
	else {
		ras->fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		if (ras->fd == -1) {
			(void) ESprintf(errno, errmsg, name);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) ras_calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	if (comment != (char *) NULL) {
		ras->text = (char *)ras_calloc((unsigned)(strlen(comment)+1),1);
		(void) strcpy(ras->text, comment);
	}
	else {
		ras->text = (char *) NULL;
	}

	ras->file_nx     = nx;
	ras->file_ny     = ny;
	ras->file_type   = encoding;

	ras->nx     = nx;
	ras->ny     = ny;
	ras->type   = encoding;

	if (encoding == RAS_INDEXED) {
		ras->length = ras->nx * ras->ny;
		ras->ncolor = 256;
		ras->red  =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		if (ras->red == (unsigned char *) NULL) {
			(void) ESprintf(errno, errmsg, ras->name);
			return((Raster *) NULL);
		}
		ras->green=(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		if (ras->green == (unsigned char *) NULL) {
			(void) ESprintf(errno, errmsg, ras->name);
			return((Raster *) NULL);
		}
		ras->blue =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		if (ras->blue == (unsigned char *) NULL) {
			(void) ESprintf(errno, errmsg, ras->name);
			return((Raster *) NULL);
		}
	}
	else if (encoding == RAS_DIRECT) {
		ras->length = ras->nx * ras->ny * 3;
		ras->ncolor = 256 * 256 * 256;
	}

	ras->data   = (unsigned char *) ras_calloc((unsigned) ras->length, 1);
	if (ras->data == (unsigned char *) NULL) {
		(void) ESprintf(errno, errmsg, ras->name);
		return((Raster *) NULL);
	}

	AbekasSetFunctions(ras);

	return(ras);
}

int
AbekasWrite(ras)
	Raster	*ras;
{
	int		status;
	char		*errmsg = "AbekasWrite(\"%s\")";
	static Raster	*outraster = (Raster *) NULL;

	/*
	** Allocate an Abekas raster object, which is always
	** a fixed size. Then take whatever raster is passed
	** in and format it into the new object for writing.
	*/
	if (outraster == (Raster *) NULL) {
		outraster = RasterCreate(RAS_ABEKAS_NX,
					 RAS_ABEKAS_NY,
					 RAS_DIRECT);
		if (outraster == (Raster *) NULL) {
			return(RAS_ERROR);
		}
	}

	status = RasterCenterCrop(ras, outraster);
	if (status != RAS_OK) return(status);

	status=write(ras->fd,(char*)outraster->data, outraster->length);
	if (status != outraster->length) {
		(void) ESprintf(errno, errmsg, ras->name);
		return(RAS_ERROR);
	}
	ras->written = True;
	return(RAS_OK);
}

int
AbekasClose(ras)
	Raster	*ras;
{
	int	status;

	status = GenericClose(ras);
	return(status);
}

/*ARGSUSED*/
AbekasPrintInfo(ras)
	Raster	*ras;
{

	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "Abekas Rasterfile Information\n");
	(void) fprintf(stderr, "---------------------------\n");
	
	return(RAS_OK);
}

int
AbekasSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = AbekasOpen;
	ras->OpenWrite = AbekasOpenWrite;
	ras->Read      = AbekasRead;
	ras->Write     = AbekasWrite;
	ras->Close     = AbekasClose;
	ras->PrintInfo = AbekasPrintInfo;
	ras->ImageCount = ImageCount_;
}
