/*
 *	$Id: binary.c,v 1.3 1993-01-17 06:51:36 don Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	sgiraster.c
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
 *		which provides access to a raster image sequence
 *		using a general abstraction.
 *
 *		This particular set of routines provides a file read
 *		function for generic binary raster files. It was written
 *		in about 5 minutes, so don't expect much ;-).
 *		
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include "ncarg_ras.h"

#define BINARY_FORMAT_NAME	"bin"

extern int			OptionInX;
extern int			OptionInY;
extern int			OptionInInvert;

Raster *
BinaryOpen(name)
	char	*name;
{
	Raster	*ras;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "BinaryOpen(\"%s\")", name);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "BinaryOpen(\"%s\")", name);
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, "BinaryOpen(\"%s\")", name);
			return( (Raster *) NULL );
		}
	}

	/* Record the name of the file. */

	ras->name = (char *) ras_calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, name);

	/* Record the format. */

	ras->format=(char *)ras_calloc((unsigned)(strlen(BINARY_FORMAT_NAME)+1),1);
	(void) strcpy(ras->format, BINARY_FORMAT_NAME);

	BinarySetFunctions(ras);

	return(ras);
}

int
BinaryRead(ras)
	Raster	*ras;
{
	int		status;
	int		y, length;
	unsigned char	*p;

	if (OptionInX == 0 || OptionInY == 0) {
		(void) ESprintf(E_UNKNOWN,
			"Binary(\"%s\") - Input resolution not specified",
			ras->name);
		return(RAS_ERROR);
	}

	if (ras->read == False) {
		/* Set file-related variables. */
		ras->read	= True;
		ras->written	= False;
		ras->file_nx	= OptionInX;
		ras->file_ny	= OptionInY;
		ras->file_type	= RAS_DIRECT;

		ras->nx		= OptionInX;
		ras->ny		= OptionInY;
		ras->type	= RAS_DIRECT;
		ras->length	= ras->nx * ras->ny * 3;
		ras->ncolor	= 256*256*256;

		/* Allocate image storage. */

		ras->data = (unsigned char *) ras_calloc(ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "BinaryRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}
	}

	if (OptionInInvert == False) { /* Don't invert the picture. */
		status = fread(ras->data, 1, ras->length, ras->fp);
		if (status == 0) {
			return(RAS_EOF);
		}
		else if (status != ras->length) {
			(void) ESprintf(errno, "BinaryRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}
	}
	else {
		for(y=0; y<ras->ny; y++) {
			p = &DIRECT_RED(ras, 0, ras->ny-y-1);
			length = ras->ny * 3;
			status = fread((char *) p, 1, length, ras->fp);
			if (status == 0) {
				return(RAS_EOF);
			}
			else if (status != length) {
				(void) ESprintf(errno,
					"BinaryRead(\"%s\" - Inverted)",
					ras->name);
				return(RAS_ERROR);
			}
		}
	}

	return(RAS_OK);
}

/* ARGSUSED */
Raster *
BinaryOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	char		*errmsg = "BinaryOpenWrite(\"%s\")";

	(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION, errmsg, name);

	return((Raster *) NULL);
}

int
BinaryWrite(ras)
	Raster	*ras;
{
	char		*errmsg = "BinaryWrite(\"%s\")";

	(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION, errmsg, ras->name);
	return(RAS_ERROR);
}

/* ARGSUSED */
int
BinaryPrintInfo(ras)
	Raster		*ras;
{
	return(RAS_OK);
}

int
BinaryClose(ras)
	Raster	*ras;
{
	int	status;

	status = GenericClose(ras);
	return(status);
}

int
BinarySetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = BinaryOpen;
	ras->OpenWrite = BinaryOpenWrite;
	ras->Read      = BinaryRead;
	ras->Write     = BinaryWrite;
	ras->Close     = BinaryClose;
	ras->PrintInfo = BinaryPrintInfo;
	ras->ImageCount = ImageCount_;
	return(RAS_OK);
}
