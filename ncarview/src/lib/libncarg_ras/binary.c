/*
 *	$Id: binary.c,v 1.4 1993-11-03 18:19:16 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	binary.c.c
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
extern int			OptionIndexed;

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

		ras->nx		= OptionInX;
		ras->ny		= OptionInY;


		if (OptionIndexed) {
			ras->file_type	= RAS_INDEXED;
			ras->type	= RAS_INDEXED;
			ras->length	= ras->nx * ras->ny;
			ras->ncolor	= 256;
		}
		else {
			ras->file_type	= RAS_DIRECT;
			ras->type	= RAS_DIRECT;
			ras->length	= ras->nx * ras->ny * 3;
			ras->ncolor	= 256*256*256;
		}

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
		if (OptionIndexed) {
			(void) ESprintf(E_UNKNOWN,"Can't invert Indexed image");
			return(RAS_ERROR);
		}
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
	Raster		*ras;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "File name not specified");
		return( (Raster *) NULL );
	}

	if (encoding != RAS_INDEXED) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
		"Only INDEXED color is supported for Binary rasterfiles");
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "ras_calloc(%d)", sizeof(Raster));
		return( (Raster *) NULL );
	}


	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
	}
	else {
		ras->fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0644);

		if (ras->fd == -1) {
			(void) ESprintf(errno, "open(%s,,0664)", name);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) ras_calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc(
			(unsigned) strlen(BINARY_FORMAT_NAME) + 1, 1
			);
	(void) strcpy(ras->format, BINARY_FORMAT_NAME);

	if (comment != (char *) NULL) {
		ras->text = (char *) ras_calloc(
			(unsigned) (strlen(comment) + 1),1
			);

		(void) strcpy(ras->text, comment);
	}
	else {
		ras->text = (char *) NULL;
	}

	ras->nx	= nx;
	ras->ny	= ny;
	ras->length	= ras->nx * ras->ny;
	ras->ncolor	= 256;
	ras->type	= RAS_INDEXED;
	ras->red	= (unsigned char *) ras_calloc((unsigned) ras->ncolor, 1);
	ras->green	= (unsigned char *) ras_calloc((unsigned) ras->ncolor, 1);
	ras->blue	= (unsigned char *) ras_calloc((unsigned) ras->ncolor, 1);
	ras->data	= (unsigned char *) ras_calloc((unsigned) ras->length, 1);
	ras->type = RAS_INDEXED;


	BinarySetFunctions(ras);

	return(ras);
}

int
BinaryWrite(ras)
	Raster	*ras;
{

	int	nb;

	nb = write(ras->fd, (char *) ras->data, ras->length);
	if (nb != ras->length) {
		(void) ESprintf(errno, "write(%d, ,%d)", ras->fd, ras->length);
		return(RAS_ERROR);
	}

	return(RAS_OK);
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
