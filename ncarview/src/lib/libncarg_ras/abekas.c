/*
 *	$Id: abekas.c,v 1.2 1992-09-17 18:06:08 don Exp $
 */
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

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->dep = calloc(sizeof(AbekasInfo),1);
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

	ras->name = (char *) calloc( (unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) (strlen(FormatName) + 1), 1);
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
		ras->data = (unsigned char *) calloc((unsigned)ras->length, 1);
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

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->dep = calloc(sizeof(AbekasInfo),1);
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

	ras->name = (char *) calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	if (comment != (char *) NULL) {
		ras->text = (char *) calloc((unsigned) (strlen(comment) + 1),1);
		(void) strcpy(ras->text, comment);
	}
	else {
		ras->text = (char *) NULL;
	}

	ras->file_nx     = RAS_ABEKAS_NX;
	ras->file_ny     = RAS_ABEKAS_NY;
	ras->file_type   = RAS_DIRECT;

	ras->nx     = RAS_ABEKAS_NX;
	ras->ny     = RAS_ABEKAS_NY;
	ras->type   = RAS_DIRECT;
	ras->length = RAS_ABEKAS_NX * RAS_ABEKAS_NY * 3;

	ras->data   = (unsigned char *) calloc((unsigned) ras->length, 1);
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
	return(AbekasWriteRaster(ras, ras));
}

int
AbekasWriteRaster(ras, rasfile)
	Raster	*ras;
	Raster	*rasfile;
{
	char			*errmsg = "AbekasWriteRaster(\"%s\")";
	int			status;

	if (ras != rasfile) {
		status = RasterCenterCrop(ras, rasfile);
		if (status != RAS_OK) return(status);
	}

	status=write(rasfile->fd,(char*)rasfile->data, rasfile->length);
	if (status != rasfile->length) {
		(void) ESprintf(errno, errmsg, rasfile->name);
		return(RAS_ERROR);
	}
	rasfile->written = True;
	return(RAS_OK);
}

#ifdef DEAD
	int			sx, sy, dy;	/* source and dest indices */
	int			src_x, src_y;	/* source upper-left corner */
	int			src_nx, src_ny;	/* source extent */
	int			dst_x, dst_y;	/* dest upper-left corner */
	int			width, height;	/* video frame buffer extent */
	int			i;
	unsigned char		pixel, *src_ptr, *dst_ptr;

	width  = RAS_ABEKAS_NX;
	height = RAS_ABEKAS_NY;

	/* Set defaults for image positioning. */

	src_x = 0; src_y = 0;
	src_nx = ras->nx; src_ny = ras->ny;
	dst_x = 0; dst_y = 0;

	/* Calculate X mapping */

	if (ras->nx > width) {
		dst_x = 0;
		src_nx = width;
		if (OptionCenter)
			src_x = (ras->nx - width) / 2;
		else
			src_x = 0;
	}
	else {
		src_x = 0;
		src_nx = ras->nx;
		if (OptionCenter)
			dst_x = (width - src_nx) / 2;
		else
			dst_x = 0;
	}

	/* Calculate Y mapping */

	if (ras->ny >= height) {
		dst_y = 0;
		src_ny = height;
		if (OptionCenter)
			src_y = (ras->ny - height) / 2;
		else
			src_y = 0;
	}
	else {
		src_y = 0;
		src_ny = ras->ny;
		if (OptionCenter)
			dst_y = (height - src_ny) / 2;
		else
			dst_y = 0;
	}

	/*
	In order to avoid repetitive and costly address arithmetic,
	this loop works with pointers more than would be ideal.
	*/

	for(sy=src_y, dy=dst_y; sy<src_y+src_ny; sy++, dy++) {

		dst_ptr = &DIRECT_RED(rasfile, src_x, sy);

		if (ras->type == RAS_INDEXED) {
			src_ptr = &INDEXED_PIXEL(ras, src_x, sy);
		}
		else if (ras->type == RAS_DIRECT) {
			src_ptr = &DIRECT_RED(ras, src_x, sy);
		}

		for(sx=src_x; sx<src_x+src_nx; sx++) {
			if (ras->type  == RAS_INDEXED) {
				pixel      = *src_ptr++;
				*dst_ptr++ = ras->red[pixel];
				*dst_ptr++ = ras->green[pixel];
				*dst_ptr++ = ras->blue[pixel];
			}
			else if (ras->type == RAS_DIRECT) {
				*dst_ptr++ = *src_ptr++;
				*dst_ptr++ = *src_ptr++;
				*dst_ptr++ = *src_ptr++;
			}
		}
	}

	status = fwrite((char *)rasfile->data,rasfile->length,1,rasfile->fp);
	if (status != rasfile->length) {
		(void) ESprintf(errno, errmsg, rasfile->name);
		return(RAS_ERROR);
	}

	ras->written = True;

	return(RAS_OK);
#endif /* DEAD */

int
AbekasClose(ras)
	Raster	*ras;
{
	int	status;

	if (ras->fp != (FILE *) NULL) {
		if(ras->fp != stdin && ras->fp != stdout) {
			status = fclose(ras->fp);
			if (status != 0) {
				ESprintf(errno, "AbekasClose()");
				return(RAS_ERROR);
			}
		}
	}
	else {
		if (ras->fd != fileno(stdin) && ras->fd != fileno(stdout)) {
			status = close(ras->fd);
			if (status != 0) {
				ESprintf(errno, "AbekasClose()");
				return(RAS_ERROR);
			}
		}
	}
	switch(ras->type) {
		case RAS_INDEXED:
			free((char *) ras->data);
			free((char *) ras->red);
			free((char *) ras->green);
			free((char *) ras->blue);
			break;
		
		case RAS_DIRECT:
			free((char *) ras->data);
			break;
	}

	return(RAS_OK);
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
