/* $Id: avsraster.c,v 1.5 1992-09-01 23:44:33 clyne Exp $ */

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	avsraster.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	August 91
 *
 *	Description:
 *		This file contains a collection of functions
 *		which provides access to a raster sequence
 *		using a general abstraction.
 *
 *		This particular set of routines provides
 *		basic file access functions for AVS raster
 *		files.
 *
 *		Encoding schemes are limited to 32-bit rasters 
 *		where each pixel is stored as a 4 byte vector: 
 *		Alpha, Red, Green, Blue
 *		
 */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <malloc.h>
#include "ncarg_ras.h"
#include "avsraster.h"

static char	*FormatName = "avs";
extern char	*ProgramName;

Raster *
AVSOpen(name)
	char	*name;
{
	Raster	*ras;

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return( (Raster *) NULL );
		}
	}

	ras->dep = (char *) NULL;

	ras->name = (char *) calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	(void) AVSSetFunctions(ras);

	return(ras);
}

int
AVSRead(ras)
	Raster	*ras;
{
	AVSInfo			*dep;
	unsigned int		image_size;
	int			status;
	int			x, y;
	unsigned long		swaptest = 1;
	unsigned char		*ptmp;
	static unsigned char	*tmpbuf = (unsigned char *) NULL;
	static int		tmplen = 0;

	/* Allocate the raster format dependent (header) structure. */

	if (ras->dep == (char *) NULL) {
		ras->dep =  (char *) calloc(sizeof(AVSInfo),1);
		if (ras->dep == (char *) NULL) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return(RAS_ERROR);
		}
	}

	dep = (AVSInfo *) ras->dep;

	/* Read the AVS raster file header. */

	status = fread((char *) dep, 1, sizeof(AVSInfo), ras->fp);
	if (status != sizeof(AVSInfo)) return(RAS_EOF);

	/* Swap bytes if necessary. */

	if (*(char *) &swaptest) {
		_swapshort((char *) dep, sizeof(AVSInfo));
	}

	ras->type  = RAS_DIRECT;
	ras->depth = 24;
	ras->nx    = dep->nx;
	ras->ny    = dep->ny;
	ras->length = ras->nx * ras->ny * 3;

	/* If not initialized, allocate image memory */

	if (ras->data == (unsigned char *) NULL) {
		image_size = ras->nx * ras->ny * 3;

		ras->data = (unsigned char *) calloc( (unsigned) image_size, 1);

		if (ras->data == (unsigned char *) NULL) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return(RAS_ERROR);
		}
	}

	/* We'll want to discard the alpha channel.  */

	if (tmpbuf == (unsigned char *) NULL) {
		tmplen = ras->nx * ras->ny * 4;
		tmpbuf = (unsigned char *) calloc( (unsigned) tmplen, 1);
		if (tmpbuf == (unsigned char *) NULL) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return(RAS_ERROR);
		}
	}
	status = fread( (char *) tmpbuf, 1,
		(int) tmplen, ras->fp);
	if (status != tmplen) return(RAS_EOF);

	ptmp = tmpbuf;

	for(y=0; y<ras->ny; y++) {
	for(x=0; x<ras->nx; x++) {
		DIRECT_RED(ras, x, y)   = ptmp[1];
		DIRECT_GREEN(ras, x, y) = ptmp[2];
		DIRECT_BLUE(ras, x, y)  = ptmp[3];
		ptmp += 4;
	}}

	return(RAS_OK);
}

Raster *
AVSOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	int		encoding;
{
	Raster		*ras;
	AVSInfo		*dep;

	if (name == (char *) NULL) {
		(void) RasterSetError(RAS_E_NULL_NAME);
		return( (Raster *) NULL );
	}

	if (encoding != RAS_DIRECT) {
		(void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
		return( (Raster *) NULL );
	}

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	ras->dep = calloc(sizeof(AVSInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	dep = (AVSInfo *) ras->dep;

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
	}
	else {
		ras->fd = open(name, O_WRONLY | O_CREAT, 0644);
		if (ras->fd == -1) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) calloc((unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	ras->text = (char *) calloc((unsigned) (strlen(comment) + 1), 1);
	(void) strcpy(ras->text, comment);

	ras->nx		= nx;
	ras->ny		= ny;
	ras->length	= ras->nx * ras->ny * 3;
	ras->ncolor	= 0;
	ras->type	= RAS_DIRECT;
	ras->red	= (unsigned char *) NULL;
	ras->green	= (unsigned char *) NULL;
	ras->blue	= (unsigned char *) NULL;
	ras->data	= (unsigned char *) calloc((unsigned) ras->length, 1);

	dep->unused1	= 0;
	dep->nx		= nx;
	dep->unused2	= 0;
	dep->ny		= ny;

	(void) AVSSetFunctions(ras);

	return(ras);
}

int
AVSWrite(ras)
	Raster	*ras;
{
	AVSInfo			*dep;
	int			nb;
	int			x, y;
	unsigned long		swaptest = 1;
	static unsigned char	*ptmp, *tmpbuf = (unsigned char *) NULL;
	static unsigned int	tmplen = 0;

	dep = (AVSInfo *) ras->dep;

	/* Swap bytes if necessary. */

	if (*(char *) &swaptest)
		_swapshort((char *) dep, sizeof(AVSInfo));

	nb = write(ras->fd, (char *) ras->dep, sizeof(AVSInfo));
	if (nb != sizeof(AVSInfo)) return(RAS_EOF);

	/* Massage the RGB structure into the AVS ARGB form */

	if (tmpbuf == (unsigned char *) NULL) {
		tmplen = ras->nx * ras->ny * 4;
		tmpbuf = (unsigned char *) calloc( (unsigned) tmplen, 1);
		if (tmpbuf == (unsigned char *) NULL) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return(RAS_ERROR);
		}
	}

	ptmp = tmpbuf;

	for(y=0; y<ras->ny; y++) {
	for(x=0; x<ras->nx; x++) {
		ptmp[0] = 0;
		ptmp[1] = DIRECT_RED(ras, x, y);
		ptmp[2] = DIRECT_GREEN(ras, x, y);
		ptmp[3] = DIRECT_BLUE(ras, x, y);
		ptmp += 4;
	}}

	nb = write(ras->fd, (char *) tmpbuf, (int) tmplen);
	if (nb != tmplen) return(RAS_EOF);

	return(RAS_OK);
}

int
AVSPrintInfo(ras)
	Raster		*ras;
{
	if (ras->text != (char *) NULL) {
		(void) fprintf(stderr, "AVS Rasterfile Information\n");
		(void) fprintf(stderr, "--------------------------\n");
		(void) fprintf(stderr, "text: %s\n", ras->text);
	}
	return(RAS_OK);
}

int
AVSClose(ras)
	Raster	*ras;
{
	if (ras->data != (unsigned char *) NULL) {
		(void) free( (char *) ras->data);
	}
	if (ras->dep != (char *) NULL) {
		(void) free( (char *) ras->dep);
	}

	if (ras->type == RAS_INDEXED) {
		(void) free( (char *) ras->red);
		(void) free( (char *) ras->green);
		(void) free( (char *) ras->blue);
	}
	return(RAS_OK);
}

int
AVSSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = AVSOpen;
	ras->OpenWrite = AVSOpenWrite;
	ras->Read      = AVSRead;
	ras->Write     = AVSWrite;
	ras->Close     = AVSClose;
	ras->PrintInfo = AVSPrintInfo;
	ras->ImageCount= ImageCount_;
	return(RAS_OK);
}
