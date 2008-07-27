/* 
 * $Id: avsraster.c,v 1.15 2008-07-27 03:18:45 haley Exp $
 */
/************************************************************************
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
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include "ncarg_ras.h"
#include "avsraster.h"

static char	*FormatName = "avs";

Raster *
AVSOpen(name)
	char	*name;
{
	Raster	*ras;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "AVSOpen(%s)", name);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "AVSOpen(%s)", name);
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, "AVSOpen(%s)", name);
			return( (Raster *) NULL );
		}
	}

	ras->dep = (char *) NULL;

	ras->name = (char *) ras_calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *)ras_calloc((unsigned)(strlen(FormatName)+1),1);
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
		ras->dep =  (char *) ras_calloc(sizeof(AVSInfo),1);
		if (ras->dep == (char *) NULL) {
			(void) ESprintf(errno, "");
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
	ras->nx    = dep->nx;
	ras->ny    = dep->ny;
	ras->ncolor = 256*256*256;
	ras->length = ras->nx * ras->ny * 3;

	/* If not initialized, allocate image memory */

	if (ras->data == (unsigned char *) NULL) {
		image_size = ras->nx * ras->ny * 3;

		ras->data = (unsigned char *)ras_calloc((unsigned)image_size,1);

		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "");
			return(RAS_ERROR);
		}
	}

	/* The ALPHA channel is discarded. */

	if (tmpbuf == (unsigned char *) NULL) {
		tmplen = ras->nx * ras->ny * 4;
		tmpbuf = (unsigned char *) ras_calloc( (unsigned) tmplen, 1);
		if (tmpbuf == (unsigned char *) NULL) {
			(void) ESprintf(errno, "");
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
	RasterEncoding	encoding;
{
	Raster		*ras;
	AVSInfo		*dep;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "AVSOpenWrite()");
		return( (Raster *) NULL );
	}

	if (encoding != RAS_DIRECT) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING, "AVSOpenWrite()");
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "");
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(AVSInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) ESprintf(errno, "");
		return( (Raster *) NULL );
	}

	dep = (AVSInfo *) ras->dep;

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
	}
	else {
		ras->fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "");
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) ras_calloc((unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned)(strlen(FormatName)+1), 1);
	(void) strcpy(ras->format, FormatName);

	if (comment != (char *) NULL) {
		ras->text = (char *)ras_calloc((unsigned)(strlen(comment)+1),1);
		(void) strcpy(ras->text, comment);
	}
	else {
		ras->text = (char *) NULL;
	}

	ras->nx		= nx;
	ras->ny		= ny;
	ras->length	= ras->nx * ras->ny * 3;
	ras->ncolor	= 256*256*256;
	ras->type	= RAS_DIRECT;
	ras->red	= (unsigned char *) NULL;
	ras->green	= (unsigned char *) NULL;
	ras->blue	= (unsigned char *) NULL;
	ras->data	= (unsigned char *) ras_calloc((unsigned)ras->length,1);

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
	static AVSInfo		*swapbuf = (AVSInfo *) NULL;
	int			nb;
	int			x, y;
	unsigned long		swaptest = 1;
	static unsigned char	*ptmp, *tmpbuf = (unsigned char *) NULL;
	static unsigned int	tmplen = 0;

	/* Swap bytes if necessary. */

	if (*(char *) &swaptest) {
		if (swapbuf == (AVSInfo *) NULL) {
			swapbuf = (AVSInfo *) ras_calloc(sizeof(AVSInfo), 1);
			if (swapbuf == (AVSInfo *) NULL) {
				(void) ESprintf(errno,
					"AVSWrite(\"%s\",...)\n",
					ras->name);
				return(RAS_ERROR);
			}
		}
		(void) memmove((Voidptr) swapbuf,(Voidptr)ras->dep,
				sizeof(AVSInfo));
		_swapshort((char *) swapbuf, sizeof(AVSInfo));
		dep = swapbuf;
	}
	else {
		dep = (AVSInfo *) ras->dep;
	}

	nb = write(ras->fd, (char *) dep, sizeof(AVSInfo));
	if (nb != sizeof(AVSInfo)) return(RAS_EOF);

	/* Massage the RGB structure into the AVS ARGB form */

	if (tmpbuf == (unsigned char *) NULL) {
		tmplen = ras->nx * ras->ny * 4;
		tmpbuf = (unsigned char *) ras_calloc( (unsigned) tmplen, 1);
		if (tmpbuf == (unsigned char *) NULL) {
			(void) ESprintf(errno, "");
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
	int	status;

	status = GenericClose(ras);
	return(status);
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
