/*
 *	$Id: nrif.c,v 1.21 2008-07-27 03:18:46 haley Exp $
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
/*	File:	nrif.c
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
 *		basic file access functions for NRIF (NCAR
 *		Raster Interchange Format) files.
 *
 *		All pixels, colormap values, and run lengths must be 8 bit.
 *
 *		Encoding schemes supported include:
 *			* Indexed Color
 *			* Indexed Color Run-length Encoded
 *			* Direct Color
 *			* Direct Color Run-length Encoded
 *		
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include "ncarg_ras.h"
#include "options.h"
#include "nrif.h"

/*LINTLIBRARY*/

static char	*FormatName = "nrif";

static int	_NrifReadIndexed();
static int	_NrifReadIndexedRLE();
static int	_NrifReadDirect();
static int	_NrifReadDirectRLE();
static int	_NrifWriteHeader();
static int	_NrifWriteIndexed();
static int	_NrifWriteIndexedRLE();
static int	_NrifWriteDirect();
static int	_NrifWriteDirectRLE();
static int	char_encode();
static int	char_decode();
static int	read_decode();

static unsigned char	buf[NRIF_HEADER_SIZE]; /* for unpacking NRIF headers. */

static unsigned char	*tmpbuf		= (unsigned char *) NULL;
static int		tmpbuf_size	= 0;

Raster *
NrifOpen(name)
	char	*name;
{
	char		*errmsg = "NrifOpen(\"%s\")";
	Raster		*ras;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(NrifInfo),1);
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

	NrifSetFunctions(ras);

	return(ras);
}

int
NrifRead(ras)
	Raster	*ras;
{
	char			*errmsg = "NrifRead(\"%s\")";
	NrifInfo		*dep;
	int			status;

	dep = (NrifInfo *) ras->dep;

	status = fread((char *) buf, 1, 4, ras->fp);
	if (status == 0) {
		(void) ESprintf(RAS_E_NOT_IN_CORRECT_FORMAT, errmsg, ras->name);
		return(RAS_EOF);
	}
	else if (status != 4) {
		(void) ESprintf(RAS_E_NOT_IN_CORRECT_FORMAT, errmsg, ras->name);
		return(RAS_ERROR);
	}

	if (strncmp( (char *) buf, NRIF_MAGIC, 4)) {
		dep->objtype = char_decode(&buf[0], 2);
		dep->control = char_decode(&buf[2], 2);
		dep->recsize = read_decode((int) ras->fp, 4);
		if (dep->recsize == -1) return(RAS_EOF);

		status = fread((char *) buf, 1, 4, ras->fp);
		if (status != 4) return(RAS_EOF);
		if (strncmp( (char *) buf, NRIF_MAGIC, 4)) {
			(void) ESprintf(E_UNKNOWN,
			"NrifOpen(\"%s\") - File is not in NRIF format",
			ras->name);
			return(RAS_ERROR);
		}
		else {
			(void) strncpy(dep->magic, (char *) buf, 4);
		}

		dep->encapsulated = True;
	}
	else {
		dep->encapsulated = False;
	}

	status = fread( (char *)buf, 1, 32, ras->fp);
	if (status != 32) return(RAS_EOF);

	dep->flags	= char_decode(&buf[0], 4);
	dep->nx		= char_decode(&buf[4], 4);
	dep->ny		= char_decode(&buf[8], 4);
	dep->cmtlen	= char_decode(&buf[12], 4);
	dep->device	= char_decode(&buf[16], 4);
	dep->devlen	= char_decode(&buf[20], 4);
	dep->encoding	= char_decode(&buf[24], 4);
	dep->enclen	= char_decode(&buf[28], 4);

	/* Comment Field */

	if (dep->cmtlen > 0) {
		ras->text = ras_calloc(dep->cmtlen, 1);
		status = fread((char *)ras->text, 1, (int)dep->cmtlen, ras->fp);
		if (status != dep->cmtlen) return(RAS_EOF);
	}
	else {
		ras->text = (char *) NULL;
	}

	/* Device Information (nobody cares) */

	if (dep->devlen > 0) {
		dep->device_info = ras_calloc(dep->devlen, 1);
		status = fread( (char *) dep->device_info, 1,
			 (int) dep->devlen,ras->fp);
		if (status != dep->devlen) return(RAS_EOF);
	}
	else {
		dep->device_info = (char *) NULL;
	}

	/* Check the VPLOT flag. */

	dep->vplot = (dep->flags & 0x02 ) >> 1;

	/* Initialize per-file information on first read. */

	if (ras->read == False) {
		ras->read      = True;
		ras->file_nx   = dep->nx;
		ras->file_ny   = dep->ny;
		switch(dep->encoding) {
			case NRIF_INDEXED:
			case NRIF_INDEXED_RLE:
				ras->file_type = RAS_INDEXED;
				break;

			case NRIF_DIRECT:
			case NRIF_DIRECT_RLE:
				ras->file_type = RAS_DIRECT;
				break;
			
			default:
				ras->file_type = RAS_UNKNOWN;
		}
	}

	/* Make sure that resolution has not changed between frames.  */

	if (dep->nx != ras->file_nx || dep->ny != ras->file_ny) {
		(void) ESprintf(RAS_E_IMAGE_SIZE_CHANGED, errmsg, ras->name);
		return(RAS_ERROR);
	}

	/* Make sure that resolution has not changed between frames.  */

	switch(dep->encoding) {
		case NRIF_INDEXED:
		case NRIF_INDEXED_RLE:
			if (ras->file_type != RAS_INDEXED) {
				(void) ESprintf(RAS_E_IMAGE_TYPE_CHANGED,
						errmsg, ras->name);
				return(RAS_ERROR);
			}
			break;

		case NRIF_DIRECT:
		case NRIF_DIRECT_RLE:
			if (ras->file_type != RAS_DIRECT) {
				(void) ESprintf(RAS_E_IMAGE_TYPE_CHANGED,
						errmsg, ras->name);
				return(RAS_ERROR);
			}
			break;
	}

	/* Process each encoding scheme. */

	switch(dep->encoding) {
		case NRIF_INDEXED:
			status = _NrifReadIndexed(ras);
			break;

		case NRIF_INDEXED_RLE:
			status = _NrifReadIndexedRLE(ras);
			break;

		case NRIF_DIRECT:
			status = _NrifReadDirect(ras);
			break;

		case NRIF_DIRECT_RLE:
			status = _NrifReadDirectRLE(ras);
			break;

		default:
		  (void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
			"NrifRead(\"%s\") - %s",
			ras->name, nrif_types[dep->encoding]);
		  return(RAS_ERROR);
	}

	if (status != RAS_OK) return(status);

	if (dep->vplot) {
		/* Perform an in-place image inversion. */
		status = RasterInvert(ras, ras);
	}

	return(status);
}

static int
_NrifReadIndexed(ras)
	Raster		*ras;
{
	int		status;
	char		*errmsg = "_NrifReadIndexed(\"%s\")";
	char		dummy[RAS_DEFAULT_NCOLORS];
	NrifInfo	*dep;

	dep = (NrifInfo *) ras->dep;

	/* Read the NRIF encoding information. */
	status = fread( (char *)buf, 1, 12, ras->fp);
	if (status != 12) return(RAS_EOF);

	dep->ibits  = char_decode(&buf[0], 4);
	dep->ncolor = char_decode(&buf[4], 4);
	dep->cbits  = char_decode(&buf[8], 4);

	if (dep->ibits != 8) {
		(void) ESprintf(RAS_E_8BIT_INTENSITIES_ONLY, errmsg, ras->name);
		return(RAS_ERROR);
	}

	if (dep->cbits != 8) {
		(void) ESprintf(RAS_E_8BIT_PIXELS_ONLY, errmsg, ras->name);
		return(RAS_ERROR);
	}

	/* Add new information to the raster structure */

	ras->nx = dep->nx;
	ras->ny = dep->ny;
	ras->type = RAS_INDEXED;
	ras->ncolor = dep->ncolor;
	ras->length = ras->nx * ras->ny;

	/* Allocate space for color tables and image. */
	
	if (ras->data == (unsigned char *) NULL) {
		ras->red  =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->green=(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->blue =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->data =(unsigned char *)ras_calloc((unsigned)ras->length,1);
	}

	/* Read color table and image frame. */

	if (ras->map_forced == False) {
		status=fread((char *)ras->red,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);

		status=fread((char *)ras->green,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);

		status=fread((char *)ras->blue,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);
	}
	else {
		status=fread((char *)dummy,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);

		status=fread((char *)dummy,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);

		status=fread((char *)dummy,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);
	}

	status = fread((char *)ras->data, 1, ras->length, ras->fp);
	if (status != ras->length) return(RAS_EOF);

	return(RAS_OK);
}

static int
_NrifReadIndexedRLE(ras)
	Raster		*ras;
{
	int		status;
	char		*errmsg = "_NrifReadIndexedRLE(\"%s\")";
	char		dummy[RAS_DEFAULT_NCOLORS];
	int		x, y, length;
	NrifInfo	*dep;
	unsigned char	*cptr;

	dep = (NrifInfo *) ras->dep;

	/* Read the NRIF encoding information. */
	status = fread( (char *)buf, 1, 16, ras->fp);
	if (status != 16) return(RAS_EOF);

	dep->ibits  = char_decode(&buf[0], 4);
	dep->ncolor = char_decode(&buf[4], 4);
	dep->cbits  = char_decode(&buf[8], 4);
	dep->rbits  = char_decode(&buf[12], 4);

	if (dep->ibits != 8) {
		(void) ESprintf(RAS_E_8BIT_INTENSITIES_ONLY, errmsg, ras->name);
		return(RAS_ERROR);
	}

	if (dep->cbits != 8) {
		(void) ESprintf(RAS_E_8BIT_PIXELS_ONLY, errmsg, ras->name);
		return(RAS_ERROR);
	}

	if (dep->rbits != 8) {
		(void) ESprintf(RAS_E_8BIT_RUNLENGTHS_ONLY, errmsg, ras->name);
		return(RAS_ERROR);
	}

	/* Add new information to the raster structure */

	ras->nx     = dep->nx;
	ras->ny     = dep->ny;
	ras->type   = RAS_INDEXED;
	ras->ncolor = dep->ncolor;
	ras->length = ras->nx * ras->ny;

	/* Allocate space for color tables and image. */
	
	if (ras->data == (unsigned char *) NULL) {
		ras->red = (unsigned char *)ras_calloc((unsigned) ras->ncolor,1);
		ras->green =(unsigned char *)ras_calloc((unsigned) ras->ncolor,1);
		ras->blue = (unsigned char *)ras_calloc((unsigned) ras->ncolor,1);
		ras->data = (unsigned char *)ras_calloc((unsigned) ras->length,1);
	}

	/* Read color table and image frame. */

	if (ras->map_forced == False) {
		status=fread((char *)ras->red,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);

		status=fread((char *)ras->green,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);

		status=fread((char *)ras->blue,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);
	}
	else {
		status=fread((char *)dummy,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);

		status=fread((char *)dummy,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);

		status=fread((char *)dummy,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) return(RAS_EOF);
	}

	for(length=0, y=0, cptr=ras->data; y<ras->ny; y++)
	for(x=0; x<ras->nx; x++) {
		if (length == 0) {
		status = fread((char *)buf, 1, 2, ras->fp);
			if (status != 2) return(RAS_EOF);
			length = buf[0];
		}
#ifdef	DEAD
		INDEXED_PIXEL(ras, x, y) = buf[1];
#endif
		*cptr++ = buf[1];
		length--;
	}

	return(RAS_OK);
}

static int
_NrifReadDirect(ras)
	Raster		*ras;
{
	int		status;
	char		*errmsg = "_NrifReadDirect(\"%s\")";
	NrifInfo	*dep;

	dep = (NrifInfo *) ras->dep;

	dep->cbits = read_decode((int) ras->fp, 4);
	if (dep->cbits == RAS_EOF) return(RAS_EOF);

	if (dep->cbits != 8) {
		(void) ESprintf(RAS_E_8BIT_PIXELS_ONLY, errmsg, ras->name);
		return(RAS_ERROR);
	}

	ras->nx = dep->nx;
	ras->ny = dep->ny;
	ras->type = RAS_DIRECT;
	ras->ncolor = 256*256*256;
	ras->length = ras->nx * ras->ny * 3;

	if (ras->data == (unsigned char *) NULL) {
		ras->data = (unsigned char *) ras_calloc((unsigned)ras->length,1);
	}
	
	status = fread((char *)ras->data, 1, ras->length, ras->fp);
	if (status != ras->length) return(RAS_EOF);

	return(RAS_OK);
}

static int
_NrifReadDirectRLE(ras)
	Raster		*ras;
{
	int		status;
	char		*errmsg = "_NrifReadDirectRLE(\"%s\")";
	int		x, y, length;
	NrifInfo	*dep;
	unsigned char	*cptr;

	dep = (NrifInfo *) ras->dep;

	dep->cbits = read_decode((int) ras->fp, 4);
	if (dep->cbits == -1) return(RAS_EOF);

	if (dep->cbits != 8) {
		(void) ESprintf(RAS_E_8BIT_PIXELS_ONLY, errmsg, ras->name);
		return(RAS_ERROR);
	}

	dep->rbits = read_decode((int) ras->fp, 4);
	if (dep->rbits == -1) return(RAS_EOF);

	if (dep->rbits != 8) {
		(void) ESprintf(RAS_E_8BIT_RUNLENGTHS_ONLY, errmsg, ras->name);
		return(RAS_ERROR);
	}

	ras->nx		= dep->nx;
	ras->ny		= dep->ny;
	ras->type	= RAS_DIRECT;
	ras->ncolor	= 256*256*256;
	ras->length	= ras->nx * ras->ny * 3;

	if (ras->data == (unsigned char *) NULL) {
		ras->data = (unsigned char *)ras_calloc ((unsigned) ras->length, 1);
	}

	for(length=0, y=0, cptr=ras->data; y<ras->ny; y++)
	for(x=0; x<ras->nx; x++) {
		if (length == 0) {
		status = fread((char *)buf, 1, 4, ras->fp);
			if (status != 4) return(RAS_EOF);
			length = buf[0];
		}
#ifdef	DEAD
/*
 * this code is too slow :-(
 */
		DIRECT_RED(ras,x,y)	= buf[1];
		DIRECT_GREEN(ras,x,y)	= buf[2];
		DIRECT_BLUE(ras,x,y)	= buf[3];
#endif
		*cptr++ = buf[1];
		*cptr++ = buf[2];
		*cptr++ = buf[3];
		length--;
	}

	return(RAS_OK);
}

Raster *
NrifOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	char		*errmsg = "NrifOpenWrite(\"%s\")";
	Raster		*ras;
	NrifInfo	*dep;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(NrifInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	dep = (NrifInfo *) ras->dep;

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
		ras->text = (char *) ras_calloc((unsigned) (strlen(comment)+1),1);
		(void) strcpy(ras->text, comment);
	}
	else {
		ras->text = (char *) NULL;
	}

	switch (encoding) {
		case RAS_INDEXED:
			if (OptionCompression == RAS_COMPRESS_OFF) {
				dep->encoding = NRIF_INDEXED;
			}
			else {
				dep->encoding = NRIF_INDEXED_RLE;
			}
			ras->nx		= nx;
			ras->ny		= ny;
			ras->length	= nx * ny;
			ras->ncolor	= RAS_DEFAULT_NCOLORS;
			ras->type	= RAS_INDEXED;
			ras->red	= (unsigned char *) 
					  ras_calloc((unsigned) ras->ncolor, 1);
			ras->green	= (unsigned char *) 
					  ras_calloc((unsigned) ras->ncolor, 1);
			ras->blue	= (unsigned char *) 
					  ras_calloc((unsigned) ras->ncolor, 1);
			ras->data	= (unsigned char *) 
					  ras_calloc((unsigned) ras->length, 1);

			break;
		
		case RAS_DIRECT:
			if (OptionCompression == RAS_COMPRESS_OFF) {
				dep->encoding = NRIF_DIRECT;
			}
			else {
				dep->encoding = NRIF_DIRECT_RLE;
			}
			ras->nx		= nx;
			ras->ny		= ny;
			ras->type	= RAS_DIRECT;
			ras->red	= (unsigned char *) NULL;
			ras->blue	= (unsigned char *) NULL;
			ras->green	= (unsigned char *) NULL;
			ras->ncolor	= 256*256*256;
			ras->length	= nx * ny * 3;

			ras->data = (unsigned char *) 
				ras_calloc ((unsigned) ras->length, 1);

			break;
		
		default:
			(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,errmsg,name);
			return( (Raster *) NULL );
	}

	NrifSetFunctions(ras);

	return(ras);
}

int
NrifClose(ras)
	Raster	*ras;
{
	int	status;

	status = GenericClose(ras);
	return(status);
}

int
NrifWrite(ras)
	Raster	*ras;
{
	char			*errmsg = "NrifWrite(\"%s\")";
	NrifInfo		*dep;
	int			status;

	dep = (NrifInfo *) ras->dep;

	switch(dep->encoding) {
		case NRIF_INDEXED:
			status = _NrifWriteIndexed(ras);
			break;

		case NRIF_INDEXED_RLE:
			status = _NrifWriteIndexedRLE(ras);
			break;

		case NRIF_DIRECT:
			status = _NrifWriteDirect(ras);
			break;

		case NRIF_DIRECT_RLE:
			status = _NrifWriteDirectRLE(ras);
			break;
		
		default:
			(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
					errmsg, ras->name);
			status = RAS_OK;
	}

	return(status);
}

static int
_NrifWriteHeader(ras)
	Raster		*ras;
{
	NrifInfo	*dep;
	int		status;

	dep = (NrifInfo *) ras->dep;

	/* If it's an encapsulated file, write out the encap header. */

	if (dep->encapsulated == True) {
		dep->objtype = 0;
		dep->control = NRIF_CONTROL;
		dep->recsize = 8 + NRIF_HEADER_SIZE +
				dep->enclen +
				dep->devlen +
				dep->cmtlen +
				ras->length;
		(void) char_encode((int) dep->objtype, &buf[0], 2);
		(void) char_encode((int) dep->control, &buf[2], 2);
		(void) char_encode((int) dep->recsize, &buf[4], 4);
		status = write(ras->fd, buf, 8);
		if (status != 8) return(RAS_EOF);
	}

	/* Write the NRIF header.  */

	(void) strncpy((char *) buf, NRIF_MAGIC, 4);
	(void) char_encode((int) dep->flags, &buf[4], 4);
	(void) char_encode((int) dep->nx, &buf[8], 4);
	(void) char_encode((int) dep->ny, &buf[12], 4);
	(void) char_encode((int) dep->cmtlen, &buf[16], 4);
	(void) char_encode((int) dep->device, &buf[20], 4);
	(void) char_encode((int) dep->devlen, &buf[24], 4);
	(void) char_encode((int) dep->encoding, &buf[28], 4);
	(void) char_encode((int) dep->enclen, &buf[32], 4);
	status = write(ras->fd, (char *) buf, NRIF_HEADER_SIZE);
	if (status != NRIF_HEADER_SIZE) return(RAS_EOF);


	/* Write the comment field. */

	if (dep->cmtlen > 0) {
		status = write(ras->fd, dep->comment, (int) dep->cmtlen);
		if (status != dep->cmtlen) return(RAS_EOF);
	}

	/* Device Information (nobody cares) */

	if (dep->devlen > 0) {
		status = write(ras->fd, dep->device_info, (int) dep->devlen);
		if (status != dep->devlen) return(RAS_EOF);
	}

	return(RAS_OK);
}

static int
_NrifWriteIndexed(ras)
	Raster		*ras;
{
	NrifInfo	*dep;
	int		status;

	dep = (NrifInfo *) ras->dep;

	(void) strncpy(dep->magic, NRIF_MAGIC, 4);
	dep->encapsulated = False;
	dep->vplot	= False;
	dep->nx		= ras->nx;
	dep->ny		= ras->ny;
	dep->encoding	= NRIF_INDEXED;
	dep->ibits	= 8;
	dep->ncolor	= RAS_DEFAULT_NCOLORS;
	dep->cbits	= 8;
	dep->enclen	= 12 + dep->ncolor * 3;

	status = _NrifWriteHeader(ras);
	if (status != RAS_OK) return(status);

	(void) char_encode((int) dep->ibits, &buf[0], 4);
	(void) char_encode((int) dep->ncolor, &buf[4], 4);
	(void) char_encode((int) dep->cbits, &buf[8], 4);
	status = write(ras->fd, (char *) buf, 12);
	if (status != 12) return(RAS_EOF);

	status = write(ras->fd,(char *) ras->red,ras->ncolor);
	if (status != ras->ncolor) return(RAS_EOF);
	status = write(ras->fd,(char *) ras->green,ras->ncolor);
	if (status != ras->ncolor) return(RAS_EOF);
	status = write(ras->fd,(char *) ras->blue,ras->ncolor);
	if (status != ras->ncolor) return(RAS_EOF);

	status = write(ras->fd, (char *) ras->data,ras->length);
	if (status != ras->length) return(RAS_EOF);

	return(RAS_OK);
}

static int
_NrifWriteIndexedRLE(ras)
	Raster		*ras;
{
	char		*errmsg = "_NrifWriteIndexedRLE(\"%s\")";
	NrifInfo	*dep;
	int		status;
	int		x, y, runx, length;
	int		image_length = 0;
	unsigned char	*cptr1, *cptr2;

	dep = (NrifInfo *) ras->dep;

	(void) strncpy(dep->magic, NRIF_MAGIC, 4);
	dep->encapsulated = False;
	dep->vplot	= False;
	dep->nx		= ras->nx;
	dep->ny		= ras->ny;
	dep->encoding	= NRIF_INDEXED_RLE;
	dep->ibits	= 8;
	dep->ncolor	= RAS_DEFAULT_NCOLORS;
	dep->cbits	= 8;
	dep->rbits	= 8;
	dep->enclen	= 16 + dep->ncolor * 3;

	status = _NrifTmpbuf(ras->length);
	if (status != RAS_OK) {
		(void) ESprintf(errno, errmsg, ras->name);
		return(RAS_ERROR);
	}
	
	for(y=0, cptr1=ras->data; y<ras->ny; y++) {
	for(x=0; x<ras->nx; x+=length, cptr1+=length) {

		for(
			runx=x, length=0, cptr2=cptr1; 
			length<255 && runx<ras->nx;
			runx++, length++, cptr2++) 
		{
			/*
			(void) fprintf(stderr,
			"Comparing x=%d to runx=%d (len=%d)\n",x,runx,length);
			*/
			if (*cptr1 != *cptr2) break;
		}

		/*
		If the compressed image becomes longer than
		what the uncompressed would be, bail.
		*/

		if (image_length > tmpbuf_size - 2) {
		(void) fprintf(stderr,
		"Note: %s was written uncompressed, which was more efficient\n",
		ras->name);
		status = _NrifWriteIndexed(ras);
		return(status);
		}

		tmpbuf[image_length++] = length;
		tmpbuf[image_length++] = *cptr1;
	}}

	/* Write the header. */

	status = _NrifWriteHeader(ras);
	if (status != RAS_OK) return(status);

	/* Write the encoding information. */

	(void) char_encode((int) dep->ibits,  &buf[0], 4);
	(void) char_encode((int) dep->ncolor, &buf[4], 4);
	(void) char_encode((int) dep->cbits,  &buf[8], 4);
	(void) char_encode((int) dep->rbits,  &buf[12], 4);
	status = write(ras->fd, (char *) buf, 16);
	if (status != 16) return(RAS_EOF);

	/* Write the color table. */

	status = write(ras->fd,(char *) ras->red,ras->ncolor);
	if (status != ras->ncolor) return(RAS_EOF);
	status = write(ras->fd,(char *) ras->green,ras->ncolor);
	if (status != ras->ncolor) return(RAS_EOF);
	status = write(ras->fd,(char *) ras->blue,ras->ncolor);
	if (status != ras->ncolor) return(RAS_EOF);

	/* Write the image. */
	status = write(ras->fd, (char *) tmpbuf, image_length);
	if (status != image_length) {
		(void) ESprintf(errno, errmsg, ras->name);
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

static int
_NrifWriteDirect(ras)
	Raster		*ras;
{
	NrifInfo	*dep;
	int		status;

	dep = (NrifInfo *) ras->dep;

	(void) strncpy(dep->magic, NRIF_MAGIC, 4);
	dep->encapsulated = False;
	dep->vplot	= False;
	dep->nx		= ras->nx;
	dep->ny		= ras->ny;
	dep->encoding	= NRIF_DIRECT;
	dep->enclen	= 4;
	dep->cbits	= 8;

	status = _NrifWriteHeader(ras);
	if (status != RAS_OK) return(status);

	(void) char_encode((int) dep->cbits, buf, 4);
	status = write(ras->fd, (char *) buf, 4);
	if (status != 4) return(RAS_EOF);

	status = write(ras->fd, (char *) ras->data,ras->length);
	if (status != ras->length) return(RAS_EOF);

	return(RAS_OK);
}

static int
_NrifWriteDirectRLE(ras)
	Raster		*ras;
{
	char		*errmsg = "_NrifWriteDirectRLE(\"%s\")";
	NrifInfo	*dep;
	int		status;
	int		x, y, runx, length;
	int		image_length = 0;
	unsigned char	*cptr1, *cptr2;

	dep = (NrifInfo *) ras->dep;

	(void) strncpy(dep->magic, NRIF_MAGIC, 4);
	dep->encapsulated = False;
	dep->vplot	= False;
	dep->nx		= ras->nx;
	dep->ny		= ras->ny;
	dep->encoding	= NRIF_DIRECT_RLE;
	dep->enclen	= 8;
	dep->cbits	= 8;
	dep->rbits	= 8;

	status = _NrifTmpbuf(ras->length);
	if (status != RAS_OK) {
		(void) ESprintf(errno, errmsg, ras->name);
		return(RAS_ERROR);
	}
	
	for(y=0, cptr1=ras->data; y<ras->ny; y++) {
	for(x=0; x<ras->nx; x+=length, cptr1+=(length*3)) {
		for(
			runx=x+1,length=1,cptr2=cptr1+3;
			length<255&&runx<ras->nx;
			runx++,length++,cptr2+=3) {

			/*
			(void) fprintf(stderr,
			"Comparing x=%d to runx=%d (len=%d)\n",x,runx,length);
			*/
			if (
				cptr1[0] != cptr2[0] || /* compare reds */
				cptr1[1] != cptr2[1] ||	/* compare greens */
				cptr1[2] != cptr2[2]	/* compare blues */
			) break;
		}

		/*
		If the compressed image becomes longer than
		what the uncompressed would be, bail.
		*/

		if (image_length > (tmpbuf_size - 4)) {
		(void) fprintf(stderr,
		"Note: %s was written uncompressed, which was more efficient\n",
		ras->name);
		status = _NrifWriteDirect(ras);
		return(status);
		}

		tmpbuf[image_length++] = length;
		tmpbuf[image_length++] = cptr1[0];
		tmpbuf[image_length++] = cptr1[1];
		tmpbuf[image_length++] = cptr1[2];
	}}

	/* Write the header. */

	status = _NrifWriteHeader(ras);
	if (status != RAS_OK) return(status);

	/* Write the encoding information. */

	(void) char_encode((int) dep->cbits, &buf[0], 4);
	(void) char_encode((int) dep->rbits, &buf[4], 4);
	status = write(ras->fd, (char *) buf, 8);
	if (status != 8) return(RAS_EOF);

	/* Write the image. */

	status = write(ras->fd, (char *) tmpbuf, image_length);
	if (status != image_length) {
		(void) ESprintf(errno, errmsg, ras->name);
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

static int
read_decode(fp, nbytes)
	FILE	*fp;
	int	nbytes;
{
	int		status;

	if (nbytes > 4) {
		(void) ESprintf(RAS_E_INTERNAL, "read_decode(fp, %s)", nbytes);
		return(RAS_ERROR);
	}

	status  = fread( (char *) buf, 1, nbytes, fp);
	if (status != nbytes) {
		(void) ESprintf(RAS_E_PREMATURE_EOF,
			"read_decode(fp,%d)", nbytes);
		return(RAS_ERROR);
	}

	return(char_decode(buf, nbytes));
}

static int
char_decode(buf, nbytes)
	unsigned char	*buf;
	int		nbytes;
{
	int		i;
	unsigned int	result = 0;

	for(i=0; i<nbytes; i++)
		result = (result << 8) | buf[i];
	
	return(result);
}

static int
char_encode(value, buf, nbytes)
	int		value;
	unsigned char	*buf;
	int		nbytes;
{
	switch (nbytes) {
		case 2:
			buf[1] = (value & 0x000000ff) >> 0;
			buf[0] = (value & 0x0000ff00) >> 8;
			break;

		case 4:
			buf[3] = (value & 0x000000ff) >> 0;
			buf[2] = (value & 0x0000ff00) >> 8;
			buf[1] = (value & 0x00ff0000) >> 16;
			buf[0] = (value & (unsigned) 0xff000000) >> 24;
			break;
		
		default:
			(void) ESprintf(RAS_E_INTERNAL,
				"char_encode(%d,,%d)", value, nbytes);
			return(RAS_ERROR);
	}
	return(RAS_OK);
}

NrifPrintInfo(ras)
	Raster	*ras;
{
	NrifInfo	*dep;

	dep = (NrifInfo *) ras->dep;


	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "NRIF Rasterfile Information\n");
	(void) fprintf(stderr, "---------------------------\n");
	(void) fprintf(stderr, "ENCODING:      %s\n",nrif_types[dep->encoding]);
	(void) fprintf(stderr, "ENCAPSULATED:  %d\n", dep->encapsulated);
	(void) fprintf(stderr, "FLAGS:         %x\n", dep->flags);
	(void) fprintf(stderr, "VPLOT:         %d\n", dep->vplot);
	(void) fprintf(stderr, "NCOLOR:        %d\n", dep->ncolor);
	(void) fprintf(stderr, "IBITS:         %d\n", dep->ibits);
	(void) fprintf(stderr, "CBITS          %d\n", dep->cbits);
	(void) fprintf(stderr, "RBITS:         %d\n", dep->rbits);
	(void) fprintf(stderr, "PBITS:         %d\n", dep->pbits);
	(void) fprintf(stderr, "FCOLRED:       %d\n", dep->fcolred);
	(void) fprintf(stderr, "FCOLGRN:       %d\n", dep->fcolgrn);
	(void) fprintf(stderr, "FCOLBLU:       %d\n", dep->fcolblu);
	(void) fprintf(stderr, "BCOLRED:       %d\n", dep->bcolred);
	(void) fprintf(stderr, "BCOLGRN:       %d\n", dep->bcolgrn);
	(void) fprintf(stderr, "BCOLBLU:       %d\n", dep->bcolblu);
	
	return(RAS_OK);
}

int
NrifSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = NrifOpen;
	ras->OpenWrite = NrifOpenWrite;
	ras->Read      = NrifRead;
	ras->Write     = NrifWrite;
	ras->Close     = NrifClose;
	ras->PrintInfo = NrifPrintInfo;
	ras->ImageCount = ImageCount_;
}

int
_NrifTmpbuf(size)
	int	size;
{
	unsigned char	*p;

	if (tmpbuf_size >= size) {
		return(RAS_OK);
	}
	else {
		if (tmpbuf == (unsigned char *) NULL) {
			p = (unsigned char *) ras_malloc(size);
		}
		else {
			p = (unsigned char *) realloc(tmpbuf, size);
		}
		if (p == (unsigned char *) NULL) return(RAS_ERROR);
		tmpbuf		= p;
		tmpbuf_size	= size;
	}
	return(RAS_OK);
}
