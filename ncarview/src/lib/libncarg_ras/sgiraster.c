/*
 *	$Id: sgiraster.c,v 1.6 1993-05-11 18:49:26 haley Exp $
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
 *		This particular set of routines provides
 *		basic file access functions for SGI true color
 *		raster files. RLE compression is supported
 *		and both scanline interleaved and scanplane
 *		interleaved RLE files are handled properly.
 *		The driver structure is complicated by checks
 *		procedures taken to read the file sequentially.
 *		SGI RLE raster files contain a table of contents
 *		insure that the file can be read sequentially
 *		
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include "ncarg_ras.h"
#include "sgiraster.h"

static int	_SGIReadPixelInterleaved();
static int	_SGIReadScanplaneInterleaved();
static int	_SGIExpandRLE();
static int	_SGIExpand();

Raster *
SGIOpen(name)
	char	*name;
{
	Raster	*ras;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "SGIOpen(\"%s\")", name);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "SGIOpen(\"%s\")", name);
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, "SGIOpen(\"%s\")", name);
			return( (Raster *) NULL );
		}
	}

	/* Record the name of the file. */

	ras->name = (char *) ras_calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, name);

	/* Record the format. */

	ras->format=(char *)ras_calloc((unsigned)(strlen(SGI_FORMAT_NAME)+1),1);
	(void) strcpy(ras->format, SGI_FORMAT_NAME);

	SGISetFunctions(ras);

	return(ras);
}

int
SGIRead(ras)
	Raster	*ras;
{
	int		status;
	SGIInfo		*dep;
	int		hdr_length, length, tablesize;
	int		i, loc, y;
	unsigned char	*tmpbuf;
	unsigned long   swaptest = 1;
	SgiInterleaveType interleave = SGI_IL_UNKNOWN;

	/* Allocate the raster format dependent (header) structure. */

	if (ras->dep == (char *) NULL) {
		ras->dep =  (char *) ras_calloc(sizeof(SGIInfo),1);
		if (ras->dep == (char *) NULL) {
			(void) ESprintf(errno, "SGIRead()");
			return(RAS_ERROR);
		}
	}

	dep = (SGIInfo *) ras->dep;

	/*
	This is shameful, but reading in the pointers from
	disk screws up temp storage for multiple frame files.
	*/

	hdr_length = sizeof(SGIInfo) - 3 * sizeof(unsigned short *) -
		 2 * sizeof(long) - 2 * sizeof(unsigned long *);

	/* Read the SGI raster file header and swap bytes if necessary. */

	status = fread((char *) dep, 1, hdr_length, ras->fp);
	if (status == 0) {
		return(RAS_EOF);
	}
	else if (status != hdr_length) {
		(void) ESprintf(RAS_E_NOT_IN_CORRECT_FORMAT,
				"\"%s\"", ras->name);
		return(RAS_ERROR);
	}

	/* Swap those bytes. */
	if (*(char *) &swaptest) {
		_swaplong((char *) dep, sizeof(SGIInfo));
	}

	/* Make sure magic number is there. */
	if (dep->imagic != SGI_MAGIC) {
		(void) ESprintf(
			RAS_E_NOT_IN_CORRECT_FORMAT,
			"SGI rasterfile has bogus magic cookie!");
		return(RAS_ERROR);
	}

	/*
	Images with other than one byte per pixel storage
	(one byte for red, as opposed to two) are not supported.
	*/

	if (SGI_BPP(dep->type) != 1) {
		(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION,
			"SGIRead(\"%s\") - Two byte pixels not supported");
		return(RAS_ERROR);
	}

	/*
	We currently only support RGB images. An SGI_CM_NORMAL
	colormap parm can mean an RGB file if zsize == 3 and a grayscale
	file if zsize == 1.
	*/

	if (dep->colormap != SGI_CM_NORMAL || dep->zsize != 3) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
			"Must be SGI_CM_NORMAL with zsize == 3");
		return(RAS_ERROR);
	}

	/*
	If this is the first read, establish fixed parameters for the file.
	Otherwise, make sure fixed parameters stay the same.
	*/

	if (ras->read == False) {
		ras->read	= True;
		ras->written	= False;
		ras->file_nx	= dep->xsize;
		ras->file_ny	= dep->ysize;
		ras->file_type	= RAS_DIRECT;

		ras->nx		= dep->xsize;
		ras->ny		= dep->ysize;
		ras->type	= RAS_DIRECT;
		ras->length	= ras->nx * ras->ny * 3;
		ras->ncolor	= 256*256*256;

		/* Allocate image storage. */

		ras->data = (unsigned char *) ras_calloc(ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "SGIRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}

		/*
		Allocate memory for unpacking RLE lines, one color at a time.
		Funny thing (HAWHAW) - RLE lines are often longer than what
		the raw data would have been. Allocate three times the normal,
		just to be safe.
		*/

		dep->tmpbuf = (unsigned short *) ras_calloc(3*dep->xsize, 1);
		if (dep->tmpbuf == (unsigned short *) NULL) {
			(void) ESprintf(errno, "SGIRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}
	}
	else {
		if (ras->file_nx != dep->xsize || ras->file_ny != dep->ysize) {
			(void) ESprintf(RAS_E_IMAGE_SIZE_CHANGED,
				"SGIRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}
	}

	/* This area is used to unpack the RLE raw imagery. */
	tmpbuf = (unsigned char *) dep->tmpbuf;

	if( ((dep->imagic>>8) | ((dep->imagic&0xff)<<8)) == SGI_MAGIC) {
		(void) ESprintf(E_UNKNOWN,
		"SGIRead(\"%s\") - Byte-swapped (old) rasters not supported",
		ras->name);
		return(RAS_ERROR);
	}

	/*
	Move up in the file to where the table of contents
	for RLE imagery is stored.  We could be reading from
	stdin, so read, not seek, up to the tables. This can't
	be more than 512 bytes, and we'll just use the ras->data
	area for the moment.
	*/

	length = RAS_SGI_RESERVED - hdr_length;

	status = fread(ras->data, 1, length, ras->fp);
	if (status != length) {
		(void) ESprintf(errno, "SGIRead(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	/*
	RLE and non-RLE files can theoretically be mixed in
	a single file, as long as they all have the same dimensions.
	*/

	if (SGI_ISRLE(dep->type)) { /* We've got an RLE file. */

		/* Allocate memory for RLE tables. */
		tablesize = dep->ysize * dep->zsize * sizeof(long);
		dep->rowstart = (unsigned long *) ras_calloc(tablesize, 1);
		dep->rowsize  = (long *) ras_calloc(tablesize, 1);

		if (dep->rowstart == (unsigned long *) NULL ||
		    dep->rowsize  == (long *) NULL) {
			(void) ESprintf(errno, "SGIRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}

		/* Read the rowstart[] table. */

		status = fread(dep->rowstart, 1, tablesize, ras->fp);
		if (status != tablesize) {
			(void) ESprintf(errno,
				"SGIRead(\"%s\") - reading rowstart table",
				ras->name);
			return(RAS_ERROR);
		}

		/* Read the rowsize[] table. */

		status = fread(dep->rowsize, 1, tablesize, ras->fp);
		if (status != tablesize) {
			(void) ESprintf(errno,
				"SGIRead(\"%s\") - reading rowsize table",
				ras->name);
			return(RAS_ERROR);
		}

		/* Where are we at now in the file i.e. Is it packed? */

		loc = 512 + 2 * tablesize;

		if (loc != dep->rowstart[0]) {
			(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
				"SGIRead(\"%s\") - RLE image not packed",
				ras->name);
			return(RAS_ERROR);
		}

		/* Check for complete image packing. */

		if (dep->rowstart[0] + dep->rowsize[0] ==
		    dep->rowstart[dep->ysize]) {
			interleave = SGI_IL_PIXEL;
		}
		else if (dep->rowstart[0]+dep->rowsize[0] == dep->rowstart[1]){
			interleave = SGI_IL_SCANPLANE;
		}
		else {
			(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
				"SGIRead(\"%s\") - Weird interleaving!",
				ras->name);
		}

		switch(interleave) {
			case SGI_IL_PIXEL:
				status = _SGIReadPixelInterleaved(ras);
				break;

			case SGI_IL_SCANPLANE:
				status = _SGIReadScanplaneInterleaved(ras);
				break;
		}
	}
	else if (SGI_ISVERBATIM(dep->type)) { /* Got a verbatim file */
		/*
		Image is not RLE. SGI_VERBATIM images are stacked by
		scan-planes: R, G, then B. We use "tmpbuf" to unpack
		them.
		*/

		for(i=0; i<3; i++) {
		for(y=0; y<ras->ny; y++) {
			status = fread(tmpbuf, 1, ras->nx, ras->fp);
			if (status != ras->nx) {
				(void) ESprintf(errno, "SGIRead(\"%s\")");
				return(RAS_ERROR);
			}

			(void) _SGIExpand(tmpbuf,
					&DIRECT_RED(ras, 0, ras->ny-y-1)+i,
					ras->nx);
		}}
	}
	else {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING, "SGIRead(\"%s\")");
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

static int
_SGIReadPixelInterleaved(ras)
	Raster		*ras;
{
	SGIInfo		*dep;
	int		status;
	int		i, y;
	unsigned char	*tmpbuf;

	dep    = (SGIInfo *) ras->dep;
	tmpbuf = (unsigned char *) dep->tmpbuf;

	/*
	Red, green, and blue lines are interleaved for each row of the image.
	Even though there is a table of contents, we can only use it to
	determine the order of information so that the image can be
	read from stdin as a stream.
	*/

	for(y=0; y<ras->ny; y++) for(i=0; i<3; i++) {

		/* Read an RLE line and unpack it. */
		status = fread(dep->tmpbuf,1,dep->rowsize[y+i*ras->ny],ras->fp);

		if (status != dep->rowsize[y+i*ras->ny]) {
			(void) ESprintf(errno,
			"SGIRead(\"%s\") - Incomplete RLE line", ras->name);
			return(RAS_ERROR);
		}
		(void) _SGIExpandRLE(tmpbuf, &DIRECT_RED(ras,0,ras->ny-y-1)+i);
	}
	return(RAS_OK);
}

static int
_SGIReadScanplaneInterleaved(ras)
	Raster		*ras;
{
	SGIInfo		*dep;
	int		status;
	int		i, y;
	unsigned char	*tmpbuf;

	dep    = (SGIInfo *) ras->dep;
	tmpbuf = (unsigned char *) dep->tmpbuf;

	/*
	Red, green, and blue *planes* are interleaved for the image.
	Even though there is a table of contents, we can only use it to
	determine the order of information so that the image can be
	read from stdin as a stream.
	*/

	for(i=0; i<3; i++) for(y=0; y<ras->ny; y++) {

		/* Read an RLE line and unpack it. */
		status = fread(dep->tmpbuf,1,dep->rowsize[y+i*ras->ny],ras->fp);

		if (status != dep->rowsize[y+i*ras->ny]) {
			(void) ESprintf(errno,
			"SGIRead(\"%s\") - Incomplete RLE line", ras->name);
			return(RAS_ERROR);
		}

		(void) _SGIExpandRLE(tmpbuf, &DIRECT_RED(ras,0,ras->ny-y-1)+i);
	}

	return(RAS_OK);
}

static int
_SGIExpandRLE(iptr, optr)
	unsigned char	*iptr;
	unsigned char	*optr;
{
	unsigned char	pixel;
	int		count;
	int		verbatim;

	while(1) {
		pixel = *iptr++;
		count = pixel & 0x7f;
		if (count == 0) break;
		verbatim = pixel & 0x80;
		if (verbatim) {
			while(count--) {
				*optr = *iptr++;
				optr += 3;
			}
		}
		else {
			pixel = *iptr++;
			while(count--) {
				*optr = pixel;
				optr += 3;
			}
		}
	}

	return(RAS_OK);
}

static int
_SGIExpand(iptr, optr, nx)
	unsigned char	*iptr;
	unsigned char	*optr;
	int		nx;
{
	int		i;

	for(i=0; i<nx; i++) {
		*optr = *iptr++;
		optr += 3;
	}

	return(RAS_OK);
}

Raster *
SGIOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	char		*errmsg = "SGIOpenWrite(\"%s\")";
	Raster		*ras;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, errmsg, name);
		return( (Raster *) NULL );
	}

	if (encoding != RAS_DIRECT) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
		"Only DIRECT color is supported for SGI rasterfiles");
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(SGIInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) ESprintf(errno, "SGIOpenWrite(\"%s\")", name);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
	}
	else {
		ras->fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "SGIOpenWrite(\"%s\")", name);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) ras_calloc((unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, name);

	ras->format=(char *)ras_calloc((unsigned)(strlen(SGI_FORMAT_NAME)+1),1);
	(void) strcpy(ras->format, SGI_FORMAT_NAME);

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
	ras->ncolor	= 256 * 256 * 256;
	ras->type	= RAS_DIRECT;
	ras->data	=(unsigned char *)ras_calloc((unsigned)(ras->length),1);

	(void) SGISetFunctions(ras);

	return(ras);
}

int
SGIWrite(ras)
	Raster	*ras;
{
	char			*errmsg = "SGIWrite(\"%s\")";
	SGIInfo			*dep;
	int			x, y, i, nb;
	unsigned long		swaptest = 1;
	static unsigned char	*tmpbuf;
	static int		tmpbuf_size = 0;
	unsigned char		*iptr, *optr;

	dep = (SGIInfo *) ras->dep;

	if (tmpbuf == (unsigned char *) NULL) {
		if (ras->nx > RAS_SGI_RESERVED) {
			tmpbuf_size = ras->nx;
		}
		else {
			tmpbuf_size = RAS_SGI_RESERVED;
		}
		tmpbuf = (unsigned char *) calloc(tmpbuf_size, 1);
		if (tmpbuf == (unsigned char *) NULL) {
			(void) ESprintf(errno, errmsg, ras->name);
			return(RAS_ERROR);
		}
	}

	dep->imagic     = SGI_MAGIC;
	dep->type       = SGI_TYPE_VERBATIM | 1;
	dep->dim        = 3;
	dep->xsize      = ras->nx; 
	dep->ysize      = ras->ny;
	dep->zsize      = 3; /* RGB image. */
	dep->min        = 0;
	dep->max        = 255;
	dep->wastebytes = 0;
	dep->colormap   = SGI_CM_NORMAL;

	/* Write the header, swapping bytes if necessary. */

	if (*(char *) &swaptest)
		_swaplong((char *) dep, sizeof(SGIInfo));

	nb = write(ras->fd, (char *) ras->dep, sizeof(SGIInfo));
	if (nb != sizeof(SGIInfo)) return(RAS_EOF);

	ras->written = True;

	/* Write bytes remaining before actual image data. */

	nb = write(ras->fd, (char *)tmpbuf, (RAS_SGI_RESERVED-sizeof(SGIInfo)));
	if (nb != (RAS_SGI_RESERVED-sizeof(SGIInfo))) return(RAS_EOF);

	/*
	** SGI_VERBATIM rasterfiles are scan-plane interleaved,
	** so we write out the red plane, the green plane, and then
	** the blue plane. There is no compression done here.
	*/

	for(i=0; i<3; i++) {
		for(y=0; y<ras->ny; y++) {
			iptr = &DIRECT_RED(ras, 0, ras->ny-y-1)+i;
			optr = tmpbuf;
			for(x=0; x<ras->nx; x++) {
				*optr = *iptr;
				optr += 1;
				iptr += 3;
			}
			nb = write(ras->fd, (char *) tmpbuf, ras->nx);
			if (nb != ras->nx) return(RAS_EOF);
		}
	}

	return(RAS_OK);
}

int
SGIPrintInfo(ras)
	Raster		*ras;
{
	SGIInfo		*dep;

	dep = (SGIInfo *) ras->dep;

	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "SGI Rasterfile Information\n");
	(void) fprintf(stderr, "--------------------------\n");
	(void) fprintf(stderr, "imagic:           %8x\n", dep->imagic);
	(void) fprintf(stderr, "type:             %d\n", dep->type);
	(void) fprintf(stderr, "dim:              %d\n", dep->dim);
	(void) fprintf(stderr, "xsize:            %d\n", dep->xsize);
	(void) fprintf(stderr, "ysize:            %d\n", dep->ysize);
	(void) fprintf(stderr, "zsize:            %d\n", dep->zsize);
	(void) fprintf(stderr, "min:              %d\n", dep->min);
	(void) fprintf(stderr, "max:              %d\n", dep->max);
	(void) fprintf(stderr, "wastebytes:       %d\n", dep->wastebytes);
	(void) fprintf(stderr, "name:             %s\n", dep->name);

	return(RAS_OK);
}

int
SGIClose(ras)
	Raster	*ras;
{
	int		status;
	char		*errmsg = "SGIClose(\"%s\")";
	SGIInfo		*dep;

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
		if (ras->fd != fileno(stdin) && ras->fd != fileno(stdout)) {
			status = close(ras->fd);
			if (status != 0) {
				ESprintf(errno, errmsg, ras->name);
				return(RAS_ERROR);
			}
		}
	}

	if (ras->data  != (unsigned char *)NULL) ras_free( (char *) ras->data);
	if (ras->red   != (unsigned char *)NULL) ras_free( (char *) ras->red);
	if (ras->green != (unsigned char *)NULL) ras_free( (char *) ras->green);
	if (ras->blue  != (unsigned char *) NULL) ras_free( (char *) ras->blue);

	/* Free up file-dependent memory. */

	dep = (SGIInfo *) ras->dep;

	if (dep->rowstart != (unsigned long *)NULL)
		ras_free((void *)dep->rowstart);

	if(dep->rowsize  != (long *) NULL) {
		ras_free( (void *) dep->rowsize);
	}

	if (ras->dep != (char *) NULL) {
		ras_free( (char *) ras->dep);
	}

	return(RAS_OK);
}

int
SGISetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = SGIOpen;
	ras->OpenWrite = SGIOpenWrite;
	ras->Read      = SGIRead;
	ras->Write     = SGIWrite;
	ras->Close     = SGIClose;
	ras->PrintInfo = SGIPrintInfo;
	ras->ImageCount = ImageCount_;
	return(RAS_OK);
}
