/*
 *	$Id: sgiraster.c,v 1.14 2008-07-27 03:18:46 haley Exp $
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
#include "options.h"
#include "sgiraster.h"

static int	_SGIReadPixelInterleaved();
static int	_SGIReadScanplaneInterleaved();
static int	_SGIExpandRLE();
static int	_SGIExpand();

static void	swap_sgiheader(
	SGIFileHeader_T		*header
) {
#ifndef	CRAY
	_swapshort((char *) &header->imagic, sizeof(header->imagic));
	_swapshort((char *) &header->type, sizeof(header->type));
	_swapshort((char *) &header->dim, sizeof(header->dim));
	_swapshort((char *) &header->xsize, sizeof(header->xsize));
	_swapshort((char *) &header->ysize, sizeof(header->ysize));
	_swapshort((char *) &header->zsize, sizeof(header->zsize));

	_swaplong((char *) &header->min, sizeof(header->min));
	_swaplong((char *) &header->max, sizeof(header->max));
	_swaplong((char *) &header->wastebytes, sizeof(header->wastebytes));
	_swaplong((char *) &header->colormap, sizeof(header->colormap));
#endif
	
}

static	SGIInfo_T	*alloc_sgiinfo()
{
	SGIInfo_T	*sgiinfo;

	sgiinfo = ras_malloc(sizeof(SGIInfo_T));
	if (! sgiinfo) {
		ESprintf(errno, "malloc(%d)", sizeof(SGIInfo_T));
		return(NULL);
	}

	memset(sgiinfo, '\0', sizeof(SGIInfo_T));

	/*
	**	This is redundent, but what the heck
	*/
	sgiinfo->ptr = NULL;
	sgiinfo->base = NULL;
	sgiinfo->tmpbuf = NULL;

	sgiinfo->rowstart = NULL;
	sgiinfo->rowsize = NULL;
	sgiinfo->rlebuf = NULL;
	sgiinfo->decodebuf = NULL;

	return(sgiinfo);
}

static void	free_sgiinfo(SGIInfo_T *sgiinfo)
{
	if (sgiinfo->ptr) free(sgiinfo->ptr);
	if (sgiinfo->base) free(sgiinfo->base);
	if (sgiinfo->tmpbuf) free(sgiinfo->tmpbuf);
	if (sgiinfo->rowstart) free(sgiinfo->rowstart);
	if (sgiinfo->rowsize) free(sgiinfo->rowsize);
	if (sgiinfo->rlebuf) free(sgiinfo->rlebuf);
	if (sgiinfo->decodebuf) free(sgiinfo->decodebuf);
}

static void	decode_uint32_array(
	unsigned char	*cptr,
	unsigned int	*iptr,
	int		n
) {
	int		i;

	for(i=0; i<n; i++) {
		*iptr = cptr[0] << 24 | cptr[1] << 16 | cptr[2] << 8 | cptr[3];
		iptr++;
		cptr += 4;
	}
}

static void	encode_uint32_array(
	unsigned int	*iptr,
	unsigned char	*cptr,
	int		n
) {
	int		i;
	unsigned char	c1, c2, c3, c4;

	for(i=0; i<n; i++) {
		cptr[0] = (unsigned char) (*iptr >> 24);
		cptr[1] = (unsigned char) (*iptr >> 16);
		cptr[2] = (unsigned char) (*iptr >> 8);
		cptr[3] = (unsigned char) (*iptr >> 0);

		iptr++;
		cptr += 4;
	}
}
	

Raster *
SGIOpen(name)
	char	*name;
{
	Raster	*ras;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "malloc(%d)", sizeof(Raster));
		return( (Raster *) NULL );
	}
	ras->dep = NULL;

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "open(%s)", name);
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, "fdopen(%d)", ras->fd);
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
	SGIInfo_T	*sgiinfo;
	SGIFileHeader_T	*header;
	int		hdr_length, length, tablesize;
	int		i, loc, y;
	unsigned char	*tmpbuf;
	unsigned long   swaptest = 1;
	SgiInterleaveType interleave = SGI_IL_UNKNOWN;

	/* Allocate the raster format dependent (header) structure. */

	if (ras->dep == (char *) NULL) {
		sgiinfo = alloc_sgiinfo();
		if (! sgiinfo) return(RAS_ERROR);

		ras->dep = (char *) sgiinfo;
	}

	sgiinfo = (SGIInfo_T *) ras->dep;
	header = &sgiinfo->header;

	/* Read the SGI raster file header and swap bytes if necessary. */

	status = fread(header, 1, sizeof(header[0]), ras->fp);
	if (status == 0) {
		return(RAS_EOF);
	}
	else if (status != sizeof(header[0])) {
		ESprintf(errno, "fread(%d)", sizeof(header[0]));
		return(RAS_ERROR);
	}

	/* Swap those bytes. */
	if (*(char *) &swaptest) {
		swap_sgiheader(header);
	}

	/* Make sure magic number is there. */
	if (header->imagic != SGI_MAGIC) {
		(void) ESprintf(
			RAS_E_NOT_IN_CORRECT_FORMAT,
			"SGI rasterfile has bogus magic cookie!");
		return(RAS_ERROR);
	}

	/*
	Images with other than one byte per pixel storage
	(one byte for red, as opposed to two) are not supported.
	*/

	if (SGI_BPP(header->type) != 1) {
		(void) ESprintf(
			RAS_E_UNSUPPORTED_FUNCTION,
			"Two byte pixels not supported"
		);
		return(RAS_ERROR);
	}

	/*
	We currently only support RGB images. An SGI_CM_NORMAL
	colormap parm can mean an RGB file if zsize == 3 and a grayscale
	file if zsize == 1.
	*/

	if (header->colormap != SGI_CM_NORMAL || header->zsize != 3) {
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
		ras->file_nx	= header->xsize;
		ras->file_ny	= header->ysize;
		ras->file_type	= RAS_DIRECT;

		ras->nx		= header->xsize;
		ras->ny		= header->ysize;
		ras->type	= RAS_DIRECT;
		ras->length	= ras->nx * ras->ny * 3;
		ras->ncolor	= 256*256*256;

		/* Allocate image storage. */

		ras->data = (unsigned char *) ras_calloc(ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "malloc(%d)", (ras->length));
			return(RAS_ERROR);
		}

		/*
		Allocate memory for unpacking RLE lines, one color at a time.
		Funny thing (HAWHAW) - RLE lines are often longer than what
		the raw data would have been. Allocate three times the normal,
		just to be safe.
		*/

		sgiinfo->tmpbuf = (uint16_t *) ras_calloc(3*header->xsize, 1);
		if (sgiinfo->tmpbuf == (uint16_t *) NULL) {
			(void) ESprintf(errno, "malloc(%d)", (3*header->xsize));
			return(RAS_ERROR);
		}
	}
	else {
		if (ras->file_nx != header->xsize || 
			ras->file_ny != header->ysize) 
		{
			(void) ESprintf(
				RAS_E_IMAGE_SIZE_CHANGED,
				"File changed dimensions"
			);
			return(RAS_ERROR);
		}
	}

	/* This area is used to unpack the RLE raw imagery. */
	tmpbuf = (unsigned char *) sgiinfo->tmpbuf;

	if( ((header->imagic>>8) | ((header->imagic&0xff)<<8)) == SGI_MAGIC) {
		(void) ESprintf(
			E_UNKNOWN, "Byte-swapped (old) rasters not supported"
		);
		return(RAS_ERROR);
	}

	/*
	Move up in the file to where the table of contents
	for RLE imagery is stored.  We could be reading from
	stdin, so read, not seek, up to the tables. This can't
	be more than 512 bytes, and we'll just use the ras->data
	area for the moment.
	*/

	length = RAS_SGI_RESERVED - sizeof(header[0]);

	status = fread(ras->data, 1, length, ras->fp);
	if (status != length) {
		(void) ESprintf(errno, "fread(%d)", length);
		return(RAS_ERROR);
	}

	/*
	RLE and non-RLE files can theoretically be mixed in
	a single file, as long as they all have the same dimensions.
	*/

	if (SGI_ISRLE(header->type)) { /* We've got an RLE file. */
		int	n = header->ysize * header->zsize;
		int	disk_tablesize = n * 4;

		/* Allocate memory for RLE tables. */
		tablesize = n * sizeof(sgiinfo->rowstart[0]);

		if (! sgiinfo->rowstart) {
			sgiinfo->rowstart = ras_calloc(tablesize, 1);
			sgiinfo->rowsize  = ras_calloc(tablesize, 1);
			sgiinfo->decodebuf  = ras_calloc(disk_tablesize, 1);
		}

		if (! sgiinfo->rowstart || ! sgiinfo->rowsize || ! sgiinfo->decodebuf) {
			(void) ESprintf(errno, "malloc(%d)", tablesize);
			return(RAS_ERROR);
		}

		/* Read the rowstart[] table. */

		status = fread(sgiinfo->decodebuf, 1, disk_tablesize, ras->fp);
		if (status != disk_tablesize) {
			(void) ESprintf(errno, "fread(%d)", disk_tablesize);
			return(RAS_ERROR);
		}
		decode_uint32_array(
			(unsigned char *) sgiinfo->decodebuf, 
			(unsigned int *) sgiinfo->rowstart, n
		);

		/* Read the rowsize[] table. */

		status = fread(sgiinfo->decodebuf, 1, disk_tablesize, ras->fp);
		if (status != disk_tablesize) {
			(void) ESprintf(errno, "fread(%d)", disk_tablesize);
			return(RAS_ERROR);
		}
		decode_uint32_array(
			(unsigned char *) sgiinfo->decodebuf, 
			(unsigned int *) sgiinfo->rowsize, n
		);

		/* Where are we at now in the file i.e. Is it packed? */

		loc = 512 + 2 * disk_tablesize;

		if (loc != sgiinfo->rowstart[0]) {
			(void) ESprintf(
				RAS_E_UNSUPPORTED_ENCODING,
				"RLE image not packed"
			);
			return(RAS_ERROR);
		}

		/* Check for complete image packing. */

		if (sgiinfo->rowstart[0] + sgiinfo->rowsize[0] ==
			sgiinfo->rowstart[header->ysize]) 
		{
			interleave = SGI_IL_PIXEL;
		}
		else if (sgiinfo->rowstart[0]+sgiinfo->rowsize[0] == 
			sgiinfo->rowstart[1])
		{

			interleave = SGI_IL_SCANPLANE;
		}
		else {
			(void) ESprintf(
				RAS_E_UNSUPPORTED_ENCODING,
				"Weird interleaving!"
			);
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
	else if (SGI_ISVERBATIM(header->type)) { /* Got a verbatim file */

		/*
		Image is not RLE. SGI_VERBATIM images are stacked by
		scan-planes: R, G, then B. We use "tmpbuf" to unpack
		them.
		*/

		for(i=0; i<3; i++) {
		for(y=0; y<ras->ny; y++) {
			status = fread(tmpbuf, 1, ras->nx, ras->fp);
			if (status != ras->nx) {
				(void) ESprintf(errno, "fread(%d)", ras->nx);
				return(RAS_ERROR);
			}

			(void) _SGIExpand(tmpbuf,
					&DIRECT_RED(ras, 0, ras->ny-y-1)+i,
					ras->nx);
		}}
	}
	else {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING, "");
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

static int
_SGIReadPixelInterleaved(ras)
	Raster		*ras;
{
	SGIInfo_T	*sgiinfo;
	int		status;
	int		i, y;
	unsigned char	*tmpbuf;

	sgiinfo    = (SGIInfo_T *) ras->dep;
	tmpbuf = (unsigned char *) sgiinfo->tmpbuf;

	/*
	Red, green, and blue lines are interleaved for each row of the image.
	Even though there is a table of contents, we can only use it to
	determine the order of information so that the image can be
	read from stdin as a stream.
	*/

	for(y=0; y<ras->ny; y++) for(i=0; i<3; i++) {
		int	size = sgiinfo->rowsize[y+i*ras->ny];

		/* Read an RLE line and unpack it. */
		status = fread(sgiinfo->tmpbuf,1,size,ras->fp);

		if (status != sgiinfo->rowsize[y+i*ras->ny]) {
			(void) ESprintf(errno, "fread(%d)", size);
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
	SGIInfo_T	*sgiinfo;
	int		status;
	int		i, y;
	unsigned char	*tmpbuf;

	sgiinfo    = (SGIInfo_T *) ras->dep;
	tmpbuf = (unsigned char *) sgiinfo->tmpbuf;

	/*
	Red, green, and blue *planes* are interleaved for the image.
	Even though there is a table of contents, we can only use it to
	determine the order of information so that the image can be
	read from stdin as a stream.
	*/

	for(i=0; i<3; i++) for(y=0; y<ras->ny; y++) {
		int	size = sgiinfo->rowsize[y+i*ras->ny];

		/* Read an RLE line and unpack it. */
		status = fread(sgiinfo->tmpbuf,1,size,ras->fp);

		if (status != sgiinfo->rowsize[y+i*ras->ny]) {
			(void) ESprintf(errno, "fread(%d)", size);
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

static unsigned long
_SGICompactRLE(
	unsigned char	*iptr,
	unsigned char	*optr,
	int		n
) {
	unsigned char	run;
	unsigned char	*cptr;				/* count ptr	*/
	unsigned char	*last = iptr + (3*(n-1));	/* last input	*/
	unsigned char	*optr_save = optr;		/* optr start	*/
	unsigned char	max = 0x7f;			/* max run len	*/

	cptr = optr++;	

	while (iptr <= last) {


		/*
		**	look for runs of the same pixels
		*/
		run = 1;
		while (iptr < last && *iptr == *(iptr+3) && (run < max)) {
			iptr += 3;
			run++;
		}
		if (run > 1) {
			*optr++ = *iptr;
			*cptr = 0x7f & run;
			cptr = optr++;
			iptr += 3;
		}
		if (iptr > last) break;

		/*
		**	 look for runs of different pixels
		*/
		run = 0;
		while (iptr < last && *iptr != *(iptr+3) && (run < max)) {
			*optr++ = *iptr;
			iptr += 3;
			run++;
		}
		if (iptr == last) {	/* pathological case	*/
			*optr++ = *iptr;
			iptr += 3;
			run++;
		}
		if (run > 0) {
			*cptr = 0x80 | run;
			cptr = optr++;	/* save address of run encoding	*/
		}

	}
	*cptr++ = 0;	/* flag end of run	*/

	return(cptr - optr_save);
}

static int
_SGIWritePixelInterleaved(ras)
	Raster		*ras;
{
	SGIInfo_T	*sgiinfo;
	SGIFileHeader_T	*header;
	int		i, y;
	unsigned long	row_len;	/* length of an image row	*/
	unsigned long	image_len;	/* length of entire image	*/
	unsigned char	*rlebuf;
	int		disk_tablesize;	
	unsigned char	*cptr;
	int		rc;
	int		iminus1;
	int		n;

	sgiinfo    = (SGIInfo_T *) ras->dep;
	header = &sgiinfo->header;
	rlebuf = (unsigned char *) sgiinfo->rlebuf;
	cptr = rlebuf;
	

	n = header->ysize * header->zsize;
	disk_tablesize = n * 4;


	/*
	Red, green, and blue lines are interleaved for each row of the image.
	*/

	image_len = 0;
	for(y=0; y<ras->ny; y++) for(i=0; i<3; i++) {

		row_len = _SGICompactRLE(
			&DIRECT_RED(ras,0,ras->ny-y-1)+i, cptr, ras->nx
		);

		cptr += row_len;

		sgiinfo->rowsize[y+i*ras->ny] = row_len;

		if (y == 0 && i == 0) {	/* bootstrap	*/
			sgiinfo->rowstart[y+i*ras->ny] = 
				RAS_SGI_RESERVED + (2 * disk_tablesize);
			
			iminus1 = y+i*ras->ny;
		}
		else {
			sgiinfo->rowstart[y+i*ras->ny] = 
				sgiinfo->rowstart[iminus1] + 
				sgiinfo->rowsize[iminus1];

			iminus1 = y+i*ras->ny;
		}
		image_len += row_len;


	}
	encode_uint32_array(
		(unsigned int *) sgiinfo->rowstart, 
		(unsigned char *) sgiinfo->decodebuf, n
	);

	rc = fwrite(sgiinfo->decodebuf, 1, disk_tablesize, ras->fp);
	if (rc < 0) {
		ESprintf(errno, "fwrite(1,%d)", disk_tablesize);
		return(-1);
	}

	encode_uint32_array(
		(unsigned int *) sgiinfo->rowsize, 
		(unsigned char *) sgiinfo->decodebuf, n
	);

	rc = fwrite(sgiinfo->decodebuf, 1, disk_tablesize, ras->fp);
	if (rc < 0) {
		ESprintf(errno, "fwrite(1,%d)", disk_tablesize);
		return(-1);
	}

	rc = fwrite(rlebuf, 1, image_len, ras->fp);
	if (rc < 0) {
		ESprintf(errno, "fwrite(1,%d)", image_len);
		return(-1);
	}

	return(0);
}

Raster *
SGIOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	Raster		*ras;
	SGIInfo_T	*sgiinfo;
	SGIFileHeader_T	*header;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "");
		return( (Raster *) NULL );
	}

	if (encoding != RAS_DIRECT) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
		"Only DIRECT color is supported for SGI rasterfiles");
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "malloc(%d)", sizeof(Raster));
		return( (Raster *) NULL );
	}

	sgiinfo = alloc_sgiinfo();
	if (! sgiinfo) {
		return(NULL);
	}

	header = &sgiinfo->header;

	ras->dep = (char *) sgiinfo;

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
		ras->fp = stdout;
	}
	else {
		ras->fp = fopen(name, "w");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, "fopen(%s)", name);
			return( (Raster *) NULL );
		}
		ras->fd = fileno(ras->fp);
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

	if (OptionCompression == RAS_COMPRESS_RLE) {
		int	size;

		size = ras->ny * 3 * sizeof(sgiinfo->rowstart[0]);
		sgiinfo->rowstart = ras_calloc(size, 1);
		sgiinfo->rowsize  = ras_calloc(size, 1);

		size = ras->ny * 3 * 4;
		sgiinfo->decodebuf  = ras_calloc(size, 1);

		if (! sgiinfo->rowstart || ! sgiinfo->rowsize || ! sgiinfo->decodebuf) {
			ESprintf(errno, "malloc(%s)", size);
			return( (Raster *) NULL );
		}

		/*
		** alloc rle buf big enough for worse case scenario
		*/
		size = (ras->nx * 2) * ras->ny * 3;
		sgiinfo->rlebuf =  ras_malloc(size);

		if (! sgiinfo->rlebuf) {
			ESprintf(errno, "malloc(%s)", size);
			return( (Raster *) NULL );
		}
	}

	(void) SGISetFunctions(ras);

	return(ras);
}

int
SGIWrite(ras)
	Raster	*ras;
{
	SGIInfo_T		*sgiinfo;
	SGIFileHeader_T		*header;
	int			x, y, i, nb;
	unsigned long		swaptest = 1;
	static unsigned char	*tmpbuf;
	static int		tmpbuf_size = 0;
	unsigned char		*iptr, *optr;

	sgiinfo = (SGIInfo_T *) ras->dep;
	header = &sgiinfo->header;

	if (tmpbuf == (unsigned char *) NULL) {
		if (ras->nx > RAS_SGI_RESERVED) {
			tmpbuf_size = ras->nx;
		}
		else {
			tmpbuf_size = RAS_SGI_RESERVED;
		}
		tmpbuf = (unsigned char *) calloc(tmpbuf_size, 1);
		if (tmpbuf == (unsigned char *) NULL) {
			(void) ESprintf(errno, "calloc(%d, 1)", tmpbuf_size);
			return(RAS_ERROR);
		}
	}

	
	if (OptionCompression == RAS_COMPRESS_RLE)
		header->type       = SGI_TYPE_RLE | 1;
	else
		header->type       = SGI_TYPE_VERBATIM | 1;

	header->imagic     = SGI_MAGIC;
	header->dim        = 3;
	header->xsize      = ras->nx; 
	header->ysize      = ras->ny;
	header->zsize      = 3; /* RGB image. */
	header->min        = 0;
	header->max        = 255;
	header->wastebytes = 0;
	header->colormap   = SGI_CM_NORMAL;
	strcpy(header->name,"no name");

	/* Write the header, swapping bytes if necessary. */

	if (*(char *) &swaptest)
		swap_sgiheader(header);

	nb = fwrite(header, sizeof(header[0]),1, ras->fp);
	if (nb != 1) {
		(void) ESprintf(errno, "fwrite(%d,1)", sizeof(header[0]));
		return(RAS_EOF);
	}

	ras->written = True;

	/* Write bytes remaining before actual image data. */

	memset(tmpbuf, 0,RAS_SGI_RESERVED-sizeof(header[0])); 
	nb = fwrite(tmpbuf, 1, (RAS_SGI_RESERVED-sizeof(header[0])), ras->fp);
	if (nb != (RAS_SGI_RESERVED-sizeof(header[0]))) { 
		(void) ESprintf(
			errno, "fwrite(1, %d)",
			(RAS_SGI_RESERVED-sizeof(header[0]))
		);
		return(RAS_EOF);
	}

	if (OptionCompression == RAS_COMPRESS_RLE) {

		if (_SGIWritePixelInterleaved(ras) < 0) {
			return(RAS_EOF);
		}
		return(RAS_OK);
	}

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
			nb = fwrite(tmpbuf, 1, ras->nx, ras->fp);
			if (nb != ras->nx) {
				(void) ESprintf(
					errno, "fwrite(1, %d)", ras->nx
				);
				return(RAS_EOF);
			}
		}
	}

	return(RAS_OK);
}

int
SGIPrintInfo(ras)
	Raster		*ras;
{
	SGIInfo_T	*sgiinfo;
	SGIFileHeader_T	*header;

	sgiinfo = (SGIInfo_T *) ras->dep;
	header = &sgiinfo->header;

	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "SGI Rasterfile Information\n");
	(void) fprintf(stderr, "--------------------------\n");
	(void) fprintf(stderr, "imagic:           %8x\n", header->imagic);
	(void) fprintf(stderr, "type:             %d\n", header->type);
	(void) fprintf(stderr, "dim:              %d\n", header->dim);
	(void) fprintf(stderr, "xsize:            %d\n", header->xsize);
	(void) fprintf(stderr, "ysize:            %d\n", header->ysize);
	(void) fprintf(stderr, "zsize:            %d\n", header->zsize);
	(void) fprintf(stderr, "min:              %d\n", header->min);
	(void) fprintf(stderr, "max:              %d\n", header->max);
	(void) fprintf(stderr, "wastebytes:       %d\n", header->wastebytes);
	(void) fprintf(stderr, "name:             %s\n", header->name);

	return(RAS_OK);
}

int
SGIClose(ras)
	Raster	*ras;
{
	int		status;
	char		*errmsg = "SGIClose(\"%s\")";
	SGIInfo_T	*sgiinfo;

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

	sgiinfo = (SGIInfo_T *) ras->dep;

	free_sgiinfo(sgiinfo);

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
