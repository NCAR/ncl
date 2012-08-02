/*
 *	$Id: xwd.c,v 1.24 2008-07-27 03:18:47 haley Exp $
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
/*	File:	xwd.c
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
 *		basic file access functions for XWD (X Window
 *		System Dump) files.
 *
 *		Encoding schemes are limited to:
 *			* 8-bit indexed	color with 8-bit color map values.
 *		
 */
#include <stdlib.h>
#include <errno.h>

#include <fcntl.h>
#include <sys/types.h>

#if defined(NGLONG2XINT)
#define LONG64;
#endif 

#include <X11/Intrinsic.h>
#include <X11/XWDFile.h>
#include "ncarg_ras.h"

static char	*FormatName = "xwd";
static char	*Comment = "XWD file from NCAR raster utilities";

Raster *
XWDOpen(name)
	char	*name;
{
	Raster		*ras;
	XWDFileHeader	*dep;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "XWDOpen(\"%s\")", name);
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "XWDOpen(\"%s\")", name);
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(XWDFileHeader),1);
	if (ras->dep == (char *) NULL) {
		(void) ESprintf(errno, "XWDOpen(\"%s\")", name);
		return( (Raster *) NULL );
	}
	
	dep = (XWDFileHeader *) ras->dep;

	/* Tell other routines structure hasn't been filled. */

	dep->pixmap_width = 0;

	/* Set up file descriptors and pointers. */

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "XWDOpen(\"%s\")", name);
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, "XWDOpen(\"%s\")", name);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) ras_calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	XWDSetFunctions(ras);

	return(ras);
}

int
XWDPrintInfo(ras)
	Raster	*ras;
{
	XWDFileHeader	*dep;

	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "XWD Rasterfile Information\n");
	(void) fprintf(stderr, "--------------------------\n");

	dep = (XWDFileHeader *) ras->dep;
	(void) fprintf(stderr, "header_size      %d\n", dep->header_size);
	(void) fprintf(stderr, "file_version     %d\n", dep->file_version);
	(void) fprintf(stderr, "pixmap_format    %d\n", dep->pixmap_format);
	(void) fprintf(stderr, "pixmap_depth     %d\n", dep->pixmap_depth);
	(void) fprintf(stderr, "pixmap_width     %d\n", dep->pixmap_width);
	(void) fprintf(stderr, "pixmap_height    %d\n", dep->pixmap_height);
	(void) fprintf(stderr, "xoffset          %d\n", dep->xoffset);
	(void) fprintf(stderr, "byte_order       %d\n", dep->byte_order);
	(void) fprintf(stderr, "bitmap_unit      %d\n", dep->bitmap_unit);
	(void) fprintf(stderr, "bitmap_bit_order %d\n", dep->bitmap_bit_order);
	(void) fprintf(stderr, "bitmap_pad       %d\n", dep->bitmap_pad);
	(void) fprintf(stderr, "bits_per_pixel   %d\n", dep->bits_per_pixel);
	(void) fprintf(stderr, "bytes_per_line   %d\n", dep->bytes_per_line);
	(void) fprintf(stderr, "visual_class     %d\n", dep->visual_class);
	(void) fprintf(stderr, "red_mask         %d\n", dep->red_mask);
	(void) fprintf(stderr, "green_mask       %d\n", dep->green_mask);
	(void) fprintf(stderr, "blue_mask        %d\n", dep->blue_mask);
	(void) fprintf(stderr, "bits_per_rgb     %d\n", dep->bits_per_rgb);
	(void) fprintf(stderr, "colormap_entries %d\n", dep->colormap_entries);
	(void) fprintf(stderr, "ncolors          %d\n", dep->ncolors);
	(void) fprintf(stderr, "window_width     %d\n", dep->window_width);
	(void) fprintf(stderr, "window_height    %d\n", dep->window_height);
	(void) fprintf(stderr, "window_x         %d\n", dep->window_x);
	(void) fprintf(stderr, "window_y         %d\n", dep->window_y);
	(void) fprintf(stderr, "window_bdrwidth  %d\n", dep->window_bdrwidth);
	return(RAS_OK);
}

/*
 *
 *	Author:		John Clyne
 *
 *	Date		Mon Mar 30 14:25:43 MST 1990
 *
 *	XWDRead:
 *
 *	read in the next image from a X dump. read_8bit_raster sequentially
 *	reads in images from a X dump file with each invocation. It than
 *	converts the image to something the sun library routines can handle
 *
 * on entry:
 *	xdump_fd	: file descriptor for X dump file
 * on exit
 *	*raster_data	: contains data for a single image
 *	return		: 0 => EOF, < 0 => failure, else ok
 */

int
XWDRead(ras)
	Raster	*ras;
{
	XWDFileHeader		*dep;		/* Format dependent struct */
	XWDFileHeader		old_dep;
	static unsigned		buffer_size;	/* size of image	*/
	unsigned char		*cptr1, *cptr2;
	int			status, i;
	XWDColor		xcolors[256];	/* color palette in X dump */
	int			win_name_size;	/* not used */
	unsigned		image_size();
	static unsigned long 	swaptest = 1;

	/* Stash a copy of "dep" in order to check for mixed image sizes. */
	/* Read in header, "dep" is format dependent data. */

	dep = (XWDFileHeader *) ras->dep;
	memmove((void *) &old_dep, (const void *) dep, sizeof(XWDFileHeader));

	status = fread( (char *)dep, 1, sizeof(XWDFileHeader), ras->fp);
	if (status == 0) {
		return(RAS_EOF);
	}
	else if (status != sizeof(XWDFileHeader)) {
		(void) ESprintf(RAS_E_NOT_IN_CORRECT_FORMAT,
			"XWDRead(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	if (*(char *) &swaptest) {
		_swaplong((char *) dep, sizeof(XWDFileHeader));
	}

	/* Check to see if the xwd file is the proper revision. */

	if (dep->file_version != XWD_FILE_VERSION) {
		(void) ESprintf(RAS_E_NOT_IN_CORRECT_FORMAT,
			"XWDRead(\"%s\")", ras->name);
		(void) fprintf(stderr,
			"Warning: XWD file format version mismatch\n");
	}

	if (dep->header_size < sizeof(XWDFileHeader)) {
		(void) ESprintf(RAS_E_NOT_IN_CORRECT_FORMAT,
			"XWDRead(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	ras->nx = dep->pixmap_width;
	ras->ny = dep->pixmap_height;
	ras->length = ras->nx * ras->ny;

	if (dep->ncolors > 256) {
		(void) ESprintf(RAS_E_COLORMAP_TOO_BIG,
			"XWDRead(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	ras->ncolor = dep->ncolors;
			
	/* Make sure we have a format we can handle */

	if (dep->pixmap_format != ZPixmap) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
			"XWDRead(\"%s\") - Only ZPixmap supported", ras->name);
		return(RAS_ERROR);
	}

	if (dep->pixmap_depth == 8 && dep->bits_per_pixel == 8) {
		ras->type = RAS_INDEXED;
	}
	else {
		(void) ESprintf(RAS_E_8BIT_PIXELS_ONLY,
			"XWDRead(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	/* If this is the first read on this Raster structure, */
	/* allocate the necessary memory. Otherwise, make sure */
	/* that the image size hasn't changed between frames   */

	if (ras->data == (unsigned char *) NULL) {
		buffer_size = image_size(dep);
		ras->data = (unsigned char *) ras_malloc (buffer_size);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "XWDRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}

		ras->red  =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->green=(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->blue =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
	}
	else {
		if (dep->pixmap_width != old_dep.pixmap_width) {
			(void) ESprintf(RAS_E_IMAGE_SIZE_CHANGED,
				"XWDRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}

		if (dep->pixmap_height != old_dep.pixmap_height) {
			(void) ESprintf(RAS_E_IMAGE_SIZE_CHANGED,
				"XWDRead(\"%s\")", ras->name);
			return(RAS_ERROR);
		}
	}

	/* Read in the window name (not used) */

	win_name_size = (dep->header_size - sizeof(XWDFileHeader));
	if ((ras->text = ras_malloc((unsigned) win_name_size)) == NULL) {
		(void) ESprintf(errno, "XWDRead(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	status = fread(ras->text, 1, win_name_size, ras->fp);
	if (status != win_name_size) {
		(void) ESprintf(RAS_E_PREMATURE_EOF,
				"XWDRead(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	/* Read in the color palette */

	status  = fread((char *) xcolors, 1, 
		(int) (ras->ncolor * sizeof(XWDColor)), ras->fp);
	if (status != ras->ncolor * sizeof(XWDColor)) {
		(void) ESprintf(RAS_E_NOT_IN_CORRECT_FORMAT,
			"XWDRead(\"%s\")", ras->name);
	}

	/* Swap bytes in the color table, if appropriate. */

	if (*(char *) &swaptest) {
		for (i = 0; i < ras->ncolor; i++) {
			_swaplong((char *) &xcolors[i].pixel, sizeof(xcolors[i].pixel));
			_swapshort((char *) &xcolors[i].red, 3 * sizeof(short));
		}
        }

	/* Load ras with the color palette, if there isn't one already. */

	if (ras->map_forced != True) {
		for (i = 0; i < ras->ncolor; i++ ) {
			ras->red[i]	= xcolors[i].red / 256;
			ras->green[i]	= xcolors[i].green / 256;
			ras->blue[i]	= xcolors[i].blue / 256;
		}
	}
	
	/* Read in the image */

	status   = fread((char *) ras->data, 1, (int) buffer_size, ras->fp);
	if (status != buffer_size) {
		(void) ESprintf(RAS_E_NOT_IN_CORRECT_FORMAT,
			"XWDRead(\"%s\")", ras->name);
	}

	/* Remove padding if it exists (X dumps padded to word boundaries) */

	if (dep->bytes_per_line - dep->pixmap_width) {
		cptr1 = ras->data + ras->nx;
		cptr2 = ras->data + dep->bytes_per_line;
		for (i = 1; i < dep->pixmap_height; i++) {
			memmove((void *) cptr1, (const void *) cptr2, ras->nx);
			cptr1 += ras->nx; 
			cptr2 += dep->bytes_per_line;
		}
	}
			
	return(RAS_OK);
}

unsigned
image_size(header)
	XWDFileHeader *header;
{
	if (header->pixmap_format != ZPixmap)
		return(header->bytes_per_line * header->pixmap_height * 
			header->pixmap_depth);

	return((unsigned)header->bytes_per_line * header->pixmap_height);
}

/*ARGSUSED*/
Raster *
XWDOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	Raster		*ras;
	XWDFileHeader	*dep;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "XWDOpenWrite(\"%s\")", name);
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);

	ras->dep = ras_calloc(sizeof(XWDFileHeader),1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "XWDOpenWrite(\"%s\")", name);
		return( (Raster *) NULL );
	}

	dep = (XWDFileHeader *) ras->dep;

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
	}
	else {
		ras->fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0644);

		if (ras->fd == -1) {
			(void) ESprintf(errno, "XWDOpenWrite(\"%s\")", name);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) ras_calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	if (comment != (char *) NULL) {
		ras->text = (char *) ras_calloc((unsigned) (strlen(comment) + 1),1);
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

	if (encoding != RAS_INDEXED) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
			"Only INDEXED encoding is supported for XWD");
		return( (Raster *) NULL );
	}
	else {
		ras->type = RAS_INDEXED;
	}

	dep->header_size	= sizeof(XWDFileHeader) + strlen(Comment);
	dep->file_version	= XWD_FILE_VERSION;
	dep->pixmap_format	= ZPixmap;
	dep->pixmap_depth	= 8;
	dep->pixmap_width	= nx;
	dep->pixmap_height	= ny;
	dep->xoffset		= 0;
	dep->byte_order		= 1;
	dep->bitmap_unit	= 32;
	dep->bitmap_bit_order	= 1;
	dep->bitmap_pad		= 32;
	dep->bits_per_pixel	= 8;
	dep->bytes_per_line	= nx;
	dep->visual_class	= PseudoColor;
	dep->red_mask		= 0;
	dep->green_mask		= 0;
	dep->blue_mask		= 0;
	dep->bits_per_rgb	= 8;
	dep->colormap_entries	= 256;
	dep->ncolors		= 256;
	dep->window_width	= nx;
	dep->window_height	= ny;
	dep->window_x		= 0;
	dep->window_y		= 0;
	dep->window_bdrwidth	= 0;

	XWDSetFunctions(ras);

	return(ras);
}

int
XWDWrite(ras)
	Raster	*ras;
{
	XWDColor	xcolors[256];		/* color palette in X dump */
	int		nb;
	int		i;
	unsigned long	swaptest = 1;
	char		*dep;
	static char	*swapbuf = (char *) NULL;

	/* Allocate a static swapping buffer, if necessary. */

	if (*(char *) &swaptest) {
		if (swapbuf == (char *) NULL) {
			swapbuf = ras_calloc(sizeof(XWDFileHeader),1);
			if (swapbuf == (char *) NULL) {
				(void) ESprintf(errno,
					"XWDWrite(\"%s\")", ras->name);
			}
				
		}
		memmove((void *)swapbuf,(const void *)ras->dep, sizeof(XWDFileHeader));
		_swaplong(swapbuf, sizeof(XWDFileHeader));
		dep = swapbuf;
	}
	else {
		dep = ras->dep;
	}

	/* Write the header from the proper buffer */

	nb = write(ras->fd, dep, sizeof(XWDFileHeader));
	if (nb != sizeof(XWDFileHeader)) {
		(void) ESprintf(errno, "XWDWrite(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	nb = write(ras->fd, Comment, strlen(Comment));
	if (nb != strlen(Comment)) {
		(void) ESprintf(errno, "XWDWrite(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	for(i=0; i<ras->ncolor; i++) {
		xcolors[i].red = ras->red[i] * 256;
		xcolors[i].green = ras->green[i] * 256;
		xcolors[i].blue = ras->blue[i] * 256;
	}

	/* Swap bytes in the color table, if appropriate. */

	if (*(char *) &swaptest) {
		for (i = 0; i < ras->ncolor; i++) {
			_swaplong((char *) &xcolors[i].pixel, sizeof(xcolors[i].pixel));
			_swapshort((char *) &xcolors[i].red, 3 * sizeof(short));
		}
        }

	nb = write(ras->fd, (char *) xcolors, sizeof(xcolors));
	if (nb != sizeof(xcolors)) {
		(void) ESprintf(errno, "XWDWrite(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	nb = write(ras->fd, (char *) ras->data, ras->nx * ras->ny);
	if (nb != ras->nx * ras->ny) {
		(void) ESprintf(errno, "XWDWrite(\"%s\")", ras->name);
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

int
XWDClose(ras)
	Raster	*ras;
{
	int	status;

	status = GenericClose(ras);
	return(status);
}

int
XWDSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open        = XWDOpen;
	ras->OpenWrite   = XWDOpenWrite;
	ras->Read        = XWDRead;
	ras->Write       = XWDWrite;
	ras->Close       = XWDClose;
	ras->PrintInfo   = XWDPrintInfo;
	ras->ImageCount  = ImageCount_;

	return 0;
}
