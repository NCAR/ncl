/*
 *	$Id: sunraster.c,v 1.17 1993-01-17 06:52:00 don Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	sunraster.c
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
 *		basic file access functions for Sun raster
 *		files.
 *
 *		Encoding schemes are limited to:
 *			* 8-bit indexed	color with 8-bit color map values.
 *		
 */
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include "ncarg_ras.h"
#include "sunraster.h"

static char	*FormatName = "sun";

Raster *
SunOpen(name)
	char	*name;
{
	Raster	*ras;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "");
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "");
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, "");
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) ras_calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	SunSetFunctions(ras);

	return(ras);
}

Raster *
SunOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	Raster		*ras;
	SunInfo		*dep;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME,
			"SunOpenWrite(%s,,,,)", name);
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "");
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(SunInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) ESprintf(errno, "");
		return( (Raster *) NULL );
	}

	dep = (SunInfo *) ras->dep;

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

	ras->format = (char *) ras_calloc((unsigned) (strlen(FormatName)+1),1);
	(void) strcpy(ras->format, FormatName);

	if (comment != (char *) NULL) {
		ras->text = (char *)ras_calloc((unsigned)(strlen(comment)+1),1);
		(void) strcpy(ras->text, comment);
	}
	else {
		ras->text = (char *) NULL;
	}

	if (encoding != RAS_INDEXED) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
		"Sun true color output not available");
		return( (Raster *) NULL );
	}

	ras->nx	= nx;
	ras->ny	= ny;
	ras->length	= ras->nx * ras->ny;
	ras->ncolor	= 256;
	ras->type	= RAS_INDEXED;
	ras->red	= (unsigned char *) ras_calloc((unsigned)ras->ncolor,1);
	ras->green	= (unsigned char *) ras_calloc((unsigned)ras->ncolor,1);
	ras->blue	= (unsigned char *) ras_calloc((unsigned)ras->ncolor,1);

	/*
	Allocate a few extra bytes at the end of the image, so that the 
	output routines can pad to 16 bits without referencing bogus
	memory.
	*/
	ras->data = (unsigned char *)ras_calloc((unsigned)(ras->length+4),1);

	/* Set up Sun header structure. */

	dep->ras_magic		= RAS_MAGIC;
	dep->ras_width		= ras->nx;
	dep->ras_height		= ras->ny;
	dep->ras_depth		= 8;
	dep->ras_length		= ras->nx * ras->ny;
	dep->ras_type		= RT_STANDARD;
	dep->ras_maptype	= RMT_EQUAL_RGB;
	dep->ras_maplength	= 768;

	(void) SunSetFunctions(ras);

	return(ras);
}

int
SunWrite(ras)
	Raster	*ras;
{
	SunInfo		*dep;
	int		nb;
	int		y;
	unsigned char	*p;
	unsigned long	swaptest = 1;

	dep = (SunInfo *) ras->dep;

	/* Write the header, swapping bytes if necessary. */

	if (*(char *) &swaptest)
		_swaplong((char *) dep, SUN_HEADER_SIZE);

	nb = write(ras->fd, (char *) ras->dep, SUN_HEADER_SIZE);
	if (nb != SUN_HEADER_SIZE) return(RAS_EOF);

	/* Write the color table. */

	nb = write(ras->fd, (char *) ras->red, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->green, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->blue, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	/*
	Write the image, padding to 16 bits if "nx" is odd.
	Padding is accomplished by using multiple writes instead
	of one long one. Each padded write just includes an extra byte.
	The memory allocation routines allocate a little extra memory
	so this won't result in a segmentation fault.
	*/

	/* Code below handles only indexed files, no compression. */

	if (ras->nx % 2 == 0) {
		nb = write(ras->fd, (char *) ras->data, ras->length);
		if (nb != ras->length) return(RAS_EOF);
	}
	else {
		for(p = ras->data, y=0; y<ras->ny; y++, p += ras->nx) {
			nb = write(ras->fd,(char *) p, ras->nx + 1);
		}
	}

	return(RAS_OK);
}

int
SunPrintInfo(ras)
	Raster		*ras;
{
	SunInfo		*dep;

	dep = (SunInfo *) ras->dep;

	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "Sun Rasterfile Information\n");
	(void) fprintf(stderr, "--------------------------\n");
	(void) fprintf(stderr, "ras_magic:        %8x\n", dep->ras_magic);
	(void) fprintf(stderr, "ras_width:        %d\n", dep->ras_width);
	(void) fprintf(stderr, "ras_height:       %d\n", dep->ras_height);
	(void) fprintf(stderr, "ras_depth:        %d\n", dep->ras_depth);
	(void) fprintf(stderr, "ras_length:       %d\n", dep->ras_length);
	(void) fprintf(stderr, "ras_type:         %d\n", dep->ras_type);
	(void) fprintf(stderr, "ras_maptype:      %d\n", dep->ras_maptype);
	(void) fprintf(stderr, "ras_maplength:    %d\n", dep->ras_maplength);

	return(RAS_OK);
}

int
SunRead(ras)
	Raster	*ras;
{
	char			*errmsg = "SunRead(\"%s\")";
	unsigned int		image_size;
	SunInfo			*dep;
	int			i;
	int			count, value; 
	int			length;
	int			status;
	int			x, y;
	unsigned long		swaptest = 1;
	static unsigned char	*datap, *rlep;
	static unsigned char	*ptmp  = (unsigned char *) NULL;
	static unsigned char	*tmpbuf = (unsigned char *) NULL;
	static int		tmpbuf_size = 0;
	unsigned char		rgb_buf[256];
	unsigned char		*p;
	unsigned char		dummy;
	int			do_pad;

	/* Allocate the raster format dependent (header) structure. */

	if (ras->dep == (char *) NULL) {
		ras->dep =  (char *) ras_calloc(sizeof(SunInfo),1);
		if (ras->dep == (char *) NULL) {
			(void) ESprintf(errno, errmsg, ras->name);
			return(RAS_ERROR);
		}
	}

	dep = (SunInfo *) ras->dep;

	/* Read the Sun raster file header and swap bytes if necessary. */

	status = fread((char *) dep, 1, SUN_HEADER_SIZE, ras->fp);
	if (status == 0) {
		return(RAS_EOF);
	}
	else if (status != sizeof(SunInfo)) {
		(void) ESprintf(RAS_E_PREMATURE_EOF, errmsg, ras->name);
		return(RAS_ERROR);
	}

	if (*(char *) &swaptest) {
		_swaplong((char *) dep, SUN_HEADER_SIZE);
	}

	/* Weed out the unsupported encoding forms */

	if (dep->ras_type == RT_OLD || dep->ras_type == RT_EXPERIMENTAL) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
		"Sun RT_OLD and RT_EXPERIMENTAL not supported for input");
		return(RAS_ERROR);
	}

	/* Weed out unsupported color mapping schemes */

	if (dep->ras_maptype == RMT_EQUAL_RGB) {
		if (ras->read == False) {
			ras->read	= True;
			ras->file_nx	= dep->ras_width;
			ras->file_ny	= dep->ras_height;
			ras->file_type	= RAS_INDEXED;
		}

		if (ras->file_nx != dep->ras_width ||
		    ras->file_ny != dep->ras_height) {
			(void) ESprintf(RAS_E_IMAGE_SIZE_CHANGED,
					errmsg, ras->name);
			return(RAS_ERROR);
		}

		if (ras->file_type != RAS_INDEXED) {
			(void) ESprintf(RAS_E_IMAGE_TYPE_CHANGED,
				errmsg, ras->name);
			return(RAS_ERROR);
		}

		ras->nx		= dep->ras_width;
		ras->ny		= dep->ras_height;
		ras->ncolor	= dep->ras_maplength / 3;
		ras->type	= RAS_INDEXED;
		ras->length	= ras->nx * ras->ny;
	}
	else if (dep->ras_maptype == RMT_NONE) {
		if (ras->read == False) {
			ras->read	= True;
			ras->file_nx	= dep->ras_width;
			ras->file_ny	= dep->ras_height;
			ras->file_type	= RAS_DIRECT;
		}

		if (ras->file_nx != dep->ras_width ||
		    ras->file_ny != dep->ras_height) {
			(void) ESprintf(RAS_E_IMAGE_SIZE_CHANGED,
					errmsg, ras->name);
			return(RAS_ERROR);
		}

		if (ras->file_type != RAS_DIRECT) {
			(void) ESprintf(RAS_E_IMAGE_TYPE_CHANGED,
					errmsg, ras->name);
			return(RAS_ERROR);
		}

		ras->nx		= dep->ras_width;
		ras->ny		= dep->ras_height;
		ras->ncolor	= 0;
		ras->type	= RAS_DIRECT;
		ras->length	= ras->nx * ras->ny * 3;
	}
	else {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
		"Only Sun RMT_EQUAL_RGB and RMT_NONE color maps supported");
		return(RAS_ERROR);
	}
		
	/* If not initialized, allocate image memory */

	if (ras->data == (unsigned char *) NULL) {

		ras->data = (unsigned char *) ras_calloc( ras->length, 1);

		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "");
			return(RAS_ERROR);
		}

		/* Allocate memory for colormap if necessary. */

		if (dep->ras_maptype == RMT_EQUAL_RGB) {
			ras->red = (unsigned char *) ras_calloc(256, 1);
			ras->green = (unsigned char *) ras_calloc(256, 1);
			ras->blue = (unsigned char *) ras_calloc(256, 1);
			if (ras->red == (unsigned char *) NULL ||
			    ras->green == (unsigned char *) NULL ||
			    ras->blue == (unsigned char *) NULL) {
			    (void) ESprintf(errno, "");
			    return(RAS_ERROR);
			}
		}
	}

	/*
	Read in the color table information. If someone else has 
	already loaded a color map then just discard the information.
	*/

	if (!ras->map_forced) {
		status=fread((char *)ras->red,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) {
			(void)ESprintf(RAS_E_PREMATURE_EOF, errmsg, ras->name);
			return(RAS_ERROR);
		}

		status=fread((char *)ras->green,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) {
			(void)ESprintf(RAS_E_PREMATURE_EOF, errmsg, ras->name);
			return(RAS_ERROR);
		}

		status=fread((char *)ras->blue,1,ras->ncolor,ras->fp);
		if (status != ras->ncolor) {
			(void)ESprintf(RAS_E_PREMATURE_EOF, errmsg, ras->name);
			return(RAS_ERROR);
		}
	}
	else {
		for(i=0; i<3; i++) {
		  status=fread((char *)rgb_buf,1,ras->ncolor,ras->fp);
		  if (status != ras->ncolor) {
			  (void)ESprintf(RAS_E_PREMATURE_EOF,
					  errmsg, ras->name);
			  return(RAS_ERROR);
		  }
		}
	}

	/*
	The common steps have been taken. Now read the image
	itself, where a number of formats are possible.
	*/

	if (dep->ras_type == RT_STANDARD && dep->ras_depth == 8) {

		/* Indexed color encoding - handle 16bit padding. */

		if (ras->nx % 2 == 0) {
			status = fread( (char *) ras->data, 1,
					ras->length, ras->fp);
			if (status != ras->length) {
				(void) ESprintf(RAS_E_PREMATURE_EOF,
						errmsg, ras->name);
				return(RAS_ERROR);
			}
		}
		else {
			/* Odd X dimension so pad to 16-bits. */
			length = ras->nx + 1;

			for(p=ras->data, y=0; y<ras->ny-1; y++, p+=ras->nx) {
				status = fread((char *) p, 1, length, ras->fp);
				if (status != length) {
					(void) ESprintf(RAS_E_PREMATURE_EOF,
							errmsg, ras->name);
					return(RAS_ERROR);
				}
			}

			status = fread( (char *) p, 1, ras->nx, ras->fp);
			if (status != ras->nx) {
				(void) ESprintf(RAS_E_PREMATURE_EOF,
						errmsg, ras->name);
				return(RAS_ERROR);
			}

			/* Read that last, hateful byte. */
			status = fread( &dummy, 1, 1, ras->fp);
			if (status != 1) {
				(void) ESprintf(RAS_E_PREMATURE_EOF,
						errmsg, ras->name);
				return(RAS_ERROR);
			}
		}
	}
	else if (dep->ras_type == RT_STANDARD && dep->ras_depth == 32) {
		/*
		Direct color encoding. We'll have to discard one of four bytes.
		*/

		if (tmpbuf == (unsigned char *) NULL) {
			tmpbuf = (unsigned char *) 
				ras_calloc( (unsigned) dep->ras_length, 1);
			if (tmpbuf == (unsigned char *) NULL) {
				(void) ESprintf(errno, "");
				return(RAS_ERROR);
			}
		}
		status = fread( (char *) tmpbuf, 1,
			(int) dep->ras_length, ras->fp);
		if (status != dep->ras_length) {
			(void) ESprintf(RAS_E_PREMATURE_EOF,
					errmsg, ras->name);
			return(RAS_ERROR);
		}

		ptmp = tmpbuf;

		for(y=0; y<ras->ny; y++) {
		for(x=0; x<ras->nx; x++) {
			DIRECT_RED(ras, x, y) = ptmp[3];
			DIRECT_GREEN(ras, x, y) = ptmp[2];
			DIRECT_BLUE(ras, x, y) = ptmp[1];
			ptmp += 4;
		}}
	}
	else {
		/* RLE Encoding */
		image_size = dep->ras_length;
		if (image_size > tmpbuf_size) {
			if (tmpbuf == (unsigned char *) NULL) {
			  tmpbuf=(unsigned char *) ras_malloc(image_size);
			  tmpbuf_size = image_size;
			}
			else {
			  tmpbuf = (unsigned char *)
				realloc( (char *) tmpbuf,image_size);
			  tmpbuf_size = image_size;
			} 
			if (tmpbuf == (unsigned char *) NULL) {
				(void) ESprintf(errno, "");
				return(RAS_ERROR);
			}
		}

		status=fread((char *)tmpbuf,1,(int)image_size,ras->fp);
#ifdef DEAD
		if (status != image_size) return(RAS_EOF);
#endif
		if (status != image_size) image_size = status;

		if (ras->nx % 2) {
			do_pad = True;
		}
		else {
			do_pad = False;
		}
		x = 0; y = 0;

		for(datap = ras->data, rlep = tmpbuf;
		rlep < (tmpbuf+image_size) ; ) {
			if (*rlep == RAS_SUN_ESC) {
				rlep++;
				if (*rlep == 0) {
					*datap++ = RAS_SUN_ESC;
					rlep++;
					if (do_pad) {
						if (x == ras->nx) {
							datap--;
							x = 0;
							y++;
						}
						else {
							x++;
						}
					}
					else {
						if (x == ras->nx-1) {
							x = 0;
							y++;
						}
						else {
							x++;
						}
					}
				}
				else {
					count = *rlep++ + 1;
					value = *rlep++;
					for(i=0; i<count; i++) {
						*datap++=value;
						if (do_pad) {
							if (x == ras->nx) {
								datap--;
								x = 0;
								y++;
							}
							else {
								x++;
							}
						}
						else {
							if (x == ras->nx-1) {
								x = 0;
								y++;
							}
							else {
								x++;
							}
						}
					}
				}
			}
			else {
				*datap++ = *rlep++;
				if (do_pad && x == ras->nx) {
					datap--;
					x = 0; y++;
				}
				else if (!do_pad && x == ras->nx-1) {
					x = 0; y++;
				}
				else {
					x++;
				}
			}
		}
#ifdef DEBUG
		(void) fprintf(stderr, "x = %d   y = %d\n", x, y);
		(void) fprintf(stderr, "sizeof(rle) = %d\n", dep->ras_length);
		(void) fprintf(stderr, "sizeof(ras) = %d\n", ras->length);
		(void) fprintf(stderr, "datap start = %x\n", ras->data);
		(void) fprintf(stderr, "datap end   = %x\n", datap-1);
		(void) fprintf(stderr, "diff        = %d\n", datap-ras->data);
		(void) fprintf(stderr, "datap = %x\n", datap);
#endif
	}

	return(RAS_OK);
}

#ifdef DEAD
int
_SunSetRLEPixel(do_pad, data, nx, ny, x, y)
	int		do_pad;
	unsigned char	*data;
	int		*x;
	int		*y;
{
	if (do_pad) {
	}
	else {
	}
}
#endif

int
SunClose(ras)
	Raster	*ras;
{
	int		status;
	status = GenericClose(ras);

	return(status);
}

int
SunSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = SunOpen;
	ras->OpenWrite = SunOpenWrite;
	ras->Read      = SunRead;
	ras->Write     = SunWrite;
	ras->Close     = SunClose;
	ras->PrintInfo = SunPrintInfo;
	ras->ImageCount = ImageCount_;
	return(RAS_OK);
}
