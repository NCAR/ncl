/*
 *	$Id: sunraster.c,v 1.2 1991-08-16 11:09:56 clyne Exp $
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
#include <fcntl.h>
#include <string.h>
#include "ncarg_ras.h"
#include "sunraster.h"

static char	*FormatName = "sun";
extern char	*ProgramName;

int
SunProbe(name)
	char	*name;
{
	int		status;
	SunInfo		dep;
	FILE		*fp;

	if (name == (char *) NULL) return(False);

	if (!strcmp(name, "stdin")) return(False);

	fp = fopen(name, "r");
	if (fp == (FILE *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	/* Read the Sun raster file header. */

	status = fread((char *) &dep, 1, SUN_HEADER_SIZE, fp);
	if (status != sizeof(SunInfo)) return(RAS_EOF);

	if (dep.ras_magic != RAS_MAGIC) 
		return(False);
	else
		return(True);
}

Raster *
SunOpen(name)
	char	*name;
{
	Raster	*ras;
	char	*calloc();

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

	ras->name = (char *) calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	SunSetFunctions(ras);

	return(ras);
}

/*ARGSUSED*/
Raster *
SunOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	int		encoding;
{
	Raster		*ras;
	SunInfo		*dep;

	if (name == (char *) NULL) {
		(void) RasterSetError(RAS_E_NULL_NAME);
		return( (Raster *) NULL );
	}

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	ras->dep = calloc(sizeof(SunInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	dep = (SunInfo *) ras->dep;

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

	ras->nx	= nx;
	ras->ny	= ny;
	ras->length	= ras->nx * ras->ny;
	ras->ncolor	= 256;
	ras->type	= RAS_INDEXED;
	ras->red	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->green	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->blue	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->data	= (unsigned char *) calloc((unsigned) ras->length, 1);

	if (encoding != RAS_INDEXED) {
		(void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
		return( (Raster *) NULL );
	}
	else {
		ras->type = RAS_INDEXED;
	}

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
	unsigned long	swaptest = 1;

	dep = (SunInfo *) ras->dep;

	/* Swap bytes if necessary. */

	if (*(char *) &swaptest)
		_swaplong((char *) dep, SUN_HEADER_SIZE);

	nb = write(ras->fd, (char *) ras->dep, SUN_HEADER_SIZE);
	if (nb != SUN_HEADER_SIZE) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->red, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->green, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->blue, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->data, ras->nx * ras->ny);
	if (nb != ras->nx * ras->ny) return(RAS_EOF);

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
	unsigned int		image_size;
	SunInfo			*dep;
	int			i;
	int			count, value; 
	int			status;
	char			*malloc(), *calloc(), *realloc();
	unsigned long		swaptest = 1;
	static unsigned char	*datap, *rlep, *tmpbuf = (unsigned char *) NULL;
	static int		tmpbuf_size = 0;
	unsigned char		rgb_buf[256];

	/* Allocate the raster format dependent (header) structure. */

	if (ras->dep == (char *) NULL) {
		ras->dep =  (char *) calloc(sizeof(SunInfo),1);
		if (ras->dep == (char *) NULL) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return(RAS_ERROR);
		}
	}

	dep = (SunInfo *) ras->dep;

	/* Read the Sun raster file header. */

	status = fread((char *) dep, 1, SUN_HEADER_SIZE, ras->fp);
	if (status != sizeof(SunInfo)) return(RAS_EOF);

	/* Swap bytes if necessary. */

	if (*(char *) &swaptest) {
		_swaplong((char *) dep, SUN_HEADER_SIZE);
	}

	if(dep->ras_type == RT_STANDARD || dep->ras_type == RT_BYTE_ENCODED) {

		if(dep->ras_maptype != RMT_EQUAL_RGB) {
			(void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
			return(RAS_ERROR);
		}
		else {
			ras->type = RAS_INDEXED;
		}

		ras->nx = dep->ras_width;
		ras->ny = dep->ras_height;

		ras->ncolor = dep->ras_maplength / 3;

		/* If not initialized, allocate image memory */

		if (ras->data == (unsigned char *) NULL) {
			ras->data = (unsigned char *)
				calloc( (unsigned) ras->nx*ras->ny, 1);
			if (ras->data == (unsigned char *) NULL) {
				(void) RasterSetError(RAS_E_SYSTEM);
				return(RAS_ERROR);
			}

			/* Allocate the color tables. */

			ras->red = (unsigned char *) calloc(256, 1);
			ras->green = (unsigned char *) calloc(256, 1);
			ras->blue = (unsigned char *) calloc(256, 1);
		}


		if (!ras->map_loaded) {
			status=fread((char *)ras->red,1,ras->ncolor,ras->fp);
			if (status != ras->ncolor) return(RAS_EOF);

			status=fread((char *)ras->green,1,ras->ncolor,ras->fp);
			if (status != ras->ncolor) return(RAS_EOF);

			status=fread((char *)ras->blue,1,ras->ncolor,ras->fp);
			if (status != ras->ncolor) return(RAS_EOF);
		}
		else {
			for(i=0; i<3; i++) {
			  status=fread((char *)rgb_buf,1,ras->ncolor,ras->fp);
			  if (status != ras->ncolor) return(RAS_EOF);
			}
		}


		if (dep->ras_type == RT_STANDARD) {
			/* Standard encoding */
			image_size = (unsigned) dep->ras_length;
			ras->data = (unsigned char *) calloc(image_size, 1);
			status = fread( (char *) ras->data, 1,
				(int) image_size, ras->fp);
			if (status != image_size) return(RAS_EOF);
		}
		else {
			/* RLE Encoding */
			image_size = dep->ras_length;
			if (image_size > tmpbuf_size) {
				if (tmpbuf == (unsigned char *) NULL) {
				  tmpbuf=(unsigned char *) malloc(image_size);
				  tmpbuf_size = image_size;
				}
				else {
				  tmpbuf = (unsigned char *)
					realloc( (char *) tmpbuf,image_size);
				  tmpbuf_size = image_size;
				} 
				if (tmpbuf == (unsigned char *) NULL) {
					(void) RasterSetError(RAS_E_SYSTEM);
					return(RAS_ERROR);
				}
			}

			status=fread((char *)tmpbuf,1,(int)image_size,ras->fp);
			if (status != image_size) return(RAS_EOF);

			for(datap = ras->data, rlep = tmpbuf;
			rlep < (tmpbuf+image_size) ; ) {
				if (*rlep == RAS_SUN_ESC) {
					rlep++;
					if (*rlep == 0) {
						*datap++ = RAS_SUN_ESC;
						rlep++;
					}
					else {
						count = *rlep++ + 1;
						value = *rlep++;
						for(i=0; i<count; i++) {
							*datap++=value;
						}
					}
				}
				else {
					*datap++ = *rlep++;
				}
			}
		}
	}
	else {
		(void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

int
SunClose(ras)
	Raster	*ras;
{
	free( (char *) ras->data);
	free( (char *) ras->red);
	free( (char *) ras->green);
	free( (char *) ras->blue);
	free( (char *) ras->dep);
	return(RAS_OK);
}

int
SunSetFunctions(ras)
	Raster	*ras;
{
	ras->Open      = SunOpen;
	ras->OpenWrite = SunOpenWrite;
	ras->Read      = SunRead;
	ras->Write     = SunWrite;
	ras->Close     = SunClose;
	ras->PrintInfo = SunPrintInfo;
	return(RAS_OK);
}
