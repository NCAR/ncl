/*
 *	$Id: nrif.c,v 1.2 1991-08-16 11:10:57 clyne Exp $
 */
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
 *		Encoding schemes are limited to:
 *			* 8-bit indexed	color with 8-bit color map values.
 *			* true color with 8-bit color map values.
 *		Others may be easily added if necessary.
 *		
 */
#include <stdio.h>
#include <fcntl.h>
#include "ncarg_ras.h"
#include "nrif.h"

/*LINTLIBRARY*/

static char	*FormatName = "nrif";

extern char	*ProgramName;

char		*calloc(), *strcpy(), *strncpy();

/*ARGSUSED*/
int
NrifProbe(name)
	char	*name;
{
#ifdef DEAD
	int		status;
	FILE		*fp;

	if (name == (char *) NULL) return(False);

	if (!strcmp(name, "stdin")) return(False);

	fp = fopen(name, "r");
	if (fp == (FILE *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}
	
	(void) fclose(fp);
#endif DEAD
}

Raster *
NrifOpen(name)
	char	*name;
{
	Raster		*ras;

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	ras->dep = calloc(sizeof(NrifInfo),1);
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

	ras->name = (char *) calloc( (unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	NrifSetFunctions(ras);

	return(ras);
}

int
NrifRead(ras)
	Raster	*ras;
{
	NrifInfo		*dep;
	unsigned char		buf[1024];
	int			length;
	int			x, y;
	int			status;

	dep = (NrifInfo *) ras->dep;

	status = fread((char *) buf, 1, 4, ras->fp);
	if (status == 0) {
		return(RAS_EOF);
	}
	else if (status != 4) {
		(void) RasterSetError(RAS_E_SYSTEM);
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
			(void) RasterSetError(RAS_E_NOT_IN_CORRECT_FORMAT);
			return(RAS_ERROR);
		}
		else {
			(void) strncpy(dep->magic, (char *) buf, 4);
		}

		dep->encapsulated = TRUE;
	}
	else {
		dep->encapsulated = FALSE;
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
		ras->text = calloc(dep->cmtlen, 1);
		status = fread((char *)ras->text, 1, (int)dep->cmtlen, ras->fp);
		if (status != dep->cmtlen) return(RAS_EOF);
	}
	else {
		ras->text = (char *) NULL;
	}

	/* Device Information (nobody cares) */

	if (dep->devlen > 0) {
		dep->device_info = calloc(dep->devlen, 1);
		status = fread( (char *) dep->device_info, 1,
			 (int) dep->devlen,ras->fp);
		if (status != dep->devlen) return(RAS_EOF);
	}
	else {
		dep->device_info = (char *) NULL;
	}

	/* Encoding Information */

	switch(dep->encoding) {
		case NRIF_INDEXED:
		  /* Read the NRIF encoding information. */
		  status = fread( (char *)buf, 1, 12, ras->fp);
		  if (status != 12) return(RAS_EOF);

		  dep->ibits = char_decode(&buf[0], 4);
		  dep->ncolor = char_decode(&buf[4], 4);
		  dep->cbits = char_decode(&buf[8], 4);

		  if (dep->ibits != 8) {
		    (void) RasterSetError(RAS_E_8BIT_INTENSITIES_ONLY);
		    return(RAS_ERROR);
		  }

		  if (dep->cbits != 8) {
		    (void) RasterSetError(RAS_E_8BIT_PIXELS_ONLY);
		    return(RAS_ERROR);
		  }

		  /* Add new information to the raster structure */

		  ras->nx = dep->nx;
		  ras->ny = dep->ny;
		  ras->type = RAS_INDEXED;
		  ras->ncolor = dep->ncolor;
		  ras->length = ras->nx * ras->ny;

		  ras->red = (unsigned char *)calloc((unsigned) ras->ncolor,1);
		  ras->green =(unsigned char *)calloc((unsigned) ras->ncolor,1);
		  ras->blue = (unsigned char *)calloc((unsigned) ras->ncolor,1);
		  ras->data = (unsigned char *)calloc((unsigned) ras->length,1);

		  status=fread((char *)ras->red,1,ras->ncolor,ras->fp);
		  if (status != ras->ncolor) return(RAS_EOF);

		  status=fread((char *)ras->green,1,ras->ncolor,ras->fp);
		  if (status != ras->ncolor) return(RAS_EOF);

		  status=fread((char *)ras->blue,1,ras->ncolor,ras->fp);
		  if (status != ras->ncolor) return(RAS_EOF);

		  status = fread((char *)ras->data, 1, ras->length, ras->fp);
		  if (status != ras->length) return(RAS_EOF);

		  break;

		case NRIF_DIRECT:
		  dep->cbits = read_decode((int) ras->fp, 4);
		  if (dep->cbits == RAS_EOF) return(RAS_EOF);

		  if (dep->cbits != 8) {
		    (void) RasterSetError(RAS_E_8BIT_PIXELS_ONLY);
		    return(RAS_ERROR);
		  }

		  ras->nx = dep->nx;
		  ras->ny = dep->ny;
		  ras->type = RAS_DIRECT;
		  ras->ncolor = dep->ncolor;
		  ras->length = ras->nx * ras->ny * 3;

		  ras->data = (unsigned char *) calloc((unsigned)ras->length,1);
		
		  status = fread((char *)ras->data, 1, ras->length, ras->fp);
		  if (status != ras->length) return(RAS_EOF);
		  break;

		case NRIF_DIRECT_RLE:
		  dep->cbits = read_decode((int) ras->fp, 4);
		  if (dep->cbits == -1) return(RAS_EOF);

		  if (dep->cbits != 8) {
			(void) RasterSetError(RAS_E_8BIT_PIXELS_ONLY);
			return(RAS_ERROR);
		  }

		  dep->rbits = read_decode((int) ras->fp, 4);
		  if (dep->rbits == -1) return(RAS_EOF);

		  if (dep->cbits != 8) {
			(void) RasterSetError(RAS_E_8BIT_RUNLENGTHS_ONLY);
			return(RAS_ERROR);
		  }

		  ras->nx	= dep->nx;
		  ras->ny	= dep->ny;
		  ras->type	= RAS_DIRECT;
		  ras->ncolor	= 0;
		  ras->length	= ras->nx * ras->ny * 3;
		  ras->data	= (unsigned char *) 
					calloc ((unsigned) ras->length, 1);

		  for(length=0, y=0; y<ras->ny; y++)
		  for(x=0; x<ras->nx; x++) {
			if (length == 0) {
		  		status = fread((char *)buf, 1, 4, ras->fp);
				if (status != 4) return(RAS_EOF);
				length = buf[0];
			}
			DIRECT_RED(ras,x,y)	= buf[1];
			DIRECT_GREEN(ras,x,y)	= buf[2];
			DIRECT_BLUE(ras,x,y)	= buf[3];
			length--;
		  }
		  break;
		
		default:
		  (void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
		  return(RAS_ERROR);
		  break;
	}

	return(RAS_OK);
}

Raster *
NrifOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	int		encoding;
{
	Raster		*ras;
	NrifInfo	*dep;

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	ras->dep = calloc(sizeof(NrifInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	dep = (NrifInfo *) ras->dep;

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
	}
	else {
		ras->fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		if (ras->fd == -1) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	(void) strncpy(dep->magic, NRIF_MAGIC, 4);
	dep->encapsulated = False;
	dep->vplot = False;
	dep->nx = nx;
	dep->ny = ny;

	dep->comment = comment;
	if (comment != (char *) NULL) {
		dep->cmtlen = strlen(comment);
	}

	switch (encoding) {
		case RAS_INDEXED:
			dep->encoding = NRIF_INDEXED;
			dep->cbits = 8;
			dep->ibits = 8;
			dep->rbits = 0;
			dep->ncolor = 256;
			dep->enclen = 12 + dep->ncolor * 3;

			ras->nx = dep->nx;
			ras->ny = dep->ny;
			ras->length = ras->nx * ras->ny;
			ras->ncolor = dep->ncolor;
			ras->type = RAS_INDEXED;
			ras->red = (unsigned char *) 
					calloc((unsigned) ras->ncolor, 1);
			ras->green = (unsigned char *) 
					calloc((unsigned) ras->ncolor, 1);
			ras->blue = (unsigned char *) 
					calloc((unsigned) ras->ncolor, 1);
			ras->data = (unsigned char *) 
					calloc((unsigned) ras->length, 1);

			if (dep->encapsulated) {
				dep->objtype = 0;
				dep->control = 0x1000;
				dep->recsize = 8 + NRIF_HEADER_SIZE +
						dep->enclen +
						dep->devlen +
						dep->cmtlen +
						ras->length;
			}
			else {
				dep->recsize = 0;
			}

			break;
		
		case RAS_DIRECT:
			dep->encoding = NRIF_DIRECT;
			dep->cbits = 8;
			dep->ibits = 0;
			dep->rbits = 0;
			dep->ncolor = 0;
			dep->enclen = 4;

			ras->nx = dep->nx;
			ras->ny = dep->ny;
			ras->type = RAS_DIRECT;
			ras->red = (unsigned char *) NULL;
			ras->blue = (unsigned char *) NULL;
			ras->green = (unsigned char *) NULL;
			ras->ncolor = 0;
			ras->length = ras->nx * ras->ny * 3;

			ras->data = (unsigned char *) 
				calloc ((unsigned) ras->length, 1);

			if (dep->encapsulated) {
				dep->objtype = 0;
				dep->control = 0x1000;
				dep->recsize = 8 + NRIF_HEADER_SIZE +
						dep->enclen +
						dep->devlen +
						dep->cmtlen +
						ras->length;
			}
			else {
				dep->recsize = 0;
			}
		
			break;
		
		default:
			(void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
			return( (Raster *) NULL );
			break;
	}

	NrifSetFunctions(ras);

	return(ras);
}

int
NrifClose(ras)
	Raster	*ras;
{
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

int
NrifWrite(ras)
	Raster	*ras;
{
	NrifInfo		*dep;
	unsigned char		buf[1024];
	int			status;

	dep = (NrifInfo *) ras->dep;

	if (dep->encapsulated == TRUE) {
		(void) char_encode((int) dep->objtype, &buf[0], 2);
		(void) char_encode((int) dep->control, &buf[2], 2);
		(void) char_encode((int) dep->recsize, &buf[4], 4);
		status = write(ras->fd, (char *) buf, 8);
		if (status != 8) return(RAS_EOF);
	}

	(void) strncpy((char *) buf, NRIF_MAGIC, 4);
	status = write(ras->fd, (char *) buf, 4);
	if (status != 4) return(RAS_EOF);

	(void) char_encode((int) dep->flags, &buf[0], 4);
	(void) char_encode((int) dep->nx, &buf[4], 4);
	(void) char_encode((int) dep->ny, &buf[8], 4);
	(void) char_encode((int) dep->cmtlen, &buf[12], 4);
	(void) char_encode((int) dep->device, &buf[16], 4);
	(void) char_encode((int) dep->devlen, &buf[20], 4);
	(void) char_encode((int) dep->encoding, &buf[24], 4);
	(void) char_encode((int) dep->enclen, &buf[28], 4);
	status = write(ras->fd, (char *) buf, 32);
	if (status != 32) return(RAS_EOF);


	/* Comment Field */

	if (dep->cmtlen > 0) {
		status = write(ras->fd, dep->comment, (int) dep->cmtlen);
		if (status != dep->cmtlen) return(RAS_EOF);
	}

	/* Device Information (nobody cares) */

	if (dep->devlen > 0) {
		status = write(ras->fd, dep->device_info, (int) dep->devlen);
		if (status != dep->devlen) return(RAS_EOF);
	}

	/* Encoding Information */

	switch(dep->encoding) {
		case NRIF_INDEXED:
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

			break;

		case NRIF_DIRECT:
			(void) char_encode((int) dep->cbits, buf, 4);
			status = write(ras->fd, (char *) buf, 4);
			if (status != 4) return(RAS_EOF);
			status = write(ras->fd, (char *) ras->data,ras->length);
			if (status != ras->length) return(RAS_EOF);
			break;
		
		default:
			(void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
			return(RAS_ERROR);
			break;
	}
	return(RAS_OK);
}

static int
read_decode(fp, nbytes)
	FILE	*fp;
	int	nbytes;
{
	int		status;
	unsigned char	buf[4];

	if (nbytes > 4) {
		(void) fprintf(stderr, 
			"%s: Programming error in read_decode()\n",ProgramName);
		exit(1);
	}

	status  = fread( (char *) buf, 1, nbytes, fp);
	if (status != nbytes) return(RAS_EOF);

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
			buf[0] = (value & 0xff000000) >> 24;
			break;
		
		default:
			(void) RasterSetError(RAS_E_INTERNAL_PROGRAMMING);
			return(RAS_ERROR);
			break;
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
	ras->Open      = NrifOpen;
	ras->OpenWrite = NrifOpenWrite;
	ras->Read      = NrifRead;
	ras->Write     = NrifWrite;
	ras->Close     = NrifClose;
	ras->PrintInfo = NrifPrintInfo;
}
