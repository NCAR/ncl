/*
 *	$Id: hdf.c,v 1.8 1992-03-27 00:19:09 don Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	hdf.c
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
 *		basic file access functions for HDF (Hierarchical
 *		Data Format) from NCSA.
 *
 *		Encoding schemes are limited to:
 *			* 8-bit indexed	color with 8-bit color map values.
 *
 *	Other libraries required:
 *		libdf.a - HDF C library.
 *		
 */
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include "dfgr.h"
#include "ncarg_ras.h"
#include "options.h"

#ifndef	TMPDIR
#define	TMPDIR	"/tmp"
#endif	/* TMPDIR */

#define	TMPFILE	"/hdf.XXXXXX"

static char	*FormatName = "hdf";

extern char	*ProgramName;

extern int	OptionCompression;

extern	char	*calloc();

/**********************************************************************
 *	Function: HDFOpen(name)
 *
 *	Description:
 *		Opens the HDF file "name" and returns Raster
 *		structure for other routines to use.
 *
 *	Returns:
 *		Pointer to Raster structure, to be used
 *		subsequently by other image access routines.
 *		
 *********************************************************************/
Raster *
HDFOpen(name)
	char	*name;
{
	Raster		*ras;
	char		*calloc();
	int		status;
	int		palette_exists;

	ras = (Raster *) calloc(sizeof(Raster), 1);

	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdin")) {
		(void) RasterSetError(RAS_E_NO_STDIN_WITH_HDF);
		return( (Raster *) NULL );
	}

	ras->name = (char *) calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	status = DFR8getdims(ras->name, &ras->nx, &ras->ny, &palette_exists);
	if (status == -1) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}
	
	ras->length = ras->nx * ras->ny;
	
	ras->data = (unsigned char *) calloc((unsigned) ras->length, 1);
	if (ras->data == (unsigned char *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}
	
	ras->red = (unsigned char *) calloc(256, 1);
	ras->green = (unsigned char *) calloc(256, 1);
	ras->blue = (unsigned char *) calloc(256, 1);
	if (ras->red == (unsigned char *) NULL ||
	    ras->green == (unsigned char *) NULL ||
	    ras->blue == (unsigned char *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	ras->type = RAS_INDEXED; 
	ras->ncolor = 256;

	(void) HDFSetFunctions(ras);

	return(ras);
}

/*ARGSUSED*/
Raster *
HDFOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	int		encoding;
{
	Raster		*ras;

	if (name == (char *) NULL) {
		(void) RasterSetError(RAS_E_NULL_NAME);
		return( (Raster *) NULL );
	}

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);

		/*
		 * hdf routines will only write to a disk file
		 */
		name = malloc (strlen(TMPDIR) + strlen(TMPFILE) + 1);
		(void) strcpy(name, TMPDIR);
		(void) strcat(name, TMPFILE);
		(void) mktemp(name);
	}

	ras->name = (char *) calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	ras->written	= False;
	ras->nx		= nx;
	ras->ny		= ny;
	ras->length	= ras->nx * ras->ny;
	ras->ncolor	= 256;
	ras->type	= RAS_INDEXED;
	ras->red	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->green	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->blue	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->data	= (unsigned char *) calloc((unsigned) ras->length, 1);

	if (encoding != RAS_INDEXED) {
		(void) RasterSetError(RAS_E_8BIT_PIXELS_ONLY);
		return( (Raster *) NULL );
	}
	else {
		ras->type = RAS_INDEXED;
	}

	(void) HDFSetFunctions(ras);

	return(ras);
}

int
HDFWrite(ras)
	Raster	*ras;
{
	static unsigned char	palette[768];
	int			i;
	int			status;
	int			compress;

	for(i=0; i<256; i++) {
		palette[i*3 + 0] = ras->red[i];
		palette[i*3 + 1] = ras->green[i];
		palette[i*3 + 2] = ras->blue[i];
	}

	if (OptionCompression == RAS_COMPRESS_OFF) {
		compress = 0;
	}
	else if (OptionCompression == RAS_COMPRESS_RLE) {
		compress = DFTAG_RLE;
	}
	else {
		compress = 0;
	}

	if (!ras->written) {
		status = DFR8setpalette(palette);
		if (status == -1) return(RAS_EOF);
		
		status = DFR8putimage(ras->name, ras->data, 
					ras->nx, ras->ny, compress);
			
		if (status == -1) return(RAS_EOF);
		ras->written = True;
	}
	else {
		status = DFR8addimage(ras->name, ras->data, 
					ras->nx, ras->ny, compress);
		if (status == -1) return(RAS_EOF);
	}
	return(RAS_OK);
}


/**********************************************************************
 *	Function: HDFPrintInfo()
 *
 *	Description:
 *		HDFPrintInfo() prints textual information
 *		about "ras" on stderr. The return code
 *		is always zero.
 *		
 *********************************************************************/
/*ARGSUSED*/
int
HDFPrintInfo(ras)
	Raster		*ras;
{
	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "HDF Rasterfile Information\n");
	(void) fprintf(stderr, "--------------------------\n");
	return(RAS_OK);
}

int
HDFRead(ras)
	Raster		*ras;
{
	static unsigned char	pal[768];
	int		i;
	int		retry;
	int		status;

	for(retry = 0, status = EOF; status == EOF; retry++) {
		if (retry == 4) return(RAS_EOF);

#ifdef DEAD
		if (retry != 0)
			(void) fprintf(stderr, "Retrying HDF Read\n");
#endif /* DEAD */

		status = DFR8getimage(ras->name, ras->data, 
				ras->nx, ras->ny, pal);

		if (status != 0) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return(RAS_ERROR);
		}
	}
		
	if (ras->map_loaded == False) {
		for(i=0; i<256; i++) {
			ras->red[i]   = pal[i*3 + 0];
			ras->green[i] = pal[i*3 + 1];
			ras->blue[i]  = pal[i*3 + 2];
		}
	}

	return(RAS_OK);
}

/**********************************************************************
 *	Function: HDFClose()
 *
 *	Description:
 *		HDFClose() closes open file descriptors
 *		and frees memory allocated to "ras".
 *		
 *********************************************************************/
int
HDFClose(ras)
	Raster		*ras;
{
	int	status = RAS_OK;

	/*
	 * if we're supposed to be writing to stdout cat contents of tmp 
	 * file to stdout.
	 */
	if (ras->fd == fileno(stdout)) {
		char	*buf;
		int	tmp_fd;

		if ((buf = malloc (BUFSIZ)) == NULL) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return(RAS_ERROR);
		}

		if ((tmp_fd = open(ras->name, O_RDONLY)) < 0 ) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return(RAS_ERROR);
		}

		while ((status = read(tmp_fd, buf, BUFSIZ)) > 0) {
			if ((status = write(ras->fd, buf, BUFSIZ)) < 0 ) {
				break;
			}
		}
		(void) close(tmp_fd);
		(void) unlink(ras->name);
		(void) free(buf);
        }

	free( (char *) ras->data);
	free( (char *) ras->red);
	free( (char *) ras->green);
	free( (char *) ras->blue);
	if ( ras->dep != (char *) NULL ) free( (char *) ras->dep);

	return(status);
}

int
HDFSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = HDFOpen;
	ras->OpenWrite = HDFOpenWrite;
	ras->Read      = HDFRead;
	ras->Write     = HDFWrite;
	ras->Close     = HDFClose;
	ras->PrintInfo = HDFPrintInfo;
	ras->ImageCount = ImageCount_;
	return(RAS_OK);
}
