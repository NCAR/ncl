/*
 *	$Id: hdf.c,v 1.22 2008-07-27 03:18:46 haley Exp $
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
 *		Encoding schemes supported are:
 *			* HDF 8-bit images.
 *			* HDF 24-bit images.
 *
 *	Other libraries required:
 *		libdf.a - HDF C library.
 *		
 */
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <ncarg/c.h>
#include <hdf.h>
#include <df.h>
#include <dfgr.h>
#include "ncarg_ras.h"
#include "hdfP.h"
#include "options.h"

#define	TMPFILE	"/hdf.XXXXXX"

static char	*FormatName = "hdf";

int	DFerror;


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
	int		status;
	Raster		*ras;
	HDFInfo		*dep;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "ras_calloc()");
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(HDFInfo),1);
	if (ras->dep == (char *) NULL) {
		(void) ESprintf(E_UNKNOWN, "ras_calloc()");
		return( (Raster *) NULL );
	}
	dep = (HDFInfo *) ras->dep;

	if (!strcmp(name, "stdin")) {
		(void) ESprintf(E_UNKNOWN, "stdin cannot be used with HDF");
		return( (Raster *) NULL );
	}

	ras->name = (char *) ras_calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	/* Insidious bugs unless these initializations are done. */

	(void) DFR8restart();
	(void) DF24restart();

	/* Get initial information on file. Check for 8 and 24 bit formats. */

	status = DFR8getdims(ras->name, (int32 *) &ras->nx, (int32 *) &ras->ny,
				&dep->palette_exists);
	if (status != -1) {
		ras->type = RAS_INDEXED;
		ras->length = ras->nx * ras->ny;
	}
	else {
		status = DF24getdims(ras->name,
					(int32 *) &ras->nx, (int32 *) &ras->ny,
					(int *) &dep->interlace);

		if (status < 0) {
			(void) ESprintf(HDF_ERRNO,
				"HDFOpen(\"%s\") - DF24getdims(%s) failed",
				ras->name, ras->name);
			return( (Raster *) NULL );
		}

		ras->type = RAS_DIRECT;
		ras->length = ras->nx * ras->ny * 3;
		
		switch(dep->interlace) {
			/* Pixel interlacing is supported */
			case HDF_IL_PIXEL:
			break;

			/*
			Scanplane interlacing is not supported in HDF3.1r5.
			*/
			case HDF_IL_SCANPLANE:
			(void) ESprintf(E_UNKNOWN,
			"Scanplane interlace is not supported by HDF3.1r5");
			return( (Raster *) NULL );

			case HDF_IL_SCANLINE:
			(void) ESprintf(E_UNKNOWN,
			"Scan-line interlace not supported by NCAR Graphics");
			return( (Raster *) NULL );

			default:
			(void) ESprintf(E_UNKNOWN,
			"Internal Error - Bogus interlace type for HDF");
			return( (Raster *) NULL );
		}
	}
	

	/* Set the constants associated with the file. */

	if (ras->file_nx == 0) {
		ras->file_nx   = ras->nx;
		ras->file_ny   = ras->ny;
		ras->file_type = ras->type;
	}

	/* Allocate memory for image storage. */
	
	ras->data = (unsigned char *) ras_calloc((unsigned) ras->length, 1);
	if (ras->data == (unsigned char *) NULL) {
		(void) ESprintf(errno, "ras_calloc()");
		return( (Raster *) NULL );
	}

	/* Allocate memory for color table for RAS_INDEXED files. */
	
	if (ras->type == RAS_INDEXED) {
		ras->ncolor = RAS_DEFAULT_NCOLORS;
		ras->red   = (unsigned char *) ras_calloc(RAS_DEFAULT_NCOLORS, 1);
		ras->green = (unsigned char *) ras_calloc(RAS_DEFAULT_NCOLORS, 1);
		ras->blue  = (unsigned char *) ras_calloc(RAS_DEFAULT_NCOLORS, 1);
		if (ras->red   == (unsigned char *) NULL ||
		    ras->green == (unsigned char *) NULL ||
		    ras->blue  == (unsigned char *) NULL) {
			(void) ESprintf(errno, "ras_calloc()");
			return( (Raster *) NULL );
		}
	}
	else if (ras->type == RAS_DIRECT) {
		ras->ncolor = 256 * 256 * 256;
	}

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
	RasterEncoding	encoding;
{
	Raster		*ras;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "HDFOpenWrite(\"%s\")",name);
		return( (Raster *) NULL );
	}

	ras = RasterCreate(nx, ny, encoding);
	if (ras == (Raster *) NULL) return(ras);

	if (!strcmp(name, "stdout")) {
		const char	*s;

		ras->fd = fileno(stdout);

		/*
		 * hdf routines will only write to a disk file
		 */
		s = GetNCARGPath(NGTMPDIR);
		if (! s) {
			(void) ESprintf(E_UNKNOWN,"GetNCARGPath(%s)", NGTMPDIR);
			return( (Raster *) NULL );
		}
		name = ras_malloc (strlen(s) + strlen(TMPFILE) + 1);
		(void) strcpy(name, s);
		(void) strcat(name, TMPFILE);
		(void) mktemp(name);
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

	if (encoding == RAS_DIRECT) {
		DF24setil(HDF_IL_PIXEL);
	}

	(void) HDFSetFunctions(ras);

	return(ras);
}

/*
 * Function:		HDFWrite(ras)
 *
 * Description:		Writes the supplied raster structure to
 *			an HDF file.
 *
 * In Args:		"ras", the raster structure to write.
 *
 * Out Args:		None.
 *
 * Return Values:	RAS_OK or RAS_EOF
 *
 * Side Effects:	The "written" structure element of "ras"
 *			is set to True once a frame is written.
 */
int
HDFWrite(ras)
	Raster	*ras;
{
	static unsigned char	palette[768];
	int			i;
	int			status;
	int			compress;

	if (ras->type == RAS_INDEXED) {

		/* Load the HDF color palette array. */

		for(i=0; i<RAS_DEFAULT_NCOLORS; i++) {
			palette[i*3 + 0] = ras->red[i];
			palette[i*3 + 1] = ras->green[i];
			palette[i*3 + 2] = ras->blue[i];
		}

		/* Set a new palette for every frame write. */

		status = DFR8setpalette(palette);
		if (status < 0) return(RAS_EOF);

		/*
		Set compression option based on current package options.
		Only applies to RIS8 files.
		*/

		if (OptionCompression == RAS_COMPRESS_OFF) {
			compress = 0;
		}
		else if (OptionCompression == RAS_COMPRESS_RLE) {
			compress = DFTAG_RLE;
		}
		else {
			compress = 0;
		}
	}

	/*
	The first image must go out with a putimage(), subsequent images
	with addimage(). ras->written is used for book-keeping.
	*/

	if (!ras->written) {
		if (ras->type == RAS_INDEXED) {
		status = DFR8putimage(ras->name, (VOIDP) ras->data, 
					(int32) ras->nx, (int32) ras->ny,
					(uint16) compress);
		}
		else if (ras->type == RAS_DIRECT) {
		status = DF24putimage(ras->name, (VOIDP) ras->data, 
					(int32) ras->nx, (int32) ras->ny);
		}
			
		if (status < 0) {
			(void) ESprintf(HDF_ERRNO, "HDFWrite()");
			return(RAS_ERROR);
		}
		ras->written = True;
	}
	else {
		switch(ras->type) {

			case RAS_INDEXED:
			status = DFR8addimage(ras->name, (VOIDP) ras->data, 
					(int32) ras->nx, (int32) ras->ny,
					(uint16) compress);
			break;

			case RAS_DIRECT:
			status = DF24addimage(ras->name, (VOIDP) ras->data, 
					(int32) ras->nx, (int32) ras->ny);
		}

		if (status < 0) {
			(void) ESprintf(HDF_ERRNO, "HDFWrite()");
			return(RAS_ERROR);
		}
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
	HDFInfo		*dep;

	dep = (HDFInfo *) ras->dep;

	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "HDF Rasterfile Information\n");
	(void) fprintf(stderr, "--------------------------\n");

	if (ras->type == RAS_INDEXED) {
	(void) fprintf(stderr, "Has color palette: %d\n",dep->palette_exists);
	}

	if (ras->type == RAS_DIRECT) {
	(void) fprintf(stderr, "Interleaving type: %d\n",dep->interlace);
	}

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

		switch(ras->type) {
			case RAS_INDEXED:
				status = DFR8getimage(ras->name, ras->data, 
					ras->nx, ras->ny, pal);
				break;

			case RAS_DIRECT:
				/*
				This isn't tested yet and seems
				to cause different effects in
				different HDF releases.
				*/
#ifdef DEAD
				status = DF24reqil(HDF_IL_PIXEL);
#endif 
				status = DF24getimage(ras->name, 
							(VOIDP) ras->data, 
							(int32) ras->nx,
							(int32) ras->ny);
				break;

			default:
				break;
		}
	}

	/*
	Load a new color table, providing it hasn't been previously forced.
	*/
		
	if (ras->type == RAS_INDEXED && ras->map_forced != True) {
		for(i=0; i<RAS_DEFAULT_NCOLORS; i++) {
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

		if ((buf = ras_malloc (BUFSIZ)) == NULL) {
			(void) ESprintf(errno, "ras_malloc()");
			return(RAS_ERROR);
		}

		if ((tmp_fd = open(ras->name, O_RDONLY)) < 0 ) {
			(void) ESprintf(errno, "open(%s, %d)",
					ras->name, O_RDONLY);
			return(RAS_ERROR);
		}

		while ((status = read(tmp_fd, buf, BUFSIZ)) > 0) {
			if ((status = write(ras->fd, buf, BUFSIZ)) < 0 ) {
				break;
			}
		}
		(void) close(tmp_fd);
		(void) unlink(ras->name);
		(void) ras_free(buf);
        }

	status = GenericClose(ras);

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
