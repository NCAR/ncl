/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	raster.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	1/31/91
 *
 *	Description:
 *		This file contains functions that provide
 *		basic operations on the Raster data
 *		structure.
 *		
 */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include "ncarg_ras.h"
#include "options.h"
#include "devices.h"

int	OptionOrientation = RAS_PORTRAIT;
int	OptionCompression = RAS_COMPRESS_OFF;
int	OptionDotsPerInch = 75;
int	OptionDitherPopular = False;
int	OptionDitherColors = 256;
int	OptionDitherBits = 5;

char	*ProgramName;


/**********************************************************************
 *	Function: RasterInit()
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	3/7/91
 *
 *	Description:
 *		RasterInit() takes "argc" and "argv" and pulls
 *		out the name of the program, which is used primarily
 *		by the error routines.
 *
 *		It also destructively parses the options passed
 *		to it, processing and munching those it recognizes
 *		and leaving the rest alone.
 *		
 *********************************************************************/
int
RasterInit(argc, argv)
	int	*argc;
	char	*argv[];
{
	int	i;

	ProgramName = argv[0];

	if (*argc < 2) exit(RAS_OK);

	for(i=1; i<*argc; ) {
		if (!strcmp(argv[i], "-printoptions")) {
			argdel(argc, argv, i);
			RasterPrintOptions();
		}
		else if (!strcmp(argv[i], "-landscape")) {
			OptionOrientation = RAS_LANDSCAPE;
			argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-portrait")) {
			OptionOrientation = RAS_PORTRAIT;
			argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-nocompress")) {
			OptionCompression = RAS_COMPRESS_OFF;
			argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-compress")) {
			OptionCompression = RAS_COMPRESS_RLE;
			argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-rle")) {
			OptionCompression = RAS_COMPRESS_RLE;
			argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-ditherpopular")) {
			OptionDitherPopular = True;
			argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-ditherbits")) {
			if (i >= (*argc-1)) {
				(void) RasterSetError(RAS_E_BAD_OPTION);
				return(RAS_ERROR);
			}
			argdel(argc, argv, i);
			OptionDitherBits = atoi(argv[i]);
			argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-dithercolors")) {
			if (i >= (*argc-1)) {
				(void) RasterSetError(RAS_E_BAD_OPTION);
				return(RAS_ERROR);
			}
			argdel(argc, argv, i);
			OptionDitherColors = atoi(argv[i]);
			argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-dpi")) {
			if (i >= (*argc-1)) {
				(void) RasterSetError(RAS_E_BAD_OPTION);
				return(RAS_ERROR);
			}
			argdel(argc, argv, i);
			OptionDotsPerInch = atoi(argv[i]);
			argdel(argc, argv, i);
		}
		else {
			i++;
		}
	}
	return(RAS_OK);
}

int
RasterPrintOptions()
{
	if (OptionOrientation == RAS_PORTRAIT) {
		fprintf(stderr, "Orientation     = Portrait\n");
	}
	else if (OptionOrientation == RAS_LANDSCAPE) {
		fprintf(stderr, "Orientation     = Landscape\n");
	}

	if (OptionCompression == 0) {
		fprintf(stderr, "Compression     = None\n");
	}
	else if (OptionCompression == 1) {
		fprintf(stderr, "Compression     = Run-length-encoding\n");
	}

		fprintf(stderr, "Dots Per Inch   = %d\n", OptionDotsPerInch);
	if (OptionDitherPopular == 0) {
		fprintf(stderr, "Dithering       = Standard 332\n");
	}
	else {
		fprintf(stderr, "Dithering       = Popularity\n");
	}

		fprintf(stderr, "Dither Bits     = %d\n", OptionDitherBits);
		fprintf(stderr, "Dither Map Size = %d\n", OptionDitherColors);
}

argdel(argc, argv, i)
	int	*argc;
	char	*argv[];
	int	i;
{
	int	arg;

	if (i != 0) {
		if (i != (*argc - 1)) {
			for(arg=i; arg<(*argc - 1); arg++) {
				argv[arg] = argv[arg+1];
			}
		}
		--(*argc);
	}

	return(RAS_OK);
}

/**********************************************************************
 *	Function: RasterOpen()
 *
 *	Description:
 *		RasterOpen() attempts to open a raster file called
 *		"name". If "format" is not NULL, "format" is used
 *		as the assumed format of the image, otherwise
 *		the format is derived from the file extension
 *		contained in "name".
 *
 *		RasterOpen() allocates and returns a (Raster *)
 *		data structure that contains pointers to the
 *		other routines used to access the image file
 *		for reading, writing, and closing.
 *
 *		If you want to add a new format, you must add
 *		some code here, and in RasterOpenWrite().
 *		
 *********************************************************************/
Raster *
RasterOpen(name, format)
	char	*name;
	char	*format;
{
	Raster	*ras = (Raster *) NULL;
	int	i;

	if (name == (char *) NULL) {
		RasterSetError(RAS_E_NULL_NAME);
		return ((Raster *) NULL);
	}

	if (!strcmp(name, "parallax") || !strcmp(name, "Parallax")) {
		format = "parallax";
	}

	if (!strcmp(name, "cleartext") || !strcmp(name, "ClearText")) {
		format = "cleartext";
	}

	if (format == (char *) NULL) {
		format = strrchr(name, '.');
		if (format != (char *) NULL) {
			format++;
		}
		else {
			RasterSetError(RAS_E_NO_FORMAT_SPECIFIED);
			return ((Raster *) NULL);
		}
	}

	for(i=0; i<NumberOfDevices; i++) {
		if (!strcmp(rasdevices[i].name, format)) {
			ras = rasdevices[i].Open(name);
			return(ras);
		}
	}

	RasterSetError(RAS_E_UNKNOWN_FORMAT);
	return( (Raster *) NULL);
}

/**********************************************************************
 *	Function: RasterOpenWrite()
 *
 *	Description:
 *		RasterOpenWrite() attempts to create a file
 *		called "name", for the purpose of writing
 *		imagery into it. The raster file will have
 *		dimensions "nx" by "ny" and, if possible,
 *		the "comment" string will be saved in the
 *		new file's header. The file will be
 *		encoded according to "encoding" (see
 *		ncarg_ras.h) and currently supported types
 *		include RAS_DIRECT and RAS_INDEXED. The format
 *		of the new file will be determined from
 *		the file extension on "name" (e.g. .hdf,
 *		.nrif, .sun, .xwd) unless the argument
 *		"format" is specified, which gives the
 *		file extension for the new file. If you're
 *		letting "name" determine the format,
 *		"format" should be (char *) NULL.
 *
 *		RasterOpenWrite() allocates and returns a (Raster *)
 *		data structure that contains pointers to the
 *		other routines used to access the image file
 *		for reading, writing, and closing.
 *		
 *********************************************************************/
Raster *
RasterOpenWrite(name, nx, ny, comment, encoding, format)
	char	*name;
	int	nx;
	int	ny;
	char	*comment;
	int	encoding;
	char	*format;
{
	Raster	*ras = (Raster *) NULL;
	int	i;

	if (name == (char *) NULL) {
		RasterSetError(RAS_E_NULL_NAME);
		return ((Raster *) NULL);
	}

	if (!strcmp(name, "parallax") || !strcmp(name, "Parallax")) {
		format = "parallax";
	}

	if (!strcmp(name, "cleartext") || !strcmp(name, "ClearText")) {
		format = "cleartext";
	}

	if (format == (char *) NULL) {
		format = strrchr(name, '.');
		if (format != (char *) NULL) {
			format++;
		}
		else {
			RasterSetError(RAS_E_UNKNOWN_FORMAT);
			return ((Raster *) NULL);
		}
	}

	for(i=0; i<NumberOfDevices; i++) {
		if (!strcmp(rasdevices[i].name, format)) {
			ras = rasdevices[i].OpenWrite(name, nx, ny,
						      comment, encoding);
			return(ras);
		}
	}

	RasterSetError(RAS_E_UNKNOWN_FORMAT);
	return( (Raster *) NULL);
}

/**********************************************************************
 *	Function: RasterWrite()
 *
 *	Description:
 *		RasterWrite() writes to the raster file
 *		referenced by "ras" and	was returned from
 *		RasterOpenWrite().
 *
 *		This function really justs passes on the
 *		arguments to the function pointer.
 *		
 *********************************************************************/
int
RasterWrite(ras)
	Raster	*ras;
{
	return(ras->Write(ras));
}

/**********************************************************************
 *	Function: RasterRead()
 *
 *	Description:
 *		RasterRead() attempts to read the next frame
 *		of the image file reference by "ras".
 *
 *		This function really justs passes on the
 *		arguments to the function pointer.
 *		
 *********************************************************************/
int
RasterRead(ras)
	Raster	*ras;
{
	return(ras->Read(ras));
}

/**********************************************************************
 *	Function: RasterClose()
 *
 *	Description:
 *		RasterClose() closes the image file referenced
 *		by "ras" and frees memory allocated to the
 *		data structure.
 *
 *		This function really justs passes on the
 *		arguments to the function pointer.
 *		
 *********************************************************************/
int
RasterClose(ras)
	Raster	*ras;
{
	return(ras->Close(ras));
}


/**********************************************************************
 *	Function: RasterPrintInfo()
 *
 *	Description:
 *		RasterPrintInfo() prints format-independent
 *		information about the raster referenced by
 *		"ras". It then calls a format-dependent
 *		printing routine using the function pointer
 *		contained in "ras".
 *		
 *********************************************************************/
int
RasterPrintInfo(ras)
	Raster	*ras;
{
	fprintf(stderr, "Format-Independent Information\n");
	fprintf(stderr, "------------------------------\n");
	fprintf(stderr, "Name:             %s\n", ras->name);
	fprintf(stderr, "NX:               %d\n", ras->nx);
	fprintf(stderr, "NY:               %d\n", ras->ny);
	fprintf(stderr, "Encoding:         %s\n", raster_encodings[ras->type]);
	if (ras->text != (char *) NULL)
		fprintf(stderr, "Text:             %s\n", ras->text);
	else
		fprintf(stderr, "Text:             %s\n", "No Text");
	fprintf(stderr, "Number of Colors: %d\n", ras->ncolor);

	(void) ras->PrintInfo(ras);
	return(RAS_OK);
}

/**********************************************************************
 *	Function: RasterLoadPalette()
 *
 *	Description:
 *		RasterLoadPalette() attempts to load a color palette 
 *		from file "name" into the raster structure
 *		pointed to by "ras". There is a flag in the
 *		Raster structure called "map_loaded", which
 *		is intended to tell other applications that
 *		a color map has been loaded from disk and
 *		should supercede any map contained in the image
 *		file. An application can, of course, choose
 *		to ignore this and this should not cause
 *		any problems.
 *
 *		The function returns RAS_OK if successful and 
 *		RAS_ERROR or RAS_EOF if something went wrong.
 *		
 *********************************************************************/
RasterLoadPalette(ras, colors)
	Raster		*ras;
	unsigned char	colors[768];
{
	int	i;

	if (ras->map_loaded) {
		RasterSetError(RAS_E_IMPROPER_COLORMAP_LOAD);
		return(RAS_ERROR);
	}

	if (ras->type == RAS_DIRECT) {
		RasterSetError(RAS_E_IMPROPER_COLORMAP_LOAD);
		return(RAS_ERROR);
	}

	for(i=0; i<256; i++) {
		ras->red[i]   = colors[i +   0];
		ras->green[i] = colors[i + 256];
		ras->blue[i]  = colors[i + 512];
	}

	ras->map_loaded = True;
	return(RAS_OK);
}

/**********************************************************************
 *	Function: RasterPrintColors()
 *
 *	Description:
 *		RasterPrintColors() prints the color table,
 *		assuming that there is one for the given
 *		encoding.
 *		
 *********************************************************************/
RasterPrintColors(ras)
	Raster	*ras;
{
	int	i;

	if (ras->type == RAS_INDEXED) {
		for(i=0; i<ras->ncolor; i++) {
			fprintf(stderr, "Index %3d: (%3d,%3d,%3d)\n",
				i, ras->red[i], ras->green[i], ras->blue[i]);
		}
	}
}

/**********************************************************************
 *	Function: RasterCopyColormap()
 *
 *	Description:
 *		RasterCopyColormap() attempts to copy the
 *		colormap from "src" to "dst". This really
 *		only makes sense if both rasters are
 *		RAS_INDEXED color.
 *		
 *********************************************************************/
int
RasterCopyColormap(src, dst)
	Raster	*src;
	Raster	*dst;
{
	int	i;

	if (src->type == RAS_DIRECT || dst->type == RAS_DIRECT) {
	  RasterSetError(RAS_E_IMPROPER_COLORMAP_LOAD);
	  return(RAS_ERROR);
	}

	for(i=0; i<src->ncolor; i++) {
		dst->red[i] = src->red[i];
		dst->green[i] = src->green[i];
		dst->blue[i] = src->blue[i];
	}

	return(RAS_OK);
}

/**********************************************************************
 *	Function: RasterCreate()
 *
 *	Description:
 *		RasterCreate() allocates a Raster data
 *		structure and initializes with appropriate
 *		values and memory for a raster structure
 *		that is "nx" by "ny" and of "encoding",
 *		where "encoding" can be either RAS_DIRECT or RAS_INDEXED.
 *		
 *********************************************************************/
Raster *
RasterCreate(nx, ny, encoding)
	int	nx;
	int	ny;
	int	encoding;
{
	Raster	*ras;
	char	*calloc();

	ras = (Raster *) calloc(sizeof(Raster), 1);

	if (ras == (Raster *) NULL) {
		RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	if (encoding == RAS_INDEXED) {
		ras->nx = nx;
		ras->ny = ny;

		ras->length = ras->nx * ras->ny;
		ras->data = (unsigned char *) calloc((unsigned) ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			RasterSetError(RAS_E_SYSTEM);
			return( (Raster *) NULL );
		}

		ras->red = (unsigned char *) calloc(256, 1);
		ras->green = (unsigned char *) calloc(256, 1);
		ras->blue = (unsigned char *) calloc(256, 1);

		if (ras->red == (unsigned char *) NULL ||
		    ras->green == (unsigned char *) NULL ||
		    ras->blue == (unsigned char *) NULL) {
			RasterSetError(RAS_E_SYSTEM);
			return( (Raster *) NULL );
		}

		ras->text = "Memory raster structure";
		ras->type = RAS_INDEXED; 
		ras->ncolor = 256;
	}
	else if (encoding == RAS_DIRECT) {
		ras->nx = nx;
		ras->ny = ny;

		ras->length = ras->nx * ras->ny * 3;
		ras->data = (unsigned char *) calloc((unsigned)ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			RasterSetError(RAS_E_SYSTEM);
			return( (Raster *) NULL );
		}

		ras->text = "Memory raster structure";
		ras->type = RAS_DIRECT;
		ras->ncolor = 0;
	}

	return(ras);
}

/**********************************************************************
 *	Function: RasterDestroy()
 *
 *	Description:
 *		RasterDestroy() deallocates a Raster data
 *		structure, after freeing memory associated
 *		with it.
 *		
 *********************************************************************/
int
RasterDestroy(ras)
	Raster	*ras;
{
	if (ras != (Raster *) NULL) {
		if (ras->red != (unsigned char *) NULL)
			free( (char *) ras->red);
		if (ras->green != (unsigned char *) NULL)
			free( (char *) ras->green);
		if (ras->blue != (unsigned char *) NULL)
			free( (char *) ras->blue);
		if (ras->data != (unsigned char *) NULL)
			free( (char *) ras->data);

		if (ras != (Raster *) NULL)
			free( (char *) ras);
	}
	return(True);
}
