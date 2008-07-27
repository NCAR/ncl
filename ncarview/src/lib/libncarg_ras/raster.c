/*
 *	$Id: raster.c,v 1.32 2008-07-27 03:18:46 haley Exp $
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
#include <errno.h>
#include <ncarg/c.h>
#include "ncarg_ras.h"
#include "raster.h"
#include "options.h"
#include "devices.h"

int	OptionOrientation = RAS_PORTRAIT;
int	OptionCompression = RAS_COMPRESS_RLE;
int	OptionDotsPerInch = 75;
int	OptionDitherPopular = False;
int	OptionDitherColors = 256;
int	OptionDitherBits = 5;
int	OptionInX = 0;
int	OptionInY = 0;
int	OptionInInvert = False;
int	OptionIndexed = False;
int	OptionInOffset = 0;
int	OptionOutCMap = 0;

char	*NrtProgramName;

static int	argdel();

RasterDevice *_RasterGetDevice(
#ifdef NeedFuncProto
	const char	*format
#endif
	);

/*LINTLIBRARY*/


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
	extern int	ras_nerr;
	extern char	**ras_errlist;

	NrtProgramName = argv[0];

	(void) RasterInitError();

	for(i=1; i<*argc; ) {
		if (!strcmp(argv[i], "-printoptions")) {
			(void) argdel(argc, argv, i);
			RasterPrintOptions();
		}
		else if (!strcmp(argv[i], "-landscape")) {
			OptionOrientation = RAS_LANDSCAPE;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-portrait")) {
			OptionOrientation = RAS_PORTRAIT;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-nocompress")) {
			OptionCompression = RAS_COMPRESS_OFF;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-compress")) {
			OptionCompression = RAS_COMPRESS_RLE;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-rle")) {
			OptionCompression = RAS_COMPRESS_RLE;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-ditherpopular")) {
			OptionDitherPopular = True;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-ditherbits")) {
			if (i >= (*argc-1)) {
				(void) ESprintf(RAS_E_NO_OPTION_PARM,
					"%s", argv[i]);
				return(RAS_ERROR);
			}
			(void) argdel(argc, argv, i);
			OptionDitherBits = atoi(argv[i]);
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-dithercolors")) {
			if (i >= (*argc-1)) {
				(void) ESprintf(RAS_E_NO_OPTION_PARM,
					"%s", argv[i]);
				return(RAS_ERROR);
			}
			(void) argdel(argc, argv, i);
			OptionDitherColors = atoi(argv[i]);
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-dpi")) {
			if (i >= (*argc-1)) {
				(void) ESprintf(RAS_E_NO_OPTION_PARM,
					"%s", argv[i]);
				return(RAS_ERROR);
			}
			(void) argdel(argc, argv, i);
			OptionDotsPerInch = atoi(argv[i]);
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-inx")) {
			if (i >= (*argc-1)) {
				(void) ESprintf(RAS_E_NO_OPTION_PARM,
					"%s", argv[i]);
				return(RAS_ERROR);
			}
			(void) argdel(argc, argv, i);
			OptionInX = atoi(argv[i]);
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-iny")) {
			if (i >= (*argc-1)) {
				(void) ESprintf(RAS_E_NO_OPTION_PARM,
					"%s", argv[i]);
				return(RAS_ERROR);
			}
			(void) argdel(argc, argv, i);
			OptionInY = atoi(argv[i]);
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-in_invert")) {
			OptionInInvert = True;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-indexed")) {
			OptionIndexed = True;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-outcmap")) {
			OptionOutCMap = True;
			(void) argdel(argc, argv, i);
		}
		else if (!strcmp(argv[i], "-inoffset")) {
			if (i >= (*argc-1)) {
				(void) ESprintf(RAS_E_NO_OPTION_PARM,
					"%s", argv[i]);
				return(RAS_ERROR);
			}
			(void) argdel(argc, argv, i);
			OptionInOffset = atoi(argv[i]);
			(void) argdel(argc, argv, i);
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
		(void) fprintf(stderr, "Orientation     = Portrait\n");
	}
	else if (OptionOrientation == RAS_LANDSCAPE) {
		(void) fprintf(stderr, "Orientation     = Landscape\n");
	}

	if (OptionCompression == 0) {
		(void) fprintf(stderr, "Compression     = None\n");
	}
	else if (OptionCompression == 1) {
		(void) fprintf(stderr, 
			"Compression     = Run-length-encoding\n");
	}

		(void) fprintf(stderr, 
			"Dots Per Inch   = %d\n", OptionDotsPerInch);
	if (OptionDitherPopular == 0) {
		(void) fprintf(stderr, "Dithering       = Standard 332\n");
	}
	else {
		(void) fprintf(stderr, "Dithering       = Popularity\n");
	}

		(void) fprintf(stderr, 
			"Dither Bits     = %d\n", OptionDitherBits);
		(void) fprintf(stderr, 
			"Dither Map Size = %d\n", OptionDitherColors);
}

static int
argdel(argc, argv, i)
	int	*argc;
	char	*argv[];
	int	i;
{
	int	arg;

	if (i != 0) {
		if (i != (*argc - 1)) {
			for(arg=i; arg<(*argc - 1); arg++) {
				argv[arg]   = argv[arg+1];
				argv[arg+1] = (char *) NULL;
			}
		}
		else {
			argv[i] = (char *) NULL;
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
	Raster		*ras = (Raster *) NULL;
	RasterDevice	*rasdev;
	const char	*locformat;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "RasterOpen(\"%s\")", name);
		return ((Raster *) NULL);
	}
	
	if (format == (char *) NULL) {
		locformat = RasterGetFormatFromName(name);
	}
	else {
		locformat = format;
	}

	if (locformat == (char *) NULL) {
		(void) ESprintf(RAS_E_NO_FORMAT_SPECIFIED,
				"RasterOpen(\"%s\")", name);
			return ((Raster *) NULL);
	}

	rasdev = _RasterGetDevice(locformat);
	if (rasdev == (RasterDevice *) NULL) {
		return((Raster *) NULL);
	}

	ras = rasdev->Open(name);
	return(ras);
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
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
	char		*format;
{
	Raster	*ras = (Raster *) NULL;
	RasterDevice	*rasdev;
	const char	*locformat;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME,"RasterOpenWrite(\"%s\")",name);
		return ((Raster *) NULL);
	}
	
	if (format == (char *) NULL) {
		locformat = RasterGetFormatFromName(name);
	}
	else {
		locformat = format;
	}

	if (locformat == (char *) NULL) {
		(void) ESprintf(RAS_E_NO_FORMAT_SPECIFIED,
				"RasterOpen(\"%s\")", name);
			return ((Raster *) NULL);
	}

	rasdev = _RasterGetDevice(locformat);
	if (rasdev == (RasterDevice *) NULL) {
		return((Raster *) NULL);
	}

	ras = rasdev->OpenWrite(name, nx, ny, comment, encoding);
	return(ras);
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
	if (ras == (Raster *) NULL) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterClose() - NULL ras structure\n");
		return(RAS_ERROR);
	}
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
	(void) fprintf(stderr, "\n**** %s ****\n", ras->name);
	(void) fprintf(stderr, "Format-Independent Information\n");
	(void) fprintf(stderr, "------------------------------\n");
	(void) fprintf(stderr, "NX:               %d\n", ras->nx);
	(void) fprintf(stderr, "NY:               %d\n", ras->ny);
	(void) fprintf(stderr, 
		"Encoding:         %s\n", raster_encodings[ras->type]);
	(void) fprintf(stderr, "Image Length:     %d\n", ras->length);

	if (ras->text != (char *) NULL)
		(void) fprintf(stderr, "Text:             %s\n", ras->text);
	else
		(void) fprintf(stderr, "Text:             %s\n", "No Text");
	(void) fprintf(stderr, "Number of Colors: %d\n", ras->ncolor);

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
 *		Raster structure called "map_forced", which
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

	if (ras->map_forced) {
		(void) ESprintf(RAS_E_IMPROPER_COLORMAP_LOAD,
			"Illegal colormap reload");
		return(RAS_ERROR);
	}

	if (ras->type == RAS_DIRECT) {
		(void) ESprintf(RAS_E_IMPROPER_COLORMAP_LOAD,
			"You can't load a colormap for a direct-color image");
		return(RAS_ERROR);
	}

	for(i=0; i<256; i++) {
		ras->red[i]   = colors[i +   0];
		ras->green[i] = colors[i + 256];
		ras->blue[i]  = colors[i + 512];
	}

	ras->map_forced = True;
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
			(void) fprintf(stderr, "Index %3d: (%3d,%3d,%3d)\n",
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
	  (void) ESprintf(RAS_E_IMPROPER_COLORMAP_LOAD,
		"You can't load a colormap for a direct-color image");
	  return(RAS_ERROR);
	}

	if (dst->map_forced) {
	  (void) ESprintf(RAS_E_IMPROPER_COLORMAP_LOAD,
		"Attempt to reload a color map for a rasterfile");
	  return(RAS_OK);
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
	int		nx;
	int		ny;
	RasterEncoding	encoding;
{
	Raster		*ras;
	char		*comment = "Memory raster structure";
	static int	count = 0;
	char		buf[32];

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);

	if (ras == (Raster *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "RasterCreate()");
		return( (Raster *) NULL );
	}

	ras->text = (char *)ras_calloc((unsigned)(strlen(comment)+1),1);
	(void) strcpy(ras->text, comment);

	(void) sprintf(buf, "memory.%d", ++count);
	ras->name = (char *)ras_calloc((unsigned)(strlen(buf)+1),1);
	(void) strcpy(ras->name, buf);

	if (encoding == RAS_INDEXED) {
		ras->written = False;
		ras->nx      = nx;
		ras->ny      = ny;
		ras->length  = ras->nx * ras->ny;
		ras->ncolor  = RAS_DEFAULT_NCOLORS;
		ras->type    = RAS_INDEXED;

		ras->red  =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->green=(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->blue =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);

		if (ras->red == (unsigned char *) NULL ||
		    ras->green == (unsigned char *) NULL ||
		    ras->blue == (unsigned char *) NULL) {
			(void) ESprintf(errno, "RasterCreate()");
			return( (Raster *) NULL );
		}

		ras->data=(unsigned char *)ras_calloc((unsigned)ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "RasterCreate()");
			return( (Raster *) NULL );
		}
	}
	else if (encoding == RAS_DIRECT) {
		ras->written = False;
		ras->nx      = nx;
		ras->ny      = ny;
		ras->length  = ras->nx * ras->ny * 3;
		ras->ncolor  = 256 * 256 * 256;
		ras->type    = RAS_DIRECT;

		ras->data =(unsigned char *)ras_calloc((unsigned)ras->length,1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "RasterCreate()");
			return( (Raster *) NULL );
		}
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
			ras_free( (char *) ras->red);
		if (ras->green != (unsigned char *) NULL)
			ras_free( (char *) ras->green);
		if (ras->blue != (unsigned char *) NULL)
			ras_free( (char *) ras->blue);
		if (ras->data != (unsigned char *) NULL)
			ras_free( (char *) ras->data);

		if (ras != (Raster *) NULL)
			ras_free( (char *) ras);
	}
	return(True);
}

/*
 *
 *	RasterStat()
 *
 *	RasterStat() obtains information about the raster file named by path.
 *	Read permission of the named file is required.
 * 
 *	If "format" is not NULL, "format" is used
 *	as the assumed format of the image, otherwise
 *	the format is derived from the file extension
 *	contained in "path". 
 *
 *	The structure referenced by 'ras_stat' is filled in with information
 *	about the file.
 *
 *	If 'icount' is non-null the number of images contained in 
 *	the file are stored at the address referenced by 'icount'. Warning!
 *	counting images can take a long time.
 *
 * on entry
 *	*path		: path name to a raster file.
 *	*format		: optional format specifier for file referenced
 *			  by 'path'
 *
 * on exit
 *	*ras_stat	: file status
 *	*icount		: contains image count if non-null on entry.
 *	return		: -1 => failure and ESprintf() is invoked.
 *
 */
int	RasterStat(path, format, ras_stat, icount)
	char		*path;
	char		*format;
	RasStat		*ras_stat;
	int		*icount;
{

	Raster	*ras;
	int	rc;

	if (strcmp(path, "stdin") == 0) {
		if (fstat(fileno(stdin), &(ras_stat->stat)) < 0) {
			ESprintf(errno, "fstat(%d, )", fileno(stdin));
			return(RAS_ERROR);
		}
	}
	else {
		if (stat(path, &(ras_stat->stat)) < 0) {
			ESprintf(errno, "stat(\"%s\", )", path);
			return(RAS_ERROR);
		}
	}

	if ((ras = RasterOpen(path, format)) == (Raster *) NULL) {
		return(RAS_ERROR);
	}

	rc = RasterRead(ras);
	if (rc == RAS_ERROR) {
		return(RAS_ERROR);
	}
	else if (rc == RAS_EOF) {
		ESprintf(E_UNKNOWN, "RasterStat() - Unexpected EOF");
		RasterClose(ras);
		return(RAS_ERROR);
	}

	ras_stat->type = ras->type;
	ras_stat->nx = ras->nx;
	ras_stat->ny = ras->ny;


	/*
	 * get frame count if 'icount' is not NULL
	 */
	if (icount) {
		if ((*icount = ras->ImageCount(path,format)) < 0) {
			RasterClose(ras);
			return(RAS_ERROR);
		}
	}
	(void) RasterClose(ras);
	return(RAS_OK);
}

char
*RasterTypeString(encoding)
	RasterEncoding	encoding;
{
	return(raster_encodings[encoding]);
}

#ifdef __STDC__
int RasterGetValues(Raster *ras, ...)
#else
int RasterGetValues(ras, va_alist)
	Raster	*ras;
	va_dcl
#endif
{
	va_list		ap;
	char		*_RasterGetFormatDesc();
	char		*resource;
	char		**svalue;	/* String Value */

#ifdef __STDC__
	va_start(ap, ras);
#else
	va_start(ap);
#endif

	if (ras == (Raster *) NULL) {
		(void) ESprintf(RAS_E_PROGRAMMING, "RasterGetValues()");
		return(RAS_ERROR);
	}

	while( (resource = va_arg(ap, char *)) != (char *) NULL) {

		if (!strcmp(resource, NrtNformatName)) {
			svalue = va_arg(ap, char **);
			*svalue = NmuStrdup(ras->format);
		}
		else if (!strcmp(resource, NrtNformatDesc)) {
			svalue = va_arg(ap, char **);
			*svalue = NmuStrdup(_RasterGetFormatDesc(ras->format));
		}
		else {
			(void) ESprintf(RAS_E_UNKNOWN_RESOURCE,
				"RasterGetValues(%s)", resource);
			return(RAS_ERROR);
		}
	}
	return(RAS_OK);
}

const char *
RasterGetFormatFromName(name)
	const char	*name;
{
	char	*format = (char *) NULL;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME,
			"RasterGetFormatFromName(NULL)");
		return((char *) NULL);
	}

	if (!strcmp(name, "parallax") || !strcmp(name, "Parallax")) {
		format = "parallax";
	}
	else if (!strcmp(name, "cleartext") || !strcmp(name, "ClearText")) {
		format = "cleartext";
	}
	else {
		format = strrchr(name, '.');
		if (format != (char *) NULL) {
			format++;
		}
	}

	return(format);
}

/********************** Private Functions **************************/

RasterDevice *
_RasterGetDevice(format)
	const char	*format;
{
	int	i;

	for(i=0; i<NumberOfDevices; i++) {
		if (!strcmp(rasdevices[i].name, format)) {
			return(&rasdevices[i]);
		}
	}

	(void) ESprintf(RAS_E_UNKNOWN_FORMAT, "\"%s\"", format);
	return( (RasterDevice *) NULL );
}

char *
_RasterGetFormatDesc(format)
	char	*format;
{
	int	i;

	for(i=0; i<NumberOfDevices; i++) {
		if (!strcmp(rasdevices[i].name, format)) {
			return(rasdevices[i].description);
		}
	}
	return((char *) NULL);
}
