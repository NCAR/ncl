/*
 *	$Id: rasview.c,v 1.22 2008-07-27 03:18:41 haley Exp $
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

/*
 *	rasview.c
 *
 *	Author		John Clyne
 *
 *	Date		Fri Mar  1 14:58:27 MST 1991
 *
 *	Display a raster file to an X window. 
 */
#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>     /* Get standard string definations. */
#include <X11/Shell.h>
#include <ncarg/c.h>
#include <ncarg/ncarg_ras.h>
#include "rasdraw.h"



/*
 *      a global structure that contains values of command line options
 */
static	struct	{
	char	*palette;	/* color palette		*/
	boolean	quiet;		/* quiet or verbose mode	*/
	boolean	version;	/* print version		*/
	boolean movie;		/* movie mode			*/
	boolean	loop;		/* animate forever?		*/
	char	*ifmt;		/* input format			*/
	int	scale;		/* image scale factor		*/
	boolean	not_used;	/* "-" option, we toss it	*/
	} opt;

/*
 *	the options that we want to have parsed
 */
static	OptDescRec	set_options[] = {
	{"palette", 1, NULL, "Specify a color palette file"},
	{"quiet", 0, NULL, "Operate in quite mode"},
	{"Version", 0, NULL, "Print version number end exit"},
	{"movie", 0, NULL, "Display frames in movie mode"},
	{"loop", 0, NULL, "Display frames endlessly"},
	{"ifmt", 1, NULL, "Input format"},
	{"scale", 1, "1", "Integral, image scaling factor"},
        {"", 0, NULL, "Read rasterfile from the standard input"},
	{NULL}
};
	
/*
 *	return structure for loading options
 */
static	Option	get_options[] = {
	{"palette", NCARGCvtToString, (Voidptr)&opt.palette, sizeof(opt.palette)
	},
	{"quiet", NCARGCvtToBoolean, (Voidptr) &opt.quiet, sizeof(opt.quiet)
	},
	{"Version", NCARGCvtToBoolean, (Voidptr)&opt.version,sizeof(opt.version)
	},
	{"movie", NCARGCvtToBoolean, (Voidptr)&opt.movie,sizeof(opt.movie)
	},
	{"loop", NCARGCvtToBoolean, (Voidptr)&opt.loop,sizeof(opt.loop)
	},
	{"ifmt", NCARGCvtToString, (Voidptr)&opt.ifmt,sizeof(opt.ifmt)
	},
	{"scale", NCARGCvtToInt, (Voidptr)&opt.scale,sizeof(opt.scale)
	},
	{"", NCARGCvtToBoolean, (Voidptr) &opt.not_used, sizeof(opt.not_used)
	},
	{
	NULL
	}
};

static	int	oD;
static	char	*progName;

Raster	*raster_scale(
	Raster	*src, 
	int	scale
) {
	int		dst_nx;
	int		dst_ny;
	unsigned char	*src_data, *dst_data;
	unsigned char	*cptr;
	int		x,y;
	int		row_i, col_i;
	int		row_len;

	static	Raster	*dst = NULL;

	dst_nx = src->nx * scale;
	dst_ny = src->ny * scale;

	if (! dst) {
		dst = RasterCreate(dst_nx, dst_ny, src->type);
		if (! dst) { 
			(void) fprintf (
				stderr, "%s: RasterCreate(%d,%d) [ %s ]\n",
				progName, dst_nx, dst_ny, ErrGetMsg()
			);
			return(NULL);
		}
	}

	if (src->type == RAS_INDEXED) { 
		RasterCopyColormap(src, dst);
		row_len = src->nx;
	}
	else {
		row_len = src->nx * 3;
	}

	src_data = src->data;
	dst_data = dst->data;
	cptr = src_data;
	for(y=0; y<dst->ny; y++) {

		row_i = y % scale;

		for(x=0; x<dst->nx; x++) {

			col_i = x % scale;

			if (src->type == RAS_INDEXED) {


				*dst_data++ = *src_data;

				if (col_i == (scale - 1)) src_data++;
			}
			else {

				*dst_data++ = src_data[0];
				*dst_data++ = src_data[1];
				*dst_data++ = src_data[2];

				if (col_i == (scale - 1)) src_data+=3;
			}

		}
		if (row_i == (scale - 1)) {
			cptr += row_len;
		}
		src_data = cptr;
	}

	return(dst);
	
}
/*
 *	display_image
 *
 *	display an image. If the image is Direct colour format dither it
 *	to an 8-bit image.
 *
 * on entry
 *	*ras		: contains image to display
 *	*context	: context returned by RasDrawOpen()
 *	verbose		: verbose or quiet mode?
 */
static	int	display_image(ras, context, verbose)
	Raster	*ras;
	Context	*context;
	int	verbose;
{
	static	Raster	*indexed_ras = (Raster *) NULL;
	int	status;

	if (context->encoding == RASDRAW_0BIT) {
		(void) fprintf(
			stderr, 
			"%s : Unsupported display depth - Only 8 and 24 bit frame buffers supported\n", 
			progName
		);
		return(-1);
	}

	if (ras->type == RAS_INDEXED && (context->encoding & RASDRAW_8BIT)) {
		(void) RasDraw(ras, context);
	}
	else if(ras->type == RAS_DIRECT && (context->encoding & RASDRAW_24BIT)){
		(void) RasDraw(ras, context);
	}
	/*
	 * if true color dither to indexed 8-bit image
	 */
	else if (ras->type == RAS_DIRECT && context->encoding & RASDRAW_8BIT) {
		if (! indexed_ras) {	/* alloc memory for indexed image */
			indexed_ras = RasterCreate(ras->nx,ras->ny,RAS_INDEXED);
			if (indexed_ras == (Raster *) NULL) {
				(void) fprintf(
					stderr, "%s: Allocating memory, [ %s ]\n",
					progName, ErrGetMsg()
				);
				return(-1);
			}
		}
		(void) fprintf(stderr, 
			"Warning: dithering 24-bit to 8-bit image\n");

		status = RasterDither(ras, indexed_ras, verbose);

		if (status == RAS_ERROR) {
			(void) fprintf(
				stderr, "%s: Quantizing imagery, [ %s ]\n",
				progName, ErrGetMsg()
			);
			return(-1);
		}

		(void) RasDraw(indexed_ras, context);
	} 
	else {
		(void) fprintf(stderr, "%s : Unknown image format\n", progName);
		return(-1);
	}
	return(0);
}

static	void	usage(progName, message) 
	char	*progName;
	char	*message;
{

	if (message) {
		(void) fprintf(stderr, "%s: %s", progName, message);
	}

	(void) fprintf(stderr, 
		"%s: Usage: %s [-Version] [-pal palette_file] [-quiet] [raster_file...]\n",
		progName, progName);
	PrintOptionHelp(oD, stderr);

	exit(1);
}


static int	display(
	char	**files,
	int	scale,
	Context	*context,
	FILE	*verbose
) {
	int	status = 0;
	int	count = 0;		/* number of images displayed	*/
	Raster	*ras;
	/*
	 * anything left on command line is assumed to be a raster file
	 */
	while(*files)
	{

		if ((ras = RasterOpen(*files, opt.ifmt)) == (Raster *) NULL){
			(void) fprintf (
				stderr, "%s: RasterOpen(%s,%s) [ %s ]\n",
				progName, *files, 
				opt.ifmt ? opt.ifmt : "NULL", ErrGetMsg()
			);
			status = -1;
			files++;
			continue;	/* skip this file	*/
		}

		if (verbose) {
			(void) fprintf(verbose, 
				"Displaying images from file: %s\n", *files);
		}

		/*
		 * display all the images in the file
		 */
		count = 0;
		for (;;) {
			Raster	*rasptr;

			if ((status = RasterRead(ras)) != RAS_OK) {
				break;
			}

			rasptr = ras;

			if (opt.scale > 1) {
				rasptr = raster_scale(ras, opt.scale);
				if (! rasptr) exit(1);
			}

			if (verbose) {
				count++;
				(void) fprintf(verbose,
					"	image number: %d\n",count);
			}


			if (display_image(rasptr, context, verbose ? 1 : 0) < 0) {
				status = -1;
			}
		}

		(void) RasterClose(ras);

		if (status == RAS_ERROR) {
			(void) fprintf(
				stderr, 
				"%s: Couldn't read rasterfile(%s) [ %s ]\n",
				progName, *files, ErrGetMsg()
			);
			status = -1;
		}
		files++;
	}
	return(0);
}


main(argc, argv)
	int	argc;
	char	**argv;
{
	Context	*context;
	int	status;
	unsigned char	default_colors[768];
	char	*pal_name;	/* name of a default color palette	*/
	FILE	*verbose;	/* verbose or quite mode		*/
	int	i;
	char	**files;

	int	exit_status = 0;

	progName = argv[0];

	files = (char **) malloc((argc + 1) * sizeof(char *));

	/*
	 * register the options we're interested in and have them parsed
	 */
	oD = OpenOptionTbl();
	if (ParseOptionTable(oD, &argc, argv, set_options) < 0) {
		fprintf(
			stderr,"%s : Error parsing command line options : %s\n",
			progName, ErrGetMsg()
		);
		exit(1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(oD, get_options) < 0) {
		fprintf(
			stderr,"%s : GetOptions(,) : %s\n",
			progName,ErrGetMsg()
		);
		exit(1);
	}
	pal_name = opt.palette;
	if (opt.quiet) verbose = NULL;
	else verbose = stderr;

	if (opt.version) {
		PrintVersion(progName);
		exit(0);
	}


	/*
	 * init libraster
	 */
	(void) RasterInit(&argc, argv);

	if ((context = RasDrawOpen(&argc, argv, opt.movie))==(Context *)NULL) {
		fprintf(
			stderr, "%s : Error initializing display : %s\n",
			progName, ErrGetMsg()
		);
		exit(1);
	}

	/*
	 * make sure nothing left on command line execpt file names
	 */
	argv++; argc--;
	for (i=0; i<argc; i++) {
		if (*argv[i] == '-') usage(progName, (char *) NULL);
		files[i] = argv[i];
	}
	files[i] = NULL;

	/*
	 * if no files read from stdin
	 */
	if (files[0] == NULL) {
		files[0] = "stdin";
		files[1] = NULL;
	} 

	/*
	 * load default palette if one was requested.
	 */
	if (pal_name) {
		if (PaletteRead(pal_name,NULL,default_colors)== RAS_OK){
			(void) RasDrawSetPalette(context, default_colors,
				default_colors + 256,
				default_colors + 512, 
				256);
		}
		else {
			(void) fprintf (
				stderr, 
				"%s: Couldn't read palette file(%s) [ %s ]\n",
				progName, pal_name, ErrGetMsg()
			);
			
			exit_status++;
		}
	}

	if (opt.loop) {
		while (1) {
			if (display(files, opt.scale, context, verbose) < 0) {
				exit_status++;
			}
		}
	}
	else {
		if (display(files, opt.scale, context, verbose) < 0) {
			exit_status++;
		}
	}

	RasDrawClose(context);
	exit(exit_status);
}

