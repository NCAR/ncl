/*
 *	$Id: rasview.c,v 1.16 1994-04-14 18:45:24 clyne Exp $
 */
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
	char	*ifmt;		/* input format			*/
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
	{"ifmt", 1, NULL, "Input format"},
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
	{"ifmt", NCARGCvtToString, (Voidptr)&opt.ifmt,sizeof(opt.ifmt)
	},
	{"", NCARGCvtToBoolean, (Voidptr) &opt.not_used, sizeof(opt.not_used)
	},
	{
	NULL
	}
};

static	int	oD;
static	char	*progName;

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
main(argc, argv)
	int	argc;
	char	**argv;
{
	Raster	*ras;
	Context	*context;
	int	status;
	unsigned char	default_colors[768];
	char	*pal_name;	/* name of a default color palette	*/
	int	verbose;	/* verbose or quite mode		*/
	int	count;		/* number of image displayed		*/
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
	verbose = ! opt.quiet;

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
			exit_status++;
			files++;
			continue;	/* skip this file	*/
		}

		if (verbose) {
			(void) fprintf(stderr, 
				"Displaying images from file: %s\n", *files);
		}

		/*
		 * display all the images in the file
		 */
		count = 0;
		for (;;) {
			if ((status = RasterRead(ras)) != RAS_OK) {
				break;
			}
			if (verbose) {
				count++;
				(void) fprintf(stderr,
					"	image number: %d\n",count);
			}

			exit_status += display_image(ras, context, verbose);
		}

		(void) RasterClose(ras);

		if (status == RAS_ERROR) {
			(void) fprintf(
				stderr, 
				"%s: Couldn't read rasterfile(%s) [ %s ]\n",
				progName, *files, ErrGetMsg()
			);
			exit_status++;
		}
		files++;
	}
	RasDrawClose(context);
	exit(exit_status);
}

