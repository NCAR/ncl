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
#include <raster.h>
#include <ncarv.h>
#include "rasdraw.h"


/*
 *      a global structure that contains values of command line options
 */
static	struct	{
	StringType_	palette;	/* color palette		*/
	BoolType_	quiet;		/* quiet or verbose mode	*/
	} commLineOpt;

/*
 *	the options that we want to have parsed
 */
static	OptDescRec	set_options[] = {
	{"palette", OptSepArg, NULL},
	{"quiet", OptIsArg, "false"},
	{NULL}
};
	
/*
 *	return structure for loading options
 */
static	Option	get_options[] = {
	{"palette", StringType, (unsigned long) &(commLineOpt.palette),
						sizeof (StringType_)
	},
	{"quiet", BoolType, (unsigned long) &(commLineOpt.quiet),
						sizeof (BoolType_)
	},
	{
	NULL
	}
};



main(argc, argv)
	int	argc;
	char	**argv;
{
	Raster	*ras, *RasterOpen();
	Context	*context, *RasDrawOpen();
	int	status;
	unsigned char	default_colors[768];
	char	*pal_name;	/* name of a default color palette	*/
	int	verbose;	/* verbose or quite mode		*/
	int	count;		/* number of image displayed		*/
	char	*program_name;

	int	exit_status = 0;

	program_name = argv[0];

	/*
	 * register the options we're interested in and have them parsed
	 */
	parseOptionTable(&argc, argv, set_options);

	/*
	 * load the options into commLineOpt
	 */
	getOptions((caddr_t) 0, get_options);
	pal_name = commLineOpt.palette;
	verbose = ! commLineOpt.quiet;


	/*
	 * init libraster
	 */
	(void) RasterInit(&argc, argv);
		

	if ((context = RasDrawOpen(&argc, argv, 0)) == (Context *) NULL) {
		perror(program_name);
		exit(1);
	}

	/*
	 * anything left on command line is assumed to be a raster file
	 */
	while (*(++argv)) {

		if ((ras = RasterOpen(*argv, NULL)) == (Raster *) NULL) {
			(void) RasterPrintError(*argv);
			exit_status++;
			continue;	/* skip this file	*/
		}

		/*
		 * load default color palette if one is requested
		 */
		if (pal_name) {
			if (verbose) {
				(void) fprintf(stderr, 
					"Loading palette: %s\n", pal_name);
			}
			if (PaletteRead(pal_name,NULL,default_colors)== RAS_OK){
				RasDrawSetPalette(context, default_colors,
					default_colors + 256,
					default_colors + 512, 
					256);
			}
			else {
				RasterPrintError(pal_name);
				exit_status++;
			}
		}

		if (verbose) {
			(void) fprintf(stderr, 
				"Displaying images from file: %s\n", *argv);
		}

		/*
		 * display all the images in the file
		 */
		count = 0;
		while((status = RasterRead(ras)) == RAS_OK) {
			if (verbose) {
				count++;
				(void) fprintf(stderr,
					"	image number: %d\n",count);
			}

			exit_status += display_image(ras, context);
		}

		(void) RasterClose(ras);

		if (status == RAS_ERROR) {
			RasterPrintError(*argv);
			exit_status++;
		}
	}
	RasDrawClose(context);
	exit(exit_status);
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
static	display_image(ras, context, verbose)
	Raster	*ras;
	Context	*context;
	int	verbose;
{
	static	Raster	*indexed_ras = (Raster *) NULL;
	int	status;

	Raster	*RasterCreate();

	if (ras->type == RAS_INDEXED) {
		(void) RasDraw(ras, context);
	}

	/*
	 * if true color dither to indexed 8-bit image
	 */
	else if (ras->type == RAS_DIRECT) {
		if (! indexed_ras) {	/* alloc memory for indexed image */
			indexed_ras = RasterCreate(ras->nx,ras->ny,RAS_INDEXED);
			if (indexed_ras == (Raster *) NULL) {
				(void) RasterPrintError((char *)NULL);
				return(-1);
			}
		}
		(void) fprintf(stderr, 
			"Warning: dithering 24-bit to 8-bit image\n");

		status = RasterDither(ras, indexed_ras, verbose);

		if (status == RAS_ERROR) {
			(void) RasterPrintError((char *) NULL);
			return(-1);
		}

		(void) RasDraw(indexed_ras, context);
	} 
	else {
		(void) fprintf(stderr, "Error: unknow image format\n");
		return(-1);
	}
	return(0);
}
