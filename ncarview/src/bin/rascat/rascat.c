
/*
 *      $Id: rascat.c,v 1.3 1992-03-26 19:31:49 clyne Exp $
 */
/*
 *	File:		rascat.c
 *
 *	Author:		Don Middleton-Link
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Rev 1.1		John Clyne
 *
 *	Date:		Wed Feb 12 20:12:58 MST 1992
 *
 *	Description:	Read raster files and concatenate them to the
 *			standard output. The input files need not be of 
 *			the same format but they must have the same resolution
 *			and encoding.
 *
 *	Syntax:		rascat [-v] [-V] [-ifmt format] [-ofmt format] 
 *			[-win nx ny x y] [-o file] [ - | file... ]
 *
 *	Options:
 *
 *			-verbose	Operate in verbose mode
 *
 *			-Version	Print version number to the tty
 *
 *			-ifmt <format>
 *				'format' specifies the file format of *all*
 *				the input raster files. If ommitted, file
 *				format is determined by the file name extension
 *
 *			-ofmt <format>
 *				'format' specifies the file format of the raster
 *				file to be written to. If ommitted, the output
 *				file format will be the same as the format 
 *				of the *first* input file. 
 *
 *			-win <nx ny x y>
 *				nx, ny, x, and y specify a sub-region of the
 *				input file. 'x' and 'y' specify the upper-left 
 *				corner. 'nx' and 'ny' specify the width 
 *				and height of the region, respectively. The
 *				default is to copy the entire image.
 *
 *			-outfile <file>
 *				Write output to 'file' instead of the standard
 *				output.
 *
 *			- 	Read from standard input. The default.
 *
 *	Environment:
 *				
 */
#include <stdio.h>
#ifdef	SYSV
#include <strings.h>
#else
#include <string.h>
#endif
#include <ncarv.h>
#include <ncarg_ras.h>

static	void	Usage(msg) 
	char	*msg;
{
	char	*opts = "[-v] [-ifmt format] [-ofmt format] [-win nx ny x y] [-o file] [ - | file... ]";

	if (msg) {
		fprintf(stderr, "rascat: %s\n", msg);
	}
	fprintf(stderr, "Usage: rascat %s\n", opts);
	PrintOptionHelp(stderr);
}

typedef struct	Subregion_ {
	int	nx;
	int	ny;
	int	x;
	int	y;
	} Subregion;

/*
 * 	command line options
 */
static	struct	{
	boolean	do_verbose;
	boolean	do_version;
	Subregion	win;
	boolean	do_stdin;
	char	*dstfile;
	char	*srcformat;
	char	*dstformat;
	} opt;

static  OptDescRec      set_options[] = {
	{"verbose", 0, NULL, "Do operate in verbose mode"},
	{"Version", 0, NULL, "Print version and exit"},
	{"window", 4, "-1 -1 -1 -1", "Extract a subregion - nx ny x y"},
	{"", 0, NULL, "Do get input from standard input"},
	{"output", 1, "stdout", "Specify output file"},
	{"ifmt", 1, NULL, "Specify format of input file"},
	{"ofmt", 1, NULL, "Specify format of output file"},
	{NULL},
};

static	Option get_options[] = {
	{"verbose", NCARGCvtToBoolean, (Voidptr) &opt.do_verbose, 
							sizeof(opt.do_verbose)
	},
	{"Version", NCARGCvtToBoolean, (Voidptr) &opt.do_version, 
							sizeof(opt.do_version)
	},
	{"window", NCARGCvtToInt, (Voidptr) &opt.win, sizeof (opt.win.nx)
	},
	{"", NCARGCvtToBoolean, (Voidptr) &opt.do_stdin, sizeof(opt.do_stdin)
	},
	{"output", NCARGCvtToString, (Voidptr) &opt.dstfile, sizeof(opt.dstfile)
	},
	{"ifmt", NCARGCvtToString, (Voidptr) &opt.srcformat, 
							sizeof(opt.srcformat)
	},
	{"ofmt", NCARGCvtToString, (Voidptr) &opt.dstformat, 
							sizeof(opt.dstformat)
	},
	{NULL
	}
};

	
/*
 * default input file
 */
static	char	*stdin_array[] = {"stdin"};


main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	do_window  = False;

	char	*Comment = "Created by rascat";
	char	*arg;
	Raster	*src, *dst, *RasterOpen(), *RasterOpenWrite();
	int	frame = 0;
	int	status;
	int	nX, nY;
	RasterEncoding	rasterType;
	Subregion	*win;
	int	rcount;		/* number of files to process	*/
	char	**rfiles;	/* the input files		*/
	int	i;

	char	*prog_name;

	prog_name = (prog_name = strrchr(argv[0],'/')) ? ++prog_name : *argv;

	(void) RasterInit(&argc, argv);


	if (ParseOptionTable(&argc, argv, set_options) < 0) {
		fprintf(
			stderr, "%s : Error parsing options - %s\n", 
			prog_name, ErrGetMsg()
		);
		exit(1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(get_options) < 0) {
		fprintf(
			stderr, "%s : Error getting options - %s\n", 
			prog_name, ErrGetMsg()
		);
		Usage(NULL);
		exit(1);
	}


	win = &opt.win;
	do_window = win->nx != -1;


	if (opt.do_version) {
		PrintVersion(prog_name);
		exit(0);
	}

	/*
	 * make  sure no options left on command line
	 */
	for(i=1; i<argc; i++) {
		if (*argv[i] == '-') {
			fprintf(
				stderr, "%s: Invalid option - %s\n", 
				prog_name, argv[i]
			);
			Usage(NULL);
			exit(1);
		}
	}

	/*
	 * if no files read from stdin
	 */
	if (argc < 2) {
		rcount = 1;
		rfiles = stdin_array;
	}
	else {
		rcount = argc - 1;
		rfiles = argv + 1;
	}

	dst = (Raster *) NULL;
	for(i=0; i<rcount; i++) {

		/* Open the source file and read the header. */

		src = RasterOpen(rfiles[i], opt.srcformat);
		if (src == (Raster *) NULL) {
			(void) RasterPrintError(rfiles[i]);
			continue;
		}

		status = RasterRead(src);
		if (status != RAS_OK) {
			(void) RasterPrintError(rfiles[i]);
			(void) RasterClose(src);
			continue;
		}

		if (!do_window) {
			win->x = 0; win->nx = src->nx;
			win->y = 0; win->ny = src->ny;
		}
		else {
			if (win->x < 0 || win->x > src->nx - 1 ||
				win->y < 0 || win->y > src->ny - 1 ||
				(win->x + win->nx - 1) > (src->nx - 1) ||
				(win->y + win->ny - 1) > (src->ny - 1)) {

				fprintf(stderr, "Window out of range\n");
				exit(1);
			}
		}

		/*
		 * if this is the first time through open the destination
		 * file. We have to do this now because we can't open
		 * the output file until we know the encoding of the input
		 * file.
		 */
		if (! dst) {
			/*
			 * if output is stdout and output format is unspecified
			 * use the input format. If thats not specified 
			 * use whatever format the first input file is
			 */
			if (!strcmp(opt.dstfile,"stdout") && opt.dstformat == NULL){
				opt.dstformat = src->format;
			}

			if (src->type == RAS_INDEXED) {
				dst = RasterOpenWrite(
					opt.dstfile,win->nx,win->ny, Comment, 
					RAS_INDEXED, opt.dstformat
				);
				if (dst == (Raster *) NULL) {
					(void) RasterPrintError((char *) NULL);
					(void) RasterClose(src);
					exit(1);
				}
				rasterType = RAS_INDEXED;
			}
			else {
				dst = RasterOpenWrite(
					opt.dstfile, win->nx, win->ny, Comment, 
					RAS_DIRECT, opt.dstformat
				);
				if (dst == (Raster *) NULL) {
					(void) RasterPrintError((char *) NULL);
					(void) RasterClose(src);
					exit(1);
				}
				rasterType = RAS_DIRECT;
			}
			
			nX = src->nx;
			nY = src->ny;
		}
		else {
			if (! (src->nx == nX && src->ny == nY)) {
				fprintf(
					stderr, 
					"Raster size changes not allowed\n"
				);
				RasterClose(src); 
				continue;
			}
			if (src->type != rasterType) {
				fprintf(
					stderr, 
					"Raster enconding changes not allowed\n"
					);
				RasterClose(src);
				continue;
			}
		}

		do {
			RasterOp(src, dst, win->x, win->y, win->nx, win->ny,0,0,0);

			status = RasterWrite(dst);
			if (status != RAS_OK) {
				(void) RasterPrintError((char *) NULL);
				exit(1);
			}

			if (opt.do_verbose) {
				fprintf(
					stderr, "Copied frame %5d to %s\n", 
					frame++, opt.dstfile
				);
			}
		
			status = RasterRead(src);
		} while ( status == RAS_OK );

		if (status == RAS_ERROR) {
			(void) RasterPrintError((char *) NULL);
		}
		(void) RasterClose(src);
	}
	exit(0);
}

RasterCopy(src, dst, src_x, src_y, src_nx, src_ny)
	Raster	*src;
	Raster	*dst;
	int	src_x, src_y, src_nx, src_ny;
{
	int	x, y;
	int	r, g, b;
	int	x1, y1, x2, y2;

	x1 = src_x;
	y1 = src_y;
	x2 = src_x + src_nx - 1;
	y2 = src_y + src_ny - 1;

	if (src->type == RAS_INDEXED)
		RasterCopyColormap(src, dst);
	
	for(y=y1; y<=y2; y++)
	for(x=x1; x<=x2; x++)
	{
		if (src->type == RAS_DIRECT) {
			r = DIRECT_RED(src, x, y);
			g = DIRECT_GREEN(src, x, y);
			b = DIRECT_BLUE(src, x, y);
			DIRECT_RED(dst, x, y) = r;
			DIRECT_GREEN(dst, x, y) = g;
			DIRECT_BLUE(dst, x, y) = b;
		}
		else if (src->type == RAS_INDEXED) {
			INDEXED_PIXEL(dst, x, y) = INDEXED_PIXEL(src, x, y);
		}
	}
}

/*ARGSUSED*/
RasterOp(src, dst, src_x, src_y, src_nx, src_ny, dst_x, dst_y, op)
	Raster	*src;
	Raster	*dst;
	int	src_x, src_y, src_nx, src_ny, dst_x, dst_y;
	int	op;
{
	int	r, g, b;
	int	sx, sy, dx, dy;
	int	sx1, sx2, sy1, sy2;

	if (src->type == RAS_INDEXED) {
		RasterCopyColormap(src, dst);
	}

	sx1 = src_x;
	sx2 = src_x + src_nx - 1;
	sy1 = src_y;
	sy2 = src_y + src_ny - 1;
	
	for(sy=sy1, dy=dst_y; sy<=sy2; sy++, dy++)
	for(sx=sx1, dx=dst_x; sx<=sx2; sx++, dx++)
	{
		if (src->type == RAS_DIRECT) {
			r = DIRECT_RED(src, sx, sy);
			g = DIRECT_GREEN(src, sx, sy);
			b = DIRECT_BLUE(src, sx, sy);
			DIRECT_RED(dst, dx, dy) = r;
			DIRECT_GREEN(dst, dx, dy) = g;
			DIRECT_BLUE(dst, dx, dy) = b;
		}
		else if (src->type == RAS_INDEXED) {
			INDEXED_PIXEL(dst, sx, sy) = INDEXED_PIXEL(src, dx, dy);
		}
	}
}

