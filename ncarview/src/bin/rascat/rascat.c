
/*
 *      $Id: rascat.c,v 1.10 1992-08-17 15:03:01 clyne Exp $
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
 *			-scale <factor>
 *				Scale the input image by <factor>
 *
 *			-resolution <nx>x<ny>
 *				Resample the input image to the resolution
 *				'nx'x'ny'.
 *
 *			-ralgo	[NN|BL]
 *				Specify Nearest Neighbor (NN) or Bilinear 
 *				interpolation (BL) algorithm for resampling.
 *
 *	Environment:
 *				
 */
#include <stdio.h>
#include <errno.h>

#ifdef	SYSV
#include <strings.h>
#else
#include <string.h>
#endif

#include <ncarv.h>
#include <ncarg_ras.h>


int	cvt_to_rsfunc();

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
	boolean		do_verbose;
	boolean		do_version;
	Subregion	win;
	boolean		do_stdin;
	char		*dstfile;
	char		*srcformat;
	char		*dstformat;
	float		scale;
	Dimension2D	resolution;
	boolean		do_help;
	int		(*resample)();
	} opt;

static  OptDescRec      set_options[] = {
	{"verbose", 0, NULL, "Do operate in verbose mode"},
	{"Version", 0, NULL, "Print version and exit"},
	{"window", 4, "-1 -1 -1 -1", "Extract a subregion - nx ny x y"},
	{"", 0, NULL, "Do get input from standard input"},
	{"output", 1, "stdout", "Specify output file"},
	{"ifmt", 1, NULL, "Specify format of input file"},
	{"ofmt", 1, NULL, "Specify format of output file"},
	{"scale", 1, "0.0", "Specify image scaling factor"},
	{"resolution", 1, "0x0", "Specify output image resolution"},
	{"help", 0, "NULL", "Print this message and exit"},
	{"ralgo", 1, "NN", "Specify resampling algo, NN or BL"},
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
	{"scale", NCARGCvtToFloat, (Voidptr) &opt.scale, sizeof(opt.scale)
	},
	{"resolution", NCARGCvtToDimension2D, (Voidptr) &opt.resolution, 
							sizeof(opt.resolution)
	},
	{"help", NCARGCvtToBoolean, (Voidptr) &opt.do_help, 
							sizeof(opt.do_help)
	},
	{"ralgo", cvt_to_rsfunc, (Voidptr) &opt.resample, sizeof(opt.resample)
	},
	{NULL
	}
};

	
/*
 * default input file
 */
static	char	*stdin_array[] = {"stdin"};
static	char	*progName;
static	int	oD;

static	void	Usage(msg) 
	char	*msg;
{
	char	*opts = "[-v] [-ifmt format] [-ofmt format] [-win nx ny x y] [-o file] [-scale factor | -res resolution [ -ralgo NN | BL]] [ - | file... ]";

	if (msg) {
		(void) fprintf(stderr, "%s: %s\n", progName, msg);
	}
	(void) fprintf(stderr, "Usage: %s %s\n", progName, opts);
	PrintOptionHelp(oD, stderr);
	exit(1);
}


main(argc, argv)
	int	argc;
	char	*argv[];
{
	boolean		do_window  = False;
	boolean		do_scale  = False;
	boolean		do_res  = False;

	char		*Comment = "Created by rascat";
	char		*arg;
	Raster		*src, *dst;
	int		frame = 1;	/* num frames processed		*/
	int		src_frame;	/* current source frame		*/
	int		rc;		/* return codes			*/
	int		nX, nY;		/* source dimension		*/
	RasterEncoding	rasterType;	/* source encoding type		*/
	Subregion	*win;
	int		rcount;		/* number of files to process	*/
	char		**rfiles;	/* the input files		*/
	int		pipe_count = 0;	/* number filters in pipeline	*/
	Raster	*win_ras = (Raster *) NULL;
	Raster	*sca_ras = (Raster *) NULL;
	int		err;		/* exit rc			*/
	int		i;


	progName = (progName = strrchr(argv[0],'/')) ? ++progName : *argv;

	(void) RasterInit(&argc, argv);


	oD = OpenOptionTbl();
	if (ParseOptionTable(oD, &argc, argv, set_options) < 0) {
		(void) fprintf(
			stderr, "%s : Error parsing options : %s\n", 
			progName, ErrGetMsg()
		);
		exit(1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(oD, get_options) < 0) {
		(void) fprintf(
			stderr, "%s : Error getting options : %s\n", 
			progName, ErrGetMsg()
		);
		Usage(NULL);
	}

	if (opt.do_version) {
		PrintVersion(progName);
		exit(0);
	}
	if (opt.do_help) {
		Usage(NULL);
	}


	win = &opt.win;
	do_window = win->nx != -1;
	do_scale = opt.scale != 0;
	do_res = (! (opt.resolution.nx == 0 && opt.resolution.ny == 0));


	if (do_scale && do_res) {
		Usage("-scale and -res are mutually exclusive options");
	}

	/*
	 * count the number of image filters
	 */
	if (do_window) pipe_count++;
	if (do_scale) pipe_count++;
	if (do_res) pipe_count++;


	/*
	 * make  sure no options left on command line
	 */
	for(i=1; i<argc; i++) {
		if (*argv[i] == '-') {
			(void) fprintf(
				stderr, "%s: Invalid option : %s\n", 
				progName, argv[i]
			);
			Usage(NULL);
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
	err = 0;
	for(i=0; i<rcount; i++) {
		src_frame = 1;

		/* Open the source file and read the header. */

		src = RasterOpen(rfiles[i], opt.srcformat);
		if (src == (Raster *) NULL) {
			(void) fprintf(
				stderr, "RasterOpen(%s, %s) [ %s ]", 
				rfiles[i],opt.srcformat, ErrGetMsg()
			);
			err++;
			continue;
		}

		rc = RasterRead(src);
		if (rc != RAS_OK) {
			fprintf(
				stderr, "Reading input file(%s) [ %s ]",
				rfiles[i], ErrGetMsg()
			);
			(void) RasterClose(src);
			err++;
			continue;
		}


		/*
		 * if this is the first time through open the destination
		 * file. We have to do this now because we can't open
		 * the output file until we know the encoding of the input
		 * file.
		 */
		if (! dst) {
			int	nx, ny;
			int	pipe_count_ = pipe_count;
			/*
			 * if output is stdout and output format is unspecified
			 * use the input format. If thats not specified 
			 * use whatever format the first input file is
			 */
			if (!strcmp(opt.dstfile,"stdout") && 
				opt.dstformat == NULL)
			{

				opt.dstformat = src->format;
				(void) fprintf(stderr, 
					"%s: Warning: output format not specified, using input format (%s)\n", progName, src->format
					);
			}

			nx = src->nx;
			ny = src->ny;

			if (do_window) {
				pipe_count_--;
				nx = win->nx;
				ny = win->ny;

				check_win(win, src);

				if (pipe_count_ > 0) {
					win_ras = RasterCreate(nx,ny,src->type);
					if (! win_ras) {
						(void) fprintf(stderr, 
						"%s : RasterCreate() : %s\n",
						progName, RasterGetError());
					}
				} 
			}
			else {
				win->x = 0; win->nx = src->nx;
				win->y = 0; win->ny = src->ny;
			}

			if (do_scale || do_res) {
				pipe_count_--;
				if (do_scale) {
					nx *= opt.scale;
					ny *= opt.scale;
				}
				else if (do_res) {
					nx = opt.resolution.nx;
					ny = opt.resolution.ny;
				}
				if (pipe_count_ > 0) {
					sca_ras = RasterCreate(nx,ny,src->type);
					if (! sca_ras) {
						(void) fprintf(stderr, 
						"%s : RasterCreate() : %s\n",
						progName, RasterGetError());
					}
				} 
			}


			dst = RasterOpenWrite(
				opt.dstfile,nx,ny, Comment, 
				src->type, opt.dstformat
			);
			if (dst == (Raster *) NULL) {
				(void) fprintf(stderr, 
					"RasterOpenWrite(%s, %d, %d, %s,%d,%s) [ %s ]",
					opt.dstfile, nx, ny, Comment, 
					src->type, opt.dstformat,ErrGetMsg()
				);
				(void) RasterClose(src);
				exit(1);
			}

			rasterType = src->type;;
			nX = src->nx;
			nY = src->ny;
		}
		else {
			if (! (src->nx == nX && src->ny == nY)) {
				(void) fprintf(
					stderr, 
					"Raster size changes not allowed\n"
				);
				RasterClose(src); 
				continue;
			}
			if (src->type != rasterType) {
				(void) fprintf(
					stderr, 
					"Raster enconding changes not allowed\n"
					);
				RasterClose(src);
				continue;
			}
		}

		do {
			Raster	*tmp = src;
			Raster	*new = src;
			if (do_window || pipe_count == 0) {
				new = win_ras ? win_ras : dst;
				RasterOp(
					tmp,new,win->x,win->y,
					win->nx, win->ny,0,0,0
				);
				tmp = win_ras;
			}

			if (do_scale || do_res) {
				new = sca_ras ? sca_ras : dst;
				rc = opt.resample(tmp, new, opt.do_verbose);
				if (rc != RAS_OK) {
					(void) fprintf(
						stderr, 
						"%s: Resampling failed : %s\n",
						progName, RasterGetError()
						);
					exit(1);
				}
				tmp = sca_ras;
			}

			rc = RasterWrite(dst);
			if (rc != RAS_OK) {
				(void) fprintf(
					stderr, 
					"Writing output file(%s) [ %s ]", 
					opt.dstfile, ErrGetMsg()
				);
				exit(1);
			}

			if (opt.do_verbose) {
				(void) fprintf(
					stderr, 
					"Copied frame %s[%d] to %s[%d]\n", 
					rfiles[i], src_frame++, 
					opt.dstfile, frame++
				);
			}
		
			rc = RasterRead(src);
		} while ( rc == RAS_OK );

		if (rc == RAS_ERROR) {
			(void) fprintf(
				stderr,"Reading source file(%s) [ %s ]",
				rfiles[i], ErrGetMsg()
			);
		}
		(void) RasterClose(src);
	}
	if (dst) RasterClose(dst);
	exit(err);
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


static	int	cvt_to_rsfunc(from, to)
	char	*from;
	Voidptr to;
{
	int	(**fptr)() = (int (**)()) to;

	extern	int	RasterResampleBilinear();
	extern	int	RasterResampleNearestNeighbor();

	if (! from) {
		*fptr = RasterResampleNearestNeighbor;
	}
	else if (strcmp(from, "NN") == 0) {
		*fptr = RasterResampleNearestNeighbor;
	}
	else if (strcmp(from, "BL") == 0) {
		*fptr = RasterResampleBilinear;
	}
	else {
		ESprintf(EINVAL, "Convert(%s) to function failed", from);
		return(-1);
	}
	return(1);
}


check_win(win, src)
	Subregion	*win;
	Raster		*src;
{
	if (win->x < 0 || win->x > src->nx - 1 ||
		win->y < 0 || win->y > src->ny - 1 ||
		(win->x + win->nx - 1) > src->nx - 1 ||
		(win->y + win->ny - 1) > src->ny - 1) {

		(void) fprintf(stderr,"Window out of range\n");
		exit(1);
	}
}
