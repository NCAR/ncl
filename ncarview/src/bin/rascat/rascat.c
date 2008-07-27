/*
 *      $Id: rascat.c,v 1.25 2008-07-27 03:18:40 haley Exp $
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
 *			-ira	[nn|bl]
 *				Specify Nearest Neighbor (nn) or Bilinear 
 *				interpolation (bl) algorithm for resampling.
 *
 *	Environment:
 *				
 */
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <ncarg/c.h>
#include <ncarg/ncarg_ras.h>


static int	cvt_to_rsfunc(const char *from, Voidptr	to);
static	int	cvt_to_etype(const char	*from, Voidptr 	to);

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
	float		rgbscale;
	Dimension2D	resolution;
	boolean		do_help;
	char		*pal;
	RasterEncoding	outtype;
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
	{"rgbscale", 1, "1.0", "Specify color intensity scaling factor"},
	{"resolution", 1, "0x0", "Specify output image resolution"},
	{"help", 0, "NULL", "Print this message and exit"},
	{"ira", 1, "nn", "Specify resampling algo, nn or bl"},
	{"pal", 1, NULL, "Specify a color palette"},
	{"outtype", 1, NULL, "Force output encoding type, indexed or direct"},
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
	{"rgbscale", NCARGCvtToFloat, (Voidptr) &opt.rgbscale, 
							sizeof(opt.rgbscale)
	},
	{"resolution", NCARGCvtToDimension2D, (Voidptr) &opt.resolution, 
							sizeof(opt.resolution)
	},
	{"help", NCARGCvtToBoolean, (Voidptr) &opt.do_help, 
							sizeof(opt.do_help)
	},
	{"ira", cvt_to_rsfunc, (Voidptr) &opt.resample, sizeof(opt.resample)
	},
	{"pal", NCARGCvtToString, (Voidptr) &opt.pal, 
							sizeof(opt.pal)
	},
	{"outtype", cvt_to_etype, (Voidptr) &opt.outtype, 
							sizeof(opt.outtype)
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
	char	*opts = "[-v] [-ifmt format] [-ofmt format] [-win nx ny x y] [-o file] [-scale factor | -res resolution [ -ira nn | bl]] [-rgbscale  factor] [ - | file... ]";

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
	boolean		do_quant  = False;

	char		*Comment = "Created by rascat";
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
	Raster	*quant_ras = (Raster *) NULL;
	int		err = 0;	/* exit rc			*/
	int		i;
	unsigned char	colors[768];


#ifdef DEAD
	progName = (progName = strrchr(argv[0],'/')) ? ++progName : *argv;
#endif

	if ((progName = strrchr(argv[0],'/')) == (char *) NULL) {
		progName = *argv;
	}
	else {
		++progName;
	}

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
	do_quant = (opt.outtype != RAS_UNKNOWN);


	if (do_scale && do_res) {
		Usage("-scale and -res are mutually exclusive options");
	}

	/*
	 * count the number of image filters
	 */
	if (do_window) pipe_count++;
	if (do_scale) pipe_count++;
	if (do_res) pipe_count++;
	if (do_quant) pipe_count++;


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
			(void) fprintf(stderr,
				"%s: RasterOpen(%s, %s) [ %s ]\n",
				progName, rfiles[i],
				opt.srcformat ? opt.srcformat : "NULL",
				ErrGetMsg()
			);
			err++;
			continue;
		}

		rc = RasterRead(src);
		if (rc != RAS_OK) {
			fprintf(
				stderr, "%s: Reading input file(%s) [ %s ]\n",
				progName, rfiles[i], ErrGetMsg()
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
			int		nx = src->nx;
			int		ny = src->ny;
			RasterEncoding	type = src->type;

			int		pipe_count_ = pipe_count;
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

			/*
			 * if an output encoding type was specified and
			 * the source type is the same as the requested
			 * out type - do nothing
			 */
			if (do_quant) {
				if (opt.outtype == src->type) {
					do_quant = False;
					pipe_count--; pipe_count_--;
				}
			}

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

			if (do_quant) {

				pipe_count_--;
				type = opt.outtype;
				if (pipe_count_ > 0) {
					quant_ras = RasterCreate(nx,ny,type);
					if (! quant_ras) {
						(void) fprintf(stderr, 
						"%s : RasterCreate() : %s\n",
						progName, RasterGetError());
					}
				} 

			}

			dst = RasterOpenWrite(
				opt.dstfile,nx,ny, Comment, 
				type, opt.dstformat
			);
			if (dst == (Raster *) NULL) {
				(void) fprintf(stderr, 
					"%s: RasterOpenWrite(%s, %d, %d, %s,%d,%s) [ %s ]\n",
					progName, opt.dstfile, nx, ny, Comment, 
					type,
					opt.dstformat ? opt.dstformat : "NULL",
					ErrGetMsg()
				);
				(void) RasterClose(src);
				exit(1);
			}

			rasterType = src->type;
			nX = src->nx;
			nY = src->ny;
		}
		else {
			if (! (src->nx == nX && src->ny == nY)) {
				(void) fprintf(
					stderr, 
					"%s: Raster size changes not allowed\n",
					progName
				);
				RasterClose(src); 
				continue;
			}
			if (src->type != rasterType) {
				(void) fprintf(
					stderr, 
				"%s: Raster enconding changes not allowed\n",
					progName
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
				(void) RasterCopyColormap(tmp,new);
				RasterOp(
					tmp,new,win->x,win->y,
					win->nx, win->ny,0,0,0
				);
				tmp = win_ras;
			}

			if (do_scale || do_res) {
				new = sca_ras ? sca_ras : dst;
				(void) RasterCopyColormap(tmp,new);
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

			if (do_quant) {
				new = quant_ras ? quant_ras : dst;
				if (opt.outtype == RAS_INDEXED) {
					rc = RasterDither(
						tmp, new, opt.do_verbose
					);
				}
				else {
					rc = Raster8to24bit(
						tmp, new, opt.do_verbose
					);
				}

				if (rc != RAS_OK) {
					(void) fprintf(
						stderr, 
						"%s: Quantizing failed : %s\n",
						progName, RasterGetError()
						);
					exit(1);
				}
				tmp = sca_ras;
			}

			if (opt.rgbscale != 1.0) {
				if (RasterRGBScale(dst, opt.rgbscale) < 0) {
					(void) fprintf(
						stderr, 
					"%s: Color scaling failed : %s\n",
						progName, RasterGetError()
						);
					err++;
				}
			}

			if (dst->type == RAS_INDEXED && opt.pal &&
			!dst->map_forced) {
				rc = PaletteRead(opt.pal,NULL,colors);
				if (rc != RAS_OK) {
				  (void) fprintf(stderr,
				  "%s: PaletteRead of \"%s\" failed: %s\n",
				  progName, opt.pal, RasterGetError());
				  err++;
				}
				rc = RasterLoadPalette(dst, colors);
				if (rc != RAS_OK) {
				  (void) fprintf(stderr,
				  "%s: RasterLoadPalette failed\n",
				  progName, opt.pal, RasterGetError());
				  err++;
				}
			}

			if (dst->type == RAS_DIRECT && opt.pal) {
				(void) fprintf(stderr,
			"%s: You can't add a palette to a true-color image\n",
				progName);
				exit(1);
			}

			rc = RasterWrite(dst);
			if (rc != RAS_OK) {
				(void) fprintf(
					stderr, 
					"Writing output file(%s) [ %s ]\n", 
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
				stderr,"Reading source file(%s) [ %s ]\n",
				rfiles[i], ErrGetMsg()
			);
		}
		(void) RasterClose(src);
	}
	if (dst) RasterClose(dst);
	exit(err);
}

static	int	cvt_to_rsfunc(from, to)
	const char	*from;
	Voidptr 	to;
{
	int	(**fptr)() = (int (**)()) to;


	if (! from) {
		*fptr = RasterResampleNearestNeighbor;
	}
	else if (strcmp(from, "NN") == 0 || strcmp(from, "nn") == 0) {
		*fptr = RasterResampleNearestNeighbor;
	}
	else if (strcmp(from, "BL") == 0 || strcmp(from, "bl") == 0) {
		*fptr = RasterResampleBilinear;
	}
	else {
		ESprintf(EINVAL, "Convert(%s) to function failed", from);
		return(-1);
	}
	return(1);
}

static	int	cvt_to_etype(const char	*from, Voidptr 	to)
{
	RasterEncoding	*type = (RasterEncoding *) to;

	if (! from) {
		*type = RAS_UNKNOWN;
	}
	else if (strncmp(from, "indexed", 3) == 0) {
		*type = RAS_INDEXED;
	}
	else if (strncmp(from, "direct", 3) == 0) {
		*type = RAS_DIRECT;
	}
	else {
		ESprintf(EINVAL, "Convert(%s) to encoding type failed", from);
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
