/*
**      $Id: ras2ccir601.c,v 1.4 2008-07-27 03:18:40 haley Exp $
*/
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
**	File:		ras2ccir601.c
**
**	Author:		John Clyne
**			National Center for Atmospheric Research
**			PO 3000, Boulder, Colorado
**
**	Date:		Tue Jan 24 11:01:37 MST 1995
**
**	Description:	
*/
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <ncarg/c.h>
#include <ncarg/ncarg_ras.h>


/*
**	Resolution of NTSC, CCIR601 images
*/
#define	IMAGE_WIDTH	720
#define	IMAGE_HEIGHT	486

/*
**	CCIR601 pixels aren't square so we need to scale
*/
#define	ASPECT_CORRECT	1.0911


/*
**	pointer to a raster image resampling function
*/
typedef	int	(*IRAFuncPtr_T)	(Raster *src, Raster *dst, int verbose);

/*
**	type converter from a resample algorithm name ("nn" or "bl") to
**	a coresponding image resampling function
*/
static	int	cvt_to_rsfunc(from, to)
	const char	*from;
	Voidptr 	to;
{
#ifdef	DEAD
	int	(**fptr)() = (int (**)()) to;
#endif
	IRAFuncPtr_T *fptr = (IRAFuncPtr_T *) to;


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


/*
 * 	command line options
 */
static	struct	{
	boolean		do_verbose;
	boolean		do_version;
	boolean		do_stdin;
	char		*dstfile;
	char		*srcformat;
	boolean		do_help;
	IRAFuncPtr_T	resample;
	} opt;

static  OptDescRec      set_options[] = {
	{"verbose", 0, NULL, "Do operate in verbose mode"},
	{"Version", 0, NULL, "Print version and exit"},
	{"", 0, NULL, "Do get input from standard input"},
	{"output", 1, "stdout", "Specify output file"},
	{"ifmt", 1, NULL, "Specify format of input file"},
	{"help", 0, "NULL", "Print this message and exit"},
	{"ira", 1, "bl", "Specify resampling algo for aspect ratio correct, nn or bl"},
	{NULL},
};

static	Option get_options[] = {
	{"verbose", NCARGCvtToBoolean, (Voidptr) &opt.do_verbose, 
							sizeof(opt.do_verbose)
	},
	{"Version", NCARGCvtToBoolean, (Voidptr) &opt.do_version, 
							sizeof(opt.do_version)
	},
	{"", NCARGCvtToBoolean, (Voidptr) &opt.do_stdin, sizeof(opt.do_stdin)
	},
	{"output", NCARGCvtToString, (Voidptr) &opt.dstfile, sizeof(opt.dstfile)
	},
	{"ifmt", NCARGCvtToString, (Voidptr) &opt.srcformat, 
							sizeof(opt.srcformat)
	},
	{"help", NCARGCvtToBoolean, (Voidptr) &opt.do_help, 
							sizeof(opt.do_help)
	},
	{"ira", cvt_to_rsfunc, (Voidptr) &opt.resample, sizeof(opt.resample)
	},
	{NULL
	}
};

static	char	*ProgName;

	
static	void	usage(
	int		od,
	const char	*msg
) {

	if (msg) {
		(void) fprintf(stderr, "%s: %s\n", ProgName, msg);
	}
	(void) fprintf(stderr, "Usage: %s [options] \n", ProgName);
	PrintOptionHelp(od, stderr);
	exit(1);
}



/*
**	create a Raster structure with appropriate spatial
**	dimenisions, appropriate for scaling `old_ras' to correct 
**	for CCIR601's non-square pixels
*/
static Raster	*create_aspect_correct_ras(
	Raster	*old_ras
) {
	Raster	*new_ras;
	int	new_ny = old_ras->ny;
	int	new_nx;

	new_nx = (int) (((float) old_ras->nx) * ASPECT_CORRECT);

	new_ras = RasterCreate(new_nx, new_ny, RAS_DIRECT);
	if (! new_ras) {
		(void) fprintf(
			stderr, "%s : RasterCreate() : %s\n",
			ProgName, RasterGetError()
		);
		return (NULL);
	}

	return(new_ras);
}

/*
**	Create a Raster of type `RAS_DIRECT' with the same dimension
**	as `old_ras'
*/
static Raster	*create_direct_ras(
	Raster	*old_ras
) {
	Raster	*new_ras;

	new_ras = RasterCreate(old_ras->nx, old_ras->ny, RAS_DIRECT);
	if (! new_ras) {
		(void) fprintf(
			stderr, "%s : RasterCreate() : %s\n",
			ProgName, RasterGetError()
		);
		return (NULL);
	}

	return(new_ras);
}

/*
**	Copy "src" Raster to "dst" Raster, performing aspect ratio
**	correction necessitated by CCIR601's non-square pixels
*/
static int	aspect_correct(
	Raster		*src,
	Raster		*dst,
	IRAFuncPtr_T	resample_func_ptr,
	int		do_verbose
) {
	if (resample_func_ptr(src, dst, do_verbose) < 0) {

		(void) fprintf(
			stderr, "%s: Resampling failed : %s\n",
			ProgName, RasterGetError()
		);
		return(-1);
	}

	return(0);
}


main(int argc, char **argv)
{

	Raster		*src, *dst;
	int		frame = 1;	/* num frames processed		*/
	int		src_frame;	/* current source frame		*/
	int		rc;		/* return codes			*/
	int		rcount;		/* number of files to process	*/
	char		**rfiles;	/* the input files		*/
	char		*stdin_array[] = {"stdin", NULL};
	int		err = 0;	/* exit status			*/

	int		od;		/* option descriptor		*/
	int		i;


	ProgName = (ProgName = strrchr(argv[0],'/')) ? ++ProgName : *argv;

	(void) RasterInit(&argc, argv);

	od = OpenOptionTbl();
	if (ParseOptionTable(od, &argc, argv, set_options) < 0) {
		(void) fprintf(
			stderr, "%s : Error parsing options : %s\n", 
			ProgName, ErrGetMsg()
		);
		exit(1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(od, get_options) < 0) {
		(void) fprintf(
			stderr, "%s : Error getting options : %s\n", 
			ProgName, ErrGetMsg()
		);
		usage(od, NULL);
	}

	if (opt.do_version) {
		PrintVersion(ProgName);
		exit(0);
	}

	if (opt.do_help) {
		usage(od, NULL);
	}


	/*
	 * make  sure no options left on command line
	 */
	for(i=1; i<argc; i++) {
		if (*argv[i] == '-') {
			(void) fprintf(
				stderr, "%s: Invalid option : %s\n", 
				ProgName, argv[i]
			);
			usage(od, NULL);
		}
	}

	/*
	 * if no files left on command line then read from stdin
	 */
	if (argc < 2) {
		rcount = 1;
		rfiles = stdin_array;
	}
	else {
		rcount = argc - 1;
		rfiles = argv + 1;
	}

	/*
	**	Open the output file appropriate for CCIR601.
	*/
	dst = RasterOpenWrite(
		opt.dstfile,IMAGE_WIDTH,IMAGE_HEIGHT, NULL, RAS_DIRECT, "yuv"
	);
	if (! dst) {
		(void) fprintf(stderr, 
			"%s: RasterOpenWrite(%s, %d, %d, ,,yuv) [ %s ]\n",
			ProgName, opt.dstfile, IMAGE_WIDTH, IMAGE_HEIGHT,
			ErrGetMsg()
		);
		exit(1);
	}

	for(i=0; i<rcount; i++) {
		int	first = 1;

		Raster	*direct_ras = NULL;
		Raster	*aspect_ras = NULL;
		Raster	*ras;

		src_frame = 1;

		/*
		** Open the source file and read the header.
		*/
		src = RasterOpen(rfiles[i], opt.srcformat);
		if (src == (Raster *) NULL) {
			(void) fprintf(stderr,
				"%s: RasterOpen(%s, %s) [ %s ]\n",
				ProgName, rfiles[i],
				opt.srcformat ? opt.srcformat : "NULL",
				ErrGetMsg()
			);
			err++;
			continue;
		}


		while ((rc = RasterRead(src)) == RAS_OK) {

			ras = src;

			if (first) {

				/*
				** first time through create Rasters
				** necessary for aspect ratio correction
				** and possibly for type conversion (indexed 
				** to direct)
				*/
				aspect_ras = create_aspect_correct_ras(ras);
				if (! aspect_ras) {
					exit(1);
				}

				if (ras->type == RAS_INDEXED) {
					direct_ras = create_direct_ras(ras);
					if (! direct_ras) {
						exit(1);
					}
				}
				first = 0;
			}

			if (ras->type == RAS_INDEXED) {
				(void) Raster8to24bit(ras, direct_ras, 0);
				ras = direct_ras;
			}

			/*
			** performa aspect ratio correction
			*/
			aspect_correct(
				ras, aspect_ras, opt.resample, opt.do_verbose
			);

			/*
			** crop and center the image to 720x486
			*/
			RasterCenterCrop(aspect_ras, dst);

			if (RasterWrite(dst) != RAS_OK) {
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
		
		}

		if (aspect_ras) {
			RasterDestroy(aspect_ras);
			aspect_ras = NULL;
		}

		if (direct_ras) {
			RasterDestroy(direct_ras);
			direct_ras = NULL;
		}


		if (rc == RAS_ERROR) {
			(void) fprintf(
				stderr,"Reading source file(%s) [ %s ]\n",
				rfiles[i], ErrGetMsg()
			);
			err++;
		}

		(void) RasterClose(src);
	}

	if (dst) RasterClose(dst);
	exit(err);
}
