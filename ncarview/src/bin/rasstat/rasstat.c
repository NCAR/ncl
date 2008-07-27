/*
 *      $Id: rasstat.c,v 1.7 2008-07-27 03:18:41 haley Exp $
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
 *	File:		rasstat
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr 1 14:53:53 MST 1992
 *
 *	Description:	
 */
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ncarg/ncarg_ras.h>
#include <ncarg/c.h>



/*
 * 	command line options
 */
static	struct	{
	boolean		do_dimension;
	boolean		do_type;
	boolean		do_image_count;
	boolean		do_version;
	boolean		do_help;
	char		*format;
	} opt;

static  OptDescRec      set_options[] = {
	{"dimension", 0, NULL, "Print file dimensions and exit"},
	{"type", 0, NULL, "Print file type and exit"},
	{"count", 0, NULL, "Report number of images in file and exit"},
	{"Version", 0, NULL, "Print version and exit"},
	{"help", 0, NULL, "Print this message and exit"},
	{"format", 1, NULL, "Raster file format"},
	{NULL},
};

static	Option get_options[] = {
	{"dimension", NCARGCvtToBoolean, (Voidptr) &opt.do_dimension, 
							sizeof(opt.do_dimension)
	},
	{"type", NCARGCvtToBoolean, (Voidptr) &opt.do_type, 
							sizeof(opt.do_type)
	},
	{"count", NCARGCvtToBoolean, (Voidptr) &opt.do_image_count, 
						sizeof(opt.do_image_count)
	},
	{"Version", NCARGCvtToBoolean, (Voidptr) &opt.do_version, 
							sizeof(opt.do_version)
	},
	{"help", NCARGCvtToBoolean, (Voidptr) &opt.do_help, 
							sizeof(opt.do_help)
	},
	{"format", NCARGCvtToString, (Voidptr) &opt.format, 
							sizeof(opt.format)
	},
	{NULL
	}
};

static	char	*progName;
static	int	oD;
	
static	void	Usage(msg) 
	char	*msg;
{
	char	*opts = "[-format <format>] [-V|d|t|c] <file>";

	if (msg) {
		fprintf(stderr, "%s: %s\n", progName, msg);
	}
	fprintf(stderr, "Usage: %s %s\n", progName, opts);
	PrintOptionHelp(oD, stderr);
	exit(1);
}

static	void	print_ras(ras_stat)
	RasStat	*ras_stat;
{
	fprintf(stdout, "Dimension: %dx%d\n", ras_stat->nx, ras_stat->ny);
	fprintf(
		stdout, "Type: %s\n", 
		(ras_stat->type == RAS_INDEXED ? "indexed" : "direct")
	);
}


main(argc, argv)
	int	argc;
	char	*argv[];
{
	RasStat	ras_stat;
	int	*ic = (int *) NULL;
	int	image_count;

	progName = (progName = strrchr(argv[0],'/')) ? ++progName : *argv;

	(void) RasterInit(&argc, argv);


	oD = OpenOptionTbl();
	if (ParseOptionTable(oD, &argc, argv, set_options) < 0) {
		fprintf(
			stderr, "%s : Error parsing options : %s\n", 
			progName, ErrGetMsg()
		);
		exit(1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(oD, get_options) < 0) {
		fprintf(
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

	if (argc != 2) {
		Usage(NULL);
	}

	ic = opt.do_image_count ? &image_count : NULL;

	if (RasterStat(argv[1], opt.format, &ras_stat, ic) == -1) {
		fprintf(
			stderr, "%s : stat failed : %s\n", 
			progName, ErrGetMsg()
		);
		exit(2);
	}

	if (opt.do_dimension) {
		fprintf(stdout, "%dx%d\n", ras_stat.nx, ras_stat.ny);
	}
	else if (opt.do_type) {
		if (ras_stat.type == RAS_INDEXED) {
			fprintf(stdout, "indexed\n");
		}
		else {
			fprintf(stdout, "direct\n");
		}
	}
	else if (opt.do_image_count) {
		fprintf(stdout, "%d\n", image_count);
	}
	else {
		print_ras(&ras_stat);
	}

	exit(0);
}

