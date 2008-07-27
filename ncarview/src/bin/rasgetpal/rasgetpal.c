/*
 * $Id: rasgetpal.c,v 1.6 2008-07-27 03:18:40 haley Exp $
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

#include <stdio.h>
#include <fcntl.h>
#include "ncarg/c.h"
#include "ncarg/ncarg_ras.h"

static char	*ProgramName		= (char *) NULL;

static struct {
	boolean		help;
	boolean		version;
	char		*ifmt;
} opt;

/* Options we want parsed. */

static  OptDescRec      set_options[] = {
	{"help",	0, NULL, "Print help message"},
	{"ifmt", 	1, NULL, "Input file format"},
	{"Version",	0, NULL, "Print version number and exit"},
	{NULL}
};

static	Option	get_options[] = {
{"help",    NCARGCvtToBoolean, (Voidptr) &opt.help,   sizeof(opt.help)},
{"ifmt", NCARGCvtToString, (Voidptr) &opt.ifmt,sizeof(opt.ifmt)},
{"Version", NCARGCvtToBoolean, (Voidptr) &opt.version,sizeof(opt.version)},
{NULL}
};

static void	Usage();
static void	Examples();
static int	PrintPalette();

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int		i;
	int		opt_id;
	static char	*srcfile = (char *) NULL;
	static char	*dstfile = (char *) NULL;

	ProgramName = argv[0];

	RasterInit(&argc, argv);

	opt_id = OpenOptionTbl();
	if (ParseOptionTable(opt_id, &argc, argv, set_options) < 0) {
		(void) fprintf(
			stderr,"%s : Error parsing command line options : %s\n",
			ProgramName, ErrGetMsg());
		exit(1);
	}

	if (GetOptions(opt_id, get_options) < 0) {
		(void) fprintf(
			stderr,"%s : GetOptions(,) : %s\n",
			ProgramName,ErrGetMsg());
		exit(1);
	}

	if (opt.version) {
		(void) PrintVersion(ProgramName);
		exit(0);
	}

	if (opt.help) {
		Usage(ProgramName, (char *) NULL, opt_id);
		exit(0);
	}

	if (argc == 1) {
		(void) fprintf(stderr,
			"%s: No filename arguments\n",
			ProgramName);
		exit(1);
	}

	/* Make sure nothing left on command line execpt file names. */

	if (argc == 1) {
		(void) fprintf(stderr,
			"%s: No filenames on command line\n",
			ProgramName);
		exit(1);
	}

	for (i=1; i<argc; i++) {
		if (*argv[i] == '-') {
			(void) fprintf(stderr,
				"%s: Unknown option \"%s\"\n",
				ProgramName, argv[i]);
			Usage(ProgramName, (char *) NULL, opt_id);
		}
		else {
			if (srcfile == (char *) NULL) {
				srcfile = argv[i];
			}
			else if (dstfile == (char *) NULL) {
				dstfile = argv[i];
			}
			else {
				(void) fprintf(stderr,
				"%s: Too many filename arguments\n",
				ProgramName);
				exit(1);
			}
		}
	}

	if (srcfile == (char *) NULL) {
		(void) fprintf(stderr,
			"%s: No input rasterfile\n",
			ProgramName);
		exit(1);
	}

	if (dstfile == (char *) NULL) {
		PrintPalette(srcfile, "stdout", opt.ifmt);
	}
	else {
		PrintPalette(srcfile, dstfile, opt.ifmt);
	}

	return(0);
}

static int PrintPalette(rasfile, palfile, ifmt)
	char	*rasfile;
	char	*palfile;
	char	*ifmt;
{
	int		i, status;
	Raster		*ras, *RasterOpen();
	unsigned char	colors[768];

	ras = RasterOpen(rasfile, ifmt);
	if (ras == (Raster *) NULL) {
		(void) RasterPrintError();
		return(RAS_ERROR);
	}

	status = RasterRead(ras);

	/* Terminate for error or EOF */

	if (status != RAS_OK) {
		(void) RasterPrintError();
		return(RAS_ERROR);
	}

	if (ras->type == RAS_DIRECT) {
		(void) fprintf(stderr,
		"%s: %s is a DirectColor file and has no palette\n",
		ProgramName, rasfile);
		return(RAS_ERROR);
	}
	else if (ras->type == RAS_INDEXED) {
		for(i=0; i<256; i++) {
			colors[i]     = ras->red[i];
			colors[i+256] = ras->green[i];
			colors[i+512] = ras->blue[i];
		}

		if (!strcmp(palfile, "stdout")) {
			status = PaletteWrite(palfile, "txt", colors);
		}
		else {
			status = PaletteWrite(palfile, (char *) NULL, colors);
		}

		if (status != RAS_OK) {
			(void) fprintf(stderr,
				"%s: %s\n",
				ProgramName, ErrGetMsg());
			return(status);
		}
			
		if (status != RAS_OK) return(status);
	}

	return(RAS_OK);
}

static	void	Usage(progName, message, opt_id)
	char	*progName;
	char	*message;
	int	opt_id;
{
	if (message) {
		(void) fprintf(stderr, "%s: %s", progName, message);
	}

	(void) fprintf(stderr, 
	"Usage: %s [-help] [-Version] rasterfile palfile\n",
	progName);

	(void) fprintf(stderr, 
	"Usage: %s [-help] [-Version] rasterfile >palfile\n",
	progName);

	PrintOptionHelp(opt_id, stderr);

	Examples();

	exit(1);
}

static	void	Examples()
{
	int		i;

	static char	*help_msg[] = {
	"\n",
	"Examples:",
	"	rasgetpal temp.sun temp.txt",
	"	rasgetpal temp.sun >temp.txt",
	"	rasgetpal temp.sun temp.pal"};

	for(i=0; i<sizeof(help_msg)/sizeof(char *); i++) {
		(void) fprintf(stderr, "%s\n", help_msg[i]);
	}
}
