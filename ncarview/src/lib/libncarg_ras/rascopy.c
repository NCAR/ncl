/* 
 * $Id: rascopy.c,v 1.4 2008-07-27 03:18:46 haley Exp $
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
#include "ncarg_ras.h"

static char	*ProgramName = "rascopy";

int		OptionVerbose = False;
int		OptionWindow  = False;

static char	*srcfile = (char *) NULL;
static char	*dstfile = (char *) NULL;
static char	*Comment = "Created by rascopy";

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	i;
	char	*arg;
	int	status;
	Raster	*src, *dst, *RasterOpen(), *RasterOpenWrite();
	int	frame = 0;
	int	src_x, src_nx, src_y, src_ny;

	if (argc == 1) {
		(void) fprintf(stderr, "%s: Wrong number of arguments\n",
				ProgramName);
		(void) fprintf(stderr, "%s: Try 'rascopy -help'\n",
				ProgramName);
		exit(1);
	}

	(void) RasterInit(&argc, argv);

	while(--argc) {
		arg = *++argv;

		if (!strcmp(arg, "-")) {
			srcfile = "stdin";
		}
		else if (!strcmp(arg, "-help")) {
			(void) PrintHelp();
			exit(0);
		}
		else if (!strcmp(arg, "-v")) {
			OptionVerbose = True;
		}
		else if (!strcmp(arg, "-win")) {
			if (argc >= 4) {
				argc -= 4;
				OptionWindow = True;
				src_nx = atoi(*++argv);
				src_ny = atoi(*++argv);
				src_x  = atoi(*++argv);
				src_y  = atoi(*++argv);
			}
			else {
				(void) fprintf(stderr,
				"%s: Ran out of arguments for option '-win'\n",
					ProgramName);
				exit(1);
			}
		}
		else if (*arg == '-') {
			(void) fprintf(stderr, "%s: Bad option '%s'\n", 
				ProgramName, arg);
			exit(1);
		}
		else {
			if (srcfile == (char *) NULL)
				srcfile = arg;
			else if (dstfile == (char *) NULL)
				dstfile = arg;
			else {
				(void) fprintf(stderr, 
					"%s: Too many arguments\n",
					ProgramName);
				exit(1);
			}
		}
	}

	if (srcfile == (char *) NULL || dstfile == (char *) NULL) {
		(void) fprintf(stderr, 
			"%s: Not enough arguments\n",
			ProgramName);
		exit(1);
	}
	
	if (dstfile == (char *) NULL) dstfile = "stdout";

	/* Open the source file and read the header. */

	src = RasterOpen(srcfile, (char *) NULL);
	if (src == (Raster *) NULL) {
		(void) RasterPrintError(srcfile);
		exit(1);
	}

	status = RasterRead(src);
	if (status == RAS_EOF) {
		(void) fprintf(stderr, 
			"%s: EOF reading %s\n", ProgramName, srcfile);
		exit(1);
	}
	else if (status != RAS_OK) {
		(void) RasterPrintError((char *) NULL);
		exit(1);
	}

	if (!OptionWindow) {
		src_x = 0; src_nx = src->nx;
		src_y = 0; src_ny = src->ny;
	}
	else {
		if (src_x < 0 || src_x > src->nx - 1 ||
		    src_y < 0 || src_y > src->ny - 1 ||
		    (src_x + src_nx - 1) > (src->nx - 1) ||
		    (src_y + src_ny - 1) > (src->ny - 1)) {
			(void) fprintf(stderr, 
				"%s: Window out of range\n",
				ProgramName);
			exit(1);
		}
	}

	/* Open the destination file. */

	if (src->type == RAS_INDEXED) {
		dst = RasterOpenWrite(dstfile,src_nx,src_ny, 
			Comment, RAS_INDEXED, (char *) NULL);

		if (dst == (Raster *) NULL) {
			(void) RasterPrintError((char *) NULL);
			exit(1);
		}
		
		(void) RasterCopyColormap(src, dst);
	}
	else {
		dst = RasterOpenWrite(dstfile, src_nx, src_ny, 
			Comment, RAS_DIRECT, (char *) NULL);
		if (dst == (Raster *) NULL) {
			(void) RasterPrintError((char *) NULL);
			exit(1);
		}
	}

	do {
		if (src->nx != dst->nx || src->ny != dst->ny) {
			status = RasterCenterCrop(src, dst);
			if (status == RAS_ERROR) {
				(void) RasterPrintError();
			}
		}
		else {
		(void)RasterOp(src, dst, src_x, src_y, src_nx, src_ny, 0, 0, 0);
		}

		status = RasterWrite(dst);
		if (status != RAS_OK) {
			(void) RasterPrintError((char *) NULL);
			exit(1);
		}

		if (OptionVerbose) {
			(void) fprintf(stderr, 
				"Copied frame %5d to %s\n", frame, dstfile);
		}

		frame++;
		
		status = RasterRead(src);
	} while ( status == RAS_OK );

	exit(0);
}

PrintHelp()
{
	int		i;
	static char	*help_msg[] = {
	"rascopy -  Copy a raster file, providing for format conversion.",
	"",
	"Usage: rassplit options srcfile dstfile",
	"",
	"Options:",
	"",
	"	-help			Print this help information",
	"	-v			Verbose",
	"	-win NX NY X Y		Copy a window NX by NY",
	"				with upper left corner at (X,Y)",
	"				(In X terms NXxNY+X+Y)",
	"",
	"Examples:",
	"",
	"	Assume you have an NRIF file called 'temp.nrif', and you wish",
	"	to copy it to an HDF file of the same name:",
	"",
	"		rascopy temp.nrif temp.hdf",
	"",
	"	Assume 'temp.nrif' from above is a 640x512 picture and",
	"	you want to crop the top 32 lines:",
	"",
	"		rascopy -win 640 480 0 32 temp.nrif temp.hdf",
	"",
	};

	for(i=0; i<sizeof(help_msg)/sizeof(char *); i++) {
		(void) fprintf(stderr, "%s\n", help_msg[i]);
	}
}
