/*
 *	$Id: rasinfo.c,v 1.3 1991-11-15 17:09:05 don Exp $
 */
#include <stdio.h>
#include <fcntl.h>
#include "ncarg_ras.h"

int	OptionDebug = 0;
int	OptionPrintColors = False;

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	i;
	char	*arg;

	if (argc == 1) {
		(void) fprintf(stderr, "Wrong number of arguments\n");
		Usage();
	}

	RasterInit(&argc, argv);

	for(i=1; i<argc; i++) {
		arg = argv[i];

		if (*arg == '-') {
			if (arg[1] == 'c') {
				OptionPrintColors = True;
			}
			else {
				(void) fprintf(stderr, "Bad option: %s\n", arg);
				Usage();
			}
		}
		else {
			(void) Print(arg);
		}
	}
	exit(0);
}

int
Print(name)
	char	*name;
{
	Raster	*ras, *RasterOpen();
	int	status;

	ras = RasterOpen(name, (char *) NULL);
	if (ras == (Raster *) NULL) {
		(void) RasterPrintError(name);
		exit(1);
	}

	status = RasterRead(ras);
	if (status == RAS_ERROR) {
		(void) RasterPrintError( (char *) NULL );
		exit(1);
	}

	status = RasterPrintInfo(ras);
	if (status == RAS_ERROR) {
		(void) RasterPrintError( (char *) NULL );
		exit(1);
	}

	if (OptionPrintColors) {
		(void) fprintf(stderr, "\nColor Table\n");
		(void) fprintf(stderr,   "-----------\n");
		status = RasterPrintColors(ras);
		if (status == RAS_ERROR) {
			(void) RasterPrintError( (char *) NULL );
			exit(1);
		}
	}
	status = RasterClose(ras);
	if (status == RAS_ERROR) {
		(void) RasterPrintError( (char *) NULL );
		exit(1);
	}
}

Usage()
{
	(void) fprintf(stderr, "usage: rasinfo rasterfile\n");
	exit(1);
}
