#include <stdio.h>
#include <fcntl.h>
#include "ncarg/c.h"
#include "ncarg/ncarg_ras.h"

static char	*ProgramName		= (char *) NULL;

static struct {
	boolean		help;
	boolean		version;
} opt;

/* Options we want parsed. */

static  OptDescRec      set_options[] = {
	{"help",	0, NULL, "Print help message"},
	{"Version",	0, NULL, "Print version number and exit"},
	{NULL}
};

static	Option	get_options[] = {
{"help",    NCARGCvtToBoolean, (Voidptr) &opt.help,   sizeof(opt.help)},
{"Version", NCARGCvtToBoolean, (Voidptr) &opt.version,sizeof(opt.version)},
{NULL}
};

static void	Usage();
static int	PrintPalette();

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	i;
	int	opt_id;

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

	if (argc == 1 || opt.help) {
		Usage(ProgramName, (char *) NULL, opt_id);
		exit(0);
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
			(void) PrintPalette(argv[i]);
		}
	}
}

static int PrintPalette(name)
	char	*name;
{
	int		i, status;
	Raster		*ras, *RasterOpen();

	ras = RasterOpen(name, (char *) NULL);
	if (ras == (Raster *) NULL) {
		(void) RasterPrintError(name);
		return(RAS_ERROR);
	}

	status = RasterRead(ras);

	/* Terminate for error or EOF */

	if (status != RAS_OK) {
		(void) RasterPrintError(name);
		return(RAS_ERROR);
	}

	if (ras->type == RAS_DIRECT) {
		(void) fprintf(stderr,
		"%s: %s is a DirectColor file and has no palette\n",
		ProgramName, name);
		return(RAS_OK);
	}
	else if (ras->type == RAS_INDEXED) {
		for(i=0; i<ras->ncolor; i++) {
			(void) fprintf(stdout,
				"%3d %3d %3d %3d\n", i,
				ras->red[i],
				ras->green[i],
				ras->blue[i]);
		}
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
		"%s: Usage: %s [-help] [-Version] rasterfile\n",
		progName, progName);
	PrintOptionHelp(opt_id, stderr);
	(void) fprintf(stderr,
		"\n    Example:   rasgetpal temp.sun >temp.txt\n\n");
	exit(1);
}
