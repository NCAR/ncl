#include <stdio.h>
#include <fcntl.h>
#include "ncarg_ras.h"

static char	*ProgramName		= (char *) NULL;
int		OptionDebug		= False;
char		*OptionColorfile	= (char *) NULL;
int		OptionVerbose		= False;

static struct {
	char		*colorfile;	
	boolean		verbose;
	boolean		version;
} opt;

/* Options we want parsed. */

static  OptDescRec      set_options[] = {
	{"colorfile",	1, NULL, "Specify a file to save the colortable in"},
	{"verbose",	0, NULL, "Set verbose mode (ls mode is default)"},
	{"Version",	0, NULL, "Print version number end exit"},
	{NULL}
};

static	Option	get_options[] = {
{"colorfile", NCARGCvtToString, (Voidptr)&opt.colorfile,sizeof(opt.colorfile)},
{"verbose", NCARGCvtToBoolean, (Voidptr) &opt.verbose, sizeof(opt.verbose)},
{"Version", NCARGCvtToBoolean, (Voidptr)&opt.version,sizeof(opt.version)},
{NULL}
};

static int	_RaslsPrint();
static void	Usage();

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	i;
	char	*arg;
	int	opt_id;

	if (argc == 1) {
		fprintf(stderr, "Wrong number of arguments\n");
		Usage();
	}

	ProgramName = argv[0];

	RasterInit(&argc, argv);

	/*
	 * register the options we're interested in and have them parsed
	 */
	opt_id = OpenOptionTbl();
	if (ParseOptionTable(opt_id, &argc, argv, set_options) < 0) {
		(void) fprintf(
			stderr,"%s : Error parsing command line options : %s\n",
			ProgramName, ErrGetMsg());
		exit(1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(opt_id, get_options) < 0) {
		(void) fprintf(
			stderr,"%s : GetOptions(,) : %s\n",
			ProgramName,ErrGetMsg());
		exit(1);
	}

	/* Make sure nothing left on command line execpt file names. */

	if (argc < 2) {
		(void) fprintf(stderr,
			"%s: No filenames on command line\n", ProgramName);
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
			(void) _RaslsPrint(argv[i]);
		}
	}
}

static int _RaslsPrint(name)
	char	*name;
{
	int		i, status;
	Raster		*ras, *RasterOpen();
	int		errno = 0;
	char		*format, *desc;
	int		nx, ny;
	RasterEncoding	type;

	ras = RasterOpen(name, (char *) NULL);
	if (ras == (Raster *) NULL) {
		errno = ErrGetNum();
		if (errno != RAS_E_UNKNOWN_FORMAT) {
			(void) RasterPrintError(name);
			return(RAS_ERROR);
		}

		if (opt.verbose) {
			(void) fprintf(stdout,
				"%s: \"%s\" is in an unknown format\n");
		}
		else {
			(void) PrintLine(name, 0, 0, RAS_UNKNOWN,
					"Unknown Format");
		}
		return(RAS_ERROR);
	}

	status = RasterRead(ras);
	if (status != RAS_OK) {
		(void) RasterPrintError();
		(void) RasterClose(ras);
		return(RAS_ERROR);
	}

	status = RasterGetValues(ras,	NrtNformatName, &format,
					NrtNformatDesc, &desc,
					NULL);
	if (status != RAS_OK) {
		(void) RasterPrintError();
		(void) RasterClose(ras);
		return(RAS_ERROR);
	}

	/* Otherwise print the information about the rasterfile. */

	if (opt.verbose) {
		status = RasterPrintInfo(ras);
		if (status == RAS_ERROR) {
			(void) RasterClose(ras);
			(void) RasterPrintError(name);
			return(RAS_ERROR);
		}
	}
	else {
		(void) PrintLine(name, ras->nx, ras->ny, ras->type, desc);
	}

	status = RasterClose(ras);
	if (status == RAS_ERROR) {
		(void) RasterPrintError( (char *) NULL );
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

int
PrintLine(name, nx, ny, type, desc)
	char		*name;
	int		nx, ny;
	RasterEncoding	type;
	char		*desc;
{
	int	i, lastchar;
	char	msgbuf[512];

	for(i=0; i<512; i++) msgbuf[i] = ' ';

	(void) sprintf(msgbuf, "%s", desc);
	(void) sprintf(&msgbuf[32], "%s", RasterTypeString(type));
	(void) sprintf(&msgbuf[48], "%dX%d", nx, ny);
	(void) sprintf(&msgbuf[60], "%s", name);

	for(i=0; i<512; i++) {
		if (msgbuf[i] == '\0' || msgbuf[i] == '\n') {
			msgbuf[i] = ' ';
		}
		if (msgbuf[i] != ' ') {
			lastchar = i;
		}
	}
	msgbuf[lastchar+1] = '\0';
	(void) fprintf(stderr, "%s\n", msgbuf);
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
"%s: Usage: %s [-Version] [-pal palette_file] [-quiet] [raster_file...]\n",
		progName, progName);
	PrintOptionHelp(opt_id, stderr);

	exit(1);
}
