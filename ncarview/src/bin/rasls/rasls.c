#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <ncarg/ncarg_ras.h>

static char	*ProgramName		= (char *) NULL;

static struct {
	boolean		type;
	boolean		count;
	boolean		resolution;
	boolean		help;
	boolean		verbose;
	boolean		version;
	char		*format;
} opt;

/* Options we want parsed. */

static  OptDescRec      set_options[] = {
	{"type",	0, NULL, "Print rasterfile encoding type"},
	{"count",	0, NULL, "Print number of frames in rasterfile"},
	{"resolution",	0, NULL, "Print spatial resolution"},
	{"help",	0, NULL, "Print help information"},
	{"verbose",	0, NULL, "Set verbose mode"},
	{"Version",	0, NULL, "Print version number"},
        {"ifmt", 	1, NULL, "Specify format of input file"},
	{NULL}
};

static	Option	get_options[] = {
{"type",    NCARGCvtToBoolean, (Voidptr) &opt.type, sizeof(opt.type)},
{"count",   NCARGCvtToBoolean, (Voidptr) &opt.count, sizeof(opt.count)},
{"resolution",   NCARGCvtToBoolean, (Voidptr) 
				&opt.resolution, sizeof(opt.resolution)},
{"help",    NCARGCvtToBoolean, (Voidptr) &opt.help, sizeof(opt.help)},
{"verbose", NCARGCvtToBoolean, (Voidptr) &opt.verbose, sizeof(opt.verbose)},
{"Version", NCARGCvtToBoolean, (Voidptr) &opt.version,sizeof(opt.version)},
{"ifmt",    NCARGCvtToString,  (Voidptr) &opt.format, sizeof(opt.format)},
{NULL}
};

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int		estatus = 0;
	int		i;
	int		opt_id;

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

	if (opt.version) {
		(void) PrintVersion(ProgramName);
		exit(0);
	}

	/* Make sure nothing left on command line execpt file names. */

	if (argc < 2) {
		Usage(ProgramName, (char *) NULL, opt_id);
	}

	for (i=1; i<argc; i++) {
		if (*argv[i] == '-') {
			(void) fprintf(stderr,
				"%s: Unknown option \"%s\"\n",
				ProgramName, argv[i]);
			Usage(ProgramName, (char *) NULL, opt_id);
		}
		else {
			if (opt.type || opt.count || opt.resolution) {
				if (Print(argv[i], opt.format) != RAS_OK) {
					estatus++;
				};
			}
			else {
				if (PrintLs(argv[i], opt.format) != RAS_OK) {
					(void) fprintf(stderr,
						"%s: %s\n",
						ProgramName, ErrGetMsg());

					estatus++;
				}
			}
		}
	}
	exit(estatus);
}

int PrintLs(name, fformat)
	char	*name;
	char	*fformat;
{
	int		status;
	Raster		*ras, *RasterOpen();
	int		error_number = 0;
	char		*format, *desc;
	struct stat	statb;

	status = stat(name, &statb);
	if (status < 0) {
		(void) ESprintf(errno, "stat(\"%s\",buf)", name);
		return(RAS_ERROR);
	}

	if (S_ISDIR(statb.st_mode)) {
		(void) PrintLine(name,
			0, 0, RAS_UNKNOWN, "**Directory**");
		return(RAS_ERROR);
	}

	if (statb.st_size == 0) {
		(void) PrintLine(name,
			0, 0, RAS_UNKNOWN, "****Empty****");
		return(RAS_ERROR);
	}

	ras = RasterOpen(name, fformat);
	if (ras == (Raster *) NULL) {
		error_number = ErrGetNum();
		if (error_number == RAS_E_UNKNOWN_FORMAT) {
			(void) PrintLine(name,
				0, 0, RAS_UNKNOWN, "Unknown Format");
		}
		else {
			(void) PrintLine(name,
				0, 0, RAS_UNKNOWN, "****Bogus****");
		}
			
		if (opt.verbose) {
			error_number = ErrGetNum();
			if (error_number != RAS_E_UNKNOWN_FORMAT) {
				(void) RasterPrintError(name);
				return(RAS_ERROR);
			}
		}
		return(RAS_ERROR);
	}

	status = RasterRead(ras);
	if (status != RAS_OK) {
		(void) PrintLine(name, 0, 0, RAS_UNKNOWN, "Unknown Format");
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
Print(name, fformat)
	char	*name;
	char	*fformat;
{
	int		status;
	RasStat		ras_stat;
	int		frame_count;

	status = RasterStat(name, fformat, &ras_stat, &frame_count);
	if (status == RAS_ERROR) {
		(void) RasterPrintError(name);
		return(RAS_ERROR);
	}

	if (opt.type) {
		if (ras_stat.type == RAS_INDEXED) {
			(void) fprintf(stdout, "indexed\n");
		}
		else if (ras_stat.type == RAS_DIRECT) {
			(void) fprintf(stdout, "direct\n");
		}
	}

	if (opt.count) {
		(void) fprintf(stdout, "%d\n", frame_count);
	}

	if (opt.resolution) {
		(void) fprintf(stdout, "%dx%d\n", ras_stat.nx, ras_stat.ny);
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
	(void) sprintf(&msgbuf[16], "%s", RasterTypeString(type));
	(void) sprintf(&msgbuf[30], "%dX%d", nx, ny);
	(void) sprintf(&msgbuf[42], "%s", name);

	for(i=0; i<512; i++) {
		if (msgbuf[i] == '\0' || msgbuf[i] == '\n') {
			msgbuf[i] = ' ';
		}
		if (msgbuf[i] != ' ') {
			lastchar = i;
		}
	}
	msgbuf[lastchar+1] = '\0';
	(void) fprintf(stdout, "%s\n", msgbuf);
	return(RAS_OK);
}

Usage(progName, message, opt_id)
	char	*progName;
	char	*message;
	int	opt_id;
{

	if (message) {
		(void) fprintf(stderr, "%s: %s", progName, message);
	}

	(void) fprintf(stderr, 
		"%s: Usage: %s [-help] [-verbose] [-Version] files\n",
		progName, progName);
	PrintOptionHelp(opt_id, stderr);
	exit(1);
}
