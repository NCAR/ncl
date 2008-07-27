/*
 *	$Id: main.c,v 1.40 2008-07-27 03:18:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*
 *	main.c:
 *
 *
 *	Authors		John Clyne	(clyne@redcloud.ucar.edu)
 *			Tinsley Galyean	(taga@boulder.colorado.edu)
 *
 *	Date		Mon Nov 28 13:54:09 MST 1988
 *
 *	
 *		This is the main driver for ctrans. It is responsible
 *	for parsing the command line arguments, intializing ctrans
 *	and processing the named metafile.
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<ctype.h>
#include	<errno.h>
#include	<sys/types.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<ncarg/c.h>
#include	<ncarg/ctrans.h>
#include	"main.h"

#define	NCAR_REC_SIZE	1440


/*
 *	a global structure that contains values of command line options
 */
static	struct	{
	char	*device;	/* the device name		*/
	char	*font;		/* the font name		*/
	int	movie;		/* movie or batch mode		*/
	boolean	soft_fill;	/* software fill of polygons	*/
	boolean	debug;		/* software fill of polygons	*/
	boolean	do_bell;	/* turn off the bell		*/
	boolean	read_stdin;	/* read metafile from stdin	*/
	float 	min_line_width;	/* minimum line width		*/
	float 	max_line_width;	/* maximun line width		*/
	float 	line_scale;	/* additional line scaling	*/
	char	*pal;		/* optional color palette	*/
	boolean	quiet;	/* optional color palette	*/
	boolean	version;	/* print the verison number	*/
	boolean	verbose;	/* operate in verbose mode	*/
	boolean	help;		/* print usage string		*/
	boolean	pause;		/* pause between frames		*/
	} opt;
	

static	OptDescRec	set_options[] = {
	{"device", 1, NULL, "Specify output device type"},
	{"font", 1, "font1", "Specify path to font"},
	{"movie", 1, "-1", "Operate in batch mode with 'arg0' second delay"},	
        {"softfill", 0, NULL, "Do perform polygon scan conversion in software"},
        {"Debug", 0, NULL, "Do operate in debug mode"},
        {"bell", 0, NULL, "Do ring bell between frames"},
        {"", 0, NULL, "Read metafile from the standard input"},
	{"lmin", 1, "-1", "Set minimum line width"},
	{"lmax", 1, "-1", "Set maximum line width"},
	{"lscale", 1, "-1", "Scale all lines by 'arg0'"},
	{"pal", 1, NULL, "'arg0' is a color palette file"},
	{"quiet", 0, NULL, "Be quiet - Don't report non-fatal errors"},
        {"Version", 0, NULL, "Print version and exit"},
        {"verbose", 0, NULL, "Operate in verbose mode"},
        {"help", 0, NULL, "Print usage and exit"},
        {"pause", 0, NULL, "Pause between frames until user hits <RETURN>"},
	{NULL}
	};

static	Option	get_options[] = {
	{"device", NCARGCvtToString, (Voidptr)&(opt.device),sizeof(opt.device)
	},
	{"font", NCARGCvtToString, (Voidptr) &opt.font, sizeof(opt.font)
	},
	{"movie", NCARGCvtToInt, (Voidptr) &opt.movie, sizeof(opt.movie)
	},
        {"softfill", NCARGCvtToBoolean, (Voidptr) &opt.soft_fill, 
							sizeof (opt.soft_fill)
	},
        {"Debug", NCARGCvtToBoolean, (Voidptr) &opt.debug, sizeof (opt.debug)
	},
        {"bell", NCARGCvtToBoolean,(Voidptr)&opt.do_bell,sizeof(opt.do_bell)
	},
        {"",NCARGCvtToBoolean,(Voidptr)&opt.read_stdin, sizeof(opt.read_stdin)
	},
	{"lmin", NCARGCvtToFloat, (Voidptr) &opt.min_line_width, 
					sizeof(opt.min_line_width )
	},
        {"lmax", NCARGCvtToFloat, (Voidptr) &opt.max_line_width, 
						sizeof (opt.max_line_width )
	},
        {"lscale", NCARGCvtToFloat, (Voidptr) &opt.line_scale, 
						sizeof (opt.line_scale)
	},
	{"pal", NCARGCvtToString, (Voidptr) &(opt.pal), sizeof(opt.pal)
	},
        {"quiet",NCARGCvtToBoolean,(Voidptr)&opt.quiet,sizeof(opt.quiet)
	},
        {"Version",NCARGCvtToBoolean,(Voidptr)&opt.version,sizeof(opt.version)
	},
        {"verbose",NCARGCvtToBoolean,(Voidptr)&opt.verbose,sizeof(opt.verbose)
	},
        {"help",NCARGCvtToBoolean,(Voidptr)&opt.help,sizeof(opt.help)
	},
        {"pause",NCARGCvtToBoolean,(Voidptr)&opt.pause,sizeof(opt.pause)
	},
	{NULL
	}
};

/*
 *	module globals for error reporting
 */
static  char    logFile[80];		/* temp log file name		*/
static	FILE	*logFP;			/* log file pointer		*/
static	boolean	doLogToFile = FALSE;	/* log messags to a temp file?	*/

static	int	eStatus;		/* exit status			*/
static	char	*progName;

extern	boolean *softFill;
extern	boolean *deBug;
extern	boolean *doBell;

usage(od, progName, msg)
	int	od;
	char	*progName;
	char	*msg;
{
	char	*usage = "[options] [device options] <metafile... | ->";

	if (msg) fprintf(logFP, "%s: %s\n", progName, msg);

	fprintf(logFP, "Usage: %s %s\n", progName, usage);
	fprintf(logFP, "\nWhere \"options\" are:\n\n");
	PrintOptionHelp(od, logFP);

}


static	void	open_log()
{
	(void) strcpy(logFile, TMPDIR);
	(void) strcat(logFile, TMPFILE);

	(void) mktemp(logFile);

	if ((logFP = fopen(logFile,"w")) == NULL) {
		logFP = stderr;	/* use stderr instead	*/
		fprintf(logFP, "%s: fopen(%s, w)\n", progName, logFile);
	}
	else {
		doLogToFile = TRUE;
	}
	setbuf(logFP, NULL);  /* turn off buffered i/o        */
}

static	void	close_log()
{
	int	c;

	if (doLogToFile) {
		if (logFP != stderr) (void) fclose(logFP);
		if ((logFP = fopen(logFile,"r")) == NULL) {
			logFP = stderr;	/* use stderr instead	*/
			fprintf(logFP, "%s: fopen(%s, r)\n", progName, logFile);
			return;
		}
	}

	while ((c = getc(logFP)) != EOF) {
		putc(c, stderr);
	}
	if (logFP != stderr) (void) fclose(logFP);

	doLogToFile = FALSE;
	logFP = stderr;
	(void) unlink(logFile);
	
}

static	void	log_ct(err)
	CtransRC	err;
{
	char	*s;

	while (s = ReadCtransMsg()) {
		fprintf(
			logFP, "%s: %s - %s\n", progName, 
			(err == FATAL ? "FATAL" : "WARNING"), s
		);
	}
}

static	void	cleanup(err)
	int	err;
{
	close_log();
	exit(err);
}

int	process(record, batch, sleep_time, tty, verbose, do_all)
	int		record;
	boolean		batch;
	unsigned	sleep_time;
	FILE		*tty;
	boolean		verbose;
	boolean		do_all;
{
	CtransRC	ctrc;
	static		int	frame_count = 1;
	int		status = 0;
	boolean		abort = FALSE;

	do {
		ctrc = ctrans(record);

		switch ((int) ctrc) {
		case	FATAL:
			log_ct(FATAL);
			return(-1);

		case	WARN:
			if (! opt.quiet) log_ct(WARN);
			break;

		case	OK:
			break;

		case	EOM:
			abort = TRUE;
			do_all = FALSE;
			break;
		}
	
		if (verbose  && ! abort) {
			fprintf( logFP, "plotted %d frames\n", frame_count++);
		}

		if (tty && ! abort) {
			int	c;
			while ((c = getc(tty)) != EOF) {
				if (c == '\n') break;
			}
			if (c == EOF)  return(-1);

		} else if (batch && ! abort) {
			if (sleep_time>0) sleep(sleep_time);
			CtransClear();
		}
	
	} while (do_all);

	return(status);
}

/*
 *      sigint_handler
 *
 *              interupt signal handler
 */
static	void    sigint_handler(sig)
	int	sig;
{
	close_metafile();
	close_ctrans();
	cleanup(eStatus);
}

/*
**
**	M A I N   P R O G R A M
**
*/

main(argc,argv)
int	argc;
char	**argv;
{

	char	*fcap,			/* path to font			*/
		*gcap;			/* path to device		*/
	Cgm_fd	cgm_fd;			/* CGM file descriptor		*/
	boolean batch = FALSE;		/* device library should interact
					 * with the user. Else main module
					 * is responsible for interaction
					 */
	int sleep_time = 0;	/* time to wait between displaying
					 * frames if batch is TRUE
					 */
	int	*record = NULL;		/* list of records to process	*/

					/*
					 * list of metafiles to process
					 */
	FILE	*tty = NULL;		/* tty for prompting user	*/

	char	**meta_files;
	int	i,j;
	int	od;
	CtransRC	ctrc;

	logFP = stderr;			/* log to stderr by default	*/
	progName = (progName = strrchr(argv[0], '/')) ? ++progName : *argv;

 	meta_files = (char **) malloc ((unsigned) ((argc * sizeof(char *)) +1));

	/*
	 *	parse command line argument. Separate ctrans specific
	 *	args from device specific args
	 */
	softFill = &opt.soft_fill;
	deBug = &opt.debug;
	doBell = &opt.do_bell;
	od = OpenOptionTbl();
	if (ParseOptionTable(od, &argc, argv, set_options) < 0) {
		fprintf(
			logFP, "%s : Error parsing options [ %s ]\n", 
			progName, ErrGetMsg()
		);
		cleanup(1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(od, get_options) < 0) {
		fprintf(
			logFP, "%s : Error getting options [ %s ]\n", 
			progName, ErrGetMsg()
		);
		PrintOptionHelp(od, logFP);
		cleanup(1);
	}

	/*
 	 * the pause option and the movie option option are mutually
	 * exclusive. ignore the movie option if both are present
	 */
	if (! opt.pause) {
		batch = opt.movie != -1;
		sleep_time = opt.movie;
	}
	else {
		/*
		 * open tty to read user prompt
		 */
		char	*tty_in = "/dev/tty";
		if (! (tty = fopen(tty_in, "r"))) {
			fprintf(
				logFP, "%s : Error opening tty(%s) [ %s ]\n",
				progName, tty_in, strerror(errno)
			);
			cleanup(1);
		}
	}

	if (opt.version) {
		PrintVersion(progName);
		cleanup(0);
	}
	if (opt.help) {
		usage(od, progName, NULL);
		cleanup(0);
	}

	/*
	 * set line scaling options
	 */
	if (opt.min_line_width > -1) 
		SetMinLineWidthDefault(opt.min_line_width);
	if (opt.max_line_width > -1) 
		SetMaxLineWidthDefault(opt.max_line_width);
	if (opt.line_scale > -1) 
		SetAdditionalLineScale(opt.line_scale);

        /*
	 *	If a device was given on command line build the full path
	 *	to it. Else try and get device from the GRAPHCAP variable
	 *	Note: if the device is a software library interface and
	 *	not a graphcap then a path will still be built to it but
	 *	it will have no meaning.
         */
        if ((gcap = getGcapname( opt.device )) == NULL ) {
		usage(od, progName, ErrGetMsg());
		cleanup(1);
	}


        /*
	 *	if a font was given on command line build the full path
	 *	to it. Else try and get font from the FONTCAP variable
	 *	Else try and use the default font
         */
        if ((fcap = getFcapname( opt.font )) == NULL) {

		/*
		 *	try to use default font
		 */
		if ((fcap = getFcapname( DEFAULTFONT )) == NULL) {
			fprintf(
				logFP,"%s: Warning - no known font [ %s ]",
				progName, ErrGetMsg()
			);
		}
        }

	/*
	 * inform ctrans to use a default color palette if one was
	 * requested
	 */
	if (opt.pal) {
		SetDefaultPalette(opt.pal);
	}

	if (!batch) (void)signal(SIGINT,&sigint_handler);


	/*
	 *	init ctrans
	 */
	if ((ctrc = init_ctrans(&argc, argv, gcap, fcap, batch)) != OK) {
		log_ct(ctrc);
		if (ctrc == FATAL) cleanup(1);
	}
	/*
	 * if graphical output is going to a tty log error messages to 
	 * a file that can be catted out later.
	 */
	if (IsOutputToTty()) {
		open_log();
	}

	/*
	 * assume everthing left in the command line is a metafile or 
	 * a record list. 
	 */
	i = 1;
	/*
	 *	look for rec list
	 */
	if (i < argc && strcmp (argv[i], "-record") == 0) {
		i++;
		j = 0;
		record = (int *) malloc ((argc + 1) * sizeof (int));
		while (IsAsciiInt(argv[i])) {

			record[j++] = atoi(argv[i++]);
		}
		record[j] = -1;
	}
			
	/*
	 * grab metafiles
	 */
	meta_files[0] = NULL;
	for (j = 0 ; i < argc; i++, j++) {
		if (*argv[i] == '-') {
			usage(od, progName, NULL);
			cleanup(1);
		} 
		else {
			meta_files[j] = argv[i];
		}
	}

	meta_files[j] = NULL;	/* terminate list	*/

	/*
	 *	if a metafile was not given read from stdin
	 */
	if (meta_files[0] == NULL) {
		meta_files[0] = "-"; meta_files[1] = NULL;
	}


	for ( ; *meta_files; meta_files++) {

		/*
		 *	open the metafile
		 */
		if ((cgm_fd = CGM_open(*meta_files, NCAR_REC_SIZE, "r")) < 0) {
			fprintf(
				logFP, "%s: Can't open metafile(%s) [ %s ]\n",
				progName, *meta_files, strerror(errno)
			);
			continue;
		}


		/* 
		 *	init the metafile we wish to process. CGM allows 
		 *	multible metafiles to reside in a single file. This 
		 *	driver will only process the first.
		 */
		ctrc = init_metafile(NEXT, cgm_fd);
		if (ctrc == FATAL) {
			(void) CGM_close(cgm_fd);
			log_ct(FATAL);
			continue;	/* skip to next metafile	*/
		}
		else if (ctrc == WARN) {
			if (! opt.quiet) log_ct(WARN);
		}

	
		if (record == NULL) {	/* process all records in file	*/
			/* 
			 *	process frames until the end of the file or an 
			 *	unrecoverable error occurs
			 */
			if (process(NEXT, batch, (unsigned) 
					sleep_time, tty, opt.verbose,TRUE) < 0){
				eStatus = 1;
			}
		}
		else {
			/* 
			 * process single frames begining at record `record`
			 */
			i = 0;
			while (record[i] != -1) {
				if (process(record[i++], batch, (unsigned) 
					sleep_time, tty, opt.verbose,FALSE)< 0){

					eStatus = 1;
				}
			}
		}

		close_metafile();
		(void) CGM_close(cgm_fd);

	}	/* while	*/

	/*
	 *	terminate ctrans
	 */
	(void) close_ctrans();

	cleanup(eStatus);
}
