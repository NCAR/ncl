/*
 *	$Id: ictrans.c,v 1.18 1993-01-12 20:11:50 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/
/*
 * test of cvs 1.3beta2
 */
/*LINTLIBRARY*/

/*
 *	ictrans.c
 *
 *	Author		John Clyne
 *
 *	Date		Fri Apr 13 13:34:33 MDT 1990
 *
 *	This is the program driver for ictrans
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#ifndef	CRAY
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#include <ncarg/cgm_tools.h>
#include <ncarg/c.h>
#include "ictrans.h"
#include "lex.h"
#include "get_cmd.h"

extern	char	yytext[];

extern	IcState	icState;
extern	int	spoolerJobs;

static	struct	{
	char	*device;	/* the device name		*/
	char	*font;		/* the font name		*/
	boolean	soft_fill;	/* software fill of piolygons	*/
	boolean	do_bell;	/* turn the bell on		*/
	float 	min_line_width;	/* minimum line width		*/
	float 	max_line_width;	/* maximun line width		*/
	float 	line_scale;	/* additional line scaling	*/
	char	*pal;		/* optional color palette       */
	int	fd;		/* output file descriptor	*/
	boolean	version;	/* software fill of piolygons	*/
	} opt;

static	OptDescRec	set_options[] = {
	{"device", 1, NULL, "Specify output device type"},
	{"font", 1, NULL, "Specify path to font"},
        {"softfill", 0, NULL, "Do perform polygon scan conversion in software"},
        {"bell", 0, NULL, "Do ring bell between frames"},
	{"lmin", 1, "-1", "Set minimum line width"},
	{"lmax", 1, "-1", "Set maximum line width"},
	{"lscale", 1, "-1", "Scale all lines by 'arg0'"},
	{"pal", 1, NULL, "'arg0' is a color palette file"},
        {"fdn", 1, "-1", ""},
        {"Version", 0, NULL, "Print version and exit"},
	{NULL}
};

static	Option	get_options[] = {
	{"device", NCARGCvtToString, (Voidptr) &opt.device, sizeof(opt.device)},
	{"font", NCARGCvtToString, (Voidptr) &opt.font, sizeof(opt.font)},	
	{"softfill", NCARGCvtToBoolean, (Voidptr) &opt.soft_fill, 
						sizeof (opt.soft_fill )},
	{"bell", NCARGCvtToBoolean, (Voidptr) &opt.do_bell, 
						sizeof (opt.do_bell )},
        {"lmin", NCARGCvtToFloat, (Voidptr) &opt.min_line_width, 
						sizeof (opt.min_line_width)},
        {"lmax", NCARGCvtToFloat, (Voidptr) &opt.max_line_width, 
						sizeof (opt.max_line_width )},
        {"lscale", NCARGCvtToFloat, (Voidptr) &opt.line_scale, 
						sizeof (opt.line_scale )},
	{"pal", NCARGCvtToString, (Voidptr) &(opt.pal), sizeof(opt.pal)},
	{"fdn", NCARGCvtToInt, (Voidptr) &(opt.fd), sizeof(opt.fd)},
	{"Version", NCARGCvtToBoolean, (Voidptr) &opt.version, 
						sizeof (opt.version )},

	{NULL}
	};

extern	boolean	*softFill;
extern	boolean	*doBell;

char	*progName;


usage(od, msg)
	int	od;
	char	*msg;
{
	char	*usage = "-d device [-f font] [-e cmd]+ [options] [device options]";

	if (msg) fprintf(stderr, "%s: %s\n", progName, msg);

	fprintf(stderr, "Usage: %s %s\n", progName, usage);
	PrintOptionHelp(od, stderr);

}


/*
 *	report errors from ctrans
 */
void	log_ct(err)
	CtransRC	err;
{
	char	*s;

	while (s = ReadCtransMsg()) {
		fprintf(
			stderr, "%s: %s - %s\n", progName, 
			(err == FATAL ? "FATAL" : "WARNING"), s
		);
	}
}


/*
 *	ICTrans
 *	PUBLIC
 *
 *	This function represents the complete programmatic interface
 *	to ictrans. 
 */
ICTrans(argc, argv, mem_cgm) 
	int	argc;
	char	**argv;
	char	*mem_cgm;
{


	ICommand	icommand;
	int		status;

	char	*fcap,			/* path to font			*/
		*gcap;			/* path to device		*/
	boolean	batch = FALSE;		/* operate in batch mode?	*/
	int	od;
	FILE	*fp;			/* status messages/user interaction */
	char	*dev_null	= "/dev/null";	/* bit bucket		*/

	/*
	 * list of metafiles to process
	 */
	char	**meta_files = (char **) malloc ((argc * sizeof(char *)) + 1);
	int	i,j;


	extern	char	*GetCurrentAlias();


	progName = (progName = strrchr(argv[0], '/')) ?
						++progName : *argv;
	if (! meta_files) {
		(void) fprintf(
			stderr,"%s : malloc(%d)\n", progName,
			argc * sizeof(char *)
		);
		return(-1);
	}

	/*
	 *	parse command line argument. Separate ctrans specific
	 *	args from device specific args
	 */
	softFill = &opt.soft_fill;
	doBell = &opt.do_bell;;
	od = OpenOptionTbl();
	if (ParseOptionTable(od, &argc, argv, set_options) < 0) {
		fprintf(
			stderr, "%s : Error loading options - %s\n",
			progName, ErrGetMsg()
		);
		return(-1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(od, get_options) < 0) {
		fprintf(
			stderr, "%s : Error getting options - %s\n",
			progName, ErrGetMsg()
		);
		PrintOptionHelp(od, stderr);
		return(-1);
	}


	if (opt.version) {
		PrintVersion(progName);
		return(0);
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
		usage(od, ErrGetMsg());
		return(-1);
	}

	icState.device = (icState.device = strrchr(gcap, '/')) ?
						++icState.device : gcap;


        /*
	 *	if a font was given on command line build the full path
	 *	to it. Else try and get font from the FONTCAP variable
	 *	Else try and use the default font
         */
        if ((fcap = getFcapname( opt.font )) == NULL) {
		/*
		*	use default font
		*/
		if ((fcap = getFcapname( DEFAULTFONT )) == NULL) {
                        fprintf(
                                stderr,"%s: Warning - no known font [ %s ]\n",
                                progName, ErrGetMsg()
                        );

		}
        }

	icState.font = (icState.font = strrchr(fcap, '/')) ?
						++icState.font : fcap;

	/*
	 * inform ctrans to use a default color palette if one was
	 * requested
	 */
	if (opt.pal) {
		SetDefaultPalette(opt.pal);
	}


	/*
	 *	init ctrans
	 */
	if (init_ctrans(&argc,argv,gcap,fcap,TRUE) != OK) {
		log_ct(FATAL);
		return(-1);
	}

	/*
	 * assume everthing left in the command line is a metafile. check
	 * and make sure.
	 */
	meta_files[0] = NULL;
	for (i = 1, j=0; i < argc; i++) {
		if (*argv[i] == '-') {
			if (! strcmp(argv[i], "-e")) {
				i++;
				if (i >= argc) {
					usage(od, "No such option");
					return(-1);
				}
				(void) AppendString(argv[i]);
				batch = TRUE;
			}
			else {
				usage(od, "No such option");
				return(-1);
			}
		} 
		else {
			meta_files[j++] = argv[i];
		}
	}

	meta_files[j] = NULL;	/* terminate list	*/

	/*
	 *	if a metafile was not given exit
	 */
	if (meta_files[0] == NULL && !mem_cgm) {
		usage(od, "Metafile not specified");
		return(-1);
	}

	icState.file = meta_files;	/* record the file name	*/

	/*
	 * initialize the print spooler stuff
	 */
	InitSpool();
	icState.spool_alias = GetCurrentAlias();

	if (init_icommand(&icommand) < 0) return(-1);

	/*
	 * establish communication path (one way) with invokee
	 */
	if (opt.fd != -1) {
		if ((fp = fdopen(opt.fd, "w")) == NULL) {
			fprintf(stderr, "fdopen(%d, w)\n", opt.fd);
			return(-1);
		}
		setbuf(fp, (char *) NULL);	/* make unbuffered	*/
	}
	else if (! isatty(fileno(stdout))) {

		/*
		 * graphics go to stdout, everything else goes to
		 * bit bucket
		 */
		if ((fp = fopen(dev_null, "w")) == NULL) {
			fprintf(stderr, "fdopen(%s, w)\n", dev_null);
			return(-1);
		}
	}
	else {
		fp = stdout;
	}
	icommand.fp = fp;

	/*
	 * prime the system by executing a file() command
	 */
	if (mem_cgm) {	/* CGM in memory?	*/
		(void) processMemoryCGM(&icommand, mem_cgm);
	} 
	else {		/* CGM on disk		*/
		icommand.cmd.name = "file";
		icommand.cmd.data = meta_files[0];
		(void) ex_command(&icommand);
	}

	/*
	 * if in batch mode don't prompt user for input
	 */
	if (batch) {
		icommand.cmd.name = "movie";
		icommand.cmd.data = NULL;
		(void) ex_command(&icommand);
	}

	/*
	 * loop forever processing user commands
	 */
	while (1) {

		/*
		 * clean up any terminated process spawned by the 
		 * spooler
		 */
#ifndef	CRAY
		while ((status = wait3((union wait *) NULL, 
			WNOHANG, (struct rusage *) NULL)) != 0) {

			if (status > 0) {
				(void) fprintf(fp, "Done	%d\n", status);
				spoolerJobs--;
			}
			else {
#ifdef	DEAD
				(void) fprintf(stderr,"bad status %d\n",status);
				perror(NULL);
#endif
				break;
			}
		}
#endif
		

		status = get_command(&icommand);


		switch (status) {

		case GET_EOLN:  /* normal termination   */
			if (ex_command(&icommand) == 0) {	/* quit	*/
				(void) close_ctrans();
				return(0);
			}
			break;
		case GET_OUTOFRANGE:
			(void) fprintf(stderr,
				"Address Out of Range. There are %d frames\n",
				icommand.last_frame);
			break;
		case GET_SYMBOL:
			(void) fprintf(stderr, "Invalid Symbol: %s\n", yytext);
			break;
		case GET_SYNTAX:
			(void) fprintf(stderr, "Invalid Syntax: %s\n", yytext);
			break;
		case GET_EOF:
			/*
			 *	terminate ctrans
			 */
			(void) close_ctrans();
			return(0);
			break;
		}
	}
}

int	ex_command(ic)
	ICommand	*ic;
{

	CmdOp	*c;
	CmdOp	*getcmdOp();


	/*
	 * find the command object in the command table
	 */
	c = getcmdOp(ic->cmd.name);

	if (c == (CmdOp *) -1) {
		(void) fprintf(stderr, "%s: Ambiguous command?\n",progName);
		return(-1);
	}
	else if (c == (CmdOp *) NULL) {
		(void) fprintf(stderr, "%s: Invalid command?\n",progName);
		return(-1);
	}
	else {

		/*
		 * execute the command
		 */
		ic->c = c;
		return((*c->c_handler) (ic));
	}
}
