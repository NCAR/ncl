/*
 *	$Id: main.c,v 1.20 1992-06-24 20:59:57 clyne Exp $
 */
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
#include	<ctype.h>
#include	<sys/types.h>
#include	<fcntl.h>

#ifdef	SYSV
#include        <string.h>
#else
#include        <strings.h>
#endif

#include	<cgm_tools.h>
#include        <ncarv.h>
#include	<cterror.h>
#include	"main.h"

#define	NCAR_REC_SIZE	1440

extern	char	*getFcapname();
extern	char	*getGcapname();
extern	Ct_err	init_ctrans();
extern	Ct_err	ctrans();

extern	char	*strrchr();
extern	char	*malloc();

char	*ProgramName;

/*
 *	a global structure that contains values of command line options
 */
static	struct	{
	char	*device;		/* the device name		*/
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
	boolean	version;	/* print the verison number	*/
	boolean	verbose;	/* operate in verbose mode	*/
	boolean	help;		/* print usage string		*/
	} opt;
	

static	OptDescRec	set_options[] = {
	{"device", 1, NULL, "Specify output device type"},
	{"font", 1, NULL, "Specify path to font"},
	{"movie", 1, "-1", "Operate in batch mode with 'arg0' second delay"},	
        {"softfill", 0, NULL, "Do perform polygon scan conversion in software"},
        {"Debug", 0, NULL, "Do operate in debug mode"},
        {"bell", 0, NULL, "Do ring bell between frames"},
        {"", 0, NULL, "Read metafile from the standard input"},
	{"lmin", 1, "-1", "Set minimum line width"},
	{"lmax", 1, "-1", "Set maximum line width"},
	{"lscale", 1, "-1", "Scale all lines by 'arg0'"},
	{"pal", 1, NULL, "'arg0' is a color palette file"},
        {"Version", 0, NULL, "Print version and exit"},
        {"verbose", 0, NULL, "Operate in verbose mode"},
        {"help", 0, NULL, "Print usage and exit"},
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
        {"Version",NCARGCvtToBoolean,(Voidptr)&opt.version,sizeof(opt.version)
	},
        {"verbose",NCARGCvtToBoolean,(Voidptr)&opt.verbose,sizeof(opt.verbose)
	},
        {"help",NCARGCvtToBoolean,(Voidptr)&opt.help,sizeof(opt.help)
	},
	{NULL
	}
};

extern	boolean *softFill;
extern	boolean *deBug;
extern	boolean *doBell;
	
main(argc,argv)
int	argc;
char	**argv;
{

	char	*fcap,			/* path to font			*/
		*gcap;			/* path to device		*/
	char	*program_name;
	Cgm_fd	cgm_fd;			/* CGM file descriptor		*/
	boolean batch = FALSE;		/* device library should interact
					 * with the user. Else main module
					 * is responsible for interaction
					 */
	unsigned sleep_time = 0;	/* time to wait between displaying
					 * frames if batch is TRUE
					 */
	int	*record = NULL;		/* list of records to process	*/

					/*
					 * list of metafiles to process
					 */
	char	**meta_files = (char **) 
			malloc ((unsigned) ((argc * sizeof(char *)) + 1));
	int	i,j;
	int	frame_count = 1;
	int	od;

	extern	void	SetDefaultPalette();

	/* put the program name in a global variable */

	program_name = (program_name = strrchr(argv[0], '/')) ?
						++program_name : *argv;

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
			stderr, "%s : Error parsing options [ %s ]\n", 
			program_name, ErrGetMsg()
		);
		exit(1);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(od, get_options) < 0) {
		fprintf(
			stderr, "%s : Error getting options [ %s ]\n", 
			program_name, ErrGetMsg()
		);
		PrintOptionHelp(od, stderr);
		exit(1);
	}

	/*
	 * 	init ctrans' error module so we can use it
	 */
	init_ct_error(program_name, TRUE);

	batch = opt.movie != -1;
	sleep_time = opt.movie;

	if (opt.version) {
		PrintVersion(program_name);
		exit(0);
	}
	if (opt.help) {
		usage(od, program_name, NULL);
		exit(0);
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
        if ((gcap = getGcapname( opt.device )) == NULL )
		ct_error(T_MD,"");


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
			ct_error(T_MF,"");
		}
        }

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
	if (init_ctrans(&argc,argv,program_name,gcap,fcap,TRUE,batch) != OK) {
		close_ctrans();
		exit(1);
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
		record = (int *) icMalloc ((argc + 1) * sizeof (int));
		while (isint(argv[i])) {

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
			usage(od, program_name, NULL);
			ct_error(T_NSO, "");
			break;
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


	while (*meta_files) {

		/*
		 *	open the metafile
		 */
		if ((cgm_fd = CGM_open(*meta_files, NCAR_REC_SIZE, "r")) < 0) {
			ct_error(T_FOE, *meta_files);
			exit(1);
		}


		/* 
		 *	init the metafile we wish to process. CGM allows 
		 *	multible metafiles to reside in a single file. This 
		 *	driver will only process the first.
		 */
		if (init_metafile(NEXT, cgm_fd) < 1) {
			meta_files++;
			continue;	/* skip to next metafile	*/
		}

	
		if (record == NULL) {	/* process all records in file	*/
			/* 
			 *	process frames until the end of the file or an 
			 *	unrecoverable error occurs
			 */
			while (ctrans(NEXT) == OK) {
				if (opt.verbose) {
					fprintf(
						stderr, 
						"plotted %d frames\n", 
						frame_count++
					);
				}
				if (batch)
					if (sleep_time>0) sleep(sleep_time);
			}
		}
		else {
			/* 
			 * process single frames begining at record `record`
			 */
			i = 0;
			while (record[i] != -1) {
				(void) ctrans(record[i++]);
				if (opt.verbose) {
					fprintf(
						stderr, 
						"plotted %d frames\n", 
						frame_count++
					);
				}
				if (batch)
					if (sleep_time>0) sleep(sleep_time);
			}
		}


		(void) CGM_close(cgm_fd);

		meta_files++;
	}	/* while	*/

	/*
	 *	terminate ctrans
	 */
	(void) close_ctrans();

	exit(0);
}

usage(od, prog_name, msg)
	int	od;
	char	*prog_name;
	char	*msg;
{
	char	*usage = "-d device [-f font] [options] [device options]";

	if (msg) fprintf(stderr, "%s: %s\n", prog_name, msg);

	fprintf(stderr, "Usage: %s %s\n", prog_name, usage);
	PrintOptionHelp(od, stderr);

}
