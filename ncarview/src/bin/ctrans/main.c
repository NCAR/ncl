/*
 *	$Id: main.c,v 1.13 1992-02-13 18:14:53 clyne Exp $
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
	StringType_	device;		/* the device name		*/
	StringType_	font;		/* the font name		*/
	IntType_	movie;		/* movie or batch mode		*/
	BoolType_       soft_fill;	/* software fill of polygons	*/
	BoolType_       debug;		/* software fill of polygons	*/
	BoolType_       bell_off;	/* turn off the bell		*/
	BoolType_       read_stdin;	/* read metafile from stdin	*/
	FloatType_ 	min_line_width;	/* minimum line width		*/
	FloatType_ 	max_line_width;	/* maximun line width		*/
	FloatType_ 	line_scale;	/* additional line scaling	*/
	StringType_	pal;		/* optional color palette	*/
	BoolType_       version;	/* print the verison number	*/
	} commLineOpt;
	

static	OptDescRec	set_options[] = {
	{"device", OptSepArg, NULL},	
	{"font", OptSepArg, NULL},	
	{"movie", OptSepArg, "-1"},	
        {"softfill", OptIsArg, "false"},
        {"Debug", OptIsArg, "false"},
        {"bell", OptIsArg, "true"},
        {"", OptIsArg, "false"},
	{"lmin", OptSepArg, "-1"},	
	{"lmax", OptSepArg, "-1"},	
	{"lscale", OptSepArg, "-1"},	
	{"pal", OptSepArg, NULL},	
        {"Version", OptIsArg, "false"},
	{NULL}
	};

static	Option	get_options[] = {
	{"device", StringType, (unsigned long) &(commLineOpt.device), 
							sizeof(StringType_)},	
	{"font", StringType, (unsigned long) &commLineOpt.font, 
							sizeof(StringType_)},	
	{"movie", IntType, (unsigned long) &commLineOpt.movie, 
							sizeof(IntType_ ) },	
        {"softfill", BoolType, (unsigned long) &commLineOpt.soft_fill, 
							sizeof (BoolType_ )},
        {"Debug", BoolType, (unsigned long) &commLineOpt.debug, 
							sizeof (BoolType_ )},
        {"bell", BoolType, (unsigned long) &commLineOpt.bell_off, 
							sizeof (BoolType_ )},
        {"", BoolType, (unsigned long) &commLineOpt.read_stdin, 
							sizeof (BoolType_ )},
        {"lmin", FloatType, (unsigned long) &commLineOpt.min_line_width, 
							sizeof (FloatType_ )},
        {"lmax", FloatType, (unsigned long) &commLineOpt.max_line_width, 
							sizeof (FloatType_ )},
        {"lscale", FloatType, (unsigned long) &commLineOpt.line_scale, 
							sizeof (FloatType_ )},
	{"pal", StringType, (unsigned long) &(commLineOpt.pal), 
							sizeof(StringType_)},	
        {"Version", BoolType, (unsigned long) &commLineOpt.version, 
							sizeof (BoolType_ )},
	{NULL}
	};

extern	boolean *softFill;
extern	boolean *deBug;
extern	boolean *bellOff;
	
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

	extern	void	SetDefaultPalette();

	/* put the program name in a global variable */

	program_name = (program_name = strrchr(argv[0], '/')) ?
						++program_name : *argv;
	/*
	 * 	init ctrans' error module so we can use it
	 */
	init_ct_error(program_name, TRUE);

	/*
	 *	parse command line argument. Separate ctrans specific
	 *	args from device specific args
	 */
	softFill = &commLineOpt.soft_fill;
	deBug = &commLineOpt.debug;
	bellOff = &commLineOpt.bell_off;
	parseOptionTable(&argc, argv, set_options);

	/*
	 * load the options into commLineOpt
	 */
	getOptions((caddr_t) 0, get_options);

	batch = commLineOpt.movie != -1;
	sleep_time = commLineOpt.movie;

	if (commLineOpt.version) {
		PrintVersion(program_name);
		exit(0);
	}

	/*
	 * set line scaling options
	 */
	if (commLineOpt.min_line_width > -1) 
		SetMinLineWidthDefault(commLineOpt.min_line_width);
	if (commLineOpt.max_line_width > -1) 
		SetMaxLineWidthDefault(commLineOpt.max_line_width);
	if (commLineOpt.line_scale > -1) 
		SetAdditionalLineScale(commLineOpt.line_scale);

        /*
	 *	If a device was given on command line build the full path
	 *	to it. Else try and get device from the GRAPHCAP variable
	 *	Note: if the device is a software library interface and
	 *	not a graphcap then a path will still be built to it but
	 *	it will have no meaning.
         */
        if ((gcap = getGcapname( commLineOpt.device )) == NULL )
		ct_error(T_MD,"");


        /*
	 *	if a font was given on command line build the full path
	 *	to it. Else try and get font from the FONTCAP variable
	 *	Else try and use the default font
         */
        if ((fcap = getFcapname( commLineOpt.font )) == NULL) {

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
	if (commLineOpt.pal) {
		SetDefaultPalette(commLineOpt.pal);
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
				if (batch)
					sleep(sleep_time);
			}
		}
		else {
			/* 
			 * process single frames begining at record `record`
			 */
			i = 0;
			while (record[i] != -1) {
				(void) ctrans(record[i++]);
				if (batch)
					sleep(sleep_time);
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
