/*
 *	$Id: ictrans.c,v 1.9 1992-02-13 18:27:07 clyne Exp $
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
#include <sys/types.h>
#ifndef	CRAY
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#include <cgm_tools.h>
#include <ncarv.h>
#include <stdio.h>
#include <cterror.h>
#include "ictrans.h"
#include "icmalloc.h"
#include "lex.h"
#include "get_cmd.h"

extern	char	yytext[];
extern	char	*getFcapname();
extern	char	*getGcapname();
extern	Ct_err	init_ctrans();
extern	Ct_err	init_metafile();
extern	Ct_err	ctrans();
extern	char	*strrchr();

extern	IcState	icState;
extern	int	spoolerJobs;

static	struct	{
	StringType_	device;		/* the device name		*/
	StringType_	font;		/* the font name		*/
	BoolType_	soft_fill;	/* software fill of piolygons	*/
	BoolType_	bell_off;	/* turn the bell off		*/
	FloatType_ 	min_line_width;	/* minimum line width		*/
	FloatType_ 	max_line_width;	/* maximun line width		*/
	FloatType_ 	line_scale;	/* additional line scaling	*/
	StringType_     pal;            /* optional color palette       */
	IntType_	fd;		/* output file descriptor	*/
	BoolType_	version;	/* software fill of piolygons	*/
	} commLineOpt;

static	OptDescRec	set_options[] = {
	{"device", OptSepArg, NULL},	
	{"font", OptSepArg, NULL},	
	{"softfill", OptIsArg, "false"},
	{"bell", OptIsArg, "true"},
	{"lmin", OptSepArg, "-1"},	
	{"lmax", OptSepArg, "-1"},	
	{"lscale", OptSepArg, "-1"},	
        {"pal", OptSepArg, NULL},
        {"fdn", OptSepArg, "-1"},
        {"Version", OptIsArg, "false"},
	{NULL}
	};

static	Option	get_options[] = {
	{"device", StringType, (unsigned long) &commLineOpt.device, 
						sizeof(StringType_)},	
	{"font", StringType, (unsigned long) &commLineOpt.font, 
						sizeof(StringType_)},	
	{"softfill", BoolType, (unsigned long) &commLineOpt.soft_fill, 
						sizeof (BoolType_ )},
	{"bell", BoolType, (unsigned long) &commLineOpt.bell_off, 
						sizeof (BoolType_ )},
        {"lmin", FloatType, (unsigned long) &commLineOpt.min_line_width, 
							sizeof (FloatType_ )},
        {"lmax", FloatType, (unsigned long) &commLineOpt.max_line_width, 
							sizeof (FloatType_ )},
        {"lscale", FloatType, (unsigned long) &commLineOpt.line_scale, 
							sizeof (FloatType_ )},
	{"pal", StringType, (unsigned long) &(commLineOpt.pal),
							sizeof(StringType_)},
	{"fdn", IntType, (unsigned long) &(commLineOpt.fd),
							sizeof(IntType_)},
	{"Version", BoolType, (unsigned long) &commLineOpt.version, 
						sizeof (BoolType_ )},

	{NULL}
	};

extern	boolean	*softFill;
extern	boolean	*bellOff;

char	*programName;


ICTrans(argc, argv, mem_cgm) 
	int	argc;
	char	**argv;
	char	*mem_cgm;
{


	ICommand	icommand;
	int		status;

	char	*fcap,			/* path to font			*/
		*gcap;			/* path to device		*/
	boolean	batch = FALSE;

					/*
					 * list of metafiles to process
					 */
	char	**meta_files = (char **) icMalloc ((argc * sizeof(char *)) + 1);
	int	i,j;

	void	ex_command();
	extern	char	*GetCurrentAlias();
	char	buf[80];

	/* put the program name in a global variable */

	programName = (programName = strrchr(argv[0], '/')) ?
						++programName : *argv;
	/*
	 * 	init ctrans' error module so we can use it
	 */
	init_ct_error(programName, TRUE);

	/*
	 *	parse command line argument. Separate ctrans specific
	 *	args from device specific args
	 */
	softFill = &commLineOpt.soft_fill;
	bellOff = &commLineOpt.bell_off;;
	parseOptionTable(&argc, argv, set_options);

	/*
	 * load the options into commLineOpt
	 */
	getOptions((caddr_t) 0, get_options);

	if (commLineOpt.version) {
		PrintVersion(programName);
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

	icState.device = (icState.device = strrchr(gcap, '/')) ?
						++icState.device : gcap;


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

	icState.font = (icState.font = strrchr(fcap, '/')) ?
						++icState.font : fcap;

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
	if (init_ctrans(&argc,argv,programName,gcap,fcap,TRUE,TRUE) != OK) {
		(void) close_ctrans();
		exit(1);
	}

#ifdef	DEAD
	(void) set_action_err(NO_ACT);
#endif

	/*
	 * assume everthing left in the command line is a metafile. check
	 * and make sure
	 */
	meta_files[0] = NULL;
	for (i = 1, j=0; i < argc; i++) {
		if (*argv[i] == '-') {
			if (! strcmp(argv[i], "-e")) {
				i++;
				if (i >= argc) ct_error(T_NSO, "");
				(void) AppendString(argv[i]);
				batch = TRUE;
			}
			else {
				ct_error(T_NSO, "");
				break;
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
		ct_error(T_NULL, "Metafile not specified");
		exit(1);
	}

	icState.file = meta_files;	/* record the file name	*/

	/*
	 * initialize the print spooler stuff
	 */
	InitSpool();
	icState.spool_alias = GetCurrentAlias();

	init_icommand(&icommand);
	icommand.fd = commLineOpt.fd == -1 ? fileno(stdout) : commLineOpt.fd;

	/*
	 * prime the system by executing a file() command
	 */
	if (mem_cgm) {	/* CGM in memory?	*/
		processMemoryCGM(&icommand, mem_cgm);
	} 
	else {		/* CGM on disk		*/
		icommand.cmd.name = "file";
		icommand.cmd.data = meta_files[0];
		ex_command(&icommand);
	}

	/*
	 * if in batch mode don't prompt user for input
	 */
	if (batch) {
		icommand.cmd.name = "movie";
		icommand.cmd.data = NULL;
		ex_command(&icommand);
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
				(void) sprintf(buf, "Done	%d\n", status);
				(void) write(icommand.fd, buf, strlen(buf));
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
			ex_command(&icommand);
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
			exit(0);
			break;
		}
	}
}

void	ex_command(ic)
	ICommand	*ic;
{

	CmdOp	*c;
	CmdOp	*getcmdOp();


	/*
	 * find the command object in the command table
	 */
	c = getcmdOp(ic->cmd.name);

	if (c == (CmdOp *) -1) {
		(void) fprintf(stderr, "%s: Ambiguous command?\n",programName);
	}
	else if (c == (CmdOp *) NULL) {
		(void) fprintf(stderr, "%s: Invalid command?\n",programName);
	}
	else {

		/*
		 * execute the command
		 */
		ic->c = c;
		(*c->c_handler) (ic);
	}
}


