/*
 *	$Id: ctrans.c,v 1.4 1991-01-09 11:09:19 clyne Exp $
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
/*LINTLIBRARY*/
/*
 *	Ctrans.c
 *
 *
 *	Modified:	John Clyne	(clyne@redcloud.ucar.edu)
 *
 *	Date:		Mon Nov 28 13:54:09 MST 1988
 *
 *
 *		Ctrans.c no longer contains the ctrans driver. Ctrans
 *	is now callable as a function after proper intializtion. When
 *	invoked ctrans processes one frame in the metafile
 *
 *
 *	Author:	Tinsley Galyean (tag@boulder.colorado.edu)
 *
 *	Date:	Thu Mar 10 15:15:44 MST 1988
 *	
 * 	This file contains the main routine doing the argument parsing
 *	and the Process command the uses the jump table and the
 *	current device to send the cgmc to the correct command.
 *
 * rev 1.01 clyne 4/18/90	: expanded application programmer interace
 */
#ifndef lint
static char *RCSid = "$Header: /home/brownrig/SVN/CVS/ncarg/ncarview/src/lib/libctrans/ctrans.c,v 1.4 1991-01-09 11:09:19 clyne Exp $";
#endif


#include <stdio.h> 

#ifdef	SYSV
#include 	<string.h>
#else
#include	<strings.h>
#endif


#include 	<ctype.h>
#include 	<signal.h>
#include 	<ncarv.h>
#include	<cterror.h>
#include 	"cgmc.h"
#include	<cgm_tools.h>
#include 	"defines.h"
#include 	"translate.h"
#include 	"devices.h"
#include 	"graphcap.h"




extern char *malloc();
extern char *getenv();
extern Ct_err GP_Init();
extern Ct_err Init_Font();
extern Ct_err InitInput();
extern Ct_err SetRecord();
extern Ct_err Instr_Dec();

extern	Ct_err	(*cmdtab[][MAXCLASS+1][MAXFUNCPERCLASS+1]) ();
extern	struct	device	devices[];

extern  char    *strrchr();

char	**Argv;
int	Argc;
boolean	stand_Alone;	/* if false then the driver provides a window	*/
boolean	Batch;		/* if true don't prompt for user interaction	*/ 

/*
 * storate for some global command line options. 
 */
static	boolean softfill = 0;
static	boolean debug = 0;
static	boolean bell_off = 0;

boolean *softFill = &softfill;
boolean *deBug = &debug;
boolean *bellOff = &bell_off;

CGMC	command;

FILE	*tty;

boolean	ctransIsInit = FALSE;	/* ctrans device independent initialization
				 * state 
				 */
boolean	deviceIsInit = FALSE;	/* device dependent initialization state
				 * (cgi, X11, graphcap, clear_text)
				 */

/* 
 *	The current device index -- the default is the Gcap routines.
 */
int	currdev = -1;



/*
 *	init_ctrans:
 * 	PUBLIC
 *
 *		initialize the ctrans translator
 *
 * on entry
 *	*argc		: the argument count to the main program
 *	**argv		: the arguments to the main program
 *	*prog_name	: name of main progrm (for error messages)
 *	*gcap		: path to the graphcap
 *	*fcap		: path to the fontcap
 *	stand_alone	: False => ctrans under control of interactive interface
 *	batch		: True => don't prompt for user interaction
 *
 * on exit
 *	*argc		: contains argc
 *	**argv		: points to argv
 *	stand_Alone	: stand_alone
 *	Batch		: batch
 *	
 *	return		== -1 => error else ok
 *	
 * Note:
 *	gcap may be specify a software library. Currently Sun's cgi and X11 
 *	are supported. If this the case the library name is obtained by
 *	parsing it of the end of the path. For example, Sun's cgi may
 *	be specified as /usr/local/lib/cgi even though there is no such
 *	file.
 */
Ct_err	init_ctrans(argc, argv, prog_name, gcap, fcap, stand_alone,				batch)
	int	*argc;
	char	**argv;

	char	*prog_name,
		*gcap,
		*fcap;
	boolean	stand_alone;
	boolean	batch;

{
	extern	void	sigint_handler();
	char	*minw;
 
	stand_Alone = stand_alone;
	Batch = batch;

	if (ctransIsInit) {
		close_ctrans();
	}


	if (!batch)
		(void)signal(SIGINT,sigint_handler);

	/*
	 * open tty for user interaction if not batch
	 */
	if (!Batch && !BATCH) {
		if ( (tty = fopen("/dev/tty", "r")) == (FILE *) NULL )
		{
			ct_error(T_FOE, "tty");
			return(DIE);
		}
	}


	/*
	 * set the output device
	 */
	if (SetDevice(gcap) < 0) {
		return(pre_err);
	}

	/*
	 * load in any device specific command line options in to the option
	 * table and parse them
	 */
	parseOptionTable(argc, argv, devices[currdev].opt);

	Argv = argv;
	Argc = *argc;

	Init_cgmc(&command);

	/*
	 * the following is a kludge to ensure a minimum linewidth
	 */
	if (minw = getenv ("MINWIDTH")) {
		SetMinLineWidthDefault((float) atoi(minw));
	}


	/*
	 * set the fontcap
	 */
	if (SetFont(fcap) < 0) {
		return(pre_err);
	}


	ctransIsInit = TRUE;
	return(OK);
}


/*
 *	init_metafile:
 *	PUBLIC
 *
 *		intialize a single metafile within the named metafile.
 *	CGM allows multiple metafiles to reside with in a single file.
 *	This routine must be called prior to processing each single
 *	metafile. In most instances only one metafile will be contained
 *	in a metafile and this routine need only be invoked once
 *
 * on entry
 *	record		: the number of the record containing the first
 *	cgm_fd		: CGM file descriptor returned from CGM_open
 *			  element of the metafile. 0 if first metafile
 * on exit
 *	return		== one of {OK, DIE, SICK}
 */
Ct_err	init_metafile(record, cgm_fd)
	int	record;
	Cgm_fd	cgm_fd;
{
	/*
	 * 	make sure we've been initialized
	 */
	if (!ctransIsInit) {
		ct_error(T_NULL, "not in proper state");
		return(DIE);
	}

	/*
	 * initialize the input module
	 */
	if (InitInput(cgm_fd) != OK) {
		return(pre_err);
	}
	/*
	 *	Jump to the first frame in the metafile
	 *	in the metafile begining at record 'record' 
 	 */
	if (record != NEXT) {
		if (SetRecord(record) != OK) {
			return (pre_err);
		}
	}

	/*
	 *	make sure first element is a BEGIN METAFILE
	 */
	if (Instr_Dec(&command) != OK) {
		return(pre_err);
	}
	if (command.class == DEL_ELEMENT && command.command == BEG_MF) 
		Process(&command);
	else {
		ct_error(T_FRE, "missing CGM BEGIN METAFILE element");
		return(DIE);
	}

	/*
	 * 	process until the first frame or an end of metafile element	
	 */
	if (Instr_Dec(&command) != OK) {
		return (pre_err);
	}
	while((command.class != DEL_ELEMENT || command.command != BEG_PIC)
		&& (command.class != DEL_ELEMENT || command.command != END_MF)){

		Process(&command);

		if (Instr_Dec(&command) != OK) {
			return (pre_err);
		}
	}	 

	/*
	 * 'command' now contains a BEG_PIC or an END_METAFILE element 
	 */
	return (OK);

}


/*
 *	ctrans:
 *	PUBLIC
 *
 *		processes a frame in a metafile. 'command' should contain
 *	a CGM BEG_PIC or END_MF element
 *
 * on entry
 *	record		: == NEXT => process next frame in metafile
 *			  else process frame begining at record `record`
 * on exit
 *	return		: one of [ERROR, OK, EOM]
 */

Ct_err	ctrans(record)
	int	record;
{
	/*
	 * 	make sure we've been initialized
	 */
	if (!ctransIsInit) {
		ct_error(T_NULL, "not in proper state");
		return(DIE);
	}

	if (record != NEXT ) {
		if (SetRecord(record) != OK) {
			return(pre_err);
		}
		if (Instr_Dec(&command) != OK) {
			return(pre_err);
		}
	}

	/*
	 * 	Do until get a END PICTURE or END METAFILE element
	 */
	while (((command.class != DEL_ELEMENT) 
			|| (command.command != END_PIC))
		&& ((command.class != DEL_ELEMENT) 
			|| (command.command != END_MF))) {

		Process(&command);

		if (Instr_Dec(&command) != OK) {
			return(pre_err);
		}
	}

	/*
	 * process END_PICTURE or END_METAFILE
	 */
	Process(&command);

	if (command.command == END_MF)

		/* end of metafile		*/
		return(EOM);
	else {
		/* 
		 * end of a single frame. Get BEG_PIC command for next 
		 * invocation of  ctrans()
		 */
		if (Instr_Dec(&command) != OK) {
			return(pre_err);
		}
		return(OK);
	}

}

/*
 *	SetDevice()
 *	PUBLIC
 *
 *	Set the output device to specified device. The output device is
 *	specified on the call to init_ctrans. This functions allows
 *	the device to be changed. Note: if the device is changed in mid
 *	file than init_meta will need to be called to ensure that the
 *	output device receives the appropriate initialization.
 * on entry
 *	*gcap		: name of output device
 * on exit
 *	return		: < 0 => error
 */
SetDevice(gcap)
	char	*gcap;
{
	char	*device;
	int	i;

	/*
	 *	find out the name of the device (remove the path)
	 */
	device = (device = strrchr(gcap, '/')) ? ++device : gcap;


	/*
	 * Find what number in the device array the device is
	 */
	for(i=0;(i<devicenum) && (strcmp(device,devices[i].name) != 0);i++);
	if (i<devicenum)
		currdev = devices[i].number;
	else
		currdev = 0;

	/*
	 *	This is a hack for Sun's cgi which seems to capture
	 *	system signals. Thus making it impossible for ctrans
	 *	to perform proper termination on interrupt. So, we
	 *	close the error module and then open it again using
	 *	stderr for direct error messages instead of a file
	 */
		if (devices[currdev].number == CGI_I ||
			devices[currdev].number == X11_I) {

			close_ct_error();
			init_ct_error("ctrans", FALSE);
		}



	/*
	 *	Init the graph cap if one is used.
	 */
	if (devices[currdev].usegcap) {


		/*
		 *	init the graphcap stuff
		 */
		if (GP_Init(gcap) != OK) {
			return(-1);
		}

	}

	return(1);

}

/*
 *	SetFont()
 *	PUBLIC
 *
 *	Set the font device to fontcap. The font is
 *	specified on the call to init_ctrans. This functions allows
 *	the font to be changed.
 * on entry
 *	*fcap		: name of font
 * on exit
 *	return		: < 0 => error
 */
SetFont(fcap)
	char	*fcap;
{
	/*
	 *	Init the font cap if on is used.
	 */
	if( devices[currdev].usefcap ) {

		/*
		 *	Init the font Cap stuff
		 */
		if(Init_Font(fcap) != OK) {
			return (-1);
		}

	/*	init the fontlist	*/
	InitFontList();
	}
	return (1);
}

/*
 *	Process
 *	PRIVATE
 *
 *	This function takes a cgmc and then calls the correct
 *	function using the class and id along with the current device.
 */
Process(c)
CGMC	*c;
{
#ifdef DEBUG
		(void)fprintf(stdout,"class = %d, id = %d\n",	
					c->class,c->command);
#else

		if ((c->class > MAXCLASS) || (c->command > MAXFUNCPERCLASS))
			ct_error(NT_IOUE,""); /* Illegal or Unsupported cgm  */

		else if (cmdtab[currdev][c->class][c->command])
			(void)(*cmdtab[currdev][c->class][c->command])(c);
		else
			ct_error(NT_IOUE,""); /* Illegal or Unsupported cgm  */

#endif
}


/*
 *	The number of each type in the cgmc to allocate space for.
 */
#define NUMTOALLOC	32

/*
 *	Init_cgmc:
 *	PRIVATE
 *
 *	Inits the cgmc by allocating it space for each type.
 *	This is not done staticly at compile time because
 *	there is the need to dynamicaly change it.
 */
Init_cgmc (cgmc)
CGMC *cgmc;
{
	int	i;

	cgmc->ci = (CItype *) malloc (NUMTOALLOC * sizeof(CItype));  	
	cgmc->CIspace = NUMTOALLOC;
	cgmc->CInum = 0;

	cgmc->cd = (CDtype *) malloc (NUMTOALLOC * sizeof(CDtype));  	
	cgmc->CDspace = NUMTOALLOC;
	cgmc->CDnum = 0;

	cgmc->ix = (IXtype *) malloc (NUMTOALLOC * sizeof(IXtype));  	
	cgmc->IXnum = 0;
	cgmc->IXspace = NUMTOALLOC;

	cgmc->e  = (Etype *) 	malloc (NUMTOALLOC * sizeof(Etype));    	
	cgmc->Espace = NUMTOALLOC;
	cgmc->Enum = 0;

	cgmc->i  = (Itype *) 	malloc (NUMTOALLOC * sizeof(Itype));    	
	cgmc->Ispace = NUMTOALLOC;
	cgmc->Inum = 0;

	cgmc->r  = (Rtype *) 	malloc (NUMTOALLOC * sizeof(Rtype));    	
	cgmc->Rspace = NUMTOALLOC;
	cgmc->Rnum = 0;

	cgmc->s  = (Stype *) 	malloc (sizeof(Stype));    	
	cgmc->Sspace = NUMTOALLOC;
	cgmc->Snum = 0;
		cgmc->s->string = (char **)
			malloc(cgmc->Sspace * sizeof(char *));

		cgmc->s->string_space = (int *)
			malloc(cgmc->Sspace * sizeof(int));

		cgmc->s->string_space[0] = 256;
		cgmc->s->string[0] = (char *) malloc 
			((unsigned) (cgmc->s->string_space[0] * sizeof(char)));

		for (i = 1; i < cgmc->Sspace; i++) {
			cgmc->s->string_space[i] = 0;
			cgmc->s->string[i] = NULL;
		}

	cgmc->vdc= (VDCtype *)malloc (NUMTOALLOC * sizeof(VDCtype));	
	cgmc->VDCspace = NUMTOALLOC;
	cgmc->VDCnum = 0;

	cgmc->p  = (Ptype *) 	malloc (NUMTOALLOC * sizeof(Ptype));    	
	cgmc->Pspace = NUMTOALLOC;
	cgmc->Pnum = 0;

	cgmc->c  = (Ctype *) 	malloc (NUMTOALLOC * sizeof(Ctype));    	
	cgmc->Cspace = NUMTOALLOC;
	cgmc->Cnum = 0;

	cgmc->d  = (Ctype *) 	malloc (NUMTOALLOC * sizeof(Dtype));    	
	cgmc->Dspace = NUMTOALLOC;
	cgmc->Dnum = 0;

	cgmc->more = FALSE;
}


/*
 *	GraphicsMode
 *	PUBLIC
 *
 *	toggle graphics/text mode on the output device. This command only
 *	has effect for graphcap devices. 
 * on entry
 *	on 		: if set put device in graphics mode, else put device
 *			  in text mode
 */
GraphicsMode(on)
	boolean	on;
{

	/*
	 * do nothing if init_ctrans has not been called or if not a
	 * graphcap device
	 */
	if (! ctransIsInit || ! devices[currdev].usegcap)
		return;

	if (on)	{	/* put device in graphics mode	*/
		(void) buffer(GRAPHIC_INIT, GRAPHIC_INIT_SIZE);
		(void) buffer(ERASE, ERASE_SIZE);
	}
	else {	/* put device in text mode	*/
#ifdef	DEAD
		(void) buffer(GRAPHIC_INIT, GRAPHIC_INIT_SIZE);
		(void) buffer(ERASE, ERASE_SIZE);
#endif
		(void) buffer(TEXT_INIT, TEXT_INIT_SIZE);
	}

	(void) flush();	/* send instruction to device	*/

}

/*
 *	close_ctrans:
 *	PUBLIC
 *
 *		terminate ctrans
 *
 */
close_ctrans()
{

	if (!ctransIsInit) {
		return;
	}


	if (!Batch && !BATCH) {
		(void) fclose (tty);
	}

	/*
	 * invoke driver specific termination routine. If it has not
	 * already been invoked and we are not in debug mode.
	 */
	if (deviceIsInit && ! (*deBug)) {
		(void)(*cmdtab[currdev][DEL_ELEMENT][END_MF])(&command);
	}

	/*
	 *	flush the output buffer
	 */
	if (devices[currdev].usegcap)
		flush();

	/*
	 *	close the error module
	 */
	close_ct_error();

	/*
	 * free the cgmc
	 */
	cfree((char *) command.c);
	cfree((char *) command.ci);
	cfree((char *) command.cd);
	cfree((char *) command.e);
	cfree((char *) command.i);
	cfree((char *) command.ix);
	cfree((char *) command.p);
	cfree((char *) command.r);
			cfree((char *) command.s->string[0]);
		cfree((char *) command.s->string);
		cfree((char *) command.s->string_space);
	cfree((char *) command.s);
	cfree((char *) command.vdc);

	ctransIsInit = FALSE;
}

/*
 *	sigint_handler
 *	PRIVATE
 *
 *		interupt signal handler
 */
void	sigint_handler()
{
	close_ctrans();
	exit(0);
}
