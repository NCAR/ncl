/*
 *	$Id: ctrans.c,v 1.9 1991-09-26 16:29:25 clyne Exp $
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
static char *RCSid = "$Header: /home/brownrig/SVN/CVS/ncarg/ncarview/src/lib/libctrans/ctrans.c,v 1.9 1991-09-26 16:29:25 clyne Exp $";
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
#include	<cgm_tools.h>
#include 	"cgmc.h"
#include 	"defines.h"
#include 	"translate.h"
#include 	"devices.h"
#include 	"ctrandef.h"
#include 	"graphcap.h"




extern char *getenv();
extern Ct_err GP_Init();
extern Ct_err Init_Font();
extern Ct_err InitInput();
extern Ct_err SetRecord();

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
static	char	*palFname = NULL;

boolean *softFill = &softfill;
boolean *deBug = &debug;
boolean *bellOff = &bell_off;

FILE	*tty = NULL;

/*
 * device dependent initialization state  (cgi, X11, graphcap, clear_text)
 */
boolean	deviceIsInit = FALSE;	


/* 
 * ctrans device independent initialization state 
 */
static	boolean	ctransIsInit = FALSE;	
static	CGMC	command;


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
/*ARGSUSED*/
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


	if (!Batch)
		(void)signal(SIGINT,sigint_handler);

	/*
	 * open tty for user interaction if not batch
	 */
	if (!Batch && !BATCH) {
		tty = fopen("/dev/tty", "r");
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

	init_cgmc(&command);

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
 *	return		: -1 => error, 0 => EOF, 1 => OK
 */
int	init_metafile(record, cgm_fd)
	int	record;
	Cgm_fd	cgm_fd;
{

	int	status;
	int	devnum = devices[currdev].number;	

	Ct_err	noop();

	/*
	 * 	make sure we've been initialized
	 */
	if (!ctransIsInit) {
		ct_error(T_NULL, "not in proper state");
		return(-1);
	}

	/*
	 * initialize the input module
	 */
	if (InitInput(cgm_fd) != OK) {
		return(-1);
	}
	/*
	 *	Jump to the first frame in the metafile
	 *	in the metafile begining at record 'record' 
 	 */
	if (record != NEXT) {
		if (SetRecord(record) != OK) {
			return (-1);
		}
	}

	/*
	 *	make sure first element is a BEGIN METAFILE
	 */
	if ((status = Instr_Dec(&command)) < 1) {
		if (status < 0) {	/* else eof	*/
			ct_error(T_FRE, "metafile");
		}
		return(status);
	}
	if (command.class == DEL_ELEMENT && command.command == BEG_MF) 
		Process(&command);
	else {
		ct_error(T_FRE, "missing CGM BEGIN METAFILE element");
		return(-1);
	}

	/*
	 * load the default color palette now if there is one. We need to
	 * do this after the BEGIN METAFILE is processed and before the
	 * first BEGIN PICTURE
	 */
	if (palFname) {
		if (LoadPalette(&command, palFname)) {
			Process(&command);	/* command contains pal	*/
			/*
			 * disable future CGM color table entries
			 */
			cmdtab[devnum][ATT_ELEMENT][COLOR_TABLE] = noop;
			
		}
	}

	/*
	 * 	process until the first frame or an end of metafile element	
	 */
	if (Instr_Dec(&command) < 1) {
		ct_error(T_FRE, "metafile");
		return (-1);
	}
	while((command.class != DEL_ELEMENT || command.command != BEG_PIC)
		&& (command.class != DEL_ELEMENT || command.command != END_MF)){

		Process(&command);

		if (Instr_Dec(&command) < 1) {
			ct_error(T_FRE, "metafile");
			return (-1);
		}
	}	 

	/*
	 * 'command' now contains a BEG_PIC or an END_METAFILE element 
	 */
	return (1);

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

	Ct_err	status;

	/*
	 * 	make sure we've been initialized
	 */
	if (!ctransIsInit) {
		ct_error(T_NULL, "not in proper state");
		return(DIE);
	}

	/*
	 * do we need to do random frame access
	 */
	if (record != NEXT ) {
		if (SetRecord(record) != OK) {
			return(pre_err);
		}
		if (Instr_Dec(&command) < 1) {
			ct_error(T_FRE, "metafile");
			return(DIE);
		}
	}

	/*
	 * see if we've reached the end of the file
	 */
	if (command.class == DEL_ELEMENT && command.command == END_MF) {
		Process(&command);	/* process the END MF command	*/
		return(EOM);
	}

	/*
	 * current element better be a begin picture
	 */
	if (! (command.class == DEL_ELEMENT && command.command == BEG_PIC)) {
		ct_error(T_NULL, "not in proper state");
		return(DIE);
	}
	Process(&command); /* process the begin picture element    */


	/*
	 * if we're in batch mode we clear the device *before*  processing
	 * the frame. We need to do this now because we can't do it at 
	 * the end of this call because when we're in batch mode it is
	 * expected that the calling program will do any user interaction
	 * instead of having it handled by the devices EndPic element
	 * handler. 
	 */
	if (Batch) clear_device();


	/*
	 * process elements until we get an end of picture 
	 */
	while ((status = Instr_Dec(&command)) > 0) {

		/*
		 * execute the cgmc
		 */
		Process(&command);

		if (command.class == DEL_ELEMENT && command.command == END_PIC){

			break;	/* we're done	*/
		}
	}

	if (status < 1) {
		ct_error(T_FRE, "metafile");
		return(DIE);
	}


	/*
	 * get the next instruction. It should be either a Begin Pic or 
	 * an End MF
	 */
	if (Instr_Dec(&command) < 1) {
		ct_error(T_FRE, "metafile");
		return(DIE);
	}

	if (! Batch) clear_device();

	return(OK);
}


/*
 *	ctrans_merge:
 *	PUBLIC
 *
 *	process one frame on top of anonther. Like ctrans() except second
 *	frame is drawn over first.  
 *
 * on entry
 *	record1 	: first frame to process beginning at record 'record'. 
 *	record1 	: second frame to process beginning at record 'record'. 
 * on exit
 *	return		: one of [ERROR, OK, EOM]
 */

Ct_err	ctrans_merge(record1, record2)
	int	record1, record2;
{
	/*
	 * 	make sure we've been initialized
	 */
	if (!ctransIsInit) {
		ct_error(T_NULL, "not in proper state");
		return(DIE);
	}

	/*
	 * advance to first record
	 */
	if (SetRecord(record1) != OK) {
		return(pre_err);
	}

	/*
	 * 	Do until get a END PICTURE or END METAFILE element
	 */
	do {
		if (Instr_Dec(&command) < 1) {
			ct_error(T_FRE, "metafile");
			return(DIE);
		}

		Process(&command);

	} while (((command.class != DEL_ELEMENT) 
			|| (command.command != END_PIC))
		&& ((command.class != DEL_ELEMENT) 
			|| (command.command != END_MF)));

	/*
	 * advance to second record (skip the END_PIC command)
	 */
	if (SetRecord(record2) != OK) {
		return(pre_err);
	}

	/*
	 * for the second frame we skip over everthing between the 
	 * begin-pic and begin-pic-body
	 */
	do {

		if (Instr_Dec(&command) < 1) {
			ct_error(T_FRE, "metafile");
			return (DIE);
		}
	}while((command.class != DEL_ELEMENT || command.command != BEG_PIC_B)
		&& (command.class != DEL_ELEMENT || command.command != END_MF));


	/*
	 * get the next command
	 */
	if (Instr_Dec(&command) < 1) {
		ct_error(T_FRE, "metafile");
		return (DIE);
	}

	/*
	 * 	Do until get a END PICTURE or END METAFILE element
	 */
	while (((command.class != DEL_ELEMENT) 
			|| (command.command != END_PIC))
		&& ((command.class != DEL_ELEMENT) 
			|| (command.command != END_MF))) {

		Process(&command);

		if (Instr_Dec(&command) < 1) {
			ct_error(T_FRE, "metafile");
			return(DIE);
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
		if (Instr_Dec(&command) < 1) {
			ct_error(T_FRE, "metafile");
			return(DIE);
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
		currdev = i;
	else
		currdev = 0;


	/*
	 *	This is a hack for Sun's cgi which seems to capture
	 *	system signals. Thus making it impossible for ctrans
	 *	to perform proper termination on interrupt. So, we
	 *	close the error module and then open it again using
	 *	stderr for direct error messages instead of a file
	 */
		if ( devices[currdev].number == X11_I) {

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

	/*
	 * load device dependent portion of common routines
	 */
	if (devices[currdev].use_common) {
		ComSetDevice(devices[currdev].name);
	}

	return(1);

}

/*
 *	SetFont()
 *	PUBLIC
 *
 *	Set the font device to fontcap. The font is
 *	specified on the call to init_ctrans. This functions allows
 *	the font to be changed. This function may only be called after
 *	SetDevice() or after init_ctrans() has been called.
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
 *	SetDefaultPalette
 *	PUBLIC
 *
 *	Inform ctrans to use a default color palette to overide colormap
 *	entries supplied by the CGM being translated. This routine must
 *	be called *before* init_metafile() or it will have no effect
 *
 * on entry
 *	*pal_fname	: name of file containing palette
 */
void	SetDefaultPalette(pal_fname) 
	char	*pal_fname;
{
	palFname = pal_fname;
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
	int	devnum = devices[currdev].number;	
#ifdef DEBUG
	(void)fprintf(stdout,"class = %d, id = %d\n",	c->class,c->command);
#else

		if ((c->class > MAXCLASS) || (c->command > MAXFUNCPERCLASS))
			ct_error(NT_IOUE,""); /* Illegal or Unsupported cgm  */

		else if (cmdtab[devnum][c->class][c->command])
			(void)(*cmdtab[devnum][c->class][c->command])(c);
		else
			ct_error(NT_IOUE,""); /* Illegal or Unsupported cgm  */

#endif
}


/*
 *	The number of each type in the cgmc to allocate space for.
 */
#define NUMTOALLOC	32

/*
 *	init_cgmc:
 *	PRIVATE
 *
 *	Inits the cgmc by allocating it space for each type.
 *	This is not done staticly at compile time because
 *	there is the need to dynamicaly change it.
 */
init_cgmc (cgmc)
CGMC *cgmc;
{
	int	i;

	cgmc->ci = (CItype *) icMalloc((unsigned)(NUMTOALLOC * sizeof(CItype)));
	cgmc->CIspace = NUMTOALLOC;
	cgmc->CInum = 0;

	cgmc->cd = (CDtype *) icMalloc((unsigned)(NUMTOALLOC * sizeof(CDtype)));
	cgmc->CDspace = NUMTOALLOC;
	cgmc->CDnum = 0;

	cgmc->ix = (IXtype *) icMalloc((unsigned)(NUMTOALLOC * sizeof(IXtype)));
	cgmc->IXnum = 0;
	cgmc->IXspace = NUMTOALLOC;

	cgmc->e  = (Etype *) icMalloc((unsigned)(NUMTOALLOC * sizeof(Etype)));
	cgmc->Espace = NUMTOALLOC;
	cgmc->Enum = 0;

	cgmc->i  = (Itype *) icMalloc ((unsigned) (NUMTOALLOC * sizeof(Itype)));
	cgmc->Ispace = NUMTOALLOC;
	cgmc->Inum = 0;

	cgmc->r  = (Rtype *) icMalloc ((unsigned) (NUMTOALLOC * sizeof(Rtype)));
	cgmc->Rspace = NUMTOALLOC;
	cgmc->Rnum = 0;

	cgmc->s  = (Stype *) icMalloc (sizeof(Stype));
	cgmc->Sspace = NUMTOALLOC;
	cgmc->Snum = 0;
		cgmc->s->string = (char **)
			icMalloc((unsigned) (cgmc->Sspace * sizeof(char *)));

		cgmc->s->string_space = (int *)
			icMalloc((unsigned) (cgmc->Sspace * sizeof(int)));

		cgmc->s->string_space[0] = 256;
		cgmc->s->string[0] = (char *) icMalloc 
			((unsigned) (cgmc->s->string_space[0] * sizeof(char)));

		for (i = 1; i < cgmc->Sspace; i++) {
			cgmc->s->string_space[i] = 0;
			cgmc->s->string[i] = NULL;
		}

	cgmc->vdc=(VDCtype *)icMalloc((unsigned)(NUMTOALLOC * sizeof(VDCtype)));
	cgmc->VDCspace = NUMTOALLOC;
	cgmc->VDCnum = 0;

	cgmc->p  = (Ptype *) icMalloc ((unsigned) (NUMTOALLOC * sizeof(Ptype)));
	cgmc->Pspace = NUMTOALLOC;
	cgmc->Pnum = 0;

	cgmc->c  = (Ctype *) icMalloc ((unsigned) (NUMTOALLOC * sizeof(Ctype)));
	cgmc->Cspace = NUMTOALLOC;
	cgmc->Cnum = 0;

	cgmc->d  = (Ctype *) icMalloc ((unsigned) (NUMTOALLOC * sizeof(Dtype)));
	cgmc->Dspace = NUMTOALLOC;
	cgmc->Dnum = 0;

	cgmc->more = FALSE;
}

free_cgmc(cgmc)
	CGMC	*cgmc;
{
	/*
	 * free the cgmc
	 */
	cfree((char *) cgmc->c);
	cfree((char *) cgmc->ci);
	cfree((char *) cgmc->cd);
	cfree((char *) cgmc->e);
	cfree((char *) cgmc->i);
	cfree((char *) cgmc->ix);
	cfree((char *) cgmc->p);
	cfree((char *) cgmc->r);
	cfree((char *) cgmc->s->string[0]);
	cfree((char *) cgmc->s->string);
	cfree((char *) cgmc->s->string_space);
	cfree((char *) cgmc->s);
	cfree((char *) cgmc->vdc);
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
		(void) buffer(ERASE, ERASE_SIZE);
		(void) buffer(TEXT_INIT, TEXT_INIT_SIZE);
		deviceIsInit = FALSE;
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

	int	devnum = devices[currdev].number;	

	if (!ctransIsInit) {
		return;
	}


	if (!Batch && !BATCH) {
		if (tty) (void) fclose (tty);
	}

	if (devices[currdev].use_common) ComClose();

	/*
	 * invoke driver specific termination routine. If it has not
	 * already been invoked and we are not in debug mode.
	 */
	if (deviceIsInit && ! (*deBug)) {
		(void)(*cmdtab[devnum][DEL_ELEMENT][END_MF])(&command);
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

	free_cgmc(&command);

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

static	clear_device()
{
	CGMC	temp_cgmc;

	temp_cgmc.class = DEL_ELEMENT;
	temp_cgmc.command = CLEAR_DEVICE;
	Process(&temp_cgmc);
}
