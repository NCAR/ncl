/*
 *	$Id: ctrans.c,v 1.24 1992-08-26 19:34:33 clyne Exp $
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

#include <stdio.h> 
#include <string.h>


#include <ctype.h>
#include <errno.h>
#include <ncarv.h>
#include <cgm_tools.h>
#include "ctrans.h"
#include "cgmc.h"
#include "defines.h"
#include "translate.h"
#include "devices.h"
#include "ctrandef.h"
#include "graphcap.h"




extern char *getenv();
extern int GP_Init();
extern int Init_Font();
extern int InitInput();
extern int SetRecord();

extern	int	(*cmdtab[][MAXCLASS+1][MAXFUNCPERCLASS+1]) ();
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
boolean *doBell = &bell_off;

FILE	*tty = NULL;

/*
 * device dependent initialization state  (cgi, X11, graphcap, clear_text)
 */
boolean	deviceIsInit = FALSE;	

/*
 * device dependent option Descriptor
 */
int	optionDesc;


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
 *	error reporting stuff
 */
static	char	eLog[32][256];
static	int	eIndex = 0;
#define	MAX_MSG	(sizeof(eLog) / sizeof(eLog[0]))


static	void	elog(msg)
	char	*msg;
{
	if (eIndex < MAX_MSG) {
		strncpy(eLog[eIndex],msg, sizeof(eLog[eIndex])-1);
		eIndex++;
	}
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

void	free_cgmc(cgmc)
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
 *	Process
 *	PRIVATE
 *
 *	This function takes a cgmc and then calls the correct
 *	function using the class and id along with the current device.
 */
CtransRC	Process(c)
	CGMC	*c;
{
	int	devnum = devices[currdev].number;	
	int	rc;

	if ((c->class > MAXCLASS) || (c->command > MAXFUNCPERCLASS)) {
		ESprintf(
			EINVAL, "Illegal metafile element(class=%d, id=%d)",
			c->class, c->command
		);
		elog(ErrGetMsg());
		return(WARN);
	}
	else if (cmdtab[devnum][c->class][c->command]) {
		rc = (*cmdtab[devnum][c->class][c->command])(c);
	}
	else {
		/*
		 * no function for element
		 */
		ESprintf(
			EINVAL, "Illegal metafile element(class=%d, id=%d)",
			c->class, c->command
		);
		elog(ErrGetMsg());
		return(WARN);
	}

	if (rc >= 0) {
		return(OK);
	}
	/*
	 * fatal if descriptor or delimiter element
	 */
	else if (c->class == DEL_ELEMENT || c->class == DES_ELEMENT) {
		ESprintf(
			E_UNKNOWN, 
			"Could not process CGM element(class=%d, id=%d) [ %s ]",
			c->class, c->command, ErrGetMsg()
		);
		elog(ErrGetMsg());
		return(FATAL);
	}
	/*
	 * non-fatal error (we hope)
	 */
	else {
		ESprintf(
			E_UNKNOWN, 
			"Could not process CGM element(class=%d, id=%d) [ %s ]",
			c->class, c->command, ErrGetMsg()
		);
		elog(ErrGetMsg());
		return(WARN);
	}
}

static	void	clear_device()
{
	CGMC	temp_cgmc;

	temp_cgmc.class = DEL_ELEMENT;
	temp_cgmc.command = CLEAR_DEVICE;
	(void) Process(&temp_cgmc);
}

/*
 *	if the cgmc contains a escape element or a noop process it and 
 *	fetch the next element into the cgmc and repeat
 */
DoEscapes(cgmc)
	CGMC	*cgmc;
{
	while ((cgmc->class == ESC_ELEMENT && cgmc->command == ESCAPE_ID) ||
		(cgmc->class == DEL_ELEMENT && cgmc->command == NOOP_ID)) {

		if (Process(cgmc) != FATAL) {
			(void) Instr_Dec(cgmc);
		}
	}
}

/**********************************************************************
**********************************************************************
**
**	C T R A N S   A P I
**
**********************************************************************
*********************************************************************/


/*
 *	init_ctrans:
 * 	PUBLIC
 *
 *		initialize the ctrans translator
 *
 * on entry
 *	*argc		: the argument count to the main program
 *	**argv		: the arguments to the main program
 *	*gcap		: path to the graphcap
 *	*fcap		: path to the fontcap, if NULL default font used
 *	stand_alone	: False => ctrans under control of interactive interface
 *	batch		: True => don't prompt for user interaction
 *
 * on exit
 *	*argc		: contains argc
 *	**argv		: points to argv
 *	stand_Alone	: stand_alone
 *	Batch		: batch
 *	
 *	return		== [FATAL=-2, WARN=-1, OK=1]
 *	
 * Note:
 *	gcap may be specify a software library. Currently Sun's cgi and X11 
 *	are supported. If this the case the library name is obtained by
 *	parsing it of the end of the path. For example, Sun's cgi may
 *	be specified as /usr/local/lib/cgi even though there is no such
 *	file.
 */
/*ARGSUSED*/
CtransRC	init_ctrans(argc, argv, gcap, fcap, stand_alone, batch)
	int	*argc;
	char	**argv;

	char	*gcap,
		*fcap;
	boolean	stand_alone;
	boolean	batch;

{
	char	*tty_in	= "/dev/tty";
	char	*minw;
 
	stand_Alone = stand_alone;
	Batch = batch;

	if (ctransIsInit) {
		close_ctrans();
	}


	/*
	 * open tty for user interaction if not batch
	 */
	if (!Batch && !BATCH) {
		if (! (tty = fopen(tty_in, "r"))) {
			ESprintf(errno,"fopen(%s, r)", tty_in);
			elog(ErrGetMsg());
			return(FATAL);
		}
	}


	/*
	 * set the output device
	 */
	if (SetDevice(gcap) < 0) {
		ESprintf(
			E_UNKNOWN, "Can't initialize device(%s) [ %s ]",
			gcap, ErrGetMsg()
		); 
		elog(ErrGetMsg());
		return(FATAL);
	}

	/*
	 * load in any device specific command line options in to the option
	 * table and parse them
	 */
	optionDesc = OpenOptionTbl();
	if (ParseOptionTable(optionDesc, argc, argv, devices[currdev].opt) < 0){
		ESprintf(
			E_UNKNOWN, "Can't process device options [ %s ]",
			ErrGetMsg()
		); 
		elog(ErrGetMsg());
		return(WARN);
	}	

	Argv = argv;
	Argc = *argc;


	init_cgmc(&command);

	/*
	 * the following is a kludge to ensure a minimum linewidth
	 */
	minw = getenv ("MINWIDTH");
	if (minw) {
		SetMinLineWidthDefault((float) atoi(minw));
	}


	/*
	 * set the fontcap
	 */
	if (fcap) {
		(void) SetFont(fcap);
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
 *	return		== [FATAL=-2, WARN=-1, OK=1]
 */
CtransRC	init_metafile(record, cgm_fd)
	int	record;
	Cgm_fd	cgm_fd;
{

	CtransRC	rc;
	CtransRC	rcx = OK;
	int	devnum = devices[currdev].number;	

	int	noop();

	/*
	 * 	make sure we've been initialized
	 */
	if (!ctransIsInit) {
		ESprintf(E_UNKNOWN, "Not in proper state\n");
		elog(ErrGetMsg());
		return(FATAL);
	}

	/*
	 * intialize the CGM default table
	 */
	InitDefault();	

	/*
	 * initialize the input module
	 */
	if (InitInput(cgm_fd) < 0) {
		elog(ErrGetMsg());
		return(FATAL);
	}
	/*
	 *	Jump to the first frame in the metafile
	 *	in the metafile begining at record 'record' 
 	 */
	if (record != NEXT) {
		if (SetRecord(record) < 0) {
			ESprintf(E_UNKNOWN,"Can't seek to record(%d)",record);
			elog(ErrGetMsg());
			return (WARN);
		}
	}

	/*
	 *	Make sure first elements is a BEGIN METAFILE
	 *	Instr_Dec returns 0 of EOF, -1 on error.
	 */
	if (Instr_Dec(&command) < 1) {
		elog(ErrGetMsg());
		return(FATAL);
	}
	DoEscapes(&command);
	if (command.class == DEL_ELEMENT && command.command == BEG_MF_ID) {
		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;
	}
	else {
		ESprintf(E_UNKNOWN,"Metafile missing BEGIN METAFILE element");
		elog(ErrGetMsg());
		return(FATAL);
	}

	/*
	 * load the default color palette now if there is one. We need to
	 * do this after the BEGIN METAFILE is processed and before the
	 * first BEGIN PICTURE
	 */
	if (palFname) {
		if (LoadPalette(&command, palFname)) {
			/*
			 * command contains pal	
			 */
			if ((rc = Process(&command)) == FATAL) return(FATAL);
			if (rc == WARN) rcx = WARN;
			/*
			 * disable future CGM color table entries
			 */
			cmdtab[devnum][ATT_ELEMENT][COLOR_TABLE_ID] = noop;
			
		}
	}

	/*
	 * 	process until the first frame or an end of metafile element	
	 */
	if (Instr_Dec(&command) < 1) {
		elog(ErrGetMsg());
		return (FATAL);
	}
	while((command.class != DEL_ELEMENT || command.command != BEG_PIC_ID) &&
		(command.class != DEL_ELEMENT || command.command != END_MF_ID)){

		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

		if (Instr_Dec(&command) < 1) {
			elog(ErrGetMsg());
			return (FATAL);
		}
	}	 

	/*
	 * 'command' now contains a BEG_PIC or an END_METAFILE element 
	 */
	return (rcx);
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
 *	return		== [FATAL=-2, WARN=-1, EOM=0, OK=1]
 */

CtransRC	ctrans(record)
	int	record;
{

	int	status = 0;
	CtransRC	rc;
	CtransRC	rcx = OK;

	/*
	 * 	make sure we've been initialized
	 */
	if (!ctransIsInit) {
		ESprintf(E_UNKNOWN, "Not in proper state\n");
		elog(ErrGetMsg());
		return(FATAL);
	}

	/*
	 * do we need to do random frame access
	 */
	if (record != NEXT ) {
		if (SetRecord(record) < 0) {
			ESprintf(E_UNKNOWN, "Can't seek to record(%d)",record);
			elog(ErrGetMsg());
			return (WARN);
		}
		if (Instr_Dec(&command) < 1) {
			elog(ErrGetMsg());
			return(FATAL);
		}
	}

	DoEscapes(&command);
	/*
	 * see if we've reached the end of the file
	 */
	if (command.class == DEL_ELEMENT && command.command == END_MF_ID) {
		/* 
		 * process the END MF command	
		 */
		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;
		return(EOM);
	}

	/*
	 * current element better be a begin picture
	 */
	if (! (command.class == DEL_ELEMENT && command.command == BEG_PIC_ID)) {
		ESprintf(E_UNKNOWN, "BEGIN PICTURE element expected");
		elog(ErrGetMsg());
		return(FATAL);
	}
	/* 
	 * process the begin picture element    
	 */
	if ((rc = Process(&command)) == FATAL) return(FATAL);
	if (rc == WARN) rcx = WARN;

	/*
	 * process elements until we get an end of picture 
	 */
	while ((status = Instr_Dec(&command)) > 0) {

		/*
		 * execute the cgmc
		 */
		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

		if (command.class == DEL_ELEMENT && command.command == END_PIC_ID){

			break;	/* we're done	*/
		}
	}

	if (status < 1) {
		elog(ErrGetMsg());
		return(FATAL);
	}


	/*
	 * get the next instruction. It should be either a Begin Pic or 
	 * an End MF or an escape
	 */
	if (Instr_Dec(&command) < 1) {
		elog(ErrGetMsg());
		return(FATAL);
	}

	if (! Batch) clear_device();

	return(rcx);
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
 *	return		== [FATAL=-2, WARN=-1, EOM=0, OK=1]
 */

CtransRC	ctrans_merge(record1, record2)
	int	record1, record2;
{
	CtransRC	rc;
	CtransRC	rcx;
	/*
	 * 	make sure we've been initialized
	 */
	if (!ctransIsInit) {
		ESprintf(E_UNKNOWN, "Not in proper state\n");
		elog(ErrGetMsg());
		return(FATAL);
	}

	/*
	 * advance to first record
	 */
	if (SetRecord(record1) < 0) {
		ESprintf(E_UNKNOWN,"Can't seek to record(%d)",record1);
		elog(ErrGetMsg());
		return (WARN);
	}

	/*
	 * 	Do until get a END PICTURE or END METAFILE element
	 */
	do {
		if (Instr_Dec(&command) < 1) {
			elog(ErrGetMsg());
			return(FATAL);
		}

		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

	} while (((command.class != DEL_ELEMENT) 
			|| (command.command != END_PIC_ID))
		&& ((command.class != DEL_ELEMENT) 
			|| (command.command != END_MF_ID)));

	/*
	 * advance to second record (skip the END_PIC command)
	 */
	if (SetRecord(record2) < 0) {
		ESprintf(E_UNKNOWN,"Can't seek to record(%d)",record1);
		elog(ErrGetMsg());
		return (WARN);
	}

	/*
	 * for the second frame we skip over everthing between the 
	 * begin-pic and begin-pic-body
	 */
	do {

		if (Instr_Dec(&command) < 1) {
			elog(ErrGetMsg());
			return (FATAL);
		}
	}while(
		(command.class != DEL_ELEMENT || 
		command.command != BEG_PIC_B_ID)
		&& (command.class != DEL_ELEMENT || 
		command.command != END_MF_ID));


	/*
	 * get the next command
	 */
	if (Instr_Dec(&command) < 1) {
		elog(ErrGetMsg());
		return (FATAL);
	}

	/*
	 * 	Do until get a END PICTURE or END METAFILE element
	 */
	while (((command.class != DEL_ELEMENT) 
			|| (command.command != END_PIC_ID))
		&& ((command.class != DEL_ELEMENT) 
			|| (command.command != END_MF_ID))) {

		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

		if (Instr_Dec(&command) < 1) {
			elog(ErrGetMsg());
			return(FATAL);
		}
	}

	/*
	 * process END_PICTURE or END_METAFILE
	 */
	if ((rc = Process(&command)) == FATAL) return(FATAL);
	if (rc == WARN) rcx = WARN;

	if (command.command == END_MF_ID)

		/* end of metafile		*/
		return(EOM);
	else {
		/* 
		 * end of a single frame. Get BEG_PIC command for next 
		 * invocation of  ctrans()
		 */
		if (Instr_Dec(&command) < 1) {
			elog(ErrGetMsg());
			return(FATAL);
		}
		return(rcx);
	}
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
void	GraphicsMode(on)
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
		if (!BATCH) {	/* don't clear batch devices */
			(void)buffer(ERASE, ERASE_SIZE);
		}
	}
	else {	/* put device in text mode	*/
		if (!BATCH) {	/* don't clear batch devices */
			(void)buffer(ERASE, ERASE_SIZE);
		}
		(void) buffer(TEXT_INIT, TEXT_INIT_SIZE);
		deviceIsInit = FALSE;
	}

	(void) flush();	/* send instruction to device	*/
}

/*
 *	CtransClear
 *	PUBLIC
 *
 *	Clear the output device.
 */
void	CtransClear()
{
	clear_device();
}


/*
 *	close_metafile:
 *	PUBLIC
 *
 *		terminate a metafile processing
 *
 */
void	close_metafile()
{
	int	devnum = devices[currdev].number;	

	/*
	 * invoke driver specific termination routine. If it has not
	 * already been invoked and we are not in debug mode.
	 */
	if (deviceIsInit && ! (*deBug)) {
#ifdef  DEAD
		/*
		 * #ifdef'd out  because on when ctrans is run in random
		 * access mode two CLEAR_DEVICES's end up getting called
		 */
		(void)(*cmdtab[devnum][DEL_ELEMENT][CLEAR_DEVICE])(&command);
#endif
		(void)(*cmdtab[devnum][DEL_ELEMENT][END_MF_ID])(&command);
	}
}

/*
 *	close_ctrans:
 *	PUBLIC
 *
 *		terminate ctrans
 *
 */
void	close_ctrans()
{


	if (!ctransIsInit) {
		return;
	}


	if (!Batch && !BATCH) {
		if (tty) (void) fclose (tty);
	}

	if (devices[currdev].use_common) ComClose();


	/*
	 *	flush the output buffer
	 */
	if (devices[currdev].usegcap)
		flush();

	(void) CloseOptionTbl(optionDesc);

	free_cgmc(&command);

	ctransIsInit = FALSE;
}

/*
 *	IsOutputToTty()
 *	PUBLIC
 *
 *	Is ctrans device output writen to a device terminal
 */
boolean	IsOutputToTty()
{
	if (!ctransIsInit) {
		return(TRUE);
	}

	return(devices[currdev].usegcap && isatty(fileno(stdout)));
}

/*
 *	ReadCtransMsg()
 *	PUBLIC
 *	
 *	Read the next ctrans error message
 *
 * on exit
 *	return		: NULL => all messages read, else the next error msg.
 */

char	*ReadCtransMsg()
{
	static	current = 0;
	if (current < eIndex) {
		return(eLog[current++]);
	}

	current = eIndex = 0;
	return(NULL);
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
 *	return		== [FATAL=-2, WARN=-1, OK=1]
 */
CtransRC	SetDevice(gcap)
	char	*gcap;
{
	char	*device;
	int	i;

	/*
	 * if we've already initialized a device close it
	 * BOGUS
	 */
	if (deviceIsInit) {
		int	devnum = devices[currdev].number;	
		(void)(*cmdtab[devnum][DEL_ELEMENT][END_MF_ID])(&command);
	}

	/*
	 *	find out the name of the device (remove the path)
	 */
	device = (device = strrchr(gcap, '/')) ? ++device : gcap;

	if (! strcmp(device, "xbfr")) {
		fprintf(stderr, "ctrans : Warning - 'xbfr' device won't be available in next release. Use 'xwd' instead\n");
		device = "xwd";
	}


	/*
	 * Find what number in the device array the device is
	 */
	for(i=0;(i<devicenum) && (strcmp(device,devices[i].name) != 0);i++)
		;
	if (i<devicenum)
		currdev = i;
	else
		currdev = 0;


	/*
	 *	Init the graph cap if one is used.
	 */
	if (devices[currdev].usegcap) {

		/*
		 *	init the graphcap stuff
		 */
		if (GP_Init(gcap) < 0) {
			elog(ErrGetMsg());
			return(FATAL);
		}
	}

	/*
	 * load device dependent portion of common routines
	 */
	if (devices[currdev].use_common) {
		ComSetDevice(devices[currdev].name);
	}

	return(OK);
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
 *	return		== [FATAL=-2, WARN=-1, OK=1]
 */
CtransRC	SetFont(fcap)
	char	*fcap;
{
	/*
	 *	Init the font cap if on is used.
	 */
	if( devices[currdev].usefcap ) {

		/*
		 *	Init the font Cap stuff
		 */
		if(Init_Font(fcap) < 0) {
			elog(ErrGetMsg());
			return (WARN);
		}

	/*	init the fontlist	*/
	InitFontList();
	}
	return (OK);
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

