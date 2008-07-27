/*
 *	$Id: ctrans.c,v 1.43 2008-07-27 03:18:43 haley Exp $
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
#include <stdlib.h> 
#include <string.h>


#include <ctype.h>
#include <errno.h>
#include <ncarg/c.h>
#include <ncarg/cgm_tools.h>
#include "ctrans.h"
#include "cgmc.h"
#include "defines.h"
#include "translate.h"
#include "devices.h"
#include "ctrandef.h"
#include "graphcap.h"
#include "fontlist.h"




extern int GP_Init();
extern int Init_Font();
extern int InitInput();
extern int SetRecord();

extern	int	(*cmdtab[][MAXCLASS+1][MAXFUNCPERCLASS+1]) ();
extern	struct	device	devices[];

char	**Argv;
int	Argc;
boolean	Batch;		/* if true don't prompt for user interaction	*/ 
/*
 * this flag is a hack to prevent bogus CGM's from attempting to change 
 * background colour after drawing has begun for a frame
 */
boolean startedDrawing = FALSE;

/*
 * storate for some global command line options. 
 */
static	boolean softfill = 0;
static	boolean debug = 0;
static	boolean bell_off = 0;
static	const char	*palFname = NULL;

boolean *softFill = &softfill;
boolean *deBug = &debug;
boolean *doBell = &bell_off;

/*
 * device dependent initialization state  (cgi, X11, graphcap, clear_text)
 */
boolean	deviceIsInit = FALSE;	
static boolean	metafileIsInit	= FALSE;

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
		(void) strncpy(eLog[eIndex],msg, sizeof(eLog[eIndex])-1);
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
int	init_cgmc (cgmc)
CGMC *cgmc;
{
	int	i;

	unsigned ci_size = NUMTOALLOC * sizeof(CItype);
	unsigned cd_size = NUMTOALLOC * sizeof(CDtype);
	unsigned ix_size = NUMTOALLOC * sizeof(IXtype);
	unsigned e_size = NUMTOALLOC * sizeof(Etype);
	unsigned i_size = NUMTOALLOC * sizeof(Itype);
	unsigned r_size = NUMTOALLOC * sizeof(Rtype);
	unsigned s_size = sizeof(Stype);
	unsigned vdc_size = NUMTOALLOC * sizeof(VDCtype);
	unsigned p_size = NUMTOALLOC * sizeof(Ptype);
	unsigned c_size = NUMTOALLOC * sizeof(Ctype);
	unsigned d_size = NUMTOALLOC * sizeof(Dtype);

	if (! (cgmc->ci = (CItype *) malloc(ci_size))) {
		ESprintf(errno, "malloc(%d)", ci_size);
		return(-1);
	}
	cgmc->CIspace = NUMTOALLOC;
	cgmc->CInum = 0;

	if (! (cgmc->cd = (CDtype *) malloc(cd_size))) {
		ESprintf(errno, "malloc(%d)", cd_size);
		return(-1);
	}
	cgmc->CDspace = NUMTOALLOC;
	cgmc->CDnum = 0;

	if (! (cgmc->ix = (IXtype *) malloc(ix_size))) {
		ESprintf(errno, "malloc(%d)", ix_size);
		return(-1);
	}
	cgmc->IXnum = 0;
	cgmc->IXspace = NUMTOALLOC;

	if (! (cgmc->e  = (Etype *) malloc(e_size))) {
		ESprintf(errno, "malloc(%d)", e_size);
		return(-1);
	}
	cgmc->Espace = NUMTOALLOC;
	cgmc->Enum = 0;

	if (! (cgmc->i  = (Itype *) malloc (i_size))) {
		ESprintf(errno, "malloc(%d)", i_size);
		return(-1);
	}
	cgmc->Ispace = NUMTOALLOC;
	cgmc->Inum = 0;

	if (! (cgmc->r  = (Rtype *) malloc (r_size))) {
		ESprintf(errno, "malloc(%d)", r_size);
		return(-1);
	}
	cgmc->Rspace = NUMTOALLOC;
	cgmc->Rnum = 0;

	if (! (cgmc->s  = (Stype *) malloc (s_size))) {
		ESprintf(errno, "malloc(%d)", s_size);
		return(-1);
	}
	cgmc->Sspace = 1;
	cgmc->Snum = 0;
		cgmc->s->string = (char **) malloc(
			(unsigned) (cgmc->Sspace * sizeof(char *))
		);
		if (! cgmc->s->string) {
			ESprintf(errno, "malloc()");
			return(-1);
		}

		cgmc->s->string_space = (int *) malloc(
			(unsigned) (cgmc->Sspace * sizeof(int))
		);
		if (! cgmc->s->string_space) {
			ESprintf(errno, "malloc()");
			return(-1);
		}

		cgmc->s->string_space[0] = 256;
		cgmc->s->string[0] = (char *) malloc (
			(unsigned) (cgmc->s->string_space[0] * sizeof(char))
		);
		if (! cgmc->s->string_space[0]) {
			ESprintf(errno, "malloc()");
			return(-1);
		}

		for (i = 1; i < cgmc->Sspace; i++) {
			cgmc->s->string_space[i] = 0;
			cgmc->s->string[i] = NULL;
		}

	if (! (cgmc->vdc=(VDCtype *)malloc(vdc_size))) {
		ESprintf(errno, "malloc(%d)", vdc_size);
		return(-1);
	}
	cgmc->VDCspace = NUMTOALLOC;
	cgmc->VDCnum = 0;

	if (! (cgmc->p  = (Ptype *) malloc (p_size))) {
		ESprintf(errno, "malloc(%d)", p_size);
		return(-1);
	}
	cgmc->Pspace = NUMTOALLOC;
	cgmc->Pnum = 0;

	if (! (cgmc->c  = (Ctype *) malloc (c_size))) {
		ESprintf(errno, "malloc(%d)", c_size);
		return(-1);
	}
	cgmc->Cspace = NUMTOALLOC;
	cgmc->Cnum = 0;

	if (! (cgmc->d  = (Ctype *) malloc (d_size))) {
		ESprintf(errno, "malloc(%d)", d_size);
		return(-1);
	}
	cgmc->Dspace = NUMTOALLOC;
	cgmc->Dnum = 0;

	cgmc->more = FALSE;
	return(1);
}

void	free_cgmc(cgmc)
	CGMC	*cgmc;
{
	/*
	 * free the cgmc
	 */
	free((Voidptr) cgmc->c);
	free((Voidptr) cgmc->ci);
	free((Voidptr) cgmc->cd);
	free((Voidptr) cgmc->e);
	free((Voidptr) cgmc->i);
	free((Voidptr) cgmc->ix);
	free((Voidptr) cgmc->p);
	free((Voidptr) cgmc->r);
	free((Voidptr) cgmc->s->string[0]);
	free((Voidptr) cgmc->s->string);
	free((Voidptr) cgmc->s->string_space);
	free((Voidptr) cgmc->s);
	free((Voidptr) cgmc->vdc);
}


static	void	decode_err(status)
	int	status;
{
	if (status < 0) {
		elog(ErrGetMsg());
	}
	else if (status == 0) {
		elog("Premature end of metafile");
	}
}

static	const char	*element_name(cgmclass, id)
	int	cgmclass, id;
{
	const	char	*name;

	name = CGM_ElementLookup((unsigned int) cgmclass, (unsigned int) id);

	if (! name) name = "Unknown";

	return(name);
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

	if ((c->cgmclass > MAXCLASS) || (c->command > MAXFUNCPERCLASS)) {
		const char	*name = element_name(c->cgmclass, c->command);

		ESprintf(
			EINVAL, 
			"Illegal metafile element(class=%d, id=%d, name=\"%s\")",
			c->cgmclass, c->command, name
		);
		elog(ErrGetMsg());
		return(WARN);
	}
	else if (cmdtab[devnum][c->cgmclass][c->command]) {
		rc = (*cmdtab[devnum][c->cgmclass][c->command])(c);
	}
	else {
		const char	*name = element_name(c->cgmclass, c->command);
		/*
		 * no function for element
		 */
		ESprintf(
			EINVAL, 
			"Illegal metafile element(class=%d, id=%d, name=\"%s\")",
			c->cgmclass, c->command, name
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
	else if (c->cgmclass == DEL_ELEMENT || c->cgmclass == DES_ELEMENT) {
		const char	*name = element_name(c->cgmclass, c->command);

		ESprintf(
			E_UNKNOWN, 
			"Could not process CGM element(class=%d, id=%d, name=\"%s\") [ %s ]",
			c->cgmclass, c->command, name, ErrGetMsg()
		);
		elog(ErrGetMsg());
		return(FATAL);
	}
	/*
	 * non-fatal error (we hope)
	 */
	else {
		const char	*name = element_name(c->cgmclass, c->command);

		ESprintf(
			E_UNKNOWN, 
			"Error processing CGM element(class=%d, id=%d, name=\"%s\") [ %s ]",
			c->cgmclass, c->command, name, ErrGetMsg()
		);
		elog(ErrGetMsg());
		return(WARN);
	}
}

static	void	clear_device()
{
	CGMC	temp_cgmc;

	temp_cgmc.cgmclass = DEL_ELEMENT;
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
	while ((cgmc->cgmclass == ESC_ELEMENT && cgmc->command == ESCAPE_ID) ||
		(cgmc->cgmclass == DEL_ELEMENT && cgmc->command == NOOP_ID)) {

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
 *	batch		: True => don't prompt for user interaction
 *
 * on exit
 *	*argc		: contains argc
 *	**argv		: points to argv
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
CtransRC	init_ctrans(argc, argv, gcap, fcap, batch)
	int	*argc;
	char	**argv;

	const char	*gcap,
			*fcap;
	boolean	batch;

{
	char	*minw;
	CtransRC	rc = OK;
 
	Batch = batch;

	if (ctransIsInit) {
		close_ctrans();
	}


	/*
	 * set the output device
	 */
	if ((int) SetDevice(gcap) < 0) {
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
		rc = WARN;
	}	

	Argv = argv;
	Argc = *argc;


	if (init_cgmc(&command) < 0) {
		return(FATAL);
	}

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
		if (SetFont(fcap) < 0) {
			rc = WARN;
		}
	}


	ctransIsInit = TRUE;
	return(rc);
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
	int		status;
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
	 *	Instr_Dec returns 0 on EOF, -1 on error.
	 */
	if ((status = Instr_Dec(&command)) < 1) {
		decode_err(status);
		return(FATAL);
	}
	DoEscapes(&command);
	if (command.cgmclass == DEL_ELEMENT && command.command == BEG_MF_ID) {
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
	if ((status = Instr_Dec(&command)) < 1) {
		decode_err(status);
		return(FATAL);
	}
	while((command.cgmclass != DEL_ELEMENT || command.command != BEG_PIC_ID) &&
		(command.cgmclass != DEL_ELEMENT || command.command != END_MF_ID)){

		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

		if ((status = Instr_Dec(&command)) < 1) {
			decode_err(status);
			return(FATAL);
		}
	}	 
	metafileIsInit = TRUE;

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
		if ((status = Instr_Dec(&command)) < 1) {
			decode_err(status);
			return(FATAL);
		}
	}

	DoEscapes(&command);
	/*
	 * see if we've reached the end of the file
	 */
	if (command.cgmclass == DEL_ELEMENT && command.command == END_MF_ID) {
		/* 
		 * process the END MF command	
		 */
		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;
		return((CtransRC) EOM);
	}

	/*
	 * current element better be a begin picture
	 */
	if (! (command.cgmclass == DEL_ELEMENT && command.command == BEG_PIC_ID)) {
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

		if (command.cgmclass == DEL_ELEMENT && command.command == END_PIC_ID){

			break;	/* we're done	*/
		}
	}

	if (status < 1) {
		decode_err(status);
		return(FATAL);
	}


	/*
	 * get the next instruction. It should be either a Begin Pic or 
	 * an End MF or an escape
	 */
	if ((status = Instr_Dec(&command)) < 1) {
		decode_err(status);
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
	int		status;
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
	if ((status = Instr_Dec(&command)) < 1) {
		decode_err(status);
		return(FATAL);
	}

	/*
	 * 	Do until get a END PICTURE or END METAFILE element
	 */
	while (((command.cgmclass != DEL_ELEMENT) 
			|| (command.command != END_PIC_ID))
		&& ((command.cgmclass != DEL_ELEMENT) 
			|| (command.command != END_MF_ID))) {
			

		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

		if ((status = Instr_Dec(&command)) < 1) {
			decode_err(status);
			return(FATAL);
		}
	}


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

		if ((status = Instr_Dec(&command)) < 1) {
			decode_err(status);
			return(FATAL);
		}
	}while(
		(command.cgmclass != DEL_ELEMENT || 
		command.command != BEG_PIC_B_ID)
		&& (command.cgmclass != DEL_ELEMENT || 
		command.command != END_MF_ID));


	/*
	 * get the next command
	 */
	if ((status = Instr_Dec(&command)) < 1) {
		decode_err(status);
		return(FATAL);
	}

	/*
	 * 	Do until get a END PICTURE or END METAFILE element
	 */
	while (((command.cgmclass != DEL_ELEMENT) 
			|| (command.command != END_PIC_ID))
		&& ((command.cgmclass != DEL_ELEMENT) 
			|| (command.command != END_MF_ID))) {

		if ((rc = Process(&command)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

		if ((status = Instr_Dec(&command)) < 1) {
			decode_err(status);
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
		return((CtransRC) EOM);
	else {
		/* 
		 * end of a single frame. Get BEG_PIC command for next 
		 * invocation of  ctrans()
		 */
		if ((status = Instr_Dec(&command)) < 1) {
			decode_err(status);
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

	extern	int	gcap_graphics_mode_();

	/*
	 * do nothing if init_ctrans has not been called or if not a
	 * graphcap device. Ugh. This really ought to call a device-specific
	 * routine instead of hard-coding the gcap version.
	 */
	if (! ctransIsInit || ! devices[currdev].usegcap)
		return;

	(void) gcap_graphics_mode_(on);
}

/*
 *	CtransClear
 *	PUBLIC
 *
 *	Clear the output device.
 */
void	CtransClear()
{
	if (metafileIsInit) clear_device();
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

	metafileIsInit = FALSE;
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
	const char	*gcap;
{
	const char	*device;
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
		(void) fprintf(stderr, "ctrans : Warning - 'xbfr' device won't be available in next release. Use 'xwd' instead\n");
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
	const char	*fcap;
{
	/*
	 *	Init the font cap if on is used.
	 */
	if( devices[currdev].usefcap ) {

		/*	init the fontlist	*/
		InitFontList(fcap);
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
	const char	*pal_fname;
{
	palFname = pal_fname;
}

