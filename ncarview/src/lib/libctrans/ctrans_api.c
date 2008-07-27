/*
 *      $Id: ctrans_api.c,v 1.23 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1995                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	File:		ctrans_api.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 29 11:22:43 MDT 1991
 *
 *	Description:	This file contains a simplified programming interface
 *			to ctrans. It is generally preferable to the routines
 *			found in the file ctrans.c. These routines should
 *			not be mixed with those found in ctrans.c. 
 *
 *			All routines return an error status of FATAL - No
 *			further processing possible, WARN - warning, 
 *			EOM - end of metafile, or OK. If an error (FATAL 
 *			or WARN) is
 *			indicated the routine CtransGetErrorMessage() 
 *			may be useful.
 *
 *			CtransOpenBatch() needs to be invoked prior to any
 *			other routine.
 */
/*LINTLIBRARY*/


#include <stdio.h> 
#include <fcntl.h> 
#include <errno.h> 
#include <string.h>
#include <ncarg/c.h>
#include <ncarg/cgm_tools.h>
#include "ctrans.h"
#include "cgmc.h"
#include "defines.h"
#include "devices.h"
#include "ctrandef.h"


/*
 *      a global structure that contains values of command line options
 */
static  struct  {
	float   lmin;		/* minimum line width           */
	float   lmax;		/* maximun line width           */
	float   lscale;		/* additional line scaling      */
	float   rgbscale;	/* rbg color intensity scaling	*/
} opt;


static  OptDescRec      set_options[] = {
	{"lmin", 1, "-1", "Set minimum line width"},
	{"lmax", 1, "-1", "Set maximum line width"},
	{"lscale", 1, "-1", "Scale all lines by 'arg0'"},
	{"rgbscale", 1, "1.0", "Scale rgb color intensities by 'arg0'"},
	{NULL}
};

static  Option  get_options[] = {
	{"lmin", NCARGCvtToFloat, (Voidptr) &opt.lmin, sizeof(opt.lmin )
	},
	{"lmax", NCARGCvtToFloat, (Voidptr) &opt.lmax, sizeof (opt.lmax )
	},
	{"lscale", NCARGCvtToFloat, (Voidptr) &opt.lscale, sizeof (opt.lscale)
	},
	{"rgbscale", NCARGCvtToFloat, (Voidptr) &opt.rgbscale, 
						sizeof (opt.rgbscale)
	},
	{NULL
	}
};





extern	char	**Argv;
extern	int	Argc;
extern	boolean	Batch;
extern	boolean *softFill;
extern	boolean *deBug;
extern	boolean *doBell;
extern	boolean	deviceIsInit;	
extern	int	optionDesc;
extern	int	currdev;
extern	struct	device	devices[];
extern	boolean	startedDrawing;

static	CGMC	cgmc;			/* shuttle for cgm elements	*/
static	Cgm_fd	cgm_fd = -1;		/* file descriptor for metafile	*/
static	boolean	initialized = FALSE;	/* initialization state		*/
static	int	frameCount = 0;		/* current frame offset		*/

static	char	*argv_[10];

/*
 *
 *	CtransOpenBatch
 *
 *	Initialize ctrans for batch processing of a metafile. The various
 *	ctrans drivers will perform no interaction with the user. It is
 *	up to the programmer to supply whatever interation is desired. e.g.
 *	The X driver will not respond to X events from the user.
 *
 * on entry:
 *	*device_name	: name of device (graphcap) to translate to. If 
 *			device_name is a graphcap and does not contain
 *			an explicit path ctrans will look for the graphcap
 *			in the default directory. Otherwise ctrans will use
 *			whatever path is specified.
 *	*font_name	: name of fontcap to use for rendering fonts. As
 *			with device_name if an explicit path is not provided
 * 			ctrans will look in the default font directory for the
 *			named font.
 *	*metafile	: name of metafile to translate.
 *	**dev_argc	: device specific argument  count
 *	*dev_argv	: device specific arguments
 *
 * on exit
 *	return		: [FATAL, WARN, EOM, OK]
 * 
 * Notes
 *	if metafile is null then SetMetafile must be called before plotting
 *	can begin. If device_name or font_name is null ctrans will use
 *	the value in GRAPHCAP and FONTCAP variables respectively. 
 */
CtransRC	CtransOpenBatch(device_name, font_name, metafile, dev_argc, dev_argv)
	const char	*device_name,
		*font_name,
		*metafile;
	int	*dev_argc;
	char	**dev_argv;
{
	char	*gcap,		/* the path to the graphcap	*/
		*fcap;		/* path to the fontcap		*/

#ifdef	DEAD
	char	**dev_argv_;
#endif

	static	boolean softFillOption = FALSE;
	static	boolean deBugOption = FALSE;
	static	boolean doBellOption = FALSE;

	if (initialized) CtransCloseBatch();

	/*
	 * set all these hideous global variables to something resonable
	 */
	argv_[0] = dev_argv[0];
	argv_[1] = NULL;
	Argv = argv_;
	Argc = 1;
	Batch = TRUE;			/* drivers don't prompt user	*/
	softFill = &softFillOption;	/* no soft fill			*/
	deBug = &deBugOption;		/* no debugging			*/
	doBell = &doBellOption;		/* no bell			*/
	deviceIsInit = FALSE;
	startedDrawing = FALSE;		/* haven't begun to draw yet	*/


	frameCount = 0;

	/*
	 * get complete name of output device (path to graphcap if a
	 * gcap device
	 */
	if (! (gcap = getGcapname(device_name))) {
		ESprintf(
			E_UNKNOWN, "Device (%s) unknown [ %s ]", 
			device_name, ErrGetMsg()
		);
		return(FATAL);
	}  

        /*
         * inform ctrans of which output device to use
         */
        if ((int) SetDevice(gcap) < 0) {
		ESprintf(
			E_UNKNOWN, "Can't initialize device(%s) [ %s ]",
			gcap, ErrGetMsg()
		);
		return(FATAL);
        }

#ifdef	DEAD
	/*
	 * force the loading of device specific option defaults. The process
	 * is destructive to dev_argc/dev_argv so pass a copy of the arg list
	 */
	if (! (dev_argv_ = (char **) malloc ((dev_argc+1) * sizeof (char *)))){
		ESprintf(errno, "malloc(%d)", (devic_argc+1) * sizeof (char*));
		return(FATAL);
	}
	for (i=0; i<dev_argc; i++) {
		dev_argv_[i] = (char *) malloc (strlen(dev_argv[i] + 1));
		(void) strcpy (dev_argv_[i], dev_argv[i]);
	}
#endif
	optionDesc = OpenOptionTbl();
	(void) LoadOptionTable(optionDesc, set_options);
	if (ParseOptionTable(
		optionDesc, dev_argc, dev_argv, devices[currdev].opt) < 0) 
	{
		ESprintf(
			E_UNKNOWN, "Can't process device options [ %s ]",
			ErrGetMsg()
		);
		return(FATAL);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(optionDesc, get_options) < 0) {
		ESprintf(
			E_UNKNOWN,"GetOptions(%d,) [ %s ]",
			optionDesc, ErrGetMsg()
		);
		return(FATAL);
	}

	/*
	 * set line scaling options
	 */
	if (opt.lmin > -1) SetMinLineWidthDefault(opt.lmin);
	if (opt.lmax > -1) SetMaxLineWidthDefault(opt.lmax);
	if (opt.lscale > -1) SetAdditionalLineScale(opt.lscale);
	if (opt.lscale != 1.0) SetRGBIntensityScale(opt.rgbscale);




	/*
	 * get path name to the font cap
	 */
	if (! (fcap = getFcapname(font_name))) {

		/*
		 * no font, try and use default font
		 */
		if ((fcap = getFcapname(DEFAULTFONT)) == NULL) {
			ESprintf(
				E_UNKNOWN, "Device (%s) unknown [ %s ]", 
				device_name, ErrGetMsg()
			);
			return(FATAL);
		}
	}

	/*
	 * inform ctrans of which font to use
	 */
	if ((int) SetFont(fcap) < 0) {
		ESprintf(
			E_UNKNOWN, "Can't initialize font(%s) [ %s ]",
			fcap, ErrGetMsg()
		);
		return(FATAL);
	}

	if (init_cgmc(&cgmc) < 0) {
		return(FATAL);
	}


	/*
	 * after we init the cgmc we want to set the initialize flag to
	 * keep us from renitialzing the cgmc twice
	 */
	initialized = TRUE;

	if (metafile) {
		return(CtransSetMetafile(metafile));
	}

	return(OK);
}

/*
 *	CtransSetMetafile
 *
 *	Specify the metafile to be translated.
 *
 * on entry
 *	metafile	: name of metafile
 *
 * on exit 
 *	return		: [FATAL, WARN, EOM, OK]
 */
CtransRC	CtransSetMetafile(metafile)
	const char	*metafile;
{
	int	status;
	CtransRC	rc = OK;
	CtransRC	rcx = OK;

	int	InitInput();

	/*
	 * make sure we've been initialized
	 */
	if (! initialized) {
		ESprintf(E_UNKNOWN, "ctrans not in proper state");
		return(FATAL);
	}

	/*
	 * if we currently have a metafile open close it
	 */
	if (cgm_fd != -1) {
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
	}

	/*
	 *      open the metafile
	 */
	if ((cgm_fd = CGM_open(metafile, 1440, "r")) < 0) {
		ESprintf(errno, "CGM_open(%s, 1440, r)", metafile);
		return(FATAL);
	}

	/*
	 * initialize the input module
	 */
	(void) InitInput(cgm_fd);

	/*
	 * intialize the CGM default table
	 */
	InitDefault();	

	/*
	 * get the first cgm element
	 */
	if ((status = Instr_Dec(&cgmc)) < 1) {
		if (status == 0) {
			ESprintf(E_UNKNOWN,"Premature end of metafile");
		}
		else {
			ESprintf(E_UNKNOWN,
				"Fetching CGM element [ %s ]", ErrGetMsg()
			);
		}
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
		return(FATAL);
	}

	DoEscapes(&cgmc);
	/*
	 * make sure the first element is a Begin Metafile element
	 */
	if (cgmc.cgmclass == DEL_ELEMENT && cgmc.command == BEG_MF_ID) {
		if ((rc = Process(&cgmc)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

	}
	else {
		ESprintf(E_UNKNOWN,"Metafile missing BEGIN METAFILE element");
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
		return(FATAL);
	}


	/*
	 * process cgm elements until the first frame or an end of metafile 
	 * element
	 */
	while ((status = Instr_Dec(&cgmc)) > 0) {

		if ((cgmc.cgmclass == DEL_ELEMENT && cgmc.command == BEG_PIC_ID) ||
		(cgmc.cgmclass == DEL_ELEMENT && cgmc.command == END_MF_ID)){

			break;	/* where done	*/
		}
		/*
		 * execute the cgmc
		 */
		if ((rc = Process(&cgmc)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;
	}

	if (status < 1) {
		if (status == 0) {
			ESprintf(E_UNKNOWN,"Premature end of metafile");
		}
		else {
			ESprintf(E_UNKNOWN,
				"Fetching CGM element [ %s ]", ErrGetMsg()
			);
		}
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
		return(FATAL);
	}

	frameCount = 0;

	/*
	 * 'cgmc' now contains a BEG_PIC_ID or an END_METAFILE element
	 */
	return (rcx);
}

/*
 *	CtransPlotFrame
 *
 *	Plot the current metafile frame and advance the frame offset to 
 *	the next frame. 
 *	
 * on exit
 *	return		: [FATAL, WARN, EOM, OK]
 */
CtransRC	CtransPlotFrame()
{
	int		status;
	CtransRC	rc = OK;
	CtransRC	rcx = OK;

	/*
	 * make sure we've been initialized
	 */
	if (! initialized || cgm_fd == -1) {
		ESprintf(E_UNKNOWN, "ctrans not in proper state");
		return(FATAL);
	}

	DoEscapes(&cgmc);
	/*
	 * See if we've reached the end of the file
	 */
	if (cgmc.cgmclass == DEL_ELEMENT && cgmc.command == END_MF_ID) {
		return((CtransRC) EOM);
	}

	/*
	 * current element better be a begin picture
	 */
	if (! (cgmc.cgmclass == DEL_ELEMENT && cgmc.command == BEG_PIC_ID)) {
		ESprintf(E_UNKNOWN, "BEGIN PICTURE element expected");
		return(FATAL);
	}
	/* process the begin picture element	*/
	if ((rc = Process(&cgmc)) == FATAL) return(FATAL);
	if (rc == WARN) rcx = WARN;


	/*
	 * process elements until we get an end of picture 
	 */
	while ((status = Instr_Dec(&cgmc)) > 0) {

		/*
		 * execute the cgmc
		 */
		if ((rc = Process(&cgmc)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

		if (cgmc.cgmclass == DEL_ELEMENT && cgmc.command == END_PIC_ID) {

			break;	/* we're done	*/
		}
	}

	if (status < 1) {
		if (status == 0) {
			ESprintf(E_UNKNOWN,"Premature end of metafile");
		}
		else {
			ESprintf(E_UNKNOWN,
				"Fetching CGM element [ %s ]", ErrGetMsg()
			);
		}
		return(FATAL);
	}


	/*
	 * get the next instruction. It should be either a Begin Pic or 
	 * an End MF
	 */
	if (Instr_Dec(&cgmc) < 1) {
		if (status == 0) {
			ESprintf(E_UNKNOWN,"Premature end of metafile");
		}
		else {
			ESprintf(E_UNKNOWN,
				"Fetching CGM element [ %s ]", ErrGetMsg()
			);
		}
		return(FATAL);
	}

	frameCount++;
	return(rcx);
}

/*
 *	CtransClearDisplay
 *
 *	Generate the appropriate 'clear display' command for the device
 *
 * on exit
 *	return		: [FATAL, WARN, EOM, OK]
 */
CtransRC	CtransClearDisplay()
{
	CGMC	temp_cgmc;

	if (! initialized || cgm_fd == -1) {
		ESprintf(E_UNKNOWN, "ctrans not in proper state");
		return(FATAL);
	}

	temp_cgmc.cgmclass = DEL_ELEMENT;
	temp_cgmc.command = CLEAR_DEVICE;
	(void) Process(&temp_cgmc);
	return(OK);
}

/*
 *	CtransCloseBatch
 *
 *	Close the translator. 
 *
 * on exit
 */
void	CtransCloseBatch()
{
	extern	void	free_cgmc();

	if (! initialized) return;

	if (cgm_fd != -1) {
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
	}

	(void) CloseOptionTbl(optionDesc);

	if (devices[currdev].use_common) ComClose();

	/*
	 * invoke driver specific termination routine. If it has not
	 * already been invoked and we are not in debug mode.
	 */
	if (deviceIsInit && ! (*deBug)) {
		cgmc.cgmclass = DEL_ELEMENT;
		cgmc.command = END_MF_ID;
		(void) Process(&cgmc);
	}
	/*
	 *      flush the output buffer
	 */
	if (devices[currdev].usegcap) {
		flush();
	}

        /*
         * free the cgmc
         */
	free_cgmc(&cgmc);

	initialized = FALSE;
}

/*
 *	CtransLSeekBatch
 * 
 *	Perform a 'seek' on the metafile
 * entry
 *	offset		: frame offset
 *	whence		: (SEEK_SET | SEEK_CUR)
 *
 *	Advance the current frame count as follows:
 *	
 * 	If whence is SEEK_SET, the pointer is set to offset frames.
 *
 *	If whence is SEEK_CUR, the pointer is set to its  current
 *	location plus offset frames.
 *
 *	Currently only forward seeking is possible. The first frame is 
 *	frame zero.	You cannot seek past the end of the file. 
 *
 * on exit
 *	return		: [FATAL, WARN, EOM, OK]
 */
CtransRC	CtransLSeekBatch(offset, whence)
	unsigned	offset,
			whence;
{
	unsigned	skip;
	int		frame_count = frameCount;

	int		status;


	if (whence == SEEK_SET) {
		skip = offset - frameCount;
		frame_count = offset;
	}
	else if (whence == SEEK_CUR) {
		skip = offset;
		frame_count += offset;
	}
	else {
		ESprintf(EINVAL, "");
		return(FATAL);
	}

	/*
	 *
	 */
	while (skip != 0 && ((status = Instr_Dec(&cgmc)) > 0)) {

		if (cgmc.cgmclass == DEL_ELEMENT && cgmc.command == BEG_PIC_ID){
			skip--;
		}
		if (cgmc.cgmclass == DEL_ELEMENT && cgmc.command == END_MF_ID) {
			return((CtransRC) EOM);	/* seek past end of file */
		}
	}

	if (status < 1) {
		if (status == 0) {
			ESprintf(E_UNKNOWN,"Premature end of metafile");
		}
		else {
			ESprintf(E_UNKNOWN,
				"Fetching CGM element [ %s ]", ErrGetMsg()
			);
		}
		return(FATAL);
	}

	frameCount = frame_count;	/* update current frame offset	*/
	return(OK);
}

/*
 *	CtransGraphicsMode
 *
 *	toggle graphics mode on or off. This may be useful when the output
 *	device is the user's terminal
 *
 * on entry 
 *	on		: (1 | 0)
 */
void	CtransGraphicsMode(on)
	boolean	on;
{
	GraphicsMode(on);
}

/*
 *	CtransGetErrorNumber
 *
 *	returns the current error number. The returned value is only 
 *	meaningful if an error has been indicated by one of the other routines.
 *	A negative return value means the unix global variable 'errno' is 
 *	the absolute of the return value.
 */
#ifdef	DEAD
int	CtransGetErrorNumber()
{
	return(CtransGetErrorNumber_());
}
#endif


/*
 *	CtransGetErrorMessage
 *
 *	return the current error message. The message is on meanigful if 
 *	an error has been indicated by the return value of one of the other
 *	ctrans routines.
 */
const	char	*CtransGetErrorMessage()
{
	return(ErrGetMsg());
}
