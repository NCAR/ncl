/*
 *      $Id: ctrans_api.c,v 1.13 1992-07-30 00:47:33 clyne Exp $
 */
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
 *			indicated the routines CtransGetErrorNumber() and
 *			CtransGetErrorMessage() may be useful.
 *
 *			CtransOpenBatch() needs to be invoked prior to any
 *			other routine.
 */
/*LINTLIBRARY*/


#include <stdio.h> 
#include <fcntl.h> 
#include <errno.h> 
#include <string.h>
#include <ncarv.h>
#include <cgm_tools.h>
#include <ctrans.h>
#include "cgmc.h"
#include "error.h"
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
extern	boolean	stand_Alone;
extern	boolean	Batch;
extern	boolean *softFill;
extern	boolean *deBug;
extern	boolean *doBell;
extern	FILE	*tty;
extern	boolean	deviceIsInit;	
extern	int	optionDesc;
extern	int	currdev;
extern	struct	device	devices[];


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
	char	*device_name,
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
	int	i;

	char	*getGcapname(), *getFcapname();

	if (initialized) CtransCloseBatch();

	/*
	 * set all these hideous global variables to something resonable
	 */
	argv_[0] = dev_argv[0];
	argv_[1] = NULL;
	Argv = argv_;
	Argc = 1;
	stand_Alone = TRUE;		/* X driver creates own window	*/
	Batch = TRUE;			/* drivers don't prompt user	*/
	softFill = &softFillOption;	/* no soft fill			*/
	deBug = &deBugOption;		/* no debugging			*/
	doBell = &doBellOption;		/* no bell			*/
	deviceIsInit = FALSE;


	frameCount = 0;

	/*
	 * get complete name of output device (path to graphcap if a
	 * gcap device
	 */
	if (! (gcap = getGcapname(device_name))) {
		CtransSetError_(ERR_NO_DEVICE);
		return(FATAL);
	}  

        /*
         * inform ctrans of which output device to use
         */
        if (SetDevice(gcap) < 0) {
		CtransSetError_(ERR_INIT_DEVICE);
		return(FATAL);
        }

#ifdef	DEAD
	/*
	 * force the loading of device specific option defaults. The process
	 * is destructive to dev_argc/dev_argv so pass a copy of the arg list
	 */
	if (! (dev_argv_ = (char **) malloc ((dev_argc+1) * sizeof (char *)))){
		CtransSetError_(errno);
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
		CtransSetError_(ERR_INV_ARG);
		return(FATAL);
	}

	/*
	 * load the options into opt
	 */
	if (GetOptions(optionDesc, get_options) < 0) {
		CtransSetError_(ERR_INV_ARG);
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
			CtransSetError_(ERR_NO_FONT);
			return(FATAL);
		}
	}

	/*
	 * inform ctrans of which font to use
	 */
	if (SetFont(fcap) < 0) {
		CtransSetError_(ERR_INIT_FONT);
		return(FATAL);
	}

	init_cgmc(&cgmc);


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
	char	*metafile;
{
	int	status;
	CtransRC	rc = OK;
	CtransRC	rcx = OK;

	int	InitInput();

	/*
	 * make sure we've been initialized
	 */
	if (! initialized) {
		CtransSetError_(ERR_INV_STATE);
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
		CtransSetError_(ERR_OPEN_META);
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
	if (Instr_Dec(&cgmc) < 1) {
		CtransSetError_(ERR_PAR_META);
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
		return(FATAL);
	}

	DoEscapes(&cgmc);
	/*
	 * make sure the first element is a Begin Metafile element
	 */
	if (cgmc.class == DEL_ELEMENT && cgmc.command == BEG_MF_ID) {
		if ((rc = Process(&cgmc)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;

	}
	else {
		CtransSetError_(ERR_INV_META);
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
		return(FATAL);
	}


	/*
	 * process cgm elements until the first frame or an end of metafile 
	 * element
	 */
	while ((status = Instr_Dec(&cgmc)) > 0) {

		if ((cgmc.class == DEL_ELEMENT && cgmc.command == BEG_PIC_ID) ||
		(cgmc.class == DEL_ELEMENT && cgmc.command == END_MF_ID)){

			break;	/* where done	*/
		}
		/*
		 * execute the cgmc
		 */
		if ((rc = Process(&cgmc)) == FATAL) return(FATAL);
		if (rc == WARN) rcx = WARN;
	}

	if (status < 1) {
		CtransSetError_(ERR_INV_META);
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
		CtransSetError_(ERR_INV_STATE);
		return(FATAL);
	}

	DoEscapes(&cgmc);
	/*
	 * See if we've reached the end of the file
	 */
	if (cgmc.class == DEL_ELEMENT && cgmc.command == END_MF_ID) {
		return(EOM);
	}

	/*
	 * current element better be a begin picture
	 */
	if (! (cgmc.class == DEL_ELEMENT && cgmc.command == BEG_PIC_ID)) {

		CtransSetError_(ERR_INV_STATE);
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

		if (cgmc.class == DEL_ELEMENT && cgmc.command == END_PIC_ID) {

			break;	/* we're done	*/
		}
	}

	if (status < 1) {
		CtransSetError_(ERR_INV_META);
		return(FATAL);
	}


	/*
	 * get the next instruction. It should be either a Begin Pic or 
	 * an End MF
	 */
	if (Instr_Dec(&cgmc) < 1) {
		CtransSetError_(ERR_PAR_META);
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
		CtransSetError_(ERR_INV_STATE);
		return(FATAL);
	}

	temp_cgmc.class = DEL_ELEMENT;
	temp_cgmc.command = CLEAR_DEVICE;
	Process(&temp_cgmc);
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
		cgmc.class = DEL_ELEMENT;
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
 *	whence		: (L_SET | L_INCR)
 *
 *	Advance the current frame count as follows:
 *	
 * 	If whence is L_SET, the pointer is set to offset frames.
 *
 *	If whence is L_INCR, the pointer is set to its  current
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


	if (whence == L_SET) {
		skip = offset - frameCount;
		frame_count = offset;
	}
	else if (whence == L_INCR) {
		skip = offset;
		frame_count += offset;
	}
	else {
		CtransSetError_(ERR_INV_ARG);
		return(FATAL);
	}

	/*
	 *
	 */
	while (skip != 0 && ((status = Instr_Dec(&cgmc)) > 0)) {

		if (cgmc.class == DEL_ELEMENT && cgmc.command == BEG_PIC_ID){
			skip--;
		}
		if (cgmc.class == DEL_ELEMENT && cgmc.command == END_MF_ID) {
			return(EOM);	/* seek past end of file	*/
		}
	}

	if (status < 1) {
		CtransSetError_(ERR_INV_META);
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
CtransGetErrorNumber()
{
	return(CtransGetErrorNumber_());
}


/*
 *	CtransGetErrorMessage
 *
 *	return the current error message. The message is on meanigful if 
 *	an error has been indicated by the return value of one of the other
 *	ctrans routines.
 */
char	*CtransGetErrorMessage()
{
	return(CtransGetErrorMessage_());
}
