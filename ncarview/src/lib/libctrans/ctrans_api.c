/*
 *      $Id: ctrans_api.c,v 1.1 1991-08-16 10:55:56 clyne Exp $
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
 *			All routines return an error status. If an error is
 *			indicated the routines CtransGetErrorNumber() and
 *			CtransGetErrorMessage() may be useful.
 *
 *			CtransOpenBatch() needs to be invoked prior to any
 *			other routine.
 */
/*LINTLIBRARY*/


#include <stdio.h> 
#include <fcntl.h> 

#ifdef	SYSV
#include <string.h>
#else
#include <strings.h>
#endif


#include <ncarv.h>
#include <cgm_tools.h>
#include <cterror.h>
#include "cgmc.h"
#include "error.h"
#include "defines.h"
#include "devices.h"
#include "ctrandef.h"



extern	char	**Argv;
extern	int	Argc;
extern	boolean	stand_Alone;
extern	boolean	Batch;
extern	boolean *softFill;
extern	boolean *deBug;
extern	boolean *bellOff;
extern	FILE	*tty;
extern	boolean	deviceIsInit;	
extern	int	currdev;
extern	struct	device	devices[];


static	CGMC	cgmc;			/* shuttle for cgm elements	*/
static	Cgm_fd	cgm_fd = -1;		/* file descriptor for metafile	*/
static	boolean	initialized = FALSE;	/* initialization state		*/
static	int	frameCount = 0;		/* current frame offset		*/

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
 *	device_name	: name of device (graphcap) to translate to. If 
 *			device_name is a graphcap and does not contain
 *			an explicit path ctrans will look for the graphcap
 *			in the default directory. Otherwise ctrans will use
 *			whatever path is specified.
 *	font_name	: name of fontcap to use for rendering fonts. As
 *			with device_name if an explicit path is not provided
 * 			ctrans will look in the default font directory for the
 *			named font.
 *	metafile	: name of metafile to translate.
 *
 * on exit
 *	return		: < 0 => error, else ok.
 * 
 * Notes
 *	if metafile is null then SetMetafile must be called before plotting
 *	can begin. If device_name or font_name is null ctrans will use
 *	the value in GRAPHCAP and FONTCAP variables respectively. 
 */
CtransOpenBatch(device_name, font_name, metafile)
	char	*device_name,
		*font_name,
		*metafile;
{
	char	*gcap,		/* the path to the graphcap	*/
		*fcap;		/* path to the fontcap		*/

	int	dummy;

	static	boolean softFillOption = FALSE;
	static	boolean deBugOption = FALSE;
	static	boolean bellOffOption = TRUE;

	char	*getGcapname(), *getFcapname();

	if (initialized) CtransCloseBatch();
        /*
         *      init ctrans' error module. 
         */
        init_ct_error("ctrans", TRUE);

	/*
	 * set all these hideous global variables to something resonable
	 */
	Argv = (char **) NULL;
	Argc = 0;
	stand_Alone = TRUE;		/* X driver creates own window	*/
	Batch = TRUE;			/* drivers don't prompt user	*/
	softFill = &softFillOption;	/* no soft fill			*/
	deBug = &deBugOption;		/* no debugging			*/
	bellOff = &bellOffOption;	/* no bell			*/
	deviceIsInit = FALSE;

	frameCount = 0;

	/*
	 * get complete name of output device (path to graphcap if a
	 * gcap device
	 */
	if (! (gcap = getGcapname(device_name))) {
		CtransSetError_(ERR_NO_DEVICE);
		return(-1);
	}  

        /*
         * inform ctrans of which output device to use
         */
        if (SetDevice(gcap) < 0) {
		CtransSetError_(ERR_INIT_DEVICE);
		return(-1);
        }

	/*
	 * force the loading of device specific option defaults
	 */
	dummy = 0;
	parseOptionTable(&dummy, (char **) NULL, devices[currdev].opt);



		/*
		 * get path name to the font cap
		 */
		if (! (fcap = getFcapname(font_name))) {

			/*
			 * no font, try and use default font
			 */
			if ((fcap = getFcapname(DEFAULTFONT)) == NULL) {
				CtransSetError_(ERR_NO_FONT);
				return(-1);
			}
		}

		/*
		 * inform ctrans of which font to use
		 */
		if (SetFont(fcap) < 0) {
			CtransSetError_(ERR_INIT_FONT);
			return(-1);
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

		return(1);
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
 *	return		: -1 => error, else ok
 */
CtransSetMetafile(metafile)
	char	*metafile;
{
	Ct_err	status;

	Ct_err	Instr_Dec(), InitInput();

	/*
	 * make sure we've been initialized
	 */
	if (! initialized) {
		CtransSetError_(ERR_INV_STATE);
		return(-1);
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
	if ((cgm_fd = CGM_open(metafile, 1440, O_RDONLY)) < 0) {
		CtransSetError_(ERR_OPEN_META);
		return(-1);
	}

	/*
	 * initialize the input module
	 */
	(void) InitInput(cgm_fd);


	/*
	 * get the first cgm element
	 */
	if (Instr_Dec(&cgmc) != OK) {
		CtransSetError_(ERR_PAR_META);
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
		return(-1);
	}

	/*
	 * make sure the first element is a Begin Metafile element
	 */
	if (cgmc.class == DEL_ELEMENT && cgmc.command == BEG_MF) {
		Process(&cgmc);
	}
	else {
		CtransSetError_(ERR_INV_META);
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
		return(-1);
	}


	/*
	 * process cgm elements until the first frame or an end of metafile 
	 * element
	 */
	while ((status = Instr_Dec(&cgmc)) == OK) {

		if ((cgmc.class == DEL_ELEMENT && cgmc.command == BEG_PIC) || 
			(cgmc.class == DEL_ELEMENT && cgmc.command == END_MF)){

			break;	/* where done	*/
		}
		/*
		 * execute the cgmc
		 */
		Process(&cgmc);
	}

	if (status != OK) {
		CtransSetError_(ERR_INV_META);
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
		return(-1);
	}

	frameCount = 0;

	/*
	 * 'cgmc' now contains a BEG_PIC or an END_METAFILE element
	 */
	return (1);
}

/*
 *	CtransPlotFrame
 *
 *	Plot the current metafile frame and advance the frame offset to 
 *	the next frame. 
 *	
 * on exit
 *	return		: -1 => error, 0 => no more frames (EOF) , 1 => OK
 */
CtransPlotFrame()
{
	Ct_err	status;

	Ct_err	Instr_Dec();

	/*
	 * make sure we've been initialized
	 */
	if (! initialized || cgm_fd == -1) {
		CtransSetError_(ERR_INV_STATE);
		return(-1);
	}

	/*
	 * See if we've reached the end of the file
	 */
	if (cgmc.class == DEL_ELEMENT && cgmc.command == END_MF) {
		return(0);
	}

	/*
	 * current element better be a begin picture
	 */
	if (! (cgmc.class == DEL_ELEMENT && cgmc.command == BEG_PIC)) {

		CtransSetError_(ERR_INV_STATE);
		return(-1);
	}
	Process(&cgmc);	/* process the begin picture element	*/


	/*
	 * process elements until we get an end of picture 
	 */
	while ((status = Instr_Dec(&cgmc)) == OK) {

		/*
		 * execute the cgmc
		 */
		Process(&cgmc);

		if (cgmc.class == DEL_ELEMENT && cgmc.command == END_PIC) {

			break;	/* we're done	*/
		}
	}

	if (status != OK) {
		CtransSetError_(ERR_INV_META);
		return(-1);
	}


	/*
	 * get the next instruction. It should be either a Begin Pic or 
	 * an End MF
	 */
	if (Instr_Dec(&cgmc) != OK) {
		CtransSetError_(ERR_PAR_META);
		return(-1);
	}

	frameCount++;
	return(1);
}

/*
 *	CtransClearDisplay
 *
 *	Generate the appropriate 'clear display' command for the device
 *
 * on exit
 *	return		: -1 => error, 0 => no more frames (EOF) , 1 => OK
 */
CtransClearDisplay()
{
	CGMC	temp_cgmc;

	if (! initialized || cgm_fd == -1) {
		CtransSetError_(ERR_INV_STATE);
		return(-1);
	}

	temp_cgmc.class = DEL_ELEMENT;
	temp_cgmc.command = CLEAR_DEVICE;
	Process(&temp_cgmc);
	return(1);
}

/*
 *	CtransCloseBatch
 *
 *	Close the translator. 
 *
 * on exit
 *	return		: -1 => error, 0 => no more frames (EOF) , 1 => OK
 */
CtransCloseBatch()
{
	if (! initialized) return;

	if (cgm_fd != -1) {
		(void) CGM_close(cgm_fd);
		cgm_fd = -1;
	}

	if (devices[currdev].use_common) ComClose();

	/*
	 * invoke driver specific termination routine. If it has not
	 * already been invoked and we are not in debug mode.
	 */
	if (deviceIsInit && ! (*deBug)) {
		cgmc.class = DEL_ELEMENT;
		cgmc.command = END_MF;
		Process(&cgmc);
	}
	/*
	 *      flush the output buffer
	 */
	if (devices[currdev].usegcap) {
		flush();
	}

	/*
	 *      close the error module
	 */
	close_ct_error();


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
 *	return		: -1 => error, 0 = > eof, 1 => ok
 */
CtransLSeekBatch(offset, whence)
	unsigned	offset,
			whence;
{
	unsigned	skip;
	int		frame_count = frameCount;

	Ct_err		status;

	Ct_err		Instr_Dec();

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
		return(-1);
	}

	/*
	 *
	 */
	while (skip != 0 && ((status = Instr_Dec(&cgmc)) == OK)) {

		if (cgmc.class == DEL_ELEMENT && cgmc.command == BEG_PIC) {
			skip--;
		}
		if (cgmc.class == DEL_ELEMENT && cgmc.command == END_MF) {
			return(0);	/* seek past end of file	*/
		}
	}

	if (status != OK) {
		CtransSetError_(ERR_INV_META);
		return(-1);
	}

	frameCount = frame_count;	/* update current frame offset	*/
	return(1);
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
 *	meaningful if an error has been indicated by one of the other routines
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
