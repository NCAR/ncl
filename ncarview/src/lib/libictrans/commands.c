/*
 *	$Id: commands.c,v 1.11 1991-12-30 12:33:05 clyne Exp $
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

#include <stdio.h>
#include <signal.h>
#include <ctype.h>

#ifndef	CRAY
#include <sys/wait.h>
#endif

#include <errno.h>
#include <cgm_tools.h>
#include <ncarv.h>
#include "ictrans.h"

#ifdef	sun
#include <vfork.h>
#define	FORK	vfork
#else
#define	FORK	fork
#endif

#ifndef	DEBUG
extern	char	*getFcapname();
extern	char	*getGcapname();
#endif

extern	char	*strcpy();
extern	char	*strcat();

IcState	icState = {
		FALSE, 1, 1, 1, 0, 0, FALSE, 0, NULL, NULL, NULL, NULL, NULL,
		{0.0, 0.0, 1.0, 1.0}
		};

static	Directory	*Toc;
static	short		loopAbort;
static	char		buf[1024];


int	iCHelp(ic)
	ICommand	*ic;
{

	int	i;
	CmdOp	*c;
	int	fd = ic->fd;

	extern	int	NUM_CMDS;
	extern	CmdOp	cmdOptab[];
	extern	CmdOp	*getcmdOp();

	/*
	 * if user is not asking for help on a particular command print help
	 * for all commands
	 */
	if (!ic->cmd.data) {
		for (i = 0, c = &cmdOptab[0]; i < NUM_CMDS; i++, c++) {
			(void) sprintf (buf,"%-10s : %s\n",c->c_name,c->c_help);
			(void) write (fd, buf, strlen(buf));
		} 
			(void) sprintf(buf,
			"\nfor usage of a specific command type: help <command>\n");
			(void) write (fd, buf, strlen(buf));
		return;
	}

	/*
	 * user wants help on a single command. See if its a valid one
	 * and print a usage message
	 */
	if ((c = getcmdOp(ic->cmd.data)) == (CmdOp *) -1) {
		(void) fprintf(stderr,"Ambiguous command < %s >", ic->cmd.data);
		return;
	}
	if (c == (CmdOp *) NULL) {
		(void) fprintf(stderr,"No such command < %s >\n", ic->cmd.data);
		return;
	}

	/*
	 * print the useage message
	 */
	(void) sprintf(buf, "<%s> usage: %s\n", c->c_name, c->c_usage);
	(void) write (fd, buf, strlen(buf));
		
}

int	iCFile(ic)
	ICommand	*ic;
{

	char	*s = ic->cmd.data;
	int	fd = ic->fd;
	static	char	*fileName = NULL;

	int	argc;
	char	**argv;
	
	Directory	*toc;

	if (!s) {	/* if no file report current file	*/
		if (*icState.file) {
			(void) sprintf(buf, "%s\n", *icState.file);
			(void) write (fd, buf, strlen(buf));
		}
		else {
			(void) sprintf(buf, "No file\n");
			(void) write (fd, buf, strlen(buf));
		}
		return;
	}

	glob(s, &argv, &argc);

	if (argc == 0) {	/* do nothing	*/
		(void) fprintf(stderr, "File: %s not found\n", ic->cmd.data);
		return;
	}

	if (argc > 1) {
		(void) fprintf(stderr, "Too many file names\n");
		return;
	}
	(void) sprintf(buf, "%s\n", argv[0]);
	(void) write (fd, buf, strlen(buf));

		

	if ((toc = CGM_initMetaEdit(argv[0], 1440, NULL)) == NULL) {
		perror((char *) NULL);
		return;
	}

	ic->current_frame = 1;
	ic->last_frame = CGM_NUM_FRAMES(toc);
	Toc = toc;

	icState.num_frames = CGM_NUM_FRAMES(toc);
	icState.stop_segment = CGM_NUM_FRAMES(toc);

	
	(void) init_metafile(0, CGM_FD(toc));


	/*
	 * record the file name
	 */
	if (fileName) cfree(fileName);
	fileName = icMalloc(strlen(argv[0]) + 1);
	(void) strcpy(fileName, argv[0]);
	*icState.file = fileName;

	(void) sprintf(buf, "%d frames\n", CGM_NUM_FRAMES(toc));
	(void) write (fd, buf, strlen(buf));
	
}

int	iCSave(ic)
	ICommand	*ic;
{

	char	*s = ic->cmd.data;		/* file to save to	*/
	int	fd = ic->fd;
	static	char	*saveFile = NULL;	/* save name of file	*/

	int	start_frame, num_frames;
	int	status;	
	int	type;	/* type of write, 1 => write, 0 => append	*/
	int	i;
	int	c;

	extern	int	errno;

	int	argc;
	char	**argv;
	

	/*
	 * see if file given. if not use last saved file if it exists
	 */
	if (!s) {
		if (!(s = icState.save_file)) {
			(void) fprintf(stderr, "No current save file\n");
			return;
		}
	}

	glob(s, &argv, &argc);	/* file name expansion	*/

	if (argc == 0) {	/* do nothing	*/
		(void) fprintf(stderr, "File: %s not found\n", ic->cmd.data);
		return;
	}

	if (argc > 1) {
		(void) fprintf(stderr, "Too many file names\n");
		return;
	}

	(void) sprintf(buf, "Saving to %s\n", argv[0]);
	(void) write (fd, buf, strlen(buf));

	/*
	 * check status of file to save to 
	 */
	status = CGM_validCGM(argv[0]);

	/*
	 * Does the file exist and if so is it a NCAR binary CGM
	 */
	if (status == -1 ) {	/* error?, file may not exist	*/
		if (errno == ENOENT) {
			type = 1;	/* file does NOT already exist	*/
		}
		else {
			perror ((char *) NULL);	/* error		*/
			return;
		}
	}
	else if (status == 0) {	/* file exists but is not a NCAR CGM	*/
		(void) sprintf(buf,"Non valid CGM, overwrite file? [y,n](y)");
		(void) write (fd, buf, strlen(buf));

		while (c = getchar()) if (isalpha(c)) break; 

		if (c == 'n' || c == 'N') {
			return;
		}
		else {
			type = 1;	/* write to file	*/
		}
		while ((c = getchar()) != '\n');
	} else  {		/* file exists and is a valid CGM	*/
		(void)sprintf(buf,"File exists, overwrite or append? [o,a](a)");
		(void) write (fd, buf, strlen(buf));

		while (c = getchar()) if (isalpha(c)) break; 

		if (c == 'o' || c == 'O') {
			type = 1;		/* write to file	*/
		}
		else {
			type = 0;		/* append to file	*/
		}
		while ((c = getchar()) != '\n');
	}

	/*
	 * If no frames specified use the current frame.
	 */
	if (ic->cmd.src_frames.num == 0) {	/* use current frame	*/
		ic->cmd.src_frames.fc[0].start_frame = ic->current_frame; 
		ic->cmd.src_frames.fc[0].num_frames = 1;
		ic->cmd.src_frames.num = 1;
	}

	/*
	 * if type is 1, then we do a write with the first batch of frames.
	 * Since the cgm_tools library function for writing frames has limited
	 * addressing we end up doing appends for all succeeding frames in 
	 * the command
	 */
	i = 0;
	if (type == 1) {	/* doing a write, not an append	*/

		start_frame = ic->cmd.src_frames.fc[i].start_frame - 1;
		num_frames = ic->cmd.src_frames.fc[i].num_frames;
		i++;

		if (CGM_writeFrames(argv[0], start_frame, num_frames) < 0) {
			(void) fprintf(stderr, "Error writing frames\n");
			if (errno) {
				perror((char *) NULL);
			}
			return;
		}
		
	}

	/*
	 * append the rest of the frames is there are any
	 */
	for ( ; i < ic->cmd.src_frames.num; i++) {
		start_frame = ic->cmd.src_frames.fc[i].start_frame - 1; 
		num_frames = ic->cmd.src_frames.fc[i].num_frames;
		
		if (CGM_appendFrames(argv[0], start_frame, num_frames) < 0) {
			(void) fprintf(stderr, "Error writing frames\n");
			if (errno) {
				perror((char *) NULL);
			}
			return;
		}
	}

		
	/*
	 * record the file name
	 */
	if (saveFile) cfree(saveFile);
	saveFile = icMalloc(strlen(argv[0]) + 1);
	(void) strcpy(saveFile, argv[0]);
	icState.save_file = saveFile;

}

int	iCNextfile(ic)
	ICommand	*ic;
{
	int	fd = ic->fd;

	if (! *icState.file) {
		(void) sprintf(buf, "No more files\n");
		(void) write (fd, buf, strlen(buf));
		return;
	}


	if (! *(icState.file + 1)) {
		(void) sprintf(buf, "No more files\n");
		(void) write (fd, buf, strlen(buf));
		return;
	}

	icState.file++;

	/*
	 * build a bogus ICommand and call iCFile() with it
	 */
	ic->cmd.data = *icState.file;
	iCFile(ic);
}

/* 
 *
 *	The current frame becomes the last frame plotted. Unless iCPlot
 *	is called with no frames specified than the current frame becomes
 *	the current frame + 1;
 */
int	iCPlot(ic)
	ICommand	*ic;
{
	int	i,j;
	int	frame, 		/* frame to be plotted		*/
		last_frame;	/* the last frame plotted	*/
	int	inc,	/* increment size for stepping through a frame list*/
		abs_inc;	/* absolute value of inc	*/
	int	num_frames;	/* number of frames in a list	*/
	int	count = 0;	/* number of frames plotted	*/
	int	fd = ic->fd;

	register void (*istat)();

	static	short	incCurrentFrame = FALSE;
	void	sigint_handler();

	if (!Toc) return;

	/*
	 * put the device in graphics mode for plotting
	 */
#ifndef	DEBUG
	GraphicsMode(TRUE);
#endif

	/*
	 * enable interupts to abort plotting
	 */
	istat = signal(SIGINT, sigint_handler);
	loopAbort = FALSE;
	last_frame = 0;

	inc	= icState.skip + 1;
	abs_inc	= inc;

	for (i = 0; i < ic->cmd.src_frames.num; i++) {

		num_frames = ic->cmd.src_frames.fc[i].num_frames;
		if (num_frames < 0) {
			inc *= -1;		/* make inc negative	*/
			num_frames *= -1;	/* abs(num_frames)	*/
		}
			
		for (j = 0, frame = ic->cmd.src_frames.fc[i].start_frame; 
			j < num_frames && !loopAbort;
			j += abs_inc, frame += inc) {

			if(plotit(frame)) {
				count++;
				last_frame = frame;
			}
		}
	}


	/*
	 * if the last group of frames in the frame list contained more
	 * then a single frame check and see if looping is requested
	 */
	i--;
	if (ic->cmd.src_frames.num != 0 && icState.loop &&
		(num_frames < -1 || num_frames > 1)) {

		while (! loopAbort) {	/* loop until SIGINT		*/

			frame = ic->cmd.src_frames.fc[i].start_frame % abs_inc;
			if (inc < 0) {
				frame = ((icState.stop_segment - frame)/abs_inc)
				* abs_inc + frame;

				for (; frame>icState.stop_segment; frame+=inc)
					;
			} 
			else {
				for (; frame<icState.start_segment; frame+=inc)
					;
			}

			num_frames = 
				icState.stop_segment - icState.start_segment +1;

			for (j = 0; j < num_frames && ! loopAbort; 
				j += abs_inc, frame += inc) {

				if(plotit(frame)) {
					count++;
					last_frame = i;
				}
			}
		}
	}


	/*
	 * if no frame specified assume current and increment current if 
	 * possible
	 */
	if (ic->cmd.src_frames.num == 0) { 

		/*
		 * if incCurrentFrame was set from last time than we
		 * increment the current frame. We wait to this so the user
		 * can "save", "print", etc. the frame he is presently viewing
		 */
		if (incCurrentFrame) {

			incCurrentFrame = FALSE;
			if (ic->current_frame < icState.stop_segment)
				ic->current_frame++;
		}

		frame = ic->current_frame;

		if (plotit(frame)) {
			count++;
			last_frame = frame;
			incCurrentFrame = TRUE;
		}

	}
	else {
		if (last_frame) ic->current_frame = last_frame;
	}

	/*
	 * put the device in text mode
	 */
#ifndef	DEBUG
	GraphicsMode(FALSE);
#endif


	(void) sprintf(buf, "Plotted %d frames, last frame plotted: %d\n",
							count, last_frame);
	(void) write (fd, buf, strlen(buf));

	/*
	 * restore interupts
	 */
	(void) signal(SIGINT, istat);
}

static	plotit(frame)
	int	frame;	
{
	int	record;
	int	i;

	if (frame < icState.start_segment || frame > icState.stop_segment) {

		return(0);
	}

	record = CGM_RECORD(Toc, frame - 1);

	/*
	 * call ctrans to plot the frame begining at record
	 */
	for (i = 0; i < icState.dup; i++) {
#ifndef	DEBUG
		(void) ctrans (record);
#else
		(void) fprintf(stderr, "plotted frame %d\n", frame);
		sleep(1);
#endif
	}

	if (icState.movie) {
		sleep((unsigned) icState.movietime);
	}
	else {	
		/* 
		 * if get newline continue 
		 */
		while (getchar() != '\n')
		; 
	}
	return(1);
}
static	void	sigint_handler()
{
	loopAbort = TRUE;
}



int	iCMerge(ic)
	ICommand	*ic;
{
	int	frame1, frame2;
	int	record1, record2;
	int	fd = ic->fd;
	if (!Toc) return;

	/*
	 * put the device in graphics mode for plotting
	 */
#ifndef	DEBUG
	GraphicsMode(TRUE);
#endif

	if (ic->cmd.src_frames.num != 2) {
		(void) fprintf(stderr,
			"<%s> usage: %s\n",ic->c->c_name,ic->c->c_usage);
		return;
	}

	if (ic->cmd.src_frames.fc[0].num_frames != 1 || 
		ic->cmd.src_frames.fc[1].num_frames != 1 ) {
		(void) fprintf(stderr,
			"<%s> usage: %s\n",ic->c->c_name,ic->c->c_usage);
		return;
	}
	frame1 = ic->cmd.src_frames.fc[0].start_frame;
	frame2 = ic->cmd.src_frames.fc[1].start_frame;

	record1 = CGM_RECORD(Toc, frame1 - 1);
	record2 = CGM_RECORD(Toc, frame2 - 1);

	(void) ctrans_merge(record1, record2);

	/*
	 * put the device in text mode
	 */
#ifndef	DEBUG
	GraphicsMode(FALSE);
#endif

	sprintf(buf,"Merged frames %d and %d\n", frame1, frame2); 
	(void) write (fd, buf, strlen(buf));

}


int	iCPrint(ic)
	ICommand	*ic;
{
	int	i,j;
	int	frame;
	int	count;
	char	*record;
	int	len = (sizeof (int) * 3) + 1;

	int	argc;
	char	**argv = NULL;
	char	*binpath;
	int	fd = ic->fd;
	static char	*translator = NULL;
	static char	*record_opt = NULL;

	if (!Toc) return;

	binpath = GetNCARGPath("BINDIR");
	binpath = binpath ? binpath : ".";

	/*
	 * one time creation of spooler translator name
	 */
	if (! translator) {
		translator = icMalloc(strlen(binpath) +
				      strlen(SPOOL_TRANS) + 2);

		(void) strcpy(translator, binpath);
		(void) strcat(translator, "/");
		(void) strcat (translator, SPOOL_TRANS); 

		record_opt = icMalloc (strlen ("-record") + 1);
		(void) strcpy (record_opt, "-record");
	}


	/*
	 * count how many frames we have so we can malloc enough memory
	 * correctly and easily
	 */
	for (i = 0, count = 0; i < ic->cmd.src_frames.num; i++) {
		count += ic->cmd.src_frames.fc[i].num_frames; 
	}

	argv = (char **) icMalloc ((count + 5) * sizeof (char *));

	argv[0] = translator;
	argv[1] = record_opt;
	argc = 2;

	/*
	 * build the arg list from the list of frames
	 */
	for (i = 0; i < ic->cmd.src_frames.num; i++) {
		for (j = 0, frame = ic->cmd.src_frames.fc[i].start_frame; 
			j < ic->cmd.src_frames.fc[i].num_frames; j++, frame++){

			record = icMalloc (len);
			(void) sprintf(record, "%d", 
					(int) CGM_RECORD(Toc, frame - 1));
			argv[argc++] = record;
		}
	} 



	/*
	 * if no frame specified assume current 
	 */
	if (ic->cmd.src_frames.num == 0) { 
		frame = ic->current_frame;
		record = icMalloc (len);
		(void) sprintf(record, "%d", (int) CGM_RECORD(Toc, frame - 1));
		argv[argc++] = record;
	}

	argv[argc++] = *icState.file;	/* terminate arg list	*/
	argv[argc] = NULL;	/* terminate arg list	*/

	/*
	 * spawn the translator and filter chain	
	 */
	(void) PipeLine(argc, argv, fd);

	/*
	 * free all memory except for translator, record_opt and file
	 */
	for (i = 2; i < (argc - 1); i++)
		cfree(argv[i]);
	cfree((char *) argv);
}


int	iCMovie(ic)
	ICommand	*ic;
{
	int	time = 0;
	int	fd = ic->fd;

	if (ic->cmd.data) {
		time = atoi(ic->cmd.data);
	}

	/*
	 * if a time was specified or if no time was specified and movie
	 * is not set than set movie, else turn off movie.
	 */
	if (time || !icState.movie) {
		icState.movie = TRUE;
		icState.movietime = time;
		(void) sprintf(buf, "movie on %d seconds\n", time);
		(void) write (fd, buf, strlen(buf));
	} else {
		icState.movie = FALSE;
		(void) sprintf(buf, "movie off\n");
		(void) write (fd, buf, strlen(buf));
	}
}


/*ARGSUSED*/
int	iCLoop(ic)
	ICommand	*ic;
{
	int	fd = ic->fd;
	/*
	 * toggle looping on or off
	 */
	if (icState.loop) {
		icState.loop = FALSE;
		(void) sprintf(buf, "loop off\n");
		(void) write (fd, buf, strlen(buf));
	} else {
		icState.loop = TRUE;
		(void) sprintf(buf, "loop on\n");
		(void) write (fd, buf, strlen(buf));
	}
}
int	iCDup(ic)
	ICommand	*ic;
{
	int	dup = 0;
	int	fd = ic->fd;

	if (ic->cmd.data) {
		dup = atoi(ic->cmd.data);
	}

	/*
	 * if dup not set (or its zero) report number of frames set for 
	 * dup. Else record number of frames for duping in future plots.
	 */
	if (dup) {
		icState.dup = dup;
		(void) sprintf(buf, "dup %d frames\n", dup);
		(void) write (fd, buf, strlen(buf));
	} else {
		(void) sprintf(buf, "%d\n", icState.dup);
		(void) write (fd, buf, strlen(buf));
	}
}

int	iCStartSegment(ic)
	ICommand	*ic;
{
	int	start = 0;
	int	fd = ic->fd;

	if (ic->cmd.src_frames.num != 0) {
		start = ic->cmd.src_frames.fc[0].start_frame; 
	}

	/*
	 * if start not set (or its zero) report first frame in segment
	 * Else set first frame in segment
	 */
	if (start) {

		if (start > icState.stop_segment || start < 1) {
			(void) fprintf(stderr, 
				"start segment cannot proceed stop segment\n");
		}
		else {
			icState.start_segment = start;
			(void)sprintf(buf,"segment begins at frame %d\n",start);
			(void) write (fd, buf, strlen(buf));

			if (start > ic->current_frame) {
				ic->current_frame = start;
			}
		}
	} else {
		(void) sprintf(buf, "%d\n", icState.start_segment);
		(void) write (fd, buf, strlen(buf));
	}
}

int	iCStopSegment(ic)
	ICommand	*ic;
{
	int	stop = 0;
	int	fd = ic->fd;

	if (ic->cmd.src_frames.num != 0) {
		stop = ic->cmd.src_frames.fc[0].start_frame; 
	}

	/*
	 * if stop not set (or its zero) report last frame in segment
	 * Else set last frame in segment
	 */
	if (stop) {

		if (stop < icState.start_segment || stop > icState.num_frames){
			(void) fprintf(stderr, 
				"stop segment cannont preceed start segment\n");
		}
		else {
			icState.stop_segment = stop;
			(void) sprintf(buf, "segment ends at frame %d \n",stop);
			(void) write (fd, buf, strlen(buf));

			if (stop < ic->current_frame) {
				ic->current_frame = stop;
			}
		}
	} else {
		(void) sprintf(buf, "%d\n", icState.stop_segment);
		(void) write (fd, buf, strlen(buf));
	}
}


int	iCSkip(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	int	fd = ic->fd;
	int	skip;

	if (s) {
		skip = atoi(s);
		icState.skip = skip;
		(void) sprintf(buf, "Skip %d frames\n", skip);
		(void) write (fd, buf, strlen(buf));
	}
	else {
		(void) sprintf(buf, "%d\n", icState.skip);
		(void) write (fd, buf, strlen(buf));
	}
}

int	iCDevice(ic)
	ICommand	*ic;
{
	static	char *deviceName = NULL;
	char	*dev;
	char	*s = ic->cmd.data;
	int	fd = ic->fd;

	if (!s) {	/* if no device report current device	*/
		if (icState.device) {
			(void) sprintf(buf, "%s\n", icState.device);
			(void) write (fd, buf, strlen(buf));
		}
		else {
			(void) sprintf(buf, "No device\n");
			(void) write (fd, buf, strlen(buf));
		}
		return;
	}

#ifdef	DEBUG
	(void) fprintf(stderr, "set device to: %s\n", s);
#else
	/*
	 * make sure device is valid. If valid and device uses a graphcap
	 * get complete path to graphcap.
	 */
	if ((dev = getGcapname( s )) == NULL ) {
		(void) fprintf(stderr, "Invalid device %s\n", s);
		return;
	}

	/*
	 * store device name and set it.
	 */
	if (SetDevice(dev) < 0) {
		(void) fprintf(stderr, "Invalid device %s\n", dev);
		return;
	}

	(void) init_metafile(0, CGM_FD(Toc));


	if (deviceName) cfree (deviceName);
	deviceName = icMalloc(strlen ( s ) + 1);
	(void) strcpy(deviceName, s);
	icState.device = deviceName;
#endif
}

int	iCFont(ic)
	ICommand	*ic;
{
	static	char *fontName;
	char	*font;
	char	*s = ic->cmd.data;
	int	fd = ic->fd;

	if (!s) {	/* if no font report current font	*/
		if (icState.font) {
			(void) sprintf(buf, "%s\n", icState.font);
			(void) write (fd, buf, strlen(buf));
		}
		else {
			(void) sprintf(buf, "No font\n");
			(void) write (fd, buf, strlen(buf));
		}
		return;
	}

#ifdef	DEBUG
	(void) fprintf(stderr, "set font to: %s\n", s);
#else
	/*
	 * make sure device is valid. If valid and device uses a graphcap
	 * get complete path to graphcap.
	 */
	if ((font = getFcapname( s )) == NULL ) {
		(void) fprintf(stderr, "Invalid font %s\n", s);
		return;
	}
	/*
	 * store font name and set it.
	 */

	if (SetFont(font) < 0) {
		(void) fprintf(stderr, "Invalid device %s\n", font);
	}

	if (fontName) cfree (fontName);
	fontName = icMalloc (strlen ( s ) + 1);
	(void) strcpy(fontName, s);
	icState.font = fontName;
#endif
}

/* 
 *
 *	The current frame becomes the last frame listed. Unless iCList
 *	is called with no frames specified than the current frame becomes
 *	the current frame + 1;
 */

int	iCList(ic)
	ICommand	*ic;
{
	int	i,j;
	int	frame;
	int	fd = ic->fd;

	static	short	incCurrentFrame = FALSE;

	if (! Toc)  return;

	for (i = 0; i < ic->cmd.src_frames.num; i++) {
		for (j = 0, frame = ic->cmd.src_frames.fc[i].start_frame; 
			j < ic->cmd.src_frames.fc[i].num_frames; j++, frame++){

			(void) sprintf(buf, 
				"\tFrame %d contains %d record(s) starting at record %d\n",
				frame, 
				CGM_NUM_RECORD(Toc, frame - 1), 
				CGM_RECORD(Toc, frame - 1));

			
			(void) write (fd, buf, strlen(buf));
			
		}

	}
	

	/*
	 * if no frame give assume the current frame
	 */
	if (ic->cmd.src_frames.num == 0) {

		/*
		 * if incCurrentFrame was set from last time than we
		 * increment the current frame. We wait to this so the user
		 * can "save", "print", etc. the frame he is presently viewing
		 */
		if (incCurrentFrame) {

			incCurrentFrame = FALSE;
			if (ic->current_frame < ic->last_frame)
				ic->current_frame++;
		}

		frame = ic->current_frame;

		(void) sprintf(buf, 
		"\tFrame %d contains %d record(s) starting at record %d\n",
			frame, CGM_NUM_RECORD(Toc, frame - 1), 
			CGM_RECORD(Toc, frame - 1));

		(void) write (fd, buf, strlen(buf));

		incCurrentFrame = TRUE;
	}
	else {
		frame--;
		ic->current_frame = frame;
	}
}

int	iCAlias(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	char	**aliases = NULL;
	int	fd = ic->fd;

	char	*cptr;
	
	extern	char	*GetCurrentAlias();
	extern	char	**GetSpoolers();

	/*
 	 * if a complete alias and a definition is given than assign the name
	 * to the definition. If only name is given than print its definition
	 * if neither name or definition is given than print all aliases
	 */
	if ( s ) {	
		/*
		 * see if definition is given
		 */
		for (cptr = s; *cptr && *cptr != ':'; cptr++)
		;
		if (*cptr == ':') {	/* definition is given	*/
			if (AddSpooler (s) > 0) {
				icState.spool_alias = GetCurrentAlias();
			}
		} 
		else {	/* get a specific alias		*/
			aliases = GetSpoolers( s );
		}
	}
	else {	/* get all the aliases	*/
		aliases = GetSpoolers ((char *) NULL);
	}

	
	/*
	 * print out the spooler aliases
	 */
	for ( ; aliases && *aliases; aliases++) {
		(void) sprintf(buf, "%s\n", *aliases);
		(void) write (fd, buf, strlen(buf));
	}
	
}

int	iCSpooler(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	int	fd = ic->fd;

	extern	char	*GetCurrentAlias();
	
	/*
	 * if a spooler alias is given than set it as the current spooler
	 * if no alias given report the current spooler
	 */
	if ( s ) {
		if (SetCurrentAlias ( s ) > 0 ) {
			icState.spool_alias = s;
		}
		else {
			(void) fprintf(stderr, "Invalid spooler: %s\n", s);
		}
	} 
	else {
		(void) sprintf(buf, "%s\n", GetCurrentAlias());
		(void) write (fd, buf, strlen(buf));
	}

}


/*
 * iCZoom: Change device window
 */
int	iCZoom(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	DevWindow	*dev_win = &icState.dev_window;
	int	fd = ic->fd;

	long	llx, lly, urx, ury;

	/*
	 * if new coordinates not given report current
	 */
	if (!s) {
		(void) sprintf(buf, 
			"llx = %f, lly = %f, urx = %f, ury = %f\n",
			dev_win->llx, dev_win->lly,
			dev_win->urx, dev_win->ury);

			
		(void) write (fd, buf, strlen(buf));
	
		return;
	}

	/*
	 * convert dev win coordinates from asci to ints
	 */
	(void) sscanf(s, "%f %f %f %f", &(dev_win->llx),
		&(dev_win->lly),
		&(dev_win->urx),
		&(dev_win->ury));

	llx = (long) (dev_win->llx * MAXX);
	lly = (long) (dev_win->lly * MAXY);
	urx = (long) (dev_win->urx * MAXX);
	ury = (long) (dev_win->ury * MAXY);
	/*
	 * change device window coordinate for future plots
	 */
	SetDevWin(llx, lly, urx, ury);

}

/*
 * iCShell: escape to the shell
 */
int	iCShell(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	int	fd = ic->fd;

	int	system();

	if (!s) return;

	/*
	 * echo the command to the screen
	 */
	(void) sprintf(buf, "! %s\n", s);
	(void) write (fd, buf, strlen(buf));

	/*
	 * call the shell with the command
	 */
	(void) system(s);
}

/*ARGSUSED*/
int	iCCount(ic)
	ICommand	*ic;
{
	int	fd = ic->fd;

	(void) sprintf(buf, "%d\n", icState.num_frames);
	(void) write (fd, buf, strlen(buf));
}

/*ARGSUSED*/
int	iCCurrent(ic)
	ICommand	*ic;
{
	int	fd = ic->fd;
	(void) sprintf(buf, "%d\n", ic->current_frame);
	(void) write (fd, buf, strlen(buf));
}

/*ARGSUSED*/
int	iCQuit(ic)
	ICommand	*ic;
{
	close_ctrans();
	CGM_termMetaEdit();
	exit(0);
}

/*ARGSUSED*/
int	Noop(ic)
	ICommand	*ic;
{
}

processMemoryCGM(ic, mem_file)
	ICommand	*ic;
	char	*mem_file;
{
	Directory	*toc;
	int		fd = ic->fd;

	if ((toc = CGM_initMetaEdit(mem_file, -1440, NULL)) == NULL) {
		perror((char *) NULL);
		return;
	}

	ic->current_frame = 1;
	ic->last_frame = CGM_NUM_FRAMES(toc);
	Toc = toc;

	
	(void) init_metafile(0, CGM_FD(toc));


	(void) sprintf(buf, "%d frames\n", CGM_NUM_FRAMES(toc));
	(void) write (fd, buf, strlen(buf));
	
}


#ifdef	DEAD
static	system(s)
char	*s;
{
	int	status;
	int	pid, w;
	register void (*istat)(), (*qstat)();

	extern int FORK(), execl(), wait();

	if((pid = FORK()) == 0) {
		(void) execl("/bin/csh", "sh", "-c", s, (char *)0);
		_exit(127);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	while((w = wait((union wait *) &status)) != pid && w != -1)
		;
	(void) signal(SIGINT, istat);
	(void) signal(SIGQUIT, qstat);
	return((w == -1)? w: status);
}
#endif
