/*
 *	$Id: commands.c,v 1.39 2008-07-27 03:18:45 haley Exp $
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
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#include <fcntl.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <errno.h>
#include <ncarg/c.h>
#include <ncarg/cgm_tools.h>
#include <ncarg/ctrans.h>
#include "ictrans.h"
#include "glob.h"

IcState	icState = {
		FALSE, 1, 1, 1, 0, 0, FALSE, 0, NULL, NULL, NULL, NULL, 
		NULL, NULL,
		{0.0, 0.0, 1.0, 1.0}
		};

static	Directory	*Toc;
static	short		loopAbort;
static	short		doMemFile = FALSE;
static	short		newFile = FALSE;


static	CtransRC	plotit(frame)
	int	frame;	
{
	int		record;
	int		i;
	CtransRC	ctrc;
	static	boolean	first = TRUE;

	if (frame < icState.start_segment || frame > icState.stop_segment) {
		return(OK);
	}

	record = CGM_RECORD(Toc, frame - 1);

	/*
	 * call ctrans to plot the frame begining at record
	 */
	for (i = 0; i < icState.dup; i++) {

		/*
		 * we clear the device before the start of a new frame (as
		 * opposed to immediately after completing the previous
		 * frame). However, we don't clear before drawing the 
		 * frist frame.
		 */
		if (! first) CtransClear();
		else first = FALSE;

		ctrc = ctrans(record);

		if (icState.movie) {
			if (icState.movietime) {
				 sleep((unsigned) icState.movietime);
			}
		}
		else {	
			/* 
			 * if get newline continue 
			 */
			while (getchar() != '\n')
			; 
		}
#ifdef	DEAD
		CtransClear();
#endif
	}
	return(ctrc);
}

static	void	sigint_handler(sig)
	int	sig;
{
	loopAbort = TRUE;
}

#ifdef	BOZO
static	int	system(s)
	const char	*s;
{
	int	status;
	int	pid, w;
	register void (*istat)(), (*qstat)();

	if((pid = fork()) == 0) {
		(void) execl("/bin/sh", "sh", "-c", s, (char *)0);
		_exit(127);
	}
	fprintf(stderr, "pid=%d\n", pid);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	while((w = wait(&status)) != pid && w != -1)
		;
	(void) signal(SIGINT, istat);
	(void) signal(SIGQUIT, qstat);
	return((w == -1)? w: status);
}
#endif

/*
**
**	I C T R A N S   C O M M A N D S
**
**
**	All ictrans commands a 1 on success, a -1 on failure, or a 0
**	to terminate all processing.
**
*/


int	iCHelp(ic)
	ICommand	*ic;
{

	int	i;
	CmdOp	*c;
	FILE	*fp = ic->fp;

	extern	int	NUM_CMDS;
	extern	CmdOp	cmdOptab[];
	extern	CmdOp	*getcmdOp();

	/*
	 * if user is not asking for help on a particular command print help
	 * for all commands
	 */
	if (!ic->cmd.data) {
		for (i = 0, c = &cmdOptab[0]; i < NUM_CMDS; i++, c++) {
			(void) fprintf (fp,"%-10s : %s\n",c->c_name,c->c_help);
		} 
			(void) fprintf(fp,
			"\nfor usage of a specific command type: help <command>\n");
		return(1);
	}

	/*
	 * user wants help on a single command. See if its a valid one
	 * and print a usage message
	 */
	if ((c = getcmdOp(ic->cmd.data)) == (CmdOp *) -1) {
		(void) fprintf(stderr,"Ambiguous command < %s >\n", ic->cmd.data);
		return(-1);
	}
	if (c == (CmdOp *) NULL) {
		(void) fprintf(stderr,"No such command < %s >\n", ic->cmd.data);
		return(-1);
	}

	/*
	 * print the useage message
	 */
	(void) fprintf(fp, "<%s> usage: %s\n", c->c_name, c->c_usage);
		
	return(1);
}

int	iCFile(ic)
	ICommand	*ic;
{

	char	*s = ic->cmd.data;
	FILE	*fp = ic->fp;
	static	char	*fileName = NULL;
	CtransRC	ctrc;

	int	argc;
	char	**argv;
	
	Directory	*toc;

	if (!s) {	/* if no file report current file	*/
		if (*icState.file) {
			(void) fprintf(fp, "%s\n", *icState.file);
		}
		else {
			(void) fprintf(fp, "No file\n");
		}
		return(1);
	}

	glob(s, &argv, &argc);

	if (argc == 0) {	/* do nothing	*/
		(void) fprintf(stderr, "File: %s not found\n", ic->cmd.data);
		return(-1);
	}

	if (argc > 1) {
		(void) fprintf(stderr, "Too many file names\n");
		return(-1);
	}

	if (! ic->quiet) (void) fprintf(fp, "%s\n", argv[0]);

		

	toc = CGM_initMetaEdit(argv[0],1440,NULL, ic->quiet ? (FILE *) NULL : fp);
	if (! toc) {
		perror((char *) NULL);
		return(-1);
	}

	ic->current_frame = 1;
	ic->last_frame = CGM_NUM_FRAMES(toc);
	Toc = toc;

	icState.num_frames = CGM_NUM_FRAMES(toc);
	icState.stop_segment = CGM_NUM_FRAMES(toc);

	
	newFile = TRUE;


	/*
	 * record the file name
	 */
	if (fileName) free((Voidptr) fileName);
	if ( !(fileName = malloc((unsigned) (strlen(argv[0]) + 1)))) {
		perror("malloc");
		return(-1);
	}
	(void) strcpy(fileName, argv[0]);
	*icState.file = fileName;

	if (! ic->quiet) {
		(void) fprintf(fp, "%d frames\n", CGM_NUM_FRAMES(toc));
	}
	doMemFile = FALSE;
	
	return(1);
}

int	iCSave(ic)
	ICommand	*ic;
{

	char	*s = ic->cmd.data;		/* file to save to	*/
	FILE	*fp = ic->fp;
	static	char	*saveFile = NULL;	/* save name of file	*/

	int	start_frame, num_frames;
	int	status;	
	int	type;	/* type of write, 1 => write, 0 => append	*/
	int	i;
	int	c;
	boolean	force = FALSE;			/* force the save?	*/

	int	argc;
	char	**argv;

	if (ic->cmd.name[0] == 'S') force = TRUE;
	if (ic->quiet) force = TRUE;
		
	

	/*
	 * see if file given. if not use last saved file if it exists
	 */
	if (!s) {
		if (!(s = icState.save_file)) {
			(void) fprintf(stderr, "No current save file\n");
			return(-1);
		}
	}

	glob(s, &argv, &argc);	/* file name expansion	*/

	if (argc == 0) {	/* do nothing	*/
		(void) fprintf(stderr, "File: %s not found\n", ic->cmd.data);
		return(-1);
	}

	if (argc > 1) {
		(void) fprintf(stderr, "Too many file names\n");
		return(-1);
	}

	if (! ic->quiet) (void) fprintf(fp, "Saving to %s\n", argv[0]);

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
			return(-1);
		}
	}
	else if (status == 0) {	/* file exists but is not a NCAR CGM	*/
		if (force) { 
			type = 1;
		}
		else {
			(void) fprintf(
				fp,"Non valid CGM, overwrite file? [y,n](y)"
			);

			while (c = getchar()) {
				if (isalpha(c) || c == '\n') break; 
			}

			if (c == 'n' || c == 'N') {
				return(1);
			}
			else {
				type = 1;	/* write to file	*/
			}
			while (c != '\n') c = getchar();
		}
	} else  {		/* file exists and is a valid CGM	*/
		if (force) {
			type = 0;
		}
		else {
			(void)fprintf(
				fp,"File exists, overwrite or append? [o,a](a)"
			);

			while (c = getchar()) {
				if (isalpha(c) || c == '\n') break; 
			}

			if (c == 'o' || c == 'O') {
				type = 1;	/* write to file	*/
			}
			else {
				type = 0;	/* append to file	*/
			}
			while (c != '\n') c = getchar();
		}
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
			return(-1);
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
			return(-1);
		}
	}

		
	/*
	 * record the file name
	 */
	if (saveFile) free((Voidptr) saveFile);
	if ( !(saveFile = malloc((unsigned) (strlen(argv[0]) + 1)))) {
		perror("malloc()");
		return(-1);
	}
	(void) strcpy(saveFile, argv[0]);
	icState.save_file = saveFile;

	return(1);
}

int	iCNextfile(ic)
	ICommand	*ic;
{
	FILE	*fp = ic->fp;

	if (! *icState.file) {
		(void) fprintf(fp, "No more files\n");
		return(1);
	}


	if (! *(icState.file + 1)) {
		(void) fprintf(fp, "No more files\n");
		return(1);
	}

	icState.file++;

	/*
	 * build a bogus ICommand and call iCFile() with it
	 */
	ic->cmd.data = *icState.file;

	return (iCFile(ic));
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
	FILE	*fp = ic->fp;

	register void (*istat)();

	static		short	incCurrentFrame = FALSE;
	CtransRC	ctrc;
	CtransRC	status = OK;

	if (!Toc) return(-1);

	if (newFile) {
		ctrc = init_metafile(0, CGM_FD(Toc));
		if (ctrc == FATAL) {
			(void) CGM_termMetaEdit();
			log_ct(FATAL);
			Toc = NULL;
			return(-1);
		}
		else if (ctrc == WARN) {
			log_ct(WARN);
			return(-1);
		}
		newFile = FALSE;
	}

	/*
	 * put the device in graphics mode for plotting
	 */
#ifndef	DEBUG
	GraphicsMode(TRUE);
#endif

	/*
	 * enable interupts to abort plotting
	 */
	istat = signal(SIGINT, &sigint_handler);
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

			ctrc = plotit(frame);
			if (ctrc == FATAL) {
				(void) CGM_termMetaEdit();
				GraphicsMode(FALSE);
				log_ct(FATAL);
				Toc = NULL;
				return(-1);
			}
			else if (ctrc == WARN) {
				status = WARN;
			}
			count++;
			last_frame = frame;
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

				ctrc = plotit(frame);
				if (ctrc == FATAL) {
					(void) CGM_termMetaEdit();
					GraphicsMode(FALSE);
					log_ct(FATAL);
					Toc = NULL;
					return(-1);
				}
				else if (ctrc == WARN) {
					status = WARN;
				}
				count++;
				last_frame = i;
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

		ctrc = plotit(frame);
		if (ctrc == FATAL) {
			(void) CGM_termMetaEdit();
			GraphicsMode(FALSE);
			log_ct(FATAL);
			Toc = NULL;
			return(-1);
		}
		else if (ctrc == WARN) {
			status = WARN;
		}
		count++;
		last_frame = frame;
		incCurrentFrame = TRUE;

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
	/* 
	 * report errors *after* turning off graphics mode
	 */
	if (status == WARN) {	
		log_ct(WARN);
	}

	if (! ic->quiet) {
		(void) fprintf(
			fp, "Plotted %d frames, last frame plotted: %d\n",
			count, last_frame
		);
	}

	/*
	 * restore interupts
	 */
	(void) signal(SIGINT, istat);
	return(1);
}

int	iCMerge(ic)
	ICommand	*ic;
{
	int	frame1, frame2;
	int	record1, record2;
	FILE	*fp = ic->fp;
	CtransRC	ctrc;

	if (!Toc) return(-1);

	if (newFile) {
		ctrc = init_metafile(0, CGM_FD(Toc));
		if (ctrc == FATAL) {
			(void) CGM_termMetaEdit();
			log_ct(FATAL);
			Toc = NULL;
			return(-1);
		}
		else if (ctrc == WARN) {
			log_ct(WARN);
			return(-1);
		}
		newFile = FALSE;
	}
	/*
	 * put the device in graphics mode for plotting
	 */
#ifndef	DEBUG
	GraphicsMode(TRUE);
#endif

	if (ic->cmd.src_frames.num != 2) {
		(void) fprintf(stderr,
			"<%s> usage: %s\n",ic->c->c_name,ic->c->c_usage);
		return(-1);
	}

	if (ic->cmd.src_frames.fc[0].num_frames != 1 || 
		ic->cmd.src_frames.fc[1].num_frames != 1 ) {
		(void) fprintf(stderr,
			"<%s> usage: %s\n",ic->c->c_name,ic->c->c_usage);
		return(-1);
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

	if (! ic->quiet) fprintf(fp,"Merged frames %d and %d\n", frame1,frame2);

	return(1);
}

static	print_file(ic, translator, dev_win_string)
	ICommand	*ic;
	char		*translator;
	char		*dev_win_string;
{
	int	i,j;
	int	frame;
	int	count;
	char	*record;
	int	step;
	int	len = (sizeof (int) * 3) + 1;

	int	argc;
	char	**argv = NULL;
	FILE	*fp = ic->fp;
	static char	*record_opt = "-record";
	static char	*window_opt = "-window";



	/*
	 * count how many frames we have so we can malloc enough memory
	 * correctly and easily
	 */
	for (i = 0, count = 0; i < ic->cmd.src_frames.num; i++) {
		count += ABS(ic->cmd.src_frames.fc[i].num_frames);
	}

	if ( !(argv = (char **) malloc ((count + 9) * sizeof (char *)))) {
		perror("malloc()");
		return(-1);
	}


	argc = 0;
	argv[argc++] = translator;
	argv[argc++] = window_opt;
	argv[argc++] = dev_win_string;
	argv[argc++] = record_opt;
	if (icState.palette_file) {
		argv[argc++] = "-pal";
		argv[argc++] = icState.palette_file;
	}

	/*
	 * build the arg list from the list of frames
	 */
	for (i = 0; i < ic->cmd.src_frames.num; i++) {
		step = ic->cmd.src_frames.fc[i].num_frames < 0 ? -1 : 1;
		for (j = 0, frame = ic->cmd.src_frames.fc[i].start_frame; 
			j < ABS(ic->cmd.src_frames.fc[i].num_frames); 
			j++, frame+=step) {

			if (! (record = malloc ((unsigned) len))) {
				perror("malloc()");
				return(-1);
			}
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
		if (! (record = malloc ((unsigned) len))) {
			perror("malloc()");
			return(-1);
		}
		(void) sprintf(record, "%d", (int) CGM_RECORD(Toc, frame - 1));
		argv[argc++] = record;
	}

	argv[argc++] = *icState.file;	/* terminate arg list	*/
	argv[argc] = NULL;	/* terminate arg list	*/

	/*
	 * spawn the translator and filter chain	
	 */
	(void) PipeLine(argc, argv, ic->quiet ? (FILE *) NULL : fp);

	free((Voidptr) record);
	free((Voidptr) argv);
	return(1);
}

int	iCPrint(ic)
	ICommand	*ic;
{
	const char	*binpath;
	static	char	*translator = NULL;
	DevWindow	*dev_win = &icState.dev_window;
	char		dev_win_string[80];

	if (!Toc) return(-1);


	/*
	 * one time creation of spooler translator name
	 */
	if (! translator) {
		if ( !(binpath = GetNCARGPath(BINDIR))) {
			fprintf(
				stderr, "NCARG bin path not found [ %s ]\n", 
				ErrGetMsg()
			);
			return(-1);
		}
		translator = malloc(
			(unsigned) (strlen(binpath) + strlen(SPOOL_TRANS) + 2)
		);
		if (! translator) {
			perror("malloc()");
			return(-1);
		}

		(void) strcpy(translator, binpath);
		(void) strcat(translator, "/");
		(void) strcat (translator, SPOOL_TRANS); 

	}

	(void) sprintf(
		dev_win_string, "%f:%f:%f:%f", 
		dev_win->llx, dev_win->lly, dev_win->urx, dev_win->ury
	);

	if (! doMemFile) {
		(void) print_file(ic, translator, dev_win_string);
	}
	else {
		(void) print_mem_file(ic, translator, dev_win_string);
	}

	return(1);
}

print_mem_file(ic, translator, dev_win_string)
	ICommand	*ic;
	char		*translator;
	char		*dev_win_string;
{
	char		*tmpfile, *create_tmp_fname();
	int		argc = 0;
	char		*argv[7];
	FILE		*fp = ic->fp;
	time_t		access_time;
	struct stat	stat_buf;
	int		sanity = 60;
	static	char	*window_opt = "-window";


	tmpfile = create_tmp_fname();

	ic->cmd.data = tmpfile;

	/*
	 * save the frames to be printed out to disk
	 */
	(void) iCSave(ic);

	if (stat(tmpfile, &stat_buf) < 0) {
		perror("ictrans");
		return(-1);
	} 
	access_time = stat_buf.st_atime;

	/*
	 * wait to ensure that when spooler accesses tmpfile it will have a 
	 * different access time
	 */
	sleep(2);

	argv[argc++] = translator;
	argv[argc++] = window_opt;
	argv[argc++] = dev_win_string;
	argv[argc++] = tmpfile;
	argv[argc] = NULL;


	/*
	 * spawn the translator to process the tmp file
	 */
	(void) PipeLine(argc, argv, ic->quiet ? (FILE *) NULL : fp);

	/*
	 * wait until spooler starts reading tmpfile before we unlink
	 * it. tmpfile won't actually be removed until translator closes
	 * it.
	 */
	while (sanity-- > 0) {
		if (stat(tmpfile, &stat_buf) < 0) {
			perror("ictrans");
			return(-1);
		} 
		if (access_time != stat_buf.st_atime) break;
		sleep(1);
	}
		
	(void) unlink(tmpfile);
	free((Voidptr) tmpfile);
	return(1);
}



int	iCMovie(ic)
	ICommand	*ic;
{
	int	time = 0;
	FILE	*fp = ic->fp;

	if (ic->cmd.data) {
		time = atoi(ic->cmd.data);
	}

	/*
	 * if a time was specified or if no time was specified and movie
	 * is not set than set movie, else turn off movie.
	 */
	if (ic->cmd.data || !icState.movie) {
		icState.movie = TRUE;
		icState.movietime = time;
		if (! ic->quiet) (void)fprintf(fp,"movie on %d seconds\n",time);
	} else {
		icState.movie = FALSE;
		if (! ic->quiet) (void) fprintf(fp, "movie off\n");
	}
	return(1);
}


/*ARGSUSED*/
int	iCLoop(ic)
	ICommand	*ic;
{
	FILE	*fp = ic->fp;
	/*
	 * toggle looping on or off
	 */
	if (icState.loop) {
		icState.loop = FALSE;
		if (! ic->quiet) (void) fprintf(fp, "loop off\n");
	} else {
		icState.loop = TRUE;
		if (! ic->quiet) (void) fprintf(fp, "loop on\n");
	}
	return(1);
}
int	iCDup(ic)
	ICommand	*ic;
{
	int	dup = 0;
	FILE	*fp = ic->fp;

	if (ic->cmd.data) {
		dup = atoi(ic->cmd.data);
	}

	/*
	 * if dup not set (or its zero) report number of frames set for 
	 * dup. Else record number of frames for duping in future plots.
	 */
	if (dup) {
		icState.dup = dup;
		if (! ic->quiet) (void) fprintf(fp, "dup %d frames\n", dup);
	} else {
		if (! ic->quiet) (void) fprintf(fp, "%d\n", icState.dup);
	}
	return(1);
}

int	iCStartSegment(ic)
	ICommand	*ic;
{
	int	start = 0;
	FILE	*fp = ic->fp;

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
			if (! ic->quiet) {
				(void) fprintf(
					fp,"segment begins at frame %d\n",start
				);
			}

			if (start > ic->current_frame) {
				ic->current_frame = start;
			}
		}
	} else {
		(void) fprintf(fp, "%d\n", icState.start_segment);
	}
	return(1);
}

int	iCStopSegment(ic)
	ICommand	*ic;
{
	int	stop = 0;
	FILE	*fp = ic->fp;

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
			if (! ic->quiet) {
				(void) fprintf(
					fp, "segment ends at frame %d \n",stop
				);
			}

			if (stop < ic->current_frame) {
				ic->current_frame = stop;
			}
		}
	} else {
		(void) fprintf(fp, "%d\n", icState.stop_segment);
	}
	return(1);
}


int	iCSkip(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	FILE	*fp = ic->fp;
	int	skip;

	if (s) {
		skip = atoi(s);
		icState.skip = skip;
		if (! ic->quiet) (void) fprintf(fp, "Skip %d frames\n", skip);
	}
	else {
		(void) fprintf(fp, "%d\n", icState.skip);
	}
	return(1);
}

int	iCDevice(ic)
	ICommand	*ic;
{
	static	char *deviceName = NULL;
	char	*dev;
	char	*s = ic->cmd.data;
	FILE	*fp = ic->fp;
	CtransRC	ctrc;

	if (!s) {	/* if no device report current device	*/
		if (icState.device) {
			(void) fprintf(fp, "%s\n", icState.device);
		}
		else {
			(void) fprintf(fp, "No device\n");
		}
		return(1);
	}

#ifdef	DEBUG
	(void) fprintf(stderr, "set device to: %s\n", s);
#else
	/*
	 * make sure device is valid. If valid and device uses a graphcap
	 * get complete path to graphcap.
	 */
	if ((dev = getGcapname( s )) == NULL ) {
		(void) fprintf(
			stderr, "Invalid device(%s) [ %s ]\n",
			s, ErrGetMsg()
		);
		return(-1);
	}

	/*
	 * store device name and set it.
	 */
	if ((int) SetDevice(dev) < 0) {
		(void) fprintf(stderr, "Invalid device %s\n", dev);
		return(-1);
	}

	newFile = TRUE;

	if (deviceName) free ((Voidptr) deviceName);
	if ( !(deviceName = malloc((unsigned) (strlen ( s ) + 1)))) {
		perror("malloc()");
		return(-1);
	}
	(void) strcpy(deviceName, s);
	icState.device = deviceName;
#endif
	return(1);
}

int	iCFont(ic)
	ICommand	*ic;
{
	static	char *fontName;
	char	*font;
	char	*s = ic->cmd.data;
	FILE	*fp = ic->fp;

	if (!s) {	/* if no font report current font	*/
		if (icState.font) {
			(void) fprintf(fp, "%s\n", icState.font);
		}
		else {
			(void) fprintf(fp, "No font\n");
		}
		return(1);
	}

#ifdef	DEBUG
	(void) fprintf(stderr, "set font to: %s\n", s);
#else
	/*
	 * make sure device is valid. If valid and device uses a graphcap
	 * get complete path to graphcap.
	 */
	if ((font = getFcapname( s )) == NULL ) {
		(void) fprintf(
			stderr, "Can't initialize font(%s) [ %s ]\n",
			s, ErrGetMsg()
		);
		return(-1);
	}
	/*
	 * store font name and set it.
	 */

	if ((int) SetFont(font) < 0) {
		(void) fprintf(stderr, "Invalid device %s\n", font);
	}

	if (fontName) free ((Voidptr) fontName);
	if ( !(fontName = malloc ((unsigned) (strlen ( s ) + 1)))) {
		perror("malloc()");
		return(-1);
	}
	(void) strcpy(fontName, s);
	icState.font = fontName;
#endif
	return(1);
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
	FILE	*fp = ic->fp;

	static	short	incCurrentFrame = FALSE;

	if (! Toc)  return(-1);

	for (i = 0; i < ic->cmd.src_frames.num; i++) {
		for (j = 0, frame = ic->cmd.src_frames.fc[i].start_frame; 
			j < ic->cmd.src_frames.fc[i].num_frames; j++, frame++){

			(void) fprintf(fp, 
				"\tFrame %d contains %d record(s) starting at record %d\n",
				frame, 
				CGM_NUM_RECORD(Toc, frame - 1), 
				CGM_RECORD(Toc, frame - 1));

			
			
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

		(void) fprintf(fp, 
		"\tFrame %d contains %d record(s) starting at record %d\n",
			frame, CGM_NUM_RECORD(Toc, frame - 1), 
			CGM_RECORD(Toc, frame - 1));


		incCurrentFrame = TRUE;
	}
	else {
		frame--;
		ic->current_frame = frame;
	}
	return(1);
}

int	iCAlias(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	char	**aliases = NULL;
	FILE	*fp = ic->fp;

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
		(void) fprintf(fp, "%s\n", *aliases);
	}
	
	return(1);
}

int	iCSpooler(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	FILE	*fp = ic->fp;

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
		(void) fprintf(fp, "%s\n", GetCurrentAlias());
	}

	return(1);
}


/*
 * iCZoom: Change device window
 */
int	iCZoom(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	DevWindow	*dev_win = &icState.dev_window;
	FILE	*fp = ic->fp;

	long	llx, lly, urx, ury;

	/*
	 * if new coordinates not given report current
	 */
	if (!s) {
		(void) fprintf(fp, 
			"llx = %f, lly = %f, urx = %f, ury = %f\n",
			dev_win->llx, dev_win->lly,
			dev_win->urx, dev_win->ury);

			
	
		return(1);
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

	return(1);
}

/*
 * iCShell: escape to the shell
 */
int	iCShell(ic)
	ICommand	*ic;
{
	char	*s = ic->cmd.data;
	FILE	*fp = ic->fp;

	if (!s) return(-1);

	/*
	 * echo the command to the screen
	 */
	if (! ic->quiet) (void) fprintf(fp, "! %s\n", s);

	/*
	 * call the shell with the command
	 */
	(void) system(s);
	return(1);
}

/*ARGSUSED*/
int	iCCount(ic)
	ICommand	*ic;
{
	FILE	*fp = ic->fp;

	(void) fprintf(fp, "%d\n", icState.num_frames);
	return(1);
}

/*ARGSUSED*/
int	iCCurrent(ic)
	ICommand	*ic;
{
	FILE	*fp = ic->fp;

	(void) fprintf(fp, "%d\n", ic->current_frame);
	return(1);
}

/*ARGSUSED*/
int	iCQuit(ic)
	ICommand	*ic;
{
	CtransClear();
	(void) close_metafile();
	(void) CGM_termMetaEdit();
	return(0);	/* exit	*/
}

/*ARGSUSED*/
int	Noop(ic)
	ICommand	*ic;
{
	return(1);
}

processMemoryCGM(ic, mem_file)
	ICommand	*ic;
	char	*mem_file;
{
	Directory	*toc;
	FILE		*fp = ic->fp;
	CtransRC	ctrc;

	toc = CGM_initMetaEdit(
		mem_file, -1440, NULL, ic->quiet ? (FILE *) NULL : fp
	);
	if (! toc) {
		perror((char *) NULL);
		return(-1);
	}

	ic->current_frame = 1;
	ic->last_frame = CGM_NUM_FRAMES(toc);
	Toc = toc;

	icState.num_frames = CGM_NUM_FRAMES(toc);
	icState.stop_segment = CGM_NUM_FRAMES(toc);

	
	newFile = TRUE;


	if (! ic->quiet) (void) fprintf(fp, "%d frames\n", CGM_NUM_FRAMES(toc));
	doMemFile = TRUE;
	return(1);	
}

char	*create_tmp_fname()
{
	char	*fname;
	char	pidbuf[20];
	const char	*tmp_path = GetNCARGPath(NGTMPDIR);

	(void) sprintf(pidbuf, "ictrans.%d", getpid());

	fname = malloc(
		(unsigned) (strlen(tmp_path) + strlen("/") + strlen(pidbuf) + 1)
	);
	(void) strcpy(fname, tmp_path);
	fname = strcat(fname, "/");
	fname = strcat(fname, pidbuf);

	return(fname);
}


