/*
 *	$Id: commands.c,v 1.16 2008-07-27 03:18:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/


/*
 *	commands.c
 *
 *	Author		John Clyne
 *
 *	Date		Fri Jan 26 16:43:39 MST 1990
 *
 *	This file contains the user functions accessed via the command table
 *	These functions make the appropriate calls the the cgm_tools library
 *	with the appropriate data. Note: the first frame in a file addressed
 *	by cgm_tools is frame 0. The first frame in med is frame 1;
 *
 *	REVISIONS
 *
 *	11/14/90	Added 'split' function
 */
#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<ctype.h>
#include	<ncarg/cgm_tools.h>
#include	"med.h"

#ifndef	DEBUG
/* 
 * pointer to cgm_tools table of contents	
 */
static	Directory	*toc = NULL;
#endif

static	short	dirty = FALSE;
static	char	editFile[80];	/* name of file being edited	*/

static	Instr	e_instr;	/* used by medLabel		*/

extern	char	*localTmp;	/* a local tmp directory	*/

/*
 * a macro to supply default frame numbers when none is given
 */
#define	DEFAULT_FRAME(default, cdptr)  (cdptr)->add1 = (cdptr)->add1 == -1 ? \
		(default) : (cdptr)->add1; \
		(cdptr)->add2 = (cdptr)->add2 == -1 ? \
		(cdptr)->add1 : (cdptr)->add2; \
		(cdptr)->add3 = (cdptr)->add3 == -1 ? \
		(default) : (cdptr)->add3;

#define	MIN(A,B)	((A) < (B) ? (A) : (B))

/*
 * medCopy: copy 1 or more frames to a new location in the file buffer
 */
void	medCopy(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;
	Directory	*tmp_toc;

	unsigned int	start_frame,
			target;
	int		num_frames;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address [, address] ] copy [address]
	 */
	if ((c_data->add2 != -1 && c_data->add1 == -1) ||
		c_data->file != NULL) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for copy. use current frame for default
	 */
	DEFAULT_FRAME(med_data->current_frame, c_data);

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	

	start_frame = c_data->add1;
	num_frames = c_data->add2 - start_frame + 1;
	target = c_data->add3;

#ifdef	DEBUG
	(void) fprintf(stderr, "copy frames %d + %d to %d\n",
		start_frame, num_frames, target);
#else

	/*
	 * execute the command
	 */
	if ((tmp_toc = CGM_copyFrames(start_frame - 1, 
					num_frames, target )) == NULL) {

		command_error_message (
				med_data, errno, 0, (char * ) NULL, MED_WARN);
		return;
	}

	toc = tmp_toc;
#endif

	/*
	 * update the current and last frame
	 */
	med_data->current_frame = target + num_frames;
	med_data->last_frame += num_frames;

	dirty = TRUE;	/* buffer has been modified	*/
}

/*
 * medDelete: delete 1 or more frames in the file buffer
 */
void	medDelete(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;
	Directory	*tmp_toc;

	unsigned	start_frame,
			num_frames;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address [, address] ] delete 
	 */
	if ((c_data->add2 != -1 && c_data->add1 == -1) ||
		c_data->add3 != -1 || c_data->file != NULL) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for delete. use current frame for default
	 */
	DEFAULT_FRAME(med_data->current_frame, c_data);

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	

	start_frame = c_data->add1;
	num_frames = c_data->add2 - start_frame + 1;

#ifdef	DEBUG
	(void) 
	fprintf(stderr, "delete frames %d + %d \n", start_frame, num_frames);
#else
	/*
	 * execute the command
	 */
	if ((tmp_toc = CGM_deleteFrames(start_frame - 1, num_frames)) == NULL) {
		command_error_message (
			med_data, errno, 0, (char *) NULL, MED_WARN);
		return;
	}

	toc = tmp_toc;
#endif

	med_data->last_frame -= num_frames;
	med_data->current_frame = MIN(start_frame, med_data->last_frame);

	dirty = TRUE;	/* buffer has been modified	*/
}

/*
 * medEdit: edit a new file
 */
void	medEdit(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/

	/*
	 * check to see if command in valid format:
	 * edit [ ! ] [file name]
	 */
	if (c_data->add1 != -1 || c_data->add2 != -1 || c_data->add3 != -1 ) {

		command_error_message (
				med_data, errno, 1, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * look for a '!' to overide the dirty flag. This is a kludge 
	 * and should really be done by the parser but its not.
	 */
	if (c_data->file && *c_data->file == '!') {
		dirty = FALSE;
		while (isspace(*(++c_data->file)))
		;

		if (*c_data->file == '\0')
			c_data->file = NULL;
	}

	/*
	 * don't edit new file if dirty flag set and no '!' given
	 */
	if (dirty) { /* if dirty (and not edit!)	*/
		command_error_message(med_data, errno, 0, 
			"No write since last change (edit! overides)", 
			MED_WARN);
		return;
	}

	/*
	 * if no file specified then reopen original file
	 */
	c_data->file = c_data->file == NULL ? editFile : c_data->file;

#ifdef	DEBUG
	(void) fprintf(stderr, "edit file %s \n", c_data->file);
#else
	/*
	 * execute the command
	 */
	if ((toc = CGM_initMetaEdit(c_data->file,1440,localTmp,
		med_data->fromatty ? stderr : (FILE *) NULL)) == NULL) {
		command_error_message (
				med_data, errno, 0, (char *) NULL, MED_WARN);
		return;
	}

	med_data->current_frame = med_data->last_frame = toc->num_frames;


#endif

	dirty = FALSE;	/* buffer has been cleared	*/
	(void) strcpy(editFile, c_data->file);

}


/*
 * medLabel: give a name to 1 or more frames in the file buffer
 */
void	medLabel(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;
	Directory	*tmp_toc;

	int		i;

	unsigned	start_frame;
	int		num_frames;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address [, address] ] label <string>
	 */
	if ((c_data->add2 != -1 && c_data->add1 == -1) ||
		c_data->add3 != -1 || c_data->file == NULL) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for label. use current frame for default
	 */
	DEFAULT_FRAME(med_data->current_frame, c_data);

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	

	start_frame = c_data->add1;
	num_frames = c_data->add2 - start_frame + 1;

#ifdef	DEBUG
	(void) 
	fprintf(stderr, "label frames %d + %d \n", start_frame, num_frames);
#else
	/*
	 * encode the instr to be placed in the CGM
	 * See CGM standard in regard to string data encoding
	 */
	(void) strcpy((char *) &e_instr.buf[1], c_data->file);
	e_instr.cgmclass = DEL_ELEMENT;
	e_instr.id = BEG_PIC_ID;
	e_instr.buf[0] = (unsigned char) strlen(c_data->file);
	e_instr.data_length = strlen(c_data->file) + 1;
	e_instr.data = e_instr.buf;

	for (i = start_frame; i < start_frame + num_frames; i++) {
		if ((tmp_toc = CGM_editFrame(i-1,&e_instr,1)) == NULL) {
			command_error_message (med_data, errno, 
				0, (char *) NULL, MED_WARN);
			return;
		}
	}

	toc = tmp_toc;

#endif

	dirty = TRUE;	/* buffer has been modified	*/
}

/*
 * medHelp: give help on a command
 */
void	medHelp(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;
	int	i;
	

	extern	int	NUM_CMDS;
	extern	Cmd	cmdtab[];

	Cmd	*c;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * label [ string>
	 */
	if ((c_data->add1 != -1 || c_data->add2 != -1) || c_data->add3 != -1) { 

		command_error_message (
			med_data, errno, 1, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * if user is not asking for help on a particular command print help
	 * for all commands
	 */
	if (!c_data->file) {
		for (i = 0, c = &cmdtab[0]; i < NUM_CMDS; i++, c++) {
			(void) fprintf (
				stderr,"%-10s : %s\n", c->c_name, c->c_help);
		} 
		if (med_data->fp) {	/* if not from a tty	*/
			(void) fprintf(stderr,
			"\nfor usage of a specific command type: help <command>\n");
		}
		return;
	}

	/*
	 * user wants help on a single command. See if its a valid one
	 * and print a usage message
	 */
	if ((c = getcmd(c_data->file)) == (Cmd *) -1) {
		(void) fprintf (
			stderr, "med: Ambiguous command < %s >", c_data->file);
		return;
	}
	if (c == (Cmd *) NULL) {
		(void) fprintf (
			stderr, "med: No such command < %s >\n", c_data->file);
		return;
	}

	/*
	 * print the useage message
	 */
	(void) fprintf(stderr, "med: <%s> usage: %s\n", c->c_name, c->c_usage);
		

}

/*
 * medMerge: merge two or more frames on top of each other in the buffer
 */
void	medMerge(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;
	Directory	*tmp_toc;

	unsigned	bottom_frame,
			top_frame;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * < address , address >  merge 
	 */
	if ((c_data->add1 == -1 || c_data->add2 == -1) ||
		c_data->add3 != -1 || c_data->file != NULL) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for merge. use current frame for default
	 */
	DEFAULT_FRAME(med_data->current_frame, c_data);

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	

	bottom_frame = c_data->add1;
	top_frame = c_data->add2;

#ifdef	DEBUG
	(void)
	fprintf(stderr, "merge frame %d on to %d \n", top_frame, bottom_frame);
#else
	/*
	 * execute the command
	 */
	if ((tmp_toc = CGM_mergeFrames(bottom_frame-1, top_frame-1)) == NULL) {
		command_error_message (
			med_data, errno, 0, (char *) NULL, MED_WARN);
		return;
	}

	toc = tmp_toc;
#endif

	med_data->current_frame = bottom_frame;

	dirty = TRUE;	/* buffer has been modified	*/
}

/*
 * medMove: move 1 or more frames to a new location in the file buffer
 */
void	medMove(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;
	Directory	*tmp_toc;

	unsigned	start_frame,
			num_frames,
			target;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address [, address] ] move [address]
	 */
	if ((c_data->add2 != -1 && c_data->add1 == -1) ||
		c_data->file != NULL) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for move. use current frame for default
	 */
	DEFAULT_FRAME(med_data->current_frame, c_data);

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	

	start_frame = c_data->add1;
	num_frames = c_data->add2 - start_frame + 1;
	target = c_data->add3;

#ifdef	DEBUG
	(void) fprintf(stderr, "move frames %d + %d to %d\n",
		start_frame, num_frames, target);
#else
	/*
	 * execute the command
	 */
	if ((tmp_toc = CGM_moveFrames (
		start_frame - 1, num_frames, target )) == NULL) {

		command_error_message (
			med_data, errno, 0, (char *) NULL, MED_WARN);
		return;
	}
	toc = tmp_toc;
#endif
	dirty = TRUE;	/* buffer has been modified	*/
}

/*
 * medPrint: print contents of buffer at a given line
 */
void	medPrint(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;

	unsigned	start_frame,
			num_frames;

	int	i;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address [, address] ] copy [address]
	 */
	if ((c_data->add2 != -1 && c_data->add1 == -1) ||
		c_data->add3 != -1 || c_data->file != NULL) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for print. use current frame for default
	 */
	DEFAULT_FRAME(med_data->current_frame, c_data);

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	
	start_frame = c_data->add1;
	num_frames = c_data->add2 - start_frame + 1;

#ifdef	DEBUG
	(void)
	fprintf(stderr, "print frames %d + %d \n", start_frame, num_frames);
#else


	if (med_data->last_frame == 0) {
		(void) fprintf(stderr, "No frames in buffer\n");
		return;
	}

	for (i = start_frame; i < (start_frame + num_frames) &&
				i <= med_data->last_frame; i++) {
		(void) fprintf(stderr, 
			"frame: %d, contains %d records, starts at record: %d, label: %s\n",
			i,
			CGM_NUM_RECORD(toc, (i-1)),
			CGM_RECORD(toc, (i-1)),
			CGM_PIC_DES(toc, (i-1)));
	}
#endif

	med_data->current_frame = start_frame + num_frames -1;
}

/*
 * medQuit: exit med
 */
void	medQuit(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;
	int	status;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * quit [!]
	 */
	if ((c_data->add1 != -1 || c_data->add2 != -1 || c_data->add3 != -1) 
		|| (c_data->file != NULL && *c_data->file != '!')) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

#ifdef	DEBUG
	(void) fprintf(stderr, "quit\n");
#else
	status = CGM_termMetaEdit();
#endif

	if (dirty && !c_data->file) { /* if dirty and not q!	*/
		command_error_message(med_data, errno, 0, 
			"No write since last change (quit! overides)", 
			MED_WARN);
		return;
	}
		
	exit(status > 0 ? 0 : 1);
}

/*
 * medRead: read the contents of a metafile into the buffer
 */
void	medRead(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;
	Directory	*tmp_toc;

	unsigned	start_frame;

	int	frame_count;	/* num frame in buffer before the read	*/

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address ] read <file name>
	 */
	if (c_data->add2 != -1 || c_data->add3 != -1 ||
		c_data->file == NULL) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for read. use current frame for default
	 */
	DEFAULT_FRAME(med_data->current_frame, c_data);

	if (med_data->last_frame != 0 && c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	
	start_frame = c_data->add1;

#ifdef	DEBUG
	(void) fprintf(stderr, "read file %s in at line %d\n", 
				c_data->file, start_frame);
#else

	/*
	 * record how man frames are already in file so we can set the 
	 * current frame after the read to the last of the read in frames
	 */ 
	if (toc)
		frame_count = CGM_NUM_FRAMES(toc);
	else {
		c_data->add1 = c_data->add2 = c_data->add3 = -1;
		medEdit(med_data);
#ifdef	DEAD
		command_error_message(med_data, errno, 0, 
			"Must be currently editing a file. Use edit", MED_WARN);
#endif
		return;
	}

	/*
	 * execute the command
	 */
	if ((tmp_toc = CGM_readFrames (
		c_data->file, 0, -1, start_frame, 1440)) == NULL) {

		command_error_message (
			med_data, errno, 0, (char *) NULL, MED_WARN);
		return;
	}

	toc = tmp_toc;
	med_data->current_frame = start_frame + 
			(CGM_NUM_FRAMES(toc) - frame_count);

	(void) fprintf(stderr, "%d frames\n",CGM_NUM_FRAMES(toc) - frame_count);

	med_data->last_frame = CGM_NUM_FRAMES(toc);
#endif
	dirty = TRUE;	/* buffer has been modified	*/
}

/*
 * medWrite: write buffer contents to a file
 */
void	medWrite(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;

	unsigned	start_frame,
			num_frames;

	short	whole_buffer;	/* true if user wants entire buffer written */

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address [, address] ] write [ ! ] [file name]
	 */
	if ((c_data->add2 != -1 && c_data->add1 == -1) || c_data->add3 != -1) {

		command_error_message (
			med_data, errno, 1, (char *) NULL, MED_WARN);
		return;
	}

	whole_buffer = c_data->add1 == -1 ? TRUE : FALSE;


	/*
	 * calculate address for write. use entire buffer as default
	 */
	if (c_data->add2 == -1) {
		if (c_data->add1 == -1) {
			c_data->add1 = 1;
			c_data->add2 = med_data->last_frame;
		}
		else {
			c_data->add2 = c_data->add1;
		}
	}

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	
	start_frame = c_data->add1;
	num_frames = c_data->add2 - start_frame + 1;

	/*
	 * if no file is specified then use the current edit file
	 */
	c_data->file = c_data->file == NULL ? editFile : c_data->file;

#ifdef	DEBUG
	(void) fprintf(stderr, "write frames %d + %d to %s\n", start_frame, 
					num_frames, c_data->file);
#else
	/*
	 * see if user is writing entire file or just a piece of it
	 */
	if (whole_buffer) {	/* write entire file	*/
		if (CGM_writeFile(c_data->file) < 0) { 
			command_error_message (
				med_data, errno, 0, (char *) NULL, MED_WARN);
			return;
		}
		dirty = FALSE;	/* buffer has been cleared	*/
	}
	else {
		if (CGM_writeFrames(c_data->file, 
				start_frame - 1, num_frames) < 0) {
			command_error_message (
				med_data, errno, 0, (char *) NULL, MED_WARN);
			return;
		}
	}
#endif

}

/*
 * medAppend: append buffer contents to a file
 */
void	medAppend(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;

	unsigned	start_frame, num_frames;
	int	status;	
	int	type;	/* type of write, 1 => write, 0 => append	*/
	int	c;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address [, address] ] write [ ! ] [file name]
	 */
	if ((c_data->add2 != -1 && c_data->add1 == -1) || c_data->add3 != -1) {

		command_error_message (
			med_data, errno, 1, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for write. use entire buffer as default
	 */
	if (c_data->add2 == -1) {
		if (c_data->add1 == -1) {
			c_data->add1 = 1;
			c_data->add2 = med_data->last_frame;
		}
		else {
			c_data->add2 = c_data->add1;
		}
	}

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	
	start_frame = c_data->add1;
	num_frames = c_data->add2 - start_frame + 1;

	start_frame--;

	/*
	 * if no file is specified then use the current edit file
	 */
	c_data->file = c_data->file == NULL ? editFile : c_data->file;

	(void) fprintf(stderr, "Saving to %s\n", c_data->file);

	/*
	 * check status of file to save to 
	 */
	status = CGM_validCGM(c_data->file);

	/*
	 * Does the file exist and if so is it a NCAR binary CGM
	 */
	if (status == -1 ) {	/* error?, file may not exist	*/
		if (errno == ENOENT) {
			type = 1;	/* file does NOT already exist	*/
			(void) fprintf(stderr,
			"Creating file %s\n", c_data->file);
		}
		else {
			perror ((char *) NULL);	/* error		*/
			return;
		}
	}
	else if (status == 0) {	/* file exists but is not a NCAR CGM	*/
		(void)fprintf(stderr,"Non valid CGM, overwrite file? [y,n](n)");
		while (c = getchar()) { 
			if (c == 'y' || c == 'Y') {
				while ((c = getchar()) != '\n');
				type = 1;	/* write to file	*/
				break;
			}
			if (c == '\n') 
				return;
		}
	} else  {	/* file exists and is a valid CGM	*/
			type = 0;	/* append to file	*/
	}

	/*
	 * if type is 1, then we do a write 
	 */
	if (type == 1) {	/* doing a write, not an append	*/

		if (CGM_writeFrames(c_data->file,start_frame,num_frames) < 0) {
			command_error_message (
				med_data, errno, 0, (char *) NULL, MED_WARN);
			return;
		}
	}
	else {	/* file already exists and is valid, do an append	*/

		if (CGM_appendFrames(c_data->file,start_frame,num_frames) < 0) {
			command_error_message (
				med_data, errno, 0, (char *) NULL, MED_WARN);
			return;
		}
	}

}

/*
 * medSplit: split the file into pieces
 */
void	medSplit(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;

	unsigned	start_frame,
			num_frames;

	char	*prefix;	/* prefix for split file name		*/
	char	*file_name = NULL;	/* complete split file name	*/
	int	num_split;	/* number of files to split into	*/
	int	file_size;	/* number of frames contained in files	*/
	int	r;		/* excess frames			*/
	int	i;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * [ address , address ] split < number > [ outfile ]
	 */
	if ((c_data->add2 != -1 && c_data->add1 == -1) || c_data->add3 == -1) {

		command_error_message (
			med_data, errno, PRI_USAGE, (char *) NULL, MED_WARN);
		return;
	}

	/*
	 * calculate address for write. use entire buffer as default
	 */
	if (c_data->add2 == -1) {
		if (c_data->add1 == -1) {
			c_data->add1 = 1;
			c_data->add2 = med_data->last_frame;
		}
		else {
			c_data->add2 = c_data->add1;
		}
	}

	c_data->file = c_data->file == NULL ? SPLIT_PREFIX : c_data->file;

	if (c_data->add1 > c_data->add2) {
		fprintf(stderr, "Invalid address\n");
		return;
	}
	if (c_data->add1 > med_data->last_frame) {
		fprintf(stderr, "Address out of range\n");
		return;
	}	
	start_frame = c_data->add1;
	num_frames = c_data->add2 - start_frame + 1;
	num_split = c_data->add3;
	prefix = c_data->file;

	file_name = malloc ((unsigned) (strlen (prefix) + strlen ("XXX") 
			+ strlen ((char *) SPLIT_POSTFIX) + 1));


	/*
	 * figure out how big the files will be
	 */
	file_size = num_frames / num_split;
	r = num_frames % num_split;
#ifdef	DEBUG
#else
	file_size++;
	for (i = 0; i < num_split; i++) {
		if (r == 0) file_size--; 
		r--;

		(void) sprintf( file_name,"%s%03d%s", prefix,i+1,SPLIT_POSTFIX);
		if (CGM_writeFrames(file_name, 
				start_frame - 1, (unsigned) file_size) < 0) {
			command_error_message (
				med_data, errno, 0, (char *) NULL, MED_WARN);
			if (file_name) free (file_name);
			return;
		}

		start_frame += file_size;
	}
#endif
	if (file_name) free (file_name);
}


/*
 * medShell: escape to the shell
 */
void	medShell(med_data)
	MedData	*med_data;
{
	CommandData	*c_data;

	c_data = &med_data->command_data;

	errno = 0;	/* clear global error number	*/
	/*
	 * check to see if command in valid format:
	 * 	! < shell command >
	 */
	if ((c_data->add1 != -1 || c_data->add2 != -1) || c_data->add3 != -1
		|| c_data->file == NULL) {

		command_error_message (
			med_data, errno, 1, (char *) NULL, MED_WARN);
		return;
	}

#ifdef	DEBUG
	(void) fprintf(stderr, "! %s\n", c_data->file);
#else

	/*
	 * echo the command to the screen
	 */
	(void) fprintf(stderr, "! %s\n", c_data->file);

	/*
	 * call the shell with the command
	 */
	(void) system(c_data->file);
#endif

}
