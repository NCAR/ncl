/*
 *	$Id: meta_edit.c,v 1.25 2008-07-27 03:18:42 haley Exp $
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


#include	<stdio.h>
#include	<fcntl.h>
#include	<unistd.h>
#include	<sys/types.h>
#include	<errno.h>
#include	<string.h>
#include	<stdlib.h>

#ifdef	SYSV
#include	<sys/stat.h>
#include	<errno.h>
#else
#include	<sys/file.h>
#endif

#include	<ncarg/c.h>
#include	"cgm_tools.h"
#include	"meta_edit.h"
#include	"internals.h"

/*	meta_edit.c:
 *
 *	Author 		John Clyne
 *	Date		Tue May 23 13:33:13 MDT 1989
 *
 *		This file contains a set of routines for editing a metafile
 *	No real editing is actually done until the file is 
 *	writen. A working list is kept to record the metafile state
 *	The supported editing capabilities include:
 *		read frames, write frames, delete frames, write file,
 *		merge frames, append frames to file 
 *
 *	At the beginning of each function call 'errno' is set to zero.
 *
 *	
 * rev 1.01 clyne 2/20/90	: memory allocation error in copy_dir() and
 *				  shift_frames().
 * rev 1.02 clyne 2/21/90	: added ability to specify a tmp directory to 
 *				  use other than the default
 * rev 1.03 clyne 3/23/90	: wrote function CGM_writeFrames 
 *
 * rev 1.04 clyne 11/16/90	: changed name of scratch files to be < 14 chars
 */

/*LINTLIBRARY*/

/*
 * a list that is a representation of the contents of a  metafile in the
 * process of being edited. The metafile is not actually edited itself
 * until the user writes it.
 */
static	Working_list	workingList;

static	boolean Initialized = FALSE; 	/* true after CGM_InitMetaEdit is 
					 * called false after 
					 * CGM_TermMetaEdit is called
					 */

static	char	*ncarCgm;		/* CGM we are editing*/
static	int	recordSize;	/* record size of ncarCgm	*/
static	Directory *workingDir;	/* dir representing the metafile being edited */
static	Directory *saveDir;	/* actual dir for the metafile being edited */
static	Cgm_fd	workingFd;	/* read file descriptor for working file   */
static	FILE *verboseFP = NULL;	/* operate in verbose mode		*/

#define	TMPFILE		"/libcgm.XXXXXX"
#define	WRITEFILE	"cgm.XXXXXX"
#define	ABS(X)		((X) < 0 ? (-(X)) : (X))

static	char	*tempFile = NULL;	/* name of temp scratch file	*/
static	char	*writeFile = NULL;	/* name of scratch file for writes */


static	unsigned char	*tmpBuf;	/* tmp storage for r/w metafile	*/

#define	ERR	((Directory *) NULL)

/*
 *	shift_frames
 *	PRIVATE
 *		shift a group of frames to the "right" or the left 
 *	depending on the value of src and dest
 *	of frames. If more memory is needed in the directory then more
 *	is allocated. 
 *
 * on entry
 *	src		: first frame to be moved
 *	dest		: destination of frames to be moved
 *	num_frames		: number of frames to be moved
 *	gap		: if (dest > src) => right shift and gap is size of hole *			  in records to be left. Else left shift and gap is size
 *			  in records of space being filled by shifted frames. 
 * on exit
 *	dir		: has been updated to reflect move
 *	list		: has been updated to reflect move
 */
static	shift_frames (src, dest, num_frames, gap, dir, list)
	unsigned int	src,
			dest,
			num_frames,
			gap;
	Directory	*dir;
	Working_list	*list;
{
	int	i;

	/*
	 * see if need more memory
	 */
	if ((dest + num_frames) > dir->dir_size) {

		(void) ReallocDir(dir, dest + num_frames);
	}

	/*
	 * shift frames over taking into account overlapping frames
	 */
	if (src < dest) {	/* right shift	*/
		for (i = 0, dest += (num_frames-1), src += (num_frames-1); 
						i < num_frames; 
						i++, dest--, src--) {

			list->list[dest] = list->list[src];

			/*
			 *	update the dir
			 */
			dir->d[dest].num_record  = dir->d[src].num_record;
			dir->d[dest].type  = dir->d[src].type;

			dir->d[dest].record = dir->d[src].record + gap;

			if (dir->d[src].text) {
				if (dir->d[dest].text) {
					(void) free((Voidptr) dir->d[dest].text);
				}
				dir->d[dest].text = malloc (
					(unsigned) strlen(dir->d[src].text)+1);
				if (! dir->d[dest].text) {
					return(-1);
				}

				(void) strcpy(dir->d[dest].text, 
							dir->d[src].text);
			}

		}
	}
	else	{	/* left shift		*/
		for (i = 0; i < num_frames; i++, dest++, src++) {

			list->list[dest] = list->list[src];

			/*
			 *	update the dir
			 */
			dir->d[dest].num_record  = dir->d[src].num_record;
			dir->d[dest].type  = dir->d[src].type;

			dir->d[dest].record = dir->d[src].record - gap;

			if (dir->d[src].text) {
				if (dir->d[dest].text) {
					(void) free((Voidptr) dir->d[dest].text);
				}
				dir->d[dest].text = malloc (
					(unsigned) strlen(dir->d[src].text)+1);
				if (! dir->d[dest].text) {
					return(-1);
				}

				(void) strcpy(dir->d[dest].text, 
							dir->d[src].text);
			}

		}
	}
	return(1);
}
/*	copy_dir
 *	PRIVATE
 *		copy a portion of a directory to another directory. The
 *	frames effected in the destination directory are overwritten
 *	If need be, more memory is allocated for the destination dir.
 * on entry
 *	d1		: the destination directory
 *	d2		: the source directory
 *	src		: starting frame in d2
 *	dest		: target frame in d1
 *	num_frames	: num frames to overwrite/copy
 * on exit
 *	d1		: has been updated
 */
static	copy_dir(d1, d2, src, dest, num_frames)
	Directory	*d1, *d2;
	unsigned int	src,
			dest,
			num_frames;
{
	int	i;
	int	offset;	/* new offset of first frame copied	*/

	/*
	 *	calculate offset
	 */
	offset = (dest == 0 ? d1->d[0].record :
		d1->d[dest - 1].record + d1->d[dest -1].num_record);
	/*
	 * see if need more memory
	 */
	if ((dest + num_frames) > d1->dir_size) {

		(void) ReallocDir(d1, dest + num_frames);
	}

	/*
	 * copy over dir contents
	 */
	for (i = 0; i < num_frames; i++, src++, dest++) {
		d1->d[dest].num_record  = 
			d2->d[src].num_record;
		d1->d[dest].type  = 
			d2->d[src].type;

		if (i) {	/* don't change first new frame offset	*/
			d1->d[dest].record =  
				d1->d[dest-1].record + d1->d[dest-1].num_record;
		}
		else
			d1->d[dest].record = offset;

		if (d2->d[src].text) {
			if (d1->d[dest].text) (void) free((Voidptr) d1->d[dest].text);
			d1->d[dest].text = malloc (
					(unsigned) strlen(d2->d[src].text)+1);
			if (! d1->d[dest].text) {
				return(-1);
			}

			(void) strcpy(d1->d[dest].text, d2->d[src].text);
		}
	}

	/*
	 * update offsets of frames following new frames copied in
	 */
	for (i = dest + num_frames; i < d1->num_frames; i++) {
		d1->d[i].record = d1->d[i-1].record + d1->d[i-1].num_record;
	}


	return(1);
}
/*
 *	write_header
 *	PRIVATE
 *
 *		install the header of metafile in another metafile
 * on entry:
 *	src_fd		: Cgm file descriptor for source file
 *	dest_fd		: Cgm file descriptor for destination file
 *	dir		: directory for src file
 * on exit
 *	return		: < 0 then error
 */
static
write_header(src_fd, dest_fd, dir)
	Cgm_fd		src_fd,
			dest_fd;
	Directory	*dir;
{
	int	i;

	if ((CGM_lseek(src_fd, 0, SEEK_SET)) < 0) {
		return(-1);
	}


	for (i = 0; i < dir->d[0].record; i++) {
		if((CGM_read(src_fd, tmpBuf)) < 0) {
			return(-1);
		}
		if((CGM_write(dest_fd, tmpBuf)) < 0) {
			return(-1);
		}
	}
	return(1);
}
		
/*
 *	write_trailer
 *	PRIVATE
 *
 *		install the trailer of metafile in another metafile
 * on entry:
 *	src_fd		: Cgm file descriptor for source file
 *	dest_fd		: Cgm file descriptor for destination file
 *	dir		: original directory for src file  (save dir) 
 */
static
write_trailer(src_fd, dest_fd, dir)
	Cgm_fd		src_fd,
			dest_fd;
	Directory	*dir;
{
	int	offset;	/* location of header in src file	*/

	/*
	 * get address of trailer in records
	 */
	offset = dir->d[dir->num_frames - 1].record +
		 dir->d[dir->num_frames - 1].num_record;


	if ((CGM_lseek(src_fd, offset, SEEK_SET)) < 0) {
		return(-1);
	}	/* go to end of last frame	*/

	while ((CGM_read(src_fd, tmpBuf)) > 0) { /* while not EOF	*/

		if((CGM_write(dest_fd, tmpBuf)) < 0) {
			return(-1);
		}
	}
	return(1);
}
/*
 *	write_frame
 *	PRIVATE
 *
 *		append a frame from one file to another at the current
 *	file ptrs position
 * on entry:
 *	src_fd		: Cgm file descriptor for source file
 *	dest_fd		: Cgm file descriptor for destination file
 *	tmp_fd		: Cgm file descriptor for the temp file 
 *	list		: working list for src file
 *	dir		: directory for src file
 *	index		: which frame from src to append
 */
static
write_frame(src_fd, dest_fd, tmp_fd,  list, dir, index)
	Cgm_fd		src_fd,
			dest_fd,
			tmp_fd;
	Working_list	list;
	Directory	*dir;
	unsigned	index;
{

	int	j;
	int	offset,
		frame;

	/*
	 * see if frame is in old file or read in from another file
	 */
	if (list.list[index].utype == ACTUAL) {
		frame = list.list[index].uval.frame;
		offset = dir->d[frame].record;
		if ((CGM_lseek(src_fd, offset, SEEK_SET)) < 0) {
			return(-1);
		}

		for (j = 0; j < dir->d[frame].num_record; j++){
			if((CGM_read(src_fd, tmpBuf)) < 0) {
				return(-1);
			}
			if((CGM_write(dest_fd, tmpBuf)) < 0) {
				return(-1);
			}
		}
	} 
	else {
		offset= list.list[index].uval.frame_entry.start_rec;
		if ((CGM_lseek(tmp_fd, offset, SEEK_SET)) < 0) {
			return(-1);
		}

		for (j = 0;
		j < list.list[index].uval.frame_entry.num_rec;j++) {

			if((CGM_read(tmp_fd,tmpBuf))< 0) {
				return(-1);
			}
			if((CGM_write(dest_fd, tmpBuf)) < 0) {
				return(-1);
			}
		}
	}
	return(1);
}

/*	CGM_copyFrames
 *	PUBLIC
 *
 *	Copy a block of frames to a target destination.
 *
 * on entry
 *	start_frame	: first frame in block to be copied
 *	num_frames	: num frames to be copied starting with start_frame
 *	target		: position where first frame will be copied in to.
 * on exit
 *	workingDir	: will be updated to reflect changes
 *	workingList	: will be updated
 *	return		: a pointer to the directory representing the modified
 *			  state of the file on success, else return NULL
 */
/*ARGSUSED*/
Directory	*CGM_copyFrames(start_frame, num_frames, target )
	unsigned int	start_frame;
	int		num_frames;
	unsigned int	target;
{	
	int	dest;		/* where old frames are moved to 	*/
	int	src;		/* where new frames come from		*/
	unsigned	new_size = 0;	/* size in records new frames read in*/

	int	i;

	errno  = 0;
	
	if (!Initialized)
		return(ERR);

	/*
	 * See if frame list is valid. If not punt, we do nothing
	 */
	if (target > workingList.number ||
			(num_frames + start_frame > workingDir->num_frames)) {

		return (ERR);
	}

	/*
	 * make sure there is room in the working list, if not malloc more mem
	 */
	if (workingList.number + num_frames > workingList.size) {
		workingList.list = (List *) realloc 
			((char *) workingList.list, (unsigned) (sizeof(List ) * 
			(workingList.number + num_frames)));
		if (! workingList.list) {
			return(ERR);
		}

		workingList.size = workingList.number + num_frames;
	}


	/*
	 * make sure there is room in the directory, if not malloc more mem
	 */
	if (workingDir->num_frames + num_frames > workingDir->dir_size) {

		(void) ReallocDir(workingDir, (unsigned) 
			(workingDir->num_frames + num_frames));
	}

	/*
	 *	find out how many records are contained in frames to be copied
	 * 	so we can use "shift_frames"
	 */
	for (i = 0, new_size = 0; i < num_frames; i++) {
		new_size += workingDir->d[i + start_frame].num_record;
	}

	/*
	 *	Shift old frames over to make room for copied frames. 
	 *	Only shift those frames "right" of destination.
	 */
	(void) shift_frames(target, target + num_frames, 
		workingList.number - target,new_size, workingDir, &workingList);
		
	/*
	 * record new frames in working list and working directory
	 */
	for (i = 0, dest = target; i < num_frames; i++, dest++, start_frame++) {

		src = start_frame < target ? 
				start_frame : start_frame + num_frames;

		workingList.list[dest] = workingList.list[src];
		workingDir->d[dest] = workingDir->d[src];
	}

	workingList.number += num_frames;	/* update frame count	*/

	/*
	 * update frame count in working dir
	 */
	workingDir->num_frames += num_frames;


	return(workingDir);
}

/*	CGM_deleteFrames:
 *	PUBLIC
 *
 *		delete the list of frames from a reference list.  
 * on entry:
 *	start_frame	: first frame to be deleted
 *	num_frames	: num frames to be deleted starting with start_frame
 * on exit:
 *	workingList	: will be updated to reflect the desired deletions 
 *			  unless return < 0. In which case no action occurs
 *	workingDir	: will be updated if return >= 0.
 *	return		: a pointer to the directory representing the modified
 *			  state of the file on success, else return NULL
 */
Directory	*CGM_deleteFrames(start_frame, num_frames)
	unsigned int	start_frame,
			num_frames;
{
	int	i;
	unsigned num_move;	/* num frames not deleted but need to be moved*/
	unsigned del_size = 0;	/* size in records of frames we will delete */

	errno = 0;

	if (!Initialized)
		return(ERR);

	/*
	 * check to see that all frames listed are valid and too many 
	 * weren't specified.
	 */
	if (start_frame + num_frames > workingList.number) 
			return(ERR);	/* invalid frame number */

	/*
	 * determine how many old frames will need to be moved to fill in
	 * space of deleted frames
	 */
	num_move = workingList.number - (start_frame + num_frames);

	/*
	 * move frames not being deleted to fill in position of 
	 * deleted frames. First find out how many records old frames occupy
	 */
	for (i = start_frame; i < (start_frame + num_frames); i++) {
		del_size += workingDir->d[i].num_record;
	}

	/*
	 * shift "left" any frames to the "right" of frames being deleted
	 * in the working directory and list
	 */
	(void) shift_frames(start_frame + num_frames, start_frame, num_move,
		del_size, workingDir, &workingList);

	/*
	 * update frame count
	 */
	workingDir->num_frames -= num_frames;
	workingList.number -= num_frames;

	return(workingDir);

}


/*	CGM_readFrames
 *	PUBLIC
 *
 *		read in frame(s) from the named metafile into the working
 *	metafile. The frames will be placed sequentially in to the working
 *	metafile. No Actual changes are made until CGM_TermMetaEdit is invoked
 * on entry
 *	*ncar_cgm	: name of file to get frames from
 *	start_frame	: first frame in file to be read in
 *	num_frames	: num frames to be read starting with start_frame. If
 *			  num_frames < 0 then all frames in the file starting
 *			  with start_frame are read in.
 *	target		: position where first frame will be read in to.
 *			  Cannot exceed working file by more then one frame
 *	record_size	: size in bytes of record in file (not used)
 * on exit
 *	workingDir	: will be updated to reflect changes
 *	workingList	: will be updated
 *	return		: a pointer to the directory representing the modified
 *			  state of the file on success, else return NULL
 */
/*ARGSUSED*/
Directory	*CGM_readFrames(ncar_cgm, start_frame, num_frames, target, 
		record_size)
	char		*ncar_cgm;
	unsigned int	start_frame;
	int		num_frames;
	unsigned int	target,
			record_size;
{	
	Cgm_fd	fd;		/* file descriptor for reading the file	*/
	Directory	*dir;	/* directory for the read file		*/
	int	save_offset;	/* save the tmp file ptr positioning	*/
	int	dest;		/* where old frames are moved to 	*/
	int	src;		/* where new frames come from		*/
	unsigned	new_size = 0;	/* size in records new frames read in*/

	int	i,j;
	int	r = ABS(recordSize);

	errno = 0;
	
	if (!Initialized)
		return(ERR);

	if (target > workingList.number)
		return(ERR);	/* target cannot not exceed end of file	*/

	if ((fd = CGM_open(ncar_cgm, r, "r")) < 0) /*open readfile */
		return(ERR);

	if ((dir = CGM_directory(fd,verboseFP)) == NULL) {/* create directory*/
		(void) (void) CGM_close(fd);
		return (ERR);
	}

	if (num_frames < 0) {
		num_frames = dir->num_frames - start_frame;
	}

	/*
	 * See if frame list is valid. If not punt, we do nothing
	 */
	if (num_frames + start_frame > dir->num_frames) {
		CGM_freeDirectory(dir);
		(void) CGM_close(fd);
		return (ERR);		/* too many frames	*/
	}

	/*
	 * make sure there is room in the working list, if not malloc more mem
	 */
	if (workingList.number + num_frames > workingList.size) {
		workingList.list = (List *) realloc 
			((char *) workingList.list, (unsigned) (sizeof(List ) * 
			(workingList.number + num_frames)));
		if (! workingList.list) {
			return(ERR);
		}

		workingList.size = workingList.number + num_frames;
	}


	/*
	 * make sure there is room in the directory, if not malloc more mem
	 */
	if (workingDir->num_frames + num_frames > workingDir->dir_size) {

		(void) ReallocDir(workingDir, (unsigned) 
				(workingDir->num_frames + num_frames));
	}

	/*
	 *	save postion of tmp file file ptr
	 */
	save_offset = workingList.offset;

	/* 
	 *	read designated frames into the temp file. Later they
	 *	will be merged with the working file. 
	 */
	for (i = 0, src = start_frame; i < num_frames; i++, src++) {
	
		/* move to next frame to be read in	*/
		if ((CGM_lseek(fd, dir->d[src].record, SEEK_SET)) < 0) {

			(void) CGM_lseek(workingList.tmp_fd, save_offset,SEEK_SET);
			workingList.offset = save_offset; /* reset file ptr */
			CGM_freeDirectory(dir);
			(void) CGM_close(fd);
			return(ERR);
		}

		/* append frame to temp file		*/
		for (j = 0; j < dir->d[src].num_record; j++) {
			if((CGM_read(fd, tmpBuf)) < 0) {
				(void)CGM_lseek(
					workingList.tmp_fd,save_offset, SEEK_SET);

				workingList.offset = save_offset;
				CGM_freeDirectory(dir);
				(void) CGM_close(fd);
				return(ERR);
			}
			if((CGM_write(workingList.tmp_fd, tmpBuf)) < 0) {
				(void) CGM_lseek (
					workingList.tmp_fd,save_offset, SEEK_SET);

				workingList.offset = save_offset;
				CGM_freeDirectory(dir);
				(void) CGM_close(fd);
				return(ERR);
			}
		}

		
		/*
		 * update record of file ptr for temp file
		 */
		workingList.offset += dir->d[src].num_record;
		new_size += dir->d[src].num_record;
	}

	/*
	 *	NOW that frames have been CORRECTLY read in update working list
	 *	and the working directory. First shift old frames over to make
	 *	room for new frames. Only shift those frames "right" of 
	 *	destination.
	 */
	(void) shift_frames(target, target + num_frames, 
		workingList.number - target,new_size, workingDir, &workingList);
		
	/*
	 * record new frames in working list
	 */
	for (i = 0, dest = target, src = start_frame; i < num_frames; 
							i++, dest++, src++) {
		/*
		 * record new frames in working list
		 */
		workingList.list[dest].utype = REFERENCE;
		workingList.list[dest].uval.frame_entry.num_rec = 
			dir->d[src].num_record;

		/*
		 * use save_offset to determine where new frames are stored
		 * in the temp file
		 */
		workingList.list[dest].uval.frame_entry.start_rec = save_offset;
		save_offset += dir->d[src].num_record;	/* update offset*/

	}

	workingList.number += num_frames;	/* update frame count	*/

	/*
	 * record new frames in working directory
	 */
	(void) copy_dir(workingDir, dir, 
			start_frame, target, (unsigned) num_frames);

	/*
	 * update frame count in working dir
	 */
	workingDir->num_frames += num_frames;

	CGM_freeDirectory(dir);	/* free dir for read file	*/
	(void) CGM_close(fd);	/* close read file		*/

	return(workingDir);
}

/*	CGM_moveFrames
 *	PUBLIC
 *
 *		move a block of frames within the working file
 * on entry
 *	start_frame	: the first frame in the block
 *	num_frames	: number of frames to be moved starting with start_frame
 *	target		: destination of block 
 * on exit
 *	workingDir	: is updated to reflect move
 *	workingList	: is updated to reflect move
 *	return		: a pointer to the directory representing the modified
 *			  state of the file on success, else return NULL
 */
Directory	*CGM_moveFrames (start_frame, num_frames, target)
	unsigned int	start_frame,
			num_frames,
			target;
{
	extern	Directory	*init_dir();

	Working_list	list;	/* tmp storage for frames to be moved	*/
	Directory	*dir;	/* tmp storage for frames to be moved	*/

	unsigned	src,
			dest;

	unsigned int	block_size = 0; /* size in records of moved frames */
	int	i, tmp;

	errno = 0;

	if (!Initialized)
		return(ERR);

	/*
	 * see if block is valid, else punt
	 */
	if (start_frame + num_frames > workingList.number)
		return(ERR); 

	/*
	 * malloc memory for list and dir to hold frames to be swaped
	 */
	list.list = (List *) malloc ((unsigned) (sizeof(List ) * num_frames));
	if (! list.list) {
		return(ERR);
	}

	list.size = num_frames;

	/*
	 * intialize a tmp directory
	 */
	if ((dir = init_dir()) == NULL) {
		return(ERR);
	}

	/*
	 * store frames to be moved in tmp dir. We will effectively
	 * be doing a swap so we need tmp storage. 
	 */
	if (copy_dir(dir, workingDir, start_frame, 0, num_frames) < 0)
		return(ERR);
	dir->num_frames = num_frames;	/* copy_dir does not touch this	*/
	/*
	 * store block of frames to be moved in tmp working list
	 * find out how much space they occupy
	 */
	for (i = 0, src = start_frame; i < num_frames; src++, i++) {
		list.list[i] = workingList.list[src];
		block_size += dir->d[i].num_record;
	}

	/*
	 * shift left ALL frames "right" of move block into position occupied 
	 * by move block
	 */
	if (shift_frames(start_frame + num_frames, start_frame,
		workingList.number - (start_frame + num_frames), 
		block_size, workingDir, &workingList) < 0) {

		return(ERR);
	}


	/*
	 * create a "hole" where moved block is supposed to go
	 * i.e shift "right" frames which should be to the "right" of 
	 * moved block.
	 */
	tmp = target > start_frame ? target - num_frames : target;
	src = ZERO_INDEX(tmp);
	dest = src + num_frames;
	if ( shift_frames(src, dest, workingList.number - num_frames - src,
		block_size, workingDir,
		&workingList) < 0) {

		return(ERR);
	}

	/*
	 * put stored block of frames back in new position in 
	 * working directory and working list.
	 */
	dest = src;	/* target > start_frame ? target - numframes : target*/
	if (copy_dir(workingDir, dir, 0, dest, num_frames) < 0)
		return(ERR);

	for (i = 0; i < num_frames; dest++, i++) {
		workingList.list[dest] = list.list[i];
	}
		
	(void) free((Voidptr) list.list);
	CGM_freeDirectory(dir);
	return(workingDir);
}

/*	CGM_initMetaEdit
 *	PUBLIC
 *
 *		intialize the meta_edit module. This routine must be called
 *	prior to any of the other meta edit routines. Invoking this module a 
 *	second time withiout invoking CGM_writeFile(file) may result in the
 *	loss of any changes made to the file of the previous invocation.
 *	This is analogous to performing a "e! [filename]" from within
 *	ed or vi.
 * on entry:
 *	*ncar_cgm	: the name of NCAR Computer Graphics Metafile to edit.
 *	record_size	: number of bytes per record in ncar_cgm. if negative
 *			  space for the file is created in memory
 *	*local_tmp	: path to scratch directory. If null use /tmp
 *	verbose_fp	: operate in verbose mode. 
 * on exit
 *	return		: a pointer to the directory representing the modified
 *			  state of the file on success, else return NULL
 */
Directory	*CGM_initMetaEdit (ncar_cgm, record_size, local_tmp, verbose_fp)
	char		*ncar_cgm;
	int	record_size;
	char	*local_tmp;
	FILE	*verbose_fp;
{
	int	i;
	int	r = ABS(record_size);
	const char	*tmp_path;

	errno = 0;

	/*
	 *	if already initialized, close working file without making
	 *	free up resources and start over again
	 */
	if (Initialized) {
		(void) CGM_termMetaEdit();
	}


	recordSize = record_size;
	verboseFP = verbose_fp;

	if (!ncar_cgm) {
		return(ERR);	/* no file	*/
	}

	if (! (tmp_path = GetNCARGPath(NGTMPDIR))) {
		return ((Directory *) NULL);
	}

	/*
	 *	store the name of the file
	 */
	ncarCgm = (char *) malloc ((unsigned) strlen(ncar_cgm) + 1);
	if (! ncarCgm) {
		return(ERR);
	}
	(void) strcpy(ncarCgm, ncar_cgm);

	/*
	 * create buffer for r/w of metafiles
	 */
	tmpBuf = (unsigned char *) malloc(r * sizeof(unsigned char));
	if (! tmpBuf) {
		return(ERR);
	}

	/*
	 * open the file and create a directory for it
	 */
	if ((workingFd = CGM_open(ncarCgm,recordSize,"r")) < 0) {
		return(ERR);
	}

	if ((workingDir = CGM_directory(workingFd,verboseFP)) == NULL) {
		(void) CGM_close(workingFd);
		return(ERR);
	}

	/*
	 * initialize working list with contents of ncar_file
	 */
	workingList.list = (List *) malloc 
		((unsigned) (workingDir->num_frames * sizeof (List)));
	if (! workingList.list) {
		return(ERR);
	}

	workingList.number = workingDir->num_frames;
	workingList.size = workingDir->num_frames;
	workingList.offset = 0;	/* file ptr is at beginning	*/
	for (i = 0; i < workingDir->num_frames; i++) {
		workingList.list[i].utype = ACTUAL;
		workingList.list[i].uval.frame = i;
	}


	/*
	 *	create a archive of metafile directory
	 */
#ifdef	DEAD
	if ((saveDir = CGM_directory(workingFd,verboseFP)) == NULL) {
		(void) CGM_close(workingFd);
		CGM_freeDirectory(workingDir);
		(void) free((Voidptr) workingList.list);
		return(ERR);
	}
#else
	if ((saveDir = CGM_copyCreateDir(workingDir)) == NULL) {
		(void) CGM_close(workingFd);
		CGM_freeDirectory(workingDir);
		(void) free((Voidptr) workingList.list);
		return(ERR);
	}
#endif
	


	/*
	 *	create a tmp file where results of file reads will
	 *	be stored until the working file is ready to be written
	 *	Use a default tmp directory if user has not specified 
	 *	differently.
	 */
	if (tempFile) (void) free (tempFile);
	if (local_tmp) {
		tempFile = malloc(strlen(local_tmp) + strlen(TMPFILE) + 1);
		if (! tempFile) {
			return(ERR);
		}
		(void) strcpy(tempFile, local_tmp);
	}
	else  {
		tempFile = malloc(strlen(tmp_path) + strlen(TMPFILE) + 1);
		if (! tempFile) {
			return(ERR);
		}
		(void) strcpy(tempFile, tmp_path);
	}

	(void) strcat(tempFile, TMPFILE);

	if (writeFile) (void) free (writeFile);
	writeFile = malloc(strlen(WRITEFILE) + 1);
	if (! writeFile) {
		return(ERR);
	}
	strcpy(writeFile, WRITEFILE);

	(void) mktemp(tempFile);
	(void) mktemp(writeFile);
	if ((workingList.tmp_fd = CGM_open(tempFile, r, "w"))<0) {

		(void) CGM_close(workingFd);
		CGM_freeDirectory(workingDir);
		CGM_freeDirectory(saveDir);
		(void) free((Voidptr) workingList.list);

		return(ERR);
	}


	Initialized = TRUE;
	return(workingDir);
}

/*	CGM_termMetaEdit
 *	PUBLIC
 *
 *	Terminate the editing session 
 *
 * on exit
 *	return		: ( < 0) => failure
 */
int	CGM_termMetaEdit()
{

	errno = 0;

	if (!Initialized)
		return(1);


	/*
	 *	clean up and go home
	 */
	(void) CGM_close(workingList.tmp_fd);	/* close the temp file	*/
	(void) CGM_close(workingFd);	/* */
	(void) unlink(tempFile);	/* remove the temp file		*/
	(void) CGM_freeDirectory(workingDir);	/* free the working directory*/
	(void) CGM_freeDirectory(saveDir);	/* free the backup directory*/
	(void) free((Voidptr) workingList.list);
	(void) free((Voidptr) tmpBuf);
	if (recordSize > 0) (void) free((Voidptr) ncarCgm);

	Initialized = 0;		/* no longer are we initialized	*/
	return(1);			/* success			*/
}

/*
 *	CGM_writeFile
 *	PUBLIC
 *
 *		write out the edited file. writeFile will overwrite
 *	an existing file
 *
 * on entry
 *	*ncar_cgm	: path to output file
 * on exit
 *	return		: (< 0) => error
 */
int	CGM_writeFile(ncar_cgm)
	char	*ncar_cgm;
{

	errno = 0;

	if (!Initialized)
		return(-1);

	return(CGM_writeFrames(ncar_cgm, 0, (unsigned) workingList.number));
}

/*
 *	CGM_writeFrames
 *	PUBLIC
 *
 *		write a list of frames from the working file to a
 *	specified file. The header and trailer of the working file are
 *	installed in the new file. writeFrames will overwrite an
 *	existing file.
 * on entry
 *	start_frame	: first frame in list
 *	num_frames	: number of frames starting with start frame
 *	*ncar_cgm	: path to file to write to 
 */
int	CGM_writeFrames(ncar_cgm, start_frame, num_frames)
	char		*ncar_cgm;
	unsigned	start_frame,
			num_frames;
{ 
	char	*newfile;
	Cgm_fd	fd;
	Cgm_fd	tmp_fd;
	char	*s;
	int	i;

	int	r = ABS(recordSize);

	errno = 0;

	/*
	 *	open the temp file where some frames may be
	 *	stored
	 */
	(void) CGM_flush(workingList.tmp_fd);
	if ((tmp_fd = CGM_open(tempFile, r, "r")) < 0)
		return(-1);
	
	/*
	 *	create && open a a scratch file to write edited metafile to
	 */
	newfile = malloc ((unsigned) (strlen(ncar_cgm) +strlen(writeFile)+1));
	if (! newfile) {
		return(-1);
	}
	newfile = strcpy(newfile, ncar_cgm);

	/*
	 * See if the intented new file resides in a different directory. If
	 * so create a scratch file with the approriate path.
	 */
	if (s = strrchr(newfile, '/')) {
		s++;
		(void) strcat(s, writeFile);
	}
	else {
		newfile = strcpy(newfile, writeFile);
	}

	if ((fd = CGM_open(newfile, r, "w")) < 0) {
		(void) CGM_close(tmp_fd);
		return(-1);
	}



	/*
	 *	install the header from working file into the new file
	 */
	if (write_header(workingFd, fd, saveDir) < 0) {
		(void) CGM_close(tmp_fd);
		(void) CGM_close(fd);
		(void) unlink(newfile);
		return(-1);
	}
	

	/*
	 *	copy the desired frames to the new file
	 */
	for (i = 0; i < num_frames; i++, start_frame++ ) {
		if (write_frame(workingFd, fd, tmp_fd, workingList, 
						saveDir,start_frame) < 0){
			(void) CGM_close(tmp_fd);
			(void) CGM_close(fd);
			(void) unlink(newfile);
			return(-1);
		}
	}

	/*
	 * copy everything in working file from last frame to end of
	 * file to the new file
	 */
	if (write_trailer(workingFd, fd, saveDir) < 0) {
		(void) CGM_close(tmp_fd);
		(void) CGM_close(fd);
		(void) unlink(newfile);
		return(-1);
	}

		
	/*
	 * rename the newfile to the desired output file
	 */
	if (rename(newfile, ncar_cgm) < 0) {
		(void) CGM_close(tmp_fd);
		(void) CGM_close(fd);
		(void) unlink(newfile);
		return(-1);
	}

	/*
	 *	clean up 
	 */
	(void) CGM_close(tmp_fd);
	(void) CGM_close(fd);		/* close the new file		*/
	(void) free((Voidptr) newfile);
	return(1);			/* success			*/
}
/*
 *	CGM_appendFrames
 *	PUBLIC
 *
 *		append a list of frames from the working file to a
 *	specified file. The file must already exist and be a valid
 *	NCAR Graphics binary CGM.
 *
 * on entry
 *	start_frame	: first frame in list
 *	num_frames	: number of frames starting with start frame
 *	*ncar_cgm	: path to file to write to 
 * on exit
 *	return		: < 0 => failure, else success.
 */
int	CGM_appendFrames(ncar_cgm, start_frame, num_frames)
	char		*ncar_cgm;
	unsigned	start_frame,
			num_frames;
{ 
	Cgm_fd	fd;
	Cgm_fd	tmp_fd;
	unsigned char	*buf = NULL;
	int	i;
	unsigned long	tmp;

	int	r = ABS(recordSize);

	errno = 0;


	/*
	 * check to see that all frames listed are valid and too many 
	 * weren't specified.
	 */
	if (start_frame + num_frames > workingList.number) 
			return(-1);	/* invalid frame number */

	/*
	 *	open the temp file where some frames may be
	 *	stored
	 */
	(void) CGM_flush(workingList.tmp_fd);
	if ((tmp_fd = CGM_open(tempFile, r, "r")) < 0)
		return(-1);
	

	/*
	 * perform a quick check to see if the file is a valid CGM
	 */
	if (CGM_validCGM(ncar_cgm) < 1 ) {
		return (-1);
	}

	/*
	 * open the file for appending
	 */
	if ((fd = CGM_open(ncar_cgm, r, "r" )) < 0) {
		(void) CGM_close(tmp_fd);
		return(-1);
	}

	/*
	 * move file pointer to start of last record in file
	 */
	if (CGM_lseek(fd, -1, SEEK_END) < 0 ) {
		(void) CGM_close(tmp_fd);
		(void) CGM_close(fd);
		return(-1);
	}

	/* 
	 * read in the last record
	 */
	buf = (unsigned char *) malloc (r);
	if (! buf) {
		return(-1);
	}
	if (CGM_read(fd, buf) < 0 ) {
		(void) CGM_close(tmp_fd);
		(void) CGM_close(fd);
		if (buf) (void) free((Voidptr) buf);
		return(-1);
	}
	(void) CGM_close(fd);

	/*
	 * make sure record contains a CGM END_MF element as first instruction
	 * in record
	 */
	tmp = buf[HEADERSIZE] << 8 | buf[HEADERSIZE + 1];
	if (GETBITS(tmp, CLASS_POSS, CLASS_BITS) != DEL_ELEMENT ||
		GETBITS(tmp, ID_POSS, ID_BITS ) != END_MF_ID) {

		(void) CGM_close(tmp_fd);
		if (buf) (void) free((Voidptr) buf);
		return(-1);
	}

	/*
	 * now open the file for writing
	 */
	if ((fd = CGM_open(ncar_cgm, r, "a" )) < 0) {
		(void) CGM_close(tmp_fd);
		if (buf) (void) free((Voidptr) buf);
		return(-1);
	}

	/* overwrite last record */
	if (CGM_lseek(fd, -1, SEEK_END) < 0 ) {
		(void) CGM_close(tmp_fd);
		(void) CGM_close(fd);
		return(-1);
	}

	/*
	 *	append the desired frames to the file
	 */
	for (i = 0; i < num_frames; i++, start_frame++ ) {
		if (write_frame(workingFd, fd, tmp_fd, workingList, 
						saveDir,start_frame) < 0){
			(void) CGM_close(tmp_fd);
			(void) CGM_close(fd);
			if (buf) (void) free((Voidptr) buf);
			return(-1);
		}
	}

	/*
	 * restore last record containing END_MF element to the file
	 */
	(void) CGM_write(fd, buf);


	/*
	 *	clean up 
	 */
	(void) CGM_close(tmp_fd);
	(void) CGM_close(fd);		/* close the new file		*/
	if (buf) (void) free((Voidptr) buf);
	return(1);			/* success			*/
}

/*
 *	CGM_mergeFrames
 *	PUBLIC
 *		Overlay one frame on top of another. This function may 
 *	not always produce the desired results. The attributes of the 
 *	"bottom" frame will preside over the attributes of the "top" frame. 
 * on entry
 *	bottom		: number of frame to go on the "bottom"
 *	top		: number of frame to go on the "top"
 * on exit
 *	return		: a pointer to the directory representing the modified
 *			  state of the file on success, else return NULL
 */
Directory	*CGM_mergeFrames(bottom, top)
	unsigned	bottom, top;
{

	Cgm_fd	t_fd, b_fd;	/* file descriptors for files with
				 * top and bottom frames
				 */
	Instr	instr;		/* stores a CGM instruction	*/
	int	frame_size = 0;	/* size of newly created frame	*/
	int	growth;		/* how much bigger new frame is	*/

	int	i;

	int	r = ABS(recordSize);

	errno = 0;
	/*
	 *	make sure top and bottom are valid frames
	 */
	if (bottom >= workingList.number || top >= workingList.number)
		return(ERR);

	/*
	 * open file with bottom frame for reading. set file ptr to frame
	 */
	if (workingList.list[bottom].utype == ACTUAL) {
		(void) CGM_flush(workingList.tmp_fd);
		if ((b_fd = CGM_open(ncarCgm, recordSize, "r")) < 0) {
			return (ERR);
		}
		if (CGM_lseek(b_fd, saveDir->d[workingList.list[bottom]
			.uval.frame].record, SEEK_SET) < 0)

			return(ERR);

	}
	else {	/* open tmp file for *reading*	*/
		(void) CGM_flush(workingList.tmp_fd);
		if ((b_fd = CGM_open(tempFile, r, "r")) < 0) {
			return (ERR);
		}
		if (CGM_lseek(b_fd,workingList.list[bottom].
			uval.frame_entry.start_rec, SEEK_SET) < 0) {

		(void) CGM_close(b_fd);
		return(ERR);
		}
	}

	/*
	 * open file with top frame for reading. set file ptr to frame
	 */
	if (workingList.list[top].utype == ACTUAL) {
		(void) CGM_flush(workingList.tmp_fd);
		if ((t_fd = CGM_open(ncarCgm, recordSize, "r")) < 0) {
			(void) CGM_close(b_fd);
			return (ERR);
		}
		if (CGM_lseek(t_fd, saveDir->d[workingList.list[top]
			.uval.frame] .record, SEEK_SET) < 0) {

			(void) CGM_close(b_fd);
			(void) CGM_close(t_fd);
			return(ERR);
		}

	}
	else {	/* open tmp file for *reading*	*/
		(void) CGM_flush(workingList.tmp_fd);
		if ((t_fd = CGM_open(tempFile, r, "r")) < 0) {
			(void) CGM_close(b_fd);
			return (ERR);
		}
		if (CGM_lseek(t_fd, workingList.list[top]
			.uval.frame_entry.start_rec, SEEK_SET) < 0) {

		(void) CGM_close(b_fd);
		(void) CGM_close(t_fd);
		return(ERR);
		}
	}

	/* 
	 * copy elements from "bottom" frame to tmp file until an
	 * end picture element is found
	 */
	while(1) {
		if (CGM_getInstr(b_fd, &instr) < 0) {
			(void) CGM_close(b_fd);
			(void) CGM_close(t_fd);
			return(ERR);
		}
		if (instr.cgmclass == DEL_ELEMENT && instr.id == END_PIC_ID)
			break;

		if ((i = CGM_putInstr(workingList.tmp_fd, &instr)) < 0) {
			(void) CGM_close(b_fd);
			(void) CGM_close(t_fd);
			return(ERR);
		}
		frame_size += i;
	}
	(void) CGM_close(b_fd);
	/*
	 * advance past "begin picture body" instruction in "top" frame
	 */
	do {
		if (CGM_getInstr(t_fd, &instr) < 0) {
			(void) CGM_close(t_fd);
			return(ERR);
		}
	} while (!((instr.cgmclass == DEL_ELEMENT) && (instr.id == BEG_PIC_B_ID)));

	/* 
	 * copy elements from "top" frame to tmp file until after an
	 * end picture element is found
	 */
	do {
		if (CGM_getInstr(t_fd, &instr) < 0) {
			(void) CGM_close(t_fd);
			return(ERR);
		}
		if ((i = CGM_putInstr(workingList.tmp_fd, &instr)) < 0) {
			(void) CGM_close(t_fd);
			return(ERR);
		}
		frame_size += i;
		
	} while (!((instr.cgmclass == DEL_ELEMENT) && (instr.id == END_PIC_ID))); 
	(void) CGM_flushOutputInstr(workingList.tmp_fd);
	frame_size++;
	(void) CGM_close(t_fd);

	/*
	 * the new merged frame has been created. now update
	 * the working list and directory
	 */
	growth = frame_size - workingDir->d[bottom].num_record;

	/*
	 * the new frame is now in the tmp file. So keep track of it
	 */
	workingList.list[bottom].utype = REFERENCE;
	workingList.list[bottom].uval.frame_entry.start_rec = 
						workingList.offset;
	workingList.list[bottom].uval.frame_entry.num_rec =  frame_size;

	/*
	 * update merged frames size
	 */
	workingDir->d[bottom].num_record = frame_size;

	/*
	 * advance status of position of all frames after the new bigger
	 * merged frame
	 */
	for (i = bottom + 1; i < workingList.number; i++) { 
		workingDir->d[i].record += growth;
	}
	workingList.offset += frame_size;

	return(workingDir);
}
/*
 *	CGM_editFrame
 *	PUBLIC
 *
 *		Edit a single instruction or multiple occurences of the
 *	same instruction. CGM_editFrame replaces num_occur occurences of 
 *	the CGM element data identified in edit_instr with the data contents
 *	of edit_instr
 * on entry
 *	frame		: number of frame to edit
 *	*edit_instr	: contains cgm element to replace
 *	num_occur	: number of occurences of the cgm element to 
 *			  replace. -1 => replace all occurences
 * on exit
 */
Directory	*CGM_editFrame(frame, edit_instr, num_occur)
	unsigned	frame;
	Instr		*edit_instr;
	int		num_occur;
{

	Cgm_fd	fd;		/* file descriptors for files with
				 * top and bottom frames
				 */
	Instr	instr;		/* stores a CGM instruction	*/
	int	frame_size = 0;	/* size of newly created frame	*/
	int	growth;		/* the change in size by records of new frame */

	int	i;
	int	r = ABS(recordSize);

	errno = 0;
	/*
	 *	make sure top and bottom are valid frames
	 */
	if (frame >= workingList.number) 
		return(ERR);

	/*
	 * open file containing frame for reading
	 */
	if (workingList.list[frame].utype == ACTUAL) {
		(void) CGM_flush(workingList.tmp_fd);
		if ((fd = CGM_open(ncarCgm, recordSize, "r")) < 0) {
			return (ERR);
		}
		if (CGM_lseek(fd, saveDir->d[workingList.list[frame]
			.uval.frame].record, SEEK_SET) < 0) {

			return(ERR);
		}

	}
	else {	/* open tmp file for *reading*	*/
		(void) CGM_flush(workingList.tmp_fd);
		if ((fd = CGM_open(tempFile, r, "r")) < 0) {
			return (ERR);
		}
		if (CGM_lseek(fd, workingList.list[frame]
			.uval.frame_entry.start_rec, SEEK_SET) < 0) {

		(void) CGM_close(fd);
		return(ERR);
	}
	}

	/* 
	 * copy elements from edit frame to temp file. when a matching
	 * CGM instruction is found replace it with edit_instr. Stop when
	 * we have cycled through all elements in the frame.
	 */
	do {
		if (CGM_getInstr(fd, &instr) < 0) {	/* get an instr */
			(void) CGM_close(fd);
			return(ERR);
		}

		/*
		 * if we have a match and num_occur != 0 replace old 
 		 * instruction with new. not if num_occur was negative on 
		 * entry we will replace all occurences
		 */	
		if (instr.cgmclass == edit_instr->cgmclass 
			&& instr.id == edit_instr->id 
			&& num_occur !=0) {

			if ((i = CGM_putInstr (	/* write edit_instr	*/
				workingList.tmp_fd, edit_instr)) < 0) {

				(void) CGM_close(fd);
				return(ERR);
			}
			num_occur--;
		}	
		else {	/* just copy the instruction	*/

			if ((i = CGM_putInstr(workingList.tmp_fd, &instr))< 0){
				(void) CGM_close(fd);
				return(ERR);
			}
		}
		frame_size += i; /* keep track of num of recordss in frame */
	} while (!((instr.cgmclass == DEL_ELEMENT) && (instr.id == END_PIC_ID))); 
		
	/*
	 * flusth the output buffer to the tmp file
	 */
	(void) CGM_flushOutputInstr(workingList.tmp_fd);
	frame_size++;
	(void) CGM_close(fd);
	/*
	 * the new edited frame has been created. now update
	 * the working list and directory
	 */
	growth = frame_size - workingDir->d[frame].num_record;

	/*
	 * the new frame is now in the tmp file. So keep track of it
	 */
	workingList.list[frame].utype = REFERENCE;
	workingList.list[frame].uval.frame_entry.start_rec = 
						workingList.offset;
	workingList.list[frame].uval.frame_entry.num_rec =  frame_size;

	/*
	 * update merged frames size
	 */
	workingDir->d[frame].num_record = frame_size;

	/*
	 * if the edited element was a Begin Picture element we need
	 * to update the working directory textual description of that
	 * frame. The textual description comes from the BEG PIC data.
	 */
	if (edit_instr->cgmclass == DEL_ELEMENT && edit_instr->id == BEG_PIC_ID) {
		if (workingDir->d[frame].text != NULL) {
			(void) free ((Voidptr) workingDir->d[frame].text);
		}

		workingDir->d[frame].text = malloc (edit_instr->data_length);
		if (! workingDir->d[frame].text) {
			return((Directory *) NULL);
		}
		memmove((void *)workingDir->d[frame].text,(const void *) &edit_instr->buf[1], 
			(int) edit_instr->data_length-1);

		workingDir->d[frame].text[edit_instr->data_length] = '\0';
	}

	/*
	 * advance status of position of all frames after the new bigger
	 * merged frame
	 */
	for (i = frame + 1; i < workingList.number; i++) { 
		workingDir->d[i].record += growth;
	}
	workingList.offset += frame_size;

	return(workingDir);
}
