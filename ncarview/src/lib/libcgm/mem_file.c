/*
 *	$Id: mem_file.c,v 1.5 1992-03-12 22:14:56 clyne Exp $
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
#include <fcntl.h>
#include <errno.h>
#include <ncarv.h>
#include <cgm_tools.h>
#include "mem_file.h"

/*LINTLIBRARY*/

extern	char	*realloc();
extern	char	*strcpy();

static	CGM_iobuf	cgm_iobuf[MAX_MEM_FILE];

static
unsigned long   usedFd = 0;     /* bitmap of used file descriptors      */
static
unsigned int    numOpen = 0;    /* number of files currently open       */


CGM_openMemFile(metafile, record_size, type)
	char	*metafile;
	int	record_size;
	char	*type;
{

	int	i,
		mask,
		index;

	int	old_index = -1;


	if (!metafile) {
		errno = EACCES;
		return(-1);     /* no file      */
	}

	/*
	 * make sure have enough file descriptors
	 */
	if (numOpen >= MAX_MEM_FILE) {
		errno = EMFILE;
		return(-1);
	}

	/*
	 * see if file already exists
	 */
	for (i = 0, mask = 1; i < MAX_MEM_FILE; i++, mask += mask) {
		if (usedFd & mask) {
			if (strcmp(metafile, cgm_iobuf[i].name) == 0) {
				old_index = i;	/* file exists	*/
				break;
			}
		}
	}

	/*
	 * check some flags based on state of file
	 */
	if (old_index == -1) {	
		/* 
		 * file does not exists	
		 */
		if (! (strcmp(type, "r")) || ! (strcmp(type, "r+"))) {
			errno = ENOENT;
			return(-1);
		}
	}


	/* 
	 * find a free index
	 */
	for(index = 0; index<MAX_MEM_FILE && ((usedFd >> index) & 1); index++);


	/*
	 * add a reference to the file in the file descriptor table.
	 * allocate resources if the file does not already exist
	 */
	if (old_index >= 0) {	/* if file exists	*/
		cgm_iobuf[index].name = cgm_iobuf[old_index].name;
		cgm_iobuf[index]._base = cgm_iobuf[old_index]._base;
		cgm_iobuf[index]._ptr = cgm_iobuf[old_index]._base;
		cgm_iobuf[index].size = cgm_iobuf[old_index].size;
		cgm_iobuf[index].len = cgm_iobuf[old_index].len;

		if (record_size != *cgm_iobuf[old_index].r_size) return (-1);
		cgm_iobuf[index].r_size = cgm_iobuf[old_index].r_size;

		/*
		 * if appending move to end of file
		 */
		if (! strcmp(type, "a") || ! strcmp(type, "a+")) {
			cgm_iobuf[index]._ptr += *cgm_iobuf[index].len;
		}
		
		

	}
	else {	/* a new file	*/
		cgm_iobuf[index].name = icMalloc(strlen (metafile) + 1);
		cgm_iobuf[index]._base = (unsigned char *) 
					icMalloc(record_size * F_INIT_SIZE);

		cgm_iobuf[index].size = (long *) icMalloc(sizeof (long));
		cgm_iobuf[index].len = (long *) icMalloc(sizeof (long));
		cgm_iobuf[index].r_size = (int *) icMalloc(sizeof (int));

		cgm_iobuf[index]._ptr = cgm_iobuf[index]._base;
		*cgm_iobuf[index].size = record_size * F_INIT_SIZE;
		(void) strcpy( cgm_iobuf[index].name, metafile);
		*cgm_iobuf[index].len = 0;
		*cgm_iobuf[index].r_size = record_size;
	}

	/*
	 * truncate file if opened with "w" or "w+"
	 */
	if (! strcmp(type, "w") || ! strcmp(type, "w+")) {

		*cgm_iobuf[index].len = 0;
		cgm_iobuf[index]._ptr = cgm_iobuf[index]._base;

	}


	numOpen++;
	usedFd |= (1 << index);	/* update bitmap to include new addition*/
	return(index);		/* return users file descriptor         */
}

CGM_readMemFile(fd, buf)
	int	fd;
	unsigned char	*buf;
{

	int	r = (int) *cgm_iobuf[fd].r_size;

	/*
	 * make sure not at end of file
	 */
	if ((*cgm_iobuf[fd].len ==  (long)
		(cgm_iobuf[fd]._ptr - cgm_iobuf[fd]._base))) {

		return (0);     /* EOF  */
	}
	else {

		bcopy((char *) cgm_iobuf[fd]._ptr, (char *) buf, r);
		cgm_iobuf[fd]._ptr += r;
		return(r);
	}
}


CGM_writeMemFile(fd, buf)
	int	fd;
	unsigned char	*buf;
{

	int	r = (int) *cgm_iobuf[fd].r_size;
	unsigned long offset;

	char	*ptr;

	offset = (long) (cgm_iobuf[fd]._ptr - cgm_iobuf[fd]._base);
	/*
	 * see if at end of file and need more space.
	 */
	if (*cgm_iobuf[fd].size ==  (long) (offset)) {

		ptr = (char *) cgm_iobuf[fd]._base;
		ptr = realloc(ptr,
			(unsigned) (*cgm_iobuf[fd].size + (r * F_INIT_SIZE)));

		if (ptr == NULL) {
			errno = ENOSPC;
			return (-1);
		}

		cgm_iobuf[fd]._base = (unsigned char *) ptr;
		cgm_iobuf[fd]._ptr = cgm_iobuf[fd]._base + offset;
		*cgm_iobuf[fd].size +=  (r * F_INIT_SIZE);
	}

	/*
	 * write to file
	 */
	bcopy((char *) buf, (char *) cgm_iobuf[fd]._ptr, r);
	cgm_iobuf[fd]._ptr += r;

	/*
	 * update file length if necessary
	 */
	if ((cgm_iobuf[fd]._ptr - cgm_iobuf[fd]._base) > *cgm_iobuf[fd].len) {
		
		*cgm_iobuf[fd].len = (long) 
			(cgm_iobuf[fd]._ptr - cgm_iobuf[fd]._base);

	}
	return(r);
}


CGM_lseekMemFile(fd, offset, whence)
	int	fd;
	int	offset;
	int	whence;
{

	int	r = (int) *cgm_iobuf[fd].r_size;
	char	*ptr;

	long	off;	/* offset from base in bytes	*/

	switch (whence) {
	case L_SET:	/* from base			*/
		off = (offset * r);
		break;

	case L_INCR:	/* from current location	*/
		off = cgm_iobuf[fd]._ptr - cgm_iobuf[fd]._base + (offset * r);
		break;

	case L_XTND:	/* from end of file		*/
		off = *cgm_iobuf[fd].len + (offset * r);
		break;
		
	default:
		errno = EINVAL;
		return(-1);
	}

	if (off < 0) {
		errno = ENOSPC;
		return (-1);	/* can't seek back past file base */
	}

	/*
	 * if off exceeds file space allocate more memory
	 */
	if (off > *cgm_iobuf[fd].size) {
		ptr = (char *) cgm_iobuf[fd]._base;
		ptr = realloc(ptr, (unsigned) (off * sizeof (unsigned char *)));

		if (ptr == NULL) {
			errno = ENOSPC;
			return (-1);
		}

		cgm_iobuf[fd]._base = (unsigned char *) ptr;
		*cgm_iobuf[fd].size =  off;
	}

	cgm_iobuf[fd]._ptr = cgm_iobuf[fd]._base + off;

	/*
	 * update length if file grew
	 */
	if (off > *cgm_iobuf[fd].len) {
		*cgm_iobuf[fd].len = off;
	}

	return ((int) off);
}

CGM_closeMemFile(fd)
	int	fd;
{
	int	i;
	unsigned int	mask;
	int	count;

	char	*metafile;

	/*
	 * count number of occurences of the file in the descriptor table.
	 * if there is only one we can't do anything
	 */

	metafile = cgm_iobuf[fd].name;

	for (i = 0, count = 0, mask = 1; i < MAX_MEM_FILE; i++, mask += mask) {
		if (usedFd & mask) {
			if (strcmp(metafile, cgm_iobuf[i].name) == 0) {
				count++;	/* file exists	*/
			}
		}
	}

	if (count == 0) {
		errno = EBADF;
		return (-1);
	}

	if (count == 1) return (1);	/* do nothing	*/

	/*
	 * free file descriptor 
	 */
	cgm_iobuf[fd].name	= NULL;
	cgm_iobuf[fd]._base	= (unsigned char *) NULL;
	cgm_iobuf[fd]._ptr	= (unsigned char *) NULL;
	cgm_iobuf[fd].size	= (long *) NULL;
	cgm_iobuf[fd].len	= (long *) NULL;
	cgm_iobuf[fd].r_size	= (int *) NULL; 

	usedFd &= (~(1 << fd));
	numOpen--;
	return(1);
}

CGM_unlinkMemFile(metafile)
	char	*metafile;
{

	int	i, mask;
	int	index = -1;

	
	/*
	 * close every open file descriptor for metafile 
	 */

	for (i = 0, mask = 1; i < MAX_MEM_FILE; i++, mask += mask) {
		if (usedFd & mask) {
			if (strcmp(metafile, cgm_iobuf[i].name) == 0) {
				(void) CGM_closeMemFile(i);
			}
		}
	}

	/*
	 * now there  should be only one reference to the file in the 
	 * descriptor table. Find it and remove its resouces
	 */
	for (i = 0, mask = 1; i < MAX_MEM_FILE; i++, mask += mask) {
		if (usedFd & mask) {
			if (strcmp(metafile, cgm_iobuf[i].name) == 0) {
				index = -1;
				break;
			}
		}
	}

	if (index == -1) {
		errno = EBADF;
		return (-1);
	}

	cfree( (char *) cgm_iobuf[index].name);
	cfree( (char *) cgm_iobuf[index]._base);
	cfree( (char *) cgm_iobuf[index]._ptr);
	cfree( (char *) cgm_iobuf[index].size);
	cfree( (char *) cgm_iobuf[index].len);
	cfree( (char *) cgm_iobuf[index].r_size);


	cgm_iobuf[index].name	= NULL;
	cgm_iobuf[index]._base	= (unsigned char *) NULL;
	cgm_iobuf[index]._ptr	= (unsigned char *) NULL;
	cgm_iobuf[index].size	= (long *) NULL;
	cgm_iobuf[index].len	= (long *) NULL;
	cgm_iobuf[index].r_size	= (int *) NULL; 

	return (index);
}
