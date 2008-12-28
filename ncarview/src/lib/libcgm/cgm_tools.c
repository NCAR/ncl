/*
 *	$Id: cgm_tools.c,v 1.33 2008-12-28 13:31:23 haley Exp $
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

/*
 *
 *	cgm_tools:
 *
 *		author	John Clyne	(clyne@redcloud.ucar.edu)
 *			11/15/88
 *
 *	This module maintains a database of file descriptors for
 *	NCAR CGM's. It provides routines for opening, closing, 
 *	reading and writing metafiles in a protected environment.
 *	It also provides a utility for creating a table of contents
 *	for a metafiles as well a routine for printing the table
 *	of contents to standard out
 *
 * rev 1.01 clyne 2/22/90	: CGM_read would not read from a pipe corectly
 *				if data was slow coming
 * rev 1.02 clyne 4/24/90	: CGM_putInstr produced extra begin meta bits
 * rev 1.03 clyne 6/1/90	: Added memory file capability
 * rev 1.04 grubin 10/2004	: changed sys/file.h to sys/filio.h for FreeBSD
 */
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<errno.h>
#include	<fcntl.h>
#include	<unistd.h>
#include	<sys/types.h>
#ifdef	__FreeBSD__
#include	<sys/filio.h>
#else
#include	<sys/file.h>
#endif	/* __FreeBSD__ */
#include	<ncarg/c.h>
#include	"cgm_tools.h"
#include	"cgmdef.h"
#include	"internals.h"
#include	"mem_file.h"
	
/*LINTLIBRARY*/

static	int	noop()
{
	return(0);
}

/*
 * file descriptor table. Records file type, record size, I/O type,
 * file descriptor(s) and a pointer structure for performing metafile
 * operations
 */
static	Cgm_tab	cgmTab[] = {
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop},
	{0, File, NULL, NULL,-1,-1,(Pg_struct *)NULL, noop,noop,noop,noop,noop} 
	};

/* maximum files allowed open	*/
#define	MAX_FILE (sizeof (cgmTab) / sizeof (Cgm_tab))

static	
unsigned long	usedFd = 0; 	/* bitmap of used file descriptors	*/
static	
unsigned int	numOpen = 0;	/* number of files currently open	*/

static	
char	*headerType[] = {
		"header", "NCAR formated printer", 
		"pre-CGM NCAR", "NCAR CGM", "invalid"
		};		 /* valid frame types in a NCAR CGM	*/



static	int	stream_read(cgm_fd, buf)
	Cgm_fd		cgm_fd;
	unsigned char	*buf;
{
	char *b;
	int	n, l;
	int	r = (int) cgmTab[cgm_fd].record_size;
	FILE	*fp = cgmTab[cgm_fd].fp;

	b=(char *)buf;
	for (n=0; n<r;) {
		if ((l=fread(b+n,1,r-n,fp))<0)
			return(l);
		else if (l==0)
			break;
		n+=l;
	}
	return(n);
}
static	int	raw_read(cgm_fd, buf)
	Cgm_fd		cgm_fd;
	unsigned char	*buf;
{
	char *b;
	int	n, l;
	int	r = (int) cgmTab[cgm_fd].record_size;
	int	fd = cgmTab[cgm_fd].fd;

	b=(char *)buf;
	for (n=0; n<r; ) {
		if ((l=read(fd,b+n,r-n))<0)
			return(l);
		else if (l==0)
			break;
		n+=l;
	}
	return(n);
}
static	int	memory_read(cgm_fd, buf)
	Cgm_fd		cgm_fd;
	unsigned char	*buf;
{
	int	fd = cgmTab[cgm_fd].fd;

	return(CGM_readMemFile(fd, buf));
}

static	int	stream_write(cgm_fd, buf)
	Cgm_fd		cgm_fd;
	unsigned char	*buf;
{
	int	r = cgmTab[cgm_fd].record_size;
	FILE	*fp = cgmTab[cgm_fd].fp;

	return(fwrite((char *) buf, 1, r, fp));
}

static	int	raw_write(cgm_fd, buf)
	Cgm_fd		cgm_fd;
	unsigned char	*buf;
{
	int	r = cgmTab[cgm_fd].record_size;
	int	fd = cgmTab[cgm_fd].fd;

	if (cgmTab[cgm_fd].mtype == Pipe ) {
		fd = cgmTab[cgm_fd].fdw;
	}

	return(write(fd, (char *) buf, r));
}

static	int	memory_write(cgm_fd, buf)
	Cgm_fd		cgm_fd;
	unsigned char	*buf;
{
	int	fd = cgmTab[cgm_fd].fd;

	return (CGM_writeMemFile(fd, buf));
}

static	int	stream_seek(cgm_fd, offset, whence)
	Cgm_fd		cgm_fd;
	long	offset;
	int	whence;
{
	FILE	*fp = cgmTab[cgm_fd].fp;

	return(fseek(fp, offset, whence));
}

static	int	raw_seek(cgm_fd, offset, whence)
	Cgm_fd		cgm_fd;
	long	offset;
	int	whence;
{
	int	fd = cgmTab[cgm_fd].fd;

	return((int) lseek(fd, (off_t) offset, whence));
}
static	int	memory_seek(cgm_fd, offset, whence)
	Cgm_fd		cgm_fd;
	long	offset;
	int	whence;
{
	int	fd = cgmTab[cgm_fd].fd;
	int	r = cgmTab[cgm_fd].record_size;
	int	offset_ = r ? offset / r : 0;

	return (CGM_lseekMemFile(fd, offset_, whence));
}
static	int	stream_close(cgm_fd)
	Cgm_fd		cgm_fd;
{
	FILE	*fp = cgmTab[cgm_fd].fp;

	return(fclose(fp));
}

static	int	stream_flush(cgm_fd)
	Cgm_fd		cgm_fd;
{
	FILE	*fp = cgmTab[cgm_fd].fp;

	return(fflush(fp));
}

static	int	raw_close(cgm_fd)
	Cgm_fd		cgm_fd;
{
	int	fd = cgmTab[cgm_fd].fd;
	int	error;

	error = close(fd);
	if (cgmTab[cgm_fd].mtype == Pipe ) {
		int	fdw = cgmTab[cgm_fd].fdw;
		error -= close(fdw);
	}
	return(error);
}
	
static	int	memory_close(cgm_fd)
	Cgm_fd		cgm_fd;
{
	int	fd = cgmTab[cgm_fd].fd;

	return(CGM_closeMemFile (fd));
}


#ifdef	cray
/*
*	If output is to a standard file, buffered I/O is used. On
*	some systems it is desirable to adjust this and several
*	options for this are available.
*	Buffer size is adjustable as follows:
*
*	1. If environment variable NCARG_GKS_BUFSIZE is set to N
*
*			Buffer Size = N * BUFSIZ / 32 bytes.
*
*	2. Otherwise, if the value of DEFAULT_GKS_BUFSIZE in wks.h
*	   is edited by the Makefile to be non-zero.
*
*			Buffer Size = DEFAULT_GKS_BUFSIZE * BUFSIZ / 32 bytes.
*
*	3. Otherwise, (when DEFAULT_GKS_BUFSIZE is 0)
*
*			Buffer Size = BUFSIZ (from stdio.h)
*
*/
static	int	setvbuf_(fp, r)
	FILE	*fp;
	int	r;
{
	
	int	i;
	int	size;
	char	*bufsize_env;
	int	bufsize;


	/*
	The environment variable NCARG_GKS_BUFSIZE, a macro
	definition, or a constant from stdio.h can determine
	buffer size used. See notes at the top of this file.
	*/

	bufsize_env = getenv("NCARG_GKS_BUFSIZE");
	if (bufsize_env == (char *) NULL) {
		if (DEFAULT_GKS_BUFSIZE == 0) {
			bufsize = BUFSIZ;
		}
		else {
			bufsize = BUFSIZ * DEFAULT_GKS_BUFSIZE / 32;
		}
	}
	else {
		bufsize = BUFSIZ * atoi(bufsize_env) / 32;
		if (bufsize <= 0) {
			bufsize = BUFSIZ;
		}
	}

	/*
	 * find the smallest buffer size that is bigger then r and 
	 * is an integral of bufsize
	 */
	i = r / bufsize;
	i++;
	size = i * bufsize;

	return(setvbuf(fp, (char *) NULL, _IOFBF, size));
}
#endif


/*	CGM_open:
 *	PUBLIC
 *
 *		Open a file and return a file descriptor. Initialize 
 *	CGM_getInstr() and/or CGM_putInstr for use on that file. If the 
 *	record_size arg is negative than metafile is actually the address 
 *	of a CGM that resides in memory. 
 *
 * on entry
 *	metafile	: points to the pathname of a metafile
 *			  if metafile == '-' => use stdin/stdout
 *			  if metafile == '--' => open a pipe
 *			
 *	record_size:	: size in bytes of records in metafile. if Negative
 *			  file is opened in memory, not on disk
 *	type		: one of ("w"|"r"|"a"|"w+"|"r+"|"a+")
 * on exit
 *	cgmTab[cgm_fd]	: has been initialized;
 *	return:		== -1 => error else
 *			a CGM_file descriptor for reading/writing
 *				  
 */
/*VARARGS3*/
Cgm_fd	CGM_open(metafile, record_size, type)
	const char	*metafile;
	int		record_size;
	const char	*type;
{

	int	fildes[2];	/* file descriptor for a pipe	*/
	Cgm_fd	index;		/* index into the cgmTab	*/

#ifdef	SYSV
	int	fd;
	FILE	*fp;
	int	a_mask = O_WRONLY | O_CREAT;
	int	arw_mask = O_RDWR | O_CREAT;
#endif

#ifdef	cray
	int	r_mask = O_RDONLY | O_RAW;
	int	rrw_mask = O_RDWR | O_RAW;
	int	w_mask = O_TRUNC | O_CREAT | O_WRONLY | O_RAW;
	int	wrw_mask = O_TRUNC | O_CREAT | O_RDWR | O_RAW;
	a_mask = O_WRONLY | O_CREAT | O_RAW;
	arw_mask = O_RDWR | O_CREAT | O_RAW;
#endif


	errno = 0;
	if (numOpen >= MAX_FILE) {
		errno = ENOENT;
		return(-1);
	}

	if (!metafile) {
		errno = EACCES;
		return(-1);	/* no file	*/
	}

	/* find a free index	*/
	for(index = 0; index<MAX_FILE  && ((usedFd >> index) & 1); index++); 

	/* record record size for CGM_read/CGM_write	*/
	cgmTab[index].record_size =  (unsigned) 
			(record_size < 0 ? -record_size : record_size);

	/*
	 * see if CGM resides in memory and not on disk
	 */
	if (record_size < 0 ) {

		cgmTab[index].mtype = MemFile;
		if ((cgmTab[index].fd = (int) CGM_openMemFile(metafile, 
			(int) cgmTab[index].record_size, type)) < 0) {

			return (-1);
		}
		cgmTab[index].fp = (FILE *) NULL;
		cgmTab[index].write = memory_write;
		cgmTab[index].read = memory_read;
		cgmTab[index].seek = memory_seek;
		cgmTab[index].close = memory_close;
	} 
	/*
	 * 	see if read/write from stdin/stdout
	 */
	else if (strcmp (metafile, rw_stdin) == 0) {

		cgmTab[index].mtype = Tty;
		if (! (strcmp("r",type)) || ! (strcmp("r+",type))) {
			cgmTab[index].fp = stdin;
			cgmTab[index].fd = fileno(stdin);
		}
		else if (! (strcmp("w",type)) || ! (strcmp("w+",type))
			|| ! (strcmp("a",type)) || ! (strcmp("a+",type))) {

			cgmTab[index].fp = stdout;
			cgmTab[index].fd = fileno(stdout);
		}
		else {
			errno = EINVAL;
			return(-1);
		}
		cgmTab[index].write = stream_write;
		cgmTab[index].read = stream_read;
		cgmTab[index].seek = stream_seek;
		cgmTab[index].close = stream_close;
		cgmTab[index].flush = stream_flush;

#ifdef	cray
		if (setvbuf_(cgmTab[index].fp, record_size) < 0) return(-1);
#endif
	}

	/*
	 *	see if read/write from pipe
	 */
	else if (strcmp (metafile, rw_pipe) == 0) {
	
		cgmTab[index].mtype = Pipe;
		if (pipe(fildes) < 0)  {
			return(-1);
		}

		cgmTab[index].fd = fildes[0];	/* read file descriptor	*/
		cgmTab[index].fdw = fildes[1];	/* write file desctiptor*/
		cgmTab[index].fp = (FILE *) NULL;
		cgmTab[index].fpw = (FILE *) NULL;

		cgmTab[index].write = raw_write;
		cgmTab[index].read = raw_read;
		cgmTab[index].seek = raw_seek;
		cgmTab[index].close = raw_close;

	}

	else {
	/*
	 *	read/write from a file
	 */

		cgmTab[index].mtype = File;

#if defined(SYSV) || defined(cray)
		if (! strcmp(type, "a")) {
			if ((fd = open(metafile,a_mask, 0666)) < 0) return(-1);
				/* Changed from instead passing type to now passing "w". */
			if (! (fp = fdopen(fd, "w"))) return(-1);
		}
		else if (! strcmp(type, "a+")) {
			if ((fd = open(metafile, arw_mask,0666))< 0) return(-1);
			if (!(fp = fdopen(fd, type))) return(-1);
		}
#ifdef	cray
		else if (! strcmp(type, "r")) {
			if ((fd = open(metafile, r_mask, 0666))< 0) return(-1);
			if (!(fp = fdopen(fd, type))) return(-1);
		}
		else if (! strcmp(type, "r+")) {
			if ((fd = open(metafile, rrw_mask, 0666))< 0)return(-1);
			if (!(fp = fdopen(fd, type))) return(-1);
		}
		else if (! strcmp(type, "w")) {
			if ((fd = open(metafile, w_mask, 0666))< 0) return(-1);
			if (!(fp = fdopen(fd, type))) return(-1);
		}
		else if (! strcmp(type, "w+")) {
			if ((fd = open(metafile, wrw_mask, 0666))< 0)return(-1);
			if (!(fp = fdopen(fd, type))) return(-1);
		}
#else	/* cray	*/
		else {
			if (!(fp = fopen(metafile, type))) return(-1);
			if ((fd = fileno(fp)) < 0)  return(-1);
		}

#endif	/* cray	*/
		cgmTab[index].fp = fp;
		cgmTab[index].fd = fd;
#else
		if ((cgmTab[index].fp = fopen(metafile, type)) == NULL) {
			return(-1);
		}
		cgmTab[index].fd = fileno(cgmTab[index].fp);
#endif

		cgmTab[index].write = stream_write;
		cgmTab[index].read = stream_read;
		cgmTab[index].seek = stream_seek;
		cgmTab[index].close = stream_close;
		cgmTab[index].flush = stream_flush;

#ifdef	cray
		if (setvbuf_(cgmTab[index].fp, record_size) < 0) return(-1);
#endif
	}

	/*
	 * malloc space for use by CGM_getInstr() and/or CGM_putInstr()
	 * and intialize flags in the Pg_struct;
	 */
	cgmTab[index].pg_struct = (Pg_struct *) malloc(sizeof(Pg_struct));
	if (! cgmTab[index].pg_struct) {
		return(-1);
	}

	cgmTab[index].pg_struct->buf = (unsigned char *) malloc (
		(cgmTab[index].record_size * sizeof(unsigned char))
	);
	if (! cgmTab[index].pg_struct->buf) {
		return(-1);
	}


	/*
	 * intialize the Pg_struct for this cgm_Fd
	 */
	cgmTab[index].pg_struct->buf_ptr = 
				cgmTab[index].pg_struct->buf + HEADERSIZE;

	cgmTab[index].pg_struct->byte_count	= 
	cgmTab[index].pg_struct->over_flow	=
	cgmTab[index].pg_struct->more		= 
	cgmTab[index].pg_struct->new_frame	=
	cgmTab[index].pg_struct->beg_meta	= 
	cgmTab[index].pg_struct->end_meta	= 0;

	numOpen++;
	usedFd |= (1 << index);	/* update bitmap to include new addition*/	
	return(index);		/* return users file descriptor		*/
}



/*	CGM_close:
 *	PUBLIC
 *
 *		close a file 
 *
 * on entry
 *	cgm_fd		: is a valid entry into the cgmTab created by CGM_open
 *
 * on exit
 *	return		: < 0 => error closeing file else
 *			  file described by fd is closed
 */
int	CGM_close(cgm_fd)
	Cgm_fd	cgm_fd;
{
	int	error = 0;

	errno = 0;
	if (!(usedFd & (1 << cgm_fd))) {
		errno = EBADF;
		return(-1);	/* invalid file descriptor	*/
	}	

	error = (cgmTab[cgm_fd].close(cgm_fd));

	/*	
	 *	mark index into cgmTab as being open so a new file can
	 *	use it
	 */
	if (!error) {
		usedFd &= (~(1 << cgm_fd));
		numOpen--;
	}

	/*
	 * free structures for get/put instr
	 */
	(void) free((Voidptr) cgmTab[cgm_fd].pg_struct->buf);
	(void) free((Voidptr) cgmTab[cgm_fd].pg_struct);

	return(error);
}

/*	CGM_read:
 *	PUBLIC
 *
 *		read a single record from the metafile
 *
 * on entry
 *	cgm_fd		: valid file descriptor for reading returned by CGM_open
 * on exit
 *	buf		: points to record read in if successful
 *	return		: == numbytes write in if successful else
 *			  == -1 if an error occured
 */
CGM_read(cgm_fd, buf)
	Cgm_fd		cgm_fd;
	unsigned char	*buf;
{
	errno = 0;
	return(cgmTab[cgm_fd].read(cgm_fd, buf));
}



/*	CGM_write:
 *	PUBLIC
 *
 *		write a single record to the metafile
 *
 * on entry
 *	cgm_fd		: valid file descriptor for writing returned by CGM_open
 *	buf		: points to record to write
 * on exit
 *	return		: == numbytes written if successful else
 *			  == -1 if an error occured
 */
CGM_write(cgm_fd, buf)
	Cgm_fd		cgm_fd;
	const unsigned char	*buf;
{
	errno = 0;
	return(cgmTab[cgm_fd].write(cgm_fd, buf));
}

/*	CGM_lseek:
 *	PUBLIC
 *
 *		advance the file pointer to the specified offset
 *
 * on entry
 *	cgm_fd		: valid file descriptor for *reading* returned 
 *			  by CGM_open
 *	offset		: record number to go to.
 *	whence		: one of (SEEK_SET, SEEK_CUR, SEEK_END)
 * on exit
 *	return		: == numbytes write in if successful else
 *			  == -1 if an error occured
 */
CGM_lseek(cgm_fd, offset, whence)
	Cgm_fd		cgm_fd;
	int	offset;
	int	whence;
{
	int	r = cgmTab[cgm_fd].record_size;
	long 	offset_ = r * offset;

	errno = 0;
	return(cgmTab[cgm_fd].seek(cgm_fd, offset_, whence));
}

/*
 *	CGM_flush()
 *
 *	writes any unwritten data for an output  stream associated  or
 *      an  update stream in which the most recent operation was not
 *	input to be delivered to the host environment to  the  file;
 *	otherwise it is ignored.  The named stream remains open.
 *
 * on entry
 *	cgm_fd		: a valid CGM file descriptor.
 * on exit
 *	return		  == -1 if an error occured
 */
CGM_flush(cgm_fd)
	Cgm_fd	cgm_fd;
{
	errno = 0;
	return(cgmTab[cgm_fd].flush(cgm_fd));
}




/*	CGM_directory:
 *	PUBLIC
 *
 *		create a metafile directory containing a list of all frames,
 *		their location in the metafile, each frames type, a the status
 *		of the metafile (whether errors were encountered) and 
 *		optionally a  textual description of the metafile if it exist, 
 *		and a textual description of each frame if it exist.
 *
 * on entry
 *	cgm_fd		: a file descriptor of a metafile created with CGM_open
 *			  the file pointer is at the begining of the file
 * on exit
 *	return		: == NULL => error creating directory, else
 *			  points to a directory
 */	
Directory	*CGM_directory(cgm_fd, fp)
	Cgm_fd	cgm_fd;
	FILE	*fp;
{
	int	frame_count = 0;
	Directory *init_dir();

	enum {
		BEGIN,
		END
	} state = END;			/* record state of metafile	*/

	Directory	*dir = NULL;	/* the directory to creat	*/
	int	error;
	int	record = 0;		/* current record in CGM	*/

	unsigned data_len;
	unsigned tmp;
	unsigned char	*cptr;
	unsigned char	*buf = NULL;	/* buffer for read	*/

	Directory	*ReallocDir();

	/*
	 *	see if file descriptor is valid
	 */
	errno = 0;
	if (cgm_fd < 0 || cgm_fd >= MAX_FILE) {
		errno = EBADF;
		return(dir);
	}

	/* 
	 *	malloc memory for buffer and directory
	 */
	buf = (unsigned char *) malloc (cgmTab[cgm_fd].record_size * 
			sizeof( unsigned char));
	if (! buf) {
		return((Directory *) NULL);
	}


	if ((dir = init_dir()) == NULL )
		return(NULL);

	dir->cgm_fd = cgm_fd;	/* record the file descriptor	*/

	/*
	 *	parse until the end of file or an error
	 */
	if (fp) (void) fprintf(fp, "Building CGM table of contents\n");
	while ((error = CGM_read(cgm_fd, buf)) > 0) {

		/* 
		 * 	look for Begin Metafile bit. See Section on the NCAR
		 *	CGM in the NCAR Graphics installers guide
		 */
		if (GETBITS(buf[2], BEG_MF_POS, LEN))  {

			/*	see if we are in correct state	*/
			if (state != END) {
				(void) fprintf(stderr, 
					"%s : error in Begin meta bit\n", NAME);
				break;
			}

			/*	see if we have enough mem	*/
			if (dir->num_meta >= dir->meta_size)  {

				dir->meta_size += DIR_2_ALLOC;
				dir->meta = (int *) realloc 
					((char *) dir->meta, 
					(unsigned)dir->meta_size * sizeof(int));
				if (! dir->meta) {
					return((Directory *) NULL);
				}
			}
				

			state = BEGIN; 		/* change state	*/

			dir->MFDescription[dir->num_meta] = NULL;
			dir->meta[dir->num_meta] = record;
			dir->num_meta++;
		}
				

		/* 
		 * 	look for new frame bit. 
		 */
		if (GETBITS(buf[2], FRAME_POS, LEN )) {

			frame_count++;
			if (fp) {
				if ((frame_count % 50) == 0) {
					(void) fprintf(
						fp,
						"	Read %d frames\n", 
						frame_count
					);
				}
			}

			/* see if room in directory	*/
			if (dir->num_frames >= dir->dir_size) {

				/* alloc more space	*/

				(void) ReallocDir(dir,
					(unsigned) dir->dir_size + DIR_2_ALLOC);


			}

			/*
			 * extract the cgm beg. pic. description string. this
			 * code relies on the beg. pic. element being the first
			 * element in the record.
			 */
			cptr = buf + HEADERSIZE;
			tmp = cptr[0] << 8 | cptr[1];
			cptr += COMM_SIZE;
			/*
			 * make sure first element is beg pic
			 */
			if (GETBITS(tmp, CLASS_POSS, CLASS_BITS) 
					== DEL_ELEMENT && 
					GETBITS(tmp, ID_POSS, ID_BITS )
					== BEG_PIC_ID) {

				data_len = GETBITS(tmp, PARM_POSS, PARM_BITS);

				if (data_len == LONGFORM) {
					tmp = cptr[0] << 8 | cptr[1];
					data_len = 
					GETBITS(tmp,PARM_POSS_L,PARM_BITS_L);
					cptr += COMM_SIZE;
				}

				cptr++;	/* skip past char count	*/
				dir->d[dir->num_frames].text = (char *) 
					malloc(data_len);
				if (! dir->d[dir->num_frames].text) {
					return((Directory *) NULL);
				}

				memmove((void *)dir->d[dir->num_frames].text,
					(const void *) cptr, 
					(size_t) data_len-1);

				dir->d[dir->num_frames].text[data_len-1] = '\0';
			}
			else {
				dir->d[dir->num_frames].text = NULL;
			}

			dir->d[dir->num_frames].record = record;

			/* extract frame type from record header	*/
			dir->d[dir->num_frames].type = 
				GETBITS(buf[2],TYPE_POS,TYPE_LEN);

			/*
			 *	calculate the number of records in the previous
			 *	frame
			 */
			if (dir->num_frames) {
				dir->d[dir->num_frames - 1].num_record = 
					record - 
					dir->d[dir->num_frames - 1].record;
			}

			dir->num_frames++;
		}

		/*
		 *	look for end metafile bit
		 */
		if (GETBITS(buf[2], END_MF_POS, LEN ))  {
			if (state != BEGIN) {
				(void) fprintf(stderr, 
					"%s : error in End meta bit\n", NAME);
				
				error = -1;
				break;
			}

			/*
			 *	calculate number of records in LAST frame
			 */
			dir->d[dir->num_frames - 1].num_record = 
				record - dir->d[dir->num_frames - 1].record;

			state = END;
		}

			
	record++;
	}
	if (fp) (void) fprintf(fp,"Read %d frames\n", frame_count);

	/*
	 *	reset the file pointer to the beginning of the file
	 */
	(void) CGM_lseek(cgm_fd, 0, SEEK_SET);

	if (error == 0 && state == END)
		dir->status = CGM_OK;
		
	(void) free ((Voidptr) buf);
	return(dir);

}


/*	MFPrintDirectory
 *	PUBLIC
 *
 *		prints out the contents of a directory created with
 *		MFDirectory.
 *
 * on entry 
 *	dir		: a pointer to a directory created with MFDirectory
 */
void	CGM_printDirectory(dir)
	const Directory	*dir;
{
	int	i, j, k;


	if (dir == NULL) {
		(void) fprintf(stderr, "%s : invalid directory\n", NAME);
		return;
	}

	(void) fprintf(stdout, 
		"file contains %d frames in %d metafile(s) ",
		dir->num_frames, dir->num_meta);
		

	if (dir->status == CGM_OK)
		(void) fprintf(stdout, "status : CGM_OK\n");
	else
		(void) fprintf(stdout, "status : CGM_ERROR\n");




	for (k=0; k < dir->num_meta; k++ ) {
		(void) fprintf(stdout, 
			"\nmetafile %d starts record %d descriptor : [%s]\n\n",
			k, dir->meta[k], 
			dir->MFDescription[k] ? dir->MFDescription[k] : "null"
		);


		/*
		 *	print info about each frame
		 */
		for (i = 0; i < dir->num_frames; i++) {

			switch (dir->d[i].type) {
				case	HEADER:
					j = 0;
					break;
				case	PRINTER:
					j = 1;
					break;
				case	PRE_CGM:
					j = 2;
					break;
				case	NCAR_CGM:
					j = 3;
					break;
				default	:
					j = 4;
					break;
			}

			(void) fprintf(stdout,
				"	frame: %d, descriptor: [%s], first record: %d,\n		contains %d records, type : %s\n",
				i, dir->d[i].text, dir->d[i].record, 
				dir->d[i].num_record, headerType[j]);
		}
	}

}

/*	fetch_input
 *	PRIVATE
 *		read the next valid binary NCAR CGM record in to the input 
 *	buffer. Other valid encodings (header, pre-cgm and ncar-formated
 *	printer) are skipped and ignored.
 *
 * on entry
 *	cgm_fd		: a Cgm_fd open for reading
 *	*pg		: pointer to the Pg_struct for the cgm_fd
 * on exit
 *	return		: number of bytes of useful data in buf
 */
static	fetch_input(cgm_fd, pg)
	Cgm_fd		cgm_fd;
	Pg_struct	*pg;
{
	int		data_count;
	register unsigned	tmp = 0;
	/*
	 *	do until data_count is not zero or EOF or error
	 */
	do {
		if ((data_count = CGM_read(cgm_fd, pg->buf)) < 1) {
			return(data_count);
		}
		pg->buf_ptr = pg->buf;

		

		/*
		 * make sure valid NCAR CGM. Only record type supported now
		 */
		switch (GETBITS(pg->buf_ptr[2],TYPE_POS,TYPE_LEN)) {

		case	NCAR_CGM:
			break;

		case	HEADER:
		case	PRINTER:
		case	PRE_CGM:
			data_count = 0;
			continue;
		default:
			return(-1);
		}
		
		/*
		 * each record contains a valid byte count 
		 * in first two bytes
		 */
		tmp = pg->buf_ptr[0] << 8 | pg->buf_ptr[1];
		data_count = GETBITS(tmp, COUNT_POS, COUNT_LEN);
		/*
		 * advance the buf pointer past the NCAR CGM header
		 */
		pg->buf_ptr += HEADERSIZE;
	} while (data_count < 1);

	return(data_count);
}



/*	CGM_getInstr:
 *	PUBLIC
 *
 *		Fetch the next instruction from the metafile. CGM_getInstr
 *	reads in a single CGM instruction and its data from the previously
 *	opened metafile. CGM_getInstr advances the file pointer as necessary
 *	In the event that the CGM element's partition flag is set only 32760
 *	bytes of data will be returned at a time. The maximum allowed by
 *	the CGM standard is 32767 but thats a very ugly number. 32760 
 *	(MAX_CGM_INS_LEN ) is divisible by 8. 
 *	The next
 *	invocation of CGM_getInstr will return the rest of the data up to 
 *	the same limit. The process is repeated until the partition flag
 *	is no longer set. The boolean instr->more indicates the status
 *	of the partition flag
 * on entry
 *	cgm_fd		: file descriptor for the metafile opened for reading
 * on exit
 *	*instr		: contains a CGM instruction if EOF has not been reached
 *		cgmclass: CGM class
 *		id	: the cgm element id
 *		buf	: the data buffer
 *		data	: pointer to begining of buf
 *		data_len: num bytes of valid data in buf, <= 32760
 *		more	: true if not there is more data to come for same cgm
 *	return		: return > 0 => succuess, == 0 => EOF, < 0 => error
 */
int	CGM_getInstr(cgm_fd, instr)
	Cgm_fd	cgm_fd;
	Instr	*instr;
{

	register unsigned int	tmp;
	int	num_need;
	int	num_copy;
	register Pg_struct	*pg = cgmTab[cgm_fd].pg_struct;
	
	instr->more = FALSE;	/* reset to normal instruction	*/
	instr->data_length = 0;	/* reset data count		*/
	instr->data = instr->buf;

	/*
	 * if the CGM partition flag was set on the previous invocation
	 * we did not return all of the data on last call, copy that
	 * data to beginning of the data buffer
	 */
	errno = 0;
	if (pg->over_flow) {
		(void) memmove((void *) instr->data,
				(const void *)(instr->data + MAX_CGM_INS_LEN),(size_t) pg->over_flow);

		instr->data += pg->over_flow;
	}

	/*
	 * if more or over_flow is set then only extract data, 
	 * not the command itself
	 */
	if (!pg->more && !pg->over_flow) {
		/*
		 * extract class, command and parameter length
		 */
		if (pg->byte_count < COMM_SIZE) {/* make sure data in buf */
			if ((pg->byte_count = fetch_input(cgm_fd, pg)) < 1)
				return(pg->byte_count);
		}
		tmp = pg->buf_ptr[0] << 8 | pg->buf_ptr[1];
		instr->cgmclass = GETBITS(tmp, CLASS_POSS, CLASS_BITS);
		instr->id = GETBITS(tmp, ID_POSS, ID_BITS);
		instr->data_length = GETBITS(tmp, PARM_POSS, PARM_BITS);

		/*
		 * advance to next field, decrement available data count
		 */
		pg->buf_ptr += COMM_SIZE;
		pg->byte_count -= COMM_SIZE;

	} /* if !more */
	else {
		/*
		 * this is a hack to make sure that the long instruction
		 * code is invoked as flaged by more. However, if 
		 * more is false and over_flow is true we don't 
		 * want to extract ANY data. This will be accomplished
		 * since instr.data_length is set to zero at the top
		 */
		if (pg->more) {
			instr->data_length = LONGFORM;	/* more flag was set*/
		}
	}
	

	/*
	 * see if we have a long instruction
	 */
	if (instr->data_length == LONGFORM) {
		if (pg->byte_count < COMM_SIZE) {/* make sure data in buffer */
			if ((pg->byte_count = fetch_input(cgm_fd, pg)) < 1)
				return(pg->byte_count);
		}
		tmp = pg->buf_ptr[0] << 8 | pg->buf_ptr[1];
		instr->data_length = GETBITS(tmp, PARM_POSS_L, PARM_BITS_L);
		instr->more = pg->more = GETBITS(tmp, P_POSS, P_BITS);

		/*
		 * advance to next field
		 */
		pg->buf_ptr += COMM_SIZE;
		pg->byte_count -= COMM_SIZE;

	}


	/*
	 *	extract the command's data
	 */
	num_need = instr->data_length;
	while(num_need) {

		if (pg->byte_count < COMM_SIZE) {/* make sure data in buffer */
			if ((pg->byte_count = fetch_input(cgm_fd, pg)) < 1)
				return(pg->byte_count);
		}

		num_copy = MIN(num_need, pg->byte_count);

		/* copy data to instr*/
		(void) memmove((void *) instr->data,
				(const void *) pg->buf_ptr, (size_t)num_copy);

		instr->data += num_copy;		/* update data pointer*/
		num_need -= num_copy;			/* dec need count     */
		pg->byte_count -= num_copy;
		pg->buf_ptr += num_copy;
	}
	instr->data = instr->buf;	/* reset data ptr to start */

	/*
	 * This short bit of code handles the deceivingly complex problem
	 * of truncating data streams to be 32760 bytes long and ensuring
	 * the remainding bytes are returned on the next invocation
	 */
	if (pg->over_flow) {
		instr->data_length += pg->over_flow;
		pg->over_flow = 0;
	}
	if (instr->data_length > MAX_CGM_INS_LEN) {
		pg->over_flow += instr->data_length - MAX_CGM_INS_LEN;
		instr->data_length = MAX_CGM_INS_LEN;
	}	


        /*
	 *	skip over padding in record of odd number of bytes of data
	 */
        if (instr->data_length % 2) {
                pg->buf_ptr++;
                pg->byte_count--;
        }

	pg->more = instr->more;
	instr->more = (instr->more || pg->over_flow);
	instr->data = instr->buf;	/* reset the data pointer to begining */
	return(1);
}



/*	CGM_flushGetInstr(cgm_fd)
 *	PUBLIC
 *
 *		Flush the input buffer for the designated file.
 * on entry
 *	cgm_fd		: a file descriptor for a file being processed by
 *			  CGM_getInstr
 * on exit
 *	Cgm_fd[cgm_fd].pg_struct->byte_count = 0
 *	Cgm_fd[cgm_fd].pg_struct->more_input = FALSE
 *	
 *
 */
void	CGM_flushGetInstr(cgm_fd)
	Cgm_fd	cgm_fd;
{
	cgmTab[cgm_fd].pg_struct->byte_count = 0;
	cgmTab[cgm_fd].pg_struct->more = FALSE;
}



/*	put_output
 *	PRIVATE
 *		install NCAR CGM header in output buffer and 
 *	write the output buffer to the CGM
 * on entry
 *	cgm_fd		: a Cgm_fd open for writing
 *	*pg		: pointer to Pg_struct for the cgm_fd
 *	numbytes	: number bytes of data left unused in the buffer
 * on exit
 *	return		: number of bytes now available in buffer
 */
static	put_output(cgm_fd, pg, numbytes )
	Cgm_fd		cgm_fd;
	Pg_struct	*pg;
	unsigned int	numbytes;
{
	register unsigned int 	tmp = 0;

	int	bytecount = cgmTab[cgm_fd].record_size - HEADERSIZE - numbytes;
	/*
	 * insert bytecount, datatype flag and frame bits into first
	 * four bytes of the output buffer. (fourth byte is unused)
	 */
	PUTBITS(tmp, COUNT_POS, COUNT_LEN, bytecount);/* dataCount*/
	pg->buf[1] = (char) (tmp & 0x000000ff);
	pg->buf[0] = (char) ((tmp & 0x0000ff00) >> 8);

	PUTBITS(pg->buf[2], TYPE_POS, TYPE_LEN, 3);	/* type NCAR CGM*/
	PUTBITS(pg->buf[2], BEG_MF_POS, LEN, pg->beg_meta);/* begin meta*/
	PUTBITS(pg->buf[2], END_MF_POS, LEN, pg->end_meta);/* end meta	*/
	PUTBITS(pg->buf[2], FRAME_POS, LEN, pg->new_frame);/* new frame	*/

	pg->buf_ptr = pg->buf + HEADERSIZE;	/* reset output buffer pointer*/

	if (CGM_write(cgm_fd, pg->buf) != cgmTab[cgm_fd].record_size) {
		return(-1);
	}

	return(cgmTab[cgm_fd].record_size - HEADERSIZE);
}


/*	CGM_putInstr:
 *	PUBLIC
 *
 *		Write a CGM instruction from an Instr into a NCAR CGM metafile,
 *	at the current file pointer position. This is the inverse function
 *	of CGM_getInstr(). Data is actually buffered until record size 
 *	specified when the CGM was opened is met. The format of the records
 *	written is as specified in the NCAR Graphics Installer's Guide.
 *	If instr.more is set then instr.data_length is <= 32760 and
 *	instr.data_length is even.
 *	
 *	CGM begin picture elements are guaranteed to be the first element
 *	in a record.
 * on entry
 *	cgm_fd		: CGM file descriptor for the metafile opened 
 *	*instr		: contains a valid CGM instruction 
 *		cgmclass: identifies the class of the cgm element
 *		id	: identifies the id of the cgm element
 *		buf	: contains the cgm element data
 *		data	: points to the begining of the data in buf,
 *			  usually &buf[0]
 *		data_len: num bytes of data in buf, <= 32760
 *		more	: 1 if more data for the same instruction will follow
 *
 * on exit
 *	*instr->data	: set to instr->buf[0]
 *	return		: 0 => success
 *			 >0 => success & the output buffer was written
 *			  return times
 *			 -1 => failure
 */
int	CGM_putInstr(cgm_fd, instr)
	Cgm_fd	cgm_fd;
	Instr	*instr;
{
	int		num_need,
			num_copy;
	unsigned	free_count;	/* num bytes unused in output buffer*/

	register unsigned int	tmp;
	register Pg_struct	*pg = cgmTab[cgm_fd].pg_struct;

	int	flush = 0;	/* number of times output buffer was written */ 

	free_count = cgmTab[cgm_fd].record_size - pg->byte_count - HEADERSIZE;

	/*
	 *      if "more" is set then only write data, not the command itself
	 */
	errno = 0;
	if (!pg->more) {

		/*
		 * if the buffer is full or the current instruction is a 
		 * begin picture or the current instruction is a end
		 * end metafile and the instruction is not already the first
		 * element in the record flush the buffer. We flush on a 
		 * begin picture and a end metafile  to ensure that the 
		 * beg pic/end mf is the first element in a record
		 */
		if ((free_count < COMM_SIZE) || (
			(instr->cgmclass == DEL_ELEMENT && instr->id == BEG_PIC_ID)
			&&
			free_count != cgmTab[cgm_fd].record_size - HEADERSIZE
			) || (
			(instr->cgmclass == DEL_ELEMENT && instr->id == END_MF_ID)
			&&
			free_count != cgmTab[cgm_fd].record_size - HEADERSIZE)
			){

			if ((free_count = put_output(cgm_fd, pg, free_count))<1)
				return(free_count);
			/*
			 * reset frame bits since we just wrote a record
			 */
			pg->beg_meta = pg->end_meta = pg->new_frame = FALSE;
			flush += 1;
		}

		/*
		 *	insert command class, id and data length
		 */
		tmp = 0;
		PUTBITS(tmp, CLASS_POSS, CLASS_BITS, instr->cgmclass);
		PUTBITS(tmp, ID_POSS, ID_BITS, instr->id);
		if (! instr->more && instr->data_length < LONGFORM)  {
			PUTBITS(tmp, PARM_POSS, PARM_BITS, instr->data_length);
		}
		else {	/* long form command	*/
			PUTBITS(tmp, PARM_POSS, PARM_BITS, LONGFORM);
		}

		/*
		 * insert the data in the actual buffer
		 */
		pg->buf_ptr[1] = (char) (tmp & 0x000000ff);
		pg->buf_ptr[0] = (char) ((tmp & 0x0000ff00) >> 8);


		/*
		 * advance to next field
		 */
		free_count -= COMM_SIZE;
		pg->buf_ptr += COMM_SIZE;

		/*
		 * record status of frame bits for NCAR CGM header
		 */
		if (instr->cgmclass == DEL_ELEMENT) {
			pg->beg_meta = (pg->beg_meta || instr->id == BEG_MF_ID);
			pg->end_meta = (pg->end_meta || instr->id == END_MF_ID);
			pg->new_frame =(pg->new_frame || instr->id == BEG_PIC_ID);
		}
	}

	/*
	 * insert data length if long form command
	 */
	if (instr->more || instr->data_length >= LONGFORM || pg->more ) {
		/*
		 * make sure room in buffer, if not write it
		 */
		if (free_count < COMM_SIZE) {
			if ((free_count = put_output(cgm_fd,pg, free_count))< 1)
				return(free_count);

			flush += 1;
			/*
			 * reset frame bits since we just wrote a record
			 */
			pg->beg_meta = pg->end_meta = pg->new_frame = FALSE;
		}

		tmp = 0;
		PUTBITS(tmp, PARM_POSS_L, PARM_BITS_L, instr->data_length);
		if (instr->more) {
			PUTBITS(tmp, P_POSS, P_BITS, 1);
		}

		pg->buf_ptr[1] = (char) (tmp & 0x000000ff);
		pg->buf_ptr[0] = (char) ((tmp & 0x0000ff00) >> 8);
		/*
		 * advance to the next field
		 */
		pg->buf_ptr += COMM_SIZE;
		free_count -= COMM_SIZE;
	}

	/*
	 * insert the data
	 */
	num_need = instr->data_length;
	while(num_need > 0) {

		/*
		 * make sure room in buffer, if not write it
		 */
		if (free_count < 1) {
			if ((free_count = put_output(cgm_fd,pg, free_count))< 1)
				return(free_count);

			flush += 1;
			/*
			 * reset frame bits since we just wrote a record
			 */
			pg->beg_meta = pg->end_meta = pg->new_frame = FALSE;
		}

		num_copy = MIN(num_need, free_count);
		(void) memmove((void *) pg->buf_ptr, 
				(const void *) instr->data, (size_t)num_copy);

		free_count -= num_copy;
		num_need -= num_copy;
		pg->buf_ptr += num_copy;
		instr->data += num_copy;
	}
	/*
	 * reset instr->data to point to begining of buffer
	 */
	instr->data = instr->buf;


	/*
	 *      insert padding in record if odd number of bytes of data
	 */
	if (instr->data_length % 2) {
		pg->buf_ptr++;
		free_count--;
	}

	pg->more = instr->more;

	/*
	 * recalculate num bytes stored in the buffer
	 */
	pg->byte_count = cgmTab[cgm_fd].record_size - free_count - HEADERSIZE;
	return((int) flush);
}


/*	CGM_flushOutoutInstr:
 *	PUBLIC
 *		write the output buffer for file being processed by
 *	CGM_putInstr.
 * on entry
 *	cgm_fd		: file descriptor for file being processed by 
 *			  CGM_getInstr
 * on exit 		
 *	return		: < 0 => failure
 *
 */
int	CGM_flushOutputInstr(cgm_fd)
	Cgm_fd	cgm_fd;
{
	int	status;
	Pg_struct	*pg = cgmTab[cgm_fd].pg_struct;

	errno = 0;
	status = put_output(cgm_fd, pg,
		cgmTab[cgm_fd].record_size - pg->byte_count - HEADERSIZE);
	pg->more = FALSE;
	pg->byte_count = 0;

	return(status);
}



/*	CGM_freeDirectory:
 *	PUBLIC
 *
 *		free memory allocated to a directory
 * on entry
 *	dir		: is a pointer to a directory created with 
 *			  CGM_directory
 * on exit
 *	dir		: is NULL
 */
void	CGM_freeDirectory(dir)
	Directory	*dir;
{
	int	i;

	if (dir == (Directory *) NULL)
		return;

	if (dir->meta != (int *) NULL)
		(void) free ((Voidptr) dir->meta);

	for (i = 0; i < dir->dir_size; i++) {
		if (dir->d[i].text != NULL)
			(void) free((Voidptr) dir->d[i].text);
	}

	for (i = 0; i < dir->MFDes_size; i++) {
		if (dir->MFDescription[i] != NULL)
			(void) free((Voidptr) dir->MFDescription[i]);
	}

	(void) free((Voidptr) dir->d);
	(void) free((Voidptr) dir->MFDescription);
	(void) free((Voidptr) dir);
	dir = NULL;
}

Directory	*ReallocDir(dir, num_frames)
		Directory	*dir;
		unsigned	num_frames;
{

	int	i;
	int	dir_size;	/* current memory allocation	*/

	dir_size = dir->dir_size;

	/*
	 * we only grow
	 */
	errno = 0;
	if (dir_size > num_frames) return ((Directory *) NULL);

	dir->d = (Directory_entry *) realloc ((char *) dir->d, (unsigned)
		(sizeof (Directory_entry) * num_frames));
	if (! dir->d) {
		return((Directory *) NULL);
	}

	/*
	 * record new directory size
	 */
	dir->dir_size = num_frames;

	for (i=dir_size; i < num_frames; i++) {
		dir->d[i].text = NULL;
	}

	return(dir);
}

/*
 *	CGM_copyCreateDir
 *
 *	create a new directory and copy the contents of the input directory
 *	to the new directory
 *
 * on entry
 *	*d1		: a directory 
 * on exit
 *	return		: NULL => failure, else a copy of d1 is returned
 */
Directory	*CGM_copyCreateDir(d1)
	Directory	*d1;
{
	Directory	*d2;
	Directory	*init_dir();
	int		i;

	int	meta_size = d1->meta_size;
	int	dir_size = d1->dir_size;
	int	des_size = d1->MFDes_size;

	errno = 0;
	if (! (d2 = init_dir())) return (Directory *) NULL;

	/*
	 * copy over staring record for each metafile within the CGM (usually
	 * only one) checking for memory allocation while we're at it.
	 */
	d2->num_meta = d1->num_meta;
	if (d2->meta_size < meta_size) {
		d2->meta = (int *) realloc((char *) d2->meta,
					(unsigned) meta_size * sizeof (int)); 
		if (! d2->meta) {
			return((Directory *) NULL);
		}
	}
	for (i = 0; i < d1->num_meta; i++) {
		d2->meta[i] = d1->meta[i];
	}
	d2->meta_size = meta_size;

	/*
	 * copy over the directory structure
	 */
	d2->num_frames = d1->num_frames;
	if (d2->dir_size < dir_size) {
		d2 = ReallocDir(d2, (unsigned) dir_size);
	}
	for (i = 0; i < d1->num_frames; i++) {
		d2->d[i].record = d1->d[i].record;
		d2->d[i].num_record = d1->d[i].num_record;
		d2->d[i].type = d1->d[i].type;
		if (d1->d[i].text) {
			d2->d[i].text = (char *)malloc(strlen(d1->d[i].text)+1);
			if (! d2->d[i].text) {
				return((Directory *) NULL);
			}
			(void) strcpy(d2->d[i].text, d1->d[i].text);
		}
		else {
			d1->d[i].text = NULL;
		}
	}
	d2->dir_size = d2->dir_size;

	/*
	 * metafile description information
	 */
	if (d2->MFDes_size < des_size) {
		d2->MFDescription = (char **) realloc((char *) d2->meta,
				(unsigned) meta_size * sizeof (char *));
		if (! d2->MFDescription) {
			return((Directory *) NULL);
		}
	}
	for (i = 0; i < des_size; i++ ) {
		d2->MFDescription[i] = NULL;	/* no used	*/
	}
	d2->MFDes_size = des_size;

	d2->status = d1->status;
	d2->cgm_fd = d1->cgm_fd;

	return(d2);
}


/*	init_dir():
 *	Private
 *
 *		allocate memory for directory
 */
Directory	*init_dir()
{
	Directory	*dir;
	int	i;

	errno = 0;
	dir = (Directory *) malloc (sizeof( Directory));
	if (! dir) {
		return((Directory *) NULL);
	}

	dir->meta = (int *) malloc (DIR_2_ALLOC * sizeof(int));
	if (! dir->meta) {
		return((Directory *) NULL);
	}
	dir->meta_size = DIR_2_ALLOC;

	dir->MFDescription = (char **) malloc (DIR_2_ALLOC * sizeof(char *));
	if (! dir->MFDescription) {
		return((Directory *) NULL);
	}

	dir->MFDes_size = DIR_2_ALLOC;


	dir->d = (Directory_entry *) malloc (
		DIR_2_ALLOC * sizeof(Directory_entry)
	);
	if (! dir->d) {
		return((Directory *) NULL);
	}

	dir->dir_size = DIR_2_ALLOC;

	for (i=0; i < DIR_2_ALLOC; i++) {
		dir->MFDescription[i] = NULL;
		dir->d[i].text = NULL;
	}



	dir->status = CGM_ERROR;
	dir->num_frames = 0;
	dir->num_meta = 0;

	return(dir);
}
