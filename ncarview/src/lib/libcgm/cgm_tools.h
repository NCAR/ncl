/*
 *	$Id: cgm_tools.h,v 1.13 2008-07-27 03:22:38 haley Exp $
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
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

#ifndef	_cgm_tools_
#define _cgm_tools_

#include <ncarg/c.h>
#include <ncarg/cgmdef.h>

typedef	int	Cgm_fd;		/* a file descriptor for a metafile	*/

typedef	enum {
	CGM_OK,
	CGM_ERROR
	} CGM_Status;

/*
 *	elements of the directory
 */
typedef	struct	{		
	int	record,		/* offset of record in the file		*/
		num_record,	/* number of records in this frame	*/
		type;		/* type of record			*/
	char	*text;		/* optional picture descriptor		*/
	} Directory_entry;

/*
 *	the directory
 */
typedef	struct	{
	int	num_meta;	/* number of metafiles in single file	*/
	int	*meta;		/* begining record of each metafile	*/
	int	meta_size;	/* amount of memory allocated to *meta	*/
	int	num_frames;	/* number of frames in the file		*/
	Directory_entry	*d;	/* pointer to elements of the directory	*/
	int	dir_size;	/* amount of memory allocated to *d	*/
	char		**MFDescription;/* optional metafile description*/
	int		MFDes_size;	/* mem allocated to MFDescription*/
	CGM_Status	status;		
	Cgm_fd		cgm_fd;	/* file descriptor associated with this file */
	}	Directory;

#define	CGM_NUM_META(D)		((D)->num_meta)	
#define	CGM_META_START(D, M)	((D)->meta[(M)]) 
#define	CGM_NUM_FRAMES(D)	((D)->num_frames)	
#define	CGM_RECORD(D, F)	((D)->d[(F)].record)
#define	CGM_NUM_RECORD(D, F)	((D)->d[(F)].num_record)
#define	CGM_RECORD_TYPE(D, F)	((D)->d[(F)].type)
#define	CGM_PIC_DES(D, F)	((D)->d[(F)].text)
#define	CGM_STATUS(D)		((D)->status)
#define	CGM_FD(D)		((D)->cgm_fd)




/*
 *	structure for reading in a raw cgm instruction
 */
typedef	struct {
	unsigned int	cgmclass;		/* CGM class (0-7)	*/
	unsigned int	id;		/* CGM element id	*/
	unsigned char	buf[32767 * 2];	/* CGM element data	*/
	unsigned char	*data;		/* pointer to begining of data in
					 * buf. usually this is &buf[0]
					 */
	unsigned int	data_length;	/* num bytes data in buf*/
	int	more;			/* boolean, 1 => more data to follow */
	} Instr;	


/*
 *	the valid frame types. See section on NCAR CGM in the NCAR
 *	Graphics installers guide
 */
#define	HEADER		4
#define	PRINTER		8
#define	PRE_CGM		2
#define	NCAR_CGM	3

/*
 *	maximum amount of CGM instruction data that can be processed
 *	by a single invocation of CGM_getInstr() or CGM_putInstr()
 */
#define	MAX_CGM_INS_LEN	32760


/*
 * maximum file descriptors for memory files. Also max number of files that
 * can exist. Must be <= (sizeof (long) * BYTESIZE)
 */
#define	MAX_MEM_FILE	20

/*
 *	Flag passed to CGM_open to indicate CGM resides in memory, not 
 *	on disk
 */
#define	MEM_FILE	-1

#ifndef	SEEK_SET
#define	SEEK_SET	0
#define	SEEK_CUR	1
#define	SEEK_END	2
#endif



/*
**
**	C G M    T O O L S
**
*/

/* This macro protects C function names from C++ name-mangling. */
NCARG_PROTO_BEGIN

extern	Cgm_fd	CGM_open(
#ifdef	NeedFuncProto
	const char	*metafile,
	int		record_size,
	const char	*type
#endif
);

extern	int	CGM_close(
#ifdef	NeedFuncProto
	Cgm_fd	cgm_fd
#endif
);

extern	CGM_read(
#ifdef	NeedFuncProto
	Cgm_fd		cgm_fd,
	unsigned char	*buf
#endif
);

extern	CGM_write(
#ifdef	NeedFuncProto
	Cgm_fd		cgm_fd,
	const unsigned char	*buf
#endif
);

extern	CGM_lseek(
#ifdef	NeedFuncProto
	Cgm_fd		cgm_fd,
	int	offset,
	int	whence
#endif
);

extern	CGM_flush(
#ifdef	NeedFuncProto
	Cgm_fd	cgm_fd
#endif
);

extern	Directory	*CGM_directory(
#ifdef	NeedFuncProto
	Cgm_fd	cgm_fd,
	FILE	*fp
#endif
);

extern	void	CGM_printDirectory(
#ifdef	NeedFuncProto
	const Directory	*dir
#endif
);


extern	int	CGM_getInstr(
#ifdef	NeedFuncProto
	Cgm_fd	cgm_fd,
	Instr	*instr
#endif
);


extern	void	CGM_flushGetInstr(
#ifdef	NeedFuncProto
	Cgm_fd	cgm_fd
#endif
);


extern	int	CGM_putInstr(
#ifdef	NeedFuncProto
	Cgm_fd	cgm_fd,
	Instr	*instr
#endif
);

extern	int	CGM_flushOutputInstr(
#ifdef	NeedFuncProto
	Cgm_fd	cgm_fd
#endif
);


extern	void	CGM_freeDirectory(
#ifdef	NeedFuncProto
	Directory	*dir
#endif
);

Directory	*CGM_copyCreateDir(
#ifdef	NeedFuncProto
	Directory	*d1
#endif
);


extern	Directory	*ReallocDir(
#ifdef	NeedFuncProto
		Directory	*dir,
		unsigned	num_frames
#endif
);



extern	Directory	*CGM_copyFrames(
#ifdef	NeedFuncProto
	unsigned int	start_frame,
	int		num_frames,
	unsigned int	target
#endif
);

extern	Directory	*CGM_deleteFrames(
#ifdef	NeedFuncProto
	unsigned int	start_frame,
	unsigned int	num_frames
#endif
);

extern	Directory	*CGM_readFrames(
#ifdef	NeedFuncProto
	char		*ncar_cgm,
	unsigned int	start_frame,
	int		num_frames,
	unsigned int	target,
	unsigned int	record_size
#endif
);

extern	Directory	*CGM_moveFrames (
#ifdef	NeedFuncProto
	unsigned int	start_frame,
	unsigned int	num_frames,
	unsigned int	target
#endif
);

extern	Directory	*CGM_initMetaEdit (
#ifdef	NeedFuncProto
	char		*ncar_cgm,
	int	record_size,
	char	*local_tmp,
	FILE	*verbose_fp
#endif
);

extern	int	CGM_termMetaEdit(
#ifdef	NeedFuncProto
#endif
);

extern	int	CGM_writeFile(
#ifdef	NeedFuncProto
	char	*ncar_cgm
#endif
);

extern	int	CGM_writeFrames(
#ifdef	NeedFuncProto
	char		*ncar_cgm,
	unsigned	start_frame,
	unsigned	num_frames
#endif
);
extern	int	CGM_appendFrames(
#ifdef	NeedFuncProto
	char		*ncar_cgm,
	unsigned	start_frame,
	unsigned	num_frames
#endif
);

extern	Directory	*CGM_mergeFrames(
#ifdef	NeedFuncProto
	unsigned	bottom, 
	unsigned 	top
#endif
);
		
extern	Directory	*CGM_editFrame(
#ifdef	NeedFuncProto
	unsigned	frame,
	Instr		*edit_instr,
	int		num_occur
#endif
);

extern	const char	*CGM_ClassLookup(
#ifdef	NeedFuncProto
	unsigned	int cgmclass
#endif
);

extern	const char	*CGM_ElementLookup(
#ifdef	NeedFuncProto
	unsigned	int cgmclass,
	unsigned	int id
#endif
);

NCARG_PROTO_END

#endif	/* _cgm_tools_	*/
