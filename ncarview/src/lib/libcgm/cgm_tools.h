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

#include <cgmdef.h>

typedef	int	Cgm_fd;		/* a file descriptor for a metafile	*/

typedef	enum {
	CGM_OK,
	CGM_ERROR,
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

#ifndef	CGM_TOOLS
	extern	Directory	*CGM_directory();
#endif



/*
 *	structure for reading in a raw cgm instruction
 */
typedef	struct {
	unsigned int	class;		/* CGM class (0-7)	*/
	unsigned int	id;		/* CGM element id	*/
	unsigned char	buf[32767 * 2];	/* CGM element data	*/
	unsigned char	*data;		/* pointer to begining of data in
					 * buf. usually this is &buf[0]
					 */
	unsigned int	data_length;	/* num bytes data in buf*/
	int	more;			/* boolean, 1 => more data to follow */
	} Instr;	

/*
 *	externs for meta_edit capabilities
 */
#ifndef	META_EDIT
	extern	Directory	*CGM_copyFrames();
	extern	Directory	*CGM_deleteFrames();
	extern	Directory	*CGM_editFrame();
	extern	Directory	*CGM_initMetaEdit();
	extern	Directory	*CGM_mergeFrames();
	extern	Directory	*CGM_moveFrames();
	extern	Directory	*CGM_readFrames();
#endif

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

#ifndef	L_SET
#define	L_SET	0
#define	L_INCR	1
#define	L_XTND	2
#endif

#endif	_cgm_tools_
