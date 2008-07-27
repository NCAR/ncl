/*
 *	$Id: mem_file.h,v 1.6 2008-07-27 03:22:38 haley Exp $
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

#ifndef	_mem_file_
#define	_mem_file_

typedef	struct	{
	char	*name;		/* name of file			*/
	unsigned char	*_base;	/* the file			*/
	unsigned char	*_ptr;	/* pointer into _base		*/
	long	*size;		/* space allocated in bytes	*/
	long	*len;		/* lengh of file in bytes	*/
	int	*r_size;	/* size of a record in bytes	*/
	} CGM_iobuf;
	

	

/*
 * number of records initialy allocated to a newly created file
 */
#define F_INIT_SIZE	10


extern	int	CGM_openMemFile(
#ifdef	NeedFuncProto
	const char	*metafile,
	int		record_size,
	const char	*type
#endif
);

extern	int	CGM_readMemFile(
#ifdef	NeedFuncProto
	int	fd,
	unsigned char	*buf
#endif
);


extern	int	CGM_writeMemFile(
#ifdef	NeedFuncProto
	int	fd,
	unsigned char	*buf
#endif
);


extern	int	CGM_lseekMemFile(
#ifdef	NeedFuncProto
	int	fd,
	int	offset,
	int	whence
#endif
);

extern	int	CGM_closeMemFile(
#ifdef	NeedFuncProto
	int	fd
#endif
);

extern	int	CGM_unlinkMemFile(
#ifdef	NeedFuncProto
	char	*metafile
#endif
);

#endif	/* _mem_file_	*/
