/*
 *	$Id: mem_file.h,v 1.5 2000-08-22 03:30:31 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
