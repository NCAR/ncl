/*
 *      $Id: ctrans.h,v 1.8 2000-08-22 03:30:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1993                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
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

/*
 *	File:		ctrans.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jul 10 11:22:23 MDT 1992
 *
 *	Description:	header file for ctrans.c
 */

#include <ncarg/c.h>
#include <ncarg/cgm_tools.h>

#ifndef	_ctrans_
#define	_ctrans_

typedef	enum	{
	FATAL = -2,
	WARN = -1,
	EOM = 0,
	OK = 1
	} CtransRC;


#define	EOM	0	/* End of Metafile	*/

/* This macro protects C function names from C++ name-mangling. */
NCARG_PROTO_BEGIN

extern	CtransRC	init_ctrans(
#ifdef	NeedFuncProto
	int	*argc,
	char	**argv,
	const char	*gcap,
	const char	*fcap,
	boolean	batch
#endif
);




extern	CtransRC	init_metafile(
#ifdef	NeedFuncProto
	int	record,
	Cgm_fd	cgm_fd
#endif
);

extern	void	close_metafile(
#ifdef	NeedFuncProto
#endif
);

extern	CtransRC	ctrans(
#ifdef	NeedFuncProto
	int	record
#endif
);

extern	CtransRC	ctrans_merge(
#ifdef	NeedFuncProto
	int	record1,
	int	record2
#endif
);

extern	CtransRC	SetDevice(
#ifdef	NeedFuncProto
	const char	*gcap
#endif
);

extern	CtransRC	SetFont(
#ifdef	NeedFuncProto
	const char	*fcap
#endif
);

extern	void	SetDefaultPalette(
#ifdef	NeedFuncProto
	const char	*pal_fname
#endif
);

extern	void	GraphicsMode(
#ifdef	NeedFuncProto
	boolean	on
#endif
);

extern	void	close_ctrans(
#ifdef	NeedFuncProto
#endif
);

extern	char	*getGcapname(
#ifdef	NeedFuncProto
	const char	*gcap
#endif
);

extern	char	*getFcapname(
#ifdef	NeedFuncProto
	const char*	fcap
#endif
);

extern	boolean	IsOutputToTty(
#ifdef	NeedFuncProto
#endif
);

extern	char	*ReadCtransMsg(
#ifdef	NeedFuncProto
#endif
);

extern	void	CtransClear(
#ifdef	NeedFuncProto
#endif
);

/*
**
**	yet another ctrans programming interface. 
**
*/



extern	CtransRC	CtransOpenBatch(
#ifdef	NeedFuncProto
	const char	*device_name,
	const char	*font_name,
	const char	*metafile,
	int	*dev_argc,
	char	**dev_argv
#endif
);

extern	CtransRC	CtransSetMetafile(
#ifdef	NeedFuncProto
	const char	*metafile
#endif
);

extern	CtransRC	CtransPlotFrame(
#ifdef	NeedFuncProto
#endif
);

extern	CtransRC	CtransClearDisplay(
#ifdef	NeedFuncProto
#endif
);

void	CtransCloseBatch(
#ifdef	NeedFuncProto
#endif
);

extern	CtransRC	CtransLSeekBatch(
#ifdef	NeedFuncProto
	unsigned	offset,
	unsigned	whence
#endif
);

void	CtransGraphicsMode(
#ifdef	NeedFuncProto
	boolean	on
#endif
);

#ifdef	DEAD
extern	int	CtransGetErrorNumber(
#ifdef	NeedFuncProto
#endif
);
#endif


extern	const	char	*CtransGetErrorMessage(
#ifdef	NeedFuncProto
#endif
);

NCARG_PROTO_END

#endif
