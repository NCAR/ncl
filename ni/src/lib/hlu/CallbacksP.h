/*
 *      $Id: CallbacksP.h,v 1.4 1996-11-28 01:14:22 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CallbacksP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sat May 4 00:40:29 MDT 1996
 *
 *	Description:	
 */

#ifndef	_NHL_CALLBACKSP_H
#define	_NHL_CALLBACKSP_H
#include <ncarg/hlu/Callbacks.h>

#define	_NhlCBCALLING		(0x01)
#define	_NhlCBNODEDESTROY	(0x02)
#define _NhlCBLISTDESTROY	(0x04)

struct _NhlCBRec {
	int		state;
	_NhlCBList	cblist;
	int		index;
	_NhlCBFunc	cbfunc;
	NhlArgVal	udata;
	NhlPointer	cbnode_data;
	_NhlCB		next;
};

struct _NhlCBListRec {
	int		state;
	int		size;
	long		mask;
	_NhlCB		single;
	_NhlCB		*hash;
	_NhlCBAddHash	add_hash;
	_NhlCBCallHash	call_hash;
	_NhlCBTaskProc  task_proc;
	NhlPointer	task_proc_data;
};

#endif	/* _NHL_CALLBACKSP_H */
