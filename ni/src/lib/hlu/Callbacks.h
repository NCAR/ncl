/*
 *      $Id: Callbacks.h,v 1.4 1997-03-31 16:16:01 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Callbacks.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sat May 4 00:48:23 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NHL_CALLBACKS_H
#define	_NHL_CALLBACKS_H
#include <ncarg/hlu/hlu.h>

/*
 * Definition of the CB function type.  The cbdata is data passed in by
 * the _NhlCBCallCallbacks function.  The udata is the "user" data passed
 * in from the _NhlCBAdd function call.  It is re-used, so it is important
 * that the user realize that they can't free any data they pass into
 * the _NhlCBAdd function as the "udata" until they remove the given
 * CB.
 */
typedef void (*_NhlCBFunc)(
#if	NhlNeedProto
	NhlArgVal	cbdata,		/* data from_NhlCBCallCallbacks	*/
	NhlArgVal	udata		/* user data from _NhlCBAdd	*/
#endif
);

/*
 * This function is called by _NhlCBAdd to determine where the given
 * CB function should be placed in the "hash" table.  It is only used if
 * this is a "hashing" CB list.
 */
typedef long (*_NhlCBAddHash)(
#if	NhlNeedProto
	NhlArgVal	selector,	/* from _NhlCBAdd - not saved	*/
	NhlArgVal	udata
#endif
);

/*
 * This function is called by _NhlCBCallCallbacks to determine which
 * CB's should be called.
 */
typedef long (*_NhlCBCallHash)(
#if	NhlNeedProto
	NhlArgVal	selector,	/* from _NhlCBCallCallbacks	*/
	NhlArgVal	cbdata
#endif
);

typedef enum __NhlCBTask {
	_NhlcbADD,
	_NhlcbDELETE,
	_NhlcbCALL
} _NhlCBTask;

/*
 * This function does private stuff appropriate to the particular
 * callback during add, call, and delete
 */

typedef NhlErrorTypes (*_NhlCBTaskProc)(
#if	NhlNeedProto
	NhlPointer	proc_data,
	_NhlCBTask	task,
	NhlArgVal	selector,
	NhlBoolean	*do_it,					
	NhlArgVal	*cbdata,				     
	NhlPointer	*cbnode_data
#endif
);

typedef struct _NhlCBRec _NhlCBRec, *_NhlCB;
typedef struct _NhlCBListRec _NhlCBListRec, *_NhlCBList;

/*
 * Create a callback list.
 * A hash_mult of 0 indicates that all CB funcs should be called when
 * _NhlCBCallCallbacks is called, and there is no reason to hash them.
 * The hash table is 2^hash_mult in size.
 * The valid values of hash_mult are >= 0 and <= 8.
 * If add_hash is NULL, then the arg "selector" to _NhlCBAdd is treated
 * as a "longval", and used as the hash value.  call_hash works the
 * same way, but from the _NhlCBCallCallbacks side.  If hash_mult
 * is 0, then add_hash and call_hash are ignored.  Likewise, the "selector"
 * arg to _NhlCBAdd and _NhlCBCallCallbacks is ignored.
 *
 * The "longval" used for the hash must be a positive integer (0 ok).
 */
extern _NhlCBList
_NhlCBCreate(
#if	NhlNeedProto
	int		hash_mult,
	_NhlCBAddHash	add_hash,
	_NhlCBCallHash	call_hash,
        _NhlCBTaskProc	task_proc,
	NhlPointer	task_proc_data
#endif
);

extern void
_NhlCBDestroy(
#if	NhlNeedProto
	_NhlCBList	cblist
#endif
);

/*
 * Add a function to the callback list
 */
extern _NhlCB
_NhlCBAdd(
#if	NhlNeedProto
	_NhlCBList	cblist,
	NhlArgVal	selector,
	_NhlCBFunc	cbfunc,
	NhlArgVal	udata
#endif
);

/*
 * Remove a function from a callback list
 */
extern void
_NhlCBDelete(
#if	NhlNeedProto
	_NhlCB	cb
#endif
);

/*
 * execute the callback list.
 */
extern void
_NhlCBCallCallbacks(
#if	NhlNeedProto
	_NhlCBList	cblist,
	NhlArgVal	selector,
	NhlArgVal	cbdata
#endif
);

extern void
_NhlCBIterate(
#if	NhlNeedProto
	_NhlCBList	cblist,
 	_NhlCBTask	task,
	NhlArgVal	cbdata
#endif
);

#endif	/* _NHL_CALLBACKS_H */
