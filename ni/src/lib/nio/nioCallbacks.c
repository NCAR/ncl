/*
 *      $Id: nioCallbacks.c,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Callbacks.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sat May 4 00:36:45 MDT 1996
 *
 *	Description:	A generic callback creation interface.  Used to
 *			create a callback list, add and remove callbacks
 *			from the list, and execute the callback list with
 *			a given set of data.
 */
#include "nioCallbacksP.h"

_NhlCBList
_NhlCBCreate
#if	NhlNeedProto
(
	int		hash_mult,
	_NhlCBAddHash	add_hash,
	_NhlCBCallHash	call_hash,
	_NhlCBTaskProc	task_proc,
	NhlPointer	task_proc_data
)
#else
(hash_mult,add_hash,call_hash,task_proc,task_proc_data)
	int		hash_mult;
	_NhlCBAddHash	add_hash;
	_NhlCBCallHash	call_hash;
	_NhlCBTaskProc	task_proc;
	NhlPointer	task_proc_data;
#endif
{
	_NhlCBList	cblist = NhlMalloc(sizeof(_NhlCBListRec));
	int		i;
	int		size=1;

	if(!cblist){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}

	if(hash_mult < 0)
		hash_mult = 0;
	else if(hash_mult > 8)
		hash_mult = 8;

	for(i=0;i<hash_mult;i++)
		size *= 2;

	cblist->state = 0;
	cblist->size = size;
	cblist->single = NULL;
	cblist->mask = size - 1;
	if(size == 1){
		cblist->hash = &cblist->single;
		cblist->add_hash = NULL;
		cblist->call_hash = NULL;
	}
	else{
		cblist->hash = NhlMalloc(size * sizeof(_NhlCB));
		if(!cblist->hash){
			NhlPError(NhlFATAL,ENOMEM,NULL);
			NhlFree(cblist);
			return NULL;
		}
		memset(cblist->hash,0,size * sizeof(_NhlCB));
		cblist->add_hash = add_hash;
		cblist->call_hash = call_hash;
	}
	cblist->task_proc = task_proc;
	cblist->task_proc_data = task_proc_data;

	return cblist;
}

void
_NhlCBDestroy
#if	NhlNeedProto
(
	_NhlCBList	cblist
)
#else
(cblist)
	_NhlCBList	cblist;
#endif
{
	_NhlCB	cbt1,cbt2;
	int	i;

	if(!cblist)
		return;

	if(cblist->state & _NhlCBCALLING){
		cblist->state |= _NhlCBLISTDESTROY;
		return;
	}

	for(i=0;i<cblist->size;i++){
		cbt1 = cblist->hash[i];
		while(cbt1){
			if (cblist->task_proc) {
				NhlBoolean	yes;
				NhlArgVal	cbdata;
				NhlArgVal	sel;

                                NhlINITVAR(sel);
                                NhlINITVAR(cbdata);
				sel.lngval = cbt1->index;
				(*cblist->task_proc)
					(cblist->task_proc_data,
					 _NhlcbDELETE,sel,&yes,&cbdata,
					 &cbt1->cbnode_data);
			}
			cbt2 = cbt1;
			cbt1 = cbt1->next;
			NhlFree(cbt2);
		}
	}

	if(cblist->hash != &cblist->single)
		NhlFree(cblist->hash);

	NhlFree(cblist);

	return;
}

_NhlCB
_NhlCBAdd
#if	NhlNeedProto
(
	_NhlCBList	cblist,
	NhlArgVal	selector,
	_NhlCBFunc	cbfunc,
	NhlArgVal	udata
)
#else
(cblist,selector,cbfunc,udata)
	_NhlCBList	cblist;
	NhlArgVal	selector;
	_NhlCBFunc	cbfunc;
	NhlArgVal	udata;
#endif
{
	_NhlCB		cb;
	_NhlCB		*cbptr;

	if(!cblist){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Invalid cblist..."));
		return NULL;
	}

	cb = NhlMalloc(sizeof(_NhlCBRec));
	if(!cb){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	cb->state = 0;
	cb->cblist = cblist;
	if(cblist->add_hash)
		cb->index = (*cblist->add_hash)(selector,udata);
	else if(cblist->size == 1)
		cb->index = 0;
	else
		cb->index = selector.lngval;


	cb->cbfunc = cbfunc;
	cb->udata = udata;
	cb->next = NULL;
	cb->cbnode_data = NULL;

	if (cblist->task_proc) {
		NhlArgVal sel;
		NhlBoolean yes;
		
		yes = True;
                NhlINITVAR(sel);
		sel.lngval = cb->index;
		(*cblist->task_proc) 
			(cblist->task_proc_data,
			 _NhlcbADD,sel,&yes,NULL,&cb->cbnode_data);
		if (! cb->cbnode_data) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "CB not installed"));
			NhlFree(cb);
			return NULL;
		}
	}

	cbptr = &cblist->hash[cb->index & cblist->mask];
	/*
	 * MUST be added to end of list so it will get called even if
	 * it has been added during a CallCallbacks.
	 */
	while(*cbptr)
		cbptr = &(*cbptr)->next;
	*cbptr = cb;

	return cb;
}

void
_NhlCBDelete
#if	NhlNeedProto
(
	_NhlCB	cb
)
#else
(cb)
	_NhlCB	cb;
#endif
{
	char		func[] = "_NhlCBDelete";
	_NhlCBList	cblist;
	_NhlCB		*cbptr;
	NhlBoolean	found = False;

	if(!cb)
		return;
	cblist = cb->cblist;

	if(cblist->state & _NhlCBCALLING){
		cb->state |= _NhlCBNODEDESTROY;
		return;
	}

	cbptr = &cblist->hash[cb->index & cblist->mask];

	while(*cbptr){
		if(*cbptr == cb){
			if (cblist->task_proc) {
				NhlBoolean	yes;
				NhlArgVal	cbdata;
				NhlArgVal	sel;

                                NhlINITVAR(sel);
                                NhlINITVAR(cbdata);
				sel.lngval = cb->index;
				(*cblist->task_proc)
					(cblist->task_proc_data,
					 _NhlcbDELETE,sel,&yes,&cbdata,
					 &cb->cbnode_data);
			}
			*cbptr = cb->next;
			found = True;
			break;
		}
		cbptr = &(*cbptr)->next;
	}

	if(!found){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to remove CB",func);
		return;
	}

	NhlFree(cb);

	return;
}

void
_NhlCBCallCallbacks
#if	NhlNeedProto
(
	_NhlCBList	cblist,
	NhlArgVal	selector,
	NhlArgVal	cbdata
)
#else
(cblist,selector,cbdata)
	_NhlCBList	cblist;
	NhlArgVal	selector;
	NhlArgVal	cbdata;
#endif
{
	long	index;
	_NhlCB	cb,tcb;
	int	i;

	if(!cblist)
		return;

	if(cblist->state & _NhlCBCALLING)
		return;

	cblist->state = _NhlCBCALLING;
	if(cblist->call_hash)
		index = (*cblist->call_hash)(selector,cbdata);
	else if(cblist->size == 1)
		index = 0;
	else
		index = selector.lngval;

	cb = cblist->hash[index & cblist->mask];
	if (! cblist->task_proc) {
		while(cb){
			/*
			 * Only call the cb if the index matches, and the 
			 * callback wasn't removed during this call to 
			 * CallCallbacks.
			 */
			if(cb->index == index && 
			   !(cb->state & _NhlCBNODEDESTROY))
				(*cb->cbfunc)(cbdata,cb->udata);
			if(cblist->state & _NhlCBLISTDESTROY)
				break;
			cb = cb->next;
		}
	}
	else {
		while(cb){
			NhlBoolean	yes;
			NhlArgVal	sel;

			yes = True;
			sel.lngval = index;
                        NhlINITVAR(sel);
			(*cblist->task_proc)
				(cblist->task_proc_data,_NhlcbCALL,
				 sel,&yes,&cbdata,&cb->cbnode_data);
				
			if (yes && !(cb->state & _NhlCBNODEDESTROY)) {
				(*cb->cbfunc)(cbdata,cb->udata);
			}
			if(cblist->state & _NhlCBLISTDESTROY)
				break;
			cb = cb->next;
		}
	}
	cblist->state &= ~_NhlCBCALLING;

	/*
	 * If _NhlCBDestroy was called during the call of the list
	 */
	if(cblist->state & _NhlCBLISTDESTROY){
		_NhlCBDestroy(cblist);
		return;
	}

	/*
	 * Remove any nodes that were deleted during the call of the list
	 */
	for(i=0;i<cblist->size;i++){
		_NhlCB	*cbptr;

		cbptr = &cblist->hash[i];
		while(*cbptr){
			cb = *cbptr;
			if(cb->state & _NhlCBNODEDESTROY){
				*cbptr = cb->next;
				NhlFree(cb);
				continue;
			}
			cbptr = &cb->next;
		}
	}

	return;
}

void
_NhlCBIterate
#if	NhlNeedProto
(
	_NhlCBList	cblist,
	_NhlCBTask	task, /* for now only accepted task is '_NhlcbCALL' */
	NhlArgVal	cbdata
)
#else
(cblist,task,cbdata)
	_NhlCBList	cblist;
	_NhlCBTask	task;
	NhlArgVal	cbdata;
#endif
{
	NhlArgVal sel;
	_NhlCB	cb;
	int	i;

	if(!cblist)
		return;

	if(cblist->state & _NhlCBCALLING)
		return;

	if (task == _NhlcbADD || task == _NhlcbDELETE) /* not supported */
		return;

	cblist->state = _NhlCBCALLING;
        NhlINITVAR(sel);
        
	for(i=0;i<cblist->size;i++){
		_NhlCB	*cbptr;

		cbptr = &cblist->hash[i];
		if (! cblist->task_proc) {
			while(*cbptr){
				cb = *cbptr;
				if (!(cb->state & _NhlCBNODEDESTROY)) {
					(*cb->cbfunc)(cbdata,cb->udata);
				}
				if(cblist->state & _NhlCBLISTDESTROY)
					goto DONE;
				cbptr = &cb->next;
			}
		}
		else {
			while(*cbptr){
				NhlArgVal lcbdata;
				NhlBoolean yes;
				
				lcbdata = cbdata;
				yes = True;
				cb = *cbptr;
				sel.lngval = cb->index;
				(*cblist->task_proc)
					(cblist->task_proc_data,task,
					 sel,&yes,&lcbdata,&cb->cbnode_data);
				
				if (yes && !(cb->state & _NhlCBNODEDESTROY)) {
					(*cb->cbfunc)(lcbdata,cb->udata);
				}
				if(cblist->state & _NhlCBLISTDESTROY)
					goto DONE;
				cbptr = &cb->next;
			}
		}
	}

DONE:

	cblist->state &= ~_NhlCBCALLING;

	/*
	 * If _NhlCBDestroy was called during the call of the list
	 */
	if(cblist->state & _NhlCBLISTDESTROY){
		_NhlCBDestroy(cblist);
		return;
	}
	/*
	 * Remove any nodes that were deleted during the call of the list
	 */
	sel.lngval = 0;
	for(i=0;i<cblist->size;i++){
		_NhlCB	*cbptr;

		cbptr = &cblist->hash[i];
		while(*cbptr){
			NhlBoolean yes;
			cb = *cbptr;
			if(cb->state & _NhlCBNODEDESTROY){
				if (cblist->task_proc) {
					(*cblist->task_proc)
					(cblist->task_proc_data,
					 _NhlcbDELETE,sel,&yes,&cbdata,
					 &cb->cbnode_data);
				}
				*cbptr = cb->next;
				NhlFree(cb);
				continue;
			}
			cbptr = &cb->next;
		}
	}

	return;
}
