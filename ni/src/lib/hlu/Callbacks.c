/*
 *      $Id: Callbacks.c,v 1.3 1996-09-14 17:05:51 boote Exp $
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
#include <ncarg/hlu/CallbacksP.h>

_NhlCBList
_NhlCBCreate
#if	NhlNeedProto
(
	int		hash_mult,
	_NhlCBAddHash	add_hash,
	_NhlCBCallHash	call_hash
)
#else
(hash_mult,add_hash,call_hash)
	int		hash_mult;
	_NhlCBAddHash	add_hash;
	_NhlCBCallHash	call_hash;
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

	for(i=0;i<cblist->size;i++){
		cbt1 = cblist->hash[i];
		while(cbt1){
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

	cb->cblist = cblist;
	if(cblist->add_hash)
		cb->index = (*cblist->add_hash)(selector,udata);
	else
		cb->index = selector.lngval;

	cb->cbfunc = cbfunc;
	cb->udata = udata;
	cb->next = NULL;

	cbptr = &cblist->hash[cb->index & cblist->mask];
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

	cbptr = &cblist->hash[cb->index & cblist->mask];

	while(*cbptr){
		if(*cbptr == cb){
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

	if(!cblist)
		return;

	if(cblist->call_hash)
		index = (*cblist->call_hash)(selector,cbdata);
	else
		index = selector.lngval;

	cb = cblist->hash[index & cblist->mask];
	while(cb){
		/*
		 * Get the "next" pointer now, in case cbfunc does a
		 * CBDelete.
		 */
		tcb = cb->next;
		if(cb->index == index)
			(*cb->cbfunc)(cbdata,cb->udata);
		cb = tcb;
	}

	return;
}
