/*
 *      $Id: DataMgr.c,v 1.1 1993-07-12 22:36:12 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataMgr.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jun 24 10:22:34 MDT 1993
 *
 *	Description:	This class is used to control the conversion process
 *			of a DataItemLayerClass.  There will be a DataMgr
 *			created for each instance of a DataItem Class object.
 *			Since this class is basically the control mechanism
 *			for the DataItem class, it includes the Private
 *			header file for the DataItem class - It dereferences
 *			fields in the DataItem directly instead of going
 *			threw the get/set values mechanism.
 */
#include <stdio.h>
#include <ncarg/hlu/DataMgrP.h>
#include <ncarg/hlu/DataItemP.h>

/************************************************************************
*									*
*	DataMgr Class declarations					*
*									*
************************************************************************/

static NhlErrorTypes DataMgrInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataMgrDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

DataMgrLayerClassRec dataMgrLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* superclass			*/	(LayerClass)&baseLayerClassRec,
/* class_name			*/	"DataMgr",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(DataMgrLayerRec),
/* layer_resources		*/	NULL,
/* num_resources		*/	0,
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	DataMgrInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	DataMgrDestroy
	},
	/* DataMgrLayerClassPart */
	{
/* foo				*/	NULL
	}
};
	
LayerClass dataMgrLayerClass = (LayerClass)&dataMgrLayerClassRec;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/* none yet */

/************************************************************************
*									*
*	Methode definitions						*
*									*
************************************************************************/

/*
 * Function:	DataMgrInitialize
 *
 * Description:	This function initializes the DataMgr instance.
 *
 * In Args:	
 *		LayerClass	lc,	class
 *		Layer		req,	requested
 *		Layer		new,	new
 *		_NhlArgList	args,	args
 *		int		nargs	nargs
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
DataMgrInitialize
#if	__STDC__
(
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	LayerClass	lc;	/* class	*/
	Layer		req;	/* requested	*/
	Layer		new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	DataMgrLayer	dmgr = (DataMgrLayer)new;

	dmgr->datamgr.uptodate = True;
	dmgr->datamgr.connection_list = NULL;
	dmgr->datamgr.data_list = NULL;

	return NOERROR;
}

/*
 * Function:	FreeHandles
 *
 * Description:	This function free's a list of data handles.
 *
 * In Args:	
 *		_NhlDHandle	list	list of handles to free
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
FreeHandles
#if	__STDC__
(
	_NhlDHandle	list	/* list of handles to free	*/
)
#else
(list)
	_NhlDHandle	list;	/* list of handles to free	*/
#endif
{
	/*
	 * If null terminate recursion
	 */
	if(list == NULL)
		return;

	FreeHandles(list->next);
	(void)NhlFree(list);

	return;
}

/*
 * Function:	FreeCache
 *
 * Description:	This function free's a list of _NhlDCache structures
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
FreeCache
#if	__STDC__
(
	_NhlDCache	list	/* list to free	*/
)
#else
(list)
	_NhlDCache	list;	/* list to free	*/
#endif
{
	/*
	 * terminate recursion on NULL
	 */
	if(list == NULL)
		return;

	FreeCache(list->next);

	(void)NhlDestroy(list->dataset_id);
	_NhlFreeConvertContext(list->cvt_context);
	(void)NhlFree(list);

	return;
}

/*
 * Function:	DataMgrDestroy
 *
 * Description:	This fuction is used to free all the memory allocated on
 *		behalf of the DataMgr layer given.
 *
 * In Args:	
 *		Layer	l	layer to destroy
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataMgrDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
{
	DataMgrLayer	mgr = (DataMgrLayer)l;

	FreeHandles(mgr->datamgr.connection_list);
	FreeCache(mgr->datamgr.data_list);

	return NOERROR;
}

/************************************************************************
*									*
*	Private API for sub-classes to add additional data.		*
*									*
************************************************************************/

/* none yet */


/************************************************************************
*									*
*	Private API for DataComm Class to communicate with the DataMgr	*
*									*
************************************************************************/

/*
 * Function:	_NhlInitDataConnection
 *
 * Description:	This function is used to allocate a Data Handle record in
 *		the manager so the manager can keep track of the datacomm
 *		Layers it is providing data for.
 *
 * In Args:	
 *		DataItemLayer	item,		DataItem sub-class
 *		NrmQuark	type_req	type wanted
 *
 * Out Args:	
 *
 * Scope:	Global - Privately used by DataComm class
 * Returns:	_NhlDHandle
 * Side Effect:	
 */
_NhlDHandle
_NhlInitDataConnection
#if	__STDC__
(
	DataItemLayer	item,		/* DataItem sub-class	*/
	NrmQuark	type_req	/* type wanted		*/
)
#else
(item,type_req)
	DataItemLayer	item;		/* DataItem sub-class	*/
	NrmQuark	type_req;	/* type wanted		*/
#endif
{
	DataMgrLayer	mgr = (DataMgrLayer)item->dataitem.manager;
	_NhlDHandle	new = NhlMalloc(sizeof(_NhlDHandleRec));

	if(new == NULL){
		NhlPError(FATAL,ENOMEM,NULL);
		return NULL;
	}

	new->type = type_req;
	new->cache = NULL;
	new->next = mgr->datamgr.connection_list;
	mgr->datamgr.connection_list = new;

	return new;
}

/*
 * Function:	ReleaseCache
 *
 * Description:	This function is used to release a cache reference.  If
 *		the ref_count goes below 1 and the cache is not uptodate
 *		the dataset is Destroyed.
 *
 * In Args:	
 *		DataMgrLayer	mgr,	manager
 *		_NhlDCache	cache	ptr to node to release
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
ReleaseCache
#if	__STDC__
(
	DataMgrLayer	mgr,	/* manager			*/
	_NhlDCache	cache	/* ptr to node to release	*/
)
#else
(mgr,cache)
	DataMgrLayer	mgr;	/* manager			*/
	_NhlDCache	cache;	/* ptr to node to release	*/
#endif
{
	_NhlDCache	*cptr = NULL;

	(cache->ref_count)--;

	if(!cache->uptodate && (cache->ref_count < 1)){
		NhlDestroy(cache->dataset_id);
		_NhlFreeConvertContext(cache->cvt_context);

		for(cptr = &mgr->datamgr.data_list;
					*cptr != NULL; cptr = &((*cptr)->next)){
			if(*cptr == cache){
				*cptr = cache->next;
				(void)NhlFree(cache);

				break;
			}
		}
	}

	return;
}

/*
 * Function:	CreateCache
 *
 * Description:	This function creates a cache record.  It calls the converter
 *		and returns the information in the _NhlDCache record.
 *
 * In Args:	
 *		DataMgrLayer	mgr,	data manager
 *		NrmQuark	type	convert to type
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	_NhlDCache
 * Side Effect:	
 */
static _NhlDCache
CreateCache
#if	__STDC__
(
	DataMgrLayer	mgr,	/* data manager		*/
	NrmQuark	type	/* convert to type	*/
)
#else
(mgr,type)
	DataMgrLayer	mgr;	/* data manager		*/
	NrmQuark	type;	/* convert to type	*/
#endif
{
	_NhlDCache	new;
	NhlErrorTypes	ret = NOERROR;
	NrmQuark	fromQ =
		mgr->base.parent->base.layer_class->base_class.nrm_class;
	NrmValue	fromdata,todata;

	/*
	 * First check cache and see if the requested type is already there.
	 */
	for(new = mgr->datamgr.data_list;new != NULL; new = new->next){
		if(new->type == type){
			new->ref_count++;
			return new;
		}
	}

	new = NhlMalloc(sizeof(_NhlDCacheRec));
	if(new == NULL){
		NhlPError(FATAL,ENOMEM,NULL);
		return NULL;
	}

	new->type = type;
	new->uptodate = True;
	new->ref_count = 1;
	new->cvt_context = _NhlCreateConvertContext();
	if(new->cvt_context == NULL){
		NhlPError(FATAL,ENOMEM,NULL);
		(void)NhlFree(new);
		return NULL;
	}

	fromdata.size = sizeof(int);
	fromdata.addr = &mgr->base.parent->base.id;
	todata.size = sizeof(int);
	todata.addr = &new->dataset_id;

	ret = _NhlConvertData(new->cvt_context,fromQ,type,&fromdata,&todata);
	if(ret < WARNING){
		NhlPError(FATAL,E_UNKNOWN,"Unable to convert from %s to %s",
				NrmNameToString(fromQ),NrmNameToString(type));
		_NhlFreeConvertContext(new->cvt_context);
		(void)NhlFree(new);
		return NULL;
	}

	/*
	 * Conversion was successful, add new to data_list
	 */
	new->next = mgr->datamgr.data_list;
	mgr->datamgr.data_list = new;

	return new;
}

/*
 * Function:	UpdateCacheList
 *
 * Description:	This is a recursive function that updates the data-cache.
 *		It works on the node that it is called with and calls itself
 *		with node->next and returns either the node it is working
 *		on, or the return value of the rest of the list.
 *
 * In Args:	
 *		_NhlDCache	node	node to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	_NhlDCache
 * Side Effect:	
 */
static _NhlDCache
UpdateCacheList
#if	__STDC__
(
	_NhlDCache	node	/* node to update	*/
)
#else
(node)
	_NhlDCache	node;	/* node to update	*/
#endif
{
	_NhlDCache	tnode=NULL;

	/*
	 * If called with NULL return NULL - this terminates the recursion.
	 */
	if(node == NULL)
		return NULL;

	/*
	 * If this node is no-longer referenced, free it and return the
	 * updated version of the rest of the list.
	 */
	if(node->ref_count < 1){
		tnode = node->next;
		NhlDestroy(node->dataset_id);
		_NhlFreeConvertContext(node->cvt_context);
		(void)NhlFree(node);

		return UpdateCacheList(tnode);
	}

	/*
	 * Since this node is still referenced, do not destroy it - but
	 * it does need to be marked as out-of-date, and the rest of
	 * the list need's to be updated.
	 */
	node->uptodate = False;
	node->next = UpdateCacheList(node->next);

	return node;
}

/*
 * Function:	_NhlRetrieveData
 *
 * Description:	This function is used retrieve the converted data from the
 *		DataMgr.  The data mgr first determines if the data has
 *		already been converted.  If it has, and it is uptodate, then
 *		it returns the id of that dataset object.  Otherwise, it
 *		converts the data for the caller and returns the newly created
 *		dataset objects id.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global - Privately used by DataComm class
 * Returns:	NhlBoolean - SUCCESS==True
 * Side Effect:	
 */
NhlBoolean
_NhlRetrieveData
#if	__STDC__
(
	DataItemLayer	item,		/* dataItem sub-class	*/
	_NhlDHandle	dhandle,	/* id for Connection	*/
	NhlBoolean	*new,		/* is data new/changed	*/
	int		*dset_ret	/* rtrn dataset object	*/
)
#else
(item,dhandle,new,dset_ret)
	DataItemLayer	item;		/* dataItem sub-class	*/
	_NhlDHandle	dhandle;	/* id for Connection	*/
	NhlBoolean	*new;		/* is data new/changed	*/
	int		*dset_ret;	/* rtrn dataset object	*/
#endif
{
	DataMgrLayer	mgr = (DataMgrLayer)item->dataitem.manager;
	_NhlDHandle	thandle = mgr->datamgr.connection_list;

	/*
	 * First make sure the given dhandle exists in the manager
	 */
	while(thandle != NULL){
		if (thandle == dhandle)
			break;
		thandle = thandle->next;
	}
	if(thandle == NULL){
		NhlPError(FATAL,E_UNKNOWN,
	"_NhlRetrieveData:The given dhandle does not exist in DataItem %s",
							NhlName(item->base.id));
		return False;
	}

	/*
	 * If the data has changed since the last time Retieve was called,
	 * update the cache.
	 */
	if(!mgr->datamgr.uptodate){
		mgr->datamgr.data_list =UpdateCacheList(mgr->datamgr.data_list);
		mgr->datamgr.uptodate = True;
	}

	/*
	 * If the dhandle points to a cache
	 */
	if(dhandle->cache != NULL){
		/*
		 * If the given dhandle points to an uptodate valid cache then
		 * return it without messing around further.
		 */
		if(dhandle->cache->uptodate){
			*new = False;
			*dset_ret = dhandle->cache->dataset_id;

			return True;
		}

		/*
		 * The cache is not uptodate - release it.
		 */
		ReleaseCache(mgr,dhandle->cache);
	}

	/*
	 * Create a cache
	 */

	dhandle->cache = CreateCache(mgr,dhandle->type);
	if(dhandle->cache == NULL){
		NhlPError(FATAL,E_UNKNOWN,
		"_NhlRetrieveData:Unable to convert data in DataItem %s",
							NhlName(item->base.id));
		return False;
	}

	/*
	 * Conversion was successful
	 */
	*new = True;
	*dset_ret = dhandle->cache->dataset_id;

	return True;
}

/*
 * Function:	_NhlCloseDataConnection
 *
 * Description:	This function is called by the DataComm class when it is
 *		done using the given connection to the data.  This allows
 *		the DataMgr to keep a ref_count of the data, and free data
 *		when it is no longer needed.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global - Privately used by DataComm class
 * Returns:	void
 * Side Effect:	
 */
void
_NhlCloseDataConnection
#if	__STDC__
(
	DataItemLayer	item,	/* DataItem sub-class	*/
	_NhlDHandle	dhandle	/* id for Connection	*/
)
#else
(item,dhandle)
	DataItemLayer	item;		/* DataItem sub-class	*/
	_NhlDHandle	dhandle;	/* id for Connection	*/
#endif
{
	DataMgrLayer	mgr = (DataMgrLayer)item->dataitem.manager;
	_NhlDHandle	*dhptr;

	for(dhptr = &mgr->datamgr.connection_list; *dhptr != NULL;
						dhptr = &((*dhptr)->next)){
		if(*dhptr == dhandle){
			*dhptr = dhandle->next;
			ReleaseCache(mgr,dhandle->cache);
			(void)NhlFree(dhandle);

			return;
		}
	}

	NhlPError(WARNING,E_UNKNOWN,
		"_NhlCloseDataConnection:Unable to find dhandle in %s",
							NhlName(item->base.id));
	return;
}

/************************************************************************
*									*
*	Private API for DataItem Class to communicate with the DataMgr	*
*									*
************************************************************************/

/*
 * Function:	_NhlDataModified
 *
 * Description:	This function is used by the DataItem class to notify
 *		the manager that it's cached data is out-of-date due
 *		to a change in the DataItem.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Private to DataItem class
 * Returns:	void
 * Side Effect:	
 */
void
_NhlDataItemModified
#if	__STDC__
(
	DataMgrLayer	mgr	/* DataMgr	*/
)
#else
(mgr)
	DataMgrLayer	mgr;	/* DataMgr	*/
#endif
{
	mgr->datamgr.uptodate = False;

	return;
}
