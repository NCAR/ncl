/*
 *      $Id: DataMgr.c,v 1.4 1994-02-01 18:22:37 boote Exp $
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
 *			of a NhlDataItemLayerClass.  There will be a DataMgr
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
#include <ncarg/hlu/DataCommF.h>

/************************************************************************
*									*
*	DataMgr Class declarations					*
*									*
************************************************************************/

static NhlErrorTypes DataMgrInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataMgrDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

NhlDataMgrLayerClassRec NhldataMgrLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"DataMgr",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlDataMgrLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

/* layer_resources		*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	DataMgrInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	DataMgrDestroy
	},
	/* NhlDataMgrLayerClassPart */
	{
/* foo				*/	0
	}
};
	
NhlLayerClass NhldataMgrLayerClass = (NhlLayerClass)&NhldataMgrLayerClassRec;

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
 *		NhlLayerClass	lc,	class
 *		NhlLayer	req,	requested
 *		NhlLayer	new,	new
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
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass	lc;	/* class	*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	NhlDataMgrLayer	dmgr = (NhlDataMgrLayer)new;

	dmgr->datamgr.uptodate = True;
	dmgr->datamgr.connection_list = NULL;
	dmgr->datamgr.data_list = NULL;
	dmgr->datamgr.dspec_list = NULL;

	return NhlNOERROR;
}

/*
 * Function:	ReleaseDSpecs
 *
 * Description:	This function is used to remove the dataitem parent from
 *		each DataSpec class it is used in.  It just calls the
 *		private _NhlReleaseDMgr call to the dspec to do this.
 *
 *
 * In Args:	
 *		_NhlDSpec	list,	list of nodes to free
 *		int		ditemid	dataitem parent of mgr
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
ReleaseDSpecs
#if	__STDC__
(
	_NhlDSpec	list,	/* list of nodes to free	*/
	int		ditemid	/* dataitem parent of mgr	*/
)
#else
(list,ditemid)
	_NhlDSpec	list;		/* list of nodes to free	*/
	int		ditemid;	/* dataitem parent of mgr	*/
#endif
{
	/*
	 * If null terminate recursion
	 */
	if(list == NULL)
		return;

	ReleaseDSpecs(list->next,ditemid);
	_NhlReleaseDMgr(list->dspec_id,ditemid);
	(void)NhlFree(list);

	return;
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
	_NhlDHandle	list;		/* list of handles to free	*/
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
 * Function:	ReleaseHandles
 *
 * Description:	This function is used to remove the dataitem parent from
 *		each DataComm class it is used in.  It just calls the
 *		public NhlRemoveData function to do this.  The datacomm
 *		class will then call functions that will re-enter this
 *		object, hopefully freeing the handle in the manager.
 *
 *
 * In Args:	
 *		_NhlDHandle	list,	list of handles to free
 *		int		ditemid	dataitem parent of mgr
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
ReleaseHandles
#if	__STDC__
(
	_NhlDHandle	list,	/* list of handles to free	*/
	int		ditemid	/* dataitem parent of mgr	*/
)
#else
(list,ditemid)
	_NhlDHandle	list;		/* list of handles to free	*/
	int		ditemid;	/* list of handles to free	*/
#endif
{
	/*
	 * If null terminate recursion
	 */
	if(list == NULL)
		return;

	ReleaseHandles(list->next,ditemid);
	(void)NhlRemoveData(list->datacommid,NrmQuarkToString(list->res_name),
								ditemid);

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

	(void)NhlDestroy(list->dataset->base.id);
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
 *		NhlLayer	l	layer to destroy
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
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlDataMgrLayer	mgr = (NhlDataMgrLayer)l;

	/*
	 * if the datacomm classes do the right thing ReleaseHandles should
	 * free all the handles, but just in case FreeHandles free's anything
	 * still in the list.
	 */
	ReleaseDSpecs(mgr->datamgr.dspec_list,mgr->base.parent->base.id);
	ReleaseHandles(mgr->datamgr.connection_list,mgr->base.parent->base.id);
	FreeHandles(mgr->datamgr.connection_list);
	FreeCache(mgr->datamgr.data_list);

	return NhlNOERROR;
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
 *		NhlLayers it is providing data for.
 *
 * In Args:	
 *		NhlDataItemLayer	item,		DataItem sub-class
 *		NrmQuark	*type_req	type wanted
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
	NhlLayer		l,		/* DataItem sub-class	*/
	int			dcommid,	/* id for datacomm layer*/
	NrmQuark		res_name,	/* resource name	*/
	NrmQuark		*type_req,	/* type wanted		*/
	NrmQuark		*type_ret	/* type will be created	*/
)
#else
(l,dcommid,res_name,type_req,type_ret)
	NhlLayer		l;		/* DataItem sub-class	*/
	int			dcommid;	/* id for datacomm layer*/
	NrmQuark		res_name;	/* resource name	*/
	NrmQuark		*type_req;	/* type wanted		*/
	NrmQuark		*type_ret;	/* type will be created	*/
#endif
{
	NhlDataItemLayer	item = (NhlDataItemLayer)l;
	NhlDataMgrLayer		mgr = (NhlDataMgrLayer)item->dataitem.manager;
	_NhlDHandle		new;
	NrmQuark		from =
				item->base.layer_class->base_class.nrm_class;
	NrmQuark		*type;

	if(mgr == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"_NhlInitDataConnection:Called without a Data Manager");
		return NULL;
	}

	type = type_req;
	while(*type != NrmNULLQUARK){
		if(_NhlConverterExists(from,*type,NrmNULLQUARK))
			break;
		type++;
	}
	if(*type == NrmNULLQUARK){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"No Conversion available");
		return NULL;
	}

	*type_ret = *type;

	new = NhlMalloc(sizeof(_NhlDHandleRec));
	if(new == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}

	new->datacommid = dcommid;
	new->res_name = res_name;
	new->type = *type;
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
 *		NhlDataMgrLayer	mgr,	manager
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
	NhlDataMgrLayer	mgr,	/* manager			*/
	_NhlDCache	cache	/* ptr to node to release	*/
)
#else
(mgr,cache)
	NhlDataMgrLayer	mgr;	/* manager			*/
	_NhlDCache	cache;	/* ptr to node to release	*/
#endif
{
	_NhlDCache	*cptr = NULL;

	(cache->ref_count)--;

	if(!cache->uptodate && (cache->ref_count < 1)){
		NhlDestroy(cache->dataset->base.id);
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
 *		NhlDataMgrLayer	mgr,	data manager
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
	NhlDataMgrLayer	mgr,	/* data manager		*/
	NrmQuark	type	/* convert to type	*/
)
#else
(mgr,type)
	NhlDataMgrLayer	mgr;	/* data manager		*/
	NrmQuark	type;	/* convert to type	*/
#endif
{
	_NhlDCache	new;
	int		dataset_id;
	NhlErrorTypes	ret = NhlNOERROR;
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
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}

	new->type = type;
	new->uptodate = True;
	new->ref_count = 1;
	new->cvt_context = _NhlCreateConvertContext();
	if(new->cvt_context == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		(void)NhlFree(new);
		return NULL;
	}

	fromdata.size = sizeof(int);
	fromdata.addr = &mgr->base.parent->base.id;
	todata.size = sizeof(int);
	todata.addr = &dataset_id;

	ret = _NhlConvertData(new->cvt_context,fromQ,type,&fromdata,&todata);
	new->dataset = _NhlGetLayer(dataset_id);
	if((ret < NhlWARNING) || (new->dataset == NULL)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to convert from %s to %s",
				NrmNameToString(fromQ),NrmNameToString(type));
		_NhlFreeConvertContext(new->cvt_context);
		if(new->dataset != NULL)
			(void)NhlDestroy(new->dataset->base.id);
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
		NhlDestroy(node->dataset->base.id);
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
 *		NhlDataItemLayer	item,		dataItem sub-class
 *		_NhlDHandle	dhandle,	id for Connection
 *
 * Out Args:	
 *		NhlBoolean	*new,		is data new/changed
 *		int		*dset_ret	rtrn dataset object
 *
 * Scope:	Global - Privately used by DataComm class
 * Returns:	NhlLayer - Failure==NULL
 * Side Effect:	
 */
NhlLayer
_NhlRetrieveData
#if	__STDC__
(
	NhlLayer		l,		/* dataItem sub-class	*/
	_NhlDHandle		dhandle,	/* id for Connection	*/
	NhlBoolean		*new		/* is data new/changed	*/
)
#else
(l,dhandle,new)
	NhlLayer		l;		/* dataItem sub-class	*/
	_NhlDHandle		dhandle;	/* id for Connection	*/
	NhlBoolean		*new;		/* is data new/changed	*/
#endif
{
	NhlDataItemLayer	item = (NhlDataItemLayer)l;
	NhlDataMgrLayer		mgr = (NhlDataMgrLayer)item->dataitem.manager;
	_NhlDHandle		thandle;

	/*
	 * set new in case caller didn't
	 */
	*new = False;

	if(mgr == NULL){
	NhlPError(NhlFATAL,NhlEUNKNOWN,
				"_NhlRetrieveData:Called without a Data Mgr");
		return NULL;
	}

	thandle = mgr->datamgr.connection_list;

	/*
	 * First make sure the given dhandle exists in the manager
	 */
	while(thandle != NULL){
		if (thandle == dhandle)
			break;
		thandle = thandle->next;
	}
	if(thandle == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"_NhlRetrieveData:The given dhandle does not exist in DataItem %s",
							NhlName(item->base.id));
		return NULL;
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
			return dhandle->cache->dataset;
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"_NhlRetrieveData:Unable to convert data in DataItem %s",
							NhlName(item->base.id));
		return NULL;
	}

	/*
	 * Conversion was successful
	 */
	*new = True;
	return dhandle->cache->dataset;
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
 *		NhlDataItemLayer	item,	DataItem sub-class
 *		_NhlDHandle	dhandle	id for Connection
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
	NhlLayer		l,	/* DataItem sub-class	*/
	_NhlDHandle		dhandle	/* id for Connection	*/
)
#else
(l,dhandle)
	NhlLayer		l;		/* DataItem sub-class	*/
	_NhlDHandle		dhandle;	/* id for Connection	*/
#endif
{
	NhlDataItemLayer	item = (NhlDataItemLayer)l;
	NhlDataMgrLayer		mgr = (NhlDataMgrLayer)item->dataitem.manager;
	_NhlDHandle		*dhptr;

	if(mgr == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"_NhlCloseDataConnection:called without a DataMgr");
		return;
	}

	for(dhptr = &mgr->datamgr.connection_list; *dhptr != NULL;
						dhptr = &((*dhptr)->next)){
		if(*dhptr == dhandle){
			*dhptr = dhandle->next;
			ReleaseCache(mgr,dhandle->cache);
			(void)NhlFree(dhandle);

			return;
		}
	}

	NhlPError(NhlWARNING,NhlEUNKNOWN,
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
 * Function:	_NhlDataItemModified
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
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
void
_NhlDataItemModified
#if	__STDC__
(
	NhlLayer	l	/* DataMgr	*/
)
#else
(l)
	NhlLayer	l;	/* DataMgr	*/
#endif
{
	NhlDataMgrLayer	mgr = (NhlDataMgrLayer)l;

	if(mgr == NULL)
		return;

	mgr->datamgr.uptodate = False;

	return;
}

/*
 * Function:	_NhlNotifyDataComm
 *
 * Description:	This function is used by DataItem to notify the datacomm
 *		subclasses that the dataitem that is associated with the plot
 *		has changed in such a way that the form of the data may have
 *		changed, invalidating some of the resources in the plot.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Private to SetValues
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlNotifyDataComm
#if	__STDC__
(
	NhlLayer	l	/* dmgr layer */
)
#else
(l)
	NhlLayer	l;	/* dmgr layer */
#endif
{
	NhlDataMgrLayer	dmgr = (NhlDataMgrLayer)l;
	_NhlDHandle	list = NULL;
	NhlErrorTypes	ret = NhlNOERROR, lret = NhlNOERROR;

	if(dmgr == NULL)
		return NhlNOERROR;

	if(dmgr->datamgr.uptodate)
		return NhlNOERROR;
	
	list = dmgr->datamgr.connection_list;
	while(list != NULL){
		lret = _NhlUpdateData(list->datacommid);
		ret = MIN(lret,ret);
		list = list->next;
	}

	return ret;
}

/*
 * Function:	PushDSpec
 *
 * Description:	This function adds the given dspec id to the linked list.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlBoolean
 * Side Effect:	
 */
static NhlBoolean
PushDSpec
#if	__STDC__
(
	_NhlDSpec	*listptr,	/* ptr to dspec_list	*/
	int		dspecid		/* dspecid		*/
)
#else
(listptr,dspecid)
	_NhlDSpec	*listptr;	/* ptr to dspec_list	*/
	int		dspecid;	/* dspecid		*/
#endif
{
	if(*listptr != NULL)
		return PushDSpec(&(*listptr)->next,dspecid);

	*listptr = (_NhlDSpec)NhlMalloc(sizeof(_NhlDSpecRec));
	if(*listptr == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return False;
	}

	(*listptr)->dspec_id = dspecid;
	(*listptr)->next = NULL;

	return True;
}

/*
 * Function:	_NhlRegisterDSpec
 *
 * Description:	This function is used to notify the DataMgr that it is
 *		being used as part of a DataSpec object.  So if the dmgr gets
 *		destroyed, it should notify each of the dspec objects.
 *
 * In Args:	
 *		NhlDataItemLayer	item,		DataItem sub-class
 *		int		dspecid		id for dataspec layer
 *
 * Out Args:	
 *
 * Scope:	private to DataSpec class
 * Returns:	NhlBoolean
 * Side Effect:	
 */
NhlBoolean
_NhlRegisterDSpec
#if	__STDC__
(
	NhlLayer		l,		/* DataItem sub-class	*/
	int			dspecid		/* id for dataspec layer*/
)
#else
(l,dspecid)
	NhlLayer		l;		/* DataItem sub-class	*/
	int			dspecid;	/* id for dataspec layer*/
#endif
{
	NhlDataItemLayer	item = (NhlDataItemLayer)l;
	NhlDataMgrLayer		mgr = (NhlDataMgrLayer)item->dataitem.manager;

	if((mgr == NULL) || !_NhlIsDataMgr(mgr)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"_NhlRegisterDSpec:Called without a Data Manager");
		return False;
	}

	return PushDSpec(&mgr->datamgr.dspec_list,dspecid);
}

/*
 * Function:	PopDSpec
 *
 * Description:	This function removes the given dspec id from the linked list.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlBoolean
 * Side Effect:	
 */
static NhlBoolean
PopDSpec
#if	__STDC__
(
	_NhlDSpec	*listptr,	/* ptr to dspec_list	*/
	int		dspecid		/* dspecid		*/
)
#else
(listptr,dspecid)
	_NhlDSpec	*listptr;	/* ptr to dspec_list	*/
	int		dspecid;	/* dspecid		*/
#endif
{
	_NhlDSpec	tptr;

	if(*listptr == NULL)
		return False;

	if((*listptr)->dspec_id == dspecid){
		tptr = *listptr;
		*listptr = (*listptr)->next;
		(void)NhlFree(tptr);
		return True;
	}

	return PopDSpec(&(*listptr)->next,dspecid);
}

/*
 * Function:	_NhlUnRegisterDSpec
 *
 * Description:	This function is used to notify the DataMgr that it is
 *		no longer being used as part of a DataSpec object.
 *
 * In Args:	
 *		NhlDataItemLayer	item,		DataItem sub-class
 *		int		dspecid		id for dataspec layer
 *
 * Out Args:	
 *
 * Scope:	private to DataSpec class
 * Returns:	void
 * Side Effect:	
 */
void
_NhlUnRegisterDSpec
#if	__STDC__
(
	NhlLayer		l,		/* DataItem sub-class	*/
	int			dspecid		/* id for dataspec layer*/
)
#else
(l,dspecid)
	NhlLayer		l;		/* DataItem sub-class	*/
	int			dspecid;	/* id for dataspec layer*/
#endif
{
	NhlDataItemLayer	item = (NhlDataItemLayer)l;
	NhlDataMgrLayer	mgr = (NhlDataMgrLayer)item->dataitem.manager;

	if((mgr == NULL) || !_NhlIsDataMgr(mgr)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"_NhlUnRegisterDSpec:Called without a Data Manager");
		return;
	}

	if(!PopDSpec(&mgr->datamgr.dspec_list,dspecid))
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Unable to find %d in DataMgr's DSpec list",dspecid);

	return;
}
