/*
 *      $Id: DataComm.c,v 1.1 1993-07-12 22:35:58 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataComm.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 16 17:39:20 MDT 1993
 *
 *	Description:	This class is used to manage all the data a plot
 *			may use.  It maintains a list of all the data
 *			objects.
 */
#include <stdio.h>
#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/DataMgrF.h>

/************************************************************************
*									*
*	DataComm Class declarations					*
*									*
************************************************************************/

static NhlErrorTypes DataCommInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataCommDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

DataCommLayerClassRec dataCommLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* superclass			*/	(LayerClass)&transformLayerClassRec,
/* class_name			*/	"DataComm",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(DataCommLayerRec),
/* layer_resources		*/	NULL,
/* num_resources		*/	0,
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	DataCommInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	DataCommDestroy
	},
	/* ViewLayerClassPart */
	{
/* segment_wkid			*/	NULL,
/* get_bb			*/	NULL
	},
	/* TransformLayerClassPart */
	{
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL
	},
	/* DataCommLayerClassPart */
	{
/* foo				*/	NULL
	}
};
	
LayerClass dataCommLayerClass = (LayerClass)&dataCommLayerClassRec;

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
 * Function:	DataCommInitialize
 *
 * Description:	This function initializes the DataComm instance.
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
DataCommInitialize
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
	DataCommLayer	comm = (DataCommLayer)new;

	comm->datacomm.data_list = NULL;

	return NOERROR;
}

/*
 * Function:	FreeDataList
 *
 * Description:	This function is used to free an _NhlDataList.
 *
 * In Args:	
 *		_NhlDataList	list	list to free
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
FreeDataList
#if	__STDC__
(
	_NhlDataList	list	/* list to free	*/
)
#else
(list)
	_NhlDataList	list;	/* list to free	*/
#endif
{
	DataItemLayer	item;

	if(list == NULL)
		return;

	FreeDataList(list->next);

	item = (DataItemLayer)_NhlGetLayer(list->item);

	if((item == NULL) || (!_NhlIsDataItem(item))){
		NHLPERROR((WARNING,E_UNKNOWN,
			"FreeDataList:DataList is corrupted - bad item value"));
	}
	else{
		/*
		 * Close Data Connection in Mgr
		 */
		_NhlCloseDataConnection(item,list->dhandle);
	}

	(void)NhlFree(list);

	return;
}

/*
 * Function:	DataCommDestroy
 *
 * Description:	This fuction is used to free all the memory allocated on
 *		behalf of the DataComm layer given. It closes any data
 *		connections that have not been closed as they should have
 *		been by sub-classes.
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
DataCommDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
{
	DataCommLayer	comm = (DataCommLayer)l;

	FreeDataList(comm->datacomm.data_list);

	return NOERROR;
}

/************************************************************************
*									*
*	Private API for sub-classes to add additional data.		*
*									*
************************************************************************/

/*
 * Function:	_NhlDataConverterType
 *
 * Description:	This function determines if there is a converter to one of the
 *		requested types.  It returns the first one it finds - it
 *		returns NrmNULL_QUARK if it doesn't find one.
 *
 * In Args:	
 *		int	item_id,	data item for (from)
 *		...			list of Q's for (to)
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NrmQuark
 * Side Effect:	
 */
NrmQuark
_NhlDataConverterType
#if	NeedVarArgProto
(
	int	item_id,	/* data item for (from)	*/
	...			/* list of Q's for (to)	*/
)
#else
(item_id, va_alist)
	int	item_id;	/* data item for (from)	*/
	va_dcl			/* list of Q's for (to)	*/
#endif	/* NeedVarArgProto */
{
	va_list		ap;
	Layer		item = _NhlGetLayer(item_id);
	NrmQuark	to,from;

	if((item == NULL) || (!_NhlIsDataItem(item))){
		NHLPERROR((FATAL,E_UNKNOWN,
		"_NhlDataConverterType:not called with a DataItem sub-class"));
		return FATAL;
	}

	from = item->base.layer_class->base_class.nrm_class;

	VA_START(ap,item);
	for(to = va_arg(ap,NrmQuark);to!=NrmNULLQUARK;to = va_arg(ap,NrmQuark)){
		if(_NhlConverterExists(from,to))
			break;
	}
	va_end(ap);

	return to;
}

/*
 * Function:	_NhlRegisterData
 *
 * Description:	This function is used to add this data to the list of data
 *		kept in the DataComm superclass.  It keeps a seperate record
 *		for each time this function is called.  When the plot is
 *		done with a piece of data, it should call _NhlFreeData to
 *		remove this record.
 *		
 * In Args:	
 *		Layer		self,		DataComm sub-class
 *		int		item_id,	DataItem sub-class
 *		NrmQuark	type_requested,	Q of type wanted
 *
 * Out Args:	
 *
 * Scope:	DataComm sub-classes
 * Returns:	_NhlDHandle
 * Side Effect:	
 */
/*ARGSUSED*/
_NhlDHandle
_NhlRegisterData
#if	__STDC__
(
	Layer		self,		/* DataComm sub-class		*/
	int		item_id,	/* DataItem sub-class		*/
	NrmQuark	type_requested	/* Q of type wanted		*/
)
#else
(self,item_id,type_requested)
	Layer		self;		/* DataComm sub-class		*/
	int		item_id;	/* DataItem sub-class		*/
	NrmQuark	type_requested;	/* Q of type wanted		*/
#endif
{
	_NhlDHandle	dhandle = NULL;
	_NhlDataList	dnode = NULL;
	DataCommLayer	comm = (DataCommLayer)self;
	DataItemLayer	item = (DataItemLayer)_NhlGetLayer(item_id);

	if((comm == NULL) || (!_NhlIsDataComm(comm))){
		NhlPError(FATAL,E_UNKNOWN,
		"_NhlRegisterData: must be called with a DataComm sub-class");
		return NULL;
	}
	if((item == NULL) || (!_NhlIsDataItem(item))){
		NhlPError(FATAL,E_UNKNOWN,
		"_NhlRegisterData: must be called with a DataItem sub-class");
		return NULL;
	}

	if(type_requested !=
		_NhlDataConverterType(item_id,type_requested,NrmNULLQUARK)){
		NHLPERROR((FATAL,E_UNKNOWN,
			"Unable to find a conversion for dataitem %s to %s",
			NhlName(item_id),NrmNameToString(type_requested)));
		return NULL;
	}

	/*
	 * Call function in DataMgr that acts upon the DataItem to create
	 * the DHandle.
	 */
	dhandle = _NhlInitDataConnection(item,type_requested);
	if(dhandle == NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"_NhlRegisterData:Unable to inialize Data Connection");
		return NULL;
	}

	/*
	 * Allocate a _NhlDataList node to save the data-connection
	 */
	dnode = (_NhlDataList)NhlMalloc(sizeof(_NhlDataListRec));
	if(dnode == NULL){
		NhlPError(FATAL,ENOMEM,NULL);
		_NhlCloseDataConnection(item,dhandle);
		return NULL;
	}

	/*
	 * insert the dnode into the list of data used by the DataComm sub-class
	 */
	dnode->item = item_id;
	dnode->dhandle = dhandle;
	dnode->next = comm->datacomm.data_list;
	comm->datacomm.data_list = dnode;

	/*
	 * Return the DHandle
	 */
	 return dhandle;
}

/*
 * Function:	_NhlGetDataSet
 *
 * Description:	This function is used to actually convert the data in the data
 *		item into the specific data type needed by the plot.  It
 *		should be called each time the plot access the data.  It will
 *		return True if it succeded in getting the data. new_ret
 *		indicates if the data being retrieved has been retrieved via
 *		this same DHandle previously.
 *
 * In Args:	
 *		int		item_id,	DataItem sub-class
 *		_NhlDHandle	dhandle,	data id
 *
 * Out Args:	
 *		NhlBoolean	*new_ret,	is data new?
 *		int		*dset_ret	actual data object
 *
 * Scope:	DataComm sub-classes
 * Returns:	NhlBoolean - True==Success
 * Side Effect:	
 */
/*ARGSUSED*/
NhlBoolean
_NhlGetDataSet
#if	__STDC__
(
	int		item_id,	/* DataItem sub-class	*/
	_NhlDHandle	dhandle,	/* data id		*/
	NhlBoolean	*new_ret,	/* is data new?		*/
	int		*dset_ret	/* actual data object	*/
)
#else
(item_id,dhandle,new_ret,dset_ret)
	int		item_id;	/* DataItem sub-class	*/
	_NhlDHandle	dhandle;	/* data id		*/
	NhlBoolean	*new_ret;	/* is data new?		*/
	int		*dset_ret;	/* rtrn dataset object	*/
#endif
{
	DataItemLayer	item = (DataItemLayer)_NhlGetLayer(item_id);

	if((item == NULL) || (!_NhlIsDataItem(item))){
		NhlPError(FATAL,E_UNKNOWN,
		"_NhlGetDataSet: must be called with a DataItem sub-class");
		return NULL;
	}

	/*
	 * Retrieve acual dataset from DateMgr
	 */
	 return _NhlRetrieveData(item,dhandle,new_ret,dset_ret);
}

/*
 * Function:	_NhlReleaseData
 *
 * Description:	This function is used to tell that DataMgr that this plot
 *		is nolonger going to use this connection to the data.  It
 *		is also used to free the datalist in the DataComm superclass.
 *		If it hasn't happened by the time the DataCommDestroy function
 *		is called this function will be called on every datalist node
 *		still in the list.
 *
 * In Args:	
 *		Layer		self,		DataComm sub-class
 *		int		item_id,	DataItem sub-class
 *		_NhlDHandle	dhandle		data id
 *
 * Out Args:	
 *
 * Scope:	DataComm sub-classes
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
void
_NhlReleaseData
#if	__STDC__
(
	Layer		self,		/* DataComm sub-class	*/
	int		item_id,	/* DataItem sub-class	*/
	_NhlDHandle	dhandle		/* data id		*/
)
#else
(self,item_id,dhandle)
	Layer		self;		/* DataComm sub-class	*/
	int		item_id;	/* DataItem sub-class	*/
	_NhlDHandle	dhandle;	/* data id		*/
#endif
{
	DataCommLayer	comm = (DataCommLayer)self;
	DataItemLayer	item = (DataItemLayer)_NhlGetLayer(item_id);
	_NhlDataList	*list = NULL;
	NhlBoolean	found = False;

	if((comm == NULL) || (!_NhlIsDataComm(comm))){
		NhlPError(FATAL,E_UNKNOWN,
		"_NhlReleaseData: must be called with a DataComm sub-class");
		return;
	}

	if((item == NULL) || (!_NhlIsDataItem(item))){
		NhlPError(FATAL,E_UNKNOWN,
		"_NhlReleaseData: must be called with a DataItem sub-class");
		return;
	}

	/*
	 * Close Data Connection in Mgr
	 */
	_NhlCloseDataConnection(item,dhandle);

	/*
	 * Free the DataList node in the DataComm part
	 */
	for(list = &comm->datacomm.data_list;
					*list != NULL;
						list = &((*list)->next)){
		if((*list)->dhandle == dhandle){
			_NhlDataList	tmp = *list;

			*list = (*list)->next;
			(void)NhlFree(tmp);
			found = True;

			break;
		}
	}

	if(!found){
		NhlPError(WARNING,E_UNKNOWN,
		"_NhlReleaseData:Unable to find DHandle in list of Data");
	}

	return;
}
