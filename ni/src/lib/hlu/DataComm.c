/*
 *      $Id: DataComm.c,v 1.5 1994-01-06 19:50:30 boote Exp $
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
#include <ncarg/hlu/DataMgrF.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/VarArg.h>

/************************************************************************
*									*
*	DataComm Class declarations					*
*									*
************************************************************************/

#define	Oset(field)	NhlOffset(DataCommLayerRec,datacomm.field)
static NhlResource dcresources[] = {
	{NhlNdcDelayCompute,NhlCdcDelayCompute,NhlTBoolean,sizeof(NhlBoolean),
		Oset(delay_compute),NhlTImmediate,(NhlPointer)False}
};
#undef	Oset

#define Oset(field)	NhlOffset(DataSpecLayerRec,dataspec.field)
static NhlResource dsresources[] = {
	{NhlNdsDataItem,NhlCdsDataItem,NhlTInteger,sizeof(int),
/*SUPPRESS 25*/
		Oset(data_item),NhlTImmediate,(NhlPointer)NULL_LAYER}
};
#undef Oset

static NhlErrorTypes DataCommClassPartInitialize(
#if	NhlNeedProto
	LayerClass	lc	/* pointer to class structure to update */
#endif
);

static NhlErrorTypes DataCommClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes DataCommInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataCommSetValues(
#if	NhlNeedProto
	Layer		old,	/* old		*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataCommSetValuesHook(
#if	NhlNeedProto
	Layer		old,	/* old		*/
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

static NhlErrorTypes DataCommDraw(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

DataCommLayerClassRec dataCommLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* class_name			*/	"DataComm",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(DataCommLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&transformLayerClassRec,

/* layer_resources		*/	dcresources,
/* num_resources		*/	NhlNumber(dcresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	DataCommClassPartInitialize,
/* class_initialize		*/	DataCommClassInitialize,
/* layer_initialize		*/	DataCommInitialize,
/* layer_set_values		*/	DataCommSetValues,
/* layer_set_values_hook	*/	DataCommSetValuesHook,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	DataCommDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	DataCommDraw,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	/* ViewLayerClassPart */
	{
/* segment_wkid			*/	0,
/* get_bb			*/	NULL
	},
	/* TransformLayerClassPart */
	{
/* handles_overlays 		*/	False,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL
	},
	/* DataCommLayerClassPart */
	{
/* data_offsets			*/	NULL,
/* update_data			*/	NULL
	}
};
	
LayerClass dataCommLayerClass = (LayerClass)&dataCommLayerClassRec;

/*
 * DataSpec def's
 */

static NhlErrorTypes DataSpecInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataSpecSetValues(
#if	NhlNeedProto
	Layer		old,	/* old		*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataSpecDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

DataSpecLayerClassRec dataSpecLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* class_name			*/	"DataSpec",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(DataSpecLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&objLayerClassRec,

/* layer_resources		*/	dsresources,
/* num_resources		*/	NhlNumber(dsresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	DataSpecInitialize,
/* layer_set_values		*/	DataSpecSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	DataSpecDestroy
	},
	/* DataSpecLayerClassPart */
	{
/* foo				*/	0
	}
};
	
LayerClass dataSpecLayerClass = (LayerClass)&dataSpecLayerClassRec;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

static _NhlDataNodePtr CreateDataNode(
#if	NhlNeedProto
	DataCommLayer	dcl,	/* DataComm subclass using data		*/
	_NhlDataOffset	oset,	/* oset ptr - Data Description		*/
	Layer		dl	/* Data to connect to			*/
#endif
);

static void FreeDataNode(
#if	NhlNeedProto
	_NhlDataNodePtr	node	/* node to free	*/
#endif
);

/*
 * Function:	AddData
 *
 * Description:	This function adds additional data item's into the given
 *		DataList resource in the given Layer.
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:
 */
/*ARGSUSED*/
static NhlErrorTypes
AddData
#if	__STDC__
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	_NhlDataNodePtr		dnode=NULL;
	int			i;
	NhlGenArray		gen=NULL,newgen=NULL;
	_NhlInternDataList	dlistold=NULL,dlistnew=NULL;
	Layer			dil = _NhlGetLayer((int)from->addr);
	NhlErrorTypes		ret=NOERROR;

	if(nargs != 0){
		NhlPError(FATAL,E_UNKNOWN,"AddData:called incorrectly???");
		return FATAL;
	}

	if((dil == NULL) || !(_NhlIsDataItem(dil) || _NhlIsDataSpec(dil))){
		NhlPError(FATAL,E_UNKNOWN,"AddData:invalid Data Object %d",
							(int)from->addr);
		return FATAL;
	}

	gen = *(NhlGenArray*)(to->addr);

	if((gen == NULL) || (gen->data == NULL)){
		int	*tintptr;
		/*
		 * if first element, then make a 1 dim array that will be
		 * compiled by the DataCommSetValues function.
		 */
		newgen = NhlConvertMalloc(sizeof(NhlGenArrayRec));
		tintptr = NhlConvertMalloc(sizeof(int));
		if((newgen == NULL) || (tintptr == NULL))
			return FATAL;
		newgen->num_dimensions = 1;
		newgen->len_dimensions = &newgen->num_elements;
		newgen->num_elements = 1;
		newgen->typeQ = NrmStringToQuark(NhlTInteger);
		newgen->size = sizeof(int);
		*tintptr = (int)from->addr;
		newgen->data = tintptr;
		newgen->my_data = False; /* doesn't matter - will be free'd from
					convert context. */
	}
	else{
		dlistold = gen->data;

		/*
		 * don't allow a duplicate id.
		 */
		if(dlistold != NULL){
			for(i=0;i < dlistold->num_items;i++){
				if(dil->base.id == dlistold->list[i]->id){
					NhlPError(FATAL,E_UNKNOWN,
	"AddData:Attempting to add Duplicate Data \"%s\" to \"%s\" resource",
							NhlName(dil->base.id),
				NrmQuarkToString(dlistold->oset->res_name));
					return FATAL;
				}
			}
		}

		dnode = CreateDataNode(dlistold->dcl,dlistold->oset,dil);
		if(dnode == NULL){
			NhlPError(FATAL,E_UNKNOWN,
			"AddData:Unable to add DataItem %s to datalist \"%s\"",
				NhlName(dil->base.id),
				NrmNameToString(dlistold->oset->res_name));
			return FATAL;
		}

		newgen = _NhlCreateGenArray(NULL,_NhlTDListCompiled,0,-1111,
								NULL,False);
		dlistnew = (_NhlInternDataList)NhlMalloc(
						sizeof(_NhlInternDataListRec));
		if((newgen == NULL) || (dlistnew == NULL)){
			NhlPError(FATAL,ENOMEM,NULL);
			FreeDataNode(dnode);
			return FATAL;
		}
		newgen->data = dlistnew;

		if(dlistold == NULL){	/* This is the first in the list */
			dlistnew->num_items = 1;
			dlistnew->shared_list = False;
		}
		else{
			memcpy((char*)dlistnew,(char*)dlistold,
						sizeof(_NhlInternDataListRec));
			dlistnew->num_items++;
			dlistnew->shared_list = True;
			dlistold->shared_list = True;
		}

		dlistnew->list = NhlMalloc(sizeof(_NhlDataNodeRec)*
							dlistnew->num_items);
		if(dlistnew->list == NULL){
			NhlPError(FATAL,ENOMEM,NULL);
			(void)FreeDataNode(dnode);
			(void)NhlFree(dlistnew->list);
			(void)NhlFree(dlistnew);
			return FATAL;
		}

		for(i=0;i < dlistnew->num_items - 1;i++){
			dlistnew->list[i] = dlistold->list[i];
		}
		dlistnew->list[dlistnew->num_items - 1] = dnode;
		dlistnew->extra = dnode;
	}

	*(NhlGenArray *)(to->addr) = newgen;

	return ret;
}

/*
 * Function:	RemoveData
 *
 * Description:	This function removes data item's from the DataList resource
 *		in the given Layer.
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:
 */
/*ARGSUSED*/
static NhlErrorTypes
RemoveData
#if	__STDC__
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	_NhlDataNodePtr		dnode=NULL;
	int			i;
	NhlGenArray		gen=NULL,newgen=NULL;
	_NhlInternDataList	dlistold=NULL,dlistnew=NULL;
	Layer			dil = _NhlGetLayer((int)from->addr);

	if(nargs != 0){
		NhlPError(FATAL,E_UNKNOWN,"RemoveData:called incorrectly???");
		return FATAL;
	}

	if((dil == NULL) || !(_NhlIsDataItem(dil) || _NhlIsDataSpec(dil))){
		NHLPERROR((FATAL,E_UNKNOWN,"RemoveData:invalid Data Object %d",
							(int)from->addr));
		return FATAL;
	}

	gen = *(NhlGenArray*)(to->addr);
	if((gen == NULL) || (gen->data == NULL)){
		NhlPError(FATAL,E_UNKNOWN,
			"RemoveData:Data Object \"%s\" isn't in DataList",
								_NhlName(dil));
		return FATAL;
	}

	dlistold = gen->data;

	for(i=0;i < dlistold->num_items;i++){
		if(dlistold->list[i]->id == dil->base.id){
			dnode = dlistold->list[i];
			break;
		}
	}
	if(dnode == NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"RemoveData:Data Object \"%s\" isn't in DataList",
								_NhlName(dil));
		return FATAL;
	}

	if(dlistold->num_items == 1){
		newgen = NULL;
	}
	else if(dlistold->num_items > 1){
		newgen = _NhlCreateGenArray(NULL,_NhlTDListCompiled,0,-1111,
								NULL,False);
		dlistnew = (_NhlInternDataList)NhlMalloc(
						sizeof(_NhlInternDataListRec));
		if((newgen == NULL) || (dlistnew == NULL)){
			NhlPError(FATAL,ENOMEM,NULL);
			return FATAL;
		}
		newgen->data = dlistnew;
		memcpy((char*)dlistnew,(char*)dlistold,
						sizeof(_NhlInternDataListRec));
		dlistnew->num_items--;
		dlistnew->shared_list = True;
		dlistold->shared_list = True;

		dlistnew->list = NhlMalloc(dlistnew->num_items *
						sizeof(_NhlDataNodeRec));
		for(i=0;i < dlistnew->num_items;i++){
			int found = 0;
			if(dlistold->list[i + found] == dnode)
				found++;
			dlistnew->list[i] = dlistold->list[i + found];
		}

	}
	else{
		NhlPError(FATAL,E_UNKNOWN,
			"RemoveData:Shouldn't get this Error - Bad Joke...");
		return FATAL;
	}

	dlistold->extra = dnode;

	*(NhlGenArray*)(to->addr) = newgen;

	return NOERROR;
}


/************************************************************************
*									*
*	Methode definitions						*
*									*
************************************************************************/

/*
 * Function:	DataCommClassPartInitialize
 *
 * Description:	This function initializes the data_offsets field in the
 *		DataCommLayerClassPart.  This is used to keep track of
 *		all the data resources so they can be set via the NhlAddData
 *		and NhlRemoveData functions.
 *
 * In Args:	
 *		LayerClass	lc	pointer to class structure to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataCommClassPartInitialize
#if	__STDC__
(
	LayerClass	lc	/* pointer to class structure to update */
)
#else
(lc)
	LayerClass	lc;	/* pointer to class structure to update */
#endif
{
	DataCommLayerClass	dc = (DataCommLayerClass)lc;

	/*
	 * This field can only be set by _NhlRegisterDataRes calls in
	 * sub-classes ClassPartInit functions.
	 */
	dc->datacomm_class.data_offsets = NULL;

	return NOERROR;
}

static NrmQuark	QListCompiled = NrmNULLQUARK;

/*
 * Function:	DataCommClassInitialize
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataCommClassInitialize
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NOERROR;

	QListCompiled = NrmStringToQuark(_NhlTDListCompiled);

	ret = NhlRegisterConverter(_NhlTAddData,_NhlTDataList,AddData,NULL,0,
								False,NULL);
	ret = NhlRegisterConverter(_NhlTRemoveData,_NhlTDataList,RemoveData,
							NULL,0,False,NULL);

	return ret;
}

/*
 * Function:	CreateDataNode
 *
 * Description:	This fuction is used to create the DataNode that contains
 *		all the information needed for the DataComm instance to
 *		communicate with the DataItem for the specified resource.
 *
 * In Args:	
 *		DataCommLayer	dcl,	DataComm subclass using data
 *		_NhlDataOffset	oset,	oset ptr - Data Description
 *		Layer		dl,	Data to connect to
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	_NhlDataNodePtr
 * Side Effect:	
 */
static _NhlDataNodePtr
CreateDataNode
#if	__STDC__
(
	DataCommLayer	dcl,	/* DataComm subclass using data		*/
	_NhlDataOffset	oset,	/* oset ptr - Data Description		*/
	Layer		dl	/* Data to connect to			*/
)
#else
(dcl,oset,dl)
	DataCommLayer	dcl;	/* DataComm subclass using data		*/
	_NhlDataOffset	oset;	/* oset ptr - Data Description		*/
	Layer		dl;	/* DataItem to connect to		*/
#endif
{
	_NhlDataNodePtr	dnode = NULL;
	int		dspecid;
	Layer		dil = NULL;
	DataSpecLayer	dsl = NULL;
	NhlErrorTypes	ret=NOERROR;

	if(_NhlIsDataItem(dl)){
		dil = dl;
	}
	else if(dl->base.layer_class != oset->dataspec_class){
		NhlPError(FATAL,E_UNKNOWN,
"The \"%s\" resource must be set with a \"%s\" DataSpec object, not a \"%s\" object",
					NrmNameToString(oset->res_name),
					_NhlClassName(oset->dataspec_class),
					_NhlClassName(dl->base.layer_class));
		return NULL;
	}
	else{
		dsl = (DataSpecLayer)dl;
		dil = _NhlGetLayer(dsl->dataspec.data_item);
		if(dil == NULL){
			NhlPError(FATAL,E_UNKNOWN,
			"The given DataSpec class does not have any data");
			return NULL;
		}
	}

	dnode = NhlMalloc(sizeof(_NhlDataNodeRec));
	if(dnode == NULL){
		NhlPError(FATAL,ENOMEM,NULL);
		return NULL;
	}

	dnode->dhandle = _NhlInitDataConnection((DataItemLayer)dil,dcl->base.id,
				oset->res_name,oset->qlist,&dnode->type);
	if(dnode->dhandle == NULL){
		NhlPError(FATAL,E_UNKNOWN,
				"Unable to init a connection with Data \"%s\"",
							NhlName(dl->base.id));
		(void)NhlFree(dnode);
		return NULL;
	}

	dnode->id = dl->base.id;
	dnode->item = dil->base.id;

	if(dsl == NULL){
		ret = NhlCreate(&dspecid,dil->base.name,oset->dataspec_class,
							dcl->base.id,NULL);
		dnode->dataspec = (DataSpecLayer)_NhlGetLayer(dspecid);
		if((ret < WARNING) || (dnode->dataspec == NULL)){
			NhlPError(FATAL,E_UNKNOWN,
				"Unable to process data-dependent resources");
			_NhlCloseDataConnection((DataItemLayer)dil,
								dnode->dhandle);
			(void)NhlFree(dnode);
			return NULL;
		}
	}
	else{
		_NhlDCommList	dclist = (_NhlDCommList)NhlMalloc(
						sizeof(_NhlDCommListRec));

		if(dclist == NULL){
			NhlPError(FATAL,ENOMEM,NULL);
			_NhlCloseDataConnection((DataItemLayer)dil,
								dnode->dhandle);
			(void)NhlFree(dnode);
			return NULL;
		}
		dclist->dcommid = dcl->base.id;
		dclist->res_name = oset->res_name;
		dclist->next = dsl->dataspec.dcomm_list;
		dsl->dataspec.dcomm_list = dclist;
		dnode->dataspec = dsl;
	}

	return	dnode;
}

/*
 * Function:	CompileDataList
 *
 * Description:	This function takes an Array of the public DataList and
 *		compiles it into an internal format.
 *
 * In Args:	
 *		DataCommLayer	dcl,	DataComm object
 *		_NhlDataOffset	oset	offset record
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CompileDataList
#if	__STDC__
(
	DataCommLayer	dcl,	/* DataComm object	*/
	_NhlDataOffset	oset	/* offset record	*/
)
#else
(dcl,oset)
	DataCommLayer	dcl;	/* DataComm object	*/
	_NhlDataOffset	oset;	/* offset record	*/
#endif
{
	NhlGenArray		gen=NULL;
	_NhlInternDataList	ilist;
	_NhlDataNodePtr		*dnodes;
	int			*llist;
	int			i,j,len;
	Layer			dil=NULL;
	static NrmQuark		Qint=NrmNULLQUARK;

	if(Qint == NrmNULLQUARK)
		Qint = NrmStringToQuark(NhlTInteger);

	gen = *(NhlGenArray*)((char*)dcl + oset->offset);

	if((gen == NULL) || (gen->typeQ == QListCompiled))
		return NOERROR;

	if((gen->num_dimensions != 1) || (gen->size != sizeof(int)) ||
		(gen->len_dimensions[0] < 1) || (gen->typeQ != Qint)){
		NhlPError(WARNING,E_UNKNOWN,
			"Unable to set Data resource \"%s\":Invalid Array",
					NrmQuarkToString(oset->res_name));
		return WARNING;
	}

	/*
	 * Remove any duplicate id's
	 */
	len = gen->len_dimensions[0];
	llist = (int*)gen->data;
	i = 0;
	while(i < len){
		j = i + 1;
		while(j < len){
			if(llist[i] == llist[j]){
				NhlPError(WARNING,E_UNKNOWN,
			"Ignoring duplicate data \"%s\" in resource \"%s\"",
							NhlName(llist[j]),
					NrmQuarkToString(oset->res_name));

				llist[j] = llist[--len];
			}
			else
				j++;
		}
		i++;
	}

	/* -1111 is a hack to get an empty GenArray */
	gen = _NhlCreateGenArray(NULL,_NhlTDListCompiled,0,-1111,NULL,False);
	ilist = NhlMalloc(sizeof(_NhlInternDataListRec));
	dnodes = NhlMalloc(sizeof(_NhlDataNodePtr) * len);
	if((ilist == NULL) || (dnodes == NULL) || (gen == NULL)){
		NhlPError(FATAL,ENOMEM,NULL);
		(void)NhlFree(ilist);
		(void)NhlFree(dnodes);
		return FATAL;
	}

	gen->data = ilist;

	j = 0;
	ilist->list = dnodes;
	for(i=0;i < len;i++){
		dil = _NhlGetLayer(llist[i]);
		if((dil == NULL)|| !(_NhlIsDataItem(dil)||_NhlIsDataSpec(dil))){
			NhlPError(WARNING,E_UNKNOWN,
			"Unable to add DataItem \"%s\" to DataList \"%s\"",
			NhlName(llist[i]),NrmQuarkToString(oset->res_name));
			j--;
		}
		else{
			*dnodes = CreateDataNode(dcl,oset,dil);
			if(*dnodes != NULL){
				dnodes++;
			}
			else{
				NhlPError(WARNING,E_UNKNOWN,
			"Unable to add DataItem \"%s\" to DataList \"%s\"",
			NhlName(llist[i]),NrmQuarkToString(oset->res_name));
				j--;
			}
		}
	}

	ilist->num_items = len + j;
	ilist->extra = NULL;
	ilist->shared_list = False;
	ilist->dcl = dcl;
	ilist->oset = oset;

	*(NhlGenArray*)((char*)dcl + oset->offset) = gen;

	return NOERROR;
}

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
	NhlErrorTypes		ret = NOERROR;
	NhlErrorTypes		lret = NOERROR;
	DataCommLayerClass	cc = (DataCommLayerClass)lc;
	_NhlDataOffset		oset = cc->datacomm_class.data_offsets;
	
	while(oset != NULL){

		lret = CompileDataList((DataCommLayer)new,oset);
		ret = MIN(ret,lret);

		oset = oset->next;
	}

	((DataCommLayer)new)->datacomm.data_changed = False;

	return ret;
}

/*
 * Function:	DataCommSetValues
 *
 * Description:	This is the setvalues method def for the DataComm class.
 *		It takes any data resources the user sets with a GenArray
 *		and converts it into an _NhlInternDataList so the sub-classes
 *		can use it.
 *
 * In Args:	
 *		Layer		old,	old
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
DataCommSetValues
#if	__STDC__
(
	Layer		old,	/* old		*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(old,req,new,args,nargs)
	Layer		old;	/* old		*/
	Layer		req;	/* requested	*/
	Layer		new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	DataCommLayerClass	cc = (DataCommLayerClass)new->base.layer_class;
	_NhlDataOffset		oset = cc->datacomm_class.data_offsets;
	NhlErrorTypes		lret=NOERROR,ret=NOERROR;

	while(oset != NULL){

		if(*(NhlPointer*)((char*)new + oset->offset) !=
				*(NhlPointer*)((char*)old + oset->offset)){
			lret = CompileDataList((DataCommLayer)new,oset);
			/*
			 * If there is an error compiling the DataList then
			 * reset the NewData list to the OldData list.
			 */
			if(lret != NOERROR){
				NhlPError(WARNING,E_UNKNOWN,
			"DataCommSetValues:Unable to set \"%s\" resource",
					NrmQuarkToString(oset->res_name));
				/*
				 * Boy - this is pretty.
				 */
				*(NhlPointer*)((char*)new + oset->offset) =
				*(NhlPointer*)((char*)old + oset->offset);
			
				ret = MIN(ret,WARNING);
			}
		}

		oset = oset->next;
	}

	return ret;
}

/*
 * Function:	FreeInternDataList
 *
 * Description:	This function is used to free a all the Memory associated
 *		with a DataResource including the gen array and the
 *		_NhlInternDataList it points at.
 *
 * In Args:	
 *		_NhlInternDataList	ilist	list to free
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
FreeInternDataList
#if	__STDC__
(
	NhlGenArray		gen	/* list to free	*/
)
#else
(gen)
	NhlGenArray		gen;	/* list to free	*/
#endif
{
	int			i;
	_NhlInternDataList	ilist = NULL;

	if((gen == NULL) || (gen->typeQ != QListCompiled))
		return;

	ilist = gen->data;
	NhlFreeGenArray(gen);

	if(!ilist->shared_list){
		for(i=0;i < ilist->num_items;i++)
			FreeDataNode(ilist->list[i]);
	}
	else{
		FreeDataNode(ilist->extra);
	}

	(void)NhlFree(ilist->list);
	(void)NhlFree(ilist);

	return;
}

/*
 * Function:	DataCommSetValuesHook
 *
 * Description:	This function is used by setvalues to free the datalists
 *		that are not being kept.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
DataCommSetValuesHook
#if	__STDC__
(
	Layer		old,	/* old		*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(old,req,new,args,nargs)
	Layer		old;	/* old		*/
	Layer		req;	/* requested	*/
	Layer		new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	DataCommLayerClass	dc = (DataCommLayerClass)new->base.layer_class;
	_NhlDataOffset		oset = dc->datacomm_class.data_offsets;
	NhlGenArray		*ol, *nl, *rl;
	
	while(oset != NULL){

		ol = (NhlGenArray*)((char *)old + oset->offset);
		nl = (NhlGenArray*)((char *)new + oset->offset);
		rl = (NhlGenArray*)((char *)req + oset->offset);

		if(*ol != *rl)	/* if ol == rl then resource not involved */
			if(*nl == *ol)
				FreeInternDataList(*rl);
			else
				FreeInternDataList(*ol);

		oset = oset->next;
	}

	return NOERROR;
}

/*
 * Function:	FreeDataNode
 *
 * Description:	This fuction is used to free a DataNode.
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
FreeDataNode
#if	__STDC__
(
	_NhlDataNodePtr	node	/* node to free	*/
)
#else
(node)
	_NhlDataNodePtr	node;	/* node to free	*/
#endif
{
	DataItemLayer	item;

	if(node == NULL)
		return;

	item = (DataItemLayer)_NhlGetLayer(node->item);

	if((item == NULL) || (!_NhlIsDataItem(item))){
		NHLPERROR((WARNING,E_UNKNOWN,
			"FreeDataNode:Data is corrupted - bad item value"));
	}
	else{
		/*
		 * Close Data Connection in Mgr
		 */
		_NhlCloseDataConnection(item,node->dhandle);
	}

	/*
	 * if dataspec is internal, then destroy it.
	 */
	if(node->id != node->dataspec->base.id)
		(void)NhlDestroy(node->dataspec->base.id);
	(void)NhlFree(node);

	return;
}

/*
 * Function:	CallUpdateData
 *
 * Description:	This function is used to call the update_data method of the
 *		datacomm class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallUpdateData
#if	__STDC__
(
	DataCommLayerClass	lc,		/* class pointer	*/
	DataCommLayer		l,		/* instance pointer	*/
	DataCommLayer		old		/* instance pointer	*/
)
#else
(lc,l,old)
	DataCommLayerClass	lc;		/* class pointer	*/
	DataCommLayer		l;		/* instance pointer	*/
	DataCommLayer		old;		/* instance pointer	*/
#endif
{
	NhlErrorTypes	ret = NOERROR, lret = NOERROR;
	LayerClass	newlc = lc->base_class.superclass;

	if((newlc != NULL) &&
		(newlc->base_class.class_inited & DataCommLayerClassFlag)){
		lret = CallUpdateData((DataCommLayerClass)newlc,l,old);
	}

	if(lret < WARNING)
		return lret;

	if(lc->datacomm_class.update_data != NULL)
		ret = (*(lc->datacomm_class.update_data))(l,old);

	return MIN(lret,ret);
}

/*
 * Function:	UpdateData
 *
 * Description:	This function is the call for the UpdateData method of the
 *		datacomm class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
UpdateData
#if	__STDC__
(
	DataCommLayer	dcomm
)
#else
(dcomm)
	DataCommLayer	dcomm;
#endif
{
	DataCommLayer		oldl;
	DataCommLayerClass	lc =(DataCommLayerClass)_NhlClass((Layer)dcomm);
	NhlErrorTypes		ret = NOERROR;

	if(!dcomm->datacomm.data_changed)
		return NOERROR;

	oldl = NhlMalloc(lc->base_class.layer_size);
	if(oldl == NULL){
		NhlPError(FATAL,ENOMEM,NULL);
		return FATAL;
	}
	
	memcpy((char*)oldl,(char*)dcomm,lc->base_class.layer_size);

	ret = CallUpdateData(lc,dcomm,oldl);

	(void)NhlFree(oldl);

	dcomm->datacomm.data_changed = False;

	return ret;
}

/*
 * Function:	DataCommDraw
 *
 * Description:	This function makes sure that the datacomm's data resources
 *		are uptodate so it is in a state that allows it to draw.
 *		data.  It is only really usefull if the delay_compute resource
 *		is True.  If it is False, then the datacomm class should
 *		have already recomputed when it was notified by the DataMgr.
 *
 * In Args:	
 *		int	dcommid		id of a datacomm object
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataCommDraw
#if	__STDC__
(
	Layer	l	/* id of a datacomm object	*/
)
#else
(l)
	Layer	l;	/* id of a datacomm object	*/
#endif
{
	return UpdateData((DataCommLayer)l);
}

/*
 * Function:	DataCommDestroy
 *
 * Description:	This fuction is used to free all the memory allocated on
 *		behalf of the DataComm layer given. It closes all data
 *		connections.
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
	DataCommLayerClass	cc = (DataCommLayerClass)l->base.layer_class;
	_NhlDataOffset		oset = cc->datacomm_class.data_offsets;
	NhlPointer		field;

	while(oset != NULL){
		field = (NhlPointer)((char *)l + oset->offset);

		FreeInternDataList(*(NhlGenArray*)field);

		oset = oset->next;
	}

	return NOERROR;
}

/*
 * Method definitions for DataSpec class
 */

/*
 * Function:	DataSpecInitialize
 *
 * Description:	init for dataspec
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
DataSpecInitialize
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
	DataSpecLayer	dsnew = (DataSpecLayer)new;
	Layer		dil;

	dsnew->dataspec.dcomm_list = NULL;

	if(dsnew->dataspec.data_item != NULL_LAYER){
		dil = _NhlGetLayer(dsnew->dataspec.data_item);

		if((dil == NULL) || !_NhlIsDataItem(dil) ||
			!_NhlRegisterDSpec((DataItemLayer)dil,dsnew->base.id)){
			NhlPError(WARNING,E_UNKNOWN,
				"DataSpecInit:%s resource not valid",
								NhlNdsDataItem);
			dsnew->dataspec.data_item = NULL_LAYER;
			return WARNING;
		}
	}

	return NOERROR;
}

/*
 * Function:	ReleaseDataComms
 *
 * Description:	This function recursively removes the dcomm_list from the
 *		dspec object.  It is used to remove the dspec from the given
 *		dcomm resource and free the memory allocated by the list
 *		itself.
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
ReleaseDataComms
#if	__STDC__
(
	int		dspecid,
	_NhlDCommList	dc_list
)
#else
(dspecid,dc_list)
	int		dspecid;
	_NhlDCommList	dc_list;
#endif
{
	if(dc_list == NULL)
		return;

	ReleaseDataComms(dspecid,dc_list->next);

	NhlRemoveData(dc_list->dcommid,NrmNameToString(dc_list->res_name),
								dspecid);

	(void)NhlFree(dc_list);

	return;
}

/*
 * Function:	DataSpecSetValues
 *
 * Description:	setvalues method
 *
 * In Args:	
 *		Layer		old,	old
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
DataSpecSetValues
#if	__STDC__
(
	Layer		old,	/* old		*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(old,req,new,args,nargs)
	Layer		old;	/* old		*/
	Layer		req;	/* requested	*/
	Layer		new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	DataSpecLayer	dsnew = (DataSpecLayer)new;
	DataSpecLayer	dsold = (DataSpecLayer)old;
	Layer		dil;

	if(dsnew->dataspec.data_item == dsold->dataspec.data_item){
		return NOERROR;
	}

	dil = _NhlGetLayer(dsold->dataspec.data_item);
	if((dil != NULL) && _NhlIsDataItem(dil))
		_NhlUnRegisterDSpec((DataItemLayer)dil,dsold->base.id);

	ReleaseDataComms(dsnew->base.id,dsnew->dataspec.dcomm_list);
	dsnew->dataspec.dcomm_list = NULL;

	if(dsnew->dataspec.data_item == NULL_LAYER)
		return NOERROR;

	dil = _NhlGetLayer(dsnew->dataspec.data_item);
	if((dil != NULL) && _NhlIsDataItem(dil))
		if(_NhlRegisterDSpec((DataItemLayer)dil,dsnew->base.id))
			return NOERROR;

	NhlPError(WARNING,E_UNKNOWN,"DataSpecSetValues:Invalid %s resource",
								NhlNdsDataItem);
	dsnew->dataspec.data_item = NULL_LAYER;

	return WARNING;
}

/*
 * Function:	DataSpecDestroy
 *
 * Description:	destroy method
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataSpecDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
{
	DataSpecLayer	dsl = (DataSpecLayer)l;
	Layer		dil;

	dil = _NhlGetLayer(dsl->dataspec.data_item);
	if((dil != NULL) && _NhlIsDataItem(dil))
		_NhlUnRegisterDSpec((DataItemLayer)dil,dsl->base.id);

	ReleaseDataComms(dsl->base.id,dsl->dataspec.dcomm_list);

	return NOERROR;
}

/************************************************************************
*									*
*	Private functions for use by internal HLU library		*
*									*
************************************************************************/


/************************************************************************
*									*
*	Private API for sub-classes to add additional data.		*
*									*
************************************************************************/

/*
 * Function:	_NhlRegisterDataRes
 *
 * Description:	This function is used to add this data resources to the list
 *		of data resources managed by the DataComm superclass.  This
 *		must be done to use the NhlAddData function to add the data.
 *		If a sub-class calls this function for a data resource in
 *		it's super-class the sub-class will effectively over-ride
 *		the call the super-class made.
 *		
 * In Args:	
 *		DataCommLayerClass	dc,		DataComm sub-class
 *		NhlString		res_name,	name of data resource
 *		LayerClass		dataspec,	DataSpecific object
 *		NhlString		...		types requested
 *					NULL		end of list
 *
 * Out Args:	
 *
 * Scope:	DataComm sub-classes - in thier ClassPartInit function.
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlRegisterDataRes
#if	NeedVarArgProto
(
	DataCommLayerClass	dc,		/* DataComm sub-class	*/
	NrmString		res_name,	/* name of data res	*/
	LayerClass		dataspec,	/* DataSpecific object	*/
	...					/* types requested	*/
)
#else
(dc,res_name,dataspec,va_alist)
	DataCommLayerClass	dc;		/* DataComm sub-class	*/
	NrmString		res_name;	/* name of data res	*/
	LayerClass		dataspec;	/* DataSpecific object	*/
	va_dcl					/* types requested	*/
#endif
{
	static NrmQuark	QDataList=NrmNULLQUARK;
	va_list		ap;
	int		i,num_types = 0;
	LayerClass	tmp;
	NrmQuark	Qres_name;
	NrmQuarkList	qlist=NULL;
	unsigned int	offset = 0;
	_NhlDataOffset	oset=NULL;
	NhlBoolean	found = False;
	NhlBoolean	old = False;
	NrmResourceList	nrmres = (NrmResourceList)dc->base_class.resources;
	NhlErrorTypes	ret = NOERROR;

	if(QDataList == NrmNULLQUARK){
		QDataList= NrmStringToQuark(_NhlTDataList);
	}

	/*
	 * Make sure res_name is a resource in the class - then find out
	 * what the offset is for that resource.
	 */
	Qres_name = NrmStringToQuark(res_name);
	for(i=0;i < dc->base_class.num_resources; i++){
		if((nrmres[i].nrm_name == Qres_name) &&
					(nrmres[i].nrm_type == QDataList)){
			offset = nrmres[i].nrm_offset;
			found = True;
			break;
		}
	}
	if(!found){
		NhlPError(FATAL,E_UNKNOWN,
		"Trying to register a non-existant DataList resource %s",
								res_name);
		return FATAL;
	}

	/*
	 * If the sub-class is re-defining a data resource then over-write
	 * the current offset record, otherwise create a new one.
	 */
	oset = dc->datacomm_class.data_offsets;
	while(oset != NULL){
		if(oset->res_name == Qres_name){
			old = True;
			break;
		}
		oset = oset->next;
	}
	if(oset == NULL){
		oset = (_NhlDataOffset)NhlMalloc(sizeof(_NhlDataOffsetRec));
		if(oset == NULL){
			NhlPError(FATAL,ENOMEM,NULL);
			return FATAL;
		}
	}

	VA_START(ap,dataspec);
	for(tmp = (LayerClass)(va_arg(ap,LayerClass));tmp != NULL;
				tmp = (LayerClass)(va_arg(ap,LayerClass)))
		num_types++;
	va_end(ap);

	if(!num_types){
		NhlPError(FATAL,E_UNKNOWN,
			"Unable to register DataList resource %s",res_name);
		if(!old)
			(void)NhlFree(oset);
		return FATAL;
	}

	qlist = (NrmQuarkList)NhlMalloc(sizeof(NrmQuark)*(num_types + 1));
	if(qlist == NULL){
		NhlPError(FATAL,ENOMEM,NULL);
		if(!old)
			(void)NhlFree(oset);
		return FATAL;
	}
	if(old)
		(void)NhlFree(oset->qlist);
	oset->qlist = qlist;

	VA_START(ap,dataspec);
	for(i=0;i < num_types;i++){
		tmp = (LayerClass)va_arg(ap,LayerClass);
		ret = _NhlInitializeLayerClass(tmp);
		if(ret < WARNING)
			return ret;
		oset->qlist[i] = tmp->base_class.nrm_class;
	}
	va_end(ap);

	oset->qlist[num_types] = NrmNULLQUARK;
	oset->res_name = Qres_name;
	oset->offset = offset;
	oset->dataspec_class = dataspec;
	if(!old){
		oset->next = dc->datacomm_class.data_offsets;
		dc->datacomm_class.data_offsets = oset;
	}

	return NOERROR;
}

/*
 * Function:	_NhlGetDataInfo
 *
 * Description:	Returns the number of DataItem's in the data list. Along with
 *		an array of that length.  With each element of the array
 *		being an _NhlDataNodePtr.
 *
 * In Args:	
 *		_NhlInternDataList	datares,	pointer to datalist
 *
 * Out Args:	
 *		_NhlDataNodePtr		*dinfo		data info RET
 *
 * Scope:	private to sub-classes
 * Returns:	int - number of items in dinfo
 * Side Effect:	
 */
/*ARGSUSED*/
int
_NhlGetDataInfo
#if	__STDC__
(
	NhlGenArray		gen,		/* pointer to datalist	*/
	_NhlDataNodePtr		**dinfo		/* data info RET	*/
)
#else
(gen,dinfo)
	NhlGenArray		gen;		/* pointer to datalist	*/
	_NhlDataNodePtr		**dinfo;	/* data info RET	*/
#endif
{
	_NhlInternDataList	dlist;

	if((gen == NULL) || (gen->data == NULL)){
		*dinfo = NULL;
		return 0;
	}

	dlist = gen->data;
	*dinfo = dlist->list;

	return dlist->num_items;
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
 *		_NhlDataNodePtr		dnode,		datanode
 *
 * Out Args:	
 *		NhlBoolean	*new_ret,	is data new?
 *
 * Scope:	DataComm sub-classes
 * Returns:	Layer - NULL==Failed
 * Side Effect:	
 */
Layer
_NhlGetDataSet
#if	__STDC__
(
	_NhlDataNodePtr		dnode,		/* datanode	*/
	NhlBoolean		*new_ret	/* is data new?	*/
)
#else
(dnode,new_ret)
	_NhlDataNodePtr		dnode;		/* datanode	*/
	NhlBoolean		*new_ret;	/* is data new?	*/
#endif
{
	DataItemLayer		item = NULL;

	/*
	 * Make sure the DataItem still exists.
	 */
	item = (DataItemLayer)_NhlGetLayer(dnode->item);
	if((item == NULL) || (!_NhlIsDataItem(item))){
		NhlPError(FATAL,E_UNKNOWN,
				"_NhlGetDataSet: Data Item no-longer exists.");
		return NULL;
	}

	/*
	 * Retrieve actual dataset from DateMgr
	 */
	 return _NhlRetrieveData(item,dnode->dhandle,new_ret);
}

/************************************************************************
*									*
*	API for DataMgr to notify DataComm of data changes		*
*									*
************************************************************************/

/*
 * Function:	_NhlUpdateData
 *
 * Description:	This function is used to notify the datacomm class that a
 *		dataitem had a resource change that effected it's data
 *		representation.  If recompute_data is true, then this function
 *		should call the update_data method, otherwise it should just
 *		mark that the data is outofdate by setting the data_changed
 *		to true.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private to DataMgr class and DataComm class
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlUpdateData
#if	__STDC__
(
	int	dcommid		/* id of datacomm class	*/
)
#else
(dcommid)
	int	dcommid;	/* id of datacomm class	*/
#endif
{
	DataCommLayer	dcomm = (DataCommLayer)_NhlGetLayer(dcommid);

	if((dcomm == NULL) || !_NhlIsDataComm(dcomm)){
		NhlPError(FATAL,E_UNKNOWN,
			"_NhlDataUpdate:called for a non-DataComm class");
		return FATAL;
	}

	dcomm->datacomm.data_changed = True;

	if(!dcomm->datacomm.delay_compute)
		return UpdateData(dcomm);

	return NOERROR;
}

/************************************************************************
*									*
*	Public API for adding data					*
*									*
************************************************************************/

/*
 * Function:	NhlAddData
 *
 * Description:	This function is a convienience function for adding a single
 *		data item to a data resource.
 *		**NOTE: This function is currently calling _NhlSetValues.  It
 *		should eventually use another SetValues interface so
 *		_NhlSetValues can be a static function again.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlAddData
#if	__STDC__
(
	int		dcommid,	/* id of layer w/ data resource	*/
	NhlString	res_name,	/* name of data resource	*/
	int		ditemid		/* id of data to add		*/
)
#else
(dcommid,res_name,ditemid)
	int		dcommid;	/* id of layer w/ data resource	*/
	NhlString	res_name;	/* name of data resource	*/
	int		ditemid;	/* id of data to add		*/
#endif
{
	_NhlExtArg	larg;
	Layer		dcl = _NhlGetLayer(dcommid);

	if((dcl == NULL) || !_NhlIsDataComm(dcl)){
		NhlPError(FATAL,E_UNKNOWN,"NhlAddData:Invalid DataComm id %d",
								dcommid);
		return FATAL;
	}

	larg.quark = NrmStringToQuark(res_name);
	larg.value = ditemid;
	larg.type = NrmStringToQuark(_NhlTAddData);

	return _NhlSetValues(dcl,&larg,1);
}

/*
 * Function:	NhlRemoveData
 *
 * Description:	This function is a convienience function for removing a single
 *		data item from a data resource.
 *		**NOTE: This function is currently calling _NhlSetValues.  It
 *		should eventually use another SetValues interface so
 *		_NhlSetValues can be a static function again.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRemoveData
#if	__STDC__
(
	int		dcommid,	/* id of layer w/ data resource	*/
	NhlString	res_name,	/* name of data resource	*/
	int		ditemid		/* id of data to add		*/
)
#else
(dcommid,res_name,ditemid)
	int		dcommid;	/* id of layer w/ data resource	*/
	NhlString	res_name;	/* name of data resource	*/
	int		ditemid;	/* id of data to add		*/
#endif
{
	_NhlExtArg	larg;
	Layer		dcl = _NhlGetLayer(dcommid);

	if((dcl == NULL) || !(_NhlIsDataComm(dcl) || _NhlIsDataSpec(dcl))){
		NhlPError(FATAL,E_UNKNOWN,
				"NhlRemoveData:Invalid DataComm id %d",dcommid);
		return FATAL;
	}

	larg.quark = NrmStringToQuark(res_name);
	larg.value = ditemid;
	larg.type = NrmStringToQuark(_NhlTRemoveData);

	return _NhlSetValues(dcl,&larg,1);
}

/*
 * Function:	NhlUpdateData
 *
 * Description:	This function is the public interface to tell a datacomm object
 *		that is should insure that it's resources are up-to-date. ie.
 *		the object is ready for drawing.  The user does not have to
 *		call this function since the Draw method of the DataComm
 *		class also insures this, but if the user wants finer control
 *		over when the update happens they can call this.  Also, this
 *		will not do anything unless NhlNdelayCompute is True since
 *		the data resources will already be uptodate if it is False - the
 *		dataMgr notifies the DataComm class of each change and the
 *		datacomm class updates immediately unless this resource is True.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlUpdateData
#if	__STDC__
(
	int	dcommid		/* id of dcomm object	*/
)
#else
(dcommid)
	int	dcommid;	/* id of dcomm object	*/
#endif
{
	DataCommLayer	dcomm = (DataCommLayer)_NhlGetLayer(dcommid);

	if((dcomm == NULL) || !_NhlIsDataComm(dcomm)){
		NhlPError(FATAL,E_UNKNOWN,"NhlUpdateData:Invalid Object id #%d",
								dcommid);
		return FATAL;
	}

	return UpdateData(dcomm);
}

/*
 * Function:	_NhlReleaseDMgr
 *
 * Description:	This function is used by the DataMgr to notify the dspec
 *		class that is using it, that it is being destroyed - so the
 *		spec object should set it's dataitem resource to NULL. And
 *		remove itself from all datalists that it is currently in.
 *
 * In Args:	
 *		int	dspecid,
 *		int	ditemid
 *
 * Out Args:	
 *
 * Scope:	global private(used only by DataMgr class)
 * Returns:	void
 * Side Effect:	
 */
void
_NhlReleaseDMgr
#if	__STDC__
(
	int	dspecid,
	int	ditemid
)
#else
(dspecid,ditemid)
	int	dspecid;
	int	ditemid;
#endif
{
	DataSpecLayer	dspec = (DataSpecLayer)_NhlGetLayer(dspecid);

	if((dspec == NULL) || !_NhlIsDataSpec(dspec)){
		NhlPError(FATAL,E_UNKNOWN,
				"_NhlReleaseDMgr: called with invalid dspecid");
		return;
	}

	if(ditemid != dspec->dataspec.data_item){
		NhlPError(FATAL,E_UNKNOWN,
				"_NhlReleaseDMgr:incorrect dataitem id");
		return;
	}

	dspec->dataspec.data_item = NULL_LAYER;

	/*
	 * Remove self from all datacomm resource lists.
	 */
	ReleaseDataComms(dspec->base.id,dspec->dataspec.dcomm_list);
	dspec->dataspec.dcomm_list = NULL;

	return;
}
