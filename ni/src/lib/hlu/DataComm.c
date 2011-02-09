/*
 *      $Id: DataComm.c,v 1.47.4.1 2008-03-28 20:37:34 grubin Exp $
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
#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/DataMgrF.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/ConvertersP.h>

/************************************************************************
*									*
*	DataComm Class declarations					*
*									*
************************************************************************/

#define	Oset(field)	NhlOffset(NhlDataCommLayerRec,datacomm.field)
static NhlResource dcresources[] = {

/* Begin-documented-resources */

	{NhlNdcDelayCompute,NhlCdcDelayCompute,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(delay_compute),NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL}

/* End-documented-resources */

};
#undef	Oset

static NhlErrorTypes DataCommClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc	/* pointer to class structure to update */
#endif
);

static NhlErrorTypes DataCommClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes DataCommInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataCommSetValues(
#if	NhlNeedProto
	NhlLayer	old,	/* old		*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataCommSetValuesHook(
#if	NhlNeedProto
	NhlLayer	old,	/* old		*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataCommGetValues(
#if	NhlNeedProto
	NhlLayer	l,	/* NhlLayer	*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataCommDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes DataCommDraw(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

NhlDataCommClassRec NhldataCommClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"dataCommClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlDataCommLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhltransformClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	dcresources,
/* num_resources		*/	NhlNumber(dcresources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	DataCommClassPartInitialize,
/* class_initialize		*/	DataCommClassInitialize,
/* layer_initialize		*/	DataCommInitialize,
/* layer_set_values		*/	DataCommSetValues,
/* layer_set_values_hook	*/	DataCommSetValuesHook,
/* layer_get_values		*/	DataCommGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	DataCommDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	DataCommDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	/* NhlViewClassPart */
	{
/* segment_wkid			*/	0,
/* get_bb			*/	NULL
	},
	/* NhlTransformClassPart */
	{
/* overlay_capability 		*/	_tfNotOverlayCapable,
/* data_to_ndc			*/	NhlInheritTransFunc,
/* ndc_to_data			*/	NhlInheritTransFunc,
/* data_polyline		*/	NhlInheritPolyTransFunc,
/* ndc_polyline			*/	NhlInheritPolyTransFunc,
/* data_polygon			*/	NhlInheritPolyTransFunc,
/* ndc_polygon			*/	NhlInheritPolyTransFunc,
/* data_polymarker		*/	NhlInheritPolyTransFunc,
/* ndc_polymarker		*/	NhlInheritPolyTransFunc
	},
	/* NhlDataCommClassPart */
	{
/* data_offsets			*/	NULL,
/* update_data			*/	NULL
	}
};
	
NhlClass NhldataCommClass = (NhlClass)&NhldataCommClassRec;

/*
 * DataSpec def's
 */
#define	Oset(field)	NhlOffset(NhlDataSpecLayerRec,dataspec.field)
static NhlResource dsresources[] = {

	/* initialize this field to false */
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(destroying),NhlTImmediate,_NhlUSET((NhlPointer)False),
         	_NhlRES_PRIVATE,NULL}

};
#undef	Oset

static NhlErrorTypes DataSpecDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

NhlDataSpecClassRec NhldataSpecClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"dataSpecClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlDataSpecLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlbaseClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	dsresources,
/* num_resources		*/	NhlNumber(dsresources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	DataSpecDestroy
	},
	/* NhlDataSpecClassPart */
	{
/* foo				*/	0
	}
};
	
NhlClass NhldataSpecClass = (NhlClass)&NhldataSpecClassRec;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

static _NhlDataNodePtr CreateDataNode(
#if	NhlNeedProto
	NhlDataCommLayer	dcl,	/* DataComm subclass using data	*/
	_NhlDataOffset		oset,	/* oset ptr - Data Description	*/
	NhlLayer		dl	/* Data to connect to		*/
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
 *		DataList resource in the given NhlLayer.
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
#if	NhlNeedProto
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
	NhlLayer		dil = _NhlGetLayer(from->data.intval);
	NhlErrorTypes		ret=NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"AddData:called incorrectly???");
		return NhlFATAL;
	}

	if((dil == NULL) || !(_NhlIsDataItem(dil))){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"AddData:invalid Data Object %d",
							from->data.intval);
		return NhlFATAL;
	}

	gen = *(NhlGenArray*)(to->data.ptrval);

	if((gen == NULL) || (gen->data == NULL)){
		int	*tintptr;
		/*
		 * if first element, then make a 1 dim array that will be
		 * compiled by the DataCommSetValues function.
		 */
		newgen = NhlConvertMalloc(sizeof(NhlGenArrayRec));
		tintptr = NhlConvertMalloc(sizeof(int));
		if((newgen == NULL) || (tintptr == NULL))
			return NhlFATAL;
		newgen->num_dimensions = 1;
		newgen->len_dimensions = &newgen->num_elements;
		newgen->num_elements = 1;
		newgen->typeQ = NrmStringToQuark(NhlTInteger);
		newgen->size = sizeof(int);
		*tintptr = from->data.intval;
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
				if(dil->base.id == dlistold->list[i]->item){
					NhlPError(NhlFATAL,NhlEUNKNOWN,
	"AddData:Attempting to add Duplicate Data \"%s\" to \"%s\" resource",
							NhlName(dil->base.id),
				NrmQuarkToString(dlistold->oset->res_name));
					return NhlFATAL;
				}
			}
		}

		dnode = CreateDataNode(dlistold->dcl,dlistold->oset,dil);
		if(dnode == NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"AddData:Unable to add DataItem %s to datalist \"%s\"",
				NhlName(dil->base.id),
				NrmNameToString(dlistold->oset->res_name));
			return NhlFATAL;
		}

		newgen = _NhlCreateGenArray(NULL,_NhlTDListCompiled,0,-1111,
								NULL,False);
		dlistnew = (_NhlInternDataList)NhlMalloc(
						sizeof(_NhlInternDataListRec));
		if((newgen == NULL) || (dlistnew == NULL)){
			NhlPError(NhlFATAL,ENOMEM,NULL);
			FreeDataNode(dnode);
			return NhlFATAL;
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
			NhlPError(NhlFATAL,ENOMEM,NULL);
			(void)FreeDataNode(dnode);
			(void)NhlFree(dlistnew->list);
			(void)NhlFree(dlistnew);
			return NhlFATAL;
		}

		for(i=0;i < dlistnew->num_items - 1;i++){
			dlistnew->list[i] = dlistold->list[i];
		}
		dlistnew->list[dlistnew->num_items - 1] = dnode;
		dlistnew->extra = dnode;
	}

	*(NhlGenArray *)(to->data.ptrval) = newgen;

	return ret;
}

/*
 * Function:	RemoveData
 *
 * Description:	This function removes data item's from the DataList resource
 *		in the given NhlLayer.
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
#if	NhlNeedProto
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
	int			i,found;
	NhlGenArray		gen=NULL,newgen=NULL;
	_NhlInternDataList	dlistold=NULL,dlistnew=NULL;
	NhlLayer		dil = _NhlGetLayer(from->data.intval);

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"RemoveData:called incorrectly???");
		return NhlFATAL;
	}

	if((dil == NULL) || !(_NhlIsDataItem(dil))){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"RemoveData:invalid Data Object %d",from->data.intval));
		return NhlFATAL;
	}

	gen = *(NhlGenArray*)(to->data.ptrval);
	if((gen == NULL) || (gen->data == NULL)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"RemoveData:Data Object \"%s\" isn't in DataList",
								_NhlName(dil));
		return NhlFATAL;
	}

	dlistold = gen->data;

	for(i=0;i < dlistold->num_items;i++){
		if(dlistold->list[i]->item == dil->base.id){
			dnode = dlistold->list[i];
			break;
		}
	}
	if(dnode == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"RemoveData:Data Object \"%s\" isn't in DataList",
								_NhlName(dil));
		return NhlFATAL;
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
			NhlPError(NhlFATAL,ENOMEM,NULL);
			return NhlFATAL;
		}
		newgen->data = dlistnew;
		*dlistnew = *dlistold;
		dlistnew->num_items--;
		dlistnew->shared_list = True;
		dlistold->shared_list = True;

		dlistnew->list = NhlMalloc(dlistnew->num_items *
						sizeof(_NhlDataNodeRec));
		found = 0;
		for(i=0;i < dlistnew->num_items;i++){
			if(dlistold->list[i + found] == dnode)
				found++;
			dlistnew->list[i] = dlistold->list[i + found];
		}

	}
	else{
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"RemoveData:Shouldn't get this Error - Bad Joke...");
		return NhlFATAL;
	}

	dlistold->extra = dnode;

	*(NhlGenArray*)(to->data.ptrval) = newgen;

	return NhlNOERROR;
}

/*
 * Function:	CvtGenToData
 *
 * Description:	This function converts a GenArray to an NhlTDataList -
 *		actually just converts the GenArray to an int GenArray.
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
CvtGenToData
#if	NhlNeedProto
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
	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"CvtGenToData:called incorrectly???");
		return NhlFATAL;
	}

	return NhlReConvertData(NhlTGenArray,NhlTIntegerGenArray,from,to);
}

/*
 * Function:	CvtScalarToData
 *
 * Description:	This function converts an int to an NhlTDataList -
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
CvtScalarToData
#if	NhlNeedProto
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
	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"CvtScalarToData:called incorrectly???");
		return NhlFATAL;
	}

	return _NhlReConvertData(from->typeQ,
				NrmStringToQuark(NhlTIntegerGenArray),from,to);
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
 *		NhlDataCommClassPart.  This is used to keep track of
 *		all the data resources so they can be set via the NhlAddData
 *		and NhlRemoveData functions.
 *
 * In Args:	
 *		NhlClass	lc	pointer to class structure to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataCommClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class structure to update */
)
#else
(lc)
	NhlClass	lc;	/* pointer to class structure to update */
#endif
{
	NhlDataCommClass	dc = (NhlDataCommClass)lc;

	/*
	 * This field can only be set by _NhlRegisterDataRes calls in
	 * sub-classes ClassPartInit functions.
	 */
	dc->datacomm_class.data_offsets = NULL;

	return NhlNOERROR;
}

static NrmQuark	QListCompiled = NrmNULLQUARK;
static NrmQuark	QGenArray = NrmNULLQUARK;

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
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, lret = NhlNOERROR;

        _NhlEnumVals   levelselectionlist[] = {
        {NhlAUTOMATICLEVELS,	"AutomaticLevels"},
        {NhlMANUALLEVELS, 	"ManualLevels"},
        {NhlEXPLICITLEVELS, 	"ExplicitLevels"},
        {NhlEQUALSPACEDLEVELS,  "EqualSpacedLevels"}
        };

        _NhlEnumVals   scalingmodelist[] = {
        {NhlSCALEFACTOR,	"ScaleFactor"},
        {NhlCONFINETORANGE, 	"ConfineToRange"},
        {NhlTRIMZEROS, 		"TrimZeros"},
        {NhlMAXSIGDIGITSLEFT,	"MaxSigDigitsLeft"},
	{NhlALLINTEGERS,	"AllIntegers"},
	{NhlINTEGERLINELABELS,	"IntegerLineLabels"} /* obsolete synonym */
        };

        _NhlEnumVals   labelbarendstylelist[] = {
	{NhlINCLUDEOUTERBOXES,		"IncludeOuterBoxes"},
	{NhlINCLUDEMINMAXLABELS,	"IncludeMinMaxLabels"},
	{NhlEXCLUDEOUTERBOXES,		"ExcludeOuterBoxes"}
        };

	_NhlRegisterEnumType(NhldataCommClass,NhlTLevelSelectionMode,
		levelselectionlist,NhlNumber(levelselectionlist));
	_NhlRegisterEnumType(NhldataCommClass,NhlTScalingMode,scalingmodelist,
			     NhlNumber(scalingmodelist));
	_NhlRegisterEnumType(NhldataCommClass,NhlTLabelBarEndStyle,
		labelbarendstylelist,NhlNumber(labelbarendstylelist));

	QListCompiled = NrmStringToQuark(_NhlTDListCompiled);
	QGenArray = NrmStringToQuark(NhlTGenArray);

	ret = NhlRegisterConverter(NhldataCommClass,_NhlTAddData,_NhlTDataList,
		AddData,NULL,0,False,NULL);
	lret = NhlRegisterConverter(NhldataCommClass,_NhlTRemoveData,
		_NhlTDataList,RemoveData,NULL,0,False,NULL);
	ret = MIN(ret,lret);
	lret = NhlRegisterConverter(NhldataCommClass,NhlTGenArray,_NhlTDataList,
		CvtGenToData,NULL,0,False,NULL);
	ret = MIN(ret,lret);
	lret = NhlRegisterConverter(NhldataCommClass,NhlTScalar,_NhlTDataList,
		CvtScalarToData,NULL,0,False,NULL);
	return MIN(ret,lret);
}

/*
 * Function:	CreateDataNode
 *
 * Description:	This fuction is used to create the DataNode that contains
 *		all the information needed for the DataComm instance to
 *		communicate with the DataItem for the specified resource.
 *
 * In Args:	
 *		NhlDataCommLayer	dcl,	DataComm subclass using data
 *		_NhlDataOffset	oset,	oset ptr - Data Description
 *		NhlLayer		dl,	Data to connect to
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	_NhlDataNodePtr
 * Side Effect:	
 */
static _NhlDataNodePtr
CreateDataNode
#if	NhlNeedProto
(
	NhlDataCommLayer	dcl,	/* DataComm subclass using data	*/
	_NhlDataOffset		oset,	/* oset ptr - Data Description	*/
	NhlLayer		dil	/* Data to connect to		*/
)
#else
(dcl,oset,dl)
	NhlDataCommLayer	dcl;	/* DataComm subclass using data	*/
	_NhlDataOffset		oset;	/* oset ptr - Data Description	*/
	NhlLayer		dil;	/* DataItem to connect to	*/
#endif
{
	_NhlDataNodePtr		dnode = NULL;
	int			dspecid;
	NhlErrorTypes		ret=NhlNOERROR;

	if(!_NhlIsDataItem(dil)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
"The \"%s\" resource must be set with a DataItem object, not a \"%s\" object",
					NrmNameToString(oset->res_name),
					_NhlClassName(dil->base.layer_class));
		return NULL;
	}

	dnode = NhlMalloc(sizeof(_NhlDataNodeRec));
	if(dnode == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}

	dnode->dhandle = _NhlInitDataConnection(dil,(NhlLayer)dcl,
				oset->res_name,oset->qlist,&dnode->type);
	if(dnode->dhandle == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Unable to init a connection with Data \"%s\"",
							NhlName(dil->base.id));
		(void)NhlFree(dnode);
		return NULL;
	}

	dnode->item = dil->base.id;

	ret = NhlVACreate(&dspecid,dil->base.name,oset->dataspec_class,
							dcl->base.id,NULL);
	dnode->dataspec = (NhlDataSpecLayer)_NhlGetLayer(dspecid);
	if((ret < NhlWARNING) || (dnode->dataspec == NULL)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Unable to process data-dependent resources");
		_NhlCloseDataConnection(dil,dnode->dhandle);
		(void)NhlFree(dnode);
		return NULL;
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
 *		NhlDataCommLayer	dcl,	DataComm object
 *		_NhlDataOffset		oset	offset record
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CompileDataList
#if	NhlNeedProto
(
	NhlDataCommLayer	dcl,	/* DataComm object	*/
	_NhlDataOffset		oset	/* offset record	*/
)
#else
(dcl,oset)
	NhlDataCommLayer	dcl;	/* DataComm object	*/
	_NhlDataOffset		oset;	/* offset record	*/
#endif
{
	char			func[]="CompileDataList";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlGenArray		gen=NULL;
	_NhlInternDataList	ilist;
	_NhlDataNodePtr		*dnodes;
	int			*llist;
	int			i,j,k,len;
	NhlLayer		dil=NULL;
	static NrmQuark		Qint=NrmNULLQUARK;

	if(Qint == NrmNULLQUARK)
		Qint = NrmStringToQuark(NhlTInteger);

	gen = *(NhlGenArray*)((char*)dcl + oset->offset);

	if((gen == NULL) || (gen->typeQ == QListCompiled))
		return NhlNOERROR;

	if((gen->num_dimensions != 1) || (gen->len_dimensions[0] < 1)
					|| (gen->typeQ != Qint)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Unable to set Data resource \"%s\":Invalid Array",
					NrmQuarkToString(oset->res_name));
		ret = NhlWARNING;
		goto BADARRAY;
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
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Ignoring duplicate data \"%s\" in resource \"%s\"",
							NhlName(llist[j]),
					NrmQuarkToString(oset->res_name));
				ret = NhlWARNING;
				len--;
				for(k=j;k<len;k++)
					llist[k] = llist[k+1];

			}
			else
				j++;
		}
		i++;
	}

	/* -1111 is a hack to get an empty GenArray */
	gen = _NhlCreateGenArray(NULL,_NhlTDListCompiled,0,0,NULL,False);
	ilist = NhlMalloc(sizeof(_NhlInternDataListRec));
	dnodes = NhlMalloc(sizeof(_NhlDataNodePtr) * len);
	if((ilist == NULL) || (dnodes == NULL) || (gen == NULL)){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		(void)NhlFree(ilist);
		(void)NhlFree(dnodes);
		(void)NhlFree(gen);
		ret = NhlWARNING;
		goto BADARRAY;
	}

	gen->data = ilist;

	j = 0;
	ilist->list = dnodes;
	for(i=0;i < len;i++){
		dil = _NhlGetLayer(llist[i]);
		if((dil == NULL)|| !(_NhlIsDataItem(dil))){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
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
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Unable to add DataItem \"%s\" to DataList \"%s\"",
			NhlName(llist[i]),NrmQuarkToString(oset->res_name));
				j--;
			}
		}
	}

	ilist->num_items = len + j;

	if(ilist->num_items < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:DataList has no valid members",func);
		(void)NhlFree(ilist);
		(void)NhlFree(dnodes);
		(void)NhlFree(gen);
		ret = NhlWARNING;
		goto BADARRAY;
	}
	ilist->extra = NULL;
	ilist->shared_list = False;
	ilist->dcl = dcl;
	ilist->oset = oset;

	*(NhlGenArray*)((char*)dcl + oset->offset) = gen;

	return NhlNOERROR;

BADARRAY:
	*(NhlGenArray*)((char*)dcl + oset->offset) = NULL;

	return ret;
}

/*
 * Function:	DataCommInitialize
 *
 * Description:	This function initializes the DataComm instance.
 *
 * In Args:	
 *		NhlClass	lc,	class
 *		NhlLayer		req,	requested
 *		NhlLayer		new,	new
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
#if	NhlNeedProto
(
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlClass	lc;	/* class	*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlErrorTypes		lret = NhlNOERROR;
	NhlDataCommClass	cc = (NhlDataCommClass)req->base.layer_class;
	_NhlDataOffset		oset = cc->datacomm_class.data_offsets;
	
	while(oset != NULL){

		lret = CompileDataList((NhlDataCommLayer)new,oset);
		ret = MIN(ret,lret);

		if(oset->dsoffset){
			*(NhlGenArray*)((char*)new + oset->dsoffset) = NULL;
		}

		oset = oset->next;
	}

	((NhlDataCommLayer)new)->datacomm.data_changed = False;

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
 *		NhlLayer	old,	old
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
DataCommSetValues
#if	NhlNeedProto
(
	NhlLayer	old,	/* old		*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(old,req,new,args,nargs)
	NhlLayer	old;	/* old		*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	NhlDataCommClass	cc = (NhlDataCommClass)
							new->base.layer_class;
	_NhlDataOffset		oset = cc->datacomm_class.data_offsets;
	NhlErrorTypes		lret=NhlNOERROR,ret=NhlNOERROR;

	while(oset != NULL){

		if(*(NhlPointer*)((char*)new + oset->offset) !=
				*(NhlPointer*)((char*)old + oset->offset)){
			lret = CompileDataList((NhlDataCommLayer)new,oset);
			/*
			 * If there is an error compiling the DataList then
			 * reset the NewData list to the OldData list.
			 */
			if(lret != NhlNOERROR){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			"DataCommSetValues:Unable to set \"%s\" resource",
					NrmQuarkToString(oset->res_name));
				/*
				 * Boy - this is pretty.
				 */
				*(NhlPointer*)((char*)new + oset->offset) =
				*(NhlPointer*)((char*)old + oset->offset);
			
				ret = MIN(ret,NhlWARNING);
			}
		}

		if(oset->dsoffset &&
			(*(NhlPointer*)((char*)new + oset->dsoffset) !=
				*(NhlPointer*)((char*)old + oset->dsoffset))){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"DataCommSetValues:\"%s\" is a GetValues only resource",
					NrmQuarkToString(oset->dsres_name));
			/*
			 * Boy - this is pretty.
			 */
			*(NhlPointer*)((char*)new + oset->dsoffset) =
				*(NhlPointer*)((char*)old + oset->dsoffset);
			
			ret = MIN(ret,NhlWARNING);
		}

		oset = oset->next;
	}

	return ret;
}

/*
 * Function:	DataCommGetValues
 *
 * Description:	This is the getvalues method def for the DataComm class.
 *		It takes any data resources the user retrieves. It turns
 *		the compiled data list into a gen array.
 *		
 *
 * In Args:	
 *		NhlLayer	old,	old
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
DataCommGetValues
#if	NhlNeedProto
(
	NhlLayer	l,	/* layer	*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(l,args,nargs)
	NhlLayer	l;	/* layer	*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	NhlDataCommClass	cc = (NhlDataCommClass)
							l->base.layer_class;
	_NhlDataOffset		oset;
	_NhlInternDataList	ilist;
	NhlGenArray		gen;
	int			i, *iarray;
	ng_size_t		j, len;
	NhlBoolean		dsres;

	for(i=0;i < nargs; i++){
		dsres = False;
		for(oset = cc->datacomm_class.data_offsets;
					oset != NULL; oset = oset->next){
			if(args[i].quark == oset->res_name)
				break;
			else if(oset->dsoffset && 
				(args[i].quark == oset->dsres_name)){
				dsres = True;
				break;
			}
		}

		if(oset == NULL)
			continue;


		gen = *(NhlGenArray*)((char*)l + oset->offset);
		if(gen){
			ilist = (_NhlInternDataList)gen->data;
			len = ilist->num_items;
			iarray = NhlMalloc(sizeof(int)*len);
			if(iarray == NULL){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}

			if(dsres){
				for(j=0;j < len;j++)
					iarray[j] =
					ilist->list[j]->dataspec->base.id;
			}
			else{
				for(j=0;j < len;j++)
					iarray[j] = ilist->list[j]->item;
			}

			gen = _NhlCreateGenArray(iarray,NhlTObjId,sizeof(int),
								1,&len,False);
			if(gen == NULL){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
			gen->my_data = True;
		}

		*(NhlGenArray *)args[i].value.ptrval = gen;
		*args[i].type_ret = QGenArray;
		*args[i].size_ret = sizeof(NhlGenArray);
		*args[i].free_func = (_NhlFreeFunc)NhlFreeGenArray;
	}

	return NhlNOERROR;
}

/*
 * Function:	ListNotShared
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static void
ListNotShared
#if	NhlNeedProto
(
	NhlGenArray		gen	/* list to free	*/
)
#else
(gen)
	NhlGenArray		gen;	/* list to free	*/
#endif
{
	_NhlInternDataList	ilist = NULL;

	if((gen == NULL) || (gen->typeQ != QListCompiled))
		return;

	ilist = gen->data;
	ilist->shared_list = False;
	ilist->extra = NULL;

	return;
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
#if	NhlNeedProto
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
#if	NhlNeedProto
(
	NhlLayer	old,	/* old		*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(old,req,new,args,nargs)
	NhlLayer	old;	/* old		*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	NhlDataCommClass	dc = (NhlDataCommClass)
							new->base.layer_class;
	_NhlDataOffset		oset = dc->datacomm_class.data_offsets;
	NhlGenArray		*ol, *nl, *rl;
	
	while(oset != NULL){

		ol = (NhlGenArray*)((char *)old + oset->offset);
		nl = (NhlGenArray*)((char *)new + oset->offset);
		rl = (NhlGenArray*)((char *)req + oset->offset);

		if(*ol != *rl){	/* if ol == rl then resource not involved */
			if(*nl == *ol)
				FreeInternDataList(*rl);
			else
				FreeInternDataList(*ol);
			ListNotShared(*nl);
		}

		oset = oset->next;
	}

	return NhlNOERROR;
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
#if	NhlNeedProto
(
	_NhlDataNodePtr	node	/* node to free	*/
)
#else
(node)
	_NhlDataNodePtr	node;	/* node to free	*/
#endif
{
	NhlLayer	item;

	if(node == NULL)
		return;

	item = _NhlGetLayer(node->item);

	if((item == NULL) || (!_NhlIsDataItem(item))){
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"FreeDataNode:Data is corrupted - bad item value"));
	}
	else{
		/*
		 * Close Data Connection in Mgr
		 */
		_NhlCloseDataConnection(item,node->dhandle);
	}

	/*
	 * Must set this internal dspec field so the dspec knows it is
	 * the dcomm that is destroying it.  Otherwise it forces a
	 * destroy of the dcomm.
	 */
	node->dataspec->dataspec.destroying = True;
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
#if	NhlNeedProto
(
	NhlDataCommClass	lc,		/* class pointer	*/
	NhlDataCommLayer	l,		/* instance pointer	*/
	NhlDataCommLayer	old		/* instance pointer	*/
)
#else
(lc,l,old)
	NhlDataCommClass	lc;		/* class pointer	*/
	NhlDataCommLayer	l;		/* instance pointer	*/
	NhlDataCommLayer	old;		/* instance pointer	*/
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, lret = NhlNOERROR;
	NhlClass	newlc = lc->base_class.superclass;

	if((newlc != NULL) &&
		(newlc->base_class.class_inited & _NhlDataCommClassFlag)){
		lret = CallUpdateData((NhlDataCommClass)newlc,l,old);
	}

	if(lret < NhlWARNING)
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
#if	NhlNeedProto
(
	NhlDataCommLayer	dcomm
)
#else
(dcomm)
	NhlDataCommLayer	dcomm;
#endif
{
	NhlDataCommLayer		oldl;
	NhlDataCommClass	lc =(NhlDataCommClass)
						_NhlClass((NhlLayer)dcomm);
	NhlErrorTypes		ret = NhlNOERROR;

	if(!dcomm->datacomm.data_changed)
		return NhlNOERROR;

	oldl = NhlMalloc(lc->base_class.layer_size);
	if(oldl == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
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
#if	NhlNeedProto
(
	NhlLayer	l	/* id of a datacomm object	*/
)
#else
(l)
	NhlLayer	l;	/* id of a datacomm object	*/
#endif
{
	return UpdateData((NhlDataCommLayer)l);
}

/*
 * Function:	DataCommDestroy
 *
 * Description:	This fuction is used to free all the memory allocated on
 *		behalf of the DataComm layer given. It closes all data
 *		connections.
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
DataCommDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlDataCommClass	cc = (NhlDataCommClass)l->base.layer_class;
	_NhlDataOffset		oset = cc->datacomm_class.data_offsets;
	NhlPointer		field;

	while(oset != NULL){
		field = (NhlPointer)((char *)l + oset->offset);

		FreeInternDataList(*(NhlGenArray*)field);

		oset = oset->next;
	}

	return NhlNOERROR;
}

/*
 * Method definitions for DataSpec class
 */

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
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	char			func[] = "DataSpecDestroy";
	NhlDataSpecLayer	dsl = (NhlDataSpecLayer)l;

	if(!dsl->dataspec.destroying){
		if (! dsl->base.parent)
			return NhlNOERROR;

		NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s:Destroy called from somewhere other than parent! Destroying parent to keep integrity",func);
		return NhlDestroy(dsl->base.parent->base.id);
	}

	return NhlNOERROR;
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
 *		NhlDataCommClass	dc,		DataComm sub-class
 *		NhlString		res_name,	name of data resource
 *		NhlClass		dataspec,	DataSpecific object
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
#if	NhlNeedVarArgProto
(
	NhlDataCommClass	dc,		/* DataComm sub-class	*/
	NrmString		res_name,	/* name of data res	*/
	NrmString		dsres_name,	/* name of dataspec res	*/
	NhlClass		dataspec,	/* DataSpecific object	*/
	...					/* types requested	*/
)
#else
(dc,res_name,dsres_name,dataspec,va_alist)
	NhlDataCommClass	dc;		/* DataComm sub-class	*/
	NrmString		res_name;	/* name of data res	*/
	NrmString		dsres_name;	/* name of dataspec res	*/
	NhlClass		dataspec;	/* DataSpecific object	*/
	va_dcl					/* types requested	*/
#endif
{
	static NrmQuark	QDataList=NrmNULLQUARK;
	static NrmQuark	QDataSpecList=NrmNULLQUARK;
	va_list		ap;
	int		i,num_types = 0;
	NhlClass	tmp;
	NrmQuark	Qres_name,Qdsres_name;
	NrmQuarkList	qlist=NULL;
	unsigned int	offset = 0;
	_NhlDataOffset	oset=NULL;
	NhlBoolean	found = False;
	NhlBoolean	old = False;
	NrmResourceList	nrmres = (NrmResourceList)dc->base_class.resources;
	NhlErrorTypes	ret = NhlNOERROR;

	if(QDataList == NrmNULLQUARK){
		QDataList= NrmStringToQuark(_NhlTDataList);
	}
	if(QDataSpecList == NrmNULLQUARK){
		QDataSpecList= NrmStringToQuark(_NhlTDataSpecList);
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"Trying to register a non-existent DataList resource %s",
								res_name);
		return NhlFATAL;
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
			NhlPError(NhlFATAL,ENOMEM,NULL);
			return NhlFATAL;
		}
	}

	VA_START(ap,dataspec);
	for(tmp = (NhlClass)(va_arg(ap,NhlClass));tmp != NULL;
				tmp = (NhlClass)(va_arg(ap,NhlClass)))
		num_types++;
	va_end(ap);

	if(!num_types){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to register DataList resource %s",res_name);
		if(!old)
			(void)NhlFree(oset);
		return NhlFATAL;
	}

	qlist = (NrmQuarkList)NhlMalloc(sizeof(NrmQuark)*(num_types + 1));
	if(qlist == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		if(!old)
			(void)NhlFree(oset);
		return NhlFATAL;
	}
	if(old)
		(void)NhlFree(oset->qlist);
	oset->qlist = qlist;

	VA_START(ap,dataspec);
	for(i=0;i < num_types;i++){
		tmp = (NhlClass)va_arg(ap,NhlClass);
		ret = _NhlInitializeClass(tmp);
		if(ret < NhlWARNING)
			return ret;
		oset->qlist[i] = tmp->base_class.nrm_class;
	}
	va_end(ap);

	oset->qlist[num_types] = NrmNULLQUARK;
	oset->res_name = Qres_name;
	oset->offset = offset;
	oset->dataspec_class = dataspec;
	if(dsres_name){
		/*
		 * Make sure res_name is a resource in the class - then find out
		 * what the offset is for that resource.
		 */
		found = False;
		Qdsres_name = NrmStringToQuark(dsres_name);
		for(i=0;i < dc->base_class.num_resources; i++){
			if((nrmres[i].nrm_name == Qdsres_name) &&
					(nrmres[i].nrm_type == QDataSpecList)){
				offset = nrmres[i].nrm_offset;
				found = True;
				break;
			}
		}
		if(!found){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
		"Trying to register a non-existent DataSpecList resource %s",
								dsres_name);
			NhlFree(qlist);
			NhlFree(oset);
			return NhlFATAL;
		}

		oset->dsres_name = Qdsres_name;
		oset->dsoffset = offset;
	}
	else{
		oset->dsres_name = NrmNULLQUARK;
		oset->dsoffset = 0;
	}

	if(!old){
		oset->next = dc->datacomm_class.data_offsets;
		dc->datacomm_class.data_offsets = oset;
	}

	return NhlNOERROR;
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
#if	NhlNeedProto
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
 * Returns:	NhlLayer - NULL==Failed
 * Side Effect:	
 */
NhlLayer
_NhlGetDataSet
#if	NhlNeedProto
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
	NhlLayer		item = NULL;

	/*
	 * Make sure the DataItem still exists.
	 */
	item = _NhlGetLayer(dnode->item);
	if((item == NULL) || (!_NhlIsDataItem(item))){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
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
#if	NhlNeedProto
(
	int	dcommid		/* id of datacomm class	*/
)
#else
(dcommid)
	int	dcommid;	/* id of datacomm class	*/
#endif
{
	NhlDataCommLayer	dcomm = (NhlDataCommLayer)_NhlGetLayer(dcommid);

	if((dcomm == NULL) || !_NhlIsDataComm(dcomm)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"_NhlDataUpdate:called for a non-DataComm class");
		return NhlFATAL;
	}

	dcomm->datacomm.data_changed = True;

	if(!dcomm->datacomm.delay_compute)
		return UpdateData(dcomm);

	return NhlNOERROR;
}

/*
 * Function:	_NhlGetDSpecId
 *
 * Description:	This function should return 0 on failure. This will indicate
 *		to the caller of NhlAddData that their Data was added,
 *		but it isn't possible to retrive the DSpec id for some
 *		reason.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static int
_NhlGetDSpecId
#if	NhlNeedProto
(
	NhlLayer	l,
	NrmQuark	qname,
	int		ditemid
)
#else
(l,qname,ditemid)
	NhlLayer	l;
	NrmQuark	qname;
	int		ditemid;
#endif
{
	NhlDataCommClassPart	*cp =
		&((NhlDataCommClass)l->base.layer_class)->datacomm_class;
	_NhlDataOffset			oset;
	_NhlInternDataList		ilist;
	NhlGenArray			gen;
	int				i;

	for(oset = cp->data_offsets;oset != NULL;oset = oset->next){
		if(qname == oset->res_name)
			break;
	}

	if(oset == NULL)
		return NhlNULLOBJID;

	gen = *(NhlGenArray*)((char*)l + oset->offset);
	if(!gen)
		return NhlNULLOBJID;

	ilist = gen->data;
	for(i=0;i < ilist->num_items;i++){
		if(ditemid == ilist->list[i]->item)
			return ilist->list[i]->dataspec->base.id;
	}

	return NhlNULLOBJID;
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
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
int
NhlAddData
#if	NhlNeedProto
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
	_NhlArg		larg;
	NhlLayer	dcl = _NhlGetLayer(dcommid);
	NhlErrorTypes	ret = NhlNOERROR;

	if((dcl == NULL) || !_NhlIsDataComm(dcl)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"NhlAddData:Invalid DataComm id %d",dcommid);
		return NhlFATAL;
	}

	larg.quark = NrmStringToQuark(res_name);
	larg.value.intval = ditemid;
	larg.type = NrmStringToQuark(_NhlTAddData);

	ret = _NhlSetLayerValues(dcl,&larg,1);

	if(ret < NhlWARNING)
		return ret;

	return _NhlGetDSpecId(dcl,larg.quark,ditemid);
}

/*
 * Function:	nhlf_adddata
 *
 * Description:	Private Fortran binding for NhlAddData
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlfpadddata,NHLFPADDDATA)
#if	NhlNeedProto
(
	int		*pid,
	_NhlFString	fname,
	int		*fname_len,
	int		*did,
	int		*err
)
#else
(pid,fname,fname_len,did,err)
	int		*pid;
	_NhlFString	fname;
	int		*fname_len;
	int		*did;
	int		*err;
#endif
{
	char	name[_NhlMAXRESNAMLEN];

	if(!_NhlFstrToCstr(name,sizeof(name),fname,*fname_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to create C string from Fortran string");
		*err = NhlFATAL;
		return;
	}

	*err = NhlAddData(*pid,name,*did);

	return;
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
#if	NhlNeedProto
(
	int		dcommid,	/* id of layer w/ data resource	*/
	NhlString	res_name,	/* name of data resource	*/
	int		ditemid		/* id of data to rm		*/
)
#else
(dcommid,res_name,ditemid)
	int		dcommid;	/* id of layer w/ data resource	*/
	NhlString	res_name;	/* name of data resource	*/
	int		ditemid;	/* id of data to rm		*/
#endif
{
	_NhlArg		larg;
	NhlLayer	dcl = _NhlGetLayer(dcommid);

	if((dcl == NULL) || !_NhlIsDataComm(dcl)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"NhlRemoveData:Invalid DataComm id %d",dcommid);
		return NhlFATAL;
	}

	larg.quark = NrmStringToQuark(res_name);
	larg.value.intval = ditemid;
	larg.type = NrmStringToQuark(_NhlTRemoveData);

	return _NhlSetLayerValues(dcl,&larg,1);
}

/*
 * Function:	nhlf_removedata
 *
 * Description:	Private Fortran binding for NhlAddData
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlfpremovedata,NHLFPREMOVEDATA)
#if	NhlNeedProto
(
	int		*pid,
	_NhlFString	fname,
	int		*fname_len,
	int		*did,
	int		*err
)
#else
(pid,fname,fname_len,did,err)
	int		*pid;
	_NhlFString	fname;
	int		*fname_len;
	int		*did;
	int		*err;
#endif
{
	char	name[_NhlMAXRESNAMLEN];

	if(!_NhlFstrToCstr(name,sizeof(name),fname,*fname_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to create C string from Fortran string");
		*err = NhlFATAL;
		return;
	}

	*err = NhlRemoveData(*pid,name,*did);

	return;
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
#if	NhlNeedProto
(
	int	dcommid		/* id of dcomm object	*/
)
#else
(dcommid)
	int	dcommid;	/* id of dcomm object	*/
#endif
{
	NhlDataCommLayer	dcomm = (NhlDataCommLayer)_NhlGetLayer(dcommid);

	if((dcomm == NULL) || !_NhlIsDataComm(dcomm)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"NhlUpdateData:Invalid Object id #%d",dcommid);
		return NhlFATAL;
	}

	return UpdateData(dcomm);
}

/*
 * Function:	nhlf_updatedata
 *
 * Description:	Fortran binding for NhlUpdateData.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Private Fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlfpupdatedata,NHLFPUPDATEDATA)
#if	NhlNeedProto
(
	int	*id,
	int	*err
)
#else
(id,err)
	int	*id;
	int	*err;
#endif
{
	*err = NhlUpdateData(*id);

	return;
}

/*
 * Function:	NhlIsDataComm
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlBoolean
NhlIsDataComm
#if	NhlNeedProto
(
	int	pid
)
#else
(pid)
	int	pid;
#endif
{
	NhlLayer	l = _NhlGetLayer(pid);

	if(l && _NhlIsDataComm(l))
		return True;

	return False;
}

/*
 * Function:	nhlpfisdatacomm
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfisdatacomm,NHLPFISDATACOMM)
#if	NhlNeedProto
(
	int	*id,
	int	*status
)
#else
(id,status)
	int	*id;
	int	*status;
#endif
{
	*status = NhlIsDataComm(*id);

	return;
}

/*
 * Function:	NhlIsDataSpec
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlBoolean
NhlIsDataSpec
#if	NhlNeedProto
(
	int	pid
)
#else
(pid)
	int	pid;
#endif
{
	NhlLayer	l = _NhlGetLayer(pid);

	if(l && _NhlIsDataSpec(l))
		return True;

	return False;
}

/*
 * Function:	nhlpfisdataspec
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfisdataspec,NHLPFISDATASPEC)
#if	NhlNeedProto
(
	int	*id,
	int	*status
)
#else
(id,status)
	int	*id;
	int	*status;
#endif
{
	*status = NhlIsDataSpec(*id);

	return;
}
