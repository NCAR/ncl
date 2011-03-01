/*
 *      $Id: DataItem.c,v 1.18 2003-09-10 21:29:53 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataItem.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jun 24 10:13:13 MDT 1993
 *
 *	Description:	This class is used to communicate with the DataMgr
 *			class.  It is intended as the Superclass of all
 *			future data classes or at least an example of how
 *			to write data classes.
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/DataItemP.h>
#include <ncarg/hlu/DataMgrF.h>
#include <ncarg/hlu/ConvertersP.h>

#define	PFIX	"-mgr"

/************************************************************************
*									*
*	DataItem Class declarations					*
*									*
************************************************************************/

/*
 * Resource list
 */
#define	Oset(field)	NhlOffset(NhlDataItemLayerRec,dataitem.field)
static NhlResource resources[] = {
	{ NhlNnoManager, NhlCnoManager, NhlTBoolean, sizeof(NhlBoolean),
		  Oset(no_manager), NhlTImmediate,_NhlUSET((NhlPointer)False),
          		_NhlRES_PRIVATE,NULL}
};

#undef Oset
static NhlErrorTypes DataItemClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes DataItemClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes DataItemInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataItemSetValues(
#if	NhlNeedProto
	NhlLayer	old,	/* old		*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataItemSetValuesHook(
#if	NhlNeedProto
	NhlLayer	old,	/* old		*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataItemDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

NhlDataItemClassRec NhldataItemClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"dataItemClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlDataItemLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlbaseClassRec,
/* cvt_table			*/	NULL,

/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	DataItemClassPartInitialize,
/* class_initialize		*/	DataItemClassInitialize,
/* layer_initialize		*/	DataItemInitialize,
/* layer_set_values		*/	DataItemSetValues,
/* layer_set_values_hook	*/	DataItemSetValuesHook,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	DataItemDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	/* NhlDataItemClassPart */
	{
/* foo				*/	0
	}
};
	
NhlClass NhldataItemClass = (NhlClass)&NhldataItemClassRec;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/* none yet */


/************************************************************************
*									*
*	Method definitions						*
*									*
************************************************************************/

/*
 * Function:	DataItemClassInitialize
 *
 * Description:
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
DataItemClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{

        _NhlEnumVals   gridtypelist[] = {
        {NhlBASICGRID,		"BasicGrid"},
        {NhlSPHERICALGRID, 	"SphericalGrid"},
        };


	_NhlRegisterEnumType(NhldataItemClass,NhlTdiGridType,
		gridtypelist,NhlNumber(gridtypelist));

	return NhlNOERROR;
}

/*
 * Function:	DataItemClassPartInitialize
 *
 * Description:	This function is used to initialize the NhlDataItemClassPart
 *		record.
 *
 * In Args:	
 *		NhlClass	lc
 *
 * Out Args:	none
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataItemClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc
)
#else
(lc)
	NhlClass	lc;
#endif
{
	/*
	 * Register child classes - DataMgr.  Should automatically call
	 * setvalues - and this class should not intercept any of the
	 * resources for the manager.
	 */
	return _NhlRegisterChildClass(lc,NhldataMgrClass,True,False,NULL);
}

/*
 * Function:	DataItemInitialize
 *
 * Description:	This function initializes an instance of a DataItem class
 *		object.
 *
 * In Args:	
 *	NhlClass	lc,	class
 *	NhlLayer	req,	requested
 *	NhlLayer	new,	new
 *	_NhlArgList	args,	args
 *	int		nargs	nargs
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
DataItemInitialize
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
	NhlDataItemLayer	dinew = (NhlDataItemLayer)new;
	NhlDataItemLayer	direq = (NhlDataItemLayer)req;
	NhlErrorTypes		ret = NhlNOERROR;
	char			tstring[_NhlMAXRESNAMLEN];
	int			tint;

	/* initialize field - used in setvalues/hook */
	dinew->dataitem.change_called = True;

	/*
	 * if this object is being created by a converter as an internal
	 * data set, then a Manager should not be created.
	 */
	if(direq->dataitem.no_manager){
		dinew->dataitem.manager = NULL;
		return NhlNOERROR;
	}

	/*
	 * Create the Manager
	 */
	strcpy(tstring,direq->base.name);
	strcat(tstring,PFIX);
	ret = _NhlVACreateChild(&tint,tstring,NhldataMgrClass,new,
				NULL);

	if(ret < NhlWARNING)
		return ret;

	dinew->dataitem.manager = _NhlGetLayer(tint);
	if(dinew->dataitem.manager == NULL)
		return NhlFATAL;

	return ret;
}

/*
 * Function:	DataItemSetValues
 *
 * Description:
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
DataItemSetValues
#if	NhlNeedProto
(
	NhlLayer		old,	/* old		*/
	NhlLayer		req,	/* requested	*/
	NhlLayer		new,	/* new		*/
	_NhlArgList		args,	/* args		*/
	int			nargs	/* nargs	*/
)
#else
(old,req,new,args,nargs)
	NhlLayer		old;	/* old		*/
	NhlLayer		req;	/* requested	*/
	NhlLayer		new;	/* new		*/
	_NhlArgList		args;	/* args		*/
	int			nargs;	/* nargs	*/
#endif
{
	NhlDataItemLayer	dil = (NhlDataItemLayer)new;

	dil->dataitem.change_called = False;

	return NhlNOERROR;
}

/*
 * Function:	DataItemSetValuesHook
 *
 * Description:
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
DataItemSetValuesHook
#if	NhlNeedProto
(
	NhlLayer		old,	/* old		*/
	NhlLayer		req,	/* requested	*/
	NhlLayer		new,	/* new		*/
	_NhlArgList		args,	/* args		*/
	int			nargs	/* nargs	*/
)
#else
(old,req,new,args,nargs)
	NhlLayer		old;	/* old		*/
	NhlLayer		req;	/* requested	*/
	NhlLayer		new;	/* new		*/
	_NhlArgList		args;	/* args		*/
	int			nargs;	/* nargs	*/
#endif
{
	NhlDataItemLayer	dil = (NhlDataItemLayer)new;

	if(!dil->dataitem.change_called){
		dil->dataitem.change_called = True;
		if(memcmp((void*)old,(void*)new,
				new->base.layer_class->base_class.layer_size))
			_NhlDataItemModified(dil->dataitem.manager);
	}
	return _NhlNotifyDataComm(dil->dataitem.manager);
}

/*
 * Function:	DataItemDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlDataItemClass.
 *
 * In Args:	NhlLayer	l
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataItemDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlDataItemLayer	dil = (NhlDataItemLayer)l;
	NhlLayer		tmp = (NhlLayer)dil->dataitem.manager;
	NhlErrorTypes	ret = NhlNOERROR;

	if(tmp != NULL)
		ret = _NhlDestroyChild(tmp->base.id,l);

	return ret;
}

/************************************************************************
*									*
*	Private API for sub-classes					*
*									*
************************************************************************/

/*
 * Function:	_NhlDataChanged
 *
 * Description:	This function is used by sub-classes of the DataItem during
 *		a set-values, to notify the super-class, and the manager
 *		that the data has changed.
 *
 * In Args:	
 *		NhlLayer	l	data item layer
 *
 * Out Args:	
 *
 * Scope:	Private to DataItem sub-classes
 * Returns:	void
 * Side Effect:	
 */
void
_NhlDataChanged
#if	NhlNeedProto
(
	NhlDataItemLayer	l,	/* data item layer	*/
	NhlBoolean		status
)
#else
(l,status)
	NhlDataItemLayer	l;	/* data item layer	*/
	NhlBoolean		status;
#endif
{
	if(!_NhlIsDataItem(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"_NhlDataChanged:Can only be called with a DataItem sub-class");
		return;
	}

	l->dataitem.change_called = True;

	if(status)
		_NhlDataItemModified((NhlLayer)l->dataitem.manager);

	return;
}

/************************************************************************
*									*
*	Public API							*
*									*
************************************************************************/

/*
 * Function:	NhlIsDataItem
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
NhlIsDataItem
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

	if(l && _NhlIsDataItem(l))
		return True;

	return False;
}

/*
 * Function:	nhlpfisdataitem
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
void _NHLCALLF(nhlpfisdataitem,NHLPFISDATAITEM)
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
	*status = NhlIsDataItem(*id);

	return;
}
