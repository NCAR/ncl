/*
 *      $Id: DataItem.c,v 1.1 1993-07-12 22:36:05 boote Exp $
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

#define	PFIX	"-mgr"

/************************************************************************
*									*
*	DataItem Class declarations					*
*									*
************************************************************************/

static NhlErrorTypes DataItemClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes DataItemInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes DataItemDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

DataItemLayerClassRec dataItemLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* superclass			*/	(LayerClass)&baseLayerClassRec,
/* class_name			*/	"DataItem",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(DataItemLayerRec),
/* resources			*/	NULL,
/* num_resources		*/	0,
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	DataItemClassInitialize,
/* layer_initialize		*/	DataItemInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	DataItemDestroy
	},
	/* DataItemLayerClassPart */
	{
/* foo				*/	NULL
	}
};
	
LayerClass dataItemLayerClass = (LayerClass)&dataItemLayerClassRec;

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
 * Function:	DataItemClassInitialize
 *
 * Description:	This function is used to initialize the DataItem LayerClass
 *		record.
 *
 * In Args:	none
 *
 * Out Args:	none
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataItemClassInitialize
#if	__STDC__
(
	void
)
#else
()
#endif
{
	/*
	 * Register child classes - DataMgr.  Should automatically call
	 * setvalues - and this class should not intercept any of the
	 * resources for the manager.
	 */
	return _NhlRegisterChildClass(dataItemLayerClass,dataMgrLayerClass,
							True,False,NULL);
}

/*
 * Function:	DataItemInitialize
 *
 * Description:	This function initializes an instance of a DataItem class
 *		object.
 *
 * In Args:	
 *	LayerClass	lc,	class
 *	Layer		req,	requested
 *	Layer		new,	new
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
	DataItemLayer	dinew = (DataItemLayer)new;
	DataItemLayer	direq = (DataItemLayer)req;
	NhlErrorTypes	ret = NOERROR;
	char		tstring[MAXRESNAMLEN];
	int		tint;

	/*
	 * Create the Manager
	 */
	strcpy(tstring,direq->base.name);
	strcat(tstring,PFIX);
	ret = _NhlCreateChild(&tint,tstring,dataMgrLayerClass,new,
				NULL);

	if(ret < WARNING)
		return ret;

	dinew->dataitem.manager = _NhlGetLayer(tint);
	if(dinew->dataitem.manager == NULL)
		return FATAL;

	return ret;
}

/*
 * Function:	DataItemDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the DataItemLayerClass.
 *
 * In Args:	Layer	l
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
DataItemDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
{
	DataItemLayer	dil = (DataItemLayer)l;
	NhlErrorTypes	ret = NOERROR;

	ret = _NhlDestroyChild(dil->dataitem.manager->base.id,l);

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
 *		a set-values, to notify the supre-class, and the manager
 *		that the data has changed.
 *
 * In Args:	
 *		Layer	l	data item layer
 *
 * Out Args:	
 *
 * Scope:	Private to DataItem sub-classes
 * Returns:	void
 * Side Effect:	
 */
void
_NhlDataChanged
#if	__STDC__
(
	Layer	l	/* data item layer	*/
)
#else
(l)
	Layer	l;	/* data item layer	*/
#endif
{
	DataItemLayer	dl = (DataItemLayer)l;

	if(!_NhlIsDataItem(l)){
		NhlPError(FATAL,E_UNKNOWN,
		"_NhlDataChanged:Can only be called with a DataItem sub-class");
		return;
	}

	_NhlDataItemModified((DataMgrLayer)dl->dataitem.manager);

	return;
}

/************************************************************************
*									*
*	Public API							*
*									*
************************************************************************/

/* none yet */
