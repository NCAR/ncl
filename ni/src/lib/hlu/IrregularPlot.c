/*
 *      $Id: IrregularPlot.c,v 1.3 1994-01-12 00:34:23 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularPlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a generic Irregular plot that
 *			can serve as the base of an overlay plot or be used 
 *			for Irregular plots that do not use hlu plot objects.
 *			In this context irregular means a plot where 
 *			the distance along either or both coordinate axes
 *			is not uniformly scaled.
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/IrregularPlotP.h>

#if 0
static NhlResource resources[] = {
	{ NhlNtfOverlayPlotBase,NhlCtfOverlayPlotBase,
		NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(IrregularPlotLayerRec,trans.overlay_plot_base),
		NhlTImmediate,(NhlPointer)False},
};
#endif

/* base methods */


static NhlErrorTypes IrregularPlotClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes IrregularPlotClassPartInitialize(
#ifdef NhlNeedProto
	LayerClass	lc
#endif
);

static NhlErrorTypes IrregularPlotInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes IrregularPlotSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes IrregularPlotDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes IrregularPlotDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	IrregularPlotLayer	xnew,
	IrregularPlotLayer	xold,
	NhlBoolean	init
#endif
);

IrregularPlotLayerClassRec irregularPlotLayerClassRec = {
        {
/* class_name			*/      "IrregularPlot",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(IrregularPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (LayerClass)&transformLayerClassRec,

/* layer_resources		*/	NULL, /* resources */
/* num_resources		*/	0, /*NhlNumber(resources), */
/* all_resources		*/	NULL,

/* class_part_initialize	*/	IrregularPlotClassPartInitialize,
/* class_initialize		*/	IrregularPlotClassInitialize,
/* layer_initialize		*/	IrregularPlotInitialize,
/* layer_set_values		*/	IrregularPlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	IrregularPlotDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      IrregularPlotDraw,

/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	NULL
	},
	{
/* overlay_capability 		*/	_tfOverlayBaseOrMember,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL
	},
	{
/* foo				*/	NULL
	}
};
	
LayerClass irregularPlotLayerClass = (LayerClass)&irregularPlotLayerClassRec;


/*
 * Function:	IrregularPlotClassInitialize
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
IrregularPlotClassInitialize
#if __STDC__
(
	void
)
#else
()
#endif
{

	return NOERROR;
}

/*
 * Function:	IrregularPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		IrregularPlotLayerClassPart that cannot be initialized 
 *		statically.
 *		Calls _NhlRegisterChildClass for the overlay manager object.
 *
 * In Args:	
 *		LayerClass	lc	Layer Class to init
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

static NhlErrorTypes
IrregularPlotClassPartInitialize
#if	__STDC__
(
	LayerClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	LayerClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes	ret = NOERROR, subret = NOERROR;
	char		*e_text;
	char		*entry_name = "IrregularPlotClassPartInitialize";

/*
 * Register children objects
 */
	subret = _NhlRegisterChildClass(lc,overlayLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < WARNING) {
		e_text = "%s: error registering %s";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  "overlayLayerClass");
		return(FATAL);
	}

	subret = _NhlRegisterChildClass(lc,irregularType2TransObjLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < WARNING) {
		e_text = "%s: error registering %s";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  "mapTransObjLayerClass");
		return(FATAL);
	}

	return ret;
}


/*
 * Function:	IrregularPlotInitialize
 *
 * Description: 
 *
 * In Args: 	class	objects layer_class
 *		req	instance record of requested values
 *		new	instance record of new object
 *		args	list of resources and values for reference
 *		num_args 	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	state change in GKS due to mapping transformations.
 */
/*ARGSUSED*/
static NhlErrorTypes
IrregularPlotInitialize
#if     __STDC__
(
	LayerClass	class,
	Layer		req,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        LayerClass      class;
        Layer           req;
        Layer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*entry_name = "IrregularPlotInitialize";
	IrregularPlotLayer	inew = (IrregularPlotLayer) new;
	IrregularPlotLayerPart	*irp = &(inew->irrplot);



/* Initialize private fields */

	irp->overlay_object = NULL;
	
	
/* Set up the Irregular transformation */

	subret = SetUpTransObj(inew, (IrregularPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&irp->overlay_object,new,req,
			       True,NULL,0,entry_name);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

	return ret;
}

/*
 * Function:	IrregularPlotSetValues
 *
 * Description: 
 *
 * In Args:	old	copy of old instance record
 *		reference	requested instance record
 *		new	new instance record	
 *		args 	list of resources and values for reference
 *		num_args	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes IrregularPlotSetValues
#if  __STDC__
(
	Layer		old,
	Layer		reference,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	Layer		old;
	Layer		reference;
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*entry_name = "IrregularPlotSetValues";
	IrregularPlotLayer	inew = (IrregularPlotLayer) new;
	IrregularPlotLayerPart	*irp = &(inew->irrplot);



/* Set up the Irregular transformation */

	subret = SetUpTransObj(inew, (IrregularPlotLayer) old, False);
	ret = MIN(ret,subret);

/* Manage the overlay */

	subret = _NhlManageOverlay(&irp->overlay_object,new,old,
			       False,NULL,0,entry_name);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

	return ret;
}

/*
 * Function:	IrregularPlotDestroy
 *
 * Description:
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes IrregularPlotDestroy
#if __STDC__
(Layer inst)
#else
(inst)
Layer inst;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
#if 0
	char			*e_text;
	char			*entry_name = "LogLinPlotDestroy";
#endif
	IrregularPlotLayerPart	*irp = &(((IrregularPlotLayer) inst)->irrplot);
	TransformLayerPart	*irtp = &(((TransformLayer) inst)->trans);


	if (irtp->overlay_status == _tfCurrentOverlayMember) {
		subret = NhlRemoveFromOverlay(
				irtp->overlay_object->base.parent->base.id,
					      inst->base.id,False);
		if ((ret = MIN(subret,ret)) < WARNING)
			return FATAL;
	}

	if (irp->overlay_object != NULL) {
		(void) _NhlDestroyChild(irp->overlay_object->base.id,inst);
		irp->overlay_object = NULL;
	}
	if (irtp->trans_obj != NULL) {
		(void) _NhlDestroyChild(irtp->trans_obj->base.id,inst);
		irtp->trans_obj = NULL;
	}
	
	return(ret);
}

/*
 * Function:	IrregularPlotDraw
 *
 * Description:	Currently NULL; may be a candidate for removal.
 *
 * In Args:	layer	IrregularPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes IrregularPlotDraw
#if  __STDC__
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	NhlErrorTypes		ret = NOERROR;

	return ret;
}

/*
 * Function:	SetUpTransObjs
 *
 * Description: Sets up the Irregular transformation object for the generic
 *		Irregular plot object. 
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes SetUpTransObj
#if  __STDC__
(
	IrregularPlotLayer	irnew,
	IrregularPlotLayer	irold,
	NhlBoolean	init
)
#else 
(irnew,xold,init)
	IrregularPlotLayer	irnew;
	IrregularPlotLayer	xold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char 			*e_text;
	char			*entry_name;
	IrregularPlotLayerPart	*irp = &(irnew->irrplot);
	IrregularPlotLayerPart	*oirp = &(irold->irrplot);
	TransformLayerPart	*tfp = &(irnew->trans);
	char			buffer[MAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[16];
        int			nargs = 0;


	entry_name = (init) ? 
		"IrregularPlotInitialize" : "IrregularPlotSetValues";
/*
 * The irregular plot actually uses the irregular type 2 transformation
 * object.
 */
	if (init || tfp->trans_obj == NULL) {

		sprintf(buffer,"%s",irnew->base.name);
		strcat(buffer,".Trans");

		subret = _NhlCreateChild(&tmpid,buffer,
					 irregularType2TransObjLayerClass,
					 (Layer) irnew, NULL);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}

		return ret;
	}

	subret = _NhlALSetValuesChild(tfp->trans_obj->base.id,
				      (Layer) irnew,sargs,nargs);
	return MIN(ret,subret);

}
