/*
 *      $Id: LogLinPlot.c,v 1.4 1994-01-13 21:46:27 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LogLinPlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a generic Log-Linear plot that
 *			can serve as an overlay base plot or be used for
 *			plots that do not fit into any of the other hlu
 *			plot object catagories.
 */

#include <stdio.h>
#include <ncarg/hlu/LogLinPlotP.h>

/* base methods */


static NhlErrorTypes LogLinPlotClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes LogLinPlotClassPartInitialize(
#ifdef NhlNeedProto
	LayerClass	lc
#endif
);

static NhlErrorTypes LogLinPlotInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes LogLinPlotSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes LogLinPlotDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes LogLinPlotDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	LogLinPlotLayer	xnew,
	LogLinPlotLayer	xold,
	NhlBoolean	init
#endif
);


LogLinPlotLayerClassRec logLinPlotLayerClassRec = {
        {
/* class_name			*/      "LogLinPlot",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(LogLinPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (LayerClass)&transformLayerClassRec,

/* layer_resources		*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,

/* class_part_initialize	*/	LogLinPlotClassPartInitialize,
/* class_initialize		*/	LogLinPlotClassInitialize,
/* layer_initialize		*/	LogLinPlotInitialize,
/* layer_set_values		*/	LogLinPlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	LogLinPlotDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      LogLinPlotDraw,

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
	
LayerClass logLinPlotLayerClass = (LayerClass)&logLinPlotLayerClassRec;


/*
 * Function:	LogLinPlotClassInitialize
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
LogLinPlotClassInitialize
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
 * Function:	LogLinPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		LogLinPlotLayerClassPart that cannot be initialized statically.
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
LogLinPlotClassPartInitialize
#if	__STDC__
(
	LayerClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	LayerClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "LogLinPlotClassPartInitialize";

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

	subret = _NhlRegisterChildClass(lc,logLinTransObjLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < WARNING) {
		e_text = "%s: error registering %s";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  "logLinTransObjLayerClass");
		return(FATAL);
	}

	return ret;
}


/*
 * Function:	LogLinPlotInitialize
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
LogLinPlotInitialize
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
	char			*entry_name = "LogLinPlotInitialize";
	LogLinPlotLayer		lnew = (LogLinPlotLayer) new;
	LogLinPlotLayerPart	*llp = &(lnew->llplot);

/* Initialize private fields */

	llp->overlay_object = NULL;
	
/* Set up the loglin transformation */

	subret = SetUpTransObj(lnew, (LogLinPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&llp->overlay_object,new,req,
				   True,NULL,0,entry_name);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

	return ret;
}

/*
 * Function:	LogLinPlotSetValues
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
static NhlErrorTypes LogLinPlotSetValues
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
	char			*entry_name = "LogLinPlotSetValues";
	LogLinPlotLayer		lnew = (LogLinPlotLayer) new;
	LogLinPlotLayerPart	*llp = &(lnew->llplot);


/* Set up the loglin transformation */

	subret = SetUpTransObj(lnew, (LogLinPlotLayer) old, False);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&llp->overlay_object,new,old,
			       False,NULL,0,entry_name);
	ret = MIN(ret,subret);

	return ret;
}

/*
 * Function:	LogLinPlotDestroy
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
static NhlErrorTypes LogLinPlotDestroy
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
	LogLinPlotLayerPart	*llp = &(((LogLinPlotLayer) inst)->llplot);
	TransformLayerPart	*lltp = &(((TransformLayer) inst)->trans);

	if (lltp->overlay_status == _tfCurrentOverlayMember) {
		subret = NhlRemoveFromOverlay(
				lltp->overlay_object->base.parent->base.id,
					      inst->base.id,False);
		if ((ret = MIN(subret,ret)) < WARNING)
			return FATAL;
	}

	if (llp->overlay_object != NULL) {
		(void) _NhlDestroyChild(llp->overlay_object->base.id,inst);
		llp->overlay_object = NULL;
	}
	if (lltp->trans_obj != NULL) {
		(void) _NhlDestroyChild(lltp->trans_obj->base.id,inst);
		lltp->trans_obj = NULL;
	}
	
	return(ret);
}

/*
 * Function:	LogLinPlotDraw
 *
 * Description:	
 *
 * In Args:	layer	LogLinPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes LogLinPlotDraw
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
 * Description: Sets up the LogLinear transformation object for the generic
 *		LogLinear plot object. Note that since this trans object
 *		does not require any dynamic memory, the same trans object
 *		persists for the life of the LogLinPlot object.
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
	LogLinPlotLayer	llnew,
	LogLinPlotLayer	llold,
	NhlBoolean	init
)
#else 
(llnew,llold,init)
	LogLinPlotLayer	llnew;
	LogLinPlotLayer	llold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char 			*e_text;
	char			*entry_name;
	TransformLayerPart	*tfp = &(llnew->trans);
	char			buffer[MAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[16];
        int			nargs = 0;


	entry_name = (init) ? "LogLinPlotInitialize" : "LogLinPlotSetValues";
/*
 * Since no dynamic memory is involved a LogLin transformation only
 * needs to be created once. It will not be freed until the object
 * is destroyed.
 */
	if (init || tfp->trans_obj == NULL) {

		sprintf(buffer,"%s",llnew->base.name);
		strcat(buffer,".Trans");

		subret = _NhlCreateChild(&tmpid,buffer,
					 logLinTransObjLayerClass,
					 (Layer) llnew, NULL);

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
				      (Layer) llnew,sargs,nargs);
	return MIN(ret,subret);

}


