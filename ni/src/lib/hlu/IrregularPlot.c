/*
 *      $Id: IrregularPlot.c,v 1.2 1993-12-22 00:55:52 dbrown Exp $
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

static NhlResource resources[] = {
	{ NhlNtfOverlayPlotBase,NhlCtfOverlayPlotBase,
		NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(IrregularPlotLayerRec,trans.overlay_plot_base),
		NhlTImmediate,(NhlPointer)False},
};


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

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
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
/* handles_overlays 		*/	True,
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
	IrregularPlotLayer	lnew = (IrregularPlotLayer) new;
	IrregularPlotLayerPart	*irp = &(lnew->irrplot);
	TransformLayerPart	*tfp = &(lnew->trans);
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			buffer[MAXFNAMELEN];
	int			tmpid = -1;
	char			*e_text;
	char			*entry_name = "IrregularPlotInitialize";

	
/* Set up the Irregular transformation */

	subret = SetUpTransObj(lnew, (IrregularPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

/* 
 * Set up the overlay if required -- be sure to set the overlay view to be
 * coincident with the plot view.
 */

 	if (tfp->overlay_plot_base == True) {

		strcpy(buffer,lnew->base.name);
		strcat(buffer,".Overlay");
		subret = _NhlCreateChild(&tmpid,buffer,overlayLayerClass,
					 new,
					 NhlNvpXF,lnew->view.x,
					 NhlNvpYF,lnew->view.y,
					 NhlNvpWidthF,lnew->view.width,
					 NhlNvpHeightF,lnew->view.height,
					 NULL);
		ret = MIN(ret,subret);

		if (ret < WARNING ||
		    (tfp->overlay_object = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: overlay creation failure";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return(FATAL);
		}
	}

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
	char			*e_text;
	char			*entry_name = "IrregularPlotSetValues";
	IrregularPlotLayer	lnew = (IrregularPlotLayer) new;
	IrregularPlotLayer	lold = (IrregularPlotLayer) old;
	IrregularPlotLayerPart	*irp = &(lnew->irrplot);
	TransformLayerPart	*tfp = &(lnew->trans);
        NhlSArg			sargs[16];
        int			nargs = 0;

/* 
 * Warn if user tries to modify overlay base status - then revert to
 * status at initialization.
 */
	
	if (lnew->trans.overlay_plot_base != lold->trans.overlay_plot_base) {
		e_text = "%s: Attempt to modify create only resource";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
		ret = MIN(ret,WARNING);
		lnew->trans.overlay_plot_base = lold->trans.overlay_plot_base;
	}

/*
 * If there is an overlay and this is the base plot propagate any view
 * change to the overlay object. Else if this is an overlay plot 
 * set the view to the overlay's view, warning of any attempt to set it
 * to something different. Note that when the Overlay manager calls 
 * SetValues for an overlay plot it always sets the view identical to
 * its own view.
 */

	if (tfp->overlay_object != NULL && tfp->overlay_plot_base) {
		if (lnew->view.x != lold->view.x)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,lnew->view.x);
		if (lnew->view.y != lold->view.y)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,lnew->view.y);
		if (lnew->view.width != lold->view.width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,lnew->view.width);
		if (lnew->view.height != lold->view.height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,lnew->view.height);
		
		subret = _NhlALSetValuesChild(tfp->overlay_object->base.id,
					      new,sargs,nargs);

		if ((ret = MIN(subret, ret)) < WARNING) {
			e_text = "%s: error setting overlay object view";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}
	}
	else if (tfp->overlay_object != NULL) {
		
		ViewLayer	vl = (ViewLayer) (tfp->overlay_object);
		
		if (lnew->view.x != vl->view.x ||
		    lnew->view.y != vl->view.y ||
		    lnew->view.width != vl->view.width ||
		    lnew->view.height != vl->view.height) {

			lnew->view.x = vl->view.x;
			lnew->view.y = vl->view.y;
			lnew->view.width = vl->view.width;
			lnew->view.height = vl->view.height;

			e_text="%s: attempt to set overlay plot view ignored";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret,WARNING);
		}
	}

/* Set up the Irregular transformation */

	subret = SetUpTransObj(lnew, (IrregularPlotLayer) old, False);
	ret = MIN(ret,subret);

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
	IrregularPlotLayerPart	*irp = &(((IrregularPlotLayer) inst)->irrplot);
	TransformLayerPart	*lltp = &(((TransformLayer) inst)->trans);
	NhlErrorTypes		ret = NOERROR;

	if (lltp->overlay_plot_base && lltp->overlay_object != NULL) {
		(void) _NhlDestroyChild(lltp->overlay_object->base.id,inst);
	}
	if (lltp->trans_obj != NULL) {
		(void) _NhlDestroyChild(lltp->trans_obj->base.id,inst);
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
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "IrregularPlotDraw";
	IrregularPlotLayer	ll = (IrregularPlotLayer) layer;
	IrregularPlotLayerPart	*irp = &(ll->irrplot);
	TransformLayerPart	*tfp = &(ll->trans);

	return ret;
}

/*
 * Function:	SetUpTransObjs
 *
 * Description: Sets up the Irregular transformation object for the generic
 *		Irregular plot object. Note that since this trans object
 *		does not require any dynamic memory, the same trans object
 *		persists for the life of the IrregularPlot object.
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
