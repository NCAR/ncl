/*
 *      $Id: MapPlot.c,v 1.2 1993-12-22 00:56:09 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapPlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a generic Map plot that
 *			can serve as an overlay base plot or be used for
 *			general map plots that do not use other hlu
 *			plot objects.
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/MapPlotP.h>
#include <ncarg/hlu/LogLinTransObj.h>

static NhlResource resources[] = {
	{ NhlNtfOverlayPlotBase,NhlCtfOverlayPlotBase,
		NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(MapPlotLayerRec,trans.overlay_plot_base),
		NhlTImmediate,(NhlPointer)False},
};

/* base methods */


static NhlErrorTypes MapPlotClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes MapPlotClassPartInitialize(
#ifdef NhlNeedProto
	LayerClass	lc
#endif
);

static NhlErrorTypes MapPlotInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes MapPlotSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes MapPlotDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes MapPlotDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	MapPlotLayer	mpnew,
	MapPlotLayer	mpold,
	NhlBoolean	init
#endif
);

MapPlotLayerClassRec mapPlotLayerClassRec = {
        {
/* class_name			*/      "MapPlot",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(MapPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (LayerClass)&transformLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	MapPlotClassPartInitialize,
/* class_initialize		*/	MapPlotClassInitialize,
/* layer_initialize		*/	MapPlotInitialize,
/* layer_set_values		*/	MapPlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	MapPlotDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      MapPlotDraw,

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
	
LayerClass mapPlotLayerClass = (LayerClass)&mapPlotLayerClassRec;

/*
 * Function:	MapPlotClassInitialize
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
MapPlotClassInitialize
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
 * Function:	MapPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		MapPlotLayerClassPart that cannot be initialized statically.
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
MapPlotClassPartInitialize
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
	char			*entry_name = "MapPlotClassPartInitialize";

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

	subret = _NhlRegisterChildClass(lc,mapTransObjLayerClass,
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
 * Function:	MapPlotInitialize
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
MapPlotInitialize
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
	MapPlotLayer		mpnew = (MapPlotLayer) new;
	MapPlotLayerPart	*mpp = &(mpnew->mapplot);
	TransformLayerPart	*tfp = &(mpnew->trans);
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			buffer[MAXFNAMELEN];
	int			tmpid = -1;
	char			*e_text;
	char			*entry_name = "MapPlotInitialize";
	
/* Set up the Map transformation */

	subret = SetUpTransObj(mpnew, (MapPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

/* 
 * Since the Map plot does not have any annotation objects it only
 * must put data into the first two field of the overlay record, in case
 * the plot is added to an overlay. 
 * Set up the overlay if required -- be sure to set the overlay view to be
 * coincident with the plot view.
 */

 	if (tfp->overlay_plot_base == True) {

		strcpy(buffer,mpnew->base.name);
		strcat(buffer,".Overlay");
		subret = _NhlCreateChild(&tmpid,buffer,overlayLayerClass,
					 new,
					 NhlNvpXF,mpnew->view.x,
					 NhlNvpYF,mpnew->view.y,
					 NhlNvpWidthF,mpnew->view.width,
					 NhlNvpHeightF,mpnew->view.height,
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
 * Function:	MapPlotSetValues
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
static NhlErrorTypes MapPlotSetValues
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
	char			*entry_name = "MapPlotSetValues";
	MapPlotLayer		mpnew = (MapPlotLayer) new;
	MapPlotLayer		mpold = (MapPlotLayer) old;
	MapPlotLayerPart	*mpp = &(mpnew->mapplot);
	TransformLayerPart	*tfp = &(mpnew->trans);
        NhlSArg			sargs[16];
        int			nargs = 0;

/* 
 * Warn if user tries to modify overlay base status - then revert to
 * status at initialization.
 */
	
	if (mpnew->trans.overlay_plot_base != mpold->trans.overlay_plot_base) {
		e_text = "%s: Attempt to modify create only resource";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
		ret = MIN(ret,WARNING);
		mpnew->trans.overlay_plot_base = 
			mpold->trans.overlay_plot_base;
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
		if (mpnew->view.x != mpold->view.x)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,mpnew->view.x);
		if (mpnew->view.y != mpold->view.y)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,mpnew->view.y);
		if (mpnew->view.width != mpold->view.width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,mpnew->view.width);
		if (mpnew->view.height != mpold->view.height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,mpnew->view.height);
		
		subret = _NhlALSetValuesChild(tfp->overlay_object->base.id,
					      new,sargs,nargs);

		if ((ret = MIN(subret, ret)) < WARNING) {
			e_text = "%s: error setting overlay object view";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}
	}
/*
 * It shouldn't be possible for a map plot object to be any part of any
 * overlay other than the base -- but for now leave this code in as
 * a test.
 */
	else if (tfp->overlay_object != NULL) {
		
		ViewLayer	vl = (ViewLayer) (tfp->overlay_object);
		
		if (mpnew->view.x != vl->view.x ||
		    mpnew->view.y != vl->view.y ||
		    mpnew->view.width != vl->view.width ||
		    mpnew->view.height != vl->view.height) {

			mpnew->view.x = vl->view.x;
			mpnew->view.y = vl->view.y;
			mpnew->view.width = vl->view.width;
			mpnew->view.height = vl->view.height;

			e_text="%s: attempt to set overlay plot view ignored";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret,WARNING);
		}
	}

/* Set up the Map transformation */

	subret = SetUpTransObj(mpnew, (MapPlotLayer) old, False);
	ret = MIN(ret,subret);

	return ret;
}

/*
 * Function:	MapPlotDestroy
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
static NhlErrorTypes MapPlotDestroy
#if __STDC__
(Layer inst)
#else
(inst)
Layer inst;
#endif
{
	MapPlotLayerPart	*mpp = &(((MapPlotLayer) inst)->mapplot);
	TransformLayerPart	*tfp = &(((TransformLayer) inst)->trans);
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;

	if (tfp->overlay_plot_base && tfp->overlay_object != NULL) {
		subret =  _NhlDestroyChild(tfp->overlay_object->base.id,inst);
	}
	if (tfp->trans_obj != NULL) {
		subret = _NhlDestroyChild(tfp->trans_obj->base.id,inst);
	}
	
	return(ret);
}

/*
 * Function:	MapPlotDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes MapPlotDraw
#if  __STDC__
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "MapPlotDraw";
	MapPlotLayer		mp = (MapPlotLayer) layer;
	TransformLayerPart	*tfp = &(mp->trans);

	subret = _NhlActivateWorkstation(mp->base.wkptr);

	if ((ret = MIN(subret,ret)) < WARNING) {
		e_text = "%s: Error activating workstation";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}

	subret = _NhlSetTrans((Layer)tfp->trans_obj,layer);

	if ((ret = MIN(subret,ret)) < WARNING) {
		e_text = "%s: Error setting transformation";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}

	c_mapsti("PE",0);
	c_mapdrw();

	subret = _NhlDeactivateWorkstation(mp->base.wkptr);

	if ((ret = MIN(subret,ret)) < WARNING) {
		e_text = "%s: Error setting transformation";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}

	return ret;
}

/*
 * Function:	SetUpTransObjs
 *
 * Description: Sets up the Map transformation object for the generic
 *		Map plot object. Note that since this trans object
 *		does not require any dynamic memory, the same trans object
 *		persists for the life of the MapPlot object.
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
	MapPlotLayer	mpnew,
	MapPlotLayer	mpold,
	NhlBoolean	init
)
#else 
(mpnew,mpold,init)
	MapPlotLayer	mpnew;
	MapPlotLayer	mpold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name;
	MapPlotLayerPart	*mpp = &(mpnew->mapplot);
	MapPlotLayerPart	*ompp = &(mpold->mapplot);
	TransformLayerPart	*tfp = &(mpnew->trans);
	char			buffer[MAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[16];
        int			nargs = 0;


	entry_name = (init) ? "MapPlotInitialize" : "MapPlotSetValues";
/*
 * Since no dynamic memory is involved a Map transformation only
 * needs to be created once. It will not be freed until the object
 * is destroyed. For now all map trans resources are simply passed through.
 */
	if (init || tfp->trans_obj == NULL) {

		sprintf(buffer,"%s",mpnew->base.name);
		strcat(buffer,".Trans");

		subret = _NhlCreateChild(&tmpid,buffer,mapTransObjLayerClass,
					 (Layer) mpnew, NULL);

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
				      (Layer) mpnew,sargs,nargs);
	return MIN(ret,subret);

}
