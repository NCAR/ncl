/*
 *      $Id: MapPlot.c,v 1.4 1994-01-27 21:24:36 boote Exp $
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

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/MapPlotP.h>
#include <ncarg/hlu/LogLinTransObj.h>

#if 0
static NhlResource resources[] = {
	{ NhlNtfOverlayPlotBase,NhlCtfOverlayPlotBase,
		NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlMapPlotLayerRec,trans.overlay_plot_base),
		NhlTImmediate,(NhlPointer)False},
};
#endif

/* base methods */


static NhlErrorTypes MapPlotClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes MapPlotClassPartInitialize(
#ifdef NhlNeedProto
	NhlLayerClass	lc
#endif
);

static NhlErrorTypes MapPlotInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes MapPlotSetValues(
#ifdef NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes MapPlotDestroy(
#ifdef NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes MapPlotDraw(
#ifdef NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init
#endif
);

NhlMapPlotLayerClassRec NhlmapPlotLayerClassRec = {
        {
/* class_name			*/      "MapPlot",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlMapPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (NhlLayerClass)&NhltransformLayerClassRec,

/* layer_resources		*/	NULL,
/* num_resources		*/	0,
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
/* overlay_capability 		*/	_tfOverlayBaseOnly,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL
	},
	{
/* foo				*/	NULL
	}
};
	
NhlLayerClass NhlmapPlotLayerClass = (NhlLayerClass)&NhlmapPlotLayerClassRec;

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

	return NhlNOERROR;
}

/*
 * Function:	MapPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlMapPlotLayerClassPart that cannot be initialized statically.
 *		Calls _NhlRegisterChildClass for the overlay manager object.
 *
 * In Args:	
 *		NhlLayerClass	lc	NhlLayer Class to init
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
	NhlLayerClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "MapPlotClassPartInitialize";

	/*
	 * Register children objects
	 */

	subret = _NhlRegisterChildClass(lc,NhloverlayLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhloverlayLayerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlmapTransObjLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlmapTransObjLayerClass");
		return(NhlFATAL);
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
	NhlLayerClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlLayerClass   class;
        NhlLayer        req;
        NhlLayer        new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapPlotInitialize";
	NhlMapPlotLayer		mpnew = (NhlMapPlotLayer) new;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);
	
/* Initialize private fields */

	mpp->overlay_object = NULL;

/* Set up the Map transformation */

	subret = SetUpTransObj(mpnew, (NhlMapPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&mpp->overlay_object,new,req,
				   True,NULL,0,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

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
	NhlLayer	old,
	NhlLayer	reference,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapPlotSetValues";
	NhlMapPlotLayer		mpnew = (NhlMapPlotLayer) new;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);


/* Set up the Map transformation */

	subret = SetUpTransObj(mpnew, (NhlMapPlotLayer) old, False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&mpp->overlay_object,new,old,
			       False,NULL,0,entry_name);
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
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlMapPlotLayerPart	*mpp = &(((NhlMapPlotLayer) inst)->mapplot);
	NhlTransformLayerPart	*mptp = &(((NhlTransformLayer) inst)->trans);
	NhlErrorTypes		ret = NhlNOERROR;

	if (mpp->overlay_object != NULL) {
		(void) _NhlDestroyChild(mpp->overlay_object->base.id,inst);
		mpp->overlay_object = NULL;
	}
	if (mptp->trans_obj != NULL) {
		(void) _NhlDestroyChild(mptp->trans_obj->base.id,inst);
		mptp->trans_obj = NULL;
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
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "MapPlotDraw";
	NhlMapPlotLayer		mp = (NhlMapPlotLayer) layer;
	NhlTransformLayerPart	*tfp = &(mp->trans);

	subret = _NhlActivateWorkstation(mp->base.wkptr);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	subret = _NhlSetTrans((NhlLayer)tfp->trans_obj,layer);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error setting transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	gset_line_colr_ind((Gint)_NhlGetGksCi(mp->base.wkptr,0));
	gset_linewidth(1.0);

	c_mapsti("PE",0);
	c_mapdrw();

	subret = _NhlDeactivateWorkstation(mp->base.wkptr);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error setting transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
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
/*ARGSUSED*/
static NhlErrorTypes SetUpTransObj
#if  __STDC__
(
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init
)
#else 
(mpnew,mpold,init)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlTransformLayerPart	*tfp = &(mpnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
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

		subret = _NhlCreateChild(&tmpid,buffer,NhlmapTransObjLayerClass,
					 (NhlLayer) mpnew, NULL);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		return ret;
	}

	subret = _NhlALSetValuesChild(tfp->trans_obj->base.id,
				      (NhlLayer) mpnew,sargs,nargs);
	return MIN(ret,subret);

}
