/*
 *      $Id: LogLinPlot.c,v 1.7 1994-10-07 18:47:59 dbrown Exp $
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

#include <ncarg/hlu/LogLinPlotP.h>


#define	Oset(field)	NhlOffset(NhlLogLinPlotLayerRec,llplot.field)
static NhlResource resources[] = {

	{ NhlNovUpdateReq,NhlCovUpdateReq,NhlTBoolean,sizeof(NhlBoolean),
		  Oset(update_req),
		  NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL}
};
#undef Oset

/* base methods */


static NhlErrorTypes LogLinPlotClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes LogLinPlotClassPartInitialize(
#ifdef NhlNeedProto
	NhlLayerClass	lc
#endif
);

static NhlErrorTypes LogLinPlotInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes LogLinPlotSetValues(
#ifdef NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes LogLinPlotDestroy(
#ifdef NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes LogLinPlotDraw(
#ifdef NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	NhlLogLinPlotLayer	xnew,
	NhlLogLinPlotLayer	xold,
	NhlBoolean	init
#endif
);


NhlLogLinPlotLayerClassRec NhllogLinPlotLayerClassRec = {
        {
/* class_name			*/      "LogLinPlot",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlLogLinPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (NhlLayerClass)&NhltransformLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
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
	
NhlLayerClass NhllogLinPlotLayerClass = (NhlLayerClass)
						&NhllogLinPlotLayerClassRec;


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

	return NhlNOERROR;
}

/*
 * Function:	LogLinPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlLogLinPlotLayerClassPart that cannot be initialized statically.
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
LogLinPlotClassPartInitialize
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
	char			*entry_name = "LogLinPlotClassPartInitialize";

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

	subret = _NhlRegisterChildClass(lc,NhllogLinTransObjLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhllogLinTransObjLayerClass");
		return(NhlFATAL);
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
	char			*entry_name = "LogLinPlotInitialize";
	NhlLogLinPlotLayer		lnew = (NhlLogLinPlotLayer) new;
	NhlLogLinPlotLayerPart	*llp = &(lnew->llplot);

/* Initialize private fields */

	llp->update_req = False;
	llp->overlay_object = NULL;
	
/* Set up the loglin transformation */

	subret = SetUpTransObj(lnew, (NhlLogLinPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&llp->overlay_object,new,req,
				   True,NULL,0,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
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
	char			*entry_name = "LogLinPlotSetValues";
	NhlLogLinPlotLayer	lnew = (NhlLogLinPlotLayer) new;
	NhlLogLinPlotLayerPart	*llp = &(lnew->llplot);
	NhlSArg			sargs[1];
	int			nargs = 0;


/* Set up the loglin transformation */

	subret = SetUpTransObj(lnew, (NhlLogLinPlotLayer) old, False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	/* 1 arg */

	if (llp->update_req) {
		NhlSetSArg(&sargs[nargs++],NhlNovUpdateReq,True);
	}
		
	subret = _NhlManageOverlay(&llp->overlay_object,new,old,
			       False,sargs,nargs,entry_name);
	ret = MIN(ret,subret);

	llp->update_req = False;

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
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
#if 0
	char			*e_text;
	char			*entry_name = "LogLinPlotDestroy";
#endif
	NhlLogLinPlotLayerPart	*llp = &(((NhlLogLinPlotLayer) inst)->llplot);
	NhlTransformLayerPart	*lltp = &(((NhlTransformLayer) inst)->trans);

	if (lltp->overlay_status == _tfCurrentOverlayMember) {
		subret = NhlRemoveFromOverlay(
				lltp->overlay_object->base.parent->base.id,
					      inst->base.id,False);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return NhlFATAL;
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

/*ARGSUSED*/
static NhlErrorTypes LogLinPlotDraw
#if  __STDC__
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;

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
/*ARGSUSED*/
static NhlErrorTypes SetUpTransObj
#if  __STDC__
(
	NhlLogLinPlotLayer	llnew,
	NhlLogLinPlotLayer	llold,
	NhlBoolean		init
)
#else 
(llnew,llold,init)
	NhlLogLinPlotLayer	llnew;
	NhlLogLinPlotLayer	llold;
	NhlBoolean		init;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char 			*e_text;
	char			*entry_name;
	NhlTransformLayerPart	*tfp = &(llnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
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

		subret = _NhlVACreateChild(&tmpid,buffer,
					 NhllogLinTransObjLayerClass,
					 (NhlLayer) llnew, NULL);

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
				      (NhlLayer) llnew,sargs,nargs);
	return MIN(ret,subret);

}


