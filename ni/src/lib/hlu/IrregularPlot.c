/*
 *      $Id: IrregularPlot.c,v 1.16 1995-05-03 03:11:09 dbrown Exp $
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


#define	Oset(field)	NhlOffset(NhlIrregularPlotLayerRec,irrplot.field)
static NhlResource resources[] = {

	{ NhlNpmUpdateReq,NhlCpmUpdateReq,NhlTBoolean,sizeof(NhlBoolean),
		  Oset(update_req),
		  NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL}
};
#undef Oset

/* base methods */


static NhlErrorTypes IrregularPlotClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes IrregularPlotClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes IrregularPlotInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes IrregularPlotSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes IrregularPlotDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes IrregularPlotGetBB(
#if	NhlNeedProto
        NhlLayer        instance,
        NhlBoundingBox	*thebox
#endif
);


static NhlErrorTypes IrregularPlotDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#if	NhlNeedProto
	NhlIrregularPlotLayer	xnew,
	NhlIrregularPlotLayer	xold,
	NhlBoolean	init
#endif
);

NhlIrregularPlotClassRec NhlirregularPlotClassRec = {
        {
/* class_name			*/      "irregularPlotClass",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlIrregularPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (NhlClass)&NhltransformClassRec,

/* layer_resources		*/	resources, /* resources */
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
/* get_bb			*/	IrregularPlotGetBB
	},
	{
/* overlay_capability 		*/	_tfOverlayBaseOrMember,
/* data_to_ndc			*/	NhlInheritTransFunc,
/* ndc_to_data			*/	NhlInheritTransFunc,
/* data_polyline		*/	NhlInheritPolyTransFunc,
/* ndc_polyline			*/	NhlInheritPolyTransFunc
	},
	{
/* foo				*/	NULL
	}
};
	
NhlClass NhlirregularPlotClass = (NhlClass)
					&NhlirregularPlotClassRec;

/*
 * Function:	nhlfirregularplotclass
 *
 * Description:	fortran ref to irregular plot class
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlfirregularplotclass,NHLFIRREGULARPLOTCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlirregularPlotClass;
}

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
#if	NhlNeedProto
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
 * Function:	IrregularPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlIrregularPlotClassPart that cannot be initialized 
 *		statically.
 *		Calls _NhlRegisterChildClass for the overlay manager object.
 *
 * In Args:	
 *		NhlClass	lc	NhlLayer Class to init
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

static NhlErrorTypes
IrregularPlotClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;
	char		*entry_name = "IrregularPlotClassPartInitialize";

/*
 * Register children objects
 */
	subret = _NhlRegisterChildClass(lc,NhlplotManagerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhloverlayClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlirregularTransObjClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlmapTransObjClass");
		return(NhlFATAL);
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
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass      class;
        NhlLayer           req;
        NhlLayer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "IrregularPlotInitialize";
	NhlIrregularPlotLayer	inew = (NhlIrregularPlotLayer) new;
	NhlIrregularPlotLayerPart	*irp = &(inew->irrplot);



/* Initialize private fields */

	irp->update_req = False;
	irp->overlay_object = NULL;
	
	
/* Set up the Irregular transformation */

	subret = SetUpTransObj(inew, (NhlIrregularPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&irp->overlay_object,new,req,
			       True,NULL,0,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
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
#if	NhlNeedProto
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
	NhlErrorTypes			ret = NhlNOERROR, subret = NhlNOERROR;
	char				*entry_name = "IrregularPlotSetValues";
	NhlIrregularPlotLayer		inew = (NhlIrregularPlotLayer) new;
	NhlIrregularPlotLayerPart	*irp = &(inew->irrplot);
	NhlSArg				sargs[1];
	int				nargs = 0;


/* Set up the Irregular transformation */

	subret = SetUpTransObj(inew, (NhlIrregularPlotLayer) old, False);
	ret = MIN(ret,subret);

/* Manage the overlay */

	/* 1 arg */

	if (irp->update_req) {
		NhlSetSArg(&sargs[nargs++],NhlNpmUpdateReq,True);
	}
	subret = _NhlManageOverlay(&irp->overlay_object,new,old,
			       False,sargs,nargs,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

	irp->update_req = False;

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
#if	NhlNeedProto
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
	NhlIrregularPlotLayerPart	*irp =
				&(((NhlIrregularPlotLayer) inst)->irrplot);
	NhlTransformLayerPart	*irtp = &(((NhlTransformLayer) inst)->trans);


	if (irtp->overlay_status == _tfCurrentOverlayMember) {
		subret = NhlRemoveOverlay(
				irtp->overlay_object->base.parent->base.id,
					      inst->base.id,False);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return NhlFATAL;
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
 * Function:    IrregularPlotGetBB
 *
 * Description: 
 *
 * In Args:     instance        the object instance record
 *              thebox          a data structure used to hold bounding box 
 *                              information.
 *
 * Out Args:    NONE
 *
 * Return Values:       Error Conditions
 *
 * Side Effects:        NONE
 */
static NhlErrorTypes IrregularPlotGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	char			*entry_name  = "IrregularPlotGetBB";
	char			*e_text;
	NhlIrregularPlotLayer	irl = (NhlIrregularPlotLayer) instance;
	NhlTransformLayerPart	*irtp = &(((NhlTransformLayer)irl)->trans);
	NhlViewLayerPart	*irvp = &(((NhlViewLayer) irl)->view);

	if (! _NhlIsTransform(instance)) {
		e_text = "%s: invalid object id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
/*
 * If the IrregularPlot object is a overlay base, return the bounding box
 * of the complete overlay.
 * Otherwise, return its viewport only.
 */
	if (irtp->overlay_status == _tfCurrentOverlayBase) {
		return _NhlGetBB(irtp->overlay_object,thebox);
	}

	_NhlAddBBInfo(irvp->y,irvp->y - irvp->height,
		      irvp->x + irvp->width,irvp->x,thebox);

	return NhlNOERROR;
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

/*ARGSUSED*/
static NhlErrorTypes IrregularPlotDraw
#if	NhlNeedProto
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
/*ARGSUSED*/
static NhlErrorTypes SetUpTransObj
#if	NhlNeedProto
(
	NhlIrregularPlotLayer	irnew,
	NhlIrregularPlotLayer	irold,
	NhlBoolean		init
)
#else 
(irnew,irold,init)
	NhlIrregularPlotLayer	irnew;
	NhlIrregularPlotLayer	irold;
	NhlBoolean		init;
#endif
{
	NhlErrorTypes			ret = NhlNOERROR, subret = NhlNOERROR;
	char 				*e_text;
	char				*entry_name;
	NhlTransformLayerPart		*tfp = &(irnew->trans);
	char				buffer[_NhlMAXRESNAMLEN];
	int				tmpid;
        NhlSArg				sargs[16];
        int				nargs = 0;


	entry_name = (init) ? 
		"IrregularPlotInitialize" : "IrregularPlotSetValues";
/*
 * The irregular plot actually uses the irregular type 2 transformation
 * object.
 */
	if (init || tfp->trans_obj == NULL) {

		sprintf(buffer,"%s",irnew->base.name);
		strcat(buffer,".Trans");

		subret = _NhlVACreateChild(&tmpid,buffer,
					 NhlirregularTransObjClass,
					 (NhlLayer) irnew, NULL);

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
				      (NhlLayer) irnew,sargs,nargs);
	return MIN(ret,subret);

}
