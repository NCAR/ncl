/*
 *      $Id: LogLinPlot.c,v 1.28 2003-09-10 21:29:54 dbrown Exp $
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
#define	Osettr(field)	NhlOffset(NhlLogLinPlotLayerRec,trans.field)
static NhlResource resources[] = {
        
/* Begin-documented-resources */
/* End-documented-resources */

	{ NhlNpmUpdateReq,NhlCpmUpdateReq,NhlTBoolean,sizeof(NhlBoolean),
          Oset(update_req),NhlTImmediate,
          _NhlUSET((NhlPointer) False),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
         Osettr(x_axis_type_set),NhlTImmediate,
         _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTAxisType,sizeof(NhlAxisType),
          Osettr(x_axis_type),NhlTImmediate,
          _NhlUSET((NhlPointer)NhlLINEARAXIS),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
         Osettr(y_axis_type_set),NhlTImmediate,
         _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTAxisType,sizeof(NhlAxisType),
          Osettr(y_axis_type),NhlTImmediate,
          _NhlUSET((NhlPointer)NhlLINEARAXIS),_NhlRES_PRIVATE,NULL},
};
#undef Oset
#undef Osettr

/* base methods */


static NhlErrorTypes LogLinPlotClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes LogLinPlotClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes LogLinPlotInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes LogLinPlotSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes LogLinPlotDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes LogLinPlotGetBB(
#if	NhlNeedProto
        NhlLayer        instance,
        NhlBoundingBox	*thebox
#endif
);


static NhlErrorTypes LogLinPlotDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#if	NhlNeedProto
	NhlLogLinPlotLayer	xnew,
	NhlLogLinPlotLayer	xold,
	NhlBoolean	init
#endif
);


NhlLogLinPlotClassRec NhllogLinPlotClassRec = {
        {
/* class_name			*/      "logLinPlotClass",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlLogLinPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (NhlClass)&NhltransformClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

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
/* get_bb			*/	LogLinPlotGetBB
	},
	{
/* overlay_capability 		*/	_tfOverlayBaseOrMember,
/* data_to_ndc			*/	NhlInheritTransFunc,
/* ndc_to_data			*/	NhlInheritTransFunc,
/* data_polyline		*/	NhlInheritPolyTransFunc,
/* ndc_polyline			*/	NhlInheritPolyTransFunc,
/* data_polygon			*/	NhlInheritPolyTransFunc,
/* ndc_polygon			*/	NhlInheritPolyTransFunc,
/* data_polymarker		*/	NhlInheritPolyTransFunc,
/* ndc_polymarker		*/	NhlInheritPolyTransFunc
	},
	{
/* foo				*/	NULL
	}
};
	
NhlClass NhllogLinPlotClass = (NhlClass)
						&NhllogLinPlotClassRec;

/*
 * Function:	nhlfloglinplotclass
 *
 * Description:	fortran ref to loglin plot class
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
_NHLCALLF(nhlfloglinplotclass,NHLFLOGLINPLOTCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhllogLinPlotClass;
}

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
 * Function:	LogLinPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlLogLinPlotClassPart that cannot be initialized statically.
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
LogLinPlotClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "LogLinPlotClassPartInitialize";

/*
 * Register children objects
 * NOTE: order of registration should be the reverse of the
 * desired 'canonical' order
 */
	subret = _NhlRegisterChildClass(lc,NhlplotManagerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlplotManagerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhllogLinTransObjClass,
					False,True,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhllogLinTransObjClass");
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
        NhlClass   class;
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

	lnew->trans.grid_type = NhltrLOGLIN;

/* Initialize private fields */

	llp->update_req = False;
	llp->trans_change_count = 0;
	llp->overlay_object = NULL;
	
/* Set up the loglin transformation */

	subret = SetUpTransObj(lnew, (NhlLogLinPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&llp->overlay_object,new,req,
				   _NhlCREATE,NULL,0,entry_name);
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
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "LogLinPlotSetValues";
	NhlLogLinPlotLayer	lnew = (NhlLogLinPlotLayer) new;
	NhlLogLinPlotLayerPart	*llp = &(lnew->llplot);
	NhlSArg			sargs[1];
	int			nargs = 0;

	lnew->trans.grid_type = NhltrLOGLIN;

/* Set up the loglin transformation */

	subret = SetUpTransObj(lnew, (NhlLogLinPlotLayer) old, False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	/* 1 arg */

	if (llp->update_req) {
		NhlSetSArg(&sargs[nargs++],NhlNpmUpdateReq,True);
	}
		
	subret = _NhlManageOverlay(&llp->overlay_object,new,old,
			       _NhlSETVALUES,sargs,nargs,entry_name);
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
	NhlLogLinPlotLayerPart	*llp = &(((NhlLogLinPlotLayer) inst)->llplot);
	NhlTransformLayerPart	*lltp = &(((NhlTransformLayer) inst)->trans);

	if (lltp->overlay_status == _tfCurrentOverlayMember) {
		subret = NhlRemoveOverlay(
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
 * Function:    LogLinPlotGetBB
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
static NhlErrorTypes LogLinPlotGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	char			*entry_name  = "LogLinPlotGetBB";
	char			*e_text;
	NhlLogLinPlotLayer	lll = (NhlLogLinPlotLayer) instance;
	NhlTransformLayerPart	*lltp = &(((NhlTransformLayer)lll)->trans);
	NhlViewLayerPart	*llvp = &(((NhlViewLayer) lll)->view);

	if (! _NhlIsTransform(instance)) {
		e_text = "%s: invalid object id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
/*
 * If the LogLinPlot object is a overlay base, return the bounding box
 * of the complete overlay.
 * Otherwise, return its viewport only.
 */
	if (lltp->overlay_status == _tfCurrentOverlayBase) {
		return _NhlGetBB(lltp->overlay_object,thebox);
	}

	_NhlAddBBInfo(llvp->y,llvp->y - llvp->height,
		      llvp->x + llvp->width,llvp->x,thebox);

	return NhlNOERROR;
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
 * Function:	SetUpTransObj
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
#if	NhlNeedProto
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
	int			trans_change_count;
        NhlSArg			sargs[32];
        int			nargs = 0;


	entry_name = (init) ? "LogLinPlotInitialize" : "LogLinPlotSetValues";
/*
 * Since no dynamic memory is involved a LogLin transformation only
 * needs to be created once. It will not be freed until the object
 * is destroyed.
 */
        if (tfp->x_min_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
        if (tfp->x_max_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
        if (tfp->y_min_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
        if (tfp->y_max_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
        if (tfp->x_log_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrXLog,tfp->x_log);
        if (tfp->y_log_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrYLog,tfp->y_log);
        if (tfp->x_reverse_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,tfp->x_reverse);
        if (tfp->y_reverse_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,tfp->y_reverse);
	if (init || tfp->trans_obj == NULL ||
	    tfp->line_interpolation_on != llold->trans.line_interpolation_on)
                NhlSetSArg(&sargs[nargs++],NhlNtrLineInterpolationOn,
			   tfp->line_interpolation_on);
        
	if (init || tfp->trans_obj == NULL) {

		sprintf(buffer,"%s",llnew->base.name);
		strcat(buffer,".Trans");
                
		subret = _NhlALCreateChild(&tmpid,buffer,
                                           NhllogLinTransObjClass,
                                           (NhlLayer)llnew,sargs,nargs);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

	}
        else {
                subret = _NhlALSetValuesChild(tfp->trans_obj->base.id,
                                              (NhlLayer) llnew,sargs,nargs);
        }

	subret = NhlVAGetValues(tfp->trans_obj->base.id,
                                NhlNtrXReverse,&tfp->x_reverse,
                                NhlNtrYReverse,&tfp->y_reverse,
                                NhlNtrXLog,&tfp->x_log,
                                NhlNtrYLog,&tfp->y_log,
                                NhlNtrXMinF,&tfp->x_min,
                                NhlNtrXMaxF,&tfp->x_max,
                                NhlNtrYMinF,&tfp->y_min,
                                NhlNtrYMaxF,&tfp->y_max,
                                NhlNtrDataXStartF,&tfp->data_xstart,
                                NhlNtrDataXEndF,&tfp->data_xend,
                                NhlNtrDataYStartF,&tfp->data_ystart,
                                NhlNtrDataYEndF,&tfp->data_yend,
				NhlNtrChangeCount,&trans_change_count,
				NULL);
	
	if (trans_change_count > llnew->llplot.trans_change_count) {
		llnew->llplot.trans_change_count = trans_change_count;
		llnew->llplot.update_req = True;
	}
        
        tfp->x_reverse_set = tfp->y_reverse_set = False;
        tfp->x_log_set = tfp->y_log_set = False;
        tfp->x_axis_type_set = tfp->y_axis_type_set = False;
        tfp->x_min_set = tfp->y_min_set = False;
        tfp->x_max_set = tfp->y_max_set = False;

	return MIN(ret,subret);

}


