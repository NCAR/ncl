/*
 *      $Id: Contour.c,v 1.2 1993-12-22 00:55:37 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Contour.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a Contour plot object
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/ContourP.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/IrregularType2TransObj.h>
#include "testdata.h"

static NhlResource resources[] = {
	{ NhlNtfOverlayPlotBase,NhlCtfOverlayPlotBase,
		NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(ContourLayerRec,trans.overlay_plot_base),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNcnOutOfRangeValF,NhlCcnOutOfRangeValF,NhlTFloat,sizeof(float),
		NhlOffset(ContourLayerRec,contour.out_of_range_val),
		NhlTString,"1.0E12"},
	{ NhlNcnXMinF,NhlCcnXMinF,NhlTFloat,sizeof(float),
		NhlOffset(ContourLayerRec,contour.x_min),
		NhlTString,"0.0"},
	{ NhlNcnXMaxF,NhlCcnXMaxF,NhlTFloat,sizeof(float),
		NhlOffset(ContourLayerRec,contour.x_max),
		NhlTString,"1.0"},
	{ NhlNcnXLog,NhlCcnXLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(ContourLayerRec,contour.x_log),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNcnXReverse,NhlCcnXReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(ContourLayerRec,contour.x_reverse),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNcnYMinF,NhlCcnYMinF,NhlTFloat,sizeof(float),
		NhlOffset(ContourLayerRec,contour.y_min),
		NhlTString,"0.0"},
	{ NhlNcnYMaxF,NhlCcnYMaxF,NhlTFloat,sizeof(float),
		NhlOffset(ContourLayerRec,contour.y_max),
		NhlTString,"1.0"},
	{ NhlNcnYLog,NhlCcnYLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(ContourLayerRec,contour.y_log),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNcnYReverse,NhlCcnYReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(ContourLayerRec,contour.y_reverse),
		NhlTImmediate,(NhlPointer)False},
};

/* base methods */


static NhlErrorTypes ContourClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes ContourClassPartInitialize(
#ifdef NhlNeedProto
	LayerClass	lc
#endif
);

static NhlErrorTypes ContourInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes ContourSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes ContourDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes ContourDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	ContourLayer	xnew,
	ContourLayer	xold,
	NhlBoolean	init
#endif
);

ContourLayerClassRec contourLayerClassRec = {
        {
/* class_name			*/      "Contour",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(ContourLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (LayerClass)&transformLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	ContourClassPartInitialize,
/* class_initialize		*/	ContourClassInitialize,
/* layer_initialize		*/	ContourInitialize,
/* layer_set_values		*/	ContourSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ContourDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      ContourDraw,

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
/* ndc_polyline			*/	NULL,
	},
	{
/* foo				*/	NULL
	}
};
	
LayerClass contourLayerClass = (LayerClass)&contourLayerClassRec;


/*
 * Function:	ContourClassInitialize
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
ContourClassInitialize
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
 * Function:	ContourClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		ContourLayerClassPart that cannot be initialized statically.
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
ContourClassPartInitialize
#if	__STDC__
(
	LayerClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	LayerClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes lret = NOERROR;

	/*
	 * Register children objects
	 */
	lret = _NhlRegisterChildClass(lc,overlayLayerClass,False,False,
				      NULL);

	_NhlInitializeLayerClass(logLinTransObjLayerClass);
	_NhlInitializeLayerClass(irregularType2TransObjLayerClass);
	_NhlInitializeLayerClass(irregularTransObjLayerClass);

	return MIN(lret,ret);
}


/*
 * Function:	ContourInitialize
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
ContourInitialize
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
	ContourLayer		cnew = (ContourLayer) new;
	ContourLayerPart	*cnp = &(cnew->contour);
	TransformLayerPart	*tfp = &(cnew->trans);
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			buffer[MAXFNAMELEN];
	int			tmpid = -1;
	char			*e_text;
	char			*entry_name = "ContourInitialize";

/* initialize private members */

	cnp->overlay_object = NULL;

/* Set up the contour object transformation  */

	subret = SetUpTransObj(cnew, (ContourLayer) req, True);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;

/* 
 * Set up the overlay if required -- be sure to set the overlay view to be
 * coincident with the plot view.
 */


	if (tfp->overlay_plot_base == True) {

		strcpy(buffer,cnew->base.name);
		strcat(buffer,".Overlay");
		subret = _NhlCreateChild(&tmpid,buffer,overlayLayerClass,
					 new,
					 NhlNvpXF,cnew->view.x,
					 NhlNvpYF,cnew->view.y,
					 NhlNvpWidthF,cnew->view.width,
					 NhlNvpHeightF,cnew->view.height,
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
 * Function:	ContourSetValues
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
static NhlErrorTypes ContourSetValues
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
	char			*entry_name = "ContourSetValues";
	ContourLayer		cnew = (ContourLayer) new;
	ContourLayer		cold = (ContourLayer) old;
	ContourLayerPart	*cnp = &(cnew->contour);
	TransformLayerPart	*tfp = &(cnew->trans);
        NhlSArg			sargs[16];
        int			nargs = 0;

/* 
 * Warn if user tries to modify overlay base status - then revert to
 * status at initialization.
 */
	
	if (cnew->trans.overlay_plot_base != cold->trans.overlay_plot_base) {
		e_text = "%s: Attempt to modify create only resource";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
		ret = MIN(ret,WARNING);
		cnew->trans.overlay_plot_base = cold->trans.overlay_plot_base;
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
		if (cnew->view.x != cold->view.x)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,cnew->view.x);
		if (cnew->view.y != cold->view.y)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,cnew->view.y);
		if (cnew->view.width != cold->view.width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,cnew->view.width);
		if (cnew->view.height != cold->view.height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,cnew->view.height);
		
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
		
		if (cnew->view.x != vl->view.x ||
		    cnew->view.y != vl->view.y ||
		    cnew->view.width != vl->view.width ||
		    cnew->view.height != vl->view.height) {

			cnew->view.x = vl->view.x;
			cnew->view.y = vl->view.y;
			cnew->view.width = vl->view.width;
			cnew->view.height = vl->view.height;

			e_text="%s: attempt to set overlay plot view ignored";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret,WARNING);
		}
	}

/* Set up the contour object's transformation */

	subret = SetUpTransObj(cnew, (ContourLayer) old, False);
	ret = MIN(ret,subret);

	return ret;
}

/*
 * Function:	ContourDestroy
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
static NhlErrorTypes ContourDestroy
#if __STDC__
(Layer inst)
#else
(inst)
Layer inst;
#endif
{
	ContourLayerPart	*cnp = &(((ContourLayer) inst)->contour);
	TransformLayerPart	*cntp = &(((TransformLayer) inst)->trans);
	NhlErrorTypes		ret = NOERROR;

	if (cnp->overlay_object != NULL) {
		(void) _NhlDestroyChild(cnp->overlay_object->base.id,inst);
	}
	if (cntp->trans_obj != NULL) {
		(void) NhlDestroy(cntp->trans_obj->base.id);
	}
	
	return(ret);
}

/*
 * Function:	ContourDraw
 *
 * Description:	
 *
 * In Args:	layer	Contour instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes ContourDraw
#if  __STDC__
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "ContourDraw";
	ContourLayer		cl = (ContourLayer) layer;
	ContourLayerPart	*clp = &(cl->contour);
	TransformLayerPart	*tfp = &(cl->trans);
	Layer			trans_obj;
	float			out_of_range_val;
	float			rwrk[5000];
        int			iwrk[1000];

/*
 * If the plot is part of an overlay, but not the overlay plot base, use
 * the overlay manager's trans object.
 */
	if (! tfp->overlay_plot_base && tfp->overlay_trans_obj != NULL) {

		trans_obj = tfp->overlay_trans_obj;
	}
	else {

		trans_obj = tfp->trans_obj;

		subret = _NhlSetTrans(tfp->trans_obj, layer);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: error setting transformation";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(ret);
		}
	}

	subret = _NhlActivateWorkstation(cl->base.wkptr);

	if ((ret = MIN(ret,subret)) < WARNING) {
		e_text = "%s: error activating workstation";
		NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
		return(ret);
	}

	NhlGetValues(trans_obj->base.id, 
		     NhlNtrOutOfRangeF, &out_of_range_val,
		     NULL);
        c_cpsetr("ORV",out_of_range_val);

	if (! clp->x_reverse) {
		c_cpsetr("XC1",clp->x_min);
		c_cpsetr("XCM",clp->x_max);
	}
	else {
		c_cpsetr("XC1",clp->x_max);
		c_cpsetr("XCM",clp->x_min);
	}
	if (! clp->y_reverse) {
		c_cpsetr("YC1",clp->y_min);
		c_cpsetr("YCN",clp->y_max);
	}
	else {
		c_cpsetr("YC1",clp->y_max);
		c_cpsetr("YCN",clp->y_min);
	}
        c_cpsetr("CIS",5.0);
        c_cpsetr("DPS",.02);
        c_cpseti("LIS",2);
        c_cpseti("SET",0);
        c_cpseti("MAP",3);

        c_cprect(T,73,73,10,rwrk,5000,iwrk,1000);
	fprintf(stderr, "calling cpcldr\n");
        c_cpcldr(T,rwrk,iwrk);

        subret = _NhlDeactivateWorkstation(cl->base.wkptr);

	if ((ret = MIN(ret,subret)) < WARNING) {
		e_text = "%s: error deactivating workstation";
		NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
		return(ret);
	}

	return ret;
}

/*
 * Function:	SetUpTransObjs
 *
 * Description: Sets up the LogLinear transformation object for the generic
 *		LogLinear plot object. Note that since this trans object
 *		does not require any dynamic memory, the same trans object
 *		persists for the life of the Contour object.
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
	ContourLayer	cnnew,
	ContourLayer	cnold,
	NhlBoolean	init
)
#else 
(cnnew,xold,init)
	ContourLayer	cnnew;
	ContourLayer	xold;
	NhlBoolean	init;
#endif
{
 	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name;
	ContourLayerPart	*cnp = &(cnnew->contour);
	ContourLayerPart	*ocnp = &(cnold->contour);
	TransformLayerPart	*tfp = &(cnnew->trans);
	char			buffer[MAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[16];
        int			nargs = 0;

	entry_name = (init) ? "ContourInitialize" : "ContourSetValues";
/*
 * Since no dynamic memory is involved a LogLin transformation only
 * needs to be created once. It will not be freed until the object
 * is destroyed.
 */
	if (init || tfp->trans_obj == NULL) {

		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,cnp->x_log);
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,cnp->y_log);

		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,cnp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,cnp->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,cnp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,cnp->y_max);

		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,cnp->x_reverse);
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,cnp->y_reverse);

		sprintf(buffer,"%s",cnnew->base.name);
		strcat(buffer,".Trans");

		subret = NhlALCreate(&tmpid,buffer,logLinTransObjLayerClass,
				     cnnew->base.id,sargs,nargs);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}

		return ret;
	}
		
	if(cnp->x_min != ocnp->x_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,cnp->x_min);
	if(cnp->x_max != ocnp->x_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,cnp->x_max);
	if(cnp->y_min != ocnp->y_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,cnp->y_min);
	if(cnp->y_max != ocnp->y_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,cnp->y_max);

	if(cnp->x_reverse != ocnp->x_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,cnp->x_reverse);
	if(cnp->y_reverse != ocnp->y_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,cnp->y_reverse);

	if(cnp->x_log != ocnp->x_log)
		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,cnp->x_log);
	if(cnp->y_log != ocnp->y_log)
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,cnp->y_log);

	return NhlALSetValues(tfp->trans_obj->base.id,sargs,nargs);

}
