/*
 *      $Id: Contour.c,v 1.6 1994-01-27 21:21:31 boote Exp $
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
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/IrregularType2TransObj.h>
#include "testdata.h"

static NhlResource resources[] = {
	{ NhlNcnOutOfRangeValF,NhlCcnOutOfRangeValF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.out_of_range_val),
		NhlTString,"1.0E12"},
	{ NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.x_min),
		NhlTString,"0.0"},
	{ NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.x_max),
		NhlTString,"1.0"},
	{ NhlNtrXLog,NhlCtrXLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.x_log),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNtrXReverse,NhlCtrXReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.x_reverse),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.y_min),
		NhlTString,"0.0"},
	{ NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.y_max),
		NhlTString,"1.0"},
	{ NhlNtrYLog,NhlCtrYLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.y_log),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNtrYReverse,NhlCtrYReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.y_reverse),
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
	NhlLayerClass	lc
#endif
);

static NhlErrorTypes ContourInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes ContourSetValues(
#ifdef NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes ContourDestroy(
#ifdef NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes ContourDraw(
#ifdef NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	NhlContourLayer	xnew,
	NhlContourLayer	xold,
	NhlBoolean	init
#endif
);

NhlContourLayerClassRec NhlcontourLayerClassRec = {
        {
/* class_name			*/      "Contour",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlContourLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (NhlLayerClass)
						&NhltransformLayerClassRec,

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
/* overlay_capability 		*/	_tfOverlayBaseOrMember,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL,
	},
	{
/* foo				*/	NULL
	}
};
	
NhlLayerClass NhlcontourLayerClass = (NhlLayerClass)&NhlcontourLayerClassRec;


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

	return NhlNOERROR;
}

/*
 * Function:	ContourClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlContourLayerClassPart that cannot be initialized statically.
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
ContourClassPartInitialize
#if	__STDC__
(
	NhlLayerClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;
	char		*entry_name = "ContourClassPartInitialize";

/*
 * Register children objects
 */
	subret = _NhlRegisterChildClass(lc,NhloverlayLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "overlayNhlLayerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhllogLinTransObjLayerClass,
					False,False,
					NhlNtrXMinF,
					NhlNtrXMaxF,
					NhlNtrXLog,
					NhlNtrXReverse,
					NhlNtrYMinF,
					NhlNtrYMaxF,
					NhlNtrYLog,
					NhlNtrYLog,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhllogLinTransObjLayerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlirregularType2TransObjLayerClass,
					False,False,
					NhlNtrXMinF,
					NhlNtrXMaxF,
					NhlNtrXLog,
					NhlNtrXReverse,
					NhlNtrYMinF,
					NhlNtrYMaxF,
					NhlNtrYLog,
					NhlNtrYLog,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlirregularType2TransObjLayerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlirregularTransObjLayerClass,
					False,False,
					NhlNtrXMinF,
					NhlNtrXMaxF,
					NhlNtrXLog,
					NhlNtrXReverse,
					NhlNtrYMinF,
					NhlNtrYMaxF,
					NhlNtrYLog,
					NhlNtrYLog,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlirregularTransObjLayerClass");
		return(NhlFATAL);
	}

	return ret;
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
	NhlLayerClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlLayerClass      class;
        NhlLayer           req;
        NhlLayer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "ContourInitialize";
	NhlContourLayer		cnew = (NhlContourLayer) new;
	NhlContourLayerPart	*cnp = &(cnew->contour);

/* global temp initialization */

	int i,j;
	float x, y;
	M=25;
	N=25;
	for (i=-N/2;i<=N/2;i++) 
		for (j=-M/2;j<=M/2;j++) {
			x = 8.0 * i;
			y = 8.0 * j;
			*(T+(M*(i+N/2)+j+M/2)) = sqrt((double)(x*x + y*y));
		}
	
/* initialize private members */

	cnp->overlay_object = NULL;

/* Set up the contour object transformation  */

	subret = SetUpTransObj(cnew, (NhlContourLayer) req, True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&cnp->overlay_object,new,req,
			       True,NULL,0,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

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
	char			*entry_name = "ContourSetValues";
	NhlContourLayer		cnew = (NhlContourLayer) new;
	NhlContourLayerPart	*cnp = &(cnew->contour);
	NhlSArg			sargs[16];
	int			nargs = 0;


/* Set up the contour object's transformation */

	subret = SetUpTransObj(cnew, (NhlContourLayer) old, False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;


/* Manage the overlay */

	subret = _NhlManageOverlay(&cnp->overlay_object,new,old,
				   False,sargs,nargs,entry_name);
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
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
#if 0
	char			*e_text;
	char			*entry_name = "ContourDestroy";
#endif
	NhlContourLayerPart	*cnp = &(((NhlContourLayer) inst)->contour);
	NhlTransformLayerPart	*cntp = &(((NhlTransformLayer) inst)->trans);

	if (cntp->overlay_status == _tfCurrentOverlayMember) {
		subret = NhlRemoveFromOverlay(
				cntp->overlay_object->base.parent->base.id,
					      inst->base.id,False);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return NhlFATAL;
	}

	if (cnp->overlay_object != NULL) {
		(void) _NhlDestroyChild(cnp->overlay_object->base.id,inst);
		cnp->overlay_object = NULL;
	}
	if (cntp->trans_obj != NULL) {
		(void) _NhlDestroyChild(cntp->trans_obj->base.id,inst);
		cntp->trans_obj = NULL;
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
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "ContourDraw";
	NhlContourLayer		cl = (NhlContourLayer) layer;
	NhlContourLayerPart	*clp = &(cl->contour);
	NhlTransformLayerPart	*tfp = &(cl->trans);
	NhlLayer		trans_obj;
	float			out_of_range_val;
	float			rwrk[5000];
        int			iwrk[1000];

/*
 * If the plot is an overlay member, use the overlay manager's trans object.
 */
	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    tfp->overlay_trans_obj != NULL) {

		trans_obj = tfp->overlay_trans_obj;
	}
	else {

		trans_obj = tfp->trans_obj;

		subret = _NhlSetTrans(tfp->trans_obj, layer);

		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
	}

	subret = _NhlActivateWorkstation(cl->base.wkptr);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	NhlVAGetValues(trans_obj->base.id, 
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

	gset_line_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_linewidth(1.0);

        c_cpsetr("CIS",5.0);
        c_cpsetr("DPS",.02);
        c_cpseti("LIS",2);
        c_cpseti("SET",0);
        c_cpseti("MAP",3);

        c_cprect(T,M,M,N,rwrk,5000,iwrk,1000);
        c_cpcldr(T,rwrk,iwrk);

        subret = _NhlDeactivateWorkstation(cl->base.wkptr);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error deactivating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
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
	NhlContourLayer	cnnew,
	NhlContourLayer	cnold,
	NhlBoolean	init
)
#else 
(cnnew,cnold,init)
	NhlContourLayer	cnnew;
	NhlContourLayer	cnold;
	NhlBoolean	init;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourLayerPart	*cnp = &(cnnew->contour);
	NhlContourLayerPart	*ocnp = &(cnold->contour);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
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

		subret = _NhlALCreateChild(&tmpid,buffer,
					   NhllogLinTransObjLayerClass,
					   (NhlLayer)cnnew,sargs,nargs);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		return ret;
	}
		
	if (cnp->x_min != ocnp->x_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,cnp->x_min);
	if (cnp->x_max != ocnp->x_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,cnp->x_max);
	if (cnp->y_min != ocnp->y_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,cnp->y_min);
	if (cnp->y_max != ocnp->y_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,cnp->y_max);

	if (cnp->x_reverse != ocnp->x_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,cnp->x_reverse);
	if (cnp->y_reverse != ocnp->y_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,cnp->y_reverse);

	if (cnp->x_log != ocnp->x_log)
		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,cnp->x_log);
	if (cnp->y_log != ocnp->y_log)
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,cnp->y_log);

	subret = _NhlALSetValuesChild(tfp->trans_obj->base.id,
				      (NhlLayer) cnnew,sargs,nargs);
	return MIN(ret,subret);

}
