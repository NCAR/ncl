/*
 *      $Id: Annotation.c,v 1.5 1994-09-23 23:36:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Annotation.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri May 20 14:22:36 MDT 1994
 *
 *	Description:	The annotation object is a wrapper for 
 *		view class objects that need to be managed as plot
 *		annotations by the Overlay object
 */

#include <math.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/AnnotationP.h>

#define Oset(field)     NhlOffset(NhlAnnotationLayerRec,annotation.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNanOn,NhlCanOn,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(on),NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNanPlotId,NhlCanPlotId,NhlTInteger,sizeof(int),
		 Oset(plot_id),NhlTImmediate,_NhlUSET((NhlPointer) -1),0,NULL},
	{NhlNanOverlayBaseId,NhlCanOverlayBaseId,NhlTInteger,sizeof(int),
		 Oset(overlay_base_id),
		 NhlTImmediate,_NhlUSET((NhlPointer) -1),0,NULL},
	{NhlNanResizeNotify,NhlCanResizeNotify,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(resize_notify),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),0,NULL},
	{NhlNanZone,NhlCanZone,NhlTInteger,sizeof(int),
		 Oset(zone),NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNanSide, NhlCanSide,NhlTPosition,sizeof(NhlPosition),
		 Oset(side),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlBOTTOM),0,NULL},
	{NhlNanJust,NhlCanJust,NhlTJustification,
		 sizeof(NhlJustification),Oset(just),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlCENTERCENTER),0,NULL},
	{NhlNanOrthogonalPosF,NhlCanOrthogonalPosF,NhlTFloat,sizeof(float),
		 Oset(ortho_pos),NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNanParallelPosF,NhlCanParallelPosF,NhlTFloat,sizeof(NhlFont),
		 Oset(para_pos),NhlTString,_NhlUSET("0.0"),0,NULL },
	{NhlNanTrackData,NhlCanTrackData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(track_data),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),0,NULL},
	{NhlNanDataXF,NhlCanDataXF,NhlTFloat,sizeof(NhlFont),
		 Oset(data_x),NhlTString,_NhlUSET("0.0"),0,NULL },
	{NhlNanDataYF,NhlCanDataYF,NhlTFloat,sizeof(NhlFont),
		 Oset(data_y),NhlTString,_NhlUSET("0.0"),0,NULL }

/* End-documented-resources */

};

/*
* Base Methods used
*/

static NhlErrorTypes AnnotationSetValues(
#ifdef NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes    AnnotationInitialize(
#ifdef NhlNeedProto
        NhlLayerClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes	AnnotationDestroy(
#ifdef NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes 	AnnotationClassInitialize();

NhlAnnotationLayerClassRec NhlannotationLayerClassRec = {
	{
/* class_name			*/	"Annotation",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlAnnotationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	AnnotationClassInitialize,
/* layer_initialize		*/	AnnotationInitialize,
/* layer_set_values		*/	AnnotationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	AnnotationDestroy
	},
	{
					NULL
	}
};

NhlLayerClass NhlannotationLayerClass = 
			(NhlLayerClass)&NhlannotationLayerClassRec;

/*
 * Function:	nhlfannotationclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlLayerClass
 * Side Effect:	
 */
NhlLayerClass
_NHLCALLF(nhlfannotationclass,NHLFANNOTATIONCLASS)
#if	__STDC__
(
	void
)
#else
()
#endif
{
	return NhlannotationLayerClass;
}


/*
 * Function:	AnnotationInitialize
 *
 * Description:	
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: Plotchar state affected
 */
/*ARGSUSED*/
static NhlErrorTypes
AnnotationInitialize
#if __STDC__
(
	NhlLayerClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
	NhlLayerClass	class;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlAnnotationLayer	annew = (NhlAnnotationLayer) new;
	NhlErrorTypes		ret=NhlNOERROR;
	NhlString		e_text, entry_name = "AnnotationInitialize";

	if (annew->annotation.track_data) {
		if (annew->annotation.zone > 1) {
			e_text = 
			      "%s: zone invalid for tracking data: resetting";
			ret = MIN(NhlWARNING,ret);
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			annew->annotation.zone = 1;
		}
	}
	return ret;
}

/*
 * Function:	AnnotationSetValues
 *
 * Description: 
 *
 * In Args:	Standard SetValues args
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: GKS and plotchar state changes.
 */
/*ARGSUSED*/
static NhlErrorTypes 
AnnotationSetValues
#if __STDC__
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
	NhlAnnotationLayer 	annew = (NhlAnnotationLayer) new;
	NhlErrorTypes 		ret = NhlNOERROR;
	NhlString		e_text, entry_name = "AnnotationSetValues";

	if (annew->annotation.track_data) {
		if (annew->annotation.zone > 1) {
			e_text = 
			      "%s: zone invalid for tracking data: resetting";
			ret = MIN(NhlWARNING,ret);
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			annew->annotation.zone = 1;
		}
	}
	return ret;
}

/*
 * Function:	AnnotationDestroy
 *
 * Description: 
 *
 * In Args:	NhlLayer inst	instance of Annotation
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes
AnnotationDestroy
#if  __STDC__
(
	NhlLayer	layer)
#else
(layer)
	NhlLayer	layer;
#endif
{
	NhlAnnotationLayer 	an = (NhlAnnotationLayer) layer;
	NhlErrorTypes		ret = NhlNOERROR;

	if (an->annotation.overlay_base_id > -1) {
		ret = NhlUnregisterAnnotation(an->annotation.overlay_base_id,
					      an->base.id);
	}
	return ret;
}



/*
 * Function:	AnnotationClassInitialize
 *
 * Description: Just calls StringToQuark to register new types
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    AnnotationClassInitialize
#if  __STDC__
(void)
#else
()
#endif
{
	return(NhlNOERROR);	
}

