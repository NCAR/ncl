/*
 *      $Id: AnnoManager.c,v 1.7 1999-03-27 00:44:44 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AnnoManager.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri May 20 14:22:36 MDT 1994
 *
 *	Description:	The AnnoManager object is a wrapper for 
 *		view class objects that need to be managed as plot
 *		annotations by the Overlay object
 */

#include <math.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/AnnoManagerP.h>

#define Oset(field)     NhlOffset(NhlAnnoManagerLayerRec,annomanager.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNamOn,NhlCamOn,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(on),NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNamViewId,NhlCamViewId,NhlTObjId,sizeof(int),
		 Oset(view_id),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlNULLOBJID),_NhlRES_CGONLY,NULL},
	{NhlNamResizeNotify,NhlCamResizeNotify,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(resize_notify),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),0,NULL},
	{NhlNamZone,NhlCamZone,NhlTInteger,sizeof(int),
		 Oset(zone),NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNamSide, NhlCamSide,NhlTPosition,sizeof(NhlPosition),
		 Oset(side),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlBOTTOM),0,NULL},
	{NhlNamJust,NhlCamJust,NhlTJustification,
		 sizeof(NhlJustification),Oset(just),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlCENTERCENTER),0,NULL},
	{NhlNamOrthogonalPosF,NhlCamOrthogonalPosF,NhlTFloat,sizeof(float),
		 Oset(ortho_pos),NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNamParallelPosF,NhlCamParallelPosF,NhlTFloat,sizeof(float),
		 Oset(para_pos),NhlTString,_NhlUSET("0.0"),0,NULL },
	{NhlNamTrackData,NhlCamTrackData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(track_data),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),0,NULL},
	{NhlNamDataXF,NhlCamDataXF,NhlTFloat,sizeof(float),
		 Oset(data_x),NhlTString,_NhlUSET("0.0"),0,NULL },
	{NhlNamDataYF,NhlCamDataYF,NhlTFloat,sizeof(float),
		 Oset(data_y),NhlTString,_NhlUSET("0.0"),0,NULL },

/* End-documented-resources */

	{NhlNamOverlayId,NhlCamOverlayId,NhlTObjId,sizeof(int),
		 Oset(overlay_id),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlNULLOBJID),
         	 _NhlRES_PRIVATE,NULL}

};

/*
* Base Methods used
*/

static NhlErrorTypes AnnoManagerSetValues(
#if	NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes    AnnoManagerInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes	AnnoManagerDestroy(
#if	NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes 	AnnoManagerClassInitialize();

NhlAnnoManagerClassRec NhlannoManagerClassRec = {
	{
/* class_name			*/	"annoManagerClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlAnnoManagerLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	AnnoManagerClassInitialize,
/* layer_initialize		*/	AnnoManagerInitialize,
/* layer_set_values		*/	AnnoManagerSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	AnnoManagerDestroy
	},
	{
					NULL
	}
};

NhlClass NhlannoManagerClass = 
			(NhlClass)&NhlannoManagerClassRec;

/*
 * Function:	nhlfannomanagerclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
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
_NHLCALLF(nhlfannomanagerclass,NHLFANNOMANAGERCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlannoManagerClass;
}


/*
 * Function:	AnnoManagerInitialize
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
AnnoManagerInitialize
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
	NhlClass	class;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlAnnoManagerLayer	annew = (NhlAnnoManagerLayer) new;
	NhlErrorTypes		ret=NhlNOERROR;
	NhlString		e_text, entry_name = "AnnoManagerInitialize";

	if (annew->annomanager.track_data) {
		if (annew->annomanager.zone > 1) {
			e_text = 
			      "%s: zone invalid for tracking data: resetting";
			ret = MIN(NhlWARNING,ret);
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			annew->annomanager.zone = 1;
		}
	}
	return ret;
}

/*
 * Function:	AnnoManagerSetValues
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
AnnoManagerSetValues
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
	NhlAnnoManagerLayer 	annew = (NhlAnnoManagerLayer) new;
	NhlErrorTypes 		ret = NhlNOERROR;
	NhlString		e_text, entry_name = "AnnoManagerSetValues";

	if (annew->annomanager.track_data) {
		if (annew->annomanager.zone > 1) {
			e_text = 
			      "%s: zone invalid for tracking data: resetting";
			ret = MIN(NhlWARNING,ret);
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			annew->annomanager.zone = 1;
		}
	}
	return ret;
}

/*
 * Function:	AnnoManagerDestroy
 *
 * Description: 
 *
 * In Args:	NhlLayer inst	instance of AnnoManager
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes
AnnoManagerDestroy
#if	NhlNeedProto
(
	NhlLayer	layer)
#else
(layer)
	NhlLayer	layer;
#endif
{
	NhlAnnoManagerLayer 	an = (NhlAnnoManagerLayer) layer;
	NhlErrorTypes		ret = NhlNOERROR;

	if (an->annomanager.overlay_id > 0) {
		ret = NhlUnregisterAnnotation(an->annomanager.overlay_id,
					      an->base.id);
	}
	return ret;
}



/*
 * Function:	AnnoManagerClassInitialize
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
static NhlErrorTypes    AnnoManagerClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return(NhlNOERROR);	
}

