/*
 *      $Id: TransformP.h,v 1.13 1995-04-07 10:44:12 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransformP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	Provides generic hooks for plot classes to assign
 *			functions that compute the forward and backward 
 *			data transformations to support point-n-click 
 *			features.
 */

#ifndef _NTRANSFORMP_h
#define _NTRANSFORMP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/PlotManagerI.h>
#include <ncarg/hlu/TransObjI.h>
#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TransformI.h>

#define NhlNtfOverlayObject	".tfOverlayObject"
#define NhlCtfOverlayObject	".TfOverlayObject"
#define NhlNtfOverlayTrans	".tfOverlayTrans"
#define NhlCtfOverlayTrans	".TfOverlayTrans"
#define NhlNtfOverlayStatus	".tfOverlayStatus"
#define NhlCtfOverlayStatus	".TfOverlayStatus"

typedef enum _NhltfOverlayStatus {
	_tfNotInOverlay,
	_tfCurrentOverlayBase,
	_tfCurrentOverlayMember
} NhltfOverlayStatus;

typedef struct NhlTransformLayerPart {

	/* Public resource fields */

	NhlBoolean		overlay_on;

	/* Private resource fields, set only by the overlay manager */

	NhlLayer		overlay_trans_obj;  
	NhlLayer		overlay_object; 
	NhltfOverlayStatus	overlay_status;

	/* 
	 * Private field for members of the Transform class 
	 * All plots supporting overlays must fill in this field
	 */
 
	NhlLayer		trans_obj;

} NhlTransformLayerPart;

typedef struct _NhlTransformLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
} NhlTransformLayerRec;

typedef NhlErrorTypes (*NhlTransFunction)(
#ifdef	NhlNeedFuncProto
        NhlLayer           /* plot */,
        float*          /* x */,
        float*          /* y */,
        int             /* n */,
        float*          /* xout */,
        float*          /* yout */,
        float*          /*xmissing*/,
        float*          /*ymissing*/,
	int *		/*status*/,
	float *		/*out_of_range*/
#endif
);

typedef enum _NhltfOverlayCapability {
	_tfNotOverlayCapable,
	_tfOverlayBaseOnly,
	_tfOverlayMemberOnly,
	_tfOverlayBaseOrMember
} NhltfOverlayCapability;

typedef NhlErrorTypes (*NhlTransPolyFunc)(
#ifdef	NhlNeedFuncProto
        NhlLayer           /* plot */,
        float*          /* x */,
        float*          /* y */,
        int             /* n */,
#endif
);

#define	NhlInheritTransFunc	((NhlTransFunction)_NhlInherit)
#define	NhlInheritPolyTransFunc	((NhlTransPolyFunc)_NhlInherit)

typedef struct NhlTransformClassPart{
	NhltfOverlayCapability	overlay_capability;
	NhlTransFunction	data_to_ndc;
	NhlTransFunction	ndc_to_data;
	NhlTransPolyFunc	data_polyline;
	NhlTransPolyFunc	ndc_polyline;
} NhlTransformClassPart;

typedef struct _NhlTransformClassRec{
	NhlBaseClassPart	base_class;
	NhlViewClassPart	view_class;
	NhlTransformClassPart	trans_class;
} NhlTransformClassRec;


typedef struct _NhlTransformClassRec *NhlTransformClass;
typedef struct _NhlTransformLayerRec *NhlTransformLayer;

extern NhlTransformClassRec NhltransformClassRec;

#endif  /* _NTRANSFORMP_h */
