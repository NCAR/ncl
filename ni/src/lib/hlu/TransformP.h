/*
 *      $Id: TransformP.h,v 1.4 1993-12-13 23:35:08 ethan Exp $
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

#ifndef _NTransformP_h
#define _NTransformP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/Transform.h>

typedef struct TransformLayerPart {
	/* User settable resource fields */
	/* none */
	/* Private Fields */
	Layer overlay_trans;
	Layer plot_trans;
	Layer title;
	Layer tic_marks;
	Layer legend;
	Layer labelbar;
} TransformLayerPart;

typedef struct _TransformLayerRec {
	BaseLayerPart	base;
	ViewLayerPart	view;
	TransformLayerPart trans;
} TransformLayerRec;

typedef NhlErrorTypes (*NhlTransFunction)(
#ifdef	NhlNeedFuncProto
        Layer           /* plot */,
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

typedef struct TransformLayerClassPart{
	NhlBoolean	handles_overlays;
	NhlTransFunction data_to_ndc;
	NhlTransFunction ndc_to_data;
	NhlErrorTypes  (*data_polyline)();
	NhlErrorTypes  (*ndc_polyline)();
} TransformLayerClassPart;

typedef struct _TransformLayerClassRec{
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TransformLayerClassPart	trans_class;
} TransformLayerClassRec;

extern TransformLayerClassRec transformLayerClassRec;

#endif  /* _NTransformP_h */
