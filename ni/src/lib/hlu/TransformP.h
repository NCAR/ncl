
/*
 *      $Id: TransformP.h,v 1.1 1993-04-30 17:25:32 boote Exp $
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

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/Transform.h>

typedef struct TransformLayerPart {
	/* User setable resource fields */
	int foo;
	/* Private Fields */
} TransformLayerPart;

typedef struct _TransformLayerRec {
	BaseLayerPart	base;
	ViewLayerPart	view;
	TransformLayerPart trans;
} TransformLayerRec;

typedef struct TransformLayerClassPart{
	NhlErrorTypes	(*data_to_ndc)();
	NhlErrorTypes	(*ndc_to_data)();
} TransformLayerClassPart;

typedef struct _TransformLayerClassRec{
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TransformLayerClassPart	trans_class;
} TransformLayerClassRec;

extern TransformLayerClassRec transformLayerClassRec;





#endif  /* _NTransformP_h */
