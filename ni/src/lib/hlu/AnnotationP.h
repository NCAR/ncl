/*
 *      $Id: AnnotationP.h,v 1.1 1994-06-03 19:23:32 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AnnotationP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri May 20 14:22:36 MDT 1994
 *
 *	Description:	Private header file for Annotation class
 */
#ifndef _NAnnotationP_h
#define _NAnnotationP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/Annotation.h>

typedef struct _NhlAnnotationLayerPart{

	/* public resource fields */

	NhlBoolean		on;
	int			plot_id;
	int			zone;
	NhlPosition		side;
	NhlJustification	just;
	float			para_pos;
	float			ortho_pos;

}NhlAnnotationLayerPart;

typedef struct _NhlAnnotationLayerRec{
	NhlObjLayerPart		base;
	NhlAnnotationLayerPart  annotation;
}NhlAnnotationLayerRec;

typedef struct _NhlAnnotationLayerClassPart {
	char *foo;
}NhlAnnotationLayerClassPart;

typedef struct _NhlAnnotationLayerClassRec{
	NhlObjLayerClassPart		base_class;
	NhlAnnotationLayerClassPart	annotation_class;
}NhlAnnotationLayerClassRec;

typedef struct _NhlAnnotationLayerClassRec	*NhlAnnotationLayerClass;
typedef struct _NhlAnnotationLayerRec		*NhlAnnotationLayer;

extern NhlAnnotationLayerClassRec		NhlannotationLayerClassRec;

#endif  /*_NAnnotationP_h*/
