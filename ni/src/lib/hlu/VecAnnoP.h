/*
 *      $Id: VecAnnoP.h,v 1.2 1996-01-19 18:06:37 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VecAnnoP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct  9 19:11:27 MDT 1995
 *
 *	Description:	Private header file for VecAnno object.
 */
#ifndef _NVecAnnoP_h
#define _NVecAnnoP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/VecAnno.h>

typedef struct _vabox {
	float x,y,width,height;
} vaBox;

typedef struct _NhlVecAnnoLayerPart{

	/* User settable resource fields */

	NhlBoolean	string1_on;
	NhlString	string1;
	NhlBoolean	string2_on;
	NhlString	string2;
	float		vec_len;
	NhlColorIndex	vec_line_color;
	NhlColorIndex	vec_fill_color;
	float		ah_line_thickness;
	float		ah_angle;
	float		ah_space;
	float		ah_min_offset;
	_NhlvaArrowParams *a_params;
	_NhlvaDrawParams *d_params;

	float		max_len;	/* read-only */

	/* textItem resources that need to be managed */

	float			angle;
	int			font;
	int			just;
	NhlFontQuality		font_quality;
	float			font_height;
	float			font_aspect;
	float			font_thickness;
	NhlColorIndex		font_color;
	char			func_code[2];
	float			constant_spacing;
	NhlTextDirection	direction;
	NhlBoolean		perim_on;
	NhlColorIndex		perim_color;
	float			perim_thickness;
	float			perim_space;
	NhlColorIndex		bg_fill_color;

	/* Internal private fields */

	int			textitem1;
	int			textitem2;
	/*
	 * these fields are saved from the 1st string placement
	 * so changes in the view class can be propogated to the
	 * text object.
	 */
	vaBox			ti1;
	vaBox			ti2;
	vaBox			vec;
	float			vxb;
	float			vyb;
	float			vxe;
	float			vye;


}NhlVecAnnoLayerPart;

typedef struct _NhlVecAnnoLayerRec{
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlVecAnnoLayerPart	vecanno;
}NhlVecAnnoLayerRec;

typedef struct _NhlVecAnnoClassPart {
	int foo;
}NhlVecAnnoClassPart;

typedef struct _NhlVecAnnoClassRec{
	NhlBaseClassPart	base_class;
	NhlViewClassPart	view_class;
	NhlVecAnnoClassPart	vecanno_class;
}NhlVecAnnoClassRec;

typedef struct _NhlVecAnnoClassRec 	*NhlVecAnnoClass;
typedef struct _NhlVecAnnoLayerRec	*NhlVecAnnoLayer;

extern NhlVecAnnoClassRec NhlvecAnnoClassRec;

#endif  /*_NVecAnnoP_h*/
