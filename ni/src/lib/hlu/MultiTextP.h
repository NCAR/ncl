/*
 *      $Id: MultiTextP.h,v 1.7 2001-12-05 00:19:04 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MultiTextP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Dec 3 11:36:10 MST 1992
 *
 *	Description:	Private header file for MultiText object.
 */
#ifndef _NMultiTextP_h
#define _NMultiTextP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/MultiText.h>

typedef struct _NhlMultiTextLayerPart{

	/* User setable resource fields */

	int			num_strings;
	char			**text_strings;
	NhlMTextOrientatonType	orientation;
	float			const_pos;
	float			*pos_array;
	float			max_len;	/* read-only */
	NhlBoolean		auto_stride;
	NhlBoolean		keep_end_items;

	/* textItem resources that need to be managed */

	float			angle;
	int			font;
	int			just;
	NhlFontQuality		font_quality;
	float			font_height;
	float			font_aspect;
	float			font_thickness;
	float			constant_spacing;
	NhlTextDirection	direction;
	char			func_code;

	/* Internal private fields */

	int			text_object;
	/*
	 * these fields are saved from the 1st string placement
	 * so changes in the view class can be propogated to the
	 * text object.
	 */
	float			text_x;
	float			text_y;
	float			text_width;
	float			text_height;

	float			max_extent;
	float			*extents;
	NhlBoolean		*do_draw;
}NhlMultiTextLayerPart;

typedef struct _NhlMultiTextLayerRec{
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlMultiTextLayerPart	multitext;
}NhlMultiTextLayerRec;

typedef struct _NhlMultiTextClassPart {
	int foo;
}NhlMultiTextClassPart;

typedef struct _NhlMultiTextClassRec{
	NhlBaseClassPart	base_class;
	NhlViewClassPart	view_class;
	NhlMultiTextClassPart	multitext_class;
}NhlMultiTextClassRec;

typedef struct _NhlMultiTextClassRec *NhlMultiTextClass;
typedef struct _NhlMultiTextLayerRec	*NhlMultiTextLayer;

extern NhlMultiTextClassRec NhlmultiTextClassRec;

#endif  /*_NMultiTextP_h*/
