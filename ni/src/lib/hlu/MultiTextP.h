/*
 *      $Id: MultiTextP.h,v 1.1 1993-04-30 17:23:12 boote Exp $
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

typedef struct _MultiTextLayerPart{

	/* User setable resource fields */

	int			num_strings;
	char			**text_strings;
	NhlMTextOrientatonType	orientation;
	float			const_pos;
	float			*pos_array;

	/* textItem resources that need to be managed */

	float			angle;
	int			font;
	int			just;
	FontQuality		font_quality;
	float			font_height;
	float			font_aspect;
	float			font_thickness;
	float			constant_spacing;
	TextDirection		direction;

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


}MultiTextLayerPart;

typedef struct _MultiTextLayerRec{
	BaseLayerPart	base;
	ViewLayerPart	view;
	MultiTextLayerPart multitext;
}MultiTextLayerRec;

typedef struct _MultiTextLayerClassPart {
	int foo;
}MultiTextLayerClassPart;

typedef struct _MultiTextLayerClassRec{
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	MultiTextLayerClassPart	multitext_class;
}MultiTextLayerClassRec;

extern MultiTextLayerClassRec multiTextLayerClassRec;


#endif  /*_NMultiTextP_h*/
