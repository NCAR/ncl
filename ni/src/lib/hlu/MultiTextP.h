/*
 *      $Id: MultiTextP.h,v 1.2 1994-01-27 21:25:03 boote Exp $
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


}NhlMultiTextLayerPart;

typedef struct _NhlMultiTextLayerRec{
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlMultiTextLayerPart	multitext;
}NhlMultiTextLayerRec;

typedef struct _NhlMultiTextLayerClassPart {
	int foo;
}NhlMultiTextLayerClassPart;

typedef struct _NhlMultiTextLayerClassRec{
	NhlBaseLayerClassPart	base_class;
	NhlViewLayerClassPart	view_class;
	NhlMultiTextLayerClassPart	multitext_class;
}NhlMultiTextLayerClassRec;

typedef struct _NhlMultiTextLayerClassRec *NhlMultiTextLayerClass;
typedef struct _NhlMultiTextLayerRec	*NhlMultiTextLayer;

extern NhlMultiTextLayerClassRec NhlmultiTextLayerClassRec;

#endif  /*_NMultiTextP_h*/
