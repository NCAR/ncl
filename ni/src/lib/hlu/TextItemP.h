
/*
 *      $Id: TextItemP.h,v 1.1 1993-04-30 17:24:30 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TextItemP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 10 13:11:35 MST 1992
 *
 *	Description:	Private header file for TextItem class
 */
#ifndef _NTextItemP_h
#define _NTextItemP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/TextItem.h>

typedef struct _TextItemLayerPart{
	/* user setable resource fields */
	char	*string;
	float	pos_x;
	float	pos_y;
	float	angle;
	int	just;
	TextDirection direction;
	int	font;
	int	font_color;
	float	font_height;
	float	font_aspect;
	float 	font_thickness;
	FontQuality	font_quality;
	float	constant_spacing;
	char	func_code;
	float	*x_corners;
	float	*y_corners;

	/* Private fields */

	char	*real_string;
	float	cntr;
	float	real_x_pos;
	float	real_y_pos;
	float   real_size;
	float   real_ph_width;
	float   real_ph_height;
	float	heightvecx[2];
	float	heightvecy[2];
	char	dirstr[4];
	int 	qual;
}TextItemLayerPart;

typedef struct _TextItemLayerRec{
	BaseLayerPart	base;
	ViewLayerPart	view;
	TextItemLayerPart text;
}TextItemLayerRec;

typedef struct _TextItemLayerClassPart {
	char *foo;
}TextItemLayerClassPart;

typedef struct _TextItemLayerClassRec{
	BaseLayerClassPart base_class;
	ViewLayerClassPart view_class;
	TextItemLayerClassPart text_class;
}TextItemLayerClassRec;

extern TextItemLayerClassRec textItemLayerClassRec;


#endif  /*_NTextItemP_h*/
