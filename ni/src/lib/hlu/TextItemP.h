/*
 *      $Id: TextItemP.h,v 1.5 1994-12-16 23:35:23 dbrown Exp $
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

typedef struct _NhlTextItemLayerPart{
	/* user setable resource fields */
	char	*string;
	float	pos_x;
	float	pos_y;
	float	angle;
	NhlJustification just;
	NhlTextDirection direction;
	NhlFont	font;
	int	font_color;
	float	font_height;
	float	font_aspect;
	float 	font_thickness;
	NhlFontQuality	font_quality;
	float	constant_spacing;
	char	func_code;
	float	*x_corners;
	float	*y_corners;
	
	int	perim_on;
	int	perim_color;
	float	perim_thickness;
	int	perim_dash_pattern;
	float	perim_dash_length;
	float	perim_space;
	int	bg_fill_color;

	/* Private fields */

	NhlBoolean	pos_x_set;
	NhlBoolean	pos_y_set;
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
	float	xperim[5];
	float	yperim[5];
}NhlTextItemLayerPart;

typedef struct _NhlTextItemLayerRec{
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTextItemLayerPart text;
}NhlTextItemLayerRec;

typedef struct _NhlTextItemLayerClassPart {
	char *foo;
}NhlTextItemLayerClassPart;

typedef struct _NhlTextItemLayerClassRec{
	NhlBaseLayerClassPart base_class;
	NhlViewLayerClassPart view_class;
	NhlTextItemLayerClassPart text_class;
}NhlTextItemLayerClassRec;

typedef struct _NhlTextItemLayerClassRec *NhlTextItemLayerClass;
typedef struct _NhlTextItemLayerRec	*NhlTextItemLayer;

extern NhlTextItemLayerClassRec NhltextItemLayerClassRec;

#endif  /*_NTextItemP_h*/
