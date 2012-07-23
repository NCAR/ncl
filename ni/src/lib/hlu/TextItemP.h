/*
 *      $Id: TextItemP.h,v 1.11.12.1 2010-03-17 20:47:07 brownrig Exp $
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
	char			*string;
	NhlBoolean		pos_x_set;
	float			pos_x;
	NhlBoolean		pos_y_set;
	float			pos_y;
	NhlBoolean		angle_set;
	float			angle;
	NhlBoolean		just_set;
	NhlJustification	just;
	NhlBoolean		direction_set;
	NhlTextDirection	direction;
	NhlFont			font;
	int			font_color;
	float           font_opacity;
	NhlBoolean		font_height_set;
	float			font_height;
	NhlBoolean		font_aspect_set;
	float			font_aspect;
	NhlBoolean		font_thickness_set;
	float 			font_thickness;
	NhlFontQuality		font_quality;
	NhlBoolean		constant_spacing_set;
	float			constant_spacing;
	char			func_code;
	
	int	perim_on;
	int	perim_color;
	float	perim_thickness;
	int	perim_dash_pattern;
	float	perim_dash_length;
	float	perim_space;
	int	bg_fill_color;

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
	float	xperim[5];
	float	yperim[5];
	NhlBoolean	new_draw_req;	
        NhlTransDat	*trans_dat;	/* segment transform data */
	NhlLayer last_wks;
}NhlTextItemLayerPart;

typedef struct _NhlTextItemLayerRec{
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTextItemLayerPart text;
}NhlTextItemLayerRec;

typedef struct _NhlTextItemClassPart {
	char *foo;
}NhlTextItemClassPart;

typedef struct _NhlTextItemClassRec{
	NhlBaseClassPart base_class;
	NhlViewClassPart view_class;
	NhlTextItemClassPart text_class;
}NhlTextItemClassRec;

typedef struct _NhlTextItemClassRec *NhlTextItemClass;
typedef struct _NhlTextItemLayerRec	*NhlTextItemLayer;

extern NhlTextItemClassRec NhltextItemClassRec;

#endif  /*_NTextItemP_h*/
