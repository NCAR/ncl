
/*
 *      $Id: LegendP.h,v 1.1 1993-07-27 18:03:09 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LegendP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	Private header file for Legend class
 */
#ifndef _NLegendP_h
#define _NLegendP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/Legend.h>

typedef struct _LegendLayerPart{
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
}LegendLayerPart;

typedef struct _LegendLayerRec{
	BaseLayerPart	base;
	ViewLayerPart	view;
	LegendLayerPart text;
}LegendLayerRec;

typedef struct _LegendLayerClassPart {
	char *foo;
}LegendLayerClassPart;

typedef struct _LegendLayerClassRec{
	BaseLayerClassPart base_class;
	ViewLayerClassPart view_class;
	LegendLayerClassPart text_class;
}LegendLayerClassRec;

extern LegendLayerClassRec legendLayerClassRec;


#endif  /*_NLegendP_h*/
