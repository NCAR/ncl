/*
 *      $Id: TitleP.h,v 1.1 1993-04-30 17:25:07 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TitleP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Nov 18 15:53:22 MST 1992
 *
 *	Description:	
 */
#ifndef  _NTitleP_h
#define _NTitleP_h 

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/TextItem.h>

typedef struct _TitleLayerPart {
/* Publically setable resource fields */
	float	delta;
	char	*main_string;
	int	main_on;
	TitlePositions	main_side;
	TitlePositions	main_position;
	int	main_just;
	int	main_font;
	float 	main_font_height;
	float 	main_font_aspect;
	float 	main_font_thickness;
	float   main_angle;
	TextDirection	main_direction;
	float	main_constant_spacing;
	char 	main_func_code;
	float	main_offset_x;
	float	main_offset_y;
	FontQuality	main_font_quality;
	int	use_main_attributes; /* if set only those TextItem attributes 
					blonging to the main resources will be 
					used for all the titles */
	int 	main_font_color;
	char	*x_axis_string;
	int	x_axis_on;
	TitlePositions	x_axis_side;
	TitlePositions	x_axis_position;
	int	x_axis_just;
	int	x_axis_font;
	float 	x_axis_font_height;
	float	x_axis_font_aspect;
	float	x_axis_font_thickness;
	float	x_axis_angle;
	TextDirection	x_axis_direction;
	float 	x_axis_constant_spacing;
	float	x_axis_offset_x;
	float	x_axis_offset_y;
	char	x_axis_func_code;
	FontQuality	x_axis_font_quality;
	int 	x_axis_font_color;

	char	*y_axis_string;
	int	y_axis_on;
	TitlePositions	y_axis_side;
	TitlePositions	y_axis_position;
	int	y_axis_just;
	int	y_axis_font;
	float	y_axis_font_height;
	float	y_axis_font_aspect;
	float	y_axis_font_thickness;
	float	y_axis_angle;
	TextDirection	y_axis_direction;
	float	y_axis_constant_spacing;
	float	y_axis_offset_x;
	float	y_axis_offset_y;
	char	y_axis_func_code;
	FontQuality	y_axis_font_quality;
	int 	y_axis_font_color;
/* Private internal fields */
	int	main_id;
	int	x_axis_id;
	int	y_axis_id;
	float	main_pos_x;
	float	main_pos_y;
	float	x_axis_pos_x;
	float	x_axis_pos_y;
	float	y_axis_pos_x;
	float	y_axis_pos_y;
}TitleLayerPart;

typedef struct _TitleLayerRec {
	BaseLayerPart	base;
	ViewLayerPart	view;
	TitleLayerPart	title;
}TitleLayerRec;

typedef struct _TitleLayerClassPart {
	void *foo;
}TitleLayerClassPart;

typedef struct _TitleLayerClassRec {
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TitleLayerClassPart	title_class;
}TitleLayerClassRec;

extern TitleLayerClassRec	titleLayerClassRec;
#endif /* _NTitleP_h */
