/*
 *      $Id: MapPlotP.h,v 1.6 1994-06-24 00:39:38 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapPlotP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	Generic Map Plotting object
 */

#ifndef _NMapPlotP_h
#define _NMapPlotP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/MapPlot.h>

typedef struct _NhlmpLineAttrs {
	NhlBoolean	on;
	NhlBoolean	delay;
	int		color;
	int		dash_pat;
	float		dash_len;
	float		thickness;
} NhlmpLineAttrs;

typedef struct _NhlmpLabelAttrs {
	NhlBoolean		on;
	NhlString		*text;
	NhlString		format;
	NhlBoolean		height_set;
	float			height;
	NhlTextDirection	direction;
	NhlFont			font;
	int			mono_color;
	int			*colors;
	float			aspect;
	float			thickness;
	NhlFontQuality		quality;
	float			cspacing;
	float			angle;
	char			fcode[2];
	int			back_color;
	NhlBoolean		perim_on;
	float			perim_space;
	float			perim_lthick;
	int			perim_lcolor;
	int			gks_bcolor;
	int			gks_plcolor;
	float			real_height;
	float			pheight;
	float			pwidth;
	float			x_pos;
	float			y_pos;
	NhlJustification	just;
} NhlmpLabelAttrs;

typedef struct NhlMapPlotLayerPart {

	/* Public resources */

	NhlBoolean	mono_fill_color;
	NhlBoolean	mono_fill_pattern;
	NhlBoolean	mono_fill_scale;
	NhlGenArray	fill_colors;
	NhlGenArray	fill_patterns;
	NhlGenArray	fill_scales;
	NhlMapOutlineType outline_type;
	NhlBoolean	delay_outline;
	NhlmpLineAttrs	continents;
	NhlmpLineAttrs	us_states;
	NhlmpLineAttrs	countries;
	float		grid_spacing;
	NhlmpLineAttrs	grid;
	NhlmpLineAttrs	limb;
	NhlmpLineAttrs	perim;
	NhlmpLabelAttrs labels;

	/* Private Fields */

	NhlLayer		overlay_object;

} NhlMapPlotLayerPart;

typedef struct _NhlMapPlotLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlMapPlotLayerPart	mapplot;
} NhlMapPlotLayerRec;

typedef struct NhlMapPlotLayerClassPart{
	void *foo;
} NhlMapPlotLayerClassPart;

typedef struct _NhlMapPlotLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlMapPlotLayerClassPart	mapplot_class;
} NhlMapPlotLayerClassRec;

typedef struct _NhlMapPlotLayerClassRec *NhlMapPlotLayerClass;
typedef struct _NhlMapPlotLayerRec *NhlMapPlotLayer;

extern NhlMapPlotLayerClassRec NhlmapPlotLayerClassRec;

#endif  /* _NMapPlotP_h */
