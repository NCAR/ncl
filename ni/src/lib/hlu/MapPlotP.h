/*
 *      $Id: MapPlotP.h,v 1.18 1998-05-22 01:59:10 dbrown Exp $
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
#include <ncarg/hlu/MapTransObjP.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/WorkspaceI.h>

/* private resources */

#define NhlNmpDumpAreaMap		"mpDumpAreaMap"
#define NhlCmpDumpAreaMap		"MpDumpAreaMap"

#define Nhl_mpDEF_DASH_SEGLEN	0.15
#define Nhl_mpDEF_LABEL_HEIGHT  0.008
#define Nhl_mpMAX_AREA_GROUPS	256
#define Nhl_mpMIN_AREA_GROUPS	10
#define mpALLOC_UNIT		128

typedef struct _NhlmpLineAttrs {
	NhlBoolean	on;
	NhlDrawOrder	order;
	int		color;
	int		gks_color;
	int		dash_pat;
	NhlBoolean	dash_seglen_set;
	float		dash_seglen;
	float		thickness;
} NhlmpLineAttrs;

typedef struct _NhlmpFillAttrs {
	NhlBoolean	color_set;
	int		color;
	NhlBoolean	pattern_set;
	int		pattern;
	NhlBoolean	scale_set;
	float		scale;
} NhlmpFillAttrs;

typedef struct _NhlmpLabelAttrs {
	NhlBoolean		on;
	NhlDrawOrder		order;
	NhlBoolean		height_set;
	float			height;
	NhlTextDirection	direction;
	NhlFont			font;
	int			color;
	int			gks_color;
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

typedef enum _mpDrawOp { 
	mpDRAWFILL,
	mpDRAWINVERSEFILL, 
	mpDRAWOUTLINE,
	mpDRAWGRID
} mpDrawOp;

typedef struct NhlMapPlotLayerPart {

	/* Public resources */

	NhlMapShapeMode	shape_mode;
	NhlGenArray	area_names;
	NhlGenArray	area_types;
	NhlGenArray	fixed_groups;
	NhlGenArray	dynamic_groups;
	NhlMapDataBaseVersion database_version;

	NhlBoolean	outline_on;
	NhlDrawOrder	outline_order;
	NhlMapBoundarySets outline_boundaries;
	NhlGenArray	outline_specs;
	NhlmpLineAttrs	geophysical;
	NhlmpLineAttrs	us_state;
	NhlmpLineAttrs	national;

	NhlBoolean	area_masking_on;
	NhlGenArray	mask_area_specs;

	NhlBoolean	fill_on;
	NhlDrawOrder	fill_order;
	int		fill_pattern_background;
        NhlMapBoundarySets fill_boundaries;

	NhlGenArray	fill_area_specs;
	NhlSpecifiedFillPriority spec_fill_priority;
	NhlBoolean	spec_fill_direct;
	NhlGenArray	spec_fill_colors;
	NhlGenArray	spec_fill_patterns;
	NhlGenArray	spec_fill_scales;

	int		area_group_count;
	NhlBoolean	mono_fill_color;
	NhlColorIndex	fill_color;
	NhlGenArray	fill_colors;
	NhlBoolean	mono_fill_pattern;
	NhlFillIndex	fill_pattern;
	NhlGenArray	fill_patterns;
	NhlBoolean	mono_fill_scale;
	float		fill_scale;
	NhlGenArray	fill_scales;

	NhlmpFillAttrs	fill_default;
	NhlmpFillAttrs	ocean;
	NhlmpFillAttrs	land;
	NhlmpFillAttrs	inland_water;

	NhlBoolean	relative_grid_spacing;
	float		grid_spacing;
	NhlMapGridMaskMode grid_mask_mode;
	NhlmpLineAttrs	grid;
	NhlmpLineAttrs	limb;
	NhlmpLineAttrs	perim;
	NhlmpLabelAttrs labels;

        /* Private resources */
        
	NhlBoolean	dump_area_map;

	/* Private Fields */

	NhlBoolean	update_req;
	NhlBoolean	new_draw_req;
        NhlTransDat	*predraw_dat;
        NhlTransDat	*draw_dat;
        NhlTransDat	*postdraw_dat;

	NhlLayer	overlay_object;
	NhlGenArray	dash_table;
	int		spec_fill_color_count;
	int		spec_fill_pattern_count;
	int		spec_fill_scale_count;
	int		trans_change_count;
        NhlLayer	map_data_handler;
        NhlBoolean	init_draw;
        NhlBoolean	view_changed;

} NhlMapPlotLayerPart;

typedef struct _NhlMapPlotLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlMapPlotLayerPart	mapplot;
} NhlMapPlotLayerRec;

typedef struct NhlMapPlotClassPart{
	void *foo;
} NhlMapPlotClassPart;

typedef struct _NhlMapPlotClassRec{
	NhlBaseClassPart		base_class;
	NhlViewClassPart		view_class;
	NhlTransformClassPart	trans_class;
	NhlMapPlotClassPart	mapplot_class;
} NhlMapPlotClassRec;

typedef struct _NhlMapPlotClassRec *NhlMapPlotClass;
typedef struct _NhlMapPlotLayerRec *NhlMapPlotLayer;

extern NhlMapPlotClassRec NhlmapPlotClassRec;

#endif  /* _NMapPlotP_h */
