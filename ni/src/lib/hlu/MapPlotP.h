/*
 *      $Id: MapPlotP.h,v 1.8 1994-09-12 21:01:09 dbrown Exp $
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

#define Nhl_mpSTD_VIEW_WIDTH	0.5
#define Nhl_mpSTD_VIEW_HEIGHT	0.5
#define Nhl_mpDEF_DASH_SEGLEN	0.075
#define Nhl_mpDEF_LABEL_HEIGHT  0.01
#define Nhl_mpMAX_FILL_GROUPS	256
#define Nhl_mpMIN_FILL_GROUPS	10
#define Nhl_mpMAPDATAFILE	"NhlMapData"
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
	int		color;
	int		pattern;
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

typedef enum _mpOutlineSet { 
	mpEMPTY = -1,
	mpCO,
	mpPO, 
	mpUS,
	mpPS
} mpOutlineSet;

typedef enum _mpGlobalSetMode {
	mpNONE = 0,
	mpGEO,
	mpIMPLIED_NAT,
	mpNAT
} mpGlobalSetMode;

typedef enum _mpStateSetMode {
	mpNOSET,
	mpIMPLIED_SET,
	mpSET
} mpStateSetMode;

typedef enum _mpOutlineType { 
	mpOcean = 0,
	mpContinent,
	mpLargeIsland,
	mpSmallIsland,
	mpInlandWater,
	mpNational,
	mpUSStateLand,
	mpUSStateWater 
} mpOutlineType;

#define NhlmpOUTLINE_TYPE_COUNT 8

typedef struct _mpOutlineRec {
	mpOutlineType type;
	short id[3];
	short cix[2];
	char *name;
} mpOutlineRec;

typedef struct _mpNameRec {
	short	name_ix;
	char	draw_mode;
	short	cix; 
} mpNameRec;

typedef struct _mpDrawIdRec {
	unsigned char draw_mode;
	unsigned char cix;
} mpDrawIdRec;

typedef enum _mpBGroups {
	mpALLNATIONAL,
	mpALLGEOPHYSICAL,
	mpLAND,
	mpWATER,
	mpINLANDWATER,
	mpOCEANS,
	mpCONTINENTS,
	mpISLANDS,
	mpLARGEISLANDS,
	mpSMALLISLANDS,
	mpALLUSSTATES,
	mpUSSTATESLAND,
	mpUSSTATESWATER 
} mpBGroups;

typedef struct NhlMapPlotLayerPart {

	/* Public resources */

	NhlBoolean	outline_on;
	NhlMapBoundarySets outline_boundaries;
	NhlDrawOrder	outline_order;
	NhlBoolean	fill_on;
        NhlMapBoundarySets fill_boundaries;
	NhlDrawOrder	fill_order;
	int		fill_group_count;
	NhlBoolean	inverse_fill;
	NhlDrawOrder	inverse_fill_order;
	NhlGenArray	fill_area_specs;
	NhlGenArray	mask_area_specs;
	NhlGenArray	outline_specs;

	int		fill_pattern_background;

	NhlGenArray	area_names;
	NhlGenArray	area_types;
	NhlGenArray	area_groups;
	NhlGenArray	fill_area_colors;
	NhlBoolean	direct_fill_area_color;

	NhlAreaGroupPriority group_priority;

	NhlBoolean	use_standard_indexes;
	NhlBoolean	mono_fill_group_color;
	NhlBoolean	mono_fill_group_pattern;
	NhlBoolean	mono_fill_group_scale;

	NhlGenArray	fill_group_colors;
	NhlGenArray	fill_group_patterns;
	NhlGenArray	fill_group_scales;

	NhlmpFillAttrs	fill_default;
	NhlmpFillAttrs	ocean;
	NhlmpFillAttrs	land;
	NhlmpFillAttrs	inland_water;

	NhlmpLineAttrs	geophysical;
	NhlmpLineAttrs	us_state;
	NhlmpLineAttrs	national;
	NhlBoolean	relative_grid_spacing;
	float		grid_spacing;
	NhlMapGridMaskMode grid_mask_mode;
	NhlmpLineAttrs	grid;
	NhlmpLineAttrs	limb;
	NhlmpLineAttrs	perim;
	NhlmpLabelAttrs labels;

	/* Private Fields */

	NhlBoolean	update_req;
	NhlBoolean	new_draw_req;
        NhlTransDat	*predraw_dat;
        NhlTransDat	*draw_dat;
        NhlTransDat	*postdraw_dat;

	NhlLayer	overlay_object;
	NhlGenArray	dash_table;
	int		co_aws_id;
	int		po_aws_id;
        int		us_aws_id;
	mpNameRec	*fill_recs;
	int		fill_rec_alloc;
	int		fill_rec_count;
	mpNameRec	fill_groups[NhlmpOUTLINE_TYPE_COUNT];
	mpGlobalSetMode	global_fill_mode;
	mpStateSetMode	usstates_fill_mode;
	int		usstates_color;
	mpNameRec	*outline_recs;
	int		outline_rec_alloc;
	int		outline_rec_count;
	mpNameRec	outline_groups[NhlmpOUTLINE_TYPE_COUNT];
	mpGlobalSetMode	global_outline_mode;
	mpStateSetMode	usstates_outline_mode;
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
