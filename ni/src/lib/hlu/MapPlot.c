/*
 *      $Id: MapPlot.c,v 1.99.4.1 2008-03-28 20:37:36 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapPlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a generic Map plot that
 *			can serve as an overlay base plot or be used for
 *			general map plots that do not use other hlu
 *			plot objects.
 */

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/MapPlotP.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/MapV40DataHandler.h>
#include <ncarg/hlu/MapV41DataHandler.h>
#include <ncarg/hlu/MapRGDataHandler.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/hluutil.h>
#include <ctype.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <ncarg/c.h>

#ifndef FLT_MAX
#define FLT_MAX			10.0e37
#endif

#define Oset(field)	NhlOffset(NhlMapPlotLayerRec,mapplot.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

/* Map area database resources */

	{NhlNmpShapeMode, NhlCmpShapeMode, NhlTMapShapeMode,
		 sizeof(NhlBoolean),Oset(shape_mode),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlFIXEDASPECTFITBB),0,NULL},
	{NhlNmpAreaNames,NhlCmpAreaNames,
		 NhlTStringGenArray,sizeof(NhlPointer),
		 Oset(area_names),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpAreaTypes,NhlCmpAreaTypes,
         	NhlTIntegerGenArray,sizeof(NhlPointer),Oset(area_types),
         	NhlTImmediate,_NhlUSET((NhlPointer) NULL),
         	_NhlRES_GONLY,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpDynamicAreaGroups,NhlCmpDynamicAreaGroups,
		NhlTIntegerGenArray,sizeof(NhlPointer),
		Oset(dynamic_groups),NhlTImmediate,
		_NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpFixedAreaGroups,NhlCmpFixedAreaGroups,
         	NhlTIntegerGenArray,sizeof(NhlPointer),Oset(fixed_groups),
         	NhlTImmediate,_NhlUSET((NhlPointer) NULL),
         	_NhlRES_GONLY,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpDataBaseVersion,NhlCmpDataBaseVersion,
		NhlTMapDataBaseVersion,sizeof(NhlMapDataBaseVersion),
	 	Oset(database_version),NhlTImmediate, 
	 	_NhlUSET((NhlPointer)NhlDYNAMIC_MAPS),0,NULL},
	{NhlNmpDataSetName,NhlCmpDataSetName,NhlTString,
		 sizeof(NhlString),Oset(data_set_name),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,NULL},
	{NhlNmpDataResolution,NhlCmpDataResolution,
		NhlTMapDataResolution,sizeof(NhlMapDataResolution),
	 	Oset(data_resolution),NhlTImmediate, 
	 	_NhlUSET((NhlPointer)NhlUNSPECIFIEDRESOLUTION),0,NULL},

/* Outline masking resources */

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(outline_masking_on_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpOutlineMaskingOn, NhlCmpOutlineMaskingOn, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(outline_masking_on),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpMaskOutlineSpecifiers,NhlCmpMaskOutlineSpecifiers,NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(mask_outline_specs),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},



/* Outline resources */

	{NhlNmpOutlineOn, NhlCmpOutlineOn, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(outline_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpOutlineDrawOrder,NhlCmpOutlineDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(outline_order),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlPOSTDRAW),0,NULL},
	{NhlNmpOutlineBoundarySets,NhlCmpOutlineBoundarySets,
                 NhlTMapBoundarySets,sizeof(NhlMapBoundarySets),
                 Oset(outline_boundaries),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlGEOPHYSICAL),0,NULL},
	{NhlNmpOutlineSpecifiers,NhlCmpOutlineSpecifiers,NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(outline_specs),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},

/* Area masking resources */

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(area_masking_on_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpAreaMaskingOn, NhlCmpAreaMaskingOn, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(area_masking_on),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpMaskAreaSpecifiers,NhlCmpMaskAreaSpecifiers,NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(mask_area_specs),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},

/* Fill resources */

	{NhlNmpFillOn, NhlCmpFillOn, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(fill_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNmpFillDrawOrder,NhlCmpFillDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(fill_order),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlDRAW),0,NULL},
	{NhlNmpFillPatternBackground,NhlCFillBackgroundColor,
		 NhlTColorIndex,sizeof(NhlColorIndex),
		 Oset(fill_pattern_background),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNmpFillBoundarySets,NhlCmpFillBoundarySets,
                 NhlTMapBoundarySets,sizeof(NhlMapBoundarySets),
                 Oset(fill_boundaries),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlGEOPHYSICAL),0,NULL},

	{NhlNmpFillAreaSpecifiers,NhlCmpFillAreaSpecifiers, 
		 NhlTStringGenArray,sizeof(NhlPointer),Oset(fill_area_specs),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpSpecifiedFillPriority,NhlCmpSpecifiedFillPriority,
		 NhlTSpecifiedFillPriority,
		 sizeof(NhlSpecifiedFillPriority),Oset(spec_fill_priority),
		 NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlPOLITICALPRIORITY),0,NULL},
	{NhlNmpSpecifiedFillDirectIndexing, NhlCmpSpecifiedFillDirectIndexing, 
		 NhlTBoolean,sizeof(NhlBoolean),Oset(spec_fill_direct),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpSpecifiedFillColors,NhlCmpSpecifiedFillColors,
		 NhlTColorIndexFullEnumGenArray,sizeof(NhlPointer),
		 Oset(spec_fill_colors),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpSpecifiedFillPatterns,NhlCmpSpecifiedFillPatterns,
		 NhlTFillIndexFullEnumGenArray,sizeof(NhlPointer),
		 Oset(spec_fill_patterns),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpSpecifiedFillScales,NhlCmpSpecifiedFillScales,
		 NhlTFloatGenArray,sizeof(NhlPointer),
		 Oset(spec_fill_scales),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},

	{NhlNmpAreaGroupCount,NhlCmpAreaGroupCount,NhlTInteger,
		 sizeof(int),Oset(area_group_count),NhlTImmediate,
		 _NhlUSET((NhlPointer) Nhl_mpMIN_AREA_GROUPS),0,NULL},
	{NhlNmpMonoFillColor, NhlCmpMonoFillColor, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNmpFillColor, NhlCFillColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNmpFillColors, NhlCmpFillColors, NhlTColorIndexGenArray,
		 sizeof(NhlPointer),Oset(fill_colors),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
 	{NhlNmpMonoFillPattern, NhlCmpMonoFillPattern, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
 	{NhlNmpFillPattern, NhlCFillPattern, NhlTFillIndex,
		 sizeof(NhlFillIndex),Oset(fill_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlSOLIDFILL),0,NULL},
	{NhlNmpFillPatterns,NhlCmpFillPatterns,NhlTFillIndexGenArray,
		 sizeof(NhlPointer),Oset(fill_patterns),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpMonoFillScale, NhlCmpMonoFillScale, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_scale),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpFillScaleF, NhlCFillScaleF, NhlTFloat,
		 sizeof(float),Oset(fill_scale),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNmpFillScales, NhlCmpFillScales,  NhlTFloatGenArray,
		 sizeof(NhlPointer),Oset(fill_scales),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpFillDotSizeF, NhlCFillDotSizeF, NhlTFloat,
		 sizeof(float),Oset(fill_dot_size),
		 NhlTString,_NhlUSET("0.0"),0,NULL},

/* default area resources */

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(fill_default.color_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpDefaultFillColor,NhlCFillColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(fill_default.color),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(fill_default.pattern_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpDefaultFillPattern,NhlCFillPattern,NhlTFillIndex,
		 sizeof(NhlFillIndex),Oset(fill_default.pattern),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(fill_default.scale_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpDefaultFillScaleF,NhlCFillScaleF,NhlTFloat,
		 sizeof(float),Oset(fill_default.scale),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

/* ocean area resources */


	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ocean.color_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpOceanFillColor,NhlCFillColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(ocean.color),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ocean.pattern_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpOceanFillPattern,NhlCFillPattern,NhlTFillIndex,
		 sizeof(NhlFillIndex),Oset(ocean.pattern),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ocean.scale_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpOceanFillScaleF,NhlCFillScaleF,NhlTFloat,
		 sizeof(float),Oset(ocean.scale),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

/* land area resources */


	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(land.color_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpLandFillColor,NhlCFillColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(land.color),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(land.pattern_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpLandFillPattern,NhlCFillPattern,NhlTFillIndex,
		 sizeof(NhlFillIndex),Oset(land.pattern),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(land.scale_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpLandFillScaleF,NhlCFillScaleF,NhlTFloat,
		 sizeof(float),Oset(land.scale),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

/* inland water area resources */

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(inland_water.color_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpInlandWaterFillColor,NhlCFillColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(inland_water.color),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(inland_water.pattern_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpInlandWaterFillPattern,NhlCFillPattern,
		 NhlTFillIndex,
		 sizeof(NhlFillIndex),Oset(inland_water.pattern),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(inland_water.scale_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpInlandWaterFillScaleF,NhlCFillScaleF,NhlTFloat,
		 sizeof(float),Oset(inland_water.scale),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},


/* Geophysical line resources */

	{NhlNmpGeophysicalLineColor,NhlCLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(geophysical.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpGeophysicalLineDashPattern,NhlCLineDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),Oset(geophysical.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(geophysical.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpGeophysicalLineDashSegLenF,NhlCLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(geophysical.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpGeophysicalLineThicknessF,NhlCLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(geophysical.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* USState line resources */

	{NhlNmpUSStateLineColor,NhlCLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(us_state.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpUSStateLineDashPattern,NhlCLineDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),Oset(us_state.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(us_state.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpUSStateLineDashSegLenF,NhlCLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(us_state.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpUSStateLineThicknessF,NhlCLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(us_state.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(provincial.color_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpProvincialLineColor,NhlCLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(provincial.color),
	         NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(provincial.dash_pat_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpProvincialLineDashPattern,NhlCLineDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),Oset(provincial.dash_pat),
	         NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(provincial.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpProvincialLineDashSegLenF,NhlCLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(provincial.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(provincial.thickness_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpProvincialLineThicknessF,NhlCLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(provincial.thickness),
	         NhlTProcedure, _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

/* County line resources */

	{NhlNmpCountyLineColor,NhlCLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(county.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpCountyLineDashPattern,NhlCLineDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),Oset(county.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(county.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpCountyLineDashSegLenF,NhlCLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(county.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpCountyLineThicknessF,NhlCLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(county.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* National line resources */

	{NhlNmpNationalLineColor,NhlCLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(national.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpNationalLineDashPattern,NhlCLineDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),Oset(national.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(national.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpNationalLineDashSegLenF,NhlCLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(national.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpNationalLineThicknessF,NhlCLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(national.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* grid (and limb) line resources */

	{NhlNmpGridAndLimbOn,NhlCmpGridAndLimbOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(grid.on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpGridAndLimbDrawOrder,NhlCmpGridAndLimbDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(grid.order),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlPOSTDRAW),0,NULL},
        
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(grid_spacing_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpGridSpacingF,NhlCmpGridSpacingF,
		 NhlTFloat,sizeof(float),Oset(grid_spacing),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpGridLatSpacingF,NhlCmpGridLatSpacingF,
		 NhlTFloat,sizeof(float),Oset(grid_lat_spacing),
		 NhlTString, _NhlUSET("15.0"),0,NULL},
	{NhlNmpGridLonSpacingF,NhlCmpGridLonSpacingF,
		 NhlTFloat,sizeof(float),Oset(grid_lon_spacing),
		 NhlTString, _NhlUSET("15.0"),0,NULL},
	{NhlNmpGridMaxLatF,NhlCmpGridMaxLatF,
		 NhlTFloat,sizeof(float),Oset(grid_max_lat),
		 NhlTString, _NhlUSET("90.0"),0,NULL},
	{NhlNmpGridPolarLonSpacingF,NhlCmpGridPolarLonSpacingF,
		 NhlTFloat,sizeof(float),Oset(grid_polar_lon_spacing),
		 NhlTString, _NhlUSET("90.0"),0,NULL},
	{NhlNmpGridMaskMode,NhlCmpGridMaskMode,NhlTMapGridMaskMode,
		 sizeof(NhlMapGridMaskMode),Oset(grid_mask_mode),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlMASKNONE),0,NULL},
	{NhlNmpGridLineColor,NhlCLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(grid.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpGridLineDashPattern,NhlCLineDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),Oset(grid.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(grid.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpGridLineDashSegLenF,NhlCLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(grid.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpGridLineThicknessF,NhlCLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(grid.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNmpLimbLineColor,NhlCLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(limb.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpLimbLineDashPattern,NhlCLineDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),Oset(limb.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(limb.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpLimbLineDashSegLenF,NhlCLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(limb.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpLimbLineThicknessF,NhlCLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(limb.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* map perimeter resources */

	{NhlNmpPerimOn,NhlCEdgesOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(perim.on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNmpPerimDrawOrder,NhlCmpPerimDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(perim.order),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlDRAW),0,NULL},
	{NhlNmpPerimLineColor,NhlCEdgeColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(perim.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpPerimLineDashPattern,NhlCEdgeDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),Oset(perim.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(perim.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNmpPerimLineDashSegLenF,NhlCEdgeDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(perim.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpPerimLineThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(perim.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* Map label resources */

	{NhlNmpLabelsOn,NhlCPlotLabelsOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(labels.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNmpLabelDrawOrder,NhlCmpLabelDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(labels.order),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlPOSTDRAW),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(labels.height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
        {NhlNmpLabelFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),Oset(labels.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNmpLabelFontColor,NhlCFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(labels.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},


/* End-documented-resources */

	{NhlNmpDumpAreaMap, NhlCmpDumpAreaMap,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(dump_area_map),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),
         	 _NhlRES_PRIVATE,NULL},
        
	{"no.res","No.res",NhlTFloat,sizeof(float),
		 NhlOffset(NhlMapPlotLayerRec,trans.x_min),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTFloat,sizeof(float),
		 NhlOffset(NhlMapPlotLayerRec,trans.x_max),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTAxisType,sizeof(NhlAxisType),
		 NhlOffset(NhlMapPlotLayerRec,trans.x_axis_type),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlLINEARAXIS),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlMapPlotLayerRec,trans.x_log),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlMapPlotLayerRec,trans.x_reverse),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTFloat,sizeof(float),
		 NhlOffset(NhlMapPlotLayerRec,trans.y_min),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTFloat,sizeof(float),
		 NhlOffset(NhlMapPlotLayerRec,trans.y_max),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTAxisType,sizeof(NhlAxisType),
		 NhlOffset(NhlMapPlotLayerRec,trans.y_axis_type),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlLINEARAXIS),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlMapPlotLayerRec,trans.y_log),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlMapPlotLayerRec,trans.y_reverse),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},

/* Intercepted resources */

	{ NhlNtrLineInterpolationOn, NhlCtrLineInterpolationOn,NhlTBoolean,sizeof(NhlBoolean),
		  NhlOffset(NhlMapPlotLayerRec,trans.line_interpolation_on),
	  	  NhlTImmediate,
	  	  _NhlUSET((NhlPointer)True),_NhlRES_INTERCEPTED,NULL},
	{NhlNpmTickMarkDisplayMode,NhlCpmTickMarkDisplayMode,
	 NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
	 Oset(display_tickmarks),NhlTImmediate,
	 _NhlUSET((NhlPointer) NhlNEVER),_NhlRES_INTERCEPTED,NULL},
	{ NhlNtmLabelAutoStride, NhlCLabelAutoStride, 
	  NhlTBoolean, sizeof(NhlBoolean),
	  Oset(label_auto_stride),
	  NhlTImmediate,_NhlUSET((NhlPointer)True),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xb_major_length_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
	 Oset(xb_major_length),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xt_major_length_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
	 Oset(xt_major_length),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yl_major_length_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
	 Oset(yl_major_length),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yr_major_length_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYRMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
	 Oset(yr_major_length),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xb_major_outward_length_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtmXBMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, 
		 NhlTFloat,sizeof(float),
	 Oset(xb_major_outward_length),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xt_major_outward_length_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtmXTMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, 
		 NhlTFloat,sizeof(float),
	 Oset(xt_major_outward_length),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yl_major_outward_length_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtmYLMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, 
		 NhlTFloat,sizeof(float),
	 Oset(yl_major_outward_length),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yr_major_outward_length_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtmYRMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, 
		 NhlTFloat,sizeof(float),
	 Oset(yr_major_outward_length),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xb_font_height_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBLabelFontHeightF,NhlCFontHeightF,NhlTFloat,
	 sizeof(float),Oset(xb_font_height),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xt_font_height_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTLabelFontHeightF,NhlCFontHeightF,NhlTFloat,
	 sizeof(float),Oset(xt_font_height),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yl_font_height_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLLabelFontHeightF,NhlCFontHeightF,NhlTFloat,
	 sizeof(float),Oset(yl_font_height),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yr_font_height_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYRLabelFontHeightF,NhlCFontHeightF,NhlTFloat,
	 sizeof(float),Oset(yr_font_height),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xb_on_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBOn,NhlCtmXBOn,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(xb_on),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xt_on_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTOn,NhlCtmXTOn,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(xt_on),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yl_on_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLOn,NhlCtmYLOn,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(yl_on),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yr_on_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYROn,NhlCtmYROn,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(yr_on),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xb_labels_on_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBLabelsOn,NhlCtmXBLabelsOn,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(xb_labels_on),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(xt_labels_on_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTLabelsOn,NhlCtmXTLabelsOn,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(xt_labels_on),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yl_labels_on_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLLabelsOn,NhlCtmYLLabelsOn,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(yl_labels_on),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 Oset(yr_labels_on_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYRLabelsOn,NhlCtmYRLabelsOn,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(yr_labels_on),NhlTProcedure,
	 _NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_INTERCEPTED,NULL},

	{ NhlNtmXBMode,NhlCtmXBMode,NhlTTickMarkMode,sizeof(NhlTickMarkMode),
	  Oset(xb_mode),NhlTImmediate,_NhlUSET((NhlPointer) -1),
	  _NhlRES_PRIVATE,NULL},
	{ NhlNtmXTMode,NhlCtmXTMode,NhlTTickMarkMode,sizeof(NhlTickMarkMode),
	  Oset(xt_mode),NhlTImmediate,_NhlUSET((NhlPointer) -1),
	  _NhlRES_PRIVATE,NULL},
	{ NhlNtmYLMode,NhlCtmYLMode,NhlTTickMarkMode,sizeof(NhlTickMarkMode),
	  Oset(yl_mode),NhlTImmediate,_NhlUSET((NhlPointer) -1),
	  _NhlRES_PRIVATE,NULL},
	{ NhlNtmYRMode,NhlCtmYRMode,NhlTTickMarkMode,sizeof(NhlTickMarkMode),
	  Oset(yr_mode),NhlTImmediate,_NhlUSET((NhlPointer) -1),
	  _NhlRES_PRIVATE,NULL}

};
#undef Oset

/* base methods */


static NhlErrorTypes MapPlotClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes MapPlotClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes MapPlotInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes MapPlotSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes    MapPlotGetValues(
#if	NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
#endif
);

static NhlErrorTypes MapPlotDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes MapPlotGetBB(
#if	NhlNeedProto
        NhlLayer        instance,
        NhlBoundingBox	*thebox
#endif
);


static NhlErrorTypes MapPlotPreDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes MapPlotDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);


static NhlErrorTypes MapPlotPostDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);


static NhlGenArray mpGenArraySubsetCopy(
#if	NhlNeedProto
	NhlGenArray    ga,
        ng_size_t      length
#endif
);

static NhlErrorTypes mpDraw(
#if	NhlNeedProto
        NhlMapPlotLayer	mpl,
	NhlDrawOrder	order,
	NhlString	entry_name
#endif
);

static NhlErrorTypes    mpManageViewDepResources(
#if	NhlNeedProto
	NhlMapPlotLayer mpnew,
	NhlMapPlotLayer	mpold,
        NhlBoolean	init					    
#endif
);

static NhlErrorTypes    SetLineAttrs(
#if	NhlNeedProto
	NhlMapPlotLayer mpnew,
	NhlMapPlotLayer	mpold,
        NhlBoolean	init					    
#endif
);

static NhlErrorTypes ManageTickMarks(
#if	NhlNeedProto
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes    mpManageDynamicArrays(
#if	NhlNeedProto
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    mpManageGenArray(
#if	NhlNeedProto
	NhlGenArray	*ga,
	ng_size_t	count,
	NhlGenArray	copy_ga,
	NrmQuark	type,
	NhlPointer	init_val,
	ng_size_t	*old_count,
	ng_size_t	*init_count,
	NhlBoolean	*need_check,
	NhlBoolean	*changed,				       
	NhlString	resource_name,
	NhlString	entry_name
#endif
);

static void mpAdjustDashSegLen(
#if	NhlNeedProto
	float		*seglen,
	NhlBoolean	init,
	float		new_vpwidth,
	float		old_vpwidth
#endif
);

static NhlErrorTypes    mpAdjustText(
#if	NhlNeedProto
	NhlmpLabelAttrs *lbl_attrs,
	NhlMapPlotLayer	new, 
	NhlMapPlotLayer	old,
	NhlBoolean	init
#endif
);

static NhlErrorTypes mpSetUpTransObj(
#if	NhlNeedProto
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init
#endif
);

static NhlErrorTypes mpSetUpDataHandler(
#if	NhlNeedProto
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);


static void   load_hlumap_routines(
#if	NhlNeedProto
	NhlBoolean	flag
#endif
);

extern void   (_NHLCALLF(hlumapusr,HLUMAPUSR))(
#if	NhlNeedProto
	int	*iprt
#endif
);

NhlMapPlotClassRec NhlmapPlotClassRec = {
        {
/* class_name			*/      "mapPlotClass",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlMapPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (NhlClass)&NhltransformClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	MapPlotClassPartInitialize,
/* class_initialize		*/	MapPlotClassInitialize,
/* layer_initialize		*/	MapPlotInitialize,
/* layer_set_values		*/	MapPlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	MapPlotGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	MapPlotDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      MapPlotDraw,

/* layer_pre_draw		*/      MapPlotPreDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      MapPlotPostDraw,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	MapPlotGetBB
	},
	{
/* overlay_capability 		*/	_tfOverlayBaseOnly,
/* data_to_ndc			*/	NhlInheritTransFunc,
/* ndc_to_data			*/	NhlInheritTransFunc,
/* data_polyline		*/	NhlInheritPolyTransFunc,
/* ndc_polyline			*/	NhlInheritPolyTransFunc,
/* data_polygon			*/	NhlInheritPolyTransFunc,
/* ndc_polygon			*/	NhlInheritPolyTransFunc,
/* data_polymarker		*/	NhlInheritPolyTransFunc,
/* ndc_polymarker		*/	NhlInheritPolyTransFunc
	},
	{
/* foo				*/	NULL
	}
};

NhlClass NhlmapPlotClass = (NhlClass)&NhlmapPlotClassRec;

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark Qdatabase_version = NrmNULLQUARK;
static NrmQuark Qfill_area_specs = NrmNULLQUARK;
static NrmQuark Qmask_area_specs = NrmNULLQUARK;
static NrmQuark Qmask_outline_specs = NrmNULLQUARK;
static NrmQuark Qoutline_specs = NrmNULLQUARK;
static NrmQuark Qarea_names = NrmNULLQUARK;
static NrmQuark Qarea_types = NrmNULLQUARK;
static NrmQuark Qdynamic_groups = NrmNULLQUARK;
static NrmQuark Qfixed_groups = NrmNULLQUARK;
static NrmQuark Qdata_set_name = NrmNULLQUARK;
static NrmQuark Qdata_resolution = NrmNULLQUARK;
static NrmQuark Qspec_fill_colors = NrmNULLQUARK;
static NrmQuark Qspec_fill_patterns = NrmNULLQUARK;
static NrmQuark Qspec_fill_scales = NrmNULLQUARK;
static NrmQuark Qfill_colors = NrmNULLQUARK;
static NrmQuark Qfill_patterns = NrmNULLQUARK;
static NrmQuark Qfill_scales = NrmNULLQUARK;

static NhlMapPlotLayerPart *Mpp = NULL;

/*static int Init_Colors[] ={16,10, 8,10,26,22,11,23,13,19,24,25,21,20,18};*/
static int Init_Colors[] ={2139029631,2130706559,2139029504,2130706559,2139062143,2139062016,2147483519,2130739071,
			   2147450624,2130738944,2147450751,2130771839,2147418239,2139062271,2147450879};

/*
 * Function:	nhlfmapplotclass
 *
 * Description:	fortran ref to mapplot class
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlfmapplotclass,NHLFMAPPLOTCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlmapPlotClass;
}

/*
 * Function:	MapPlotClassInitialize
 *
 * Description:
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
MapPlotClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
        _NhlEnumVals   mapboundarysetslist[] = {
        {NhlNOBOUNDARIES,		"NoBoundaries"},
        {NhlGEOPHYSICAL, 		"Geophysical"},
        {NhlNATIONAL,			"National"},
        {NhlUSSTATES,      		"USStates"},
	{NhlGEOPHYSICALANDUSSTATES,	"GeophysicalAndUSStates"},
        {NhlALLBOUNDARIES,    	"AllBoundaries"},
        };

	_NhlEnumVals specifiedfillprioritylist[] =  {
	{NhlGEOPHYSICALPRIORITY,		"GeophysicalPriority"},
	{NhlPOLITICALPRIORITY,		"PoliticalPriority"}
	};

	_NhlEnumVals mapgridmaskmodelist[] =  {
	{NhlMASKNONE,		"MaskNone"},
	{NhlMASKOCEAN,	"MaskOcean"},
	{NhlMASKNOTOCEAN,	"MaskNotOcean"},
	{NhlMASKLAND,		"MaskLand"},
	{NhlMASKNOTLAND,	"MaskNotLand"},
	{NhlMASKFILLAREA,	"MaskFillArea"},
	{NhlMASKMASKAREA,	"MaskMaskArea"}
	};

	_NhlEnumVals mapshapemodelist[] =  {
	{NhlFREEASPECT,		"FreeAspect"},
	{NhlFIXEDASPECTFITBB,		"FixedAspectFitBB"},
	{NhlFIXEDASPECTNOFITBB,	"FixedAspectNoFitBB"}
	};

	_NhlEnumVals mapdatabaseversionlist[] =  {
        {NhlDYNAMIC_MAPS,       "Dynamic"},
	{NhlNCARG4_0,		"LowRes"},
	{NhlNCARG4_0,		"Ncarg4_0"},
	{NhlNCARG4_1,		"MediumRes"},
	{NhlNCARG4_1,		"Ncarg4_1"},
	{NhlRANGS_GSHHS,        "HighRes"},
	{NhlRANGS_GSHHS,        "RANGS"},
	{NhlRANGS_GSHHS,        "RANGS_GSHHS"}
	};

	_NhlEnumVals mapdataresolutionlist[] =  {
		{NhlUNSPECIFIEDRESOLUTION, 	"Unspecified"},
		{NhlUNSPECIFIEDRESOLUTION, 	"UnspecifiedResolution"},
		{NhlFINESTRESOLUTION,		"Finest"},
		{NhlFINESTRESOLUTION,		"FinestResolution"},
		{NhlFINERESOLUTION,		"Fine"},
		{NhlFINERESOLUTION,		"FineResolution"},
		{NhlMEDIUMRESOLUTION,		"Medium"},
		{NhlMEDIUMRESOLUTION,		"MediumResolution"},
		{NhlCOARSERESOLUTION,		"Coarse"},
		{NhlCOARSERESOLUTION,		"CoarseResolution"},
		{NhlCOARSESTRESOLUTION,		"Coarsest"},
		{NhlCOARSESTRESOLUTION,		"CoarsestResolution"}
	};

	load_hlumap_routines(False);

        _NhlRegisterEnumType(NhlmapPlotClass,NhlTMapBoundarySets,
		mapboundarysetslist,NhlNumber(mapboundarysetslist));

        _NhlRegisterEnumType(NhlmapPlotClass,NhlTSpecifiedFillPriority,
		specifiedfillprioritylist,
                             NhlNumber(specifiedfillprioritylist));

        _NhlRegisterEnumType(NhlmapPlotClass,NhlTMapGridMaskMode,
		mapgridmaskmodelist,NhlNumber(mapgridmaskmodelist));

        _NhlRegisterEnumType(NhlmapPlotClass,NhlTMapShapeMode,mapshapemodelist,
		NhlNumber(mapshapemodelist));

        _NhlRegisterEnumType(NhlmapPlotClass,NhlTMapDataBaseVersion,
			     mapdatabaseversionlist,
			     NhlNumber(mapdatabaseversionlist));
        _NhlRegisterEnumType(NhlmapPlotClass,NhlTMapDataResolution,
			     mapdataresolutionlist,
			     NhlNumber(mapdataresolutionlist));

	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qdatabase_version = NrmStringToQuark(NhlNmpDataBaseVersion);
	Qfill_area_specs = NrmStringToQuark(NhlNmpFillAreaSpecifiers);
	Qmask_area_specs = NrmStringToQuark(NhlNmpMaskAreaSpecifiers);
	Qmask_outline_specs = NrmStringToQuark(NhlNmpMaskOutlineSpecifiers);
	Qoutline_specs = NrmStringToQuark(NhlNmpOutlineSpecifiers);
	Qarea_names = NrmStringToQuark(NhlNmpAreaNames);
	Qarea_types = NrmStringToQuark(NhlNmpAreaTypes);
	Qdynamic_groups = NrmStringToQuark(NhlNmpDynamicAreaGroups);
	Qfixed_groups = NrmStringToQuark(NhlNmpFixedAreaGroups);
	Qdata_set_name = NrmStringToQuark(NhlNmpDataSetName);
	Qdata_resolution = NrmStringToQuark(NhlNmpDataResolution);
	Qspec_fill_colors = NrmStringToQuark(NhlNmpSpecifiedFillColors);
	Qspec_fill_patterns = NrmStringToQuark(NhlNmpSpecifiedFillPatterns);
	Qspec_fill_scales = NrmStringToQuark(NhlNmpSpecifiedFillScales);
	Qfill_colors = NrmStringToQuark(NhlNmpFillColors);
	Qfill_patterns = NrmStringToQuark(NhlNmpFillPatterns);
	Qfill_scales = NrmStringToQuark(NhlNmpFillScales);

	return NhlNOERROR;
}

/*
 * Function:	MapPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlMapPlotClassPart that cannot be initialized statically.
 *		Calls _NhlRegisterChildClass for the overlay manager object.
 *
 * In Args:	
 *		NhlClass	lc	NhlLayer Class to init
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

static NhlErrorTypes
MapPlotClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "MapPlotClassPartInitialize";

	/*
	 * Register children objects
	 */

	subret = _NhlRegisterChildClass(lc,NhlplotManagerClass,
					False,False,
					NhlNpmTickMarkDisplayMode,
					NhlNtmEqualizeXYSizes,
					NhlNtmLabelAutoStride,
					NhlNtmXBOn,
					NhlNtmXBLabelsOn,
					NhlNtmXBMode,
					NhlNtmXBValues,
					NhlNtmXBLabels,
					NhlNtmXBLabelFuncCode,
					NhlNtmXBLabelFontHeightF,
					NhlNtmXBMajorLengthF,
					NhlNtmXBMajorOutwardLengthF,
					NhlNtmXTOn,
					NhlNtmXTLabelsOn,
					NhlNtmXTMode,
					NhlNtmXTValues,
					NhlNtmXTLabels,
					NhlNtmXTLabelFuncCode,
					NhlNtmXTLabelFontHeightF,
					NhlNtmXTMajorLengthF,
					NhlNtmXTMajorOutwardLengthF,
					NhlNtmYLOn,
					NhlNtmYLLabelsOn,
					NhlNtmYLMode,
					NhlNtmYLValues,
					NhlNtmYLLabels,
					NhlNtmYLLabelFuncCode,
					NhlNtmYLLabelFontHeightF,
					NhlNtmYLMajorLengthF,
					NhlNtmYLMajorOutwardLengthF,
					NhlNtmYROn,
					NhlNtmYRLabelsOn,
					NhlNtmYRMode,
					NhlNtmYRValues,
					NhlNtmYRLabels,
					NhlNtmYRLabelFuncCode,
					NhlNtmYRLabelFontHeightF,
					NhlNtmYRMajorLengthF,
					NhlNtmYRMajorOutwardLengthF,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhloverlayClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlmapTransObjClass,
					False,False,
					NhlNmpPreserveAspectRatio,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlmapTransObjClass");
		return(NhlFATAL);
	}

	return ret;
}

/*
 * Function:	MapPlotInitialize
 *
 * Description: 
 *
 * In Args: 	class	objects layer_class
 *		req	instance record of requested values
 *		new	instance record of new object
 *		args	list of resources and values for reference
 *		num_args 	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes
MapPlotInitialize
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass   class;
        NhlLayer        req;
        NhlLayer        new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapPlotInitialize";
	char			*e_text;
        NhlMapPlotLayer		mpl = (NhlMapPlotLayer) new;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
        NhlSArg			sargs[64];
        int			nargs = 0;

#if 0
	mpl->view.keep_aspect = True;
#endif

	mpl->trans.grid_type = NhltrMAP;

/* Initialize private fields */

	mpp->overlay_object = NULL;
        mpp->map_data_handler = NULL;
	mpp->dash_table = NULL;
	mpp->predraw_dat = NULL;
	mpp->draw_dat = NULL;
	mpp->postdraw_dat = NULL;
	mpp->new_draw_req = True;
	mpp->update_req = False;
	mpp->limb.on = mpp->grid.on;
	mpp->limb.order = mpp->grid.order;
	mpp->spec_fill_color_count = 0;
	mpp->spec_fill_pattern_count = 0;
	mpp->spec_fill_scale_count = 0;
	mpp->trans_change_count = 0;
        mpp->view_changed = True;
        if (! mpp->area_masking_on_set)
                mpp->area_masking_on = mpp->mask_area_specs ? True : False;
        if (! mpp->outline_masking_on_set)
                mpp->outline_masking_on = mpp->mask_outline_specs ? True : False;
        
        if (mpp->grid_spacing_set)
                mpp->grid_lat_spacing = mpp->grid_lon_spacing =
                        mpp->grid_spacing;
        else
                mpp->grid_spacing = 15.0;
        if (mpp->grid_lat_spacing <= 0.0) mpp->grid_lat_spacing = 15.0;
        if (mpp->grid_lon_spacing <= 0.0) mpp->grid_lon_spacing = 15.0;
	/* the provincial values override the usstates values if they are set */
	if (mpp->provincial.color_set)
		mpp->us_state.color = mpp->provincial.color;
	if (mpp->provincial.dash_pat_set)
		mpp->us_state.dash_pat = mpp->provincial.dash_pat;
	if (mpp->provincial.dash_seglen_set) {
		mpp->us_state.dash_seglen = mpp->provincial.dash_seglen;
		mpp->us_state.dash_seglen_set = True;
	}
	if (mpp->provincial.thickness_set)
		mpp->us_state.thickness = mpp->provincial.thickness;

/*
 * tickmark modes cannot be set for now; hopefully this will change
 */

	if (mpp->xb_mode != -1 ||
	    mpp->xt_mode != -1 ||
	    mpp->yl_mode != -1 ||
	    mpp->yr_mode != -1) {
		e_text = "%s: tm[XB|XT|YL|YR]Mode resources are not currently enabled for MapPlot tick marks";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		 mpp->xb_mode = mpp->xt_mode =  
			 mpp->yl_mode = mpp->yr_mode = -1;
	}

		
                
/*
 * Necessary to initialize these for NDCToData to work correctly.
 */
	mpl->trans.data_xstart = -180.0;
	mpl->trans.data_xend = 180.0;
	mpl->trans.data_ystart = -90.0;
	mpl->trans.data_yend = 90.0;
        
/* Set up the Map data handler */

        subret = mpSetUpDataHandler(mpl, (NhlMapPlotLayer) req, True, args, num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;
                                    
/* Set up the Map transformation */

	subret = mpSetUpTransObj(mpl, (NhlMapPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Set view dependent resources */

	subret = mpManageViewDepResources(mpl,(NhlMapPlotLayer)req,True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

	subret = SetLineAttrs(mpl,(NhlMapPlotLayer)req,True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the dynamic arrays */

	subret = mpManageDynamicArrays(mpl,(NhlMapPlotLayer)req,
				     True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

        _NhlUpdateDrawList(mpp->map_data_handler,True,
                           mpl,(NhlMapPlotLayer)req,args,num_args);
        
                
/* Manage TickMarks object */

	/* 42 arguments possible */
	subret = ManageTickMarks(mpl,NULL,True,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing TickMarks";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the overlay */

	subret = _NhlManageOverlay(&mpp->overlay_object,new,req,
				   _NhlCREATE,sargs,nargs,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

	if (mpp->overlay_object &&  mpp->display_tickmarks > NhlNOCREATE) {
		NhlVAGetValues(mpp->overlay_object->base.id,
			       NhlNtmXBMajorLengthF,&mpp->xb_major_length,
			       NhlNtmXTMajorLengthF,&mpp->xt_major_length,
			       NhlNtmYLMajorLengthF,&mpp->yl_major_length,
			       NhlNtmYRMajorLengthF,&mpp->yr_major_length,
			       NhlNtmXBLabelFontHeightF,&mpp->xb_font_height,
			       NhlNtmXTLabelFontHeightF,&mpp->xt_font_height,
			       NhlNtmYLLabelFontHeightF,&mpp->yl_font_height,
			       NhlNtmYRLabelFontHeightF,&mpp->yr_font_height,
			       NULL);
	}

        mpp->view_changed = False;

/*
 * MapPlot simply passes these resources to the DataHandler, because they
 * are dependent on the contents of the Map DataBase in use. Therefore,
 * once they are set, change their value to NULL in order ensure that the
 * the next SetValues call will be able notice they have changed. (Otherwise
 * they could be assigned to the same memory as the last time.
 */
        mpp->area_names = NULL;
        mpp->dynamic_groups = NULL;
	mpp->data_set_name = NULL;
        mpp->area_masking_on_set = False;
        mpp->outline_masking_on_set = False;
	mpp->provincial.color_set = False;
	mpp->provincial.dash_pat_set = False;
        mpp->provincial.dash_seglen_set = False;
	mpp->provincial.thickness_set = False;
        mpp->grid_spacing_set = False;
        
	mpp = NULL;
	return ret;
}

/*ARGSUSED*/
static NhlBoolean NewDrawArgs
#if	NhlNeedProto
(
	_NhlArgList	args,
	int		num_args
)
#else
(args,num_args)
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlString pass_args[] = {
		NhlNvpXF,
		NhlNvpYF,
		NhlNvpWidthF,
		NhlNvpHeightF,
                NhlNvpOn,
		NhlNtfBaseXF,
		NhlNtfBaseYF,
		NhlNtfBaseWidthF,
		NhlNtfBaseHeightF,
		NhlNpmTickMarkDisplayMode,
		NhlNtmEqualizeXYSizes,
		NhlNtmLabelAutoStride,
		NhlNtmXBOn,
		NhlNtmXBLabelsOn,
		NhlNtmXBMode,
		NhlNtmXBValues,
		NhlNtmXBLabels,
		NhlNtmXBLabelFuncCode,
		NhlNtmXBLabelFontHeightF,
		NhlNtmXBMajorLengthF,
		NhlNtmXBMajorOutwardLengthF,
		NhlNtmXTOn,
		NhlNtmXTLabelsOn,
		NhlNtmXTMode,
		NhlNtmXTValues,
		NhlNtmXTLabels,
		NhlNtmXTLabelFuncCode,
		NhlNtmXTLabelFontHeightF,
		NhlNtmXTMajorLengthF,
		NhlNtmXTMajorOutwardLengthF,
		NhlNtmYLOn,
		NhlNtmYLLabelsOn,
		NhlNtmYLMode,
		NhlNtmYLValues,
		NhlNtmYLLabels,
		NhlNtmYLLabelFuncCode,
		NhlNtmYLLabelFontHeightF,
		NhlNtmYLMajorLengthF,
		NhlNtmYLMajorOutwardLengthF,
		NhlNtmYROn,
		NhlNtmYRLabelsOn,
		NhlNtmYRMode,
		NhlNtmYRValues,
		NhlNtmYRLabels,
		NhlNtmYRLabelFuncCode,
		NhlNtmYRLabelFontHeightF,
		NhlNtmYRMajorLengthF,
		NhlNtmYRMajorOutwardLengthF
	};
	int i,pass_count = 0;

	for (i = 0; i < NhlNumber(pass_args); i++)
		if (_NhlArgIsSet(args,num_args,pass_args[i]))
			pass_count++;
	if (num_args > pass_count) 
		return True;
	return False;
}

/*
 * Function:	MapPlotSetValues
 *
 * Description: 
 *
 * In Args:	old	copy of old instance record
 *		reference	requested instance record
 *		new	new instance record	
 *		args 	list of resources and values for reference
 *		num_args	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes MapPlotSetValues
#if	NhlNeedProto
(
	NhlLayer	old,
	NhlLayer	reference,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapPlotSetValues";
	char			*e_text;
        NhlSArg			sargs[64];
        int			nargs = 0;
        NhlMapPlotLayer		mpl = (NhlMapPlotLayer) new;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
        NhlMapPlotLayer		ompl = (NhlMapPlotLayer) old;

	mpl->trans.grid_type = NhltrMAP;

        if (mpl->view.x != ompl->view.x ||
            mpl->view.y != ompl->view.y ||
            mpl->view.width != ompl->view.width ||
            mpl->view.height != ompl->view.height) {
                mpp->view_changed = True;
        }
                
	if (mpl->view.use_segments != ompl->view.use_segments) {
		mpp->new_draw_req = True;
	}
	if (mpl->view.use_segments) {
                NhlTransDat *trans_dat = NULL;
                
		if (NewDrawArgs(args,num_args))
			mpp->new_draw_req = True;
                else {
                        if (mpp->draw_dat)
                                trans_dat = mpp->draw_dat;
                        else if (mpp->postdraw_dat)
                                trans_dat = mpp->postdraw_dat;
                        else if (mpp->predraw_dat)
                                trans_dat = mpp->predraw_dat;
                        if (! _NhlSegmentSpansArea
                            (trans_dat,
                             mpl->view.x,
                             mpl->view.x + mpl->view.width,
                             mpl->view.y - mpl->view.height,
                             mpl->view.y))
                                mpp->new_draw_req = True;
                }
	}

	mpp->limb.on = mpp->grid.on;
	mpp->limb.order = mpp->grid.order;

	if (_NhlArgIsSet(args,num_args,NhlNmpDefaultFillColor))
		mpp->fill_default.color_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpDefaultFillPattern))
		mpp->fill_default.pattern_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpDefaultFillScaleF))
		mpp->fill_default.scale_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpOceanFillColor))
		mpp->ocean.color_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpOceanFillPattern))
		mpp->ocean.pattern_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpOceanFillScaleF))
		mpp->ocean.scale_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpLandFillColor))
		mpp->land.color_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpLandFillPattern))
		mpp->land.pattern_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpLandFillScaleF))
		mpp->land.scale_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpInlandWaterFillColor))
		mpp->inland_water.color_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpInlandWaterFillPattern))
		mpp->inland_water.pattern_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpInlandWaterFillScaleF))
		mpp->inland_water.scale_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpGeophysicalLineDashSegLenF))
		mpp->geophysical.dash_seglen_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpProvincialLineDashSegLenF)) {
		mpp->us_state.dash_seglen = mpp->provincial.dash_seglen;
		mpp->us_state.dash_seglen_set = True;
	}
	else if (_NhlArgIsSet(args,num_args,NhlNmpUSStateLineDashSegLenF))
		mpp->us_state.dash_seglen_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpProvincialLineColor)) 
		mpp->us_state.color = mpp->provincial.color;
	if (_NhlArgIsSet(args,num_args,NhlNmpProvincialLineDashPattern)) 
		mpp->us_state.dash_pat = mpp->provincial.dash_pat;
	if (_NhlArgIsSet(args,num_args,NhlNmpProvincialLineThicknessF)) 
		mpp->us_state.thickness = mpp->provincial.thickness;
	if (_NhlArgIsSet(args,num_args,NhlNmpCountyLineDashSegLenF))
		mpp->county.dash_seglen_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpNationalLineDashSegLenF))
		mpp->national.dash_seglen_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpGridLineDashSegLenF))
		mpp->grid.dash_seglen_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpLimbLineDashSegLenF))
		mpp->limb.dash_seglen_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpPerimLineDashSegLenF))
		mpp->perim.dash_seglen_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpLabelFontHeightF))
		mpp->labels.height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpAreaMaskingOn))
		mpp->area_masking_on_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpMaskAreaSpecifiers)) {
                if (! mpp->area_masking_on_set)
                        mpp->area_masking_on = True;
        }
	if (_NhlArgIsSet(args,num_args,NhlNmpOutlineMaskingOn))
		mpp->outline_masking_on_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNmpMaskOutlineSpecifiers)) {
                if (! mpp->outline_masking_on_set)
                        mpp->outline_masking_on = True;
        }
        if ( _NhlArgIsSet(args,num_args,NhlNmpGridSpacingF))
                mpp->grid_lat_spacing = mpp->grid_lon_spacing =
                        mpp->grid_spacing;
        if (mpp->grid_lat_spacing <= 0.0) mpp->grid_lat_spacing = 15.0;
        if (mpp->grid_lon_spacing <= 0.0) mpp->grid_lon_spacing = 15.0;

	if (_NhlArgIsSet(args,num_args,NhlNtmXBMajorLengthF)) 
		mpp->xb_major_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTMajorLengthF)) 
		mpp->xt_major_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLMajorLengthF)) 
		mpp->yl_major_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRMajorLengthF)) 
		mpp->yr_major_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBMajorOutwardLengthF)) 
		mpp->xb_major_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTMajorOutwardLengthF)) 
		mpp->xt_major_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLMajorOutwardLengthF)) 
		mpp->yl_major_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRMajorOutwardLengthF)) 
		mpp->yr_major_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBLabelFontHeightF)) 
		mpp->xb_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTLabelFontHeightF)) 
		mpp->xt_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLLabelFontHeightF)) 
		mpp->yl_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRLabelFontHeightF)) 
		mpp->yr_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBOn)) 
		mpp->xb_on_set = True;
/*
 * once the following are set they remain set for the life of the object
 */
	if (_NhlArgIsSet(args,num_args,NhlNtmXTOn)) 
		mpp->xt_on_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLOn)) 
		mpp->yl_on_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYROn)) 
		mpp->yr_on_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBLabelsOn)) 
		mpp->xb_labels_on_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTLabelsOn)) 
		mpp->xt_labels_on_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLLabelsOn)) 
		mpp->yl_labels_on_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRLabelsOn)) 
		mpp->yr_labels_on_set = True;

/*
 * tickmark modes cannot be set for now; hopefully this will change
 */
	if (mpp->xb_mode != -1 ||
	    mpp->xt_mode != -1 ||
	    mpp->yl_mode != -1 ||
	    mpp->yr_mode != -1) {
		e_text = "%s: tm[XB|XT|YL|YR]Mode resources are not currently enabled for MapPlot tick marks";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		 mpp->xb_mode = mpp->xt_mode =  
			 mpp->yl_mode = mpp->yr_mode = -1;
	}
        
        
/* Set up the Map data handler */

        subret = mpSetUpDataHandler(mpl,ompl,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;
        
/* Set up the Map transformation */

	subret = mpSetUpTransObj(mpl,ompl,False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Set view dependent resources */

	subret = mpManageViewDepResources(mpl,ompl,False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

	subret = SetLineAttrs(mpl,ompl,False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the dynamic arrays */

	subret = mpManageDynamicArrays(mpl,ompl,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


        _NhlUpdateDrawList(mpp->map_data_handler,False,
                           mpl,ompl,args,num_args);

/* Manage TickMarks object */

	/* 42 arguments possible */
	subret = ManageTickMarks(mpl,ompl,False,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing TickMarks";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	
	/* 1 arg */
	if (mpp->update_req) {
		NhlSetSArg(&sargs[(nargs)++],NhlNpmUpdateReq,True);
	}
	
/* Manage the overlay */

	subret = _NhlManageOverlay(&mpp->overlay_object,new,old,
			       _NhlSETVALUES,sargs,nargs,entry_name);
	ret = MIN(ret,subret);

	if (mpp->overlay_object &&  mpp->display_tickmarks > NhlNOCREATE) {
		NhlVAGetValues(mpp->overlay_object->base.id,
			       NhlNtmXBMajorLengthF,&mpp->xb_major_length,
			       NhlNtmXTMajorLengthF,&mpp->xt_major_length,
			       NhlNtmYLMajorLengthF,&mpp->yl_major_length,
			       NhlNtmYRMajorLengthF,&mpp->yr_major_length,
			       NhlNtmXBMajorOutwardLengthF,
			       		&mpp->xb_major_outward_length,
			       NhlNtmXTMajorOutwardLengthF,
			       		&mpp->xt_major_outward_length,
			       NhlNtmYLMajorOutwardLengthF,
			       		&mpp->yl_major_outward_length,
			       NhlNtmYRMajorOutwardLengthF,
			       		&mpp->yr_major_outward_length,
			       NhlNtmXBLabelFontHeightF,&mpp->xb_font_height,
			       NhlNtmXTLabelFontHeightF,&mpp->xt_font_height,
			       NhlNtmYLLabelFontHeightF,&mpp->yl_font_height,
			       NhlNtmYRLabelFontHeightF,&mpp->yr_font_height,
			       NULL);
	}
	mpp->update_req = False;
	mpp->view_changed = False;

/*
 * MapPlot simply passes these resources to the DataHandler, because they
 * are dependent on the contents of the Map DataBase in use. Therefore,
 * once they are set, change their value to NULL in order ensure that the
 * the next SetValues call will be able notice they have changed. (Otherwise
 * they could be assigned to the same memory as the last time.
 */
        mpp->area_names = NULL;
        mpp->dynamic_groups = NULL;
        mpp->data_set_name = NULL;
        
        mpp->area_masking_on_set = False;
        mpp->outline_masking_on_set = False;
        mpp->grid_spacing_set = False;
        
	mpp = NULL;
        
	return ret;
}


/*
 * Function:    MapPlotGetValues
 *
 * Description: Retrieves the current setting of one or more MapPlot resources.
 *      This routine only retrieves resources that require special methods
 *      that the generic GetValues method cannot handle. For now this means
 *      all the GenArray resources. Note that space is allocated; the user
 *      is responsible for freeing this space.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *		NhlNmpFillAreaSpecifiers
 *		NhlNmpMaskAreaSpecifiers
 *		NhlNmpOutlineSpecifiers
 *		NhlNmpAreaNames
 *		NhlNmpAreaTypes
 *		NhlNmpDynamicAreaGroups
 *		NhlNmpSpecifiedFillColors
 *		NhlNmpFillColors
 *		NhlNmpFillPatterns
 *		NhlNmpFillScales
 *
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    MapPlotGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlMapPlotLayer mp = (NhlMapPlotLayer) l;
        NhlMapPlotLayerPart *mpp = &(mp->mapplot);
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
        NhlGenArray ga;
        char *e_text;
        int i, count = 0;
        int data_handler_args[6];
        int data_handler_arg_count = 0;

        for (i = 0; i < num_args; i++ ) {

                ga = NULL;
                if(args[i].quark == Qfill_area_specs) {
                        ga = mpp->fill_area_specs;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qmask_area_specs) {
                        ga = mpp->mask_area_specs;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qmask_outline_specs) {
                        ga = mpp->mask_outline_specs;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qoutline_specs) {
                        ga = mpp->outline_specs;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qarea_names) {
                        data_handler_args[data_handler_arg_count++] = i;
                }
                else if (args[i].quark == Qarea_types) {
                        data_handler_args[data_handler_arg_count++] = i;
                }
                else if (args[i].quark == Qdynamic_groups) {
                        data_handler_args[data_handler_arg_count++] = i;
                }
                else if (args[i].quark == Qfixed_groups) {
                        data_handler_args[data_handler_arg_count++] = i;
                }
                else if (args[i].quark == Qdata_set_name) {
                        data_handler_args[data_handler_arg_count++] = i;
                }
                else if (args[i].quark == Qspec_fill_colors) {
                        ga = mpp->spec_fill_colors;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qspec_fill_patterns) {
                        ga = mpp->spec_fill_patterns;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qspec_fill_scales) {
                        ga = mpp->spec_fill_scales;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qfill_colors) {
                        ga = mpp->fill_colors;
                        count = mpp->area_group_count;
                }
                else if (args[i].quark == Qfill_patterns) {
                        ga = mpp->fill_patterns;
                        count = mpp->area_group_count;
                }
                else if (args[i].quark == Qfill_scales) {
                        ga = mpp->fill_scales;
                        count = mpp->area_group_count;
                }
                if (ga != NULL) {
                        if ((ga = mpGenArraySubsetCopy(ga, count)) == NULL) {
                                e_text = "%s: error copying %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          "MapPlotGetValues",
					  NrmQuarkToString(args[i].quark));
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
                }
        }

        if (data_handler_arg_count) {
                NhlGArg		gargs[6];
                int             nargs = 0;
                NhlGenArray 	dhga[6];
                NhlString	dhstr[6];
                
                for (i = 0; i < data_handler_arg_count; i++) {
                        dhstr[i] = NrmQuarkToString
                                (args[data_handler_args[i]].quark);
                        NhlSetGArg(&gargs[nargs++],dhstr[i],&dhga[i]);
                }
                subret = NhlALGetValues
                        (mpp->map_data_handler->base.id,gargs,nargs);
                if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                
                for (i = 0; i < data_handler_arg_count; i++) {
			if (args[data_handler_args[i]].quark == Qdata_set_name)
				*((NhlString *)
				  (args[data_handler_args[i]].value.ptrval)) 
					= (NhlString) dhga[i];
			else
				*((NhlGenArray *)
				  (args[data_handler_args[i]].value.ptrval)) 
					= dhga[i];
                }
        }
        return(ret);
}

/*
 * Function:  mpGenArraySubsetCopy
 *
 * Description: Since the internal GenArrays maintained by the MapPlot object
 *      may be bigger than the size currently in use, this function allows
 *      a copy of only a portion of the array to be created. This is for
 *      use by the GetValues routine when returning GenArray resources to
 *      the user level. The array is assumed to be valid. The only pointer
 *      type arrays that the routine can handle are NhlString arrays.
 *      Note: this might be another candidate for inclusion as a global
 *      routine.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlGenArray mpGenArraySubsetCopy
#if	NhlNeedProto
        (NhlGenArray    ga,
        ng_size_t       length)
#else
(ga,length)
        NhlGenArray     ga;
        ng_size_t       length;
#endif
{
        NhlGenArray gto;

        if (length > ga->num_elements)
                return NULL;

        if ((gto = _NhlCopyGenArray(ga,False)) == NULL) {
                return NULL;
        }
        if ((gto->data = (NhlPointer) NhlMalloc(length * ga->size)) == NULL) {
                return NULL;
        }
        if (ga->typeQ != Qstring) {
                memcpy((void *)gto->data, (Const void *) ga->data,
                       length * ga->size);
        }
        else {
                NhlString *cfrom = (NhlString *) ga->data;
                NhlString *cto = (NhlString *) gto->data;
                int i;
                for (i=0; i<length; i++) {
                        if ((*cto = (char *)
                             NhlMalloc(strlen(*cfrom)+1)) == NULL) {
                                return NULL;
                        }
                        strcpy(*cto++,*cfrom++);
                }
        }
        gto->num_elements = length;
	gto->my_data = True;
        return gto;
}

/*
 * Function:	MapPlotDestroy
 *
 * Description: Destroys MapPlot instance. No need to check for 
 *		Overlay Member Plot status. MapPlot cannot be a Member Plot.
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes MapPlotDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlMapPlotLayerPart	*mpp = &(((NhlMapPlotLayer) inst)->mapplot);
	NhlTransformLayerPart	*mptp = &(((NhlTransformLayer) inst)->trans);
	NhlErrorTypes		ret = NhlNOERROR;

	if (mpp->overlay_object != NULL) {
		(void) _NhlDestroyChild(mpp->overlay_object->base.id,inst);
		mpp->overlay_object = NULL;
	}
	if (mptp->trans_obj != NULL) {
		(void) _NhlDestroyChild(mptp->trans_obj->base.id,inst);
		mptp->trans_obj = NULL;
	}
        if (mpp->map_data_handler) {
                NhlDestroy(mpp->map_data_handler->base.id);
                mpp->map_data_handler = NULL;
        }
	NhlFreeGenArray(mpp->dash_table);
	NhlFreeGenArray(mpp->fill_area_specs);
	NhlFreeGenArray(mpp->mask_area_specs);
	NhlFreeGenArray(mpp->outline_specs);
	NhlFreeGenArray(mpp->fill_colors);
	NhlFreeGenArray(mpp->fill_patterns);
	NhlFreeGenArray(mpp->fill_scales);
	NhlFreeGenArray(mpp->spec_fill_colors);
	NhlFreeGenArray(mpp->spec_fill_patterns);
	NhlFreeGenArray(mpp->spec_fill_scales);
	if (mpp->xbvalues)
		NhlFreeGenArray(mpp->xbvalues);
	if (mpp->xblabels)
		NhlFreeGenArray(mpp->xblabels);
	if (mpp->xtvalues)
		NhlFreeGenArray(mpp->xtvalues);
	if (mpp->xtlabels)
		NhlFreeGenArray(mpp->xtlabels);
	if (mpp->ylvalues)
		NhlFreeGenArray(mpp->ylvalues);
	if (mpp->yllabels)
		NhlFreeGenArray(mpp->yllabels);
	if (mpp->yrvalues)
		NhlFreeGenArray(mpp->yrvalues);
	if (mpp->yrlabels)
		NhlFreeGenArray(mpp->yrlabels);


	if (mpp->predraw_dat != NULL)
		_NhlDeleteViewSegment(inst,mpp->predraw_dat);
	if (mpp->draw_dat != NULL)
		_NhlDeleteViewSegment(inst,mpp->draw_dat);
	if (mpp->postdraw_dat != NULL)
		_NhlDeleteViewSegment(inst,mpp->postdraw_dat);

	return(ret);
}

/*
 * Function:    MapPlotGetBB
 *
 * Description: 
 *
 * In Args:     instance        the object instance record
 *              thebox          a data structure used to hold bounding box 
 *                              information.
 *
 * Out Args:    NONE
 *
 * Return Values:       Error Conditions
 *
 * Side Effects:        NONE
 */
static NhlErrorTypes MapPlotGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	char			*entry_name  = "MapPlotGetBB";
	char			*e_text;
	NhlMapPlotLayer		mpl = (NhlMapPlotLayer) instance;
	NhlTransformLayerPart	*mptp = &(((NhlTransformLayer)mpl)->trans);
	NhlViewLayerPart	*mpvp = &(((NhlViewLayer) mpl)->view);
        NhlMapPlotLayerPart	*mpp = &(((NhlMapPlotLayer) mpl)->mapplot);
	float			map_left,map_right,map_bottom,map_top;

	if (! _NhlIsTransform(instance)) {
		e_text = "%s: invalid object id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
/*
 * If the MapPlot object is a overlay base, return the bounding box
 * of the complete overlay.
 * Otherwise, return its viewport only.
 */
	if (mptp->overlay_status == _tfCurrentOverlayBase) {
		_NhlGetBB(mptp->overlay_object,thebox);
                if (! mpp->labels.on)
                        return NhlNOERROR;
        }
        if (mpp->labels.on) {
                float h = mpp->labels.height;

                NhlVAGetValues(mptp->trans_obj->base.id,
                               NhlNmpLeftMapPosF, &map_left,
                               NhlNmpRightMapPosF, &map_right,
                               NhlNmpBottomMapPosF, &map_bottom,
                               NhlNmpTopMapPosF, &map_top,
                               NULL);
                _NhlAddBBInfo(map_top+h,map_bottom-h,
                              map_right+h,map_left-h,thebox);
        }
        else
                _NhlAddBBInfo(mpvp->y,mpvp->y - mpvp->height,
                              mpvp->x + mpvp->width,mpvp->x,thebox);
        

	return NhlNOERROR;
}


/*
 * Function:	MapPlotPreDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes MapPlotPreDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString		e_text,entry_name = "MapPlotPreDraw";
	NhlMapPlotLayer		mpl = (NhlMapPlotLayer) layer;
	NhlMapPlotLayerPart	*mpp = &mpl->mapplot;
	NhlTransformLayerPart	*tfp = &(mpl->trans);

        mpp->init_draw = True;
        
	/* update the dash table, which could have been edited */
	
	if (mpp->dash_table)
		NhlFreeGenArray(mpp->dash_table);
	subret = NhlVAGetValues(mpl->base.wkptr->base.id,
				_NhlNwkDashTable,&mpp->dash_table,
				NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: NhlFATAL error retrieving dash table";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		mpp->dash_table = NULL;
		return ret;
	}


	if (tfp->overlay_status == _tfNotInOverlay) {
		subret = _NhlSetTrans((NhlLayer)tfp->trans_obj,(NhlLayer)mpl);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: Error setting transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	if (mpp->fill_order != NhlPREDRAW &&
	    mpp->outline_order != NhlPREDRAW &&
	    mpp->grid.order != NhlPREDRAW &&
	    mpp->perim.order != NhlPREDRAW &&
	    mpp->labels.order != NhlPREDRAW)
		return NhlNOERROR;

	Mpp = mpp;

	if (mpl->view.use_segments && ! mpp->new_draw_req &&
	    mpp->predraw_dat && mpp->predraw_dat->id != NgNOT_A_SEGMENT) {
		ret = _NhltfDrawSegment((NhlLayer)mpl,tfp->trans_obj,
					mpp->predraw_dat,entry_name);
		Mpp = NULL;
		return ret;
	}

	subret = _NhlActivateWorkstation(mpl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		Mpp = NULL;
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (mpl->view.use_segments) {
		subret = _NhltfInitSegment((NhlLayer)mpl,tfp->trans_obj,
					    &mpp->predraw_dat,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			Mpp = NULL;
			e_text = "%s: Error initializing segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	subret = mpDraw((NhlMapPlotLayer) layer,NhlPREDRAW,entry_name);
	ret = MIN(subret,ret);

	if (mpl->view.use_segments) {
		_NhlEndSegment(mpp->predraw_dat);
	}

	subret = _NhlDeactivateWorkstation(mpl->base.wkptr);

	Mpp = NULL;

	return MIN(subret,ret);
}

/*
 * Function:	MapPlotDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes MapPlotDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString		e_text,entry_name = "MapPlotDraw";
	NhlMapPlotLayer		mpl = (NhlMapPlotLayer) layer;
	NhlMapPlotLayerPart	*mpp = &mpl->mapplot;
	NhlTransformLayerPart	*tfp = &(mpl->trans);

	if (mpp->fill_order != NhlDRAW &&
	    mpp->outline_order != NhlDRAW &&
	    mpp->grid.order != NhlDRAW &&
	    mpp->perim.order != NhlDRAW &&
	    mpp->labels.order != NhlDRAW)
		return NhlNOERROR;

	Mpp = mpp;

	if (mpl->view.use_segments && ! mpp->new_draw_req &&
	    mpp->draw_dat && mpp->draw_dat->id != NgNOT_A_SEGMENT) {
		ret = _NhltfDrawSegment((NhlLayer)mpl,tfp->trans_obj,
					mpp->draw_dat,entry_name);
		Mpp = NULL;
		return ret;
	}

	subret = _NhlActivateWorkstation(mpl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		Mpp = NULL;
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (mpl->view.use_segments) {
		subret = _NhltfInitSegment((NhlLayer)mpl,tfp->trans_obj,
					    &mpp->draw_dat,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			Mpp = NULL;
			e_text = "%s: Error initializing segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	subret = mpDraw((NhlMapPlotLayer) layer,NhlDRAW,entry_name);
	ret = MIN(subret,ret);


	if (mpl->view.use_segments) {
		_NhlEndSegment(mpp->draw_dat);
	}
	subret = _NhlDeactivateWorkstation(mpl->base.wkptr);

	Mpp = NULL;

	return MIN(subret,ret);

}

/*
 * Function:	MapPlotPostDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes MapPlotPostDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString		e_text,entry_name = "MapPlotPostDraw";
	NhlMapPlotLayer		mpl = (NhlMapPlotLayer) layer;
	NhlMapPlotLayerPart	*mpp = &mpl->mapplot;
	NhlTransformLayerPart	*tfp = &(mpl->trans);


	if (mpp->fill_order != NhlPOSTDRAW &&
	    mpp->outline_order != NhlPOSTDRAW &&
	    mpp->grid.order != NhlPOSTDRAW &&
	    mpp->perim.order != NhlPOSTDRAW &&
	    mpp->labels.order != NhlPOSTDRAW) {
		Mpp = NULL;
		mpp->new_draw_req = False;
		return NhlNOERROR;
	}

	Mpp = mpp;

	if (mpl->view.use_segments && ! mpp->new_draw_req &&
	    mpp->postdraw_dat && mpp->postdraw_dat->id != NgNOT_A_SEGMENT) {
		ret = _NhltfDrawSegment((NhlLayer)mpl,tfp->trans_obj,
					mpp->postdraw_dat,entry_name);
		Mpp = NULL;
		return ret;
	}
	subret = _NhlActivateWorkstation(mpl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		Mpp = NULL;
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (mpl->view.use_segments) {
		subret = _NhltfInitSegment((NhlLayer)mpl,tfp->trans_obj,
					    &mpp->postdraw_dat,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			Mpp = NULL;
			e_text = "%s: Error initializing segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	subret = mpDraw((NhlMapPlotLayer) layer,NhlPOSTDRAW,entry_name);
	ret = MIN(subret,ret);

	if (mpl->view.use_segments) {
		_NhlEndSegment(mpp->postdraw_dat);
	}

	subret = _NhlDeactivateWorkstation(mpl->base.wkptr);

	mpp->new_draw_req = False;
	Mpp = NULL;

	return MIN(subret,ret);
}

/*
 * Function:	mpDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpDraw
#if	NhlNeedProto
(
	NhlMapPlotLayer	mp,
	NhlDrawOrder	order,
	NhlString	entry_name
)
#else
(mp,order,entry_name)
        NhlMapPlotLayer mp;
	NhlDrawOrder	order;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mp->mapplot);
	NhlBoolean		do_labels = False,
				do_perim = False;

	NhlVASetValues(mp->base.wkptr->base.id,
		_NhlNwkReset,	True,
		NULL);

	NGCALLF(setdashchar,SETDASHCHAR)();

	c_mpsetr("OT",0.0);



/* Do the fill first */

	if (mpp->fill_on && mpp->fill_order == order) {
                subret = _NhlDrawMapList
                        (mpp->map_data_handler,mp,mpDRAWFILL,order);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
        }

/* Draw the grid and the limb line */

	if (mpp->grid.on && mpp->grid.order == order) {
                subret = _NhlDrawMapList
                        (mpp->map_data_handler,mp,mpDRAWGRID,order);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	}

/* Set up the map outlines */

	if (mpp->outline_on && mpp->outline_order == order) {
                subret = _NhlDrawMapList
                        (mpp->map_data_handler,mp,mpDRAWOUTLINE,order);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
        }

/* Draw labels and/or the perimeter */

	if (mpp->labels.on && mpp->labels.order == order && mpp->labels.gks_color > -1) {
		int ls = mpp->labels.real_height * 1024;
		c_mpseti("LS",ls);
 		c_mpseti("C3",mpp->labels.gks_color);
		c_mpseti("LA",1);
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
		do_labels = True;
	}
	else {
		c_mpseti("LA",0);
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	
	if (mpp->perim.on && mpp->perim.order == order && mpp->perim.gks_color > -1) {
		c_mpseti("C1",mpp->perim.gks_color);
		c_mpseti("PE",1);
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
		do_perim = True;
	}
	else {
		c_mpseti("PE",0);
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (do_labels || do_perim)  {
		c_mpsetc("OU","NO");
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
		c_maplbl();
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}

	NGCALLF(resetdashchar,RESETDASHCHAR)();
	return ret;
}

/*
 * Function:  mpAdjustText
 *
 * Description: Adjusts the text height and aspect ratio
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static NhlErrorTypes    mpAdjustText
#if	NhlNeedProto
(
	NhlmpLabelAttrs *lbl_attrs,
	NhlMapPlotLayer	new, 
	NhlMapPlotLayer	old,
	NhlBoolean	init
)
#else
(lbl_attrs,new,old,init)
	NhlmpLabelAttrs *lbl_attrs;
	NhlLayer	new;
	NhlLayer	old;
	NhlBoolean	init;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char 			*e_text;
	char			*entry_name;
	NhlMapPlotLayer		mpnew = (NhlMapPlotLayer) new;
	NhlMapPlotLayer		mpold = (NhlMapPlotLayer) old;

	entry_name = (init) ? "MapPlotInitialize" : "MapPlotSetValues";

/* 
 * Adjust text height. Then determine principal width and height
 * and the "real " text height based on aspect ratio. 21.0 is the default 
 * principle height. This code handles aspect ratio like the TextItem.
 */

	if (! lbl_attrs->height_set) {
		if (init) {
			lbl_attrs->height = Nhl_mpDEF_LABEL_HEIGHT *
				mpnew->view.width / NHL_DEFAULT_VIEW_WIDTH;
		}
		else if (mpnew->view.width != mpold->view.width) {
			lbl_attrs->height *= 
				mpnew->view.width / mpold->view.width;
		}
	}
	lbl_attrs->aspect = 1.3125;

        if (lbl_attrs->aspect <= 0.0 ) {
		e_text = "%s: Invalid value for text aspect ratio %d";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,lbl_attrs->aspect);
                ret = NhlWARNING;
                lbl_attrs->aspect = 1.3125;
        }
        if (lbl_attrs->aspect <= 1.0) {
                lbl_attrs->pheight = 21.0 * lbl_attrs->aspect;
                lbl_attrs->pwidth = 21.0;
        } else {
                lbl_attrs->pwidth = 21.0 * 1.0/lbl_attrs->aspect;
                lbl_attrs->pheight = 21.0;
        }
        lbl_attrs->real_height = 
		1.0 / lbl_attrs->aspect * lbl_attrs->height;

	return ret;
}

/*
 * Function:  mpAdjustDashSegLen
 *
 * Description: Adjust the dash segment length
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static void mpAdjustDashSegLen
#if	NhlNeedProto
(
	float		*seglen,
	NhlBoolean	init,
	float		new_vpwidth,
	float		old_vpwidth
)
#else
(seglen,init,new_vpwidth,old_vpwidth)
	float		*seglen;
	NhlBoolean	init;
	float		new_vpwidth;
	float		old_vpwidth;
#endif
{
	
	if (init)
		*seglen = Nhl_mpDEF_DASH_SEGLEN * 
			new_vpwidth / NHL_DEFAULT_VIEW_WIDTH;
	else if (new_vpwidth != old_vpwidth)
		*seglen *= new_vpwidth / old_vpwidth;
	return;
}

/*
 * Function:  mpManageViewDepResources
 *
 * Description: Modifies resources that may need to change when the view
 *	is modified.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static NhlErrorTypes    mpManageViewDepResources
#if	NhlNeedProto
(
	NhlMapPlotLayer mpnew, 
	NhlMapPlotLayer mpold,
	NhlBoolean	init
)
#else
(mpnew,mpold,init)
	NhlMapPlotLayer mpnew;
	NhlMapPlotLayer mpold;
	NhlBoolean	init;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);

/* Adjust line dash segment lengths */

	if (! mpp->geophysical.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->geophysical.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->us_state.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->us_state.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->county.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->county.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->national.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->national.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->grid.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->grid.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->limb.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->limb.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->perim.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->perim.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);

	subret = mpAdjustText(&mpp->labels,mpnew,mpold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	mpp->geophysical.dash_seglen_set = False;
	mpp->us_state.dash_seglen_set = False;
	mpp->county.dash_seglen_set = False;
	mpp->national.dash_seglen_set = False;
	mpp->grid.dash_seglen_set = False;
	mpp->limb.dash_seglen_set = False;
	mpp->perim.dash_seglen_set = False;
	mpp->labels.height_set = False;

	return ret;
}

 
/*
 * Function:  CheckColor
 *
 * Description: Checks whether a color value is invalid 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static NhlErrorTypes    CheckColor
#if	NhlNeedProto
(
	NhlMapPlotLayer mpnew,
	int		cix,
	NhlString	resname,
	int		*gks_cix,
	char		*entry_name
)
#else
(mpnew,cix,resname,gks_cix,entry_name)
	NhlMapPlotLayer mpnew;
	int		cix;
	NhlString	resname;
	int		*gks_cix;
	char		*entry_name;

#endif

{
	char 		*e_text;
	NhlErrorTypes	ret = NhlNOERROR;

	if (cix == NhlTRANSPARENT) {
		*gks_cix = -1;
		return ret;
	}
	else if (cix < NhlBACKGROUND) {
		e_text = "%s: invalid color index for %s; defaulting";
		ret = NhlWARNING;
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,resname);
		cix = NhlFOREGROUND;
	}
	*gks_cix = _NhlGetGksCi(mpnew->base.wkptr,cix);
	return ret;
} 

/*
 * Function:  SetLineAttrs
 *
 * Description: Sets line resources (mainly gks colors right now)
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static NhlErrorTypes    SetLineAttrs
#if	NhlNeedProto
(
	NhlMapPlotLayer mpnew, 
	NhlMapPlotLayer mpold,
	NhlBoolean	init
)
#else
(mpnew,mpold,init)
	NhlMapPlotLayer mpnew;
	NhlMapPlotLayer mpold;
	NhlBoolean	init;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);
	char			*entry_name;

	entry_name = init ? "MapPlotInitialize" : "MapPlotSetValues";

	subret = CheckColor(mpnew,mpp->grid.color,
			    NhlNmpGridLineColor,
			    &mpp->grid.gks_color,entry_name);
	ret = MIN(subret,ret);

	subret = CheckColor(mpnew,mpp->limb.color,
			    NhlNmpLimbLineColor,
			    &mpp->limb.gks_color,entry_name);
	ret = MIN(subret,ret);

	subret = CheckColor(mpnew,mpp->geophysical.color,
			    NhlNmpGeophysicalLineColor,
			    &mpp->geophysical.gks_color,entry_name);
	ret = MIN(subret,ret);

	subret = CheckColor(mpnew,mpp->national.color,
			    NhlNmpNationalLineColor,
			    &mpp->national.gks_color,entry_name);
	ret = MIN(subret,ret);

	subret = CheckColor(mpnew,mpp->us_state.color,
			    NhlNmpUSStateLineColor,
			    &mpp->us_state.gks_color,entry_name);
	ret = MIN(subret,ret);

	subret = CheckColor(mpnew,mpp->county.color,
			    NhlNmpCountyLineColor,
			    &mpp->county.gks_color,entry_name);
	ret = MIN(subret,ret);

	subret = CheckColor(mpnew,mpp->perim.color,
			    NhlNmpPerimLineColor,
			    &mpp->perim.gks_color,entry_name);
	ret = MIN(subret,ret);

	subret = CheckColor(mpnew,mpp->labels.color,
			    NhlNmpLabelFontColor,
			    &mpp->labels.gks_color,entry_name);
	ret = MIN(subret,ret);

	return ret;
}

/*
 * Function:  mpManageDynamicArrays
 *
 * Description: Creates and manages internal copies of each of the 
 *	MapPlot GenArrays. Populates the copies with the values specified 
 *	via MapPlotCreate or MapPlotSetValues calls. Assigns default 
 *	values and sizes to any array resource not set by the caller.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static NhlErrorTypes    mpManageDynamicArrays
#if	NhlNeedProto
(
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args)
#else
(mpnew,mpold,init,args,num_args)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlMapPlotLayerPart *mpp = &(mpnew->mapplot);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *entry_name;
	char *e_text;
	NhlGenArray ga;
	NhlMapPlotLayerPart *ompp = &(mpold->mapplot);
	int i;
	ng_size_t count;
	int *ip;
	float *fp;
	NhlString *sp;
	float fval;
	ng_size_t init_count;
	NhlBoolean need_check,changed;
	ng_size_t old_count;
	int cmap_len = 0;

	entry_name =  init ? "MapPlotInitialize" : "MapPlotSetValues";

/* 
 * need a copy of the dash table
 */	
	if (init) {
		subret = NhlVAGetValues(mpnew->base.wkptr->base.id,
					_NhlNwkDashTable, &ga, NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: NhlFATAL error retrieving dash table";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		mpp->dash_table = ga;
	}
/*
 * The fill group count has minimum and maximum values
 */

	if (mpp->area_group_count > Nhl_mpMAX_AREA_GROUPS) {
		e_text = "%s: %s exceeds maximum value, %d: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNmpAreaGroupCount,Nhl_mpMAX_AREA_GROUPS);
		ret = MIN(ret, NhlWARNING);
		mpp->area_group_count = Nhl_mpMAX_AREA_GROUPS;
	}
	else if (mpp->area_group_count < Nhl_mpMIN_AREA_GROUPS) {
		e_text = "%s: %s less than minimum value, %d: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNmpAreaGroupCount,Nhl_mpMIN_AREA_GROUPS);
		ret = MIN(ret, NhlWARNING);
		mpp->area_group_count = Nhl_mpMIN_AREA_GROUPS;
	}
/*
 * Fill group colors
 */

	ga = init ? NULL : ompp->fill_colors;
	count = mpp->area_group_count;
  	subret = mpManageGenArray(&ga,count,mpp->fill_colors,Qint,NULL,
				  &old_count,&init_count,&need_check,&changed,
				  NhlNmpFillColors,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ompp->fill_colors = changed ? NULL : mpp->fill_colors;
	mpp->fill_colors = ga;

	ip = (int *) ga->data;
	NhlVAGetValues(mpnew->base.wkptr->base.id,
		       NhlNwkColorMapLen,&cmap_len,NULL);
	if (need_check) {
		for (i=init_count; i < count; i++) {
			ip[i] = i<NhlNumber(Init_Colors) ? Init_Colors[i] : i;
		}
	}
	
/*
 * If any of the individual convenience fill color resources are set --
 * override the corresponding array resource.
 */
	if (mpp->fill_default.color_set) {
		ip[NhlmpDEFAULTGROUPINDEX] = mpp->fill_default.color;
	}
	else {
		mpp->fill_default.color = ip[NhlmpDEFAULTGROUPINDEX];
	}
	mpp->fill_default.color_set = False;
	
	if (mpp->ocean.color_set) {
		ip[NhlmpOCEANGROUPINDEX] = mpp->ocean.color;
	}
	else {
		mpp->ocean.color = ip[NhlmpOCEANGROUPINDEX];
	}
	mpp->ocean.color_set = False;

	if (mpp->land.color_set) {
		ip[NhlmpLANDGROUPINDEX] = mpp->land.color;
	}
	else {
		mpp->land.color = ip[NhlmpLANDGROUPINDEX];
	}
	mpp->land.color_set = False;

	if (mpp->inland_water.color_set) {
		ip[NhlmpINLANDWATERGROUPINDEX] = mpp->inland_water.color;
	}
	else {
		mpp->inland_water.color = ip[NhlmpINLANDWATERGROUPINDEX];
	}
	mpp->inland_water.color_set = False;
/*
 * Fill patterns
 */

	ga = init ? NULL : ompp->fill_patterns;
	count = mpp->area_group_count;
	subret = mpManageGenArray(&ga,count,mpp->fill_patterns,Qint,NULL,
				  &old_count,&init_count,&need_check,&changed,
				  NhlNmpFillPatterns,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ompp->fill_patterns = changed ? NULL : mpp->fill_patterns;
	mpp->fill_patterns = ga;

	ip = (int *) ga->data;
	if (need_check) {
		int len;

		NhlVAGetValues(mpnew->base.wkptr->base.id,
			        NhlNwkFillTableLength, &len, NULL);

		for (i=init_count; i < count; i++) {
			ip[i] = i  % len + 1;
		}
		for (i=0; i<init_count; i++) {
			if (ip[i] < NhlHOLLOWFILL || ip[i] > len) {
				e_text =
	      "%s: %s index %d holds an invalid pattern value, %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,
					  NhlNmpFillPatterns,i,ip[i]);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlSOLIDFILL;
			}
		}
	}
	
/*
 * If any of the individual convenience fill pattern resources are set --
 * override the corresponding array resource.
 */

	if (mpp->fill_default.pattern_set) {
			ip[NhlmpDEFAULTGROUPINDEX] = mpp->fill_default.pattern;
	}
	else {
		mpp->fill_default.pattern = ip[NhlmpDEFAULTGROUPINDEX];
	}
	mpp->fill_default.pattern_set = False;
	
	if (mpp->ocean.pattern_set) {
			ip[NhlmpOCEANGROUPINDEX] = mpp->ocean.pattern;
	}
	else {
		mpp->ocean.pattern = ip[NhlmpOCEANGROUPINDEX];
	}
	mpp->ocean.pattern_set = False;

	if (mpp->land.pattern_set) {
			ip[NhlmpLANDGROUPINDEX] = mpp->land.pattern;
	}
	else {
		mpp->land.pattern = ip[NhlmpLANDGROUPINDEX];
	}
	mpp->land.pattern_set = False;

	if (mpp->inland_water.pattern_set) {
			ip[NhlmpINLANDWATERGROUPINDEX] =
				mpp->inland_water.pattern;
	}
	else {
		mpp->inland_water.pattern = ip[NhlmpINLANDWATERGROUPINDEX];
	}
	mpp->inland_water.pattern_set = False;

/*
 * Fill scales
 */

	ga = init ? NULL : ompp->fill_scales;
	count = mpp->area_group_count;
	fval = 1.0;
	subret = mpManageGenArray(&ga,count,mpp->fill_scales,
				  Qfloat,&fval,&old_count,&init_count,
				  &need_check,&changed,
				  NhlNmpFillScales,entry_name);
	
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ompp->fill_scales = changed ? NULL : mpp->fill_scales;
	mpp->fill_scales = ga;
	
	fp = (float *) ga->data;
	if (need_check) {
		for (i=0; i<count; i++) {
			if (fp[i] <= 0.0) {
				e_text =
	            "%s: %s index %d holds an invalid fill scale: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNmpFillScales,i);
				ret = MIN(ret, NhlWARNING);
				fp[i] = 1.0;
			}
		}
	}

/*
 * If any of the individual convenience fill pattern resources are set --
 * override the corresponding array resource. 
 */

	if (mpp->fill_default.scale_set) {
		if (mpp->fill_default.scale > 0.0)
			fp[NhlmpDEFAULTGROUPINDEX] = mpp->fill_default.scale;
	}
	else {
		mpp->fill_default.scale = fp[NhlmpDEFAULTGROUPINDEX];
	}
	mpp->fill_default.scale_set = False;
	
	if (mpp->ocean.scale_set) {
		if (mpp->ocean.scale > 0.0)
			fp[NhlmpOCEANGROUPINDEX] = mpp->ocean.scale;
	}
	else {
		mpp->ocean.scale = fp[NhlmpOCEANGROUPINDEX];
	}
	mpp->ocean.scale_set = False;

	if (mpp->land.scale_set) {
		if (mpp->land.scale > 0.0)
			fp[NhlmpLANDGROUPINDEX] = mpp->land.scale;
	}
	else {
		mpp->land.scale = fp[NhlmpLANDGROUPINDEX];
	}
	mpp->land.scale_set = False;

	if (mpp->inland_water.scale_set) {
		if (mpp->inland_water.scale > 0.0)
			fp[NhlmpINLANDWATERGROUPINDEX] =
				mpp->inland_water.scale;
	}
	else {
		mpp->inland_water.scale = fp[NhlmpINLANDWATERGROUPINDEX];
	}
	mpp->inland_water.scale_set = False;

/*
 * Fill area specifiers
 */

	ga = init ? NULL : ompp->fill_area_specs;
        if (! mpp->fill_area_specs && ga) {
               NhlFreeGenArray(ga);
               ompp->fill_area_specs = (NhlGenArray)0xdeadbeef;
        }
	else if (ga != mpp->fill_area_specs) {
		NhlFreeGenArray(ga);
		if ((ga = _NhlCopyGenArray(mpp->fill_area_specs,
					   True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpFillAreaSpecifiers);
			return NhlFATAL;
		}
		mpp->fill_area_specs = ga;
		ompp->fill_area_specs = NULL;
		/* Check elements for null strings */
		sp = (NhlString *) mpp->fill_area_specs->data;
		for (i = 0; i < mpp->fill_area_specs->num_elements; i++) {
			if (sp[i] == NULL || strlen(sp[i]) == 0) {
				e_text = 
		 "%s: Null or zero length %s string for index %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNmpFillAreaSpecifiers,
					  i,NhlmpNULLAREA);
				ret = MIN(ret,NhlWARNING);
				if (sp[i] != NULL) NhlFree(sp[i]);
				sp[i] = NhlMalloc(strlen(NhlmpNULLAREA) + 1);
				if (sp[i] == NULL) {
					e_text = 
				       "%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				strcpy(sp[i],NhlmpNULLAREA);
			}
		}
	}
		
/*
 * Mask area specifiers
 */
	ga = init ? NULL : ompp->mask_area_specs;

        if (! mpp->mask_area_specs && ga) {
               NhlFreeGenArray(ga);
               ompp->mask_area_specs = (NhlGenArray)0xdeadbeef;
        }
	else if (ga != mpp->mask_area_specs) {
		NhlFreeGenArray(ga);
		if ((ga = _NhlCopyGenArray(mpp->mask_area_specs,
					   True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpMaskAreaSpecifiers);
			return NhlFATAL;
		}
		mpp->mask_area_specs = ga;
		ompp->mask_area_specs = NULL;
		/* Check elements for null strings */
		sp = (NhlString *) mpp->mask_area_specs->data;
		for (i = 0; i < mpp->mask_area_specs->num_elements; i++) {
			if (sp[i] == NULL) {
				e_text = 
		 "%s: Null or zero length %s string for index %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNmpMaskAreaSpecifiers,
					  i,NhlmpNULLAREA);
				ret = MIN(ret,NhlWARNING);
				if (sp[i] != NULL) NhlFree(sp[i]);
				sp[i] = NhlMalloc(strlen(NhlmpNULLAREA) + 1);
				if (sp[i] == NULL) {
					e_text = 
				       "%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				strcpy(sp[i],NhlmpNULLAREA);
			}
		}

	}
		
/*
 * Mask outline specifiers
 */
	ga = init ? NULL : ompp->mask_outline_specs;

        if (! mpp->mask_outline_specs && ga) {
               NhlFreeGenArray(ga);
               ompp->mask_outline_specs = (NhlGenArray)0xdeadbeef;
        }
	else if (ga != mpp->mask_outline_specs) {
		NhlFreeGenArray(ga);
		if ((ga = _NhlCopyGenArray(mpp->mask_outline_specs,
					   True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpMaskOutlineSpecifiers);
			return NhlFATAL;
		}
		mpp->mask_outline_specs = ga;
		ompp->mask_outline_specs = NULL;
		/* Check elements for null strings */
		sp = (NhlString *) mpp->mask_outline_specs->data;
		for (i = 0; i < mpp->mask_outline_specs->num_elements; i++) {
			if (sp[i] == NULL) {
				e_text = 
		 "%s: Null or zero length %s string for index %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNmpMaskOutlineSpecifiers,
					  i,NhlmpNULLAREA);
				ret = MIN(ret,NhlWARNING);
				if (sp[i] != NULL) NhlFree(sp[i]);
				sp[i] = NhlMalloc(strlen(NhlmpNULLAREA) + 1);
				if (sp[i] == NULL) {
					e_text = 
				       "%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				strcpy(sp[i],NhlmpNULLAREA);
			}
		}

	}
		
/*
 * Outline specifiers
 */
	ga = init ? NULL : ompp->outline_specs;

        if (! mpp->outline_specs && ga) {
               NhlFreeGenArray(ga);
               ompp->outline_specs = (NhlGenArray)0xdeadbeef;
        }
	else if (ga != mpp->outline_specs) {
		NhlFreeGenArray(ga);
		if ((ga = _NhlCopyGenArray(mpp->outline_specs,
					   True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpOutlineSpecifiers);
			return NhlFATAL;
		}
		mpp->outline_specs = ga;
		ompp->outline_specs = NULL;
		/* Check elements for null strings */
		sp = (NhlString *) mpp->outline_specs->data;
		for (i = 0; i < mpp->outline_specs->num_elements; i++) {
			if (sp[i] == NULL) {
				e_text = 
		 "%s: Null or zero length %s string for index %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNmpOutlineSpecifiers,
					  i,NhlmpNULLAREA);
				ret = MIN(ret,NhlWARNING);
				if (sp[i] != NULL) NhlFree(sp[i]);
				sp[i] = NhlMalloc(strlen(NhlmpNULLAREA) + 1);
				if (sp[i] == NULL) {
					e_text = 
				       "%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				strcpy(sp[i],NhlmpNULLAREA);
			}
		}
	}

/*
 * The specified fill colors may be indexed either to the fill groups
 * or directly to the hlu color index depending on the setting of 
 * mpSpecifiedFillDirectIndexing. Issue a warning if the number of elements is
 * not the same as the elements in the fill_area_specs list.
 */		

	ga = init ? NULL : ompp->spec_fill_colors;

        if (! mpp->spec_fill_colors && ga) {
                mpp->spec_fill_color_count = 0;
                NhlFreeGenArray(ga);
                ompp->spec_fill_colors = (NhlGenArray)0xdeadbeef;
        }
	else if (ga != mpp->spec_fill_colors) {
		int max_val,min_val;
                NhlFreeGenArray(ga);
                ga = _NhlCopyGenArray(mpp->spec_fill_colors,True);
                if (ga == NULL) {
                        e_text = "%s: error copying %s GenArray";
                        NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                  entry_name,
                                  NhlNmpSpecifiedFillColors);
                        return NhlFATAL;
                }
                ompp->spec_fill_colors = NULL;
                mpp->spec_fill_colors = ga;
                mpp->spec_fill_color_count
                        = mpp->spec_fill_colors->num_elements;
		if (mpp->spec_fill_direct) {
                        min_val = NhlUNSPECIFIEDCOLOR;
                }
		else {
                        min_val = 0;
                }
                ip = (int *) mpp->spec_fill_colors->data; 
		for (i = 0; i < mpp->spec_fill_color_count; i++) {
                        if (ip[i] == NhlUNSPECIFIEDCOLOR)
                                continue;
			if (ip[i] > INT_MAX || ip[i] < min_val) {
				e_text = 
	         "%s: %s index %d holds an invalid color index: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNmpSpecifiedFillColors,i);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlUNSPECIFIEDCOLOR;
			}
		}
        }

/*
 * The specified fill patterns may be indexed either to the fill groups
 * or directly to the hlu color index depending on the setting of 
 * mpSpecifiedFillDirectIndexing. Issue a warning if the number of elements is
 * not the same as the elements in the fill_area_specs list.
 */		

	ga = init ? NULL : ompp->spec_fill_patterns;
        if (! mpp->spec_fill_patterns && ga) {
                mpp->spec_fill_pattern_count = 0;
                NhlFreeGenArray(ga);
                ompp->spec_fill_patterns = (NhlGenArray)0xdeadbeef;
        }
	else if (ga != mpp->spec_fill_patterns) {
		int max_val,min_val;
                NhlFreeGenArray(ga);
                ga = _NhlCopyGenArray(mpp->spec_fill_patterns,True);
                if (ga == NULL) {
                        e_text = "%s: error copying %s GenArray";
                        NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
                                  NhlNmpSpecifiedFillPatterns);
                        return NhlFATAL;
                }
                ompp->spec_fill_patterns = NULL;
                mpp->spec_fill_patterns = ga;
                mpp->spec_fill_pattern_count = 
                        mpp->spec_fill_patterns->num_elements;
		if (mpp->spec_fill_direct) {
			NhlVAGetValues(mpnew->base.wkptr->base.id,
				       NhlNwkFillTableLength, &max_val, NULL);
                        min_val = NhlUNSPECIFIEDFILL;
                }
		else {
			max_val = mpp->area_group_count - 1;
                        min_val = 0;
                }

		ip = (int *) mpp->spec_fill_patterns->data; 
		for (i = 0; i < mpp->spec_fill_pattern_count; i++) {
                        if (ip[i] == NhlUNSPECIFIEDFILL)
                                continue;
			if (ip[i] > max_val || ip[i] < min_val) {
				e_text = 
	         "%s: %s index %d holds an invalid pattern index: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNmpSpecifiedFillPatterns,i);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlUNSPECIFIEDFILL;
			}
		}
	}

/*
 * The specified fill scales may be indexed either to the fill groups
 * or directly to the hlu scale index depending on the setting of 
 * mpSpecifiedFillDirectIndexing. Issue a warning if the number of elements is
 * not the same as the elements in the fill_area_specs list.
 */		


	ga = init ? NULL : ompp->spec_fill_scales;

        if (! mpp->spec_fill_scales && ga) {
                mpp->spec_fill_scale_count = 0;
                NhlFreeGenArray(ga);
                ompp->spec_fill_scales = (NhlGenArray)0xdeadbeef;
        }
	else if (ga != mpp->spec_fill_scales) {
		float max_val;
                NhlFreeGenArray(ga);
                ga = _NhlCopyGenArray(mpp->spec_fill_scales,True);
                if (ga == NULL) {
                        e_text = "%s: error copying %s GenArray";
                        NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
                                  NhlNmpSpecifiedFillScales);
                        return NhlFATAL;
                }
                ompp->spec_fill_scales = NULL;
                mpp->spec_fill_scales = ga;
                mpp->spec_fill_scale_count = 
                        mpp->spec_fill_scales->num_elements;
		fp = (float *) mpp->spec_fill_scales->data; 
		if (! mpp->spec_fill_direct) {
			max_val = mpp->area_group_count - 1;
                }
		else {
			max_val = FLT_MAX;
                }
                
		for (i = 0; i < mpp->spec_fill_scale_count; i++) {
                        if (fp[i] == NhlmpUNSETFILLSCALE)
                                continue;
			if (fp[i] > max_val || fp[i] < 0.0) {
				e_text = 
	           "%s: %s index %d holds an invalid scale: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNmpSpecifiedFillScales,i);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlmpUNSETFILLSCALE;
			}
		}
	}

	return ret;
}

/*
 * Function:    mpManageGenArray
 *
 * Description:	Handles details of managing a GenArray
 *
 * In Args:	count		number of elements to create in the GenArray
 *		copy_ga 	GenArray to copy values from - if
 *				NULL it is ignored.
 *		type		type of GenArray to create - int,float,string
 *		*init_val	if non-null an initialization value to use -
 *				strings have the array index appended
 *		resource_name	name of the GenArray resource 		
 *		entry_name	name of the high level caller of the routine 
 *
 * Out Args:	*ga		If non-NULL on input, contains a previously
 *				allocated GenArray, whose data will be 
 *				replaced if necessary.
 *				Out: An allocated GenArray with allocated data
 *		*old_count	the previous count in the old gen array
 *		*init_count	number of values initialized - if init_val is
 *				non-NULL, will contain count; if init_val is
 *				NULL will contain MIN(count,number of 
 *				elements in copy_ga); if copy_ga is also NULL
 *				will contain 0.
 *		*need_check     True if a GenArray copy occurs or the number
 *				of elements increases and no initialization
 *				value is supplied. False otherwise.
 *		*changed	True if the data has been modified in any way.
 *
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via MapPlotSetValues
 */

/*ARGSUSED*/
static NhlErrorTypes    mpManageGenArray
#if	NhlNeedProto
	(NhlGenArray	*ga,
	 ng_size_t	count,
	 NhlGenArray	copy_ga,
	 NrmQuark	type,
	 NhlPointer	init_val,
	 ng_size_t	*old_count,
	 ng_size_t	*init_count,
	 NhlBoolean	*need_check,
	 NhlBoolean	*changed,
	 NhlString	resource_name,
	 NhlString	entry_name)
#else
(ga,count,copy_ga,type,init_val,old_count,init_count,
 need_check,changed,resource_name,entry_name)
	NhlGenArray	*ga;
	ng_size_t	count;
	NhlGenArray	copy_ga;
	NrmQuark	type;
	NhlPointer	init_val;
	ng_size_t       *old_count;
	ng_size_t	*init_count;
	NhlBoolean	*need_check;
	NhlBoolean	*changed;
	NhlString	resource_name;
	NhlString	entry_name;
#endif
{
	char		*str_type;
	NhlErrorTypes	ret = NhlNOERROR;
	int		i, size;
	NhlPointer	datap;
	char		*e_text;

	*init_count = 0;
	*need_check = False;
	*changed = False;
	*old_count = 0;

	if (type == Qint) {
		str_type = NhlTInteger;
		size = sizeof(int);
	}
	else if (type == Qfloat) {
		str_type = NhlTFloat;
		size = sizeof(float);
	}
	else if (type == Qstring) {
		str_type = NhlTString;
		size = sizeof(NhlString);
	}
	else {
		e_text = "%s: internal error; unsupported type for %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  resource_name);
		return NhlFATAL;
	}

	if (*ga != NULL) {
		datap = (*ga)->data;
		*old_count = (*ga)->num_elements;
		*init_count = *old_count;

		if (count > (*ga)->num_elements) {
			if ((datap = (NhlPointer)
			     NhlRealloc(datap, count * size)) == NULL) {
				e_text = "%s: error reallocating %s data";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,resource_name);
				return NhlFATAL;
			}
			memset((char*)datap + (*ga)->num_elements * size,0,
			       (count-(*ga)->num_elements) * size);
			(*ga)->data = datap;
			(*ga)->num_elements = count;
			*changed = True;
		}
		else if (*ga == copy_ga) {
			*init_count = (*ga)->num_elements;
			return ret;
		}
	}
	else {
		if ((datap = (NhlPointer) NhlMalloc(count * size)) == NULL) {
			e_text = "%s: error creating %s array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  resource_name);
			return NhlFATAL;
		}
		memset(datap,0,count * size);

		if ((*ga = NhlCreateGenArray((NhlPointer)datap,str_type,
					     size,1,&count)) == NULL) {
			e_text = "%s: error creating %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  resource_name);
			return NhlFATAL;
		}
		(*ga)->my_data = True;
		*changed = True;
	}

/* 
 * If there is a GenArray to copy, copy it; then initialize all remaining
 * uninitialized elements if an initialization value has been passed in.
 */

	if (copy_ga != NULL && copy_ga != *ga) {

		*need_check = True;
		ret = _NhlValidatedGenArrayCopy(ga,copy_ga,
                                                Nhl_mpMAX_AREA_GROUPS,
						True,False,resource_name, 
						entry_name);
		if (ret < NhlWARNING) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,
				  resource_name);
			return ret;
		}
		*init_count = copy_ga->num_elements;
		*changed = True;
	}

	if (*init_count < count) {

		if (init_val == NULL) {
			if (type == Qstring) {
				NhlString *sp = (NhlString *) datap;
				for (i = *init_count; i< count; i++) {
					if (i < *old_count) NhlFree(sp[i]);
					sp[i] = NULL;
				}
			}
			*need_check = True;
			return ret;
		}
		else if (type == Qint)
			for (i = *init_count; i< count; i++)
				((int *)datap)[i] = *((int *)init_val);
		else if (type == Qfloat)
			for (i = *init_count; i< count; i++)
				((float *)datap)[i] = *((float *)init_val);
		else if (type == Qstring) {
			char *sp;
			char *init_str = (char *) init_val;
			char numstr[10];
			for (i = *init_count; i< count; i++) {
				sprintf(numstr,"%d",i);
				if ((sp = (char *) 
				     NhlMalloc(sizeof(init_str)+
					       sizeof(numstr)+1)) == NULL) {
					e_text = "%s: error creating %s array";
					NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						  entry_name,resource_name);
					return NhlFATAL;
				}
				((char **)datap)[i] = sp;
				strcpy(sp,init_str);
				strcat(sp,numstr);
			}
		}
		*init_count = count;
		*changed = True;
	}

	return ret;
}


/*
 * Function:	mpSetUpDataHandler
 *
 * Description: Sets up the Map data handler object. The data handler
 *              used depends on the mpDataBaseVersion (and the resolution ??)
 *
 * In Args:	mpnew	new instance record
 *		mpold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
/*ARGSUSED*/
static NhlErrorTypes mpSetUpDataHandler
#if	NhlNeedProto
(
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else 

(mpnew,mpold,init,args,num_args)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlMapPlotLayerPart 	*mpp = &(mpnew->mapplot);
	NhlMapPlotLayerPart 	*ompp = &(mpold->mapplot);
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	char			buffer[_NhlMAXRESNAMLEN];
        int			tmpid;
        NhlSArg			sargs[32];
        int			nargs = 0;
        NhlClass		mapdh_class;

	entry_name = (init) ? "MapPlotInitialize" : "MapPlotSetValues";
        
        if (mpp->area_names)
                NhlSetSArg(&sargs[nargs++],NhlNmpAreaNames,mpp->area_names);
        if ((init && mpp->dynamic_groups) || 
	    _NhlArgIsSet(args,num_args,NhlNmpDynamicAreaGroups))
                NhlSetSArg(&sargs[nargs++],
                           NhlNmpDynamicAreaGroups,mpp->dynamic_groups);

	if (init ||
	    (!init && mpp->area_group_count != ompp->area_group_count))
                NhlSetSArg(&sargs[nargs++],
                           NhlNmpAreaGroupCount,mpp->area_group_count);
        
	if (! init && mpp->database_version != ompp->database_version) {
                NhlDestroy(mpp->map_data_handler->base.id);
                mpp->map_data_handler = NULL;
        }        
        else if (! init && mpp->database_version == NhlDYNAMIC_MAPS &&
                mpp->map_data_handler->base.layer_class == NhlmapV40DataHandlerClass &&
                mpp->outline_specs == NULL  && mpp->fill_area_specs == NULL && 
                mpp->mask_outline_specs == NULL && mpp->mask_area_specs == NULL &&
                (mpp->outline_boundaries != NhlGEOPHYSICAL || mpp->fill_boundaries != NhlGEOPHYSICAL))
        {
                /* "Dynamic" maps in effect, and we've previously defaulted to V40 database. But user 
                 * has specified something other than geophysical features, so we need to switch to 
                 * V41 database.
                 */
                NhlDestroy(mpp->map_data_handler->base.id);
                mpp->map_data_handler = NULL;            
        }
        
	if (! mpp->map_data_handler) {
                if (mpp->database_version == NhlDYNAMIC_MAPS) {
                    if ((mpp->outline_boundaries == NhlGEOPHYSICAL && mpp->fill_boundaries == NhlGEOPHYSICAL) ||
                        mpp->outline_specs != NULL || mpp->fill_area_specs != NULL || 
                        mpp->mask_outline_specs != NULL || mpp->mask_area_specs != NULL)
                        /* NCL-2126 */
                        mapdh_class = NhlmapV40DataHandlerClass;
                    else {
			mapdh_class = NhlmapV41DataHandlerClass;
			if (mpp->data_set_name)
				NhlSetSArg(&sargs[nargs++], NhlNmpDataSetName, mpp->data_set_name);
                    }
                }
                else if (mpp->database_version == NhlNCARG4_0) {
                    mapdh_class = NhlmapV40DataHandlerClass;
                }
		else if (mpp->database_version == NhlNCARG4_1) {
                    mapdh_class = NhlmapV41DataHandlerClass;
                    if (mpp->data_set_name)
			NhlSetSArg(&sargs[nargs++], NhlNmpDataSetName, mpp->data_set_name);
                }		
		else if (mpp->database_version == NhlRANGS_GSHHS) {
			mapdh_class = NhlmapRGDataHandlerClass;
			NhlSetSArg(&sargs[nargs++],
				   NhlNmpDataResolution,mpp->data_resolution);
                }		
                
                sprintf(buffer,"%s",mpnew->base.name);
                strcat(buffer,".DataHandler");
                subret = NhlALCreate(&tmpid,buffer,mapdh_class,
                                     mpnew->base.id,sargs,nargs);
                if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

                mpp->map_data_handler = _NhlGetLayer(tmpid);

                if (! mpp->map_data_handler) {
                        e_text = "%s: Error creating map data handler object";
                        NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
                        return NhlFATAL;
                }
	}
        else {
		if (mpp->database_version == NhlRANGS_GSHHS) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNmpDataResolution,mpp->data_resolution);
		}
		subret = NhlALSetValues(mpp->map_data_handler->base.id,
                                        sargs,nargs);
	}
        
	return MIN(ret,subret);
}

/*
 * Function:	mpSetUpTransObj
 *
 * Description: Sets up the Map transformation object for the generic
 *		Map plot object. Note that since this trans object
 *		does not require any dynamic memory, the same trans object
 *		persists for the life of the MapPlot object.
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
/*ARGSUSED*/
static NhlErrorTypes mpSetUpTransObj
#if	NhlNeedProto
(
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init
)
#else 
(mpnew,mpold,init)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlTransformLayerPart	*tfp = &(mpnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[8];
        int			nargs = 0;
	NhlBoolean		trans_change_count;
	float			map_left,map_right,map_bottom,map_top;
	NhlBoolean		preserve_aspect = True;


	entry_name = (init) ? "MapPlotInitialize" : "MapPlotSetValues";
/*
 * Since the Map Plot only uses one transformation a MapTransObj only
 * needs to be created once. It will not be freed until the object
 * is destroyed. For now all map trans resources are simply passed through.
 */
	if (init || tfp->trans_obj == NULL) {

		mpnew->mapplot.new_draw_req = True;
		sprintf(buffer,"%s",mpnew->base.name);
		strcat(buffer,".Trans");

		if (mpnew->mapplot.shape_mode == NhlFREEASPECT)
			preserve_aspect = False;

		subret = _NhlVACreateChild(&tmpid,buffer,
					   NhlmapTransObjClass,
					   (NhlLayer) mpnew, 
                                           NhlNtrDataXStartF,tfp->data_xstart,
                                           NhlNtrDataXEndF,tfp->data_xend,
                                           NhlNtrDataYStartF,tfp->data_ystart,
                                           NhlNtrDataYEndF,tfp->data_yend,
					   NhlNmpPreserveAspectRatio,
					   preserve_aspect,
					   NhlNtrLineInterpolationOn, tfp->line_interpolation_on,
					   NULL);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

	}
	else {
                    /*
                     * Note that MapPlot never changes the data limits
                     * itself, but plot objects must update the data limits
                     * each time they draw in order to ensure that MapTransObj
                     * is operating with the correct longitude cycle.
                     */
                
		if (mpnew->mapplot.shape_mode != mpold->mapplot.shape_mode) {
			if (mpnew->mapplot.shape_mode == NhlFREEASPECT)
				preserve_aspect = False;
			NhlSetSArg(&sargs[nargs++],
				   NhlNmpPreserveAspectRatio,preserve_aspect);
		}
		if (tfp->line_interpolation_on != mpold->trans.line_interpolation_on)
			NhlSetSArg(&sargs[nargs++],NhlNtrLineInterpolationOn,
				   tfp->line_interpolation_on);

		subret = _NhlALSetValuesChild(tfp->trans_obj->base.id,
					      (NhlLayer) mpnew,sargs,nargs);

		if (mpnew->view.x != mpold->view.x ||
		    mpnew->view.y != mpold->view.y ||
		    mpnew->view.height != mpold->view.height ||
		    mpnew->view.width != mpold->view.width)
			_NhlSetTrans(tfp->trans_obj,
				     tfp->trans_obj->base.parent);
	}

	NhlVAGetValues(tfp->trans_obj->base.id,
		       NhlNmpLeftMapPosF, &map_left,
		       NhlNmpRightMapPosF, &map_right,
		       NhlNmpBottomMapPosF, &map_bottom,
		       NhlNmpTopMapPosF, &map_top,
		       NhlNtrChangeCount,&trans_change_count,
		       NULL);

	if ((mpnew->mapplot.shape_mode == NhlFIXEDASPECTFITBB) && 
	    (_NhlCmpFAny(mpnew->view.x,map_left,6) != 0.0 ||
	     _NhlCmpFAny(mpnew->view.y,map_top,6) != 0.0 ||
	     _NhlCmpFAny(mpnew->view.width,map_right - map_left,6) != 0.0 ||
	     _NhlCmpFAny(mpnew->view.height,map_top - map_bottom,6) != 0.0)) {

		_NhlInternalSetView((NhlViewLayer)mpnew,
				    map_left,map_top,
				    map_right - map_left,
				    map_top - map_bottom, False);

	}
		
	if (trans_change_count > mpnew->mapplot.trans_change_count) {
		mpnew->mapplot.trans_change_count = trans_change_count;
		mpnew->mapplot.update_req = True;
		mpnew->mapplot.new_draw_req = True;
	}

	return MIN(ret,subret);

}

typedef enum _coordStatus {
	_OUTOFRANGE, _INCREASING, _DECREASING,_DEFERRED
} coordStatus;

typedef struct _tickStatus {
	int index;
	coordStatus status;
} tickStatus;
 
/*
 * Function:	GetXAxisTicks
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
static NhlErrorTypes GetXAxisTicks
#if	NhlNeedProto
(
	NhlTransformLayerPart	*tfp,
	float lx,
	float rx,
	float y,
	ng_size_t *count,
	float **values,
	NhlString **labels
)
#else 
(tfp,lx,rx,y,count,values,labels)
	NhlTransformLayerPart	*tfp;
	float lx;
	float rx;
	float y;
	ng_size_t *count;
	float **values;
	NhlString **labels;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	double lon[101],lat[101],xw[101];
	double minlon,maxlon,minlat,maxlat;
	tickStatus ts[32];
	double seg_nice[32];
	coordStatus status,cstatus[32];
	int six[32],eix[32];
	int isl[32],iel[32];
	int seg_count;
	int statix;
	double max_angle,nice;
	int i,j;
	double dlx,drx,dy;
	double rlon,rlat,xwin,ywin;
	char label[128];
	int nchr;
	ng_size_t size;
	int start_ix, end_ix;
	float angle_div;
	NhlBoolean dolat = False;
	ng_size_t max_count;
	int max_seg;
	int jstart,jend,jsign;

	*values = NULL;
	*labels = NULL;
	*count = 0;
	dlx = (double) lx;
	drx = (double) rx;
	dy = (double) y;

	xw[0] = dlx;
	c_mdptri(xw[0],dy,&lat[0],&lon[0]);
	ts[0].index = 0;
	if (lat[0] > 1e11) {
		ts[0].status = _OUTOFRANGE;
		minlon = minlat = 9999;
		maxlon = maxlat = -9999;
	}
	else {
		ts[0].status = _DEFERRED;
		minlon = maxlon = lon[0];
		minlat = maxlat = lat[0];
	}
	statix = 0;
	for (i = 1; i < 101; i++) {
		xw[i] = dlx + (double) (i) * 0.01 * (drx-dlx);
		c_mdptri(xw[i],dy,
			 &lat[i],&lon[i]);
		if (lat[i] > 1e11)
			continue;
		if (lat[i-1] < 1e11 && fabs(lon[i] - lon[i-1]) > 180.0) {
			lon[i] += (lon[i-1] - lon[i] > 0.0 ? 360.0 : -360.0);
		}
		if (minlon > lon[i]) minlon = lon[i];
		if (maxlon < lon[i]) maxlon = lon[i];
		if (minlat > lat[i]) minlat = lat[i];
		if (maxlat < lat[i]) maxlat = lat[i];
	}
	if (1.45 * (maxlon - minlon) < (maxlat - minlat))
		dolat = True;
	
	for (i = 1; i < 101; i++) {
		if (lat[i] > 1e11) {
			switch (ts[statix].status) {
			case _OUTOFRANGE:
				break;
			case _INCREASING:
			case _DECREASING:
				statix++;
				ts[statix].index = i;
				ts[statix].status = _OUTOFRANGE;
				break;
			case _DEFERRED:
				ts[statix].status = _OUTOFRANGE;
				break;
			}
		}
		else {
			double ang0,ang1;
			if (dolat) {
				ang0 = lat[i-1];
				ang1 = lat[i];
			}
			else {
				ang0 = lon[i-1];
				ang1 = lon[i];
			}
			switch (ts[statix].status) {
			case _OUTOFRANGE:
				statix++;
				ts[statix].index = i;
				ts[statix].status = _DEFERRED;
				continue;
			case _INCREASING:
				if (ang1 < ang0) {
					statix++;
					ts[statix].index = i;
					ts[statix].status = _DECREASING;
				}
				break;
			case _DECREASING:
				if (ang1 > ang0) {
					statix++;
					ts[statix].index = i;
					ts[statix].status = _INCREASING;
				}
				break;
			case _DEFERRED:
				if (ang1 > ang0) {
					ts[statix].status = _INCREASING;
				}
				else {
					ts[statix].status = _DECREASING;
				}
			}
		}
	}	
	
	start_ix = end_ix = -1;
	seg_count = 0;
	max_count = -999;
	size = 0;
	status = _OUTOFRANGE;

	for (i = 0; i <= statix; i++) {
		if (ts[i].status == _DEFERRED)
			ts[i].status = _OUTOFRANGE;
		if (ts[i].status == _OUTOFRANGE) {
			if (start_ix == -1)
				continue;
			if (ts[i].index - start_ix < 15) {
				start_ix = -1;
				continue;
			}
			end_ix = ts[i].index - 1;
		}
		else if (ts[i].status == _INCREASING ||
			 ts[i].status == _DECREASING) {
			if (start_ix == -1) {
				start_ix = ts[i].index;
				status = ts[i].status;
				continue;
			}
			else if (ts[i].index - start_ix < 15) {
				start_ix = ts[i].index;
				continue;
			}
			end_ix = ts[i].index - 1;
		}
		if (end_ix == start_ix) 
			continue;
		angle_div = (end_ix - start_ix) / 25.0;
		if (dolat) {
			max_angle = 
				fabs((lat[end_ix] - lat[start_ix])/ angle_div);
			NGCALLF(mdgnin,MDGNIN)(&max_angle,&nice);
			if (nice <= 0) {
				start_ix = ts[i].index;
				status = ts[i].status;
				continue;
			}
			isl[seg_count] = (int) 
				((lat[start_ix] + 90.0) / nice);
			iel[seg_count] =  (int)
				((lat[end_ix] + 90.0) / nice);
		}
		else {
			max_angle = 
				fabs((lon[end_ix] - lon[start_ix])/ angle_div);
			NGCALLF(mdgnin,MDGNIN)(&max_angle,&nice);
			if (nice <= 0) {
				start_ix = ts[i].index;
				status = ts[i].status;
				continue;
			}
			isl[seg_count] = (int) 
				((lon[start_ix] + 180.0) / nice);
			iel[seg_count] =  (int)
				((lon[end_ix] + 180.0) / nice);
		}
		seg_nice[seg_count] = nice;
		cstatus[seg_count] = status;
		six[seg_count] =  start_ix;
		eix[seg_count] =  end_ix;
		if (max_count < end_ix - start_ix) {
			max_count = end_ix - start_ix;
			max_seg = seg_count;
		}
		size += 4 + MAX(isl[seg_count],iel[seg_count]) -
			MIN(isl[seg_count],iel[seg_count]);
		seg_count++;
		start_ix = ts[i].status == _OUTOFRANGE ? -1 : ts[i].index;
		status = ts[i].status;
		end_ix = -1;
	}
	if (start_ix > -1 && start_ix < 86 && end_ix == -1) {
		end_ix = 100;
		angle_div = (end_ix - start_ix) / 25.0;
		if (dolat) {
			max_angle = 
				fabs((lat[end_ix] - lat[start_ix])/ angle_div);
			NGCALLF(mdgnin,MDGNIN)(&max_angle,&nice);
			if (nice > 0) {
				isl[seg_count] = (int) 
					((lat[start_ix] + 90.0) / nice);
				iel[seg_count] =  (int)
					((lat[end_ix] + 90.0) / nice);
			}
		}
		else {
			max_angle = 
				fabs((lon[end_ix] - lon[start_ix])/ angle_div);
			NGCALLF(mdgnin,MDGNIN)(&max_angle,&nice);
			if (nice > 0) {
				isl[seg_count] = (int) 
					((lon[start_ix] + 180.0) / nice);
				iel[seg_count] =  (int)
					((lon[end_ix] + 180.0) / nice);
			}
		}
		if (nice > 0) {
			seg_nice[seg_count] = nice;
			cstatus[seg_count] = status;
			six[seg_count] =  start_ix;
			eix[seg_count] =  end_ix;
			if (max_count < end_ix - start_ix) {
				max_count = end_ix - start_ix;
				max_seg = seg_count;
			}
			size += 4 + MAX(isl[seg_count],iel[seg_count]) -
				MIN(isl[seg_count],iel[seg_count]);
			seg_count++;
		}
	}

	if (size <= 0) {
		return ret;
	}
		
	*values = NhlMalloc(size * sizeof(float));
	*labels = NhlMalloc(size * sizeof(NhlString));

	nice = seg_nice[max_seg];
	if (dolat) {
		for (i = 0; i < seg_count; i++) {
			if (cstatus[i] == _DECREASING) {
				jstart = MAX(isl[i],iel[i]) + 2;
				jend = MIN(isl[i],iel[i]) - 2;
				jsign = -1;
			} 
			else {
				jstart = MIN(isl[i],iel[i]) - 2;
				jend = MAX(isl[i],iel[i]) + 2;
				jsign = 1;
			}
			for (j = jstart; j != jend; j += jsign) {
				rlat = (double) j * nice - 90.0;

				if ((rlat > MIN(lat[six[i]],lat[eix[i]]) 
				     - nice / 100.0) &&
				    (rlat < MAX(lat[six[i]],lat[eix[i]]) + 
				     nice / 100.0)) {
					NGCALLF(mdilat,MDILAT)
						(&(lat[six[i]]),&(lon[six[i]]),
						 &(xw[six[i]]),&dy,
						 &(lat[eix[i]]),&(lon[eix[i]]),
						 &(xw[eix[i]]),&dy,
						 &rlat,&xwin,&ywin);
					NGCALLF(mdlach,MDLACH)
						(&rlat,label,&nchr,128);
					label[nchr] = '\0';
					(*values)[*count] = xwin;
					(*labels)[*count] = NhlMalloc(nchr+1);
					strcpy((*labels)[*count],label);
					(*count)++;
				}
			}
		}
		return ret;
	}
	for (i = 0; i < seg_count; i++) {
		if (cstatus[i] == _DECREASING) {
			jstart = MAX(isl[i],iel[i]) + 2;
			jend = MIN(isl[i],iel[i]) - 2;
			jsign = -1;
		} 
		else {
			jstart = MIN(isl[i],iel[i]) - 2;
			jend = MAX(isl[i],iel[i]) + 2;
			jsign = 1;
		}
		for (j = jstart; j != jend; j += jsign) {
			rlon = (double) j * nice - 180.0;

			if ((rlon > MIN(lon[six[i]],lon[eix[i]]) 
			     - nice / 100.0) &&
			    (rlon < MAX(lon[six[i]],lon[eix[i]]) + 
			     nice / 100.0)) {
				NGCALLF(mdilon,MDILON)
					(&(lat[six[i]]),&(lon[six[i]]),
					 &(xw[six[i]]),&dy,
					 &(lat[eix[i]]),&(lon[eix[i]]),
					 &(xw[eix[i]]),&dy,
					 &rlon,&xwin,&ywin);
				NGCALLF(mdloch,MDLOCH)
					(&rlon,label,&nchr,128);
				label[nchr] = '\0';
									      
				(*values)[*count] = xwin;
				(*labels)[*count] = NhlMalloc(nchr+1);
				strcpy((*labels)[*count],label);
				(*count)++;
			}
		}
	}
	return ret;
}

/*
 * Function:	GetYAxisTicks
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
static NhlErrorTypes GetYAxisTicks
#if	NhlNeedProto
(
	NhlTransformLayerPart	*tfp,
	float by,
	float ty,
	float x,
	ng_size_t *count,
	float **values,
	NhlString **labels
)
#else 
(tfp,by,ty,x,count,values,labels)
	NhlTransformLayerPart	*tfp;
	float by;
	float ty;
	float x;
	ng_size_t *count;
	float **values;
	NhlString **labels;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	double lon[101],lat[101],yw[101];
	double minlon,maxlon,minlat,maxlat;
	tickStatus ts[32];
	double seg_nice[32];
	coordStatus status,cstatus[32];
	int six[32],eix[32];
	int isl[32],iel[32];
	int seg_count;
	int statix;
	double max_angle,nice;
	int i,j;
	double dby,dty,dx;
	double rlon,rlat,xwin,ywin;
	char label[128];
	int nchr;
	int size;
	int start_ix, end_ix;
	float angle_div;
	NhlBoolean dolon = False;
	int max_count;
	int max_seg;
	int jstart,jend,jsign;

	*values = NULL;
	*labels = NULL;
	*count = 0;
	dby = (double) by;
	dty = (double) ty;
	dx = (double) x;

	yw[0] = dby;
	c_mdptri(dx,yw[0],&lat[0],&lon[0]);
	ts[0].index = 0;
	if (lat[0] > 1e11) {
		ts[0].status = _OUTOFRANGE;
		minlon = minlat = 9999;
		maxlon = maxlat = -9999;
	}
	else {
		ts[0].status = _DEFERRED;
		minlon = maxlon = lon[0];
		minlat = maxlat = lat[0];
	}
	statix = 0;
	for (i = 1; i < 101; i++) {
		yw[i] = dby + (double) (i) * 0.01 * (dty-dby);
		c_mdptri(dx,yw[i],
			 &lat[i],&lon[i]);
		if (lat[i] > 1e11)
			continue;
		if (lat[i-1] < 1e11 && fabs(lon[i] - lon[i-1]) > 180.0) {
			lon[i] += (lon[i-1] - lon[i] > 0.0 ? 360.0 : -360.0);
		}
		if (minlon > lon[i]) minlon = lon[i];
		if (maxlon < lon[i]) maxlon = lon[i];
		if (minlat > lat[i]) minlat = lat[i];
		if (maxlat < lat[i]) maxlat = lat[i];
	}
	if (1.45 * (maxlat - minlat) < (maxlon - minlon))
		dolon = True;
	
	for (i = 1; i < 101; i++) {
		if (lat[i] > 1e11) {
			switch (ts[statix].status) {
			case _OUTOFRANGE:
				break;
			case _INCREASING:
			case _DECREASING:
				statix++;
				ts[statix].index = i;
				ts[statix].status = _OUTOFRANGE;
				break;
			case _DEFERRED:
				ts[statix].status = _OUTOFRANGE;
				break;
			}
		}
		else {
			double ang0,ang1;
			if (dolon) {
				ang0 = lon[i-1];
				ang1 = lon[i];
			}
			else {
				ang0 = lat[i-1];
				ang1 = lat[i];
			}
			switch (ts[statix].status) {
			case _OUTOFRANGE:
				statix++;
				ts[statix].index = i;
				ts[statix].status = _DEFERRED;
				continue;
			case _INCREASING:
				if (ang1 < ang0) {
					statix++;
					ts[statix].index = i;
					ts[statix].status = _DECREASING;
				}
				break;
			case _DECREASING:
				if (ang1 > ang0) {
					statix++;
					ts[statix].index = i;
					ts[statix].status = _INCREASING;
				}
				break;
			case _DEFERRED:
				if (ang1 > ang0) {
					ts[statix].status = _INCREASING;
				}
				else {
					ts[statix].status = _DECREASING;
				}
			}
		}
	}	
	
	start_ix = end_ix = -1;
	seg_count = 0;
	max_count = -999;
	size = 0;
	status = _OUTOFRANGE;

	for (i = 0; i <= statix; i++) {
		if (ts[i].status == _DEFERRED)
			ts[i].status = _OUTOFRANGE;
		if (ts[i].status == _OUTOFRANGE) {
			if (start_ix == -1)
				continue;
			if (ts[i].index - start_ix < 15) {
				start_ix = -1;
				continue;
			}
			end_ix = ts[i].index - 1;
		}
		else if (ts[i].status == _INCREASING ||
			 ts[i].status == _DECREASING) {
			if (start_ix == -1) {
				start_ix = ts[i].index;
				status = ts[i].status;
				continue;
			}
			else if (ts[i].index - start_ix < 15) {
				start_ix = ts[i].index;
				continue;
			}
			end_ix = ts[i].index - 1;
		}
		if (end_ix == start_ix) 
			continue;
		angle_div = (end_ix - start_ix) / 25.0;
		if (dolon) {
			max_angle = 
				fabs((lon[end_ix] - lon[start_ix])/ angle_div);
			NGCALLF(mdgnin,MDGNIN)(&max_angle,&nice);
			if (nice <= 0) {
				start_ix = ts[i].index;
				status = ts[i].status;
				continue;
			}
			isl[seg_count] = (int) 
				((lon[start_ix] + 180.0) / nice);
			iel[seg_count] =  (int)
				((lon[end_ix] + 180.0) / nice);
		}
		else {
			max_angle = 
				fabs((lat[end_ix] - lat[start_ix])/ angle_div);
			NGCALLF(mdgnin,MDGNIN)(&max_angle,&nice);
			if (nice <= 0) {
				start_ix = ts[i].index;
				status = ts[i].status;
				continue;
			}
			isl[seg_count] = (int) 
				((lat[start_ix] + 90.0) / nice);
			iel[seg_count] =  (int)
				((lat[end_ix] + 90.0) / nice);
		}
		seg_nice[seg_count] = nice;
		cstatus[seg_count] = status;
		six[seg_count] =  start_ix;
		eix[seg_count] =  end_ix;
		if (max_count < end_ix - start_ix) {
			max_count = end_ix - start_ix;
			max_seg = seg_count;
		}
		size += 4 + MAX(isl[seg_count],iel[seg_count]) -
			MIN(isl[seg_count],iel[seg_count]);
		seg_count++;
		start_ix = ts[i].status == _OUTOFRANGE ? -1 : ts[i].index;
		status = ts[i].status;
		end_ix = -1;
	}
	if (start_ix > -1 && start_ix < 86 && end_ix == -1) {
		end_ix = 100;
		angle_div = (end_ix - start_ix) / 25.0;
		if (dolon) {
			max_angle = 
				fabs((lon[end_ix] - lon[start_ix])/ angle_div);
			NGCALLF(mdgnin,MDGNIN)(&max_angle,&nice);
			if (nice > 0) {
				isl[seg_count] = (int) 
					((lon[start_ix] + 180.0) / nice);
				iel[seg_count] =  (int)
					((lon[end_ix] + 180.0) / nice);
			}
		}
		else {
			max_angle = 
				fabs((lat[end_ix] - lat[start_ix])/ angle_div);
			NGCALLF(mdgnin,MDGNIN)(&max_angle,&nice);
			if (nice > 0) {
				isl[seg_count] = (int) 
					((lat[start_ix] + 90.0) / nice);
				iel[seg_count] =  (int)
					((lat[end_ix] + 90.0) / nice);
			}
		}
		if (nice > 0) {
			seg_nice[seg_count] = nice;
			cstatus[seg_count] = status;
			six[seg_count] =  start_ix;
			eix[seg_count] =  end_ix;
			if (max_count < end_ix - start_ix) {
				max_count = end_ix - start_ix;
				max_seg = seg_count;
			}
			size += 4 + MAX(isl[seg_count],iel[seg_count]) -
				MIN(isl[seg_count],iel[seg_count]);
			seg_count++;
		}
	}

	if (size <= 0) {
		return ret;
	}
		
	*values = NhlMalloc(size * sizeof(float));
	*labels = NhlMalloc(size * sizeof(NhlString));

	nice = seg_nice[max_seg];
	if (dolon) {
		for (i = 0; i < seg_count; i++) {
			if (cstatus[i] == _DECREASING) {
				jstart = MAX(isl[i],iel[i]) + 2;
				jend = MIN(isl[i],iel[i]) - 2;
				jsign = -1;
			} 
			else {
				jstart = MIN(isl[i],iel[i]) - 2;
				jend = MAX(isl[i],iel[i]) + 2;
				jsign = 1;
			}
			for (j = jstart; j != jend; j += jsign) {
				rlon = (double) j * nice - 180.0;

				if ((rlon > MIN(lon[six[i]],lon[eix[i]]) 
				     - nice / 100.0) &&
				    (rlon < MAX(lon[six[i]],lon[eix[i]]) + 
				     nice / 100.0)) {
					NGCALLF(mdilon,MDILON)
						(&(lat[six[i]]),&(lon[six[i]]),
						 &dx,&(yw[six[i]]),
						 &(lat[eix[i]]),&(lon[eix[i]]),
						 &dx,&(yw[eix[i]]),
						 &rlon,&xwin,&ywin);
					NGCALLF(mdloch,MDLOCH)
						(&rlon,label,&nchr,128);
					label[nchr] = '\0';
					(*values)[*count] = ywin;
					(*labels)[*count] = NhlMalloc(nchr+1);
					strcpy((*labels)[*count],label);
					(*count)++;
				}
			}
		}
		return ret;
	}
	for (i = 0; i < seg_count; i++) {
		if (cstatus[i] == _DECREASING) {
			jstart = MAX(isl[i],iel[i]) + 2;
			jend = MIN(isl[i],iel[i]) - 2;
			jsign = -1;
		} 
		else {
			jstart = MIN(isl[i],iel[i]) - 2;
			jend = MAX(isl[i],iel[i]) + 2;
			jsign = 1;
		}
		for (j = jstart; j != jend; j += jsign) {
			rlat = (double) j * nice - 90.0;

			if ((rlat > MIN(lat[six[i]],lat[eix[i]]) 
			     - nice / 100.0) &&
			    (rlat < MAX(lat[six[i]],lat[eix[i]]) + 
			     nice / 100.0)) {
				NGCALLF(mdilat,MDILAT)
					(&(lat[six[i]]),&(lon[six[i]]),
					 &dx,&(yw[six[i]]),
					 &(lat[eix[i]]),&(lon[eix[i]]),
					 &dx,&(yw[eix[i]]),
					 &rlat,&xwin,&ywin);
				NGCALLF(mdlach,MDLACH)
					(&rlat,label,&nchr,128);
				label[nchr] = '\0';
									      
				(*values)[*count] = ywin;
				(*labels)[*count] = NhlMalloc(nchr+1);
				strcpy((*labels)[*count],label);
				(*count)++;
			}
		}
	}
	return ret;
}

/*
 * Function:	ManageTickMarks
 *
 * Description: If the MapPlot object has an overlay object attached, and
 *		the TickMarks are activated, manages the TickMark resources 
 *		relevant to the MapPlot object.
 *
 * In Args:	mpnew	new instance record
 *		mpold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageTickMarks
#if	NhlNeedProto
(
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(mpnew, mpold, init, sargs, nargs)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);
	NhlMapPlotLayerPart	*ompp = &(mpold->mapplot);
	NhlTransformLayerPart	*tfp = &(mpnew->trans);
	char		*e_text;
	float b,t,l,r;
	NhlString *xblabels = NULL, *xtlabels = NULL;
        NhlString *yllabels = NULL, *yrlabels = NULL;
	float *xbvalues = NULL, *xtvalues = NULL;
	float *ylvalues = NULL, *yrvalues = NULL;
	ng_size_t xbcount,xtcount,ylcount,yrcount;
	NhlBoolean usebottom = True, useleft = True;
	NhlBoolean xbon,ylon,xton,yron;
	NhlBoolean xb_labels_on,yl_labels_on,xt_labels_on,yr_labels_on;
	NhlBoolean update = False;
	int projection;
	int i;

	if (init) {
		mpp->xbvalues = NULL;
		mpp->xblabels = NULL;
		mpp->xtvalues = NULL;
		mpp->xtlabels = NULL;
		mpp->ylvalues = NULL;
		mpp->yllabels = NULL;
		mpp->yrvalues = NULL;
		mpp->yrlabels = NULL;
	}
 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (mpp->display_tickmarks == NhlNOCREATE) {
                if (init || ompp->display_tickmarks == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        mpp->display_tickmarks = NhlNEVER;
        }

	if (init || 
	    mpp->display_tickmarks != ompp->display_tickmarks) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmTickMarkDisplayMode,
				   mpp->display_tickmarks);
			if (mpp->display_tickmarks > NhlNEVER)
				update = True;
	}
	if ( mpp->xb_major_length_set
	     || mpp->xt_major_length_set
	     || mpp->yl_major_length_set
	     || mpp->yr_major_length_set
	     || mpp->xb_major_outward_length_set
	     || mpp->xt_major_outward_length_set
	     || mpp->yl_major_outward_length_set
	     || mpp->yr_major_outward_length_set
	     || mpp->xb_font_height_set
	     || mpp->xt_font_height_set
	     || mpp->yl_font_height_set
	     || mpp->yr_font_height_set
	     || mpp->update_req)
		update = True;

	if (init) {
		float delta;
		int set_count = 0;
		delta = (mpnew->view.width/NHL_DEFAULT_VIEW_WIDTH +
			 mpnew->view.height/NHL_DEFAULT_VIEW_HEIGHT) / 2.0;

		if (mpp->xb_major_length_set) 
			set_count++;
		else
			mpp->xb_major_length = 0.0;
		if (mpp->xt_major_length_set) 
			set_count++;
		else
			mpp->xt_major_length = 0.0;
		if (mpp->yl_major_length_set) 
			set_count++;
		else
			mpp->yl_major_length = 0.0;
		if (mpp->yr_major_length_set) 
			set_count++;
		else
			mpp->yr_major_length = 0.0;

		if (set_count == 0) {
			mpp->xb_major_length = 0.014 * delta;
			mpp->xt_major_length = 0.014 * delta;
			mpp->yl_major_length = 0.014 * delta;
			mpp->yr_major_length = 0.014 * delta;
		}
		else {
			/*
			 * Since all tickmarks should be the same length,
			 * take an average of those that are set. (Others
			 * are 0.0 at this point.)
			 */
			float len = 
				(mpp->xb_major_length +
				 mpp->xt_major_length +
				 mpp->yl_major_length +
				 mpp->yr_major_length) / (float)set_count;

			mpp->xb_major_length = len;
			mpp->xt_major_length = len;
			mpp->yl_major_length = len;
			mpp->yr_major_length = len;
		}

		set_count = 0;
		if (mpp->xb_major_outward_length_set) 
			set_count++;
		else
			mpp->xb_major_outward_length = 0.0;

		if (mpp->xt_major_outward_length_set) 
			set_count++;
		else
			mpp->xt_major_outward_length = 0.0;
		if (mpp->yl_major_outward_length_set) 
			set_count++;
		else
			mpp->yl_major_outward_length = 0.0;
		if (mpp->yr_major_outward_length_set) 
			set_count++;
		else
			mpp->yr_major_outward_length = 0.0;

		/*
		 * the default outward length is the same as the 
		 * major length which puts the whole tickmark outside the
		 * plot viewport.
		 */
		if (set_count == 0) {
			mpp->xb_major_outward_length = mpp->xb_major_length;
			mpp->xt_major_outward_length = mpp->xt_major_length;
			mpp->yl_major_outward_length = mpp->yl_major_length;
			mpp->yr_major_outward_length = mpp->yr_major_length;
		}
		
		else {
			/*
			 * Since the outward length should be the same for
			 * all take an average of those that are set. (Others
			 * are 0.0 at this point.)
			 */
			float len = 
				(mpp->xb_major_outward_length +
				 mpp->xt_major_outward_length +
				 mpp->yl_major_outward_length +
				 mpp->yr_major_outward_length) 
				/ (float)set_count;

			mpp->xb_major_outward_length = len;
			mpp->xt_major_outward_length = len;
			mpp->yl_major_outward_length = len;
			mpp->yr_major_outward_length = len;
		}
		
		set_count = 0;
		if (mpp->xb_font_height_set) 
			set_count++;
		else
			mpp->xb_font_height = 0.0;
		if (mpp->xt_font_height_set) 
			set_count++;
		else
			mpp->xt_font_height = 0.0;
		if (mpp->yl_font_height_set) 
			set_count++;
		else
			mpp->yl_font_height = 0.0;
		if (mpp->yr_font_height_set) 
			set_count++;
		else
			mpp->yr_font_height = 0.0;
				
		if (set_count == 0) {
			mpp->xb_font_height = 0.014 * delta;
			mpp->xt_font_height = 0.014 * delta;
			mpp->yl_font_height = 0.014 * delta;
			mpp->yr_font_height = 0.014 * delta;
		}
		else {
			/*
			 * Since all tickmarks should be the same length,
			 * take an average of those that are set. (Others
			 * are 0.0 at this point.)
			 */
			float hgt = 
				(mpp->xb_font_height +
				 mpp->xt_font_height +
				 mpp->yl_font_height +
				 mpp->yr_font_height) / (float)set_count;

			mpp->xb_font_height = hgt;
			mpp->xt_font_height = hgt;
			mpp->yl_font_height = hgt;
			mpp->yr_font_height = hgt;
		}
	}
	else {
		float deltax,deltay;
		deltax = mpnew->view.width/mpold->view.width;
		deltay = mpnew->view.height/mpold->view.height;
		if (_NhlCmpFAny2(deltax,1.0,4,_NhlMIN_NONZERO) != 0.0) {
			if (! mpp->yl_major_length_set)
				mpp->yl_major_length *= deltax;
			if (! mpp->yr_major_length_set)
				mpp->yr_major_length *= deltax;
			if (! mpp->yl_major_outward_length_set)
				mpp->yl_major_outward_length *= deltax;
			if (! mpp->yr_major_outward_length_set)
				mpp->yr_major_outward_length *= deltax;
			if (! mpp->xb_font_height_set)
				mpp->xb_font_height *= deltax;
			if (! mpp->xt_font_height_set)
				mpp->xt_font_height *= deltax;
		}
		if (_NhlCmpFAny2(deltay,1.0,4,_NhlMIN_NONZERO) != 0.0) {
			if (! mpp->xb_major_length_set)
				mpp->xb_major_length *= deltay;
			if (! mpp->xt_major_length_set)
				mpp->xt_major_length *= deltay;
			if (! mpp->xb_major_outward_length_set)
				mpp->xb_major_outward_length *= deltay;
			if (! mpp->xt_major_outward_length_set)
				mpp->xt_major_outward_length *= deltay;
			if (! mpp->yl_font_height_set)
				mpp->yl_font_height *= deltay;
			if (! mpp->yr_font_height_set)
				mpp->yr_font_height *= deltay;
		}
		if (mpp->label_auto_stride != ompp->label_auto_stride)
			update = True;
	}

	if (! (init || update))
		return ret;

	if (mpp->xbvalues)
		NhlFreeGenArray(mpp->xbvalues);
	if (mpp->xblabels)
		NhlFreeGenArray(mpp->xblabels);
	if (mpp->xtvalues)
		NhlFreeGenArray(mpp->xtvalues);
	if (mpp->xtlabels)
		NhlFreeGenArray(mpp->xtlabels);
	if (mpp->ylvalues)
		NhlFreeGenArray(mpp->ylvalues);
	if (mpp->yllabels)
		NhlFreeGenArray(mpp->yllabels);
	if (mpp->yrvalues)
		NhlFreeGenArray(mpp->yrvalues);
	if (mpp->yrlabels)
		NhlFreeGenArray(mpp->yrlabels);
		
	subret = _NhlSetTrans((NhlLayer)tfp->trans_obj,(NhlLayer)mpnew);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error setting transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text);
		return NhlFATAL;
	}

	NhlVAGetValues(tfp->trans_obj->base.id,
		       NhlNmpBottomWindowF,&b,
		       NhlNmpTopWindowF,&t,
		       NhlNmpLeftWindowF,&l,
		       NhlNmpRightWindowF,&r,
		       NhlNmpProjection,&projection,
		       NULL);
/*
 * kludge fix for precision problem in mercator and robinson limits
 */
	if (projection == NhlMERCATOR) {
		float pi_single = 3.14159;
		l = MAX(l,-pi_single);
		b = MAX(b,-pi_single);
		r = MIN(r,pi_single);
		t = MIN(t,pi_single);
	}
	else if (projection == NhlROBINSON) {
		float vlim = 0.507199;
		b = MAX(b,-vlim);
		t = MIN(t,vlim);
	}
	subret = GetXAxisTicks(tfp,l,r,b,&xbcount,&xbvalues,&xblabels);
	ret = MIN(subret,ret);
	subret = GetXAxisTicks(tfp,l,r,t,&xtcount,&xtvalues,&xtlabels);
	ret = MIN(subret,ret);
	subret = GetYAxisTicks(tfp,b,t,l,&ylcount,&ylvalues,&yllabels);
	ret = MIN(subret,ret);
	subret = GetYAxisTicks(tfp,b,t,r,&yrcount,&yrvalues,&yrlabels);
	ret = MIN(subret,ret);
	
	NhlSetSArg(&sargs[(*nargs)++],NhlNtmEqualizeXYSizes,True);
	NhlSetSArg(&sargs[(*nargs)++],
		   NhlNtmLabelAutoStride,mpp->label_auto_stride);

	if (! xbcount) {
		xbon = False;
		xb_labels_on = False;
		mpp->xbvalues = NULL;
		mpp->xblabels = NULL;
	}
	else {
		xbon = True;
		xb_labels_on = True;

		mpp->xbvalues = NhlCreateGenArray
			((NhlPointer)xbvalues,NhlTFloat,
			 sizeof(float),1, &xbcount);
		mpp->xblabels = NhlCreateGenArray
			((NhlPointer)xblabels,NhlTString,
			 sizeof(NhlString),1, &xbcount);
		if (! (mpp->xbvalues && mpp->xblabels)) {
			 NHLPERROR((NhlFATAL,ENOMEM,NULL));
			 return NhlFATAL;
		}
		mpp->xbvalues->my_data = mpp->xblabels->my_data = True;
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXBMode,NhlEXPLICIT);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXBValues,mpp->xbvalues);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXBLabels,mpp->xblabels);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXBLabelFuncCode,':');
	}
	if (! xtcount) {
		xton = False;
		xt_labels_on = False;
		mpp->xtvalues = NULL;
		mpp->xtlabels = NULL;
	}
	else {
 		xton = True;
		xt_labels_on = True;
		if (xtcount == xbcount &&
		    ! memcmp(xbvalues,xtvalues,xtcount * sizeof(float))) {
			for (i = 0; i < xtcount; i++) {
				if (strcmp(xblabels[i],xtlabels[i])) {
					usebottom = False;
					break;
				}
			}
		}
		else {
			usebottom = False;
		}

		mpp->xtvalues = NhlCreateGenArray
			((NhlPointer)xtvalues,NhlTFloat,
			 sizeof(float),1, &xtcount);
		mpp->xtlabels = NhlCreateGenArray
			((NhlPointer)xtlabels,NhlTString,
			 sizeof(NhlString),1, &xtcount);
		if (! (mpp->xtvalues && mpp->xtlabels)) {
			 NHLPERROR((NhlFATAL,ENOMEM,NULL));
			 return NhlFATAL;
		}
		mpp->xtvalues->my_data = mpp->xtlabels->my_data = True;
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXTMode,NhlEXPLICIT);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXTValues,mpp->xtvalues);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXTLabels,mpp->xtlabels);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXTLabelFuncCode,':');
	}

	if (! ylcount) {
		ylon = False;
		yl_labels_on = False;
		mpp->ylvalues = NULL;
		mpp->yllabels = NULL;
	}
	else {
 		ylon = True;
		yl_labels_on = True;
		mpp->ylvalues = NhlCreateGenArray
			((NhlPointer)ylvalues,NhlTFloat,
			 sizeof(float),1, &ylcount);
		mpp->yllabels = NhlCreateGenArray
			((NhlPointer)yllabels,NhlTString,
			 sizeof(NhlString),1, &ylcount);
		if (! (mpp->ylvalues && mpp->yllabels)) {
			 NHLPERROR((NhlFATAL,ENOMEM,NULL));
			 return NhlFATAL;
		}
		mpp->ylvalues->my_data = mpp->yllabels->my_data = True;
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYLMode,NhlEXPLICIT);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYLValues,mpp->ylvalues);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYLLabels,mpp->yllabels);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYLLabelFuncCode,':');
	}
	if (! yrcount) {
		yron = False;
		yr_labels_on = False;
		mpp->yrvalues = NULL;
		mpp->yrlabels = NULL;
	}
	else {
 		yron = True;
		yr_labels_on = True;
		if (yrcount == ylcount &&
			 ! memcmp(ylvalues,yrvalues,yrcount * sizeof(float))) {
			for (i = 0; i < yrcount; i++) {
				if (strcmp(yllabels[i],yrlabels[i])) {
					useleft = False;
					break;
				}
			}
		}
		else {
			useleft = False;
		}

		mpp->yrvalues = NhlCreateGenArray
			((NhlPointer)yrvalues,NhlTFloat,
			 sizeof(float),1,&yrcount);
		mpp->yrlabels = NhlCreateGenArray
			((NhlPointer)yrlabels,NhlTString,
			 sizeof(NhlString),1,&yrcount);
		if (! (mpp->yrvalues && mpp->yrlabels)) {
			 NHLPERROR((NhlFATAL,ENOMEM,NULL));
			 return NhlFATAL;
		}
		mpp->yrvalues->my_data = mpp->yrlabels->my_data = True;
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYRMode,NhlEXPLICIT);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYRValues,mpp->yrvalues);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYRLabels,mpp->yrlabels);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYRLabelFuncCode,':');
	}
	/*
	 * If otherwise turned on (there is a valid set of ticks and labels), 
	 * but explicitly set by the user, the user value prevails.
	 */

	if (xbon && mpp->xb_on_set)
		xbon = mpp->xb_on;
	if (xb_labels_on && mpp->xb_labels_on_set)
		xb_labels_on = mpp->xb_labels_on;
	if (ylon && mpp->yl_on_set)
		ylon = mpp->yl_on;
	if (yl_labels_on && mpp->yl_labels_on_set)
		yl_labels_on = mpp->yl_labels_on;

	/*
	 * For the top and right, the default is to be on only if valid and
	 * at least one side different from bottom and left. However, in the
	 * valid case, the user's set value prevails.
	 * 
	 */
	if (usebottom && useleft) {
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXUseBottom,True);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYUseLeft,True);
		if (xton && mpp->xt_on_set)
			xton = mpp->xt_on;
		else
			xton = False;
		if (xt_labels_on && mpp->xt_labels_on_set)
			xt_labels_on = mpp->xt_labels_on;
		else
			xt_labels_on = False;
		if (yron && mpp->yr_on_set)
			yron = mpp->yr_on;
		else
			yron = False;
		if (yr_labels_on && mpp->yr_labels_on_set)
			yr_labels_on = mpp->yr_labels_on;
		else
			yr_labels_on = False;

	}
	else {
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmXUseBottom,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNtmYUseLeft,False);
		if (xton && mpp->xt_on_set)
			xton = mpp->xt_on;
		if (xt_labels_on && mpp->xt_labels_on_set)
			xt_labels_on = mpp->xt_labels_on;
		if (yron && mpp->yr_on_set)
			yron = mpp->yr_on;
		if (yr_labels_on && mpp->yr_labels_on_set)
			yr_labels_on = mpp->yr_labels_on;
	}

	NhlSetSArg(&sargs[(*nargs)++],NhlNtmXBOn,xbon);
	NhlSetSArg(&sargs[(*nargs)++],NhlNtmXTOn,xton);
	NhlSetSArg(&sargs[(*nargs)++],NhlNtmYLOn,ylon);
	NhlSetSArg(&sargs[(*nargs)++],NhlNtmYROn,yron);
	NhlSetSArg(&sargs[(*nargs)++],NhlNtmXBLabelsOn,xb_labels_on);
	NhlSetSArg(&sargs[(*nargs)++],NhlNtmXTLabelsOn,xt_labels_on);
	NhlSetSArg(&sargs[(*nargs)++],NhlNtmYLLabelsOn,yl_labels_on);
	NhlSetSArg(&sargs[(*nargs)++],NhlNtmYRLabelsOn,yr_labels_on);

	/*
	 * Rely on the TickMark object to equalize the setting for
	 * these resources.
	 */

	if (init) {
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmXBMajorLengthF,mpp->xb_major_length);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmXTMajorLengthF,mpp->xt_major_length);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmYLMajorLengthF,mpp->yl_major_length);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmYRMajorLengthF,mpp->yr_major_length);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmXBMajorOutwardLengthF,
			   mpp->xb_major_outward_length);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmXTMajorOutwardLengthF,
			   mpp->xt_major_outward_length);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmYLMajorOutwardLengthF,
			   mpp->yl_major_outward_length);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmYRMajorOutwardLengthF,
			   mpp->yr_major_outward_length);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmXBLabelFontHeightF,mpp->xb_font_height);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmXTLabelFontHeightF,mpp->xt_font_height);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmYLLabelFontHeightF,mpp->yl_font_height);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNtmYRLabelFontHeightF,mpp->yr_font_height);
	}
	else {
		float outlen;
		if (mpp->xb_major_length_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmXBMajorLengthF,mpp->xb_major_length);
			if (! mpp->xb_major_outward_length_set) {
				outlen = mpp->xb_major_outward_length *
					mpp->xb_major_length/ompp->xb_major_length;
				NhlSetSArg(&sargs[(*nargs)++],
					   NhlNtmXBMajorOutwardLengthF,outlen);
			}
		}
		if (mpp->xt_major_length_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmXTMajorLengthF,mpp->xt_major_length);
			if (! mpp->xt_major_outward_length_set) {
				outlen = mpp->xt_major_outward_length *
					mpp->xt_major_length/ompp->xt_major_length;
				NhlSetSArg(&sargs[(*nargs)++],
					   NhlNtmXTMajorOutwardLengthF,outlen);
			}
		}
		if (mpp->yl_major_length_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmYLMajorLengthF,mpp->yl_major_length);
			if (! mpp->yl_major_outward_length_set) {
				outlen = mpp->yl_major_outward_length *
					mpp->yl_major_length/ompp->yl_major_length;
				NhlSetSArg(&sargs[(*nargs)++],
					   NhlNtmYLMajorOutwardLengthF,outlen);
			}
		}
		if (mpp->yr_major_length_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmYRMajorLengthF,mpp->yr_major_length);
			if (! mpp->yr_major_outward_length_set) {
				outlen = mpp->yr_major_outward_length *
					mpp->yr_major_length/ompp->yr_major_length;
				NhlSetSArg(&sargs[(*nargs)++],
					   NhlNtmYRMajorOutwardLengthF,outlen);
			}
		}
		if (mpp->xb_major_outward_length_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmXBMajorOutwardLengthF,
				   mpp->xb_major_outward_length);
		}
		if (mpp->xt_major_outward_length_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmXTMajorOutwardLengthF,
				   mpp->xt_major_outward_length);
		}
		if (mpp->yl_major_outward_length_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmYLMajorOutwardLengthF,
				   mpp->yl_major_outward_length);
		}
		if (mpp->yr_major_outward_length_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmYRMajorOutwardLengthF,
				   mpp->yr_major_outward_length);
		}
		if (mpp->xb_font_height_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmXBLabelFontHeightF,mpp->xb_font_height);
		}
		if (mpp->xt_font_height_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmXTLabelFontHeightF,mpp->xt_font_height);
		}
		if (mpp->yl_font_height_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmYLLabelFontHeightF,mpp->yl_font_height);
		}
		if (mpp->yr_font_height_set) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNtmYRLabelFontHeightF,mpp->yr_font_height);
		}
	}
	mpp->xb_major_length_set = False;
	mpp->xt_major_length_set = False;
	mpp->yl_major_length_set = False;
	mpp->yr_major_length_set = False;
	mpp->xb_major_outward_length_set = False;
	mpp->xt_major_outward_length_set = False;
	mpp->yl_major_outward_length_set = False;
	mpp->yr_major_outward_length_set = False;
	mpp->xb_font_height_set = False;
	mpp->xt_font_height_set = False;
	mpp->yl_font_height_set = False;
	mpp->yr_font_height_set = False;

	return ret;
}

/*
 * Function:  hlumapusr
 *
 * Description: C version of the MAPUSR function that EZMAP invokes 
 *		before drawing each class of map objects.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
void   (_NHLCALLF(hlumapusr,HLUMAPUSR))
#if	NhlNeedProto
(
	int	*iprt
)
#else
(iprt)
	int	*iprt;
#endif

{
	float	thickness;
	int	dpat;
	float	dseglen;
	int     gks_color;
	NhlString *sp;
	float	p0,p1,jcrt;
	int	slen;
	char	buffer[128];
	char	*entry_name = "mpDraw";

	if (Mpp == NULL) {
		_NHLCALLF(mapusr,MAPUSR)(iprt);
		return;
	}

	switch (*iprt) {
	case 1:		/* perimeter */
		thickness = Mpp->perim.thickness;
		dpat = Mpp->perim.dash_pat;
		dseglen = Mpp->perim.dash_seglen;
		gks_color = Mpp->perim.color;
		break;
	case 2:		/* grid */
		thickness = Mpp->grid.thickness;
		dpat = Mpp->grid.dash_pat;
		dseglen = Mpp->grid.dash_seglen;
		gks_color = Mpp->grid.color;
		break;
	case 3:		/* labels */
		return;
	case 4:		/* limb lines */
		thickness = Mpp->limb.thickness;
		dpat = Mpp->limb.dash_pat;
		dseglen = Mpp->limb.dash_seglen;
		gks_color = Mpp->limb.gks_color;
		break;
	case 5:		/* geophysical outlines */
		thickness = Mpp->geophysical.thickness;
		dpat = Mpp->geophysical.dash_pat;
		dseglen = Mpp->geophysical.dash_seglen;
		gks_color = Mpp->geophysical.gks_color;
		break;
	case 6:		/* us states outlines */
		thickness = Mpp->us_state.thickness;
		dpat = Mpp->us_state.dash_pat;
		dseglen = Mpp->us_state.dash_seglen;
		gks_color = Mpp->us_state.gks_color;
		break;
	case 7:		/* countries */
		thickness = Mpp->national.thickness;
		dpat = Mpp->national.dash_pat;
		dseglen = Mpp->national.dash_seglen;
		gks_color = Mpp->national.gks_color;
		break;
	case 8:		/* counties */
		thickness = Mpp->county.thickness;
		dpat = Mpp->county.dash_pat;
		dseglen = Mpp->county.dash_seglen;
		gks_color = Mpp->county.gks_color;
		break;
	default:
		return;
	}
		
 	c_pcseti("CC",-1);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_pcseti("OC",-1);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	gset_linewidth(thickness);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	dpat %= Mpp->dash_table->num_elements;
	sp = (NhlString *) Mpp->dash_table->data;
	slen = strlen(sp[dpat]);
	p0 =  (float) c_kfpy(0.0);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	p1 = dseglen;
	p1 = (float) c_kfpy(p1);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	jcrt = (int) ((p1 - p0) / slen + 0.5);
	jcrt = jcrt > 1 ? jcrt : 1;
	strcpy(buffer,sp[dpat]);


#if 0
	/* unneeded now-----------------
	 * since dashchar recognizes only a single quote as
	 * the space indicator, we must change the the 
	 * underscores in the pattern into spaces.
	 */
		 
	for (i = 0; i < strlen(buffer); i++) {
		if (buffer[i] == '_')
			buffer[i] = '_';
	}	
#endif
	if (gks_color == -1) {
		int ipat = 0;
		c_dashdb(&ipat);
	}
	else
		c_dashdc(buffer,jcrt,4);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

}

/*
 * Function:  load_hlumap_routines
 *
 * Description: Forces the hlumap... routines to load from the HLU library
 *
 * In Args:   NhlBoolean flag - should always be False - dont actually
 *			        want to call the routines.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static void   load_hlumap_routines
#if	NhlNeedProto
(
	NhlBoolean	flag
)
#else
(flag)
	NhlBoolean	flag;
#endif
{
	int idum;


	if (flag) {
		_NHLCALLF(hlumapusr,HLUMAPUSR)(&idum);
	}
	return;
}

