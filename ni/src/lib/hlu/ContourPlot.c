/*
 *      $Id: ContourPlot.c,v 1.85 1999-04-05 23:30:02 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ContourPlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a ContourPlot plot object
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/ContourPlotP.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/IrregularTransObjP.h>
#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>

#define Oset(field)     NhlOffset(NhlContourPlotLayerRec,contourplot.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

/* Data resources */

	{NhlNcnScalarFieldData,NhlCcnScalarFieldData,_NhlTDataList,
		 sizeof(NhlGenArray),
		 Oset(scalar_field_data),NhlTImmediate,_NhlUSET(NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},

/* Level resources */

	{ NhlNcnLevelSelectionMode,NhlCLevelSelectionMode,
		  NhlTcnLevelSelectionMode,sizeof(NhlcnLevelSelectionMode),
		  Oset(level_selection_mode),
		  NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlAUTOMATICLEVELS),0,NULL},
	{ NhlNcnLevelCount,NhlCcnLevelCount,NhlTInteger,sizeof(int),
		  Oset(level_count),NhlTImmediate,
		  _NhlUSET((NhlPointer) 16),_NhlRES_GONLY,NULL},
	{ NhlNcnMaxLevelCount,NhlCMaxLevelCount,NhlTInteger,sizeof(int),
		  Oset(max_level_count),NhlTImmediate,
		  _NhlUSET((NhlPointer) 16),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(level_spacing_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNcnLevelSpacingF,NhlCLevelSpacingF,NhlTFloat,sizeof(float),
		  Oset(level_spacing),NhlTProcedure,
		  _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_level_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNcnMinLevelValF,NhlCMinLevelValF,NhlTFloat,sizeof(float),
		  Oset(min_level_val),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(max_level_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNcnMaxLevelValF,NhlCMaxLevelValF,NhlTFloat,sizeof(float),
		  Oset(max_level_val),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNcnLevels, NhlCLevels,  NhlTFloatGenArray,
		 sizeof(NhlPointer),Oset(levels),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnMonoLevelFlag, NhlCcnMonoLevelFlag, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_level_flag),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnLevelFlag, NhlCcnLevelFlag, NhlTcnLevelUseMode,
		 sizeof(NhlcnLevelUseMode),Oset(level_flag),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlLINEONLY),0,NULL},
	{NhlNcnLevelFlags, NhlCcnLevelFlags,NhlTcnLevelUseModeGenArray,
		 sizeof(NhlPointer),Oset(level_flags),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},

/* Rendering resources */

 	{NhlNcnSmoothingOn,NhlCcnSmoothingOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(smoothing_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
 	{NhlNcnSmoothingTensionF,NhlCcnSmoothingTensionF,NhlTFloat,
		 sizeof(float),Oset(smoothing_tension),
		 NhlTString,_NhlUSET("-2.5"),0,NULL},
 	{NhlNcnSmoothingDistanceF,NhlCcnSmoothingDistanceF,NhlTFloat,
		 sizeof(float),Oset(smoothing_distance),
		 NhlTString,_NhlUSET("0.01"),0,NULL},
 	{NhlNcnMaxPointDistanceF,NhlCcnMaxPointDistanceF,
                 NhlTFloat,sizeof(float),Oset(max_point_distance),
		 NhlTString,_NhlUSET("0.05"),0,NULL},

/* Newly added resources */

	{NhlNcnExplicitLineLabelsOn,NhlCcnExplicitLineLabelsOn,
		 NhlTBoolean,sizeof(NhlBoolean),
		 Oset(explicit_line_labels_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnExplicitLabelBarLabelsOn,NhlCcnExplicitLabelBarLabelsOn,
		 NhlTBoolean,sizeof(NhlBoolean),
		 Oset(explicit_lbar_labels_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnLabelBarEndLabelsOn,NhlCcnLabelBarEndLabelsOn,
		 NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lbar_end_labels_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnExplicitLegendLabelsOn,NhlCcnExplicitLegendLabelsOn,
		 NhlTBoolean,sizeof(NhlBoolean),
		 Oset(explicit_lgnd_labels_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnLegendLevelFlags, NhlCcnLegendLevelFlags,
		 NhlTcnLevelUseModeGenArray,
		 sizeof(NhlPointer),Oset(lgnd_level_flags),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnRasterModeOn,NhlCcnRasterModeOn,
		 NhlTBoolean,sizeof(NhlBoolean),
		 Oset(raster_mode_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(cell_size_set),NhlTImmediate,
		_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNcnRasterCellSizeF,NhlCcnRasterCellSizeF,NhlTFloat,sizeof(float),
		 Oset(cell_size),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNcnRasterSmoothingOn,NhlCcnRasterSmoothingOn,
         	NhlTBoolean,sizeof(NhlBoolean),Oset(raster_smoothing_on),
         	NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnRasterSampleFactorF,NhlCcnRasterSampleFactorF,
         	NhlTFloat,sizeof(float),Oset(raster_sample_factor),
         	NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNcnRasterMinCellSizeF,NhlCcnRasterMinCellSizeF,NhlTFloat,
         	sizeof(float),Oset(min_cell_size),
         	NhlTString,_NhlUSET("0.001"),0,NULL},

/* Line resources */

	{NhlNcnLinesOn,NhlCcnLinesOn,NhlTBoolean,sizeof(NhlBoolean),
		  Oset(lines_on),
		  NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
 	{NhlNcnLineDrawOrder,NhlCcnLineDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(line_order),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlDRAW),0,NULL},
	{NhlNcnMonoLineColor, NhlCcnMonoLineColor, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_line_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnLineColor, NhlCLineColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(line_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNcnLineColors, NhlCcnLineColors, NhlTColorIndexGenArray,
		 sizeof(NhlGenArray),Oset(line_colors),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnMonoLineDashPattern, NhlCcnMonoLineDashPattern, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_line_dash_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnLineDashPattern, NhlCLineDashPattern, NhlTDashIndex,
		 sizeof(NhlDashIndex),Oset(line_dash_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlSOLIDLINE),0,NULL},
	{NhlNcnLineDashPatterns, NhlCcnLineDashPatterns,NhlTDashIndexGenArray,
		 sizeof(NhlPointer),Oset(line_dash_patterns),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnMonoLineThickness, NhlCcnMonoLineThickness, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_line_thickness),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnLineThicknessF, NhlCLineThicknessF, NhlTFloat,
		 sizeof(float),Oset(line_thickness),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNcnLineThicknesses, NhlCcnLineThicknesses, NhlTFloatGenArray,
		 sizeof(NhlPointer),Oset(line_thicknesses),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(line_dash_seglen_set),NhlTImmediate,
		_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
        {NhlNcnLineDashSegLenF, NhlCLineDashSegLenF,NhlTFloat,sizeof(float),
		  Oset(line_dash_seglen),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

/* Fill resources */

	{NhlNcnFillOn,NhlCcnFillOn,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(fill_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnFillBackgroundColor,NhlCFillBackgroundColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(fill_background_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlTRANSPARENT),0,NULL},
 	{NhlNcnFillDrawOrder,NhlCcnFillDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(fill_order),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlDRAW),0,NULL},
	{NhlNcnMonoFillColor, NhlCcnMonoFillColor, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnFillColor, NhlCFillColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNcnFillColors, NhlCcnFillColors, NhlTColorIndexGenArray,
		 sizeof(NhlPointer),Oset(fill_colors),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnMonoFillPattern, NhlCcnMonoFillPattern, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnFillPattern, NhlCFillPattern, NhlTFillIndex,
		 sizeof(NhlFillIndex),Oset(fill_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlSOLIDFILL),0,NULL},
	{NhlNcnFillPatterns, NhlCcnFillPatterns, NhlTFillIndexGenArray,
		 sizeof(NhlPointer),Oset(fill_patterns),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnMonoFillScale, NhlCcnMonoFillScale, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_scale),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnFillScaleF, NhlCFillScaleF, NhlTFloat,
		 sizeof(float),Oset(fill_scale),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNcnFillScales, NhlCcnFillScales,  NhlTFloatGenArray,
		 sizeof(NhlPointer),Oset(fill_scales),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},

/* General label resources */

 	{NhlNcnLabelDrawOrder,NhlCcnLabelDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(label_order),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlDRAW),0,NULL},
	{NhlNcnLabelMasking,NhlCcnLabelMasking,NhlTBoolean,sizeof(NhlBoolean),
		  Oset(label_masking),
		  NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnLowUseHighLabelRes,NhlCcnLowUseHighLabelRes,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(low_use_high_attrs),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnHighUseLineLabelRes,NhlCcnHighUseLineLabelRes,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(high_use_line_attrs),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnConstFUseInfoLabelRes,NhlCcnConstFUseInfoLabelRes,
		 NhlTBoolean,sizeof(NhlBoolean),Oset(constf_use_info_attrs),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnHighLowLabelOverlapMode,NhlCcnHighLowLabelOverlapMode,
		 NhlTcnHighLowLabelOverlapMode,
		 sizeof(NhlcnHighLowLabelOverlapMode),
		  Oset(high_low_overlap),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlIGNOREOVERLAP),0,NULL},

/* General label string format option */

	{NhlNcnLabelScalingMode,NhlCcnLabelScalingMode,
                 NhlTcnLabelScalingMode,sizeof(NhlcnLabelScalingMode),
                 Oset(label_scaling_mode),NhlTImmediate,
                 _NhlUSET((NhlPointer) NhlSCALEFACTOR),0,NULL},
        {NhlNcnLabelScaleValueF,NhlCcnLabelScaleValueF,
                 NhlTFloat,sizeof(float),Oset(label_scale_value),
                 NhlTString,_NhlUSET("1.0"),0,NULL},
        {NhlNcnLabelScaleFactorF,NhlCcnLabelScaleFactorF,
                 NhlTFloat,sizeof(float),Oset(label_scale_factor),
                 NhlTString,_NhlUSET("1.0"),_NhlRES_GONLY,NULL},
	{NhlNcnMaxDataValueFormat,NhlCcnMaxDataValueFormat,
		 NhlTString,sizeof(NhlString),
		 Oset(max_data_format.fstring),NhlTImmediate,
		 _NhlUSET("*+.4^sg"),0,(NhlFreeFunc)NhlFree},

/* Line label resources */

	{NhlNcnLineLabelsOn,NhlCPlotLabelsOn,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(line_lbls.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(llabel_interval_set),NhlTImmediate,
		_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNcnLineLabelInterval,NhlCcnLineLabelInterval,
		 NhlTInteger,sizeof(int),
		 Oset(llabel_interval),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNcnLineLabelPlacementMode,NhlCcnLineLabelPlacementMode,
		 NhlTcnLineLabelPlacementMode,
		 sizeof(NhlcnLineLabelPlacementMode),
		 Oset(llabel_placement),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlRANDOMIZED),0,NULL},
	{NhlNcnLineLabelStrings, NhlCcnLineLabelStrings, NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(llabel_strings),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnMonoLineLabelFontColor,NhlCcnMonoLineLabelFontColor,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(line_lbls.mono_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnLineLabelFontColor,NhlCFontColor,NhlTColorIndex,
		 sizeof(NhlBoolean),Oset(line_lbls.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnLineLabelFontColors, NhlCcnLineLabelFontColors, 
		 NhlTColorIndexGenArray,
		 sizeof(NhlPointer),Oset(llabel_colors),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnLineLabelFormat,NhlCNumberFormat,
		 NhlTString,sizeof(NhlString),
		 Oset(line_lbls.format.fstring),NhlTString,
		 _NhlUSET(NhlcnDEF_FORMAT),0,(NhlFreeFunc)NhlFree},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(line_lbls.height_set),NhlTImmediate,
		_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
        {NhlNcnLineLabelFontHeightF,NhlCFontHeightF,
		  NhlTFloat,sizeof(float),Oset(line_lbls.height),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNcnLineLabelFont,NhlCFont,NhlTFont, 
		 sizeof(int),Oset(line_lbls.font),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNcnLineLabelFontAspectF,NhlCFontAspectF,NhlTFloat, 
		 sizeof(float),Oset(line_lbls.aspect),
		 NhlTString, _NhlUSET("1.3125"),0,NULL},
	{NhlNcnLineLabelFontThicknessF,NhlCFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(line_lbls.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNcnLineLabelFontQuality,NhlCFontQuality,
		 NhlTFontQuality, 
		 sizeof(NhlFontQuality),Oset(line_lbls.quality),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNcnLineLabelConstantSpacingF,NhlCTextConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(line_lbls.cspacing),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnLineLabelAngleF,NhlCcnLineLabelAngleF,
		 NhlTFloat,sizeof(float),Oset(line_lbls.angle),
		 NhlTString,_NhlUSET("-1.0"),0,NULL},
	{NhlNcnLineLabelFuncCode,NhlCTextFuncCode,NhlTCharacter, 
		 sizeof(char),Oset(line_lbls.fcode[0]),
		 NhlTString, _NhlUSET(":"),0,NULL},
	{NhlNcnLineLabelBackgroundColor,NhlCFillBackgroundColor,
		 NhlTColorIndex,sizeof(NhlColorIndex),
		 Oset(line_lbls.back_color),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNcnLineLabelPerimOn,NhlCEdgesOn,
                 NhlTBoolean,sizeof(NhlBoolean),
		 Oset(line_lbls.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnLineLabelPerimSpaceF,NhlCEdgeBorderWidthF,
		 NhlTFloat,sizeof(float),Oset(line_lbls.perim_space),
		 NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNcnLineLabelPerimColor,NhlCEdgeColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(line_lbls.perim_lcolor),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNcnLineLabelPerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(line_lbls.perim_lthick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* High Label resources */

	{NhlNcnHighLabelsOn,NhlCPlotLabelsOn,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(high_lbls.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNcnHighLabelString,NhlCcnHighLabelString,
		 NhlTString,sizeof(NhlString),
		 Oset(high_lbls.text),NhlTImmediate,_NhlUSET(NULL),0,
							(NhlFreeFunc)NhlFree},
	{NhlNcnHighLabelFormat,NhlCNumberFormat,
		 NhlTString,sizeof(NhlString),
		 Oset(high_lbls.format.fstring),NhlTImmediate,
		 _NhlUSET(NhlcnDEF_FORMAT),0,(NhlFreeFunc)NhlFree},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(high_lbls.height_set),NhlTImmediate,
		_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
        {NhlNcnHighLabelFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),Oset(high_lbls.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNcnHighLabelFont,NhlCFont,NhlTFont, 
		 sizeof(int),Oset(high_lbls.font),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNcnHighLabelFontColor,NhlCFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(high_lbls.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnHighLabelFontAspectF,NhlCFontAspectF,NhlTFloat, 
		 sizeof(float),Oset(high_lbls.aspect),
		 NhlTString, _NhlUSET("1.3125"),0,NULL},
	{NhlNcnHighLabelFontThicknessF,NhlCFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(high_lbls.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNcnHighLabelFontQuality,NhlCFontQuality,
		 NhlTFontQuality, 
		 sizeof(NhlFontQuality),Oset(high_lbls.quality),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNcnHighLabelConstantSpacingF,NhlCTextConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(high_lbls.cspacing),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnHighLabelAngleF,NhlCTextAngleF,
		 NhlTFloat,sizeof(float),Oset(high_lbls.angle),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnHighLabelFuncCode,NhlCTextFuncCode,NhlTCharacter, 
		 sizeof(char),Oset(high_lbls.fcode[0]),
		 NhlTString, _NhlUSET(":"),0,NULL},
	{NhlNcnHighLabelBackgroundColor,NhlCFillBackgroundColor,
		  NhlTColorIndex,sizeof(NhlColorIndex),
		  Oset(high_lbls.back_color),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNcnHighLabelPerimOn,NhlCEdgesOn,NhlTBoolean,
		sizeof(NhlBoolean),Oset(high_lbls.perim_on),NhlTImmediate,
		_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnHighLabelPerimSpaceF,NhlCEdgeBorderWidthF,
		  NhlTFloat,sizeof(float),Oset(high_lbls.perim_space),
		  NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNcnHighLabelPerimColor,NhlCEdgeColor,NhlTColorIndex,
		  sizeof(NhlColorIndex),Oset(high_lbls.perim_lcolor),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNcnHighLabelPerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(high_lbls.perim_lthick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* Low label resources */

	{NhlNcnLowLabelsOn,NhlCPlotLabelsOn,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(low_lbls.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNcnLowLabelString,NhlCcnLowLabelString,
		 NhlTString,sizeof(NhlString),
		 Oset(low_lbls.text),NhlTImmediate,_NhlUSET(NULL),0,
							(NhlFreeFunc)NhlFree},
	{NhlNcnLowLabelFormat,NhlCNumberFormat,
		 NhlTString,sizeof(NhlString),
		 Oset(low_lbls.format.fstring),NhlTImmediate,
		 _NhlUSET(NhlcnDEF_FORMAT),0,(NhlFreeFunc)NhlFree},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(low_lbls.height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
        {NhlNcnLowLabelFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),Oset(low_lbls.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNcnLowLabelFont,NhlCFont,NhlTFont, 
		 sizeof(int),Oset(low_lbls.font),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNcnLowLabelFontColor,NhlCFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(low_lbls.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnLowLabelFontAspectF,NhlCFontAspectF,NhlTFloat, 
		 sizeof(float),Oset(low_lbls.aspect),
		 NhlTString, _NhlUSET("1.3125"),0,NULL},
	{NhlNcnLowLabelFontThicknessF,NhlCFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(low_lbls.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNcnLowLabelFontQuality,NhlCFontQuality,NhlTFontQuality, 
		 sizeof(NhlFontQuality),Oset(low_lbls.quality),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNcnLowLabelConstantSpacingF,NhlCTextConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(low_lbls.cspacing),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnLowLabelAngleF,NhlCTextAngleF,
		 NhlTFloat,sizeof(float),Oset(low_lbls.angle),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnLowLabelFuncCode,NhlCTextFuncCode,NhlTCharacter, 
		 sizeof(char),Oset(low_lbls.fcode[0]),
		 NhlTString, _NhlUSET(":"),0,NULL},
	{NhlNcnLowLabelBackgroundColor,NhlCFillBackgroundColor,
		  NhlTColorIndex,sizeof(NhlColorIndex),
		  Oset(low_lbls.back_color),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNcnLowLabelPerimOn,NhlCEdgesOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(low_lbls.perim_on),
		NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{ NhlNcnLowLabelPerimSpaceF,NhlCEdgeBorderWidthF,
		  NhlTFloat,sizeof(float),Oset(low_lbls.perim_space),
		  NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNcnLowLabelPerimColor,NhlCEdgeColor,NhlTColorIndex,
		  sizeof(NhlColorIndex),Oset(low_lbls.perim_lcolor),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNcnLowLabelPerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(low_lbls.perim_lthick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* Informational label resources */

	{NhlNcnInfoLabelOn,NhlCAnnotationLabelsOn,NhlTBoolean,
	 	sizeof(NhlBoolean),Oset(info_lbl.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNcnInfoLabelString,NhlCcnInfoLabelString,
		 NhlTString,sizeof(NhlString),
		 Oset(info_string),NhlTImmediate,_NhlUSET(NULL),0,
							(NhlFreeFunc)NhlFree},
	{NhlNcnInfoLabelFormat,NhlCNumberFormat,
		 NhlTString,sizeof(NhlString),
		 Oset(info_lbl.format.fstring),NhlTImmediate,
		 _NhlUSET(NhlcnDEF_FORMAT),0,(NhlFreeFunc)NhlFree},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(info_lbl.height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
        {NhlNcnInfoLabelFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),Oset(info_lbl.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
        {NhlNcnInfoLabelTextDirection,NhlCTextDirection,
		 NhlTTextDirection,sizeof(NhlTextDirection),
		 Oset(info_lbl.direction),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNcnInfoLabelFont,NhlCFont,NhlTFont, 
		 sizeof(int),Oset(info_lbl.font),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNcnInfoLabelFontColor,NhlCFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(info_lbl.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnInfoLabelFontAspectF,NhlCFontAspectF,NhlTFloat, 
		 sizeof(float),Oset(info_lbl.aspect),
		 NhlTString, _NhlUSET("1.3125"),0,NULL},
	{NhlNcnInfoLabelFontThicknessF,NhlCFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(info_lbl.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNcnInfoLabelFontQuality,NhlCFontQuality,
		 NhlTFontQuality,sizeof(NhlFontQuality),Oset(info_lbl.quality),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNcnInfoLabelConstantSpacingF,NhlCTextConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(info_lbl.cspacing),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnInfoLabelAngleF,NhlCTextAngleF,
		 NhlTFloat,sizeof(float),Oset(info_lbl.angle),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnInfoLabelFuncCode,NhlCTextFuncCode,NhlTCharacter, 
		 sizeof(char),Oset(info_lbl.fcode[0]),
		 NhlTString, _NhlUSET(":"),0,NULL},
	{NhlNcnInfoLabelBackgroundColor,NhlCFillBackgroundColor,
		  NhlTColorIndex,sizeof(NhlColorIndex),
		  Oset(info_lbl.back_color),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{ NhlNcnInfoLabelPerimOn,NhlCEdgesOn,
                  NhlTBoolean,sizeof(NhlBoolean),
		Oset(info_lbl.perim_on),
		NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{ NhlNcnInfoLabelPerimSpaceF,NhlCEdgeBorderWidthF,
		  NhlTFloat,sizeof(float),Oset(info_lbl.perim_space),
		  NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNcnInfoLabelPerimColor,NhlCEdgeColor,NhlTColorIndex,
		  sizeof(NhlColorIndex),Oset(info_lbl.perim_lcolor),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNcnInfoLabelPerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(info_lbl.perim_lthick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNcnInfoLabelZone,NhlCcnInfoLabelZone,NhlTInteger,
		 sizeof(int),Oset(info_lbl_rec.zone),NhlTImmediate,
		 _NhlUSET((NhlPointer) 3),0,NULL},
	{NhlNcnInfoLabelSide,NhlCcnInfoLabelSide,NhlTPosition,
		 sizeof(NhlPosition),Oset(info_lbl_rec.side),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlBOTTOM),0,NULL},
	{NhlNcnInfoLabelJust,NhlCTextJustification,
		 NhlTJustification,sizeof(NhlJustification),
		 Oset(info_lbl_rec.just),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlTOPRIGHT),0,NULL},
	{NhlNcnInfoLabelParallelPosF,NhlCcnInfoLabelParallelPosF,NhlTFloat,
		 sizeof(float),Oset(info_lbl_rec.para_pos),NhlTString,
		 _NhlUSET("1.0"),0,NULL},
	{NhlNcnInfoLabelOrthogonalPosF,NhlCcnInfoLabelOrthogonalPosF,NhlTFloat,
		 sizeof(float),Oset(info_lbl_rec.ortho_pos),NhlTString,
		 _NhlUSET("0.02"),0,NULL},

	{NhlNcnNoDataLabelOn,NhlCAnnotationLabelsOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(no_data_label_on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNcnNoDataLabelString,NhlCcnNoDataLabelString,
		 NhlTString,sizeof(NhlString),Oset(no_data_string),
		 NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},

/* Constant field label resources */

	{NhlNcnConstFLabelOn,NhlCAnnotationLabelsOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(constf_lbl.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNcnConstFLabelString,NhlCcnConstFLabelString,
		 NhlTString,sizeof(NhlString),Oset(constf_string),
		 NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},
	{NhlNcnConstFLabelFormat,NhlCNumberFormat,
		 NhlTString,sizeof(NhlString),
		 Oset(constf_lbl.format.fstring),NhlTImmediate,
		 _NhlUSET(NhlcnDEF_FORMAT),0,(NhlFreeFunc)NhlFree},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(constf_lbl.height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
        {NhlNcnConstFLabelFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),Oset(constf_lbl.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
        {NhlNcnConstFLabelTextDirection,NhlCTextDirection,
		 NhlTTextDirection,sizeof(NhlTextDirection),
		 Oset(constf_lbl.direction),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNcnConstFLabelFont,NhlCFont,NhlTFont, 
		 sizeof(int),Oset(constf_lbl.font),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNcnConstFLabelFontColor,NhlCFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(constf_lbl.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnConstFLabelFontAspectF,NhlCFontAspectF,NhlTFloat, 
		 sizeof(float),Oset(constf_lbl.aspect),
		 NhlTString, _NhlUSET("1.3125"),0,NULL},
	{NhlNcnConstFLabelFontThicknessF,NhlCFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(constf_lbl.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNcnConstFLabelFontQuality,NhlCFontQuality,
		 NhlTFontQuality, 
		 sizeof(NhlFontQuality),Oset(constf_lbl.quality),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNcnConstFLabelConstantSpacingF,NhlCTextConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(constf_lbl.cspacing),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnConstFLabelAngleF,NhlCTextAngleF,
		 NhlTFloat,sizeof(float),Oset(constf_lbl.angle),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNcnConstFLabelFuncCode,NhlCTextFuncCode,NhlTCharacter, 
		 sizeof(char),Oset(constf_lbl.fcode[0]),
		 NhlTString, _NhlUSET(":"),0,NULL},
	{NhlNcnConstFLabelBackgroundColor,NhlCFillBackgroundColor,
		 NhlTColorIndex,sizeof(NhlColorIndex),
		 Oset(constf_lbl.back_color),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNcnConstFLabelPerimOn,NhlCEdgesOn,
                 NhlTBoolean,sizeof(NhlBoolean),Oset(constf_lbl.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNcnConstFLabelPerimSpaceF,NhlCEdgeBorderWidthF,
		 NhlTFloat,sizeof(float),Oset(constf_lbl.perim_space),
		 NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNcnConstFLabelPerimColor,NhlCEdgeColor,
		 NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(constf_lbl.perim_lcolor),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNcnConstFLabelPerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(constf_lbl.perim_lthick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNcnConstFLabelZone,NhlCcnConstFLabelZone,NhlTInteger,
		 sizeof(int),Oset(constf_lbl_rec.zone),NhlTImmediate,
		 _NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNcnConstFLabelSide,NhlCcnConstFLabelSide,NhlTPosition,
		 sizeof(NhlPosition),Oset(constf_lbl_rec.side),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlBOTTOM),0,NULL},
	{NhlNcnConstFLabelJust,NhlCTextJustification,
		 NhlTJustification,sizeof(NhlJustification),
		 Oset(constf_lbl_rec.just),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNcnConstFLabelParallelPosF,NhlCcnConstFLabelParallelPosF,NhlTFloat,
		 sizeof(float),Oset(constf_lbl_rec.para_pos),NhlTString,
		 _NhlUSET("0.0"),0,NULL},
	{NhlNcnConstFLabelOrthogonalPosF,NhlCcnConstFLabelOrthogonalPosF,
		 NhlTFloat,sizeof(float),Oset(constf_lbl_rec.ortho_pos),
		 NhlTString,_NhlUSET("0.0"),0,NULL},

/* Missing value area resources */

	{NhlNcnMissingValPerimOn,NhlCEdgesOn,
                 NhlTBoolean,sizeof(NhlBoolean),
                 Oset(missing_val.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnMissingValPerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(missing_val.perim_thick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNcnMissingValPerimDashPattern,NhlCEdgeDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),
		 Oset(missing_val.perim_dpat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{NhlNcnMissingValPerimColor,NhlCEdgeColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(missing_val.perim_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNcnMissingValFillColor,NhlCFillColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(missing_val.fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNcnMissingValFillPattern,NhlCFillPattern,NhlTFillIndex,
		 sizeof(NhlFillIndex),Oset(missing_val.fill_pat),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHOLLOWFILL),0,NULL},
	{NhlNcnMissingValFillScaleF,NhlCFillScaleF,
		 NhlTFloat,sizeof(float),Oset(missing_val.fill_scale),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

/* Grid boundary resources */

	{NhlNcnGridBoundPerimOn,NhlCEdgesOn,
                 NhlTBoolean,sizeof(NhlBoolean),Oset(grid_bound.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnGridBoundPerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(grid_bound.perim_thick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNcnGridBoundPerimDashPattern,NhlCEdgeDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),
		 Oset(grid_bound.perim_dpat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{NhlNcnGridBoundPerimColor,NhlCEdgeColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(grid_bound.perim_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},

/* Out of range area resources */

	{NhlNcnOutOfRangePerimOn,NhlCEdgesOn,
                 NhlTBoolean,sizeof(NhlBoolean),Oset(out_of_range.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNcnOutOfRangePerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(out_of_range.perim_thick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNcnOutOfRangePerimDashPattern,NhlCEdgeDashPattern,
		 NhlTDashIndex,sizeof(NhlDashIndex),
		 Oset(out_of_range.perim_dpat),NhlTImmediate,
		 _NhlUSET(0),0,NULL},
	{NhlNcnOutOfRangePerimColor,NhlCEdgeColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(out_of_range.perim_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},

/* End-documented-resources */

/* Private resources */

	{NhlNcnDumpAreaMap, NhlCcnDumpAreaMap,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(dump_area_map),NhlTImmediate,
		 _NhlUSET((NhlPointer) False),_NhlRES_PRIVATE,NULL},
	{NhlNcnAreaMapCRange, NhlCcnAreaMapCRange,NhlTInteger,
		 sizeof(int),Oset(amap_crange),NhlTImmediate,
		 _NhlUSET((NhlPointer) 100000),_NhlRES_PRIVATE,NULL},
	{NhlNcnConpackParams, NhlCcnConpackParams,NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(conpack_params),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),_NhlRES_PRIVATE,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNcnDataChanged,NhlCcnDataChanged,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(data_changed),NhlTImmediate,
		 _NhlUSET((NhlPointer) True),_NhlRES_PRIVATE,NULL},

/* Intercepted resources */

	{NhlNtrXTensionF,NhlCtrXTensionF,NhlTFloat,sizeof(float),
		Oset(x_tension),NhlTString,"2.0",
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},
	{NhlNtrYTensionF,NhlCtrYTensionF,NhlTFloat,sizeof(float),
		Oset(y_tension),NhlTString,"2.0",
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},
	{ NhlNpmLabelBarDisplayMode,NhlCpmLabelBarDisplayMode,
		 NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		 Oset(display_labelbar),NhlTImmediate,
          	 _NhlUSET((NhlPointer) NhlNEVER),_NhlRES_INTERCEPTED,NULL},
	{NhlNlbLabelStrings, NhlClbLabelStrings, NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(lbar_labels_res),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),_NhlRES_INTERCEPTED,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNlbLabelFuncCode, NhlCTextFuncCode, NhlTCharacter,
		 sizeof(char),Oset(lbar_func_code),
		 NhlTString,_NhlUSET(":"),_NhlRES_INTERCEPTED,NULL },
	{NhlNlbLabelAlignment,NhlClbLabelAlignment,NhlTlbLabelAlignmentMode, 
		 sizeof(NhllbLabelAlignmentMode), 
		 Oset(lbar_alignment),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlINTERIOREDGES),
         	_NhlRES_INTERCEPTED,NULL},
		
	{NhlNpmLegendDisplayMode,NhlCpmLegendDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_legend),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNEVER),
         	  _NhlRES_INTERCEPTED,NULL},
	{NhlNlgLabelStrings, NhlClgLabelStrings, NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(lgnd_labels_res),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),_NhlRES_INTERCEPTED,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNlgLabelFuncCode, NhlCTextFuncCode, NhlTCharacter,
		 sizeof(char),Oset(lgnd_func_code),
		 NhlTString,_NhlUSET(":"),_NhlRES_INTERCEPTED,NULL },
	{NhlNlgLineLabelsOn,NhlClgLineLabelsOn,NhlTBoolean,
		 sizeof(NhlBoolean), Oset(draw_lgnd_line_lbls),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_INTERCEPTED,NULL},
	{NhlNpmTickMarkDisplayMode,NhlCpmTickMarkDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_tickmarks),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlCONDITIONAL),
         	  _NhlRES_INTERCEPTED,NULL},
	{NhlNpmTitleDisplayMode,NhlCpmTitleDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_titles),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlCONDITIONAL),
         	  _NhlRES_INTERCEPTED,NULL},
	{ NhlNpmUpdateReq,NhlCpmUpdateReq,NhlTInteger,sizeof(int),
		  Oset(update_req),NhlTImmediate,
		  _NhlUSET((NhlPointer) False),_NhlRES_PRIVATE,NULL}
};
#undef Oset

typedef enum _cnCoord { cnXCOORD, cnYCOORD} cnCoord;
typedef enum _cnValueType { 
	cnCONSTFVAL, 
	cnCONINTERVAL,
	cnCONMINVAL,
	cnCONMAXVAL,
	cnDATAMINVAL,
	cnDATAMAXVAL,
	cnSCALEFACTOR
} cnValueType;

typedef enum _cnLabelType { 
	cnLINELBL,
	cnINFOLBL,
	cnHIGHLBL,
	cnLOWLBL,
	cnCONSTFLBL
} cnLabelType;

/* base methods */

static NhlErrorTypes ContourPlotClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes ContourPlotClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes ContourPlotInitialize(
#if	NhlNeedProto
        NhlClass,  /* class */
        NhlLayer,       /* req */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes ContourPlotSetValues(
#if	NhlNeedProto
        NhlLayer,       /* old */
        NhlLayer,       /* reference */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);


static NhlErrorTypes    ContourPlotGetValues(
#if	NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
#endif
);

static NhlErrorTypes ContourPlotDestroy(
#if	NhlNeedProto
        NhlLayer        /* inst */
#endif
);

static NhlErrorTypes ContourPlotGetBB(
#if	NhlNeedProto
        NhlLayer        instance,
        NhlBoundingBox	*thebox
#endif
);

static NhlErrorTypes ContourPlotPreDraw(
#if	NhlNeedProto
        NhlLayer	/* layer */
#endif
);

static NhlErrorTypes ContourPlotDraw(
#if	NhlNeedProto
        NhlLayer	/* layer */
#endif
);

static NhlErrorTypes ContourPlotPostDraw(
#if	NhlNeedProto
        NhlLayer	/* layer */
#endif
);


static NhlErrorTypes cnDraw(
#if	NhlNeedProto
        NhlContourPlotLayer	cnl,
	NhlDrawOrder		order,
	NhlString		entry_name		    
#endif
);

static NhlErrorTypes cnInitDraw(
#if	NhlNeedProto
	NhlContourPlotLayer	cnl,
	NhlString	entry_name
#endif
);

static NhlErrorTypes cnInitAreamap(
#if	NhlNeedProto
	NhlContourPlotLayer	cnl,
	NhlString	entry_name
#endif
);

static NhlErrorTypes cnInitCellArray(
#if	NhlNeedProto
	NhlContourPlotLayer	cnl,
	int		*msize,
	int		*nsize,
	NhlBoundingBox	*bbox,
	NhlString	entry_name
#endif
);

static NhlErrorTypes ContourPlotUpdateData(
#if	NhlNeedProto
	NhlDataCommLayer	new,
	NhlDataCommLayer	old
#endif
);

static NhlErrorTypes ContourPlotDataInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

/* internal static functions */

static NhlErrorTypes AddDataBoundToAreamap(
#if	NhlNeedProto
	NhlContourPlotLayer	cl,
	NhlString	entry_name
#endif
);

static NhlErrorTypes InitCoordBounds(
#if	NhlNeedProto
        NhlContourPlotLayer	cl,
        NhlContourPlotLayer	ocl,
	char			*entry_name
#endif
);

static NhlErrorTypes SetUpLLTransObj(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init
#endif
);

static NhlErrorTypes SetCoordBounds(
#if	NhlNeedProto
	NhlContourPlotLayer	cnl,
	cnCoord			ctype,
	int			count,
	NhlString		entry_name
#endif
);

static NhlErrorTypes SetUpIrrTransObj(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init
#endif
);

static NhlErrorTypes SetLabelFormats(
#if	NhlNeedProto
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init
#endif
);

static NhlErrorTypes ManageLabels(
#if	NhlNeedProto
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes SetLabelScale(
#if	NhlNeedProto
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init
#endif
);

static NhlErrorTypes ManageOverlay(
#if	NhlNeedProto
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes ManageTickMarks(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);


static NhlErrorTypes ManageTitles(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);


static NhlErrorTypes ManageLegend(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes ManageLabelBar(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes SetLabelString(
#if	NhlNeedProto
	NhlString *dest_str,
	NhlString source_str,
	NhlString def_str,
	NhlString entry_name
#endif
);

static NhlErrorTypes ManageInfoLabel(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes ManageConstFLabel(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes ManageAnnotation(
#if	NhlNeedProto
	NhlContourPlotLayer		cnnew,
	NhlContourPlotLayerPart	*ocnp,
	NhlBoolean		init,
	_cnAnnoType		atype
#endif
);

static NhlErrorTypes SetTextPosition(
#if	NhlNeedProto
	NhlContourPlotLayer		cnnew,
	NhlContourPlotLayerPart	*ocnp,
	_cnAnnoType		atype,
	NhlBoolean		*pos_changed,
	NhlString		entry_name
#endif
);

static NhlErrorTypes ReplaceSubstitutionChars(
#if	NhlNeedProto
	NhlContourPlotLayerPart	*cnp,
	NhlContourPlotLayerPart	*ocnp,
	NhlBoolean		init,
	_cnAnnoType		atype,
	NhlBoolean		*text_changed,
	NhlString		entry_name
#endif
);

static void Substitute(
#if	NhlNeedProto
	char		*buf,
	int		replace_count,
	char		*subst
#endif
);


static NhlErrorTypes SetFormatRec(
#if	NhlNeedProto
	NhlFormatRec	*format,
	NhlString	resource,
	NhlString	entry_name
#endif
);

static char *ContourPlotFormat(
#if	NhlNeedProto
	NhlContourPlotLayerPart	*cnp,
	cnValueType		vtype,
	NhlFormatRec		*format,
        char			func_code,			       
	NhlString		entry_name
#endif
);

static NhlErrorTypes    cnComputeRefLevel(
#if	NhlNeedProto
	NhlContourPlotLayerPart	*cnp,
	float			*levels,
	NhlString		entry_name
#endif
);

static NhlErrorTypes    SetupLevels(
#if	NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	float		**levels,
	NhlBoolean	*modified				    
#endif
);

static NhlErrorTypes    SetupLevelsManual(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew, 
	NhlContourPlotLayer	cold,
	float		**levels,
	char		*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsEqual(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew, 
	NhlContourPlotLayer	cold,
	float		**levels,
	char		*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsAutomatic(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew, 
	NhlContourPlotLayer	cold,
	float		**levels,
	char		*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsExplicit(
#if	NhlNeedProto
	NhlContourPlotLayer	cnew, 
	NhlContourPlotLayer	cold,
	NhlBoolean	init,
	float		**levels,
	char		*entry_name
#endif
);

static NhlErrorTypes ChooseSpacingLin(
#if	NhlNeedProto
	float		*tstart,
	float		*tend,
	float		*spacing,
	int		convert_precision,
	int		max_ticks,
	NhlString	entry_name
#endif
);

static NhlErrorTypes    ManageData(
#if	NhlNeedProto
	NhlContourPlotLayer	cnnew, 
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    ManageViewDepResources(
#if	NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    CopyTextAttrs(
#if	NhlNeedProto
	NhlcnLabelAttrs *dest,
	NhlcnLabelAttrs *source,
	NhlString	entry_name
#endif
);

static NhlErrorTypes    AdjustText(
#if	NhlNeedProto
	NhlcnLabelAttrs *lbl_attrp,
	NhlContourPlotLayer	new, 
	NhlContourPlotLayer	old,
	NhlBoolean	init
#endif
);

static NhlErrorTypes    ManageDynamicArrays(
#if	NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    ManageGenArray(
#if	NhlNeedProto
	NhlGenArray	*ga,
	int		count,
	NhlGenArray	copy_ga,
	NrmQuark	type,
	NhlPointer	init_val,
	int		*old_count,
	int		*init_count,
	NhlBoolean	*need_check,
	NhlBoolean	*changed,				       
	NhlString	resource_name,
	NhlString	entry_name
#endif
);

static NhlErrorTypes	CheckColorArray(
#if	NhlNeedProto
	NhlContourPlotLayer	cl,
	NhlGenArray	ga,
	int		count,
	int		init_count,
	int		last_count,
	int		**gks_colors,
	NhlString	resource_name,
	NhlString	entry_name
#endif
);

static NhlGenArray GenArraySubsetCopy(
#if	NhlNeedProto
        NhlGenArray     ga,
        int             length
#endif
);

static NhlErrorTypes GetData(
#if	NhlNeedProto
	NhlContourPlotLayer	cl,
	float		**scalar_field,
	int		*first_dim,
	int		*second_dim
#endif
);

static NhlErrorTypes UpdateLineAndLabelParams(
#if	NhlNeedProto
	NhlContourPlotLayer cl,
	NhlBoolean	*do_lines,
	NhlBoolean	*do_fill
#endif
);

static void SetRegionAttrs(
#if	NhlNeedProto
	NhlContourPlotLayer cl,
	NhlcnRegionAttrs *reg_attrs, 
	int cpix
#endif
);

static NhlErrorTypes UpdateFillInfo(
#if	NhlNeedProto
	NhlContourPlotLayer cl,
	NhlBoolean	*do_fill
#endif
);

extern int (_NHLCALLF(cpdrpl,CPDRPL))(
#if	NhlNeedProto
	float *xcs, 
	float *ycs,
	int *ncs,
	int *iai,
	int *iag,
	int *nai
#endif
);

extern int (_NHLCALLF(hlucpfill,HLUCPFILL))(
#if	NhlNeedProto
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
#endif
);

extern void  (_NHLCALLF(hlucpscae,HLUCPSCAE))(
#if	NhlNeedProto
	int		*icra,
	int		*ica1,
	int		*icam,
	int		*ican,
	float		*xcpf,
	float		*ycpf,
	float		*xcqf,
	float		*ycqf,
	int		*ind1,
	int		*ind2,
	int		*icaf,
	int		*iaid		      
#endif
);

extern void   (_NHLCALLF(hlucpchcl,HLUCPCHCL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hlucpchhl,HLUCPCHHL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hlucpchll,HLUCPCHLL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hlucpmpxy,HLUCPMPXY))(
#if	NhlNeedProto
	int	*imap,
	float	*xinp,
	float	*yinp,
	float	*xotp,
	float	*yotp
#endif
);

static void   load_hlucp_routines(
#if	NhlNeedProto
	NhlBoolean	flag
#endif
);


NhlContourPlotDataDepClassRec NhlcontourPlotDataDepClassRec = {
	/* base_class */
        {
/* class_name			*/	"contourPlotDataDepClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlContourPlotDataDepLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)
						&NhldataSpecClassRec,

/* cvt_table			*/	NULL,
/* layer_resources		*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	ContourPlotDataInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	},
	/* dataspec_class */
	{
/* foo				*/	0
	},
	/* contour datadep_class */
	{
/* foo				*/	0
	}
};

NhlContourPlotClassRec NhlcontourPlotClassRec = {
	/* base_class */
        {
/* class_name			*/      "contourPlotClass",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlContourPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/  (NhlClass)&NhldataCommClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	ContourPlotClassPartInitialize,
/* class_initialize		*/	ContourPlotClassInitialize,
/* layer_initialize		*/	ContourPlotInitialize,
/* layer_set_values		*/	ContourPlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	ContourPlotGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ContourPlotDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      ContourPlotDraw,

/* layer_pre_draw		*/      ContourPlotPreDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      ContourPlotPostDraw,
/* layer_clear			*/      NULL

        },
	/* view_class */
	{
/* segment_wkid			*/	0,
/* get_bb			*/	ContourPlotGetBB
	},
	/* trans_class */
	{
/* overlay_capability 		*/	_tfOverlayBaseOrMember,
/* data_to_ndc			*/	NhlInheritTransFunc,
/* ndc_to_data			*/	NhlInheritTransFunc,
/* data_polyline		*/	NhlInheritPolyTransFunc,
/* ndc_polyline			*/	NhlInheritPolyTransFunc,
/* data_polygon			*/	NhlInheritPolyTransFunc,
/* ndc_polygon			*/	NhlInheritPolyTransFunc,
/* data_polymarker		*/	NhlInheritPolyTransFunc,
/* ndc_polymarker		*/	NhlInheritPolyTransFunc
	},
	/* datacomm_class */
	{
/* data_offsets			*/	NULL,
/* update_data			*/	ContourPlotUpdateData
	},
	{
/* foo				*/	NULL
	}
};
	

NhlClass NhlcontourPlotDataDepClass =
		(NhlClass) &NhlcontourPlotDataDepClassRec;
NhlClass NhlcontourPlotClass = 
		(NhlClass) &NhlcontourPlotClassRec;

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qcolorindex = NrmNULLQUARK;
static NrmQuark Qfillindex = NrmNULLQUARK;
static NrmQuark Qdashindex = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark	Qlevels = NrmNULLQUARK; 
static NrmQuark	Qlevel_flags = NrmNULLQUARK; 
static NrmQuark	Qfill_colors = NrmNULLQUARK;
static NrmQuark	Qfill_patterns = NrmNULLQUARK;
static NrmQuark	Qfill_scales = NrmNULLQUARK;
static NrmQuark	Qline_colors = NrmNULLQUARK; 
static NrmQuark	Qline_dash_patterns = NrmNULLQUARK; 
static NrmQuark	Qline_thicknesses = NrmNULLQUARK; 
static NrmQuark	Qllabel_strings = NrmNULLQUARK;
static NrmQuark	Qllabel_colors = NrmNULLQUARK; 
static NrmQuark	Qline_label_format = NrmNULLQUARK; 
static NrmQuark	Qmax_data_value_format = NrmNULLQUARK; 
static NrmQuark	Qhigh_label_string = NrmNULLQUARK; 
static NrmQuark	Qhigh_label_format = NrmNULLQUARK; 
static NrmQuark	Qlow_label_string = NrmNULLQUARK; 
static NrmQuark	Qlow_label_format = NrmNULLQUARK; 
static NrmQuark	Qinfo_label_string = NrmNULLQUARK; 
static NrmQuark	Qinfo_label_format = NrmNULLQUARK; 
static NrmQuark	Qno_data_label_string = NrmNULLQUARK; 
static NrmQuark	Qconst_f_label_string = NrmNULLQUARK; 
static NrmQuark	Qconst_f_label_format = NrmNULLQUARK; 
static NrmQuark	Qlgnd_level_flags = NrmNULLQUARK;
static NrmQuark	Qlg_label_strings = NrmNULLQUARK;
static NrmQuark	Qlb_label_strings = NrmNULLQUARK;



typedef enum { 
	cnInt,
	cnFloat,
	cnString 
} _cnParamType;

typedef struct _cnCp_Params {
	NhlString	name;
	_cnParamType	type;
} cnCp_Params;

static cnCp_Params Cp_Params[] = {
{"HCL", cnFloat},
{"HCS", cnFloat}, 
{"HCF", cnInt}, 
{"HLX", cnInt}, 
{"HLY", cnInt}, 
{"IWM", cnInt}, 
{"PC1", cnFloat},
{"PC2", cnFloat}, 
{"PC3", cnFloat}, 
{"PC4", cnFloat}, 
{"PC5", cnFloat}, 
{"PC6", cnFloat},
{"PIC", cnInt}, 
{"PIE", cnInt},
{"PW1", cnFloat}, 
{"PW2", cnFloat}, 
{"PW3", cnFloat},
{"PW4", cnFloat}, 
{"RC1", cnFloat}, 
{"RC2", cnFloat}, 
{"RC3", cnFloat}, 
{"RWC", cnInt}, 
{"RWG", cnInt}, 
{"RWM", cnInt},
};

static NhlString cnEmptyString = "";

#define NhlDASHBUFSIZE	128

static NhlContourPlotLayer	Cnl = NULL;
static NhlContourPlotLayerPart	*Cnp = NULL;

/*
 * Function:	nhlfcontourplotlayerclass
 *
 * Description:	fortran ref to contour class
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
_NHLCALLF(nhlfcontourplotclass,NHLFCONTOURPLOTCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlcontourPlotClass;
}

/*
 * Function:	nhlfcontourplotdatadeplayerclass
 *
 * Description:	fortran ref to contourplot datadep class
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
_NHLCALLF(nhlfcontourplotdatadepclass,NHLFCONTOURPLOTDATADEPCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlcontourPlotDataDepClass;
}

/*
 * Function:	ContourPlotDataInitialize
 *
 * Description:	Initializes the ContourPlotData Dependent class instance.
 *
 * In Args:	
 *		NhlClass	class,
 *		NhlLayer		req,
 *		NhlLayer		new,
 *		_NhlArgList	args,
 *		int		num_args
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
ContourPlotDataInitialize
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer		req,
	NhlLayer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass      class;
        NhlLayer           req;
        NhlLayer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;

	return ret;
}


/*
 * Function:	ContourPlotUpdateData
 *
 * Description:	This function is called whenever the data pointed to by the
 *		data resources change.  This function needs to check if
 *		this specific data resource changed - it may have been another
 *		data resource in a sub/super class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
ContourPlotUpdateData
#if	NhlNeedProto
(
	NhlDataCommLayer	new,
	NhlDataCommLayer	old
)
#else
(new,old)
	NhlDataCommLayer	new;
	NhlDataCommLayer	old;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;

/*
 * For now simply call SetValues setting the data changed resource true
 */
	NhlVASetValues(new->base.id,NhlNcnDataChanged,True,
		       NULL);

	return ret;
}

/*
 * Function:	ContourPlotClassInitialize
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
ContourPlotClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{

        _NhlEnumVals   leveluselist[] = {
        {NhlNOLINE,		"NoLine"},
        {NhlLINEONLY, 		"LineOnly"},
        {NhlLABELONLY, 		"LabelOnly"},
        {NhlLINEANDLABEL,      "LineAndLabel"}
        };

        _NhlEnumVals   linelabelplacementlist[] = {
        {NhlCONSTANT, 		"Constant"},
        {NhlRANDOMIZED, 	"Randomized"},
        {NhlCOMPUTED,      	"Computed"}
        };

        _NhlEnumVals   highlowlabeloverlaplist[] = {
	{NhlIGNOREOVERLAP,		"IgnoreOverlap"},
	{NhlOMITOVERINFO,		"OmitOverInfo"},
	{NhlOMITOVERHL,			"OmitOverHL"},
	{NhlOMITOVERHLANDINFO,		"OmitOverHLAndInfo"},
	{NhlOMITOVERVP,			"OmitOverVP"},
	{NhlOMITOVERVPANDINFO,		"OmitOverVPAndInfo"},
	{NhlOMITOVERVPANDHL,		"OmitOverVPandHL"},
	{NhlOMITOVERVPANDHLANDINFO,	"OmitOverVPAndHLAndInfo"},
	{NhlADJUSTVP,			"AdjustVP"},
	{NhlADJUSTVPOMITOVERINFO,	"AdjustVPOmitOverInfo"},
	{NhlADJUSTVPOMITOVERHL,		"AdjustVPOmitOverHL"},
	{NhlADJUSTVPOMITOVERHLANDINFO,	"AdjustVPOmitOverHLAndInfo"}
        };

	load_hlucp_routines(False);

	_NhlRegisterEnumType(NhlcontourPlotClass,NhlTcnLevelUseMode,
		leveluselist,NhlNumber(leveluselist));
	_NhlRegisterEnumType(NhlcontourPlotClass,NhlTcnLineLabelPlacementMode,
			     linelabelplacementlist,
			     NhlNumber(linelabelplacementlist));
	_NhlRegisterEnumType(NhlcontourPlotClass,NhlTcnHighLowLabelOverlapMode,
			     highlowlabeloverlaplist,
			     NhlNumber(highlowlabeloverlaplist));

	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qcolorindex = NrmStringToQuark(NhlTColorIndex);
	Qfillindex = NrmStringToQuark(NhlTFillIndex);
	Qdashindex = NrmStringToQuark(NhlTDashIndex);
	Qlevels = NrmStringToQuark(NhlNcnLevels);
	Qlevel_flags = NrmStringToQuark(NhlNcnLevelFlags);
	Qfill_colors = NrmStringToQuark(NhlNcnFillColors);
	Qfill_patterns = NrmStringToQuark(NhlNcnFillPatterns);
	Qfill_scales = NrmStringToQuark(NhlNcnFillScales);
	Qline_colors = NrmStringToQuark(NhlNcnLineColors);
	Qline_dash_patterns = NrmStringToQuark(NhlNcnLineDashPatterns);
	Qline_thicknesses = NrmStringToQuark(NhlNcnLineThicknesses);
	Qllabel_strings = NrmStringToQuark(NhlNcnLineLabelStrings);
	Qllabel_colors = NrmStringToQuark(NhlNcnLineLabelFontColors);
	Qlgnd_level_flags = NrmStringToQuark(NhlNcnLegendLevelFlags);
	Qline_label_format = NrmStringToQuark(NhlNcnLineLabelFormat);
	Qmax_data_value_format = NrmStringToQuark(NhlNcnMaxDataValueFormat);
	Qhigh_label_string = NrmStringToQuark(NhlNcnHighLabelString);
	Qhigh_label_format = NrmStringToQuark(NhlNcnHighLabelFormat);
	Qlow_label_string = NrmStringToQuark(NhlNcnLowLabelString);
	Qlow_label_format = NrmStringToQuark(NhlNcnLowLabelFormat);
	Qinfo_label_string = NrmStringToQuark(NhlNcnInfoLabelString);
	Qinfo_label_format = NrmStringToQuark(NhlNcnInfoLabelFormat);
	Qconst_f_label_string = NrmStringToQuark(NhlNcnConstFLabelString);
	Qno_data_label_string = NrmStringToQuark(NhlNcnNoDataLabelString);
	Qconst_f_label_format = NrmStringToQuark(NhlNcnConstFLabelFormat);
	Qlg_label_strings = NrmStringToQuark(NhlNlgLabelStrings);
	Qlb_label_strings = NrmStringToQuark(NhlNlbLabelStrings);

	return NhlNOERROR;
}

/*
 * Function:	ContourPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlContourPlotClassPart that cannot be initialized statically.
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
ContourPlotClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	NhlClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;
	char		*entry_name = "ContourPlotClassPartInitialize";

/*
 * Register children objects
 * NOTE: order of registration should be the reverse of the
 * desired 'canonical' order
 */
	subret = _NhlRegisterChildClass(lc,NhlplotManagerClass,
					False,False,
					NhlNpmLegendDisplayMode,
					NhlNpmLabelBarDisplayMode,
					NhlNpmTickMarkDisplayMode,
					NhlNpmTitleDisplayMode,
					NhlNlgItemCount,
					NhlNlgMonoDashIndex,
					NhlNlgMonoMarkerIndex,
					NhlNlgDashIndex,
					NhlNlgMarkerIndex,
					NhlNlgDashIndexes,
					NhlNlgMarkerIndexes,
					NhlNlgLabelStrings,
					NhlNlgLabelFuncCode,
					NhlNlgMonoLineColor,
					NhlNlgLineColor,
					NhlNlgLineColors,
					NhlNlgMonoMarkerColor,
					NhlNlgMarkerColor,
					NhlNlgMarkerColors,
					NhlNlgMonoLineThickness,
					NhlNlgLineThicknessF,
					NhlNlgLineThicknesses,
					NhlNlgMonoMarkerThickness,
					NhlNlgMarkerThicknessF,
					NhlNlgMarkerThicknesses,
					NhlNlgLineLabelStrings,
					NhlNlgMonoLineLabelFontColor,
					NhlNlgLineLabelFontColor,
					NhlNlgLineLabelFontColors,
					NhlNlgMonoLineLabelFontHeight,
					NhlNlgLineLabelFontHeightF,
					NhlNlgLineLabelFontHeights,
					NhlNlgMonoMarkerSize,
					NhlNlgMarkerSizeF,
					NhlNlgMarkerSizes,
					NhlNlgLineLabelsOn,
					NhlNlgLineLabelFontAspectF,
					NhlNlgLineLabelFontThicknessF,
					NhlNlgLineLabelFontQuality,
					NhlNlgLineLabelConstantSpacingF,
					NhlNlgLineLabelFuncCode,
					NhlNlbBoxCount,
					NhlNlbLabelAlignment,
					NhlNlbLabelStrings,
					NhlNlbLabelFuncCode,
					NhlNlbMonoFillColor,
					NhlNlbFillColor,
					NhlNlbFillColors,
					NhlNlbMonoFillPattern,
					NhlNlbFillPattern,
					NhlNlbFillPatterns,
					NhlNlbMonoFillScale,
					NhlNlbFillScaleF,
					NhlNlbFillScales,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlplotManagerClass");
		return(NhlFATAL);
	}
        subret = _NhlRegisterChildClass(lc,NhlirregularTransObjClass,
					False,True,NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlirregularTransObjClass");
		return(NhlFATAL);
	}

        subret = _NhlRegisterChildClass(lc,NhllogLinTransObjClass,
					False,True,NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhltransObjClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterDataRes((NhlDataCommClass)lc,
				     NhlNcnScalarFieldData,
				     NULL,
				     NhlcontourPlotDataDepClass,
				     NhlscalarFieldFloatClass,
				     NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering data resource %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlcoordArrTableFloatClass");
		return(NhlFATAL);
	}

	return ret;
}
 
/*
 * Function:	ContourPlotInitialize
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
 * Side Effects:	state change in GKS due to mapping transformations.
 */
/*ARGSUSED*/
static NhlErrorTypes
ContourPlotInitialize
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer		req,
	NhlLayer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass      class;
        NhlLayer           req;
        NhlLayer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "ContourPlotInitialize";
	char			*e_text;
	NhlContourPlotLayer	cnew = (NhlContourPlotLayer) new;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	NhlSArg			sargs[64];
	int			nargs = 0;

/* Initialize ContourPlot float and integer workspaces */

	if ((cnp->iws_id =_NhlNewWorkspace(NhlwsCNINT,NhlwsNONE,
					   4000*sizeof(int))) < 0) {
		e_text = "%s: integer workspace allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if ((cnp->fws_id = _NhlNewWorkspace(NhlwsCNFLOAT,NhlwsNONE,
					    4000*sizeof(float))) < 0) {
		e_text = "%s: float workspace allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	cnp->aws_id = -1;
	cnp->cws_id = -1;
	cnp->info_anno_id = NhlNULLOBJID;
	cnp->constf_anno_id = NhlNULLOBJID;
	cnp->info_lbl_rec.id = NhlNULLOBJID;
	cnp->constf_lbl_rec.id = NhlNULLOBJID;
	cnp->ref_level = 0;

/* Initialize unset resources */

	if (! cnp->level_spacing_set) cnp->level_spacing = 5.0;
	if (! cnp->min_level_set) cnp->min_level_val = -FLT_MAX;
	if (! cnp->max_level_set) cnp->max_level_val = FLT_MAX;
	if (! cnp->llabel_interval_set) cnp->llabel_interval = 2;
	if (! cnp->line_dash_seglen_set) 
		cnp->line_dash_seglen = 0.15;
	if (! cnp->cell_size_set || cnp->cell_size <= 0.0) {
		cnp->cell_size = 0.0;
                cnp->sticky_cell_size_set = False;
        }
        else
                cnp->sticky_cell_size_set = True;

	if (! cnp->line_lbls.height_set) 
		cnp->line_lbls.height = 0.012;
	if (! cnp->high_lbls.height_set) 
		cnp->high_lbls.height = 0.012;
	if (! cnp->low_lbls.height_set) 
		cnp->low_lbls.height = 0.0012;
	if (! cnp->info_lbl.height_set) 
		cnp->info_lbl.height = 0.012;
	if (! cnp->constf_lbl.height_set) 
		cnp->constf_lbl.height = 0.012;
/*
 * raster mode implies fill on
 */
        if (cnp->raster_mode_on)
                cnp->fill_on = True;

/* Initialize private members */

	cnp->line_lbls.fcode[1] = '\0';
	cnp->info_lbl.fcode[1] = '\0';
	cnp->high_lbls.fcode[1] = '\0';
	cnp->low_lbls.fcode[1] = '\0';
	cnp->constf_lbl.fcode[1] = '\0';
	cnp->info_lbl.text = NULL;
	cnp->constf_lbl.text = NULL;
	cnp->line_lbls.text = NULL;
	cnp->new_draw_req = True;
	cnp->predraw_dat = NULL;
	cnp->draw_dat = NULL;
	cnp->postdraw_dat = NULL;
	cnp->update_req = False;
	cnp->overlay_object = NULL;
	cnp->data_changed = True;
	cnp->ll_strings = NULL;
	cnp->use_irr_trans = False;
	cnp->const_field = False;
	cnp->lbar_labels_set = False;
	cnp->lbar_labels = NULL;
	cnp->lbar_labels_res_set = cnp->lbar_labels_res ? True : False;
	cnp->lgnd_labels_set = False;
	cnp->lgnd_labels = NULL;
	cnp->lgnd_labels_res_set = cnp->lgnd_labels_res ? True : False;
	cnp->lgnd_line_count = 0;
	cnp->lgnd_l_colors = NULL;
	cnp->lgnd_l_dash_pats = NULL;
	cnp->lgnd_l_thicknesses = NULL;
	cnp->lgnd_ll_font_colors = NULL;
	cnp->lgnd_ll_strings = NULL;
	cnp->dash_table = NULL;
	cnp->sfp = NULL;
	cnp->osfp = NULL;
        cnp->gks_fill_colors = NULL;
        cnp->gks_line_colors = NULL;
        cnp->gks_llabel_colors = NULL;
        cnp->grid_bound.fill_color = cnp->out_of_range.fill_color = 0;
        cnp->grid_bound.fill_pat = cnp->out_of_range.fill_pat = 0;
        cnp->grid_bound.fill_scale = cnp->out_of_range.fill_scale = 1.0;

/*
 * Set up the data
 */
	subret = ManageData(cnew,(NhlContourPlotLayer) req,True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
 

/* Set view dependent resources */

	subret = ManageViewDepResources(new,req,True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set the label formats - must precede dynamic array handling */

	subret = SetLabelFormats(cnew,(NhlContourPlotLayer)req,True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting label formats";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set up the dynamic arrays (line label array is handled here) */

	subret = ManageDynamicArrays(new,req,True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* 
 * Set up the labels (except for the line label array) 
 * Note: may add arguments to the PlotManager argument list.
 */

	subret = ManageLabels(cnew,(NhlContourPlotLayer)req,True,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing labels";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set up the contour object transformation  */
        
	subret = InitCoordBounds(cnew,NULL,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return(ret);

	if (cnp->use_irr_trans) {
		subret = SetUpIrrTransObj(cnew,(NhlContourPlotLayer) req,True);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}
	else {
		subret = SetUpLLTransObj(cnew,(NhlContourPlotLayer) req,True);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}

/* 
 * Manage the PlotManager (including setting up the annotations managed by it)
 */
	subret = ManageOverlay(cnew,
			       (NhlContourPlotLayer)req,True,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		return(ret);
	}

	if (cnew->trans.overlay_status != _tfNotInOverlay) {
		if (cnp->constf_lbl.on || cnp->no_data_label_on) {
			subret = ManageAnnotation(cnew,cnp,True,_cnCONSTF);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
		if (cnp->info_lbl.on) {
			subret = ManageAnnotation(cnew,cnp,True,_cnINFO);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
	}

	cnp->data_changed = False;
	cnp->level_spacing_set = False;
	cnp->line_dash_seglen_set = False;
	cnp->cell_size_set = False;
	cnp->line_lbls.height_set = False;
	cnp->high_lbls.height_set = False;
	cnp->low_lbls.height_set = False;
	cnp->info_lbl.height_set = False;
	cnp->constf_lbl.height_set = False;
	cnp->lbar_labels_res_set = False;
	cnp->lgnd_labels_res_set = False;
        
        cnew->trans.x_reverse_set = cnew->trans.y_reverse_set = False;
        cnew->trans.x_log_set = cnew->trans.y_log_set = False;
        cnew->trans.x_axis_type_set = cnew->trans.y_axis_type_set = False;
        cnew->trans.x_min_set = cnew->trans.y_min_set = False;
        cnew->trans.x_max_set = cnew->trans.y_max_set = False;

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
		NhlNcnExplicitLabelBarLabelsOn,
		NhlNcnLabelBarEndLabelsOn,
		NhlNcnExplicitLegendLabelsOn,
		NhlNcnLegendLevelFlags,
		NhlNcnInfoLabelOn,
		NhlNcnInfoLabelString,
		NhlNcnInfoLabelFormat,
		NhlNcnInfoLabelFontHeightF,
		NhlNcnInfoLabelTextDirection,
		NhlNcnInfoLabelFont,
		NhlNcnInfoLabelFontColor,
		NhlNcnInfoLabelFontAspectF,
		NhlNcnInfoLabelFontThicknessF,
		NhlNcnInfoLabelFontQuality,
		NhlNcnInfoLabelConstantSpacingF,
		NhlNcnInfoLabelAngleF,
		NhlNcnInfoLabelFuncCode,
		NhlNcnInfoLabelBackgroundColor,
		NhlNcnInfoLabelPerimOn,
		NhlNcnInfoLabelPerimSpaceF,
		NhlNcnInfoLabelPerimColor,
		NhlNcnInfoLabelPerimThicknessF,
		NhlNcnInfoLabelZone,
		NhlNcnInfoLabelSide,
		NhlNcnInfoLabelJust,
		NhlNcnInfoLabelParallelPosF,
		NhlNcnInfoLabelOrthogonalPosF,
		NhlNcnNoDataLabelOn,
		NhlNcnNoDataLabelString,
		NhlNcnConstFLabelOn,
		NhlNcnConstFLabelString,
		NhlNcnConstFLabelFormat,
		NhlNcnConstFLabelFontHeightF,
		NhlNcnConstFLabelTextDirection,
		NhlNcnConstFLabelFont,
		NhlNcnConstFLabelFontColor,
		NhlNcnConstFLabelFontAspectF,
		NhlNcnConstFLabelFontThicknessF,
		NhlNcnConstFLabelFontQuality,
		NhlNcnConstFLabelConstantSpacingF,
		NhlNcnConstFLabelAngleF,
		NhlNcnConstFLabelFuncCode,
		NhlNcnConstFLabelBackgroundColor,
		NhlNcnConstFLabelPerimOn,
		NhlNcnConstFLabelPerimSpaceF,
		NhlNcnConstFLabelPerimColor,
		NhlNcnConstFLabelPerimThicknessF,
		NhlNcnConstFLabelZone,
		NhlNcnConstFLabelSide,
		NhlNcnConstFLabelJust,
		NhlNcnConstFLabelParallelPosF,
		NhlNcnConstFLabelOrthogonalPosF,
		NhlNpmLabelBarDisplayMode,
		NhlNlbLabelStrings,
		NhlNlbLabelFuncCode,
		NhlNlbLabelAlignment,
		NhlNpmLegendDisplayMode,
		NhlNlgLabelStrings,
		NhlNlgLabelFuncCode,
		NhlNlgLineLabelsOn,
		NhlNpmTickMarkDisplayMode,
		NhlNpmTitleDisplayMode
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
 * Function:	ContourPlotSetValues
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
static NhlErrorTypes ContourPlotSetValues
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
	char			*entry_name = "ContourPlotSetValues";
	char			*e_text;
	NhlContourPlotLayer		cnew = (NhlContourPlotLayer) new;
 	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	NhlContourPlotLayer		cold = (NhlContourPlotLayer) old;
	/* Note that both ManageLegend and ManageLabelBar add to sargs */
	NhlSArg			sargs[128];
	int			nargs = 0;
	int			view_args = 0;

	if (cnew->view.use_segments != cold->view.use_segments) {
		cnp->new_draw_req = True;
	}
	if (cnew->view.use_segments) {
                NhlTransDat *trans_dat = NULL;
                
		if (NewDrawArgs(args,num_args))
			cnp->new_draw_req = True;
                
                if (cnp->draw_dat)
                        trans_dat = cnp->draw_dat;
                else if (cnp->postdraw_dat)
                        trans_dat = cnp->postdraw_dat;
                else if (cnp->predraw_dat)
                        trans_dat = cnp->predraw_dat;
                if (! _NhlSegmentSpansArea(trans_dat,
                                           cnew->view.x,
                                           cnew->view.x + cnew->view.width,
                                           cnew->view.y - cnew->view.height,
                                           cnew->view.y))
                        cnp->new_draw_req = True;

	}

	if (_NhlArgIsSet(args,num_args,NhlNcnLevelSpacingF))
		cnp->level_spacing_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnLineLabelInterval))
		cnp->llabel_interval_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnLineDashSegLenF))
		cnp->line_dash_seglen_set = True;
        if (_NhlArgIsSet(args,num_args,NhlNcnRasterCellSizeF)) {
                if (cnp->cell_size <= 0.0) {
                        cnp->cell_size_set = False;
                        cnp->sticky_cell_size_set = False;
                }
                else {        
                        cnp->cell_size_set = True;
                        cnp->sticky_cell_size_set = True;
                }
        }
	if (_NhlArgIsSet(args,num_args,NhlNcnLineLabelFontHeightF))
		cnp->line_lbls.height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnHighLabelFontHeightF))
		cnp->high_lbls.height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnLowLabelFontHeightF))
		cnp->low_lbls.height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnInfoLabelFontHeightF))
		cnp->info_lbl.height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnConstFLabelFontHeightF))
		cnp->constf_lbl.height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNlbLabelStrings))
		cnp->lbar_labels_res_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNlgLabelStrings))
		cnp->lgnd_labels_res_set = True;
/*
 * raster mode implies fill on
 */
        if (cnp->raster_mode_on)
                cnp->fill_on = True;

/* Manage the data */

	subret = ManageData(cnew,cold,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Set view dependent resources */

	subret = ManageViewDepResources(new,old,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Set the label formats - must precede dynamic array handling */

	subret = SetLabelFormats(cnew,cold,False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting label formats";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the dynamic arrays  (line labels handled here) */

	subret = ManageDynamicArrays(new,old,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* 
 * Set up the labels (except for the line label array) 
 * Note: may add arguments to the PlotManager argument list.
 */

	subret = ManageLabels(cnew,cold,False,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing labels";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Set up the contour object transformation  */
        
	subret = InitCoordBounds(cnew,cold,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return(ret);

	if (cnp->use_irr_trans) {
		subret = SetUpIrrTransObj(cnew,
					  (NhlContourPlotLayer) old,False);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}
	else {
		subret = SetUpLLTransObj(cnew,(NhlContourPlotLayer) old,False);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}
/* 
 * Manage the PlotManager (including the PlotManager annotations)
 */
	subret = ManageOverlay(cnew,cold,False,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		return(ret);
	}

	cnp->update_req = False;
	cnp->data_changed = False;
	cnp->level_spacing_set = False;
	cnp->line_dash_seglen_set = False;
	cnp->cell_size_set = False;
	cnp->high_lbls.height_set = False;
	cnp->low_lbls.height_set = False;
	cnp->line_lbls.height_set = False;
	cnp->info_lbl.height_set = False;
	cnp->constf_lbl.height_set = False;
	cnp->lbar_labels_res_set = False;
	cnp->lgnd_labels_res_set = False;

        cnew->trans.x_reverse_set = cnew->trans.y_reverse_set = False;
        cnew->trans.x_log_set = cnew->trans.y_log_set = False;
        cnew->trans.x_axis_type_set = cnew->trans.y_axis_type_set = False;
        cnew->trans.x_min_set = cnew->trans.y_min_set = False;
        cnew->trans.x_max_set = cnew->trans.y_max_set = False;

	return ret;
}

/*
 * Function:    ContourPlotGetValues
 *
 * Description: Retrieves the current setting of one or more Legend resources.
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
 *              NhlNcnLevels
 *              NhlNcnLevelFlags
 *              NhlNcnFillColors
 *              NhlNcnFillPatterns
 *              NhlNcnFillScales
 *              NhlNcnLineColors
 *              NhlNcnLineDashPatterns
 *              NhlNcnLineThicknesses
 *		NhlNcnLineLabelStrings
 *		NhlNcnLineLabelFontColors
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    ContourPlotGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlContourPlotLayer cl = (NhlContourPlotLayer)l;
        NhlContourPlotLayerPart *cnp = &(cl->contourplot);
        NhlGenArray ga;
	NhlString ts;
        char *e_text;
        int i, count = 0;
        char *type = "";

        for( i = 0; i< num_args; i++ ) {

                ga = NULL;
                if(args[i].quark == Qlevels) {
                        ga = cnp->levels;
                        count = cnp->level_count;
                        type = NhlNcnLevels;
                }
                else if (args[i].quark == Qlevel_flags) {
                        ga = cnp->level_flags;
                        count = cnp->level_count;
                        type = NhlNcnLevelFlags;
                }
                else if (args[i].quark == Qfill_colors) {
                        ga = cnp->fill_colors;
                        count = cnp->fill_count;
                        type = NhlNcnFillColors;
                }
                else if (args[i].quark == Qfill_patterns) {
                        ga = cnp->fill_patterns;
                        count = cnp->fill_count;
                        type = NhlNcnFillPatterns;
                }
                else if (args[i].quark == Qfill_scales) {
                        ga = cnp->fill_scales;
                        count = cnp->fill_count;
                        type = NhlNcnFillScales;
                }
                else if (args[i].quark == Qline_colors) {
                        ga = cnp->line_colors;
                        count = cnp->level_count;
                        type = NhlNcnLineColors;
                }
                else if (args[i].quark == Qline_dash_patterns) {
                        ga = cnp->line_dash_patterns;
                        count = cnp->level_count;
                        type = NhlNcnLineDashPatterns;
                }
                else if (args[i].quark == Qline_thicknesses) {
                        ga = cnp->line_thicknesses;
                        count = cnp->level_count;
                        type = NhlNcnLineThicknesses;
                }
                else if (args[i].quark == Qllabel_strings) {
                        ga = cnp->llabel_strings;
                        count = cnp->level_count;
                        type = NhlNcnLineLabelStrings;
                }
                else if (args[i].quark == Qllabel_colors) {
                        ga = cnp->llabel_colors;
                        count = cnp->level_count;
                        type = NhlNcnLineLabelFontColors;
                } 
                else if (args[i].quark == Qlgnd_level_flags) {
                        ga = cnp->lgnd_level_flags;
                        count = cnp->level_count;
                        type = NhlNcnLegendLevelFlags;
                } 
                if (ga != NULL) {
                        if ((ga = GenArraySubsetCopy(ga, count)) == NULL) {
                                e_text = "%s: error copying %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          "ContourPlotGetValues",type);
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
			continue;
                }

		ts = NULL;
		if(args[i].quark == Qline_label_format){
			ts = cnp->line_lbls.format.fstring;
		}
		else if(args[i].quark == Qmax_data_value_format){
			ts = cnp->max_data_format.fstring;
		}
		else if(args[i].quark == Qhigh_label_string){
			ts = cnp->high_lbls.text;
		}
		else if(args[i].quark == Qhigh_label_format){
			ts = cnp->high_lbls.format.fstring;
		}
		else if(args[i].quark == Qlow_label_string){
			ts = cnp->low_lbls.text;
		}
		else if(args[i].quark == Qlow_label_format){
			ts = cnp->low_lbls.format.fstring;
		}
		else if(args[i].quark == Qinfo_label_string){
			ts = cnp->info_string;
		}
		else if(args[i].quark == Qinfo_label_format){
			ts = cnp->info_lbl.format.fstring;
		}
		else if(args[i].quark == Qno_data_label_string){
			ts = cnp->no_data_string;
		}
		else if(args[i].quark == Qconst_f_label_string){
			ts = cnp->constf_string;
		}
		else if(args[i].quark == Qconst_f_label_format){
			ts = cnp->constf_lbl.format.fstring;
		} 
                if (ts != NULL) {
			*((NhlString*)(args[i].value.ptrval)) =
				NhlMalloc(strlen(ts)+1);
			if(!*((NhlString*)(args[i].value.ptrval))){
                                e_text = "%s: error copying String";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  "ContourPlotGetValues");
                                return NhlFATAL;
                        }
			strcpy(*((NhlString*)(args[i].value.ptrval)),ts);
			continue;
                }
		ga = NULL;
		if (args[i].quark == Qlg_label_strings){
			if (cnp->overlay_object != NULL)
				NhlVAGetValues(cnp->overlay_object->base.id,
					       NhlNlgLabelStrings,&ga,NULL);
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
		else if (args[i].quark == Qlb_label_strings){
			if (cnp->overlay_object != NULL)
				NhlVAGetValues(cnp->overlay_object->base.id,
					       NhlNlbLabelStrings,&ga,NULL);
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
        }

        return(NhlNOERROR);
}


/*
 * Function:  GenArraySubsetCopy
 *
 * Description: Since the internal GenArrays maintained by ContourPlot 
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

static NhlGenArray GenArraySubsetCopy
#if	NhlNeedProto
        (NhlGenArray    ga,
        int             length)
#else
(ga,length)
        NhlGenArray     ga;
        int             length;
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
 * Function:	ContourPlotDestroy
 *
 * Description:
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes ContourPlotDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = 
		&(((NhlContourPlotLayer) inst)->contourplot);
	NhlTransformLayerPart	*cntp = &(((NhlTransformLayer) inst)->trans);
	int			ovbase_id;

/*
 * Note that the transform layer and the contour layer overlay objects
 * may be the same or different. The code must handle either case.
 */

	if (cntp->overlay_status == _tfCurrentOverlayMember ||
	    cntp->overlay_status == _tfCurrentOverlayBase) {
		if (cnp->info_anno_id != NhlNULLOBJID) {
			subret = NhlUnregisterAnnotation
				(inst->base.id,cnp->info_anno_id);
			if ((ret = MIN(subret,ret)) < NhlWARNING)
				return NhlFATAL;
		}
		if (cnp->constf_anno_id != NhlNULLOBJID) {
			subret = NhlUnregisterAnnotation
				(inst->base.id,cnp->constf_anno_id);
			if ((ret = MIN(subret,ret)) < NhlWARNING)
				return NhlFATAL;
		}
	}
	if (cntp->overlay_status == _tfCurrentOverlayMember) {
		ovbase_id = cntp->overlay_object->base.parent->base.id;
		subret = NhlRemoveOverlay(ovbase_id,inst->base.id,False);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return NhlFATAL;
	}

	if (cnp->overlay_object != NULL) {
		(void) _NhlDestroyChild(cnp->overlay_object->base.id,inst);
		cnp->overlay_object = NULL;
	}
	if (cntp->trans_obj != NULL) {
		(void) NhlDestroy(cntp->trans_obj->base.id);
		cntp->trans_obj = NULL;
	}
	if (cnp->info_anno_id != NhlNULLOBJID) {
		(void) NhlDestroy(cnp->info_anno_id);
	}
	if (cnp->constf_anno_id != NhlNULLOBJID) {
		(void) NhlDestroy(cnp->constf_anno_id);
	}
	if (cnp->info_lbl_rec.id != NhlNULLOBJID) {
 		(void) NhlDestroy(cnp->info_lbl_rec.id);
	}
	if (cnp->constf_lbl_rec.id != NhlNULLOBJID) {
 		(void) NhlDestroy(cnp->constf_lbl_rec.id);
	}

	NhlFreeGenArray(cnp->levels);
	NhlFreeGenArray(cnp->level_flags);
	NhlFreeGenArray(cnp->fill_colors);
	NhlFreeGenArray(cnp->fill_patterns);
	NhlFreeGenArray(cnp->fill_scales);
	NhlFreeGenArray(cnp->line_colors);
	NhlFreeGenArray(cnp->dash_table);
	NhlFreeGenArray(cnp->line_dash_patterns);
	NhlFreeGenArray(cnp->line_thicknesses);
	NhlFreeGenArray(cnp->llabel_strings);
	NhlFreeGenArray(cnp->llabel_colors);
        if (cnp->gks_llabel_colors)
                NhlFree(cnp->gks_llabel_colors);
        if (cnp->gks_line_colors)
                NhlFree(cnp->gks_line_colors);
        if (cnp->gks_fill_colors)
                NhlFree(cnp->gks_fill_colors);

	if (cnp->osfp != NULL)
		NhlFree(cnp->osfp);
/*
 * This array will be NULL unless the user has explicitly set
 * LegendLevelFlags.
 */
	if (cnp->lgnd_level_flags != NULL)
		NhlFreeGenArray(cnp->lgnd_level_flags);
/*
 * Note: this array has its own string pointer array but the strings
 * themselves belong to the llabel_strings array. The my_data flag is
 * specially set to False, so FreeGenArray does not try to free the data.
 */
	if (cnp->ll_strings != NULL) {
		NhlFree(cnp->ll_strings->data);
		NhlFreeGenArray(cnp->ll_strings);
	}
/*
 * These arrays may or may not have their own data. The my_data flag should
 * be set appropriately. They will be NULL only if a Legend was never 
 * created.
 */
	if (cnp->lgnd_l_colors != NULL)
		NhlFreeGenArray(cnp->lgnd_l_colors);
	if (cnp->lgnd_l_dash_pats != NULL)
		NhlFreeGenArray(cnp->lgnd_l_dash_pats);
	if (cnp->lgnd_l_thicknesses != NULL)
		NhlFreeGenArray(cnp->lgnd_l_thicknesses);
	if (cnp->lgnd_ll_font_colors != NULL)
		NhlFreeGenArray(cnp->lgnd_ll_font_colors);
	if (cnp->lgnd_ll_strings != NULL)
		NhlFreeGenArray(cnp->lgnd_ll_strings);
	if (cnp->lgnd_labels != NULL)
		NhlFreeGenArray(cnp->lgnd_labels);
/*
 * This array will be NULL only if a labelbar was never created.
 */
	if (cnp->lbar_labels != NULL)
		NhlFreeGenArray(cnp->lbar_labels);
	
	_NhlFreeWorkspace(cnp->fws_id);
	_NhlFreeWorkspace(cnp->iws_id);
	if (cnp->cws_id > 0)
		_NhlFreeWorkspace(cnp->cws_id);
	if (cnp->aws_id > 0)
		_NhlFreeWorkspace(cnp->aws_id);

        if (cnp->max_data_format.fstring != NULL)
                NhlFree(cnp->max_data_format.fstring);
        if (cnp->info_string != NULL)
                NhlFree(cnp->info_string);
        if (cnp->no_data_string != NULL)
                NhlFree(cnp->no_data_string);
        if (cnp->constf_string != NULL)
                NhlFree(cnp->constf_string);

	if (cnp->line_lbls.format.fstring != NULL)
                NhlFree(cnp->line_lbls.format.fstring);
	if (cnp->high_lbls.format.fstring != NULL)
                NhlFree(cnp->high_lbls.format.fstring);
	if (cnp->low_lbls.format.fstring != NULL)
                NhlFree(cnp->low_lbls.format.fstring);
	if (cnp->info_lbl.format.fstring != NULL)
                NhlFree(cnp->info_lbl.format.fstring);
	if (cnp->constf_lbl.format.fstring != NULL)
                NhlFree(cnp->constf_lbl.format.fstring);

	if (cnp->high_lbls.text != NULL)
                NhlFree(cnp->high_lbls.text);
	if (cnp->low_lbls.text != NULL)
                NhlFree(cnp->low_lbls.text);
	if (cnp->info_lbl.text != NULL)
                NhlFree(cnp->info_lbl.text);
	if (cnp->constf_lbl.text != NULL)
                NhlFree(cnp->constf_lbl.text);

	if (cnp->predraw_dat != NULL)
		_NhlDeleteViewSegment(inst,cnp->predraw_dat);
	if (cnp->draw_dat != NULL)
		_NhlDeleteViewSegment(inst,cnp->draw_dat);
	if (cnp->postdraw_dat != NULL)
		_NhlDeleteViewSegment(inst,cnp->postdraw_dat);

	return(ret);
}


/*
 * Function:    ContourPlotGetBB
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
static NhlErrorTypes ContourPlotGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*entry_name  = "ContourPlotGetBB";
	char			*e_text;
	NhlContourPlotLayer		cnl = (NhlContourPlotLayer) instance;
	NhlContourPlotLayerPart	*cnp = &(cnl->contourplot);
	NhlTransformLayerPart	*cntp = &(((NhlTransformLayer)cnl)->trans);
	NhlViewLayerPart	*cnvp = &(((NhlViewLayer) cnl)->view);
	float			x,y,width,height;

	if (! _NhlIsTransform(instance)) {
		e_text = "%s: invalid object id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
/*
 * If the ContourPlot object is a overlay base, return the bounding box
 * of the complete overlay. If it is a member of an overlay, return
 * only the ContourPlot's viewport, since it does not 'own' any of its
 * annotations. If it is not in an overlay at all, return its viewport
 * plus the info label and constant field annotation viewports 
 * (instantiated directly by the ContourPlot) as appropriate.
 */
	if (cntp->overlay_status == _tfCurrentOverlayBase) {
		return _NhlGetBB(cntp->overlay_object,thebox);
	}

	_NhlAddBBInfo(cnvp->y,cnvp->y - cnvp->height,
		      cnvp->x + cnvp->width,cnvp->x,thebox);

	if (cntp->overlay_status == _tfCurrentOverlayMember)
		return ret;

	if (cnp->info_anno_id != NhlNULLOBJID && cnp->info_lbl_rec.on) {
		subret = NhlVAGetValues(cnp->info_anno_id,
					NhlNvpXF,&x,
					NhlNvpYF,&y,
					NhlNvpWidthF,&width,
					NhlNvpHeightF, &height, NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

		_NhlAddBBInfo(y,y-height,x+width,x,thebox);
	}
	if (cnp->constf_anno_id != NhlNULLOBJID && cnp->constf_lbl_rec.on) {
		subret = NhlVAGetValues(cnp->constf_anno_id,
					NhlNvpXF,&x,
					NhlNvpYF,&y,
					NhlNvpWidthF,&width,
					NhlNvpHeightF, &height, NULL);

		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		_NhlAddBBInfo(y,y-height,x+width,x,thebox);
	}

	return ret;
}

/*
 * Function:	cnInitDraw
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes cnInitDraw
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl,
	NhlString	entry_name
)
#else
(cnl,entry_name)
        NhlContourPlotLayer cnl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnl->contourplot);
	NhlTransformLayerPart	*tfp = &(cnl->trans);
	int m,n;

	cnp->aws = NULL;
	cnp->fws = NULL;
	cnp->iws = NULL;
	cnp->cws = NULL;
	cnp->wk_active = False;
	cnp->current_trans_dat = NULL;
	
	subret = GetData(cnl,&cnp->data,&n,&m);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

/*
 * Set up LLU interface coordinate boundaries 
 */
        cnp->xlb = MAX(tfp->x_min,MIN(tfp->data_xstart,tfp->data_xend));
        cnp->xub = MIN(tfp->x_max,MAX(tfp->data_xstart,tfp->data_xend));
        cnp->ylb = MAX(tfp->y_min,MIN(tfp->data_ystart,tfp->data_yend));
        cnp->yub = MIN(tfp->y_max,MAX(tfp->data_ystart,tfp->data_yend));
        
	if (!cnp->use_irr_trans) {
                cnp->xc1 = tfp->data_xstart;
                cnp->xcm = tfp->data_xend;
                cnp->yc1 = tfp->data_ystart;
                cnp->ycn = tfp->data_yend;
        }
        else {
                int xcount,ycount;
                
                xcount = tfp->x_axis_type == NhlIRREGULARAXIS ?
                        cnp->sfp->x_arr->len_dimensions[0] : 3;
                ycount = tfp->y_axis_type == NhlIRREGULARAXIS ?
                        cnp->sfp->y_arr->len_dimensions[0] : 3;
                
                cnp->xc1 = 0;
                cnp->xcm = xcount - 1;
                cnp->yc1 = 0;
                cnp->ycn = ycount - 1;
        }
        
	NhlVASetValues(cnl->base.wkptr->base.id,
		       _NhlNwkReset,	True,
		       NULL);
	
	return ret;
}


/*
 * Function:	cnInitAreamap
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes cnInitAreamap
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl,
	NhlString	entry_name
)
#else
(cnl,entry_name)
        NhlContourPlotLayer cnl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnl->contourplot);

	if (cnp->aws_id < 1) {
		cnp->aws_id = 
			_NhlNewWorkspace(NhlwsAREAMAP,
					 NhlwsNONE,200000*sizeof(int));
		if (cnp->aws_id < 1) 
			return MIN(ret,(NhlErrorTypes)cnp->aws_id);
	}
	if ((cnp->aws = _NhlUseWorkspace(cnp->aws_id)) == NULL) {
		e_text = 
			"%s: error reserving label area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

#if 0
	c_arseti("lc",(int) (cnp->amap_crange * 
		 MIN(cnl->view.width,cnl->view.height)));
#endif
	subret = _NhlArinam(cnp->aws,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = AddDataBoundToAreamap(cnl,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = _NhlCpclam(cnp->data,cnp->fws,cnp->iws,cnp->aws,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	if (cnp->dump_area_map)
	    _NhlDumpAreaMap(cnp->aws,entry_name);


	return ret;
}

/*
 * Function:	GetDataBound
 *
 * Description:	Returns the bounding box of contour data in NDC
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: 
 *		 
 */	

static NhlErrorTypes GetDataBound
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlBoundingBox		*bbox,
        NhlBoolean		*xlinear,
        NhlBoolean		*ylinear,
	NhlString		entry_name
)
#else
(cl,bbox,linear,entry_name)
	NhlContourPlotLayer	cl;
	NhlBoundingBox		*bbox;
        NhlBoolean		*linear;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = 
		(NhlContourPlotLayerPart *) &cl->contourplot;
	int			status;
	NhlBoolean		ezmap = False,x_irr = False,y_irr = False;
	NhlTransformLayerPart	*tfp = &(cl->trans);

#define EPSILON 1e-2
        
        *xlinear = False;
        *ylinear = False;
        if (cnp->use_irr_trans) {
                int i;
                float *coords,start_diff,diff,eps,sum;
                if (cnp->sfp->x_arr) {
                        sum = 0;
                        coords = (float *)cnp->sfp->x_arr->data;
                        for (i = 1; i< cnp->sfp->x_arr->num_elements; i++)
                                sum += fabs(coords[i] - coords[i-1]);
                        start_diff = sum / (cnp->sfp->x_arr->num_elements-1);
                        eps = start_diff * EPSILON;
                        for (i = 1; i < cnp->sfp->x_arr->num_elements; i++) {
                                diff = fabs(coords[i] - coords[i-1]);
                                if (diff < start_diff - eps ||
                                    diff > start_diff + eps) {
                                        x_irr = True;
                                        break;
                                }
                        }
                }
                if (cnp->sfp->y_arr) {
                        sum = 0;
                        coords = (float *)cnp->sfp->y_arr->data;
                        for (i = 1; i< cnp->sfp->y_arr->num_elements; i++)
                                sum += fabs(coords[i] - coords[i-1]);
                        start_diff = sum / (cnp->sfp->y_arr->num_elements-1);
                        eps = start_diff * EPSILON;
                        for (i = 1; i < cnp->sfp->y_arr->num_elements; i++) {
                                diff = fabs(coords[i] - coords[i-1]);
                                if (diff < start_diff - eps ||
                                    diff > start_diff + eps) {
                                        y_irr = True;
                                        break;
                                }
                        }
                }
        }
#undef EPSILON
                
	if (cnp->trans_obj->base.layer_class->base_class.class_name ==
	    NhlmapTransObjClass->base_class.class_name) {
		ezmap = True;
	}
	else if (cnp->trans_obj->base.layer_class->base_class.class_name ==
                 NhllogLinTransObjClass->base_class.class_name ||
                 ! _NhlIsOverlay(cl->base.id)) {
                if (! cl->trans.x_log && ! x_irr)
                        *xlinear = True;
                if (! cl->trans.y_log && ! y_irr)
                        *ylinear = True;
	}

	if (! ezmap) {
		float tx[2],ty[2];
		float fx[2],fy[2];
		float vxd[2],vyd[2];
		float cxd[2],cyd[2];
                
		ret = NhlVAGetValues(cnp->trans_obj->base.id,
				     NhlNtrXMinF,&vxd[0],
				     NhlNtrXMaxF,&vxd[1],
				     NhlNtrYMinF,&vyd[0],
				     NhlNtrYMaxF,&vyd[1],
				     NULL);

		cxd[0] = MAX(vxd[0],cnp->xlb);
		cxd[1] = MIN(vxd[1],cnp->xub);
		cyd[0] = MAX(vyd[0],cnp->ylb);
		cyd[1] = MIN(vyd[1],cnp->yub);

		_NhlDataToWin(cnp->trans_obj,cxd,cyd,
			      2,tx,ty,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}
		_NhlWinToNDC(cnp->trans_obj,tx,ty,
			      2,fx,fy,&status,NULL,NULL);

		bbox->l = MIN(fx[0],fx[1]);
		bbox->r = MAX(fx[0],fx[1]);
		bbox->b = MIN(fy[0],fy[1]);
		bbox->t = MAX(fy[0],fy[1]);
	}
	else {
		NhlProjection projection;
		NhlMapLimitMode limit_mode;
		float center_lat, rotation;
		float min_lat,max_lat;
		NhlBoolean rel_center_lat;
		NhlVAGetValues(cnp->trans_obj->base.id,
			       NhlNmpLeftNDCF,&bbox->l,
			       NhlNmpRightNDCF,&bbox->r,
			       NhlNmpBottomNDCF,&bbox->b,
			       NhlNmpTopNDCF,&bbox->t,
			       NhlNmpProjection,&projection,
			       NhlNmpLimitMode,&limit_mode,
			       NhlNmpMinLatF,&min_lat,
			       NhlNmpMaxLatF,&max_lat,
			       NhlNmpRelativeCenterLat,&rel_center_lat,
			       NhlNmpCenterLatF,&center_lat,
			       NhlNmpCenterRotF,&rotation,
			       NULL);
		if (projection == NhlCYLINDRICALEQUIDISTANT) {
                        if (limit_mode != NhlLATLON || ! rel_center_lat) {
		    		if (center_lat == 0.0 && rotation == 0.0) {
					*xlinear = (cl->trans.x_log || x_irr) ?
                                                False : True;
					*ylinear = (cl->trans.y_log || y_irr) ?
                                                False : True;
                                }
                        }
                        else if (rotation == 0.0 && _NhlCmpFAny
                                 (max_lat-min_lat-center_lat,0.0,6) == 0.0) {
                                *xlinear = (cl->trans.x_log || x_irr) ?
                                        False : True;
                                *ylinear = (cl->trans.y_log || y_irr) ?
                                        False : True;
                        }
                }
	}
        bbox->l = MAX(0.0,bbox->l);
        bbox->r = MIN(1.0,bbox->r);
        bbox->t = MIN(1.0,bbox->t);
        bbox->b = MAX(0.0,bbox->b);

	return NhlNOERROR;
}

/*
 * Function:	cnInitCellArray
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes cnInitCellArray
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl,
	int		*msize,
	int		*nsize,
	NhlBoundingBox	*bbox,
	NhlString	entry_name
)
#else
(cnl,entry_name)
        NhlContourPlotLayer cnl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnl->contourplot);
        int dunits,dwidth,dheight;
        int max_msize, max_nsize;
        NhlBoolean xlinear,ylinear;

	c_cpseti("CAF", -1);
	subret = GetDataBound(cnl,bbox,&xlinear,&ylinear,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
        
        max_msize = (int) ((bbox->r - bbox->l) / cnp->min_cell_size);
        max_nsize = (int) ((bbox->t - bbox->b) / cnp->min_cell_size);

        subret = NhlVAGetValues(cnl->base.wkptr->base.id,
                                NhlNwkVSWidthDevUnits,&dunits,
                                NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

        dwidth = dunits * (bbox->r - bbox->l);
        dheight = dunits * (bbox->t - bbox->b);

        if (cnp->sticky_cell_size_set) {
                if ((bbox->r - bbox->l) / cnp->cell_size <= 1.0 ||
                    (bbox->t - bbox->b) / cnp->cell_size <= 1.0) {
                        e_text = 
                                "%s: invalid value for %s: defaulting";
                        NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
                                  entry_name,NhlNcnRasterCellSizeF);
                        ret = NhlWARNING;
                        cnp->sticky_cell_size_set = False;
                }
        }
                
	if (cnp->sticky_cell_size_set) {
		*msize = (int) ((bbox->r - bbox->l) / cnp->cell_size + 0.5);
		*nsize = (int) ((bbox->t - bbox->b) / cnp->cell_size + 0.5);
	}
        else if (cnp->raster_sample_factor <= 0.0) {
                *msize = cnp->sfp->fast_len;
                *nsize = cnp->sfp->slow_len;
        }
        else if (cnp->raster_smoothing_on) {
                *msize = dwidth * cnp->raster_sample_factor;
                *nsize = dheight * cnp->raster_sample_factor;
        }
        else {
                if (! xlinear)
                        *msize = dwidth * cnp->raster_sample_factor;
                else
                        *msize = MIN(dwidth,cnp->sfp->fast_len) 
                                * cnp->raster_sample_factor;
                if (! ylinear)
                        *nsize = dheight * cnp->raster_sample_factor;
                else
                        *nsize = MIN(dheight,cnp->sfp->slow_len)
                                * cnp->raster_sample_factor;
        }
        
        if (!cnp->sticky_cell_size_set && cnp->raster_sample_factor > 0.0) {
                *msize = MIN(*msize,max_msize);
                *nsize = MIN(*nsize,max_nsize);
                cnp->cell_size = (bbox->r - bbox->l) / (float) *msize;
        }
	
	if (cnp->cws_id < 1) {
		cnp->cws_id = 
			_NhlNewWorkspace(NhlwsOTHER,NhlwsNONE,
					 (*msize * *nsize) * sizeof(int));
		if (cnp->cws_id < 1) 
			return MIN(ret,(NhlErrorTypes)cnp->cws_id);
	}
	if ((cnp->cws = _NhlUseWorkspace(cnp->cws_id)) == NULL) {
		e_text = 
			"%s: error reserving cell array workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	return ret;
}

/*
 * Function:	cnUpdateTrans
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes cnUpdateTrans
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl,
        NhlBoolean		seg_draw,
	NhlString		entry_name
)
#else
(cnl,seg_draw,entry_name)
        NhlContourPlotLayer cnl;
        NhlBoolean		seg_draw;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnl->contourplot);
	NhlTransformLayerPart	*tfp = &(cnl->trans);

/*
 * If the plot is an overlay member, use the PlotManager's trans object.
 * Note that the "do_low_level_log" flag is true only if the trans object
 * is an IrregularTransObj.
 */
        cnp->low_level_log_on = False;
	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    ! tfp->do_ndc_overlay && tfp->overlay_trans_obj != NULL) {
		cnp->trans_obj = tfp->overlay_trans_obj;

		/* 
		 * if over a map the data xstart/xend values are used to
		 * determine the longitude range (max 360 from -540-+540)
		 * the data ystart/end values are not used should not be
		 * set. Otherwise it causes unnecessary redraws
		 */
		if ((cnp->trans_obj->base.layer_class)->base_class.class_name 
		    == NhlmapTransObjClass->base_class.class_name) {
			subret = NhlVASetValues
                                (cnp->trans_obj->base.id,
                                 NhlNtrDataXStartF,tfp->data_xstart,
                                 NhlNtrDataXEndF,tfp->data_xend,
                                 NULL);

			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return(ret);
			}
		}

		else if (cnp->do_low_level_log) {
			if (tfp->x_axis_type == NhlLOGAXIS) {
				subret = NhlVASetValues
                                        (tfp->trans_obj->base.id,
                                         NhlNtrXAxisType,NhlLINEARAXIS,
                                         NULL);
			}
			else {
				subret = NhlVASetValues
                                        (tfp->trans_obj->base.id,
                                         NhlNtrYAxisType,NhlLINEARAXIS,
                                         NULL);
			}
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return(ret);
			}
		}
	}
	else {
		cnp->trans_obj = tfp->trans_obj;

		if (cnp->do_low_level_log && ! seg_draw) {
			subret = NhlVASetValues(tfp->trans_obj->base.id,
						NhlNtrLowLevelLogOn,True,
						NULL);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return(ret);
			}
                        cnp->low_level_log_on = True;
		}
		if (cnp->do_low_level_log ||
                    tfp->overlay_status == _tfNotInOverlay ||
		    tfp->do_ndc_overlay) {
			subret = _NhlSetTrans(tfp->trans_obj, (NhlLayer)cnl);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = "%s: error setting transformation";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text, entry_name);
				return(ret);
			}
		}
	}

	return ret;
}


/*
 * Function:	ContourAbortDraw
 *
 * Description:	cleans up if a fatal error occurs while drawing
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static void ContourAbortDraw
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl
)
#else
(cnl)
	NhlContourPlotLayer	cnl;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	NhlTransformLayerPart	*tfp = &(cnl->trans);
	char *e_text;

	Cnp = NULL;
	Cnl = NULL;

	if (cnp->aws != NULL) {
		_NhlIdleWorkspace(cnp->aws);
		cnp->aws = NULL;
	}
	if (cnp->cws != NULL) {
		_NhlIdleWorkspace(cnp->cws);
		cnp->cws = NULL;
	}
	if (cnp->fws != NULL) {
		_NhlIdleWorkspace(cnp->fws);
		cnp->fws = NULL;
	}
	if (cnp->iws != NULL) {
		_NhlIdleWorkspace(cnp->iws);
		cnp->iws = NULL;
	}

	if (cnl->view.use_segments && cnp->current_trans_dat) {
		_NhlEndSegment(cnp->current_trans_dat);
		cnp->current_trans_dat = NULL;
	}

	if (cnp->wk_active) {
		_NhlDeactivateWorkstation(cnl->base.wkptr);
		cnp->wk_active = False;
	}

	if (cnp->low_level_log_on) {
		NhlVASetValues(tfp->trans_obj->base.id,
			       NhlNtrLowLevelLogOn,False,NULL);
                cnp->low_level_log_on = False;
        }

	e_text = "%s: draw error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,"ContourPlotDraw");
}


/*
 * Function:	ContourPlotPreDraw
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes ContourPlotPreDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString		e_text,entry_name = "ContourPlotPreDraw";
	NhlContourPlotLayer	cnl = (NhlContourPlotLayer) layer;
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
        NhlBoolean		seg_draw;

	if (! cnp->data_init || cnp->const_field)
		return NhlNOERROR;
	
	Cnp = cnp;
	Cnl = cnl;

	subret = cnInitDraw(cnl,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		Cnp = NULL;
		return ret;
	}

	if (cnp->label_order != NhlPREDRAW &&
	    cnp->line_order != NhlPREDRAW &&
	    cnp->fill_order != NhlPREDRAW) {
		Cnp = NULL;
		return NhlNOERROR;
	}
        
        seg_draw = cnl->view.use_segments && ! cnp->new_draw_req &&
		cnp->predraw_dat && cnp->predraw_dat->id != NgNOT_A_SEGMENT;
        
	subret = cnUpdateTrans(cnl,seg_draw,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		return ret;
	}
	if (seg_draw) {
		subret = _NhltfDrawSegment((NhlLayer)cnl,cnp->trans_obj,
					cnp->predraw_dat,entry_name);
	}
        else {
                subret = cnDraw(cnl,NhlPREDRAW,entry_name);
        }
	
	Cnp = NULL;
	return MIN(subret,ret);
}

/*
 * Function:	ContourPlotDraw
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes ContourPlotDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlContourPlotLayer	cnl = (NhlContourPlotLayer) layer;
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	NhlString	e_text,entry_name = "ContourPlotDraw";
        NhlBoolean		seg_draw;

	if (! cnp->data_init || cnp->const_field) {
		Cnp = NULL;
		return NhlNOERROR;
	}
	if (cnp->label_order != NhlDRAW &&
	    cnp->line_order != NhlDRAW &&
	    cnp->fill_order != NhlDRAW)
		return NhlNOERROR;

	Cnp = cnp;
	Cnl = cnl;
        
        seg_draw = cnl->view.use_segments && ! cnp->new_draw_req &&
		cnp->draw_dat && cnp->draw_dat->id != NgNOT_A_SEGMENT;
        
	subret = cnUpdateTrans(cnl,seg_draw,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		return ret;
	}

	if (seg_draw) {
		subret = _NhltfDrawSegment((NhlLayer)cnl,cnp->trans_obj,
					cnp->draw_dat,entry_name);
	}
        else {
                subret = cnDraw((NhlContourPlotLayer)layer,NhlDRAW,entry_name);
        }
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		return ret;
	}

	Cnp = NULL;
	return MIN(subret,ret);
}

/*
 * Function:	ContourPlotPostDraw
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes ContourPlotPostDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlContourPlotLayer		cnl = (NhlContourPlotLayer) layer;
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	NhlTransformLayerPart	*tfp = &cnl->trans;
	NhlString		e_text,entry_name = "ContourPostPlotDraw";
        NhlBoolean		seg_draw;

	Cnp = cnp;
	Cnl = cnl;

	if (! cnp->data_init || cnp->const_field) {
		if (cnp->display_constf_no_data &&
		    tfp->overlay_status == _tfNotInOverlay) {
			subret = NhlDraw(cnp->constf_lbl_rec.id);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				Cnp = NULL;
				return ret;
			}
		}
		Cnp = NULL;
		return ret;
	}

        seg_draw = cnl->view.use_segments && ! cnp->new_draw_req &&
		cnp->postdraw_dat && cnp->postdraw_dat->id != NgNOT_A_SEGMENT;
        
	if (cnp->label_order == NhlPOSTDRAW ||
	    cnp->line_order == NhlPOSTDRAW ||
	    cnp->fill_order == NhlPOSTDRAW) {
                subret = cnUpdateTrans(cnl,seg_draw,entry_name);
                if ((ret = MIN(subret,ret)) < NhlWARNING) {
                        ContourAbortDraw(cnl);
                        return ret;
                }
                if (seg_draw) {
                        subret = _NhltfDrawSegment
                                ((NhlLayer)cnl,cnp->trans_obj,
                                 cnp->postdraw_dat,entry_name);
                }
                else {
                        subret = cnDraw((NhlContourPlotLayer) layer,
                                        NhlPOSTDRAW,entry_name);
                }
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			ContourAbortDraw(cnl);
			return(ret);
		}
        }

	cnp->new_draw_req = False;
	Cnp = NULL;

	if (tfp->overlay_status == _tfNotInOverlay) {
		if (cnp->info_lbl.on) {
			subret = NhlDraw(cnp->info_lbl_rec.id);
			ret = MIN(subret,ret);
		}
	}
	if (cnp->aws != NULL) {
		subret = _NhlIdleWorkspace(cnp->aws);
		ret = MIN(subret,ret);
		cnp->aws = NULL;
	}

	return ret;
}


/*
 * Function:	SetCpParams
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes SetCpParams
#if	NhlNeedProto
(NhlContourPlotLayer cnl,NhlString entry_name)
#else
(cnl,entry_name)
        NhlContourPlotLayer cnl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	NhlString		*sp;
	int			i,j;
	char			param[4];
	float			value;

	if (cnp->conpack_params == NULL)
		return NhlNOERROR;

	sp = (NhlString *) cnp->conpack_params->data;

	for (i = 0; i < cnp->conpack_params->num_elements; i++) {
		NhlBoolean matched = False;
		_cnParamType type;
		if (sp[i] != NULL && sp[i][0] != '\0') {
			value = 0.0;
			sscanf(sp[i],"%3s:%f",&param[0],&value);
			for (j = 0; j < NhlNumber(Cp_Params); j ++) {
				if (! strcmp(Cp_Params[j].name,param)) {
					matched = True;
					type = Cp_Params[j].type;
					break;
				}
			}
			if (matched && type == cnInt) {
				c_cpseti(param,(int) value);
			}
			else if (matched && type == cnFloat) {
				c_cpsetr(param,value);
			}
			else {
				char * e_text = 
              "%s: %s is invalid Conpack param or cannot be from HLU library";
				NhlPError(NhlWARNING,
					  NhlEUNKNOWN,e_text,entry_name,param);
				ret = MIN(ret,NhlWARNING);
			}
		}
	}
	return ret;
}

/*
 * Function:	cnDraw
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes cnDraw
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl,
	NhlDrawOrder		order,
	NhlString		entry_name
)
#else
(cnl,order,entry_name)
        NhlContourPlotLayer cnl;
	NhlDrawOrder	order;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnl->contourplot);
	NhlTransformLayerPart	*tfp = &(cnl->trans);

        subret = _NhlActivateWorkstation(cnl->base.wkptr);
        if ((ret = MIN(subret,ret)) < NhlWARNING) {
                e_text = "%s: Error activating workstation";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
                ContourAbortDraw(cnl);
                return NhlFATAL;
        }
        cnp->wk_active = True;

        if (cnl->view.use_segments) {
                NhlTransDat **trans_dat_pp;
                switch (order) {
                    case NhlPREDRAW:
                            trans_dat_pp = &cnp->predraw_dat;
                            break;
                    case NhlDRAW:
                            trans_dat_pp = &cnp->draw_dat;
                            break;
                    case NhlPOSTDRAW:
                            trans_dat_pp = &cnp->postdraw_dat;
                            break;
                }
                subret = _NhltfInitSegment((NhlLayer)cnl,
					    cnp->trans_obj,
					    trans_dat_pp,entry_name);
                if ((ret = MIN(subret,ret)) < NhlWARNING) {
                        ContourAbortDraw(cnl);
                        return ret;
                }
                cnp->current_trans_dat = *trans_dat_pp;
        }
	
	c_cprset();

	SetCpParams(cnl,entry_name);
/*
 * Only set the ORV parameter if overlaying on EZMAP. It can cause
 * problems otherwise. (Not sure yet whether it is needed in some cases
 * though, and perhaps not needed in certain Ezmap cases.
 */
	if (cnp->trans_obj->base.layer_class->base_class.class_name ==
	    NhlmapTransObjClass->base_class.class_name) {
		NhlVAGetValues(cnp->trans_obj->base.id, 
			       NhlNtrOutOfRangeF, &cnp->out_of_range_val,
			       NULL);
		c_cpsetr("ORV",cnp->out_of_range_val);
	}

	if (cnp->sfp->missing_value_set)
		c_cpsetr("SPV",cnp->sfp->missing_value);
	else
		c_cpsetr("SPV",(float)0.0);

	if (cnp->low_level_log_on && cnl->trans.x_log) {
		c_cpsetr("XC1",(float)tfp->data_xstart);
		c_cpsetr("XCM",(float)tfp->data_xend);
	}
	else {
		c_cpsetr("XC1",(float)cnp->xc1);
		c_cpsetr("XCM",(float)cnp->xcm);
	}
	if (cnp->low_level_log_on && cnl->trans.y_log) {
		c_cpsetr("YC1",(float)tfp->data_ystart);
		c_cpsetr("YCN",(float)tfp->data_yend);
	}
	else {
		c_cpsetr("YC1",(float)cnp->yc1);
		c_cpsetr("YCN",(float)cnp->ycn);
	}
	c_cpseti("WSO", 3);		/* error recovery on */
	c_cpseti("NVS",0);		/* no vertical strips */
        c_cpseti("SET",0);
        c_cpseti("MAP",NhlcnMAPVAL);

	c_cpsetr("PIT",MAX(0.0,cnp->max_point_distance));
	
        if (cnp->smoothing_on) {
                c_cpsetr("T2D",cnp->smoothing_tension);
                c_cpsetr("SSL",cnp->smoothing_distance);
        }
        else {
                c_cpsetr("T2D",(float)0.0);
        }
	gset_fill_colr_ind((Gint)_NhlGetGksCi(cnl->base.wkptr,0));

	subret = UpdateLineAndLabelParams(cnl,&cnp->do_lines,&cnp->do_labels);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		return ret;
	}

	subret = UpdateFillInfo(cnl, &cnp->do_fill);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		return ret;
	}


/* Retrieve workspace pointers */

	if ((cnp->fws = _NhlUseWorkspace(cnp->fws_id)) == NULL) {
		e_text = "%s: error reserving float workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		ContourAbortDraw(cnl);
		return(ret);
	}
	if ((cnp->iws = _NhlUseWorkspace(cnp->iws_id)) == NULL) {
		e_text = "%s: error reserving integer workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		ContourAbortDraw(cnl);
		return(ret);
	}
	/* Draw the contours */

	subret = _NhlCprect(cnp->data,cnp->sfp->fast_dim,cnp->sfp->fast_len,
			    cnp->sfp->slow_len,cnp->fws,cnp->iws,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		return ret;
	}
#if 0
	{ /* for debugging */
		float flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;
		c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
		printf("getset - %f,%f,%f,%f,%f,%f,%f,%f\n",
		       flx,frx,fby,fuy,wlx,wrx,wby,wuy); 
  	}
#endif

	if (cnp->do_labels && 
	    cnp->label_masking && cnp->label_order == order) {

		c_cpseti("GIL",5);
		if (cnp->aws == NULL) {
			subret = cnInitAreamap(cnl,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				return ret;
			}
		}
		if (! cnp->aws) {
			e_text = "%s: Error reserving workspace";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			ContourAbortDraw(cnl);
			return NhlFATAL;
		}

		subret = _NhlCplbam(cnp->data,
				    cnp->fws,cnp->iws,cnp->aws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			ContourAbortDraw(cnl);
			return ret;
		}
	}

	if (cnp->do_fill && cnp->fill_order == order) {

		if (! cnp->raster_mode_on) {
			if (cnp->aws == NULL) {
				subret = cnInitAreamap(cnl,entry_name);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					ContourAbortDraw(cnl);
					return ret;
				}
			}
			if (! cnp->aws) {
				e_text = "%s: Error reserving workspace";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				ContourAbortDraw(cnl);
				return NhlFATAL;
			}
			subret = _NhlArscam(cnp->aws,
					    (_NHLCALLF(hlucpfill,HLUCPFILL)),
					    entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				return ret;
			}
		}
		else {
			int msize,nsize;
			NhlBoundingBox bbox;
			subret = cnInitCellArray(cnl,&msize,
						 &nsize,&bbox,entry_name);
 			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				return ret;
			}
			subret = _NhlCpcica(cnp->data,
					    cnp->fws,cnp->iws,cnp->cws,
					    msize,msize,nsize,
					    bbox.l,bbox.b,bbox.r,bbox.t,
					    cnp->raster_smoothing_on,
					    entry_name);
 			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				return ret;
			}
			if (cnp->cws != NULL) {
				subret = _NhlIdleWorkspace(cnp->cws);
				ret = MIN(subret,ret);
				cnp->cws = NULL;
			}
		}
	}

	if (cnp->line_order == order &&
	    (cnp->do_lines || cnp->missing_val.perim_on ||
	     cnp->grid_bound.perim_on || cnp->out_of_range.perim_on)) {
		if (cnp->do_labels && cnp->label_masking) {
			subret = _NhlCpcldm(cnp->data,
					    cnp->fws,cnp->iws,cnp->aws,
					    (_NHLCALLF(cpdrpl,CPDRPL)),
					    entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				return ret;
			}
		}
		else {
			subret = _NhlCpcldr(cnp->data,
					    cnp->fws,cnp->iws,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				return ret;
			}
		}
	}
	
	if (cnp->do_labels && cnp->label_order == order) {	
		gset_fill_int_style(GSTYLE_SOLID);

		subret = _NhlCplbdr(cnp->data,cnp->fws,cnp->iws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			ContourAbortDraw(cnl);
			return ret;
		}
	}

	if (cnl->view.use_segments && cnp->current_trans_dat) {
		_NhlEndSegment(cnp->current_trans_dat);
	}
	cnp->current_trans_dat = NULL;
        
	if (cnp->low_level_log_on) {
		subret = NhlVASetValues(cnp->trans_obj->base.id,
					NhlNtrLowLevelLogOn,False,NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			ContourAbortDraw(cnl);
			return(ret);
		}
                cnp->low_level_log_on = False;
	}
        
        subret = _NhlDeactivateWorkstation(cnl->base.wkptr);
	cnp->wk_active = False;
	ret = MIN(subret,ret);

	if (cnp->fws != NULL) {
		subret = _NhlIdleWorkspace(cnp->fws);
		ret = MIN(subret,ret);
		cnp->fws = NULL;
	}
	if (cnp->iws != NULL) {
		subret = _NhlIdleWorkspace(cnp->iws);
		cnp->iws = NULL;
		ret = MIN(subret,ret);
	}
	return MIN(subret,ret);
}
/*
 * Function:	AddDataBoundToAreamap
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: 
 *		 
 */	

static NhlErrorTypes AddDataBoundToAreamap
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlString	entry_name
)
#else
(cl,entry_name)
	NhlContourPlotLayer	cl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = 
		(NhlContourPlotLayerPart *) &cl->contourplot;
	int			i;
	int			status;
	NhlBoolean		ezmap = False;
	int			xrev,yrev;
	float			xa[5],ya[5];

#if 0
#define _cnMAPBOUNDINC	3700
#endif
#define _cnMAPBOUNDINC	100

	if (cnp->trans_obj->base.layer_class->base_class.class_name ==
	    NhlmapTransObjClass->base_class.class_name) {
		ezmap = True;
	}

#if 0
	gset_linewidth(4.0);
	gset_line_colr_ind(30);
	c_arseti("RC(1)",1);
	c_arseti("RC(3)",2);
#endif
	c_arseti("RC",1);
	if (! ezmap) {
		float twlx,twrx,twby,twuy;
		float gwlx,gwrx,gwby,gwuy;
		float txmin,txmax,tymin,tymax;
		float gxmin,gxmax,gymin,gymax;
		NhlBoolean lbox, rbox, bbox, tbox;

		ret = NhlVAGetValues(cnp->trans_obj->base.id,
				     NhlNtrXMinF,&txmin,
				     NhlNtrXMaxF,&txmax,
				     NhlNtrYMinF,&tymin,
				     NhlNtrYMaxF,&tymax,
				     NULL);

		_NhlDataToWin(cnp->trans_obj,&txmin,&tymin,
			      1,&twlx,&twby,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		_NhlDataToWin(cnp->trans_obj,&txmax,&tymax,
			      1,&twrx,&twuy,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		gxmin = MAX(txmin,cnp->xlb);
		gxmax = MIN(txmax,cnp->xub);
		gymin = MAX(tymin,cnp->ylb);
		gymax = MIN(tymax,cnp->yub);

		_NhlDataToWin(cnp->trans_obj,&gxmin,&gymin,
			      1,&gwlx,&gwby,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		_NhlDataToWin(cnp->trans_obj,&gxmax,&gymax,
			      1,&gwrx,&gwuy,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		xrev = twlx > twrx;
		yrev = twby > twuy;

		if (! xrev) {
			lbox = gwlx > twlx;
			rbox = gwrx < twrx;
		}
		else {
			lbox = gwlx < twlx;
			rbox = gwrx > twrx;
		}
		if (! yrev) {
			bbox = gwby > twby;
			tbox = gwuy < twuy;
		}
		else {
			bbox = gwby > twby;
			tbox = gwuy < twuy;
		}
		if (lbox) {
			xa[0] = xa[1] = xa[4] = twlx;
			xa[2] = xa[3] = gwlx;
			ya[0] = ya[3] = ya[4] = twuy;
			ya[1] = ya[2] = twby;

			if (! (xrev || yrev) || (xrev && yrev)) 
				_NhlAredam(cnp->aws,xa,ya,
					   5,17,9999,0,entry_name);
			else
				_NhlAredam(cnp->aws,xa,ya,
					   5,17,0,9999,entry_name);
		}
		if (rbox) {
			xa[0] = xa[1] = xa[4] = gwrx;
			xa[2] = xa[3] = twrx;
			ya[0] = ya[3] = ya[4] = twuy;
			ya[1] = ya[2] = twby;
			if (! (xrev || yrev) || (xrev && yrev)) 
				_NhlAredam(cnp->aws,xa,ya,
					   5,17,9999,0,entry_name);
			else
				_NhlAredam(cnp->aws,xa,ya,
					   5,17,0,9999,entry_name);
		}
		if (bbox) {
			xa[0] = xa[1] = xa[4] = gwlx;
			xa[2] = xa[3] = gwrx;
			ya[0] = ya[3] = ya[4] = gwby;
			ya[1] = ya[2] = twby;
			if (! (xrev || yrev) || (xrev && yrev)) 
				_NhlAredam(cnp->aws,xa,ya,
					   5,17,9999,0,entry_name);
			else
				_NhlAredam(cnp->aws,xa,ya,
					   5,17,0,9999,entry_name);
		}
		if (tbox) {
			xa[0] = xa[1] = xa[4] = gwlx;
			xa[2] = xa[3] = gwrx;
			ya[0] = ya[3] = ya[4] = twuy;
			ya[1] = ya[2] = gwuy;
			if (! (xrev || yrev) || (xrev && yrev)) 
				_NhlAredam(cnp->aws,xa,ya,
					   5,17,9999,0,entry_name);
			else
				_NhlAredam(cnp->aws,xa,ya,
					   5,17,0,9999,entry_name);
		}
	}
	else {
		NhlBoolean	started = False;
		float		xinc,yinc; 
		int		j;
		char		cval[4];

		xa[0] = xa[1] = xa[4] = cnp->xlb;
		xa[2] = xa[3] = cnp->xub;
		ya[0] = ya[3] = ya[4] = cnp->ylb;
		ya[1] = ya[2] = cnp->yub;

		for (i=0;  i < 4; i++) {
			xinc = (xa[i+1] - xa[i]) / _cnMAPBOUNDINC;
			yinc = (ya[i+1] - ya[i]) / _cnMAPBOUNDINC;
			if (! started) {
				_NhlMapita(cnp->aws,ya[i],xa[i],
					   0,3,-1,0,entry_name);
#if 0
				c_mapit(ya[i],xa[i],0);
#endif
				started = True;
			}
			for (j = 0; j < _cnMAPBOUNDINC + 1; j++) {
				_NhlMapita(cnp->aws,ya[i]+j*yinc,xa[i]+j*xinc,
					   1,3,-1,0,entry_name);
#if 0
				c_mapit(ya[i]+j*yinc,xa[i]+j*xinc,1);
#endif
			}
		}
		_NhlMapiqa(cnp->aws,3,-1,0,entry_name);
#if 0
		c_mapiq();
#endif

		c_mpgetc("OU",cval,3);
		c_mpsetc("OU","NO");
		c_mpseti("G2",3);
		c_mpseti("VS",1);
		_NhlMapbla(cnp->aws,entry_name);
		c_mpsetc("OU",cval);
	}
	return NhlNOERROR;
}

/*
 * Function:	GetData
 *
 * Description:	
 *
 * In Args:	cl	ContourPlotLayer instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: 
 *		 
 */	

static NhlErrorTypes GetData
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	float		**scalar_field,
	int		*first_dim,
	int		*second_dim
)
#else
(cl,scalar_field,first_dim,second_dim)
	NhlContourPlotLayer	cl;
	float		**scalar_field;
	int		*first_dim;
	int		*second_dim;
#endif
{
	char			*e_text;
	char			*entry_name = "ContourPlotDraw";
	NhlContourPlotLayerPart	*cnp = &(cl->contourplot);
	NhlScalarFieldFloatLayer	sfl;
	NhlScalarFieldFloatLayerPart	*sfp;
	_NhlDataNodePtr			*dlist = NULL;
	NhlBoolean			new;

	if (_NhlGetDataInfo(cnp->scalar_field_data,&dlist) != 1) {
		e_text = "%s: internal error retrieving data info";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (cnp->sfp != NULL && cnp->osfp == NULL) {
		cnp->osfp = NhlMalloc(sizeof(NhlScalarFieldFloatLayerPart));
		if (cnp->osfp == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	if (cnp->sfp != NULL) {
		memcpy(cnp->osfp,
		       cnp->sfp,sizeof(NhlScalarFieldFloatLayerPart));	
	}
	sfl = (NhlScalarFieldFloatLayer) _NhlGetDataSet(dlist[0],&new);
	if (sfl == NULL) {
		e_text = "%s: internal error retrieving data set";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	sfp = (NhlScalarFieldFloatLayerPart *) &sfl->sfieldfloat;

	*scalar_field = &((float *) sfp->d_arr->data)[sfp->begin];

	return NhlNOERROR;
}
/*
 * Function:	SetRegionAttrs
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: Updates various internal parameters in Conpack,Plotchar,
 *		 etc.
 *		 
 */	

static void SetRegionAttrs
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlcnRegionAttrs *reg_attrs, 
	int cpix
)
#else
(cl,reg_attrs,cpix)
	NhlContourPlotLayer	cl;
	NhlcnRegionAttrs *reg_attrs;
	int		 cpix;
#endif
{
	reg_attrs->gks_pcolor = reg_attrs->perim_color == NhlTRANSPARENT ?
                NhlTRANSPARENT : _NhlGetGksCi(cl->base.wkptr,
                                              reg_attrs->perim_color);
	reg_attrs->gks_fcolor = reg_attrs->fill_color  == NhlTRANSPARENT ?
		NhlTRANSPARENT : _NhlGetGksCi(cl->base.wkptr,
                                              reg_attrs->fill_color);
	
	c_cpseti("PAI",cpix);
	if (reg_attrs->perim_on)
		c_cpseti("CLU",1);
	else
		c_cpseti("CLU",0);

	if (cpix == -1)
		c_cpseti("AIA",0);
	else if (cpix == -2)
		c_cpseti("AIA",98);
	else
		c_cpseti("AIA",-1);

	return;

}
/*
 * Function:	UpdateLineAndLabelParams
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: Updates various internal parameters in Conpack,Plotchar,
 *		 etc.
 *		 
 */	

static NhlErrorTypes UpdateLineAndLabelParams
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlBoolean	*do_lines,
	NhlBoolean	*do_labels
)
#else
(cl,do_lines,do_labels)
        NhlContourPlotLayer	cl;
	NhlBoolean	*do_lines;
	NhlBoolean	*do_labels;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cl->contourplot);
	float			*clvp;
	int			*clup;
	int			i;
	float			height;

	cnp->line_lbls.text = (NhlString *) cnp->llabel_strings->data;
	if (cnp->line_lbls.mono_color) {
                if (cnp->line_lbls.color == NhlTRANSPARENT)
                        cnp->line_lbls.gks_color =  NhlTRANSPARENT;
                else
                        cnp->line_lbls.gks_color =
                                 _NhlGetGksCi(cl->base.wkptr,
                                                     cnp->line_lbls.color);
        }
	else
		cnp->line_lbls.colors =  cnp->gks_llabel_colors;
        if (cnp->line_lbls.back_color == NhlTRANSPARENT)
                cnp->line_lbls.gks_bcolor = NhlTRANSPARENT;
        else
                cnp->line_lbls.gks_bcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->line_lbls.back_color);
        if (cnp->line_lbls.perim_lcolor == NhlTRANSPARENT)
                cnp->line_lbls.gks_plcolor = NhlTRANSPARENT;
        else
                cnp->line_lbls.gks_plcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->line_lbls.perim_lcolor);

        if (cnp->high_lbls.color == NhlTRANSPARENT)
                cnp->high_lbls.gks_color =  NhlTRANSPARENT;
        else
                cnp->high_lbls.gks_color =
                         _NhlGetGksCi(cl->base.wkptr,
                                             cnp->high_lbls.color);
        if (cnp->high_lbls.back_color == NhlTRANSPARENT)
                cnp->high_lbls.gks_bcolor = NhlTRANSPARENT;
        else
                cnp->high_lbls.gks_bcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->high_lbls.back_color);
        if (cnp->high_lbls.perim_lcolor == NhlTRANSPARENT)
                cnp->high_lbls.gks_plcolor = NhlTRANSPARENT;
        else
                cnp->high_lbls.gks_plcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->high_lbls.perim_lcolor);


        if (cnp->low_lbls.color == NhlTRANSPARENT)
                cnp->low_lbls.gks_color =  NhlTRANSPARENT;
        else
                cnp->low_lbls.gks_color =
                         _NhlGetGksCi(cl->base.wkptr,
                                             cnp->low_lbls.color);
        if (cnp->low_lbls.back_color == NhlTRANSPARENT)
                cnp->low_lbls.gks_bcolor = NhlTRANSPARENT;
        else
                cnp->low_lbls.gks_bcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->low_lbls.back_color);
        if (cnp->low_lbls.perim_lcolor == NhlTRANSPARENT)
                cnp->low_lbls.gks_plcolor = NhlTRANSPARENT;
        else
                cnp->low_lbls.gks_plcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->low_lbls.perim_lcolor);

	SetRegionAttrs(cl,&cnp->grid_bound,-1);
	SetRegionAttrs(cl,&cnp->missing_val,-2);
	SetRegionAttrs(cl,&cnp->out_of_range,-3);

	*do_lines = True;
	*do_labels = False;

	gset_line_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_text_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_linewidth(1.0);

	c_cpseti("CLS",0);		/* Conpack not to select levels */
	c_cpseti("NCL",cnp->level_count); 
	clvp = (float *) cnp->levels->data;
	clup = (int *) cnp->level_flags->data;
	c_cpseti("DPU",-1); /* dash pattern use flag */

	cnp->gks_line_colors[0] = cnp->mono_line_color ?
		_NhlGetGksCi(cl->base.wkptr,cnp->line_color) : 
			_NhlGetGksCi(cl->base.wkptr,
				     ((int *)cnp->line_colors->data)[0]);
	if (cnp->mono_line_color && cnp->gks_line_colors[0] == NhlTRANSPARENT)
		*do_lines = False;
	if (! cnp->lines_on)
		*do_lines = False;

	for (i=0; i<cnp->level_count; i++) {
		int pai,aia,aib;
		NhlcnLevelUseMode flag;

		pai = i+1;
		aib = NhlcnAREAID_OFFSET+i;
		aia = NhlcnAREAID_OFFSET+i+1;
		c_cpseti("PAI",pai);
		c_cpsetr("CLV",(float)clvp[i]);
		flag = cnp->mono_level_flag ? cnp->level_flag :(NhlcnLevelUseMode)clup[i];
		if (*do_lines) {
			c_cpseti("CLU",flag);
		}
		else {
			switch (flag) {
			case NhlNOLINE:
			case NhlLINEONLY:
			default:
				c_cpseti("CLU",NhlNOLINE);
				break;
			case NhlLABELONLY:
			case NhlLINEANDLABEL:
				c_cpseti("CLU",NhlLABELONLY);
				break;
			}
		}
		c_cpseti("AIB",aib);
		c_cpseti("AIA",aia);
#if 0
		printf("pai %d,clv %f,aib %d,aia %d\n",pai,clvp[i],aib,aia);
#endif
		c_cpsetc("LLT",((NhlString*)cnp->line_lbls.text)[i]);
	}
	if (cnp->level_selection_mode != NhlEXPLICITLEVELS)
		c_cpsetr("CIU",(float)cnp->level_spacing);
 
/* Set up for labels */

/* Conpack not to render the Informational label */
	c_cpsetc("ILT"," ");

/* Line labels */
	if (! cnp->line_lbls.on) {
		c_cpseti("LLP",0); 
	}
	else if (cnp->llabel_placement == NhlCONSTANT) {
		*do_labels = True;
		c_cpseti("LLP",1);
		c_cpsetr("DPS",
			 (float)(cnp->line_lbls.real_height / cl->view.width));
		c_cpsetr("DPV",(float).015);
#if 0
		c_cpsetr("RC3",(float)0.0);
		c_cpseti("LLP",2);
#endif
		if (cnp->line_lbls.angle < 0.0) 
			c_cpseti("LLO",1); /* angle to contour direction */
		else {
			c_cpseti("LLO",0); /* fixed angle  */
			c_cpsetr("LLA",(float)cnp->line_lbls.angle);
		}

	}
	else if (cnp->llabel_placement == NhlRANDOMIZED) {
		*do_labels = True;
		c_cpseti("LLP",2);
		if (cnp->line_lbls.angle < 0.0) 
			c_cpseti("LLO",1); /* angle to contour direction */
		else {
			c_cpseti("LLO",0); /* fixed angle  */
			c_cpsetr("LLA",(float)cnp->line_lbls.angle);
		}
	}
	else {
		*do_labels = True;
		c_cpseti("LLP",3);
		if (cnp->line_lbls.angle < 0.0) 
			c_cpseti("LLO",1); /* angle to contour direction */
		else {
			c_cpseti("LLO",0); /* fixed angle  */
			c_cpsetr("LLA",(float)cnp->line_lbls.angle);
		}
	}

	if (*do_labels) {
		height = cnp->line_lbls.real_height / cl->view.width;
		c_cpsetr("LLS",(float)height);
		c_cpsetr("LLW", 
			 (float) (height * cnp->line_lbls.perim_space));
		if (cnp->line_lbls.back_color == NhlTRANSPARENT) {
			if (! cnp->line_lbls.perim_on) 
				c_cpseti("LLB",0); 
			else
				c_cpseti("LLB",1);
		}
		else {
			c_cpseti("LBC",cnp->line_lbls.back_color);
			if (! cnp->line_lbls.perim_on)
				c_cpseti("LLB",2);
			else
				c_cpseti("LLB",3);
		}
	}

/*
 * In order to allow user control of the high and low attributes 
 * individually set the appropriate part of the flag on if either 
 * the high or the low is on. Further distinguishing between high and low
 * occurs in the low level routine cpchhl_
 */
	if (! cnp->high_lbls.on)
		c_cpsetc("HIT"," ");
	else 
		c_cpsetc("HIT",(NhlString)cnp->high_lbls.text);

	if (! cnp->low_lbls.on)
		c_cpsetc("LOT"," ");
	else
		c_cpsetc("LOT",(NhlString)cnp->low_lbls.text);

/*
 * Due to the way Conpack works it is not possible to have different text
 * sizes, white space, background or perim on/off settings for the high
 * and low labels. The high labels take precedence, so set up accordingly.
 */

	if (cnp->high_lbls.on || cnp->low_lbls.on) {
		*do_labels = True;
		height = cnp->high_lbls.real_height / cl->view.width;
		c_cpsetr("HLS",(float)height);
		c_cpsetr("HLW",(float)(cnp->high_lbls.perim_space  * height));
		c_cpsetr("HLA",(float)cnp->high_lbls.angle);
		c_cpseti("HLO", (int) cnp->high_low_overlap);

		if (cnp->high_lbls.back_color == NhlTRANSPARENT) {
			if (! cnp->high_lbls.perim_on) 
				c_cpseti("HLB",0); 
			else
				c_cpseti("HLB",1);
		}
		else {
			if (! cnp->high_lbls.perim_on)
				c_cpseti("HLB",2);
			else
				c_cpseti("HLB",3);
		}
	}

	c_pcsetc("FC",":");
	return ret;

}

/*
 * Function:	UpdateFillInfo
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: sets the do_fill Boolean flag depending on whether 
 *		 fill is to be done.
 *		 
 */	

static NhlErrorTypes UpdateFillInfo
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlBoolean	*do_fill
)
#else
(cl,do_fill)
        NhlContourPlotLayer	cl;
	NhlBoolean	*do_fill;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cl->contourplot);
 	NhlBoolean	color_fill, pattern_fill;

	color_fill = (cnp->mono_fill_color && 
		      cnp->fill_color == NhlTRANSPARENT) ? False : True;
	pattern_fill = (cnp->mono_fill_pattern && 
			cnp->fill_pattern == NhlHOLLOWFILL) ? False : True;

	if (color_fill &&  pattern_fill &&  
	    (cnp->fill_on || cnp->raster_mode_on)) {
		*do_fill = True;
		return ret;
	}

	*do_fill = False;
	return ret;
}


/*
 * Function:	InitCoordBounds
 *
 * Description: 
 *
 * In Args:
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes InitCoordBounds
#if	NhlNeedProto
(
        NhlContourPlotLayer	cl,
        NhlContourPlotLayer	ocl,
	char			*entry_name
)
#else
(cl,ocl,entry_name)
        NhlContourPlotLayer	cl;
        NhlContourPlotLayer	ocl;
	char			*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
        NhlContourPlotLayerPart	*cnp = &cl->contourplot;
        NhlTransformLayerPart	*tfp = &cl->trans;
	char		*e_text;
	NhlBoolean	x_data_reversed,y_data_reversed;

	cnp->do_low_level_log = False;
        cnp->use_irr_trans = False;
        
	if (! cnp->data_init) {
                tfp->data_xstart = tfp->data_xend = 0.0;
                tfp->data_ystart = tfp->data_yend = 0.0;
                
		if (! tfp->x_reverse) {
			cnp->xlb = cnp->xc1 = tfp->x_min;
			cnp->xub = cnp->xcm = tfp->x_max;
		}
		else {
			cnp->xub = cnp->xc1 = tfp->x_max;
			cnp->xlb = cnp->xcm = tfp->x_min;
		}
		if (! tfp->y_reverse) {
			cnp->ylb = cnp->yc1 = tfp->y_min;
			cnp->yub = cnp->ycn = tfp->y_max;
		}
		else {
			cnp->yub = cnp->yc1 = tfp->y_max;
			cnp->ylb = cnp->ycn = tfp->y_min;
		}
                return ret;
	}
        
	x_data_reversed = cnp->sfp->x_start > cnp->sfp->x_end;
	y_data_reversed = cnp->sfp->y_start > cnp->sfp->y_end;

        tfp->data_xstart = cnp->sfp->x_start;
        tfp->data_xend = cnp->sfp->x_end;
        tfp->data_ystart = cnp->sfp->y_start;
        tfp->data_yend = cnp->sfp->y_end;
        
        if (cnp->sfp->x_arr || cnp->sfp->y_arr)
                cnp->use_irr_trans = True;
        
        if (cnp->use_irr_trans) {
                if (cnp->sfp->x_arr && ! tfp->x_axis_type_set) {
			if (! cnp->osfp || (cnp->data_changed  &&
			    (cnp->sfp->changed & _NhlsfXARR_CHANGED)))
				tfp->x_axis_type = NhlIRREGULARAXIS;
		}
                if (! cnp->sfp->x_arr && tfp->x_axis_type == NhlIRREGULARAXIS)
                        tfp->x_axis_type = NhlLINEARAXIS;
                if (cnp->sfp->x_arr && tfp->x_axis_type != NhlIRREGULARAXIS) {
                        tfp->data_xstart = cnp->sfp->ix_start;
                        tfp->data_xend = cnp->sfp->ix_end;
			x_data_reversed = False;
                }
                if (cnp->sfp->y_arr && ! tfp->y_axis_type_set) {
			if (! cnp->osfp || (cnp->data_changed  &&
			    (cnp->sfp->changed & _NhlsfYARR_CHANGED)))
				tfp->y_axis_type = NhlIRREGULARAXIS;
		}
                if (! cnp->sfp->y_arr && tfp->y_axis_type == NhlIRREGULARAXIS)
                        tfp->y_axis_type = NhlLINEARAXIS;
                if (cnp->sfp->y_arr && tfp->y_axis_type != NhlIRREGULARAXIS) {
                        tfp->data_ystart = cnp->sfp->iy_start;
                        tfp->data_yend = cnp->sfp->iy_end;
			y_data_reversed = False;
                }
        }
        
		ret = _NhltfCheckCoordBounds
                ((NhlTransformLayer)cl,(NhlTransformLayer)ocl,
                 cnp->use_irr_trans,entry_name);
        
	return ret;
}

/*
 * Function:	SetUpLLTransObj
 *
 * Description: Sets up a LogLinear transformation object.
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
static NhlErrorTypes SetUpLLTransObj
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init
)
#else 
(cnew,cold,init)
	NhlContourPlotLayer	cnew;
	NhlContourPlotLayer	cold;
	NhlBoolean	init;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cold->contourplot);
	NhlTransformLayerPart	*tfp = &(cnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[32];
        int			nargs = 0;

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";
        
	if (init)
		tfp->trans_obj = NULL;
	if (tfp->trans_obj &&
                 tfp->trans_obj->base.layer_class->base_class.class_name !=
                 NhllogLinTransObjClass->base_class.class_name) {
		subret = NhlDestroy(tfp->trans_obj->base.id);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error destroying irregular trans object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		tfp->trans_obj = NULL;
	}
        
        if (tfp->x_reverse_set)
                NhlSetSArg(&sargs[nargs++],
                           NhlNtrXReverse,tfp->x_reverse);
        if (tfp->y_reverse_set)
                NhlSetSArg(&sargs[nargs++],
                           NhlNtrYReverse,tfp->y_reverse);
        
	if (tfp->trans_obj == NULL) {

		cnp->new_draw_req = True;
                cnp->update_req = True;
                
		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,tfp->x_log);
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,tfp->y_log);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
                NhlSetSArg(&sargs[nargs++],NhlNtrLineInterpolationOn,
			   tfp->line_interpolation_on);
                
		sprintf(buffer,"%s",cnew->base.name);
		strcat(buffer,".Trans");
		subret = NhlALCreate(&tmpid,buffer,
				     NhllogLinTransObjClass,
				     cnew->base.id,sargs,nargs);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
        else {
                if (tfp->x_min != cold->trans.x_min)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
                if (tfp->x_max != cold->trans.x_max)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
                if (tfp->y_min != cold->trans.y_min)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
                if (tfp->y_max != cold->trans.y_max)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
                if (tfp->x_log != cold->trans.x_log)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXLog,tfp->x_log);
                if (tfp->y_log != cold->trans.y_log)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYLog,tfp->y_log);
		if (tfp->line_interpolation_on != 
		    cold->trans.line_interpolation_on)
			NhlSetSArg(&sargs[nargs++],NhlNtrLineInterpolationOn,
				   tfp->line_interpolation_on);

                subret = NhlALSetValues(tfp->trans_obj->base.id,sargs,nargs);
                if (nargs > 0) {
                        cnp->new_draw_req = True;
                        cnp->update_req = True;
                }
        }
        
        NhlVAGetValues(tfp->trans_obj->base.id,
                       NhlNtrXReverse,&tfp->x_reverse,
                       NhlNtrYReverse,&tfp->y_reverse,
                       NhlNtrXLog,&tfp->x_log,
                       NhlNtrYLog,&tfp->y_log,
                       NhlNtrXMinF,&tfp->x_min,
                       NhlNtrXMaxF,&tfp->x_max,
                       NhlNtrYMinF,&tfp->y_min,
                       NhlNtrYMaxF,&tfp->y_max,
                       NULL);
	tfp->x_axis_type = tfp->x_log ? NhlLOGAXIS : NhlLINEARAXIS;
	tfp->y_axis_type = tfp->y_log ? NhlLOGAXIS : NhlLINEARAXIS;

	return MIN(ret,subret);

}

/*
 * Function:	SetUpIrrTransObj
 *
 * Description: Sets up an Irregular transformation object.
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
static NhlErrorTypes SetUpIrrTransObj
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnew,
	NhlContourPlotLayer	cold,
	NhlBoolean	init
)
#else 
(cnew,cold,init)
	NhlContourPlotLayer	cnew;
	NhlContourPlotLayer	cold;
	NhlBoolean	init;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cold->contourplot);
	NhlTransformLayerPart	*tfp = &(cnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[32];
        int			nargs = 0;

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";

	if (init)
		tfp->trans_obj = NULL;
	if (tfp->trans_obj &&
            tfp->trans_obj->base.layer_class->base_class.class_name !=
	    NhlirregularTransObjClass->base_class.class_name) {
		subret = NhlDestroy(tfp->trans_obj->base.id);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error destroying irregular trans object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		tfp->trans_obj = NULL;
	}
	if (! cnp->data_init) return ret;
        
        if (tfp->x_reverse_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,tfp->x_reverse);
        if (tfp->y_reverse_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,tfp->y_reverse);

	if (init || tfp->trans_obj == NULL) {

		cnp->new_draw_req = True;
                cnp->update_req = True;
                
		if (cnp->sfp->x_arr)
			NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
				   cnp->sfp->x_arr);
		if (cnp->sfp->y_arr)
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
				   cnp->sfp->y_arr);
                
                NhlSetSArg(&sargs[nargs++],NhlNtrXAxisType,tfp->x_axis_type);
                NhlSetSArg(&sargs[nargs++],NhlNtrYAxisType,tfp->y_axis_type);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
                NhlSetSArg(&sargs[nargs++],NhlNtrDataXStartF,tfp->data_xstart);
                NhlSetSArg(&sargs[nargs++],NhlNtrDataXEndF,tfp->data_xend);
                NhlSetSArg(&sargs[nargs++],NhlNtrDataYStartF,tfp->data_ystart);
                NhlSetSArg(&sargs[nargs++],NhlNtrDataYEndF,tfp->data_yend);
                NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,cnp->x_tension);
                NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,cnp->y_tension);
                NhlSetSArg(&sargs[nargs++],NhlNtrLineInterpolationOn,
			   tfp->line_interpolation_on);
                
		sprintf(buffer,"%s",cnew->base.name);
		strcat(buffer,".Trans");

		subret = NhlALCreate(&tmpid,buffer,
				     NhlirregularTransObjClass,
				     cnew->base.id,sargs,nargs);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);
		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
        else {
                if (cnp->data_changed &&cnp->sfp->x_arr &&
                    (cnp->sfp->changed & _NhlsfXARR_CHANGED))
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrXCoordPoints,cnp->sfp->x_arr);
                if (cnp->data_changed && cnp->sfp->y_arr &&
		    (cnp->sfp->changed & _NhlsfYARR_CHANGED))
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrYCoordPoints,cnp->sfp->y_arr);
                if (tfp->x_axis_type != cold->trans.x_axis_type)        
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrXAxisType,tfp->x_axis_type);
                if (tfp->y_axis_type != cold->trans.y_axis_type)        
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrYAxisType,tfp->y_axis_type);
        
                if (tfp->x_min != cold->trans.x_min)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
                if (tfp->x_max != cold->trans.x_max)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
                if (tfp->y_min != cold->trans.y_min)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
                if (tfp->y_max != cold->trans.y_max)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
        
                if (tfp->data_xstart != cold->trans.data_xstart)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrDataXStartF,tfp->data_xstart);
                if (tfp->data_xend != cold->trans.data_xend)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrDataXEndF,tfp->data_xend);
                if (tfp->data_ystart != cold->trans.data_ystart)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrDataYStartF,tfp->data_ystart);
                if (tfp->data_yend != cold->trans.data_yend)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrDataYEndF,tfp->data_yend);
		if (tfp->line_interpolation_on != 
		    cold->trans.line_interpolation_on)
			NhlSetSArg(&sargs[nargs++],NhlNtrLineInterpolationOn,
				   tfp->line_interpolation_on);

        
                if (cnp->x_tension != ocnp->x_tension)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrXTensionF,cnp->x_tension);
                if (cnp->y_tension != ocnp->y_tension)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrYTensionF,cnp->y_tension);
                subret = NhlALSetValues(tfp->trans_obj->base.id,sargs,nargs);

                if (nargs > 0) {
                        cnp->new_draw_req = True;
                        cnp->update_req = True;
                }
        }
        
        NhlVAGetValues(tfp->trans_obj->base.id,
                       NhlNtrXReverse,&tfp->x_reverse,
                       NhlNtrYReverse,&tfp->y_reverse,
                       NhlNtrXAxisType,&tfp->x_axis_type,
                       NhlNtrYAxisType,&tfp->y_axis_type,
                       NhlNtrDataXStartF,&tfp->data_xstart,
                       NhlNtrDataXEndF,&tfp->data_xend,
                       NhlNtrDataYStartF,&tfp->data_ystart,
                       NhlNtrDataYEndF,&tfp->data_yend,
                       NhlNtrXMinF,&tfp->x_min,
                       NhlNtrXMaxF,&tfp->x_max,
                       NhlNtrYMinF,&tfp->y_min,
                       NhlNtrYMaxF,&tfp->y_max,
                       NULL);

        tfp->x_log = tfp->x_axis_type == NhlLOGAXIS ? True : False;
        tfp->y_log = tfp->y_axis_type == NhlLOGAXIS ? True : False;

        cnp->do_low_level_log = tfp->x_axis_type == NhlLOGAXIS ||
                tfp->y_axis_type == NhlLOGAXIS ? True : False;
        
	return MIN(ret,subret);

}

/*
 * Function:	SetLabelFormats
 *
 * Description: Sets up the format records for all the Conpack label.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes SetLabelFormats
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init
)
#else 
(cnnew,cnold,init)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString e_text;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	NhlString entry_name;

	entry_name =  init ? "ContourPlotInitialize" : "ContourPlotSetValues";
/*
 * check the constant spacing value - by the name of the routine this
 * does not belong here -- but for now, it will do
 */
	e_text = 
		"%s: Constant spacing cannot be less than zero, defaulting %s";
	if (cnp->line_lbls.cspacing < 0.0) {
		cnp->line_lbls.cspacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNcnLineLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}
	if (cnp->high_lbls.cspacing < 0.0) {
		cnp->high_lbls.cspacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNcnHighLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}
	if (cnp->low_lbls.cspacing < 0.0) {
		cnp->low_lbls.cspacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNcnLowLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}
	if (cnp->info_lbl.cspacing < 0.0) {
		cnp->info_lbl.cspacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNcnInfoLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}
	if (cnp->constf_lbl.cspacing < 0.0) {
		cnp->constf_lbl.cspacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNcnConstFLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}

	if (init) {
		subret = SetFormatRec(&cnp->max_data_format,
				      NhlNcnMaxDataValueFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

		subret = SetFormatRec(&cnp->line_lbls.format,
				      NhlNcnLineLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

		subret = SetFormatRec(&cnp->high_lbls.format,
				      NhlNcnHighLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

		subret = SetFormatRec(&cnp->low_lbls.format,
				      NhlNcnLowLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

		subret = SetFormatRec(&cnp->info_lbl.format,
				      NhlNcnInfoLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

		subret = SetFormatRec(&cnp->constf_lbl.format,
				      NhlNcnConstFLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		
		return ret;
	}
	if (cnp->max_data_format.fstring != ocnp->max_data_format.fstring) {
		subret = SetFormatRec(&cnp->max_data_format,
				      NhlNcnMaxDataValueFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ocnp->max_data_format.fstring != NULL)
			NhlFree(ocnp->max_data_format.fstring);
		ocnp->max_data_format.fstring = NULL;
	}
	
	if (cnp->line_lbls.format.fstring != ocnp->line_lbls.format.fstring) {
		subret = SetFormatRec(&cnp->line_lbls.format,
				      NhlNcnLineLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ocnp->line_lbls.format.fstring != NULL)
			NhlFree(ocnp->line_lbls.format.fstring);
		ocnp->line_lbls.format.fstring = NULL;
	}
	
	if (cnp->high_lbls.format.fstring != ocnp->high_lbls.format.fstring) {
		subret = SetFormatRec(&cnp->high_lbls.format,
				      NhlNcnHighLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ocnp->high_lbls.format.fstring != NULL)
			NhlFree(ocnp->high_lbls.format.fstring);
		ocnp->high_lbls.format.fstring = NULL;
	}
	if (cnp->low_lbls.format.fstring != ocnp->low_lbls.format.fstring) {
		subret = SetFormatRec(&cnp->low_lbls.format,
				      NhlNcnLowLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ocnp->low_lbls.format.fstring != NULL)
			NhlFree(ocnp->low_lbls.format.fstring);
		ocnp->low_lbls.format.fstring = NULL;
	}
	if (cnp->info_lbl.format.fstring != ocnp->info_lbl.format.fstring) {
		subret = SetFormatRec(&cnp->info_lbl.format,
				      NhlNcnInfoLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ocnp->info_lbl.format.fstring != NULL)
			NhlFree(ocnp->info_lbl.format.fstring);
		ocnp->info_lbl.format.fstring = NULL;
	}
	if (cnp->constf_lbl.format.fstring != 
	    ocnp->constf_lbl.format.fstring) {
		subret = SetFormatRec(&cnp->constf_lbl.format,
				      NhlNcnConstFLabelFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ocnp->constf_lbl.format.fstring != NULL)
			NhlFree(ocnp->constf_lbl.format.fstring);
		ocnp->constf_lbl.format.fstring = NULL;
	}
	return ret;
}

/*
 * Function:	ManageLabels
 *
 * Description: Manages all the non-array label types (not line labels).
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageLabels
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew,cnold, init, sargs, nargs)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	char *tcp;
	NhlString entry_name, e_text;

	entry_name =  init ? "ContourPlotInitialize" : "ContourPlotSetValues";

/*
 * Set up the label strings and the format records
 */
	if (init) {
		tcp = NULL;
		subret = SetLabelString(&tcp,(NhlString)cnp->high_lbls.text,
					NhlcnDEF_HIGH_LABEL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		cnp->high_lbls.text = (NhlPointer) tcp;

		tcp = NULL;
		subret = SetLabelString(&tcp,(NhlString)cnp->low_lbls.text,
					NhlcnDEF_LOW_LABEL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		cnp->low_lbls.text = (NhlPointer) tcp;
	}
	else {

		subret = SetLabelString((NhlString *)&ocnp->high_lbls.text,
					(NhlString)cnp->high_lbls.text,
					NhlcnDEF_HIGH_LABEL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

		subret = SetLabelString((NhlString *)&ocnp->low_lbls.text,
					(NhlString)cnp->low_lbls.text,
					NhlcnDEF_LOW_LABEL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
	}

	if (cnp->high_use_line_attrs && cnp->line_lbls.on) {
		subret = CopyTextAttrs(&cnp->high_lbls,
				       &cnp->line_lbls,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	}
	
	if (cnp->low_use_high_attrs && cnp->high_lbls.on) {
		subret = CopyTextAttrs(&cnp->low_lbls,
				       &cnp->high_lbls,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
		cnp->low_lbls.angle = cnp->high_lbls.angle;
	}

	if (cnp->constf_use_info_attrs && cnp->info_lbl.on) {
		subret = CopyTextAttrs(&cnp->constf_lbl,
				       &cnp->info_lbl,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
		cnp->constf_lbl.angle = cnp->info_lbl.angle;
	}

/* Manage constant field label */

	subret = ManageConstFLabel(cnnew,cnold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing constant field label";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	

/* Manage info label */

	subret = ManageInfoLabel(cnnew,cnold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing information label";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	
	return ret;
}

/*
 * Function:	SetLabelScale
 *
 * Description: Determines the label scale factor based on the label
 *		scale mode and the label scale value resources. Note that
 *		the scale factor is the amount by which the label values
 *		are multiplied to arrive at the true values in the scalar
 *		field data. Therefore the data values are divided by the
 *		scale factor to get the label values.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	
 */
static NhlErrorTypes SetLabelScale
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init
)
#else 
(cnnew,cnold, init)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	NhlString entry_name, e_text;
	float sigval,t;
	int power, i, count;
	int divpwr,sig_digits;
	float *fp;
	NhlcnLevelUseMode *lusep, luse = NhlLABELONLY;
	float test_high, test_low, max_fac = 1.0;
	int max_digit = 0;

	if (! init &&
	    ! cnp->data_changed &&
	    (cnp->label_scaling_mode == ocnp->label_scaling_mode) &&
	    (cnp->label_scale_value == ocnp->label_scale_value))
		return ret;
	entry_name =  init ? "ContourPlotInitialize" : "ContourPlotSetValues";

	if (cnp->max_data_format.left_sig_digit_flag == NhlffDYNAMIC) {
                float test_val;

		test_val = MAX(fabs(cnp->zmax),fabs(cnp->zmin)) 
                        / cnp->label_scale_factor;
		subret = _NhlGetScaleInfo(test_val,
					  &divpwr,&sig_digits,entry_name);
		cnp->max_data_format.left_sig_digit = divpwr - 1;
	}
#if 0 /* possible way to fix problem w/ left sig digits */       
	if (cnp->max_data_format.sig_digits_flag == NhlffDYNAMIC) {
                cnp->max_data_format.sig_digits =
                        MAX(4,MIN(6,cnp->max_data_format.left_sig_digit + 1));
        }
#endif
	switch (cnp->label_scaling_mode) {
	case NhlSCALEFACTOR:
		if (cnp->label_scale_value <= 0.0) {
			e_text = 
			     "%s: invalid value for scale value: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
			ret = MIN(ret,NhlWARNING);
			cnp->label_scale_value = 1.0;
		}
		cnp->label_scale_factor = cnp->label_scale_value;
		break;
	case NhlCONFINETORANGE:
		if (cnp->label_scale_value <= 0.0) {
			e_text = 
			     "%s: invalid value for scale value: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
			ret = MIN(ret,NhlWARNING);
			cnp->label_scale_value = 1.0;
		}
		sigval = MAX(fabs(cnp->zmin),fabs(cnp->zmax));
		power = 1;
		if (sigval >= cnp->label_scale_value) {
			for (t = sigval/10.0;
			     t >=cnp->label_scale_value; t /= 10.0) {
				power++;
			}
			cnp->label_scale_factor = pow(10.0,(double)power);
		}
		else {
			for (t = sigval * 10;
			     t < cnp->label_scale_value; t *= 10.0) {
				power++;
			}
			power--;
			cnp->label_scale_factor = pow(10.0,-(double)power);
		}
		break;
	case NhlTRIMZEROS:
		sigval = MAX(fabs(cnp->zmin),fabs(cnp->zmax));
		subret = _NhlGetScaleInfo(sigval,
					  &divpwr,&sig_digits,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		sig_digits = cnp->max_data_format.sig_digits;
		if (divpwr < 0) 
			power = divpwr;
		else
			power = MAX(0,divpwr - sig_digits);
		cnp->label_scale_factor = pow(10.0,(double)power);
		break;
	case NhlMAXSIGDIGITSLEFT:
		sigval = MAX(fabs(cnp->zmin),fabs(cnp->zmax));
		subret = _NhlGetScaleInfo(sigval,
					  &divpwr,&sig_digits,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		sig_digits = cnp->max_data_format.sig_digits;
		power = divpwr - sig_digits;
		cnp->label_scale_factor = pow(10.0,(double)power);
		break;
	case NhlALLINTEGERS:
		if (cnp->const_field) {
			fp = &cnp->zmax;
			count = 1;
			lusep = &luse;
		}
		else {
			fp = (float *) cnp->levels->data;
			lusep = (NhlcnLevelUseMode *) cnp->level_flags->data;
			count = cnp->level_count;
		}
		sig_digits = cnp->max_data_format.sig_digits;
		test_high = pow(10.0,sig_digits);
		test_low  = pow(10.0,sig_digits - 1);

		for (i = 0; i < count; i++) {
			int	j;
			float	test_fac = 1.0, test_val;
			char	buf[32];

			test_val = fabs(fp[i]);
			if (lusep[i] < NhlLABELONLY || test_val == 0.0)
				continue;
			if (fabs(fp[i]) < test_low) {
				while (test_val < test_low) {
					test_val *= 10.0;
					test_fac *= 10.0;
				}
			}
			else if (fabs(fp[i]) >= test_high) {
				while (test_val >= test_high) {
					test_val /= 10.0;
					test_fac /= 10.0;
				}
			}
			test_val = (float) (int) (test_val + 0.5);

			sprintf(buf,"%f",test_val);
			j = strcspn(buf,"0.");
			if (j > max_digit) {
				max_digit = j;
				max_fac = test_fac;
			}
		}
		while (sig_digits > max_digit) {
			max_fac /= 10.0;
			sig_digits--;
		}
	
		cnp->label_scale_factor = 1.0 / max_fac;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	return ret;
}
/*
 * Function:	ManageOverlay
 *
 * Description: Sets up arguments for annotations handled internally by
 *		the overlay object (TickMark,Title,Legend,LabelBar), then
 *		calls the overlay interface function _NhlManageOverlay.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	
 */
static NhlErrorTypes ManageOverlay
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew,cnold, init, sargs, nargs)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlString entry_name, e_text;

	entry_name =  init ? "ContourPlotInitialize" : "ContourPlotSetValues";

/* Manage TickMarks object */

	/* 18 arguments possible */
	subret = ManageTickMarks(cnnew,cnold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing TickMarks";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage Legend object */

	/* 18 arguments possible */
	subret = ManageTitles(cnnew,cnold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing Titles";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage Legend object */

	/* 18 arguments possible */
	subret = ManageLegend(cnnew,cnold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing Legend";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage LabelBar object */

	subret = ManageLabelBar(cnnew,cnold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing LabelBar";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the overlay */

	/* 1 arg */
	if (cnp->update_req) {
		NhlSetSArg(&sargs[(*nargs)++],NhlNpmUpdateReq,True);
	}
		
	subret = _NhlManageOverlay(&cnp->overlay_object,
			   (NhlLayer)cnnew,(NhlLayer)cnold,(_NhlCalledFrom)init,
				   sargs,*nargs,entry_name);
	ret = MIN(ret,subret);
	return ret;

}
/*
 * Function:	ManageTickMarks
 *
 * Description: If the ContourPlot object has an overlay object attached, and
 *		the TickMarks are activated, manages the TickMark resources 
 *		relevant to the ContourPlot object.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
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
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew, cnold, init, sargs, nargs)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*entry_name;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";
        
 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (cnp->display_tickmarks == NhlNOCREATE) {
                if (init || ocnp->display_tickmarks == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        cnp->display_tickmarks = NhlNEVER;
        }

	if (init || 
	    cnp->display_tickmarks != ocnp->display_tickmarks) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmTickMarkDisplayMode,
				   cnp->display_tickmarks);
	}

	return ret;
}

/*
 * Function:	ManageTitles
 *
 * Description: If the ContourPlot object has an overlay object attached, and
 *		the Titles are activated, manages the Title resources 
 *		relevant to the ContourPlot object.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageTitles
#if	NhlNeedProto
(
 	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew, cnold, init, sargs, nargs)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*entry_name;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";
        
 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (cnp->display_titles == NhlNOCREATE) {
                if (init || ocnp->display_titles == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        cnp->display_titles = NhlNEVER;
        }

	if (init || 
	    cnp->display_titles != ocnp->display_titles) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmTitleDisplayMode,
				   cnp->display_titles);
	}

	return ret;
}

/*
 * Function:	ManageLegend
 *
 * Description: If the ContourPlot object has an overlay object attached, and
 *		the Legend is activated, manages the Legend resources 
 *		relevant to the ContourPlot object.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageLegend
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew, cnold, init, sargs, nargs)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);
	NhlBoolean		do_it;
	NhlBoolean		use_line_label_strings = False;
	int			i, count;
	NhlBoolean		copy_l_colors = False,
				copy_l_dash_pats = False,
				copy_l_thicknesses = False,
				copy_ll_font_colors = False,
				copy_ll_strings = False,
				copy_l_labels = False,
				set_all = False;
	
	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";

 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (cnp->display_legend == NhlNOCREATE) {
                if (init || ocnp->display_legend == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        cnp->display_legend = NhlNEVER;
        }
        
	if (init || 
	    cnp->display_legend != ocnp->display_legend ||
	    cnp->const_field != ocnp->const_field ||
	    cnp->data_init != ocnp->data_init ||
	    cnp->lgnd_level_flags != ocnp->lgnd_level_flags) {
		if (cnp->const_field) {
			e_text = "%s: constant field: turning Legend off";
			NhlPError(NhlINFO,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlINFO);
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmLegendDisplayMode,NhlNEVER);
		}
		else {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmLegendDisplayMode,
				   cnp->display_legend);
			if (init || cnp->const_field != ocnp->const_field)
				set_all = True;
		}
	}
/*
 * Create GenArrays for all the Legend array resources, copying the 
 * corresponding ContourPlot resource, but not (for now at least) the data.
 */
	if (init) {
		cnp->lgnd_l_colors = 
			_NhlCopyGenArray(cnp->line_colors,False);
		cnp->lgnd_l_dash_pats = 
			_NhlCopyGenArray(cnp->line_dash_patterns,False);
		cnp->lgnd_l_thicknesses = 
			_NhlCopyGenArray(cnp->line_thicknesses,False);
		cnp->lgnd_ll_font_colors =
			_NhlCopyGenArray(cnp->llabel_colors,False);
		cnp->lgnd_ll_strings =
			_NhlCopyGenArray(cnp->ll_strings,False);
		cnp->lgnd_labels = 
			_NhlCopyGenArray(cnp->llabel_strings,False);
	}

/*
 * If the explicit legend labels flag is set True, and the legend labels
 * resource is set, copy that array for the legend labels. If True and
 * the resource has not yet been set, use the contour line labels once
 * only. 
 */

	if (! cnp->explicit_lgnd_labels_on) {
		cnp->lgnd_labels_set = False;
		use_line_label_strings = True;
	}
	else {
		if (cnp->lgnd_labels_res_set) {
			NhlGenArray ga;

			NhlFreeGenArray(cnp->lgnd_labels);
			if ((ga = _NhlCopyGenArray(cnp->lgnd_labels_res,
						   True)) == NULL) {
				e_text = "%s: error copying GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			cnp->lgnd_labels = ga;
			ocnp->lgnd_labels = NULL;
		}
		else if (! cnp->lgnd_labels_set) {
			use_line_label_strings = True;
		}
		cnp->lgnd_labels_set = True;
	}
	if (use_line_label_strings) {
		cnp->lgnd_func_code = cnp->line_lbls.fcode[0];
	}
	
/*
 * Decide whether all lines or only a subset is going to the Legend
 */	
	cnp->lgnd_line_count = cnp->level_count;
	if (cnp->lgnd_level_flags) {
		NhlcnLevelUseMode *flags;
		flags = (NhlcnLevelUseMode *) cnp->lgnd_level_flags->data;
		cnp->lgnd_line_count = 0;
		for (i = 0; i < cnp->level_count; i++) {
			if (flags[i] == NhlLINEONLY ||
			    flags[i] == NhlLINEANDLABEL) {
				cnp->lgnd_line_count++;
			}
		}
		
		if (cnp->lgnd_line_count == 0)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmLegendDisplayMode,NhlNEVER);
	}
/*
 * If a subset is needed create the appropriate data and patch it into the
 * GenArrays for the Legend. (For the string GenArrays, just call the GenArray
 * copy function because it handles the details of freeing the old string
 * data when necessaray.) Otherwise if not at the 
 * initialization stage just copy the appropriate Contour GenArray, 
 * without copying the data.
 */
	
	if (cnp->lgnd_line_count < cnp->level_count && 
	    cnp->lgnd_line_count > 0) {
		
		NhlcnLevelUseMode *flags;
		NhlColorIndex *cixp, *c_cixp, *ll_cixp, *c_ll_cixp;
		NhlDashIndex *dixp, *c_dixp;
		float *thkp, *c_thkp;
		NhlString *ll_sp, *c_ll_sp, *labels, *c_labels;

		flags = (NhlcnLevelUseMode *) cnp->lgnd_level_flags->data;
		if (init ||
		    cnp->lgnd_level_flags != ocnp->lgnd_level_flags) {
			copy_l_colors = True;
			copy_l_dash_pats = True;
			copy_l_thicknesses = True;
			copy_ll_font_colors = True;
			copy_ll_strings = True;
			if (use_line_label_strings)
				copy_l_labels = True;
		}
		else {
			if (cnp->line_colors != ocnp->line_colors) 
				copy_l_colors = True;
			if (cnp->line_dash_patterns != 
			    ocnp->line_dash_patterns)
				copy_l_dash_pats = True;
			if (cnp->line_thicknesses != ocnp->line_thicknesses)
				copy_l_thicknesses = True;
			if (cnp->llabel_colors != ocnp->llabel_colors)
				copy_ll_font_colors = True;
			if (cnp->llabel_strings != ocnp->llabel_strings)
				copy_ll_strings = True;
			if (use_line_label_strings &&
			    ((cnp->llabel_strings != ocnp->llabel_strings) ||
			     (cnp->explicit_lgnd_labels_on 
			     != ocnp->explicit_lgnd_labels_on)))
				copy_l_labels = True;
		}
		cixp = NULL;
		if (copy_l_colors) {
			if (cnp->lgnd_l_colors->my_data)
				NhlFree(cnp->lgnd_l_colors->data);
			cixp = (NhlColorIndex *) 
				NhlMalloc(sizeof(NhlColorIndex) * 
					  cnp->lgnd_line_count);
			c_cixp = (NhlColorIndex *) cnp->line_colors->data;
			cnp->lgnd_l_colors->num_elements = 
				cnp->lgnd_line_count;
			cnp->lgnd_l_colors->data = (NhlPointer) cixp;
			cnp->lgnd_l_colors->my_data = True;
			ocnp->lgnd_l_colors = NULL;
		}
		dixp = NULL;
		if (copy_l_dash_pats) {
			if (cnp->lgnd_l_dash_pats->my_data)
				NhlFree(cnp->lgnd_l_dash_pats->data);
			dixp = (NhlDashIndex *) 
				NhlMalloc(sizeof(NhlDashIndex) * 
					  cnp->lgnd_line_count);
			c_dixp = (NhlDashIndex *) 
				cnp->line_dash_patterns->data;
			cnp->lgnd_l_dash_pats->num_elements = 
				cnp->lgnd_line_count;
			cnp->lgnd_l_dash_pats->data = (NhlPointer) dixp;
			cnp->lgnd_l_dash_pats->my_data = True;
			ocnp->lgnd_l_dash_pats = NULL;
		}
		thkp = NULL;
		if (copy_l_thicknesses) {
			if (cnp->lgnd_l_thicknesses->my_data)
				NhlFree(cnp->lgnd_l_thicknesses->data);
			thkp = (float *) 
			       NhlMalloc(sizeof(float) * cnp->lgnd_line_count);
			c_thkp = (float *) cnp->line_thicknesses->data;
			cnp->lgnd_l_thicknesses->num_elements = 
				cnp->lgnd_line_count;
			cnp->lgnd_l_thicknesses->data = (NhlPointer) thkp;
			cnp->lgnd_l_thicknesses->my_data = True;
			ocnp->lgnd_l_thicknesses = NULL;
		}
		ll_cixp = NULL;
		if (copy_ll_font_colors) {
			if (cnp->lgnd_ll_font_colors->my_data)
				NhlFree(cnp->lgnd_ll_font_colors->data);
			ll_cixp = (NhlColorIndex *) 
				NhlMalloc(sizeof(NhlColorIndex) * 
					  cnp->lgnd_line_count);
			c_ll_cixp = (NhlColorIndex *) cnp->llabel_colors->data;
			cnp->lgnd_ll_font_colors->num_elements = 
				cnp->lgnd_line_count;
			cnp->lgnd_ll_font_colors->data = (NhlPointer) ll_cixp;
			cnp->lgnd_ll_font_colors->my_data = True;
			ocnp->lgnd_ll_font_colors = NULL;
		}
		ll_sp = NULL;
		if (copy_ll_strings) {
			NhlFreeGenArray(cnp->lgnd_ll_strings);
			cnp->lgnd_ll_strings = 
				_NhlCopyGenArray(cnp->llabel_strings,False);
			ll_sp = (NhlString *) 
			   NhlMalloc(sizeof(NhlString) * cnp->lgnd_line_count);
			c_ll_sp = (NhlString *) cnp->llabel_strings->data;
			cnp->lgnd_ll_strings->num_elements = 
				cnp->lgnd_line_count;
			cnp->lgnd_ll_strings->data = (NhlPointer) ll_sp;
			cnp->lgnd_ll_strings->my_data = True;
			ocnp->lgnd_ll_strings = NULL;
		}
		labels = NULL;
		if (copy_l_labels) {
			NhlFreeGenArray(cnp->lgnd_labels);
			cnp->lgnd_labels = 
				_NhlCopyGenArray(cnp->llabel_strings,False);
			labels = (NhlString *) 
			   NhlMalloc(sizeof(NhlString) * cnp->lgnd_line_count);
			c_labels = (NhlString *) cnp->llabel_strings->data;
			cnp->lgnd_labels->num_elements = cnp->lgnd_line_count;
			cnp->lgnd_labels->data = (NhlPointer) labels;
			cnp->lgnd_labels->my_data = True;
			ocnp->lgnd_labels = NULL;
		}
		for (i = 0, count = 0; i < cnp->level_count; i++) {
			if (flags[i] == NhlLINEONLY ||
			    flags[i] == NhlLINEANDLABEL) {
				if (cixp) cixp[count] = c_cixp[i];
				if (dixp) dixp[count] = c_dixp[i];
				if (thkp) thkp[count] = c_thkp[i];
				if (ll_cixp) ll_cixp[count] = c_ll_cixp[i];
				if (ll_sp && flags[i] == NhlLINEANDLABEL) {
					ll_sp[count] = 
					   NhlMalloc(strlen(c_ll_sp[i]) + 1);
					strcpy(ll_sp[count],c_ll_sp[i]);
				}
				else if (ll_sp) {
					ll_sp[count] = NhlMalloc(1);
					strcpy(ll_sp[count],cnEmptyString);
				}
				if (labels) {
					labels[count] = 
					   NhlMalloc(strlen(c_labels[i]) + 1);
					strcpy(labels[count],c_labels[i]);
				}
				count++;
			}
		}
	}
	else if (! init) {
		if (cnp->line_colors != ocnp->line_colors) {
			NhlFreeGenArray(cnp->lgnd_l_colors);
			cnp->lgnd_l_colors = 
				_NhlCopyGenArray(cnp->line_colors,False);
			ocnp->lgnd_l_colors = NULL;
		}
		if (cnp->line_dash_patterns != ocnp->line_dash_patterns) {
			NhlFreeGenArray(cnp->lgnd_l_dash_pats);
			cnp->lgnd_l_dash_pats = 
			    _NhlCopyGenArray(cnp->line_dash_patterns,False);
			ocnp->lgnd_l_dash_pats = NULL;
		}
		if (cnp->line_thicknesses != ocnp->line_thicknesses) {
			NhlFreeGenArray(cnp->lgnd_l_thicknesses);
			cnp->lgnd_l_thicknesses = 
				_NhlCopyGenArray(cnp->line_thicknesses,False);
			ocnp->lgnd_l_thicknesses = NULL;
		}
		if (cnp->llabel_colors != ocnp->llabel_colors) {
			NhlFreeGenArray(cnp->lgnd_ll_font_colors);
			cnp->lgnd_ll_font_colors =
				_NhlCopyGenArray(cnp->llabel_colors,False);
			ocnp->lgnd_ll_font_colors = NULL;
		}
		if (cnp->ll_strings != ocnp->ll_strings) {
			NhlFreeGenArray(cnp->lgnd_ll_strings);
			cnp->lgnd_ll_strings =
				_NhlCopyGenArray(cnp->ll_strings,False);
			ocnp->lgnd_ll_strings = NULL;
		}
		if (use_line_label_strings &&
		    ((cnp->llabel_strings != ocnp->llabel_strings) ||
		     (cnp->explicit_lgnd_labels_on
		      != ocnp->explicit_lgnd_labels_on))) {
			NhlFreeGenArray(cnp->lgnd_labels);
			cnp->lgnd_labels = 
				_NhlCopyGenArray(cnp->llabel_strings,False);
			ocnp->lgnd_labels = NULL;
		}
	}

	if (init || set_all) {
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgItemCount,cnp->lgnd_line_count);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoDashIndex,cnp->mono_line_dash_pattern);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgDashIndex,cnp->line_dash_pattern);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgDashIndexes,cnp->lgnd_l_dash_pats);
		if (! cnp->line_lbls.on)
			do_it = False;
		else
			do_it = cnp->draw_lgnd_line_lbls;
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineLabelsOn,do_it);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLabelStrings,cnp->lgnd_labels);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLabelFuncCode,cnp->lgnd_func_code);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoLineLabelFontColor,
			   cnp->line_lbls.mono_color);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontColor,cnp->line_lbls.color);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontColors,cnp->lgnd_ll_font_colors);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoLineColor,cnp->mono_line_color);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineColor,cnp->line_color);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineColors,cnp->lgnd_l_colors);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoLineThickness,cnp->mono_line_thickness);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineThicknessF,cnp->line_thickness);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineThicknesses,cnp->lgnd_l_thicknesses);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelStrings,cnp->lgnd_ll_strings);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoLineLabelFontHeight,True);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontHeightF,cnp->line_lbls.height);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineDashSegLenF,cnp->line_dash_seglen);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFont,cnp->line_lbls.font);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontAspectF,cnp->line_lbls.aspect);
		NhlSetSArg(&sargs[(*nargs)++],
		      NhlNlgLineLabelFontThicknessF,cnp->line_lbls.thickness);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontQuality,cnp->line_lbls.quality);
		NhlSetSArg(&sargs[(*nargs)++],
		   NhlNlgLineLabelConstantSpacingF,cnp->line_lbls.cspacing);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFuncCode,cnp->line_lbls.fcode[0]);
		return ret;
	}

	if (cnp->lgnd_line_count != ocnp->lgnd_line_count)
		NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgItemCount,cnp->lgnd_line_count);
	if (cnp->mono_line_dash_pattern != ocnp->mono_line_dash_pattern)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoDashIndex,cnp->mono_line_dash_pattern);
	if (cnp->lgnd_l_dash_pats != ocnp->lgnd_l_dash_pats)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgDashIndexes,cnp->lgnd_l_dash_pats);
	if (cnp->line_dash_pattern != ocnp->line_dash_pattern)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgDashIndex,cnp->line_dash_pattern);
	if (cnp->lgnd_labels != ocnp->lgnd_labels)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLabelStrings,cnp->lgnd_labels);
	if (cnp->lgnd_func_code != ocnp->lgnd_func_code)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLabelFuncCode,cnp->lgnd_func_code);
	if (cnp->mono_line_color != ocnp->mono_line_color)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoLineColor,cnp->mono_line_color);
	if (cnp->line_color != ocnp->line_color)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineColor,cnp->line_color);
	if (cnp->lgnd_l_colors != ocnp->lgnd_l_colors)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineColors,cnp->lgnd_l_colors);
	if (cnp->mono_line_thickness != ocnp->mono_line_thickness)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoLineThickness,cnp->mono_line_thickness);
	if (cnp->line_thickness != ocnp->line_thickness)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineThicknessF,cnp->line_thickness);
	if (cnp->lgnd_l_thicknesses != ocnp->lgnd_l_thicknesses)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineThicknesses,cnp->lgnd_l_thicknesses);
	if (cnp->line_lbls.mono_color != ocnp->line_lbls.mono_color)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoLineLabelFontColor,
			   cnp->line_lbls.mono_color);
	if (cnp->line_lbls.color != ocnp->line_lbls.color)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontColor,cnp->line_lbls.color);
	if (cnp->lgnd_ll_font_colors != ocnp->lgnd_ll_font_colors)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontColors,cnp->lgnd_ll_font_colors);
	if (cnp->draw_lgnd_line_lbls != ocnp->draw_lgnd_line_lbls ||
	    cnp->line_lbls.on != ocnp->line_lbls.on) {
		if (! cnp->line_lbls.on)
			do_it = False;
		else
			do_it = cnp->draw_lgnd_line_lbls;
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineLabelsOn,do_it);
	}
	if (cnp->lgnd_ll_strings != ocnp->lgnd_ll_strings)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelStrings,cnp->lgnd_ll_strings);
	if (cnp->line_lbls.height != ocnp->line_lbls.height)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontHeightF,cnp->line_lbls.height);
	if (cnp->line_dash_seglen != ocnp->line_dash_seglen)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineDashSegLenF,cnp->line_dash_seglen);
	if (cnp->line_lbls.font != ocnp->line_lbls.font)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFont,cnp->line_lbls.font);
	if (cnp->line_lbls.aspect != ocnp->line_lbls.aspect)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontAspectF,cnp->line_lbls.aspect);
	if (cnp->line_lbls.thickness != ocnp->line_lbls.thickness)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontThicknessF,
			   cnp->line_lbls.thickness);
	if (cnp->line_lbls.quality != ocnp->line_lbls.quality)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFontQuality,cnp->line_lbls.quality);
	if (cnp->line_lbls.cspacing != ocnp->line_lbls.cspacing)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelConstantSpacingF,
			   cnp->line_lbls.cspacing);
	if (cnp->line_lbls.fcode[0] != ocnp->line_lbls.fcode[0])
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLineLabelFuncCode,cnp->line_lbls.fcode[0]);

	return ret;
}


/*
 * Function:	ManageLabelBar
 *
 * Description: If the ContourPlot object has an overlay object attached, and
 *		the LabelBar is activated, manages the LabelBar resources 
 *		relevant to the ContourPlot object.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageLabelBar
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew,cnold, init, sargs, nargs)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);
	NhlBoolean		copy_line_label_strings = False;
	NhlBoolean		set_all = False;

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";
        
 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (cnp->display_labelbar == NhlNOCREATE) {
                if (init || ocnp->display_labelbar == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        cnp->display_labelbar = NhlNEVER;
        }

	if (init || 
	    cnp->display_labelbar != ocnp->display_labelbar ||
	    cnp->const_field != ocnp->const_field ||
	    cnp->data_init != ocnp->data_init) {

		if ( cnp->const_field) {
			e_text = "%s: constant field: turning Labelbar off";
			NhlPError(NhlINFO,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlINFO);
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmLabelBarDisplayMode,NhlNEVER);
		}
		else {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmLabelBarDisplayMode,
				   cnp->display_labelbar);
			if (init || cnp->const_field != ocnp->const_field) 
				set_all = True;
		}
	}

	if (cnp->const_field) return ret;

	if (! cnp->explicit_lbar_labels_on) {
		cnp->lbar_labels_set = False;
		if (init || set_all ||
		    cnp->llabel_strings != ocnp->llabel_strings ||
		    cnp->lbar_end_labels_on != ocnp->lbar_end_labels_on ||
		    cnp->explicit_lbar_labels_on 
		    			!= ocnp->explicit_lbar_labels_on) {
			copy_line_label_strings = True;
		}
		if (cnp->lbar_end_labels_on)
			cnp->lbar_alignment = NhlEXTERNALEDGES;
		else
			cnp->lbar_alignment = NhlINTERIOREDGES;
	}
	else {
		if (cnp->lbar_labels_res_set) {
			NhlGenArray ga;
			if (cnp->lbar_labels != NULL) 
				NhlFreeGenArray(cnp->lbar_labels);

			if ((ga = _NhlCopyGenArray(cnp->lbar_labels_res,
						   True)) == NULL) {
				e_text = "%s: error copying GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			cnp->lbar_labels = ga;
			ocnp->lbar_labels = NULL;
		}
		else if (! cnp->lbar_labels_set) {
			copy_line_label_strings = True;
			if (cnp->lbar_end_labels_on)
				cnp->lbar_alignment = NhlEXTERNALEDGES;
			else
				cnp->lbar_alignment = NhlINTERIOREDGES;
		}
		cnp->lbar_labels_set = True;
	}

	if (copy_line_label_strings) {
		NhlGenArray ga;

		cnp->lbar_func_code = cnp->line_lbls.fcode[0];
		if (cnp->lbar_labels != NULL) 
			NhlFreeGenArray(cnp->lbar_labels);
		if (! cnp->lbar_end_labels_on) {
			if ((ga = _NhlCopyGenArray(cnp->llabel_strings,
						   True)) == NULL) {
				e_text = "%s: error copying GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
		}
		else {
			NhlString *to_sp, *from_sp;
			NhlString s;
			int i, count;
			from_sp = (NhlString *) cnp->llabel_strings->data;
			count = cnp->llabel_strings->num_elements + 2;
			to_sp = NhlMalloc(sizeof(NhlString) * count);
			if (to_sp == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			s = ContourPlotFormat(cnp,cnDATAMINVAL,
					      &cnp->line_lbls.format,
					      cnp->lbar_func_code,
					      entry_name);
			if (s == NULL) return NhlFATAL;
			to_sp[0] = NhlMalloc(strlen(s) + 1);
			if (to_sp[0] == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			strcpy(to_sp[0],s);
			for (i = 1; i < count - 1; i++) {
				to_sp[i] = NhlMalloc(strlen(from_sp[i-1]) + 1);
				if (to_sp[i] == NULL) {
					e_text = 
					"%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,
					       NhlEUNKNOWN,e_text,entry_name);
					return NhlFATAL;
				}
				strcpy(to_sp[i],from_sp[i-1]);
			}
			s = ContourPlotFormat(cnp,cnDATAMAXVAL,
					      &cnp->line_lbls.format,
					      cnp->lbar_func_code,
					      entry_name);
			if (s == NULL) return NhlFATAL;
			to_sp[count - 1] = NhlMalloc(strlen(s) + 1);
			if (to_sp[count - 1] == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			strcpy(to_sp[count - 1],s);
			ga = NhlCreateGenArray((NhlPointer)to_sp,NhlTString,
					       sizeof(NhlString),1,&count);
			if (ga == NULL) {
				e_text = "%s: error creating GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
		}
		cnp->lbar_labels = ga;
		ocnp->lbar_labels = NULL;
	}

	if (init || set_all) {

		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbBoxCount,cnp->fill_count);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelAlignment,cnp->lbar_alignment);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelStrings,cnp->lbar_labels);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelFuncCode,cnp->lbar_func_code);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillColor,cnp->mono_fill_color);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillColor,cnp->fill_color);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillColors,cnp->fill_colors);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillPattern,cnp->mono_fill_pattern);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillPattern,cnp->fill_pattern);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillPatterns,cnp->fill_patterns);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillScale,cnp->mono_fill_scale);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillScaleF,cnp->fill_scale);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillScales,cnp->fill_scales);
		return ret;
	}

	if (cnp->fill_count != ocnp->fill_count)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbBoxCount,cnp->fill_count);
	if (cnp->lbar_alignment != ocnp->lbar_alignment)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelAlignment,cnp->lbar_alignment);
	if (cnp->lbar_labels != ocnp->lbar_labels)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelStrings,cnp->lbar_labels);
	if (cnp->lbar_func_code != ocnp->lbar_func_code)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelFuncCode,cnp->lbar_func_code);
	if (cnp->mono_fill_color != ocnp->mono_fill_color)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillColor,cnp->mono_fill_color);
	if (cnp->fill_color != ocnp->fill_color)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillColor,cnp->fill_color);
	if (cnp->fill_colors != ocnp->fill_colors)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillColors,cnp->fill_colors);
	if (cnp->mono_fill_pattern != ocnp->mono_fill_pattern)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillPattern,cnp->mono_fill_pattern);
	if (cnp->fill_pattern != ocnp->fill_pattern)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillPattern,cnp->fill_pattern);
	if (cnp->fill_patterns != ocnp->fill_patterns)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillPatterns,cnp->fill_patterns);
	if (cnp->mono_fill_scale != ocnp->mono_fill_scale)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillScale,cnp->mono_fill_scale);
	if (cnp->fill_scale != ocnp->fill_scale)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillScaleF,cnp->fill_scale);
	if (cnp->fill_scales != ocnp->fill_scales)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillScales,cnp->fill_scales);

	return ret;
}

/*
 * Function:	SetLabelString
 *
 * Description: Creates a copy of a label string when required; does 
 *	nothing if the destination and source strings are the same.
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes SetLabelString
#if	NhlNeedProto
(
	NhlString *dest_str,
	NhlString source_str,
	NhlString def_str,
	NhlString entry_name
)
#else 
(dest_str,source_str,def_str,entry_name)
	NhlString *dest_str;
	NhlString source_str;
	NhlString def_str;
	NhlString entry_name;
#endif
{
	char		*e_text;
	char		*lstring;

	if (*dest_str == NULL ||
	    *dest_str != source_str) {
		int strsize = source_str == NULL ? 
			strlen(def_str) + 1 : strlen(source_str) + 1;

		if ((lstring = NhlMalloc(strsize)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if (source_str == NULL)
			strcpy(lstring,def_str);
		else
			strcpy(lstring,source_str);
		if (*dest_str != NULL) 
			NhlFree(*dest_str);
		*dest_str = lstring;
	}
	return NhlNOERROR;
}
/*
 * Function:	ManageInfoLabel
 *
 * Description: If the information label label is 
 *		activated text items to store the strings are created.
 *		If there is an PlotManager an AnnoManager is 
 *		created so that the plotManager object can manage the
 *		annotations.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageInfoLabel
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew,cnold, init, sargs, nargs)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	char			*e_text;
	char			*entry_name;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);
	NhlcnLabelAttrs		*ilp = &cnp->info_lbl;
	NhlcnLabelAttrs		*oilp = &ocnp->info_lbl;
	NhlString		lstring;
	NhlBoolean		text_changed,pos_changed = False;
	NhlSArg			targs[24];
	int			targc = 0;
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;
	

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";


	if (init || ! cnp->info_string ||
	    cnp->info_string != ocnp->info_string) {
		int strsize = cnp->info_string == NULL ? 
			strlen(NhlcnDEF_INFO_LABEL) + 1 : 
				strlen(cnp->info_string) + 1;

		if ((lstring = NhlMalloc(strsize)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if (cnp->info_string == NULL)
			strcpy(lstring,NhlcnDEF_INFO_LABEL);
		else
			strcpy(lstring,cnp->info_string);
		cnp->info_string = lstring;
		if (!init && ocnp->info_string != NULL) 
			NhlFree(ocnp->info_string);
	}
	if (! ilp->on && (init || ! oilp->on))
		return NhlNOERROR;

	subret = ReplaceSubstitutionChars(cnp,ocnp,init,_cnINFO,
					  &text_changed,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;


	if (init || cnp->info_lbl_rec.id == NhlNULLOBJID) {
		NhlSetSArg(&targs[(targc)++],NhlNtxString,
			   (NhlString)ilp->text);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontHeightF,ilp->height);
		NhlSetSArg(&targs[(targc)++],NhlNtxDirection,ilp->direction);
		NhlSetSArg(&targs[(targc)++],NhlNtxAngleF,ilp->angle);
		NhlSetSArg(&targs[(targc)++],NhlNtxFont,ilp->font);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontColor,ilp->color);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontAspectF,ilp->aspect);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxFontThicknessF,ilp->thickness);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxConstantSpacingF,ilp->cspacing);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontQuality,ilp->quality);
		NhlSetSArg(&targs[(targc)++],NhlNtxFuncCode,ilp->fcode[0]);

		NhlSetSArg(&targs[(targc)++],NhlNtxPerimOn,ilp->perim_on);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimColor,ilp->perim_lcolor);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimThicknessF,ilp->perim_lthick);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimSpaceF,ilp->perim_space);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxBackgroundFillColor,ilp->back_color);
		NhlSetSArg(&targs[(targc)++],
			   NhlNvpUseSegments,cnnew->view.use_segments);

		sprintf(buffer,"%s",cnnew->base.name);
		strcat(buffer,".InfoLabel");
		subret = NhlALCreate(&tmpid,buffer,NhltextItemClass,
				     cnnew->base.id,targs,targc);
		
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error creating information label";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		cnp->info_lbl_rec.id = tmpid;
		if (tfp->overlay_status == _tfNotInOverlay) {
			targc = 0;
			/* go on so that text position can be set */
		}
		else {
			subret = ManageAnnotation(cnnew,ocnp,init,_cnINFO);
			return MIN(ret,subret);
		}
	}

	if (tfp->overlay_status == _tfNotInOverlay) {
		NhlContourPlotLayerPart *op;
		op = init ? NULL : ocnp;
		subret = SetTextPosition(cnnew,op,
					 _cnINFO,&pos_changed,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
	}
	if (pos_changed) {
		NhlSetSArg(&targs[(targc)++],NhlNtxPosXF,ilp->x_pos);
		NhlSetSArg(&targs[(targc)++],NhlNtxPosYF,ilp->y_pos);
		NhlSetSArg(&targs[(targc)++],NhlNtxJust,ilp->just);
	}
	if (! init) {
		if (init || text_changed)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxString,(NhlString)ilp->text);
		if (init || ilp->height != oilp->height)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontHeightF,ilp->height);
		if (init || ilp->direction != oilp->direction)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxDirection,ilp->direction);
		if (init || ilp->angle != oilp->angle)
			NhlSetSArg(&targs[(targc)++],NhlNtxAngleF,ilp->angle);
		if (init || ilp->font != oilp->font)
			NhlSetSArg(&targs[(targc)++],NhlNtxFont,ilp->font);
		if (init || ilp->color != oilp->color)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontColor,ilp->color);
		if (init || ilp->aspect != oilp->aspect)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontAspectF,ilp->aspect);
		if (init || ilp->thickness != oilp->thickness)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontThicknessF,ilp->thickness);
		if (init || ilp->cspacing != oilp->cspacing)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxConstantSpacingF,ilp->cspacing);
		if (init || ilp->quality != oilp->quality)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontQuality,ilp->quality);
		if (init || ilp->fcode[0] != oilp->fcode[0])
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFuncCode,ilp->fcode[0]);
		
		if (init || ilp->perim_on != oilp->perim_on)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimOn,ilp->perim_on);
		if (init || ilp->perim_lcolor != oilp->perim_lcolor)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimColor,ilp->perim_lcolor);
		if (init || ilp->perim_lthick != oilp->perim_lthick)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimThicknessF,ilp->perim_lthick);
		if (init || ilp->perim_space != oilp->perim_space)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimSpaceF,ilp->perim_space);
		if (init || ilp->back_color != oilp->back_color)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxBackgroundFillColor,ilp->back_color);
		if (init ||
                    cnnew->view.use_segments != cnold->view.use_segments)
			NhlSetSArg(&targs[(targc)++],
				   NhlNvpUseSegments,cnnew->view.use_segments);
	}
	subret = NhlALSetValues(cnp->info_lbl_rec.id,targs,targc);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: error setting values for information label";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (tfp->overlay_status != _tfNotInOverlay) {
		subret = ManageAnnotation(cnnew,ocnp,init,_cnINFO);
		ret = MIN(ret,subret);
	}
	return ret;
}


/*
 * Function:	ManageConstFLabel
 *
 * Description: If a constant field is detected a constant field label
 *		is created, or turned on.
 *		If there is an PlotManager an AnnoManager object is 
 *		created so that the overlay object can manage the
 *		annotations.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageConstFLabel
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew,
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew,cnold, init, sargs, nargs)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	char			*e_text;
	char			*entry_name;
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cnold->contourplot);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);
	NhlcnLabelAttrs		*cflp = &cnp->constf_lbl;
	NhlcnLabelAttrs		*ocflp = &ocnp->constf_lbl;
	NhlString		lstring, tstring;
	NhlBoolean		text_changed = False, pos_changed = False;
	NhlSArg			targs[24];
	int			targc = 0;
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";

/*
 * The constant field label resource must be turned on AND a constant
 * field condition must exist for the constant field annotation  
 * to be displayed.
 */


	if (init || ! cnp->constf_string ||
	    cnp->constf_string != ocnp->constf_string) {
		text_changed = True;
		tstring = cnp->constf_string == NULL ?
			NhlcnDEF_CONSTF_LABEL : cnp->constf_string; 
		if ((lstring = NhlMalloc(strlen(tstring) + 1)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(lstring,tstring);
		cnp->constf_string = lstring;
	}
	if (init || ! cnp->no_data_string || 
	    cnp->no_data_string != ocnp->no_data_string) {
		text_changed = True;
		tstring = cnp->no_data_string == NULL ?
			NhlcnDEF_NODATA_LABEL : cnp->no_data_string; 
		if ((lstring = NhlMalloc(strlen(tstring) + 1)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(lstring,tstring);
		cnp->no_data_string = lstring;
	}
	if (! cnp->data_init)
		cnp->constf_no_data_string = cnp->no_data_string;
	else
		cnp->constf_no_data_string = cnp->constf_string;
	if (text_changed)
		ocnp->constf_no_data_string = NULL;

	cnp->display_constf_no_data = 
		(cflp->on && cnp->const_field) || 
			(cnp->no_data_label_on && ! cnp->data_init);

	subret = ReplaceSubstitutionChars(cnp,ocnp,init,_cnCONSTF,
				  &text_changed,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

	if (init || cnp->constf_lbl_rec.id == NhlNULLOBJID) {
		if (pos_changed) {
			NhlSetSArg(&targs[(targc)++],NhlNtxPosXF,cflp->x_pos);
			NhlSetSArg(&targs[(targc)++],NhlNtxPosYF,cflp->y_pos);
			NhlSetSArg(&targs[(targc)++],NhlNtxJust,cflp->just);
		}
		NhlSetSArg(&targs[(targc)++],NhlNtxString,
			   (NhlString)cflp->text);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontHeightF,cflp->height);
		NhlSetSArg(&targs[(targc)++],NhlNtxDirection,cflp->direction);
		NhlSetSArg(&targs[(targc)++],NhlNtxAngleF,cflp->angle);
		NhlSetSArg(&targs[(targc)++],NhlNtxFont,cflp->font);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontColor,cflp->color);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontAspectF,cflp->aspect);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxFontThicknessF,cflp->thickness);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxConstantSpacingF,cflp->cspacing);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontQuality,cflp->quality);
		NhlSetSArg(&targs[(targc)++],NhlNtxFuncCode,cflp->fcode[0]);

		NhlSetSArg(&targs[(targc)++],NhlNtxPerimOn,cflp->perim_on);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimColor,cflp->perim_lcolor);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimThicknessF,cflp->perim_lthick);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimSpaceF,cflp->perim_space);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxBackgroundFillColor,cflp->back_color);

		sprintf(buffer,"%s",cnnew->base.name);
		strcat(buffer,".ConstFLabel");
		subret = NhlALCreate(&tmpid,buffer,NhltextItemClass,
				     cnnew->base.id,targs,targc);
		
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error creating information label";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		cnp->constf_lbl_rec.id = tmpid;

		if (tfp->overlay_status == _tfNotInOverlay) {
			targc = 0; 
			/* go on so text position can be set */
		}
		else {
			subret = ManageAnnotation(cnnew,ocnp,init,_cnCONSTF);
			return MIN(ret,subret);
		}
	}

	if (tfp->overlay_status == _tfNotInOverlay) {
		NhlContourPlotLayerPart *op;
		op = init ? NULL : ocnp;
		subret = SetTextPosition(cnnew,op,_cnCONSTF,
					 &pos_changed,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
	}
	if (pos_changed) {
		NhlSetSArg(&targs[(targc)++],NhlNtxPosXF,cflp->x_pos);
		NhlSetSArg(&targs[(targc)++],NhlNtxPosYF,cflp->y_pos);
		NhlSetSArg(&targs[(targc)++],NhlNtxJust,cflp->just);
	}
	if (! init) {
		if (text_changed)
			NhlSetSArg(&targs[(targc)++],NhlNtxString,
				   (NhlString)cflp->text);
		if (cflp->height != ocflp->height)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontHeightF,cflp->height);
		if (cflp->direction != ocflp->direction)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxDirection,cflp->direction);
		if (cflp->angle != ocflp->angle)
			NhlSetSArg(&targs[(targc)++],NhlNtxAngleF,cflp->angle);
		if (cflp->font != ocflp->font)
			NhlSetSArg(&targs[(targc)++],NhlNtxFont,cflp->font);
		if (cflp->color != ocflp->color)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontColor,cflp->color);
		if (cflp->aspect != ocflp->aspect)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontAspectF,cflp->aspect);
		if (cflp->thickness != ocflp->thickness)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontThicknessF,cflp->thickness);
		if (cflp->cspacing != ocflp->cspacing)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxConstantSpacingF,cflp->cspacing);
		if (cflp->quality != ocflp->quality)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontQuality,cflp->quality);
		if (cflp->fcode[0] != ocflp->fcode[0])
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFuncCode,cflp->fcode[0]);
		
		if (cflp->perim_on != ocflp->perim_on)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimOn,cflp->perim_on);
		if (cflp->perim_lcolor != ocflp->perim_lcolor)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimColor,cflp->perim_lcolor);
		if (cflp->perim_lthick != ocflp->perim_lthick)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimThicknessF,cflp->perim_lthick);
		if (cflp->perim_space != ocflp->perim_space)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimSpaceF,cflp->perim_space);
		if (cflp->back_color != ocflp->back_color)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxBackgroundFillColor,cflp->back_color);
	}
	
	subret = NhlALSetValues(cnp->constf_lbl_rec.id,targs,targc);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
	 e_text = "%s: error setting values for constant field/no data label";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (tfp->overlay_status != _tfNotInOverlay) {
		subret = ManageAnnotation(cnnew,ocnp,init,_cnCONSTF);
		ret = MIN(ret,subret);
	}
	return ret;
}

/*
 * Function:	ManageAnnotation
 *
 * Description: 
 *		
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageAnnotation
#if	NhlNeedProto
(
	NhlContourPlotLayer		cnnew,
	NhlContourPlotLayerPart	*ocnp,
	NhlBoolean		init,
	_cnAnnoType		atype
)
#else 
(cnnew,ocnp,init,atype)
	NhlContourPlotLayer		cnnew;
	NhlContourPlotLayerPart	*ocnp;
	NhlBoolean		init;
	_cnAnnoType		atype;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);
	NhlAnnotationRec	*rec, *orec;
	int			*idp;
	NhlSArg			sargs[16];
	int			nargs = 0;
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";

	if (atype == _cnINFO) {
		rec = &cnp->info_lbl_rec;
		orec = &ocnp->info_lbl_rec;
		idp = &cnp->info_anno_id;
		rec->on = cnp->info_lbl.on 
			&& cnp->data_init && ! cnp->const_field;
	}
	else {
		rec = &cnp->constf_lbl_rec;
		orec = &ocnp->constf_lbl_rec;
		idp = &cnp->constf_anno_id;
		rec->on = cnp->display_constf_no_data;
	}

	if (*idp <= NhlNULLOBJID) {
		NhlSetSArg(&sargs[(nargs)++],NhlNamTrackData,False);
		NhlSetSArg(&sargs[(nargs)++],NhlNamResizeNotify,False);
		NhlSetSArg(&sargs[(nargs)++],NhlNamOn,rec->on);
		NhlSetSArg(&sargs[(nargs)++],NhlNamViewId,rec->id);
		NhlSetSArg(&sargs[(nargs)++],NhlNamZone,rec->zone);
		NhlSetSArg(&sargs[(nargs)++],NhlNamSide,rec->side);
		NhlSetSArg(&sargs[(nargs)++],NhlNamJust,rec->just);
		NhlSetSArg(&sargs[(nargs)++],
			   NhlNamParallelPosF,rec->para_pos);
		NhlSetSArg(&sargs[(nargs)++],
			   NhlNamOrthogonalPosF,rec->ortho_pos);
		sprintf(buffer,"%s",cnnew->base.name);
		strcat(buffer,".AnnoManager");
		subret = NhlALCreate(&tmpid,buffer,NhlannoManagerClass,
				     cnnew->base.id,sargs,nargs);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error creating AnnoManager layer";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		*idp = tmpid;
/*
 * If the ContourPlot plot is an overlay plot base register the AnnoManager
 * with its own base, ensuring that it will always follow the overlay.
 */
		if (tfp->plot_manager_on)
			subret = _NhlRegisterAnnotation(cnp->overlay_object,
							_NhlGetLayer(*idp),
							NULL);
		else 
			subret = _NhlRegisterAnnotation(tfp->overlay_object,
							_NhlGetLayer(*idp),
							NULL);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error registering annotation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		return ret;
	}
	if (rec->on != orec->on) 
		NhlSetSArg(&sargs[(nargs)++],NhlNamOn,rec->on);
	if (rec->zone != orec->zone) 
		NhlSetSArg(&sargs[(nargs)++],NhlNamZone,rec->zone);
	if (rec->side != orec->side) 
		NhlSetSArg(&sargs[(nargs)++],NhlNamSide,rec->side);
	if (rec->just != orec->just) 
		NhlSetSArg(&sargs[(nargs)++],NhlNamJust,rec->just);
	if (rec->para_pos != orec->para_pos) 
		NhlSetSArg(&sargs[(nargs)++],
			   NhlNamParallelPosF,rec->para_pos);
	if (rec->ortho_pos != orec->ortho_pos) 
		NhlSetSArg(&sargs[(nargs)++],
			   NhlNamOrthogonalPosF,rec->ortho_pos);
	
	subret = NhlALSetValues(*idp,sargs,nargs);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: error setting AnnoManager object values";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	return ret;
}

/*
 * Function:	ReplaceSubstitutionChars
 *
 * Description: Replaces the substitution characters in the info and
 *		zero field labels with the correct numerical values.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ReplaceSubstitutionChars
#if	NhlNeedProto
(
	NhlContourPlotLayerPart	*cnp,
	NhlContourPlotLayerPart	*ocnp,
	NhlBoolean		init,
	_cnAnnoType		atype,
	NhlBoolean		*text_changed,
	NhlString		entry_name
)
#else 
(cnp,ocnp,init,atype,text_changed,entry_name)
	NhlContourPlotLayerPart	*cnp;
	NhlContourPlotLayerPart	*ocnp;
	NhlBoolean		init;
	_cnAnnoType		atype;
	NhlBoolean		*text_changed;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	NhlBoolean		done = False;
	char			buffer[256];
	char			*matchp,*subst;

	*text_changed = False;

	switch (atype) {
	case _cnINFO:
		if (! cnp->data_changed && ! init && 
		    (cnp->levels == ocnp->levels) &&
		    (cnp->info_string == ocnp->info_string) &&
		    (cnp->max_data_format.fstring == 
		    ocnp->max_data_format.fstring) &&
		    (cnp->info_lbl.format.fstring == 
		     ocnp->info_lbl.format.fstring) &&
		    (cnp->label_scale_value == ocnp->label_scale_value) &&
		    (cnp->label_scale_factor == ocnp->label_scale_factor)) {
			return NhlNOERROR;
		}
		strcpy(buffer,cnp->info_string);
		while (! done) {
			if ((matchp = strstr(buffer,"$CIU$")) != NULL) {
				subst = ContourPlotFormat(cnp,cnCONINTERVAL,
						      &cnp->info_lbl.format,
						      cnp->info_lbl.fcode[0],
						      entry_name);
				if (subst == NULL) return NhlFATAL;
				Substitute(matchp,5,subst);
			}
			else if ((matchp = strstr(buffer,"$CMN$")) != NULL) {
				subst = ContourPlotFormat(cnp,cnCONMINVAL,
						      &cnp->info_lbl.format,
						      cnp->info_lbl.fcode[0],
						      entry_name);
				if (subst == NULL) return NhlFATAL;
				Substitute(matchp,5,subst);
			}
			else if ((matchp = strstr(buffer,"$CMX$")) != NULL) {
				subst = ContourPlotFormat(cnp,cnCONMAXVAL,
						      &cnp->info_lbl.format,
						      cnp->info_lbl.fcode[0],
						      entry_name);

				if (subst == NULL) return NhlFATAL;
				Substitute(matchp,5,subst);
			}
			else if ((matchp = strstr(buffer,"$SFU$")) != NULL) {
				subst = ContourPlotFormat(cnp,cnSCALEFACTOR,
						      &cnp->info_lbl.format,
						      cnp->info_lbl.fcode[0],
						      entry_name);
				if (subst == NULL) return NhlFATAL;
				Substitute(matchp,5,subst);
			}
			else if ((matchp = strstr(buffer,"$ZMN$")) != NULL) {
				subst = ContourPlotFormat(cnp,cnDATAMINVAL,
						      &cnp->info_lbl.format,
						      cnp->info_lbl.fcode[0],
						      entry_name);
				if (subst == NULL) return NhlFATAL;
				Substitute(matchp,5,subst);
			}
			else if ((matchp = strstr(buffer,"$ZMX$")) != NULL) {
				subst = ContourPlotFormat(cnp,cnDATAMAXVAL,
						      &cnp->info_lbl.format,
						      cnp->info_lbl.fcode[0],
						      entry_name);
				if (subst == NULL) return NhlFATAL;
				Substitute(matchp,5,subst);
			}
			else {
				done = True;
			}
		}
		if (cnp->info_lbl.text != NULL)
			NhlFree(cnp->info_lbl.text);
		if ((cnp->info_lbl.text = 
		     NhlMalloc(strlen(buffer)+1)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy((NhlString)cnp->info_lbl.text,buffer);
		break;
	case _cnCONSTF:
		if (! init && (cnp->zmax == ocnp->zmax) &&
		    (cnp->constf_no_data_string == 
		     ocnp->constf_no_data_string))
			return NhlNOERROR;
		strcpy(buffer,cnp->constf_no_data_string);
		while (! done) {
			if ((matchp = strstr(buffer,"$ZDV$")) != NULL) {
				subst = ContourPlotFormat(cnp,cnCONSTFVAL,
						      &cnp->constf_lbl.format,
						      cnp->constf_lbl.fcode[0],
						      entry_name);
				if (subst == NULL) return NhlFATAL;
				Substitute(matchp,5,subst);
			}
			else {
				done = True;
			}
		}
		if (cnp->constf_lbl.text != NULL)
			NhlFree(cnp->constf_lbl.text);
		if ((cnp->constf_lbl.text = 
		     NhlMalloc(strlen(buffer)+1)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy((NhlString)cnp->constf_lbl.text,buffer);
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	*text_changed = True;

	return ret;
}


/*
 * Function:	Substitute
 *
 * Description: substitutes a string for a Conpack substitution sequence.
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static void Substitute
#if	NhlNeedProto
(
	char		*buf,
	int		replace_count,
	char		*subst
)
#else 
(buf,replace_count,subst)
	char		*buf;
	int		replace_count;
	char		*subst;
#endif
{
	int subst_count,add,buflen;
	char *from, *to;

	buflen = strlen(buf);
	subst_count = strlen(subst);
	if (subst_count - replace_count < 0) {
		for (from = buf+replace_count,to = buf+subst_count; ;
		     to++,from++) { 
			*to = *from;
			if (*from == '\0')
				break;
		}
	}
	else if ((add = subst_count - replace_count) > 0) {
		for (from = buf + buflen,to = buf + buflen + add; 
		     from >= buf + replace_count;)
			*to-- = *from--;
	}
	strncpy(buf,subst,subst_count);
}

/*
 * Function:	SetFormatRec
 *
 * Description: sets up the format record for a label type
 *
 * In Args:	NhlFormatRec *format -> to the format record
 *		NhlString    resource    the format resource - for error.
 *		NhlString    entry_name  the caller
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects: 
 */
static NhlErrorTypes SetFormatRec
#if	NhlNeedProto
(
	NhlFormatRec	*format,
	NhlString	resource,
	NhlString	entry_name
)
#else 
(format,resource,entry_name)
	NhlFormatRec	*format;
	NhlString	resource;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlFormatRec	*frec;


	if (format->fstring == NULL || format->fstring[0] == '\0') {
		e_text = "%s: empty format string supplied for %s; defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,resource);
		ret = NhlWARNING;
		format->fstring = NhlcnDEF_FORMAT;
	}
	if ((frec = _NhlScanFString(format->fstring,entry_name)) == NULL) {
		e_text = "%s: error in format string for %s: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,resource);
		ret = NhlWARNING;
		format->fstring = NhlcnDEF_FORMAT;
		if ((frec = _NhlScanFString(format->fstring,
					    entry_name)) == NULL) {
			e_text = "%s: internal error getting format";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(NhlFATAL);
		}
	}
	memcpy((void *)format,(Const void *)frec,sizeof(NhlFormatRec));

/* 
 * Since at this point the format string itself is not owned by the 
 * ContourPlot object, make a copy.
 */
	if (format->fstring != NULL) {
		char *cp;
		if ((cp = NhlMalloc(strlen(format->fstring)+1)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(cp,format->fstring);
		format->fstring = cp;
	}

	return ret;
}

/*
 * Function:	ContourPlotFormat
 *
 * Description: formats a numeric internal contour value type into a string.
 *		the string is stored in static memory; previous values 
 *		overwritten at each invocation.
 *
 * In Args:	cnp	ContourPlot layer part
 *		type	the type of value requested
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static char *ContourPlotFormat
#if	NhlNeedProto
(
	NhlContourPlotLayerPart	*cnp,
	cnValueType		vtype,
	NhlFormatRec		*format,
	char			func_code,
	NhlString		entry_name
)
#else 
(cnp,vtype,format,func_code,entry_name)
	NhlContourPlotLayerPart	*cnp;
	cnValueType		vtype;
	NhlFormatRec		*format;
	char			func_code;
	NhlString		entry_name;

#endif
{
	char	*cp;
	float  value;
	int    left_sig_digit = format->left_sig_digit;
	NhlffStat left_sig_digit_flag = format->left_sig_digit_flag;

	switch (vtype) {

	case cnCONSTFVAL:
		value = cnp->zmax / cnp->label_scale_factor;
		break;
	case cnCONINTERVAL:
		value = cnp->level_spacing / cnp->label_scale_factor;
		break;
	case cnCONMINVAL:
		value = cnp->min_level_val / cnp->label_scale_factor;
		break;
	case cnCONMAXVAL:
		value = cnp->max_level_val / cnp->label_scale_factor;
		break;
	case cnDATAMINVAL:
		value = cnp->zmin / cnp->label_scale_factor;
		break;
	case cnDATAMAXVAL:
		value = cnp->zmax / cnp->label_scale_factor;
		break;
	case cnSCALEFACTOR:
		format->left_sig_digit_flag = NhlffUNSPECED;
		format->left_sig_digit = -10000;
		value = cnp->label_scale_factor;
		break;
	default:
		value = 1e12;
	}

	cp = _NhlFormatFloat(format,value,NULL,
			     &cnp->max_data_format.sig_digits,
			     &cnp->max_data_format.left_sig_digit,
                             NULL,NULL,NULL,func_code,entry_name);

	format->left_sig_digit_flag = left_sig_digit_flag;
	format->left_sig_digit = left_sig_digit;

	if (cp == NULL) 
		return NULL;
	return cp;
	
}
/*
 * Function:	ConstrainJustification
 *
 * Description: Constrain justification depending on the annotation side;
 *
 * In Args:	NhlAnnoRec	anno_rec - the annotation record
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlJustification
ConstrainJustification
#if	NhlNeedProto
(
	NhlAnnotationRec	*anno_rec
)
#else
(anno_rec)
	NhlAnnotationRec	*anno_rec;
#endif
{
	switch (anno_rec->side) {
	case NhlBOTTOM:
		switch (anno_rec->just) {
		case NhlTOPLEFT:
		case NhlCENTERLEFT:
		case NhlBOTTOMLEFT:
			return NhlTOPLEFT;
		case NhlTOPCENTER:
		case NhlCENTERCENTER:
		case NhlBOTTOMCENTER:
			return NhlTOPCENTER;
		case NhlTOPRIGHT:
		case NhlCENTERRIGHT:
		case NhlBOTTOMRIGHT:
			return NhlTOPRIGHT;
		}
	case NhlTOP:
		switch (anno_rec->just) {
		case NhlTOPLEFT:
		case NhlCENTERLEFT:
		case NhlBOTTOMLEFT:
			return NhlBOTTOMLEFT;
		case NhlTOPCENTER:
		case NhlCENTERCENTER:
		case NhlBOTTOMCENTER:
			return NhlBOTTOMCENTER;
		case NhlTOPRIGHT:
		case NhlCENTERRIGHT:
		case NhlBOTTOMRIGHT:
			return NhlBOTTOMRIGHT;
		}
	case NhlLEFT:
		switch (anno_rec->just) {
		case NhlTOPLEFT:
		case NhlTOPCENTER:
		case NhlTOPRIGHT:
			return NhlTOPRIGHT;
		case NhlCENTERLEFT:
		case NhlCENTERCENTER:
		case NhlCENTERRIGHT:
			return NhlCENTERRIGHT;
		case NhlBOTTOMLEFT:
		case NhlBOTTOMCENTER:
		case NhlBOTTOMRIGHT:
			return NhlBOTTOMRIGHT;
		}
	case NhlRIGHT:
		switch (anno_rec->just) {
		case NhlTOPLEFT:
		case NhlTOPCENTER:
		case NhlTOPRIGHT:
			return NhlTOPLEFT;
		case NhlCENTERLEFT:
		case NhlCENTERCENTER:
		case NhlCENTERRIGHT:
			return NhlCENTERLEFT;
		case NhlBOTTOMLEFT:
		case NhlBOTTOMCENTER:
		case NhlBOTTOMRIGHT:
			return NhlBOTTOMLEFT;
		}
	case NhlCENTER:
	default:
		break;
	}
	return (NhlJustification) NhlFATAL;

}

/*
 * Function:	SetTextPosition
 *
 * Description: Sets the text positional attribute fields in label 
 *		annotation strings when they are not members of an overlay, 
 *		and therefore have no AnnoManager.
 *
 * In Args:	
 *		
 *		
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
static NhlErrorTypes SetTextPosition
#if	NhlNeedProto
(
	NhlContourPlotLayer		cnnew,
	NhlContourPlotLayerPart	*ocnp,
	_cnAnnoType		atype,
	NhlBoolean		*pos_changed,
	NhlString		entry_name
)
#else 
(cnnew,ocnp,atype,pos_changed,entry_name)
	NhlContourPlotLayer		cnnew;
	NhlContourPlotLayerPart	*ocnp;
	_cnAnnoType		atype;
	NhlBoolean		*pos_changed;
	NhlString		entry_name;
#endif
{
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cnnew->contourplot);
	NhlAnnotationRec	*anno_rec;
	NhlcnLabelAttrs		*lap;
	NhlcnLabelAttrs		*olap;
	float			width_vp, height_vp;
	float			x_start, y_start;
	int			sign;

	if (atype == _cnINFO) {
		anno_rec = &cnp->info_lbl_rec;
		lap = &cnp->info_lbl;
		olap = ocnp == NULL ? NULL : &ocnp->info_lbl;
	}
	else {
		anno_rec = &cnp->constf_lbl_rec;
		lap = &cnp->constf_lbl;
		olap = ocnp == NULL ? NULL : &ocnp->constf_lbl;
	}
	subret = NhlVAGetValues(anno_rec->id,
				NhlNvpWidthF,&width_vp,
				NhlNvpHeightF,&height_vp,
				NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error getting embedded annotation values";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		return ret;
	}

	x_start = anno_rec->zone != 0 ? cnnew->view.x :
		cnnew->view.x + 0.5 * cnnew->view.width; 
	y_start = anno_rec->zone != 0 ? cnnew->view.y - cnnew->view.height :
		cnnew->view.y - 0.5 * cnnew->view.height;
	sign = anno_rec->zone == 1 ? 1.0 : - 1.0;


	switch (anno_rec->side) {
	case NhlBOTTOM:
		lap->x_pos = x_start + anno_rec->para_pos * cnnew->view.width;
		lap->y_pos = y_start - 
			sign * anno_rec->ortho_pos * cnnew->view.height;
		break;
	case NhlTOP:
		lap->x_pos = x_start + anno_rec->para_pos * cnnew->view.width;
		lap->y_pos = y_start + 
			sign * anno_rec->ortho_pos * cnnew->view.height;
		break;
	case NhlLEFT:
		lap->x_pos = x_start - 
			sign * anno_rec->ortho_pos * cnnew->view.width;
		lap->y_pos = y_start +
			anno_rec->para_pos * cnnew->view.height;
		break;
	case NhlRIGHT:
		lap->x_pos = x_start + cnnew->view.width + 
			sign * anno_rec->ortho_pos * cnnew->view.width;
		lap->y_pos = y_start +
			anno_rec->para_pos * cnnew->view.height;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlBOTTOMRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}

	if (anno_rec->zone > 0)
		lap->just = ConstrainJustification(anno_rec);
	else 
		lap->just = anno_rec->just;

	if (olap == NULL ||
	    lap->x_pos != olap->x_pos ||
	    lap->y_pos != olap->y_pos ||
	    lap->just != olap->just) {
		*pos_changed = True;
	}
	return ret;
}

/*
 * Function:  ManageData
 *
 * Description: Handles updating of data dependent resources
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
static NhlErrorTypes    ManageData
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnnew, 
	NhlContourPlotLayer	cnold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else
(cnnew,cnold,init,args,num_args)
	NhlContourPlotLayer	cnnew;
	NhlContourPlotLayer	cnold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &cnnew->contourplot;
	NhlContourPlotLayerPart	*ocnp = &cnold->contourplot;
	NhlScalarFieldFloatLayer	sfl;
	_NhlDataNodePtr			*dlist = NULL;
	NhlBoolean			new;
	int				ndata = 0;

	if (! cnp->data_changed && 
	    (cnp->scalar_field_data == ocnp->scalar_field_data)) 
		return NhlNOERROR;

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";

	if (cnp->scalar_field_data != NULL)
		ndata = _NhlGetDataInfo(cnp->scalar_field_data,&dlist);
	if (ndata <= 0) {
		if (cnp->min_level_set)
			cnp->zmin = cnp->min_level_val;
		else
			cnp->zmin = 0.0;
		if (cnp->max_level_set) 
			cnp->zmax = cnp->max_level_val;
		else
			cnp->zmax = MAX(1.0,fabs(cnp->zmin*10.0));
		cnp->data_init = False;
		cnp->sfp = NULL;
		return NhlNOERROR;
	}
	else if (ndata != 1) {
		cnp->data_init = False;
		e_text = "%s: internal error retrieving data info";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (cnp->sfp != NULL && cnp->osfp == NULL) {
		cnp->osfp = NhlMalloc(sizeof(NhlScalarFieldFloatLayerPart));
		if (cnp->osfp == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	if (cnp->sfp != NULL) {
		memcpy(cnp->osfp,
		       cnp->sfp,sizeof(NhlScalarFieldFloatLayerPart));	
	}

 	sfl = (NhlScalarFieldFloatLayer) _NhlGetDataSet(dlist[0],&new);
	if (sfl == NULL) {
		cnp->data_init = False;
		e_text = "%s: internal error retrieving data set";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	
	cnp->sfp = (NhlScalarFieldFloatLayerPart *) &sfl->sfieldfloat;

	if (cnp->sfp->missing_value_set && 
	    cnp->sfp->data_max == cnp->sfp->missing_value) {
		e_text = 
          "%s: no valid values in scalar field; ContourPlot not possible";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		cnp->data_init = False;
		if (cnp->min_level_set)
			cnp->zmin = cnp->min_level_val;
		else
			cnp->zmin = 0.0;
		if (cnp->max_level_set) 
			cnp->zmax = cnp->max_level_val;
		else
			cnp->zmax = MAX(1.0,fabs(cnp->zmin*10.0));
		return MIN(NhlWARNING,ret);
	}
        if (cnp->sfp->missing_value_set && cnp->sfp->missing_value == 0.0) {
		e_text =
  "%s: 0.0 not currently supported as a missing value; expect inaccurate plot";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
        }
        
	cnp->zmin = cnp->sfp->data_min;
	cnp->zmax = cnp->sfp->data_max;
	cnp->const_field = _NhlCmpFAny(cnp->zmax,cnp->zmin,8) <= 0.0 ?
		True : False;
	if (cnp->const_field) {
		e_text = 
		 "%s: scalar field is constant; ContourPlot not possible";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(NhlWARNING,ret);
	}

	cnp->data_init = True;
	cnp->data_changed = True;

	return ret;
}

/*
 * Function:  ManageViewDepResources
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
static NhlErrorTypes    ManageViewDepResources
#if	NhlNeedProto
	(NhlLayer	new, 
	NhlLayer	old,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	NhlLayer	new;
	NhlLayer	old;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	NhlContourPlotLayer		cnew = (NhlContourPlotLayer) new;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	NhlContourPlotLayer		cold = (NhlContourPlotLayer) old;

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";

/* Adjust line dash segment length */

	if (! cnp->line_dash_seglen_set) {
		if (init) {
			cnp->line_dash_seglen *= 
				cnew->view.width / NHL_DEFAULT_VIEW_WIDTH;
		}
		else if (cnew->view.width != cold->view.width) {
			cnp->line_dash_seglen *= 
				cnew->view.width / cold->view.width;
		}
	}
	if (! cnp->cell_size_set) {
		if (init) {
			cnp->cell_size *= 
				cnew->view.width / NHL_DEFAULT_VIEW_WIDTH;
		}
		else if (cnew->view.width != cold->view.width) {
			cnp->cell_size *= 
				cnew->view.width / cold->view.width;
		}
	}

	subret = AdjustText(&cnp->line_lbls,cnew,cold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = AdjustText(&cnp->high_lbls,cnew,cold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = AdjustText(&cnp->low_lbls,cnew,cold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = AdjustText(&cnp->info_lbl,cnew,cold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = AdjustText(&cnp->constf_lbl,cnew,cold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	return ret;
}


/*
 * Function:  CopyTextAttrs
 *
 * Description: Copies the text attributes from one label type to another.
 *              The format string is copied to into a separate memory
 *		area; the text string and the angle are not copied.
 *	       
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
static NhlErrorTypes    CopyTextAttrs
#if	NhlNeedProto
(
	NhlcnLabelAttrs *dest,
	NhlcnLabelAttrs *source,
	NhlString	entry_name
)
#else
(dest,source,entry_name)
	NhlcnLabelAttrs *dest;
	NhlcnLabelAttrs *source;
	NhlString	entry_name;
#endif
{
	char 		*e_text;
	char		*save_fstring;
	NhlPointer	save_text;
	float		save_angle;
	
	save_text = dest->text;
	save_angle = dest->angle;
	save_fstring = dest->format.fstring;

	if (source->format.fstring == NULL) {
		if (save_fstring)
			NhlFree(save_fstring);
		save_fstring = NULL;
	}
	else {
		int	slen = 0,dlen = 0;
		slen = strlen(source->format.fstring);
		if (save_fstring)
			dlen = strlen(save_fstring);
		if (dlen < slen) {
			if (save_fstring)
				NhlFree(save_fstring);
			if ((save_fstring = NhlMalloc(slen+1)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
		}
		strcpy(save_fstring,source->format.fstring);
	}
	memcpy((void *)dest,(Const void *)source,sizeof(NhlcnLabelAttrs));

	dest->format.fstring = save_fstring;
	dest->text = save_text;
	dest->angle = save_angle;

	return NhlNOERROR;
}

/*
 * Function:  AdjustText
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
static NhlErrorTypes    AdjustText
#if	NhlNeedProto
(
	NhlcnLabelAttrs *lbl_attrp,
	NhlContourPlotLayer	new, 
	NhlContourPlotLayer	old,
	NhlBoolean	init
)
#else
(lbl_attrp,new,old,init)
	NhlcnLabelAttrs *lbl_attrp;
	NhlLayer	new;
	NhlLayer	old;
	NhlBoolean	init;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char 			*e_text;
	char			*entry_name;
	NhlContourPlotLayer		cnew = (NhlContourPlotLayer) new;
	NhlContourPlotLayer		cold = (NhlContourPlotLayer) old;

	entry_name = (init) ? "ContourPlotInitialize" : "ContourPlotSetValues";

/* 
 * Adjust text height. Then determine principal width and height
 * and the "real " text height based on aspect ratio. 21.0 is the default 
 * principle height. This code handles aspect ratio like the TextItem.
 */

	if (! lbl_attrp->height_set) {
		if (init) {
			lbl_attrp->height *= 
				cnew->view.width / NHL_DEFAULT_VIEW_WIDTH;
		}
		else if (cnew->view.width != cold->view.width) {
			lbl_attrp->height *= 
				cnew->view.width / cold->view.width;
		}
	}

        if (lbl_attrp->aspect <= 0.0 ) {
		e_text = "%s: Invalid value for text aspect ratio %d";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,lbl_attrp->aspect);
                ret = NhlWARNING;
                lbl_attrp->aspect = 1.3125;
        }
        if (lbl_attrp->aspect <= 1.0) {
                lbl_attrp->pheight = 21.0 * lbl_attrp->aspect;
                lbl_attrp->pwidth = 21.0;
        } else {
                lbl_attrp->pwidth = 21.0 * 1.0/lbl_attrp->aspect;
                lbl_attrp->pheight = 21.0;
        }
	/*
	 * The 1.125 factor compensates for the PLOTCHAR 'SA' parameter
	 */
        lbl_attrp->real_height = 
		1.0 / lbl_attrp->aspect * lbl_attrp->height * 1.125;

	return ret;
}
/*
 * Function:  ManageDynamicArrays
 *
 * Description: Creates and manages internal copies of each of the 
 *	ContourPlot GenArrays. Populates the copies with the values specified 
 *	via ContourPlotCreate or ContourPlotSetValues calls. Assigns default 
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
static NhlErrorTypes    ManageDynamicArrays
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlContourPlotLayer	cnew = (NhlContourPlotLayer) new;
	NhlContourPlotLayerPart *cnp = &(cnew->contourplot);
	NhlContourPlotLayer	cold = (NhlContourPlotLayer) old;
	NhlContourPlotLayerPart *ocnp = &(cold->contourplot);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	int i,count;
	NhlGenArray ga;
	char *entry_name;
	char *e_text;
	int *ip;
	float *fp;
	float fval;
	int	init_count;
	NhlBoolean need_check,changed;
	int old_count;
	float *levels = NULL;
	NhlBoolean levels_modified = False, flags_modified = False;
	NhlBoolean line_init;

	entry_name =  init ? "ContourPlotInitialize" : "ContourPlotSetValues";

/* Determine the contour level state */

	subret = SetupLevels(new,old,init,&levels,&levels_modified);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error getting contour level information";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	count = cnp->level_count;

/*=======================================================================*/

/* 
 * The levels array 
 */
	ga = init ? NULL : ocnp->levels;
	subret = ManageGenArray(&ga,count,cnp->levels,Qfloat,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnLevels,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->levels = changed || levels_modified ? NULL : cnp->levels;
	cnp->levels = ga;
	if (levels_modified) {
		if (levels == NULL) {
			e_text = "%s: internal error getting levels";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		NhlFree(cnp->levels->data);
		cnp->levels->data = (NhlPointer) levels;
		cnp->levels->num_elements = count;
#if 0
		printf("no of levels: %d\n", cnp->level_count);
		for (i= 0; i < cnp->level_count; i++)
			printf("level %d: %f\n", i, levels[i]);
#endif
	}

/*=======================================================================*/

/*
 * Level flags
 */

	ga = init ? NULL : ocnp->level_flags;
	count = cnp->level_count;
	flags_modified = 
		(ga != cnp->level_flags) ||
			(cnp->mono_level_flag != ocnp->mono_level_flag);
	subret = ManageGenArray(&ga,count,cnp->level_flags,Qint,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnLevelFlags,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->level_flags = changed ? NULL : cnp->level_flags;
	cnp->level_flags = ga;

	ip = (int *) cnp->level_flags->data;
	if (need_check) {
		if (! flags_modified && ! cnp->mono_level_flag &&
	    		(levels_modified || cnp->llabel_interval_set)) {
			flags_modified = True;
			if (cnp->llabel_interval <= 0) {
				for (i = init_count; i < count; i++) 
					ip[i] = NhlLINEONLY;
			}
			else {
				for (i = init_count; i < count; i++)
					ip[i] = (i - cnp->ref_level) % 
						cnp->llabel_interval == 0 ?
						NhlLINEANDLABEL : NhlLINEONLY;
			}
		}
		for (i=0; i<init_count; i++) {
			if (ip[i] < NhlNOLINE || 
			    ip[i] > NhlLINEANDLABEL) {
				e_text =
	      "%s: %s index %d contains an invalid level flag, %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNcnLevelFlags,i,ip[i]);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlLINEONLY;
			}
		}
	}

/* Set up label scaling - the levels and level_flags must have been set */

	subret = SetLabelScale(cnew,cold,init);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting up label scaling";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
/*=======================================================================*/

/*
 * Legend level flags
 */
	count = cnp->level_count;
	if ((init && cnp->lgnd_level_flags) ||
	    _NhlArgIsSet(args,num_args,NhlNcnLegendLevelFlags)) {
		if (! init && ocnp->lgnd_level_flags != NULL)
			NhlFreeGenArray(ocnp->lgnd_level_flags);
		if ((ga =  _NhlCopyGenArray(cnp->lgnd_level_flags,
					    True)) == NULL) {
			e_text = "%s: error copying GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(NhlFATAL);
		}
		cnp->lgnd_level_flags = ga;
	}
	if (cnp->lgnd_level_flags != NULL &&
	    cnp->lgnd_level_flags->num_elements < count) {
		ip = (int *) cnp->lgnd_level_flags->data;
		ip = NhlRealloc(ip,count * sizeof (int));
		if (ip == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(NhlFATAL);
		}
		for (i = cnp->lgnd_level_flags->num_elements;i < count; i++) {
			ip[i] = NhlNOLINE;
		}
		cnp->lgnd_level_flags->num_elements = count;
		cnp->lgnd_level_flags->data = (NhlPointer) ip;
	}

/*=======================================================================*/
	
/*
 * Fill colors
 */

	ga = init ? NULL : ocnp->fill_colors;
	count = cnp->fill_count;
	subret = ManageGenArray(&ga,count,cnp->fill_colors,Qcolorindex,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnFillColors,entry_name);

        if (init || cnp->fill_count > ocnp->fill_count)
                need_check = True;
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ocnp->fill_colors = changed ? NULL : cnp->fill_colors;
	cnp->fill_colors = ga;

	if (need_check) {
		subret = CheckColorArray(cnew,ga,count,init_count,old_count,
					 &cnp->gks_fill_colors,
					 NhlNcnFillColors,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
			return ret;
	}

/*=======================================================================*/
	
/*
 * Fill patterns
 */

	ga = init ? NULL : ocnp->fill_patterns;
	count = cnp->fill_count;

	subret = ManageGenArray(&ga,count,cnp->fill_patterns,Qfillindex,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnFillPatterns,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ocnp->fill_patterns = changed ? NULL : cnp->fill_patterns;
	cnp->fill_patterns = ga;

	if (init || changed) {
		int len;

		ip = (int *) ga->data;
		NhlVAGetValues(cnew->base.wkptr->base.id,
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
					  NhlNcnFillPatterns,i,ip[i]);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlSOLIDFILL;
			}
		}
	}

/*=======================================================================*/
	
/*
 * Fill scales
 */

	ga = init ? NULL : ocnp->fill_scales;
	count = cnp->fill_count;
	fval = 1.0;
	subret = ManageGenArray(&ga,count,cnp->fill_scales,Qfloat,&fval,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnFillScales,entry_name);
	
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->fill_scales = changed ? NULL : cnp->fill_scales;
	cnp->fill_scales = ga;
	
	if (init || changed) {

		fp = (float *) ga->data;
		for (i=0; i<count; i++) {
			if (fp[i] <= 0.0) {
				e_text =
	            "%s: %s index %d holds an invalid fill scale: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNcnFillScales,i);
				ret = MIN(ret, NhlWARNING);
				fp[i] = 1.0;
			}
		}
	}
/*=======================================================================*/
	
/*
 * Line colors
 */
	ga = init ? NULL : ocnp->line_colors;
	count = cnp->level_count;
	subret = ManageGenArray(&ga,count,cnp->line_colors,Qcolorindex,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnLineColors,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
        if (init || cnp->level_count > ocnp->level_count)
                need_check = True;
	ocnp->line_colors = changed ? NULL : cnp->line_colors;
	cnp->line_colors = ga;

		
	if (need_check) {
		subret = CheckColorArray(cnew,ga,count,init_count,old_count,
					 &cnp->gks_line_colors,
					 NhlNcnLineColors,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
			return ret;
	}
/*=======================================================================*/

/*
 * Line dash patterns
 * Since Conpack needs to know the actual strings, it is necessary to 
 * get a copy of the dash table.
 */
		
	if (cnp->dash_table == NULL) {
		subret = NhlVAGetValues(cnew->base.wkptr->base.id,
					_NhlNwkDashTable,&ga,
					NhlNwkDashTableLength,&cnp->dtable_len,
					NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: NhlFATAL error retrieving dash table";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		cnp->dash_table = ga;
		cnp->dtable = (NhlString *) ga->data;

		ga = NULL;
	}
	else {
		ga = ocnp->line_dash_patterns;
	}
	count = cnp->level_count;
	subret = ManageGenArray(&ga,count,cnp->line_dash_patterns,Qdashindex,
				NULL,&old_count,&init_count,&need_check,
				&changed,NhlNcnLineDashPatterns,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->line_dash_patterns = changed ? NULL : cnp->line_dash_patterns;
	cnp->line_dash_patterns = ga;

	if (need_check) {
		ip = (int *) ga->data;
		for (i=init_count; i < count; i++) {
			ip[i] = i % cnp->dtable_len + 1;
		}
		for (i=0; i<init_count; i++) {
			if (ip[i] < 0 || ip[i] > cnp->dtable_len) {
				e_text =
			 "%s: %s index %d has invalid value: %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,
					  NhlNcnLineDashPatterns,i,ip[i]);
				ret = MIN(ret, NhlWARNING);
				ip[i] = Nhl_cnDEF_DASH_PATTERN;
			}
		}
	}

/*=======================================================================*/
	
/*
 * Line thicknesses
 */

	ga = init ? NULL : ocnp->line_thicknesses;
	count = cnp->level_count;
	fval = 1.0;
	subret = ManageGenArray(&ga,count,cnp->line_thicknesses,Qfloat,&fval,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnLineThicknesses,entry_name);
	
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->line_thicknesses = changed ? NULL : cnp->line_thicknesses;
	cnp->line_thicknesses = ga;
	
	if (need_check) {
		fp = (float *) ga->data;
		for (i=0; i<count; i++) {
			if (fp[i] <= 0.0) {
				e_text =
	        "%s: %s index %d holds an invalid line thickness: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNcnLineThicknesses,i);
				ret = MIN(ret, NhlWARNING);
				fp[i] = 1.0;
			}
		}
	}

/*=======================================================================*/
	
/*
 * Line Label strings
 * Need both the user accessible array that holds all the Line Label strings,
 * and a private array containing pointers to the strings that get drawn,
 * for the benefit of Legend. Since this GenArray only allocates the 
 * pointer array, not the strings, it will need to be freed specially.
 */
	count = cnp->level_count;
	ga = init ? NULL : ocnp->llabel_strings;
	line_init = cnp->llabel_strings == NULL ? True : False;
	subret = ManageGenArray(&ga,count,cnp->llabel_strings,
				Qstring,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnLineLabelStrings,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->llabel_strings = changed ? NULL : cnp->llabel_strings;
	cnp->llabel_strings = ga;

	if (levels_modified || need_check ||
	    cnp->max_data_format.fstring != ocnp->max_data_format.fstring ||
	    cnp->line_lbls.format.fstring != ocnp->line_lbls.format.fstring ||
	    cnp->label_scale_value != ocnp->label_scale_value ||
	    cnp->label_scale_factor != ocnp->label_scale_factor ||
	    cnp->explicit_line_labels_on != ocnp->explicit_line_labels_on) {
		NhlString *sp = (NhlString *) ga->data;
		NhlBoolean modified = False;
		NhlString cp;

		fp = (float *) cnp->levels->data;

		/* 
		 * Ignore set values when explicit line labels is False,
		 * or when it has just been set but label strings has not
		 * been set.
		 */

		if (! cnp->explicit_line_labels_on) {
			init_count = 0;
		}
		for (i=init_count; i<count; i++) {
			float fval = fp[i] / cnp->label_scale_factor;
			NhlFormatRec *frec = &cnp->max_data_format;

			if (sp[i] != NULL) NhlFree(sp[i]);
			cp = _NhlFormatFloat(&cnp->line_lbls.format,fval,
                                             NULL,
					     &frec->sig_digits,
					     &frec->left_sig_digit,
                                             NULL,NULL,NULL,
					     cnp->line_lbls.fcode[0],
					     entry_name);
			if (cp == NULL) return NhlFATAL;
			if ((sp[i] = (char *) 
			     NhlMalloc(strlen(cp)+1)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			strcpy(sp[i],cp);
			modified = True;
                        need_check = True;
		}
		if (modified) ocnp->llabel_strings = NULL;
	}
	if (line_init) {
		NhlString *sp;

		if ((sp = (NhlString *) 
		     NhlMalloc(count * sizeof(NhlString))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if ((cnp->ll_strings = 
		     NhlCreateGenArray((NhlPointer)sp,NhlTString,
				       sizeof(NhlString),1,&count)) == NULL) {
			e_text = "%s: error creating GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		cnp->ll_strings->my_data = False;
	}
	if (flags_modified || need_check) {
		NhlString *sp = cnp->ll_strings->data;
		NhlString *llsp = cnp->llabel_strings->data;

		if (count > cnp->ll_strings->num_elements) {
			if ((sp = (NhlString *) 
			     NhlRealloc(sp, 
					count * sizeof(NhlString))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			cnp->ll_strings->data = (void *) sp;
			cnp->ll_strings->num_elements = count;
		}
		ip = (int *) cnp->level_flags->data;
		for (i = 0; i < count; i++) {
			if (ip[i] < NhlLABELONLY) {
				sp[i] = cnEmptyString;
			}
			else {
				sp[i] = llsp[i];
			}
		}
		ocnp->ll_strings = NULL;
	}
				

/*=======================================================================*/
	
/*
 * Line Label colors
 */

	ga = init ? NULL : ocnp->llabel_colors;
	count = cnp->level_count;
	subret = ManageGenArray(&ga,count,cnp->llabel_colors,Qcolorindex,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNcnLineLabelFontColors,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
        if (init || cnp->level_count > ocnp->level_count)
                need_check = True;
	ocnp->llabel_colors = changed ? NULL : cnp->llabel_colors;
	cnp->llabel_colors = ga;


	if (need_check) {
		subret = CheckColorArray(cnew,ga,count,init_count,old_count,
					 &cnp->gks_llabel_colors,
					 NhlNcnLineLabelFontColors,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
			return ret;
	}

	if (cnp->high_use_line_attrs && cnp->line_lbls.on) {
		cnp->high_lbls.color = cnp->line_lbls.color; 
	}
	if (cnp->low_use_high_attrs && cnp->high_lbls.on) {
		cnp->low_lbls.color = cnp->high_lbls.color;
	}
	

/*=======================================================================*/

	if ((init && cnp->conpack_params != NULL) ||
	    (cnp->conpack_params != ocnp->conpack_params)) {

		if ((ga = 
		     _NhlCopyGenArray(cnp->conpack_params,True)) == NULL) {
			e_text = "%s: error copying gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if (! init && ocnp->conpack_params != NULL) {
			NhlFreeGenArray(ocnp->conpack_params);
			ocnp->conpack_params = NULL;
		}
		cnp->conpack_params = ga;
	}

	return ret;
}

/*
 * Function:    ManageGenArray
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
 *	changes requested via ContourPlotSetValues
 */

/*ARGSUSED*/
static NhlErrorTypes    ManageGenArray
#if	NhlNeedProto
	(NhlGenArray	*ga,
	 int		count,
	 NhlGenArray	copy_ga,
	 NrmQuark	type,
	 NhlPointer	init_val,
	 int		*old_count,
	 int		*init_count,
	 NhlBoolean	*need_check,
	 NhlBoolean	*changed,
	 NhlString	resource_name,
	 NhlString	entry_name)
#else
(ga,count,copy_ga,type,init_val,old_count,init_count,
 need_check,changed,resource_name,entry_name)
	NhlGenArray	*ga;
	int		count;
	NhlGenArray	copy_ga;
	NrmQuark	type;
	NhlPointer	init_val;
	int		*old_count;
	int		*init_count;
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
	else if (type == Qfillindex) {
		str_type = NhlTFillIndex;
		size = sizeof(NhlFillIndex);
	}
	else if (type == Qcolorindex) {
		str_type = NhlTColorIndex;
		size = sizeof(NhlColorIndex);
	}
	else if (type == Qdashindex) {
		str_type = NhlTDashIndex;
		size = sizeof(NhlDashIndex);
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
		ret = _NhlValidatedGenArrayCopy(ga,copy_ga,Nhl_cnMAX_LEVELS+1,
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
		else if (type == Qcolorindex)
			for (i = *init_count; i< count; i++)
				((NhlColorIndex *)datap)[i] =
						*((NhlColorIndex *)init_val);
		else if (type == Qfillindex)
			for (i = *init_count; i< count; i++)
				((NhlFillIndex *)datap)[i] =
						*((NhlFillIndex *)init_val);
		else if (type == Qdashindex)
			for (i = *init_count; i< count; i++)
				((NhlDashIndex *)datap)[i] =
						*((NhlDashIndex *)init_val);
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
 * Function:    CheckColorArray
 *
 * Description:	Checks color array values for validity, initializing any
 *		currently uninitialized values and keeps the GKS index
 *		arrays up-to-date.
 *
 * In Args:	count		number of elements to create in the GenArray
 *		resource_name	name of the GenArray resource 		
 *		entry_name	name of the high level caller of the routine 
 *
 * Out Args:	*ga		If non-NULL on input, contains a previously
 *				allocated GenArray, whose data will be 
 *				replaced if necessary.
 *				Out: An allocated GenArray with allocated data
 *		*init_count	number of values initialized - if init_val is
 *				non-NULL, will contain count; if init_val is
 *				NULL will contain MIN(count,number of 
 *				elements in copy_ga); if copy_ga is also NULL
 *				will contain 0.
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via ContourPlotSetValues
 */

/*ARGSUSED*/

static NhlErrorTypes	CheckColorArray
#if	NhlNeedProto
	(NhlContourPlotLayer	cl,
	NhlGenArray	ga,
	int		count,
	int		init_count,
	int		last_count,
	int		**gks_colors,
	NhlString	resource_name,
	NhlString	entry_name)
#else
(cl,ga,count,init_count,last_count,gks_colors,resource_name,entry_name)
	NhlContourPlotLayer	cl;
	NhlGenArray	ga;
	int		count;
	int		init_count;
	int		last_count;
	int		**gks_colors;
	NhlString	resource_name;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	char *e_text;
	int *ip;
	int i;
	

	ip = (int *) ga->data;

	for (i=init_count; i < count; i++) {
		ip[i] = Nhl_cnCOLOR_ARRAY_START + i;
	}

	if (*gks_colors)
		NhlFree(*gks_colors);
	if ((*gks_colors = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	for (i=0; i<count; i++) {
                if (ip[i] == NhlTRANSPARENT)
                        (*gks_colors)[i] = NhlTRANSPARENT;
                else
                        (*gks_colors)[i] =
                                _NhlGetGksCi(cl->base.wkptr,ip[i]);
	}
	return ret;
}



/*
 * Function:  cnComputeRefLevel
 *
 * Description: Finds the reference level, the level used as a base for
 *		line labelling based on linelabelinterval. 
 *
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
static NhlErrorTypes    cnComputeRefLevel
#if	NhlNeedProto
(
	NhlContourPlotLayerPart	*cnp,
	float			*levels,
	NhlString		entry_name
)
#else
(cnp,levels,entry_name)
	NhlContourPlotLayerPart	*cnp;
	float			*levels;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	int	i;
	int	divpwr,sigdig,ref_level = 0;
	int	min_sig_digits = 64;
	int	max_sig_digits = 1;
	float	test_fac = 1.0;
	float	test_val = MAX(fabs(cnp->zmax),fabs(cnp->zmin));
	float	test_high = pow(10.0,cnp->max_data_format.sig_digits);
	float	test_low  = pow(10.0,cnp->max_data_format.sig_digits - 1);

        if (cnp->level_count == 1 || cnp->const_field || test_val == 0.0) {
                cnp->ref_level = 0;
                return ret;
        }
        
	if (test_val < test_low) {
		while (test_val < test_low) {
			test_val *= 10.0;
			test_fac *= 10.0;
		}
	}
	else if (test_val >= test_high) {
		while (test_val >= test_high) {
			test_val /= 10.0;
			test_fac /= 10.0;
		}
	}
	for (i = 0; i < cnp->level_count; i++) {
		test_val = (float) (int) (fabs((levels)[i]) * test_fac + 0.5);
		if (test_val == 0.0) {
			ref_level = i;
			break;
		}
		subret = _NhlGetScaleInfo(test_val,
					  &divpwr,&sigdig,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
			return ret;
		if (sigdig < min_sig_digits) {
			min_sig_digits = sigdig;
			ref_level = i;
		}
		if (sigdig > max_sig_digits) {
			max_sig_digits = sigdig;
		}
	}
	cnp->ref_level = ref_level;

#if 0	
	if (cnp->max_data_format.sig_digits_flag == NhlffDYNAMIC) {
		cnp->max_data_format.sig_digits = MIN(6,max_sig_digits);
	}
#endif
	return ret;
}

/*
 * Function:  SetupLevels
 *
 * Description: Depending on the setting of the LevelCount resource,
 *		decides whether to allow Conpack to determine the 
 *		number of ContourPlot levels. If so, makes the appropriate
 *		ContourPlot calls.
 *
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
static NhlErrorTypes    SetupLevels
#if	NhlNeedProto
	(NhlLayer	new, 
	 NhlLayer	old,
	 NhlBoolean	init,
	 float		**levels,
	 NhlBoolean	*modified)
#else
(new,old,init,levels,modified)
	NhlLayer		new;
	NhlLayer		old;
	NhlBoolean	init;
	float		**levels;
	NhlBoolean	*modified;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourPlotLayer		cnew = (NhlContourPlotLayer) new;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	NhlContourPlotLayer		cold = (NhlContourPlotLayer) old;
	NhlContourPlotLayerPart	*ocnp = &(cold->contourplot);

	entry_name = init ? "ContourPlotInitialize" : "ContourPlotSetValues";
	*modified = False;

	if ((! init) && 
	    (! cnp->data_changed) &&
	    (! cnp->level_spacing_set) && 
	    (cnp->levels == ocnp->levels) &&
	    (cnp->level_selection_mode == ocnp->level_selection_mode) &&
	    (cnp->max_level_count == ocnp->max_level_count) &&
	    (cnp->min_level_val == ocnp->min_level_val) &&
	    (cnp->max_level_val == ocnp->max_level_val) &&
	    (cnp->const_field == ocnp->const_field) &&
	    (cnp->max_data_format.fstring == ocnp->max_data_format.fstring))
		return ret;

	cnp->ref_level = 0;

	if (cnp->level_spacing_set && cnp->level_spacing <= 0.0) {
		e_text = 
			"%s: Invalid level spacing value set: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		cnp->level_spacing = 5.0;
                cnp->level_spacing_set = False;
	}
	if (cnp->max_level_count < 1) {
		e_text = 
			"%s: %s must be greater than 0: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNcnMaxLevelCount);
		ret = MIN(ret,NhlWARNING);
		cnp->max_level_count = 16.0;
	}
	
	switch (cnp->level_selection_mode) {

	case NhlMANUALLEVELS:
                subret = SetupLevelsManual(cnew,cold,levels,entry_name);
		break;
	case NhlEQUALSPACEDLEVELS:
		subret = SetupLevelsEqual(cnew,cold,levels,entry_name);
		break;
	case NhlAUTOMATICLEVELS:
		subret = SetupLevelsAutomatic(cnew,cold,levels,entry_name);
		break;
	case NhlEXPLICITLEVELS:
                subret = SetupLevelsExplicit(cnew,cold,init,levels,entry_name);
		break;
	default:
		ret = NhlFATAL;
		e_text = "%s: Invalid level selection mode";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
        if ((ret = MIN(subret,ret)) < NhlWARNING) {
                return ret;
        }
        *modified = True;

	subret = cnComputeRefLevel(cnp,*levels,entry_name);
	ret = MIN(subret,ret);

	cnp->min_level_set = True;
	cnp->max_level_set = True;
		
	return ret;

}

/*
 * Function:  SetupLevelsManual
 *
 * Description: Sets up Manual mode levels
 *
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
static NhlErrorTypes    SetupLevelsManual
#if	NhlNeedProto
	(NhlContourPlotLayer	cnew, 
	 NhlContourPlotLayer	cold,
	 float		**levels,
	 char		*entry_name)
#else
(cnew,cold,levels,entry_name)
	NhlContourPlotLayer	cnew;
	NhlContourPlotLayer	cold;
	float		**levels;
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	int			i, count;
	float			lmin,lmax,rem,spacing;
	float			*fp;
        NhlBoolean		do_automatic = False;
        
        
	if ((cnp->min_level_val > cnp->max_level_val) ||
            (cnp->level_count > 1 &&
             cnp->min_level_val == cnp->max_level_val)) {
		e_text =
		"%s: Invalid level values set: using AUTOMATICLEVELS mode ";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		do_automatic = True;
	}
			
	if (cnp->zmax <= cnp->min_level_val || 
	    cnp->zmin > cnp->max_level_val) {
		e_text =
          "%s: Data values out of range of levels set by MANUALLEVELS mode";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
                ret = MIN(ret,NhlWARNING);
	}
	if (! cnp->min_level_set) {
		do_automatic = True;
	}
                
	if (cnp->level_spacing <= 0.0) {
	e_text = "%s: Invalid level spacing value: using AUTOMATICLEVELS mode";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		do_automatic = True;
        }
        if (do_automatic) {
                subret = SetupLevelsAutomatic(cnew,cold,levels,entry_name);
                return (MIN(ret,subret));
        }
	spacing = cnp->level_spacing;
	lmin = cnp->min_level_val;

	if (cnp->max_level_set) {
		lmax = cnp->max_level_val;
	}
        else if (cnp->const_field) {
                lmax = cnp->min_level_val;
	}
	else {
		lmax = floor(((cnp->zmax - lmin) / spacing) * spacing + lmin);
		if (_NhlCmpFAny(lmax,cnp->zmax,6) == 0.0) {
			lmax -= spacing;
		}
		cnp->max_level_val = lmax;
	}

	if (cnp->const_field) {
		count = 1;
	}
	else {
		count = (lmax - lmin) / cnp->level_spacing;
		rem = lmax - lmin - cnp->level_spacing * count; 
		if (_NhlCmpFAny(rem,0.0,6) != 0.0)
		  count += 2;
		else
		  count += 1;
	}

	if (count <= 1) {
		e_text = 
		  "%s: cnLevelSpacingF value equals or exceeds data range";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
                ret = MIN(ret,NhlWARNING);
	}
	if (count >  Nhl_cnMAX_LEVELS) {
		ret = MIN(NhlWARNING,ret);
		e_text = 
 "%s: cnLevelSpacingF value causes level count to exceed maximum: using AUTOMATICLEVELS mode";
		do_automatic = True;
	}
	else {
		cnp->max_level_count = MAX(cnp->max_level_count, count);
	}
        if (do_automatic) {
                subret = SetupLevelsAutomatic(cnew,cold,levels,entry_name);
                return (MIN(ret,subret));
        }
	
	if ((*levels = (float *) 
	     NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	for (i=0, fp = *levels; i < count - 1; i++) {
		*(fp++) = lmin + i * cnp->level_spacing;
	}
	*fp = lmax;

	cnp->level_count = count;
	cnp->fill_count = count + 1;

	return ret;
}

/*
 * Function:  SetupLevelsEqual
 *
 * Description: Sets up Equally spaced levels
 *
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
static NhlErrorTypes    SetupLevelsEqual
#if	NhlNeedProto
	(NhlContourPlotLayer	cnew,
	 NhlContourPlotLayer	cold,
	 float		**levels,
	 char		*entry_name)
#else
(cnew,cold,levels,entry_name)
	NhlContourPlotLayer	cnew;
	NhlContourPlotLayer	cold;
	float		**levels;
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	int			i;
	float			lmin,lmax,size;

	lmin = cnp->zmin;
	lmax = cnp->zmax;

        if (! cnp->const_field) {
                size = (lmax - lmin) / (cnp->max_level_count + 1);
                cnp->level_count = cnp->max_level_count;
        }
        else {
                cnp->level_count = 1;
                size = cnp->level_spacing;
        }
	if ((*levels = (float *) 
	     NhlMalloc(cnp->level_count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	for (i=0; i < cnp->level_count; i++) {
		(*levels)[i] = cnp->zmin + (i+1) * size;
	}
	
	cnp->min_level_val = (*levels)[0];
	cnp->max_level_val = (*levels)[cnp->level_count - 1];
	cnp->level_spacing = size;
	cnp->fill_count = cnp->level_count + 1;

	return ret;
}

/*
 * Function:  SetupLevelsAutomatic
 *
 * Description: Sets up Automatic mode levels
 *
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
static NhlErrorTypes    SetupLevelsAutomatic
#if	NhlNeedProto
	(NhlContourPlotLayer	cnew, 
	 NhlContourPlotLayer	cold,
	 float		**levels,
	 char		*entry_name)
#else
(cnew,cold,levels,entry_name)
	NhlContourPlotLayer	cnew;
	NhlContourPlotLayer	cold;
	float		**levels;
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	int			i,count = 0;

	float			lmin,lmax,ftmp,ftest;
	float			spacing;
	NhlBoolean		choose_spacing = True;

	lmin = cnp->zmin;
	lmax = cnp->zmax;

        if (cnp->const_field) {
                choose_spacing = False;
                count = 1;
                spacing = cnp->level_spacing;
        }
        
	if (cnp->level_spacing_set) {
		spacing = cnp->level_spacing;
		lmin = ceil(lmin / spacing) * spacing;
		lmax = MIN(lmax,floor(lmax / spacing) * spacing);
		count =	(int)((lmax - lmin) / cnp->level_spacing + 1.5);
		if (_NhlCmpFAny(lmin,cnp->zmin,6) == 0.0) {
			lmin += spacing;
			count--;
		}
		if (_NhlCmpFAny(lmax,cnp->zmax,6) == 0.0) {
			lmax -= spacing;
			count--;
		}
		if (count <= 0) {
			ret = MIN(NhlWARNING,ret);
			lmin = cnp->zmin;
			lmax = cnp->zmax;
			e_text = 
	  "%s: cnLevelSpacingF value exceeds or equals data range: defaulting";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		}
		else if (count >  Nhl_cnMAX_LEVELS) {
			ret = MIN(NhlWARNING,ret);
			e_text = 
 "%s: cnLevelSpacingF value causes level count to exceed maximum: defaulting";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		}
		else {
			cnp->max_level_count = 
				MAX(cnp->max_level_count, count);
			choose_spacing = False;
		}
	}
	if (choose_spacing) {
		subret = ChooseSpacingLin(&lmin,&lmax,&spacing,7,
					  cnp->max_level_count,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error choosing spacing";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		if (_NhlCmpFAny(lmin,cnp->zmin,6) == 0.0) {
			lmin += spacing;
		}
		ftmp = lmin;
		ftest = cnp->zmax;
		count = 0;
		while (_NhlCmpFAny(ftmp,ftest,6) < 0.0) {
			count++;
			ftmp = lmin + count * spacing;
		}
	}

	if ((*levels = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	for (i =  0; i <  count; i++)
		(*levels)[i] = lmin + i * spacing;

	cnp->level_spacing = spacing;
	cnp->level_count = count;
	cnp->fill_count = count + 1;
	cnp->min_level_val = lmin;
	cnp->max_level_val = (*levels)[count - 1];

	return ret;
}

/*
 * Function:  SetupLevelsExplicit
 *
 * Description: Sets up Explicit mode levels
 *
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
static NhlErrorTypes    SetupLevelsExplicit
#if	NhlNeedProto
	(NhlContourPlotLayer	cnew, 
	 NhlContourPlotLayer	cold,
	 NhlBoolean		init,
	 float			**levels,
	 char			*entry_name)
#else
(cnew,cold,levels,entry_name)
	NhlContourPlotLayer	cnew;
	NhlContourPlotLayer	cold;
	NhlBoolean	init;
	float		**levels;
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnew->contourplot);
	NhlContourPlotLayerPart	*ocnp = &(cold->contourplot);
	int			i,j,count;
	float			*fp;
	float			ftmp;
        NhlBoolean		do_automatic = False;
        
        if (init && cnp->levels == NULL) {
                do_automatic = True;
        }
	if (cnp->levels == NULL || cnp->levels->num_elements < 1) {
		ret = MIN(NhlWARNING,ret);
		e_text = 
	      "%s: %s is NULL: using AUTOMATICLEVELS mode";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,NhlNcnLevels);
                do_automatic = True;
	}
        if (do_automatic) {
                subret = SetupLevelsAutomatic(cnew,cold,levels,entry_name);
                return MIN(ret,subret);
        }
                
	if (init || cnp->levels != ocnp->levels)
		count = cnp->levels->num_elements;
	else 
		count = cnp->level_count;

	if (count > Nhl_cnMAX_LEVELS) {
		ret = MIN(NhlWARNING,ret);
		e_text = 
"%s: Explicit level array count exceeds max level count: using AUTOMATICLEVELS mode";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		subret = SetupLevelsAutomatic(cnew,cold,levels,entry_name);
                return MIN(ret,subret);
	}
/*
 * Allocate space for the levels
 */
	fp = (float *) cnp->levels->data;
	if ((*levels = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	for (i = 0; i < count; i++)
		(*levels)[i] = fp[i];

	fp = *levels;
		
/*
 * Sort the array into ascending order
 */
	for (i = 0; i < count; i++) {
		int min = i;
		for (j = i + 1; j < count; j++)
			if (fp[j] < fp[min])
				min = j;
		if (min != i) {
			ftmp = fp[min];
			fp[min] = fp[i];
			fp[i] = ftmp;
		}
	}
/*
 * Find the average spacing
 */
        if (count > 1) {
                ftmp = 0;
                for (i = 1; i < count; i++) {
                        ftmp += fp[i] - fp[i-1];
                }
                cnp->level_spacing = ftmp / (count - 1);
        }
        else {
                cnp->level_spacing = 0.0;
        }
        
	cnp->min_level_val = fp[0];
	cnp->max_level_val = fp[count - 1];

	cnp->level_count = count;
	cnp->fill_count = count + 1;

	if ((cnp->min_level_val > cnp->max_level_val) ||
            (cnp->level_count > 1 &&
             cnp->min_level_val == cnp->max_level_val)) {
		e_text =
		"%s: Invalid level values set: using AUTOMATICLEVELS mode ";
                do_automatic = True;
	}
			
	if (cnp->zmax <= cnp->min_level_val || 
	    cnp->zmin > cnp->max_level_val) {
		e_text =
          "%s: Data values out of range of levels set by EXPLICITLEVELS mode";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
                ret = MIN(ret,NhlWARNING);
	}
        if (do_automatic) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
                NhlFree(*levels);
		subret = SetupLevelsAutomatic(cnew,cold,levels,entry_name);
                ret = MIN(ret,subret);
        }
	return ret;
}

/*
 * Function:	ChooseSpacingLin
 *
 * Description: Ethan's tick mark spacing code adapted to choosing 'nice'
 *		ContourPlot values; adapted by exchanging the ciel and floor
 *		functions - since the max and min contour values must be
 *		within the data space rather than just outside it as is 
 *		appropriate for tick marks. (Eventually should probably
 *		generalize the code with another parameter to handle either
 *		situation.)
 *
 * In Args:	tstart	requested tick starting location
 *		tend	requested tick ending location
 *		convert_precision  precision used to compare with
 *		max_ticks the maximum number of ticks to be choosen
 * Out Args:
 *		tstart  new start
 *		tend	new end
 *		spacing new spacing
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes ChooseSpacingLin
#if	NhlNeedProto
(
	float *tstart,
	float *tend,
	float *spacing,
	int convert_precision,
	int max_ticks,
	NhlString entry_name)
#else
(tstart,tend,spacing,convert_precision,max_ticks,entry_name)
	float *tstart;
	float *tend;
	float *spacing;
	int	convert_precision;
	int	max_ticks;
#endif
{
	double	table[] = 
	{ 1.0,2.0,2.5,4.0,5.0,
		  10.0,20.0,25.0,40.0,50.0,
		  100.0,200.0,250.0,400.0,500.0 };
	double	d,u,t,am1,ax1;
	double	am2=0.0,ax2=0.0;
	int	npts = 15;
	int	i;
	char	*e_text;

	if(_NhlCmpFAny(*tend,*tstart,8)<=0.0) {
		e_text = "%s: Scalar field is constant";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
		
	d = pow(10.0,floor(log10(*tend-*tstart)) - 2.0);
	u = *spacing = FLT_MAX;
	for(i=0;i<npts; i++) {
		t = table[i] * d;
		am1 = ceil(*tstart/t) *t;
		ax1 = floor(*tend/t) * t;
		if(((i>=npts-1)&&(*spacing == u))||
		   ((t <= *spacing)&&
		    (_NhlCmpFAny((ax1-am1)/t,(double)max_ticks,
				 convert_precision) <= 0.0))){
			*spacing = t;
			ax2 = ax1;
			am2 = am1;
		}
	}
	*tstart = am2;
	*tend = ax2;
	return(NhlNOERROR);
	
}

/*
 * Function:  hlucpfill
 *
 * Description: C version of APR user routine called from within ARSCAM 
 *		to fill areas based on the area ID.
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
int (_NHLCALLF(hlucpfill,HLUCPFILL))
#if	NhlNeedProto
(
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
)
#else
(xcs,ycs,ncs,iai,iag,nai)
	float *xcs; 
	float *ycs; 
	int *ncs; 
	int *iai; 
	int *iag; 
	int *nai;
#endif
{
	int i;
	int pat_ix, col_ix;
	float fscale;
	int *colp, *patp;
	float *sclp;

	if (Cnp == NULL) return 0;

	for (i = 0; i < *nai; i++) {
#if 0
		printf("i %d iai %d iag %d\n",i,iai[i],iag[i]);
#endif
		if (iag[i] == 17 && iai[i] == 9999) {
			return 0;
		}
		if (iag[i] == 5 && iai[i] == -1) {
			return 0;
		}
	}

	colp = (int *) Cnp->fill_colors->data;
	patp = (int *) Cnp->fill_patterns->data;
	sclp = (float *) Cnp->fill_scales->data;
	for (i = 0; i < *nai; i++) {
		if (iag[i] == 3) {
			if (iai[i] > 99 && 
			    iai[i] < 100 + Cnp->fill_count) {
				int ix = iai[i] - 100;
				col_ix = Cnp->mono_fill_color ? 
					Cnp->fill_color : colp[ix];
				pat_ix = Cnp->mono_fill_pattern ?
					Cnp->fill_pattern : patp[ix];
				fscale = Cnp->mono_fill_scale ?
					Cnp->fill_scale : sclp[ix];
			}
			else {
				NhlcnRegionAttrs *reg_attrs;

				switch (iai[i]) {
				case 98:
					reg_attrs = &Cnp->missing_val;
					break;
				default:
					return 0;
				}
				col_ix = reg_attrs->fill_color;
				pat_ix = reg_attrs->fill_pat;
				fscale = reg_attrs->fill_scale;
			}
			NhlVASetValues(Cnl->base.wkptr->base.id,
				       _NhlNwkFillIndex, pat_ix,
				       _NhlNwkFillColor, col_ix,
				       _NhlNwkFillScaleFactorF,fscale,
				       _NhlNwkFillBackground,
				       Cnp->fill_background_color,
				       _NhlNwkEdgesOn,0,
				       NULL);
			
			_NhlSetFillInfo(Cnl->base.wkptr,(NhlLayer) Cnl);
			_NhlWorkstationFill(Cnl->base.wkptr,xcs,ycs,*ncs);
		}
	}
	return 0;
}


/*
 * Function:  hlucpscae
 *
 * Description: 
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
void  (_NHLCALLF(hlucpscae,HLUCPSCAE))
#if	NhlNeedProto
(
	int		*icra,
	int		*ica1,
	int		*icam,
	int		*ican,
	float		*xcpf,
	float		*ycpf,
	float		*xcqf,
	float		*ycqf,
	int		*ind1,
	int		*ind2,
	int		*icaf,
	int		*iaid		      
)
#else
(icra,ica1,icam,ican,xcpf,ycpf,xcqf,ycqf,ind1,ind2,icaf,iaid)
	int		*icra;
	int		*ica1;
	int		*icam;
	int		*ican;
	float		*xcpf;
	float		*ycpf;
	float		*xcqf;
	float		*ycqf;
	int		*ind1;
	int		*ind2;
	int		*icaf;
	int		*iaid;
#endif
{
	int col_ix;

	if (Cnp == NULL) {
		_NHLCALLF(cpscae,CPSCAE)
			(icra,ica1,icam,ican,xcpf,ycpf,xcqf,ycqf,
			 ind1,ind2,icaf,iaid);
		return;
	}

	if (*iaid > 99 && *iaid < 100 + Cnp->fill_count) {
		col_ix = Cnp->gks_fill_colors[*iaid - 100];
		*(icra + ((*ind2 - 1) * *ica1 + (*ind1 - 1))) = col_ix;
	}
	else if (*iaid == 98) {
		col_ix = Cnp->missing_val.gks_fcolor;
		*(icra + ((*ind2 - 1) * *ica1 + (*ind1 - 1))) = col_ix;
	}
	else {
		col_ix = NhlBACKGROUND;
		*(icra + ((*ind2 - 1) * *ica1 + (*ind1 - 1))) = col_ix;
	}

	return;
}

/*
 * Function:  hlucpchcl
 *
 * Description: C version of the CPCHCL function that is called from
 *              the Conpack CPCLDR and CPCLDM functions. 
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
void   (_NHLCALLF(hlucpchcl,HLUCPCHCL))
#if	NhlNeedProto
(
	int	*iflg
)
#else
(iflg)
	int	*iflg;
#endif

{

	char func[] = "HLUCPCHCL";
	int i, pai, dpix;
	char buffer[NhlDASHBUFSIZE];
	int lcol;
	float thickness, tf;
	float *thp;
	int   *dpp;

	if (Cnp == NULL) {
		_NHLCALLF(cpchcl,CPCHCL)(iflg);
		return;
	}

	dpp = (int *) Cnp->line_dash_patterns->data;
	thp = (float *) Cnp->line_thicknesses->data;

	if (*iflg != 1) return;

	c_cpgeti("PAI", &pai);
	if (pai > 0 && pai < 256) {
		if (! Cnp->do_lines) return;
		thickness = Cnp->mono_line_thickness ? 
			Cnp->line_thickness : thp[pai-1];
		lcol = Cnp->mono_line_color ? 
			Cnp->gks_line_colors[0] : Cnp->gks_line_colors[pai-1];
		dpix = Cnp->mono_line_dash_pattern ? 
			Cnp->line_dash_pattern : dpp[pai-1];
	}
	else {
		NhlcnRegionAttrs *reg_attrs;

		switch (pai) {
		case -1:
			reg_attrs = &Cnp->grid_bound;
			break;
		case -2:
			reg_attrs = &Cnp->missing_val;
			break;
		case -3:
			reg_attrs = &Cnp->out_of_range;
			break;
		default:
			return;
		}
		thickness = reg_attrs->perim_thick;
		lcol = reg_attrs->gks_pcolor;
		dpix = reg_attrs->perim_dpat;
	}
		
	memset((void *) buffer,'\0', sizeof(buffer)*sizeof(char));

	c_pcseti("FN",0);
	c_pcseti("CL",1);
 	c_pcseti("CC",-1);
	c_pcseti("OC",-1);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	
	/*
	 * Reset DashPack so we know what state we are starting from.
	 */
	_NHLCALLF(dprset,DPRSET)();
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	c_dpsetc("CRG","'");
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	if (dpix < 0) 
		dpix = NhlSOLIDLINE;
	else if (dpix > Cnp->dtable_len)
	    dpix = 1 + (dpix - 1) % Cnp->dtable_len;
	strncpy(buffer,Cnp->dtable[dpix],sizeof(buffer) - 1);

	tf = Cnp->line_dash_seglen / (strlen(buffer)+.5);
	c_dpsetr("WOG",(float)tf);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_dpsetr("WOS",(float)tf);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	if (lcol == NhlTRANSPARENT) {
		for (i = 0; i < strlen(buffer); i++)
			buffer[i] = '\'';
	}
	else{
	        gset_line_colr_ind((Gint)lcol);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}

        gset_linewidth(thickness);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);


	if (pai > 0 && Cnp->llabel_placement == NhlCONSTANT) {
		int buff_size = sizeof(buffer) - strlen(buffer) - 1;
		char *tchar = &buffer[strlen(buffer)];
		char *ts = ((NhlString *) Cnp->line_lbls.text)[pai-1];
		NhlcnLevelUseMode *lup = 
			(NhlcnLevelUseMode *) Cnp->level_flags->data;
		NhlcnLevelUseMode flag;
		NhlColorIndex llcol;
		int j;
		NhlBoolean do_label;

		llcol = Cnp->line_lbls.mono_color ?
			 Cnp->line_lbls.gks_color : 
				Cnp->line_lbls.colors[pai-1];
		
		flag = Cnp->mono_level_flag ? 
			Cnp->level_flag : lup[pai-1];

		do_label = Cnp->line_lbls.on && flag > NhlLINEONLY;

		if (llcol == NhlTRANSPARENT && do_label) {
			/*
			 * Put spaces in for label.
			 */
			j = MIN(strlen(ts) * 2 + 1,buff_size);
			for(i=0;i < j-1;i+=2){
				tchar[i] = ' ';
				tchar[i+1] = '|';
			}
		}
		else if (do_label) {
			/*
			 * Add breaks in at each space of the label.
			 */
			i=0;
			j=0;
			while (i < buff_size && ts[j] != '\0'){
				if (ts[j] == ' ')
					tchar[i++] = '|';
				tchar[i++] = ts[j++];
			}
			c_pcseti("OC",llcol);
			c_pcseti("CC",llcol);
		}
		c_pcsetr("PH",(float)Cnp->line_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->line_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->line_lbls.cspacing);
		c_pcseti("FN",Cnp->line_lbls.font);
		c_pcseti("QU",Cnp->line_lbls.quality);
		c_pcsetc("FC",Cnp->line_lbls.fcode);
		c_pcsetr("CL",(float)Cnp->line_lbls.thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}

	c_dpsetc("DPT",buffer);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_dpsetr("WOC",(float)Cnp->line_lbls.real_height);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	return;
}

/*
 * Function:  hlucpchhl
 *
 * Description: 
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
void   (_NHLCALLF(hlucpchhl,HLUCPCHHL))
#if	NhlNeedProto
(
	int	*iflg
)
#else
(iflg)
	int	*iflg;
#endif

{
	char buf[128];
	char *fstr,*sub;
	float zdv;

	if (Cnp == NULL) {
		_NHLCALLF(cpchhl,CPCHHL)(iflg);
		return;
	}

#if 0
	{ /* for debugging */
		float flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;
		c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
		printf("getset - %f,%f,%f,%f,%f,%f,%f,%f\n",
		       flx,frx,fby,fuy,wlx,wrx,wby,wuy); 
  	}
#endif
	switch (*iflg) {
	case 1:
		if (! Cnp->high_lbls.on) {
			c_cpsetc("CTM"," ");
			return;
		}
		if ( Cnp->high_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->high_lbls.gks_color);
			c_pcseti("OC", Cnp->high_lbls.gks_color);
		}
		c_pcsetr("PH",(float)Cnp->high_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->high_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->high_lbls.cspacing);
		c_pcseti("FN",Cnp->high_lbls.font);
		c_pcseti("QU",Cnp->high_lbls.quality);
		c_pcsetc("FC",Cnp->high_lbls.fcode);
		gset_linewidth(Cnp->high_lbls.thickness);

		strcpy(buf,(char *)Cnp->high_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_cpgetr("zdv",&zdv);
		zdv /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->high_lbls.format,zdv,NULL,
				       &Cnp->max_data_format.sig_digits,
				       &Cnp->max_data_format.left_sig_digit,
                                       NULL,NULL,NULL,
				       Cnp->high_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_cpsetc("CTM",buf);
		break;
	case 2:
		if (! Cnp->high_lbls.on) return;
		gset_fill_colr_ind(Cnp->high_lbls.gks_bcolor);
		break;
	case 3:
		if (! Cnp->high_lbls.on) {
			c_cpsetc("CTM"," ");
			return;
		}
		if ( Cnp->high_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->high_lbls.gks_color);
			c_pcseti("OC", Cnp->high_lbls.gks_color);
		}
		c_pcsetr("PH",(float)Cnp->high_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->high_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->high_lbls.cspacing);
		c_pcseti("FN",Cnp->high_lbls.font);
		c_pcseti("QU",Cnp->high_lbls.quality);
		c_pcsetc("FC",Cnp->high_lbls.fcode);
		gset_linewidth((float)Cnp->high_lbls.thickness);

		strcpy(buf,(char *)Cnp->high_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_cpgetr("zdv",&zdv);
		zdv /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->high_lbls.format,zdv,NULL,
				       &Cnp->max_data_format.sig_digits,
				       &Cnp->max_data_format.left_sig_digit,
                                       NULL,NULL,NULL,
				       Cnp->high_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_cpsetc("CTM",buf);
		break;
	case 4:
		if (! Cnp->high_lbls.on || ! Cnp->high_lbls.perim_on) 
			return;
		if (Cnp->high_lbls.perim_lcolor == NhlTRANSPARENT)
			gset_line_colr_ind( Cnp->high_lbls.gks_color);
		else
			gset_line_colr_ind(Cnp->high_lbls.gks_plcolor);
		gset_linewidth(Cnp->high_lbls.perim_lthick);
		break;
	case 5:
		if (! Cnp->low_lbls.on) {
			c_cpsetc("CTM"," ");
			return;
		}
		if (Cnp->low_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->low_lbls.gks_color);
			c_pcseti("OC", Cnp->low_lbls.gks_color);
		}
		c_pcsetr("PH",(float)Cnp->low_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->low_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->low_lbls.cspacing);
		c_pcseti("FN",Cnp->low_lbls.font);
		c_pcseti("QU",Cnp->low_lbls.quality);
		c_pcsetc("FC",Cnp->low_lbls.fcode);
		gset_linewidth((float)Cnp->low_lbls.thickness);
		strcpy(buf,(char *)Cnp->low_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_cpgetr("zdv",&zdv);
		zdv /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->low_lbls.format,zdv,NULL,
				       &Cnp->max_data_format.sig_digits,
				       &Cnp->max_data_format.left_sig_digit,
                                       NULL,NULL,NULL,
				       Cnp->low_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_cpsetc("CTM",buf);
		break;
	case 6:
		if (! Cnp->low_lbls.on) return;
		gset_fill_colr_ind(Cnp->low_lbls.gks_bcolor);
		break;
	case 7:
		if (! Cnp->low_lbls.on) {
			c_cpsetc("CTM"," ");
			return;
		}
		if (Cnp->low_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->low_lbls.gks_color);
			c_pcseti("OC", Cnp->low_lbls.gks_color);
		}
		c_pcsetr("PH",(float)Cnp->low_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->low_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->low_lbls.cspacing);
		c_pcseti("FN",Cnp->low_lbls.font);
		c_pcseti("QU",Cnp->low_lbls.quality);
		c_pcsetc("FC",Cnp->low_lbls.fcode);
		gset_linewidth((float)Cnp->low_lbls.thickness);
		strcpy(buf,(char *)Cnp->low_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_cpgetr("zdv",&zdv);
		zdv /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->low_lbls.format,zdv,NULL,
				       &Cnp->max_data_format.sig_digits,
				       &Cnp->max_data_format.left_sig_digit,
                                       NULL,NULL,NULL,
				       Cnp->low_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_cpsetc("CTM",buf);
		break;
	case 8:
		if (! Cnp->low_lbls.on || ! Cnp->low_lbls.perim_on) 
			return;
		if (Cnp->low_lbls.perim_lcolor == NhlTRANSPARENT)
			gset_line_colr_ind( Cnp->low_lbls.gks_color);
		else
			gset_line_colr_ind(Cnp->low_lbls.gks_plcolor);
		gset_linewidth(Cnp->low_lbls.perim_lthick);
		break;
	default:
		break;
	}

	return;
}

/*
 * Function:  hlucpchll
 *
 * Description: 
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
void   (_NHLCALLF(hlucpchll,HLUCPCHLL))
#if	NhlNeedProto
(
	int	*iflg
)
#else
(iflg)
	int	*iflg;
#endif

{

	int pai;
	static int llcol;

	if (Cnp == NULL) {
		_NHLCALLF(cpchll,CPCHLL)(iflg);
		return;
	}
	if (Cnp->llabel_placement == NhlCONSTANT)
		return;

	if (*iflg == 2) {
		if (Cnp->line_lbls.gks_bcolor > NhlTRANSPARENT)
			gset_fill_colr_ind(Cnp->line_lbls.gks_bcolor);
	}
	else if (*iflg == 3) {
		c_cpgeti("PAI", &pai);
		if (pai > 0) {
			pai -= 1;

			llcol = Cnp->line_lbls.mono_color ?
				 Cnp->line_lbls.gks_color : 
					Cnp->line_lbls.colors[pai];
			if (llcol > NhlTRANSPARENT) {
				c_pcseti("CC",llcol);
				c_pcseti("OC",llcol);
			}
			c_pcsetr("PH",(float)Cnp->line_lbls.pheight);
			c_pcsetr("PW",(float)Cnp->line_lbls.pwidth);
			c_pcsetr("CS",(float)Cnp->line_lbls.cspacing);
			c_pcseti("FN",Cnp->line_lbls.font);
			c_pcseti("QU",Cnp->line_lbls.quality);
			c_pcsetc("FC",Cnp->line_lbls.fcode);
			gset_linewidth((float)Cnp->line_lbls.thickness);
		}
	}
	else if (*iflg == 4 && Cnp->line_lbls.perim_on) {
		if (Cnp->line_lbls.perim_lcolor == NhlTRANSPARENT)
			gset_line_colr_ind(llcol);
		else
			gset_line_colr_ind(Cnp->line_lbls.gks_plcolor);
		gset_linewidth(Cnp->line_lbls.perim_lthick);
	}

	return;
}


/* low level overlay mapping functions */

static void OverlayMapXY
#if	NhlNeedProto
(
        NhlTransformLayerPart *tfp,
        float *xin,
        float *yin,
        float* xout,
        float* yout)
#else
(tfp,xin,yin,xout,yout)
	NhlTransformLayerPart *tfp;
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
        int status = 0;

        if (! tfp->overlay_trans_obj ||
            tfp->overlay_trans_obj == tfp->trans_obj) {
		_NhlCompcToWin(tfp->trans_obj,xin,yin,1,xout,yout,
			       &status,NULL,NULL);
	}
        else {
		_NhlCompcToData(tfp->trans_obj,xin,yin,1,xout,yout,
				&status,NULL,NULL);

		if (status) return;
#if 0
		fprintf (stderr,"inter: %f %f : ",*xout,*yout);
#endif

		_NhlDataToWin(tfp->overlay_trans_obj,
			     xout,yout,1,xout,yout,&status,NULL,NULL);
        }

#if 0
	fprintf (stderr,"%f %f : %f %f \n",*xin,*yin,*xout,*yout);
#endif

	return;
}


static void OverlayInvMapXY
#if	NhlNeedProto
(
        NhlTransformLayerPart *tfp,
        float *xin,
        float *yin,
        float* xout,
        float* yout)
#else
(tfp,xin,yin,xout,yout)
	NhlTransformLayerPart *tfp;
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
        int status = 0;

        if (! tfp->overlay_trans_obj ||
            tfp->overlay_trans_obj == tfp->trans_obj) {
		_NhlWinToCompc(tfp->trans_obj,xin,yin,1,xout,yout,
			       &status,NULL,NULL);
	}
        else {
		_NhlWinToData(tfp->overlay_trans_obj,
			      xin,yin,1,xout,yout,
			      &status,NULL,NULL);

		if (status) return;
#if 0
		fprintf (stderr,"inter: %f %f : ",*xout,*yout);
#endif

		_NhlDataToCompc(tfp->trans_obj,xout,yout,1,xout,yout,
				&status,NULL,NULL);
        }

#if 0
	fprintf (stderr,"%f %f : %f %f \n",*xin,*yin,*xout,*yout);
#endif

	return;
}

/*
 * Function:  hlucpmpxy
 *
 * Description: 
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
void   (_NHLCALLF(hlucpmpxy,HLUCPMPXY))
#if	NhlNeedProto
(
	int	*imap,
	float	*xinp,
	float	*yinp,
	float	*xotp,
	float	*yotp
)
#else
(imap,xinp,yinp,xotp,yotp)
	int	*imap;
	float	*xinp;
	float	*yinp;
	float	*xotp;
	float	*yotp;
#endif

{
	int status;

	if (Cnp == NULL) {
		_NHLCALLF(cpmpxy,CPMPXY)(imap,xinp,yinp,xotp,yotp);
		return;
	}

        if (*imap == 0) {
                if ((int) *xinp == NhlcnMAPVAL)
                        *yinp = 3.0;
                else
                        *yinp = 0.0;
        }
        else if (abs(*imap) != NhlcnMAPVAL) {
                *xotp = *xinp;
                *yotp = *yinp;
        }
	else if (Cnl->trans.overlay_status == _tfCurrentOverlayMember &&
		 ! Cnl->trans.do_ndc_overlay) { 
		if (*imap > 0)
			OverlayMapXY(&Cnl->trans,xinp,yinp,xotp,yotp);
		else
			OverlayInvMapXY(&Cnl->trans,xinp,yinp,xotp,yotp);
	}
	else {
		if (*imap > 0)
			_NhlCompcToWin((NhlLayer)Cnp->trans_obj,
				       xinp,yinp,1,xotp,yotp,
				       &status,NULL,NULL);
		else 
			_NhlWinToCompc((NhlLayer)Cnp->trans_obj,
				       xinp,yinp,1,xotp,yotp,
				       &status,NULL,NULL);
	}
	return;
}


/*
 * Function:  load_hlucp_routines
 *
 * Description: Forces the hlucp... routines to load from the HLU library
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
static void   load_hlucp_routines
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
	float fdum;


	if (flag) {
		_NHLCALLF(hlucpmpxy,HLUCPMPXY)(&idum,&fdum,&fdum,&fdum,&fdum);
		_NHLCALLF(hlucpchll,HLUCPCHLL)(&idum);
		_NHLCALLF(hlucpchhl,HLUCPCHHL)(&idum);
		_NHLCALLF(hlucpchcl,HLUCPCHCL)(&idum);
		_NHLCALLF(hlucpscae,HLUCPSCAE)(&idum,&idum,&idum,&idum,
					       &fdum,&fdum,&fdum,&fdum,
					       &idum,&idum,&idum,&idum);
	}
	return;
}

/*
 * Function:  _NhlRasterFill
 *
 * Description: performs a discrete raster fill - 
 * replaces Conpack routine CPCICA - Conpack must be initialized, etc.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

NhlErrorTypes _NhlRasterFill
#if	NhlNeedProto
(
	float 		*zdat,
	int		*cell,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	char		*entry_name
)
#else
(zdat,cell,ica1,icam,ican,
 xcpf,ycpf,xcqf,ycqf,entry_name)
	float		*zdat;
	int		*cell;
	int		ica1;
	int		icam;
	int		ican;
	float		xcpf;
	float		ycpf;
	float		xcqf;
	float		ycqf;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	float		fl,fr,fb,ft,wl,wr,wb,wt;
	int		ll;
	int		start = 1;
	Gint		err_ind;
	Gclip		clip_ind_rect;
	float		xc1,xcm,yc1,ycn;
	float		xmn,xmx,ymn,ymx;
	int		i,j,k,izd1,izdm,izdn,indx,indy,icaf,map,iaid;
	float		xccf,xccu,xccd,xcci,yccf,yccu,yccd,ycci;
	float		zval,orv,spv;
	NhlBoolean	out_of_range;
        float		*levels;

        if (Cnp == NULL) {
		e_text = "%s: invalid call to _NhlRasterFill";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
        }
        levels = (float*) Cnp->levels->data;
        
/* 
 * replacement for CPCICA
 */
	c_cpgetr("XC1",&xc1);
	c_cpgetr("XCM",&xcm);
	c_cpgetr("YC1",&yc1);
	c_cpgetr("YCN",&ycn);
	c_cpgetr("ORV",&orv);
	c_cpgetr("SPV",&spv);
	c_cpgeti("ZDM",&izdm);
	c_cpgeti("ZDN",&izdn);
	c_cpgeti("ZD1",&izd1);
	c_cpgeti("CAF",&icaf);
	c_cpgeti("MAP",&map);
	xmn = MIN(xc1,xcm);
	xmx = MAX(xc1,xcm);
	ymn = MIN(yc1,ycn);
	ymx = MAX(yc1,ycn);

        map = -map;
	for (i = 0; i < icam; i++) {
		int iplus = i+1;
		if (i == 0)
			xccf = xcpf + .75 * ((xcqf-xcpf)/(float)icam);
		else if (i == icam - 1)
			xccf = xcpf + (icam - .75) * 
					((xcqf-xcpf)/(float)icam);
		else
			xccf = xcpf + (i+0.5) * ((xcqf-xcpf)/(float)icam);
		xccu = c_cfux(xccf);

		for (j = 0; j < ican; j++) {
			int jplus = j+1;
			if (j == 0)
		  		yccf = ycpf + .75 * 
				  ((ycqf-ycpf)/(float)ican);
			else if (j == ican - 1)
		  		yccf = ycpf + (ican - .75) * 
				  ((ycqf-ycpf)/(float)ican);
			else
				yccf = ycpf + (j+0.5) * 
				  ((ycqf-ycpf)/(float)ican);
			yccu = c_cfuy(yccf);
			
			out_of_range = False;
			(_NHLCALLF(hlucpmpxy,HLUCPMPXY))
				(&map,&xccu,&yccu,&xccd,&yccd);
			if (xccd == orv ||
			    xccd < xmn || xccd > xmx || 
			    yccd < ymn || yccd > ymx) {
				out_of_range = True;
				iaid = 97;
			}
			else {
				xcci =((xccd-xc1)/(xcm-xc1)) * (float)(izdm-1);
				ycci =((yccd-yc1)/(ycn-yc1)) * (float)(izdn-1);
				indx = (int)(xcci + 0.5);
				indy = (int)(ycci + 0.5);
				if (indx < 0 || indx > izdm-1 ||
				    indy < 0 || indy > izdn-1)
					iaid = 97;
				else if (spv != 0.0 &&
					 *(zdat + indy * izd1 + indx) == spv)
					iaid = 98;
				else {
                                        iaid = 0;
					zval = *(zdat + indy*izd1 + indx);
                                        for (k=0; k < Cnp->level_count; k++) {
                                                if (zval <= levels[k]) {
                                                   iaid = NhlcnAREAID_OFFSET+k;
                                                   break;
                                                }
                                        }
                                        if (iaid == 0) {
                                                iaid = NhlcnAREAID_OFFSET +
                                                        Cnp->level_count;
                                        }
				}
			}
			(_NHLCALLF(hlucpscae,HLUCPSCAE))
			  (cell,&ica1,&icam,&ican,&xcpf,&ycpf,&xcqf,&ycqf,
			   &iplus,&jplus,&icaf,&iaid);
		}
	}

	return NhlNOERROR;
}


