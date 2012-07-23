/*
 *      $Id: Legend.c,v 1.72 2008-05-27 20:55:29 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Legend.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	
 *		
 *		Creates and manages a Legend
 */

#include <math.h>
#include <stdlib.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/LegendP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/hluutil.h>


static char lgDefTitle[] = "NOTHING";

static char Init_Name[] = "LegendInitialize";
static char SetValues_Name[] = "LegendSetValues";


/* SUPPRESS 112 */

#define DEGTORAD 0.017453293

static NhlResource resources[] = {

/* Begin-documented-resources */

{NhlNlgLegendOn, NhlClgLegendOn, NhlTBoolean,
	 sizeof(NhlBoolean), NhlOffset(NhlLegendLayerRec,legend.legend_on),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},  
{NhlNlgOrientation, NhlClgOrientation, NhlTOrientation,
	 sizeof(NhlOrientation), NhlOffset(NhlLegendLayerRec,legend.orient),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlVERTICAL),0,NULL},
{NhlNlgJustification, NhlClgJustification, NhlTJustification, 
	 sizeof(NhlJustification),
	 NhlOffset(NhlLegendLayerRec,legend.just),
	 NhlTImmediate, _NhlUSET((NhlPointer)NhlBOTTOMLEFT),0,NULL},
{NhlNlgBoxMajorExtentF, NhlClgBoxMajorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.box_major_ext),
	 NhlTString, _NhlUSET("0.5"),0,NULL},
{NhlNlgBoxMinorExtentF, NhlClgBoxMinorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.box_minor_ext),
	 NhlTString, _NhlUSET("0.6"),0,NULL},
{NhlNlgItemCount, NhlClgItemCount, NhlTInteger,
	 sizeof(int), NhlOffset(NhlLegendLayerRec,legend.item_count),
	 NhlTImmediate, _NhlUSET((NhlPointer) 16),0,NULL},
{NhlNlgItemPlacement, NhlClgItemPlacement, NhlTlgItemPlacementMode,
	 sizeof(NhllgItemPlacementMode), 
	 NhlOffset(NhlLegendLayerRec,legend.item_placement),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlUNIFORMPLACEMENT),0,NULL},
{NhlNlgBoxBackground, NhlCFillBackgroundColor, NhlTColorIndex,
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLegendLayerRec,legend.box_background),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlTRANSPARENT),0,NULL},

{NhlNlgAutoManage, NhlClgAutoManage, NhlTBoolean,
	 sizeof(NhlBoolean), NhlOffset(NhlLegendLayerRec,legend.auto_manage),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgLabelOffsetF, NhlClgLabelOffsetF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.label_off),
	 NhlTString, _NhlUSET("0.02"),0,NULL},
{NhlNlgTitleOffsetF, NhlClgTitleOffsetF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.title_off),
	 NhlTString, _NhlUSET("0.03"),0,NULL},
{NhlNlgLeftMarginF, NhlClgLeftMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.margin.l),
	 NhlTString, _NhlUSET("0.05"),0,NULL},
{NhlNlgRightMarginF, NhlClgRightMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.margin.r),
	 NhlTString, _NhlUSET("0.05"),0,NULL},
{NhlNlgBottomMarginF, NhlClgBottomMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.margin.b),
	 NhlTString, _NhlUSET("0.05"),0,NULL},
{NhlNlgTopMarginF, NhlClgTopMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.margin.t),
	 NhlTString, _NhlUSET("0.05"),0,NULL},

	
{NhlNlgLineLabelStrings, NhlClgLineLabelStrings, NhlTStringGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLegendLayerRec,legend.line_labels),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlgMonoItemType, NhlClgMonoItemType, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_item_type),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgItemType, NhlClgItemType, NhlTMarkLineMode,
	 sizeof(NhlMarkLineMode), 
	 NhlOffset(NhlLegendLayerRec,legend.item_type),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer)NhlLINES),0,NULL},
{NhlNlgItemTypes, NhlClgItemTypes, NhlTMarkLineModeGenArray,
	 sizeof(NhlPointer), NhlOffset(NhlLegendLayerRec,legend.item_types),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlgMonoDashIndex, NhlClgMonoDashIndex, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_dash_index),
	 NhlTImmediate, _NhlUSET((NhlPointer) False),0,NULL},
{NhlNlgDashIndex,NhlCLineDashPattern,NhlTDashIndex,sizeof(NhlDashIndex),
	 NhlOffset(NhlLegendLayerRec,legend.dash_index),
	 NhlTImmediate,_NhlUSET((NhlPointer)0),0,NULL},
{NhlNlgDashIndexes, NhlClgDashIndexes, NhlTDashIndexGenArray,
	 sizeof(NhlPointer), NhlOffset(NhlLegendLayerRec,legend.dash_indexes),
	 NhlTImmediate, 
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlgMonoMarkerIndex, NhlClgMonoMarkerIndex, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_marker_index),
	 NhlTImmediate, _NhlUSET((NhlPointer) False),0,NULL},
{NhlNlgMarkerIndex,NhlCMarkerIndex,NhlTMarkerIndex,sizeof(NhlMarkerIndex),
	 NhlOffset(NhlLegendLayerRec,legend.marker_index),
	 NhlTImmediate,_NhlUSET((NhlPointer)0),0,NULL},
{NhlNlgMarkerIndexes, NhlClgMarkerIndexes, NhlTMarkerIndexGenArray,
	 sizeof(NhlPointer), NhlOffset(NhlLegendLayerRec,legend.marker_indexes),
	 NhlTImmediate, 
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlgMonoLineColor, NhlClgMonoLineColor, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_line_color),
	 NhlTImmediate, _NhlUSET((NhlPointer) False),0,NULL},
{NhlNlgLineColor,NhlCLineColor,NhlTColorIndex,sizeof(NhlColorIndex),
	 NhlOffset(NhlLegendLayerRec,legend.line_color),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
{NhlNlgLineColors, NhlClgLineColors, NhlTColorIndexGenArray,
	 sizeof(NhlPointer), NhlOffset(NhlLegendLayerRec,legend.line_colors),
	 NhlTImmediate, _NhlUSET((NhlPointer) NULL),0,NULL},
{NhlNlgMonoMarkerColor, NhlClgMonoMarkerColor, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_marker_color),
	 NhlTImmediate, _NhlUSET((NhlPointer) False),0,NULL},
{NhlNlgMarkerColor,NhlCMarkerColor,NhlTColorIndex,sizeof(NhlColorIndex),
	 NhlOffset(NhlLegendLayerRec,legend.marker_color),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
{NhlNlgMarkerColors, NhlClgMarkerColors, NhlTColorIndexGenArray,
	 sizeof(NhlPointer), NhlOffset(NhlLegendLayerRec,legend.marker_colors),
	 NhlTImmediate, _NhlUSET((NhlPointer) NULL),0,NULL},
{NhlNlgMonoLineThickness, NhlClgMonoLineThickness, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_line_thickness),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgLineThicknessF,NhlCLineThicknessF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlLegendLayerRec,legend.line_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlgLineThicknesses, NhlClgLineThicknesses, NhlTFloatGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLegendLayerRec,legend.line_thicknesses),
	 NhlTImmediate, _NhlUSET((NhlPointer) NULL),0,NULL},
{NhlNlgMonoMarkerThickness, NhlClgMonoMarkerThickness, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_marker_thickness),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgMarkerThicknessF,NhlCMarkerThicknessF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlLegendLayerRec,legend.marker_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlgMarkerThicknesses, NhlClgMarkerThicknesses, NhlTFloatGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLegendLayerRec,legend.marker_thicknesses),
	 NhlTImmediate, _NhlUSET((NhlPointer) NULL),0,NULL},
{NhlNlgMonoLineLabelFontHeight, NhlClgMonoLineLabelFontHeight, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_line_label_font_height),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgLineLabelFontHeightF,NhlCFontHeightF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlLegendLayerRec,legend.line_label_font_height),
	 NhlTString,_NhlUSET("0.01"),0,NULL},
{NhlNlgLineLabelFontHeights, NhlClgLineLabelFontHeights, NhlTFloatGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLegendLayerRec,legend.line_label_font_heights),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlgMonoMarkerSize, NhlClgMonoMarkerSize, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_marker_size),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgMarkerSizeF,NhlCMarkerSizeF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlLegendLayerRec,legend.marker_size),
	 NhlTString,_NhlUSET("0.01"),0,NULL},
{NhlNlgMarkerSizes, NhlClgMarkerSizes, NhlTFloatGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLegendLayerRec,legend.marker_sizes),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlgLabelStrings, NhlClgLabelStrings, NhlTStringGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLegendLayerRec,legend.label_strings),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlgItemPositions, NhlClgItemPositions, NhlTFloatGenArray,
	 sizeof(NhlGenArray),
	 NhlOffset(NhlLegendLayerRec,legend.item_positions),
	 NhlTImmediate, _NhlUSET((NhlPointer) NULL ),0,NULL},
{NhlNlgItemOrder, NhlClgItemOrder, NhlTIntegerGenArray,
	 sizeof(NhlGenArray),
	 NhlOffset(NhlLegendLayerRec,legend.item_order),
	 NhlTImmediate, _NhlUSET((NhlPointer) NULL ),0,NULL},
{NhlNlgMonoLineLabelFontColor, NhlClgMonoLineLabelFontColor, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_line_label_color),
	 NhlTImmediate, _NhlUSET((NhlPointer) False),0,NULL},
{NhlNlgLineLabelFontColor,NhlCFontColor,
	 NhlTColorIndex,sizeof(NhlColorIndex),
	 NhlOffset(NhlLegendLayerRec,legend.line_label_color),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
{NhlNlgLineLabelFontColors, NhlClgLineLabelFontColors, NhlTColorIndexGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLegendLayerRec,legend.line_label_colors),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlgMonoLineDashSegLen,NhlClgMonoLineDashSegLen,NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.mono_line_dash_seglen),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgLineDashSegLenF, NhlCLineDashSegLenF, NhlTFloat,sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.line_dash_seglen),
	 NhlTString, _NhlUSET("0.15"),0,NULL},
{NhlNlgLineDashSegLens, NhlClgLineDashSegLens, NhlTFloatGenArray,
	 sizeof(NhlGenArray), 
	 NhlOffset(NhlLegendLayerRec,legend.line_dash_seglens),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},

{NhlNlgLineLabelsOn,NhlClgLineLabelsOn,NhlTBoolean,sizeof(NhlBoolean), 
	 NhlOffset(NhlLegendLayerRec,legend.line_labels_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},

	{ NhlNlgLabelAutoStride, NhlCLabelAutoStride, 
	  NhlTBoolean, sizeof(NhlBoolean),
	  NhlOffset(NhlLegendLayerRec,legend.label_auto_stride),
	  NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},	
{NhlNlgLabelsOn, NhlClgLabelsOn, NhlTBoolean, 
	 sizeof(NhlBoolean), NhlOffset(NhlLegendLayerRec,legend.labels_on),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgLabelPosition, NhlClgLabelPosition, NhlTPosition, 
	 sizeof(NhlPosition), NhlOffset(NhlLegendLayerRec,legend.label_pos),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlRIGHT),0,NULL},
{NhlNlgLabelAngleF, NhlCTextAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.label_angle),
	 NhlTString, _NhlUSET("0.0"),0,NULL},
{NhlNlgLabelAlignment, NhlClgLabelAlignment, NhlTlgLabelAlignmentMode, 
	 sizeof(NhllgLabelAlignmentMode), 
	 NhlOffset(NhlLegendLayerRec,legend.label_alignment),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlITEMCENTERS),0,NULL},
{NhlNlgLabelDirection,NhlCTextDirection,NhlTTextDirection,
	 sizeof(NhlTextDirection),
	 NhlOffset(NhlLegendLayerRec,legend.label_direction),
	 NhlTImmediate, _NhlUSET((NhlPointer)NhlACROSS),0,NULL},
{NhlNlgLabelJust, NhlCTextJustification, NhlTJustification, 
	 sizeof(NhlJustification),
	 NhlOffset(NhlLegendLayerRec,legend.label_just),
	 NhlTImmediate, _NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
{NhlNlgLabelFont, NhlCFont, NhlTFont, 
	 sizeof(int), NhlOffset(NhlLegendLayerRec,legend.label_font),
	 NhlTImmediate, _NhlUSET((NhlPointer) 21),0,NULL},
{NhlNlgLabelFontColor, NhlCFontColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex),NhlOffset(NhlLegendLayerRec,legend.label_color),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlLG_DEF_COLOR),0,NULL},
{NhlNlgLabelFontHeightF, NhlCFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.label_height),
	 NhlTString, _NhlUSET("0.02"),0,NULL},
{NhlNlgLabelFontAspectF, NhlCFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.label_aspect),
	 NhlTString, _NhlUSET("1.3125"),0,NULL},
{NhlNlgLabelFontThicknessF, NhlCFontThicknessF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.label_thickness),
	 NhlTString, _NhlUSET("1.0"),0,NULL},
{NhlNlgLabelFontQuality, NhlCFontQuality, NhlTFontQuality, 
	 sizeof(NhlFontQuality), 
	 NhlOffset(NhlLegendLayerRec,legend.label_quality),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlHIGH),0,NULL},
{NhlNlgLabelConstantSpacingF, NhlCTextConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.label_const_spacing),
	 NhlTString, _NhlUSET("0.0"),0,NULL},
{NhlNlgLabelFuncCode, NhlCTextFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(NhlLegendLayerRec,legend.label_func_code),
	 NhlTString, _NhlUSET("~"),0,NULL},
{NhlNlgLabelStride, NhlClgLabelStride, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLegendLayerRec,legend.label_stride),
	 NhlTImmediate, _NhlUSET((NhlPointer) 1),0,NULL},

{NhlNlgTitleString, NhlClgTitleString, NhlTString, 
	 sizeof(char *), NhlOffset(NhlLegendLayerRec,legend.title_string),
	 NhlTImmediate, _NhlUSET(lgDefTitle),0,(NhlFreeFunc)NhlFree},
{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlLegendLayerRec,legend.title_on_set),
	 NhlTImmediate,_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
{NhlNlgTitleOn, NhlClgTitleOn, NhlTBoolean, 
	 sizeof(NhlBoolean), NhlOffset(NhlLegendLayerRec,legend.title_on),
	 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
{NhlNlgTitlePosition, NhlClgTitlePosition, NhlTPosition, 
	 sizeof(NhlPosition), NhlOffset(NhlLegendLayerRec,legend.title_pos),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlTOP),0,NULL},
{NhlNlgTitleExtentF, NhlClgTitleExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.title_ext),
	 NhlTString, _NhlUSET("0.15"),0,NULL},
{NhlNlgTitleAngleF, NhlCTextAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.title_angle),
	 NhlTString, _NhlUSET("0.0"),0,NULL},
{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlLegendLayerRec,legend.title_direction_set),
	 NhlTImmediate,_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
{NhlNlgTitleDirection,NhlCTextDirection,NhlTTextDirection,
	 sizeof(NhlTextDirection),
	 NhlOffset(NhlLegendLayerRec,legend.title_direction),
	 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
{NhlNlgTitleFont, NhlCFont, NhlTFont, 
	 sizeof(NhlFont), NhlOffset(NhlLegendLayerRec,legend.title_font),
	 NhlTImmediate, _NhlUSET((NhlPointer) 21),0,NULL},
{NhlNlgTitleFontColor, NhlCFontColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex), 
	 NhlOffset(NhlLegendLayerRec,legend.title_color),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlLG_DEF_COLOR),0,NULL},
{NhlNlgTitleJust, NhlCTextJustification, NhlTJustification, 
	 sizeof(NhlJustification),
	 NhlOffset(NhlLegendLayerRec,legend.title_just),
	 NhlTImmediate, _NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
{NhlNlgTitleFontHeightF, NhlCFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.title_height),
	 NhlTString, _NhlUSET("0.025"),0,NULL},
{NhlNlgTitleFontAspectF, NhlCFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.title_aspect),
	 NhlTString, _NhlUSET("1.3125"),0,NULL},
{NhlNlgTitleFontThicknessF, NhlCFontThicknessF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLegendLayerRec,legend.title_thickness),
	 NhlTString, _NhlUSET("1.0"),0,NULL},
{NhlNlgTitleFontQuality, NhlCFontQuality, NhlTFontQuality, 
	 sizeof(NhlFontQuality), 
	 NhlOffset(NhlLegendLayerRec,legend.title_quality),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlHIGH),0,NULL},
{NhlNlgTitleConstantSpacingF, NhlCTextConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.title_const_spacing),
	 NhlTString, _NhlUSET("0.0"),0,NULL},
{NhlNlgTitleFuncCode, NhlCTextFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(NhlLegendLayerRec,legend.title_func_code),
	 NhlTString, _NhlUSET("~"),0,NULL},
	
{NhlNlgBoxLinesOn, NhlClgBoxLinesOn, NhlTBoolean, 
	 sizeof(NhlBoolean), NhlOffset(NhlLegendLayerRec,legend.box_line_on),
	 NhlTImmediate, _NhlUSET((NhlPointer) False),0,NULL},
{NhlNlgBoxLineColor, NhlCLineColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLegendLayerRec,legend.box_line_color),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlLG_DEF_COLOR),0,NULL},
{NhlNlgBoxLineThicknessF, NhlCLineThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.box_line_thickness),
	 NhlTString, _NhlUSET("1.0"),0,NULL},
{NhlNlgBoxLineDashPattern, NhlCLineDashPattern, NhlTDashIndex, 
	 sizeof(NhlDashIndex), 
	 NhlOffset(NhlLegendLayerRec,legend.box_line_dash_pattern),
	 NhlTImmediate, _NhlUSET((NhlPointer) 0),0,NULL},
{NhlNlgBoxLineDashSegLenF, NhlCLineDashSegLenF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.box_line_dash_seglen),
	 NhlTString, _NhlUSET("0.15"),0,NULL},

{NhlNlgPerimOn, NhlCEdgesOn, NhlTBoolean, 
	 sizeof(NhlBoolean), NhlOffset(NhlLegendLayerRec,legend.perim_on),
	 NhlTImmediate, _NhlUSET((NhlPointer) True),0,NULL},
{NhlNlgPerimColor, NhlCEdgeColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex), 
	 NhlOffset(NhlLegendLayerRec,legend.perim_color),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
{NhlNlgPerimFill, NhlCFillPattern, NhlTFillIndex, 
	 sizeof(NhlFillIndex), NhlOffset(NhlLegendLayerRec,legend.perim_fill),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlHOLLOWFILL),0,NULL},
{NhlNlgPerimFillColor, NhlCFillColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLegendLayerRec,legend.perim_fill_color),
	 NhlTImmediate, _NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
{NhlNlgPerimThicknessF, NhlCEdgeThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.perim_thickness),
	 NhlTString, _NhlUSET("1.0"),0,NULL},
{NhlNlgPerimDashPattern, NhlCEdgeDashPattern, NhlTDashIndex, 
	 sizeof(NhlDashIndex), 
	 NhlOffset(NhlLegendLayerRec,legend.perim_dash_pattern),
	 NhlTImmediate, _NhlUSET((NhlPointer) 0),0,NULL}, 
{NhlNlgPerimDashSegLenF, NhlCEdgeDashSegLenF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.perim_dash_seglen),
	 NhlTString, _NhlUSET("0.15"),0,NULL},

{NhlNlgLineLabelFont,NhlCFont,NhlTFont, 
	 sizeof(int),NhlOffset(NhlLegendLayerRec,legend.ll_font),
	 NhlTImmediate,_NhlUSET((NhlPointer) 21),0,NULL},
{NhlNlgLineLabelFontAspectF,NhlCFontAspectF,NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.ll_aspect),
	 NhlTString, _NhlUSET("1.3125"),0,NULL},
{NhlNlgLineLabelFontThicknessF,NhlCFontThicknessF,
	 NhlTFloat,sizeof(float),
	 NhlOffset(NhlLegendLayerRec,legend.ll_thickness),
	 NhlTString, _NhlUSET("1.0"),0,NULL},
{NhlNlgLineLabelFontQuality,NhlCFontQuality,NhlTFontQuality, 
	 sizeof(NhlFontQuality), 
	 NhlOffset(NhlLegendLayerRec,legend.ll_quality),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
{NhlNlgLineLabelConstantSpacingF,NhlCTextConstantSpacingF,
	 NhlTFloat,sizeof(float), 
	 NhlOffset(NhlLegendLayerRec,legend.ll_const_spacing),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNlgLineLabelFuncCode,NhlCTextFuncCode,NhlTCharacter, 
	 sizeof(char),
	 NhlOffset(NhlLegendLayerRec,legend.ll_func_code),
	 NhlTString, _NhlUSET("~"),0,NULL},

/* End-documented-resources */

};

/*
* Base Methods used
*/


static NhlErrorTypes    LegendInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes LegendSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes 	LegendGetValues(
#if	NhlNeedProto
	NhlLayer,		/* l */
	_NhlArgList, 	/* args */
	int		/* num_args */
#endif
);

static NhlErrorTypes	LegendDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes	LegendDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes 	LegendClassInitialize();

/*
* Private functions
*/

static NhlErrorTypes    InitializeDynamicArrays(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    ManageDynamicArrays(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetLegendGeometry(
#if	NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    SetTitle(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetBoxLocations(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);


static NhlErrorTypes    ManageItemPositionsArray(
#if	NhlNeedProto
	float	*item_positions,
	int	count,
	char	*entry_name
#endif
);


static void CreateIntermediates(
#if	NhlNeedProto
	 float		*flist,
	 float		base_val,
	 int		start,
	 int		end,
         int		last
#endif
);

static NhlErrorTypes    SetLabels(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);


static NhlErrorTypes   	AdjustLabels(
#if	NhlNeedProto
	NhlLegendLayerPart	*lg_p,
	NhlLegendLayerPart	*olg_p,
	float		height,
	float		avail_space,
	float		area_x,
	float		area_y,
	char		*entry_name
#endif
);

static NhlErrorTypes    AdjustGeometry(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);


static NhlGenArray GenArraySubsetCopy(
#if	NhlNeedProto
	NhlGenArray	ga,
	int		length
#endif
);

NhlLegendClassRec NhllegendClassRec = {
	{
/* class_name			*/	"legendClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLegendLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlviewClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	LegendClassInitialize,
/* layer_initialize		*/	LegendInitialize,
/* layer_set_values		*/	LegendSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	LegendGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	LegendDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	LegendDraw,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	{
/* segment_workstation */ -1,
/* get_bb */		NULL, /* ---------> Do I need one?<---------*/
	},
	{
			NULL
	}
};

NhlClass NhllegendClass = (NhlClass)&NhllegendClassRec;

/*
 * Function:	nhlflegendclass
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
_NHLCALLF(nhlflegendclass,NHLFLEGENDCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhllegendClass;
}

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark	Qitem_types = NrmNULLQUARK;
static NrmQuark	Qdash_indexes = NrmNULLQUARK;
static NrmQuark	Qmarker_indexes = NrmNULLQUARK;
static NrmQuark	Qline_labels = NrmNULLQUARK;
static NrmQuark	Qline_colors = NrmNULLQUARK;
static NrmQuark	Qmarker_colors = NrmNULLQUARK;
static NrmQuark	Qline_thicknesses = NrmNULLQUARK;
static NrmQuark	Qline_dash_seglens = NrmNULLQUARK;
static NrmQuark	Qmarker_thicknesses = NrmNULLQUARK;
static NrmQuark	Qline_label_font_heights = NrmNULLQUARK;
static NrmQuark	Qmarker_sizes = NrmNULLQUARK;
static NrmQuark	Qlabel_strings = NrmNULLQUARK;
static NrmQuark	Qllabel_colors = NrmNULLQUARK;
static NrmQuark	Qitem_positions = NrmNULLQUARK;
static NrmQuark	Qtitle_string = NrmNULLQUARK;
/*
 * Function:	LegendInitialize
 *
 * Description:	Performs initialization of Legend. 
 *              1) Initialize some internal parameters, and coerce
 *                 several others into the proper boundaries.
 *              2) Copy the view settings.
 *              2) Create a default title string.
 *              3) InitializeDynamicArrays
 *		4) Set up the Legend geometry.
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: 
 */
/*ARGSUSED*/
static NhlErrorTypes    LegendInitialize
#if	NhlNeedProto
	(NhlClass class, 
	 NhlLayer req, 
	 NhlLayer new, 
	 _NhlArgList args,
	 int num_args)
#else
(class,req,new,args,num_args)
	NhlClass	class;
	NhlLayer		req;
	NhlLayer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLegendLayer	tnew = (NhlLegendLayer) new;
	NhlLegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes	ret=NhlNOERROR,ret1 = NhlNOERROR;

	lg_p->new_draw_req = True;
	lg_p->trans_dat = NULL;
	lg_p->labels_id = -1;
	lg_p->title_id = -1;
	lg_p->stride_labels = NULL;
	lg_p->item_locs = NULL;
	lg_p->label_locs = NULL;
	lg_p->title_x = 0.0;
	lg_p->title_y = 1.0;
/*
 * check the line label constant spacing
 */
	if (lg_p->ll_const_spacing < 0.0) {
		lg_p->ll_const_spacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: Constant spacing cannot be less than zero, defaulting %s",
			  Init_Name,NhlNlgLineLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}

/*
 * Ensure that the label and title angles range is between 0 and 360
 */
	lg_p->label_angle = fmod(lg_p->label_angle,360.0);
	lg_p->title_angle = fmod(lg_p->title_angle,360.0);

	if (lg_p->item_count < 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Minimum box count is 1",Init_Name);
		ret = NhlWARNING;
		lg_p->item_count = 1;
	}

/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (! lg_p->title_direction_set) {
		switch (lg_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lg_p->title_direction = NhlACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lg_p->title_direction = NhlDOWN;
			break;
		}
	}
	/*
	 * If the title on flag is not explicitly set, set it based on
	 * whether a string has been supplied. Note that this must
	 * occur before the title is set from SetLegendGeometry. 
	 */
	if (! lg_p->title_on_set) {
		lg_p->title_on = lg_p->title_string == lgDefTitle ?
			False : True;
	}

	lg_p->lg_x = tnew->view.x;
	lg_p->lg_y = tnew->view.y;
	lg_p->lg_width = tnew->view.width;
	lg_p->lg_height = tnew->view.height;

	lg_p->perim.l = lg_p->perim.lxtr = lg_p->lg_x;
	lg_p->perim.r = lg_p->perim.rxtr = lg_p->lg_x + lg_p->lg_width;
	lg_p->perim.b = lg_p->perim.bxtr = lg_p->lg_y - lg_p->lg_height;
	lg_p->perim.t = lg_p->perim.txtr = lg_p->lg_y;

/*
 * Set up array resources
 */

	ret1 = InitializeDynamicArrays(new,req,args,num_args);
	ret = MIN(ret,ret1);

/*
 * Calculate legend geometry
 */

	ret1 = SetLegendGeometry(new,req,True,args,num_args);
	ret = MIN(ret1,ret);

/*
 * Set the box locations
 */
	ret1 = SetBoxLocations(new,req,True,args,num_args);
	ret = MIN(ret1,ret);

/*
 * Set up the labels using a multitext object
 */

	ret1 = SetLabels(new,req,True,args,num_args);
	ret = MIN(ret1,ret);

/*
  Adjust the geometry
  */

	ret1 = AdjustGeometry(new,req,True,args,num_args);
	ret = MIN(ret1,ret);
	

	return(MIN(ret,ret1));
}

/*
 * Function:	LegendSetValues
 *
 * Description: Handles setting all the Legend object resources. Calls a
 *	number of internal subroutines to deal with each phase of the 
 *	operation of setting up the Legend object. Calls many of the same
 *	routines used by the Legend initialize call.
 *	Note: the Legend object has not been performance tuned. It is
 *	possible that a performance saving could result from eliminating
 *	some calls under certain conditions. 
 *
 * In Args:	Standard SetValues args
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: 
 */
/*ARGSUSED*/
static NhlErrorTypes LegendSetValues
#if	NhlNeedProto
	(NhlLayer old,
	NhlLayer reference,
	NhlLayer new,
	_NhlArgList args,
	int num_args)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int	num_args;
#endif
{
	NhlLegendLayer		told = (NhlLegendLayer) old;
	NhlLegendLayer		tnew = (NhlLegendLayer) new;
	NhlLegendLayerPart	*lg_p = &(tnew->legend);
	NhlErrorTypes		ret = NhlNOERROR,ret1 = NhlNOERROR;
	char			*entry_name = SetValues_Name;
	int			view_args = 0;
	NhlBoolean		do_scaling = False;
	float			tx = 1.0, ty = 1.0;
	float			lxtr,rxtr,bxtr,txtr;

	if (tnew->view.use_segments != told->view.use_segments) {
		lg_p->new_draw_req = True;
	}
	if (_NhlArgIsSet(args,num_args,NhlNvpXF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpYF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpOn)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpWidthF)) {
		view_args++;
		do_scaling = True;
	}
	if (_NhlArgIsSet(args,num_args,NhlNvpHeightF)) {
		view_args++;
		do_scaling = True;
	}

	if (num_args > view_args ||
            ! _NhlSegmentSpansArea(lg_p->trans_dat,
                                   tnew->view.x,
                                   tnew->view.x + tnew->view.width,
                                   tnew->view.y - tnew->view.height,
                                   tnew->view.y))
		lg_p->new_draw_req = True;

	if (lg_p->item_count < 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Minimum box count is 1", entry_name);
		ret = NhlWARNING;
		lg_p->item_count = 1;
	}

/*
 * check the line label constant spacing
 */
	if (lg_p->ll_const_spacing < 0.0) {
		lg_p->ll_const_spacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: Constant spacing cannot be less than zero, defaulting %s",
			  entry_name,NhlNlgLineLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}
	
/*
 * Ensure that the label and title angles range is between 0 and 360
 */
	lg_p->label_angle = fmod(lg_p->label_angle,360.0);

	lg_p->title_angle = fmod(lg_p->title_angle,360.0);

/*
 * if the title string is set, turn on the title unless it is simultaneously
 * explicitly turned off.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlgTitleString) &&
	    !_NhlArgIsSet(args,num_args,NhlNlgTitleOn))
		lg_p->title_on = True;
/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlgTitlePosition) &&
	    !_NhlArgIsSet(args,num_args,NhlNlgTitleDirection)) {
		switch (lg_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lg_p->title_direction = NhlACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lg_p->title_direction = NhlDOWN;
			break;
		}
	}

	lg_p->lg_x = tnew->view.x;
	lg_p->lg_y = tnew->view.y;
	lg_p->lg_width = tnew->view.width;
	lg_p->lg_height = tnew->view.height;

	if (do_scaling) {
		tx = tnew->view.width / told->view.width;
		ty = tnew->view.height / told->view.height;

		if (! _NhlArgIsSet(args,num_args,NhlNlgLabelFontHeightF)) {
			if (lg_p->label_direction == NhlACROSS)
				lg_p->label_height *= tx;
			else 
				lg_p->label_height *= ty;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNlgTitleFontHeightF)) {
			if (lg_p->title_direction == NhlACROSS)
				lg_p->title_height *= tx;
			else
				lg_p->title_height *= ty;
		}
	}

	lxtr = tx * (lg_p->perim.l - lg_p->perim.lxtr);
	rxtr = tx * (lg_p->perim.rxtr - lg_p->perim.r);
	bxtr = ty * (lg_p->perim.b - lg_p->perim.bxtr);
	txtr = tx * (lg_p->perim.txtr - lg_p->perim.t);
	lg_p->perim.l = lg_p->lg_x + lxtr;
	lg_p->perim.r = lg_p->lg_x + lg_p->lg_width - rxtr;
	lg_p->perim.b = lg_p->lg_y - lg_p->lg_height + bxtr;
	lg_p->perim.t = lg_p->lg_y - txtr;
	lg_p->perim.lxtr = lg_p->lg_x;
	lg_p->perim.rxtr = lg_p->perim.r + rxtr;
	lg_p->perim.bxtr = lg_p->perim.b - bxtr;
	lg_p->perim.txtr = lg_p->lg_y;

/*
 * Return now if using segments and only the view has changed
 */
	if (tnew->view.use_segments && ! lg_p->new_draw_req) {
		return ret;
	}
	if (view_args == num_args &&
	    (! (tnew->view.on && lg_p->legend_on))) {
		return ret;
	}

	ret1 = ManageDynamicArrays(new,old,args,num_args);
	ret = MIN(ret,ret1);

	ret1 = SetLegendGeometry(new,old,False,args,num_args);
	ret = MIN(ret1,ret);

	ret1 = SetBoxLocations(new,old,False,args,num_args);
	ret = MIN(ret1,ret);

	ret1 = SetLabels(new,old,False,args,num_args);
	ret = MIN(ret1,ret);

	ret1 = AdjustGeometry(new,old,False,args,num_args);
	ret = MIN(ret1,ret);

        return(MIN(ret,ret1));
}

/*
 * Function:  InitializeDynamicArrays
 *
 * Description: Creates internal copies of each of the Legend GenArrays and
 *	populates the copies with the values specified via a LegendCreate
 *	call. Assigns default values and sizes to any array whose resource
 *	the caller has not set.
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
static NhlErrorTypes    InitializeDynamicArrays
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLegendLayer	tnew = (NhlLegendLayer) new;
	NhlLegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes ret = NhlNOERROR, ret_1;
	int i;
	ng_size_t  count;
	int len_1, len_2;
	char number[10];
	NhlGenArray ga;
	char *entry_name = Init_Name;
	char *e_text;
	float *f_p;
	int *i_p;
	NhlString *s_p;
	float tfloat;

/*=======================================================================*/
/* Handle the item types array first: the item indexes array depends on
 * it being set up correctly. Note that the data ownership flag is 
 * explicitly set so that the gen array free routine will free the data.
 */

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgItemTypes);
		return NhlFATAL;
	}
	for (i=0; i < count; i++) 
		i_p[i] = NhlLINES;

	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTMarkLineMode,
			    sizeof(NhlMarkLineMode),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgItemTypes);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an item types resource array has been passed in, copy it to the ga */

	if (lg_p->item_types != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->item_types,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgItemTypes, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->item_types = ga;

/*=======================================================================*/

/*
 * The item indexes array is complicated by the fact that the default value
 * may be different, depending on the item type. Therefore it is necessary
 * to pass in an artificial default value to the Validated copy routine
 */
	NhlVAGetValues(tnew->base.wkptr->base.id,
		     NhlNwkDashTableLength, &len_1,
		     NhlNwkMarkerTableLength, &len_2, NULL);

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);

/*
 *	First do Dash Indexes...
 */
	i_p = (int *) NhlMalloc(count * sizeof(int));
	if(!i_p){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}

	for (i=0; i<count; i++)
		i_p[i] = NhlLG_MIN_LINE_INDEX + i;
	
	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTDashIndex,
				    sizeof(NhlDashIndex),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgDashIndexes);
		return NhlFATAL;
	}
	ga->my_data = True;

	if (lg_p->dash_indexes != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->dash_indexes,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgDashIndexes, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->dash_indexes = ga; 

/*
 *	Now do Marker Indexes...
 */
	i_p = (int *) NhlMalloc(count * sizeof(int));
	if(!i_p){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	for (i=0; i<count; i++)
		i_p[i] = NhlLG_MIN_MARKER_INDEX + i;

	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTMarkerIndex,
				    sizeof(NhlMarkerIndex),
				    1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgMarkerIndexes);
		return NhlFATAL;
	}
	ga->my_data = True;

	if (lg_p->marker_indexes != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->marker_indexes,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgMarkerIndexes, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->marker_indexes = ga; 

/*=======================================================================*/

/* 
 * The label strings array: 
 * Subsidary arrays managed in SetLabels hold the
 * position of labels that will be drawn, and copies of pointers to
 * the strings selected by the stride function. The two variables 
 * initialized to 0 here, keep track of the currently allocated size of
 * these arrays (called label_locs and stride_labels).
 */

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((s_p = (NhlString *) 
	     NhlMalloc(count * sizeof(NhlString))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineLabelStrings);
		return NhlFATAL;
	}
	for (i=0;i<count;i++) {
		sprintf(number,"%d",i);
		if ((s_p[i] = (char *)
		     NhlMalloc(strlen(NhlLG_DEF_ITEM_STRING) + 
			       strlen(number) + 1)) == NULL) {
			e_text = "%s: error creating %s string";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineLabelStrings);
			return NhlFATAL;
		}
		strcpy(s_p[i], 
		       (Const char *)NhlLG_DEF_ITEM_STRING);
		strcat(s_p[i],number);
	}

	if ((ga = NhlCreateGenArray((NhlPointer)s_p,NhlTString,
				    sizeof(NhlString),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineLabelStrings);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an item strings resource array has been passed in, copy it to the ga */

	if (lg_p->line_labels != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->line_labels,
						  NhlLG_MAX_LBL_STRINGS,
						  True,False,
						  NhlNlgLineLabelStrings, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->line_labels = ga;
/*
 * Copy function should have replaced any NULL strings - nothing else is
 * invalid.
 */

/*=======================================================================*/
/* 
 * Initialize the color array starting with the foreground color (index 1):  
 * Create an array that contains the larger of the default box
 * count and the supplied box count. Fill it with the default color array 
 * values up to the default box count size, and the single default color value
 * for the rest of the elements. 
 * Then if the user has supplied a generic array copy it over the 
 * created array. Then check each element to ensure that
 * it is a valid color index.
 */

	/*
	 * first do line colors, then marker colors.
	 */
	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineColors);
		return NhlFATAL;
	}
	for (i=0; i < count; i++)
		i_p[i] = i + 1;
			
	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTColorIndex,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineColors);
		return NhlFATAL;
	}
	ga->my_data = True;

	if (lg_p->line_colors != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->line_colors,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->line_colors = ga;

	/*
	 * Now do marker colors.
	 */
	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgMarkerColors);
		return NhlFATAL;
	}
	for (i=0; i < count; i++)
		i_p[i] = i + 1;
			
	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTColorIndex,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgMarkerColors);
		return NhlFATAL;
	}
	ga->my_data = True;

	if (lg_p->marker_colors != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->marker_colors,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgMarkerColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->marker_colors = ga;

/*=======================================================================*/

/* The line seg_lens array */

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineDashSegLens);
		return NhlFATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 0.15;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineDashSegLens);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an line seg_lens resource array has been passed in, 
 * copy it to the ga */

	if (lg_p->line_dash_seglens != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->line_dash_seglens,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineDashSegLens, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->line_dash_seglens = ga;
/*
 * Replace any invalid elements in the array with the default value
 */
	f_p = (float *) lg_p->line_dash_seglens->data;
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
      "%s: %s index %d holds an invalid line dash segment length: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineDashSegLens, i);
		        ret = MIN(ret, NhlWARNING);
			f_p[i] = 0.15;
		}
	}

/*=======================================================================*/

/* The line thicknesses array */

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineThicknesses);
		return NhlFATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 1.0;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineThicknesses);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an line thicknesses resource array has been passed in, 
 * copy it to the ga */

	if (lg_p->line_thicknesses != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->line_thicknesses,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineThicknesses, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->line_thicknesses = ga;
/*
 * Replace any invalid elements in the array with the default value
 */
	f_p = (float *) lg_p->line_thicknesses->data;
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
	       "%s: %s index %d holds an invalid line thickness: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineThicknesses, i);
		        ret = MIN(ret, NhlWARNING);
			f_p[i] = 1.0;
		}
	}

/*=======================================================================*/

/* The marker thicknesses array */

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgMarkerThicknesses);
		return NhlFATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 1.0;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgMarkerThicknesses);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an item thicknesses resource array has been passed in, 
 * copy it to the ga */

	if (lg_p->marker_thicknesses != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->marker_thicknesses,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgMarkerThicknesses, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->marker_thicknesses = ga;
/*
 * Replace any invalid elements in the array with the default value
 */
	f_p = (float *) lg_p->marker_thicknesses->data;
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
	       "%s: %s index %d holds an invalid item thickness: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgMarkerThicknesses, i);
		        ret = MIN(ret, NhlWARNING);
			f_p[i] = 1.0;
		}
	}

/*=======================================================================*/

/* The line_label_font_heights array */

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineLabelFontHeights);
		return NhlFATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 0.01;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineLabelFontHeights);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an line_label text heights resource array has been passed in, 
 * copy it to the ga */

	if (lg_p->line_label_font_heights != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,
						lg_p->line_label_font_heights,
						NhlLG_MAX_ITEMS,True,False,
						NhlNlgLineLabelFontHeights, 
						entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->line_label_font_heights = ga;
/*
 * Replace any invalid elements in the array with the default value
 */
	f_p = (float *) lg_p->line_label_font_heights->data;
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
       "%s: %s index %d holds an invalid line label thickness: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineLabelFontHeights, i);
		        ret = MIN(ret, NhlWARNING);
			f_p[i] = 0.01;
		}
	}

/*=======================================================================*/

/* The marker_sizes array */

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgMarkerSizes);
		return NhlFATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 0.01;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgMarkerSizes);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an item text heights resource array has been passed in, 
 * copy it to the ga */

	if (lg_p->marker_sizes != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->marker_sizes,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgMarkerSizes, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->marker_sizes = ga;
/*
 * Replace any invalid elements in the array with the default value
 */
	f_p = (float *) lg_p->marker_sizes->data;
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
	       "%s: %s index %d holds an invalid item thickness: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgMarkerSizes, i);
		        ret = MIN(ret, NhlWARNING);
			f_p[i] = 0.01;
		}
	}

/*=======================================================================*/

/* 
 * The label strings array: 
 * Subsidary arrays managed in SetLabels hold the
 * position of labels that will be drawn, and copies of pointers to
 * the strings selected by the stride function. The two variables 
 * initialized to 0 here, keep track of the currently allocated size of
 * these arrays (called label_locs and stride_labels).
 */

	lg_p->max_label_stride_count = 0;
	lg_p->max_label_draw_count = 0;

	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((s_p = (NhlString *) 
	     NhlMalloc(count * sizeof(NhlString))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLabelStrings);
		return NhlFATAL;
	}
	for (i=0;i<count;i++) {
		sprintf(number,"%d",i);
		if ((s_p[i] = (char *)
		     NhlMalloc(strlen(NhlLG_DEF_STRING) + 
			       strlen(number) + 1)) == NULL) {
			e_text = "%s: error creating %s string";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLabelStrings);
			return NhlFATAL;
		}
		strcpy(s_p[i], NhlLG_DEF_STRING);
		strcat(s_p[i], number);
	}

	if ((ga = NhlCreateGenArray((NhlPointer)s_p,NhlTString,
				    sizeof(NhlString),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLabelStrings);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If a label strings resource array has been passed in, copy it to the ga */

	if (lg_p->label_strings != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->label_strings,
						  NhlLG_MAX_LBL_STRINGS,
						  True,False,
						  NhlNlgLabelStrings, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->label_strings = ga;
/*
 * Copy function should have replaced any NULL strings - nothing else is
 * invalid.
 */

/*=======================================================================*/
/* 
 * Create an array that contains the larger of the default box
 * count and the supplied box count. Fill it with the default color array 
 * values up to the default box count size, and the single default color value
 * for the rest of the elements. 
 * Then if the user has supplied a generic array copy it over the 
 * created array.
 */
	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineLabelFontColors);
		return NhlFATAL;
	}

	for (i=0; i < count; i++)
		i_p[i] = i + 1;
			
	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTColorIndex,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgLineLabelFontColors);
		return NhlFATAL;
	}
	ga->my_data = True;

	if (lg_p->line_label_colors != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->line_label_colors,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineLabelFontColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->line_label_colors = ga;

/*=======================================================================*/
/* 
 * Set up the box_fraction array. If no array
 * is provided create a uniform array, allowing future modification of 
 * the placement. Set it up whether or not it is going to be used.
 */
	
	count = MAX(lg_p->item_count, NhlLG_DEF_ITEM_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgItemPositions);
		return NhlFATAL;
	}
	
	tfloat = 1.0/((float) lg_p->item_count);
	f_p[0] = tfloat/2.0;

	for (i=1;i<lg_p->item_count; i++)
		f_p[i] = f_p[0] + (float) i * tfloat;
	
	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlgItemPositions);
		return NhlFATAL;
	}
	ga->my_data = True;
		
/* If a box fractions array has been passed in, copy it to the ga */

	if (lg_p->item_positions != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lg_p->item_positions,
						  NhlLG_MAX_ITEMS,
						  True,False,
						  NhlNlgItemPositions, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lg_p->item_positions = ga;
	     
/*=======================================================================*/
/*
 * Allocate the location array: use one more than
 * the current box count so that both ends of the legend can be stored
 */

	lg_p->item_locs = (float *) 
		NhlMalloc((lg_p->item_count+1) * sizeof(float));
	if (! lg_p->item_locs) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
	}

/*=======================================================================*/
/* 
 * If an item order array has been passed in, copy it to the ga
 * Note there is no default array, if not set it remains NULL
 */
	if (lg_p->item_order != NULL) {
		count = lg_p->item_count;
		if (lg_p->item_order->num_elements != count) {
			lg_p->item_order = NULL;
			e_text = "%s: %s must contain %d elements; ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemOrder,count);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}
		i_p = (int *)lg_p->item_order->data;
		for (i = 0; i < count; i++) {
			int j, found = 0;
			for (j = 0; j < count; j++) {
				if (i_p[j] == i) {
					found = 1;
					break;
				}
			}
			if (! found) {
				lg_p->item_order = NULL;
				e_text = "%s: %s must contain unique values 0 - %d; ignoring";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
					  NhlNlgItemOrder,count-1);
				ret = MIN(ret,NhlWARNING);
				return ret;
			}
		}
		if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
			e_text = "%s: error creating %s array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemOrder);
			return NhlFATAL;
		}
		memcpy(i_p,lg_p->item_order->data,count * sizeof(int));
		if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
			e_text = "%s: error creating %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemOrder);
			return NhlFATAL;
		}
		ga->my_data = True;
		lg_p->item_order = ga;
	}
	     
	
	return (ret);
}


/*
 * Function:    ManageDynamicArrays
 *
 * Description:	Handles changes to any of the Legend object GenArrays after
 *	their initial creation. 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via LegendSetValues
 */

/*ARGSUSED*/
static NhlErrorTypes    ManageDynamicArrays
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLegendLayer	tnew = (NhlLegendLayer) new;
	NhlLegendLayer	told = (NhlLegendLayer) old;
	NhlLegendLayerPart *lg_p = &(tnew->legend);
	NhlLegendLayerPart *olg_p = &(told->legend);
	NhlErrorTypes ret = NhlNOERROR, ret_1 = NhlNOERROR;
	int i;
	int len_1, len_2;
	ng_size_t count;
	char number[10];
	char *entry_name = SetValues_Name;
	char *e_text;
	float *f_p;
	int *i_p;
	NhlString *s_p;

/*=======================================================================*/
/* 
 * Handle the item types array first: the item indexes array depends on
 * it being set up correctly.
 */


	count = lg_p->item_count;

	if (_NhlArgIsSet(args,num_args,NhlNlgItemTypes)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->item_types),
						  lg_p->item_types,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgItemTypes, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->item_types = olg_p->item_types;
		olg_p->item_types = NULL;

	}

	if (lg_p->item_types->num_elements < count) {
		i_p = (int *) lg_p->item_types->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemTypes);
			return NhlFATAL;
		}
		for (i=lg_p->item_types->num_elements; i<count; i++) {
			i_p[i] = NhlLINES;
		}

		lg_p->item_types->data = (NhlPointer) i_p;
		lg_p->item_types->num_elements = count;
	}

/*======================================================================*/

/*
 * Copy the new information if the resource has changed. 
 * If number of label boxes has increased, gcheck all added values for a
 * valid pattern index, setting the default pattern index if an invalid
 * value is found.
 */
	
	
	NhlVAGetValues(tnew->base.wkptr->base.id,
		     NhlNwkDashTableLength, &len_1,
		     NhlNwkMarkerTableLength, &len_2, NULL);

	if (_NhlArgIsSet(args,num_args,NhlNlgDashIndexes)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->dash_indexes),
						  lg_p->dash_indexes,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgDashIndexes,
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;

		lg_p->dash_indexes = olg_p->dash_indexes;
		olg_p->dash_indexes = NULL;
		i_p = (int *) lg_p->dash_indexes->data;
		count = MIN(lg_p->item_count,lg_p->dash_indexes->num_elements);

		for (i=0; i<count; i++)
			if (i_p[i] < NhlLG_MIN_LINE_INDEX) 
				i_p[i] = NhlLG_DEF_LINE_INDEX;
	}
	if (lg_p->dash_indexes->num_elements < lg_p->item_count) {
		count = lg_p->dash_indexes->num_elements;
		i_p = (int *) lg_p->dash_indexes->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, 
				lg_p->item_count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgDashIndexes);
			return NhlFATAL;
		}
		for (i=count;i<lg_p->item_count;i++) 
			i_p[i] = i + 1;

		lg_p->dash_indexes->data = (NhlPointer) i_p;
		lg_p->dash_indexes->num_elements = lg_p->item_count;
	}

	if (_NhlArgIsSet(args,num_args,NhlNlgMarkerIndexes)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->marker_indexes),
						  lg_p->marker_indexes,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgMarkerIndexes,
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;

		lg_p->marker_indexes = olg_p->marker_indexes;
		olg_p->marker_indexes = NULL;
		i_p = (int *) lg_p->marker_indexes->data;
		count = MIN(lg_p->item_count,
					lg_p->marker_indexes->num_elements);

		for (i=0; i<count; i++)
			if (i_p[i] < NhlLG_MIN_MARKER_INDEX)
				i_p[i]= NhlLG_DEF_MARKER_INDEX;
	}
	if (lg_p->marker_indexes->num_elements < lg_p->item_count) {
		count = lg_p->marker_indexes->num_elements;
		i_p = (int *) lg_p->marker_indexes->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, 
				lg_p->item_count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgMarkerIndexes);
			return NhlFATAL;
		}
		for (i=count;i<lg_p->item_count;i++) 
			i_p[i] = i + 1;

		lg_p->marker_indexes->data = (NhlPointer) i_p;
		lg_p->marker_indexes->num_elements = lg_p->item_count;
	}
	
/*=======================================================================*/

/* 
 * Handle the item strings. Copy the new array into the old.
 * NULL strings generate an error message and are replaced by empty 
 * (single byte null terminator) strings within the Validated copy function. 
 * If the box count has increased, but the string gen array does not 
 * contain enough elements, the array is resized and default strings are
 * created for the additional elements.
 */

	if (_NhlArgIsSet(args,num_args,NhlNlgLineLabelStrings)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->line_labels),
						  lg_p->line_labels,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineLabelStrings,
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->line_labels = olg_p->line_labels;
		olg_p->line_labels = NULL;
	}

	if (lg_p->line_labels->num_elements < lg_p->item_count) {
		s_p = (NhlString *) lg_p->line_labels->data;
		if ((s_p = (NhlString *)
		     NhlRealloc(s_p, lg_p->item_count * 
				sizeof (NhlString))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineLabelStrings);
			return NhlFATAL;
		}
		for (i=lg_p->line_labels->num_elements; 
		     i<lg_p->item_count; i++) {
			sprintf(number,"%d",i);
			if ((s_p[i] = (char *)
			     NhlMalloc(strlen(NhlLG_DEF_ITEM_STRING) + 
				       strlen(number) + 1)) == NULL) {
				e_text = "%s: error creating %s string";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlgLineLabelStrings);
				return NhlFATAL;
			}
			strcpy(s_p[i],(Const char *)NhlLG_DEF_ITEM_STRING);
			strcat(s_p[i],number);
		}

		lg_p->line_labels->data = (NhlPointer) s_p;
		lg_p->line_labels->num_elements = lg_p->item_count;
	}

/*=======================================================================*/
/*
 * Manage the colors array: if the array has changed copy the new
 * array elements.
 * Then if the box count is greater than the current array size, enlarge
 * the array and give initial values to the new elements.
 */

	/*
	 * First Line colors, then Marker colors.
	 */
	if (_NhlArgIsSet(args,num_args,NhlNlgLineColors)) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->line_colors),
						  lg_p->line_colors,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->line_colors = olg_p->line_colors;
		olg_p->line_colors = NULL;
		
		i_p = (int *) lg_p->line_colors->data;
	}

	if (lg_p->line_colors->num_elements < count) {
		i_p = (int *) lg_p->line_colors->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineColors);
			return NhlFATAL;
		}
		for (i=lg_p->line_colors->num_elements; i<count; i++) {
			i_p[i] = i + 1;
		}

		lg_p->line_colors->data = (NhlPointer) i_p;
		lg_p->line_colors->num_elements = count;
	}

	/*
	 * Now marker colors.
	 */
	if (_NhlArgIsSet(args,num_args,NhlNlgMarkerColors)) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->marker_colors),
						  lg_p->marker_colors,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgMarkerColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->marker_colors = olg_p->marker_colors;
		olg_p->marker_colors = NULL;
		
		i_p = (int *) lg_p->marker_colors->data;
	}

	if (lg_p->marker_colors->num_elements < count) {
		i_p = (int *) lg_p->marker_colors->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgMarkerColors);
			return NhlFATAL;
		}
		for (i=lg_p->marker_colors->num_elements; i<count; i++) {
			i_p[i] = i + 1;
		}

		lg_p->marker_colors->data = (NhlPointer) i_p;
		lg_p->marker_colors->num_elements = count;
	}
/*=======================================================================*/
/* 
 * Handle the line_dash_seg_len
 */

	count = lg_p->item_count;

	if (_NhlArgIsSet(args,num_args,NhlNlgLineDashSegLens)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->line_dash_seglens),
						  lg_p->line_dash_seglens,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineDashSegLens, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->line_dash_seglens = olg_p->line_dash_seglens;
		olg_p->line_dash_seglens = NULL;
		f_p = (float *) lg_p->line_dash_seglens->data;

		for (i=0; i<MIN(count,
				lg_p->line_dash_seglens->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
     "%s: %s index %d holds an invalid line dash segment length: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlgLineDashSegLens,i);
				ret = MIN(ret, NhlWARNING);
				f_p[i] = 0.15;
			}
		}
	}

	if (lg_p->line_dash_seglens->num_elements < count) {
		f_p = (float *) lg_p->line_dash_seglens->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineDashSegLens);
			return NhlFATAL;
		}
		for (i=lg_p->line_dash_seglens->num_elements; i<count; i++) {
			f_p[i] = 0.15;
		}

		lg_p->line_dash_seglens->data = (NhlPointer) f_p;
		lg_p->line_dash_seglens->num_elements = count;
	}

/*=======================================================================*/
/* 
 * Handle the line_thickness
 */

	count = lg_p->item_count;

	if (_NhlArgIsSet(args,num_args,NhlNlgLineThicknesses)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->line_thicknesses),
						  lg_p->line_thicknesses,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineThicknesses, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->line_thicknesses = olg_p->line_thicknesses;
		olg_p->line_thicknesses = NULL;
		f_p = (float *) lg_p->line_thicknesses->data;

		for (i=0; i<MIN(count,
				lg_p->line_thicknesses->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
        "%s: %s index %d holds an invalid line thickness value: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlgLineThicknesses,i);
				ret = MIN(ret, NhlWARNING);
				f_p[i] = 1.0;
			}
		}
	}

	if (lg_p->line_thicknesses->num_elements < count) {
		f_p = (float *) lg_p->line_thicknesses->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineThicknesses);
			return NhlFATAL;
		}
		for (i=lg_p->line_thicknesses->num_elements; i<count; i++) {
			f_p[i] = 1.0;
		}

		lg_p->line_thicknesses->data = (NhlPointer) f_p;
		lg_p->line_thicknesses->num_elements = count;
	}

/*=======================================================================*/
/* 
 * Handle the marker_thickness
 */

	count = lg_p->item_count;

	if (_NhlArgIsSet(args,num_args,NhlNlgMarkerThicknesses)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->marker_thicknesses),
						  lg_p->marker_thicknesses,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgMarkerThicknesses, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->marker_thicknesses = olg_p->marker_thicknesses;
		olg_p->marker_thicknesses = NULL;
		f_p = (float *) lg_p->marker_thicknesses->data;

		for (i=0; i<MIN(count,
				lg_p->marker_thicknesses->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
        "%s: %s index %d holds an invalid marker thickness value: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlgMarkerThicknesses,i);
				ret = MIN(ret, NhlWARNING);
				f_p[i] = 1.0;
			}
		}
	}

	if (lg_p->marker_thicknesses->num_elements < count) {
		f_p = (float *) lg_p->marker_thicknesses->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgMarkerThicknesses);
			return NhlFATAL;
		}
		for (i=lg_p->marker_thicknesses->num_elements; i<count; i++) {
			f_p[i] = 1.0;
		}

		lg_p->marker_thicknesses->data = (NhlPointer) f_p;
		lg_p->marker_thicknesses->num_elements = count;
	}

/*=======================================================================*/
/* line label text height is handled similarly to thickness */

	count = lg_p->item_count;

	if (_NhlArgIsSet(args,num_args,NhlNlgLineLabelFontHeights)){
		
		ret_1 = _NhlValidatedGenArrayCopy(
					&(olg_p->line_label_font_heights),
					lg_p->line_label_font_heights,
					NhlLG_MAX_ITEMS,True,False,
					NhlNlgLineLabelFontHeights, 
					entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->line_label_font_heights = olg_p->line_label_font_heights;
		olg_p->line_label_font_heights = NULL;
		f_p = (float *) lg_p->line_label_font_heights->data;

		for (i=0; i<MIN(count,
			lg_p->line_label_font_heights->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
      "%s: %s index %d holds an invalid line label thickness value: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlgLineLabelFontHeights,
					  i);
				ret = MIN(ret, NhlWARNING);
				f_p[i] = 0.01;
			}
		}
	}

	if (lg_p->line_label_font_heights->num_elements < count) {
		f_p = (float *) lg_p->line_label_font_heights->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineLabelFontHeights);
			return NhlFATAL;
		}
		for(i=lg_p->line_label_font_heights->num_elements;i<count;i++){
			f_p[i] = 0.01;
		}

		lg_p->line_label_font_heights->data = (NhlPointer) f_p;
		lg_p->line_label_font_heights->num_elements = count;
	}

/*=======================================================================*/
/* Marker size is handled similarly to thickness */

	count = lg_p->item_count;

	if (_NhlArgIsSet(args,num_args,NhlNlgMarkerSizes)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->marker_sizes),
						  lg_p->marker_sizes,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgMarkerSizes, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->marker_sizes = olg_p->marker_sizes;
		olg_p->marker_sizes = NULL;
		f_p = (float *) lg_p->marker_sizes->data;

		for (i=0; i<MIN(count,
				lg_p->marker_sizes->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
        "%s: %s index %d holds an invalid item thickness value: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlgMarkerSizes,i);
				ret = MIN(ret, NhlWARNING);
				f_p[i] = 0.01;
			}
		}
	}

	if (lg_p->marker_sizes->num_elements < count) {
		f_p = (float *) lg_p->marker_sizes->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgMarkerSizes);
			return NhlFATAL;
		}
		for (i=lg_p->marker_sizes->num_elements; i<count; i++) {
			f_p[i] = 0.01;
		}

		lg_p->marker_sizes->data = (NhlPointer) f_p;
		lg_p->marker_sizes->num_elements = count;
	}

/*=======================================================================*/

/* 
 * Handle the label strings. Copy the new array into the old.
 * NULL strings generate an error message and are replaced by empty 
 * (single byte null terminator) strings within the Validated copy function. 
 * If the box count has increased, but the string gen array does not 
 * contain enough elements, the array is resized and default strings are
 * created for the additional elements.
 */

	if (_NhlArgIsSet(args,num_args,NhlNlgLabelStrings)) {
		
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->label_strings),
						  lg_p->label_strings,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLabelStrings, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->label_strings = olg_p->label_strings;
		olg_p->label_strings = NULL;
	}

	if (lg_p->label_strings->num_elements < lg_p->item_count) {
		s_p = (NhlString *) lg_p->label_strings->data;
		if ((s_p = (NhlString *)
		     NhlRealloc(s_p, lg_p->item_count * 
				sizeof (NhlString))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLabelStrings);
			return NhlFATAL;
		}
		for (i=lg_p->label_strings->num_elements; 
		     i<lg_p->item_count; i++) {
			sprintf(number,"%d",i);
			if ((s_p[i] = (char *)
			     NhlMalloc(strlen(NhlLG_DEF_STRING) + 
				       strlen(number) + 1)) == NULL) {
				e_text = "%s: error creating %s string";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlgLabelStrings);
				return NhlFATAL;
			}
			strcpy(s_p[i],(Const char *)NhlLG_DEF_STRING);
			strcat(s_p[i],number);
		}

		lg_p->label_strings->data = (NhlPointer) s_p;
		lg_p->label_strings->num_elements = lg_p->item_count;
	}

/*=======================================================================*/
/*
 * Manage the line label colors array: if the array has changed copy the new
 * array elements.
 * Then if the box count is greater than the current array size, enlarge
 * the array and give initial values to the new elements.
 */


	if (_NhlArgIsSet(args,num_args,NhlNlgLineLabelFontColors)) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->line_label_colors),
						  lg_p->line_label_colors,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgLineLabelFontColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lg_p->line_label_colors = olg_p->line_label_colors;
		olg_p->line_label_colors = NULL;
		i_p = (int *) lg_p->line_label_colors->data;
	}

	if (lg_p->line_label_colors->num_elements < count) {
		i_p = (int *) lg_p->line_label_colors->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgLineLabelFontColors);
			return NhlFATAL;
		}
		for (i=lg_p->line_label_colors->num_elements; i<count; i++) {
			i_p[i] = i + 1;
		}

		lg_p->line_label_colors->data = (NhlPointer) i_p;
		lg_p->line_label_colors->num_elements = count;
	}

/*=======================================================================*/
/* 
 * Manage the box_fraction array. If the box count changes then reset the
 * box fraction array to a default (uniform) state. This is because there
 * is no obvious way to modify explicitly set sizes to handle a different
 * number of boxes. Even if the box_fraction resource is set in the 
 * same call the reset occurs because it is difficult to ensure that 
 * the passed in array contains the appropriate elements for the new 
 * item_count. Note that error checking for the elements of this resource 
 * is handled later. The box fraction array contains one more element than
 * the box count.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlgItemPositions)) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olg_p->item_positions),
						  lg_p->item_positions,
						  NhlLG_MAX_ITEMS,True,False,
						  NhlNlgItemPositions, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
			return ret;
		lg_p->item_positions = olg_p->item_positions;
		olg_p->item_positions = NULL;
	}
	
	if (lg_p->item_positions->num_elements < lg_p->item_count) {
		f_p = (float *) lg_p->item_positions->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, (lg_p->item_count) * 
				sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemPositions);
			return NhlFATAL;
		}

		lg_p->item_positions->data = (NhlPointer) f_p;
		lg_p->item_positions->num_elements = lg_p->item_count;
	}

	if (lg_p->item_count != olg_p->item_count) {
		float tfloat;
		f_p = (float *) lg_p->item_positions->data;
		tfloat = 1.0 / lg_p->item_count;
		f_p[0] = tfloat / 2.0;
		for (i=1;i<lg_p->item_count; i++)
			f_p[i] = f_p[0] + i * tfloat;
	}

/*=======================================================================*/
/*
 * Allocate or reallocate the location arrays: use one more than
 * the current box count so that both ends of the legend can be stored
 */

	if (lg_p->item_count > olg_p->item_count) {
		if (lg_p->item_locs == NULL) {
			lg_p->item_locs = (float *) 
				NhlMalloc((lg_p->item_count+1) * 
					  sizeof(float));
		}
		else {
			lg_p->item_locs = (float *) 
				NhlRealloc(lg_p->item_locs, 
					   (lg_p->item_count+1) *
					   sizeof(float));
		}
	}
/*=======================================================================*/
/*
 * Allocate or reallocate the order array: 
 */
	if (_NhlArgIsSet(args,num_args,NhlNlgItemOrder)) {
		NhlGenArray ga;
		NhlFreeGenArray(olg_p->item_order);
		olg_p->item_order = NULL;
		count = lg_p->item_count;
		if (lg_p->item_order->num_elements != count) {
			lg_p->item_order = NULL;
			e_text = "%s: %s must contain %d elements; ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemOrder,count);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}
		i_p = (int *)lg_p->item_order->data;
		for (i = 0; i < count; i++) {
			int j, found = 0;
			for (j = 0; j < count; j++) {
				if (i_p[j] == i) {
					found = 1;
					break;
				}
			}
			if (! found) {
				lg_p->item_order = NULL;
				e_text = "%s: %s must contain unique values 0 - %d; ignoring";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
					  NhlNlgItemOrder,count-1);
				ret = MIN(ret,NhlWARNING);
				return ret;
			}
		}
		if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
			e_text = "%s: error creating %s array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemOrder);
			return NhlFATAL;
		}
		memcpy(i_p,lg_p->item_order->data,count * sizeof(int));
		if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
			e_text = "%s: error creating %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemOrder);
			return NhlFATAL;
		}
		ga->my_data = True;
		lg_p->item_order = ga;
	}
	
	return (ret);
}


/*
 * Function: SetLegendGeometry
 *
 * Description:	Performs a rough division of the space available for the
 *		Legend into item area, label area and title area. Depending
 *		on the situation some of these areas are adjusted later.
 *
 * In Args:
 *
 * Out Args: Error conditions
 *
 * Return Values:
 *
 * Side Effects: There are no calls to Get or Set Values so the args and
 *               num_args arguments are not required.
 */

/*ARGSUSED*/
static NhlErrorTypes    SetLegendGeometry
#if	NhlNeedProto
(
	NhlLayer new,
	NhlLayer old,
	NhlBoolean init,
	_NhlArgList args,
	int num_args
)
#else
(new,old,args,init,num_args)
	NhlLayer	new;
	NhlBoolean	init;
	NhlLayer	old;
	_NhlArgList	args;
	int		num_args;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlLegendLayer	tnew = (NhlLegendLayer) new;
	NhlLegendLayerPart *lg_p = &(tnew->legend);
	enum {NO_TITLE, MINOR_AXIS, MAJOR_AXIS} title_loc;
	float bar_ext, bar_room;
	float title_ext;

	entry_name = init ? Init_Name : SetValues_Name;

/* Calculate the ndc margin from the fractional margin */

	lg_p->small_axis = MIN(lg_p->perim.r - lg_p->perim.l, 
			       lg_p->perim.t - lg_p->perim.b);

	lg_p->adj_perim.l = lg_p->perim.l + lg_p->margin.l * lg_p->small_axis;
	lg_p->adj_perim.r = lg_p->perim.r - lg_p->margin.r * lg_p->small_axis;
	lg_p->adj_perim.b = lg_p->perim.b + lg_p->margin.b * lg_p->small_axis;
	lg_p->adj_perim.t = lg_p->perim.t - lg_p->margin.t * lg_p->small_axis;
	lg_p->adj_width = lg_p->adj_perim.r - lg_p->adj_perim.l;
	lg_p->adj_height = lg_p->adj_perim.t - lg_p->adj_perim.b;
/*
 * Check the box extent values; and issue a warning if required
 */	
	if (lg_p->box_major_ext > 1.0 || lg_p->box_major_ext < 0.0) {
		e_text = "%s: %s out of range, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNlgBoxMajorExtentF);
		ret = MIN(ret,NhlWARNING);
		lg_p->box_major_ext = 0.5;
	}
	if (lg_p->box_minor_ext > 1.0 || lg_p->box_minor_ext < 0.0) {
		e_text = "%s: %s out of range, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNlgBoxMinorExtentF);
		ret = MIN(ret,NhlWARNING);
		lg_p->box_minor_ext = 0.6;
	}
/*
 * Check the title extent to make sure it does not exceed the hard-coded
 * limit; then locate the title
 */
	if (lg_p->auto_manage && 
	    lg_p->title_ext + lg_p->title_off > 0.5) {
		e_text = "%s: sum of %s and %s are too large, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,
			  NhlNlgTitleExtentF,NhlNlgTitleOffsetF);
		ret = MIN(ret,NhlWARNING);
		lg_p->title_ext = NhlLG_DEF_TITLE_EXT;
		lg_p->title_off = NhlLG_DEF_TITLE_OFF;
	}
		    
	if (!lg_p->title_on) {
		title_loc = NO_TITLE;
		lg_p->title_off_ndc = 0.0;
	}
	else if (lg_p->orient == NhlHORIZONTAL) {
		if (lg_p->title_pos == NhlRIGHT ||
		    lg_p->title_pos == NhlLEFT) {
			title_loc = MAJOR_AXIS;
			lg_p->title_off_ndc = 
				lg_p->title_off * lg_p->adj_width;
			title_ext = lg_p->title_ext * lg_p->adj_width;
		}
		else {
			title_loc = MINOR_AXIS;
			lg_p->title_off_ndc = 
				lg_p->title_off * lg_p->adj_height;
			title_ext = lg_p->title_ext * lg_p->adj_height;
		}
	}
	else {
		if (lg_p->title_pos == NhlRIGHT ||
		    lg_p->title_pos == NhlLEFT) {
			title_loc = MINOR_AXIS;
			lg_p->title_off_ndc = 
				lg_p->title_off * lg_p->adj_width;
			title_ext = lg_p->title_ext * lg_p->adj_width;
		}
		else {
			title_loc = MAJOR_AXIS;
			lg_p->title_off_ndc = 
				lg_p->title_off * lg_p->adj_height;
			title_ext = lg_p->title_ext * lg_p->adj_height;
		}
	}

	if (lg_p->orient == NhlHORIZONTAL) {
		switch (title_loc) {

		case NO_TITLE:
			lg_p->title.l = lg_p->adj_perim.l;
			lg_p->title.r = lg_p->adj_perim.r;
			lg_p->title.b = lg_p->adj_perim.b;
			lg_p->title.t = lg_p->adj_perim.t;

			break;

		case MAJOR_AXIS:
			
			lg_p->title.b = lg_p->adj_perim.b;
			lg_p->title.t = lg_p->adj_perim.t;
				
			if (lg_p->title_pos == NhlLEFT) {
				lg_p->title.l = lg_p->adj_perim.l;
				lg_p->title.r = lg_p->title.l + title_ext;
			}
			else if (lg_p->title_pos == NhlRIGHT) {
				lg_p->title.r = lg_p->adj_perim.r;
				lg_p->title.l = lg_p->title.r - title_ext;
			}
			break;

		case MINOR_AXIS:

			lg_p->title.l = lg_p->adj_perim.l;
			lg_p->title.r = lg_p->adj_perim.r;
				
			if (lg_p->title_pos == NhlBOTTOM) {
				lg_p->title.b = lg_p->adj_perim.b;
				lg_p->title.t = lg_p->title.b + title_ext;
			}
			else if (lg_p->title_pos == NhlTOP) {
				lg_p->title.t = lg_p->adj_perim.t;
				lg_p->title.b = lg_p->title.t - title_ext;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	else { /* NhlVERTICAL */
		switch (title_loc) {

		case NO_TITLE:
			lg_p->title.l = lg_p->adj_perim.l;
			lg_p->title.r = lg_p->adj_perim.r;
			lg_p->title.b = lg_p->adj_perim.b;
			lg_p->title.t = lg_p->adj_perim.t;
			break;

		case MAJOR_AXIS:
			lg_p->title.l = lg_p->adj_perim.l;
			lg_p->title.r = lg_p->adj_perim.r;
				
			if (lg_p->title_pos == NhlBOTTOM) {
				lg_p->title.b = lg_p->adj_perim.b;
				lg_p->title.t = lg_p->title.b + title_ext;
			}
			else if (lg_p->title_pos == NhlTOP) {
				lg_p->title.t = lg_p->adj_perim.t;
				lg_p->title.b = lg_p->title.t - title_ext;
			}
			break;

		case MINOR_AXIS:
			lg_p->title.b = lg_p->adj_perim.b;
			lg_p->title.t = lg_p->adj_perim.t;
				
			if (lg_p->title_pos == NhlLEFT) {
				lg_p->title.l = lg_p->adj_perim.l;
				lg_p->title.r = lg_p->title.l + title_ext;
			}
			else if (lg_p->title_pos == NhlRIGHT) {
				lg_p->title.r = lg_p->adj_perim.r;
				lg_p->title.l = lg_p->title.r - title_ext;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	subret = SetTitle(new,old,init,args,num_args);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

/*
 * Silently modify the label position if not appropriate for the
 * current legend orientation. Also determine the maximum critical
 * angle for label angle adjustments.
 */

	if (lg_p->orient == NhlHORIZONTAL) {
		switch (lg_p->label_pos) {
		case NhlCENTER:
			break;
		default:
		case NhlBOTTOM:
		case NhlRIGHT:
			lg_p->label_pos = NhlBOTTOM;
			break;
		case NhlTOP:
		case NhlLEFT:
			lg_p->label_pos = NhlTOP;
			break;
		}
	}
	else {
		switch (lg_p->label_pos) {
		case NhlCENTER:
			break;
		default:
		case NhlRIGHT:
		case NhlBOTTOM:
			lg_p->label_pos = NhlRIGHT;
			break;
		case NhlLEFT:
		case NhlTOP:
			lg_p->label_pos = NhlLEFT;
			break;
		}

	}
		
/*
 * Determine dimensions: first pass determines basic position for
 * title, bar and labels. Bar dimensions may be adjusted later to 
 * ensure that labels are visible.
 */
	if (lg_p->orient == NhlHORIZONTAL) {
		bar_room = MAX(lg_p->title.txtr - lg_p->title.bxtr,
			       lg_p->adj_height);

		bar_ext =  lg_p->box_minor_ext * bar_room;
			
		switch (title_loc) {

		case NO_TITLE:
			lg_p->bar.l = lg_p->adj_perim.l;
			lg_p->bar.r = lg_p->adj_perim.r;
			lg_p->labels.l = lg_p->bar.l;
			lg_p->labels.r = lg_p->bar.r;
					
			if (! lg_p->labels_on || 
			    lg_p->label_pos == NhlCENTER) {
				lg_p->bar.b = lg_p->adj_perim.b +
					(bar_room - bar_ext) / 2.0;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.b;
				lg_p->labels.t = lg_p->bar.t;
			}
			else if (lg_p->label_pos == NhlTOP) {
				lg_p->bar.b = lg_p->adj_perim.b;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.t;
				lg_p->labels.t = lg_p->adj_perim.t;
			}
			else if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->bar.b = lg_p->adj_perim.t - bar_ext;
				lg_p->bar.t = lg_p->adj_perim.t;
				lg_p->labels.b = lg_p->adj_perim.b;
				lg_p->labels.t = lg_p->bar.b;
			}
			break;

		case MAJOR_AXIS:
				
			if (lg_p->title_pos == NhlLEFT) {
				lg_p->bar.l = lg_p->adj_perim.l +
					title_ext + lg_p->title_off_ndc;
				lg_p->bar.r = lg_p->adj_perim.r;
			}
			else if (lg_p->title_pos == NhlRIGHT) {
				lg_p->bar.l = lg_p->adj_perim.l;
				lg_p->bar.r = lg_p->adj_perim.r - 
					title_ext - lg_p->title_off_ndc;
			}
			lg_p->labels.l = lg_p->bar.l;
			lg_p->labels.r = lg_p->bar.r;

			if (! lg_p->labels_on || 
			    lg_p->label_pos == NhlCENTER) {
				lg_p->bar.b = lg_p->adj_perim.b +
					(bar_room - bar_ext) / 2.0;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.b;
				lg_p->labels.t = lg_p->bar.t;
			}
			else if (lg_p->label_pos == NhlTOP) {
				lg_p->bar.b = lg_p->adj_perim.b;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.t;
				lg_p->labels.t = lg_p->adj_perim.t;
			}
			else if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->bar.b = lg_p->adj_perim.t - bar_ext;
				lg_p->bar.t = lg_p->adj_perim.t;
				lg_p->labels.b = lg_p->adj_perim.b;
				lg_p->labels.t = lg_p->bar.b;
			}
			break;

		case MINOR_AXIS:

			if (title_ext + 
			    lg_p->title_off_ndc + bar_ext > bar_room) {
				e_text = 
				  "%s: Maximum bar size exceeded, defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret, NhlWARNING);
				bar_ext = bar_room -
					title_ext - lg_p->title_off_ndc;
				lg_p->box_minor_ext = bar_ext / bar_room;
			}
			lg_p->bar.l = lg_p->adj_perim.l;
			lg_p->bar.r = lg_p->adj_perim.r;
			lg_p->labels.l = lg_p->adj_perim.l;
			lg_p->labels.r = lg_p->adj_perim.r;
				
			if (lg_p->title_pos == NhlBOTTOM) {
				lg_p->bar.b = 
					lg_p->title.t + lg_p->title_off_ndc;
				lg_p->bar.t = lg_p->adj_perim.t;
			}
			else if (lg_p->title_pos == NhlTOP) {
				lg_p->bar.b = lg_p->adj_perim.b;
				lg_p->bar.t = lg_p->adj_perim.t - 
					title_ext - lg_p->title_off_ndc;
			}

			
			if (!lg_p->labels_on || lg_p->label_pos == NhlCENTER) {
				lg_p->bar.b = (lg_p->bar.t - lg_p->bar.b - 
					       bar_ext) / 2.0 + lg_p->bar.b;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.b;
				lg_p->labels.t = lg_p->bar.t;
			}
			else if (lg_p->label_pos == NhlTOP) {
				lg_p->labels.t = lg_p->bar.t;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.t;
			}
			else if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->labels.b = lg_p->bar.b;
				lg_p->bar.b = lg_p->bar.t - bar_ext;
				lg_p->labels.t = lg_p->bar.b;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		/* get the box size */
		
		lg_p->box_size.x = 
			(lg_p->bar.r - lg_p->bar.l) / lg_p->item_count;
		lg_p->box_size.y = lg_p->bar.t - lg_p->bar.b;
	}

	else { /* NhlVERTICAL */

		bar_room = MAX(lg_p->title.rxtr - lg_p->title.lxtr,
			       lg_p->adj_width);
		bar_ext = lg_p->box_minor_ext * bar_room;

		switch (title_loc) {

		case NO_TITLE:
			lg_p->bar.b = lg_p->adj_perim.b;
			lg_p->bar.t = lg_p->adj_perim.t;
			lg_p->labels.b = lg_p->adj_perim.b;
			lg_p->labels.t = lg_p->adj_perim.t;
					
			if (!lg_p->labels_on || lg_p->label_pos == NhlCENTER) {
				lg_p->bar.l = lg_p->adj_perim.l +
					(bar_room - bar_ext) / 2.0;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.l;
				lg_p->labels.r = lg_p->bar.r;
			}
			else if (lg_p->label_pos == NhlRIGHT) {
				lg_p->bar.l = lg_p->adj_perim.l;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.r;
				lg_p->labels.r = lg_p->adj_perim.r;
			}
			else if (lg_p->label_pos == NhlLEFT) {
				lg_p->bar.l = lg_p->adj_perim.r - bar_ext;
				lg_p->bar.r = lg_p->adj_perim.r;
				lg_p->labels.l = lg_p->adj_perim.l;
				lg_p->labels.r = lg_p->bar.l;
			}
			break;

		case MAJOR_AXIS:
				
			if (lg_p->title_pos == NhlBOTTOM) {
				lg_p->bar.b = lg_p->adj_perim.b + 
					title_ext + lg_p->title_off_ndc;
				lg_p->bar.t = lg_p->adj_perim.t;
			}
			else if (lg_p->title_pos == NhlTOP) {
				lg_p->bar.b = lg_p->adj_perim.b;
				lg_p->bar.t = lg_p->adj_perim.t - 
					title_ext - lg_p->title_off_ndc;
			}
			lg_p->labels.b = lg_p->bar.b;
			lg_p->labels.t = lg_p->bar.t;

			if (! lg_p->labels_on || 
			    lg_p->label_pos == NhlCENTER) {
				lg_p->bar.l = lg_p->adj_perim.l +
					(bar_room - bar_ext) / 2.0;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.l;
				lg_p->labels.r = lg_p->bar.r;
			}
			else if (lg_p->label_pos == NhlRIGHT) {
				lg_p->bar.l = lg_p->adj_perim.l;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.r;
				lg_p->labels.r = lg_p->adj_perim.r;
			}
			else if (lg_p->label_pos == NhlLEFT) {
				lg_p->bar.l = lg_p->adj_perim.r - bar_ext;
				lg_p->bar.r = lg_p->adj_perim.r;
				lg_p->labels.l = lg_p->adj_perim.l;
				lg_p->labels.r = lg_p->bar.l;
			}
			break;

		case MINOR_AXIS:

			if (title_ext + 
			    lg_p->title_off_ndc + bar_ext > bar_room) {
				e_text = 
				  "%s: Maximum bar size exceeded, defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret, NhlWARNING);
				bar_ext = bar_room - 
					title_ext - lg_p->title_off_ndc;
				lg_p->box_minor_ext = bar_ext / bar_room;
			}

			lg_p->bar.b = lg_p->adj_perim.b;
			lg_p->bar.t = lg_p->adj_perim.t;
			lg_p->labels.b = lg_p->adj_perim.b;
			lg_p->labels.t = lg_p->adj_perim.t;
				
			if (lg_p->title_pos == NhlLEFT) {
				lg_p->bar.l = 
					lg_p->title.r + lg_p->title_off_ndc;
				lg_p->bar.r = lg_p->adj_perim.r;
			}
			else if (lg_p->title_pos == NhlRIGHT) {
				lg_p->bar.l = lg_p->adj_perim.l;
				lg_p->bar.r = lg_p->adj_perim.r - 
					title_ext - lg_p->title_off_ndc;
			}
			
			if (!lg_p->labels_on || lg_p->label_pos == NhlCENTER) {
				lg_p->bar.l = (lg_p->bar.r - lg_p->bar.l - 
					       bar_ext) / 2.0 + lg_p->bar.l;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.l;
				lg_p->labels.r = lg_p->bar.r;
			}
			else if (lg_p->label_pos == NhlRIGHT) {
				lg_p->labels.r = lg_p->bar.r;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.r;
			}
			else if (lg_p->label_pos == NhlLEFT) {
				lg_p->labels.l = lg_p->bar.l;
				lg_p->bar.l = lg_p->bar.r - bar_ext;
				lg_p->labels.r = lg_p->bar.l;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		lg_p->box_size.x = lg_p->bar.r - lg_p->bar.l;
		lg_p->box_size.y = 
			(lg_p->bar.t - lg_p->bar.b) / lg_p->item_count;
	}

	return ret;
				
}


/*
 * Function: SetTitle
 *
 * Description:	Calculates the title extent, then creates or sets the
 *	attributes of child text object used for the title. In auto-manage 
 *	mode, retrieves the bounding box of the child after a preliminary
 *	setting of the text height, and then resets the text height to fit
 *	into the space available.
 *	This routine needs performance evaluation and tuning.
 *	
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 *	Calls to SetValues or Create modify attributes of the child
 *	text object.
 */

/*ARGSUSED*/
static NhlErrorTypes    SetTitle
#if	NhlNeedProto
(
	NhlLayer	new, 
	NhlLayer	old,
	int		init,
	_NhlArgList	args,
	int		 num_args
)
#else
(new,old,init,args,num_args)
	NhlLayer	new;
	NhlLayer	old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlLegendLayer	tnew = (NhlLegendLayer) new;
	NhlLegendLayer	told = (NhlLegendLayer) old;
	NhlLegendLayerPart *lg_p = &(tnew->legend);
	NhlLegendLayerPart *olg_p = &(told->legend);
	char buffer[_NhlMAXRESNAMLEN];
	char *c_p;
	NhlBoundingBox titleBB;
	float w, h, wta, hta, factor, height;
	float angle;
	NhlBoolean string_changed = False;


	lg_p->title.lxtr = lg_p->title.l;
	lg_p->title.rxtr = lg_p->title.r;
	lg_p->title.bxtr = lg_p->title.b;
	lg_p->title.txtr = lg_p->title.t;
	lg_p->title_x = lg_p->title.lxtr + 
		(lg_p->title.rxtr - lg_p->title.lxtr) / 2.0;
	lg_p->title_y = lg_p->title.bxtr + 
		(lg_p->title.txtr - lg_p->title.bxtr) / 2.0;

/*
 * Only initialize a text item for the title if it is turned on
 * and the title extent is greater than 0.0.
 */
	if (!lg_p->title_on || lg_p->title_ext <= 0.0)
		return ret;

	entry_name = init ? Init_Name : SetValues_Name;
/*
 * Check constant spacing value
 */

	if (lg_p->title_const_spacing < 0.0) {
		lg_p->title_const_spacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: Constant spacing cannot be less than zero, defaulting %s",
			  entry_name,NhlNlgTitleConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}

/*
 * If the title string is NULL, create a default string.
 * The default string is the name of the label bar object
 */
	if (lg_p->title_string == lgDefTitle) {
                lg_p->title_string = (char*)
                        NhlMalloc((unsigned)strlen(tnew->base.name) +1);
                strcpy(lg_p->title_string,tnew->base.name);
        } 
	else if (init) {
                c_p = lg_p->title_string;
                lg_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
                strcpy(lg_p->title_string,c_p);
		string_changed = True;
        }
	else if (lg_p->title_string != olg_p->title_string) {
		if(olg_p->title_string != lgDefTitle)
			NhlFree(olg_p->title_string);
		c_p = lg_p->title_string;
		lg_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
		strcpy(lg_p->title_string, c_p);
		string_changed = True;
	}

	switch (lg_p->title_just) {
	case NhlBOTTOMLEFT:
		lg_p->title_x = lg_p->title.l;
		lg_p->title_y = lg_p->title.b;
		break;
	case NhlBOTTOMCENTER:
		lg_p->title_x = lg_p->title.l +
			(lg_p->title.r - lg_p->title.l)/2.0;
		lg_p->title_y = lg_p->title.b;
		break;
	case NhlBOTTOMRIGHT:
		lg_p->title_x = lg_p->title.r;
		lg_p->title_y = lg_p->title.b;
		break;
	case NhlCENTERLEFT:
		lg_p->title_x = lg_p->title.l;
		lg_p->title_y = lg_p->title.b +
			(lg_p->title.t - lg_p->title.b)/2.0;
		break;
	case NhlCENTERCENTER:
	default:
		lg_p->title_x = lg_p->title.l +
			(lg_p->title.r - lg_p->title.l)/2.0;
		lg_p->title_y = lg_p->title.b +
			(lg_p->title.t - lg_p->title.b)/2.0;
		break;
	case NhlCENTERRIGHT:
		lg_p->title_x = lg_p->title.r;
		lg_p->title_y = lg_p->title.b +
			(lg_p->title.t - lg_p->title.b)/2.0;
		break;
	case NhlTOPLEFT:
		lg_p->title_x = lg_p->title.l;
		lg_p->title_y = lg_p->title.t;
		break;
	case NhlTOPCENTER:
		lg_p->title_x = lg_p->title.l +
			(lg_p->title.r - lg_p->title.l)/2.0;
		lg_p->title_y = lg_p->title.t;
		break;
	case NhlTOPRIGHT:
		lg_p->title_x = lg_p->title.r;
		lg_p->title_y = lg_p->title.t;
		break;
	}

	if (lg_p->title_height <= 0.0) 
		height = NhlLG_DEF_CHAR_HEIGHT;
	else 
		height = lg_p->title_height;

	angle = (lg_p->title_angle < 0.0) ?
		lg_p->title_angle + 360.0 : lg_p->title_angle;
	if (lg_p->title_id < 0) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Title");
		subret = NhlVACreate(&lg_p->title_id,
				 buffer,NhltextItemClass,
				 tnew->base.id,
				 NhlNtxFont,lg_p->title_font,
				 NhlNtxString,lg_p->title_string,
				 NhlNtxPosXF,lg_p->title_x,
				 NhlNtxPosYF,lg_p->title_y,
				 NhlNtxDirection,lg_p->title_direction,
				 NhlNtxAngleF,angle,
				 NhlNtxJust,(int)lg_p->title_just,
				 NhlNtxFontColor,lg_p->title_color,
				 NhlNtxFontHeightF,height,
				 NhlNtxFontAspectF,lg_p->title_aspect,
				 NhlNtxConstantSpacingF,
				 	lg_p->title_const_spacing,
				 NhlNtxFontQuality,lg_p->title_quality,
				 NhlNtxFuncCode,lg_p->title_func_code,
				 NhlNtxFontThicknessF,lg_p->title_thickness,
				 NULL);
		
	}
	else {
		NhlSArg		sargs[16];
		int		sargc = 0;

		if (lg_p->title_font != olg_p->title_font)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFont,lg_p->title_font);
		if (string_changed)
			NhlSetSArg(&sargs[(sargc)++],
				    NhlNtxString,lg_p->title_string);
		if (lg_p->title_x != olg_p->title_x)
			NhlSetSArg(&sargs[(sargc)++],
				    NhlNtxPosXF,lg_p->title_x);
		if (lg_p->title_y != olg_p->title_y)
			NhlSetSArg(&sargs[(sargc)++],
				    NhlNtxPosYF,lg_p->title_y);
		if (lg_p->title_direction != olg_p->title_direction)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxDirection,lg_p->title_direction);
		if (angle != olg_p->title_angle)
			NhlSetSArg(&sargs[(sargc)++],NhlNtxAngleF,angle);
		if (lg_p->title_just != olg_p->title_just)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxJust,(int)lg_p->title_just);
		if (lg_p->title_color != olg_p->title_color)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontColor,lg_p->title_color);
		if (height != olg_p->title_height)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontHeightF,height);
		if (lg_p->title_aspect != olg_p->title_aspect)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontAspectF,lg_p->title_aspect);
		if (lg_p->title_const_spacing != olg_p->title_const_spacing)
			NhlSetSArg(&sargs[(sargc)++],
				    NhlNtxConstantSpacingF,
				   lg_p->title_const_spacing);
		if (lg_p->title_quality != olg_p->title_quality)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontQuality,lg_p->title_quality);
		if (lg_p->title_func_code != olg_p->title_func_code)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFuncCode,lg_p->title_func_code);
		if (lg_p->title_thickness != olg_p->title_thickness)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontThicknessF,lg_p->title_thickness);
		subret = NhlALSetValues(lg_p->title_id,sargs,sargc);
	}
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

/*
 * If title_height is 0.0 or auto-manage is in effect adjust the title
 * to the space available
 */
	subret = NhlGetBB(lg_p->title_id, &titleBB);
	w=titleBB.r-titleBB.l;
	h=titleBB.t-titleBB.b;
	if ((ret = MIN(ret,subret)) < NhlWARNING || w <= 0.0 || h <= 0.0) {
		e_text = "%s: Error getting title bounding box";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	wta=lg_p->title.r-lg_p->title.l;
	hta=lg_p->title.t-lg_p->title.b;
	if (wta <= 0.0 || hta <= 0.0) {
		e_text = "%s:  no area available for title";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (lg_p->title_height <= 0.0 || lg_p->auto_manage) {
		factor = wta / w < hta / h ? wta / w : hta / h;
		lg_p->title_height = height * factor;

		if (height != lg_p->title_height) {
			subret = NhlVASetValues(lg_p->title_id,
					 NhlNtxFontHeightF,lg_p->title_height,
						NULL);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting title height",entry_name);
				return(ret);
			}
		}
	}
	else {
		lg_p->title.lxtr = MIN(lg_p->title.l,titleBB.l);
		lg_p->title.rxtr = MAX(titleBB.r,lg_p->title.r);
		lg_p->title.bxtr = MIN(lg_p->title.b,titleBB.b);
		lg_p->title.txtr = MAX(titleBB.t,lg_p->title.t);
	}

	return ret;
}


/*
 * Function:  SetBoxLocations
 *
 * Description:	Sets the position of each Legend item. If the item placement
 *	mode is explicit, first calls an internal function to check and 
 *	coerce the item_positions array into a proper sequence of values.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The item_positions GenArray may be modified.
 */

/*ARGSUSED*/
static NhlErrorTypes    SetBoxLocations
#if	NhlNeedProto
(
	NhlLayer		new, 
	NhlLayer		old,
	int		init,
	_NhlArgList	args,
	int		num_args
)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLegendLayer	tnew = (NhlLegendLayer) new;
	NhlLegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	float bar_len;
	float box_len;
	int i;
	float *item_positions = (float *)lg_p->item_positions->data;
	char *entry_name;

	entry_name = init ? Init_Name : SetValues_Name;
	if (lg_p->item_placement == NhlEXPLICITPLACEMENT) {
		ret1 = ManageItemPositionsArray(item_positions,
						lg_p->item_count,
						entry_name);
	}
	ret = MIN(ret,ret1);

/* 
 * Adjust boundary of bar and label area, depending on the label state.
 */

	memcpy((void *)&lg_p->adj_bar, (Const void *)&lg_p->bar, 
	       sizeof(NhlBoundingBox));
	memcpy((void *)&lg_p->adj_box_size, (Const void *)&lg_p->box_size,
	       sizeof(NhlCoord));
	if (lg_p->label_alignment == NhlABOVEITEMS) {
		if (lg_p->orient == NhlHORIZONTAL) {
			lg_p->adj_box_size.x = lg_p->box_size.x * 
				lg_p->item_count / (lg_p->item_count + 0.5);
			lg_p->adj_bar.r = 
				lg_p->bar.r - lg_p->adj_box_size.x / 2.0;
			lg_p->labels.l = 
				lg_p->bar.l + lg_p->adj_box_size.x / 2.0;
		}
		else {
			lg_p->adj_box_size.y = lg_p->box_size.y * 
				lg_p->item_count / (lg_p->item_count + 0.5);
			lg_p->adj_bar.t = 
				lg_p->bar.t - lg_p->adj_box_size.y / 2.0;
			lg_p->labels.b = 
				lg_p->bar.b + lg_p->adj_box_size.y / 2.0;
		}
	}
	else if (lg_p->label_alignment == NhlBELOWITEMS) {
		if (lg_p->orient == NhlHORIZONTAL) {
			lg_p->adj_box_size.x = lg_p->box_size.x * 
				lg_p->item_count / (lg_p->item_count + 0.5);
			lg_p->adj_bar.l = 
				lg_p->bar.l + lg_p->adj_box_size.x / 2.0;
			lg_p->labels.l = lg_p->bar.l;
			lg_p->labels.r = 
				lg_p->bar.r - lg_p->adj_box_size.x / 2.0;
		}
		else {
			lg_p->adj_box_size.y = lg_p->box_size.y * 
				lg_p->item_count / (lg_p->item_count + 0.5);
			lg_p->adj_bar.b = 
				lg_p->bar.b + lg_p->adj_box_size.y / 2.0;
			lg_p->labels.t = 
				lg_p->bar.t - lg_p->adj_box_size.y / 2.0;
		}
	}


/*
 * create an array of the left or bottom varying coordinates -- 
 */
		
	if (lg_p->orient == NhlHORIZONTAL &&
	    lg_p->item_placement == NhlUNIFORMPLACEMENT) {
		bar_len = lg_p->adj_bar.r - lg_p->adj_bar.l;
		box_len = bar_len / (lg_p->item_count);
		lg_p->item_locs[0] = lg_p->adj_bar.l + 0.5 * box_len;
		for (i=1; i < lg_p->item_count; i++) {
			lg_p->item_locs[i] = lg_p->item_locs[0] + i * box_len;
		}
		lg_p->item_locs[lg_p->item_count] = lg_p->adj_bar.r;
	}
	if (lg_p->orient == NhlVERTICAL &&
	    lg_p->item_placement == NhlUNIFORMPLACEMENT) {
		bar_len = lg_p->adj_bar.t - lg_p->adj_bar.b;
		box_len = bar_len / (lg_p->item_count);
		lg_p->item_locs[0] = lg_p->adj_bar.b + 0.5 * box_len;
		for (i=1; i < lg_p->item_count; i++) {
			lg_p->item_locs[i] = lg_p->item_locs[0] + i * box_len;
		}
		lg_p->item_locs[lg_p->item_count] = lg_p->adj_bar.t;
	}
	if (lg_p->orient == NhlHORIZONTAL &&
	    lg_p->item_placement == NhlEXPLICITPLACEMENT) {
		bar_len = lg_p->adj_bar.r - lg_p->adj_bar.l;
		for (i=0; i < lg_p->item_count; i++) {
			lg_p->item_locs[i] = lg_p->adj_bar.l + 
				bar_len * item_positions[i];
		}
		lg_p->item_locs[lg_p->item_count] = lg_p->adj_bar.r;
	}
	if (lg_p->orient == NhlVERTICAL &&
	    lg_p->item_placement == NhlEXPLICITPLACEMENT) {
		bar_len = lg_p->adj_bar.t - lg_p->adj_bar.b;
		for (i=0; i < lg_p->item_count; i++) {
			lg_p->item_locs[i] = lg_p->adj_bar.b + 
				bar_len * item_positions[i];
		}
		lg_p->item_locs[lg_p->item_count] = lg_p->adj_bar.t;
	}
	return(ret);

}


/*
 * Function: ManageItemPositionsArray
 *
 * Description:	Finds and modifies out-of-range values in the item positions
 *	array, so that a monotonically increasing set of values is returned. 
 *	The assumption is that negative values are placed intentionally, in
 *	order to force linear division of the subspace between two properly
 *	ascending bounding values in the array. Values that are out-of-range
 *	in some other way, such as exceeding the value of a subsequent 
 *	element, are flagged with an informational message but still are
 *	coerced into a proper sequence.
 *	
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The item_positions GenArray may be modified.
 */

/*ARGSUSED*/
static NhlErrorTypes    ManageItemPositionsArray
#if	NhlNeedProto
(
	float	*item_positions, 
	int	count,
	char	*entry_name
) 
#else
(item_positions, count,entry_name)
	float	*item_positions;
	int	count;
	char	*entry_name;
#endif

{
	int i, first_neg = -1;
	NhlErrorTypes ret = NhlNOERROR;
	float last_val = 0.0, last_good_val = 0.0;
			
/*
 * At this point it should be guaranteed that the box_fraction array exists
 * and it is of the correct length.
 * Now check the validity of the explicit placement fractions. Negative values
 * are not an error, but cause equal divisions between the two surrounding
 * specified values. Non-monotonically increasing positive values, or
 * values greater than 1.0 cause an informational message to be generated. 
 * but give the same results as if they had been set to a negative value.
 */
	/* deal with first element manually, since in the loop the
	   previous element must be compared */

	if (item_positions[0] < 0.0)
		first_neg = 0;
	else if (item_positions[0] > 1.0) {
		NhlPError(NhlINFO,NhlEUNKNOWN,
		  "%s: Modifying invalid box fraction array element: 0",
			  entry_name);
		ret = NhlINFO;
		item_positions[0] = -1.0;
		first_neg = 0;
	}
	else {
		last_val = item_positions[0];
	}

	for (i=1; i<count;i++) {
		if (item_positions[i] < 0.0) {
			if (first_neg == -1) {
				last_good_val = last_val;
				first_neg = i;
			}
		}
		else if (first_neg != -1) {
			if (item_positions[i] > 1.0 ||
			    item_positions[i] < last_good_val) {
				NhlPError(NhlINFO,NhlEUNKNOWN,
					  "%s: Modifying out-of-range box fraction array element: %d",
					  entry_name,i);
				item_positions[i] = -1.0;
			}
			else {
				CreateIntermediates(item_positions, 
						    last_good_val, 
						    first_neg, i, count-1);
				first_neg = -1;
				last_val = item_positions[i];
			}
		}
		else if (item_positions[i] > 1.0 ||
			 (item_positions[i] < last_val)) {
			NhlPError(NhlINFO,NhlEUNKNOWN,
				  "%s:Modifying out-of-range box fraction array element: %d",
				  entry_name,i);
			ret = NhlINFO;
			item_positions[i] = -1.0;
			last_good_val = last_val;
			first_neg = i;
		}
		else {
			last_val = item_positions[i];
		}
	}
/*
 * handle case of out-of-range final element
 */
	if (first_neg != -1) {
		CreateIntermediates(item_positions, 
				    last_good_val, 
				    first_neg, count-1, count-1);
	}
	
	return (ret);
}


/*
 * Function: CreateIntermediates
 *
 * Description:	
 *
 * 	Creates evenly spaced values to replace negative values in the
 * 	item positions array. The start value is the index of the first
 * 	negative value in the chain; the end value is the index of the
 * 	first non-negative value. Note that the default distance between
 *	the boundary of the items area and the first or last items is half
 *	of the normal distance between items.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: Modifies the contents of the item positions array
 */

static void CreateIntermediates
#if	NhlNeedProto
	(float		*flist,
	 float		base_val,
	 int		start,
	 int		end,
         int		last)
#else
(flist,base_val,start,end,last)
	float		*flist;
        float		base_val;
	int		start;
	int		end;
        int		last;
#endif
{
	float	spacing;
	int	cnt;
	int	i;
	float	tfloat;

	if (end == last && flist[end] < 0.0) {
		if (start == 0) {
			cnt = last+1;
			tfloat = 1.0/((float) cnt);
			flist[0] = tfloat/2.0;
			for (i=1;i<cnt; i++)
				flist[i] = flist[0] + i * tfloat;
		}
		else {
			cnt = (end - start + 1) * 2 + 1;
			spacing = 2 * (1.0 - base_val) / (float) cnt;
			for (i=0;i<cnt/2;i++)
				flist[start+i] = base_val + (i+1) * spacing;
		}
	}
	
	else if (start == 0) {
		cnt = end * 2 + 1;
		spacing = flist[end] / (float) cnt;
		flist[start] = spacing;
		for (i=1;i<cnt/2;i++)
			flist[start+i] = flist[start] + i * 2 * spacing;
	}
	else {
		cnt = end - start + 1;
		spacing = (flist[end] - base_val) / (float) cnt;
		for (i=0; i<cnt-1; i++)
			flist[start+i] = base_val + (i+1) * spacing;
	}
}

/*
 * Function:  SetLabels
 *
 * Description:	Calculates the attributes of the Legend labels
 *	and creates or sets the child multi-text object that is responsible
 *	for rendering the labels. If in auto-manage mode the bounding box
 *	of the multitext object is inquired after a preliminary setting of
 *	the attributes, and then another internal function is called to
 *	adjust the height and/or justification of the multi-text object in
 *	order to ensure that the labels do not overlap on rotation. 
 *	Note: there is code in this routine for label stride. However, label
 *	stride is not a supported option for the Legend object. 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: Modifies attibutes of the child multi-text object.
 */

/*ARGSUSED*/
static NhlErrorTypes    SetLabels
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlLegendLayer	tnew = (NhlLegendLayer) new;
	NhlLegendLayerPart *lg_p = &(tnew->legend);
	NhlLegendLayerPart *olg_p = &((NhlLegendLayer) old)->legend;
	char buffer[_NhlMAXRESNAMLEN];
	char **labels_p;
	int count; 
	int itmp, max_strlen = 0;
	int i,j,ix;
	NhlMTextOrientatonType mtext_orient;
	float label_height, char_space, avail_char_space;
	float base_pos = 0.0, offset = 0.0, increment = 0.0;
	NhlCoord larea;
	float c_frac = 1.0;
	float angle,label_loc;
	float x,y,width,height;
	NhlBoolean label_locs_changed = False;
	NhlSArg	sargs[24];
	int	nargs = 0;
	int *order;
	char **olabels = NULL;

	if (! lg_p->labels_on) {
		lg_p->labels.lxtr = lg_p->labels.l;
		lg_p->labels.rxtr = lg_p->labels.r;
		lg_p->labels.bxtr = lg_p->labels.b;
		lg_p->labels.txtr = lg_p->labels.t;
		return NhlNOERROR;
	}

	entry_name = init ? Init_Name : SetValues_Name;
/*
 * check constant spacing
 */
	if (lg_p->label_const_spacing < 0.0) {
		lg_p->label_const_spacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: Constant spacing cannot be less than zero, defaulting %s",
			  entry_name,NhlNlgLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}

/*
 * Determine the multitext orientation and the NDC label offset
 */

	if (lg_p->orient == NhlHORIZONTAL) {
		mtext_orient = NhlMTEXT_Y_CONST;
		lg_p->label_off_ndc = lg_p->label_off * 
			(lg_p->adj_perim.t - lg_p->adj_perim.b);
	}
	else {
		mtext_orient = NhlMTEXT_X_CONST;
		lg_p->label_off_ndc = lg_p->label_off *
			(lg_p->adj_perim.r - lg_p->adj_perim.l);
	}
	count = lg_p->item_count;
		
/*
 * If the label stride is greater than 1, find the number of
 * labels to use. If greater than the previous size, reallocate
 * the array used to point to the correct labels.
 */
	
	lg_p->label_draw_count = count;
	labels_p = (NhlString *)lg_p->label_strings->data;
	order = lg_p->item_order ? (int *) lg_p->item_order->data : NULL;
	if (lg_p->label_stride > 1) {
		count = (count % lg_p->label_stride == 0) ?
			count / lg_p->label_stride :  
			count / lg_p->label_stride + 1;
		if (count > lg_p->max_label_stride_count) {
			if (lg_p->stride_labels == NULL) {
				lg_p->stride_labels = (char **) 
					NhlMalloc(count * sizeof(char *));
			}
			else {
				lg_p->stride_labels = (char **)
					NhlRealloc(lg_p->stride_labels,
						   count * sizeof(char *));
			}
			lg_p->max_label_stride_count = count;
		}
		labels_p = lg_p->stride_labels;
		if (order) {
			for (i=0,j=0; i<count; i++,j+=lg_p->label_stride) {
				labels_p[i] = 
					((NhlString *) lg_p->label_strings->data)[order[j]];
			}
		}
		else {
			for (i=0,j=0; i<count; i++,j+=lg_p->label_stride) {
				labels_p[i] = 
					((NhlString *) lg_p->label_strings->data)[j];
			}
		}
		lg_p->label_draw_count = count;
	}
	else if (order) {
		olabels = (char **) NhlMalloc(count * sizeof(char *));
		for (i=0; i<count; i++) {
			olabels[i] = ((NhlString *) lg_p->label_strings->data)[order[i]];
		}
		labels_p = olabels;
	}
		

/*
 * Now allocate the location array
 */
	if (count > lg_p->max_label_draw_count) {
		if (lg_p->label_locs == NULL) {
			lg_p->label_locs = (float *) 
				NhlMalloc(count * sizeof(float));
		}
		else {
			lg_p->label_locs = (float *)
				NhlRealloc(lg_p->label_locs,
					   count * sizeof(float));
		}
		for (i = lg_p->max_label_draw_count; i < count; i++)
			lg_p->label_locs[i] = -999.0;
		lg_p->max_label_draw_count = count;
	}

/*
 * Find the size of the longest text string, 
 * then determine a preliminary text size - text angle is accounted for later.
 * If auto-manage is off the size set by the user will override this
 * value.
 */
	for (i=0; i<count; i++) {
		if ((itmp = strlen(labels_p[i])) > max_strlen) {
			max_strlen = itmp;
		}
	}

/*
 * Determine the overall area available for the labels
 */
	if (lg_p->label_pos == NhlCENTER) {
		if (lg_p->orient == NhlHORIZONTAL) {
			larea.x = lg_p->labels.r - lg_p->labels.l;
			larea.y = lg_p->adj_bar.t - 
				lg_p->adj_bar.b - lg_p->label_off_ndc;
		}
		else {
			larea.x = lg_p->adj_bar.r - 
				lg_p->adj_bar.l - lg_p->label_off_ndc;
			larea.y = lg_p->labels.t - lg_p->labels.b;
		}
	}
	else {
		if (lg_p->orient == NhlHORIZONTAL) {
			larea.x = lg_p->labels.r - lg_p->labels.l;
			larea.y = lg_p->labels.t - lg_p->labels.b - 
				lg_p->label_off_ndc;
		}
		else {
			larea.x = lg_p->labels.r - lg_p->labels.l - 
				lg_p->label_off_ndc;
			larea.y = lg_p->labels.t - lg_p->labels.b;
		}
	}
		
/*
 * Account for the box_major_ext fraction if centered or label
 * offset is negative
 */
	if (lg_p->label_pos == NhlCENTER || lg_p->label_off_ndc < 0.0) {
		if (lg_p->label_alignment == NhlITEMCENTERS) {
			c_frac = lg_p->box_major_ext;
		}
		else if (lg_p->box_major_ext > 0.8) {
			c_frac = 1.0;
		}
		else {
			c_frac = 1.0 - lg_p->box_major_ext;
		}
	}
		
	if (lg_p->orient == NhlHORIZONTAL && 
	    lg_p->label_direction == NhlACROSS) {

		char_space = 0.8 * larea.y;
		avail_char_space = c_frac * 0.8 * 
			larea.x / (lg_p->label_draw_count * max_strlen);

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		switch (lg_p->label_pos) {
		case NhlLEFT:
		case NhlRIGHT:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlBOTTOM:
			lg_p->const_pos = lg_p->labels.t - lg_p->label_off_ndc;
			break;
		case NhlCENTER:
			lg_p->const_pos = lg_p->adj_bar.b + 
				lg_p->adj_box_size.y / 2.0;
			break;
		case NhlTOP:
			lg_p->const_pos = lg_p->labels.b + lg_p->label_off_ndc;
			break;
		}
	}
	else if (lg_p->orient == NhlHORIZONTAL){ /* NhlDOWN or UP */

		/* Set the font height */

		char_space = 0.8 * larea.y / max_strlen;
		avail_char_space = c_frac * 0.8 * 
			larea.x / lg_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		switch (lg_p->label_pos) {
		case NhlLEFT:
		case NhlRIGHT:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlBOTTOM:
			lg_p->const_pos = lg_p->labels.t - lg_p->label_off_ndc;
			break;
		case NhlCENTER:
			lg_p->const_pos = lg_p->adj_bar.b + 
				lg_p->adj_box_size.y / 2.0;
			break;
		case NhlTOP:
			lg_p->const_pos = lg_p->labels.b + lg_p->label_off_ndc;
			break;
		}
	}
	else if (lg_p->orient == NhlVERTICAL && 
		 lg_p->label_direction == NhlACROSS) {

		char_space = 0.8 * larea.x / max_strlen;
		avail_char_space = c_frac * 0.8 * 
			larea.y / lg_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		switch (lg_p->label_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlLEFT:
			lg_p->const_pos = lg_p->labels.r - lg_p->label_off_ndc;
			break;
		case NhlCENTER:
			lg_p->const_pos = lg_p->adj_bar.l + 
				lg_p->adj_box_size.x / 2.0;
			break;
		case NhlRIGHT:
			lg_p->const_pos = lg_p->labels.l + lg_p->label_off_ndc;
			break;
		}
	}
	else { /* NhlVERTICAL NhlDOWN or UP */

		char_space = 0.8 * larea.x;
		avail_char_space = c_frac * 0.5 * 
			larea.y / (lg_p->label_draw_count * max_strlen);

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		switch (lg_p->label_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlLEFT:
			lg_p->const_pos = lg_p->labels.r - lg_p->label_off_ndc;
			break;
		case NhlCENTER:
			lg_p->const_pos = lg_p->adj_bar.l + 
				lg_p->adj_box_size.x / 2.0;
			break;
		case NhlRIGHT:
			lg_p->const_pos = lg_p->labels.l + lg_p->label_off_ndc;
			break;
		}
	}

/*
 * Now find the variable label positions
 */

	if (lg_p->orient == NhlHORIZONTAL) {
		
		/* position array contains X values */
		
		base_pos = lg_p->adj_bar.l;
		
		/* determine offset */
		if (lg_p->label_alignment == NhlITEMCENTERS)
			offset = lg_p->adj_box_size.x / 2.0;
		else if (lg_p->label_alignment == NhlABOVEITEMS)
			offset = lg_p->adj_box_size.x;
		else
			offset = 0;
		
		increment = lg_p->adj_box_size.x * lg_p->label_stride;
	}
	else if (lg_p->orient == NhlVERTICAL) {
		
		/* position array contains Y values */
		
		base_pos = lg_p->adj_bar.b;
		
		/* determine offset */
		if (lg_p->label_alignment == NhlITEMCENTERS)
			offset = lg_p->adj_box_size.y / 2.0;
		else if (lg_p->label_alignment == NhlABOVEITEMS)
			offset = lg_p->adj_box_size.y;
		else
			offset = 0;
		
		increment = lg_p->adj_box_size.y * lg_p->label_stride;
	}
	if (lg_p->item_placement == NhlUNIFORMPLACEMENT) {
		for (i=0; i<lg_p->label_draw_count; i++) {
			label_loc = base_pos + offset + (float) i * increment;
			if (! label_locs_changed &&
			    _NhlCmpFAny(label_loc,lg_p->label_locs[i],6)) 
				label_locs_changed = True;
			lg_p->label_locs[i] = label_loc;
		}
	}
	else {
		if (lg_p->orient == NhlHORIZONTAL)
			offset = lg_p->adj_box_size.x/2.0;
		else
			offset = lg_p->adj_box_size.y/2.0;
		for (i=0; i < lg_p->label_draw_count; i++) {

			ix = i * lg_p->label_stride;
			if (lg_p->label_alignment == NhlITEMCENTERS)
				label_loc = lg_p->item_locs[ix];
			else if (lg_p->label_alignment == NhlABOVEITEMS) {
				float t;
				if (i == lg_p->label_draw_count - 1)
					t = lg_p->item_locs[ix+1] - 
						lg_p->item_locs[ix];
				else 
					t = 0.5 * (lg_p->item_locs[ix+1] - 
						lg_p->item_locs[ix]);
				label_loc = lg_p->item_locs[ix] +
					MIN(t, offset);
			}
			else {
				float t;
				if (i == 0) 
					t = lg_p->item_locs[ix] - base_pos;
				else
					t = 0.5 * (lg_p->item_locs[ix] - 
						   lg_p->item_locs[ix-1]);
				label_loc = lg_p->item_locs[ix] -
					MIN(t, offset);
			}
			if (! label_locs_changed &&
			    _NhlCmpFAny(label_loc,lg_p->label_locs[i],6)) 
				label_locs_changed = True;
			lg_p->label_locs[i] = label_loc;
		}
	}

	if (lg_p->label_height <= 0.0)
		lg_p->label_height = NhlLG_DEF_CHAR_HEIGHT;
	if (! lg_p->auto_manage) 
		label_height = lg_p->label_height;

	lg_p->label_height = label_height;
	angle = (lg_p->label_angle < 0.0) ? 
		lg_p->label_angle + 360.0 : lg_p->label_angle;
	if (lg_p->labels_id < 0) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Labels");
		subret = NhlVACreate(&(lg_p->labels_id),buffer,
				 NhlmultiTextClass,tnew->base.id,
				 NhlNMtextNumStrings,lg_p->label_draw_count,
				 NhlNMtextStrings,labels_p,
				 NhlNMtextOrientation,mtext_orient,
				 NhlNMtextConstPosF,lg_p->const_pos ,
				 NhlNMtextPosArray,lg_p->label_locs,
				 NhlNMtextAutoStride,
				     lg_p->label_auto_stride,
				 NhlNtxAngleF,angle,
				 NhlNtxFont,lg_p->label_font,
				 NhlNtxJust,lg_p->label_just,
				 NhlNtxFontHeightF,lg_p->label_height,
				 NhlNtxFontAspectF,lg_p->label_aspect,
				 NhlNtxDirection,lg_p->label_direction,
				 NhlNtxConstantSpacingF,
				 lg_p->label_const_spacing,
				 NhlNtxFontColor,lg_p->label_color,
				 NhlNtxFontThicknessF,lg_p->label_thickness,
				 NhlNtxFuncCode,lg_p->label_func_code,    
				 NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error creating MultiText object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		olg_p->label_height = lg_p->label_height;
		olg_p->label_just = lg_p->label_just;
		olg_p->const_pos = lg_p->const_pos;
		olg_p->label_auto_stride = lg_p->label_auto_stride;
	} 
	else {
		/* 
		 * here everything is set that might indirectly affect the 
		 * font height. These must be set before the multitext is
		 * queried for the max_len, because its value depends on
		 * these resources as well as on the font height. The 
		 * intention is to avoid this SetValues call except when
		 * necessary.
		 */

		nargs = 0;
		if (lg_p->label_draw_count != olg_p->label_draw_count ||
		    lg_p->label_strings != olg_p->label_strings ||
		    lg_p->label_stride != olg_p->label_stride) {
			NhlSetSArg(&sargs[nargs++],NhlNMtextNumStrings,
				   lg_p->label_draw_count);
			NhlSetSArg(&sargs[nargs++],NhlNMtextStrings,labels_p);
			NhlSetSArg(&sargs[nargs++],
				   NhlNMtextPosArray,lg_p->label_locs);
		}
		if (lg_p->orient != olg_p->orient)
			NhlSetSArg(&sargs[nargs++],NhlNMtextOrientation,
				   mtext_orient);
		if (lg_p->label_angle != olg_p->label_angle)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxAngleF,angle);
		if (lg_p->label_direction != olg_p->label_direction)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxDirection,lg_p->label_direction);
		if (lg_p->label_font != olg_p->label_font)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxFont,lg_p->label_font);
		if (lg_p->label_aspect != olg_p->label_aspect)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxFontAspectF,lg_p->label_aspect);
		if (lg_p->label_thickness != olg_p->label_thickness)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxFontThicknessF,lg_p->label_thickness);
		if (lg_p->label_const_spacing != olg_p->label_const_spacing)
			NhlSetSArg(&sargs[nargs++],NhlNtxConstantSpacingF,
				   lg_p->label_const_spacing);
		if (lg_p->label_func_code != olg_p->label_func_code)
			NhlSetSArg(&sargs[nargs++],NhlNtxFuncCode,
				   lg_p->label_func_code);
		subret = NhlALSetValues(lg_p->labels_id,sargs,nargs);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error setting MultiText object values";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	if (lg_p->auto_manage || lg_p->label_height <= 0.0) {
		subret = AdjustLabels(lg_p,olg_p,
				      label_height,avail_char_space,
				      larea.x,larea.y,entry_name);
		ret = MIN(ret, subret);
	}

	nargs = 0;
	if (label_locs_changed) {
		NhlSetSArg(&sargs[nargs++],
			   NhlNMtextPosArray,lg_p->label_locs);
	}
	if (lg_p->const_pos != olg_p->const_pos)
		NhlSetSArg(&sargs[nargs++],
			   NhlNMtextConstPosF,lg_p->const_pos);
	if (lg_p->label_just != olg_p->label_just)
		NhlSetSArg(&sargs[nargs++],
			   NhlNtxJust,lg_p->label_just);
	if (lg_p->label_height != olg_p->label_height)
		NhlSetSArg(&sargs[nargs++],NhlNtxFontHeightF,
			   lg_p->label_height);
	if (lg_p->label_color != olg_p->label_color)
		NhlSetSArg(&sargs[nargs++],
			   NhlNtxFontColor,lg_p->label_color);
	if (lg_p->label_auto_stride != olg_p->label_auto_stride)
		NhlSetSArg(&sargs[nargs++],NhlNMtextAutoStride,
			   lg_p->label_auto_stride);

	subret = NhlALSetValues(lg_p->labels_id,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: Error setting MultiText object values";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}


	subret = NhlVAGetValues(lg_p->labels_id,
				NhlNvpXF,&x,
				NhlNvpYF,&y,
				NhlNvpWidthF,&width,
				NhlNvpHeightF,&height,
				NhlNMtextConstPosF,&lg_p->const_pos,
				NULL);
/*
 * store the actual label bounding box in the 'xtr' variables.
 */
	lg_p->labels.lxtr = x;
	lg_p->labels.rxtr = x + width;
	lg_p->labels.bxtr = y - height;
	lg_p->labels.txtr = y;

        if (olabels) NhlFree (olabels);
	return (ret);
}

/*
 * Function: AdjustLabels
 *
 * Description:	Adjusts the label height to fit the size it has available. 
 * 	The routine ensures that under any rotation,  
 * 	no piece of multitext overlaps another piece of the 
 * 	multi-text. Adjusts the text size based on the label geometry
 * 	to accomplish this goal. Also the label text justification is
 * 	managed to ensure that the label always lines up with its correct box.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: Modifies the text height and justification of the child
 *	multi-text object
 */

#define NhlLG_TRANSITIONANGLE 7.5
static NhlErrorTypes   	AdjustLabels
#if	NhlNeedProto
(
	NhlLegendLayerPart *lg_p,
	NhlLegendLayerPart *olg_p,
 	float		height,		/* not using this now */
	float		avail_space,	/* or this */
	float		area_x,
	float		area_y,
	char		*entry_name
)
#else
(lg_p,olg_p,height,avail_space,area_x area_y,entry_name)
	NhlLegendLayerPart *lg_p;
	NhlLegendLayerPart *olg_p,
	float		height;
	float		avail_space;
	float		area_x;
	float		area_y;
	char		*entry_name;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	float theta1, theta2, theta3, theta4;
	float w, h;
	float t1;
	float theta;
	float max_len;
	float avail_len;
	float trans_angle;
	float test_angle;
	float box_major, box_minor;
	float angle;
	float space_fac = 0.2;

	angle = (lg_p->label_angle < 0.0) ? 
		lg_p->label_angle + 360.0 : lg_p->label_angle;

/*
 * Get the width and height and the maximum text length of the multitext.
 */
	subret = NhlVAGetValues(lg_p->labels_id,
				NhlNMtextMaxLenF,&max_len,
				NhlNvpWidthF,&w,
				NhlNvpHeightF,&h,
				NhlNtxFontHeightF,&height,
				NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING || w <= 0.0 || h <= 0.0) {
		e_text = "%s: Error getting label information";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

/*
 * Set up the variables used to fit the text to the available space
 * If the text basically parallels the length of the label bar then a fixed
 * space is required to separate the labels. The space_fac introduces a little
 * more space for longer strings.
 */

	if (lg_p->orient == NhlHORIZONTAL) {
		box_major = area_x / lg_p->label_draw_count;
		box_minor = area_y;

		if (lg_p->label_direction == NhlACROSS &&
		    fabs(fmod(lg_p->label_angle,180.)) 
		    < NhlLG_TRANSITIONANGLE) {
			box_major -= height;
			space_fac = 0.1;
		}
	}
	else {
		box_major = area_y / lg_p->label_draw_count;
		box_minor = area_x;
		if (lg_p->label_direction == NhlDOWN &&
		    fabs(fmod(lg_p->label_angle,180.)) 
		    < NhlLG_TRANSITIONANGLE) {
			box_major -= height;
			space_fac = 0.1;
		}
	}

/*
 * Size is adjusted to the text box based on the 
 * first quadrant equivalent angle.
 */

	if (angle < 90.0)
		test_angle = angle;
	else if (angle < 180.0)
		test_angle = 180.0 - angle;
	else if (angle < 270.0)
		test_angle = angle - 180.0;
	else 
		test_angle = 360.0 - angle;
		

	if (lg_p->label_direction == NhlDOWN)
		test_angle = 90.0 - test_angle;
	if (lg_p->orient == NhlHORIZONTAL)
		test_angle = 90.0 - test_angle;

	theta = DEGTORAD * test_angle;

	trans_angle = atan2(box_major,box_minor);
	if (theta < trans_angle) {
		float frac_major;
		frac_major = box_minor * tan(theta);
		avail_len = 
			sqrt(frac_major * frac_major + box_minor * box_minor);
	}
	else {
		float frac_minor;
		frac_minor = box_major * tan((DEGTORAD * 90) - theta);
		avail_len = 
			sqrt(frac_minor * frac_minor + box_major * box_major);
	}

	t1 = avail_len / max_len * (1.0 - space_fac * sin(theta)) ;
	height *= t1;

/*
 * Now make further adjustments to eliminate text crossover resulting 
 * from the angle at which the text is placed.
 * A diagram is really required in order to clarify the role of each 
 * of the variables used here.
 */

	if (90 - test_angle > 7.5) {
		float x1,y1,y2,ly,len,xover;

		y1 = (height / 2.0) * cos(theta);
		x1 = (height / 2.0) * sin(theta);
		y2 = x1 * tan(theta) + y1;
		ly = box_major - y2;
		len = ly * cos(theta);
		xover = height / 2.0 - len;
		
		if (xover > 0.0) {
			float text_len;
			float x2,x3;

			text_len = t1 * max_len;
			x2 = - x1 + text_len * cos(theta);
			x3 = len * sin(theta);
			if (x2 > x3)
				height -= xover;
		}
		height = MIN(height, 0.6 * box_major);
	}
	lg_p->label_height = height;

/*
 * Manage the text justification.
 */

/*
 * Get the sin and cos of the label angle - then create some
 * permutations of the angle in order to set the justification properly
 */
	theta1 = angle < 360.0 - angle ? angle : 360.0 - angle;
	theta2 = fabs(90.0 - angle);
	theta3 = fabs(180.0 - angle);
	theta4 = fabs(270.0 - angle);

/*
 * If labels are centered then center justification is always used.
 * Otherwise the justification depends on the angle.
 */

	if (lg_p->label_pos == NhlCENTER) {
		lg_p->label_just = NhlCENTERCENTER;
	}
	else if (lg_p->orient == NhlHORIZONTAL && 
	    lg_p->label_direction == NhlACROSS) {
		if (theta1 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlTOPCENTER;
			}
			else {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (theta3 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lg_p->label_just = NhlTOPCENTER;
			}
		}
		else if (angle < 180.0) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lg_p->label_just = NhlCENTERLEFT;
			}
		}
		else {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlCENTERLEFT;
			}
			else {
				lg_p->label_just = NhlCENTERRIGHT;
			}
		}
	}
	else if (lg_p->orient == NhlHORIZONTAL){ /* NhlDOWN or UP */
		if (theta2 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lg_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta4 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlCENTERLEFT;
			}
			else {
				lg_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (angle > 270.0 || angle < 90.0) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlTOPCENTER;
			}
			else {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lg_p->label_just = NhlTOPCENTER;
			}
		}
			 
	}
	else if (lg_p->orient == NhlVERTICAL && 
		 lg_p->label_direction == NhlACROSS) {
		if (theta2 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lg_p->label_just = NhlTOPCENTER;
			}
		}
		else if (theta4 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlTOPCENTER;
			}
			else {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (angle > 270.0  || angle < 90.0) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lg_p->label_just = NhlCENTERLEFT;
			}
		}
		else {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlCENTERLEFT;
			}
			else {
				lg_p->label_just = NhlCENTERRIGHT;
			}
		}
			 
	}
	else { /* NhlVERTICAL NhlDOWN or UP */

		if (theta1 <=NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lg_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta3 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlCENTERLEFT;
			}
			else {
				lg_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (angle < 180.0) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lg_p->label_just = NhlTOPCENTER;
			}
		}
		else {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlTOPCENTER;
			}
			else {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
		}
	}
	return (ret);
}

/*
 * Function: AdjustGeometry
 *
 * Description: Called after all individual elements of the Legend have been
 *	sized in order to ensure that no elements overlap and that the 
 *	overall size of the Legend is big enough to accommodate the specified
 *	size of each element in the Legend. When auto-manage mode is turned
 *     	both the width and height of the Legend may be expanded; when 
 *	auto-manage is on, only one direction (the major axis) may expand.
 *	Elements of the Legend may be moved relative to each other in order
 *	to eliminate overlap. Finally the routine adjusts the Legend position
 *	as a whole depending on the setting of the Legend justification
 *	resource.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The bounding box of the Legend may be modified, and the
 *	title and label child objects may be repositioned.
 */

/*ARGSUSED*/
static NhlErrorTypes    AdjustGeometry
#if	NhlNeedProto
(
	NhlLayer	new, 
	NhlLayer	old,
	int		init,
	_NhlArgList	args,
	int		num_args
)
#else
(new,old,init,args,num_args)
	NhlLayer	new;
	NhlLayer	old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLegendLayer	tnew = (NhlLegendLayer) new;
	NhlLegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	NhlBoundingBox legendBB;
	NhlBoundingBox tmpBB,bar_and_labelsBB;
	float title_x = lg_p->title_x;
	float title_y = lg_p->title_y;
	float pos_offset = 0.0;
	float obj_offset = 0.0;
	float x_off, y_off;
	float minor_off;
	float major_off;
	float center_off;
	float p_width,p_height,ex_width, ex_height;
	int i;

	bar_and_labelsBB.b = lg_p->adj_perim.b;
	bar_and_labelsBB.t = lg_p->adj_perim.t;
	bar_and_labelsBB.l = lg_p->adj_perim.l;
	bar_and_labelsBB.r = lg_p->adj_perim.r;
	if (lg_p->title_on && lg_p->title_ext > 0.0) {
		switch (lg_p->title_pos) {
		case NhlTOP:
			bar_and_labelsBB.t = 
				lg_p->title.b - lg_p->title_off_ndc;
			break;
		case NhlBOTTOM:
			bar_and_labelsBB.b = 
				lg_p->title.t + lg_p->title_off_ndc;
			break;
		case NhlLEFT:
			bar_and_labelsBB.l = 
				lg_p->title.r + lg_p->title_off_ndc;
			break;
		case NhlRIGHT:
			bar_and_labelsBB.r = 
				lg_p->title.l - lg_p->title_off_ndc;
			break;
		default:
			break;
		}
	}

/*
 * Get the bounding box of the labels, then adjust the box if it
 * overlaps the bar boundary. Next combine the two bounding boxes,
 * and adjust both the bar and the labels to the center of the combined box.
 */

	if (! lg_p->labels_on) {
		tmpBB.l = lg_p->adj_bar.l;
		tmpBB.r = lg_p->adj_bar.r;
		tmpBB.b = lg_p->adj_bar.b;
		tmpBB.t = lg_p->adj_bar.t;
	}
	else {
		if (lg_p->orient == NhlHORIZONTAL) {
			
			if (lg_p->label_pos == NhlTOP) {
				pos_offset = lg_p->adj_bar.t + 
				       lg_p->label_off_ndc - lg_p->labels.bxtr;
			}
			else if (lg_p->label_pos == NhlBOTTOM) {
				pos_offset = lg_p->adj_bar.b - 
				       lg_p->label_off_ndc - lg_p->labels.txtr;
			}
			lg_p->labels.bxtr += pos_offset;
			lg_p->labels.txtr += pos_offset;
		}
		else {
			if (lg_p->label_pos == NhlRIGHT) {
				pos_offset = lg_p->adj_bar.r + 
				       lg_p->label_off_ndc - lg_p->labels.lxtr;
			}
			else if (lg_p->label_pos == NhlLEFT) {
				pos_offset = lg_p->adj_bar.l - 
				       lg_p->label_off_ndc - lg_p->labels.rxtr;
			}
			lg_p->labels.lxtr += pos_offset;
			lg_p->labels.rxtr += pos_offset;
		}
		
		tmpBB.l = MIN(lg_p->labels.lxtr, lg_p->adj_bar.l);
		tmpBB.r = MAX(lg_p->labels.rxtr, lg_p->adj_bar.r);
		tmpBB.b = MIN(lg_p->labels.bxtr, lg_p->adj_bar.b);
		tmpBB.t = MAX(lg_p->labels.txtr, lg_p->adj_bar.t);
	}

	legendBB.l = MIN(tmpBB.l, bar_and_labelsBB.l);
	legendBB.r = MAX(tmpBB.r, bar_and_labelsBB.r);
	legendBB.b = MIN(tmpBB.b, bar_and_labelsBB.b);
	legendBB.t = MAX(tmpBB.t, bar_and_labelsBB.t);

	if (lg_p->orient == NhlHORIZONTAL) {

		center_off = (legendBB.t - tmpBB.t +
			      legendBB.b - tmpBB.b) / 2.0;
		lg_p->labels.bxtr += center_off;
		lg_p->labels.txtr += center_off;
		lg_p->adj_bar.b += center_off;
		lg_p->adj_bar.t += center_off;
		pos_offset += center_off;
	}
	else {
		center_off = (legendBB.r - tmpBB.r +
			      legendBB.l - tmpBB.l) / 2.0;
		lg_p->labels.lxtr += center_off;
		lg_p->labels.rxtr += center_off;
		lg_p->adj_bar.l += center_off;
		lg_p->adj_bar.r += center_off;
		pos_offset += center_off;
	}		
		
/*
 * Adjust for the title: 
 * first move it out of the way of the legend, 
 * then adjust for possibly larger justification area.
 */

	if (lg_p->title_on) {
		if (lg_p->title_pos == NhlBOTTOM &&
		    lg_p->title.txtr > legendBB.b - lg_p->title_off_ndc) {
			title_y -= lg_p->title.txtr + 
				lg_p->title_off_ndc - legendBB.b;
		}
		else if (lg_p->title_pos == NhlTOP &&
			 lg_p->title.bxtr < legendBB.t + lg_p->title_off_ndc) {
			title_y += legendBB.t + 
				lg_p->title_off_ndc - lg_p->title.bxtr;
		}
		else if (lg_p->title_pos == NhlLEFT &&
			 lg_p->title.rxtr > legendBB.l - lg_p->title_off_ndc) {
			title_x -= lg_p->title.rxtr + 
				lg_p->title_off_ndc - legendBB.l;
		}
		else if (lg_p->title_pos == NhlRIGHT &&
			 lg_p->title.lxtr < legendBB.r + obj_offset) {
			title_x += legendBB.r + 
				lg_p->title_off_ndc - lg_p->title.lxtr;
		}
		
		if (lg_p->title_pos == NhlTOP || 
		    lg_p->title_pos == NhlBOTTOM) {
			switch (lg_p->title_just) {
			case NhlBOTTOMLEFT:
			case NhlCENTERLEFT:
			case NhlTOPLEFT:
				title_x = legendBB.l;
				break;
			case NhlBOTTOMCENTER:
			case NhlCENTERCENTER:
			case NhlTOPCENTER:
			default:
				title_x = legendBB.l + 
					(legendBB.r - legendBB.l) / 2.0;
				break;
			case NhlBOTTOMRIGHT:
			case NhlCENTERRIGHT:
			case NhlTOPRIGHT:
				title_x = legendBB.r;
				break;
			}
		}
		else if (lg_p->title_pos == NhlLEFT || 
			 lg_p->title_pos == NhlRIGHT) {
			switch (lg_p->title_just) {
			case NhlBOTTOMLEFT:
			case NhlBOTTOMCENTER:
			case NhlBOTTOMRIGHT:
				title_y = legendBB.b;
				break;
			case NhlCENTERLEFT:
			case NhlCENTERCENTER:
			case NhlCENTERRIGHT:
			default:
				title_y = legendBB.b + 
					(legendBB.t - legendBB.b) / 2.0;
				break;
			case NhlTOPLEFT:
			case NhlTOPCENTER:
			case NhlTOPRIGHT:
				title_y = legendBB.t;
				break;
			}
		}
		lg_p->title.lxtr += title_x - lg_p->title_x;
		lg_p->title.rxtr += title_x - lg_p->title_x;
		lg_p->title.bxtr += title_y - lg_p->title_y;
		lg_p->title.txtr += title_y - lg_p->title_y;

/*
 * now get the real bounding box of the legend
 */
		legendBB.l = MIN(legendBB.l,lg_p->title.lxtr);
		legendBB.r = MAX(legendBB.r,lg_p->title.rxtr);
		legendBB.b = MIN(legendBB.b,lg_p->title.bxtr);
		legendBB.t = MAX(legendBB.t,lg_p->title.txtr);
	}

/*
 * Determine the external perimeter
 */
	lg_p->perim.lxtr = legendBB.l - lg_p->margin.l * lg_p->small_axis;
	lg_p->perim.rxtr = legendBB.r + lg_p->margin.r * lg_p->small_axis;
	lg_p->perim.bxtr = legendBB.b - lg_p->margin.b * lg_p->small_axis;
	lg_p->perim.txtr = legendBB.t + lg_p->margin.t * lg_p->small_axis;
/*
 * Adjust position based on the justification
 */
	switch (lg_p->just) {
	case NhlBOTTOMLEFT:
		x_off = lg_p->perim.lxtr - lg_p->lg_x;
		y_off = lg_p->perim.bxtr - (lg_p->lg_y - lg_p->lg_height);
		break;
	case NhlCENTERLEFT:
		x_off = lg_p->perim.lxtr - lg_p->perim.l;
		y_off = lg_p->perim.bxtr + 
			(lg_p->perim.txtr - lg_p->perim.bxtr)/2.0 -
				(lg_p->lg_y - lg_p->lg_height/2.0);
		break;
	case NhlTOPLEFT:
		x_off = lg_p->perim.lxtr - lg_p->perim.l;
		y_off = lg_p->perim.txtr - lg_p->perim.t;
		break;
	case NhlBOTTOMCENTER:
		x_off = lg_p->perim.lxtr + 
			(lg_p->perim.rxtr - lg_p->perim.lxtr)/2.0 -
				(lg_p->lg_x + lg_p->lg_width/2.0);
		y_off = lg_p->perim.bxtr - lg_p->perim.b;
		break;
	case NhlCENTERCENTER:
	default:
		x_off = lg_p->perim.lxtr + 
			(lg_p->perim.rxtr - lg_p->perim.lxtr)/2.0 -
				(lg_p->lg_x + lg_p->lg_width/2.0);
		y_off = lg_p->perim.bxtr + 
			(lg_p->perim.txtr - lg_p->perim.bxtr)/2.0 -
				(lg_p->lg_y - lg_p->lg_height/2.0);
		break;
	case NhlTOPCENTER:
		x_off = lg_p->perim.lxtr + 
			(lg_p->perim.rxtr - lg_p->perim.lxtr)/2.0 -
				(lg_p->lg_x + lg_p->lg_width/2.0);
		y_off = lg_p->perim.txtr - lg_p->perim.t;
		break;
	case NhlBOTTOMRIGHT:
		x_off = lg_p->perim.rxtr - lg_p->perim.r;
		y_off = lg_p->perim.bxtr - lg_p->perim.b;
		break;
	case NhlCENTERRIGHT:
		x_off = lg_p->perim.rxtr - lg_p->perim.r;
		y_off = lg_p->perim.bxtr + 
			(lg_p->perim.txtr - lg_p->perim.bxtr)/2.0 -
				(lg_p->lg_y - lg_p->lg_height/2.0);
		break;
	case NhlTOPRIGHT:
		x_off = lg_p->perim.rxtr - lg_p->perim.r;
		y_off = lg_p->perim.txtr - lg_p->perim.t;
		break;
	}

	minor_off = (lg_p->orient == NhlHORIZONTAL) ? y_off : x_off;
	major_off = (lg_p->orient == NhlHORIZONTAL) ? x_off : y_off;
/*
 * Adjust the external perimeter
 */
	lg_p->perim.lxtr -= x_off;
	lg_p->perim.rxtr -= x_off;
	lg_p->perim.bxtr -= y_off;
	lg_p->perim.txtr -= y_off;

/*
 * Adjust the nominal perimeter
 */

	p_width = lg_p->perim.r - lg_p->perim.l;
	p_height = lg_p->perim.t - lg_p->perim.b;
	ex_width = MAX(0.0, 
		       (lg_p->perim.rxtr - lg_p->perim.lxtr) - p_width);
	ex_height = MAX(0.0, 
			(lg_p->perim.txtr - lg_p->perim.bxtr) - p_height);

	switch (lg_p->just) {
	case NhlBOTTOMLEFT:
		lg_p->perim.l = lg_p->perim.lxtr;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	case NhlCENTERLEFT:
		lg_p->perim.l = lg_p->perim.lxtr;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr + ex_height / 2.0;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	case NhlTOPLEFT:
		lg_p->perim.l = lg_p->perim.lxtr;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr + ex_height;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	case NhlBOTTOMCENTER:
		lg_p->perim.l = lg_p->perim.lxtr + ex_width / 2.0;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	case NhlCENTERCENTER:
	default:
		lg_p->perim.l = lg_p->perim.lxtr + ex_width / 2.0;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr + ex_height / 2.0;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	case NhlTOPCENTER:
		lg_p->perim.l = lg_p->perim.lxtr + ex_width / 2.0;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr + ex_height;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	case NhlBOTTOMRIGHT:
		lg_p->perim.l = lg_p->perim.lxtr + ex_width;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	case NhlCENTERRIGHT:
		lg_p->perim.l = lg_p->perim.lxtr + ex_width;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr + ex_width / 2.0;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	case NhlTOPRIGHT:
		lg_p->perim.l = lg_p->perim.lxtr + ex_width;
		lg_p->perim.r = lg_p->perim.l + p_width;
		lg_p->perim.b = lg_p->perim.bxtr + ex_height;
		lg_p->perim.t = lg_p->perim.b + p_height;
		break;
	}
	
/*
 * Adjust the bar position
 */

	lg_p->adj_bar.l -= x_off;
	lg_p->adj_bar.r -= x_off;
	lg_p->adj_bar.b -= y_off;
	lg_p->adj_bar.t -= y_off;
	for (i=0; i<lg_p->item_count+1; i++) {
		lg_p->item_locs[i] -= major_off;
	}
/*
 * Adjust the labels position
 */

	if (lg_p->labels_on) {
		NhlSArg	sargs[2];
		int	nargs = 0;

		if (_NhlCmpFAny(0.0,major_off,6)) {
			for (i=0; i<lg_p->label_draw_count; i++) {
				lg_p->label_locs[i] -= major_off;
			}
			NhlSetSArg(&sargs[nargs++],
				   NhlNMtextPosArray,lg_p->label_locs);
		}
		if (_NhlCmpFAny(0.0,pos_offset-minor_off,6)) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNMtextConstPosF,
				   lg_p->const_pos + pos_offset - minor_off);
		}
			
		if ((ret1 = NhlALSetValues(lg_p->labels_id,sargs,nargs))
		    < NhlWARNING)
			return (ret1);
		ret = MIN(ret1,ret);
	}
/*
 * Set the title position
 */

	if (lg_p->title_on && lg_p->title_ext > 0.0) {
		title_x -= x_off;
		title_y -= y_off;
		lg_p->title_x = title_x;
		lg_p->title_y = title_y;
		if ((ret1 = NhlVASetValues(lg_p->title_id,
					 NhlNtxPosXF, title_x,
					 NhlNtxPosYF, title_y,
					 NULL)) < NhlWARNING)
			return (ret1);
		ret = MIN(ret1,ret);
	}

	lg_p->lg_x = lg_p->perim.lxtr;
	lg_p->lg_y = lg_p->perim.txtr;
	lg_p->lg_width = lg_p->perim.rxtr - lg_p->perim.lxtr;
	lg_p->lg_height = lg_p->perim.txtr - lg_p->perim.bxtr;

	_NhlInternalSetView((NhlViewLayer)tnew,
			    lg_p->lg_x,lg_p->lg_y,
			    lg_p->lg_width,lg_p->lg_height,
			    False);
	return (ret);

}

/*
 * Function:	LegendGetValues
 *
 * Description:	Retrieves the current setting of one or more Legend resources.
 *	This routine only retrieves resources that require special methods
 *	that the generic GetValues method cannot handle. For now this means
 *	all the GenArray resources. Note that space is allocated; the user
 *	is responsible for freeing this space.
 *	
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 *	Memory is allocated when any of the following resources are retrieved:
 *		NhlNlgItemTypes
 *		NhlNlgDashIndexes
 *		NhlNlgMarkerIndexes
 *		NhlNlgLineLabelStrings
 *		NhlNlgLineColors
 *		NhlNlgMarkerColors
 *		NhlNlgLineThicknesses
 *		NhlNlgMarkerThicknesses
 *		NhlNlgItemFontHeights
 *		NhlNlgLabelStrings
 *		NhlNlgLabelFontColors
 *		NhlNlgItemPositions
 *		NhlNlgTitleString
 *	The caller is responsible for freeing this memory.
 */

static NhlErrorTypes	LegendGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	NhlLayer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	NhlLegendLayer lgl = (NhlLegendLayer)l;
	NhlLegendLayerPart *lg_p = &(lgl->legend);
	NhlGenArray ga;
	char *e_text;
	int i, count = 0;
	char *type = "";
	
	for( i = 0; i< num_args; i++ ) {

		ga = NULL;
		if(args[i].quark == Qitem_types) {
			ga = lg_p->item_types;
			count = lg_p->item_count;
			type = NhlNlgItemTypes;
		}
		else if (args[i].quark == Qdash_indexes) {
			ga = lg_p->dash_indexes;
			count = lg_p->item_count;
			type = NhlNlgDashIndexes;
		}
		else if (args[i].quark == Qmarker_indexes) {
			ga = lg_p->marker_indexes;
			count = lg_p->item_count;
			type = NhlNlgMarkerIndexes;
		}
		else if (args[i].quark == Qline_labels) {
			ga = lg_p->line_labels;
			count = lg_p->item_count;
			type = NhlNlgLineLabelStrings; 
		}
		else if (args[i].quark == Qline_colors) {
			ga = lg_p->line_colors;
			count = lg_p->item_count;
			type = NhlNlgLineColors;
		}
		else if (args[i].quark == Qmarker_colors) {
			ga = lg_p->marker_colors;
			count = lg_p->item_count;
			type = NhlNlgMarkerColors;
		}
		else if (args[i].quark == Qline_dash_seglens) {
			ga = lg_p->line_dash_seglens;
			count = lg_p->item_count;
			type = NhlNlgLineDashSegLens;
		}
		else if (args[i].quark == Qline_thicknesses) {
			ga = lg_p->line_thicknesses;
			count = lg_p->item_count;
			type = NhlNlgLineThicknesses;
		}
		else if (args[i].quark == Qmarker_thicknesses) {
			ga = lg_p->marker_thicknesses;
			count = lg_p->item_count;
			type = NhlNlgMarkerThicknesses;
		}
		else if (args[i].quark == Qline_label_font_heights) {
			ga = lg_p->line_label_font_heights;
			count = lg_p->item_count;
			type = NhlNlgLineLabelFontHeights;
		}
		else if (args[i].quark == Qmarker_sizes) {
			ga = lg_p->marker_sizes;
			count = lg_p->item_count;
			type = NhlNlgMarkerSizes;
		}
		else if (args[i].quark == Qlabel_strings) {
			ga = lg_p->label_strings;
			count = lg_p->item_count;
			type = NhlNlgLabelStrings;
		}
		else if (args[i].quark == Qllabel_colors) {
			ga = lg_p->line_label_colors;
			count = lg_p->item_count;
			type = NhlNlgLineLabelFontColors;
		}
		else if (args[i].quark == Qitem_positions) {
			ga = lg_p->item_positions;
			count = lg_p->item_count;
			type = NhlNlgItemPositions;
		}
		else if (args[i].quark == Qtitle_string) {
			if (lg_p->title_string != NULL) {
				(*(NhlString*)args[i].value.ptrval) = 
					(NhlString)
				     NhlMalloc(strlen(lg_p->title_string) +1);
				strcpy((*(NhlString*)args[i].value.ptrval),
				       lg_p->title_string);
			} else {
				(*(NhlString*)args[i].value.ptrval) = NULL;
			}
		}
		if (ga != NULL) {
			if ((ga = GenArraySubsetCopy(ga, count)) == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  "LegendGetValues",type);
				return NhlFATAL;
			}
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
	}

	return(NhlNOERROR);

}

/*
 * Function:  GenArraySubsetCopy
 *
 * Description:	Since the internal GenArrays maintained by the Legend object
 *	may be bigger than the size currently in use, this function allows
 *	a copy of only a portion of the array to be created. This is for
 * 	use by the GetValues routine when returning GenArray resources to
 *	the usr level. The array is assumed to be valid. The only pointer 
 *	type arrays that the routine can handle are NhlString arrays.
 *	Note: this might be another candidate for inclusion as a global 
 *	routine.
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
	(NhlGenArray	ga,
	int		length)
#else
(ga,length)
	NhlGenArray	ga;
	int		length;
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
 * Function:	LegendDraw
 *
 * Description:  Activates parent workstation, then draws the Legend 
 *		 perimeter and Legend items. Deactivates the workstation
 *		 and calls the draw functions for the Legend title
 *		 text object and the Legend labels multitext object.
 *
 * In Args: the instance to be drawn
 *
 * Out Args: NONE
 *
 * Return Values: Error conditions
 *
 * Side Effects: 
 */
static NhlErrorTypes    LegendDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
	NhlLayer 	layer;
#endif
{
	NhlLegendLayer lgl = (NhlLegendLayer) layer;
	NhlLegendLayerPart *lg_p = &(lgl->legend);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *e_text;
	char *entry_name = "LegendDraw";
	float xpoints[5];
	float ypoints[5];
	int i;
	int line_color,marker_color,line_label_color;
	int item_type, dash_index, marker_index;
	float line_label_font_height, line_dash_seglen, line_thickness;
	float marker_thickness, marker_size;
	float frac, dist, tcoord;
	int *line_colors, *marker_colors, *line_label_colors;
	int *dash_indexes;
	int *marker_indexes;
	int *types;
	float *line_dash_seglens;
	float *line_thicknesses;
	float *marker_thicknesses;
	float *font_heights;
	float *marker_sizes;
	NhlString *line_labels;
	float back_dist, for_dist;
	char *string;
	int *order;

	if (! lg_p->legend_on)
		return(ret);

	if (lgl->view.use_segments && ! lg_p->new_draw_req &&
	    lg_p->trans_dat && lg_p->trans_dat->id != NgNOT_A_SEGMENT) {
                subret = _NhlActivateWorkstation(lgl->base.wkptr);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDrawSegment(lg_p->trans_dat,
				_NhlWorkstationId(lgl->base.wkptr));
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDeactivateWorkstation(lgl->base.wkptr);
		return MIN(subret,ret);
	}
	lg_p->new_draw_req = False;

	subret = _NhlActivateWorkstation(lgl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	if (lgl->view.use_segments) {
		if (lg_p->trans_dat != NULL)
			_NhlDeleteViewSegment(layer, lg_p->trans_dat);
		if ((lg_p->trans_dat = _NhlNewViewSegment(layer)) == NULL) {
			e_text = "%s: error opening segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		subret = _NhlStartSegment(lg_p->trans_dat);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
	}

	NhlVASetValues(lgl->base.wkptr->base.id,
		_NhlNwkReset,	True,
		NULL);

/* first draw the perimeter: it may have a solid background */

	if (lg_p->perim_on) {

		xpoints[0] = lg_p->perim.lxtr;
		ypoints[0] = lg_p->perim.bxtr;
		xpoints[1] = lg_p->perim.rxtr;
		ypoints[1] = lg_p->perim.bxtr;
		xpoints[2] = lg_p->perim.rxtr;
		ypoints[2] = lg_p->perim.txtr;
		xpoints[3] = lg_p->perim.lxtr;
		ypoints[3] = lg_p->perim.txtr;
		xpoints[4] = lg_p->perim.lxtr;
		ypoints[4] = lg_p->perim.bxtr;

		NhlVASetValues(lgl->base.wkptr->base.id,
			     _NhlNwkEdgesOn, 1,
			     _NhlNwkEdgeDashPattern, lg_p->perim_dash_pattern,
			     _NhlNwkEdgeThicknessF, lg_p->perim_thickness,
			     _NhlNwkEdgeDashSegLenF, lg_p->perim_dash_seglen,
			     _NhlNwkEdgeColor, lg_p->perim_color,
			     _NhlNwkFillColor, lg_p->perim_fill_color,
			     _NhlNwkFillIndex, lg_p->perim_fill,
			     NULL);
			
		_NhlSetFillInfo(lgl->base.wkptr, layer);
		_NhlWorkstationFill(lgl->base.wkptr,
				    xpoints,ypoints,5);
			
	}

/*
 * Set the values that remain constant for all boxes
 */

	NhlVASetValues(lgl->base.wkptr->base.id,
		     _NhlNwkEdgesOn, lg_p->box_line_on,
		     _NhlNwkEdgeDashPattern, lg_p->box_line_dash_pattern,
		     _NhlNwkEdgeThicknessF, lg_p->box_line_thickness,
		     _NhlNwkEdgeDashSegLenF, lg_p->box_line_dash_seglen,
		     _NhlNwkEdgeColor, lg_p->box_line_color,
		     _NhlNwkFillColor, lg_p->box_background,
		     _NhlNwkFillIndex, NhlSOLIDFILL,
		     NULL);
				     
/* 
 * Draw the items
 */
	frac = lg_p->box_major_ext;
	line_colors = (int *) lg_p->line_colors->data;
	marker_colors = (int *) lg_p->marker_colors->data;
	line_label_colors =  (int *) lg_p->line_label_colors->data;
	dash_indexes = (int *) lg_p->dash_indexes->data;
	marker_indexes = (int *) lg_p->marker_indexes->data;
	types = (int *) lg_p->item_types->data;
	line_dash_seglens = (float *) lg_p->line_dash_seglens->data;
	line_thicknesses = (float *) lg_p->line_thicknesses->data;
	marker_thicknesses = (float *) lg_p->marker_thicknesses->data;
	font_heights = (float *) lg_p->line_label_font_heights->data;
	marker_sizes = (float *) lg_p->marker_sizes->data;
	line_labels = (NhlString *) lg_p->line_labels->data;
	order = lg_p->item_order == NULL ? NULL : lg_p->item_order->data;

	if (lg_p->orient == NhlHORIZONTAL) {

		ypoints[0] = lg_p->adj_bar.b;
		ypoints[1] = lg_p->adj_bar.b;
		ypoints[2] = lg_p->adj_bar.t;
		ypoints[3] = lg_p->adj_bar.t;
		ypoints[4] = lg_p->adj_bar.b;
		tcoord = ypoints[0] + (ypoints[2] - ypoints[1]) / 2.0;
		back_dist = lg_p->item_locs[0] - lg_p->adj_bar.l;
		
		for (i=0; i<lg_p->item_count; i++) {

			int nitem = order ? order[i] : i;

			for_dist = lg_p->item_locs[i+1] - lg_p->item_locs[i];
			for_dist = i < lg_p->item_count - 1 ? 
				for_dist/2.0 : for_dist;
			dist = MIN(back_dist,for_dist);
			back_dist = for_dist;
				   
			xpoints[0] = lg_p->item_locs[i] - dist * frac;
			xpoints[1] = lg_p->item_locs[i] + dist * frac;
			xpoints[2] = xpoints[1];
			xpoints[3] = xpoints[0];
			xpoints[4] = xpoints[0];
			
			_NhlSetFillInfo(lgl->base.wkptr, layer);
			_NhlWorkstationFill(lgl->base.wkptr,
					    xpoints,ypoints,5);
			
			
			if (lg_p->mono_item_type)
				item_type = lg_p->item_type;
			else
				item_type = types[nitem];

			if (lg_p->mono_dash_index)
				dash_index = lg_p->dash_index;
			else
				dash_index = dash_indexes[nitem];

			if (lg_p->mono_marker_index)
				marker_index = lg_p->marker_index;
			else
				marker_index = marker_indexes[nitem];

			if (lg_p->mono_line_color)
				line_color = lg_p->line_color;
			else
				line_color = line_colors[nitem];

			if (lg_p->mono_marker_color)
				marker_color = lg_p->marker_color;
			else
				marker_color = marker_colors[nitem];

			if (lg_p->mono_line_label_color)
				line_label_color = lg_p->line_label_color;
			else
				line_label_color = line_label_colors[nitem];

			if (lg_p->mono_line_dash_seglen)
				line_dash_seglen = lg_p->line_dash_seglen;
			else
				line_dash_seglen = line_dash_seglens[nitem];
			
			if (lg_p->mono_line_thickness)
				line_thickness = lg_p->line_thickness;
			else
				line_thickness = line_thicknesses[nitem];
			
			if (lg_p->mono_marker_thickness)
				marker_thickness = lg_p->marker_thickness;
			else
				marker_thickness = marker_thicknesses[nitem];
			
			if (lg_p->mono_line_label_font_height)
				line_label_font_height =
					lg_p->line_label_font_height;
			else
				line_label_font_height = font_heights[nitem];

			if (lg_p->mono_marker_size)
				marker_size =  lg_p->marker_size;
			else
				marker_size = marker_sizes[nitem];

			string = lg_p->line_labels_on ? line_labels[nitem] : NULL;
				
			xpoints[0] = xpoints[0] + 
					(xpoints[1] - xpoints[0]) / 2.0;

			if (item_type != NhlMARKERS){
				NhlVASetValues(lgl->base.wkptr->base.id,
				  _NhlNwkLineLabel,string,
				  _NhlNwkDashPattern,dash_index,
				  _NhlNwkLineDashSegLenF,line_dash_seglen,
				  _NhlNwkLineThicknessF,line_thickness,
				  _NhlNwkLineLabelFontHeightF,
						line_label_font_height,
				  _NhlNwkLineColor,line_color, 
				  _NhlNwkLineLabelFontColor,line_label_color,
				  _NhlNwkLineLabelFont,lg_p->ll_font,
				  _NhlNwkLineLabelFontAspectF,lg_p->ll_aspect,
				  _NhlNwkLineLabelFontThicknessF,
					       lg_p->ll_thickness,
				  _NhlNwkLineLabelFontQuality,lg_p->ll_quality,
				  _NhlNwkLineLabelConstantSpacingF,
					       lg_p->ll_const_spacing,
				  _NhlNwkLineLabelFuncCode,lg_p->ll_func_code,
					       NULL);
				_NhlSetLineInfo(lgl->base.wkptr,layer);
				_NhlWorkstationLineTo(lgl->base.wkptr, 
					      xpoints[0],ypoints[0], 1);
				_NhlWorkstationLineTo(lgl->base.wkptr, 
					      xpoints[0],ypoints[2], 0);
		        }
			if (item_type != NhlLINES){
				NhlVASetValues(lgl->base.wkptr->base.id,
					     _NhlNwkMarkerIndex,marker_index,
					     _NhlNwkMarkerThicknessF, 
							marker_thickness,
					     _NhlNwkMarkerSizeF,marker_size,
					     _NhlNwkMarkerColor, marker_color, 
					     NULL);
				_NhlSetMarkerInfo(lgl->base.wkptr,layer);
				_NhlWorkstationMarker(lgl->base.wkptr, 
						      xpoints,
						      &tcoord, 1);
			}
			
		}
	}
	else {
		xpoints[0] = lg_p->adj_bar.l;
		xpoints[1] = lg_p->adj_bar.r;
		xpoints[2] = lg_p->adj_bar.r;
		xpoints[3] = lg_p->adj_bar.l;
		xpoints[4] = lg_p->adj_bar.l;
		tcoord = xpoints[0] + (xpoints[1] - xpoints[0]) / 2.0;
		back_dist = lg_p->item_locs[0] - lg_p->adj_bar.b;
		for (i=0; i< lg_p->item_count; i++) {
			int nitem = order ? order[i] : i;

			for_dist = lg_p->item_locs[i+1] - lg_p->item_locs[i];
			for_dist = i < lg_p->item_count - 1 ? 
				for_dist/2.0 : for_dist;
			dist = MIN(back_dist,for_dist);
			back_dist = for_dist;
			ypoints[0] = lg_p->item_locs[i] - dist * frac;
			ypoints[1] = ypoints[0];
			ypoints[2] = lg_p->item_locs[i] + dist * frac;
			ypoints[3] = ypoints[2];
			ypoints[4] = ypoints[0];

			_NhlSetFillInfo(lgl->base.wkptr, layer);
			_NhlWorkstationFill(lgl->base.wkptr,
					    xpoints,ypoints,5);

			if (lg_p->mono_item_type)
				item_type = lg_p->item_type;
			else
				item_type = types[nitem];

			if (lg_p->mono_dash_index)
				dash_index = lg_p->dash_index;
			else
				dash_index = dash_indexes[nitem];

			if (lg_p->mono_marker_index)
				marker_index = lg_p->marker_index;
			else
				marker_index = marker_indexes[nitem];

			if (lg_p->mono_line_color)
				line_color = lg_p->line_color;
			else
				line_color = line_colors[nitem];

			if (lg_p->mono_marker_color)
				marker_color = lg_p->marker_color;
			else
				marker_color = marker_colors[nitem];

			if (lg_p->mono_line_label_color)
				line_label_color = lg_p->line_label_color;
			else
				line_label_color = line_label_colors[nitem];

			if (lg_p->mono_line_dash_seglen)
				line_dash_seglen = lg_p->line_dash_seglen;
			else
				line_dash_seglen = line_dash_seglens[nitem];
			
			if (lg_p->mono_line_thickness)
				line_thickness = lg_p->line_thickness;
			else
				line_thickness = line_thicknesses[nitem];
			
			if (lg_p->mono_marker_thickness)
				marker_thickness = lg_p->marker_thickness;
			else
				marker_thickness = marker_thicknesses[nitem];
			
			if (lg_p->mono_line_label_font_height)
				line_label_font_height =
					lg_p->line_label_font_height;
			else
				line_label_font_height = font_heights[nitem];

			if (lg_p->mono_marker_size)
				marker_size = lg_p->marker_size;
			else
				marker_size = marker_sizes[nitem];

			string = lg_p->line_labels_on ? line_labels[nitem] : NULL;

			ypoints[0] = ypoints[0] + 
					(ypoints[2] - ypoints[0]) / 2.0;
			if (item_type != NhlMARKERS){
				NhlVASetValues(lgl->base.wkptr->base.id,
				  _NhlNwkLineLabel, string,
				  _NhlNwkDashPattern,dash_index,
				  _NhlNwkLineThicknessF,line_thickness,
				  _NhlNwkLineDashSegLenF,line_dash_seglen,
				  _NhlNwkLineLabelFontHeightF,
					       line_label_font_height,
				  _NhlNwkLineColor, line_color, 
				  _NhlNwkLineLabelFontColor,line_label_color, 
				  _NhlNwkLineLabelFont,lg_p->ll_font,
				  _NhlNwkLineLabelFontAspectF,lg_p->ll_aspect,
				  _NhlNwkLineLabelFontThicknessF,
					       lg_p->ll_thickness,
				  _NhlNwkLineLabelFontQuality,lg_p->ll_quality,
				  _NhlNwkLineLabelConstantSpacingF,
					       lg_p->ll_const_spacing,
				  _NhlNwkLineLabelFuncCode,lg_p->ll_func_code,
					     NULL);
			   _NhlSetLineInfo(lgl->base.wkptr,layer);
			   _NhlWorkstationLineTo(lgl->base.wkptr, 
						 xpoints[0],
						 ypoints[0], 1);
			   _NhlWorkstationLineTo(lgl->base.wkptr, 
						 xpoints[1],
						 ypoints[0], 0);
		        }
			if (item_type != NhlLINES){
				NhlVASetValues(lgl->base.wkptr->base.id,
					     _NhlNwkMarkerIndex, marker_index,
					     _NhlNwkMarkerThicknessF, 
					        marker_thickness,
					     _NhlNwkMarkerSizeF, marker_size,
					     _NhlNwkMarkerColor, marker_color, 
					     NULL);
				_NhlSetMarkerInfo(lgl->base.wkptr,layer);
				_NhlWorkstationMarker(lgl->base.wkptr, 
						      &tcoord,
						      ypoints, 1);
			}
			
		}
	}
	_NhlWorkstationLineTo(lgl->base.wkptr,0.0,0.0,1); 
	c_plotif(0.,0.,2);	

	if (lgl->view.use_segments) {
		if (lg_p->title_on && lg_p->title_ext > 0.0)
			_NhlSegDraw(_NhlGetLayer(lg_p->title_id));
		
		if (lg_p->labels_on )
			_NhlSegDraw(_NhlGetLayer(lg_p->labels_id));

		_NhlEndSegment(lg_p->trans_dat);
		_NhlDeactivateWorkstation(lgl->base.wkptr);
	}
	else {
		_NhlDeactivateWorkstation(lgl->base.wkptr);

		if (lg_p->title_on && lg_p->title_ext > 0.0)
			NhlDraw(lg_p->title_id);
		
		if (lg_p->labels_on )
			NhlDraw(lg_p->labels_id);
	}

	return(ret);
}

/*
 * Function:	LegendClassInitialize
 *
 * Description: 
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    LegendClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{
        _NhlEnumVals   labelalignmentlist[] = {
        {NhlITEMCENTERS,	"ItemCenters"},
        {NhlABOVEITEMS,	"AboveItems"},
        {NhlBELOWITEMS,	"BelowItems"},
        };

        _NhlEnumVals   itemplacementlist[] = {
        {NhlUNIFORMPLACEMENT,		"UniformPlacement"},
        {NhlEXPLICITPLACEMENT,	"ExplicitPlacement"},
        };

	_NhlRegisterEnumType(NhllegendClass,NhlTlgLabelAlignmentMode,
			labelalignmentlist,NhlNumber(labelalignmentlist));
	_NhlRegisterEnumType(NhllegendClass,NhlTlgItemPlacementMode,
			itemplacementlist,NhlNumber(itemplacementlist));

	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qitem_types = NrmStringToQuark(NhlNlgItemTypes);
	Qdash_indexes = NrmStringToQuark(NhlNlgDashIndexes);
	Qmarker_indexes = NrmStringToQuark(NhlNlgMarkerIndexes);
	Qline_labels = NrmStringToQuark(NhlNlgLineLabelStrings);
	Qline_colors = NrmStringToQuark(NhlNlgLineColors);
	Qmarker_colors = NrmStringToQuark(NhlNlgMarkerColors);
	Qline_dash_seglens = NrmStringToQuark(NhlNlgLineDashSegLens);
	Qline_thicknesses = NrmStringToQuark(NhlNlgLineThicknesses);
	Qmarker_thicknesses = NrmStringToQuark(NhlNlgMarkerThicknesses);
	Qline_label_font_heights = 
		NrmStringToQuark(NhlNlgLineLabelFontHeights);
	Qmarker_sizes = NrmStringToQuark(NhlNlgMarkerSizes);
	Qlabel_strings = NrmStringToQuark(NhlNlgLabelStrings);
	Qllabel_colors = NrmStringToQuark(NhlNlgLineLabelFontColors);
	Qitem_positions = NrmStringToQuark(NhlNlgItemPositions);
	Qtitle_string = NrmStringToQuark(NhlNlgTitleString);

	_NhlInitializeClass(NhltextItemClass);
	_NhlInitializeClass(NhlmultiTextClass);
	return(NhlNOERROR);	
}


/*
 * Function:	LegendDestroy
 *
 * Description: Frees all dynamically allocated memory
 *
 * In Args:	NhlLayer inst	instance of Legend
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes    LegendDestroy
#if	NhlNeedProto
(NhlLayer  inst)
#else
(inst)
	NhlLayer	inst;
#endif
{
	NhlLegendLayer tinst = (NhlLegendLayer) inst;
	NhlLegendLayerPart *lg_p = &(tinst->legend);

	if (lg_p->stride_labels != NULL)
		NhlFree(lg_p->stride_labels);
	if (lg_p->label_locs != NULL)
		NhlFree(lg_p->label_locs);
	if (lg_p->item_locs != NULL)
		NhlFree(lg_p->item_locs);

	NhlFreeGenArray(lg_p->line_labels);
	NhlFreeGenArray(lg_p->label_strings);
	NhlFreeGenArray(lg_p->item_types);
	NhlFreeGenArray(lg_p->dash_indexes);
	NhlFreeGenArray(lg_p->marker_indexes);
	NhlFreeGenArray(lg_p->line_colors);
	NhlFreeGenArray(lg_p->line_label_colors);
	NhlFreeGenArray(lg_p->marker_colors);
	NhlFreeGenArray(lg_p->line_dash_seglens);
	NhlFreeGenArray(lg_p->line_thicknesses);
	NhlFreeGenArray(lg_p->marker_thicknesses);
	NhlFreeGenArray(lg_p->line_label_font_heights);
	NhlFreeGenArray(lg_p->marker_sizes);
	NhlFreeGenArray(lg_p->item_positions);
	NhlFreeGenArray(lg_p->item_order);

	if (lg_p->labels_id >=0)
		NhlDestroy(lg_p->labels_id);

	if (lg_p->title_string != lgDefTitle) {
		NhlFree(lg_p->title_string);
	}
	if (lg_p->title_id >=0)
		NhlDestroy(lg_p->title_id);

	if (lg_p->trans_dat != NULL)
		_NhlDeleteViewSegment(inst,lg_p->trans_dat);

	return(NhlNOERROR);
}

