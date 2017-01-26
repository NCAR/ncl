/*
 *      $Id: LabelBar.c,v 1.75 2010-05-06 22:35:35 haley Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LabelBar.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	
 *		
 *		Creates and manages a LabelBar
 */

#include <math.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/LabelBarP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/color.h>

static char lbDefTitle[] = "NOTHING";

/* default pattern list */

static int def_patterns[] = { 
	1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

static char Init_Name[] = "LabelBarInitialize";
static char SetValues_Name[] = "LabelBarSetValues";

/* SUPPRESS 112 */

#define DEGTORAD 0.017453293

static NhlResource resources[] = { 

/* Begin-documented-resources */

	{NhlNlbLabelBarOn, NhlClbLabelBarOn, NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.labelbar_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL}, 
	{NhlNlbOrientation, NhlClbOrientation, NhlTOrientation,sizeof(NhlOrientation),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.orient),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlVERTICAL),0,NULL},
	{NhlNlbJustification, NhlClbJustification, NhlTJustification, 
	 sizeof(NhlJustification),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.just),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlBOTTOMLEFT),0,NULL},
	{NhlNlbBoxMajorExtentF, NhlClbBoxMajorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.box_major_ext),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNlbBoxMinorExtentF, NhlClbBoxMinorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.box_minor_ext),
	 NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNlbBoxCount, NhlClbBoxCount, NhlTInteger,
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.box_count),
	 NhlTImmediate,_NhlUSET((NhlPointer) 16),0,NULL},
	{NhlNlbBoxSizing, NhlClbBoxSizing, NhlTlbBoxSizingMode,
	 sizeof(NhllbBoxSizingMode),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_sizing),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlUNIFORMSIZING),0,NULL},

	{NhlNlbAutoManage, NhlClbAutoManage, NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.auto_manage),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNlbLabelOffsetF, NhlClbLabelOffsetF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.label_off),
	 NhlTString,_NhlUSET("0.1"),0,NULL},
	{NhlNlbTitleOffsetF, NhlClbTitleOffsetF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_off),
	 NhlTString,_NhlUSET("0.03"),0,NULL},
	{NhlNlbLeftMarginF, NhlClbLeftMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.margin.l),
	 NhlTString,_NhlUSET("0.05"),0,NULL},
	{NhlNlbRightMarginF, NhlClbRightMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.margin.r),
	 NhlTString,_NhlUSET("0.05"),0,NULL},
	{NhlNlbBottomMarginF, NhlClbBottomMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.margin.b),
	 NhlTString,_NhlUSET("0.05"),0,NULL},
	{NhlNlbTopMarginF, NhlClbTopMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.margin.t),
	 NhlTString,_NhlUSET("0.05"),0,NULL},

	{NhlNlbMonoFillColor, NhlClbMonoFillColor, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.mono_fill_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNlbFillColor,NhlCFillColor,NhlTColorIndex,sizeof(NhlColorIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_color),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNlbFillColors, NhlClbFillColors, NhlTColorIndexGenArray,
	 sizeof(NhlPointer),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_colors),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNlbMonoFillPattern, NhlClbMonoFillPattern, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.mono_fill_pattern),
	 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNlbFillPattern,NhlCFillPattern,NhlTFillIndex,sizeof(NhlFillIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_pattern),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlSOLIDFILL),0,NULL},
	{NhlNlbFillPatterns, NhlClbFillPatterns, NhlTFillIndexGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_patterns),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNlbMonoFillScale, NhlClbMonoFillScale, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.mono_fill_scale),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNlbFillScaleF,NhlCFillScaleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_scale),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNlbFillScales, NhlClbFillScales, NhlTFloatGenArray,
	 sizeof(NhlPointer),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_scales),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNlbFillDotSizeF,NhlCFillDotSizeF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_dot_size),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
        {NhlNlbFillOpacityF, NhlClbFillOpacityF, NhlTFloat, sizeof(float),
         NhlOffset(NhlLabelBarLayerRec, labelbar.fill_opacity),
         NhlTString, _NhlUSET("1.0"), 0, NULL},
	{NhlNlbOverrideFillOpacity, NhlClbOverrideFillOpacity, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.override_fill_opacity),
	 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNlbLabelStrings, NhlClbLabelStrings, NhlTStringGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_strings),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNlbBoxFractions, NhlClbBoxFractions,
	 NhlTFloatGenArray,sizeof(NhlPointer), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_fractions),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	
	{ NhlNlbLabelAutoStride, NhlCLabelAutoStride, 
	  NhlTBoolean, sizeof(NhlBoolean),
	  NhlOffset(NhlLabelBarLayerRec,labelbar.label_auto_stride),
	  NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNlbLabelsOn, NhlClbLabelsOn, NhlTBoolean, 
	 sizeof(NhlBoolean), NhlOffset(NhlLabelBarLayerRec,labelbar.labels_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNlbLabelPosition, NhlClbLabelPosition, NhlTPosition, 
	 sizeof(NhlPosition), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_pos),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlRIGHT),0,NULL},
	{NhlNlbLabelAngleF, NhlCTextAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.label_angle),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNlbLabelAlignment, NhlClbLabelAlignment,NhlTlbLabelAlignmentMode, 
	 sizeof(NhllbLabelAlignmentMode), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_alignment),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlBOXCENTERS),0,NULL},
	{NhlNlbLabelDirection,NhlCTextDirection,NhlTTextDirection,
	 sizeof(NhlTextDirection),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_direction),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNlbLabelJust, NhlCTextJustification,NhlTJustification, 
	 sizeof(NhlJustification),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_just),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNlbLabelFont, NhlCFont, NhlTFont, 
	 sizeof(NhlFont), NhlOffset(NhlLabelBarLayerRec,labelbar.label_font),
	 NhlTImmediate,_NhlUSET((NhlPointer) 21),0,NULL},
	{NhlNlbLabelFontColor, NhlCFontColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlLB_DEF_COLOR),0,NULL},
	{NhlNlbLabelFontHeightF, NhlCFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.label_height),
	 NhlTString,_NhlUSET("0.02"),0,NULL},
	{NhlNlbLabelFontAspectF, NhlCFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.label_aspect),
	 NhlTString,_NhlUSET("1.3125"),0,NULL},
	{NhlNlbLabelFontThicknessF, NhlCFontThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNlbLabelFontQuality, NhlCFontQuality, NhlTFontQuality, 
	 sizeof(NhlFontQuality), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_quality),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNlbLabelConstantSpacingF, NhlCTextConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_const_spacing),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNlbLabelFuncCode, NhlCTextFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(NhlLabelBarLayerRec,labelbar.label_func_code),
	 NhlTString,_NhlUSET("~"),0,NULL},
	{NhlNlbLabelStride, NhlClbLabelStride, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.label_stride),
	 NhlTImmediate,_NhlUSET((NhlPointer) 1),0,NULL},
	{NhlNlbMaxLabelLenF, NhlClbMaxLabelLenF, NhlTFloat, sizeof(float),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.max_label_len),
	 NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},
	{NhlNlbMinLabelSpacingF, NhlClbMinLabelSpacingF, NhlTFloat, sizeof(float),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.min_label_spacing),
	 NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},

	{NhlNlbTitleString, NhlClbTitleString, NhlTString, 
	 sizeof(char *), NhlOffset(NhlLabelBarLayerRec,labelbar.title_string),
	 NhlTImmediate,_NhlUSET(lbDefTitle),0,(NhlFreeFunc)NhlFree},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_on_set),
	 NhlTImmediate,_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNlbTitleOn, NhlClbTitleOn, NhlTBoolean, 
	 sizeof(NhlBoolean), NhlOffset(NhlLabelBarLayerRec,labelbar.title_on),
	 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNlbTitlePosition, NhlClbTitlePosition, NhlTPosition, 
	 sizeof(NhlPosition), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_pos),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlTOP),0,NULL},
	{NhlNlbTitleExtentF, NhlClbTitleExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_ext),
	 NhlTString,_NhlUSET("0.15"),0,NULL},
	{NhlNlbTitleAngleF, NhlCTextAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_angle),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_direction_set),
	 NhlTImmediate,_NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNlbTitleDirection,NhlCTextDirection,NhlTTextDirection,
	 sizeof(NhlTextDirection),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_direction),
	 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNlbTitleFont, NhlCFont, NhlTFont, 
	 sizeof(NhlFont), NhlOffset(NhlLabelBarLayerRec,labelbar.title_font),
	 NhlTImmediate,_NhlUSET((NhlPointer) 21),0,NULL},
	{NhlNlbTitleFontColor, NhlCFontColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlLB_DEF_COLOR),0,NULL},
	{NhlNlbTitleJust, NhlCTextJustification, NhlTJustification, 
	 sizeof(NhlJustification),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_just),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNlbTitleFontHeightF, NhlCFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_height),
	 NhlTString,_NhlUSET("0.025"),0,NULL},
	{NhlNlbTitleFontAspectF, NhlCFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_aspect),
	 NhlTString,_NhlUSET("1.3125"),0,NULL},
	{NhlNlbTitleFontThicknessF, NhlCFontThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNlbTitleFontQuality, NhlCFontQuality, NhlTFontQuality, 
	 sizeof(NhlFontQuality), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_quality),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNlbTitleConstantSpacingF, NhlCTextConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_const_spacing),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNlbTitleFuncCode, NhlCTextFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(NhlLabelBarLayerRec,labelbar.title_func_code),
	 NhlTString,_NhlUSET("~"),0,NULL},
	
	{NhlNlbBoxLinesOn,NhlClbBoxLinesOn,NhlTBoolean, 
	 sizeof(NhlBoolean),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_lines_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNlbBoxSeparatorLinesOn,NhlClbBoxSeparatorLinesOn,NhlTBoolean, 
	 sizeof(NhlBoolean),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_separator_lines_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNlbBoxLineColor, NhlCLineColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlLB_DEF_COLOR),0,NULL},
	{NhlNlbBoxLineThicknessF, NhlCLineThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNlbBoxLineDashPattern, NhlCLineDashPattern, NhlTDashIndex, 
	 sizeof(NhlDashIndex), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_dash_pattern),
	 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNlbBoxLineDashSegLenF, NhlCLineDashSegLenF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_dash_seglen),
	 NhlTString,_NhlUSET("0.15"),0,NULL},
	{NhlNlbBoxEndCapStyle, NhlClbBoxEndCapStyle, NhlTlbBoxEndCapStyle, 
	 sizeof(NhllbBoxEndCapStyle), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_end_cap_style),
         NhlTImmediate,_NhlUSET((NhlPointer)NhlRECTANGLEENDS),0,NULL},                

	{NhlNlbPerimOn, NhlCEdgesOn, NhlTBoolean,
	 sizeof(NhlBoolean), NhlOffset(NhlLabelBarLayerRec,labelbar.perim_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNlbPerimColor, NhlCEdgeColor, NhlTColorIndex, 
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNlbPerimFill, NhlCFillPattern, NhlTFillIndex,
	 sizeof(NhlFillIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_fill),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlHOLLOWFILL),0,NULL},
	{NhlNlbPerimFillColor, NhlCFillColor, NhlTColorIndex,
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_fill_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNlbPerimThicknessF, NhlCEdgeThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNlbPerimDashPattern, NhlCEdgeDashPattern, NhlTDashIndex,sizeof(int), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_dash_pattern),
	 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNlbPerimDashSegLenF, NhlCEdgeDashSegLenF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_dash_seglen),
	 NhlTString,_NhlUSET("0.15"),0,NULL},

	{NhlNlbFillBackground, NhlCFillBackgroundColor, NhlTColorIndex,
	 sizeof(NhlColorIndex),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_background),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlTRANSPARENT),0,NULL},
	{NhlNlbFillLineThicknessF, NhlCFillLineThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_line_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNlbRasterFillOn, NhlClbRasterFillOn, NhlTBoolean,
	 sizeof(NhlBoolean), NhlOffset(NhlLabelBarLayerRec,labelbar.raster_fill_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},

/* End-documented-resources */

	{NhlNlbMarginMode, NhlClbMarginMode, NhlTInteger,
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.margin_mode),
	 NhlTImmediate,_NhlUSET((NhlPointer) 0),_NhlRES_PRIVATE,NULL},

};

/*
* Base Methods used
*/


static NhlErrorTypes    LabelBarInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes LabelBarSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes 	LabelBarGetValues(
#if	NhlNeedProto
	NhlLayer,		/* l */
	_NhlArgList, 	/* args */
	int		/* num_args */
#endif
);

static NhlErrorTypes	LabelBarDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes	LabelBarDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes 	LabelBarClassInitialize();

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

static NhlErrorTypes    SetLabelBarGeometry(
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

static NhlErrorTypes    ManageBoxFractionsArray(
#if	NhlNeedProto
	float	*box_fractions,
	int	count,
	char	*entry_name
#endif
);

static void CreateIntermediates(
#if	NhlNeedProto
	float*,		/* *flist */
	int,		/* start  */
	int		/* end    */
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
	NhlLabelBarLayerPart *lb_p,
	NhlLabelBarLayerPart *olb_p,
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


NhlLabelBarClassRec NhllabelBarClassRec = {
	{
/* class_name			*/	"labelBarClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLabelBarLayerRec),
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
/* class_initialize		*/	LabelBarClassInitialize,
/* layer_initialize		*/	LabelBarInitialize,
/* layer_set_values		*/	LabelBarSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	LabelBarGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	LabelBarDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	LabelBarDraw,

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

NhlClass NhllabelBarClass = (NhlClass)&NhllabelBarClassRec;

/*
 * Function:	nhlflabelbarclass
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
_NHLCALLF(nhlflabelbarclass,NHLFLABELBARCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhllabelBarClass;
}

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark	Qfill_patterns = NrmNULLQUARK;
static NrmQuark	Qfill_colors = NrmNULLQUARK;
static NrmQuark	Qfill_scales = NrmNULLQUARK;
static NrmQuark	Qlabel_strings = NrmNULLQUARK;
static NrmQuark	Qbox_fractions = NrmNULLQUARK;
static NrmQuark	Qtitle_string = NrmNULLQUARK;

/*
 * Function:	LabelBarInitialize
 *
 * Description:	Performs initialization of LabelBar. 
 *              1) Initialize some internal parameters, and coerce
 *                 several others into the proper boundaries.
 *              2) Copy the view settings.
 *              2) Create a default title string.
 *              3) InitializeDynamicArrays
 *		4) Set up the LabelBar Geometry
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
static NhlErrorTypes    LabelBarInitialize
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
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes	ret=NhlNOERROR,ret1 = NhlNOERROR;

	lb_p->new_draw_req = True;
	lb_p->labels_id = -1;
	lb_p->title_id = -1;
	lb_p->stride_labels = NULL;
	lb_p->box_locs = NULL;
	lb_p->label_locs = NULL;
	lb_p->title_x = 0.0;
	lb_p->title_y = 1.0;
	lb_p->trans_dat = NULL;
	lb_p->label_draw_count = 0;
	lb_p->label_locs = NULL;

/*
 * Ensure that the label and title angles range is between -360 and 360
 */
	lb_p->label_angle = fmod(lb_p->label_angle,360.0);
	lb_p->title_angle = fmod(lb_p->title_angle,360.0);

	if (lb_p->box_count < 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Minimum box count is 1",Init_Name);
		ret = NhlWARNING;
		lb_p->box_count = 1;
	}

/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (!lb_p->title_direction_set) {
		switch (lb_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lb_p->title_direction = NhlACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lb_p->title_direction = NhlDOWN;
			break;
		}
	}
	/*
	 * If the title on flag is not explicitly set, set it based on
	 * whether a string has been supplied. Note that this must
	 * occur before the title is set from SetLabelBarGeometry.
	 */
	if (! lb_p->title_on_set) {
		lb_p->title_on = lb_p->title_string == lbDefTitle ?
			False : True;
	}

	lb_p->lb_x = tnew->view.x;
	lb_p->lb_y = tnew->view.y;
	lb_p->lb_width = tnew->view.width;
	lb_p->lb_height = tnew->view.height;


	lb_p->perim.l = lb_p->perim.lxtr = lb_p->lb_x;
	lb_p->perim.r = lb_p->perim.rxtr = lb_p->lb_x + lb_p->lb_width;
	lb_p->perim.b = lb_p->perim.bxtr = lb_p->lb_y - lb_p->lb_height;
	lb_p->perim.t = lb_p->perim.txtr = lb_p->lb_y;

/*
 * Set up array resources
 */

	ret1 = InitializeDynamicArrays(new,req,args,num_args);
	ret = MIN(ret,ret1);

/*
 * Calculate labelbar geometry
 */

	ret1 = SetLabelBarGeometry(new,req,True,args,num_args);
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
 * Function:	LabelBarSetValues
 *
 * Description: Handles setting all the LabelBar object resources. Calls a
 *	number of internal subroutines to deal with each phase of the 
 *	operation of setting up the LabelBar object. Calls many of the same
 *	routines used by the LabelBar initialize call.
 *	Note: the LabelBar object has not been performance tuned. It is
 *	possible that a performance saving could result from eliminating
 *	some calls under certain conditions. 
 *
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
static NhlErrorTypes LabelBarSetValues
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
	NhlLabelBarLayer	told = (NhlLabelBarLayer) old;
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart	*lb_p = &(tnew->labelbar);
	NhlErrorTypes		ret = NhlNOERROR,ret1 = NhlNOERROR;
	char			*entry_name = SetValues_Name;
	int			view_args = 0;
	NhlBoolean	        do_scaling = False;
	float			tx = 1.0, ty = 1.0;
	float			lxtr,rxtr,bxtr,txtr;

	if (tnew->view.use_segments != told->view.use_segments) {
		lb_p->new_draw_req = True;
	}
	if (_NhlArgIsSet(args,num_args,NhlNvpXF)) {
		view_args++;
	}
	if (_NhlArgIsSet(args,num_args,NhlNvpYF)) {
		view_args++;
	}
	if (_NhlArgIsSet(args,num_args,NhlNvpWidthF)) {
		view_args++;
		do_scaling = True;
	}
	if (_NhlArgIsSet(args,num_args,NhlNvpHeightF)) {
		view_args++;
		do_scaling = True;
	}
	if (_NhlArgIsSet(args,num_args,NhlNvpOn)) {
		view_args++;
	}

	if (num_args > view_args ||
            ! _NhlSegmentSpansArea(lb_p->trans_dat,
                                   tnew->view.x,
                                   tnew->view.x + tnew->view.width,
                                   tnew->view.y - tnew->view.height,
                                   tnew->view.y))
		lb_p->new_draw_req = True;

	if (lb_p->box_count < 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Minimum box count is 1", entry_name);
		ret = NhlWARNING;
		lb_p->box_count = 1;
	}
		
	/*
 * Ensure that the label and title angles range is between -360 and 360
 */
	lb_p->label_angle = fmod(lb_p->label_angle,360.0);
	lb_p->title_angle = fmod(lb_p->title_angle,360.0);

/*
 * if the title string is set, turn on the title unless it is simultaneously
 * explicitly turned off.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlbTitleString) &&
	    !_NhlArgIsSet(args,num_args,NhlNlbTitleOn))
		lb_p->title_on = True;
/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlbTitlePosition) &&
	    !_NhlArgIsSet(args,num_args,NhlNlbTitleDirection)) {
		switch (lb_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lb_p->title_direction = NhlACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lb_p->title_direction = NhlDOWN;
			break;
		}
	}

	lb_p->lb_x = tnew->view.x;
	lb_p->lb_y = tnew->view.y;
	lb_p->lb_width = tnew->view.width;
	lb_p->lb_height = tnew->view.height;

	if (do_scaling) {
		int i;

		tx = tnew->view.width / told->view.width;
		ty = tnew->view.height / told->view.height;

		if (! _NhlArgIsSet(args,num_args,NhlNlbLabelFontHeightF)) {
			if (lb_p->label_direction == NhlACROSS)
				lb_p->label_height *= tx;
			else 
				lb_p->label_height *= ty;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNlbTitleFontHeightF)) {
			if (lb_p->title_direction == NhlACROSS)
				lb_p->title_height *= tx;
			else
				lb_p->title_height *= ty;
		}
		if (lb_p->orient == NhlHORIZONTAL) {
			lb_p->max_label_len *= tx;
			lb_p->min_label_spacing = 
				tx * (lb_p->labels.rxtr - lb_p->labels.lxtr);
		}
		else {
			lb_p->max_label_len *= ty;
			lb_p->min_label_spacing = 
				tx * (lb_p->labels.txtr - lb_p->labels.bxtr);
		}

		for (i=1; i<lb_p->label_draw_count; i++) {
			lb_p->min_label_spacing = 
				MIN(lb_p->min_label_spacing,
				    tx * (lb_p->label_locs[i] - 
					  lb_p->label_locs[i-1]));
		}
	}
	
	lxtr = tx * (lb_p->perim.l - lb_p->perim.lxtr);
	rxtr = tx * (lb_p->perim.rxtr - lb_p->perim.r);
	bxtr = ty * (lb_p->perim.b - lb_p->perim.bxtr);
	txtr = tx * (lb_p->perim.txtr - lb_p->perim.t);
	lb_p->perim.l = lb_p->lb_x + lxtr;
	lb_p->perim.r = lb_p->lb_x + lb_p->lb_width - rxtr;
	lb_p->perim.b = lb_p->lb_y - lb_p->lb_height + bxtr;
	lb_p->perim.t = lb_p->lb_y - txtr;
	lb_p->perim.lxtr = lb_p->lb_x;
	lb_p->perim.rxtr = lb_p->perim.r + rxtr;
	lb_p->perim.bxtr = lb_p->perim.b - bxtr;
	lb_p->perim.txtr = lb_p->lb_y;

/*
 * Return now if using segments and only the view has changed
 */
	if (tnew->view.use_segments && ! lb_p->new_draw_req) {
		return ret;
	}
/*
 * or even if a new draw is required but the legend is turned off
 * and only the view has changed.
 */
	if (view_args == num_args &&
	    (! (tnew->view.on && lb_p->labelbar_on))) {
		return ret;
	}

	ret1 = ManageDynamicArrays(new,old,args,num_args);
	ret = MIN(ret,ret1);


/*
 * Calculate labelbar geometry
 */

	ret1 = SetLabelBarGeometry(new,old,False,args,num_args);
	ret = MIN(ret1,ret);
/*
 * Set the box locations
 */
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
 * Description: Creates internal copies of each of the LabelBar GenArrays and
 *	populates the copies with the values specified via a LabelBarCreate
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
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret_1 = NhlNOERROR;
	int i;
	ng_size_t  count;
	int len;
	char number[10];
	NhlGenArray ga;
	char *entry_name = Init_Name;
	char *e_text;
	int *i_p;
	float * f_p;
	NhlString *s_p;

/*=======================================================================*/
/* 
 * Initialize the color array starting with index 1, the foreground color. 
 * Then if the user has supplied a generic array copy it over the 
 * created array. Then check each element to ensure that
 * it is a valid color index.
 */

	count = MAX(lb_p->box_count, NhlLB_DEF_BOX_COUNT);
	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillColors);
		return NhlFATAL;
	}
	for (i=0; i<count; i++)
		i_p[i] = i + 1;
			
	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillColors);
		return NhlFATAL;
	}
	ga->my_data = True;

	if (lb_p->fill_colors != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->fill_colors,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->fill_colors = ga;

	i_p = (int *) lb_p->fill_colors->data;

/*=======================================================================*/
/* The fill patterns array
 */

	count = MAX(lb_p->box_count, NhlLB_DEF_BOX_COUNT);
	NhlVAGetValues(tnew->base.wkptr->base.id,
		     NhlNwkFillTableLength, &len, NULL);

	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillPatterns);
		return NhlFATAL;
	}
	for (i=0; i < NhlLB_DEF_BOX_COUNT; i++) 
		i_p[i] = def_patterns[i];
	for (i=NhlLB_DEF_BOX_COUNT; i<count; i++)
		i_p[i] = i + 1;

	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillPatterns);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If a fill_patterns resource array has been passed in, copy it to the ga */

	if (lb_p->fill_patterns != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->fill_patterns,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillPatterns, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->fill_patterns = ga;

/*
 * Replace any invalid elements in the array with the default value
 */
	i_p = (int *) lb_p->fill_patterns->data;
	for (i=0; i<count; i++) {
		if (i_p[i] < NhlHOLLOWFILL) {
			e_text =
	       "%s: %s index %d holds an invalid fill value, %d: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillPatterns, i, i_p[i]);
		        ret = MIN(ret, NhlWARNING);
			i_p[i] = NhlLB_DEF_PATTERN;
		}
	}

/*=======================================================================*/
/* The fill_scales array */

	count = MAX(lb_p->box_count, NhlLB_DEF_BOX_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillScales);
		return NhlFATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 1.0;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillScales);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If a fill scales resource array has been passed in, copy it to the ga */

	if (lb_p->fill_scales != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->fill_scales,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillScales, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->fill_scales = ga;
/*
 * Replace any invalid elements in the array with the default value
 */
	f_p = (float *) lb_p->fill_scales->data;
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
	       "%s: %s index %d holds an invalid item type: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillScales, i);
		        ret = MIN(ret, NhlWARNING);
			f_p[i] = 1.0;
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

	lb_p->max_label_stride_count = 0;
	lb_p->max_label_draw_count = 0;
	if (lb_p->label_alignment == NhlBOXCENTERS)
		lb_p->current_label_count = lb_p->box_count;
	else if (lb_p->label_alignment == NhlINTERIOREDGES)
		lb_p->current_label_count = lb_p->box_count - 1;
	else
		lb_p->current_label_count = lb_p->box_count + 1;

	count = MAX(lb_p->current_label_count, NhlLB_DEF_BOX_COUNT);
	if ((s_p = (NhlString *) 
	     NhlMalloc(count * sizeof(NhlString))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbLabelStrings);
		return NhlFATAL;
	}
	for (i=0;i<count;i++) {
		sprintf(number,"%d",i);
		if ((s_p[i] = (char *)
		     NhlMalloc(strlen(NhlLB_DEF_STRING) + 
			       strlen(number) + 1)) == NULL) {
			e_text = "%s: error creating %s string";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbLabelStrings);
			return NhlFATAL;
		}
		strcpy(s_p[i], NhlLB_DEF_STRING);
		strcat(s_p[i], number);
	}

	if ((ga = NhlCreateGenArray((NhlPointer)s_p,NhlTString,
				    sizeof(NhlString),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbLabelStrings);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If a label strings resource array has been passed in, copy it to the ga */

	if (lb_p->label_strings != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->label_strings,
						  NhlLB_MAX_LBL_STRINGS,
						  True,False,
						  NhlNlbLabelStrings, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->label_strings = ga;

/*
 * Copy function should have replaced any NULL strings - nothing else is
 * invalid.
 */


/*=======================================================================*/
/* 
 * Set up the box_fraction array. If no array
 * is provided create a uniform array, allowing future modification of 
 * the sizing. Set it up whether or not it is going to be used.
 */
	
	count = MAX(lb_p->box_count, NhlLB_DEF_BOX_COUNT) + 1;
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbBoxFractions);
		return NhlFATAL;
	}
	
	f_p[0] = 0.0;
	for (i=1;i<lb_p->box_count; i++)
		f_p[i] = -1.0;
	f_p[lb_p->box_count] = 1.0;
	
	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbBoxFractions);
		return NhlFATAL;
	}
	ga->my_data = True;
		
/* If a box fractions array has been passed in, copy it to the ga */

	if (lb_p->box_fractions != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->box_fractions,
						  NhlLB_MAX_BOXES+1,
						  True,False,
						  NhlNlbBoxFractions, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->box_fractions = ga;
		
/*
 * Allocate the location array: use one more than
 * the current box count so that both ends of the labelbar can be stored
 */

	lb_p->box_locs = (float *) 
		NhlMalloc((lb_p->box_count+1) * sizeof(float));
	
	return (ret);
		
}

/*
 * Function:    ManageDynamicArrays
 *
 * Description:	Handles changes to any of the LabelBar object GenArrays after
 *	their initial creation. 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via LabelBarSetValues
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
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayer	told = (NhlLabelBarLayer) old;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlLabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret_1 = NhlNOERROR;
	int i;
	int count, len;
	char number[10];
	char *entry_name = "LabelBarSetValues";
	char *e_text;
	int  *i_p;
	float *f_p;
	NhlString *s_p;

/*=======================================================================*/
/*
 * Manage the colors array: if the array has changed copy the new
 * array elements, check them for validity.
 * Then if the box count is greater than the current array size, enlarge
 * the array and give initial values to the new elements.
 */

	count = lb_p->box_count;

	if (lb_p->fill_colors != olb_p->fill_colors) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->fill_colors),
						  lb_p->fill_colors,
						  NhlLB_MAX_BOXES,True,False,
						  NhlNlbFillColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lb_p->fill_colors = olb_p->fill_colors;
		olb_p->fill_colors = NULL;
		i_p = (int *) lb_p->fill_colors->data;

	}

	if (lb_p->fill_colors->num_elements < count) {
		i_p = (int *) lb_p->fill_colors->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillColors);
			return NhlFATAL;
		}
		for (i=lb_p->fill_colors->num_elements; i<count; i++) {
			i_p[i] = i + 1;
		}

		lb_p->fill_colors->data = (NhlPointer) i_p;
		lb_p->fill_colors->num_elements = count;
	}
/*=======================================================================*/

/*
 * Manage the fill pattern array
 */

	count = lb_p->box_count;
	NhlVAGetValues(tnew->base.wkptr->base.id,
		     NhlNwkFillTableLength, &len, NULL);

	if (lb_p->fill_patterns != olb_p->fill_patterns) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->fill_patterns),
						  lb_p->fill_patterns,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillPatterns, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lb_p->fill_patterns = olb_p->fill_patterns;
		olb_p->fill_patterns = NULL;
		i_p = (int *) lb_p->fill_patterns->data;
		for (i=0; i<MIN(count,
				lb_p->fill_patterns->num_elements); i++) {
			if (i_p[i] < NhlHOLLOWFILL) {
				e_text =
	      "%s: %s index %d holds an invalid pattern value, %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNlbFillPatterns, i, i_p[i]);
				ret = MIN(ret, NhlWARNING);
				i_p[i] = NhlLB_DEF_PATTERN;
			}

		}
	}

	if (lb_p->fill_patterns->num_elements < count) {
		i_p = (int *) lb_p->fill_patterns->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillPatterns);
			return NhlFATAL;
		}
		for (i=lb_p->fill_patterns->num_elements; i<count; i++) {
			i_p[i] = i + 1;
		}

		lb_p->fill_patterns->data = (NhlPointer) i_p;
		lb_p->fill_patterns->num_elements = count;
	}

/*=======================================================================*/
/* 
 * Handle the fill scales
 */

	count = lb_p->box_count;

	if (lb_p->fill_scales != olb_p->fill_scales) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->fill_scales),
						  lb_p->fill_scales,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillScales, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lb_p->fill_scales = olb_p->fill_scales;
		olb_p->fill_scales = NULL;
		f_p = (float *) lb_p->fill_scales->data;
		for (i=0; i<MIN(count,lb_p->fill_scales->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
	      "%s: %s index %d holds an invalid fill scale value: defaulting";
				NhlPError(NhlWARNING,
					  NhlEUNKNOWN,e_text,entry_name,
					  NhlNlbFillScales, i);
				ret = MIN(ret, NhlWARNING);
				f_p[i] = 1.0;
			}
		}
	}

	if (lb_p->fill_scales->num_elements < count) {
		f_p = (float *) lb_p->fill_scales->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillScales);
			return NhlFATAL;
		}
		for (i=lb_p->fill_scales->num_elements; i<count; i++) {
			f_p[i] = 1.0;
		}

		lb_p->fill_scales->data = (NhlPointer) f_p;
		lb_p->fill_scales->num_elements = count;
	}

/*======================================================================*/

/* 
 * Handle the label strings. Copy the new array into the old.
 * NULL strings generate an error message and are replaced by empty 
 * (single byte null terminator) strings within the Validated copy function. 
 * If the box count has increased, but the string gen array does not 
 * contain enough elements, the array is resized and default strings are
 * created for the additional elements.
 */

	if (lb_p->label_alignment == NhlBOXCENTERS)
		count = lb_p->box_count;
	else if (lb_p->label_alignment == NhlINTERIOREDGES)
		count = lb_p->box_count - 1;
	else
		count = lb_p->box_count + 1;
	lb_p->current_label_count = count;

	if (lb_p->label_strings != olb_p->label_strings) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->label_strings),
						  lb_p->label_strings,
						  NhlLB_MAX_LBL_STRINGS,
						  True,False,
						  NhlNlbLabelStrings, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lb_p->label_strings = olb_p->label_strings;
		olb_p->label_strings = NULL;
	}

	if (lb_p->label_strings->num_elements < count) {
		s_p = (NhlString *) lb_p->label_strings->data;
		if ((s_p = (NhlString *)
		     NhlRealloc(s_p, count * sizeof (NhlString))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbLabelStrings);
			return NhlFATAL;
		}
		for (i=lb_p->label_strings->num_elements; i<count; i++) {
			sprintf(number,"%d",i);
			if ((s_p[i] = (char *)
			     NhlMalloc(strlen(NhlLB_DEF_STRING) + 
				       strlen(number) + 1)) == NULL) {
				e_text = "%s: error creating %s string";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlbLabelStrings);
				return NhlFATAL;
			}
			strcpy(s_p[i],(Const char *)NhlLB_DEF_STRING);
			strcat(s_p[i],number);
		}

		lb_p->label_strings->data = (NhlPointer) s_p;
		lb_p->label_strings->num_elements = count;
	}


/*======================================================================*/
/* 
 * Manage the box_fraction array. If the box count changes then reset the
 * box fraction array to a default (uniform) state. This is because there
 * is no obvious way to modify explicitly set sizes to handle a different
 * number of boxes. Even if the box_fraction resource is set in the 
 * same call the reset occurs because it is difficult to ensure that 
 * the passed in array contains the appropriate elements for the new 
 * box_count. Note that error checking for the elements of this resource 
 * is handled later. The box fraction array contains one more element than
 * the box count.
 */
	count = lb_p->box_count+1;
	if (lb_p->box_fractions != olb_p->box_fractions) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->box_fractions),
						  lb_p->box_fractions,
						  NhlLB_MAX_BOXES+1,
						  True,False,
						  NhlNlbBoxFractions, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
			return ret;
		lb_p->box_fractions = olb_p->box_fractions;
		olb_p->box_fractions = NULL;
	}
	
	if (lb_p->box_fractions->num_elements < count) {
		f_p = (float *) lb_p->box_fractions->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, (count) * 
				sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbBoxFractions);
			return NhlFATAL;
		}

		lb_p->box_fractions->data = (NhlPointer) f_p;
		lb_p->box_fractions->num_elements = count;
	}

	if (lb_p->box_count != olb_p->box_count) {
		f_p = (float *) lb_p->box_fractions->data;
		f_p[0] = 0.0;
		for (i=1;i<lb_p->box_count; i++)
			f_p[i] = -1.0;
		f_p[lb_p->box_count] = 1.0;
	}

/*======================================================================*/
	     
/*
 * Allocate or reallocate the location arrays: use one more than
 * the current box count so that both ends of the labelbar can be stored
 */

	if (lb_p->box_count > olb_p->box_count) {
		if (lb_p->box_locs == NULL) {
			lb_p->box_locs = (float *) 
				NhlMalloc((lb_p->box_count+1) * sizeof(float));
		}
		else {
			lb_p->box_locs = (float *) 
				NhlRealloc(lb_p->box_locs, 
					   (lb_p->box_count+1) *
					   sizeof(float));
		}
	}
	
	return (ret);
}

/*
 * Function: SetLabelBarGeometry
 *
 * Description:	Performs a rough division of the space available for the
 *		LabelBar into item area, label area and title area. Depending
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
static NhlErrorTypes    SetLabelBarGeometry
#if	NhlNeedProto
(
	NhlLayer new, 
	NhlLayer old,
	NhlBoolean init,
 	_NhlArgList args,
	int num_args
)
#else
(new,old,init,args,num_args)
	NhlLayer	new;
	NhlLayer	old;
	NhlBoolean	init,
	_NhlArgList	args;
	int		num_args;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, ret1 = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	enum {NO_TITLE, MINOR_AXIS, MAJOR_AXIS} title_loc;
	float bar_ext,bar_room,title_ext;

	entry_name = init ? Init_Name : SetValues_Name;

/* Calculate the ndc margin from the fractional margin */

	lb_p->small_axis = MIN(lb_p->perim.r - lb_p->perim.l, 
			       lb_p->perim.t - lb_p->perim.b);

	lb_p->adj_perim.l = lb_p->perim.l + lb_p->margin.l * lb_p->small_axis;
	lb_p->adj_perim.r = lb_p->perim.r - lb_p->margin.r * lb_p->small_axis;
	lb_p->adj_perim.b = lb_p->perim.b + lb_p->margin.b * lb_p->small_axis;
	lb_p->adj_perim.t = lb_p->perim.t - lb_p->margin.t * lb_p->small_axis;
	lb_p->adj_width = lb_p->adj_perim.r - lb_p->adj_perim.l;
	lb_p->adj_height = lb_p->adj_perim.t - lb_p->adj_perim.b;

/*
 * Check the box extent values; and issue a warning if required
 */	
	if (lb_p->box_major_ext > 1.0 || lb_p->box_major_ext < 0.0) {
		e_text = "%s: %s out of range, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNlbBoxMajorExtentF);
		ret = MIN(ret,NhlWARNING);
		lb_p->box_major_ext = 1.0;
	}
	if (lb_p->box_minor_ext > 1.0 || lb_p->box_minor_ext < 0.0) {
		e_text = "%s: %s out of range, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNlbBoxMinorExtentF);
		ret = MIN(ret,NhlWARNING);
		lb_p->box_minor_ext = 0.33;
	}
/*
 * Check the title extent to make sure it does not exceed the hard-coded
 * limit; then locate the title
 */
	if (lb_p->auto_manage && 
	    lb_p->title_ext + lb_p->title_off > 0.5) {
		e_text = "%s: sum of %s and %s are too large, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,
			  NhlNlbTitleExtentF,NhlNlbTitleOffsetF);
		ret = MIN(ret,NhlWARNING);
		lb_p->title_ext = NhlLB_DEF_TITLE_EXT;
		lb_p->title_off = NhlLB_DEF_TITLE_OFF;
	}
	if (!lb_p->title_on) {
		title_loc = NO_TITLE;
		lb_p->title_off_ndc = 0.0;
	}
	else if (lb_p->orient == NhlHORIZONTAL) {
		if (lb_p->title_pos == NhlRIGHT ||
		    lb_p->title_pos == NhlLEFT) {
			title_loc = MAJOR_AXIS;
			lb_p->title_off_ndc =
				lb_p->title_off * lb_p->adj_width;
			title_ext = lb_p->title_ext * lb_p->adj_width;
		}
		else {
			title_loc = MINOR_AXIS;
			lb_p->title_off_ndc =
				lb_p->title_off * lb_p->adj_height;
			title_ext = lb_p->title_ext * lb_p->adj_height;
		}
	}
	else {
		if (lb_p->title_pos == NhlRIGHT ||
		    lb_p->title_pos == NhlLEFT) {
			title_loc = MINOR_AXIS;
			lb_p->title_off_ndc =
				lb_p->title_off * lb_p->adj_width;
			title_ext = lb_p->title_ext * lb_p->adj_width;
		}
		else {
			title_loc = MAJOR_AXIS;
			lb_p->title_off_ndc =
				lb_p->title_off * lb_p->adj_height;
			title_ext = lb_p->title_ext * lb_p->adj_height;
		}
	}

	if (lb_p->orient == NhlHORIZONTAL) {
		switch (title_loc) {

		case NO_TITLE:
			lb_p->title.l = lb_p->adj_perim.l;
			lb_p->title.r = lb_p->adj_perim.r;
			lb_p->title.b = lb_p->adj_perim.b;
			lb_p->title.t = lb_p->adj_perim.t;

			break;

		case MAJOR_AXIS:
			
			lb_p->title.b = lb_p->adj_perim.b;
			lb_p->title.t = lb_p->adj_perim.t;
				
			if (lb_p->title_pos == NhlLEFT) {
				lb_p->title.l = lb_p->adj_perim.l;
				lb_p->title.r = lb_p->title.l + title_ext;
			}
			else if (lb_p->title_pos == NhlRIGHT) {
				lb_p->title.r = lb_p->adj_perim.r;
				lb_p->title.l = lb_p->title.r - title_ext;
			}
			break;

		case MINOR_AXIS:

			lb_p->title.l = lb_p->adj_perim.l;
			lb_p->title.r = lb_p->adj_perim.r;
				
			if (lb_p->title_pos == NhlBOTTOM) {
				lb_p->title.b = lb_p->adj_perim.b;
				lb_p->title.t = lb_p->title.b + title_ext;
			}
			else if (lb_p->title_pos == NhlTOP) {
				lb_p->title.t = lb_p->adj_perim.t;
				lb_p->title.b = lb_p->title.t - title_ext;
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
			lb_p->title.l = lb_p->adj_perim.l;
			lb_p->title.r = lb_p->adj_perim.r;
			lb_p->title.b = lb_p->adj_perim.b;
			lb_p->title.t = lb_p->adj_perim.t;
			break;

		case MAJOR_AXIS:
			lb_p->title.l = lb_p->adj_perim.l;
			lb_p->title.r = lb_p->adj_perim.r;
				
			if (lb_p->title_pos == NhlBOTTOM) {
				lb_p->title.b = lb_p->adj_perim.b;
				lb_p->title.t = lb_p->title.b + title_ext;
			}
			else if (lb_p->title_pos == NhlTOP) {
				lb_p->title.t = lb_p->adj_perim.t;
				lb_p->title.b = lb_p->title.t - title_ext;
			}
			break;

		case MINOR_AXIS:
			lb_p->title.b = lb_p->adj_perim.b;
			lb_p->title.t = lb_p->adj_perim.t;
				
			if (lb_p->title_pos == NhlLEFT) {
				lb_p->title.l = lb_p->adj_perim.l;
				lb_p->title.r = lb_p->title.l + title_ext;
			}
			else if (lb_p->title_pos == NhlRIGHT) {
				lb_p->title.r = lb_p->adj_perim.r;
				lb_p->title.l = lb_p->title.r - title_ext;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	ret1 = SetTitle(new,old,init,args,num_args);
	if ((ret = MIN(ret1,ret)) < NhlWARNING) return ret;

/*
 * Silently modify the label position if not appropriate for the
 * current labelbar orientation. Also determine the maximum critical
 * angle for label angle adjustments.
 */

	if (lb_p->orient == NhlHORIZONTAL) {
		switch (lb_p->label_pos) {
		case NhlCENTER:
			break;
		default:
		case NhlTOP:
		case NhlLEFT:
			lb_p->label_pos = NhlTOP;
			break;
		case NhlBOTTOM:
		case NhlRIGHT:
			lb_p->label_pos = NhlBOTTOM;
			break;
		}
	}
	else {
		switch (lb_p->label_pos) {
		case NhlCENTER:
			break;
		default:
		case NhlRIGHT:
		case NhlBOTTOM:
			lb_p->label_pos = NhlRIGHT;
			break;
		case NhlLEFT:
		case NhlTOP:
			lb_p->label_pos = NhlLEFT;
			break;
		}

	}
		
/*
 * Determine bar and label dimensions.
 * Bar dimensions may be adjusted later to 
 * ensure that labels are visible.
 */
	if (lb_p->orient == NhlHORIZONTAL) {
		bar_room = MAX(lb_p->title.txtr - lb_p->title.bxtr,
			       lb_p->adj_height);

		bar_ext =  lb_p->box_minor_ext * bar_room;
		switch (title_loc) {

		case NO_TITLE:
			lb_p->bar.l = lb_p->adj_perim.l;
			lb_p->bar.r = lb_p->adj_perim.r;
			lb_p->labels.l = lb_p->bar.l;
			lb_p->labels.r = lb_p->bar.r;
					
			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = lb_p->adj_perim.b +
					(bar_room - bar_ext) / 2.0;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
				lb_p->labels.t = lb_p->adj_perim.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.t - bar_ext;
				lb_p->bar.t = lb_p->adj_perim.t;
				lb_p->labels.b = lb_p->adj_perim.b;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;

		case MAJOR_AXIS:
			if (lb_p->title_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.l +
					title_ext + lb_p->title_off_ndc;
				lb_p->bar.r = lb_p->adj_perim.r;
			}
			else if (lb_p->title_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->adj_perim.r - 
					title_ext - lb_p->title_off_ndc;
			}
			lb_p->labels.l = lb_p->bar.l;
			lb_p->labels.r = lb_p->bar.r;

			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = lb_p->adj_perim.b +
					(bar_room - bar_ext) / 2.0;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
				lb_p->labels.t = lb_p->adj_perim.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.t - bar_ext;
				lb_p->bar.t = lb_p->adj_perim.t;
				lb_p->labels.b = lb_p->adj_perim.b;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;

		case MINOR_AXIS:

			if (title_ext + 
			    lb_p->title_off_ndc + bar_ext > bar_room) {
				e_text = 
				  "%s: Maximum bar size exceeded, defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret, NhlWARNING);
				bar_ext = bar_room - 
					title_ext - lb_p->title_off_ndc;
				lb_p->box_minor_ext = bar_ext / bar_room;
			}
			lb_p->bar.l = lb_p->adj_perim.l;
			lb_p->bar.r = lb_p->adj_perim.r;
			lb_p->labels.l = lb_p->adj_perim.l;
			lb_p->labels.r = lb_p->adj_perim.r;
				
			if (lb_p->title_pos == NhlBOTTOM) {
				lb_p->bar.b = 
					lb_p->title.t + lb_p->title_off_ndc;
				lb_p->bar.t = lb_p->adj_perim.t;
			}
			else if (lb_p->title_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->adj_perim.t - 
					title_ext - lb_p->title_off_ndc;
			}

			
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = (lb_p->bar.t - lb_p->bar.b - 
					       bar_ext) / 2.0 + lb_p->bar.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->labels.t = lb_p->bar.t;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->labels.b = lb_p->bar.b;
				lb_p->bar.b = lb_p->bar.t - bar_ext;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		/* get the box size */
		
		lb_p->box_size.x = 
			(lb_p->bar.r - lb_p->bar.l) / lb_p->box_count;
		lb_p->box_size.y = lb_p->bar.t - lb_p->bar.b;
	}

	else { /* NhlVERTICAL */

		bar_room = MAX(lb_p->title.rxtr - lb_p->title.lxtr,
			       lb_p->adj_width);
		bar_ext = lb_p->box_minor_ext * bar_room;
		switch (title_loc) {

		case NO_TITLE:
			lb_p->bar.b = lb_p->adj_perim.b;
			lb_p->bar.t = lb_p->adj_perim.t;
			lb_p->labels.b = lb_p->adj_perim.b;
			lb_p->labels.t = lb_p->adj_perim.t;
					
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = lb_p->adj_perim.l +
					(bar_room - bar_ext) / 2.0;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
				lb_p->labels.r = lb_p->adj_perim.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.r - bar_ext;
				lb_p->bar.r = lb_p->adj_perim.r;
				lb_p->labels.l = lb_p->adj_perim.l;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;

		case MAJOR_AXIS:
				
			if (lb_p->title_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.b + 
					title_ext + lb_p->title_off_ndc;
				lb_p->bar.t = lb_p->adj_perim.t;
			}
			else if (lb_p->title_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->adj_perim.t - 
					title_ext - lb_p->title_off_ndc;
			}
			lb_p->labels.b = lb_p->bar.b;
			lb_p->labels.t = lb_p->bar.t;

			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = lb_p->adj_perim.l +
					(bar_room - bar_ext) / 2.0;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
				lb_p->labels.r = lb_p->adj_perim.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.r - bar_ext;
				lb_p->bar.r = lb_p->adj_perim.r;
				lb_p->labels.l = lb_p->adj_perim.l;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;

		case MINOR_AXIS:

			if (title_ext + 
			    lb_p->title_off_ndc + bar_ext > bar_room) {
				e_text = 
				  "%s: Maximum bar size exceeded, defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret, NhlWARNING);
				bar_ext = bar_room - 
					title_ext - lb_p->title_off_ndc;
				lb_p->box_minor_ext = bar_ext / bar_room;
			}
			lb_p->bar.b = lb_p->adj_perim.b;
			lb_p->bar.t = lb_p->adj_perim.t;
			lb_p->labels.b = lb_p->adj_perim.b;
			lb_p->labels.t = lb_p->adj_perim.t;
				
			if (lb_p->title_pos == NhlLEFT) {
				lb_p->bar.l = 
					lb_p->title.r + lb_p->title_off_ndc;
				lb_p->bar.r = lb_p->adj_perim.r;
			}
			else if (lb_p->title_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->adj_perim.r - 
					title_ext - lb_p->title_off_ndc;
			}
			
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = (lb_p->bar.r - lb_p->bar.l - 
					       bar_ext) / 2.0 + lb_p->bar.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->labels.r = lb_p->bar.r;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->labels.l = lb_p->bar.l;
				lb_p->bar.l = lb_p->bar.r - bar_ext;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		lb_p->box_size.x = lb_p->bar.r - lb_p->bar.l;
		lb_p->box_size.y = 
			(lb_p->bar.t - lb_p->bar.b) / lb_p->box_count;
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
 *	This routine could use performance evaluation and tuning.
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
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayer	told = (NhlLabelBarLayer) old;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlLabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	char buffer[_NhlMAXRESNAMLEN];
	char *c_p;
	NhlBoundingBox titleBB;
	float w, h, wta, hta, factor, height;
	char *entry_name;
	float angle;
	NhlBoolean string_changed = False;

	lb_p->title.lxtr = lb_p->title.l;
	lb_p->title.rxtr = lb_p->title.r;
	lb_p->title.bxtr = lb_p->title.b;
	lb_p->title.txtr = lb_p->title.t;
	lb_p->title_x = lb_p->title.lxtr + 
		(lb_p->title.rxtr - lb_p->title.lxtr) / 2.0;
	lb_p->title_y = lb_p->title.bxtr + 
		(lb_p->title.txtr - lb_p->title.bxtr) / 2.0;
/*
 * Only initialize a text item for the title if it is turned on
 * and the maximum title extent is greater than 0.0.
 */
	if (!lb_p->title_on || lb_p->title_ext <= 0.0)
		return ret;

	entry_name = init ? Init_Name : SetValues_Name;
/*
 * Check the constant spacing value 
 */
	if (lb_p->title_const_spacing < 0.0) {
		lb_p->title_const_spacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: Constant spacing cannot be less than zero, defaulting %s",
			  "LabelBar",NhlNlbTitleConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}
/*
 * If the title string is NULL, create a default string.
 * The default string is the name of the label bar object
 */
	if (lb_p->title_string == lbDefTitle) {
                lb_p->title_string = (char*)
                        NhlMalloc((unsigned)strlen(tnew->base.name) +1);
                strcpy(lb_p->title_string,tnew->base.name);
        } 
	else if (init) {
                c_p = lb_p->title_string;
                lb_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
                strcpy(lb_p->title_string,c_p);
		string_changed = True;
        }
	else if (lb_p->title_string != olb_p->title_string) {
		if(olb_p->title_string != lbDefTitle)
			NhlFree(olb_p->title_string);
		c_p = lb_p->title_string;
		lb_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
		strcpy(lb_p->title_string, c_p);
		string_changed = True;
	}

	switch (lb_p->title_just) {
	case NhlBOTTOMLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlBOTTOMCENTER:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlBOTTOMRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlCENTERLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlCENTERCENTER:
	default:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlCENTERRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlTOPLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.t;
		break;
	case NhlTOPCENTER:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.t;
		break;
	case NhlTOPRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.t;
		break;
	}

	if (lb_p->title_height <= 0.0) 
		height = NhlLB_DEF_CHAR_HEIGHT;
	else 
		height = lb_p->title_height;

	angle = (lb_p->title_angle < 0.0) ?
		lb_p->title_angle + 360.0 : lb_p->title_angle;
	if (lb_p->title_id < 0) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Title");
		ret1 = NhlVACreate(&lb_p->title_id,
				   buffer,NhltextItemClass,tnew->base.id,
				   NhlNtxFont,lb_p->title_font,
				   NhlNtxString,lb_p->title_string,
				   NhlNtxPosXF,lb_p->title_x,
				   NhlNtxPosYF,lb_p->title_y,
				   NhlNtxDirection,lb_p->title_direction,
				   NhlNtxAngleF,angle,
				   NhlNtxJust,lb_p->title_just,
				   NhlNtxFontColor,lb_p->title_color,
				   NhlNtxFontHeightF,height,
				   NhlNtxFontAspectF,lb_p->title_aspect,
				   NhlNtxConstantSpacingF,
				   lb_p->title_const_spacing,
				   NhlNtxFontQuality,lb_p->title_quality,
				   NhlNtxFuncCode,lb_p->title_func_code,
				   NhlNtxFontThicknessF,lb_p->title_thickness,
				   NULL);
		if (ret1 < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: Title create error", entry_name);
			return(NhlFATAL);
		}
		ret = MIN(ret,ret1);
	}
	else {
		NhlSArg		sargs[16];
		int		sargc = 0;

		if (lb_p->title_font != olb_p->title_font)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFont,lb_p->title_font);
		if (string_changed)
			NhlSetSArg(&sargs[(sargc)++],
				    NhlNtxString,lb_p->title_string);
		if (lb_p->title_x != olb_p->title_x)
			NhlSetSArg(&sargs[(sargc)++],
				    NhlNtxPosXF,lb_p->title_x);
		if (lb_p->title_y != olb_p->title_y)
			NhlSetSArg(&sargs[(sargc)++],
				    NhlNtxPosYF,lb_p->title_y);
		if (lb_p->title_direction != olb_p->title_direction)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxDirection,lb_p->title_direction);
		if (angle != olb_p->title_angle)
			NhlSetSArg(&sargs[(sargc)++],NhlNtxAngleF,angle);
		if (lb_p->title_just != olb_p->title_just)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxJust,(int)lb_p->title_just);
		if (lb_p->title_color != olb_p->title_color)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontColor,lb_p->title_color);
		if (height != olb_p->title_height)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontHeightF,height);
		if (lb_p->title_aspect != olb_p->title_aspect)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontAspectF,lb_p->title_aspect);
		if (lb_p->title_const_spacing != olb_p->title_const_spacing)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxConstantSpacingF,
				   lb_p->title_const_spacing);
		if (lb_p->title_quality != olb_p->title_quality)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontQuality,lb_p->title_quality);
		if (lb_p->title_func_code != olb_p->title_func_code)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFuncCode,lb_p->title_func_code);
		if (lb_p->title_thickness != olb_p->title_thickness)
			NhlSetSArg(&sargs[(sargc)++],
				   NhlNtxFontThicknessF,lb_p->title_thickness);
		ret1 = NhlALSetValues(lb_p->title_id,sargs,sargc);
		if (ret1 < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: Title SetValues error", entry_name);
			return(NhlFATAL);
		}
		ret = MIN(ret,ret1);
	}
	

/*
 * If title_height is 0.0 or auto-manage is in effect adjust the title
 * to the space available
 */
	ret1 = NhlGetBB(lb_p->title_id, &titleBB);
	if (ret1 < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: Error getting title bounding box", entry_name);
		return(NhlFATAL);
	}
	ret = MIN(ret,ret1);

	w=titleBB.r-titleBB.l;
	h=titleBB.t-titleBB.b;
	if (w <= 0.0 || h <= 0.0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: no area in title bounding box",entry_name);
		return(NhlFATAL);
	}
	wta=lb_p->title.r-lb_p->title.l;
	hta=lb_p->title.t-lb_p->title.b;
	if (wta <= 0.0 || hta <= 0.0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: no area available for title",entry_name);
	}
	if (lb_p->title_height <= 0.0 || lb_p->auto_manage) {
		factor = wta / w < hta / h ? wta / w : hta / h;
		lb_p->title_height = height * factor;
		if (height != lb_p->title_height) {
			ret1 = NhlVASetValues(lb_p->title_id,
					  NhlNtxFontHeightF,lb_p->title_height,
					      NULL);
			if ((ret = MIN(ret,ret1)) < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting title height",entry_name);
				return(NhlFATAL);
			}
		}
	}
	else {
		lb_p->title.lxtr = MIN(lb_p->title.l,titleBB.l);
		lb_p->title.rxtr = MAX(titleBB.r,lb_p->title.r);
		lb_p->title.bxtr = MIN(lb_p->title.b,titleBB.b);
		lb_p->title.txtr = MAX(titleBB.t,lb_p->title.t);
	}
	return ret;
}

/*
 * Function:  SetBoxLocations
 *
 * Description:	Sets up an array containing the beginning and ending
 *	coordinate of each box of the label bar along the major axis.
 *	If the box sizing mode is explicit, first calls an internal 
 *	function to check and coerce the user-supplied values in the
 *	box sizing GenArray into a proper sequence of values.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The box_sizing GenArray may be modified.
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
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	float bar_len;
	int i;
	float *box_fractions = (float *)lb_p->box_fractions->data;
	char *entry_name;

	entry_name = init ? Init_Name : SetValues_Name;

	if (lb_p->box_sizing == NhlEXPLICITSIZING) {
		ret1 = ManageBoxFractionsArray(box_fractions,
					       lb_p->box_count,entry_name);
	}
	ret = MIN(ret,ret1);

/* 
 * Adjust boundary of bar and label area, depending on the label state.
 */

	memcpy((void *)&lb_p->adj_bar, (Const void *)&lb_p->bar, 
	       sizeof(NhlBoundingBox));
	memcpy((void *)&lb_p->adj_box_size, (Const void *)&lb_p->box_size,
	       sizeof(NhlCoord));
	if (lb_p->label_alignment == NhlEXTERNALEDGES) {
		if (lb_p->orient == NhlHORIZONTAL) {
			lb_p->adj_box_size.x = lb_p->box_size.x * 
				lb_p->box_count / (lb_p->box_count + 1);
			lb_p->adj_bar.l = 
				lb_p->bar.l + lb_p->adj_box_size.x / 2.0;
			lb_p->adj_bar.r = 
				lb_p->bar.r - lb_p->adj_box_size.x / 2.0;
		}
		else {
			lb_p->adj_box_size.y = lb_p->box_size.y * 
				lb_p->box_count / (lb_p->box_count + 1);
			lb_p->adj_bar.b = 
				lb_p->bar.b + lb_p->adj_box_size.y / 2.0;
			lb_p->adj_bar.t = 
				lb_p->bar.t - lb_p->adj_box_size.y / 2.0;
		}
	}
	else if (lb_p->label_alignment == NhlINTERIOREDGES) {
		if (lb_p->orient == NhlHORIZONTAL) {
			lb_p->labels.l = lb_p->bar.l + lb_p->box_size.x / 2.0;
			lb_p->labels.r = lb_p->bar.r - lb_p->box_size.x / 2.0;
		}
		else {
			lb_p->labels.b = lb_p->bar.b + lb_p->box_size.y / 2.0;
			lb_p->labels.t = lb_p->bar.t - lb_p->box_size.y / 2.0;
		}
	}

/*
 * create an array of the left or bottom varying coordinates -- 
 */
		
	if (lb_p->orient == NhlHORIZONTAL &&
	    lb_p->box_sizing == NhlUNIFORMSIZING) {
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.l + 
				(float) i * lb_p->adj_box_size.x;
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.r;
	}
	if (lb_p->orient == NhlVERTICAL &&
	    lb_p->box_sizing == NhlUNIFORMSIZING) {
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.b + 
				(float) i * lb_p->adj_box_size.y;
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.t;
	}
	if (lb_p->orient == NhlHORIZONTAL &&
	    lb_p->box_sizing == NhlEXPLICITSIZING) {
		bar_len = lb_p->adj_bar.r - lb_p->adj_bar.l;
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.l + 
				bar_len * box_fractions[i];
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.r;
	}
	if (lb_p->orient == NhlVERTICAL &&
	    lb_p->box_sizing == NhlEXPLICITSIZING) {
		bar_len = lb_p->adj_bar.t - lb_p->adj_bar.b;
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.b + 
				bar_len * box_fractions[i];
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.t;
	}

	return(ret);

}

/*
 * Function: ManageBoxFractionsArray
 *
 * Description:	Finds and modifies out-of-range values in the item positions
 *	array, so that a monotonically increasing set of values is returned. 
 *	The assumption is that negative values are placed intentionally, in
 *	order to force linear division of the subspace between two properly
 *	ascending bounding values in the array. Values that are out-of-range
 *	in some other way, such as exceeding the value of a subsequent 
 *	element, are flagged with an informational message but still are
 *	coerced into a proper sequence. Note that the box_sizing GenArray
 *	must contain one element more than the box count, and the 
 *	first element must contain 0.0 and the last element must contain 1.0.
 *	
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The box_sizing GenArray may be modified.
 */

/*ARGSUSED*/
static NhlErrorTypes    ManageBoxFractionsArray
#if	NhlNeedProto
(
	float *box_fractions, 
	int count,
	char *entry_name
) 
#else
(box_fractions,count,entry_name)
	float *box_fractions;
	int count;
	char *entry_name;
#endif

{
	int i, first_neg = -1;
	NhlErrorTypes ret = NhlNOERROR;
			
/*
 * At this point it should be guaranteed that the box_fraction array exists
 * and it is of the correct length.
 * Now check the validity of the explicit sizing fractions. Negative values
 * are not an error, but cause equal divisions between the two surrounding
 * specified values. Non-monotonically increasing positive values, or
 * values greater than 1.0 are an error causing a warning. However, this
 * function modifies incorrect values to make them usable.
 */
	/* deal with first element manually, since in the loop the
	   previous element must be compared */
	/* trial version */

	/* the first value must be 0.0 and the last 1.0 */

	if (box_fractions[0] < 0.0)
		box_fractions[0] = 0.0;
	else if (box_fractions[0] > 0.0) {
		NhlPError(NhlINFO,NhlEUNKNOWN,
		      "%s: Modifying invalid box fraction array element: 0",
			  entry_name);
		ret = NhlINFO;
		box_fractions[0] = 0.0;
	}
	if (box_fractions[count] < 0.0)
		box_fractions[count] = 1.0;
	else if (box_fractions[count] != 1.0) {
		NhlPError(NhlINFO,NhlEUNKNOWN,
		       "%s: Modifying invalid box fraction array element: %d",
			  entry_name,count);
		ret = NhlINFO;
		box_fractions[count] = 1.0;
	}

	for (i=1; i<count+1;i++) {
		if (box_fractions[i] < 0.0) {
			if (first_neg == -1)
				first_neg = i;
		}
		else if (first_neg != -1) {
			if (box_fractions[i] > 1.0 ||
			    box_fractions[i] <=
			    box_fractions[first_neg-1]) {
				NhlPError(NhlINFO,NhlEUNKNOWN,
		    "%s: Modifying invalid box fraction array element: %d",
					  entry_name,i);
				ret = NhlINFO;
				box_fractions[i] = -1.0;
			}
			else {
				CreateIntermediates(box_fractions, 
						    first_neg, i);
				first_neg = -1;
			}
		}
		else if (box_fractions[i] > 1.0 ||
			 (box_fractions[i] <= box_fractions[i-1])) {
			NhlPError(NhlINFO,NhlEUNKNOWN,
		      "%s: Modifying invalid box fraction array element: %d",
				  entry_name,i);
			ret = NhlINFO;
			box_fractions[i] = -1.0;
			first_neg = i;
		}
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
 * 	first non-negative value. Since the zeroth value has been dealt with
 *	separately, it is not necessary to test for it here.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: Modifies the contents of the box_fractions GenArray.
 */

static void CreateIntermediates
#if	NhlNeedProto
	(float		*flist,
	 int		start,
	 int		end)
#else
(flist,start,end)
	float		*flist;
	int		start;
	int		end;
#endif
{
	float	spacing;
	int	count;
	int	i;

	count = end - start + 1;
	spacing = (flist[end] - flist[start - 1]) / count;
	for (i=0; i<count-1; i++) {
		flist[start+i] = flist[start-1] + (float) (i+1) * spacing;
	}
}


/*
 * Function:  SetLabels
 *
 * Description:	Calculates the attributes of the LabelBar labels
 *	and creates or sets the child multi-text object that is responsible
 *	for rendering the labels. If in auto-manage mode, inquires the 
 *	bounding box of the multitext object after a preliminary setting of
 *	the attributes, then calls an internal function to
 *	adjust the height and/or justification of the multi-text object in
 *	order to ensure that the labels do not overlap on rotation. 
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
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlLabelBarLayerPart *olb_p = &((NhlLabelBarLayer) old)->labelbar;
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
	char	*entry_name;
	float angle,label_loc;
	float x,y,width,height;
	NhlBoolean label_locs_changed = False;
	NhlBoolean keep_end_items;
	NhlSArg	sargs[24];
	int	nargs = 0;
		
	if (! lb_p->labels_on) {
		lb_p->labels.lxtr = lb_p->labels.l;
		lb_p->labels.rxtr = lb_p->labels.r;
		lb_p->labels.bxtr = lb_p->labels.b;
		lb_p->labels.txtr = lb_p->labels.t;
		return NhlNOERROR;
	}

	entry_name = init ? Init_Name : SetValues_Name;
/*
 * Check the constant spacing resource
 */
	if (lb_p->label_const_spacing < 0.0) {
		lb_p->label_const_spacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: Constant spacing cannot be less than zero, defaulting %s",
			  entry_name,NhlNlbLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}

/*
 * Determine the multitext orientation and the NDC label offset
 */

	if (lb_p->orient == NhlHORIZONTAL) {
		mtext_orient = NhlMTEXT_Y_CONST;
		lb_p->label_off_ndc = lb_p->label_off * 
			(lb_p->adj_perim.t - lb_p->adj_perim.b);
	}
	else {
		mtext_orient = NhlMTEXT_X_CONST;
		lb_p->label_off_ndc = lb_p->label_off *
			(lb_p->adj_perim.r - lb_p->adj_perim.l);
	}

/*
 * The number of labels varies depending on the label alignment
 */

	count = lb_p->current_label_count;
		
/*
 * If the label stride is greater than 1, find the number of
 * labels to use. If greater than the previous size, reallocate
 * the array used to point to the correct labels.
 */
	
	lb_p->label_draw_count = count;
	labels_p = (NhlString *) lb_p->label_strings->data;
	if (lb_p->label_stride > 1) {
		count = (count % lb_p->label_stride == 0) ?
			count / lb_p->label_stride :  
			count / lb_p->label_stride + 1;
		if (count > lb_p->max_label_stride_count) {
			if (lb_p->stride_labels == NULL) {
				lb_p->stride_labels = (char **) 
					NhlMalloc(count * sizeof(char *));
			}
			else {
				lb_p->stride_labels = (char **)
					NhlRealloc(lb_p->stride_labels,
						   count * sizeof(char *));
			}
			lb_p->max_label_stride_count = count;
		}
		labels_p = lb_p->stride_labels;
		for (i=0,j=0; i<count; i++,j+=lb_p->label_stride) {
			labels_p[i] = 
				((NhlString *)lb_p->label_strings->data)[j];
		}
		lb_p->label_draw_count = count;
	}

/*
 * Now allocate the location array
 */
	if (count > lb_p->max_label_draw_count) {
		if (lb_p->label_locs == NULL) {
			lb_p->label_locs = (float *) 
				NhlMalloc(count * sizeof(float));
		}
		else {
			lb_p->label_locs = (float *)
				NhlRealloc(lb_p->label_locs,
					   count * sizeof(float));
		}
		for (i = lb_p->max_label_draw_count; i < count; i++)
			lb_p->label_locs[i] = -999.0;
		lb_p->max_label_draw_count = count;
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
	
	if (lb_p->label_pos == NhlCENTER) {
		if (lb_p->orient == NhlHORIZONTAL) {
			larea.x = lb_p->labels.r - lb_p->labels.l;
			larea.y = lb_p->adj_bar.t - 
				lb_p->adj_bar.b - lb_p->label_off_ndc;
		}
		else {
			larea.x = lb_p->adj_bar.r - 
				lb_p->adj_bar.l - lb_p->label_off_ndc;
			larea.y = lb_p->labels.t - lb_p->labels.b;
		}
	}
	else {
		if (lb_p->orient == NhlHORIZONTAL) {
			larea.x = lb_p->labels.r - lb_p->labels.l;
			larea.y = lb_p->labels.t - lb_p->labels.b - 
				lb_p->label_off_ndc;
		}
		else {
			larea.x = lb_p->labels.r - lb_p->labels.l - 
				lb_p->label_off_ndc;
			larea.y = lb_p->labels.t - lb_p->labels.b;
		}
	}
		
/*
 * Account for the box_major_ext fraction if centered or label
 * offset is negative
 */
	if (lb_p->label_pos == NhlCENTER || lb_p->label_off_ndc < 0.0) {
		if (lb_p->label_alignment == NhlBOXCENTERS) {
			c_frac = lb_p->box_major_ext;
		}
		else if (lb_p->box_major_ext > 0.8) {
			c_frac = 1.0;
		}
		else {
			c_frac = 1.0 - lb_p->box_major_ext;
		}
	}

	 
	if (lb_p->orient == NhlHORIZONTAL && 
	    lb_p->label_direction == NhlACROSS) {

		char_space = 0.8 * larea.y;
		avail_char_space = c_frac * 0.8 * 
			larea.x / (lb_p->label_draw_count * max_strlen);

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		switch (lb_p->label_pos) {
		case NhlLEFT:
		case NhlRIGHT:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlBOTTOM:
			lb_p->const_pos = lb_p->labels.t - lb_p->label_off_ndc;
			break;
		case NhlCENTER:
			lb_p->const_pos = lb_p->adj_bar.b + 
				lb_p->adj_box_size.y / 2.0;
			break;
		case NhlTOP:
			lb_p->const_pos = lb_p->labels.b + lb_p->label_off_ndc;
			break;
		}
	}
	else if (lb_p->orient == NhlHORIZONTAL){ /* NhlDOWN or UP */

		/* Set the font height */

		char_space = 0.8 * larea.y / max_strlen;
		avail_char_space = c_frac * 0.8 * 
			larea.x / lb_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		switch (lb_p->label_pos) {
		case NhlLEFT:
		case NhlRIGHT:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlBOTTOM:
			lb_p->const_pos = lb_p->labels.t - lb_p->label_off_ndc;
			break;
		case NhlCENTER:
			lb_p->const_pos = lb_p->adj_bar.b + 
				lb_p->adj_box_size.y / 2.0;
			break;
		case NhlTOP:
			lb_p->const_pos = lb_p->labels.b + lb_p->label_off_ndc;
			break;
		}
	}
	else if (lb_p->orient == NhlVERTICAL && 
		 lb_p->label_direction == NhlACROSS) {

		char_space = 0.8 * larea.x / max_strlen;
		avail_char_space = c_frac * 0.8 * 
			larea.y / lb_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		switch (lb_p->label_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlLEFT:
			lb_p->const_pos = lb_p->labels.r - lb_p->label_off_ndc;
			break;
		case NhlCENTER:
			lb_p->const_pos = lb_p->adj_bar.l + 
				lb_p->adj_box_size.x / 2.0;
			break;
		case NhlRIGHT:
			lb_p->const_pos = lb_p->labels.l + lb_p->label_off_ndc;
			break;
		}
	}
	else { /* NhlVERTICAL NhlDOWN or UP */

		char_space = 0.8 * larea.x;
		avail_char_space = c_frac * 0.5 * 
			larea.y / (lb_p->label_draw_count * max_strlen);

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		switch (lb_p->label_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlLEFT:
			lb_p->const_pos = lb_p->labels.r - lb_p->label_off_ndc;
			break;
		case NhlCENTER:
			lb_p->const_pos = lb_p->adj_bar.l + 
				lb_p->adj_box_size.x / 2.0;
			break;
		case NhlRIGHT:
			lb_p->const_pos = lb_p->labels.l + lb_p->label_off_ndc;
			break;
		}
	}

/*
 * Now find the variable label positions
 */

	if (lb_p->box_sizing == NhlUNIFORMSIZING) {

		if (lb_p->orient == NhlHORIZONTAL) {
		
			/* position array contains X values */

			base_pos = lb_p->adj_bar.l;
		       
			/* determine offset */
			if (lb_p->label_alignment == NhlBOXCENTERS)
				offset = lb_p->adj_box_size.x / 2.0;
			else if (lb_p->label_alignment == NhlINTERIOREDGES)
				offset = lb_p->adj_box_size.x;
			else
				offset = 0;

			increment = lb_p->adj_box_size.x * lb_p->label_stride;
		}
		else if (lb_p->orient == NhlVERTICAL) {
		
			/* position array contains Y values */

			base_pos = lb_p->adj_bar.b;
		       
			/* determine offset */
			if (lb_p->label_alignment == NhlBOXCENTERS)
				offset = lb_p->adj_box_size.y / 2.0;
			else if (lb_p->label_alignment == NhlINTERIOREDGES)
				offset = lb_p->adj_box_size.y;
			else
				offset = 0;

			increment = lb_p->adj_box_size.y * lb_p->label_stride;
		}
		for (i=0; i < lb_p->label_draw_count; i++) {
			label_loc = base_pos + offset + (float) i * increment;
			if (! label_locs_changed &&
			    _NhlCmpFAny(label_loc,lb_p->label_locs[i],6)) 
				label_locs_changed = True;
			lb_p->label_locs[i] = label_loc;
		}
	}
	else {
		for (i=0; i < lb_p->label_draw_count; i++) {

			ix = i * lb_p->label_stride;
			if (lb_p->label_alignment == NhlBOXCENTERS)
				label_loc = lb_p->box_locs[ix] +
					(lb_p->box_locs[ix+1] -
					  lb_p->box_locs[ix]) / 2.0;
			else if (lb_p->label_alignment == NhlINTERIOREDGES)
				label_loc = lb_p->box_locs[ix+1];
			else
				label_loc = lb_p->box_locs[ix];

			if (! label_locs_changed &&
			    _NhlCmpFAny(label_loc,lb_p->label_locs[i],6)) 
				label_locs_changed = True;
			lb_p->label_locs[i] = label_loc;
		}
	}

	if (lb_p->label_height <= 0.0)
		lb_p->label_height = NhlLB_DEF_CHAR_HEIGHT;
	if (! lb_p->auto_manage) 
		label_height = lb_p->label_height;
	lb_p->label_height = label_height;
	angle = (lb_p->label_angle < 0.0) ? 
		lb_p->label_angle + 360.0 : lb_p->label_angle;
	keep_end_items = lb_p->label_alignment == NhlEXTERNALEDGES ?
		True : False;

	if (lb_p->labels_id < 0) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Labels");
		subret = NhlVACreate(&(lb_p->labels_id),buffer,
				 NhlmultiTextClass,tnew->base.id,
				 NhlNMtextNumStrings,lb_p->label_draw_count,
				 NhlNMtextStrings,labels_p,
				 NhlNMtextOrientation,mtext_orient,
				 NhlNMtextConstPosF,lb_p->const_pos,
				 NhlNMtextPosArray,lb_p->label_locs,
				 NhlNMtextAutoStride,
				     lb_p->label_auto_stride,
				 NhlNMtextKeepEndItems,keep_end_items,
				 NhlNtxAngleF,angle,
				 NhlNtxFont,lb_p->label_font,
				 NhlNtxJust,lb_p->label_just,
				 NhlNtxFontHeightF,lb_p->label_height,
				 NhlNtxFontAspectF,lb_p->label_aspect,
				 NhlNtxDirection,lb_p->label_direction,
				 NhlNtxConstantSpacingF,
				 lb_p->label_const_spacing,
				 NhlNtxFontColor,lb_p->label_color,
				 NhlNtxFontThicknessF,lb_p->label_thickness,
				 NhlNtxFuncCode,lb_p->label_func_code,    
				 NhlNtxFontQuality,lb_p->label_quality,    
				 NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error creating MultiText object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		olb_p->label_height = lb_p->label_height;
		olb_p->label_just = lb_p->label_just;
		olb_p->const_pos = lb_p->const_pos;
		olb_p->label_auto_stride = lb_p->label_auto_stride;
		olb_p->label_alignment = lb_p->label_alignment;
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
		if (lb_p->label_draw_count != olb_p->label_draw_count ||
		    lb_p->label_strings != olb_p->label_strings ||
		    lb_p->label_stride != olb_p->label_stride) {
			NhlSetSArg(&sargs[nargs++],NhlNMtextNumStrings,
				   lb_p->label_draw_count);
			NhlSetSArg(&sargs[nargs++],NhlNMtextStrings,labels_p);
			NhlSetSArg(&sargs[nargs++],
				   NhlNMtextPosArray,lb_p->label_locs);
		}
		if (lb_p->orient != olb_p->orient)
			NhlSetSArg(&sargs[nargs++],NhlNMtextOrientation,
				   mtext_orient);
		if (lb_p->label_angle != olb_p->label_angle)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxAngleF,angle);
		if (lb_p->label_direction != olb_p->label_direction)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxDirection,lb_p->label_direction);
		if (lb_p->label_font != olb_p->label_font)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxFont,lb_p->label_font);
		if (lb_p->label_aspect != olb_p->label_aspect)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxFontAspectF,lb_p->label_aspect);
		if (lb_p->label_thickness != olb_p->label_thickness)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtxFontThicknessF,lb_p->label_thickness);
		if (lb_p->label_const_spacing != olb_p->label_const_spacing)
			NhlSetSArg(&sargs[nargs++],NhlNtxConstantSpacingF,
				   lb_p->label_const_spacing);
		if (lb_p->label_func_code != olb_p->label_func_code)
			NhlSetSArg(&sargs[nargs++],NhlNtxFuncCode,
				   lb_p->label_func_code);
		if (lb_p->label_quality != olb_p->label_quality)
			NhlSetSArg(&sargs[nargs++],NhlNtxFontQuality,
				   lb_p->label_quality);
		if (lb_p->label_auto_stride != olb_p->label_auto_stride)
			NhlSetSArg(&sargs[nargs++],NhlNMtextAutoStride,
				   lb_p->label_auto_stride);
		if (lb_p->label_alignment != olb_p->label_alignment) {
			keep_end_items = 
				lb_p->label_alignment == NhlEXTERNALEDGES ?
				True : False;
			NhlSetSArg(&sargs[nargs++],NhlNMtextKeepEndItems,
				   keep_end_items);
		}
		subret = NhlALSetValues(lb_p->labels_id,sargs,nargs);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error setting MultiText object values";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	if (lb_p->auto_manage || lb_p->label_height <= 0.0) {
		subret = AdjustLabels(lb_p,olb_p,
				      label_height,avail_char_space,
				      larea.x,larea.y,entry_name);
		ret = MIN(ret, subret);
	}

	nargs = 0;
	if (label_locs_changed) {
		NhlSetSArg(&sargs[nargs++],
			   NhlNMtextPosArray,lb_p->label_locs);
	}
	if (lb_p->const_pos != olb_p->const_pos)
		NhlSetSArg(&sargs[nargs++],
			   NhlNMtextConstPosF,lb_p->const_pos);
	if (lb_p->label_just != olb_p->label_just)
		NhlSetSArg(&sargs[nargs++],
			   NhlNtxJust,lb_p->label_just);
	if (lb_p->label_height != olb_p->label_height)
		NhlSetSArg(&sargs[nargs++],NhlNtxFontHeightF,
			   lb_p->label_height);
	if (lb_p->label_color != olb_p->label_color)
		NhlSetSArg(&sargs[nargs++],
			   NhlNtxFontColor,lb_p->label_color);

	subret = NhlALSetValues(lb_p->labels_id,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: Error setting MultiText object values";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}


	subret = NhlVAGetValues(lb_p->labels_id,
				NhlNvpXF,&x,
				NhlNvpYF,&y,
				NhlNvpWidthF,&width,
				NhlNvpHeightF,&height,
				NhlNMtextConstPosF,&lb_p->const_pos,
				NhlNMtextMaxLenF,&lb_p->max_label_len,
				NULL);
/*
 * store the actual label bounding box in the 'xtr' variables.
 */
	lb_p->labels.lxtr = x;
	lb_p->labels.rxtr = x + width;
	lb_p->labels.bxtr = y - height;
	lb_p->labels.txtr = y;

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

#define NhlLB_TRANSITIONANGLE 7.5
static NhlErrorTypes   	AdjustLabels
#if	NhlNeedProto
(
	NhlLabelBarLayerPart *lb_p,
	NhlLabelBarLayerPart *olb_p,
 	float		height,            /* not using this now */
	float		avail_space,	   /* or this */
	float		area_x,
	float		area_y,
	char		*entry_name
)
#else
(lb_p,olb_p,height,avail_space,area_x area_y,entry_name)
	NhlLabelBarLayerPart *lb_p;
	NhlLabelBarLayerPart *olb_p,
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

	angle = (lb_p->label_angle < 0.0) ? 
		lb_p->label_angle + 360.0 : lb_p->label_angle;
/*
 * Get the width and height and the maximum text length of the multitext.
 */
	subret = NhlVAGetValues(lb_p->labels_id,
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

	if (lb_p->orient == NhlHORIZONTAL) {
		box_major = area_x / lb_p->label_draw_count;
		box_minor = area_y;

		if (lb_p->label_direction == NhlACROSS &&
		    fabs(fmod(lb_p->label_angle,180.)) 
		    < NhlLB_TRANSITIONANGLE) {
			box_major -= height;
			space_fac = 0.1;
		}
	}
	else {
		box_major = area_y / lb_p->label_draw_count;
		box_minor = area_x;
		if (lb_p->label_direction == NhlDOWN &&
		    fabs(fmod(lb_p->label_angle,180.)) 
		    < NhlLB_TRANSITIONANGLE) {
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
		

	if (lb_p->label_direction == NhlDOWN)
		test_angle = 90.0 - test_angle;
	if (lb_p->orient == NhlHORIZONTAL)
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
	lb_p->label_height = height;

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

	if (lb_p->label_pos == NhlCENTER) {
		lb_p->label_just = NhlCENTERCENTER;
	}
	else if (lb_p->orient == NhlHORIZONTAL && 
	    lb_p->label_direction == NhlACROSS) {
		if (theta1 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (theta3 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
		else if (angle < 180.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
	}
	else if (lb_p->orient == NhlHORIZONTAL){ /* NhlDOWN or UP */
		if (theta2 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta4 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (angle > 270.0 || angle < 90.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
			 
	}
	else if (lb_p->orient == NhlVERTICAL && 
		 lb_p->label_direction == NhlACROSS) {
		if (theta2 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
		else if (theta4 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (angle > 270.0  || angle < 90.0) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
			 
	}
	else { /* NhlVERTICAL NhlDOWN or UP */

		if (theta1 <=NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta3 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (angle < 180.0) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
		else {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
	}
	return (ret);
}

/*
 * Function: AdjustGeometry
 *
 * Description: Called after all individual elements of the LabelBar have been
 *	sized in order to ensure that no elements overlap and that the 
 *	overall size of the LabelBar is big enough to accommodate the specified
 *	size of each element in the LabelBar. When auto-manage mode is turned
 *     	off both the width and height of the LabelBar may be expanded; when 
 *	auto-manage is on, only one direction (the major axis) may expand.
 *	Elements of the LabelBar may be moved relative to each other in order
 *	to eliminate overlap. Finally the routine adjusts the LabelBar position
 *	as a whole depending on the setting of the LabelBar justification
 *	resource.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The bounding box of the LabelBar may be modified, and the
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
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	NhlBoundingBox labelbarBB;
	NhlBoundingBox tmpBB,bar_and_labelsBB;
	float title_x = lb_p->title_x;
	float title_y = lb_p->title_y;
	float pos_offset = 0.0;
	float obj_offset = 0.0;
	float x_off, y_off;
	float minor_off;
	float major_off;
	float center_off;
	float p_width,p_height,ex_width,ex_height;
	int i;

	bar_and_labelsBB.b = lb_p->adj_perim.b;
	bar_and_labelsBB.t = lb_p->adj_perim.t;
	bar_and_labelsBB.l = lb_p->adj_perim.l;
	bar_and_labelsBB.r = lb_p->adj_perim.r;
	if (lb_p->title_on && lb_p->title_ext > 0.0) {
		switch (lb_p->title_pos) {
		case NhlTOP:
			bar_and_labelsBB.t = 
				lb_p->title.b - lb_p->title_off_ndc;
			break;
		case NhlBOTTOM:
			bar_and_labelsBB.b = 
				lb_p->title.t + lb_p->title_off_ndc;
			break;
		case NhlLEFT:
			bar_and_labelsBB.l = 
				lb_p->title.r + lb_p->title_off_ndc;
			break;
		case NhlRIGHT:
			bar_and_labelsBB.r = 
				lb_p->title.l - lb_p->title_off_ndc;
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
	if (! lb_p->labels_on) {
		tmpBB.l = lb_p->adj_bar.l;
		tmpBB.r = lb_p->adj_bar.r;
		tmpBB.b = lb_p->adj_bar.b;
		tmpBB.t = lb_p->adj_bar.t;
		lb_p->max_label_len = 0.0;
		lb_p->min_label_spacing = 0.0;
	}
	else {
		if (lb_p->orient == NhlHORIZONTAL) {
			if (lb_p->label_pos == NhlTOP) {
				pos_offset = lb_p->adj_bar.t + 
				       lb_p->label_off_ndc - lb_p->labels.bxtr;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				pos_offset = lb_p->adj_bar.b - 
				       lb_p->label_off_ndc - lb_p->labels.txtr;
			}
			lb_p->labels.bxtr += pos_offset;
			lb_p->labels.txtr += pos_offset;
		}
		else {
			if (lb_p->label_pos == NhlRIGHT) {
				pos_offset = lb_p->adj_bar.r + 
				       lb_p->label_off_ndc - lb_p->labels.lxtr;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				pos_offset = lb_p->adj_bar.l - 
				       lb_p->label_off_ndc - lb_p->labels.rxtr;
			}
			lb_p->labels.lxtr += pos_offset;
			lb_p->labels.rxtr += pos_offset;
		}
		
		tmpBB.l = MIN(lb_p->labels.lxtr, lb_p->adj_bar.l);
		tmpBB.r = MAX(lb_p->labels.rxtr, lb_p->adj_bar.r);
		tmpBB.b = MIN(lb_p->labels.bxtr, lb_p->adj_bar.b);
		tmpBB.t = MAX(lb_p->labels.txtr, lb_p->adj_bar.t);
	}
	labelbarBB.l = MIN(tmpBB.l, bar_and_labelsBB.l);
	labelbarBB.r = MAX(tmpBB.r, bar_and_labelsBB.r);
	labelbarBB.b = MIN(tmpBB.b, bar_and_labelsBB.b);
	labelbarBB.t = MAX(tmpBB.t, bar_and_labelsBB.t);

	if (lb_p->orient == NhlHORIZONTAL) {
		center_off = (labelbarBB.t - tmpBB.t + 
			      labelbarBB.b - tmpBB.b) / 2.0;
		lb_p->labels.bxtr += center_off;
		lb_p->labels.txtr += center_off;
		lb_p->adj_bar.b += center_off;
		lb_p->adj_bar.t += center_off;
		pos_offset += center_off;
	}
	else {
		center_off = (labelbarBB.r - tmpBB.r +
			      labelbarBB.l - tmpBB.l) / 2.0;
		lb_p->labels.lxtr += center_off;
		lb_p->labels.rxtr += center_off;
		lb_p->adj_bar.l += center_off;
		lb_p->adj_bar.r += center_off;
		pos_offset += center_off;
	}		

		
/*
 * Adjust for the title: 
 * first move it out of the way of the legend, 
 * then adjust for possibly larger justification area.
 */

	if (lb_p->title_on) {
		if (lb_p->title_pos == NhlBOTTOM &&
		    lb_p->title.txtr > labelbarBB.b - lb_p->title_off_ndc) {
			title_y -= lb_p->title.txtr + 
				lb_p->title_off_ndc - labelbarBB.b;
		}
		else if (lb_p->title_pos == NhlTOP && lb_p->title.bxtr < 
			 labelbarBB.t + lb_p->title_off_ndc) {
			title_y += labelbarBB.t + 
				lb_p->title_off_ndc - lb_p->title.bxtr;
		}
		else if (lb_p->title_pos == NhlLEFT && lb_p->title.rxtr > 
			 labelbarBB.l - lb_p->title_off_ndc) {
			title_x -= lb_p->title.rxtr + 
				lb_p->title_off_ndc - labelbarBB.l;
		}
		else if (lb_p->title_pos == NhlRIGHT &&
			 lb_p->title.lxtr < labelbarBB.r + obj_offset) {
			title_x += labelbarBB.r + 
				lb_p->title_off_ndc - lb_p->title.lxtr;
		}
		
		if (lb_p->title_pos == NhlTOP || 
		    lb_p->title_pos == NhlBOTTOM) {
			switch (lb_p->title_just) {
			case NhlBOTTOMLEFT:
			case NhlCENTERLEFT:
			case NhlTOPLEFT:
				title_x = labelbarBB.l;
				break;
			case NhlBOTTOMCENTER:
			case NhlCENTERCENTER:
			case NhlTOPCENTER:
			default:
				title_x = labelbarBB.l + 
					(labelbarBB.r - labelbarBB.l) / 2.0;
				break;
			case NhlBOTTOMRIGHT:
			case NhlCENTERRIGHT:
			case NhlTOPRIGHT:
				title_x = labelbarBB.r;
				break;
			}
		}
		else if (lb_p->title_pos == NhlLEFT || 
			 lb_p->title_pos == NhlRIGHT) {
			switch (lb_p->title_just) {
			case NhlBOTTOMLEFT:
			case NhlBOTTOMCENTER:
			case NhlBOTTOMRIGHT:
				title_y = labelbarBB.b;
				break;
			case NhlCENTERLEFT:
			case NhlCENTERCENTER:
			case NhlCENTERRIGHT:
			default:
				title_y = labelbarBB.b + 
					(labelbarBB.t - labelbarBB.b) / 2.0;
				break;
			case NhlTOPLEFT:
			case NhlTOPCENTER:
			case NhlTOPRIGHT:
				title_y = labelbarBB.t;
				break;
			}
		}
		lb_p->title.lxtr += title_x - lb_p->title_x;
		lb_p->title.rxtr += title_x - lb_p->title_x;
		lb_p->title.bxtr += title_y - lb_p->title_y;
		lb_p->title.txtr += title_y - lb_p->title_y;

/*
 * now get the real bounding box of the labelbar
 */
		labelbarBB.l = MIN(labelbarBB.l,lb_p->title.lxtr);
		labelbarBB.r = MAX(labelbarBB.r,lb_p->title.rxtr);
		labelbarBB.b = MIN(labelbarBB.b,lb_p->title.bxtr);
		labelbarBB.t = MAX(labelbarBB.t,lb_p->title.txtr);
	}

/*
 * Determine the external perimeter
 */
	lb_p->perim.lxtr = labelbarBB.l - lb_p->margin.l * lb_p->small_axis;
	lb_p->perim.rxtr = labelbarBB.r + lb_p->margin.r * lb_p->small_axis;
	lb_p->perim.bxtr = labelbarBB.b - lb_p->margin.b * lb_p->small_axis;
	lb_p->perim.txtr = labelbarBB.t + lb_p->margin.t * lb_p->small_axis;
/*
 * Adjust position based on the justification
 */
	switch (lb_p->just) {
	case NhlBOTTOMLEFT:
		x_off = lb_p->perim.lxtr - lb_p->lb_x;
		y_off = lb_p->perim.bxtr - (lb_p->lb_y - lb_p->lb_height);
		break;
	case NhlCENTERLEFT:
		x_off = lb_p->perim.lxtr - lb_p->perim.l;
		y_off = lb_p->perim.bxtr + 
			(lb_p->perim.txtr - lb_p->perim.bxtr)/2.0 -
				(lb_p->lb_y - lb_p->lb_height/2.0);
		break;
	case NhlTOPLEFT:
		x_off = lb_p->perim.lxtr - lb_p->perim.l;
		y_off = lb_p->perim.txtr - lb_p->perim.t;
		break;
	case NhlBOTTOMCENTER:
		x_off = lb_p->perim.lxtr + 
			(lb_p->perim.rxtr - lb_p->perim.lxtr)/2.0 -
				(lb_p->lb_x + lb_p->lb_width/2.0);
		y_off = lb_p->perim.bxtr - lb_p->perim.b;
		break;
	case NhlCENTERCENTER:
	default:
		x_off = lb_p->perim.lxtr + 
			(lb_p->perim.rxtr - lb_p->perim.lxtr)/2.0 -
				(lb_p->lb_x + lb_p->lb_width/2.0);
		y_off = lb_p->perim.bxtr + 
			(lb_p->perim.txtr - lb_p->perim.bxtr)/2.0 -
				(lb_p->lb_y - lb_p->lb_height/2.0);
		break;
	case NhlTOPCENTER:
		x_off = lb_p->perim.lxtr + 
			(lb_p->perim.rxtr - lb_p->perim.lxtr)/2.0 -
				(lb_p->lb_x + lb_p->lb_width/2.0);
		y_off = lb_p->perim.txtr - lb_p->perim.t;
		break;
	case NhlBOTTOMRIGHT:
		x_off = lb_p->perim.rxtr - lb_p->perim.r;
		y_off = lb_p->perim.bxtr - lb_p->perim.b;
		break;
	case NhlCENTERRIGHT:
		x_off = lb_p->perim.rxtr - lb_p->perim.r;
		y_off = lb_p->perim.bxtr + 
			(lb_p->perim.txtr - lb_p->perim.bxtr)/2.0 -
				(lb_p->lb_y - lb_p->lb_height/2.0);
		break;
	case NhlTOPRIGHT:
		x_off = lb_p->perim.rxtr - lb_p->perim.r;
		y_off = lb_p->perim.txtr - lb_p->perim.t;
		break;
	}

	minor_off = (lb_p->orient == NhlHORIZONTAL) ? y_off : x_off;
	major_off = (lb_p->orient == NhlHORIZONTAL) ? x_off : y_off;
/*
 * Adjust the external perimeter
 */
	lb_p->perim.lxtr -= x_off;
	lb_p->perim.rxtr -= x_off;
	lb_p->perim.bxtr -= y_off;
	lb_p->perim.txtr -= y_off;

/*
 * Adjust the nominal perimeter
 */

	p_width = lb_p->perim.r - lb_p->perim.l;
	p_height = lb_p->perim.t - lb_p->perim.b;
	ex_width = MAX(0.0, 
		       (lb_p->perim.rxtr - lb_p->perim.lxtr) - p_width);
	ex_height = MAX(0.0, 
			(lb_p->perim.txtr - lb_p->perim.bxtr) - p_height);

	switch (lb_p->just) {
	case NhlBOTTOMLEFT:
		lb_p->perim.l = lb_p->perim.lxtr;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	case NhlCENTERLEFT:
		lb_p->perim.l = lb_p->perim.lxtr;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr + ex_height / 2.0;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	case NhlTOPLEFT:
		lb_p->perim.l = lb_p->perim.lxtr;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr + ex_height;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	case NhlBOTTOMCENTER:
		lb_p->perim.l = lb_p->perim.lxtr + ex_width / 2.0;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	case NhlCENTERCENTER:
	default:
		lb_p->perim.l = lb_p->perim.lxtr + ex_width / 2.0;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr + ex_height / 2.0;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	case NhlTOPCENTER:
		lb_p->perim.l = lb_p->perim.lxtr + ex_width / 2.0;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr + ex_height;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	case NhlBOTTOMRIGHT:
		lb_p->perim.l = lb_p->perim.lxtr + ex_width;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	case NhlCENTERRIGHT:
		lb_p->perim.l = lb_p->perim.lxtr + ex_width;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr + ex_width / 2.0;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	case NhlTOPRIGHT:
		lb_p->perim.l = lb_p->perim.lxtr + ex_width;
		lb_p->perim.r = lb_p->perim.l + p_width;
		lb_p->perim.b = lb_p->perim.bxtr + ex_height;
		lb_p->perim.t = lb_p->perim.b + p_height;
		break;
	}
	
/*
 * Adjust the bar position
 */

	lb_p->adj_bar.l -= x_off;
	lb_p->adj_bar.r -= x_off;
	lb_p->adj_bar.b -= y_off;
	lb_p->adj_bar.t -= y_off;
	for (i=0; i<lb_p->box_count+1; i++) {
		lb_p->box_locs[i] -= major_off;
	}

/*
 * Adjust the labels position
 */

	if (lb_p->labels_on) {
		NhlSArg	sargs[2];
		int	nargs = 0;

		if (_NhlCmpFAny(0.0,major_off,6)) {
			for (i=0; i<lb_p->label_draw_count; i++) {
				lb_p->label_locs[i] -= major_off;
			}
			NhlSetSArg(&sargs[nargs++],
				   NhlNMtextPosArray,lb_p->label_locs);
		}
		if (_NhlCmpFAny(0.0,pos_offset-minor_off,6)) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNMtextConstPosF,
				   lb_p->const_pos + pos_offset - minor_off);
		}
			
		if ((ret1 = NhlALSetValues(lb_p->labels_id,sargs,nargs))
		    < NhlWARNING)
			return (ret1);
		ret = MIN(ret1,ret);

		lb_p->min_label_spacing = (lb_p->orient == NhlHORIZONTAL) ?
			lb_p->labels.rxtr - lb_p->labels.lxtr :
			lb_p->labels.txtr - lb_p->labels.bxtr;
		for (i=1; i<lb_p->label_draw_count; i++) {
			lb_p->min_label_spacing = MIN(lb_p->min_label_spacing,
						      lb_p->label_locs[i] - 
						      lb_p->label_locs[i-1]);
		}
	}
/*
 * Set the title position
 */

	if (lb_p->title_on && lb_p->title_ext > 0.0) {
		title_x -= x_off;
		title_y -= y_off;
		lb_p->title_x = title_x;
		lb_p->title_y = title_y;
		if ((ret1 = NhlVASetValues(lb_p->title_id,
					 NhlNtxPosXF, title_x,
					 NhlNtxPosYF, title_y,
					 NULL)) < NhlWARNING)
			return (ret1);
		ret = MIN(ret1,ret);
	}

	lb_p->lb_x = lb_p->perim.lxtr;
	lb_p->lb_y = lb_p->perim.txtr;
	lb_p->lb_width = lb_p->perim.rxtr - lb_p->perim.lxtr;
	lb_p->lb_height = lb_p->perim.txtr - lb_p->perim.bxtr;

	_NhlInternalSetView((NhlViewLayer)tnew,
			    lb_p->lb_x,lb_p->lb_y,
			    lb_p->lb_width,lb_p->lb_height,
			    tnew->view.keep_aspect);
	return (ret);
}

/*
 * Function:	LabelBarGetValues
 *
 * Description:	Retrieves the current setting of one or more LabelBar
 *	resources. This routine only retrieves resources that require 
 *	special methods that the generic GetValues method cannot handle. 
 *	For now this means the GenArray resources. Note that space is 
 *	allocated; the user is responsible for freeing this space.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 *	Memory is allocated when the following resources are retrieved:
 *		NhlNlbFillPatterns
 *		NhlNlbFillColors
 *		NhlNlbFillScales
 *		NhlNlbLabelStrings
 *		NhlNlbBoxFractions
 *              NhlNlbTitleString
 *	The caller is responsible for freeing this memory.
 */

static NhlErrorTypes	LabelBarGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	NhlLayer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	NhlLabelBarLayer lbl = (NhlLabelBarLayer)l;
	NhlLabelBarLayerPart *lb_p = &(lbl->labelbar);
	NhlGenArray ga;
	char *e_text;
	int i, count = 0;
	char *type = "";
	
	for( i = 0; i< num_args; i++ ) {

		ga = NULL;
		if(args[i].quark == Qfill_patterns) {
			ga = lb_p->fill_patterns;
			count = lb_p->box_count;
			type = NhlNlbFillPatterns;
		}
		else if (args[i].quark == Qfill_colors) {
			ga = lb_p->fill_colors;
			count = lb_p->box_count;
			type = NhlNlbFillColors;
		}
		else if (args[i].quark == Qfill_scales) {
			ga = lb_p->fill_scales;
			count = lb_p->box_count;
			type = NhlNlbFillScales; 
		}
		else if (args[i].quark == Qlabel_strings) {
			ga = lb_p->label_strings;
			count = lb_p->current_label_count;
			type = NhlNlbLabelStrings;
		}
		else if (args[i].quark == Qbox_fractions) {
			ga = lb_p->box_fractions;
			count = lb_p->box_count+1;
			type = NhlNlbBoxFractions;
		} 
		else if(args[i].quark == Qtitle_string) {
			if(lb_p->title_string != NULL) {
				(*(NhlString*)args[i].value.ptrval) = 
					(NhlString)
				     NhlMalloc(strlen(lb_p->title_string) +1);
				strcpy((*(NhlString*)args[i].value.ptrval),
				       lb_p->title_string);
			} else {
				(*(NhlString*)args[i].value.ptrval) = NULL;
			}
		}
		if (ga != NULL) {
			if ((ga = GenArraySubsetCopy(ga, count)) == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  "LabelBarGetValues",type);
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
 * Description:	Since the internal GenArrays maintained by the LabelBar object
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

static NhlErrorTypes    DrawFilledBoxes
#if	NhlNeedProto
(	
NhlLabelBarLayer lbl,
NhlBoolean edges_only
)
#else
(lbl,edges_only)
NhlLabelBarLayer lbl;
NhlBoolean edges_only;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlLabelBarLayerPart *lb_p = &(lbl->labelbar);
	int *colors, *patterns;
	float xpoints[5];
	float ypoints[5];
	int i;
	int fill_color, fill_pattern;
	float fill_scale, *fill_scales;
        float fill_opacity;
	float frac, dist;
        NhllbBoxEndCapStyle endcapStyle = lb_p->box_end_cap_style;
		
	frac = (1.0 - lb_p->box_major_ext) / 2.0;
	colors = (int *)lb_p->fill_colors->data;
	patterns = (int *)lb_p->fill_patterns->data;
	fill_scales = (float *) lb_p->fill_scales->data;

        fill_opacity = lb_p->override_fill_opacity ? 1.0 : lb_p->fill_opacity;
        
	if (lb_p->orient == NhlHORIZONTAL) {

		for (i=0; i<lb_p->box_count; i++) {
                    if (i == 0 && (endcapStyle == NhlTRIANGLEBOTHENDS || endcapStyle == NhlTRIANGLELOWEND)) {
                        ypoints[0] = (lb_p->adj_bar.t + lb_p->adj_bar.b) / 2.0; 
                        ypoints[1] = lb_p->adj_bar.b;
                        ypoints[2] = lb_p->adj_bar.t;
                        ypoints[3] = ypoints[0];
                        ypoints[4] = ypoints[0];                        
                    }
                    else if (i == (lb_p->box_count-1) && (endcapStyle == NhlTRIANGLEBOTHENDS || endcapStyle == NhlTRIANGLEHIGHEND)) {
                        ypoints[0] = lb_p->adj_bar.b;
                        ypoints[1] = (lb_p->adj_bar.b + lb_p->adj_bar.t) / 2.0;
                        ypoints[2] = ypoints[1];
                        ypoints[3] = lb_p->adj_bar.t;
                        ypoints[4] = lb_p->adj_bar.b;
                    }
                    else {
                        ypoints[0] = lb_p->adj_bar.b;
                        ypoints[1] = lb_p->adj_bar.b;
                        ypoints[2] = lb_p->adj_bar.t;
                        ypoints[3] = lb_p->adj_bar.t;
                        ypoints[4] = lb_p->adj_bar.b;
                    }

                    dist = lb_p->box_locs[i+1] - lb_p->box_locs[i];
                    xpoints[0] = lb_p->box_locs[i] + dist * frac;
                    xpoints[1] = lb_p->box_locs[i+1] - dist * frac;
                    xpoints[2] = xpoints[1];
                    xpoints[3] = xpoints[0];
                    xpoints[4] = xpoints[0];
			
                    if (edges_only) 
                        fill_color = NhlTRANSPARENT;
                    else if (lb_p->mono_fill_color)
                        fill_color = lb_p->fill_color;
                    else
                        fill_color = colors[i];

                    if (lb_p->mono_fill_pattern)
			fill_pattern = lb_p->fill_pattern;
                    else
                        fill_pattern = patterns[i];
			
                    if (lb_p->mono_fill_scale)
			fill_scale = lb_p->fill_scale;
                    else
                        fill_scale = fill_scales[i];

                    NhlVASetValues(lbl->base.wkptr->base.id,
				  _NhlNwkFillIndex, fill_pattern,
				  _NhlNwkFillColor, fill_color,
				  _NhlNwkFillScaleFactorF, fill_scale,
				  _NhlNwkFillDotSizeF, lb_p->fill_dot_size,
                                  _NhlNwkFillOpacityF, fill_opacity,
				   NULL);
			
                    _NhlSetFillInfo(lbl->base.wkptr, (NhlLayer) lbl);
                    _NhlWorkstationFill(lbl->base.wkptr, xpoints, ypoints, 5);
			
		}
	}
	else {
		for (i=0; i< lb_p->box_count; i++) {

                    if (i == 0 && (endcapStyle == NhlTRIANGLEBOTHENDS || endcapStyle == NhlTRIANGLELOWEND)) {
                        xpoints[0] = (lb_p->adj_bar.l + lb_p->adj_bar.r) / 2.0; 
                        xpoints[1] = xpoints[0];
                        xpoints[2] = lb_p->adj_bar.r;
                        xpoints[3] = lb_p->adj_bar.l;
                        xpoints[4] = xpoints[0];                        
                    }
                    else if (i == (lb_p->box_count-1) && (endcapStyle == NhlTRIANGLEBOTHENDS || endcapStyle == NhlTRIANGLEHIGHEND)) {
                        xpoints[0] = lb_p->adj_bar.l;
                        xpoints[1] = lb_p->adj_bar.r;
                        xpoints[2] = (lb_p->adj_bar.l + lb_p->adj_bar.r) / 2.0;
                        xpoints[3] = xpoints[2];
                        xpoints[4] = lb_p->adj_bar.l;
                    }
                    else {
                        xpoints[0] = lb_p->adj_bar.l;
                        xpoints[1] = lb_p->adj_bar.r;
                        xpoints[2] = lb_p->adj_bar.r;
                        xpoints[3] = lb_p->adj_bar.l;
                        xpoints[4] = lb_p->adj_bar.l;
                    }
                                        
                    dist = lb_p->box_locs[i+1] - lb_p->box_locs[i];
                    ypoints[0] = lb_p->box_locs[i] + dist * frac;
                    ypoints[1] = ypoints[0];
                    ypoints[2] = lb_p->box_locs[i+1] - dist * frac;
                    ypoints[3] = ypoints[2];
                    ypoints[4] = ypoints[0];
			
                    if (edges_only) 
			fill_color = NhlTRANSPARENT;
                    else if (lb_p->mono_fill_color)
			fill_color = lb_p->fill_color;
                    else
			fill_color = colors[i];

                    if (lb_p->mono_fill_pattern)
			fill_pattern = lb_p->fill_pattern;
                    else
			fill_pattern = patterns[i];
			
                    if (lb_p->mono_fill_scale)
			fill_scale = lb_p->fill_scale;
                    else
			fill_scale = fill_scales[i];

                    NhlVASetValues(lbl->base.wkptr->base.id,
				  _NhlNwkFillIndex, fill_pattern,
				  _NhlNwkFillColor, fill_color,
				  _NhlNwkFillScaleFactorF, fill_scale,
				  _NhlNwkFillDotSizeF, lb_p->fill_dot_size,
                                  _NhlNwkFillOpacityF, fill_opacity,
				  NULL);
			
                    _NhlSetFillInfo(lbl->base.wkptr, (NhlLayer) lbl);
                    _NhlWorkstationFill(lbl->base.wkptr, xpoints, ypoints, 5);
			
		}
	}
        
	return ret;
}

static NhlErrorTypes    DrawRasterBoxes
#if	NhlNeedProto
(	
NhlLabelBarLayer lbl
)
#else
(lbl)
NhlLabelBarLayer lbl;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlLabelBarLayerPart *lb_p = &(lbl->labelbar);
	int *colors;
	float xp1,yp1,xp2,yp2;
	int i;
	int nx,ny;
	int has_transparent = 0;
	
	colors = (int *)lb_p->fill_colors->data;
	/* 
	 * If any colors are Transparent, then raster mode
	 * cannot be used
	 */
	for (i = 0; i < lb_p->box_count; i++) {
		if (colors[i] >= 0) 
			continue;
		has_transparent = 1;
	}
        if (has_transparent || lb_p->box_sizing == NhlEXPLICITSIZING) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "LabelBarDraw: cannot use raster fill if explicit box sizing set or any color index is transparent: defaulting to normal fill");
		DrawFilledBoxes(lbl,False);
		return ret;
	}
	xp1 = lb_p->adj_bar.l;
	yp1 = lb_p->adj_bar.b;
	xp2 = lb_p->adj_bar.r;
	yp2 = lb_p->adj_bar.t;

	if (lb_p->orient == NhlHORIZONTAL) {
		nx = lb_p->box_count;
		ny = 1;
	}
	else {
		nx = 1;
		ny = lb_p->box_count;
	}
	_NhlWorkstationCellFill
		(lbl->base.wkptr,xp1,yp1,xp2,yp2,nx,ny,colors);

	DrawFilledBoxes(lbl,True);

	return ret;
}



/*
 * Function:	LabelBarDraw
 *
 * Description:  Activates parent workstation, then draws the LabelBar 
 *		 perimeter and LabelBar boxes. Deactivates the workstation
 *		 and calls the draw functions for the LabelBar title
 *		 text object and the LabelBar labels multitext object.
 *
 * In Args: the instance to be drawn
 *
 * Out Args: NONE
 *
 * Return Values: Error conditions
 *
 * Side Effects: 
 */

static NhlErrorTypes    LabelBarDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
	NhlLayer 	layer;
#endif
{
	NhlLabelBarLayer lbl = (NhlLabelBarLayer) layer;
	NhlLabelBarLayerPart *lb_p = &(lbl->labelbar);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *e_text;
	char *entry_name = "LabelBarDraw";
	float xpoints[5];
	float ypoints[5];
	int edges_on;

	if (! lb_p->labelbar_on)
		return(ret);

	if (lbl->view.use_segments && ! lb_p->new_draw_req &&
	    lb_p->trans_dat && lb_p->trans_dat->id != NgNOT_A_SEGMENT) {
                subret = _NhlActivateWorkstation(lbl->base.wkptr);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDrawSegment(lb_p->trans_dat,
				_NhlWorkstationId(lbl->base.wkptr));
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDeactivateWorkstation(lbl->base.wkptr);
		return MIN(subret,ret);
	}

	NhlVASetValues(lbl->base.wkptr->base.id,
		       _NhlNwkReset,	True,
		       NULL);

	lb_p->new_draw_req = False;

	subret = _NhlActivateWorkstation(lbl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	if (lbl->view.use_segments) {
		if (lb_p->trans_dat != NULL)
			_NhlDeleteViewSegment(layer, lb_p->trans_dat);
		if ((lb_p->trans_dat = _NhlNewViewSegment(layer)) == NULL) {
			e_text = "%s: error opening segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		subret = _NhlStartSegment(lb_p->trans_dat);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
	}

/* first draw the perimeter: it may have a solid background */

	if (lb_p->perim_on) {

		xpoints[0] = lb_p->perim.lxtr;
		ypoints[0] = lb_p->perim.bxtr;
		xpoints[1] = lb_p->perim.rxtr;
		ypoints[1] = lb_p->perim.bxtr;
		xpoints[2] = lb_p->perim.rxtr;
		ypoints[2] = lb_p->perim.txtr;
		xpoints[3] = lb_p->perim.lxtr;
		ypoints[3] = lb_p->perim.txtr;
		xpoints[4] = lb_p->perim.lxtr;
		ypoints[4] = lb_p->perim.bxtr;

		NhlVASetValues(lbl->base.wkptr->base.id,
			     _NhlNwkEdgesOn, 1,
			     _NhlNwkEdgeDashPattern, lb_p->perim_dash_pattern,
			     _NhlNwkEdgeThicknessF, lb_p->perim_thickness,
			     _NhlNwkEdgeDashSegLenF, lb_p->perim_dash_seglen,
			     _NhlNwkEdgeColor, lb_p->perim_color,
			     _NhlNwkFillColor, lb_p->perim_fill_color,
			     _NhlNwkFillIndex, lb_p->perim_fill,
			     NULL);
			
		_NhlSetFillInfo(lbl->base.wkptr, layer);
		_NhlWorkstationFill(lbl->base.wkptr,
				    xpoints,ypoints,5);
			
	}

/*
 * Set the values that remain constant for all boxes
 */

	edges_on = lb_p->box_lines_on && lb_p->box_separator_lines_on;
	NhlVASetValues(lbl->base.wkptr->base.id,
		     _NhlNwkEdgesOn, edges_on,
		     _NhlNwkEdgeDashPattern, lb_p->box_line_dash_pattern,
		     _NhlNwkEdgeThicknessF, lb_p->box_line_thickness,
		     _NhlNwkEdgeDashSegLenF, lb_p->box_line_dash_seglen,
		     _NhlNwkEdgeColor, lb_p->box_line_color,
		     _NhlNwkFillLineThicknessF, lb_p->fill_line_thickness,
		     _NhlNwkFillBackground, lb_p->fill_background,
		     NULL);
				     
/* 
 * Draw the boxes
 */
        /* NCL-2539:  We believe this code was introduced around 2005 to deal with a combination of the 
         * original ps/eps driver, and printers of the day, whereby colors drawn with cell-fill vs. polygon-fill
         * primitives had slightly difference appearances.  This code caused the labelbar boxes to be drawn as cell-fills
         * whenever the contours were drawn that way (i.e., in the case of cnFillMode = "RasterFill".
         *
         * However, DrawRasterBoxes() breaks the newly introduced triangular/rectangular labelbar endcap styles,
         * causing *both* endcap styles to be drawn if a triangular endcap is intended. Note there is no color differences
         * in drawing with cell-fill vs. polygon-fill with the cairo-based ps/eps drivers. So as this bit of 
         * conditional logic addresses issues that may no longer be relevant, I am commenting it out, invoking only
         * DrawFilledBoxes() to due the work, and this bit of code should likely be deleted at some future point.
         * --RLB 1/2017
         */
#if 0        
	if (lb_p->raster_fill_on) {
		DrawRasterBoxes(lbl);
	}
	else {
		DrawFilledBoxes(lbl,False);
	}
#endif
        DrawFilledBoxes(lbl, False);

	if (lbl->view.use_segments) {

		if (lb_p->title_on && lb_p->title_ext > 0.0)
			_NhlSegDraw(_NhlGetLayer(lb_p->title_id));
		
		if (lb_p->labels_on )
			_NhlSegDraw(_NhlGetLayer(lb_p->labels_id));

		_NhlEndSegment(lb_p->trans_dat);
		_NhlDeactivateWorkstation(lbl->base.wkptr);
	}
	else {
		_NhlDeactivateWorkstation(lbl->base.wkptr);

		if (lb_p->title_on && lb_p->title_ext > 0.0)
			NhlDraw(lb_p->title_id);
		
		if (lb_p->labels_on )
			NhlDraw(lb_p->labels_id);
	}

	if (lb_p->box_lines_on && ! lb_p->box_separator_lines_on) {
		int gsid;
		/* draw around the edge of all the boxes */
		xpoints[0] = lb_p->adj_bar.l;
		ypoints[0] = lb_p->adj_bar.b;
		xpoints[1] = lb_p->adj_bar.r;
		ypoints[1] = lb_p->adj_bar.b;
		xpoints[2] = lb_p->adj_bar.r;
		ypoints[2] = lb_p->adj_bar.t;
		xpoints[3] = lb_p->adj_bar.l;
		ypoints[3] = lb_p->adj_bar.t;
		xpoints[4] = lb_p->adj_bar.l;
		ypoints[4] = lb_p->adj_bar.b;
		NhlVAGetValues(lbl->base.wkptr->base.id,
			       NhlNwkDefGraphicStyleId,    &gsid,
			       NULL);
		NhlVASetValues(gsid,
			       NhlNgsLineDashPattern, lb_p->box_line_dash_pattern,
			       NhlNgsLineThicknessF, lb_p->box_line_thickness,
			       NhlNgsLineDashSegLenF, lb_p->box_line_dash_seglen,
			       NhlNgsLineColor, lb_p->box_line_color,
			       NhlNgsLineLabelString, "",
			       NULL);
		NhlNDCPolyline(lbl->base.wkptr->base.id,gsid,xpoints,ypoints,5);
	}		

	return(ret);
}

/*
 * Function:	LabelBarClassInitialize
 *
 * Description: Initializes the child classes, TextItem and MultiText
 *              Gets Quark values for the array data types.
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    LabelBarClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{
        _NhlEnumVals   labelalignmentlist[] = {
	{NhlBOXCENTERS, "BoxCenters"},
	{NhlINTERIOREDGES, "InteriorEdges"},
	{NhlEXTERNALEDGES, "ExternalEdges"}
        };

        _NhlEnumVals   boxsizinglist[] = {
        {NhlUNIFORMSIZING,	"UniformSizing"},
        {NhlEXPLICITSIZING,	"ExplicitSizing"},
        };
        
        _NhlEnumVals   boxendcapstylelist[] = {
            {NhlRECTANGLEENDS,     "RectangleEnds"},
            {NhlTRIANGLELOWEND,    "TriangleLowEnd"},
            {NhlTRIANGLEHIGHEND,   "TriangleHighEnd"},
            {NhlTRIANGLEBOTHENDS,  "TriangleBothEnds"},
        };

	_NhlRegisterEnumType(NhlviewClass,NhlTlbLabelAlignmentMode,
		labelalignmentlist,NhlNumber(labelalignmentlist));
	_NhlRegisterEnumType(NhllabelBarClass,NhlTlbBoxSizingMode,boxsizinglist,
			     NhlNumber(boxsizinglist));
	_NhlRegisterEnumType(NhllabelBarClass,NhlTlbBoxEndCapStyle,boxendcapstylelist,
			     NhlNumber(boxendcapstylelist));

	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qfill_patterns = NrmStringToQuark(NhlNlbFillPatterns);
	Qfill_colors = NrmStringToQuark(NhlNlbFillColors);
	Qfill_scales = NrmStringToQuark(NhlNlbFillScales);
	Qlabel_strings = NrmStringToQuark(NhlNlbLabelStrings);
	Qbox_fractions = NrmStringToQuark(NhlNlbBoxFractions);
	Qtitle_string = NrmStringToQuark(NhlNlbTitleString);

	_NhlInitializeClass(NhltextItemClass);
	_NhlInitializeClass(NhlmultiTextClass);

	return(NhlNOERROR);	
}

/*
 * Function:	LabelBarDestroy
 *
 * Description: Frees all dynamically allocated memory
 *
 * In Args:	NhlLayer inst	instance of LabelBar
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */

static NhlErrorTypes    LabelBarDestroy
#if	NhlNeedProto
(NhlLayer  inst)
#else
(inst)
	NhlLayer	inst;
#endif
{
	NhlLabelBarLayer tinst = (NhlLabelBarLayer) inst;
	NhlLabelBarLayerPart *lb_p = &(tinst->labelbar);
	
	if (lb_p->stride_labels != NULL)
		NhlFree(lb_p->stride_labels);
	if (lb_p->label_locs != NULL)
		NhlFree(lb_p->label_locs);
	if (lb_p->box_locs != NULL)
		NhlFree(lb_p->box_locs);

	NhlFreeGenArray(lb_p->label_strings);
	NhlFreeGenArray(lb_p->fill_colors);
	NhlFreeGenArray(lb_p->fill_patterns);
	NhlFreeGenArray(lb_p->fill_scales);
	NhlFreeGenArray(lb_p->box_fractions);

	if (lb_p->labels_id >=0)
		NhlDestroy(lb_p->labels_id);

        if (lb_p->title_on && lb_p->title_ext > 0.0 && 
	    lb_p->title_string != lbDefTitle) {
	  NhlFree(lb_p->title_string);
	}
	if (lb_p->title_id >=0)
		NhlDestroy(lb_p->title_id);

	if (lb_p->trans_dat != NULL)
		_NhlDeleteViewSegment(inst,lb_p->trans_dat);

	return(NhlNOERROR);
}
