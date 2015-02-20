/*
 *      $Id: TickMark.c,v 1.84.4.1 2008-03-28 20:37:37 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Dec 2 14:01:10 MST 1992
 *
 *	Description:	
 */

#include <math.h>
#include <limits.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/TickMarkP.h>
#include <ncarg/hlu/TransObjI.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/MultiText.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/TransformP.h>


/* resource list definition */

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNtmSciNoteCutoff, NhlCtmSciNoteCutoff, NhlTInteger,sizeof(int),
		  NhlOffset(NhlTickMarkLayerRec, tick.sci_note_cutoff),
		  NhlTImmediate,_NhlUSET((NhlPointer)6),0,NULL},
	{ NhlNtmEqualizeXYSizes, NhlCtmEqualizeXYSizes, 
	  NhlTBoolean, sizeof(NhlBoolean),
	  NhlOffset(NhlTickMarkLayerRec,tick.equalize_xy_sizes),
	  NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{ NhlNtmLabelAutoStride, NhlCLabelAutoStride, 
	  NhlTBoolean, sizeof(NhlBoolean),
	  NhlOffset(NhlTickMarkLayerRec,tick.label_auto_stride),
	  NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{ NhlNtmXUseBottom, NhlCtmXUseBottom, NhlTBoolean, sizeof(NhlBoolean),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_use_bottom),
		  NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtmXBOn, NhlCtmXBOn, NhlTBoolean, sizeof(NhlBoolean),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_b_on),
		  NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtmXTOn, NhlCtmXTOn, NhlTBoolean, sizeof(NhlBoolean),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_t_on),
		  NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtmXBLabelsOn, NhlCtmXBLabelsOn, NhlTBoolean, sizeof(NhlBoolean),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_b_labels_on),
		  NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmXTLabelsOn, NhlCtmXTLabelsOn, NhlTBoolean, sizeof(NhlBoolean),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_t_labels_on),
		  NhlTImmediate,_NhlUSET((NhlPointer)False ),0,NULL},
	{ NhlNtmXBBorderOn, NhlCEdgesOn, NhlTBoolean, sizeof(NhlBoolean),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_b_border_on),
		  NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmXTBorderOn, NhlCEdgesOn, NhlTBoolean, sizeof(NhlBoolean),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_t_border_on),
		  NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmXBMode,NhlCtmXBMode,NhlTTickMarkMode,sizeof(NhlTickMarkMode),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_b_mode),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlAUTOMATIC ),0,NULL},
	{ NhlNtmXTMode,NhlCtmXTMode,NhlTTickMarkMode,sizeof(NhlTickMarkMode),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_t_mode),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlAUTOMATIC ),0,NULL}, 
	{ NhlNtmXBStyle,NhlCtmXBStyle,NhlTTickMarkStyle,
		  sizeof(NhlTickMarkStyle),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_b_style),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlLINEAR ),0,NULL},
	{ NhlNtmXBIrrTensionF,NhlCtmXBIrrTensionF,NhlTFloat,sizeof(float),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_b_tension),
		  NhlTString,_NhlUSET("2.0"),0,NULL},

	{ NhlNtmXTStyle,NhlCtmXTStyle,NhlTTickMarkStyle,
		  sizeof(NhlTickMarkStyle),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_t_style),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlLINEAR ),0,NULL},
	{ NhlNtmXTIrrTensionF,NhlCtmXTIrrTensionF,NhlTFloat,sizeof(float),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_t_tension),
		  NhlTString,_NhlUSET( "2.0"),0,NULL},

	{ NhlNtmBorderThicknessF,NhlCEdgeThicknessF,NhlTFloat,
		  sizeof(float),
		  NhlOffset(NhlTickMarkLayerRec, tick.border_thickness),
		  NhlTString,_NhlUSET("2.0" ),0,NULL},
	{ NhlNtmBorderLineColor,NhlCEdgeColor,NhlTColorIndex,
		  sizeof(NhlColorIndex), 
		  NhlOffset(NhlTickMarkLayerRec,tick.border_line_color),
		  NhlTImmediate,_NhlUSET((NhlPointer)1),0,NULL},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_b_precision_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmXBPrecision, NhlCtmPrecisions, NhlTInteger,sizeof(int),
		  NhlOffset(NhlTickMarkLayerRec, tick.x_b_precision),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_t_precision_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmXTPrecision, NhlCtmPrecisions, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_precision),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmXBFormat, NhlCNumberFormat, NhlTString,sizeof(NhlString),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_format.fstring),
		NhlTString,_NhlUSET( NhltmDEF_FORMAT ),0,NULL},
	{ NhlNtmXTFormat, NhlCNumberFormat, NhlTString,sizeof(NhlString),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_format.fstring),
		NhlTString,_NhlUSET( NhltmDEF_FORMAT ),0,NULL},


	{ NhlNtmXMajorGrid, NhlCtmXMajorGrid, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_major_grid),
		NhlTImmediate,_NhlUSET( (NhlPointer)False ),0,NULL},
	{ NhlNtmXMinorGrid, NhlCtmXMinorGrid, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_minor_grid),
		NhlTImmediate,_NhlUSET( (NhlPointer)False ),0,NULL},
	{ NhlNtmXMajorGridThicknessF, NhlCtmMajorGridThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_major_grid_thickness),
		NhlTString,_NhlUSET( "2.0" ),0,NULL},
	{ NhlNtmXMajorGridLineColor, NhlCLineColor, NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec, tick.x_major_grid_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1 ),0,NULL},
	{ NhlNtmXMajorGridLineDashPattern, NhlCtmMajorGridLineDashPatterns,
		NhlTDashIndex,sizeof(NhlDashIndex),
		NhlOffset(NhlTickMarkLayerRec,
			  tick.x_major_grid_line_dash_pattern),
		NhlTImmediate,_NhlUSET( (NhlPointer)0 ),0,NULL},

	{ NhlNtmXMinorGridThicknessF, NhlCtmMinorGridThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_minor_grid_thickness),
		NhlTString,_NhlUSET( "1.0" ),0,NULL},
	{ NhlNtmXMinorGridLineColor, NhlCLineColor, NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec, tick.x_minor_grid_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1 ),0,NULL},
	{ NhlNtmXMinorGridLineDashPattern, NhlCtmMinorGridLineDashPatterns,
		NhlTDashIndex, sizeof(NhlDashIndex),
		NhlOffset(NhlTickMarkLayerRec,
			  tick.x_minor_grid_line_dash_pattern),
		NhlTImmediate,_NhlUSET( (NhlPointer)0 ),0,NULL},

        {"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_b_minor_per_major_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_PRIVATE,NULL},
	{ NhlNtmXBMinorPerMajor, NhlCtmXMinorPerMajor,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_per_major),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
        {"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_t_minor_per_major_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_PRIVATE,NULL},
	{ NhlNtmXTMinorPerMajor, NhlCtmXMinorPerMajor,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_per_major),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmXBMinorOn, NhlCtmXBMinorOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmXTMinorOn, NhlCtmXTMinorOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmXBLabelStride, NhlCtmXBLabelStride, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_stride),
		NhlTImmediate,_NhlUSET((NhlPointer)0 ),0,NULL},
	{ NhlNtmXTLabelStride, NhlCtmXTLabelStride, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_stride),
		NhlTImmediate,_NhlUSET((NhlPointer)0 ),0,NULL},

	{ NhlNtmXBDataLeftF, NhlCtmXBDataLeftF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_data_left),
		NhlTString,_NhlUSET("0.0" ),0,NULL},
	{ NhlNtmXBDataRightF, NhlCtmXBDataRightF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_data_right),
		NhlTString,_NhlUSET("1.0" ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.x_b_tick_start_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmXBTickStartF, NhlCtmXBTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_tick_start),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.x_b_tick_end_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmXBTickEndF, NhlCtmXBTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_tick_end),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmXBMaxTicks, NhlCtmXBMaxTicks, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_max_ticks),
		NhlTImmediate,_NhlUSET( (NhlPointer)7 ),0,NULL},
	{ NhlNtmXBTickSpacingF,NhlCtmXBTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_tick_spacing),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{ NhlNtmXBSpacingType, NhlCtmXBSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_spacing_type),
		NhlTImmediate,_NhlUSET( (NhlPointer)0 ),
		_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{ NhlNtmXBIrregularPoints, NhlCtmXBIrregularPoints,NhlTFloatGenArray,
		sizeof(NhlPointer),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_irregular_points),
		NhlTImmediate,_NhlUSET( NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmXBValues,NhlCtmXBValues,NhlTFloatGenArray,sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_values),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmXBMinorValues,NhlCtmXBMinorValues,NhlTFloatGenArray,sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_values),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmXBLabels, NhlCtmXBLabels, NhlTStringGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_labels),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmXBMajorThicknessF, NhlCtmMajorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_major_thickness),
		NhlTString,_NhlUSET( "2.0" ),0,NULL},
	{NhlNtmXBMajorLineColor,NhlCLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_major_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_b_major_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_major_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, 
			   tick.x_b_major_outward_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_major_outward_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmXBMinorThicknessF, NhlCtmMinorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_thickness),
		NhlTString,_NhlUSET( "1.0" ),0,NULL},
	{NhlNtmXBMinorLineColor,NhlCLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_b_minor_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBMinorLengthF, NhlCtmMinorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, 
			   tick.x_b_minor_outward_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBMinorOutwardLengthF, NhlCtmMinorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_minor_outward_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtmXBLabelFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_label_font),
		NhlTImmediate,_NhlUSET( (NhlPointer)21 ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_font_height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXBLabelFontHeightF, NhlCFontHeightF, NhlTFloat, 
		 sizeof(float),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_b_label_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtmXBLabelFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_font_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1 ),0,NULL},
	{NhlNtmXBLabelFontAspectF, NhlCFontAspectF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_font_aspect),
		NhlTString,_NhlUSET( "1.3125" ),0,NULL},
	{NhlNtmXBLabelJust,NhlCtmXBLabelJust,NhlTJustification,
	 	sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_just),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlTOPCENTER),0,NULL},
	{NhlNtmXBLabelAngleF, NhlCTextAngleF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_angle),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{NhlNtmXBLabelDirection, NhlCTextDirection, NhlTTextDirection,
		sizeof(NhlTextDirection),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_direction),
		NhlTImmediate,_NhlUSET( (NhlPointer) NhlACROSS ),0,NULL},
	{ NhlNtmXBLabelFuncCode, NhlCTextFuncCode, NhlTCharacter, 
		  sizeof(char),
		  NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_fcode),
		  NhlTString,_NhlUSET("~"),0,NULL},
	{NhlNtmXBLabelFontThicknessF,NhlCFontThicknessF,NhlTFloat,
		 sizeof(float),
		 NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_font_thickness),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNtmXBLabelFontQuality, NhlCFontQuality, NhlTFontQuality,
		 sizeof(NhlFontQuality),
		 NhlOffset(NhlTickMarkLayerRec,
			   tick.x_b_label_font_quality), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlHIGH ),0,NULL},
	{NhlNtmXBLabelConstantSpacingF, NhlCTextConstantSpacingF,NhlTFloat,
		 sizeof(float),
		 NhlOffset(NhlTickMarkLayerRec,
			   tick.x_b_label_constant_spacing),
		 NhlTString,_NhlUSET("0.0" ), 0,NULL},
	{ NhlNtmXBLabelDeltaF, NhlCtmXBLabelDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_delta),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{ NhlNtmXBAutoPrecision, NhlCtmAutoPrecision, NhlTBoolean,
		sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_auto_precision),
		NhlTImmediate,_NhlUSET( (NhlPointer)True ),0,NULL},
	{ NhlNtmXBMaxLabelLenF, NhlCtmXBMaxLabelLenF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_max_label_len),
		NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},
	{ NhlNtmXBMinLabelSpacingF, NhlCtmXBMinLabelSpacingF, 
	        NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_spacing),
		NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},
	

	{ NhlNtmXTDataLeftF, NhlCtmXTDataLeftF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_data_left),
		NhlTString,_NhlUSET("0.0" ),0,NULL},
	{ NhlNtmXTDataRightF, NhlCtmXTDataRightF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_data_right),
		NhlTString,_NhlUSET("1.0" ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.x_t_tick_start_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmXTTickStartF, NhlCtmXTTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_tick_start),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.x_t_tick_end_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmXTTickEndF, NhlCtmXTTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_tick_end),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmXTMaxTicks, NhlCtmXTMaxTicks, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_max_ticks),
		NhlTImmediate,_NhlUSET( (NhlPointer)7 ),0,NULL},
	{ NhlNtmXTTickSpacingF, NhlCtmXTTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_tick_spacing),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{ NhlNtmXTSpacingType, NhlCtmXTSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_spacing_type),
		NhlTImmediate,_NhlUSET( (NhlPointer)0 ),
		_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{ NhlNtmXTIrregularPoints, NhlCtmXTIrregularPoints,NhlTFloatGenArray,
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_irregular_points),
		NhlTImmediate,_NhlUSET( NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmXTMinorValues, NhlCtmXTMinorValues,
		  NhlTFloatGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_values),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmXTValues, NhlCtmXTValues,
		  NhlTFloatGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_values),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmXTLabels, NhlCtmXTLabels, NhlTStringGenArray,
		  sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_labels),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmXTMajorThicknessF, NhlCtmMajorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_major_thickness),
		NhlTString,_NhlUSET( "2.0" ),0,NULL},
	{NhlNtmXTMajorLineColor,NhlCLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_major_line_color),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_t_major_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_major_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, 
			   tick.x_t_major_outward_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_major_outward_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmXTMinorThicknessF, NhlCtmMinorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_thickness),
		NhlTString,_NhlUSET( "1.0" ),0,NULL},
	{NhlNtmXTMinorLineColor,NhlCLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.x_t_minor_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTMinorLengthF, NhlCtmMinorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, 
			   tick.x_t_minor_outward_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTMinorOutwardLengthF, NhlCtmMinorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_minor_outward_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtmXTLabelFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_label_font),
		NhlTImmediate,_NhlUSET( (NhlPointer)21 ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_font_height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmXTLabelFontHeightF, NhlCFontHeightF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_label_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtmXTLabelFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_font_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1 ),0,NULL},
	{NhlNtmXTLabelFontAspectF, NhlCFontAspectF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_font_aspect),
		NhlTString,_NhlUSET( "1.3125" ),0,NULL},
	{NhlNtmXTLabelJust,NhlCtmXTLabelJust,NhlTJustification,
	 	sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_just),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlBOTTOMCENTER),0,NULL},
	{NhlNtmXTLabelAngleF, NhlCTextAngleF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_angle),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{NhlNtmXTLabelDirection, NhlCTextDirection, NhlTTextDirection,
		sizeof(NhlTextDirection),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_direction),
		NhlTImmediate,_NhlUSET( (NhlPointer) NhlACROSS ),0,NULL},
	{ NhlNtmXTLabelFuncCode, NhlCTextFuncCode, NhlTCharacter, 
		  sizeof(char),
		  NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_fcode),
		  NhlTString,_NhlUSET("~"),0,NULL},
	{NhlNtmXTLabelFontThicknessF,NhlCFontThicknessF,NhlTFloat,
		 sizeof(float),
		 NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_font_thickness),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNtmXTLabelFontQuality, NhlCFontQuality, NhlTFontQuality,
		 sizeof(NhlFontQuality),
		 NhlOffset(NhlTickMarkLayerRec,
			   tick.x_t_label_font_quality), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlHIGH ),0,NULL},
	{NhlNtmXTLabelConstantSpacingF, NhlCTextConstantSpacingF,NhlTFloat,
		 sizeof(float),
		 NhlOffset(NhlTickMarkLayerRec,
			   tick.x_t_label_constant_spacing),
		 NhlTString,_NhlUSET("0.0" ), 0,NULL},
	{ NhlNtmXTLabelDeltaF, NhlCtmXTLabelDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_delta),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{ NhlNtmXTAutoPrecision, NhlCtmAutoPrecision, NhlTBoolean,
		sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_auto_precision),
		NhlTImmediate,_NhlUSET( (NhlPointer)True ),0,NULL},
	{ NhlNtmXTMaxLabelLenF, NhlCtmXTMaxLabelLenF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_max_label_len),
		NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},
	{ NhlNtmXTMinLabelSpacingF, NhlCtmXTMinLabelSpacingF, 
	        NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_spacing),
		NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},

	{ NhlNtmYUseLeft, NhlCtmYUseLeft, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_use_left),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmYLOn, NhlCtmYLOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmYROn, NhlCtmYROn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmYLLabelsOn, NhlCtmYLLabelsOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_labels_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmYRLabelsOn, NhlCtmYRLabelsOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_labels_on),
		NhlTImmediate,_NhlUSET((NhlPointer)False ),0,NULL},
	{ NhlNtmYLBorderOn, NhlCEdgesOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_border_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmYRBorderOn, NhlCEdgesOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_border_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmYLMode,NhlCtmYLMode,NhlTTickMarkMode,sizeof(NhlTickMarkMode),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_mode),
		NhlTImmediate,_NhlUSET((NhlPointer) NhlAUTOMATIC ),0,NULL},
	{ NhlNtmYRMode,NhlCtmYRMode,NhlTTickMarkMode,sizeof(NhlTickMarkMode),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_mode),
		NhlTImmediate,_NhlUSET((NhlPointer) NhlAUTOMATIC ),0,NULL}, 
	{ NhlNtmYLStyle,NhlCtmYLStyle,NhlTTickMarkStyle,
		  sizeof(NhlTickMarkStyle),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_style),
		NhlTImmediate,_NhlUSET((NhlPointer) NhlLINEAR ),0,NULL},
	{ NhlNtmYLIrrTensionF,NhlCtmYLIrrTensionF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_tension),
		NhlTString,_NhlUSET( "2.0"),0,NULL},
	{ NhlNtmYRStyle,NhlCtmYRStyle,NhlTTickMarkStyle,
		  sizeof(NhlTickMarkStyle),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_style),
		NhlTImmediate,_NhlUSET((NhlPointer) NhlLINEAR ),0,NULL},
	{ NhlNtmYRIrrTensionF,NhlCtmYRIrrTensionF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_tension),
		NhlTString,_NhlUSET( "2.0"),0,NULL},


	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.y_l_precision_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmYLPrecision, NhlCtmPrecisions, NhlTInteger,sizeof(int),
		  NhlOffset(NhlTickMarkLayerRec, tick.y_l_precision),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.y_r_precision_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmYRPrecision, NhlCtmPrecisions, NhlTInteger,sizeof(int),
		  NhlOffset(NhlTickMarkLayerRec, tick.y_r_precision),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmYLFormat, NhlCNumberFormat, NhlTString,sizeof(NhlString),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_format.fstring),
		NhlTString,_NhlUSET( NhltmDEF_FORMAT ),0,NULL},
	{ NhlNtmYRFormat, NhlCNumberFormat, NhlTString,sizeof(NhlString),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_format.fstring),
		NhlTString,_NhlUSET( NhltmDEF_FORMAT ),0,NULL},

	{ NhlNtmYMajorGrid, NhlCtmXMajorGrid, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_major_grid),
		NhlTImmediate,_NhlUSET( (NhlPointer)False ),0,NULL},
	{ NhlNtmYMinorGrid, NhlCtmXMinorGrid, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_minor_grid),
		NhlTImmediate,_NhlUSET( (NhlPointer)False ),0,NULL},
	{ NhlNtmYMajorGridThicknessF, NhlCtmMajorGridThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_major_grid_thickness),
		NhlTString,_NhlUSET( "2.0" ),0,NULL},
	{ NhlNtmYMajorGridLineColor, NhlCLineColor, NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec, tick.y_major_grid_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1 ),0,NULL},
	{ NhlNtmYMajorGridLineDashPattern, NhlCtmMajorGridLineDashPatterns,
		NhlTDashIndex,sizeof(NhlDashIndex),
		NhlOffset(NhlTickMarkLayerRec,
			  tick.y_major_grid_line_dash_pattern),
		NhlTImmediate,_NhlUSET( (NhlPointer)0 ),0,NULL},

	{ NhlNtmYMinorGridThicknessF, NhlCtmMinorGridThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_minor_grid_thickness),
		NhlTString,_NhlUSET( "1.0" ),0,NULL},
	{ NhlNtmYMinorGridLineColor, NhlCLineColor, NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec, tick.y_minor_grid_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1 ),0,NULL},
	{ NhlNtmYMinorGridLineDashPattern, NhlCtmMinorGridLineDashPatterns,
		NhlTDashIndex, sizeof(NhlDashIndex),
		NhlOffset(NhlTickMarkLayerRec,
			  tick.y_minor_grid_line_dash_pattern),
		NhlTImmediate,_NhlUSET( (NhlPointer)0 ),0,NULL},

        {"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.y_l_minor_per_major_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_PRIVATE,NULL},
	{ NhlNtmYLMinorPerMajor, NhlCtmYMinorPerMajor,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_per_major),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
        {"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.y_r_minor_per_major_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_PRIVATE,NULL},
	{ NhlNtmYRMinorPerMajor, NhlCtmYMinorPerMajor,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_per_major),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmYLMinorOn, NhlCtmYLMinorOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmYRMinorOn, NhlCtmYRMinorOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_on),
		NhlTImmediate,_NhlUSET((NhlPointer)True ),0,NULL},
	{ NhlNtmYLLabelStride, NhlCtmYLLabelStride, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_stride),
		NhlTImmediate,_NhlUSET((NhlPointer)0 ),0,NULL},
	{ NhlNtmYRLabelStride, NhlCtmYRLabelStride, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_stride),
		NhlTImmediate,_NhlUSET((NhlPointer)0 ),0,NULL},

	{ NhlNtmYLDataTopF, NhlCtmYLDataTopF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_data_top),
		NhlTString,_NhlUSET("1.0" ),0,NULL},
	{ NhlNtmYLDataBottomF, NhlCtmYLDataBottomF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_data_bottom),
		NhlTString,_NhlUSET("0.0" ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.y_l_tick_start_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmYLTickStartF, NhlCtmYLTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_tick_start),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.y_l_tick_end_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmYLTickEndF, NhlCtmYLTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_tick_end),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmYLMaxTicks, NhlCtmYLMaxTicks, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_max_ticks),
		NhlTImmediate,_NhlUSET( (NhlPointer)7 ),0,NULL},
	{ NhlNtmYLTickSpacingF, NhlCtmYLTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_tick_spacing),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{ NhlNtmYLSpacingType, NhlCtmYLSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_spacing_type),
		NhlTImmediate,_NhlUSET( (NhlPointer)0 ),
		_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{ NhlNtmYLIrregularPoints, NhlCtmYLIrregularPoints,NhlTFloatGenArray,
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_irregular_points),
		NhlTImmediate,_NhlUSET( NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmYLMinorValues, NhlCtmYLMinorValues, NhlTFloatGenArray, 
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_values),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmYLValues, NhlCtmYLValues, NhlTFloatGenArray, 
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_values),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmYLLabels, NhlCtmYLLabels, NhlTStringGenArray, 
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_labels),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmYLMajorThicknessF, NhlCtmMajorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_major_thickness),
		NhlTString,_NhlUSET( "2.0" ),0,NULL},
	{NhlNtmYLMajorLineColor,NhlCLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_major_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1),0,NULL},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.y_l_major_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_major_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, 
			   tick.y_l_major_outward_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_major_outward_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmYLMinorThicknessF, NhlCtmMinorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_thickness),
		NhlTString,_NhlUSET( "1.0" ),0,NULL},
	{NhlNtmYLMinorLineColor,NhlCLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.y_l_minor_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLMinorLengthF, NhlCtmMinorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, 
			   tick.y_l_minor_outward_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLMinorOutwardLengthF, NhlCtmMinorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_minor_outward_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtmYLLabelFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_label_font),
		NhlTImmediate,_NhlUSET( (NhlPointer)21 ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_font_height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYLLabelFontHeightF, NhlCFontHeightF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_label_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtmYLLabelFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_font_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1 ),0,NULL},
	{NhlNtmYLLabelFontAspectF, NhlCFontAspectF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_font_aspect),
		NhlTString,_NhlUSET( "1.3125" ),0,NULL},
	{NhlNtmYLLabelJust,NhlCtmYLLabelJust,NhlTJustification,
	 	sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_just),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERRIGHT),0,NULL},
	{NhlNtmYLLabelAngleF, NhlCYAxisTextAngleF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_angle),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{NhlNtmYLLabelDirection, NhlCYAxisTextDirection, NhlTTextDirection,
		sizeof(NhlTextDirection),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_direction),
		NhlTImmediate,_NhlUSET( (NhlPointer) NhlACROSS ),0,NULL},
	{ NhlNtmYLLabelFuncCode, NhlCTextFuncCode, NhlTCharacter, 
		  sizeof(char),
		  NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_fcode),
		  NhlTString,_NhlUSET("~"),0,NULL},
	{NhlNtmYLLabelFontThicknessF,NhlCFontThicknessF,NhlTFloat,
		 sizeof(float), 
		 NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_font_thickness),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNtmYLLabelFontQuality, NhlCFontQuality, NhlTFontQuality,
		 sizeof(NhlFontQuality),
		 NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_font_quality),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlHIGH ),0,NULL},
	{NhlNtmYLLabelConstantSpacingF, NhlCTextConstantSpacingF,NhlTFloat,
		 sizeof(float), 
		 NhlOffset(NhlTickMarkLayerRec,
			   tick.y_l_label_constant_spacing),
		 NhlTString,_NhlUSET("0.0" ), 0,NULL},
	{ NhlNtmYLLabelDeltaF, NhlCtmYLLabelDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_delta),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{ NhlNtmYLAutoPrecision, NhlCtmAutoPrecision, NhlTBoolean,
		sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_auto_precision),
		NhlTImmediate,_NhlUSET( (NhlPointer)True ),0,NULL},
	{ NhlNtmYLMaxLabelLenF, NhlCtmYLMaxLabelLenF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_max_label_len),
		NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},
	{ NhlNtmYLMinLabelSpacingF, NhlCtmYLMinLabelSpacingF, 
	        NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_spacing),
		NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},
	

	{ NhlNtmYRDataTopF, NhlCtmYRDataTopF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_data_top),
		NhlTString,_NhlUSET("1.0" ),0,NULL},
	{ NhlNtmYRDataBottomF, NhlCtmYRDataBottomF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_data_bottom),
		NhlTString,_NhlUSET("0.0" ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.y_r_tick_start_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmYRTickStartF, NhlCtmYRTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_tick_start),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.y_r_tick_end_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtmYRTickEndF, NhlCtmYRTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_tick_end),
        	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmYRMaxTicks, NhlCtmYRMaxTicks, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_max_ticks),
		NhlTImmediate,_NhlUSET( (NhlPointer)7 ),0,NULL},
	{ NhlNtmYRTickSpacingF, NhlCtmYRTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_tick_spacing),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{ NhlNtmYRSpacingType, NhlCtmYRSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_spacing_type),
		NhlTImmediate,_NhlUSET( (NhlPointer)0 ),
		_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{ NhlNtmYRIrregularPoints, NhlCtmYRIrregularPoints,NhlTFloatGenArray,
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_irregular_points),
		NhlTImmediate,_NhlUSET( NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmYRValues, NhlCtmYRValues, NhlTFloatGenArray, 
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_values),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmYRMinorValues, NhlCtmYRMinorValues, NhlTFloatGenArray, 
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_values),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmYRLabels, NhlCtmYRLabels, NhlTStringGenArray, 
		sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_labels),
		NhlTImmediate,_NhlUSET( NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNtmYRMajorThicknessF, NhlCtmMajorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_major_thickness),
		NhlTString,_NhlUSET( "2.0" ),0,NULL},
	{NhlNtmYRMajorLineColor,NhlCLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_major_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)1),0,NULL},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.y_r_major_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYRMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_major_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, 
			   tick.y_r_major_outward_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYRMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_major_outward_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtmYRMinorThicknessF, NhlCtmMinorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_thickness),
		NhlTString,_NhlUSET( "1.0" ),0,NULL},
	{NhlNtmYRMinorLineColor,NhlCLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_line_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, tick.y_r_minor_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYRMinorLengthF, NhlCtmMinorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec, 
			   tick.y_r_minor_outward_length_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYRMinorOutwardLengthF, NhlCtmMinorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_minor_outward_length),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtmYRLabelFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_label_font),
		NhlTImmediate,_NhlUSET( (NhlPointer)21 ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_font_height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{NhlNtmYRLabelFontHeightF, NhlCFontHeightF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_label_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtmYRLabelFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_font_color),
		NhlTImmediate,_NhlUSET( (NhlPointer)NhlFOREGROUND ),0,NULL},
	{NhlNtmYRLabelFontAspectF,NhlCFontAspectF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_font_aspect),
		NhlTString,_NhlUSET( "1.3125" ),0,NULL},
	{NhlNtmYRLabelJust,NhlCtmYRLabelJust,NhlTJustification,
	 	sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_just),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERLEFT),0,NULL},
	{NhlNtmYRLabelAngleF, NhlCYAxisTextAngleF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_angle),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{NhlNtmYRLabelDirection, NhlCYAxisTextDirection, NhlTTextDirection,
		sizeof(NhlTextDirection),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_direction),
		NhlTImmediate,_NhlUSET( (NhlPointer) NhlACROSS ),0,NULL},
	{ NhlNtmYRLabelFuncCode, NhlCTextFuncCode, NhlTCharacter, 
		  sizeof(char),
		  NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_fcode),
		  NhlTString,_NhlUSET("~"),0,NULL},
	{NhlNtmYRLabelFontThicknessF,NhlCFontThicknessF,NhlTFloat,
		 sizeof(float), 
		 NhlOffset(NhlTickMarkLayerRec,
			   tick.y_r_label_font_thickness),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNtmYRLabelFontQuality, NhlCFontQuality, NhlTFontQuality,
		 sizeof(NhlFontQuality),
		 NhlOffset(NhlTickMarkLayerRec,
			   tick.y_r_label_font_quality), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlHIGH ),0,NULL},
	{NhlNtmYRLabelConstantSpacingF, NhlCTextConstantSpacingF,NhlTFloat,
		 sizeof(float), 
		 NhlOffset(NhlTickMarkLayerRec,
			   tick.y_r_label_constant_spacing),
		 NhlTString,_NhlUSET("0.0" ), 0,NULL},
	{ NhlNtmYRLabelDeltaF, NhlCtmYRLabelDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_delta),
		NhlTString,_NhlUSET( "0.0" ),0,NULL},
	{ NhlNtmYRAutoPrecision, NhlCtmAutoPrecision, NhlTBoolean,
		sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_auto_precision),
		NhlTImmediate,_NhlUSET( (NhlPointer)True ),0,NULL},
	{ NhlNtmYRMaxLabelLenF, NhlCtmYRMaxLabelLenF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_max_label_len),
		NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL},
	{ NhlNtmYRMinLabelSpacingF, NhlCtmYRMinLabelSpacingF, 
	        NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_spacing),
		NhlTString,_NhlUSET( "0.0" ),_NhlRES_GONLY,NULL}

/* End-documented-resources */

};

static NhlErrorTypes DrawLabels (
#if	NhlNeedProto
NhlTickMarkLayer /*tlayer*/
#endif 
);
static NhlErrorTypes DrawTicks(
#if	NhlNeedProto
NhlTickMarkLayer /*tlayer*/
#endif 
);
static NhlErrorTypes	DrawGrid(
#if	NhlNeedProto
NhlTickMarkLayer /*tlayer*/
#endif 
);
static NhlErrorTypes	DrawBorder(
#if	NhlNeedProto
NhlTickMarkLayer /*tlayer*/
#endif 
);
static NhlErrorTypes ComputeMinorTickMarks(
#if	NhlNeedProto
	int minorpermajor,
	float spacing,
	float tstart,
	float tend,
	float dmax,
	float dmin,
	float* m_locs,
	int nmajor,
	float* minor_locs,
	int* nminor,
	int precision,
	NhlTickMarkStyle style,
	float min_nonzero
#endif 
);
static NhlErrorTypes AutoComputeMajorTickMarks(
#if	NhlNeedProto
        NhlTickMarkStyle  /* style*/,
        float */*array*/,
        char** /*larray*/,
        int /*max_ticks*/,
        float /*dmax*/,
        float /*dmin*/,
        float */*tstart*/,
        float */*tend*/,
        int /*convert_precision*/,
        float */*spacing*/,
        int */*nmajor*/,
        int /*cutoff*/,
	NhlFormatRec * /*format*/,
	char /*func_code*/,
	int * /*sug_minor_ticks*/,
	float /*min_nonzero*/
#endif
);
static NhlErrorTypes ManualComputeMajorTickMarks(
#if	NhlNeedProto
        NhlTickMarkStyle /*style*/,
        float */*array*/,
        char** /*larray*/,
        float /*dmax*/,
        float /*dmin*/,
        float /*tstart*/,
        float /*tend*/,
        int /*convert_precision*/,
	NhlBoolean /*auto_precision*/,
        float /*spacing*/,
        int /*spacing_type*/,
        int */*nmajor*/,
        int /*cutoff*/,
	NhlFormatRec * /*format*/,
	char 	/*func_code*/,
	float /*min_nonzero*/
#endif
);
static NhlErrorTypes ChooseSpacingLin(
#if	NhlNeedProto
float * /*tstart*/,
float * /*tend*/,
float * /*spacing*/,
int /*convert_precision*/,
int /*max_ticks*/,
int * /*sug_minor_ticks*/,
float min_nonzero
#endif
);
static NhlErrorTypes ChooseSpacingLog(
#if	NhlNeedProto
float * /*tstart*/,
float * /*tend*/,
float * /*spacing*/,
int /* convert_precision*/,
int /* max_ticks */,
int * /*sug_minor_ticks*/,
float /*min_nonzero*/
#endif
);
static NhlErrorTypes ExplicitComputeMinorTickMarks(
NhlTickMarkStyle /*style*/,
float */*array*/,
float /*dmax*/,
float /*dmin*/,
float /*tstart*/,
float /*tend*/,
int /*convert_precision*/,
float */*requested_points*/,
int */*nmajor*/,
int /* n */,
float min_nonzero
);

static NhlErrorTypes ExplicitComputeMajorTickMarks(
#if	NhlNeedProto
NhlTickMarkStyle /*style*/,
float */*array*/,
char** /*larray*/,
float /*dmax*/,
float /*dmin*/,
float */*tstart*/,
float */*tend*/,
int /*convert_precision*/,
NhlGenArray req_points,
NhlGenArray req_labels,
int */*nmajor*/,
float /*min_nonzero*/
#endif
);



/* Method function declarations */

static NhlErrorTypes	TickMarkSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes	TickMarkInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes	TickMarkDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes	TickMarkClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes	TickMarkDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes TickMarkGetValues(
#if	NhlNeedProto
        NhlLayer        l,
        _NhlArgList     args,
        int             nargs
#endif

);

static void SetTop(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */
#endif
);
static void SetRight(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */
#endif
);
static NhlErrorTypes CheckKeyVals(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckManual(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckExplicit(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
_NhlArgList	/* args */,
int		/* num_args*/,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckNotAuto(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckNotLog(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckLog(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckIrregular(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
_NhlArgList	/* args */,
int		/* num_args */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckTime(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckGeographic(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes ComputeTickInfo(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);

static NhlErrorTypes TransformLocations(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);

#define CREATE 1
#define SET 0

static NhlErrorTypes ComputeAndSetLabelInfo(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer 	/* told */,
int		/* cors */
#endif
);

static NhlErrorTypes CreateXTYRTransformInfo(
#if	NhlNeedProto
NhlTickMarkLayer 	tnew,
_NhlArgList	args,
int		num_args
#endif
);

static NhlErrorTypes ChangeTransformInfo(
#if	NhlNeedProto
NhlTickMarkLayer 	tnew,
NhlTickMarkLayer 	told,
_NhlArgList	args,
int		num_args
#endif
);

static NhlErrorTypes CreateXBYLTransformInfo(
#if	NhlNeedProto
NhlTickMarkLayer 	tnew,
_NhlArgList	args,
int		num_args
#endif
);

static NhlErrorTypes ScaleValuesForMove(
#if	NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer 	/* treq*/,
_NhlArgList	/* args */,
int		/* num_args */,
int		/* c_or_s */
#endif
);


/*
* View Methods
*/

static NhlErrorTypes TickMarkGetBB(
#if	NhlNeedProto
        NhlLayer          /* instance */,
        NhlBoundingBox * /*thebox*/
#endif
);

static char *ConvertToString(
#if	NhlNeedProto
	float		value,
	int		convert_precision,
	int		compare_precision,
	NhlTickMarkStyle	style,
	int		cutoff,
	int		left_sig_digit,
	NhlFormatRec	*format,
	char		func_code
#endif
);

NhlTickMarkClassRec NhltickMarkClassRec = {
        {
/* class_name		*/      "tickMarkClass",
/* nrm_class		*/      NrmNULLQUARK,
/* layer_size		*/      sizeof(NhlTickMarkLayerRec),
/* class_inited		*/      False,
/* superclass		*/      (NhlClass)&NhlviewClassRec,
/* cvt_table			*/	NULL,

/* layer_resources	*/      resources,
/* num_resources	*/      NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize*/      NULL,
/* class_initialize	*/      TickMarkClassInitialize,
/* layer_initialize	*/      TickMarkInitialize,
/* layer_set_values	*/      TickMarkSetValues,
/* layer_set_values_hook*/      NULL,
/* layer_get_values	*/      TickMarkGetValues,
/* layer_reparent	*/      NULL,
/* layer_destroy	*/      TickMarkDestroy,

/* child_resources	*/      NULL,

/* layer_draw		*/      TickMarkDraw,

/* layer_pre_draw	*/      NULL,
/* layer_draw_segonly	*/      NULL,
/* layer_post_draw	*/      NULL,
/* layer_clear		*/      NULL
	},
        {
/* segment_workstation  */      -1,
/* get_bb       */     TickMarkGetBB
	},
	{
		NULL
	}
};


NhlClass NhltickMarkClass = (NhlClass)&NhltickMarkClassRec;

/*
 * Function:	nhlftickmarkclass
 *
 * Description:	fortran ref to this class
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
_NHLCALLF(nhlftickmarkclass,NHLFTICKMARKCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhltickMarkClass;
}

static NrmQuark Qfloat = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark QXBIrregularPoints;
static NrmQuark QXTIrregularPoints;
static NrmQuark QYLIrregularPoints;
static NrmQuark QYRIrregularPoints;
static NrmQuark QXBValues;
static NrmQuark QXTValues;
static NrmQuark QYLValues;
static NrmQuark QYRValues;
static NrmQuark QXBMinorValues;
static NrmQuark QXTMinorValues;
static NrmQuark QYLMinorValues;
static NrmQuark QYRMinorValues;
static NrmQuark QXBLabels;
static NrmQuark QXTLabels;
static NrmQuark QYLLabels;
static NrmQuark QYRLabels;
static NrmQuark QXBFormat;
static NrmQuark QXTFormat;
static NrmQuark QYLFormat;
static NrmQuark QYRFormat;


/*
* Function definitions
*/

/*
 * Function:	TickMarkSetValues
 *
 * Description: This one just switches between functions than handle each
 *		mode and style. It calls the following functions in the
 *		following order.
 *
 *		ScaleValuesForMove
 *		SetTop
 *		SetRight
 *		CheckKeyValues
 *		CheckManual
 *		CheckExplicit
 *		CheckNotAuto
 *		CheckNotLog
 *		CheckLog
 *		CheckIrregular
 *		CheckTime
 *		CheckGeographic
 *		ChangeTransformInfo
 *		ComputeTickInfo
 *		TransformLocations
 *		ComputeAndSetLabelInfo
 *		
 *		These functions either confirm resource values, copy values
 *		into internal storage, or create children objects needed to
 *		draw tick marks and labels.
 *
 * Side Effects: Possible change of GKS viewport transformation.
 */
/*ARGSUSED*/
static NhlErrorTypes	TickMarkSetValues
#if	NhlNeedProto
(NhlLayer old, NhlLayer reference, NhlLayer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int	num_args;
#endif
{
	NhlTickMarkLayer tnew = (NhlTickMarkLayer) new;
	NhlTickMarkLayer told = (NhlTickMarkLayer) old;
	int i;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes realret = NhlNOERROR;
	int		view_args = 0;
        float	fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;

 	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

	if (tnew->view.use_segments != told->view.use_segments)
		tnew->tick.new_draw_req = True;

	if (_NhlArgIsSet(args,num_args,NhlNvpXF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpYF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpWidthF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpHeightF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpOn)) view_args++;

	if (_NhlArgIsSet(args,num_args,NhlNtmXBPrecision)) 
		tnew->tick.x_b_precision_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTPrecision)) 
		tnew->tick.x_t_precision_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLPrecision)) 
		tnew->tick.y_l_precision_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRPrecision)) 
		tnew->tick.y_r_precision_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBMinorPerMajor)) 
		tnew->tick.x_b_minor_per_major_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTMinorPerMajor)) 
		tnew->tick.x_t_minor_per_major_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLMinorPerMajor)) 
		tnew->tick.y_l_minor_per_major_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRMinorPerMajor)) 
		tnew->tick.y_r_minor_per_major_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBLabelFontHeightF)) 
		tnew->tick.x_b_label_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTLabelFontHeightF)) 
		tnew->tick.x_t_label_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLLabelFontHeightF)) 
		tnew->tick.y_l_label_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRLabelFontHeightF)) 
		tnew->tick.y_r_label_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBMajorLengthF)) 
		tnew->tick.x_b_major_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTMajorLengthF)) 
		tnew->tick.x_t_major_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLMajorLengthF)) 
		tnew->tick.y_l_major_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRMajorLengthF)) 
		tnew->tick.y_r_major_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBMajorOutwardLengthF)) 
		tnew->tick.x_b_major_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTMajorOutwardLengthF)) 
		tnew->tick.x_t_major_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLMajorOutwardLengthF)) 
		tnew->tick.y_l_major_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRMajorOutwardLengthF)) 
		tnew->tick.y_r_major_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBMinorLengthF)) 
		tnew->tick.x_b_minor_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTMinorLengthF)) 
		tnew->tick.x_t_minor_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLMinorLengthF)) 
		tnew->tick.y_l_minor_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRMinorLengthF)) 
		tnew->tick.y_r_minor_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBMinorOutwardLengthF)) 
		tnew->tick.x_b_minor_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTMinorOutwardLengthF)) 
		tnew->tick.x_t_minor_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLMinorOutwardLengthF)) 
		tnew->tick.y_l_minor_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRMinorOutwardLengthF)) 
		tnew->tick.y_r_minor_outward_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBTickStartF)) 
		tnew->tick.x_b_tick_start_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXBTickEndF)) 
		tnew->tick.x_b_tick_end_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTTickStartF)) 
		tnew->tick.x_t_tick_start_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmXTTickEndF)) 
		tnew->tick.x_t_tick_end_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLTickStartF)) 
		tnew->tick.y_l_tick_start_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYLTickEndF)) 
		tnew->tick.y_l_tick_end_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRTickStartF)) 
		tnew->tick.y_r_tick_start_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtmYRTickEndF)) 
		tnew->tick.y_r_tick_end_set = True;

	if (num_args > view_args ||
            ! _NhlSegmentSpansArea(tnew->tick.trans_dat,
                                   tnew->view.x,
                                   tnew->view.x + tnew->view.width,
                                   tnew->view.y - tnew->view.height,
                                   tnew->view.y))
	    tnew->tick.new_draw_req = True;
/*
* for resources that represent size THAT HAVE NOT been set in the current
* setvalues call will be scalled proportionally to the move and resize. Values
* that have been set in this setvalues call are left alone.
*/
	ScaleValuesForMove(tnew,told,args,num_args,SET);

	if(tnew->tick.x_use_bottom) {
		SetTop(tnew);
	}
	if(tnew->tick.y_use_left) {
		SetRight(tnew);
	}
/*
* Make sure key-values are set before proceding
*/
	ret = CheckKeyVals(tnew,told,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining input resource values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;
/*
* Now check on key values for various modes
*/
	ret = CheckManual(tnew,told,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining NhlMANUAL mode values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = CheckExplicit(tnew,told,args,num_args,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining NhlEXPLICIT mode values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = CheckNotAuto(tnew,told,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining NhlAUTOMATIC mode values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;
/*
* Now Check key values for various styles.
*/
	ret = CheckNotLog(tnew,told,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining NhlLINEAR style values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;
	ret = CheckLog(tnew,told,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining NhlLOG style values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = CheckIrregular(tnew,told,args,num_args,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining NhlIRREGULAR style values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = CheckTime(tnew,told,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining NhlTIME style values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = CheckGeographic(tnew,told,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while examining NhlGEOGRAPHICS style values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

/*
 * If the previous setting was Explicit, major_labels just has a copy of
 * each label pointer, so no freeing needs to happen; otherwise free
 * the non-null strings.
 */
        for (i = 0; i < tnew->tick.x_b_nmajor; i++) {
                if (tnew->tick.x_b_major_labels[i] != NULL &&
                    told->tick.x_b_mode != NhlEXPLICIT)
                        NhlFree(tnew->tick.x_b_major_labels[i]);
                tnew->tick.x_b_major_labels[i] = NULL;
        }
        for (i = 0; i < tnew->tick.x_t_nmajor; i++) {
                if (tnew->tick.x_t_major_labels[i] != NULL &&
                    told->tick.x_t_mode != NhlEXPLICIT)
                        NhlFree(tnew->tick.x_t_major_labels[i]);
                tnew->tick.x_t_major_labels[i] = NULL;
        }
        for (i = 0; i < tnew->tick.y_l_nmajor; i++) {
                if (tnew->tick.y_l_major_labels[i] != NULL &&
                    told->tick.y_l_mode != NhlEXPLICIT)
                        NhlFree(tnew->tick.y_l_major_labels[i]);
                tnew->tick.y_l_major_labels[i] = NULL;
        }
        for (i = 0; i < tnew->tick.y_r_nmajor; i++) {
                if (tnew->tick.y_r_major_labels[i] != NULL &&
                    told->tick.y_r_mode != NhlEXPLICIT)
                        NhlFree(tnew->tick.y_r_major_labels[i]);
                tnew->tick.y_r_major_labels[i] = NULL;
        }

	tnew->tick.x_b_data_min = MIN(tnew->tick.x_b_data_left,
					tnew->tick.x_b_data_right);
	tnew->tick.x_t_data_min = MIN(tnew->tick.x_t_data_left,
					tnew->tick.x_t_data_right);
	tnew->tick.y_r_data_min = MIN(tnew->tick.y_r_data_top,
					tnew->tick.y_r_data_bottom);
	tnew->tick.y_l_data_min = MIN(tnew->tick.y_l_data_top,
					tnew->tick.y_l_data_bottom);
	tnew->tick.x_b_data_max = MAX(tnew->tick.x_b_data_left,
					tnew->tick.x_b_data_right);
	tnew->tick.x_t_data_max = MAX(tnew->tick.x_t_data_left,
					tnew->tick.x_t_data_right);
	tnew->tick.y_r_data_max = MAX(tnew->tick.y_r_data_top,
					tnew->tick.y_r_data_bottom);
	tnew->tick.y_l_data_max = MAX(tnew->tick.y_l_data_top,
					tnew->tick.y_l_data_bottom);


/*
* ALL COMPUTATIONALLY INTENSIVE ROUTINES FOLLOW. CONVENTIONS FOR DETERMINING
* WHETHER OR NOT THESE ROUTINES NEED TO BE CALLED EVERY TIME ARE NEEDED
*/

	ret = ChangeTransformInfo(tnew,told,args,num_args);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while setting up transformation information,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

/*
* Each of the compute functions comes up with an array of data values
* for each tick mark and a string label . The labels are then skipped when
* drawn if a stride is specified.
*/

	ret =ComputeTickInfo(tnew,told,SET); 
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while computing TickMark information,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;
/*
* At this point the major_data_locs are filled with ticks that are withing
* the data range and the major_labels contain the coresponding labels
* The question is whether the data_locs should be transformed to NDC now or
* should this happen in the Draw Routine. 
*
* Initialy I'm going to compute the values here in the interest of speeding up
* the Draw.
*/
	ret = TransformLocations(tnew,told,SET); 
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while computing locations of tick marks,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;


/*
* Now make array of locations and strings and use it to create the
* MultiTextItems . Tick must be filtered out if stride > 1
*/
	ret = ComputeAndSetLabelInfo(tnew,told,SET);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while computing tick marks labels,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	tnew->tick.x_b_precision_set = False;
	tnew->tick.x_t_precision_set = False;
	tnew->tick.y_l_precision_set = False;
	tnew->tick.y_r_precision_set = False;
/*
* Commenting these out so that auto-determination of of minor 
* tickmark spacing can be done when these are false and manual
* setting of these will cause no auto-determination
*
* Bottom line. Once these are set manually by the user they'll
* always use the values intended by the user. If these are
* never set then auto-determination always happens.

	tnew->tick.x_b_minor_per_major_set = False;
	tnew->tick.x_t_minor_per_major_set = False;
	tnew->tick.y_l_minor_per_major_set = False;
	tnew->tick.y_r_minor_per_major_set = False;
*/
	tnew->tick.x_b_label_font_height_set = False;
	tnew->tick.x_t_label_font_height_set = False;
	tnew->tick.y_l_label_font_height_set = False;
	tnew->tick.y_r_label_font_height_set = False;
	tnew->tick.x_b_major_length_set = False;
	tnew->tick.x_t_major_length_set = False;
	tnew->tick.y_l_major_length_set = False;
	tnew->tick.y_r_major_length_set = False;
	tnew->tick.x_b_major_outward_length_set = False;
	tnew->tick.x_t_major_outward_length_set = False;
	tnew->tick.y_l_major_outward_length_set = False;
	tnew->tick.y_r_major_outward_length_set = False;
	tnew->tick.x_b_minor_length_set = False;
	tnew->tick.x_t_minor_length_set = False;
	tnew->tick.y_l_minor_length_set = False;
	tnew->tick.y_r_minor_length_set = False;
	tnew->tick.x_b_minor_outward_length_set = False;
	tnew->tick.x_t_minor_outward_length_set = False;
	tnew->tick.y_l_minor_outward_length_set = False;
	tnew->tick.y_r_minor_outward_length_set = False;
	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);

	return(realret);
}

/*
 * Function:	TickMarkInitialize	
 *
 * Description: This one just switches between functions than handle each
 *		mode and style. It calls the following functions in the
 *		following order.
 *
 *		SetTop
 *		SetRight
 *		CheckKeyValues
 *		CheckManual
 *		CheckExplicit
 *		CheckNotAuto
 *		CheckLog
 *		CheckIrregular
 *		CheckTime
 *		CheckGeographic
 *		CreateXTYRTransformInfo
 *		CreateXBYLTransformInfo
 *		ComputeTickInfo
 *		TransformLocations
 *		ComputeAndSetLabelInfo
 *		
 *		These functions either confirm resource values, copy values
 *		into internal storage, or create children objects needed to
 *		draw tick marks and labels.
 *
 * Side Effects: Possible change of GKS viewport transformation.
 */

/*ARGSUSED*/
static NhlErrorTypes	TickMarkInitialize
#if	NhlNeedProto
( NhlClass class, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
	NhlClass	class;
	NhlLayer		req;
	NhlLayer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlTickMarkLayer tnew = (NhlTickMarkLayer) new;
	int i;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes realret = NhlNOERROR;
        float	fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;

 	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

	if (! tnew->tick.x_b_precision_set)
		tnew->tick.x_b_precision = 4;
	if (! tnew->tick.x_t_precision_set)
		tnew->tick.x_t_precision = 4;
	if (! tnew->tick.y_l_precision_set)
		tnew->tick.y_l_precision = 4;
	if (! tnew->tick.y_r_precision_set)
		tnew->tick.y_r_precision = 4;
	if (! tnew->tick.x_b_minor_per_major_set)
		tnew->tick.x_b_minor_per_major = 3;
	if (! tnew->tick.x_t_minor_per_major_set)
		tnew->tick.x_t_minor_per_major = 3;
	if (! tnew->tick.y_l_minor_per_major_set)
		tnew->tick.y_l_minor_per_major = 3;
	if (! tnew->tick.y_r_minor_per_major_set)
		tnew->tick.y_r_minor_per_major = 3;
	if (! tnew->tick.x_b_label_font_height_set)
		tnew->tick.x_b_label_font_height = 0.02;
	if (! tnew->tick.x_t_label_font_height_set)
		tnew->tick.x_t_label_font_height = 0.02;
	if (! tnew->tick.y_l_label_font_height_set)
		tnew->tick.y_l_label_font_height = 0.02;
	if (! tnew->tick.y_r_label_font_height_set)
		tnew->tick.y_r_label_font_height = 0.02;
	if (! tnew->tick.x_b_major_length_set)
		tnew->tick.x_b_major_length = 0.02;
	if (! tnew->tick.x_t_major_length_set)
		tnew->tick.x_t_major_length = 0.02;
	if (! tnew->tick.y_l_major_length_set)
		tnew->tick.y_l_major_length = 0.02;
	if (! tnew->tick.y_r_major_length_set)
		tnew->tick.y_r_major_length = 0.02;
	if (! tnew->tick.x_b_major_outward_length_set)
		tnew->tick.x_b_major_outward_length = 0.0;
	if (! tnew->tick.x_t_major_outward_length_set)
		tnew->tick.x_t_major_outward_length = 0.0;
	if (! tnew->tick.y_l_major_outward_length_set)
		tnew->tick.y_l_major_outward_length = 0.0;
	if (! tnew->tick.y_r_major_outward_length_set)
		tnew->tick.y_r_major_outward_length = 0.0;
	if (! tnew->tick.x_b_minor_length_set)
		tnew->tick.x_b_minor_length = 0.01;
	if (! tnew->tick.x_t_minor_length_set)
		tnew->tick.x_t_minor_length = 0.01;
	if (! tnew->tick.y_l_minor_length_set)
		tnew->tick.y_l_minor_length = 0.01;
	if (! tnew->tick.y_r_minor_length_set)
		tnew->tick.y_r_minor_length = 0.01;
	if (! tnew->tick.x_b_minor_outward_length_set)
		tnew->tick.x_b_minor_outward_length = 0.0;
	if (! tnew->tick.x_t_minor_outward_length_set)
		tnew->tick.x_t_minor_outward_length = 0.0;
	if (! tnew->tick.y_l_minor_outward_length_set)
		tnew->tick.y_l_minor_outward_length = 0.0;
	if (! tnew->tick.y_r_minor_outward_length_set)
		tnew->tick.y_r_minor_outward_length = 0.0;
	if (! tnew->tick.x_b_tick_start_set)
		tnew->tick.x_b_tick_start = 0.0;
	if (! tnew->tick.x_b_tick_end_set)
		tnew->tick.x_b_tick_end = 0.0;
	if (! tnew->tick.x_t_tick_start_set)
		tnew->tick.x_t_tick_start = 0.0;
	if (! tnew->tick.x_t_tick_end_set)
		tnew->tick.x_t_tick_end = 0.0;
	if (! tnew->tick.y_l_tick_start_set)
		tnew->tick.y_l_tick_start = 0.0;
	if (! tnew->tick.y_l_tick_end_set)
		tnew->tick.y_l_tick_end = 0.0;
	if (! tnew->tick.y_r_tick_start_set)
		tnew->tick.y_r_tick_start = 0.0;
	if (! tnew->tick.y_r_tick_end_set)
		tnew->tick.y_r_tick_end = 0.0;

	tnew->tick.new_draw_req = True;
	tnew->tick.trans_dat = NULL;
	ScaleValuesForMove(tnew,NULL,args,num_args,CREATE);


	if(tnew->tick.x_use_bottom) {
		SetTop(tnew);
	}
	if(tnew->tick.y_use_left) {
		SetRight(tnew);
	}
/*
* Make sure key-values are set before proceding
*/
	ret = CheckKeyVals(tnew,NULL,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while examining input resource values,cannot continue");
		return(ret);
	}
	if(ret < realret) 
		realret = ret;
/*
* Now check on key values for various modes
*/
	ret = CheckManual(tnew,NULL,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while examining NhlMANUAL mode values,cannot continue");
		return(ret);
	}
	if(ret < realret) 
		realret = ret;

	ret = CheckExplicit(tnew,NULL,args,num_args,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while examining NhlEXPLICIT mode values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = CheckNotAuto(tnew,NULL,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while examining NhlAUTOMATIC mode values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

/*
* Now Check key values for various styles.
*/
	ret = CheckLog(tnew,NULL,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while examining NhlLOG style values,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = CheckIrregular(tnew,NULL,args,num_args,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while examining NhlIRREGULAR style values, cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = CheckTime(tnew,NULL,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while examining NhlTIME style values, cannot continue");
		return(ret);
	}
	ret = CheckGeographic(tnew,NULL,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while examining NhlGEOGRAPHIC style values, cannot continue");
		return(ret);
	}

	tnew->tick.x_b_data_min = MIN(tnew->tick.x_b_data_left,
					tnew->tick.x_b_data_right);
	tnew->tick.x_t_data_min = MIN(tnew->tick.x_t_data_left,
					tnew->tick.x_t_data_right);
	tnew->tick.y_r_data_min = MIN(tnew->tick.y_r_data_top,
					tnew->tick.y_r_data_bottom);
	tnew->tick.y_l_data_min = MIN(tnew->tick.y_l_data_top,
					tnew->tick.y_l_data_bottom);
	tnew->tick.x_b_data_max = MAX(tnew->tick.x_b_data_left,
					tnew->tick.x_b_data_right);
	tnew->tick.x_t_data_max = MAX(tnew->tick.x_t_data_left,
					tnew->tick.x_t_data_right);
	tnew->tick.y_r_data_max = MAX(tnew->tick.y_r_data_top,
					tnew->tick.y_r_data_bottom);
	tnew->tick.y_l_data_max = MAX(tnew->tick.y_l_data_top,
					tnew->tick.y_l_data_bottom);

/*
* ALL COMPUTATIONALLY INTENSIVE ROUTINES FOLLOW. CONVENTIONS FOR DETERMINING
* WHETHER OR NOT THESE ROUTINES NEED TO BE CALLED EVERY TIME ARE NEEDED
*/
/*
* At this point all resource values should be confirmed and can then procede
* with internal field generation
*/
	ret = CreateXBYLTransformInfo(tnew,args,num_args);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while setting up transformation information,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;
	ret = CreateXTYRTransformInfo(tnew,args,num_args);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while setting up transformation information,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

/*
* The data locations of the tick marks will be calculated here however the
* actuall NDC tick locations should probably wait until the Draw call.
*/
/*
* First initialize arrays that will hold values
*/
	tnew->tick.x_b_major_ndc_locs = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXTICKS);
	tnew->tick.x_b_major_data_locs = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXTICKS);
	tnew->tick.x_b_major_labels = (char**)NhlMalloc((unsigned)
						sizeof(char*)*MAXTICKS);
	tnew->tick.x_b_minor_data_locs  = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXMINORTICKS);
	tnew->tick.x_b_minor_ndc_locs  = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXMINORTICKS);
	
	tnew->tick.x_t_major_ndc_locs = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXTICKS);
	tnew->tick.x_t_major_data_locs = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXTICKS);
	tnew->tick.x_t_major_labels = (char**)NhlMalloc((unsigned)
						sizeof(char*)*MAXTICKS);
	tnew->tick.x_t_minor_data_locs  = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXMINORTICKS);
	tnew->tick.x_t_minor_ndc_locs  = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXMINORTICKS);
	
	tnew->tick.y_l_major_ndc_locs = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXTICKS);
	tnew->tick.y_l_major_data_locs = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXTICKS);
	tnew->tick.y_l_major_labels = (char**)NhlMalloc((unsigned)
						sizeof(char*)*MAXTICKS);
	tnew->tick.y_l_minor_data_locs  = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXMINORTICKS);
	tnew->tick.y_l_minor_ndc_locs  = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXMINORTICKS);
	
	tnew->tick.y_r_major_ndc_locs = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXTICKS);
	tnew->tick.y_r_major_data_locs = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXTICKS);
	tnew->tick.y_r_major_labels = (char**)NhlMalloc((unsigned)
						sizeof(char*)*MAXTICKS);
	tnew->tick.y_r_minor_data_locs  = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXMINORTICKS);
	tnew->tick.y_r_minor_ndc_locs  = (float*)NhlMalloc((unsigned)
						sizeof(float)*MAXMINORTICKS);
	for(i = 0; i< MAXTICKS; i++ ) {
		tnew->tick.x_b_major_labels[i] = NULL;
		tnew->tick.x_t_major_labels[i] = NULL;
		tnew->tick.y_l_major_labels[i] = NULL;
		tnew->tick.y_r_major_labels[i] = NULL;
	}


/*
* Each of the compute functions comes up with an array of data values
* for each tick mark and a string label . The labels are then skipped when
* drawn if a stride is specified.
*/

	ret = ComputeTickInfo(tnew,NULL,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while computing TickMark information,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;
/*
* At this point the major_data_locs are filled with ticks that are withing
* the data range and the major_labels contain the coresponding labels
* The question is whether the data_locs should be transformed to NDC now or
* should this happen in the Draw Routine. 
*
* Initialy I'm going to compute the values here in the interest of speeding up
* the Draw.
*/
	ret = TransformLocations(tnew,NULL,CREATE); 
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while computing locations of tick marks,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;


/*
* Now make array of locations and strings and use it to create the
* MultiTextItems . Tick must be filtered out if stride > 1
*/
	ret = ComputeAndSetLabelInfo(tnew,NULL,CREATE);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while computing tick marks labels,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	tnew->tick.x_b_precision_set = False;
	tnew->tick.x_t_precision_set = False;
	tnew->tick.y_l_precision_set = False;
	tnew->tick.y_r_precision_set = False;

/* See comments in SetValues
	tnew->tick.x_b_minor_per_major_set = False;
	tnew->tick.x_t_minor_per_major_set = False;
	tnew->tick.y_l_minor_per_major_set = False;
	tnew->tick.y_r_minor_per_major_set = False;
*/

	tnew->tick.x_b_label_font_height_set = False;
	tnew->tick.x_t_label_font_height_set = False;
	tnew->tick.y_l_label_font_height_set = False;
	tnew->tick.y_r_label_font_height_set = False;
	tnew->tick.x_b_major_length_set = False;
	tnew->tick.x_t_major_length_set = False;
	tnew->tick.y_l_major_length_set = False;
	tnew->tick.y_r_major_length_set = False;
	tnew->tick.x_b_major_outward_length_set = False;
	tnew->tick.x_t_major_outward_length_set = False;
	tnew->tick.y_l_major_outward_length_set = False;
	tnew->tick.y_r_major_outward_length_set = False;
	tnew->tick.x_b_minor_length_set = False;
	tnew->tick.x_t_minor_length_set = False;
	tnew->tick.y_l_minor_length_set = False;
	tnew->tick.y_r_minor_length_set = False;
	tnew->tick.x_b_minor_outward_length_set = False;
	tnew->tick.x_t_minor_outward_length_set = False;
	tnew->tick.y_l_minor_outward_length_set = False;
	tnew->tick.y_r_minor_outward_length_set = False;

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);

	return(realret);
}

/*
 * Function:	TickMarkDestroy
 *
 * Description:	Calls NhlDestroy on all instances of children objects these
 *		include 4 multitext items, two transformation objects and
 *		If allocated arrays used to hold explicit values have been
 *		allocated then they are freed.
 *		Storage for all tickmark labels are freed.
 *		If allocated arrays used to hold irregular coordinate control
 *		points are freed.
 *
 * In Args: 	inst 	is an instance of a TickMark Object
 *
 * Out Args:	NONE
 *
 * Return Values: NONE
 *
 * Side Effects: Objects have been destroyed and all internal memory freed.
 */
static NhlErrorTypes	TickMarkDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
	NhlLayer inst;
#endif
{
	NhlTickMarkLayer tinst = (NhlTickMarkLayer) inst;
	int i;

	if(tinst->tick.xb_multi != NULL)
		NhlDestroy(tinst->tick.xb_multi->base.id);
	if(tinst->tick.xt_multi != NULL)
		NhlDestroy(tinst->tick.xt_multi->base.id);
	if(tinst->tick.yl_multi != NULL)
		NhlDestroy(tinst->tick.yl_multi->base.id);
	if(tinst->tick.yr_multi != NULL)
		NhlDestroy(tinst->tick.yr_multi->base.id);
	if(tinst->tick.xb_yl_trans_obj != NULL) 
		NhlDestroy(tinst->tick.xb_yl_trans_obj->base.id);
	if(tinst->tick.xt_yr_trans_obj != NULL) 
		NhlDestroy(tinst->tick.xt_yr_trans_obj->base.id);

	NhlFreeGenArray(tinst->tick.x_b_irregular_points);
	NhlFreeGenArray(tinst->tick.x_b_values);
	NhlFreeGenArray(tinst->tick.x_b_minor_values);

/*
* Have to be carefull here because ExplicitComputeTickMarks just copies
* string pointers from tick.x_b_labels to tick.x_b_major_labels so I don't
* want to free strings twice!! The problem is compounded by the fact
* that not all strings in x_b_labels are copied to tick.x_b_major_labels
* when requested tick locations are outside of data window. 
*/
	if(tinst->tick.x_b_labels != NULL) {
		NhlFreeGenArray(tinst->tick.x_b_labels);
		NhlFree(tinst->tick.x_b_major_labels);
	} else {
		for(i=0; i< tinst->tick.x_b_nmajor; i++) {
			if(tinst->tick.x_b_major_labels[i] != NULL)
				NhlFree(tinst->tick.x_b_major_labels[i]);
		}
		NhlFree(tinst->tick.x_b_major_labels);
	}
	NhlFree(tinst->tick.x_b_major_ndc_locs);
	NhlFree(tinst->tick.x_b_major_data_locs);
	NhlFree(tinst->tick.x_b_minor_data_locs);
	NhlFree(tinst->tick.x_b_minor_ndc_locs);

	NhlFreeGenArray(tinst->tick.x_t_irregular_points);
	NhlFreeGenArray(tinst->tick.x_t_values);
	NhlFreeGenArray(tinst->tick.x_t_minor_values);

	if(tinst->tick.x_t_labels != NULL) {
		NhlFreeGenArray(tinst->tick.x_t_labels);
		NhlFree(tinst->tick.x_t_major_labels);
	} else {
		for(i=0; i< tinst->tick.x_t_nmajor; i++) {
			if(tinst->tick.x_t_major_labels[i] != NULL)
				NhlFree(tinst->tick.x_t_major_labels[i]);
		}
		NhlFree(tinst->tick.x_t_major_labels);
	}
	NhlFree(tinst->tick.x_t_major_ndc_locs);
	NhlFree(tinst->tick.x_t_major_data_locs);
	NhlFree(tinst->tick.x_t_minor_data_locs);
	NhlFree(tinst->tick.x_t_minor_ndc_locs);

	NhlFreeGenArray(tinst->tick.y_l_irregular_points);
	NhlFreeGenArray(tinst->tick.y_l_values);
	NhlFreeGenArray(tinst->tick.y_l_minor_values);

	if(tinst->tick.y_l_labels != NULL) {
		NhlFreeGenArray(tinst->tick.y_l_labels);
		NhlFree(tinst->tick.y_l_major_labels);
	} else {
		for(i=0; i< tinst->tick.y_l_nmajor; i++) {
			if(tinst->tick.y_l_major_labels[i] != NULL)
				NhlFree(tinst->tick.y_l_major_labels[i]);
		}
		NhlFree(tinst->tick.y_l_major_labels);
	}
	NhlFree(tinst->tick.y_l_major_ndc_locs);
	NhlFree(tinst->tick.y_l_major_data_locs);
	NhlFree(tinst->tick.y_l_minor_data_locs);
	NhlFree(tinst->tick.y_l_minor_ndc_locs);

	NhlFreeGenArray(tinst->tick.y_r_irregular_points);
	NhlFreeGenArray(tinst->tick.y_r_values);
	NhlFreeGenArray(tinst->tick.y_r_minor_values);

	if(tinst->tick.y_r_labels != NULL) {
		NhlFreeGenArray(tinst->tick.y_r_labels);
		NhlFree(tinst->tick.y_r_major_labels);
	} else {
		for(i=0; i< tinst->tick.y_r_nmajor; i++) {
			if(tinst->tick.y_r_major_labels[i] != NULL)
				NhlFree(tinst->tick.y_r_major_labels[i]);
		}
		NhlFree(tinst->tick.y_r_major_labels);
	}
	NhlFree(tinst->tick.y_r_major_ndc_locs);
	NhlFree(tinst->tick.y_r_major_data_locs);
	NhlFree(tinst->tick.y_r_minor_data_locs);
	NhlFree(tinst->tick.y_r_minor_ndc_locs);

	NhlFree(tinst->tick.x_b_format.fstring);
	NhlFree(tinst->tick.x_t_format.fstring);
	NhlFree(tinst->tick.y_l_format.fstring);
	NhlFree(tinst->tick.y_r_format.fstring);

	if (tinst->tick.trans_dat != NULL)
		_NhlDeleteViewSegment(inst,tinst->tick.trans_dat);
	
	return(NhlNOERROR);
}

/*
 * Function:	TickMarkClassInitialize
 *
 * Description: Just calls NrmStringToQuark to assure that the two new
 *		types NhlTTickMarkMode and NhlTTickMarkStyle are registered
 *		names.
 *
 * In Args: 	NONE
 *
 * Out Args: 	NONE
 *
 * Return Values: 	NONE
 *
 * Side Effects:	Two new quark values assigned.
 */
static NhlErrorTypes	TickMarkClassInitialize
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	_NhlEnumVals	tmarkmodes[] = {
		{NhlAUTOMATIC,	"Automatic"},
		{NhlMANUAL,	"Manual"},
		{NhlEXPLICIT,	"Explicit"}
	};

	_NhlEnumVals	tmarkstyles[] = {
		{NhlLOG,	"Log"},
		{NhlLINEAR,	"Linear"},
		{NhlIRREGULAR,	"Irregular"},
		{NhlTIME,	"Time"},
		{NhlGEOGRAPHIC,	"Geographic"}
	};

	_NhlInitializeClass(NhlmultiTextClass);

	_NhlRegisterEnumType(NhlbaseClass,NhlTTickMarkMode,tmarkmodes,
							NhlNumber(tmarkmodes));
	_NhlRegisterEnumType(NhlbaseClass,NhlTTickMarkStyle,tmarkstyles,
							NhlNumber(tmarkstyles));

	Qfloat = NrmStringToQuark(NhlTFloat);
	Qstring = NrmStringToQuark(NhlTString);

	QXBIrregularPoints = NrmStringToQuark(NhlNtmXBIrregularPoints);
	QXTIrregularPoints = NrmStringToQuark(NhlNtmXTIrregularPoints);
	QYLIrregularPoints = NrmStringToQuark(NhlNtmYLIrregularPoints);
	QYRIrregularPoints = NrmStringToQuark(NhlNtmYRIrregularPoints);
	QXBValues = NrmStringToQuark(NhlNtmXBValues);
	QXTValues = NrmStringToQuark(NhlNtmXTValues);
	QYLValues = NrmStringToQuark(NhlNtmYLValues);
	QYRValues = NrmStringToQuark(NhlNtmYRValues);
	QXBMinorValues = NrmStringToQuark(NhlNtmXBMinorValues);
	QXTMinorValues = NrmStringToQuark(NhlNtmXTMinorValues);
	QYLMinorValues = NrmStringToQuark(NhlNtmYLMinorValues);
	QYRMinorValues = NrmStringToQuark(NhlNtmYRMinorValues);
	QXBLabels = NrmStringToQuark(NhlNtmXBLabels);
	QXTLabels = NrmStringToQuark(NhlNtmXTLabels);
	QYLLabels = NrmStringToQuark(NhlNtmYLLabels);
	QYRLabels = NrmStringToQuark(NhlNtmYRLabels);
	QXBFormat = NrmStringToQuark(NhlNtmXBFormat);
	QXTFormat = NrmStringToQuark(NhlNtmXTFormat);
	QYLFormat = NrmStringToQuark(NhlNtmYLFormat);
	QYRFormat = NrmStringToQuark(NhlNtmYRFormat);

	return(NhlNOERROR);
}


/*
 * Function:	TickMarkDraw
 *
 * Description: Draws labels, ticks, grids and borders in that order. Each
 *		of these has special function thats called that actually does
 *		the drawing.
 *
 * In Args:	layer	the actuall instance of the TickMark object
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	GKS state changes 
 */
static NhlErrorTypes	TickMarkDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
	NhlLayer layer;
#endif
{
	NhlTickMarkLayer tlayer = (NhlTickMarkLayer) layer;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes realret = NhlNOERROR;
        float	fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;
	Gint		err_ind;
	Gint		save_linecolor;
	Gint		save_linetype;
	Gdouble		save_linewidth;

	if (! tlayer->tick.x_b_on && ! tlayer->tick.x_t_on &&
	    ! tlayer->tick.y_l_on && ! tlayer->tick.y_r_on  &&
	    ! tlayer->tick.x_b_border_on && ! tlayer->tick.x_t_border_on &&
	    ! tlayer->tick.y_l_border_on && ! tlayer->tick.y_r_border_on)
		return ret;

	if (tlayer->view.use_segments && ! tlayer->tick.new_draw_req &&
	    tlayer->tick.trans_dat &&
	    tlayer->tick.trans_dat->id != NgNOT_A_SEGMENT) {
                ret = _NhlActivateWorkstation(tlayer->base.wkptr);
		if ((realret = MIN(realret,ret)) < NhlWARNING) return realret;
                ret = _NhlDrawSegment(tlayer->tick.trans_dat,
				_NhlWorkstationId(tlayer->base.wkptr));
		if ((realret = MIN(realret,ret)) < NhlWARNING) return realret;
                ret = _NhlDeactivateWorkstation(tlayer->base.wkptr);
		return MIN(ret,realret);
	}
	tlayer->tick.new_draw_req = False;
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	ginq_line_colr_ind(&err_ind, &save_linecolor);
	ginq_linewidth(&err_ind, &save_linewidth);
	ginq_linetype(&err_ind, &save_linetype);

	if (tlayer->view.use_segments) {
		ret = _NhlActivateWorkstation(tlayer->base.wkptr);
		if(ret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkDraw: An error has occurred while activating the workstation, can not continue");
			return(ret);
		}
		if (tlayer->tick.trans_dat != NULL)
			_NhlDeleteViewSegment(layer, tlayer->tick.trans_dat);
		if ((tlayer->tick.trans_dat = 
		     _NhlNewViewSegment(layer)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error opening segment", "TickMarkDraw");
			return(ret);
		}
		ret = _NhlStartSegment(tlayer->tick.trans_dat);
		if ((realret = MIN(ret,realret)) < NhlWARNING)
			return ret;
	}

	ret = DrawLabels(tlayer);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkDraw: NhlFATAL error has occurred while drawing TickMark labels, can not continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	if (! tlayer->view.use_segments) {
		ret = _NhlActivateWorkstation(tlayer->base.wkptr);
		if(ret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkDraw: An error has occurred while activating the workstation, can not continue");
			return(ret);
		}
	}
	ret = DrawTicks(tlayer);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkDraw: NhlFATAL error has occurred while drawing Tick Marks , can not continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = DrawGrid(tlayer);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkDraw: NhlFATAL error has occurred while drawing TickMark grid , can not continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

	ret = DrawBorder(tlayer);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkDraw: NhlFATAL error has occurred while drawing TickMark border, can not continue");
		return(ret);
	}

	if (tlayer->view.use_segments) {
		_NhlEndSegment(tlayer->tick.trans_dat);
	}
	ret = _NhlDeactivateWorkstation(tlayer->base.wkptr);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkDraw: An error has occurred while deactivating the workstation, can not continue");
	}

	gset_line_colr_ind(save_linecolor);
	gset_linewidth(save_linewidth);
	gset_linetype(save_linetype);
	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
	if(ret < realret)
		realret = ret;
	return(realret);
}


/*
 * Function:	DrawGrid
 *
 * Description:	Draw both the major axis grid lines and minor axis grid lines
 *		for both axis if the respective axis is on. For a given axis
 *		if the default side, bottom for x and left for y, is not on
 *		the the tick mark locations are used from the top or right.Other
 *		wise the tick marks extend from the left (bottom) to the 
 *		right(top) tick mark locations for both minor and major
 *
 * In Args:	tlayer	TickMark object instance pointer
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: GKS state changes
 */
static NhlErrorTypes DrawGrid 
#if	NhlNeedProto
(NhlTickMarkLayer tlayer)
#else
(tlayer)
NhlTickMarkLayer tlayer;
#endif
{
	float xr,xl,yt,yb,wr,wl,wt,wb;
	int i,ll;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes ret1 = NhlNOERROR;
	NhlBoolean x_b_on,x_t_on,y_l_on,y_r_on;
/*
 * Since clipping is off we don't care what viewport is set, only that
 * it be inside the viewspace and that the identity trans is in effect.
 * It is more efficient if we do not change the currently set viewport
 */
	c_getset(&xl,&xr,&yb,&yt,&wl,&wr,&wb,&wt,&ll);
	c_set(xl,xr,yb,yt,xl,xr,yb,yt,1);

	xr = tlayer->view.x + tlayer->view.width;
	xl = tlayer->view.x;
	yt = tlayer->view.y;
	yb = tlayer->view.y - tlayer->view.height;

	NhlVASetValues(tlayer->base.wkptr->base.id,
		_NhlNwkReset,	True,
		NULL);

	x_b_on = tlayer->tick.x_b_on && tlayer->tick.x_b_data_valid;
	x_t_on = tlayer->tick.x_t_on && tlayer->tick.x_t_data_valid;
	y_l_on = tlayer->tick.y_l_on && tlayer->tick.y_l_data_valid;
	y_r_on = tlayer->tick.y_r_on && tlayer->tick.y_r_data_valid;
	if(tlayer->tick.x_major_grid){
		if(x_b_on) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.x_major_grid_line_color,
				_NhlNwkDashPattern,tlayer->tick.x_major_grid_line_dash_pattern,
				_NhlNwkLineThicknessF,tlayer->tick.x_major_grid_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.x_b_nmajor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_b_major_ndc_locs[i],yb,1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_b_major_ndc_locs[i],yt,0);
				_NhlWorkstationLineTo(tlayer->base.wkptr,
					1.0,1.0,1);
			}
		} else if(x_t_on) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.x_major_grid_line_color,
				_NhlNwkDashPattern,tlayer->tick.x_major_grid_line_dash_pattern,
				_NhlNwkLineThicknessF,tlayer->tick.x_major_grid_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.x_t_nmajor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_t_major_ndc_locs[i],yb,1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_t_major_ndc_locs[i],yt,0);
			}
			_NhlWorkstationLineTo(tlayer->base.wkptr,1.0,1.0,1);
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DrawGrid: Either XBOn or XTOn must be set in order to draw an XAxis grid");
			ret1 = NhlWARNING;
		}
	}
	if(tlayer->tick.x_minor_grid) {
		if(x_b_on &&(tlayer->tick.x_b_minor_on)) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.x_minor_grid_line_color,
				_NhlNwkDashPattern,tlayer->tick.x_minor_grid_line_dash_pattern,
				_NhlNwkLineThicknessF,tlayer->tick.x_minor_grid_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.x_b_nminor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_b_minor_ndc_locs[i],yb,1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_b_minor_ndc_locs[i],yt,0);
			}
			_NhlWorkstationLineTo(tlayer->base.wkptr,1.0,1.0,1);
		} else if(x_t_on &&(tlayer->tick.x_t_minor_on)){
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.x_minor_grid_line_color,
				_NhlNwkDashPattern,tlayer->tick.x_minor_grid_line_dash_pattern,
				_NhlNwkLineThicknessF,tlayer->tick.x_minor_grid_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.x_t_nminor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_t_minor_ndc_locs[i],yb,1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_t_minor_ndc_locs[i],yt,0);
			}
			_NhlWorkstationLineTo(tlayer->base.wkptr,1.0,1.0,1);
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DrawGrid: Either XBOn or XTOn must be set in order to draw an XAxis minor grid");
			ret1 = NhlWARNING;
		}
	}
	if(tlayer->tick.y_major_grid){
		if(y_l_on) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.y_major_grid_line_color,
				_NhlNwkDashPattern,tlayer->tick.y_major_grid_line_dash_pattern,
				_NhlNwkLineThicknessF,tlayer->tick.y_major_grid_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.y_l_nmajor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,xl,tlayer->tick.y_l_major_ndc_locs[i],1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,xr,tlayer->tick.y_l_major_ndc_locs[i],0);
			}
			_NhlWorkstationLineTo(tlayer->base.wkptr,1.0,1.0,1);
		} else if(y_r_on) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.y_major_grid_line_color,
				_NhlNwkDashPattern,tlayer->tick.y_major_grid_line_dash_pattern,
				_NhlNwkLineThicknessF,tlayer->tick.y_major_grid_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.y_r_nmajor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,xl,tlayer->tick.y_r_major_ndc_locs[i],1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,xr,tlayer->tick.y_r_major_ndc_locs[i],0);
			}
			_NhlWorkstationLineTo(tlayer->base.wkptr,1.0,1.0,1);
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DrawGrid: Either YLOn or YROn must be set in order to draw an YAxis grid");
			ret1 = NhlWARNING;
		}
	}
	if(tlayer->tick.y_minor_grid) {
		if(y_l_on &&(tlayer->tick.y_l_minor_on)) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.y_minor_grid_line_color,
				_NhlNwkDashPattern,tlayer->tick.y_minor_grid_line_dash_pattern,
				_NhlNwkLineThicknessF,tlayer->tick.y_minor_grid_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.y_l_nminor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,xl,tlayer->tick.y_l_minor_ndc_locs[i],1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,xr,tlayer->tick.y_l_minor_ndc_locs[i],0);
			}
			_NhlWorkstationLineTo(tlayer->base.wkptr,1.0,1.0,1);
		} else if(y_r_on &&(tlayer->tick.y_r_minor_on)){
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.y_minor_grid_line_color,
				_NhlNwkDashPattern,tlayer->tick.y_minor_grid_line_dash_pattern,
				_NhlNwkLineThicknessF,tlayer->tick.y_minor_grid_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.y_r_nminor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,xl,tlayer->tick.y_r_minor_ndc_locs[i],1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,xr,tlayer->tick.y_r_minor_ndc_locs[i],0);
			}
			_NhlWorkstationLineTo(tlayer->base.wkptr,1.0,1.0,1);
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DrawGrid: Either YLOn or YROn must be set in order to draw an YAxis minor grid");
			ret1 = NhlWARNING;
		}
	}

	c_plotif(0.0,0.0,2);

	return(MIN(ret,ret1));
}


/*
 * Function:	DrawTicks
 *
 * Description:	Draw both major and minor tick marks for all axis. The
 *		arrays tick.*_*_major_ndc_locs contain all of the locations
 *		and are have allready be checked to make sure they are in
 *		the viewport.
 *
 * In Args:	tlayer	TickMark instance pointer
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: Change in GKS state.
 */
static NhlErrorTypes DrawTicks
#if	NhlNeedProto
(NhlTickMarkLayer tlayer)
#else
(tlayer)
NhlTickMarkLayer tlayer;
#endif
{
	float xr,xl,yt,yb,wr,wl,wt,wb;
	int i,ll;
	NhlErrorTypes ret = NhlNOERROR;
	NhlBoolean x_b_on,x_t_on,y_l_on,y_r_on;


/*
 * Since clipping is off we don't care what viewport is set, only that
 * it be inside the viewspace and that the identity trans is in effect.
 * It is more efficient if we do not change the currently set viewport
 */
	c_getset(&xl,&xr,&yb,&yt,&wl,&wr,&wb,&wt,&ll);
	c_set(xl,xr,yb,yt,xl,xr,yb,yt,1);
	
	xr = tlayer->view.x + tlayer->view.width;
	xl = tlayer->view.x;
	yt = tlayer->view.y;
	yb = tlayer->view.y - tlayer->view.height;

	NhlVASetValues(tlayer->base.wkptr->base.id,
		_NhlNwkReset,	True,
		NULL);

	x_b_on = tlayer->tick.x_b_on && tlayer->tick.x_b_data_valid;
	x_t_on = tlayer->tick.x_t_on && tlayer->tick.x_t_data_valid;
	y_l_on = tlayer->tick.y_l_on && tlayer->tick.y_l_data_valid;
	y_r_on = tlayer->tick.y_r_on && tlayer->tick.y_r_data_valid;

	if(x_b_on) {
		NhlVASetValues(tlayer->base.wkptr->base.id,
			_NhlNwkLineColor,tlayer->tick.x_b_major_line_color,
			_NhlNwkDashPattern,0,
			_NhlNwkLineThicknessF,tlayer->tick.x_b_major_thickness,
			NULL);
		_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
		for(i = 0; i< tlayer->tick.x_b_nmajor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_b_major_ndc_locs[i],
					yb - tlayer->tick.x_b_major_outward_length,1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_b_major_ndc_locs[i],
					(yb+tlayer->tick.x_b_major_length) - tlayer->tick.x_b_major_outward_length,0);

		}
		if(tlayer->tick.x_b_minor_on) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.x_b_minor_line_color,
				_NhlNwkDashPattern,0,
				_NhlNwkLineThicknessF,tlayer->tick.x_b_minor_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.x_b_nminor; i++){
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_b_minor_ndc_locs[i], 
					yb - tlayer->tick.x_b_minor_outward_length,1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_b_minor_ndc_locs[i], 
					(yb + tlayer->tick.x_b_minor_length) - tlayer->tick.x_b_minor_outward_length,0);
			}
		}
	}
	if(x_t_on) {
		NhlVASetValues(tlayer->base.wkptr->base.id,
			_NhlNwkLineColor,tlayer->tick.x_t_major_line_color,
			_NhlNwkDashPattern,0,
			_NhlNwkLineThicknessF,tlayer->tick.x_t_major_thickness,
			NULL);
		_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
		for(i = 0; i< tlayer->tick.x_t_nmajor; i++) {
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_t_major_ndc_locs[i],
					yt + tlayer->tick.x_t_major_outward_length,1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_t_major_ndc_locs[i],
					(yt-tlayer->tick.x_t_major_length) + tlayer->tick.x_t_major_outward_length,0);
		}
		if(tlayer->tick.x_t_minor_on) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.x_t_minor_line_color,
				_NhlNwkDashPattern,0,
				_NhlNwkLineThicknessF,tlayer->tick.x_t_minor_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.x_t_nminor; i++){
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_t_minor_ndc_locs[i], 
					yt + tlayer->tick.x_t_minor_outward_length,1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,tlayer->tick.x_t_minor_ndc_locs[i], 
					(yt - tlayer->tick.x_t_minor_length) + tlayer->tick.x_t_minor_outward_length,0);
			}
		}
	}
	if(y_l_on) {
		NhlVASetValues(tlayer->base.wkptr->base.id,
			_NhlNwkLineColor,tlayer->tick.y_l_major_line_color,
			_NhlNwkDashPattern,0,
			_NhlNwkLineThicknessF,tlayer->tick.y_l_major_thickness,
			NULL);
		_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
		for(i = 0; i< tlayer->tick.y_l_nmajor; i++) {
				_NhlWorkstationLineTo( tlayer->base.wkptr, xl - tlayer->tick.y_l_major_outward_length,
					tlayer->tick.y_l_major_ndc_locs[i],1);
				_NhlWorkstationLineTo( tlayer->base.wkptr,
					(xl+tlayer->tick.y_l_major_length) - tlayer->tick.y_l_major_outward_length,
					tlayer->tick.y_l_major_ndc_locs[i],0);
		}
		if(tlayer->tick.y_l_minor_on) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.y_l_minor_line_color,
				_NhlNwkDashPattern,0,
				_NhlNwkLineThicknessF,tlayer->tick.y_l_minor_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.y_l_nminor; i++){
				_NhlWorkstationLineTo(tlayer->base.wkptr,
					xl - tlayer->tick.y_l_minor_outward_length,
					tlayer->tick.y_l_minor_ndc_locs[i], 
					1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,
					(xl + tlayer->tick.y_l_minor_length) - tlayer->tick.y_l_minor_outward_length,
					tlayer->tick.y_l_minor_ndc_locs[i],0);
			}
		}
	}
	if(y_r_on) {
		NhlVASetValues(tlayer->base.wkptr->base.id,
			_NhlNwkLineColor,tlayer->tick.y_r_major_line_color,
			_NhlNwkDashPattern,0,
			_NhlNwkLineThicknessF,tlayer->tick.y_r_major_thickness,
			NULL);
		_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
		for(i = 0; i< tlayer->tick.y_r_nmajor; i++) {
				_NhlWorkstationLineTo( tlayer->base.wkptr, xr + tlayer->tick.y_r_major_outward_length,
					tlayer->tick.y_r_major_ndc_locs[i],1);
				_NhlWorkstationLineTo( tlayer->base.wkptr,
					(xr-tlayer->tick.y_r_major_length) + tlayer->tick.y_r_major_outward_length,
					tlayer->tick.y_r_major_ndc_locs[i],0);
		}
		if(tlayer->tick.y_r_minor_on) {
			NhlVASetValues(tlayer->base.wkptr->base.id,
				_NhlNwkLineColor,tlayer->tick.y_r_minor_line_color,
				_NhlNwkDashPattern,0,
				_NhlNwkLineThicknessF,tlayer->tick.y_r_minor_thickness,
				NULL);
			_NhlSetLineInfo(tlayer->base.wkptr,(NhlLayer)tlayer);
			for(i = 0; i < tlayer->tick.y_r_nminor; i++){
				_NhlWorkstationLineTo(tlayer->base.wkptr,
					xr + tlayer->tick.y_r_minor_outward_length,
					tlayer->tick.y_r_minor_ndc_locs[i], 
					1);
				_NhlWorkstationLineTo(tlayer->base.wkptr,
					(xr - tlayer->tick.y_r_minor_length) + tlayer->tick.y_r_minor_outward_length,
					tlayer->tick.y_r_minor_ndc_locs[i],0);
			}
		}
	}

	c_plotif(0.0,0.0,2);
	
	return(ret);
}


/*
 * Function:	DrawBorder
 *
 * Description: Draws a line arround the viewport of the tick mark object.
 *		It uses the SPPS functions c_frstpt and c_vector to do the 
 *		drawing so that line joins are smooth.
 *
 * In Args:	tlayer	TickMark instance pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	GKS state changes
 */
static NhlErrorTypes DrawBorder
#if	NhlNeedProto
(NhlTickMarkLayer tlayer)
#else
(tlayer)
	NhlTickMarkLayer tlayer;
#endif
{
	int n;
	float xr,xl,yt,yb,wr,wl,wt,wb;
	int ll;
	NhlErrorTypes ret = NhlNOERROR;

	gset_linewidth((Gdouble)tlayer->tick.border_thickness);
	gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,
                       tlayer->tick.border_line_color));


/*
 * Since clipping is off we don't care what viewport is set, only that
 * it be inside the viewspace and that the identity trans is in effect.
 * It is more efficient if we do not change the currently set viewport
 */

	c_getset(&xl,&xr,&yb,&yt,&wl,&wr,&wb,&wt,&ll);
	c_set(xl,xr,yb,yt,xl,xr,yb,yt,1);

	n = 0;
	if(tlayer->tick.x_b_border_on) {
		c_frstpt(tlayer->view.x + tlayer->view.width,tlayer->view.y - tlayer->view.height);
		c_vector(tlayer->view.x,tlayer->view.y - tlayer->view.height);
		n++;
	}
	if(tlayer->tick.y_l_border_on) {
		if(n>0) {
			c_vector(tlayer->view.x,tlayer->view.y);
			n++;
		} else {
			c_frstpt(tlayer->view.x,tlayer->view.y - tlayer->view.height);
			c_vector(tlayer->view.x,tlayer->view.y);
			n++;
		}
	} else {
		n = 0;
	}
	if(tlayer->tick.x_t_border_on) {
		if(n>0) {
			c_vector(tlayer->view.x + tlayer->view.width,tlayer->view.y);
		} else {
			c_frstpt(tlayer->view.x,tlayer->view.y);
			c_vector(tlayer->view.x + tlayer->view.width,tlayer->view.y);
			n++;
		}
	} else {
		n = 0;
	}
	if(tlayer->tick.y_r_border_on) {
		if(n>0) {
			c_vector(tlayer->view.x +tlayer->view.width,tlayer->view.y - tlayer->view.height);
		} else {
			c_frstpt(tlayer->view.x+tlayer->view.width,tlayer->view.y);
			c_vector(tlayer->view.x +tlayer->view.width,tlayer->view.y - tlayer->view.height);
			n++;
		}
/*
*last one is special case so that line joins are consistent
*/
		if(tlayer->tick.x_b_border_on) {
			c_vector(tlayer->view.x,tlayer->view.y - tlayer->view.height);
		}
	} else {
		n = 0;
	}

	c_plotif(0.0,0.0,2);

	return(ret);
}



/*
 * Function:	DrawLabels
 *
 * Description: Draws tickmark labels for all four axis. Simply calls NhlDraw
 *		on all four MultiText objects. The objects already have 
 *		position and string information configured in the 
 *		ComputeAndSetLabelInfo function.
 *
 * In Args:	tlayer	actual instance record
 *
 * Out Args:	NONE
 *
 * Return Values:  Error Conditions
 *
 * Side Effects:	GKS state  changes
 */
static NhlErrorTypes DrawLabels
#if	NhlNeedProto
(NhlTickMarkLayer tlayer)
#else
(tlayer)
NhlTickMarkLayer tlayer;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes realret = NhlNOERROR;
	NhlBoolean x_b_on,x_t_on,y_l_on,y_r_on;

	x_b_on = tlayer->tick.x_b_on && tlayer->tick.x_b_data_valid;
	x_t_on = tlayer->tick.x_t_on && tlayer->tick.x_t_data_valid;
	y_l_on = tlayer->tick.y_l_on && tlayer->tick.y_l_data_valid;
	y_r_on = tlayer->tick.y_r_on && tlayer->tick.y_r_data_valid;

	if (tlayer->view.use_segments) {
		if(x_b_on && tlayer->tick.x_b_labels_on 
		   && tlayer->tick.xb_multi != NULL) {
			ret = _NhlSegDraw(tlayer->tick.xb_multi);
			if ((ret = MIN(ret,realret))  < NhlWARNING) 
				return(ret);
		}

		if(x_t_on && tlayer->tick.x_t_labels_on 
		   && tlayer->tick.xt_multi != NULL) {
			ret = _NhlSegDraw(tlayer->tick.xt_multi);
			if ((ret = MIN(ret,realret))  < NhlWARNING)
				return(ret);

		}
		if(y_l_on && tlayer->tick.y_l_labels_on
		   && tlayer->tick.yl_multi != NULL) {
			ret = _NhlSegDraw(tlayer->tick.yl_multi);
			if ((ret = MIN(ret,realret))  < NhlWARNING)
				return(ret);
		}
		if(y_r_on && tlayer->tick.y_r_labels_on
		   && tlayer->tick.yr_multi != NULL) {
			ret = _NhlSegDraw(tlayer->tick.yr_multi);
			if ((ret = MIN(ret,realret))  < NhlWARNING)
				return(ret);
		}
	}
	else {
		if(x_b_on && tlayer->tick.x_b_labels_on
		   && tlayer->tick.xb_multi != NULL) {
			ret = NhlDraw(tlayer->tick.xb_multi->base.id);
			if ((ret = MIN(ret,realret))  < NhlWARNING) 
				return(ret);
		}

		if(x_t_on && tlayer->tick.x_t_labels_on
		   && tlayer->tick.xt_multi != NULL) {
			ret = NhlDraw(tlayer->tick.xt_multi->base.id);
			if ((ret = MIN(ret,realret))  < NhlWARNING)
				return(ret);

		}
		if(y_l_on && tlayer->tick.y_l_labels_on
		   && tlayer->tick.yl_multi != NULL) {
			ret = NhlDraw(tlayer->tick.yl_multi->base.id);
			if ((ret = MIN(ret,realret))  < NhlWARNING)
				return(ret);
		}
		if(y_r_on && tlayer->tick.y_r_labels_on
		   && tlayer->tick.yr_multi != NULL) {
			ret = NhlDraw(tlayer->tick.yr_multi->base.id);
			if ((ret = MIN(ret,realret))  < NhlWARNING)
				return(ret);
		}
	}
	return(realret);
}



/*
 * Function:	TickMarkGetBB
 *
 * Description: Calls _NhlGetBB on all four MultiTextObjects. _NhlGetBB
 *		Takes care of merging bounding box info
 *
 * In Args:	instance	Actual tick mark object instance
 *		thebox		Data structure for holding BB info.
 *	
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: Values in thebox possibly changed
 */
/*ARGSUSED*/
static NhlErrorTypes TickMarkGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlTickMarkLayer tinstance = (NhlTickMarkLayer) instance;
	NhlBoolean x_b_on,x_t_on,y_l_on,y_r_on;

	x_b_on = tinstance->tick.x_b_on && tinstance->tick.x_b_data_valid;
	x_t_on = tinstance->tick.x_t_on && tinstance->tick.x_t_data_valid;
	y_l_on = tinstance->tick.y_l_on && tinstance->tick.y_l_data_valid;
	y_r_on = tinstance->tick.y_r_on && tinstance->tick.y_r_data_valid;

	if(x_b_on && (tinstance->tick.xb_multi != NULL)
	   &&(tinstance->tick.x_b_nmajor > 0)&&(tinstance->tick.x_b_labels_on))
		_NhlGetBB(tinstance->tick.xb_multi,thebox);
	if(x_t_on && (tinstance->tick.xt_multi != NULL)
	   &&(tinstance->tick.x_t_nmajor > 0)&&(tinstance->tick.x_t_labels_on))
		_NhlGetBB(tinstance->tick.xt_multi,thebox);
	if(y_l_on && (tinstance->tick.yl_multi != NULL)
	   &&(tinstance->tick.y_l_nmajor > 0)&&(tinstance->tick.y_l_labels_on))
		_NhlGetBB(tinstance->tick.yl_multi,thebox);
	if(y_r_on && (tinstance->tick.yr_multi != NULL)
	   &&(tinstance->tick.y_r_nmajor > 0)&&(tinstance->tick.y_r_labels_on))
		_NhlGetBB(tinstance->tick.yr_multi,thebox);

	if(x_b_on) {
		float ext;

		ext = (tinstance->tick.x_b_major_outward_length > tinstance->tick.x_b_minor_outward_length ? tinstance->tick.x_b_major_outward_length : tinstance->tick.x_b_minor_outward_length);
		if(thebox->b > tinstance->view.y - tinstance->view.height - ext) {
			thebox->b = tinstance->view.y - tinstance->view.height - ext;
		}
	}	
	if(x_t_on) {
		float ext;

		ext = (tinstance->tick.x_t_major_outward_length > tinstance->tick.x_t_minor_outward_length ? tinstance->tick.x_t_major_outward_length : tinstance->tick.x_t_minor_outward_length);
		if(thebox->t < tinstance->view.y + ext) {
			thebox->t = tinstance->view.y + ext;
		}
	}	
	if(y_r_on) {
		float ext;

		ext = (tinstance->tick.y_r_major_outward_length > tinstance->tick.y_r_minor_outward_length ? tinstance->tick.y_r_major_outward_length : tinstance->tick.y_r_minor_outward_length);
		if(thebox->r < tinstance->view.x + tinstance->view.width + ext) {
			thebox->r = tinstance->view.x + tinstance->view.width + ext;
		}
	}	
	if(y_l_on) {
		float ext;

		ext = (tinstance->tick.y_l_major_outward_length > tinstance->tick.y_l_minor_outward_length ? tinstance->tick.y_l_major_outward_length : tinstance->tick.y_l_minor_outward_length);
		if(thebox->l > tinstance->view.x - ext) {
			thebox->l = tinstance->view.x - ext;
		}
	}	
	return(NhlNOERROR);
}

/*
 * Function:	AutoComputeMajorTickMarks
 *
 * Description: Handles computing major tickmark data locations and labels
 *		for NhlAUTOMATIC mode axis. Calls either ChooseLogSpacing or
 *		ChooseLinSpacing to come up with "nice" values for the major
 *		tick mark locations. Next it computes, from the start end 
 *		and spacing info calculated in the Choose function, the
 *		tick mark locations. Then it calls, foreach tick in the 
 *		data window, ConvertToString to get a label. 
 *
 *		The real trick in this function was comming up with a 
 *		compare_precision value. This value is passed to compare when
 *		comparing a tick location with the extent values to make sure
 *		the start or end has or hasn't been passed up.
 *		What I did was to take the cieling of the log of the spacing
 *		divided by the max and the same for min.  This number + 1.0
 *		gives an integer value that represent the number of decimal 
 *		places needed when comparing a value against the start and end.
 *
 * In Args:	style		Requested style
 *		dmin, dmax	data max and min
 *		convert_precision  number of decimal places to appear in labels
 *		cut_off		number of decimal places if exceeded should use
 *				scientific notation to conver values to strings
 *		format		pointer to the format record
 *
 * Out Args:
 *		array		array in which to place major data locs
 *		larray		array in which to place major data labels 
 *		tstart		start of tick marks choosen by Choose function
 *		tend		end of tick marks choosen by Choose function
 *		spacing		choosen spacing
 *		nmajor		number of ticks and labels in array and larray
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes AutoComputeMajorTickMarks
#if	NhlNeedProto
(
	NhlTickMarkStyle  style,
	float *array,
	char** larray,
	int max_ticks,
	float dmax,
	float dmin,
	float *tstart,
	float *tend,
	int convert_precision,
	float *spacing,
	int *nmajor,
	int cutoff,
	NhlFormatRec *format,
	char	func_code,
	int *sug_minor_ticks,
	float min_nonzero
)
#else
(style,array,larray,max_ticks,dmax,dmin,tstart,tend,convert_precision,spacing,nmajor,cutoff,format,func_code,sug_minor_ticks,min_nonzero)
NhlTickMarkStyle  style;
float   *       array;
char	**	larray;
int		max_ticks;
float           dmax;
float           dmin;
float   *       tstart;
float   *       tend;
int		convert_precision;
float   *       spacing;
int     *       nmajor;
int		cutoff;
NhlFormatRec	*format;
char		func_code;
int		*sug_minor_ticks;
float 		min_nonzero;
#endif
{
	int done = 0,i = 0,j = 0;
	float tmploc;
	int compare_precision;
	int cnvt_precision;
	int log_spacing;
	NhlErrorTypes ret = NhlNOERROR;
	float min_compare,max_compare,min,max,absmax;
	int divpwr,sig_digits;
	int left_sig_digit = -10000;
	NhlBoolean zero_min,zero_max;

	switch(style) {
	case NhlLINEAR:	
	case NhlIRREGULAR:
/*
* Can't rember why this is necessary! In fact it seems to cause problems
		*tstart = _NhlRndIt(dmin,convert_precision);
		*tend = _NhlRndIt(dmax,convert_precision);
*/
		*tstart = dmin;
		*tend = dmax;
		ret = ChooseSpacingLin(tstart,tend,spacing,7,max_ticks,sug_minor_ticks,min_nonzero);
		if(ret<NhlWARNING) {
			return(NhlFATAL);
		}
		min = MAX(*tstart,dmin);
		max = MIN(*tend,dmax);
		absmax = MAX(fabs(min),fabs(max));
		ret = _NhlGetScaleInfo(absmax,&divpwr,&sig_digits,"TickMark");
		left_sig_digit = divpwr - 1;
				       
		if(_NhlCmpFAny2(min,0.0,7,min_nonzero) != 0.0) {
                        zero_min = False;
			min_compare = ceil(fabs(log10((double)((*spacing)/
							  fabs(min)))))+1.0;
                }
		else {
			zero_min = True;
			min_compare = 7.0;
		}

		if(_NhlCmpFAny2(max,0.0,7,min_nonzero) != 0.0) {
			zero_max = False;
			max_compare = ceil(fabs(log10((double)((*spacing)/
                                                            fabs(max)))))+1.0;
		}
		else {
			zero_max = True;
			max_compare = 7.0;
		}

		
		if(! zero_min && ! zero_max){
			compare_precision = MAX(min_compare,max_compare);
		} else {
			compare_precision = MIN(min_compare,max_compare);
		}
		if(convert_precision != -1) {
			cnvt_precision = convert_precision;
		} else {
			cnvt_precision = compare_precision;
		}

		if((min_compare > 7) &&(max_compare > 7)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"AutoComputeMajorTickMarks: min and max are so close together or so far apart that arithmetic error may cause problems, proceed at own risk");
			ret = NhlWARNING;
		}
		if((cnvt_precision < max_compare)&&(cnvt_precision < min_compare)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"AutoComputeMajorTickMarks: The precision specified is smaller than the precision needed tick mark labels may not be correct");
			ret = NhlWARNING;
 
		}

		while(!done) {
			tmploc = *tstart + i * *spacing;
			if((_NhlCmpFAny2(tmploc,min,7,min_nonzero/*min_compare*/)>=0.0)
				&&(_NhlCmpFAny2(tmploc,max,7,min_nonzero/*max_compare*/) <= 0.0)) {
				array[j] = tmploc;
				larray[j] = ConvertToString(array[j],cnvt_precision,compare_precision,NhlLINEAR,cutoff,left_sig_digit,format,func_code);
				j++;
				if(j == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"AutoComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			} else if(_NhlCmpFAny2(tmploc,max,7,min_nonzero/*max_compare*/) >0.0) {
				done = 1;
			} 
			i++;
		}
		*nmajor = j;
		break;
	case NhlLOG:
		if(convert_precision > 0) {
			*tstart = _NhlRndIt(dmin,convert_precision);
			*tend = _NhlRndIt(dmax,convert_precision);
		} else {
			*tstart = dmin;
			*tend = dmax;
		} 
		ret = ChooseSpacingLog(tstart,tend,spacing,5,max_ticks,sug_minor_ticks,min_nonzero);
		if((dmin <=0.0)||(dmax <=0.0)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"AutoComputeMajorTickMarks: data min or data max is less than or equal to zero, can not continue");
			return(NhlFATAL);
		}
/*
* The following makes sure that the minimum value is not compare to zero and
* found to be equal to zero;
*/
		cnvt_precision = compare_precision = 5;
/*
* tstart and tend are returned as exponents!!!
*/
		log_spacing = (int)_NhlRndIt(*spacing,2);
		while(!done) {
			tmploc = *tstart + i;
			if((_NhlCmpFAny2(tmploc,log10(dmin),compare_precision,min_nonzero)>=0.0)
				&&(_NhlCmpFAny2(tmploc,log10(dmax),compare_precision,min_nonzero) <= 0.0)) {
				array[j] = pow(10.0,tmploc);
				if(i%log_spacing == 0) {
					larray[j] = ConvertToString(array[j],cnvt_precision,compare_precision,NhlLOG,cutoff,left_sig_digit,format,func_code);
				} else {
					larray[j] = NULL;
				}
				j++;
				if(j == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"AutoComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear incomplete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			} else if(_NhlCmpFAny2(tmploc,log10(dmax),compare_precision,min_nonzero) >0.0) {
				done = 1;
			} 
			i++;
		}
		*tstart = pow(10.0,*tstart);
		*tend = pow(10.0,*tend);
		*nmajor = j;
		break;
	case NhlTIME:
	case NhlGEOGRAPHIC:
	default:
		break;
	}
	return(ret);
}

/*
 * Function:	ManualComputeMajorTickMarks
 *
 * Description: Handles computing major tickmark data locations and labels
 *		for NhlMANUAL mode axis.  Uses user supplied values for start
 *		end and spacing to compute all tick locations.
 *
 *		The real trick in this function was comming up with a 
 *		compare_precision value. This value is passed to compare when
 *		comparing a tick location with the extent values to make sure
 *		the start or end has or hasn't been passed up.
 *		What I did was to take the cieling of the log of the spacing
 *		divided by the max and the same for min.  This number + 1.0
 *		gives an integer value that represent the number of decimal 
 *		places needed when comparing a value against the start and end.
 *
 * In Args:	style		Requested style
 *		tstart		start of tick marks provided by user 
 *		tend		end of tick marks provided by user 
 *		spacing		spacing provided by user
 *		dmin, dmax	data max and min
 *		convert_precision  number of decimal places to appear in labels
 *		cut_off		number of decimal places if exceeded should use
 *				scientific notation to conver values to strings
 *		spacing_type    not used yet.
 *		format		pointer to the format record
 *
 * Out Args:
 *		array		array in which to place major data locs
 *		larray		array in which to place major data labels 
 *		nmajor		number of ticks and labels in array and larray
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
/*ARGSUSED*/
static NhlErrorTypes ManualComputeMajorTickMarks
#if	NhlNeedProto
(
	NhlTickMarkStyle style,
	float *array,
	char** larray,
	float dmax,
	float dmin,
	float tstart,
	float tend,
	int convert_precision,
	NhlBoolean auto_precision,
	float spacing,
	int spacing_type,
	int *nmajor,
	int cutoff,
	NhlFormatRec *format,
	char func_code,
	float	min_nonzero
)
#else
(style,array,larray,dmax,dmin,tstart,tend,convert_precision,auto_precision,spacing,spacing_type,nmajor,cutoff,format,func_code,min_nonzero)
NhlTickMarkStyle  style;
float   *       array;
char	** 	larray;
float           dmax;
float           dmin;
float           tstart;
float           tend;
int		convert_precision;
NhlBoolean	auto_precision;
float           spacing;
int             spacing_type;
int     *       nmajor;
int		cutoff;
NhlFormatRec	*format;
char		func_code;
float		min_nonzero;
#endif
{
	int done = 0,i=0,j=0;
	float min,max,tmploc;
	float min_compare,max_compare;
	int compare_precision;
	int cnvt_precision;
	int log_spacing;
	NhlErrorTypes ret = NhlNOERROR;
	float absmax;
	int divpwr,sig_digits,tmpi;
	int left_sig_digit = -10000;
	NhlBoolean zero_min,zero_max;

	min = dmin <= tstart ? tstart : dmin;

	max = dmax > tend ? tend : dmax;

	
	switch(style) {
	case NhlLOG:
		if((min <=0.0)||(max <=0.0)||(tstart <= 0.0)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"ManualComputeMajorTickMarks: Either data min, data max , tick start or tick end is less than or equal to 0.0, cannot continue");
			return(NhlFATAL);
		}
		min = log10(min);
		max = log10(max);
		tstart = log10(tstart);
		cnvt_precision = compare_precision = 4 /*ceil(fabs(tstart))*/;
/*
* CheckLog should have made sure spacing is valid
*/
/*
* Convert spacing to int so % operator can be used to determine weather to
* label or not
*/
		log_spacing = (int) _NhlRndIt(spacing,2);
		while(!done) {
			tmploc = tstart + i;
			if((_NhlCmpFAny2(tmploc,min,compare_precision,min_nonzero)>=0.0)
				&&(_NhlCmpFAny2(tmploc,max,compare_precision,min_nonzero) <= 0.0)) {
				array[j] = pow(10.0,tmploc);
				if(i%log_spacing == 0) {
					larray[j] = ConvertToString(array[j],cnvt_precision,compare_precision,NhlLOG,cutoff,left_sig_digit,format,func_code);
				} else {	
					larray[j] = NULL;
				}
				j++;
				if(j == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			} else if(_NhlCmpFAny2(tmploc,max,compare_precision,min_nonzero) >0.0) {
				done = 1;
			} 
			i++;
		}
		*nmajor = j;
		break;
	case NhlLINEAR:
	case NhlIRREGULAR:
		absmax = MAX(fabs(min),fabs(max));
		ret = _NhlGetScaleInfo(absmax,&divpwr,&sig_digits,"TickMark");
		left_sig_digit = divpwr - 1;

		if(spacing <= 0.0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"ManualComputeTickMarks: Spacing is less than or equal to zero, this should not happen, can not continue");
			return(NhlFATAL);
		}
		ret = _NhlGetScaleInfo(spacing,&divpwr,&sig_digits,"TickMark");
		tmpi = left_sig_digit - (divpwr - 1);

		sig_digits = MIN(sig_digits,convert_precision);
		sig_digits = MAX(sig_digits,sig_digits + tmpi);

		if(_NhlCmpFAny2(min,0.0,7,min_nonzero) != 0.0) {
                        zero_min = False;
			min_compare = ceil(fabs(log10((double)(spacing/
							   fabs(min)))))+1.0;
		}
		else {
			zero_min = True;
			min_compare = 7.0;
		}

		if(_NhlCmpFAny2(max,0.0,7,min_nonzero) != 0.0) {
			zero_max = False;
			max_compare = ceil(fabs(log10((double)(spacing/
							    fabs(max)))))+1.0;
		}
		else {
			zero_max = False;
			max_compare = 7.0;
		}

		if(! zero_min && ! zero_max){
			compare_precision = MAX(min_compare,max_compare);
		} else {
			compare_precision = MIN(min_compare,max_compare);
		}
		compare_precision = MAX(sig_digits,compare_precision);
		if(! auto_precision) {
			cnvt_precision = convert_precision;
		} else {
			cnvt_precision = compare_precision;
		}

		if((min_compare > 7)&&(max_compare > 7)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajorTickMarks: min and max are so close together or so far apart that arithmetic error may cause problems, proceed at own risk");
			ret = NhlWARNING;
		}
		if((cnvt_precision < max_compare)&&(cnvt_precision < min_compare)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajorTickMarks: The precision specified is smaller than the precision needed tick mark labels may not be correct");
			ret = NhlWARNING;
 
		}
		
		while(!done) {
			tmploc = tstart + i * spacing;
			if((_NhlCmpFAny2(tmploc,min,7,min_nonzero/*min_compare*/)>=0.0)
				&&(_NhlCmpFAny2(tmploc,max,7,min_nonzero/*max_compare*/)) <= 0.0) {
				array[j] = tmploc;
				larray[j] = ConvertToString(array[j],cnvt_precision,compare_precision,NhlLINEAR,cutoff,left_sig_digit,format,func_code);
				j++;
				if(j == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			} else if(_NhlCmpFAny2(tmploc,max,7,min_nonzero /*max_compare*/) >0.0) {
				done = 1;
			} 
			i++;
		}
		*nmajor = j;
		break;
	case NhlTIME:
	case NhlGEOGRAPHIC:
	default:
		break;
	}
	return(ret);
}

/*
 * Function:	ExplicitComputeMajorTickMarks
 *
 * Description: This function still has same compare precision problems 
 *		as the other functions even though all tick locations are 
 *		provided by the user. This is the case because tick marks stil
 *		need to be filter out if they do not lie in viewport. This
 *		function just copies values from *_*_labels and *_*_values to
 *		*_*_major_labels and *_*_major_data_locs. The compare precision
 *		is determined hueristically by taking tstart+tend/n_requested
 *
 * In Args:	style	Requested tick mark style
 *		dmax	Minimum data value
 *		dmin	Maximum data value
 *		tstart	Minimum starting tick location
 *		tend	Maximum starting tick location
 *		convert_precision    ***unused***	
 *		requested_points	array of requested points (*_*_values)
 *		requested_labels	array of requested points (*_*_labels)
 *		n_requested		number of elements in requested arrays.
 *		
 *
 * Out Args:
 *		array	array in which tick mark locations are placed.
 *		larray	array in which tick mark labels are placed.
 *		nmajor	number of elements assigned in array.
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes ExplicitComputeMajorTickMarks
#if	NhlNeedProto
(
	NhlTickMarkStyle style,
	float *array,
	char** larray,
	float dmax,
	float dmin,
	float *tstart,
	float *tend,
	int convert_precision,
	NhlGenArray req_points,
	NhlGenArray req_labels,
	int *nmajor,
	float min_nonzero
	)
#else
(style,array,larray,dmax,dmin,tstart,tend,convert_precision,
req_points,req_labels,nmajor,min_nonzero)
NhlTickMarkStyle  style;
float   *       array;
char	**	larray;
float           dmax;
float           dmin;
float           *tstart;
float           *tend;
int		convert_precision;
NhlGenArray 	req_points;
NhlGenArray 	req_labels;
int     *       nmajor;
float 		min_nonzero;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	float min,max;
	int i,k;
	int n_req_points;
	char 	**labels;
	float 	*points;
	NhlBoolean do_labels = True;
#ifdef	NOTUSED
	int	spacing_estimate,min_compare;
#endif

	n_req_points = req_points ? req_points->num_elements : 0;
	if (! req_labels || req_labels->num_elements < n_req_points)
		do_labels = False;
	points = req_points ? (float *) req_points->data : NULL;
	labels = req_labels ? (char **) req_labels->data : NULL;

	switch(style) {
	case NhlLOG:
	case NhlLINEAR:
	case NhlIRREGULAR:
		max = dmax;
		min = dmin;
#ifdef	NOTUSED
		spacing_estimate = (max - min)/n_requested_points;
		if(_NhlCmpFAny2(min,0.0,7,min_nonzero) != 0.0) {
			min_compare = ceil(fabs(log10((double)(spacing_estimate/fabs(min)))))+1.0;
		else 
			min_compare = 7;

		if(_NhlCmpFAny2(max,0.0,7,min_nonzero) != 0.0) {
			max_compare = ceil(fabs(log10((double)(spacing_estimate/fabs(max)))))+1.0;
		else 
			max_compare = 7;
#endif
		k = 0;
		for(i = 0; i< n_req_points; i++) {
			if((_NhlCmpFAny2(points[i],min,7,min_nonzero) >= 0.0) && 
			   (_NhlCmpFAny2(points[i],max,7,min_nonzero) <= 0.0)) {
				array[k] = points[i];
				if (do_labels)
					larray[k] = labels[i];
				k++;
				if(k == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"ExplicitComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			}
			
		}
		if (k == 0) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"ExplicitComputeMajorTickMarks: no explicit values are within the data coordinate range; tickmarks will not appear");
			ret = NhlINFO;
			*nmajor = k;
			*tstart = min;
			*tend = max;
			break;
		}

		*nmajor = k;
		*tstart = MIN(array[0],array[k-1]);
		*tend = MAX(array[0],array[k-1]);
		break;
	case NhlTIME:
	case NhlGEOGRAPHIC:
	default:
		NhlPError(NhlWARNING,NhlEUNKNOWN,"ExplicitComputeMajorTickMarks: NhlTIME and NhlGEOGRAPHIC explicit tick mark styles are not supported yet");
		ret = NhlWARNING;
		break;
	}
	return(ret);
}

static NhlErrorTypes ExplicitComputeMinorTickMarks
#if	NhlNeedProto
(NhlTickMarkStyle style, float *array, float dmax, float dmin, float tstart, float tend, int convert_precision, float *requested_points, int* nminor, int  n_requested,float min_nonzero )
#else
(style, array, dmax, dmin, tstart, tend, convert_precision, requested_points, nminor, n_requested, min_nonzero )
NhlTickMarkStyle style;
float *array;
float dmax;
float dmin;
float tstart;
float tend;
int convert_precision;
float *requested_points;
int  *nminor;
int  n_requested;
float min_nonzero;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	float min,max;
	int i,k;

	switch(style) {
	case NhlLOG:
	case NhlLINEAR:
	case NhlIRREGULAR:
		max = dmax;
		min = dmin;
		k = 0;
		for(i = 0; i< n_requested; i++) {
			if((_NhlCmpFAny2(requested_points[i],min,7,min_nonzero/*min_compare*/) >= 0.0) && (_NhlCmpFAny2(requested_points[i],max,7,min_nonzero /*max_compare*/) <= 0.0)) {
				array[k] = requested_points[i];
				k++;
				if(k == MAXMINORTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"ExplicitComputeMinorTickMarks: Maximum minor tickmarks (%d) has been reached, tickmarks may appear incomplete",MAXMINORTICKS);
					ret = NhlWARNING;
					break;	
				}
			}
			
		}
		*nminor = k;
		break;
	case NhlTIME:
	case NhlGEOGRAPHIC:
	default:
		NhlPError(NhlWARNING,NhlEUNKNOWN,"ExplicitComputeMajorTickMarks: NhlTIME and NhlGEOGRAPHIC explicit tick mark styles are not supported yet");
		ret = NhlWARNING;
		break;
	}
	return ret;
}
	


/*
 * Function:	ChooseSpacingLin
 *
 * Description: Used to compute "nice" values for tick mark start,  end and
 *		spacing.
 *		A complicated algorythm that I barrowed from some work I did
 *		in 1989 for Rod Frielich at CIRES. Basicly ChooseSpacingLIn
 *		has a static table of "nice" dividing values like 1,2,3,4,5.
 *		The range of the tstart and end values is detmined by taking
 *		the floor of the log of the differece. Values from the table
 *		are multiplied by the 10**<result of differnce eqn> to come
 *		up with a trial spacing. Next "nice" start and end values are
 *		computed by taking ceil of max/trial spacing  and multipling
 *		by the trial spacing this give an "even" starting location.
 *		The same is done for the min with floor instead of ceiling.
 *		Next the differnec between the new min and max is divided
 *		by the trial_spacing. if this is less than max_ticks it has
 *		a valid value. However it continues to see if there is another
 *		value that is less than the current spacing but closer to
 *		max_ticks. When this is all done it returns.
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
(float *tstart,float *tend,float *spacing,int convert_precision,int max_ticks,int *sug_minor_ticks,float min_nonzero)
#else
(tstart,tend,spacing,convert_precision,max_ticks,sug_minor_ticks,min_nonzero)
float *tstart;
float *tend;
float *spacing;
int	convert_precision;
int	max_ticks;
int	*sug_minor_ticks;
float   min_nonzero;
#endif
{
	double table[10],d,u,t,am1,am2=0.0,ax1,ax2=0.0;
	int mtab[10];
	int	npts,i,ind;

	table[0] = 1.0; 
	table[1] = 2.0; 
	table[2] = 3.0; 
	table[3] = 4.0; 
	table[4] = 5.0; 
	table[5] = 10.0;
	table[6] = 20.0;
	table[7] = 30.0;
	table[8] = 40.0;
	table[9] = 50.0;
	mtab[0] = 4;
	mtab[1] = 3;
	mtab[2] = 2;
	mtab[3] = 3;
	mtab[4] = 4;
	mtab[5] = 4;
	mtab[6] = 3;
	mtab[7] = 2;
	mtab[8] = 3;
	mtab[9] = 4;
	npts = 10;

	if(_NhlCmpFAny2(*tend,*tstart,8,min_nonzero)<=0.0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ChooseSpacingLin: An impossible condition has been detected, TickEnd and TickStart are nearly equal, cannot continue");
		return(NhlFATAL);
	}

	d = pow(10.0,floor(log10(*tend-*tstart)) - 1.0);
	u = *spacing = 1e30;
	for(i=0;i<npts; i++) {
		t = table[i] * d;
		am1 = floor(*tstart/t) *t;
		ax1 = ceil(*tend/t) * t;
		if(((i>=npts-1)&&(*spacing == u))||((t <= *spacing)
			&&(_NhlCmpFAny2((ax1-am1)/t,(double)max_ticks,convert_precision,min_nonzero) <= 0.0))){
			*spacing = t;
			ax2 = ax1;
			am2 = am1;
			ind = i;
		}
	}
	*tstart = am2;
	*tend = ax2;
	*sug_minor_ticks = mtab[ind];
	return(NhlNOERROR);
	
}

/*
 * Function:	ChooseSpacingLog
 *
 * Description: See description of ChooseSpacingLin. Essentially same alg with
 *		mods for handling log plots
 *
 * In Args:     tstart  requested tick starting location
 *              tend    requested tick ending location
 *              convert_precision  precision used to compare with
 *              max_ticks the maximum number of ticks to be choosen
 * Out Args:
 *              tstart  new start
 *              tend    new end
 *              spacing new spacing
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes ChooseSpacingLog
#if	NhlNeedProto
(float *tstart,float *tend,float *spacing,int convert_precision,
 int max_ticks,int* sug_minor_ticks,float min_nonzero)
#else
(tstart,tend,spacing,convert_precision,max_ticks,sug_minor_ticks,min_nonzero)
float *tstart;
float *tend;
float *spacing;
int	convert_precision;
int	max_ticks;
int 	*sug_minor_ticks;
float   min_nonzero;
#endif
{
	double table[10],d,u,t,am1,am2=0.0,ax1,ax2=0.0;
	int	npts,i;

	table[0] = 1.0;
	table[1] = 2.0;
	table[2] = 3.0;
	table[3] = 4.0;
	table[4] = 5.0;
	table[5] = 10.0;
	npts = 6;

	*sug_minor_ticks = 4;

	if((*tstart <= 0.0)||(*tend <= 0.0)||((*tend-*tstart)<=0.0)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ChooseSpacingLog: An internal error that should not have occurred has been detected, can not continue");
		return(NhlFATAL);
	}

	*tstart = (float)log10(*tstart);
	*tend= (float)log10(*tend);

	d = pow(10.0,floor(log10(*tend-*tstart)) - 1.0);
	u = *spacing = 1e30;
	for(i=0;i<npts; i++) {
		t = table[i] * d;
		am1 = floor(*tstart/t) *t;
		ax1 = ceil(*tend/t) * t;
		if(((i>=npts-1)&&(*spacing == u))||((t <= *spacing)
			&&(_NhlCmpFAny2((ax1-am1)/t,(double)max_ticks,convert_precision,min_nonzero) <= 0.0))){
			*spacing = t;
			ax2 = ax1;
			am2 = am1;
		}
	}
	*tstart = (float)floor((double)am2);
	*tend = (float)ceil((double)ax2);
	if(*spacing < 1.0) {
		*spacing = 1.0;
	}
	return(NhlNOERROR);
	
}

/*
 * Function: 	ConvertToString
 *
 * Description: Converts floating point numbers to strings given a precision
 *		style and scientific notation cutoff. For Log plots it has
 *		to encode plotchar function codes so that super script number
 *		are possible. For NhlLINEAR and NhlIRREGULAR it just creates a
 *		sprintf format string with the precision being the number
 *		after the period (%.<precision>f).  The sprintf function is
 *		relied upon to do the rounding.
 *
 * In Args: 	value	floating point value
 *		convert_precision	number of digits to convert to
 *		compare_precision	precision used when comparing two nums
 *		style			type of string to generate
 *		cutoff			when to swicth to sci. note.
 *
 * Out Args: 	NONE
 *
 * Return Values: The converted string.
 *
 * Side Effects: NONE
 */
static char *ConvertToString
#if	NhlNeedProto
(
	float		value,
	int		convert_precision,
	int		compare_precision,
	NhlTickMarkStyle	style,
	int		cutoff,
	int		left_sig_digit,
	NhlFormatRec	*format,
	char		func_code
)
#else
(value,convert_precision,
 compare_precision,style,cutoff,left_sig_digit,format,func_code)
float		value;
int		convert_precision;
int		compare_precision;
NhlTickMarkStyle	style;
int		cutoff;
int		left_sig_digit;
NhlFormatRec	*format;
char		func_code;
#endif
{
	char *tmpc;
	NhlFormatRec *frec;
	char *buf;

	if ((frec = _NhlScanFString(format->fstring,"TickMark")) == NULL) {
		char * e_text = "%s: internal error getting format";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,"TickMark");
		return(NULL);
	}

	switch(style) {	
	case NhlLOG:
		if (frec->exp_type_flag == NhlffUNSPECED)
			frec->exp_type = NhlffSUPERSCRIPT;
		frec->field_type = NhlffEXPONENTIALFLD;
		frec->zero = False;
		
		buf = _NhlFormatFloat(frec,value,NULL,NULL,&left_sig_digit,
				      NULL,&cutoff,NULL,func_code,"TickMark");
		tmpc = (char*)NhlMalloc((unsigned)strlen(buf)+1);
		strcpy(tmpc,buf);
		return(tmpc);
	case NhlLINEAR:
	case NhlIRREGULAR:
		if (frec->sig_digits_flag == NhlffUNSPECED)
			frec->sig_digits_flag = NhlffDYNAMIC;
		if (frec->exp_switch_flag == NhlffUNSPECED)
			frec->exp_switch_flag = NhlffDYNAMIC;
		if (frec->left_sig_digit_flag == NhlffUNSPECED)
			frec->left_sig_digit_flag = NhlffDYNAMIC;
		buf = _NhlFormatFloat(frec,value,NULL,&convert_precision,
				      &left_sig_digit,
				      NULL,&cutoff,NULL,func_code,"TickMark");
		tmpc = (char*) NhlMalloc((unsigned)strlen(buf)+1);
		strcpy(tmpc,buf);
		return(tmpc);
	case NhlTIME:
		break;
	case NhlGEOGRAPHIC:
		break;
	}
	return NULL;
}

/*
 * Function:	ComputeMinorTickMarks
 *
 * Description: Used to compute minor tick mark locations for axis using 
 *		NhlLOG, NhlLINEAR, or NhlIRREGULAR styles and in NhlAUTOMATIC or NhlMANUAL
 *		mode. NhlEXPLICIT plots don't have minor tickmarks. Minor NhlLOG
 *		style tick marks have the special requirment that the number
 *		of them be 1,4,or 8. Other values would produce weird results.
 *		NhlLOG minor tick marks were tricky because the absolute value
 *		of the minor tick spacing varies for each pair of major tick
 *		marks.
 * Modified:    6/98 to chose "nice" minor tickmarks when minor_per_major is never
 *              set.
 *		
 *
 * In Args:
 *		int minorpermajor,	number of minor ticks per major ticks , or -1
 *		float spacing,		major tick mark spacing
 *		float tstart,		tick mark starting
 *		float tend,		tick mark ending
 *		float dmax,		maximum data value
 *		float dmin,		minimum data value
 *		float* m_locs,		major tick mark locations
 *		int nmajor,		total numberr of major tick marks
 *		int precision,		precision used to compare two nums
 *		NhlTickMarkStyle style	style of tick marks
 *
 * Out Args:	minor_locs		locations of minor tick marks
 *		nminor		total number of minor tick marks.
 *
 * Return Values: Error Conditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes ComputeMinorTickMarks
#if	NhlNeedProto
(
	int minorpermajor,
	float spacing,
	float tstart,
	float tend,
	float dmax,
	float dmin,
	float* m_locs,
	int nmajor,
	float* minor_locs,
	int* nminor,
	int precision,
	NhlTickMarkStyle style,
	float min_nonzero
)
#else
(minorpermajor,spacing,tstart,tend,dmax,dmin,m_locs,nmajor,minor_locs, nminor,precision,style,min_nonzero)
	int minorpermajor;
	float	spacing;
	float	tstart;
	float	tend;
	float 	dmax;
	float 	dmin;
	float	*m_locs;
	int	nmajor;
	float	*minor_locs;
	int	*nminor;
	int	precision;
	NhlTickMarkStyle	style;
	float	min_nonzero;
#endif
{
	float minor_spacing,tmp;
	int i,j,k;
	int compare_precision = precision;
	float min,max,min2,min_compare,max_compare;
	float logminor;
	NhlErrorTypes ret = NhlNOERROR;

	if (_NhlCmpFAny2(dmin,dmax,7,min_nonzero) == 0.0) {
		*nminor = 0;
		return NhlNOERROR;
	}

	switch(style) {
		case NhlTIME:
		case NhlGEOGRAPHIC:
			break;
		case NhlLOG:
/*
* Log minor tickmarks are a problem. You just can have as many as you want
* and expect people to be able to figure out the data locations as you can 
* with linear. There for only 1, 4 and 8 minor ticks will be allowed. 
* CheckLog function guarentees nminor is 1, 4 or 8.
* also at issues is what does a spacing of 2 or more for major tickmarks mean?
* It is obviously wrong to put minor ticks that span more that one decade in.
*/	

/*
* Nice value is 4 for log ticks so I just set it
* totally different procedure for linear though
*/
			if(minorpermajor == 1.0) {
				logminor = (float)log10(5.0);
			} else if(minorpermajor == 4.0){
				logminor = (float)log10(2.0);
			} else  {
				logminor = 0.0;
			}
			if(minorpermajor == 0) {
				*nminor = 0;
				return(ret);
			}
/*
* Although checked for earlier it doesn't hurt to check again
*/
			if(MAX(tstart,dmin) <= 0.0) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeMinorTicks: A value less than or equal to zero has been set as minimum data or TickStart when NhlLOG style is set, can not compute minor tickmarks");
				return(NhlFATAL);
			}
			if(MIN(tend,dmax) <= 0.0) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeMinorTicks: A value less than or equal to zero has been set as maximum data or TickEnd when NhlLOG style is set, can not compute minor tickmarks");
				return(NhlFATAL);
			}

			min = (float)log10(MAX(tstart,dmin));
			max = (float)log10(MIN(tend,dmax));
			k = 0;
			if(nmajor > 0 ) {
			if(nmajor > 1) {
				for(i = nmajor -1; i >0; i--) {
					minor_spacing = (float)floor(
						log10((double)
						(m_locs[i]-m_locs[i-1]))) 
						+ logminor; 
					j = 1;
					tmp = m_locs[i] - j * 
						pow(10.0,minor_spacing);
					while(_NhlCmpFAny2(tmp,m_locs[i-1],precision,min_nonzero) > 0.0){
						minor_locs[k] = tmp;
						k++;
						j++;
						tmp = m_locs[i] - j 
						      *pow(10.0,minor_spacing);
					}
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
				if(_NhlCmpFAny2(pow(10.0,max),m_locs[nmajor-1],
					compare_precision,min_nonzero) > 0.0) {
					minor_spacing = floor(log10(
						m_locs[nmajor-1]))  
						+ logminor;
					j = 1;
					tmp = pow(10.0,floor(log10(
						m_locs[nmajor-1])) + 1.0) - j * 
						pow(10.0,minor_spacing);
					while(_NhlCmpFAny2(tmp,m_locs[nmajor-1],
						precision,min_nonzero) > 0.0) {
						if(_NhlCmpFAny2(tmp,pow(10.0,max),
							precision,min_nonzero) < 0.0) {
							minor_locs[k] = tmp;
							k++;
						} 
						j++;
						tmp = pow(10.0,floor(
							log10(m_locs[nmajor-1]))
							+ 1.0) - j * 
							pow(10.0,minor_spacing);
						if(k == MAXMINORTICKS) {
							*nminor = k-1;
							NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
							return(NhlWARNING);
						}

					}
				}
			} else if(pow(10.0,ceil(max)) != m_locs[0]) {
				minor_spacing = (float)floor(
						log10((double)pow(10.0,
						ceil( max)) - m_locs[0]))
						+ logminor;
				j = 1;
				tmp = pow(10.0,ceil(max)) - j * 
					pow(10.0,minor_spacing);
				while(_NhlCmpFAny2(tmp,m_locs[0],precision,min_nonzero) > 0.0){
					if(_NhlCmpFAny2(tmp,pow(10.0,max),8,min_nonzero) < 0.0) {
						minor_locs[k] = tmp;
						k++;
					}
					j++;
					tmp = pow(10.0,ceil(max)) - j 
					      *pow(10.0,minor_spacing);
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
				
			}
			if(_NhlCmpFAny2(log10(m_locs[0]),min, 8,min_nonzero) > 0.0) {
				minor_spacing = log10(m_locs[0]);
				minor_spacing = ceil(minor_spacing);
				minor_spacing -= 1.0;
				minor_spacing += logminor;
				j = 1;
				tmp = m_locs[0] - j * pow(10.0,minor_spacing);
                                while(_NhlCmpFAny2(tmp,pow(10.0,min),precision,min_nonzero) > 0.0){
                                        minor_locs[k] = tmp;
                                        k++;
                                        j++;
                                        tmp = m_locs[0] - j * pow(10.0,minor_spacing);
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
                                }

			}
			*nminor = k;
			} else {
				min = (float)log10(MAX(tstart,dmin));
				max = (float)log10(MIN(tend,dmax));
				minor_spacing = (float)floor(min) + logminor;
/*
(float)floor(log10(pow(10.0,max)-pow(10.0,min)))+logminor;
*/
				j = 1;
				k= 0;
				tmp = pow(10.0,ceil(max)) - j *pow(10.0,minor_spacing);
				while(_NhlCmpFAny2(tmp,pow(10.0,min),precision,min_nonzero) >0.0) {
					if(_NhlCmpFAny2(tmp,pow(10.0,max),precision,min_nonzero) < 0.0) {
						minor_locs[k] = tmp;
						k++;
					}
					j++;
					tmp = pow(10.0,ceil(max)) - j *pow(10.0,minor_spacing);
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
				*nminor = k;

				
			} 

				
			break;
		case NhlLINEAR:
		case NhlIRREGULAR:
			if(minorpermajor == 0) {
				*nminor = 0;
				return(ret);
			}
			min = MAX(tstart,dmin);
			max = MIN(tend,dmax);
/*
* I've determined that this is not a good way to figure the compare precision
* out
			tmp = log10(spacing);
			if(tmp < 0.0)
				compare_precision = (int)ceil(fabs(tmp));
			else 
				compare_precision = precision;
*
*/
			minor_spacing = _NhlRndIt(spacing / (float)
					(minorpermajor+1),7);

			if(min!= 0.0)
				min_compare = ceil(fabs(log10((double)(minor_spacing/fabs(min)))))
				+1.0;
			else 
				min_compare = 7.0;

			if(max!= 0.0)
				max_compare = ceil(fabs(log10((double)(minor_spacing/fabs(max)))))
				+1.0;
			else 
				max_compare = 7.0;

			compare_precision = MAX(min_compare,max_compare);
			if((min_compare > 7)&&(max_compare> 7)) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: The distance between major tickmarks is so close or so far apart that arithmetic error may cause problems determining the accurate locations of minor tickmarks");
				ret = NhlWARNING;
			}

/*
* case where ticks start some where after the viewport corner
*/
			if(nmajor > 0) {
			k = 0;
			if(_NhlCmpFAny2(min,m_locs[0],7,min_nonzero/*min_compare*/) < 0.0) {	
				j = 1;
				tmp = m_locs[0] - j * minor_spacing;
				while(_NhlCmpFAny2(min,tmp,7,min_nonzero/*min_compare*/)<=0.0) {
					minor_locs[k] = tmp;
					k++;
					j++;
					tmp = m_locs[0] - j *minor_spacing;
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
			}
			for( i = 0 ; i < nmajor - 1; i++){
				for(j = 1; j <= minorpermajor ; j++) {
					minor_locs[k] = m_locs[i] + j 
							* minor_spacing;
					k++;
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
			}
			if(_NhlCmpFAny2(max,m_locs[nmajor-1],7,min_nonzero/*compare_precision*/) > 0.0) {
				j = 1;
				tmp = m_locs[nmajor-1] + j * minor_spacing;
				while(_NhlCmpFAny2(max,tmp,7,min_nonzero/*max_compare*/)>= 0.0){
					minor_locs[k] = tmp;
					k++;
					j++;
					tmp = m_locs[nmajor-1] + j * minor_spacing;
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
			}
			*nminor = k;
			} else {
				k = 0;
				i = 0;
				while( _NhlCmpFAny2(i*spacing + tstart,max,7,min_nonzero/*max_compare*/) < 0.0) {
					i++;
				}
				min2 = (i-1)*spacing + tstart;
				j = 1;
				tmp = min2 + j *minor_spacing;
				while(_NhlCmpFAny2(max,tmp,7,min_nonzero/*max_compare*/) >=0.0){
					if(_NhlCmpFAny2(min,tmp,7,min_nonzero/*min_compare*/) < 0.0) {
					minor_locs[k] = tmp;
					k++;
					}
					j++;
					tmp = min2 + j * minor_spacing;
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
				*nminor = k;
			}
			break;
		default:
			break;
	}
	return(ret);
}


/*
 * Function:	SetTop
 *
 * Description: Copies values from bottom resource to top resources. Only
 *		following resources are affected:
 *		tmXTStyle, tmXTMode, tmXTMinorPerMajor, tmXTNoMinor, 
 *		tmXTDataLeftF, tmDataRightF, tmTickStartF, tmTickEndF, 
 *		tmXTMaxTicks, tmXTTickSpacing, tmXTSpacingType, tmXTValues, 
 *		tmXTNumValues, tmXTLabels, tmXTMajorThicknessF, 
 *		tmXTMajorLineColor, tmXTMajorLengthF, tmXTMajorOutwardLength, 
 *		tmXTLabelFont, tmXTLabelFontHeightF, tmXTLabelFontColor, 
 *		tmXTLabelFontAspectF, tmXTLabelAngleF, tmXTLabelDirection, 
 *		tmXTLabelDelta, tmXTLabelPrecision, tmXTIrregularPoints, 
 *		tmXTLabelStride.tmXTPrescision, tmXTNumIrregularPoints
 *		tmXTIrrTensionF, tmXTLabelFuncCode
 *	        All other Top TickMark resources are unaffected.
 *
 * In Args: 	tnew	TickMark object instance pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	NONE
 *
 * Side Effects:	NONE
 */
static void SetTop
#if	NhlNeedProto
(NhlTickMarkLayer tnew)
#else
(tnew)
	NhlTickMarkLayer tnew;
#endif
{

	tnew->tick.x_t_style = tnew->tick.x_b_style;
	tnew->tick.x_t_mode = tnew->tick.x_b_mode;
	tnew->tick.x_t_minor_per_major = tnew->tick.x_b_minor_per_major;
	tnew->tick.x_t_minor_on = tnew->tick.x_b_minor_on;
	tnew->tick.x_t_data_left = tnew->tick.x_b_data_left;
	tnew->tick.x_t_data_right = tnew->tick.x_b_data_right;
	tnew->tick.x_t_tick_start_set = tnew->tick.x_b_tick_start_set;
	tnew->tick.x_t_tick_start = tnew->tick.x_b_tick_start;
	tnew->tick.x_t_tick_end_set = tnew->tick.x_b_tick_end_set;
	tnew->tick.x_t_tick_end = tnew->tick.x_b_tick_end;
	tnew->tick.x_t_max_ticks = tnew->tick.x_b_max_ticks;
	tnew->tick.x_t_tick_spacing = tnew->tick.x_b_tick_spacing;
	tnew->tick.x_t_spacing_type = tnew->tick.x_b_spacing_type;
	tnew->tick.x_t_values = tnew->tick.x_b_values;
	tnew->tick.x_t_minor_values = tnew->tick.x_b_minor_values;
	tnew->tick.x_t_labels = tnew->tick.x_b_labels;
	tnew->tick.x_t_major_thickness = tnew->tick.x_b_major_thickness;
	tnew->tick.x_t_major_line_color = tnew->tick.x_b_major_line_color;
	tnew->tick.x_t_major_length_set = tnew->tick.x_b_major_length_set;
	tnew->tick.x_t_major_length = tnew->tick.x_b_major_length;
	tnew->tick.x_t_major_outward_length_set = tnew->tick.x_b_major_outward_length_set;
	tnew->tick.x_t_major_outward_length = tnew->tick.x_b_major_outward_length;
	tnew->tick.x_t_minor_thickness = tnew->tick.x_b_minor_thickness;
	tnew->tick.x_t_minor_line_color = tnew->tick.x_b_minor_line_color;
	tnew->tick.x_t_minor_length_set = tnew->tick.x_b_minor_length_set;
	tnew->tick.x_t_minor_length = tnew->tick.x_b_minor_length;
	tnew->tick.x_t_minor_outward_length_set = tnew->tick.x_b_minor_outward_length_set;
	tnew->tick.x_t_minor_outward_length = tnew->tick.x_b_minor_outward_length;
	tnew->tick.x_t_label_font = tnew->tick.x_b_label_font;
	tnew->tick.x_t_label_font_height_set = tnew->tick.x_b_label_font_height_set;
	tnew->tick.x_t_label_font_height = tnew->tick.x_b_label_font_height;
	tnew->tick.x_t_label_font_color = tnew->tick.x_b_label_font_color;
       	tnew->tick.x_t_label_font_aspect = tnew->tick.x_b_label_font_aspect;
       	tnew->tick.x_t_label_angle = tnew->tick.x_b_label_angle;
	tnew->tick.x_t_label_direction = tnew->tick.x_b_label_direction;
	tnew->tick.x_t_label_fcode = tnew->tick.x_b_label_fcode;
	tnew->tick.x_t_label_font_thickness = 
		tnew->tick.x_b_label_font_thickness;
	tnew->tick.x_t_label_font_quality = tnew->tick.x_b_label_font_quality;
	tnew->tick.x_t_label_constant_spacing = 
		tnew->tick.x_b_label_constant_spacing;
	tnew->tick.x_t_label_delta = tnew->tick.x_b_label_delta;
	tnew->tick.x_t_precision_set = tnew->tick.x_b_precision_set;
	tnew->tick.x_t_precision = tnew->tick.x_b_precision;
	tnew->tick.x_t_auto_precision = tnew->tick.x_b_auto_precision;
	tnew->tick.x_t_format = tnew->tick.x_b_format;
	tnew->tick.x_t_irregular_points = tnew->tick.x_b_irregular_points;
	tnew->tick.x_t_label_stride = tnew->tick.x_b_label_stride;
	tnew->tick.x_t_tension = tnew->tick.x_b_tension;

}

/*
 * Function:	SetRight
 *
 * Description: Copies values from left resource to right resources. Only
 *		following resources are affected:
 *		tmYRStyle, tmYRMode, tmYRMinorPerMajor, tmYRNoMinor, 
 *		tmYRDataLeftF, tmYRDataRightF, tmYRTickStartF, tmYRTickEndF, 
 *		tmYRMaxTicks, tmYRTickSpacing, tmYRSpacingType, tmYRValues, 
 *		tmYRNumValues, tmYRLabels, tmYRMajorThicknessF, 
 *		tmYRMajorLineColor, tmYRMajorLengthF, tmYRMajorOutwardLength, 
 *		tmYRLabelFont, tmYRLabelFontHeightF, tmYRLabelFontColor, 
 *		tmYRLabelFontAspectF, tmYRLabelAngleF, tmYRLabelDirection, 
 *		tmYRLabelDelta, tmYRLabelPrecision, tmYRIrregularPoints, 
 *		tmYRLabelStride, tmYRNumIrregularPoints.
 *		tmYRIrrTensionF,tmYRLabelFuncCode
 *	        All other right TickMark resources are unaffected.
 *
 * In Args: 	tnew	TickMark object instance pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	NONE
 *
 * Side Effects:	NONE
 *
 */
static void SetRight
#if	NhlNeedProto
(NhlTickMarkLayer	tnew)
#else
(tnew)
NhlTickMarkLayer	tnew;
#endif
{
	tnew->tick.y_r_style = tnew->tick.y_l_style;
	tnew->tick.y_r_mode = tnew->tick.y_l_mode;
	tnew->tick.y_r_minor_per_major = tnew->tick.y_l_minor_per_major;
	tnew->tick.y_r_minor_on = tnew->tick.y_l_minor_on;
	tnew->tick.y_r_data_top = tnew->tick.y_l_data_top;
	tnew->tick.y_r_data_bottom = tnew->tick.y_l_data_bottom;
	tnew->tick.y_r_tick_start_set = tnew->tick.y_l_tick_start_set;
	tnew->tick.y_r_tick_start = tnew->tick.y_l_tick_start;
	tnew->tick.y_r_tick_end_set = tnew->tick.y_l_tick_end_set;
	tnew->tick.y_r_tick_end = tnew->tick.y_l_tick_end;
	tnew->tick.y_r_max_ticks = tnew->tick.y_l_max_ticks;
	tnew->tick.y_r_tick_spacing = tnew->tick.y_l_tick_spacing;
	tnew->tick.y_r_spacing_type = tnew->tick.y_l_spacing_type;
	tnew->tick.y_r_values = tnew->tick.y_l_values;
	tnew->tick.y_r_minor_values = tnew->tick.y_l_minor_values;
	tnew->tick.y_r_labels = tnew->tick.y_l_labels;
	tnew->tick.y_r_major_thickness = tnew->tick.y_l_major_thickness;tnew->tick.y_r_major_line_color = tnew->tick.y_l_major_line_color;
	tnew->tick.y_r_major_length_set = tnew->tick.y_l_major_length_set;
	tnew->tick.y_r_major_length = tnew->tick.y_l_major_length;
	tnew->tick.y_r_major_outward_length = tnew->tick.y_l_major_outward_length;
	tnew->tick.y_r_major_outward_length_set = tnew->tick.y_l_major_outward_length_set;
	tnew->tick.y_r_minor_thickness = tnew->tick.y_l_minor_thickness;
	tnew->tick.y_r_minor_line_color = tnew->tick.y_l_minor_line_color;
	tnew->tick.y_r_minor_length_set = tnew->tick.y_l_minor_length_set;
	tnew->tick.y_r_minor_length = tnew->tick.y_l_minor_length;
	tnew->tick.y_r_minor_outward_length_set = tnew->tick.y_l_minor_outward_length_set;
	tnew->tick.y_r_minor_outward_length = tnew->tick.y_l_minor_outward_length;
	tnew->tick.y_r_label_font = tnew->tick.y_l_label_font;
	tnew->tick.y_r_label_font_height_set = tnew->tick.y_l_label_font_height_set;
	tnew->tick.y_r_label_font_height = tnew->tick.y_l_label_font_height;
	tnew->tick.y_r_label_font_color = tnew->tick.y_l_label_font_color;
	tnew->tick.y_r_label_font_aspect = tnew->tick.y_l_label_font_aspect;
	tnew->tick.y_r_label_angle = tnew->tick.y_l_label_angle;
	tnew->tick.y_r_label_direction = tnew->tick.y_l_label_direction;
	tnew->tick.y_r_label_font_thickness = 
		tnew->tick.y_l_label_font_thickness;
	tnew->tick.y_r_label_font_quality = tnew->tick.y_l_label_font_quality;
	tnew->tick.y_r_label_constant_spacing = 
		tnew->tick.y_l_label_constant_spacing;
	tnew->tick.y_r_label_fcode = tnew->tick.y_l_label_fcode;
	tnew->tick.y_r_label_delta = tnew->tick.y_l_label_delta;
	tnew->tick.y_r_precision_set = tnew->tick.y_l_precision_set;
	tnew->tick.y_r_precision = tnew->tick.y_l_precision;
	tnew->tick.y_r_auto_precision = tnew->tick.y_l_auto_precision;
	tnew->tick.y_r_format = tnew->tick.y_l_format;
	tnew->tick.y_r_irregular_points = tnew->tick.y_l_irregular_points;
	tnew->tick.y_r_label_stride = tnew->tick.y_l_label_stride;
	tnew->tick.y_r_tension = tnew->tick.y_l_tension;
}

/*
 * Function:	SetFormatRec
 *
 * Description: sets up the printing format record for a tickmark side
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
		format->fstring = NhltmDEF_FORMAT;
	}
	if ((frec = _NhlScanFString(format->fstring,entry_name)) == NULL) {
		e_text = "%s: error in format string for %s: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,resource);
		ret = MIN(ret,NhlWARNING);
		format->fstring = NhltmDEF_FORMAT;
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
 * TickMark object, make a copy. 
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
 * Function:	CheckKeyVals
 *
 * Description: Makes sure data extents set and precision are correct.
 *		also checks the format strings
 *
 * In Args:	tnew		new instance pointer
 *		told		old instance pointer
 *		c_or_s		flag set to SET for SetValues and CREATE for
 *				Intialize.
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckKeyVals
#if	NhlNeedProto
(NhlTickMarkLayer tnew,NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer	tnew;
	NhlTickMarkLayer	told;
	int		c_or_s;
#endif
{
	char *error_lead;
	char *e_text;
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	

	if(c_or_s == SET) {
		error_lead = "TickMarkSetValues";
	} else {
		error_lead = "TickMarkInitialize";
	}
        if(tnew->tick.x_b_precision <= 0) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Precision resource for bottom ticks is less than or equal to zero, defaulting to 1",error_lead);
                tnew->tick.x_b_precision = 1;
		ret = MIN(ret,NhlWARNING);
        }
	if (tnew->tick.x_b_precision_set)
		tnew->tick.x_b_auto_precision = False;

        if(tnew->tick.x_t_precision <= 0) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Precision resource for top ticks is less than or equal to zero, defaulting to 1",error_lead);
                tnew->tick.x_t_precision = 1;
		ret = MIN(ret,NhlWARNING);
        }
	if (tnew->tick.x_t_precision_set)
		tnew->tick.x_t_auto_precision = False;

        if(tnew->tick.y_l_precision <= 0) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Precision resource for left ticks is less than or equal to zero, defaulting to 1",error_lead);
                tnew->tick.y_l_precision = 1;
		ret = MIN(ret,NhlWARNING);
        }
	if (tnew->tick.y_l_precision_set)
		tnew->tick.y_l_auto_precision = False;

        if(tnew->tick.y_r_precision <= 0) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Precision resource for right ticks is less than or equal to zero, defaulting to 1",error_lead);
                tnew->tick.y_r_precision = 1;
		ret = MIN(ret,NhlWARNING);
        }
	if (tnew->tick.y_r_precision_set)
		tnew->tick.y_r_auto_precision = False;

	tnew->tick.x_b_min_nonzero = MIN(10e-7,0.01 *
		fabs(MIN(tnew->tick.x_b_data_right,tnew->tick.x_b_data_left)));
	tnew->tick.x_b_min_nonzero = 
		MIN(tnew->tick.x_b_min_nonzero,0.01 *
		    fabs(tnew->tick.x_b_data_right-tnew->tick.x_b_data_left));
	tnew->tick.x_b_min_nonzero = 
		MAX(_NhlMIN_NONZERO,tnew->tick.x_b_min_nonzero);

	tnew->tick.x_b_data_valid = True;
	if (_NhlCmpFAny2(tnew->tick.x_b_data_left,
			tnew->tick.x_b_data_right,7,_NhlMIN_NONZERO) == 0.0) {
		tnew->tick.x_b_data_valid = False;
		e_text = 
	       "%s: Zero span between %s and %s, turning bottom tickmarks off";
                NhlPError(NhlWARNING,NhlEZEROSPAN,e_text,error_lead,
			  NhlNtmXBDataLeftF,NhlNtmXBDataRightF);
	}
	tnew->tick.x_t_min_nonzero = MIN(10e-7,0.01 *
		fabs(MIN(tnew->tick.x_t_data_right,tnew->tick.x_t_data_left)));
	tnew->tick.x_t_min_nonzero = 
		MIN(tnew->tick.x_t_min_nonzero,0.01 *
		    fabs(tnew->tick.x_t_data_right-tnew->tick.x_t_data_left));
	tnew->tick.x_t_min_nonzero = 
		MAX(_NhlMIN_NONZERO,tnew->tick.x_t_min_nonzero);
	tnew->tick.x_t_data_valid = True;
	if (_NhlCmpFAny2(tnew->tick.x_t_data_left,
			tnew->tick.x_t_data_right,7,_NhlMIN_NONZERO) == 0.0) {
		tnew->tick.x_t_data_valid = False;
		e_text = 
		 "%s: Zero span between %s and %s, turning top tickmarks off";
                NhlPError(NhlWARNING,NhlEZEROSPAN,e_text,error_lead,
			  NhlNtmXTDataLeftF,NhlNtmXTDataRightF);
	}
	tnew->tick.y_l_min_nonzero = MIN(10e-7,0.01 *
		fabs(MIN(tnew->tick.y_l_data_top,tnew->tick.y_l_data_bottom)));
	tnew->tick.y_l_min_nonzero = 
		MIN(tnew->tick.y_l_min_nonzero,0.01 *
		    fabs(tnew->tick.y_l_data_top-tnew->tick.y_l_data_bottom));
	tnew->tick.y_l_min_nonzero = 
		MAX(_NhlMIN_NONZERO,tnew->tick.y_l_min_nonzero);
	tnew->tick.y_l_data_valid = True;
	if (_NhlCmpFAny2(tnew->tick.y_l_data_bottom,
			tnew->tick.y_l_data_top,7,_NhlMIN_NONZERO) == 0.0) {
		tnew->tick.y_l_data_valid = False;
		e_text = 
	      "%s: Zero span between %s and %s, turning left tickmarks off";
                NhlPError(NhlWARNING,NhlEZEROSPAN,e_text,error_lead,
			  NhlNtmYLDataBottomF,NhlNtmYLDataTopF);
	}
	tnew->tick.y_r_min_nonzero = MIN(10e-7,0.01 *
		fabs(MIN(tnew->tick.y_r_data_top,tnew->tick.y_r_data_bottom)));
	tnew->tick.y_r_min_nonzero = 
		MIN(tnew->tick.y_r_min_nonzero,0.01 *
		    fabs(tnew->tick.y_r_data_top-tnew->tick.y_r_data_bottom));
	tnew->tick.y_r_min_nonzero = 
		MAX(_NhlMIN_NONZERO,tnew->tick.y_r_min_nonzero);
	tnew->tick.y_r_data_valid = True;
	if (_NhlCmpFAny2(tnew->tick.y_r_data_bottom,
			tnew->tick.y_r_data_top,7,_NhlMIN_NONZERO) == 0.0) {
		tnew->tick.y_r_data_valid = False;
		e_text = 
	        "%s: Zero span between %s and %s, turning right tickmarks off";
                NhlPError(NhlWARNING,NhlEZEROSPAN,e_text,error_lead,
			  NhlNtmYRDataBottomF,NhlNtmYRDataTopF);
	}
		
	if(!tnew->tick.x_b_minor_on) {
		tnew->tick.x_b_nminor = 0;
	}
	if(!tnew->tick.x_t_minor_on) {
		tnew->tick.x_t_nminor = 0;
	}
	if(!tnew->tick.y_l_minor_on) {
		tnew->tick.y_l_nminor = 0;
	}
	if(!tnew->tick.y_r_minor_on) {
		tnew->tick.y_r_nminor = 0;
	}
	if (c_or_s == CREATE) {
		subret = SetFormatRec(&tnew->tick.x_b_format,
				      NhlNtmXBFormat,error_lead);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = SetFormatRec(&tnew->tick.x_t_format,
				      NhlNtmXTFormat,error_lead);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = SetFormatRec(&tnew->tick.y_l_format,
				      NhlNtmYLFormat,error_lead);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = SetFormatRec(&tnew->tick.y_r_format,
				      NhlNtmYRFormat,error_lead);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		return ret;
	}
	if (tnew->tick.x_b_format.fstring != told->tick.x_b_format.fstring) {
		subret = SetFormatRec(&tnew->tick.x_b_format,
				      NhlNtmXBFormat,error_lead);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (told->tick.x_b_format.fstring != NULL)
			NhlFree(told->tick.x_b_format.fstring);
		told->tick.x_b_format.fstring = NULL;
	}
	if (tnew->tick.x_t_format.fstring != told->tick.x_t_format.fstring) {
		subret = SetFormatRec(&tnew->tick.x_t_format,
				      NhlNtmXTFormat,error_lead);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (told->tick.x_t_format.fstring != NULL)
			NhlFree(told->tick.x_t_format.fstring);
		told->tick.x_t_format.fstring = NULL;
	}
	if (tnew->tick.y_l_format.fstring != told->tick.y_l_format.fstring) {
		subret = SetFormatRec(&tnew->tick.y_l_format,
				      NhlNtmYLFormat,error_lead);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (told->tick.y_l_format.fstring != NULL)
			NhlFree(told->tick.y_l_format.fstring);
		told->tick.y_l_format.fstring = NULL;
	}
	if (tnew->tick.y_r_format.fstring != told->tick.y_r_format.fstring) {
		subret = SetFormatRec(&tnew->tick.y_r_format,
				      NhlNtmYRFormat,error_lead);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (told->tick.y_r_format.fstring != NULL)
			NhlFree(told->tick.y_r_format.fstring);
		told->tick.y_r_format.fstring = NULL;
	}
	
	return(ret);
}

/*
 * Function:	CheckManual
 *
 * Description: When axis in NhlMANUAL mode checks to make sure important values
 *		such as tick start end and spacing are set.
 *
 * In Args:	tnew		new instance record
 *		told		old instance record
 *		c_or_s		flag SET means SetValues is being called.
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckManual
#if	NhlNeedProto
(NhlTickMarkLayer	tnew, NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer	tnew;
	NhlTickMarkLayer	told;
	int		c_or_s;
#endif
{
	char *error_lead;
	NhlErrorTypes ret = NhlNOERROR;

	if(c_or_s == SET) {
		error_lead = "TickMarkSetValues";
	} else {
		error_lead = "TickMarkInitialize";
	}

        if(tnew->tick.x_b_mode == NhlMANUAL){
		if (! tnew->tick.x_b_tick_start_set) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"%s: %s not set for manual mode, using minimimum of %s and %s",
				  error_lead,NhlNtmXBTickStartF,
				  NhlNtmXBDataLeftF,NhlNtmXBDataRightF);
                        tnew->tick.x_b_tick_start = 
				MIN(tnew->tick.x_b_data_left,
				    tnew->tick.x_b_data_right);
			ret = MIN(ret,NhlINFO);
		}
		if (! tnew->tick.x_b_tick_end_set) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"%s: %s not set for manual mode, using maximum of %s and %s",
				  error_lead,NhlNtmXBTickEndF,
				  NhlNtmXBDataLeftF,NhlNtmXBDataRightF);
                        tnew->tick.x_b_tick_end = 
				MAX(tnew->tick.x_b_data_left,
				    tnew->tick.x_b_data_right);
			ret = MIN(ret,NhlINFO);
		}
                if(tnew->tick.x_b_tick_spacing == 0.0){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: A tick mark spacing must be assigned for manual mode, defaulting bottom ticks to automatic",error_lead);
                        tnew->tick.x_b_mode = NhlAUTOMATIC;
			ret = MIN(ret,NhlWARNING);
                }
/*
* Always use positive spacing and determine if needs to be negative latter
*/
                tnew->tick.x_b_tick_spacing = (float)
                                fabs((double)tnew->tick.x_b_tick_spacing);
        }

        if(tnew->tick.x_t_mode == NhlMANUAL){
		if (! tnew->tick.x_t_tick_start_set) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"%s: %s not set for manual mode, using minimimum of %s and %s",
				  error_lead,NhlNtmXTTickStartF,
				  NhlNtmXTDataLeftF,NhlNtmXTDataRightF);
                        tnew->tick.x_t_tick_start = 
				MIN(tnew->tick.x_t_data_left,
				    tnew->tick.x_t_data_right);
			ret = MIN(ret,NhlINFO);
		}
		if (! tnew->tick.x_t_tick_end_set) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"%s: %s not set for manual mode, using maximum of %s and %s",
				  error_lead,NhlNtmXTTickEndF,
				  NhlNtmXTDataLeftF,NhlNtmXTDataRightF);
                        tnew->tick.x_t_tick_end = 
				MAX(tnew->tick.x_t_data_left,
				    tnew->tick.x_t_data_right);
			ret = MIN(ret,NhlINFO);
		}
                if(tnew->tick.x_t_tick_spacing == 0.0){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: A tick mark spacing must be assigned for manual mode, defaulting top ticks to automatic",error_lead);
                        tnew->tick.x_t_mode = NhlAUTOMATIC;
			ret = MIN(ret,NhlWARNING);
                }
/*
* Always use positive spacing and determine if needs to be negative latter
*/
                tnew->tick.x_t_tick_spacing = (float)
                                fabs((double)tnew->tick.x_t_tick_spacing);
        }



        if(tnew->tick.y_l_mode == NhlMANUAL){
		if (! tnew->tick.y_l_tick_start_set) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"%s: %s not set for manual mode, using minimimum of %s and %s",
				  error_lead,NhlNtmYLTickStartF,
				  NhlNtmYLDataBottomF,NhlNtmYLDataTopF);
                        tnew->tick.y_l_tick_start = 
				MIN(tnew->tick.y_l_data_bottom,
				    tnew->tick.y_l_data_top);
			ret = MIN(ret,NhlINFO);
		}
		if (! tnew->tick.y_l_tick_end_set) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"%s: %s not set for manual mode, using maximum of %s and %s",
				  error_lead,NhlNtmYLTickEndF,
				  NhlNtmYLDataBottomF,NhlNtmYLDataTopF);
                        tnew->tick.y_l_tick_end = 
				MAX(tnew->tick.y_l_data_bottom,
				    tnew->tick.y_l_data_top);
			ret = MIN(ret,NhlINFO);
		}
                if(tnew->tick.y_l_tick_spacing == 0.0){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: A tick mark spacing must be assigned for manual mode, defaulting left ticks to automatic",error_lead);
			ret = MIN(ret,NhlWARNING);
                        tnew->tick.y_l_mode = NhlAUTOMATIC;
                }
/*
* Always use positive spacing and determine if needs to be negative latter
*/
                tnew->tick.y_l_tick_spacing = (float)
                                fabs((double)tnew->tick.y_l_tick_spacing);
        }
        if(tnew->tick.y_r_mode == NhlMANUAL){
		if (! tnew->tick.y_r_tick_start_set) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"%s: %s not set for manual mode, using minimimum of %s and %s",
				  error_lead,NhlNtmYRTickStartF,
				  NhlNtmYRDataBottomF,NhlNtmYRDataTopF);
                        tnew->tick.y_r_tick_start = 
				MIN(tnew->tick.y_r_data_bottom,
				    tnew->tick.y_r_data_top);
			ret = MIN(ret,NhlINFO);
		}
		if (! tnew->tick.y_r_tick_end_set) {
			NhlPError(NhlINFO,NhlEUNKNOWN,"%s: %s not set for manual mode, using maximum of %s and %s",
				  error_lead,NhlNtmYRTickEndF,
				  NhlNtmYRDataBottomF,NhlNtmYRDataTopF);
                        tnew->tick.y_r_tick_end = 
				MAX(tnew->tick.y_r_data_bottom,
				    tnew->tick.y_r_data_top);
			ret = MIN(ret,NhlINFO);
		}
                if(tnew->tick.y_r_tick_spacing == 0.0){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: A tick mark spacing must be assigned for manual mode, defaulting right ticks to automatic",error_lead);
			ret = MIN(ret,NhlWARNING);
                        tnew->tick.y_r_mode = NhlAUTOMATIC;
                }
/*
* Always use positive spacing and determine if needs to be negative latter
*/
                tnew->tick.y_r_tick_spacing = (float)
                                fabs((double)tnew->tick.y_r_tick_spacing);
        }
	return(ret);
}


/*
 * Function:	CheckNotAuto
 *
 * Description: Checks values when instance is not in NhlAUTOMATIC mode.
 *
 * In Args:	tnew	new instance
 *		told	old instance
 *		c_or_s	SET means SetValues is caller
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckNotAuto
#if	NhlNeedProto
(NhlTickMarkLayer  tnew,NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	int	c_or_s;
#endif
{
	float tmp;
	char *error_lead;
	NhlErrorTypes ret = NhlNOERROR;

	if(c_or_s == CREATE) {
		error_lead = "TickMarkInitialize";
	} else {
		error_lead = "TickMarkSetValues";
	}

        if(tnew->tick.x_b_mode != NhlAUTOMATIC) {
                if(tnew->tick.x_b_tick_start == tnew->tick.x_b_tick_end) {
                        tnew->tick.x_b_tick_start = MIN(tnew->tick.x_b_data_left						,tnew->tick.x_b_data_right);
                        tnew->tick.x_b_tick_end = MAX(tnew->tick.x_b_data_left,
                                                tnew->tick.x_b_data_right);
                }
                if(tnew->tick.x_b_tick_start > tnew->tick.x_b_tick_end) {
                        NhlPError(NhlINFO,NhlEUNKNOWN,"%s: XBTickStart must be less than XBTickMarkEnd, swapping XBTickStart and XBTickEnd",error_lead);
                        tmp = tnew->tick.x_b_tick_start;
                        tnew->tick.x_b_tick_start = tnew->tick.x_b_tick_end;
                        tnew->tick.x_b_tick_end = tmp;
			ret = MIN(ret,NhlINFO);
                }
        }
        if(tnew->tick.x_t_mode != NhlAUTOMATIC) {
                if(tnew->tick.x_t_tick_start == tnew->tick.x_t_tick_end) {
                        tnew->tick.x_t_tick_start = MIN(tnew->tick.x_t_data_left						,tnew->tick.x_t_data_right);
                        tnew->tick.x_t_tick_end = MAX(tnew->tick.x_t_data_left,
                                                tnew->tick.x_t_data_right);
                }
                if(tnew->tick.x_t_tick_start > tnew->tick.x_t_tick_end) {
                        NhlPError(NhlINFO,NhlEUNKNOWN,"%s: XTTickStart must be less than XTTickMarkEnd, swapping XTTickStart and XTTickEnd",error_lead);
                        tmp = tnew->tick.x_t_tick_start;
                        tnew->tick.x_t_tick_start = tnew->tick.x_t_tick_end;
                        tnew->tick.x_t_tick_end = tmp;
			ret = MIN(ret,NhlINFO);
                }
        }
        if(tnew->tick.y_r_mode != NhlAUTOMATIC) {
                if(tnew->tick.y_r_tick_start == tnew->tick.y_r_tick_end) {
                        tnew->tick.y_r_tick_start = MIN(tnew->tick.y_r_data_top
                                                ,tnew->tick.y_r_data_bottom);
                        tnew->tick.y_r_tick_end = MAX(tnew->tick.y_r_data_top,
                                                tnew->tick.y_r_data_bottom);
                }
                if(tnew->tick.y_r_tick_start > tnew->tick.y_r_tick_end) {
                        NhlPError(NhlINFO,NhlEUNKNOWN,"%s: YRTickStart must be less than YRTickMarkEnd, swapping YRTickStart and YRTickEnd",error_lead);
                        tmp = tnew->tick.y_r_tick_start;
                        tnew->tick.y_r_tick_start = tnew->tick.y_r_tick_end;
                        tnew->tick.y_r_tick_end = tmp;
			ret = MIN(ret,NhlINFO);
                }
        }
        if(tnew->tick.y_l_mode != NhlAUTOMATIC) {
                if(tnew->tick.y_l_tick_start == tnew->tick.y_l_tick_end) {
                        tnew->tick.y_r_tick_start = MIN(tnew->tick.y_r_data_top
                                                ,tnew->tick.y_r_data_bottom);
                        tnew->tick.y_r_tick_end = MAX(tnew->tick.y_r_data_top,
                                                tnew->tick.y_r_data_bottom);
                }
                if(tnew->tick.y_l_tick_start > tnew->tick.y_l_tick_end) {
                        NhlPError(NhlINFO,NhlEUNKNOWN,"%s: YLTickStart must be less than YLTickMarkEnd, swapping YLTickStart and YLTickEnd",error_lead);
                        tmp = tnew->tick.y_l_tick_start;
                        tnew->tick.y_l_tick_start = tnew->tick.y_l_tick_end;
                        tnew->tick.y_l_tick_end = tmp;
			ret = MIN(ret,NhlINFO);
                }
        }
	return(ret);
}

/*
 * Function:	CopyLabelArray
 *
 * Description:	copies an array of strings, padding the end with NULL's to
 *		make it at least the size of the second gen array passed in.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlGenArray
 * Side Effect:	
 */
NhlGenArray
CopyLabelArray
#if	NhlNeedProto
(
	NhlGenArray	oldarr,
	NhlGenArray	minarr
)
#else
(oldarr,minarr)
	NhlGenArray	oldarr;
	NhlGenArray	minarr;
#endif
{
	int		i,size = 0;
	NhlGenArray	gen;
	NhlString	*otable;
	NhlString	*ntable;

	if(oldarr == NULL)
		return NULL;

	if(minarr != NULL)
		size = MAX(size,minarr->num_elements);

	gen = _NhlCopyGenArray(oldarr,False);
	if(gen == NULL)
		return NULL;

	size = MAX(size,gen->num_elements);

	gen->data = NhlMalloc(sizeof(NhlString)*size);
	if(gen->data == NULL)
		return NULL;

	gen->my_data = True;

	otable = oldarr->data;
	ntable = gen->data;

	for(i=0;i < size; i++){
		if((i >= oldarr->num_elements) || (otable[i] == NULL)){
			ntable[i] = NULL;
		}
		else{
			ntable[i] = NhlMalloc(strlen(otable[i])+1);
			if(ntable[i] == NULL)
				return NULL;
			strcpy(ntable[i],otable[i]);
		}
	}

	return gen;
}

/*
 * Function:	CheckExplicit
 *
 * Description: Takes care of copying arrays of strings and values used for
 *		Explicit tick mark placement. Gives error if problems with
 *		these resources are detected.
 *
 * In Args:	tnew	new instance
 *		told	old instance if c_or_s == SET
 *		c_or_s  if caller is Initialize c_or_s is set to CREATE
 *
 * Out Args: 	NONE
 *
 * Return Values: Error Conditions	
 *
 * Side Effects: 	NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckExplicit
#if	NhlNeedProto
(NhlTickMarkLayer tnew,NhlTickMarkLayer told,_NhlArgList args, int num_args, int c_or_s)
#else
(tnew,told,args,num_args,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer	told;	
	_NhlArgList args;
	int	num_args;
	int		c_or_s;
#endif
{
	char		*error_lead;
	NhlGenArray	gen;
	NhlErrorTypes	ret = NhlNOERROR;
	NhlBoolean	free_xb_val=False, skip_xb_val=False;
	NhlBoolean	free_xbm_val=False, skip_xbm_val=False;
	NhlBoolean	free_xt_val=False, skip_xt_val=False;
	NhlBoolean	free_xtm_val=False, skip_xtm_val=False;
	NhlBoolean	free_yl_val=False, skip_yl_val=False;
	NhlBoolean	free_ylm_val=False, skip_ylm_val=False;
	NhlBoolean	free_yr_val=False, skip_yr_val=False;
	NhlBoolean	free_yrm_val=False, skip_yrm_val=False;
	NhlBoolean	free_xb_labels=False, skip_xb_labels=False;
	NhlBoolean	free_xt_labels=False, skip_xt_labels=False;
	NhlBoolean	free_yl_labels=False, skip_yl_labels=False;
	NhlBoolean	free_yr_labels=False, skip_yr_labels=False;

	if(c_or_s == SET) {
		error_lead = "TickMarkSetValues";

		if(told->tick.x_b_values != tnew->tick.x_b_values)
			free_xb_val = True;
		else
			skip_xb_val = True;

		if(told->tick.x_b_minor_values != tnew->tick.x_b_minor_values)
			free_xbm_val = True;
		else
			skip_xbm_val = True;

		if(told->tick.x_t_values != tnew->tick.x_t_values)
			free_xt_val = True;
		else
			skip_xt_val = True;

		if(told->tick.x_t_minor_values != tnew->tick.x_t_minor_values)
			free_xtm_val = True;
		else
			skip_xtm_val = True;

		if(told->tick.y_l_values != tnew->tick.y_l_values)
			free_yl_val = True;
		else
			skip_yl_val = True;

		if(told->tick.y_l_minor_values != tnew->tick.y_l_minor_values)
			free_ylm_val = True;
		else
			skip_ylm_val = True;

		if(told->tick.y_r_values != tnew->tick.y_r_values)
			free_yr_val = True;
		else
			skip_yr_val = True;

		if(told->tick.y_r_minor_values != tnew->tick.y_r_minor_values)
			free_yrm_val = True;
		else
			skip_yrm_val = True;

		if(told->tick.x_b_labels != tnew->tick.x_b_labels)
			free_xb_labels = True;
		else
			skip_xb_labels = True;

		if(told->tick.x_t_labels != tnew->tick.x_t_labels)
			free_xt_labels = True;
		else
			skip_xt_labels = True;

		if(told->tick.y_l_labels != tnew->tick.y_l_labels)
			free_yl_labels = True;
		else
			skip_yl_labels = True;

		if(told->tick.y_r_labels != tnew->tick.y_r_labels)
			free_yr_labels = True;
		else
			skip_yr_labels = True;

	} else {
		error_lead = "TickMarkInitialize";
	}

	/*
	 * Set values fields
	 */

	if((tnew->tick.x_b_values != NULL) && !skip_xb_val){

		gen = (NhlGenArray)tnew->tick.x_b_values;

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim float array: resetting",
						error_lead,NhlNtmXBValues);

			if(c_or_s == SET)
				tnew->tick.x_b_values = told->tick.x_b_values;
			else
				tnew->tick.x_b_values = NULL;

			free_xb_val = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.x_b_values = _NhlCopyGenArray(gen,True);
			if(tnew->tick.x_b_values == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_xb_val)
		NhlFreeGenArray(told->tick.x_b_values);

	if((tnew->tick.x_b_minor_values != NULL) && !skip_xbm_val){

		gen = (NhlGenArray)tnew->tick.x_b_minor_values;

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim float array: resetting",
						error_lead,NhlNtmXBValues);

			if(c_or_s == SET)
				tnew->tick.x_b_minor_values = told->tick.x_b_minor_values;
			else
				tnew->tick.x_b_minor_values = NULL;

			free_xbm_val = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.x_b_minor_values = _NhlCopyGenArray(gen,True);
			if(tnew->tick.x_b_minor_values == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_xbm_val)
		NhlFreeGenArray(told->tick.x_b_minor_values);

	if((tnew->tick.x_t_values != NULL) && !skip_xt_val){

		gen = (NhlGenArray)tnew->tick.x_t_values;

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim float array: resetting",
						error_lead,NhlNtmXTValues);

			if(c_or_s == SET)
				tnew->tick.x_t_values = told->tick.x_t_values;
			else
				tnew->tick.x_t_values = NULL;

			free_xt_val = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.x_t_values = _NhlCopyGenArray(gen,True);
			if(tnew->tick.x_t_values == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_xt_val)
		NhlFreeGenArray(told->tick.x_t_values);

	if((tnew->tick.x_t_minor_values != NULL) && !skip_xtm_val){

		gen = (NhlGenArray)tnew->tick.x_t_minor_values;

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim float array: resetting",
						error_lead,NhlNtmXTValues);

			if(c_or_s == SET)
				tnew->tick.x_t_minor_values = told->tick.x_t_minor_values;
			else
				tnew->tick.x_t_minor_values = NULL;

			free_xtm_val = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.x_t_minor_values = _NhlCopyGenArray(gen,True);
			if(tnew->tick.x_t_minor_values == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_xtm_val)
		NhlFreeGenArray(told->tick.x_t_minor_values);

	if((tnew->tick.y_l_values != NULL) && !skip_yl_val){

		gen = (NhlGenArray)tnew->tick.y_l_values;

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim float array: resetting",
						error_lead,NhlNtmYLValues);

			if(c_or_s == SET)
				tnew->tick.y_l_values = told->tick.y_l_values;
			else
				tnew->tick.y_l_values = NULL;

			free_yl_val = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.y_l_values = _NhlCopyGenArray(gen,True);
			if(tnew->tick.y_l_values == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_yl_val)
		NhlFreeGenArray(told->tick.y_l_values);

	if((tnew->tick.y_l_minor_values != NULL) && !skip_ylm_val){

		gen = (NhlGenArray)tnew->tick.y_l_minor_values;

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim float array: resetting",
						error_lead,NhlNtmYLValues);

			if(c_or_s == SET)
				tnew->tick.y_l_minor_values = told->tick.y_l_minor_values;
			else
				tnew->tick.y_l_minor_values = NULL;

			free_ylm_val = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.y_l_minor_values = _NhlCopyGenArray(gen,True);
			if(tnew->tick.y_l_minor_values == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_ylm_val)
		NhlFreeGenArray(told->tick.y_l_minor_values);

	if((tnew->tick.y_r_values != NULL) && !skip_yr_val){

		gen = (NhlGenArray)tnew->tick.y_r_values;

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim float array: resetting",
						error_lead,NhlNtmYRValues);

			if(c_or_s == SET)
				tnew->tick.y_r_values = told->tick.y_r_values;
			else
				tnew->tick.y_r_values = NULL;

			free_yr_val = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.y_r_values = _NhlCopyGenArray(gen,True);
			if(tnew->tick.y_r_values == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_yr_val)
		NhlFreeGenArray(told->tick.y_r_values);

	if((tnew->tick.y_r_minor_values != NULL) && !skip_yrm_val){

		gen = (NhlGenArray)tnew->tick.y_r_minor_values;

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim float array: resetting",
						error_lead,NhlNtmYRValues);

			if(c_or_s == SET)
				tnew->tick.y_r_minor_values = told->tick.y_r_minor_values;
			else
				tnew->tick.y_r_minor_values = NULL;

			free_yrm_val = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.y_r_minor_values = _NhlCopyGenArray(gen,True);
			if(tnew->tick.y_r_minor_values == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_yrm_val)
		NhlFreeGenArray(told->tick.y_r_minor_values);
	/*
	 * Set labels fields
	 */

	/*
	 * Although the CopyLabelArray makes sure that the labels have 
	 * as many elements as the values array, and set them to NULL if
	 * not defined, there is still a problem: if the values array is
	 * set but the labels array is not, the CopyLabelArray function
	 * will not get called. Therefore I have taken a different approach:
	 * now the ExplicitComputeMajorTickMarks handles the problem by 
	 * ignoring the labels if there are more values than labels.
	 * (Actually I made these changes before noticing what CopyLabelArray
	 * did. I might have done things differently otherwise.)
	 * I have added warning messages for the situation in this routine.
	 * -dib-
	 */

	if((tnew->tick.x_b_labels != NULL) && !skip_xb_labels){

		gen = (NhlGenArray)tnew->tick.x_b_labels;

		if((gen->typeQ != Qstring) ||
		   (gen->size != sizeof(NhlString))||
		   (gen->num_dimensions != 1) || 
		   (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim char* array: resetting",
						error_lead,NhlNtmXTLabels);

			if(c_or_s == SET)
				tnew->tick.x_b_labels = told->tick.x_b_labels;
			else
				tnew->tick.x_b_labels = NULL;

			free_xb_labels = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.x_b_labels = CopyLabelArray(gen,
							tnew->tick.x_b_values);
			if(tnew->tick.x_b_labels == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	} 

	if(free_xb_labels)
		NhlFreeGenArray(told->tick.x_b_labels);

	if (tnew->tick.x_b_labels && tnew->tick.x_b_values &&
	    tnew->tick.x_b_labels->num_elements < 
	    tnew->tick.x_b_values->num_elements) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	       "%s: %s has fewer elements than %s; the labels will be ignored",
			  error_lead,NhlNtmXBLabels,NhlNtmXBValues);
			ret = MIN(ret,NhlWARNING);
	}


	if((tnew->tick.x_t_labels != NULL) && !skip_xt_labels){

		gen = (NhlGenArray)tnew->tick.x_t_labels;

		if((gen->typeQ != Qstring) || (gen->size != sizeof(NhlString))||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim char* array: resetting",
						error_lead,NhlNtmXTLabels);

			if(c_or_s == SET)
				tnew->tick.x_t_labels = told->tick.x_t_labels;
			else
				tnew->tick.x_t_labels = NULL;

			free_xt_labels = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.x_t_labels = CopyLabelArray(gen,
							tnew->tick.x_t_values);
			if(tnew->tick.x_t_labels == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_xt_labels)
		NhlFreeGenArray(told->tick.x_t_labels);

	if (tnew->tick.x_t_labels && tnew->tick.x_t_values &&
	    tnew->tick.x_t_labels && tnew->tick.x_t_values &&
	    tnew->tick.x_t_labels->num_elements < 
	    tnew->tick.x_t_values->num_elements) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	       "%s: %s has fewer elements than %s; the labels will be ignored",
			  error_lead,NhlNtmXTLabels,NhlNtmXTValues);
			ret = MIN(ret,NhlWARNING);
	}

	if((tnew->tick.y_l_labels != NULL) && !skip_yl_labels){

		gen = (NhlGenArray)tnew->tick.y_l_labels;

		if((gen->typeQ != Qstring) || (gen->size != sizeof(NhlString))||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim char* array: resetting",
						error_lead,NhlNtmYLLabels);

			if(c_or_s == SET)
				tnew->tick.y_l_labels = told->tick.y_l_labels;
			else
				tnew->tick.y_l_labels = NULL;

			free_yl_labels = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.y_l_labels = CopyLabelArray(gen,
							tnew->tick.y_l_values);
			if(tnew->tick.y_l_labels == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_yl_labels)
		NhlFreeGenArray(told->tick.y_l_labels);

	if (tnew->tick.y_l_labels && tnew->tick.y_l_values &&
	    tnew->tick.y_l_labels->num_elements < 
	    tnew->tick.y_l_values->num_elements) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	       "%s: %s has fewer elements than %s; the labels will be ignored",
			  error_lead,NhlNtmYLLabels,NhlNtmYLValues);
			ret = MIN(ret,NhlWARNING);
	}

	if((tnew->tick.y_r_labels != NULL) && !skip_yr_labels){

		gen = (NhlGenArray)tnew->tick.y_r_labels;

		if((gen->typeQ != Qstring) || (gen->size != sizeof(NhlString))||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim char* array: resetting",
						error_lead,NhlNtmYRLabels);

			if(c_or_s == SET)
				tnew->tick.y_r_labels = told->tick.y_r_labels;
			else
				tnew->tick.y_r_labels = NULL;

			free_yr_labels = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			tnew->tick.y_r_labels = CopyLabelArray(gen,
							tnew->tick.y_r_values);
			if(tnew->tick.y_r_labels == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}

	if(free_yr_labels)
		NhlFreeGenArray(told->tick.y_r_labels);

	if (tnew->tick.y_r_labels && tnew->tick.y_r_values &&
	    tnew->tick.y_r_labels->num_elements < 
	    tnew->tick.y_r_values->num_elements) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	       "%s: %s has fewer elements than %s; the labels will be ignored",
			  error_lead,NhlNtmYRLabels,NhlNtmYRValues);
			ret = MIN(ret,NhlWARNING);
	}

	/*
	 * Check "mode" fields
	 */

	if((tnew->tick.x_b_mode == NhlEXPLICIT) &&
					(tnew->tick.x_b_values == NULL)){
		NHLPERROR((NhlINFO,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlAUTOMATIC",error_lead,
			NhlNtmXBValues,NhlNtmXBMode));
		tnew->tick.x_b_mode = NhlAUTOMATIC;
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.x_t_mode == NhlEXPLICIT) &&
					(tnew->tick.x_t_values == NULL)){
		NHLPERROR((NhlINFO,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlAUTOMATIC",error_lead,
			NhlNtmXTValues,NhlNtmXTMode));
		tnew->tick.x_t_mode = NhlAUTOMATIC;
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.y_l_mode == NhlEXPLICIT) &&
					(tnew->tick.y_l_values == NULL)){
		NHLPERROR((NhlINFO,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlAUTOMATIC",error_lead,
			NhlNtmYLValues,NhlNtmYLMode));
		tnew->tick.y_l_mode = NhlAUTOMATIC;
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.y_r_mode == NhlEXPLICIT) &&
					(tnew->tick.y_r_values == NULL)){
		NHLPERROR((NhlINFO,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlAUTOMATIC",error_lead,
			NhlNtmYRValues,NhlNtmYRMode));
		tnew->tick.y_r_mode = NhlAUTOMATIC;
		ret = MIN(ret,NhlINFO);
	}

	/*
	 * Give NhlINFO warning if no labels are given
	 */
	if((tnew->tick.x_b_mode == NhlEXPLICIT) &&
					(tnew->tick.x_b_labels == NULL)){
		NHLPERROR((NhlINFO,NhlEUNKNOWN,"%s:%s is NhlEXPLICIT but %s not set",
					error_lead,NhlNtmXBMode,NhlNtmXBLabels));
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.x_t_mode == NhlEXPLICIT) &&
					(tnew->tick.x_t_labels == NULL)){
		NHLPERROR((NhlINFO,NhlEUNKNOWN,"%s:%s is NhlEXPLICIT but %s not set",
					error_lead,NhlNtmXTMode,NhlNtmXTLabels));
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.y_l_mode == NhlEXPLICIT) &&
					(tnew->tick.y_l_labels == NULL)){
		NHLPERROR((NhlINFO,NhlEUNKNOWN,"%s:%s is NhlEXPLICIT but %s not set",
					error_lead,NhlNtmYLMode,NhlNtmYLLabels));
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.y_r_mode == NhlEXPLICIT) &&
					(tnew->tick.y_r_labels == NULL)){
		NHLPERROR((NhlINFO,NhlEUNKNOWN,"%s:%s is NhlEXPLICIT but %s not set",
					error_lead,NhlNtmYRMode,NhlNtmYRLabels));
		ret = MIN(ret,NhlINFO);
	}


	/*
	 * Check MinorOn fields
	 */
	if(tnew->tick.x_b_mode == NhlEXPLICIT) {
/*
                if(tnew->tick.x_b_minor_on) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s cannot be on when %s is NhlEXPLICIT:setting %s off",
				error_lead,NhlNtmXBMinorOn,NhlNtmXBMode,
							NhlNtmXBMinorOn);

			ret = MIN(ret,NhlWARNING);
                        tnew->tick.x_b_minor_on = False;
                        tnew->tick.x_b_nminor = 0;
                }
*/
	}
	if(tnew->tick.x_t_mode == NhlEXPLICIT) {
/*
                if(tnew->tick.x_t_minor_on) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s cannot be on when %s is NhlEXPLICIT:setting %s off",
				error_lead,NhlNtmXTMinorOn,NhlNtmXTMode,
							NhlNtmXTMinorOn);

			ret = MIN(ret,NhlWARNING);
                        tnew->tick.x_t_minor_on = False;
                        tnew->tick.x_t_nminor = 0;
                }
*/
	}
	if(tnew->tick.y_l_mode == NhlEXPLICIT) {
/*
                if(tnew->tick.y_l_minor_on) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s cannot be on when %s is NhlEXPLICIT:setting %s off",
				error_lead,NhlNtmYLMinorOn,NhlNtmYLMode,
							NhlNtmYLMinorOn);

			ret = MIN(ret,NhlWARNING);
                        tnew->tick.y_l_minor_on = False;
                        tnew->tick.y_l_nminor = 0;
                }
*/
	}
	if(tnew->tick.y_r_mode == NhlEXPLICIT) {
/*
                if(tnew->tick.y_r_minor_on) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s cannot be on when %s is NhlEXPLICIT:setting %s off",
				error_lead,NhlNtmYRMinorOn,NhlNtmYRMode,
							NhlNtmYRMinorOn);

			ret = MIN(ret,NhlWARNING);
                        tnew->tick.y_r_minor_on = False;
                        tnew->tick.y_r_nminor = 0;
                }
*/
	}


	/*
	 * Set start and end ticks
	 */
        if(tnew->tick.x_b_mode == NhlEXPLICIT){
                if((tnew->tick.x_b_tick_start == 0.0)&&
                        (tnew->tick.x_b_tick_end == 0.0)) {
                        tnew->tick.x_b_tick_start = MIN(tnew->tick.x_b_data_left						,tnew->tick.x_b_data_right);
                        tnew->tick.x_b_tick_end = MAX(tnew->tick.x_b_data_left,
                                                tnew->tick.x_b_data_right);
                        NhlPError(NhlINFO,NhlEUNKNOWN,"%s: No tick start or end set for bottom ticks in explicit mode, using values of DataLeft and DataRight",error_lead);
			ret = MIN(ret,NhlINFO);
                }
        }
        if(tnew->tick.x_t_mode == NhlEXPLICIT){
                if((tnew->tick.x_t_tick_start == 0.0)&&
                        (tnew->tick.x_t_tick_end == 0.0)) {
                        tnew->tick.x_t_tick_start = MIN(tnew->tick.x_t_data_left						,tnew->tick.x_t_data_right);
                        tnew->tick.x_t_tick_end = MAX(tnew->tick.x_t_data_left,
                                                tnew->tick.x_t_data_right);
                       NhlPError(NhlINFO,NhlEUNKNOWN,"%s: No tick start or end set for top ticks in explicit mode, using values of DataLeft and DataRight",error_lead);
			ret = MIN(ret,NhlINFO);
                }
        }
        if(tnew->tick.y_l_mode == NhlEXPLICIT){
                if((tnew->tick.y_l_tick_start == 0.0)&&
                        (tnew->tick.y_l_tick_end == 0.0)) {
                        tnew->tick.y_l_tick_start = MIN(tnew->tick.y_l_data_top,tnew->tick.y_l_data_bottom);
                        tnew->tick.y_l_tick_end = MAX(tnew->tick.y_l_data_top,
                                                tnew->tick.y_l_data_bottom);
                       NhlPError(NhlINFO,NhlEUNKNOWN,"%s: No tick start or end set for left ticks in explicit mode, using values of DataBottom and DataTop",error_lead);	
			ret = MIN(ret,NhlINFO);
                }
        }
        if(tnew->tick.y_r_mode == NhlEXPLICIT){
                if((tnew->tick.y_r_tick_start == 0.0)&&
                        (tnew->tick.y_r_tick_end == 0.0)) {
                        tnew->tick.y_r_tick_start = MIN(tnew->tick.y_r_data_top,tnew->tick.y_r_data_bottom);
                        tnew->tick.y_r_tick_end = MAX(tnew->tick.y_r_data_top,
                                                tnew->tick.y_r_data_bottom);
                       NhlPError(NhlINFO,NhlEUNKNOWN,"%s: No tick start or end set for right ticks in explicit mode, using values of DataBottom and DataTop",error_lead);
			ret = MIN(ret,NhlINFO);
                }
        }

	return(ret);
}

/*
 * Function:	CheckNotLog
 *
 * Description: If the last setting was LOG and the minor_per_major value
 *		is not explicitly set, sets it to a usually better value for
 *              non-log spacing.
 *
 * In Args:     tnew	new instance
 *		told	old instance set only if c_or_s == SET
 *		c_or_s  either SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckNotLog
#if	NhlNeedProto
(NhlTickMarkLayer	tnew,NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	int	c_or_s;
#endif
{
	if(c_or_s == CREATE )
                return NhlNOERROR;
        
        if (tnew->tick.x_b_on && told->tick.x_b_style == NhlLOG &&
            tnew->tick.x_b_style != NhlLOG &&
            ! tnew->tick.x_b_minor_per_major_set) {
                tnew->tick.x_b_minor_per_major = 3;
        }
        if (tnew->tick.x_t_on && told->tick.x_t_style == NhlLOG &&
            tnew->tick.x_t_style != NhlLOG &&
            ! tnew->tick.x_t_minor_per_major_set) {
                tnew->tick.x_t_minor_per_major = 3;
        }
        if (tnew->tick.y_l_on && told->tick.y_l_style == NhlLOG &&
            tnew->tick.y_l_style != NhlLOG &&
            ! tnew->tick.y_l_minor_per_major_set) {
                tnew->tick.y_l_minor_per_major = 3;
        }
        if (tnew->tick.y_r_on && told->tick.y_r_style == NhlLOG &&
            tnew->tick.y_r_style != NhlLOG &&
            ! tnew->tick.y_r_minor_per_major_set) {
                tnew->tick.y_r_minor_per_major = 3;
        }

        return NhlNOERROR;
}

/*
 * Function:	CheckLog
 *
 * Description: Checks values when NhlLOG style is set. Makes sure no 0's or
 *		negative exist in input and that spacing is an integer values
 *		For NhlLOG plots spacing represents the number of decades not
 *		an absolute value for spacing.
 *
 * In Args:     tnew	new instance
 *		told	old instance set only if c_or_s == SET
 *		c_or_s  either SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckLog
#if	NhlNeedProto
(NhlTickMarkLayer	tnew,NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	int	c_or_s;
#endif
{
	char *error_lead;
	NhlErrorTypes ret = NhlNOERROR;

	if(c_or_s == CREATE ) {
		error_lead = "TickMarkInitialize";
	} else {
		error_lead = "TickMarkSetValues";
	}
	
	if(tnew->tick.x_b_on && tnew->tick.x_b_style == NhlLOG) {
                if((tnew->tick.x_b_data_left <=0.0)
                        ||(tnew->tick.x_b_data_right <=0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Bottom tick style is log and either DataLeft or DataRight is negative or zero, turning bottom ticks off",error_lead);
                        tnew->tick.x_b_data_valid = False;
			ret = NhlWARNING;

                }
		if(tnew->tick.x_b_mode == NhlMANUAL) {
			if(tnew->tick.x_b_tick_spacing < 1.0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected but spacing is less than 1 decade resetting XBTickSpacing to 1.0",error_lead);
				ret = NhlWARNING;
				tnew->tick.x_b_tick_spacing = 1.0;
			} else if(((float)((int)tnew->tick.x_b_tick_spacing)) !=
					tnew->tick.x_b_tick_spacing) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Tick Mark spacing must be an even multiple of 1, truncating decimal value to integer",error_lead);
				ret = NhlWARNING;
				tnew->tick.x_b_tick_spacing = (int)tnew->tick.x_b_tick_spacing;
				
			}
			if((tnew->tick.x_b_tick_start <=0.0)||(tnew->tick.x_b_tick_end <= 0.0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected,but XBTickStart or XBTickEnd is less than or equal to 0",error_lead);
				return(NhlFATAL);
			}
		}
		if((tnew->tick.x_b_minor_per_major != 0)&&
			(tnew->tick.x_b_minor_per_major != 1)&&
			(tnew->tick.x_b_minor_per_major != 4)&&
			(tnew->tick.x_b_minor_per_major != 8)) {
                        if (tnew->tick.x_b_minor_per_major_set) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG tick marks can only have 1, 4 or 8 minor tickmarks, resetting XBMinorPerMajor to 1,4 or 8",error_lead);
                                ret = NhlWARNING;
                        }
		
			if(tnew->tick.x_b_minor_per_major < 3) {
				tnew->tick.x_b_minor_per_major = 1;
			} else if(tnew->tick.x_b_minor_per_major < 6) {
				tnew->tick.x_b_minor_per_major = 4;
			} else  {
                                tnew->tick.x_b_minor_per_major = 8;
			}
		}
        }
        if(tnew->tick.x_t_on && tnew->tick.x_t_style == NhlLOG) {
                if((tnew->tick.x_t_data_left <=0.0)
                        ||(tnew->tick.x_t_data_right <=0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Top tick style is log and either DataLeft or DataRight is negative or zero, turning top ticks off",error_lead);
			ret = NhlWARNING;
                        tnew->tick.x_t_data_valid = False;

                }
		if(tnew->tick.x_t_mode == NhlMANUAL) {
			if(tnew->tick.x_t_tick_spacing < 1.0){
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected but spacing is less than 1 decade resetting XTTickSpacing to 1.0",error_lead);
			ret = NhlWARNING;
			tnew->tick.x_t_tick_spacing = 1.0;
			} else if(((float)((int)tnew->tick.x_t_tick_spacing)) !=
					tnew->tick.x_t_tick_spacing) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Tick Mark spacing must be an even multiple of 1, truncating decimal value to integer",error_lead);
				ret = NhlWARNING;
				tnew->tick.x_t_tick_spacing = (int)tnew->tick.x_t_tick_spacing;
				
			}
			if((tnew->tick.x_t_tick_start <=0.0)||(tnew->tick.x_t_tick_end <= 0.0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected,but XTTickStart or XTTickEnd is less than or equal to 0",error_lead);
				return(NhlFATAL);
			}
		}
		if((tnew->tick.x_t_minor_per_major != 0)&&
			(tnew->tick.x_t_minor_per_major != 1)&&
			(tnew->tick.x_t_minor_per_major != 4)&&
			(tnew->tick.x_t_minor_per_major != 8)) {
                        if (tnew->tick.x_t_minor_per_major_set) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG tick marks can only have 1, 4 or 8 minor tickmarks, resetting XTMinorPerMajor to 1,4 or 8",error_lead);
                                ret = NhlWARNING;
                        }
			if(tnew->tick.x_t_minor_per_major < 3) {
				tnew->tick.x_t_minor_per_major = 1;
			} else if(tnew->tick.x_t_minor_per_major < 6) {
				tnew->tick.x_t_minor_per_major = 4;
			} else  {
                                tnew->tick.x_t_minor_per_major = 8;
			}
		}
        }
        if(tnew->tick.y_l_on && tnew->tick.y_l_style == NhlLOG) {
                if((tnew->tick.y_l_data_top <=0.0)
                        ||(tnew->tick.y_l_data_bottom<=0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Left tick style is log and either DataBottom or DataTop is negative or zero, turning left ticks off",error_lead);
			ret = NhlWARNING;
                        tnew->tick.y_l_data_valid = False;

                }
		if(tnew->tick.y_l_mode == NhlMANUAL) {
			if(tnew->tick.y_l_tick_spacing < 1.0) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected but spacing is less than 1 decade resetting YLTickSpacing to 1.0",error_lead);
			ret = NhlWARNING;
			tnew->tick.y_l_tick_spacing = 1.0;
			} else if(((float)((int)tnew->tick.y_l_tick_spacing)) !=
					tnew->tick.y_l_tick_spacing) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Tick Mark spacing must be an even multiple of 1, truncating decimal value to integer",error_lead);
				ret = NhlWARNING;
				tnew->tick.y_l_tick_spacing = (int)tnew->tick.y_l_tick_spacing;
				
			}
			if((tnew->tick.y_l_tick_start <=0.0)||(tnew->tick.y_l_tick_end <= 0.0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected,but YLTickStart or YLTickEnd is less than or equal to 0",error_lead);
				return(NhlFATAL);
			}
		}
		if((tnew->tick.y_l_minor_per_major != 0)&&
			(tnew->tick.y_l_minor_per_major != 1)&&
			(tnew->tick.y_l_minor_per_major != 4)&&
			(tnew->tick.y_l_minor_per_major != 8)) {
                        if (tnew->tick.y_l_minor_per_major_set) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG tick marks can only have 1, 4 or 8 minor tickmarks, resetting YLMinorPerMajor to 1,4 or 8",error_lead);
                                ret = NhlWARNING;
                        }
			if(tnew->tick.y_l_minor_per_major < 3) {
				tnew->tick.y_l_minor_per_major = 1;
			} else if(tnew->tick.y_l_minor_per_major < 6) {
				tnew->tick.y_l_minor_per_major = 4;
			} else  {
                                tnew->tick.y_l_minor_per_major = 8;
			}
		}
        }
        if(tnew->tick.y_r_on && tnew->tick.y_r_style == NhlLOG) {
                if((tnew->tick.y_r_data_top <=0.0)
                        ||(tnew->tick.y_r_data_bottom <=0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Right tick style is log and either DataBottom or DataTop is negative or zero, turning right ticks off",error_lead);
			ret = NhlWARNING;
                        tnew->tick.y_r_data_valid = False;

                }
		if(tnew->tick.y_r_mode == NhlMANUAL) {
			if(tnew->tick.y_r_tick_spacing < 1.0){
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected but spacing is less than 1 decade resetting YRTickSpacing to 1.0",error_lead);
			tnew->tick.y_r_tick_spacing = 1.0;
			} else if(((float)((int)tnew->tick.y_r_tick_spacing)) !=
					tnew->tick.y_r_tick_spacing) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Tick Mark spacing must be an even multiple of 1, truncating decimal value to integer",error_lead);
				ret = NhlWARNING;
				tnew->tick.y_r_tick_spacing = (int)tnew->tick.y_r_tick_spacing;
				
			}
			if((tnew->tick.y_r_tick_start <=0.0)||(tnew->tick.y_r_tick_end <= 0.0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected,but YRTickStart or YRTickEnd is less than or equal to 0",error_lead);
				return(NhlFATAL);
			}
		}
		if((tnew->tick.y_r_minor_per_major != 0)&&
			(tnew->tick.y_r_minor_per_major != 1)&&
			(tnew->tick.y_r_minor_per_major != 4)&&
			(tnew->tick.y_r_minor_per_major != 8)) {
                        if (tnew->tick.y_r_minor_per_major_set) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG tick marks can only have 1, 4 or 8 minor tickmarks, resetting YLMinorPerMajor to 1,4 or 8",error_lead);
                                ret = NhlWARNING;
                        }
			if(tnew->tick.y_r_minor_per_major < 3) {
				tnew->tick.y_r_minor_per_major = 1;
			} else if(tnew->tick.y_r_minor_per_major < 6) {
				tnew->tick.y_r_minor_per_major = 4;
			} else  {
                                tnew->tick.y_r_minor_per_major = 8;
			}
		}
        }
	return(ret);
}

/*
 * Function:	CheckIrregular
 *
 * Description: Confirms values of resources that control irregular tickmarks
 *		It also copies array resources into internal storage. No
 *		processing is done on the input.
 *
 * In Args:	tnew	new instance pointer
 *		told	old instance pointer only when c_or_s == SET
 *		c_or_s  either CREATE or SET
 *
 * Out Args:	NONE
 *
 * Return Values: ErrorConditions
 *
 * Side Effects: NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckIrregular
#if	NhlNeedProto
(NhlTickMarkLayer tnew, NhlTickMarkLayer told, _NhlArgList args, int num_args, int c_or_s)
#else
(tnew,told,args,num_args,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	_NhlArgList	args;
	int 	num_args;
	int		c_or_s;
#endif
{
	NhlBoolean	free_xb_irreg = False, skip_xb_irreg = False;
	NhlBoolean	free_xt_irreg = False, skip_xt_irreg = False;
	NhlBoolean	free_yl_irreg = False, skip_yl_irreg = False;
	NhlBoolean	free_yr_irreg = False, skip_yr_irreg = False;
	NhlGenArray	gen;
	NhlErrorTypes ret = NhlNOERROR;
	char		*error_lead = "CheckIrregular";

		
	/*
	 * take care of irregular_points
	 */

	if(c_or_s == SET){
		if(told->tick.x_b_irregular_points !=
						tnew->tick.x_b_irregular_points)
			free_xb_irreg = True;
		else
			skip_xb_irreg = True;
		if(told->tick.x_t_irregular_points !=
						tnew->tick.x_t_irregular_points)
			free_xt_irreg = True;
		else
			skip_xt_irreg = True;
		if(told->tick.y_l_irregular_points !=
						tnew->tick.y_l_irregular_points)
			free_yl_irreg = True;
		else
			skip_yl_irreg = True;
		if(told->tick.y_r_irregular_points !=
						tnew->tick.y_r_irregular_points)
			free_yr_irreg = True;
		else
			skip_yr_irreg = True;
	}

	if((tnew->tick.x_b_irregular_points != NULL) && !skip_xb_irreg){
		gen = (NhlGenArray)tnew->tick.x_b_irregular_points;

		if((gen->size != sizeof(float)) ||
		(gen->num_dimensions != 1) || (gen->len_dimensions[0] < 3)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:%s must be a 1 dim float array with a min of 3 elements: resetting",
				error_lead,NhlNtmXBIrregularPoints);

			if(c_or_s == SET)
				tnew->tick.x_b_irregular_points =
						told->tick.x_b_irregular_points;
			else
				tnew->tick.x_b_irregular_points = NULL;

			free_xb_irreg = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			float	*tarr;

			tnew->tick.x_b_irregular_points =
						_NhlCopyGenArray(gen,True);
			if(tnew->tick.x_b_irregular_points == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			tarr = (float*)gen->data;
			tnew->tick.ir_xbmin =
				MIN(tarr[0],tarr[gen->len_dimensions[0]-1]);
			tnew->tick.ir_xbmax =
				MAX(tarr[0],tarr[gen->len_dimensions[0]-1]);
			tnew->tick.new_ir_xb = True;
		}
	}
	else
		tnew->tick.new_ir_xb = False;

	if(free_xb_irreg)
		NhlFreeGenArray(told->tick.x_b_irregular_points);

	if((tnew->tick.x_t_irregular_points != NULL) && !skip_xt_irreg){
		gen = (NhlGenArray)tnew->tick.x_t_irregular_points;

		if((gen->size != sizeof(float)) ||
		(gen->num_dimensions != 1) || (gen->len_dimensions[0] < 3)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:%s must be a 1 dim float array with a min of 3 elements: resetting",
				error_lead,NhlNtmXTIrregularPoints);

			if(c_or_s == SET)
				tnew->tick.x_t_irregular_points =
						told->tick.x_t_irregular_points;
			else
				tnew->tick.x_t_irregular_points = NULL;

			free_xt_irreg = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			float	*tarr;

			tnew->tick.x_t_irregular_points =
						_NhlCopyGenArray(gen,True);
			if(tnew->tick.x_t_irregular_points == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			tarr = (float*)gen->data;
			tnew->tick.ir_xtmin =
				MIN(tarr[0],tarr[gen->len_dimensions[0]-1]);
			tnew->tick.ir_xtmax =
				MAX(tarr[0],tarr[gen->len_dimensions[0]-1]);
			tnew->tick.new_ir_xt = True;
		}
	}
	else
		tnew->tick.new_ir_xt = False;

	if(free_xt_irreg)
		NhlFreeGenArray(told->tick.x_t_irregular_points);

	if((tnew->tick.y_l_irregular_points != NULL) && !skip_yl_irreg){
		gen = (NhlGenArray)tnew->tick.y_l_irregular_points;

		if((gen->size != sizeof(float)) ||
		(gen->num_dimensions != 1) || (gen->len_dimensions[0] < 3)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:%s must be a 1 dim float array with a min of 3 elements: resetting",
				error_lead,NhlNtmYLIrregularPoints);

			if(c_or_s == SET)
				tnew->tick.y_l_irregular_points =
						told->tick.y_l_irregular_points;
			else
				tnew->tick.y_l_irregular_points = NULL;

			free_yl_irreg = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			float	*tarr;

			tnew->tick.y_l_irregular_points =
						_NhlCopyGenArray(gen,True);
			if(tnew->tick.y_l_irregular_points == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			tarr = (float*)gen->data;
			tnew->tick.ir_ylmin =
				MIN(tarr[0],tarr[gen->len_dimensions[0]-1]);
			tnew->tick.ir_ylmax =
				MAX(tarr[0],tarr[gen->len_dimensions[0]-1]);
			tnew->tick.new_ir_yl = True;
		}
	}
	else
		tnew->tick.new_ir_yl = False;

	if(free_yl_irreg)
		NhlFreeGenArray(told->tick.y_l_irregular_points);

	if((tnew->tick.y_r_irregular_points != NULL) && !skip_yr_irreg){
		gen = (NhlGenArray)tnew->tick.y_r_irregular_points;

		if((gen->size != sizeof(float)) || (gen->num_dimensions != 1)
					|| (gen->len_dimensions[0] < 3)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:%s must be a 1 dim float array with a min of 3 elements: resetting",
				error_lead,NhlNtmYRIrregularPoints);

			if(c_or_s == SET)
				tnew->tick.y_r_irregular_points =
						told->tick.y_r_irregular_points;
			else
				tnew->tick.y_r_irregular_points = NULL;

			free_yr_irreg = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			float	*tarr;

			tnew->tick.y_r_irregular_points =
						_NhlCopyGenArray(gen,True);
			if(tnew->tick.y_r_irregular_points == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			tarr = (float*)gen->data;
			tnew->tick.ir_yrmin =
				MIN(tarr[0],tarr[gen->len_dimensions[0]-1]);
			tnew->tick.ir_yrmax =
				MAX(tarr[0],tarr[gen->len_dimensions[0]-1]);
			tnew->tick.new_ir_yr = True;
		}
	}
	else
		tnew->tick.new_ir_yr = False;

	if(free_yr_irreg)
		NhlFreeGenArray(told->tick.y_r_irregular_points);

	/*
	 * Take care of style resources
	 */
	if((tnew->tick.x_b_style == NhlIRREGULAR) &&
				(tnew->tick.x_b_irregular_points == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlLINEAR",error_lead,
			NhlNtmXBIrregularPoints,NhlNtmXBStyle);
		tnew->tick.x_b_style = NhlLINEAR;
		ret = MIN(ret,NhlWARNING);
	}
	if((tnew->tick.x_t_style == NhlIRREGULAR) &&
				(tnew->tick.x_t_irregular_points == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlLINEAR",error_lead,
			NhlNtmXTIrregularPoints,NhlNtmXTStyle);
		tnew->tick.x_t_style = NhlLINEAR;
		ret = MIN(ret,NhlWARNING);
	}
	if((tnew->tick.y_l_style == NhlIRREGULAR) &&
				(tnew->tick.y_l_irregular_points == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlLINEAR",error_lead,
			NhlNtmYLIrregularPoints,NhlNtmYLStyle);
		tnew->tick.y_l_style = NhlLINEAR;
		ret = MIN(ret,NhlWARNING);
	}
	if((tnew->tick.y_r_style == NhlIRREGULAR) &&
				(tnew->tick.y_r_irregular_points == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlLINEAR",error_lead,
			NhlNtmYRIrregularPoints,NhlNtmYRStyle);
		tnew->tick.y_r_style = NhlLINEAR;
		ret = MIN(ret,NhlWARNING);
	}

	if(tnew->tick.x_b_style == NhlIRREGULAR) {

		if(((tnew->tick.x_b_data_left >= tnew->tick.ir_xbmax)
			&&(tnew->tick.x_b_data_right >=tnew->tick.ir_xbmax))
			||((tnew->tick.x_b_data_left <=tnew->tick.ir_xbmin)&&(tnew->tick.x_b_data_right <= tnew->tick.ir_xbmin))) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMark:CheckIrregular: Both tmXBDataLeftF and tmXBDataRightF are outside of the range of the values in tmXBCoordPoints, can not continue\n");
			return(NhlFATAL);
		}
		if(tnew->tick.x_b_data_left > tnew->tick.x_b_data_right) {
			if(tnew->tick.x_b_data_left > tnew->tick.ir_xbmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XBDataLeftF is greater than the maximum value in XBCoordPoints, resetting XBDataLeftF to maximum of XBCoordPoints");
				tnew->tick.x_b_data_left = tnew->tick.ir_xbmax;
				ret = NhlWARNING;
			} 
			if(tnew->tick.x_b_data_right < tnew->tick.ir_xbmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XBDataRightF is than the minimum value in XBCoordPoints, resetting XBDataRightF to minimum of XBCoordPoints");
				tnew->tick.x_b_data_right = tnew->tick.ir_xbmin;
				ret = NhlWARNING;
			}
		} else {
			if(tnew->tick.x_b_data_left < tnew->tick.ir_xbmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XBDataLeftF is less than the minimum value in XBCoordPoints, resetting XBDataLeftF to minimum of XBCoordPoints");
				tnew->tick.x_b_data_left = tnew->tick.ir_xbmin;
				ret = NhlWARNING;
			}
			if(tnew->tick.x_b_data_right > tnew->tick.ir_xbmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XBDataRightF is greater than the maximum value in XBCoordPoints, resetting XBDataRightF to maximum of XBCoordPoints");
				tnew->tick.x_b_data_right = tnew->tick.ir_xbmax;
				ret = NhlWARNING;
			}
		}
	}
	if(tnew->tick.x_t_style == NhlIRREGULAR) {
		if(((tnew->tick.x_t_data_left >= tnew->tick.ir_xtmax)
			&&(tnew->tick.x_t_data_right >=tnew->tick.ir_xtmax))
			||((tnew->tick.x_t_data_left <=tnew->tick.ir_xtmin)&&(tnew->tick.x_t_data_right <= tnew->tick.ir_xtmin))) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMark:CheckIrregular: Both tmXTDataLeftF and tmXTDataRightF are outside of the range of the values in tmXTCoordPoints, can not continue\n");
			return(NhlFATAL);
		}
		if(tnew->tick.x_t_data_left > tnew->tick.x_t_data_right) {
			if(tnew->tick.x_t_data_left > tnew->tick.ir_xtmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XTDataLeftF is greater than the maximum value in XTCoordPoints, resetting XTDataLeftF to maximum of XTCoordPoints");
				tnew->tick.x_t_data_left = tnew->tick.ir_xtmax;
				ret = NhlWARNING;
			}
			if(tnew->tick.x_t_data_right < tnew->tick.ir_xtmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XTDataRightF is less than the minimum value in XTCoordPoints, resetting XTDataRightF to minimum of XTCoordPoints");
				tnew->tick.x_t_data_right = tnew->tick.ir_xtmin;
				ret = NhlWARNING;
			}
		} else {
			if(tnew->tick.x_t_data_left < tnew->tick.ir_xtmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XTDataLeftF is less than the minimum value in XTCoordPoints, resetting XTDataLeftF to minimum of XTCoordPoints");
				tnew->tick.x_t_data_left = tnew->tick.ir_xtmin;
				ret = NhlWARNING;
			}
			if(tnew->tick.x_t_data_right > tnew->tick.ir_xtmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XTDataRightF is greater than the maximum value in XTCoordPoints, resetting XTDataRightF to maximum of XTCoordPoints");
				tnew->tick.x_t_data_right = tnew->tick.ir_xtmax;
				ret = NhlWARNING;
			}
		}
	}
	if(tnew->tick.y_r_style == NhlIRREGULAR) {
		if(((tnew->tick.y_r_data_top >= tnew->tick.ir_yrmax)
			&&(tnew->tick.y_r_data_bottom >=tnew->tick.ir_yrmax))
			||((tnew->tick.y_r_data_top <=tnew->tick.ir_yrmin)&&(tnew->tick.y_r_data_bottom <= tnew->tick.ir_yrmin))) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMark:CheckIrregular: Both tmYRDataBottomF and tmYRDataTop are outside of the range of the values in tmYRCoordPoints, can not continue\n");
			return(NhlFATAL);
		}
		if(tnew->tick.y_r_data_bottom > tnew->tick.y_r_data_top) {
			if(tnew->tick.y_r_data_bottom > tnew->tick.ir_yrmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YRDataBottomF is greater than the maximum value in YRCoordPoints, resetting YRDataBottomF to maximum of YRCoordPoints");
				tnew->tick.y_r_data_bottom = tnew->tick.ir_yrmax;
				ret = NhlWARNING;
			}
			if(tnew->tick.y_r_data_top < tnew->tick.ir_yrmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YRDataTopF is less than the minimum value in YRCoordPoints, resetting YRDataTopF to minimum of YRCoordPoints");
				tnew->tick.y_r_data_top = tnew->tick.ir_yrmin;
				ret = NhlWARNING;
			}
		} else {
			if(tnew->tick.y_r_data_bottom < tnew->tick.ir_yrmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YRDataBottomF is less than the minimum value in YRCoordPoints, resetting YRDataBottomF to minimum of YRCoordPoints");
				tnew->tick.y_r_data_bottom = tnew->tick.ir_yrmin;
				ret = NhlWARNING;
			}
			if(tnew->tick.y_r_data_top > tnew->tick.ir_yrmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YRDataTopF is greater than the maximum value in YRCoordPoints, resetting YRDataTopF to maximum of YRCoordPoints");
				tnew->tick.y_r_data_top= tnew->tick.ir_yrmax;
				ret = NhlWARNING;
			}
		}
	}
	if(tnew->tick.y_l_style == NhlIRREGULAR) {
		if(((tnew->tick.y_l_data_top >= tnew->tick.ir_ylmax)
			&&(tnew->tick.y_l_data_bottom>=tnew->tick.ir_ylmax))
			||((tnew->tick.y_l_data_top <=tnew->tick.ir_ylmin)&&(tnew->tick.y_l_data_bottom<= tnew->tick.ir_ylmin))) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMark:CheckIrregular: Both tmYLDataBottomF and tmYLDataTop are outside of the range of the values in tmYLCoordPoints, can not continue\n");
			return(NhlFATAL);
		}
		if(tnew->tick.y_l_data_bottom > tnew->tick.y_l_data_top) {
			if(tnew->tick.y_l_data_bottom > tnew->tick.ir_ylmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YLDataBottomF is greater than the maximum value in YLCoordPoints, resetting YLDataBottomF to maximum of YLCoordPoints");
				tnew->tick.y_l_data_bottom = tnew->tick.ir_ylmax;
				ret = NhlWARNING;
			}
			if(tnew->tick.y_l_data_top < tnew->tick.ir_ylmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YLDataTopF is less than the minimum value in YLCoordPoints, resetting YLDataTopF to minimum of YLCoordPoints");
				tnew->tick.y_l_data_top= tnew->tick.ir_ylmin;
				ret = NhlWARNING;
			}
		} else {
			if(tnew->tick.y_l_data_bottom < tnew->tick.ir_ylmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YLDataBottomF is less than the minimum value in YLCoordPoints, resetting YLDataBottomF to minimum of YLCoordPoints");
				tnew->tick.y_l_data_bottom= tnew->tick.ir_ylmin;
				ret = NhlWARNING;
			}
			if(tnew->tick.y_l_data_top > tnew->tick.ir_ylmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YLDataTopF is greater than the maximum value in YLCoordPoints, resetting YLDataTopF to maximum of YLCoordPoints");
				tnew->tick.y_l_data_top = tnew->tick.ir_ylmax;
				ret = NhlWARNING;
			}
		}
	}

	return(ret);
}


/*
 * Function:	CheckTime
 *
 * Description: Does nothing currently but state that NhlTIME style has not
 *		been implemented.
 *
 * In Args:	tnew 	new instance record
 *		told	old instance record only if c_or_s == SET
 *		c_or_s	either SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckTime
#if	NhlNeedProto
(NhlTickMarkLayer tnew, NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	int		c_or_s;
#endif
{
	char *error_lead;
	NhlErrorTypes ret = NhlNOERROR;

	if(c_or_s == CREATE) {
		error_lead = "TickMarkInitialize";
	} else {
		error_lead = "TickMarkSetValues";
	}
        if((tnew->tick.x_b_on)
                &&(tnew->tick.x_b_style == NhlTIME)) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlTIME tickmarks have not been implemented, default to NhlLINEAR for bottom X-axis",error_lead);
                tnew->tick.x_b_style = NhlLINEAR;
		ret = NhlWARNING;
        }
        if((tnew->tick.x_t_on)
                &&(tnew->tick.x_t_style == NhlTIME)) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlTIME tickmarks have not been implemented, default to NhlLINEAR for top X-axis",error_lead);
                tnew->tick.x_t_style = NhlLINEAR;
		ret = NhlWARNING;
        }
        if((tnew->tick.y_r_on)
                &&(tnew->tick.y_r_style == NhlTIME)) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlTIME tickmarks have not been implemented, default to NhlLINEAR for right Y-axis",error_lead);
                tnew->tick.y_r_style = NhlLINEAR;
		ret = NhlWARNING;
        }
        if((tnew->tick.y_l_on)
                &&(tnew->tick.y_l_style == NhlTIME)) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlTIME tickmarks have not been implemented, default to NhlLINEAR for left Y-axis",error_lead);
                tnew->tick.y_l_style = NhlLINEAR;
		ret = NhlWARNING;
        }
	return(ret);

}

/*
 * Function:	CheckGeographic
 *
 * Description: Currently does nothing except report errors if NhlGEOGRAPHIC
 *		style set for any axis.
 *
 * In Args:	tnew	new instance pointer
 *		told 	old instance pointer only if c_or_s == SET
 *		c_or_s	either set to SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	NONE
 */
/*ARGSUSED*/
static NhlErrorTypes CheckGeographic
#if	NhlNeedProto
(NhlTickMarkLayer tnew,NhlTickMarkLayer told,int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	int	c_or_s;
#endif
{
	char * error_lead;
	NhlErrorTypes ret = NhlNOERROR;
	/*NhlBoolean xb_geo,yl_geo,xt_geo,yr_geo;*/
	
	if(c_or_s == CREATE) {
		error_lead = "TickMarkInitialize";
	} else {		
		error_lead = "TickMarkSetValues";
	}

        if((tnew->tick.x_b_on)
                &&(tnew->tick.x_b_style == NhlGEOGRAPHIC)) {
                 NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlGEOGRAPHIC tickmarks have not been implemented, default XB to NhlLINEAR",error_lead);
                 tnew->tick.x_b_style = NhlLINEAR;
        }
        if((tnew->tick.x_t_on)
                &&(tnew->tick.x_t_style == NhlGEOGRAPHIC)) {
                 NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlGEOGRAPHIC tickmarks have not been implemented, default XT to NhlLINEAR",error_lead);
                 tnew->tick.x_t_style = NhlLINEAR;
        }
        if((tnew->tick.y_l_on)
                &&(tnew->tick.y_l_style == NhlGEOGRAPHIC)) {
                 NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlGEOGRAPHIC tickmarks have not been implemented, default YL to NhlLINEAR",error_lead);
                 tnew->tick.y_l_style = NhlLINEAR;
        }
        if((tnew->tick.y_r_on)
                &&(tnew->tick.y_r_style == NhlGEOGRAPHIC)) {
                 NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlGEOGRAPHIC tickmarks have not been implemented, default YR to NhlLINEAR",error_lead);
                 tnew->tick.y_r_style = NhlLINEAR;
        }

	return(ret);
}



/*
 * Function:	ComputeTickInfo
 *
 * Description: Switches on all three modes for all for axis and calls the
 *		appropriate tick mark computation function. These are:
 *		ExplicitComputeMajorTickMarks
 *		AutoComputeMajorTickMarks
 *		ManualComputeMajorTickMarks
 *		For NhlMANUAL and NhlEXPLICIT mode the function ComputeMinorTickMarks
 *		is called to compute the data locations of the minor tick marks.
 *
 * In Args:	tnew	new instance record
 *		told	old instance record if c_or_s == SET
 *		c_or_s	c_or_s == SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
/*ARGSUSED*/
static NhlErrorTypes ComputeTickInfo
#if	NhlNeedProto
(NhlTickMarkLayer tnew,NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	int	c_or_s;
#endif
{
	NhlErrorTypes minorret = NhlNOERROR;
	NhlErrorTypes majorret = NhlNOERROR;
	NhlErrorTypes bottom = NhlNOERROR;
	NhlErrorTypes top = NhlNOERROR;
	NhlErrorTypes left = NhlNOERROR;
	NhlErrorTypes right = NhlNOERROR;
	int sug_minor_ticks;

/*
* NEED SOME KIND OF CHECKS HERE TO SEE IF RECOMPUTATION NEEDED
*/	
	switch(tnew->tick.x_b_mode) {
	case NhlAUTOMATIC:
		if (! tnew->tick.x_b_data_valid) {
			tnew->tick.x_b_tick_spacing = 0.0;
			tnew->tick.x_b_nmajor = 0;
			tnew->tick.x_b_nminor = 0;
			tnew->tick.x_b_tick_start = tnew->tick.x_b_data_min;
			tnew->tick.x_b_tick_end = tnew->tick.x_b_data_max;
		}
		else {
			majorret = AutoComputeMajorTickMarks(
					tnew->tick.x_b_style,
                                        tnew->tick.x_b_major_data_locs,
                                        tnew->tick.x_b_major_labels,
                                        tnew->tick.x_b_max_ticks,
                                        tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        &tnew->tick.x_b_tick_start,
                                        &tnew->tick.x_b_tick_end,
                                        (tnew->tick.x_b_auto_precision?
					 -1:tnew->tick.x_b_precision),
                                        &tnew->tick.x_b_tick_spacing,
                                        &tnew->tick.x_b_nmajor,
                                        tnew->tick.sci_note_cutoff,
					&tnew->tick.x_b_format,
					tnew->tick.x_b_label_fcode,
					&sug_minor_ticks,
					tnew->tick.x_b_min_nonzero);
			minorret = ComputeMinorTickMarks(
                                        (tnew->tick.x_b_minor_per_major_set ? tnew->tick.x_b_minor_per_major:sug_minor_ticks),
                                        tnew->tick.x_b_tick_spacing,
                                        tnew->tick.x_b_tick_start,
                                        tnew->tick.x_b_tick_end,
                                        tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        tnew->tick.x_b_major_data_locs,
                                        tnew->tick.x_b_nmajor,
                                        tnew->tick.x_b_minor_data_locs,
                                        &tnew->tick.x_b_nminor,
                                        tnew->tick.x_b_precision,
                                        tnew->tick.x_b_style,
					tnew->tick.x_b_min_nonzero);
		}
		break;
	case NhlMANUAL:
		if (! tnew->tick.x_b_data_valid) {
			tnew->tick.x_b_nmajor = 0;
			tnew->tick.x_b_nminor = 0;
		}
		else {
			majorret = ManualComputeMajorTickMarks(
                                        tnew->tick.x_b_style,
                                        tnew->tick.x_b_major_data_locs,
                                        tnew->tick.x_b_major_labels,
                                        tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        tnew->tick.x_b_tick_start,
                                        tnew->tick.x_b_tick_end,
					tnew->tick.x_b_precision,
                                        tnew->tick.x_b_auto_precision,
					tnew->tick.x_b_tick_spacing,
                                        tnew->tick.x_b_spacing_type,
                                        &tnew->tick.x_b_nmajor,
                                        tnew->tick.sci_note_cutoff,
					&tnew->tick.x_b_format,
					tnew->tick.x_b_label_fcode,
					tnew->tick.x_b_min_nonzero);
			minorret = ComputeMinorTickMarks(
                                        tnew->tick.x_b_minor_per_major,
                                        tnew->tick.x_b_tick_spacing,
                                        tnew->tick.x_b_tick_start,
                                        tnew->tick.x_b_tick_end,
                                        tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        tnew->tick.x_b_major_data_locs,
                                        tnew->tick.x_b_nmajor,
                                        tnew->tick.x_b_minor_data_locs,
                                        &tnew->tick.x_b_nminor,
                                        tnew->tick.x_b_precision,
                                        tnew->tick.x_b_style,
					tnew->tick.x_b_min_nonzero);
		}
		break;
	case NhlEXPLICIT:
		if (! tnew->tick.x_b_data_valid) {
			tnew->tick.x_b_nmajor = 0;
			tnew->tick.x_b_nminor = 0;
		}
		else {
			majorret = ExplicitComputeMajorTickMarks(
                                        tnew->tick.x_b_style,
                                        tnew->tick.x_b_major_data_locs,
                                        tnew->tick.x_b_major_labels,
                                        tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        &tnew->tick.x_b_tick_start,
                                        &tnew->tick.x_b_tick_end,
                                        tnew->tick.x_b_precision,
					tnew->tick.x_b_values,
					tnew->tick.x_b_labels,
                                        &tnew->tick.x_b_nmajor,
					tnew->tick.x_b_min_nonzero);
			minorret = ExplicitComputeMinorTickMarks(
					tnew->tick.x_b_style,
					tnew->tick.x_b_minor_data_locs,
					tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        tnew->tick.x_b_tick_start,
                                        tnew->tick.x_b_tick_end,
                                        tnew->tick.x_b_precision,
                                        (tnew->tick.x_b_minor_values == NULL ?
                                         NULL : tnew->tick.x_b_minor_values->data),
					&tnew->tick.x_b_nminor,
					(tnew->tick.x_b_minor_values == NULL ?
					 0 :tnew->tick.x_b_minor_values->num_elements),
					 tnew->tick.x_b_min_nonzero);

					
		}
		break;
	default:
		tnew->tick.x_b_nmajor = 0;
		tnew->tick.x_b_nminor = 0;
		break;
	}
	if((majorret < NhlWARNING)||(minorret < NhlWARNING)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeTickInfo: A NhlFATAL error occurred while computing Tick information for the X-Axis bottom ticks");
		bottom = MIN(majorret,minorret);
	}
	majorret = NhlNOERROR;
	minorret = NhlNOERROR;

	switch(tnew->tick.x_t_mode) {
	case NhlAUTOMATIC:
		if (! tnew->tick.x_t_data_valid) {
			tnew->tick.x_t_tick_spacing = 0.0;
			tnew->tick.x_t_nmajor = 0;
			tnew->tick.x_t_nminor = 0;
			tnew->tick.x_t_tick_start = tnew->tick.x_t_data_min;
			tnew->tick.x_t_tick_end = tnew->tick.x_t_data_max;
		}
		else {
			majorret = AutoComputeMajorTickMarks(
                                        tnew->tick.x_t_style,
                                        tnew->tick.x_t_major_data_locs,
                                        tnew->tick.x_t_major_labels,
                                        tnew->tick.x_t_max_ticks,
                                        tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        &tnew->tick.x_t_tick_start,
                                        &tnew->tick.x_t_tick_end,
                                        (tnew->tick.x_t_auto_precision?
					 -1:tnew->tick.x_t_precision),
                                        &tnew->tick.x_t_tick_spacing,
                                        &tnew->tick.x_t_nmajor,
                                        tnew->tick.sci_note_cutoff,
					&tnew->tick.x_t_format,
					tnew->tick.x_t_label_fcode,
					&sug_minor_ticks,
					tnew->tick.x_t_min_nonzero);

			minorret = ComputeMinorTickMarks(
                                        (tnew->tick.x_t_minor_per_major_set ? tnew->tick.x_t_minor_per_major: sug_minor_ticks),
                                        tnew->tick.x_t_tick_spacing,
                                        tnew->tick.x_t_tick_start,
                                        tnew->tick.x_t_tick_end,
                                        tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        tnew->tick.x_t_major_data_locs,
                                        tnew->tick.x_t_nmajor,
                                        tnew->tick.x_t_minor_data_locs,
                                        &tnew->tick.x_t_nminor,
                                        tnew->tick.x_t_precision,
                                        tnew->tick.x_t_style,
					tnew->tick.x_t_min_nonzero);
		}
		break;
	case NhlMANUAL:
		if (! tnew->tick.x_t_data_valid) {
			tnew->tick.x_t_nmajor = 0;
			tnew->tick.x_t_nminor = 0;
		}
		else {
			majorret = ManualComputeMajorTickMarks(
                                        tnew->tick.x_t_style,
                                        tnew->tick.x_t_major_data_locs,
                                        tnew->tick.x_t_major_labels,
                                        tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        tnew->tick.x_t_tick_start,
                                        tnew->tick.x_t_tick_end,
					tnew->tick.x_t_precision,
                                        tnew->tick.x_t_auto_precision,
                                        tnew->tick.x_t_tick_spacing,
                                        tnew->tick.x_t_spacing_type,
                                        &tnew->tick.x_t_nmajor,
                                        tnew->tick.sci_note_cutoff,
					&tnew->tick.x_t_format,
					tnew->tick.x_t_label_fcode,
					tnew->tick.x_t_min_nonzero);

			minorret = ComputeMinorTickMarks(
                                        tnew->tick.x_t_minor_per_major,
                                        tnew->tick.x_t_tick_spacing,
                                        tnew->tick.x_t_tick_start,
                                        tnew->tick.x_t_tick_end,
                                        tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        tnew->tick.x_t_major_data_locs,
                                        tnew->tick.x_t_nmajor,
                                        tnew->tick.x_t_minor_data_locs,
                                        &tnew->tick.x_t_nminor,
                                        tnew->tick.x_t_precision,
                                        tnew->tick.x_t_style,
					tnew->tick.x_t_min_nonzero);
		}
		break;
	case NhlEXPLICIT:
		if (! tnew->tick.x_t_data_valid) {
			tnew->tick.x_t_nmajor = 0;
			tnew->tick.x_t_nminor = 0;
		}
		else {
			majorret = ExplicitComputeMajorTickMarks(
                                        tnew->tick.x_t_style,
                                        tnew->tick.x_t_major_data_locs,
                                        tnew->tick.x_t_major_labels,
                                        tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        &tnew->tick.x_t_tick_start,
                                        &tnew->tick.x_t_tick_end,
                                        tnew->tick.x_t_precision,
					tnew->tick.x_t_values,
					tnew->tick.x_t_labels,
                                        &tnew->tick.x_t_nmajor,
					tnew->tick.x_t_min_nonzero);
			minorret = ExplicitComputeMinorTickMarks(
					tnew->tick.x_t_style,
					tnew->tick.x_t_minor_data_locs,
					tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        tnew->tick.x_t_tick_start,
                                        tnew->tick.x_t_tick_end,
                                        tnew->tick.x_t_precision,
                                        (tnew->tick.x_t_minor_values == NULL ?
                                         NULL : tnew->tick.x_t_minor_values->data),
					&tnew->tick.x_t_nminor,
					(tnew->tick.x_t_minor_values == NULL ?
					 0 :tnew->tick.x_t_minor_values->num_elements),
					tnew->tick.x_t_min_nonzero);
		}
		break;
	default:
		tnew->tick.x_t_nmajor = 0;
		tnew->tick.x_t_nminor = 0;
		break;
	}
	if((majorret < NhlWARNING)||(minorret < NhlWARNING)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeTickInfo: A NhlFATAL error occurred while computing Tick information for the X-Axis top ticks");
		top = MIN(majorret,minorret);
	}
	majorret = NhlNOERROR;
	minorret = NhlNOERROR;

	switch(tnew->tick.y_l_mode) {
	case NhlAUTOMATIC:
		if (! tnew->tick.y_l_data_valid) {
			tnew->tick.y_l_tick_spacing = 0.0;
			tnew->tick.y_l_nmajor = 0;
			tnew->tick.y_l_nminor = 0;
			tnew->tick.y_l_tick_start = tnew->tick.y_l_data_min;
			tnew->tick.y_l_tick_end = tnew->tick.y_l_data_max;
		}
		else {
			majorret = AutoComputeMajorTickMarks(
                                        tnew->tick.y_l_style,
                                        tnew->tick.y_l_major_data_locs,
                                        tnew->tick.y_l_major_labels,
                                        tnew->tick.y_l_max_ticks,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        &tnew->tick.y_l_tick_start,
                                        &tnew->tick.y_l_tick_end,
                                        (tnew->tick.y_l_auto_precision?
					 -1:tnew->tick.y_l_precision),
                                        &tnew->tick.y_l_tick_spacing,
                                        &tnew->tick.y_l_nmajor,
                                        tnew->tick.sci_note_cutoff,
					&tnew->tick.y_l_format,
					tnew->tick.y_l_label_fcode,
					&sug_minor_ticks,
					tnew->tick.y_l_min_nonzero);

			minorret = ComputeMinorTickMarks(
                                        (tnew->tick.y_l_minor_per_major_set?tnew->tick.y_l_minor_per_major:sug_minor_ticks),
                                        tnew->tick.y_l_tick_spacing,
                                        tnew->tick.y_l_tick_start,
                                        tnew->tick.y_l_tick_end,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        tnew->tick.y_l_major_data_locs,
                                        tnew->tick.y_l_nmajor,
                                        tnew->tick.y_l_minor_data_locs,
                                        &tnew->tick.y_l_nminor,
                                        tnew->tick.y_l_precision,
                                        tnew->tick.y_l_style,
					tnew->tick.y_l_min_nonzero);
		}
		break;
	case NhlMANUAL:
		if (! tnew->tick.y_l_data_valid) {
			tnew->tick.y_l_nmajor = 0;
			tnew->tick.y_l_nminor = 0;
		}
		else {
			majorret = ManualComputeMajorTickMarks(
                                        tnew->tick.y_l_style,
                                        tnew->tick.y_l_major_data_locs,
                                        tnew->tick.y_l_major_labels,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        tnew->tick.y_l_tick_start,
                                        tnew->tick.y_l_tick_end,
					tnew->tick.y_l_precision,
                                        tnew->tick.y_l_auto_precision,
                                        tnew->tick.y_l_tick_spacing,
                                        tnew->tick.y_l_spacing_type,
                                        &tnew->tick.y_l_nmajor,
                                        tnew->tick.sci_note_cutoff,
					&tnew->tick.y_l_format,
					tnew->tick.y_l_label_fcode,
					tnew->tick.y_l_min_nonzero);

			minorret = ComputeMinorTickMarks(
                                        tnew->tick.y_l_minor_per_major,
                                        tnew->tick.y_l_tick_spacing,
                                        tnew->tick.y_l_tick_start,
                                        tnew->tick.y_l_tick_end,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        tnew->tick.y_l_major_data_locs,
                                        tnew->tick.y_l_nmajor,
                                        tnew->tick.y_l_minor_data_locs,
                                        &tnew->tick.y_l_nminor,
                                        tnew->tick.y_l_precision,
                                        tnew->tick.y_l_style,
					tnew->tick.y_l_min_nonzero);
		}
		break;
	case NhlEXPLICIT:
		if (! tnew->tick.y_l_data_valid) {
			tnew->tick.y_l_nmajor = 0;
			tnew->tick.y_l_nminor = 0;
		}
		else {
			majorret = ExplicitComputeMajorTickMarks(
                                        tnew->tick.y_l_style,
                                        tnew->tick.y_l_major_data_locs,
                                        tnew->tick.y_l_major_labels,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        &tnew->tick.y_l_tick_start,
                                        &tnew->tick.y_l_tick_end,
                                        tnew->tick.y_l_precision,
					tnew->tick.y_l_values,
					tnew->tick.y_l_labels,
                                        &tnew->tick.y_l_nmajor,
					tnew->tick.y_l_min_nonzero);
			minorret = ExplicitComputeMinorTickMarks(
                                        tnew->tick.y_l_style,
                                        tnew->tick.y_l_minor_data_locs,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        tnew->tick.y_l_tick_start,
                                        tnew->tick.y_l_tick_end,
                                        tnew->tick.y_l_precision,
                                        (tnew->tick.y_l_minor_values == NULL ?
                                         NULL : tnew->tick.y_l_minor_values->data),
                                        &tnew->tick.y_l_nminor,
					(tnew->tick.y_l_minor_values == NULL ?
					 0 :tnew->tick.y_l_minor_values->num_elements),
					tnew->tick.y_l_min_nonzero);

		}
		break;
	default:
		tnew->tick.y_l_nmajor = 0;
		tnew->tick.y_l_nminor = 0;
		break;
	}
	if((majorret < NhlWARNING)||(minorret < NhlWARNING)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeTickInfo: A NhlFATAL error occurred while computing Tick information for the Y-Axis left ticks");
		left = MIN(majorret,minorret);
	}
	majorret = NhlNOERROR;
	minorret = NhlNOERROR;

	switch(tnew->tick.y_r_mode) {
	case NhlAUTOMATIC:
		if (! tnew->tick.y_r_data_valid) {
			tnew->tick.y_r_tick_spacing = 0.0;
			tnew->tick.y_r_nmajor = 0;
			tnew->tick.y_r_nminor = 0;
			tnew->tick.y_r_tick_start = tnew->tick.y_r_data_min;
			tnew->tick.y_r_tick_end = tnew->tick.y_r_data_max;
		}
		else {
			majorret = AutoComputeMajorTickMarks(
                                        tnew->tick.y_r_style,
                                        tnew->tick.y_r_major_data_locs,
                                        tnew->tick.y_r_major_labels,
                                        tnew->tick.y_r_max_ticks,
                                        tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        &tnew->tick.y_r_tick_start,
                                        &tnew->tick.y_r_tick_end,
                                        (tnew->tick.y_r_auto_precision?
					 -1:tnew->tick.y_r_precision),
                                        &tnew->tick.y_r_tick_spacing,
                                        &tnew->tick.y_r_nmajor,
                                        tnew->tick.sci_note_cutoff,
					&tnew->tick.y_r_format,
					tnew->tick.y_r_label_fcode,
					&sug_minor_ticks,
					tnew->tick.y_r_min_nonzero);

			minorret = ComputeMinorTickMarks(
                                        (tnew->tick.y_r_minor_per_major_set ? tnew->tick.y_r_minor_per_major:sug_minor_ticks),
                                        tnew->tick.y_r_tick_spacing,
                                        tnew->tick.y_r_tick_start,
                                        tnew->tick.y_r_tick_end,
                                        tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        tnew->tick.y_r_major_data_locs,
                                        tnew->tick.y_r_nmajor,
                                        tnew->tick.y_r_minor_data_locs,
                                        &tnew->tick.y_r_nminor,
                                        tnew->tick.y_r_precision,
                                        tnew->tick.y_r_style,
					tnew->tick.y_r_min_nonzero);
		}
		break;
	case NhlMANUAL:
		if (! tnew->tick.y_r_data_valid) {
			tnew->tick.y_r_nmajor = 0;
			tnew->tick.y_r_nminor = 0;
		}
		else {
			majorret = ManualComputeMajorTickMarks(
                                        tnew->tick.y_r_style,
                                        tnew->tick.y_r_major_data_locs,
                                        tnew->tick.y_r_major_labels,
                                        tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        tnew->tick.y_r_tick_start,
                                        tnew->tick.y_r_tick_end,
					tnew->tick.y_r_precision,
                                        tnew->tick.y_r_auto_precision,
                                        tnew->tick.y_r_tick_spacing,
                                        tnew->tick.y_r_spacing_type,
                                        &tnew->tick.y_r_nmajor,
                                        tnew->tick.sci_note_cutoff,
					&tnew->tick.y_r_format,
					tnew->tick.y_r_label_fcode,
					tnew->tick.y_r_min_nonzero);

			minorret = ComputeMinorTickMarks(
                                        tnew->tick.y_r_minor_per_major,
                                        tnew->tick.y_r_tick_spacing,
                                        tnew->tick.y_r_tick_start,
                                        tnew->tick.y_r_tick_end,
                                        tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        tnew->tick.y_r_major_data_locs,
                                        tnew->tick.y_r_nmajor,
                                        tnew->tick.y_r_minor_data_locs,
                                        &tnew->tick.y_r_nminor,
                                        tnew->tick.y_r_precision,
                                        tnew->tick.y_r_style,
					tnew->tick.y_r_min_nonzero);
		}
		break;
	case NhlEXPLICIT:
		if (! tnew->tick.y_r_data_valid) {
			tnew->tick.y_r_nmajor = 0;
			tnew->tick.y_r_nminor = 0;
		}
		else {
			majorret = ExplicitComputeMajorTickMarks(
                                        tnew->tick.y_r_style,
                                        tnew->tick.y_r_major_data_locs,
                                        tnew->tick.y_r_major_labels,
                                        tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        &tnew->tick.y_r_tick_start,
                                        &tnew->tick.y_r_tick_end,
                                        tnew->tick.y_r_precision,
					tnew->tick.y_r_values,
					tnew->tick.y_r_labels,
                                        &tnew->tick.y_r_nmajor,
					tnew->tick.y_r_min_nonzero);
			minorret = ExplicitComputeMinorTickMarks(
					tnew->tick.y_r_style,
					tnew->tick.y_r_minor_data_locs,
					tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        tnew->tick.y_r_tick_start,
                                        tnew->tick.y_r_tick_end,
                                        tnew->tick.y_r_precision,
                                        (tnew->tick.y_r_minor_values == NULL ?
                                         NULL : tnew->tick.y_r_minor_values->data),
					&tnew->tick.y_r_nminor,
					(tnew->tick.y_r_minor_values == NULL ?
					 0 :tnew->tick.y_r_minor_values->num_elements),
					tnew->tick.y_r_min_nonzero);

		}
		break;
	default:
		tnew->tick.y_r_nmajor = 0;
		tnew->tick.y_r_nminor = 0;
		break;
	}
	if((majorret < NhlWARNING)||(minorret < NhlWARNING)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeTickInfo: A NhlFATAL error occurred while computing Tick information for the Y-Axis right ticks");
		right = MIN(majorret,minorret);
	}
	majorret = NhlNOERROR;
	minorret = NhlNOERROR;

	return(MIN(MIN(top,bottom),MIN(left,right)));
}


/*
 * Function:	TransformLocations
 *
 * Description:	This function transforms all of the data locations for all
 *		for axis for both major and minor axis. The results are placed
 *		in *_*_major_ndc_locs and in *_*_minor_ndc_locs. The TickMark
 *		draw function uses these arrays for generating the output. The
 *		functions _NhlSetTrans _NhlDataToWin and _NhlWinToNDC to 
 *		transform the data. The bottom and left axis have one 
 *		transformation and the top and right have another.
 *
 * In Args:	tnew	new instance record
 *		told	old instance record
 *		c_or_s 	set to SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values: NONE
 *
 * Side Effects: NONE
 */
/*ARGSUSED*/
static NhlErrorTypes TransformLocations
#if	NhlNeedProto
(NhlTickMarkLayer tnew, NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	int		c_or_s;
#endif
{
	int i,tmpi=0,status = 0;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes subret = NhlNOERROR;
	char *error_lead;
	int j;
	float orv;

	if(c_or_s == SET) {
		error_lead = "TickMarkSetValues";
	} else {
		error_lead = "TickMarkInitialize";
	}

	if (! (tnew->tick.x_b_data_valid && tnew->tick.y_l_data_valid)) {
		goto DO_XT_YR; /* a no no, but it's the simplest for now */
	}
	if(tnew->tick.xb_yl_trans_obj != NULL) {
        subret = _NhlSetTrans((NhlLayer)tnew->tick.xb_yl_trans_obj,(NhlLayer)tnew);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Cannot set the transformation for bottom and left tick marks, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
/*
* Unfortunately _NhlDataToWin takes 2 equal length arrays. So something has
* to be done to the short one. Totaly bogus but it beats rewriting the
* transformation object class to support this.
*/
	if((tnew->tick.x_b_nmajor != 0)&&(tnew->tick.y_l_nmajor!=0)) {
        	if(tnew->tick.x_b_nmajor > tnew->tick.y_l_nmajor) {
                	tmpi = tnew->tick.x_b_nmajor;
                	for(i = tnew->tick.y_l_nmajor; i < tnew->tick.x_b_nmajor; i++) {tnew->tick.y_l_major_data_locs[i] = tnew->tick.y_l_major_data_locs[i%tnew->tick.y_l_nmajor];
                	}
        	} else {
                	tmpi = tnew->tick.y_l_nmajor;
                	for(i = tnew->tick.x_b_nmajor; i < tnew->tick.y_l_nmajor; i++) {tnew->tick.x_b_major_data_locs[i] = tnew->tick.x_b_major_data_locs[i%tnew->tick.x_b_nmajor];
               	 	}
        	}
	} else if(tnew->tick.x_b_nmajor == 0) {
		tmpi = tnew->tick.y_l_nmajor;
		for(i=0;i<tnew->tick.y_l_nmajor; i++) {
			tnew->tick.x_b_major_data_locs[i] = tnew->tick.x_b_data_min;
		}
	} else {
		tmpi = tnew->tick.x_b_nmajor;
		for(i=0;i<tnew->tick.x_b_nmajor; i++) {
			tnew->tick.y_l_major_data_locs[i] = tnew->tick.y_l_data_min;
		}
	}
        subret = _NhlDataToWin((NhlLayer)tnew->tick.xb_yl_trans_obj,
                tnew->tick.x_b_major_data_locs,tnew->tick.y_l_major_data_locs,
                tmpi,
                tnew->tick.x_b_major_ndc_locs,tnew->tick.y_l_major_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occurred while mapping the bottom and left tick marks to the window, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
        if(status){
/*
*
*----------> There is a MAJOR possibility of problems occuring here <----------------
*
* Essentially some of the dummy values that are filled in (see comment that starts with
* "Unfortunately" above) could be out_of_range. If a dummy is out of range the transformation
* function will replace the possibly valid non-dummy value with the orv value causing that
* tick mark to drop out!! Not sure what I can do about it!!
*/

/*
* Contains an out of range value
*/
		NhlVAGetValues(tnew->tick.xb_yl_trans_obj->base.id,
			NhlNtrOutOfRangeF,&orv,
			NULL);
		j = 0;
		for(i = 0; i< tmpi; i++) {
/*
* both arrays contain orv values at same index
*/
			if(tnew->tick.x_b_major_ndc_locs[i] != orv) {
				tnew->tick.x_b_major_ndc_locs[j] = tnew->tick.x_b_major_ndc_locs[i];
				tnew->tick.y_l_major_ndc_locs[j] = tnew->tick.y_l_major_ndc_locs[i];
				j++;
			} else {
				if(i < tnew->tick.x_b_nmajor) {
					tnew->tick.x_b_nmajor--;
				}
				if(i < tnew->tick.y_l_nmajor) {
					tnew->tick.y_l_nmajor--;
				}
				tmpi--;	
			}
		}
	}
        status = 0;
        subret = _NhlWinToNDC((NhlLayer)tnew->tick.xb_yl_trans_obj,
                tnew->tick.x_b_major_ndc_locs,tnew->tick.y_l_major_ndc_locs,
                tmpi,
                tnew->tick.x_b_major_ndc_locs,tnew->tick.y_l_major_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occurred while mapping the bottom and left tick marks from the window to NDC, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
        if(status){
/*
* Contains an out of range value
*/
		NhlVAGetValues(tnew->tick.xb_yl_trans_obj->base.id,
			NhlNtrOutOfRangeF,&orv,
			NULL);
		j = 0;
		for(i = 0; i< tmpi; i++) {
/*
* both arrays contain orv values at same index
*/
			if(tnew->tick.x_b_major_ndc_locs[i] != orv) {
				tnew->tick.x_b_major_ndc_locs[j] = tnew->tick.x_b_major_ndc_locs[i];
				tnew->tick.y_l_major_ndc_locs[j] = tnew->tick.y_l_major_ndc_locs[i];
				j++;
			} else {
				if(i < tnew->tick.x_b_nmajor) {
					tnew->tick.x_b_nmajor--;
				}
				if(i < tnew->tick.y_l_nmajor) {
					tnew->tick.y_l_nmajor--;
				}
				tmpi--;	
			}
		}
	}
	


	if(/*(tnew->tick.x_b_nmajor != 0)&&(tnew->tick.y_l_nmajor !=0)&&*/(tnew->tick.x_b_nminor != 0)&&(tnew->tick.y_l_nminor != 0)){
        	if(tnew->tick.x_b_nminor > tnew->tick.y_l_nminor) {
                	tmpi = tnew->tick.x_b_nminor;
                	for(i = tnew->tick.y_l_nminor; i < tnew->tick.x_b_nminor; i++) {
				tnew->tick.y_l_minor_data_locs[i] = tnew->tick.y_l_minor_data_locs[i%tnew->tick.y_l_nminor];
                	}
        	} else {
                	tmpi = tnew->tick.y_l_nminor;
                	for(i = tnew->tick.x_b_nminor; i < tnew->tick.y_l_nminor; i++) {                        tnew->tick.x_b_minor_data_locs[i] = tnew->tick.x_b_minor_data_locs[i%tnew->tick.x_b_nminor];
                	}
        	}
	} else if((tnew->tick.x_b_nminor == 0)/*|| (tnew->tick.x_b_nmajor ==0)*/){
		tmpi = tnew->tick.y_l_nminor;
		for(i=0;i<tnew->tick.y_l_nminor; i++) {
			tnew->tick.x_b_minor_data_locs[i] = tnew->tick.x_b_data_min;
		}
	} else if((tnew->tick.y_l_nminor == 0)/*|| (tnew->tick.y_l_nmajor ==0)*/){
		tmpi = tnew->tick.x_b_nminor;
		for(i=0;i<tnew->tick.x_b_nminor; i++) {
			tnew->tick.y_l_minor_data_locs[i] = tnew->tick.y_l_data_min;
		}
	} else { 
		tmpi = 0;
	}
        subret = _NhlDataToWin((NhlLayer)tnew->tick.xb_yl_trans_obj,
                tnew->tick.x_b_minor_data_locs,tnew->tick.y_l_minor_data_locs,
                tmpi,
                tnew->tick.x_b_minor_ndc_locs,tnew->tick.y_l_minor_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: A NhlFATAL error occurred while mapping minor tickmarks to the window, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
        if(status){
/*
* Contains and out of range value
*/
		NhlVAGetValues(tnew->tick.xb_yl_trans_obj->base.id,
			NhlNtrOutOfRangeF,&orv,
			NULL);
		j = 0;
		for(i = 0; i< tmpi; i++) {
/*
* both arrays contain orv values at same index
*/
			if(tnew->tick.x_b_minor_ndc_locs[i] != orv) {
				tnew->tick.x_b_minor_ndc_locs[j] = tnew->tick.x_b_minor_ndc_locs[i];
				tnew->tick.y_l_minor_ndc_locs[j] = tnew->tick.y_l_minor_ndc_locs[i];
				j++;
			} else {
				if(i < tnew->tick.x_b_nminor) {
					tnew->tick.x_b_nminor--;
				}
				if(i < tnew->tick.y_l_nminor) {
					tnew->tick.y_l_nminor--;
				}
				tmpi--;	
			}
		}
	}
        status= 0;
        subret = _NhlWinToNDC((NhlLayer)tnew->tick.xb_yl_trans_obj,
                tnew->tick.x_b_minor_ndc_locs,tnew->tick.y_l_minor_ndc_locs,
                tmpi,
                tnew->tick.x_b_minor_ndc_locs,tnew->tick.y_l_minor_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: A NhlFATAL error occurred while mapping minor tickmarks from the window to NDC, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
	if(status){
/*
* Contains and out of range value
*/
		NhlVAGetValues(tnew->tick.xb_yl_trans_obj->base.id,
			NhlNtrOutOfRangeF,&orv,
			NULL);
		j = 0;
		for(i = 0; i< tmpi; i++) {
/*
* both arrays contain orv values at same index
*/
			if(tnew->tick.x_b_minor_ndc_locs[i] != orv) {
				tnew->tick.x_b_minor_ndc_locs[j] = tnew->tick.x_b_minor_ndc_locs[i];
				tnew->tick.y_l_minor_ndc_locs[j] = tnew->tick.y_l_minor_ndc_locs[i];
				j++;
			} else {
				if(i < tnew->tick.x_b_nminor) {
					tnew->tick.x_b_nminor--;
				}
				if(i < tnew->tick.y_l_nminor) {
					tnew->tick.y_l_nminor--;
				}
				tmpi--;	
			}
		}
	}
	}

 DO_XT_YR:

	if (! (tnew->tick.x_t_data_valid && tnew->tick.y_r_data_valid)) {
		return ret;
	}

	if(tnew->tick.xt_yr_trans_obj != NULL) {
        subret = _NhlSetTrans((NhlLayer)tnew->tick.xt_yr_trans_obj,(NhlLayer)tnew);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Cannot set the transformation for top and right tick marks, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
	
/*
* Unfortunately _NhlDataToWin takes 2 equal length arrays. So something has
* to be done to the short one. Totaly bogus but it beats rewriting the
* transformation object class to support this.
*/
	if((tnew->tick.x_t_nmajor != 0)&&(tnew->tick.y_r_nmajor != 0)) {
        	if(tnew->tick.x_t_nmajor > tnew->tick.y_r_nmajor) {
                	tmpi = tnew->tick.x_t_nmajor;
                	for(i = tnew->tick.y_r_nmajor; i < tnew->tick.x_t_nmajor; i++) {tnew->tick.y_r_major_data_locs[i] = tnew->tick.y_r_major_data_locs[i%tnew->tick.y_r_nmajor];
                	}
        	} else {
                	tmpi = tnew->tick.y_r_nmajor;
                	for(i = tnew->tick.x_t_nmajor; i < tnew->tick.y_r_nmajor; i++) {tnew->tick.x_t_major_data_locs[i] = tnew->tick.x_t_major_data_locs[i%tnew->tick.x_t_nmajor];
                	}
        	}
	} else if(tnew->tick.x_t_nmajor==0) {
		tmpi = tnew->tick.y_r_nmajor;
		for(i=0;i<tnew->tick.y_r_nmajor; i++) {
			tnew->tick.x_t_major_data_locs[i] = tnew->tick.x_t_data_min;
		}
	} else if(tnew->tick.y_r_nmajor==0){
		tmpi = tnew->tick.x_t_nmajor;
		for(i=0;i<tnew->tick.x_t_nmajor; i++) {
			tnew->tick.y_r_major_data_locs[i] = tnew->tick.y_r_data_min;
		}
	} 
        subret = _NhlDataToWin((NhlLayer)tnew->tick.xt_yr_trans_obj,
                tnew->tick.x_t_major_data_locs,tnew->tick.y_r_major_data_locs,
                tmpi,
                tnew->tick.x_t_major_ndc_locs,tnew->tick.y_r_major_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occurred while mapping the top and right tick marks to the window, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
        if(status){
/*
* Contains and out of range value
*/
		NhlVAGetValues(tnew->tick.xt_yr_trans_obj->base.id,
			NhlNtrOutOfRangeF,&orv,
			NULL);
		j = 0;
		for(i = 0; i< tmpi; i++) {
/*
* both arrays contain orv values at same index
*/
			if(tnew->tick.x_t_major_ndc_locs[i] != orv) {
				tnew->tick.x_t_major_ndc_locs[j] = tnew->tick.x_t_major_ndc_locs[i];
				tnew->tick.y_r_major_ndc_locs[j] = tnew->tick.y_r_major_ndc_locs[i];
				j++;
			} else {
				if(i < tnew->tick.x_t_nmajor) {
					tnew->tick.x_t_nmajor--;
				}
				if(i < tnew->tick.y_r_nmajor) {
					tnew->tick.y_r_nmajor--;
				}
				tmpi--;	
			}
		}
	}
        status= 0;
        subret = _NhlWinToNDC((NhlLayer)tnew->tick.xt_yr_trans_obj,
                tnew->tick.x_t_major_ndc_locs,tnew->tick.y_r_major_ndc_locs,
                tmpi,
                tnew->tick.x_t_major_ndc_locs,tnew->tick.y_r_major_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occurred while mapping the top and right tick marks from the window to NDC, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
        if(status){
/*
* Contains and out of range value
*/
		NhlVAGetValues(tnew->tick.xt_yr_trans_obj->base.id,
			NhlNtrOutOfRangeF,&orv,
			NULL);
		j = 0;
		for(i = 0; i< tmpi; i++) {
/*
* both arrays contain orv values at same index
*/
			if(tnew->tick.x_t_major_ndc_locs[i] != orv) {
				tnew->tick.x_t_major_ndc_locs[j] = tnew->tick.x_t_major_ndc_locs[i];
				tnew->tick.y_r_major_ndc_locs[j] = tnew->tick.y_r_major_ndc_locs[i];
				j++;
			} else {
				if(i < tnew->tick.x_t_nmajor) {
					tnew->tick.x_t_nmajor--;
				}
				if(i < tnew->tick.y_r_nmajor) {
					tnew->tick.y_r_nmajor--;
				}
				tmpi--;	
			}
		}
	}

	if(/*(tnew->tick.x_t_nmajor != 0)&&(tnew->tick.y_r_nmajor != 0)&&*/(tnew->tick.x_t_nminor !=0) &&(tnew->tick.y_r_nminor !=0)) {
        	if(tnew->tick.x_t_nminor > tnew->tick.y_r_nminor) {
                	tmpi = tnew->tick.x_t_nminor;
                	for(i = tnew->tick.y_r_nminor; i < tnew->tick.x_t_nminor; i++) {tnew->tick.y_r_minor_data_locs[i] = tnew->tick.y_r_minor_data_locs[i%tnew->tick.y_r_nminor];
                	}
        	} else {
                	tmpi = tnew->tick.y_r_nminor;
                	for(i = tnew->tick.x_t_nminor; i < tnew->tick.y_r_nminor; i++) {tnew->tick.x_t_minor_data_locs[i] = tnew->tick.x_t_minor_data_locs[i%tnew->tick.x_t_nminor];
                	}
        	}
	} else if((tnew->tick.x_t_nminor == 0)/*||(tnew->tick.x_t_nmajor == 0)*/) {
		tmpi = tnew->tick.y_r_nminor;
		for(i=0;i<tnew->tick.y_r_nminor; i++) {
			tnew->tick.x_t_minor_data_locs[i] = tnew->tick.x_t_data_min;
		}
	} else if((tnew->tick.y_r_nminor == 0)/*||(tnew->tick.y_l_nmajor == 0)*/){
		tmpi = tnew->tick.x_t_nminor;
		for(i=0;i<tnew->tick.x_t_nminor; i++) {
			tnew->tick.y_r_minor_data_locs[i] = tnew->tick.y_r_data_min;
		}
	} else {
		tmpi = 0;
	}
        subret = _NhlDataToWin((NhlLayer)tnew->tick.xt_yr_trans_obj,
                tnew->tick.x_t_minor_data_locs,tnew->tick.y_r_minor_data_locs,
                tmpi,
                tnew->tick.x_t_minor_ndc_locs,tnew->tick.y_r_minor_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occurred while mapping the top and right minor tick marks to the window, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
        if(status){
/*
* Contains and out of range value
*/
		NhlVAGetValues(tnew->tick.xt_yr_trans_obj->base.id,
			NhlNtrOutOfRangeF,&orv,
			NULL);
		j = 0;
		for(i = 0; i< tmpi; i++) {
/*
* both arrays contain orv values at same index
*/
			if(tnew->tick.x_t_minor_ndc_locs[i] != orv) {
				tnew->tick.x_t_minor_ndc_locs[j] = tnew->tick.x_t_minor_ndc_locs[i];
				tnew->tick.y_r_minor_ndc_locs[j] = tnew->tick.y_r_minor_ndc_locs[i];
				j++;
			} else {
				if(i < tnew->tick.x_t_nminor) {
					tnew->tick.x_t_nminor--;
				}
				if(i < tnew->tick.y_r_nminor) {
					tnew->tick.y_r_nminor--;
				}
				tmpi--;	
			}
		}
	}
        status= 0;
        subret = _NhlWinToNDC((NhlLayer)tnew->tick.xt_yr_trans_obj,
                tnew->tick.x_t_minor_ndc_locs,tnew->tick.y_r_minor_ndc_locs,
                tmpi,
                tnew->tick.x_t_minor_ndc_locs,tnew->tick.y_r_minor_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occurred while mapping the top and right minor tick marks from the window to NDC, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
        if(status){
/*
* Contains and out of range value
*/
		NhlVAGetValues(tnew->tick.xt_yr_trans_obj->base.id,
			NhlNtrOutOfRangeF,&orv,
			NULL);
		j = 0;
		for(i = 0; i< tmpi; i++) {
/*
* both arrays contain orv values at same index
*/
			if(tnew->tick.x_t_minor_ndc_locs[i] != orv) {
				tnew->tick.x_t_minor_ndc_locs[j] = tnew->tick.x_t_minor_ndc_locs[i];
				tnew->tick.y_r_minor_ndc_locs[j] = tnew->tick.y_r_minor_ndc_locs[i];
				j++;
			} else {
				if(i < tnew->tick.x_t_nminor) {
					tnew->tick.x_t_nminor--;
				}
				if(i < tnew->tick.y_r_nminor) {
					tnew->tick.y_r_nminor--;
				}
				tmpi--;	
			}
		}
	}
	}
	return(ret);

}

/*
 * Function:	ComputeAndSetLabelInfo
 *
 * Description: Copies label pointers and ndc locations from instance into
 *		temporary array and then creates or sets the MultiText object
 *		Next the extents of the MultiText are checked to make sure
 *		that the tick labels do not overlap with the viewport. If
 *		they do then they are moved. The spacing between the viewport
 *		and labels is also computed in this function. There is a 
 *		DEFAULTOFFSET which is applied to all labels the default
 *		offset is mulitplied by the font height to get an abosolute
 *		offset. If a user supplied offset is given the the 
 *		DEFAULTOFFSET + *_*_label_delta is multiplied by the font
 *		height to get the offset.
 *
 * In Args:	tnew new instance record
 *		told old instance if c_or_s == SET
 *		c_or_s 	either SET or CREATE
 *	
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects:NONE
 */
/*ARGSUSED*/
static NhlErrorTypes ComputeAndSetLabelInfo
#if	NhlNeedProto
(NhlTickMarkLayer tnew,NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
NhlTickMarkLayer 	tnew;
NhlTickMarkLayer 	told;
int		c_or_s;
#endif
{
	int i,j,tmpid;
	int tmp;
        float delta;
        char *labels_for_multi[_NhlMAXFNAMELEN];
        float   locs_for_multi[_NhlMAXFNAMELEN];
        char buffer[_NhlMAXRESNAMLEN];
	float tmpy,tmpx,tmpwidth,tmpheight;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes subret = NhlNOERROR;
	char *error_lead;
	float label_spacing;


	if(c_or_s == SET )
		error_lead = "TickMarkSetValues";
	else 
		error_lead = "TickMarkInitialize";
		


/*
* Now make array of locations and strings and use it to create the
* MultiTextItems . Tick must be filtered out if stride > 1
*/
        if(tnew->tick.x_b_labels_on && tnew->tick.x_b_data_valid) {
                if(tnew->tick.x_b_label_stride > 1) {
                        j = 0;
                        for(i = 0; i< tnew->tick.x_b_nmajor; 
                                        i+=tnew->tick.x_b_label_stride) {
				if(tnew->tick.x_b_major_labels[i] != NULL){
                                	labels_for_multi[j] = tnew->tick.x_b_major_labels[i];
                                	locs_for_multi[j] = tnew->tick.x_b_major_ndc_locs[i];
                                	j++;
				}
                        }
                        tmp = j;
                } else {
			j = 0;
                        for(i = 0; i< tnew->tick.x_b_nmajor; i++) {
				if(tnew->tick.x_b_major_labels[i] != NULL){
                                	labels_for_multi[j] = tnew->tick.x_b_major_labels[i];
                                	locs_for_multi[j] = tnew->tick.x_b_major_ndc_locs[i];
					j++;
				}
                        }
                        tmp = j;
                }
/*
* Now compute initial label location. May need to change it if multi text
* overlaps.
*/
                tnew->tick.x_b_ndc_label_y = tnew->view.y - tnew->view.height 
                                - (DEFAULTOFFSET + tnew->tick.x_b_label_delta) 
				* tnew->tick.x_b_label_font_height;
		if(tnew->tick.x_b_on) {
			if(tnew->tick.x_b_major_outward_length > tnew->tick.x_b_minor_outward_length) {
				tnew->tick.x_b_ndc_label_y = tnew->tick.x_b_ndc_label_y - tnew->tick.x_b_major_outward_length;
			} else {
				tnew->tick.x_b_ndc_label_y = tnew->tick.x_b_ndc_label_y - tnew->tick.x_b_minor_outward_length;
			}
		}
		if((c_or_s == CREATE)||(tnew->tick.xb_multi ==NULL)) {
			strcpy(buffer,tnew->base.name);
			strcat(buffer,".xb_Multi");
			subret = NhlVACreate
				(&tmpid,buffer,
				 NhlmultiTextClass,tnew->base.id,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_Y_CONST,
                        NhlNMtextConstPosF,tnew->tick.x_b_ndc_label_y,
                        NhlNMtextPosArray,locs_for_multi,
			NhlNMtextAutoStride,tnew->tick.label_auto_stride,
                        NhlNtxAngleF,tnew->tick.x_b_label_angle,
                        NhlNtxFont,tnew->tick.x_b_label_font,
                        NhlNtxJust,tnew->tick.x_b_label_just,
                        NhlNtxFontHeightF,tnew->tick.x_b_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.x_b_label_font_aspect,
                        NhlNtxDirection,tnew->tick.x_b_label_direction,
                        NhlNtxFontColor,tnew->tick.x_b_label_font_color,
                        NhlNtxFuncCode,tnew->tick.x_b_label_fcode,
                        NhlNtxFontThicknessF,
					 tnew->tick.x_b_label_font_thickness,
                        NhlNtxFontQuality,tnew->tick.x_b_label_font_quality,
                        NhlNtxConstantSpacingF,
					tnew->tick.x_b_label_constant_spacing,
                        NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not create MultiText item for bottom tick mark labels, cannot continue",error_lead);
				return(NhlFATAL);
			} else if (subret < ret) {
				ret = subret;
			}
		} else {
			tmpid = tnew->tick.xb_multi->base.id;
			subret = NhlVASetValues(tmpid,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_Y_CONST,
                        NhlNMtextConstPosF,tnew->tick.x_b_ndc_label_y,
                        NhlNMtextPosArray,locs_for_multi,
			NhlNMtextAutoStride,tnew->tick.label_auto_stride,
                        NhlNtxAngleF,tnew->tick.x_b_label_angle,
                        NhlNtxFont,tnew->tick.x_b_label_font,
                        NhlNtxJust,tnew->tick.x_b_label_just,
                        NhlNtxFontHeightF,tnew->tick.x_b_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.x_b_label_font_aspect,
                        NhlNtxDirection,tnew->tick.x_b_label_direction,
                        NhlNtxFontColor,tnew->tick.x_b_label_font_color,
                        NhlNtxFuncCode,tnew->tick.x_b_label_fcode,
                        NhlNtxFontThicknessF,
					 tnew->tick.x_b_label_font_thickness,
                        NhlNtxFontQuality,tnew->tick.x_b_label_font_quality,
                        NhlNtxConstantSpacingF,
					tnew->tick.x_b_label_constant_spacing,
                        NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set MultiText item values for bottom tick mark labels, cannot continue",error_lead);
				return(NhlFATAL);
			} else if (subret < ret) {
				ret = subret;
			}
		}
                subret = NhlVAGetValues(tmpid,
					NhlNvpYF,&tmpy,
					NhlNMtextMaxLenF,
					&tnew->tick.x_b_max_label_len,
					NULL);
		if(subret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not retrieve key value from internal object, cannot continue",error_lead);
			return(NhlFATAL);
		} else if(subret < ret) {
			ret = subret;
		}
                if(tmpy > tnew->view.y- tnew->view.height){
                        delta = tmpy - (tnew->view.y - tnew->view.height) + 
                                ((DEFAULTOFFSET + tnew->tick.x_b_label_delta)*                                 tnew->tick.x_b_label_font_height);
                        tnew->tick.x_b_ndc_label_y -= delta; 
                        subret = NhlVASetValues(tmpid,NhlNMtextConstPosF,
                                        tnew->tick.x_b_ndc_label_y,NULL);
			if(subret < NhlWARNING){
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set key value in internal object, cannot continue",error_lead);
				return(NhlFATAL);
			} else if( subret < ret) {
				ret = subret;
			}
                }
                tnew->tick.xb_multi = _NhlGetLayer(tmpid);

		if (tnew->tick.x_b_nmajor == 1) {
			label_spacing = tnew->view.width;
		}
		else if (tnew->tick.x_b_mode == NhlEXPLICIT) {
			label_spacing = tnew->view.width;
			for (i = 1; i < tmp; i++) {
				label_spacing = 
					MIN(label_spacing, locs_for_multi[i] -
					    locs_for_multi[i-1]);
			}
		}
		else { /* all spaces are equal */
			label_spacing = MAX(1,tnew->tick.x_b_label_stride) *
				(tnew->tick.x_b_major_ndc_locs[1] -
				tnew->tick.x_b_major_ndc_locs[0]);
		}
		tnew->tick.x_b_label_spacing = label_spacing;
        } else {
/*
* Won't create one until its needed. However I don't ever plan on destroying
* multitext items when they are not turned on
*/
		if(c_or_s == CREATE)
			tnew->tick.xb_multi = NULL;
        }


/*
* Now make array of locations and strings and use it to create the
* MultiTextItems . Tick must be filtered out if stride > 1
*/
        if(tnew->tick.x_t_labels_on && tnew->tick.x_t_data_valid) {
                if(tnew->tick.x_t_label_stride > 1) {
                        j = 0;
                        for(i = 0; i< tnew->tick.x_t_nmajor; 
                                        i+=tnew->tick.x_t_label_stride) {
				if(tnew->tick.x_t_major_labels[i] != NULL) {
                                	labels_for_multi[j] = tnew->tick.x_t_major_labels[i];
                                	locs_for_multi[j] = tnew->tick.x_t_major_ndc_locs[i];
                                	j++;
				}
                        }
                        tmp = j;
                } else {
			j = 0;
                        for(i = 0; i< tnew->tick.x_t_nmajor; i++) {
				if(tnew->tick.x_t_major_labels[i] != NULL) {
                                	labels_for_multi[j] = tnew->tick.x_t_major_labels[i];
                                	locs_for_multi[j] = tnew->tick.x_t_major_ndc_locs[i];
					j++;
				}
                        }
                        tmp = j;
                }
/*
* Now compute label location
*/
                tnew->tick.x_t_ndc_label_y = 
                        tnew->view.y+(DEFAULTOFFSET+tnew->tick.x_t_label_delta)
					*tnew->tick.x_t_label_font_height;

		if(tnew->tick.x_t_on) {
			if(tnew->tick.x_t_major_outward_length > tnew->tick.x_t_minor_outward_length) {
				tnew->tick.x_t_ndc_label_y = tnew->tick.x_t_ndc_label_y + tnew->tick.x_t_major_outward_length;
			} else {
				tnew->tick.x_t_ndc_label_y = tnew->tick.x_t_ndc_label_y + tnew->tick.x_t_minor_outward_length;
			}
		}
		if((c_or_s == CREATE)||(tnew->tick.xt_multi == NULL)) {
			strcpy(buffer,tnew->base.name);
			strcat(buffer,".xt_Multi");
			subret = NhlVACreate(&tmpid,buffer,NhlmultiTextClass,tnew->base.id,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_Y_CONST,
                        NhlNMtextConstPosF,tnew->tick.x_t_ndc_label_y,
                        NhlNMtextPosArray,locs_for_multi,
			NhlNMtextAutoStride,tnew->tick.label_auto_stride,
                        NhlNtxAngleF,tnew->tick.x_t_label_angle,
                        NhlNtxFont,tnew->tick.x_t_label_font,
                        NhlNtxJust,tnew->tick.x_t_label_just,
                        NhlNtxFontHeightF,tnew->tick.x_t_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.x_t_label_font_aspect,
                        NhlNtxDirection,tnew->tick.x_t_label_direction,
                        NhlNtxFontColor,tnew->tick.x_t_label_font_color,
                        NhlNtxFuncCode,tnew->tick.x_t_label_fcode,
                        NhlNtxFontThicknessF,
					 tnew->tick.x_t_label_font_thickness,
                        NhlNtxFontQuality,tnew->tick.x_t_label_font_quality,
                        NhlNtxConstantSpacingF,
					tnew->tick.x_t_label_constant_spacing,
                        NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not create MultiText item for top tick mark labels, cannot continue",error_lead);
				return(NhlFATAL);
			} else if (subret < ret) {
				ret = subret;
			}
		} else {
			tmpid = tnew->tick.xt_multi->base.id;
			subret = NhlVASetValues(tmpid,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_Y_CONST,
                        NhlNMtextConstPosF,tnew->tick.x_t_ndc_label_y,
                        NhlNMtextPosArray,locs_for_multi,
			NhlNMtextAutoStride,tnew->tick.label_auto_stride,
                        NhlNtxAngleF,tnew->tick.x_t_label_angle,
                        NhlNtxFont,tnew->tick.x_t_label_font,
                        NhlNtxJust,tnew->tick.x_t_label_just,
                        NhlNtxFontHeightF,tnew->tick.x_t_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.x_t_label_font_aspect,
                        NhlNtxDirection,tnew->tick.x_t_label_direction,
                        NhlNtxFontColor,tnew->tick.x_t_label_font_color,
                        NhlNtxFuncCode,tnew->tick.x_t_label_fcode,
                        NhlNtxFontThicknessF,
					 tnew->tick.x_t_label_font_thickness,
                        NhlNtxFontQuality,tnew->tick.x_t_label_font_quality,
                        NhlNtxConstantSpacingF,
					tnew->tick.x_t_label_constant_spacing,
                        NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set MultiText item values for top tick mark labels, cannot continue",error_lead);
				return(NhlFATAL);
			} else if (subret < ret) {
				ret = subret;
			}
		}
                subret = NhlVAGetValues(tmpid,
					NhlNvpYF,&tmpy,
					NhlNvpHeightF,&tmpheight,
					NhlNMtextMaxLenF,
					&tnew->tick.x_t_max_label_len,
					NULL);
		if(subret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not retrieve key value from internal object, cannot continue",error_lead);
			return(NhlFATAL);
		}
                if(tmpy - tmpheight < tnew->view.y) {
                        delta = tnew->view.y - (tmpy - tmpheight) + (DEFAULTOFFSET + tnew->tick.x_t_label_delta)*tnew->tick.x_t_label_font_height;
                        tnew->tick.x_t_ndc_label_y += delta;
                        subret = NhlVASetValues(tmpid,NhlNMtextConstPosF,
                                        tnew->tick.x_t_ndc_label_y,NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set key value in internal object, cannot continue",error_lead);
				return(NhlFATAL);
			} else if(subret < ret){
				ret = subret;
			}
                }
                tnew->tick.xt_multi = _NhlGetLayer(tmpid);

		if (tnew->tick.x_t_nmajor == 1) {
			label_spacing = tnew->view.width;
		}
		else if (tnew->tick.x_t_mode == NhlEXPLICIT) {
			label_spacing = tnew->view.width;
			for (i = 1; i < tmp; i++) {
				label_spacing = 
					MIN(label_spacing, locs_for_multi[i] -
					    locs_for_multi[i-1]);
			}
		}
		else { /* all spaces are equal */
			label_spacing = MAX(1,tnew->tick.x_t_label_stride) *
				(tnew->tick.x_t_major_ndc_locs[1] -
				tnew->tick.x_t_major_ndc_locs[0]);
		}
			
		tnew->tick.x_t_label_spacing = label_spacing;
        } else {
/*
* Won't create one until its needed. However I don't ever plan on destroying
* multitext items when they are not turned on
*/
		if(c_or_s == CREATE)
                	tnew->tick.xt_multi = NULL;
        }
/*
* Now make array of locations and strings and use it to create the
* MultiTextItems . Tick must be filtered out if stride > 1
*/
        if(tnew->tick.y_l_labels_on && tnew->tick.y_l_data_valid) {
                if(tnew->tick.y_l_label_stride > 1) {
                        j = 0;
                        for(i = 0; i< tnew->tick.y_l_nmajor; 
                                        i+=tnew->tick.y_l_label_stride) {
				if(tnew->tick.y_l_major_labels[i] != NULL) {
                                	labels_for_multi[j] = tnew->tick.y_l_major_labels[i];
                                	locs_for_multi[j] = tnew->tick.y_l_major_ndc_locs[i];
                                	j++;
				}
                        }
                        tmp = j;
                } else { 
			j = 0;
                        for(i = 0; i< tnew->tick.y_l_nmajor; i++) {
				if(tnew->tick.y_l_major_labels[i] != NULL) {
                                	labels_for_multi[j] = tnew->tick.y_l_major_labels[i];
                                	locs_for_multi[j] = tnew->tick.y_l_major_ndc_locs[i];
					j++;
				}
                        }
                        tmp = j; 
                }
/*
* Now compute label location
*/
                tnew->tick.y_l_ndc_label_x = tnew->view.x - (DEFAULTOFFSET +tnew->tick.y_l_label_delta) * tnew->tick.y_l_label_font_height;
	
		if(tnew->tick.y_l_on) {
			if(tnew->tick.y_l_major_outward_length > tnew->tick.y_l_minor_outward_length) {
				tnew->tick.y_l_ndc_label_x = tnew->tick.y_l_ndc_label_x - tnew->tick.y_l_major_outward_length;
			} else {
				tnew->tick.y_l_ndc_label_x = tnew->tick.y_l_ndc_label_x - tnew->tick.y_l_minor_outward_length;
			}
		}
	
		if((c_or_s == CREATE)||(tnew->tick.yl_multi == NULL)) {
			strcpy(buffer,tnew->base.name);
			strcat(buffer,".yl_Multi");
			subret = NhlVACreate(&tmpid,buffer,NhlmultiTextClass,tnew->base.id,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_X_CONST,
                        NhlNMtextConstPosF,tnew->tick.y_l_ndc_label_x,
                        NhlNMtextPosArray,locs_for_multi,
			NhlNMtextAutoStride,tnew->tick.label_auto_stride,
                        NhlNtxAngleF,tnew->tick.y_l_label_angle,
                        NhlNtxFont,tnew->tick.y_l_label_font,
                        NhlNtxJust,tnew->tick.y_l_label_just,
                        NhlNtxFontHeightF,tnew->tick.y_l_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.y_l_label_font_aspect,
                        NhlNtxDirection,tnew->tick.y_l_label_direction,
                        NhlNtxFontColor,tnew->tick.y_l_label_font_color,
                        NhlNtxFuncCode,tnew->tick.y_l_label_fcode,
                        NhlNtxFontThicknessF,
					 tnew->tick.y_l_label_font_thickness,
                        NhlNtxFontQuality,tnew->tick.y_l_label_font_quality,
                        NhlNtxConstantSpacingF,
					tnew->tick.y_l_label_constant_spacing,
                        NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not create MultiText item for left tick mark labels, cannot continue",error_lead);
				return(NhlFATAL);
			} else if (subret < ret) {
				ret = subret;
			}
		} else {
			tmpid = tnew->tick.yl_multi->base.id;
			subret = NhlVASetValues(tmpid,
			NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_X_CONST,
                        NhlNMtextConstPosF,tnew->tick.y_l_ndc_label_x,
                        NhlNMtextPosArray,locs_for_multi,
			NhlNMtextAutoStride,tnew->tick.label_auto_stride,
                        NhlNtxAngleF,tnew->tick.y_l_label_angle,
                        NhlNtxFont,tnew->tick.y_l_label_font,
                        NhlNtxJust,tnew->tick.y_l_label_just,
                        NhlNtxFontHeightF,tnew->tick.y_l_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.y_l_label_font_aspect,
                        NhlNtxDirection,tnew->tick.y_l_label_direction,
                        NhlNtxFontColor,tnew->tick.y_l_label_font_color,
                        NhlNtxFuncCode,tnew->tick.y_l_label_fcode,
                        NhlNtxFontThicknessF,
					 tnew->tick.y_l_label_font_thickness,
                        NhlNtxFontQuality,tnew->tick.y_l_label_font_quality,
                        NhlNtxConstantSpacingF,
					tnew->tick.y_l_label_constant_spacing,
                        NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set MultiText item values for left tick mark labels, cannot continue",error_lead);
				return(NhlFATAL);
			} else if (subret < ret) {
				ret = subret;
			}
		}
                subret = NhlVAGetValues(tmpid,
					NhlNvpXF,&tmpx,
					NhlNvpWidthF,&tmpwidth,
					NhlNMtextMaxLenF,
					&tnew->tick.y_l_max_label_len,
					NULL);
		if(subret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not retrieve key value from internal object, cannot continue",error_lead);
			return(NhlFATAL);
		} else if (subret < ret ){
			ret = subret;
		}
                if(tmpx + tmpwidth > tnew->view.x) {
                        delta = tmpx+tmpwidth - tnew->view.x + (DEFAULTOFFSET + tnew->tick.y_l_label_delta)*tnew->tick.y_l_label_font_height;
                        tnew->tick.y_l_ndc_label_x -= delta;
                        subret = NhlVASetValues(tmpid,NhlNMtextConstPosF,
                                        tnew->tick.y_l_ndc_label_x,NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set key value in internal object, cannot continue",error_lead);
				return(NhlFATAL);
			} else if(subret < ret){
				ret = subret;
			}
                }
                tnew->tick.yl_multi = _NhlGetLayer(tmpid);

		if (tnew->tick.y_l_nmajor == 1) {
			label_spacing = tnew->view.width;
		}
		else if (tnew->tick.y_l_mode == NhlEXPLICIT) {
			label_spacing = tnew->view.width;
			for (i = 1; i < tmp; i++) {
				label_spacing = 
					MIN(label_spacing, locs_for_multi[i] -
					    locs_for_multi[i-1]);
			}
		}
		else { /* all spaces are equal */
			label_spacing = MAX(1,tnew->tick.y_l_label_stride) *
				(tnew->tick.y_l_major_ndc_locs[1] -
				tnew->tick.y_l_major_ndc_locs[0]);
		}
		tnew->tick.y_l_label_spacing = label_spacing;
        } else {
/*
* Won't create one until its needed. However I don't ever plan on destroying
* multitext items when they are not turned on
*/
		if(c_or_s == CREATE)
                tnew->tick.yl_multi = NULL;
	}
/*
* Now make array of locations and strings and use it to create the
* MultiTextItems . Tick must be filtered out if stride > 1
*/
        if(tnew->tick.y_r_labels_on && tnew->tick.y_r_data_valid) {
                if(tnew->tick.y_r_label_stride > 1) {
                        j = 0;
                        for(i = 0; i< tnew->tick.y_r_nmajor; 
                                        i+=tnew->tick.y_r_label_stride) {
				if(tnew->tick.y_r_major_labels[i] != NULL) {
                                	labels_for_multi[j] = tnew->tick.y_r_major_labels[i];
                                	locs_for_multi[j] = tnew->tick.y_r_major_ndc_locs[i];
                                	j++;
				}
                        }
                        tmp = j;
                } else {
			j = 0;
                        for(i = 0; i< tnew->tick.y_r_nmajor; i++) { 	
				if(tnew->tick.y_r_major_labels[i] != NULL ){
                                	labels_for_multi[j] = tnew->tick.y_r_major_labels[i];
                                	locs_for_multi[j] = tnew->tick.y_r_major_ndc_locs[i];
					j++;
				}
                        }
                        tmp = j;
                }
/*
* Now compute label location
*/
                tnew->tick.y_r_ndc_label_x = tnew->view.x+tnew->view.width +
                (tnew->tick.y_r_label_delta+ DEFAULTOFFSET)*tnew->tick.y_r_label_font_height;
		if(tnew->tick.y_r_on) {
			if(tnew->tick.y_r_major_outward_length > tnew->tick.y_r_minor_outward_length) {
				tnew->tick.y_r_ndc_label_x = tnew->tick.y_r_ndc_label_x + tnew->tick.y_r_major_outward_length;
			} else {
				tnew->tick.y_r_ndc_label_x = tnew->tick.y_r_ndc_label_x + tnew->tick.y_r_minor_outward_length;
			}
		}
		if((c_or_s == CREATE)||(tnew->tick.yr_multi == NULL)) {
			strcpy(buffer,tnew->base.name);
			strcat(buffer,".yr_Multi");
			subret = NhlVACreate(&tmpid,buffer,NhlmultiTextClass,tnew->base.id,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_X_CONST,
                        NhlNMtextConstPosF,tnew->tick.y_r_ndc_label_x,
                        NhlNMtextPosArray,locs_for_multi,
			NhlNMtextAutoStride,tnew->tick.label_auto_stride,
                        NhlNtxAngleF,tnew->tick.y_r_label_angle,
                        NhlNtxFont,tnew->tick.y_r_label_font,
                        NhlNtxJust,tnew->tick.y_r_label_just,
                        NhlNtxFontHeightF,tnew->tick.y_r_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.y_r_label_font_aspect,
                        NhlNtxDirection,tnew->tick.y_r_label_direction,
                        NhlNtxFontColor,tnew->tick.y_r_label_font_color,
                        NhlNtxFuncCode,tnew->tick.y_r_label_fcode,
                        NhlNtxFontThicknessF,
					 tnew->tick.y_r_label_font_thickness,
                        NhlNtxFontQuality,tnew->tick.y_r_label_font_quality,
                        NhlNtxConstantSpacingF,
					tnew->tick.y_r_label_constant_spacing,
                        NULL);
		if(subret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not create MultiText item for right tick mark labels, cannot continue",error_lead);
			return(NhlFATAL);
		} else if (subret < ret) {
			ret = subret;
		}
	} else {
		tmpid = tnew->tick.yr_multi->base.id;
		subret = NhlVASetValues(tmpid,
			NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_X_CONST,
                        NhlNMtextConstPosF,tnew->tick.y_r_ndc_label_x,
                        NhlNMtextPosArray,locs_for_multi,
			NhlNMtextAutoStride,tnew->tick.label_auto_stride,
                        NhlNtxAngleF,tnew->tick.y_r_label_angle,
                        NhlNtxFont,tnew->tick.y_r_label_font,
                        NhlNtxJust,tnew->tick.y_r_label_just,
                        NhlNtxFontHeightF,tnew->tick.y_r_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.y_r_label_font_aspect,
                        NhlNtxDirection,tnew->tick.y_r_label_direction,
                        NhlNtxFontColor,tnew->tick.y_r_label_font_color,
                        NhlNtxFuncCode,tnew->tick.y_r_label_fcode,
                        NhlNtxFontThicknessF,
					 tnew->tick.y_r_label_font_thickness,
                        NhlNtxFontQuality,tnew->tick.y_r_label_font_quality,
                        NhlNtxConstantSpacingF,
					tnew->tick.y_r_label_constant_spacing,
                        NULL);
		if(subret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set MultiText item values for right tick mark labels, cannot continue",error_lead);
			return(NhlFATAL);
		} else if (subret < ret) {
			ret = subret;
		}
	}
                subret = NhlVAGetValues(tmpid,
					NhlNvpXF,&tmpx,
					NhlNMtextMaxLenF,
					&tnew->tick.y_r_max_label_len,
					NULL);
		if(subret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not retrieve key value from internal object, cannot continue",error_lead);
			return(NhlFATAL);
		} else if (subret < ret ){
			ret = subret;
		}
                if(tmpx < tnew->view.x + tnew->view.width) {
                        delta = tnew->view.x + tnew->view.width - tmpx + (DEFAULTOFFSET + tnew->tick.y_r_label_delta) * tnew->tick.y_r_label_font_height;
                        tnew->tick.y_r_ndc_label_x += delta;
                        subret = NhlVASetValues(tmpid,NhlNMtextConstPosF,tnew->tick.y_r_ndc_label_x,NULL);
			if(subret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set key value in internal object, cannot continue",error_lead);
				return(NhlFATAL);
			} else if(subret < ret){
				ret = subret;
			}
                }
                tnew->tick.yr_multi = _NhlGetLayer(tmpid);

		if (tnew->tick.y_r_nmajor == 1) {
			label_spacing = tnew->view.width;
		}
		else if (tnew->tick.y_r_mode == NhlEXPLICIT) {
			label_spacing = tnew->view.width;
			for (i = 1; i < tmp; i++) {
				label_spacing = 
					MIN(label_spacing, locs_for_multi[i] -
					    locs_for_multi[i-1]);
			}
		}
		else { /* all spaces are equal */
			label_spacing = MAX(1,tnew->tick.y_r_label_stride) *
				(tnew->tick.y_r_major_ndc_locs[1] -
				tnew->tick.y_r_major_ndc_locs[0]);
		}
		tnew->tick.y_r_label_spacing = label_spacing;
        } else {
/*
* Won't create one until its needed. However I don't ever plan on destroying
* multitext items when they are not turned on
*/
		if(c_or_s == CREATE)
                tnew->tick.yr_multi = NULL;
        }
	return(ret);
}

/*
 * Function:	CreateXBYLTransformInfo
 *
 * Description: Takes care of creating and setting the transformation objects
 *		used by the tick mark object. The Bottom and Left axis share
 *		one transformation and the Top and Right axis share one.
 *		The real trick here was how should NhlLOG/NhlIRREGULAR and 
 *		NhlLINEAR/NhlIRREGULAR transformations be handled. When an irregular
 *		style is requested for an axis but the axis that shares the
 *		transformation is not irregular and irregular transformation
 *		object is created anyways. When the other transformation is
 *		NhlLINEAR a set of three control points are used, the start,
 *		end and a point half way between. For NhlLOGs it is a little
 *		more difficult. An extension to the SplineCoord approx code
 *		was needed as well as the IrregularTransObj. This new 
 *	 	option informs the spline code that the log10 should be taken
 *		on all the input values before createing the approximation.
 *		three control points are used, start, end and 
 * 		pow(10.0,(log10(start)+log10(end))/2). This fakes the 
 *		Irregular object into a log plot.
 *
 * In Args:	tnew	new instance record
 *		args    current argslist passed to initialize from Create
 *		num_args number of these
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: Sets ir_**min and ir_**max which are used by the ChangeTrans
 *		function to determine whether new mins and maxs exceed the
 *		minimum and maximum range of the coordinate points arrays of
 *		the IrregularTransObj.
 */
/*ARGSUSED*/
static NhlErrorTypes CreateXBYLTransformInfo
#if	NhlNeedProto
(NhlTickMarkLayer tnew, _NhlArgList args, int num_args)
#else
(tnew,  args, num_args)
NhlTickMarkLayer tnew;
_NhlArgList args;
int num_args;
#endif
{
	NhlSArg	sargs[20];
	int nargs = 0;
	NhlClass trans_class = NULL;
	char buffer[_NhlMAXRESNAMLEN];
	int tmpid;
	NhlErrorTypes ret = NhlNOERROR;

	
							  
	if((tnew->tick.y_l_style == NhlIRREGULAR)||
	   (tnew->tick.x_b_style == NhlIRREGULAR)) {
		switch(tnew->tick.y_l_style) {
		case NhlIRREGULAR:
/*
* THis is needed because new has already been reallocated and could possibly
* be the same as told. Also old is already freed!
*/
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
				   tnew->tick.y_l_irregular_points);
			NhlSetSArg(&sargs[nargs++],NhlNtrYAxisType,
				   NhlIRREGULARAXIS);
			break;
		case NhlLOG:
			NhlSetSArg(&sargs[nargs++],NhlNtrYAxisType,
				   NhlLOGAXIS);
			tnew->tick.ir_ylmin = tnew->tick.y_l_data_min;
			tnew->tick.ir_ylmax = tnew->tick.y_l_data_max;
			break;
		case NhlLINEAR:
			NhlSetSArg(&sargs[nargs++],NhlNtrYAxisType,
				   NhlLINEARAXIS);
			tnew->tick.ir_ylmin = tnew->tick.y_l_data_min;
			tnew->tick.ir_ylmax = tnew->tick.y_l_data_max;
			break;
		case NhlTIME:
		case NhlGEOGRAPHIC:
			break;
		}
		switch(tnew->tick.x_b_style) {
		case NhlIRREGULAR:
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXCoordPoints,
				   tnew->tick.x_b_irregular_points);
			NhlSetSArg(&sargs[nargs++],NhlNtrXAxisType,
				   NhlIRREGULARAXIS);
			break;
		case NhlLOG:
			tnew->tick.ir_xbmin = tnew->tick.x_b_data_min;
			tnew->tick.ir_xbmax = tnew->tick.x_b_data_max;
			NhlSetSArg(&sargs[nargs++],NhlNtrXAxisType,
				   NhlLOGAXIS);
			break;
		case NhlLINEAR:
			tnew->tick.ir_xbmin = tnew->tick.x_b_data_min;
			tnew->tick.ir_xbmax = tnew->tick.x_b_data_max;

			NhlSetSArg(&sargs[nargs++],NhlNtrXAxisType,
				   NhlLINEARAXIS);
			break;
		case NhlTIME:
		case NhlGEOGRAPHIC:
			break;
		}
		trans_class = NhlirregularTransObjClass;
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,
			   (tnew->tick.x_b_data_left>tnew->tick.x_b_data_right ? 1 : 0));
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,
			   (tnew->tick.y_l_data_bottom >tnew->tick.y_l_data_top? 1 : 0));
		NhlSetSArg(&sargs[nargs++],
			   NhlNtrXMinF,tnew->tick.x_b_data_min);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtrXMaxF,tnew->tick.x_b_data_max);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtrYMinF,tnew->tick.y_l_data_min);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtrYMaxF,tnew->tick.y_l_data_max);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtrYTensionF,tnew->tick.y_l_tension);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtrXTensionF,tnew->tick.x_b_tension);
	} else {
		switch(tnew->tick.y_l_style) {
		case NhlLOG:
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,1);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_l_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_l_data_max);
			break;
		case NhlLINEAR:
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,0);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_l_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_l_data_max);
			break;
		case NhlTIME:
		case NhlGEOGRAPHIC:
		default:
			break;
		}
		switch(tnew->tick.x_b_style) {
		case NhlLOG:
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,1);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_b_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_b_data_max);
			break;
		case NhlLINEAR:
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,0);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_b_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_b_data_max);
			break;
		case NhlTIME:
		case NhlGEOGRAPHIC:
		default:
			break;
		}
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_b_data_left>tnew->tick.x_b_data_right ? 1 : 0));
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_l_data_bottom >tnew->tick.y_l_data_top? 1 : 0));
		trans_class = NhllogLinTransObjClass;
	}
		
	strcpy(buffer,tnew->base.name);
	strcat(buffer,".Trans");
	NhlALCreate(&tmpid,buffer,trans_class,tnew->base.id,sargs,nargs);
	tnew->tick.xb_yl_trans_obj = _NhlGetLayer(tmpid);

	nargs = 0;
	return(ret);
}

/*
 * Function:	CreateXTYRTransformInfo
 *
 * Description:	Basically same as CreateXBYLTransformInfo
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes CreateXTYRTransformInfo
#if	NhlNeedProto
(NhlTickMarkLayer tnew, _NhlArgList args, int num_args)
#else
(tnew,  args, num_args)
NhlTickMarkLayer tnew;
_NhlArgList args;
int num_args;
#endif
{	
	NhlSArg	sargs[20];
	int nargs = 0;
	NhlClass trans_class = NULL;
	char buffer[_NhlMAXRESNAMLEN];
	int tmpid;
	NhlErrorTypes ret = NhlNOERROR;

	if((tnew->tick.y_r_style == NhlIRREGULAR)||
	   (tnew->tick.x_t_style == NhlIRREGULAR)) {
		switch(tnew->tick.y_r_style) {
		case NhlIRREGULAR:
			NhlSetSArg(&sargs[nargs++], NhlNtrYCoordPoints,
				   tnew->tick.y_r_irregular_points);
			NhlSetSArg(&sargs[nargs++],NhlNtrYAxisType,
				   NhlIRREGULARAXIS);
			break;
		case NhlLOG:
			tnew->tick.ir_yrmin = tnew->tick.y_r_data_min;
			tnew->tick.ir_yrmax = tnew->tick.y_r_data_max;
			NhlSetSArg(&sargs[nargs++],NhlNtrYAxisType,
				   NhlLOGAXIS);
			break;
		case NhlLINEAR:
			tnew->tick.ir_yrmin = tnew->tick.y_r_data_min;
			tnew->tick.ir_yrmax = tnew->tick.y_r_data_max;
			NhlSetSArg(&sargs[nargs++],NhlNtrYAxisType,
				   NhlLINEARAXIS);
			break;
		case NhlTIME:
		case NhlGEOGRAPHIC:
			break;
		}
		switch(tnew->tick.x_t_style) {
		case NhlIRREGULAR:
			NhlSetSArg(&sargs[nargs++], NhlNtrXCoordPoints,
				   tnew->tick.x_t_irregular_points);
			NhlSetSArg(&sargs[nargs++],NhlNtrXAxisType,
				   NhlIRREGULARAXIS);
			break;
		case NhlLOG:
			tnew->tick.ir_xtmin = tnew->tick.x_t_data_min;
			tnew->tick.ir_xtmax = tnew->tick.x_t_data_max;
			NhlSetSArg(&sargs[nargs++],NhlNtrXAxisType,
				   NhlLOGAXIS);
			break;
		case NhlLINEAR:
			tnew->tick.ir_xtmin = tnew->tick.x_t_data_min;
			tnew->tick.ir_xtmax = tnew->tick.x_t_data_max;

			NhlSetSArg(&sargs[nargs++],NhlNtrXAxisType,
				   NhlLINEARAXIS);
			break;
		case NhlTIME:
		case NhlGEOGRAPHIC:
			break;
		}
		trans_class = NhlirregularTransObjClass;
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_t_data_left>tnew->tick.x_t_data_right ? 1 : 0));
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_r_data_bottom >tnew->tick.y_r_data_top? 1 : 0));
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_t_data_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_t_data_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_r_data_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_r_data_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,tnew->tick.y_r_tension);
		NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,tnew->tick.x_t_tension);
	} else {
		switch(tnew->tick.y_r_style) {
		case NhlLOG:
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,1);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_r_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_r_data_max);
			break;
		case NhlLINEAR:
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,0);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_r_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_r_data_max);
			break;
		case NhlTIME:
		case NhlGEOGRAPHIC:
		default:
			break;
		}
		switch(tnew->tick.x_t_style) {
		case NhlLOG:
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,1);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_t_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_t_data_max);
			break;
		case NhlLINEAR:
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,0);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_t_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_t_data_max);
			break;
		case NhlTIME:
		case NhlGEOGRAPHIC:
		default:
			break;
		}
		trans_class = NhllogLinTransObjClass;
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_t_data_left>tnew->tick.x_t_data_right ? 1 : 0));
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_r_data_bottom >tnew->tick.y_r_data_top? 1 : 0));
	}
	
	strcpy(buffer,tnew->base.name);
	strcat(buffer,".Trans");
	ret = NhlALCreate(&tmpid,buffer,trans_class,tnew->base.id,sargs,nargs);
	tnew->tick.xt_yr_trans_obj = _NhlGetLayer(tmpid);
	return(ret);
}

/*
 * Function:	ScaleValuesForMove
 *
 * Description: Used to scale size related values when a resize has ocured.
 * 	Only values that have not changed and are related to size are scaled
 * 	Resource values that have been set on this call to TickMarkSetValues are
 * 	not scaled. It is assumed that if the user has requested a change in a
 * 	resource value that that request should be granted and not scaled . 
 *
 * In Args:	tnew 	new instance record
 *		told	old instance record
 *		c_or_s  either SET or CREATE
 *
 * Out Args: 	NONE	
 *
 * Return Values: Error Conditions
 *
 * Side Effects:	 NONE
 */
/*ARGSUSED*/
static NhlErrorTypes ScaleValuesForMove
#if	NhlNeedProto
(NhlTickMarkLayer tnew, NhlTickMarkLayer told,_NhlArgList args, int num_args, int c_or_s)
#else
(tnew,told,args,num_args,c_or_s)
	NhlTickMarkLayer	tnew;
	NhlTickMarkLayer	told;
	_NhlArgList args;
	int num_args;
	int c_or_s;
#endif
{

	NhlTickMarkLayerPart *tmp = &tnew->tick;
	float deltax,deltay;
	float h;
	NhlBoolean xbylset,xtyrset;

	if(c_or_s == CREATE) {
		deltax = tnew->view.width/NHL_DEFAULT_VIEW_WIDTH;
		deltay = tnew->view.height/NHL_DEFAULT_VIEW_HEIGHT;
	} else {
		deltax = tnew->view.width/told->view.width;
		deltay = tnew->view.height/told->view.height;
	}
/*
 * X fonts and Y ticks change relative to the X-Axis
 */
	if (_NhlCmpFAny2(deltax,1.0,4,_NhlMIN_NONZERO) != 0.0) {
		if (! tmp->x_b_label_font_height_set)
			tmp->x_b_label_font_height *= deltax;
		if (! tmp->x_t_label_font_height_set)
			tmp->x_t_label_font_height *= deltax;
		if (! tmp->y_l_major_length_set)
			tmp->y_l_major_length *= deltax;
		if (! tmp->y_l_major_outward_length_set)
			tmp->y_l_major_outward_length *= deltax;
		if (! tmp->y_l_minor_length_set)
			tmp->y_l_minor_length *= deltax;
		if (! tmp->y_l_minor_outward_length_set)
			tmp->y_l_minor_outward_length *= deltax;
		if (! tmp->y_r_major_length_set)
			tmp->y_r_major_length *= deltax;
		if (! tmp->y_r_major_outward_length_set)
			tmp->y_r_major_outward_length *= deltax;
		if (! tmp->y_r_minor_length_set)
			tmp->y_r_minor_length *= deltax;
		if (! tmp->y_r_minor_outward_length_set)
			tmp->y_r_minor_outward_length *= deltax;
	}

/*
 * Y fonts and X ticks change relative to the Y-Axis
 */
	if (_NhlCmpFAny2(deltay,1.0,4,_NhlMIN_NONZERO) != 0.0) {
		if (! tmp->y_l_label_font_height_set)
			tmp->y_l_label_font_height *= deltay;
		if (! tmp->y_r_label_font_height_set)
			tmp->y_r_label_font_height *= deltay;
		if (! tmp->x_b_major_length_set)
			tmp->x_b_major_length *= deltay;
		if (! tmp->x_b_major_outward_length_set)
			tmp->x_b_major_outward_length *= deltay;
		if (! tmp->x_b_minor_length_set)
			tmp->x_b_minor_length *= deltay;
		if (! tmp->x_b_minor_outward_length_set)
			tmp->x_b_minor_outward_length *= deltay;
		if (! tmp->x_t_major_length_set)
			tmp->x_t_major_length *= deltay;
		if (! tmp->x_t_major_outward_length_set)
			tmp->x_t_major_outward_length *= deltay;
		if (! tmp->x_t_minor_length_set)
			tmp->x_t_minor_length *= deltay;
		if (! tmp->x_t_minor_outward_length_set)
			tmp->x_t_minor_outward_length *= deltay;
	}

	if (! tmp->equalize_xy_sizes)
		return(NhlNOERROR);

	xbylset = xtyrset = False;
	if (tmp->x_b_label_font_height_set && 
	    ! tmp->y_l_label_font_height_set) {
		tmp->y_l_label_font_height = tmp->x_b_label_font_height;
		xbylset = True;
	}
	else if (tmp->y_l_label_font_height_set &&
		 ! tmp->x_b_label_font_height_set) {
		tmp->x_b_label_font_height = tmp->y_l_label_font_height;
		xbylset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_b_label_font_height + 
		     tmp->y_l_label_font_height) / 2.0;
		tmp->x_b_label_font_height = h;
		tmp->y_l_label_font_height = h;
		if (tmp->y_l_label_font_height_set)
			xbylset = True;
	}
	if (tmp->x_t_label_font_height_set && 
	    ! tmp->y_r_label_font_height_set) {
		tmp->y_r_label_font_height = tmp->x_t_label_font_height;
		xtyrset = True;
	}
	else if (tmp->y_r_label_font_height_set &&
		 ! tmp->x_t_label_font_height_set) {
		tmp->x_t_label_font_height = tmp->y_r_label_font_height;
		xtyrset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_t_label_font_height + 
		     tmp->y_r_label_font_height) / 2.0;
		tmp->x_t_label_font_height = h;
		tmp->y_r_label_font_height = h;
		if (tmp->y_r_label_font_height_set)
			xtyrset = True;
	}
	if (xbylset && ! xtyrset) {
		tmp->x_t_label_font_height = tmp->x_b_label_font_height;
		tmp->y_r_label_font_height = tmp->x_b_label_font_height;
	}
	else if (xtyrset && ! xbylset) {
		tmp->x_b_label_font_height = tmp->x_t_label_font_height;
		tmp->y_l_label_font_height = tmp->x_t_label_font_height;
	}
	else { 
		h = (tmp->x_b_label_font_height + 
		     tmp->x_t_label_font_height) / 2.0;
		tmp->x_b_label_font_height = h;
		tmp->x_t_label_font_height = h;
		tmp->y_l_label_font_height = h;
		tmp->y_r_label_font_height = h;
	}

	xbylset = xtyrset = False;
	if (tmp->x_b_major_length_set && 
	    ! tmp->y_l_major_length_set) {
		tmp->y_l_major_length = tmp->x_b_major_length;
		xbylset = True;
	}
	else if (tmp->y_l_major_length_set &&
		 ! tmp->x_b_major_length_set) {
		tmp->x_b_major_length = tmp->y_l_major_length;
		xbylset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_b_major_length + 
		     tmp->y_l_major_length) / 2.0;
		tmp->x_b_major_length = h;
		tmp->y_l_major_length = h;
		if (tmp->x_b_major_length_set)
			xbylset = True;
	}
	if (tmp->x_t_major_length_set && 
	    ! tmp->y_r_major_length_set) {
		tmp->y_r_major_length = tmp->x_t_major_length;
		xtyrset = True;
	}
	else if (tmp->y_r_major_length_set &&
		 ! tmp->x_t_major_length_set) {
		tmp->x_t_major_length = tmp->y_r_major_length;
		xtyrset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_t_major_length + 
		     tmp->y_r_major_length) / 2.0;
		tmp->x_t_major_length = h;
		tmp->y_r_major_length = h;
		if (tmp->x_t_major_length_set)
			xtyrset = True;
	}
	if (xbylset && ! xtyrset) {
		tmp->x_t_major_length = tmp->x_b_major_length;
		tmp->y_r_major_length = tmp->x_b_major_length;
	}
	else if (xtyrset && ! xbylset) {
		tmp->x_b_major_length = tmp->x_t_major_length;
		tmp->y_l_major_length = tmp->x_t_major_length;
	}
	else {
		h = (tmp->x_b_major_length + 
		     tmp->x_t_major_length) / 2.0;
		tmp->x_b_major_length = h;
		tmp->x_t_major_length = h;
		tmp->y_l_major_length = h;
		tmp->y_r_major_length = h;
	}

	xbylset = xtyrset = False;
	if (tmp->x_b_minor_length_set && 
	    ! tmp->y_l_minor_length_set) {
		tmp->y_l_minor_length = tmp->x_b_minor_length;
		xbylset = True;
	}
	else if (tmp->y_l_minor_length_set &&
		 ! tmp->x_b_minor_length_set) {
		tmp->x_b_minor_length = tmp->y_l_minor_length;
		xbylset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_b_minor_length + 
		     tmp->y_l_minor_length) / 2.0;
		tmp->x_b_minor_length = h;
		tmp->y_l_minor_length = h;
		if (tmp->x_b_minor_length_set)
			xbylset = True;
	}
	if (tmp->x_t_minor_length_set && 
	    ! tmp->y_r_minor_length_set) {
		tmp->y_r_minor_length = tmp->x_t_minor_length;
		xtyrset = True;
	}
	else if (tmp->y_r_minor_length_set &&
		 ! tmp->x_t_minor_length_set) {
		tmp->x_t_minor_length = tmp->y_r_minor_length;
		xtyrset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_t_minor_length + 
		     tmp->y_r_minor_length) / 2.0;
		tmp->x_t_minor_length = h;
		tmp->y_r_minor_length = h;
		if (tmp->x_t_minor_length_set)
			xtyrset = True;
	}
	if (xbylset && ! xtyrset) {
		tmp->x_t_minor_length = tmp->x_b_minor_length;
		tmp->y_r_minor_length = tmp->x_b_minor_length;
	}
	else if (xtyrset && ! xbylset) {
		tmp->x_b_minor_length = tmp->x_t_minor_length;
		tmp->y_l_minor_length = tmp->x_t_minor_length;
	}
	else {
		h = (tmp->x_b_minor_length + 
		     tmp->x_t_minor_length) / 2.0;
		tmp->x_b_minor_length = h;
		tmp->x_t_minor_length = h;
		tmp->y_l_minor_length = h;
		tmp->y_r_minor_length = h;
	}

	xbylset = xtyrset = False;
	if (tmp->x_b_major_outward_length_set && 
	    ! tmp->y_l_major_outward_length_set) {
		tmp->y_l_major_outward_length = tmp->x_b_major_outward_length;
		xbylset = True;
	}
	else if (tmp->y_l_major_outward_length_set &&
		 ! tmp->x_b_major_outward_length_set) {
		tmp->x_b_major_outward_length = tmp->y_l_major_outward_length;
		xbylset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_b_major_outward_length + 
		     tmp->y_l_major_outward_length) / 2.0;
		tmp->x_b_major_outward_length = h;
		tmp->y_l_major_outward_length = h;
		if (tmp->x_b_major_outward_length_set)
			xbylset = True;
	}
	if (tmp->x_t_major_outward_length_set && 
	    ! tmp->y_r_major_outward_length_set) {
		tmp->y_r_major_outward_length = tmp->x_t_major_outward_length;
		xtyrset = True;
	}
	else if (tmp->y_r_major_outward_length_set &&
		 ! tmp->x_t_major_outward_length_set) {
		tmp->x_t_major_outward_length = tmp->y_r_major_outward_length;
		xtyrset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_t_major_outward_length + 
		     tmp->y_r_major_outward_length) / 2.0;
		tmp->x_t_major_outward_length = h;
		tmp->y_r_major_outward_length = h;
		if (tmp->x_t_major_outward_length_set)
			xtyrset = True;
	}
	if (xbylset && ! xtyrset) {
		tmp->x_t_major_outward_length = tmp->x_b_major_outward_length;
		tmp->y_r_major_outward_length = tmp->x_b_major_outward_length;
	}
	else if (xtyrset && ! xbylset) {
		tmp->x_b_major_outward_length = tmp->x_t_major_outward_length;
		tmp->y_l_major_outward_length = tmp->x_t_major_outward_length;
	}
	else {
		h = (tmp->x_b_major_outward_length + 
		     tmp->x_t_major_outward_length) / 2.0;
		tmp->x_b_major_outward_length = h;
		tmp->x_t_major_outward_length = h;
		tmp->y_l_major_outward_length = h;
		tmp->y_r_major_outward_length = h;
	}

	xbylset = xtyrset = False;
	if (tmp->x_b_minor_outward_length_set && 
	    ! tmp->y_l_minor_outward_length_set) {
		tmp->y_l_minor_outward_length = tmp->x_b_minor_outward_length;
		xbylset = True;
	}
	else if (tmp->y_l_minor_outward_length_set &&
		 ! tmp->x_b_minor_outward_length_set) {
		tmp->x_b_minor_outward_length = tmp->y_l_minor_outward_length;
		xbylset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_b_minor_outward_length + 
		     tmp->y_l_minor_outward_length) / 2.0;
		tmp->x_b_minor_outward_length = h;
		tmp->y_l_minor_outward_length = h;
		if (tmp->x_b_minor_outward_length_set)
			xbylset = True;
	}
	if (tmp->x_t_minor_outward_length_set && 
	    ! tmp->y_r_minor_outward_length_set) {
		tmp->y_r_minor_outward_length = tmp->x_t_minor_outward_length;
		xtyrset = True;
	}
	else if (tmp->y_r_minor_outward_length_set &&
		 ! tmp->x_t_minor_outward_length_set) {
		tmp->x_t_minor_outward_length = tmp->y_r_minor_outward_length;
		xtyrset = True;
	}
	else { /* either both are set or none are set */
		h = (tmp->x_t_minor_outward_length + 
		     tmp->y_r_minor_outward_length) / 2.0;
		tmp->x_t_minor_outward_length = h;
		tmp->y_r_minor_outward_length = h;
		if (tmp->x_t_minor_outward_length_set)
			xtyrset = True;
	}
	if (xbylset && ! xtyrset) {
		tmp->x_t_minor_outward_length = tmp->x_b_minor_outward_length;
		tmp->y_r_minor_outward_length = tmp->x_b_minor_outward_length;
	}
	else if (xtyrset && ! xbylset) {
		tmp->x_b_minor_outward_length = tmp->x_t_minor_outward_length;
		tmp->y_l_minor_outward_length = tmp->x_t_minor_outward_length;
	}
	else {
		h = (tmp->x_b_minor_outward_length + 
		     tmp->x_t_minor_outward_length) / 2.0;
		tmp->x_b_minor_outward_length = h;
		tmp->x_t_minor_outward_length = h;
		tmp->y_l_minor_outward_length = h;
		tmp->y_r_minor_outward_length = h;
	}

	return(NhlNOERROR);
}


/*
 * Function:	ChangeTransformInfo
 *
 * Description:	Called by SetValues this functions determines what should
 *		happen to the current TransObj based on the new state of the
 *		object. This function will call either CreateXTYRTransfromInfo 
 *		or CreateXBYLTransformInfo as needed to create the appropriate
 *		TransObj.
 *
 * In Args:	tnew the new instance of TickMark
 *		told the old instance of TickMark
 *		args current args list
 *		num_args how many args
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	Sets ir_**min and ir_**max. can potentially call NhlDestroy
 *		---> Important: This uses fields set in CheckIrregular <---- 
 */
static NhlErrorTypes ChangeTransformInfo
#if	NhlNeedProto
(NhlTickMarkLayer tnew, NhlTickMarkLayer told,_NhlArgList args, int num_args)
#else
(tnew, told, args, num_args)
NhlTickMarkLayer tnew;
NhlTickMarkLayer told;
_NhlArgList args;
int num_args;
#endif
{
	int have_ir = 0;
	int need_ir = 0;
	NhlSArg sargs[20];
        int nargs = 0;
        NhlErrorTypes ret = NhlNOERROR;


	if(tnew->tick.xb_yl_trans_obj == NULL ) {
		ret = CreateXBYLTransformInfo(tnew,args, num_args);	
	} else {
/*
* Fist decide what the state change is. It is special when going from
* NhlIRREGULAR to NhlLOG or NhlLINEAR and from NhlLOG or NhlLINEAR to NhlIRREGULAR. In
* both these cases the current TransObj must be destroyed and a new one
* created. In all other cases a SetValues can be used to change the 
* transfromation 
*/

		if((told->tick.y_l_style == NhlIRREGULAR)||		
			(told->tick.x_b_style == NhlIRREGULAR)) {
			have_ir = 1;
		}
		if((tnew->tick.y_l_style == NhlIRREGULAR)||
			(tnew->tick.x_b_style == NhlIRREGULAR)) {
			need_ir = 1;
		}
/*
* Following is the case when an IrregularTransObj is already in use
* and needs to be modified using SetValues
*/
		if((have_ir)&&(need_ir)) {
			switch(tnew->tick.y_l_style) {
				case NhlIRREGULAR:
					if((told->tick.y_l_style != NhlIRREGULAR)||
						(tnew->tick.new_ir_yl)) {
/*
* tnew->tick.new_ir_yl is set in the CheckIrregular function when copiing
* the coordinate point arrays.
*/

					NhlSetSArg(&sargs[nargs++], 
					     NhlNtrYCoordPoints,
                                            tnew->tick.y_l_irregular_points);
					NhlSetSArg(&sargs[nargs++],
						   NhlNtrYAxisType,
						   NhlIRREGULARAXIS);
					}
					if(told->tick.y_l_tension != 
						tnew->tick.y_l_tension) {
                                		NhlSetSArg(&sargs[nargs++],
							NhlNtrYTensionF,
                                        		tnew->tick.y_l_tension);
					}
					break;
				case NhlLOG:
/*
* Since an IrregularTransObj is being used careful attention must be
* made to assure that the max and min do not exceed the range of the coordinate
* arrays, which in the case of log and lin are the min and max of extents 
* for a given axis. the ir_ylmin and ir_ylmax fields are set when the 
* TransObj is created so that the following checks can determin whether or
* not the entire coordinate point array needs to be recreated. 
* When the style changes to NhlLOG then the coordinate arrays must be set no
* matter what.
*/
					if((told->tick.y_l_style != NhlLOG)||
						( tnew->tick.ir_ylmin > 
						tnew->tick.y_l_data_min)
						||(tnew->tick.ir_ylmax < 
						tnew->tick.y_l_data_max)) {

						NhlSetSArg(&sargs[nargs++],
							   NhlNtrYAxisType,
							   NhlLOGAXIS);

						tnew->tick.ir_ylmin =
							tnew->tick.y_l_data_min;
						tnew->tick.ir_ylmax =
							tnew->tick.y_l_data_max;
					}
					break;
				case NhlLINEAR:
/*
* This functions in pretty much the same way as the above NhlLOG switch
*/
					if((told->tick.y_l_style != NhlLINEAR)||
						(tnew->tick.ir_ylmin >
						tnew->tick.y_l_data_min)
						||(tnew->tick.ir_ylmax >
						tnew->tick.y_l_data_max)){
						NhlSetSArg(&sargs[nargs++],
							   NhlNtrYAxisType,
							   NhlLINEARAXIS);

						tnew->tick.ir_ylmin =
							tnew->tick.y_l_data_min;
						tnew->tick.ir_ylmax =
							tnew->tick.y_l_data_max;
					}
					break;
				case NhlTIME:
				case NhlGEOGRAPHIC:
					break;
			}

			switch(tnew->tick.x_b_style) {
				case NhlIRREGULAR:
					if((told->tick.x_b_style != NhlIRREGULAR)||
						(tnew->tick.new_ir_xb)) {

					tnew->tick.new_ir_xb = False;

					NhlSetSArg(&sargs[nargs++], 
						NhlNtrXCoordPoints,
                                       	      tnew->tick.x_b_irregular_points);
					}
					NhlSetSArg(&sargs[nargs++],
						   NhlNtrXAxisType,
						   NhlIRREGULARAXIS);

					if(told->tick.x_b_tension != 
						tnew->tick.x_b_tension) {
                                		NhlSetSArg(&sargs[nargs++],
							NhlNtrXTensionF,
                                        		tnew->tick.x_b_tension);
					}
					break;
				case NhlLOG:
					if((told->tick.x_b_style != NhlLOG)||
						( tnew->tick.ir_xbmin > 
						tnew->tick.x_b_data_min)
						||(tnew->tick.ir_xbmax < 
						tnew->tick.x_b_data_max)) {
						NhlSetSArg(&sargs[nargs++],
							   NhlNtrXAxisType,
							   NhlLOGAXIS);
						tnew->tick.ir_xbmin =
							tnew->tick.x_b_data_min;
						tnew->tick.ir_xbmax =
							tnew->tick.x_b_data_max;
					}
					break;
				case NhlLINEAR:
					if((told->tick.x_b_style != NhlLINEAR)||
						(tnew->tick.ir_xbmin >
						tnew->tick.x_b_data_min)
						||(tnew->tick.ir_xbmax >
						tnew->tick.x_b_data_max)){
						NhlSetSArg(&sargs[nargs++],
							   NhlNtrXAxisType,
							   NhlLINEARAXIS);
						tnew->tick.ir_xbmin =
							tnew->tick.x_b_data_min;
						tnew->tick.ir_xbmax =
							tnew->tick.x_b_data_max;

					}
					break;
				case NhlTIME:
				case NhlGEOGRAPHIC:
					break;
			}
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_b_data_left>tnew->tick.x_b_data_right ? 1 : 0));
                        NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_l_data_bottom >tnew->tick.y_l_data_top? 1 : 0));
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_b_data_min);
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_b_data_max);
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_l_data_min);
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_l_data_max);
			ret = NhlALSetValues(tnew->tick.xb_yl_trans_obj->base.id
				,sargs,nargs);

		} else if((have_ir)&&(!need_ir)) {
			NhlDestroy(tnew->tick.xb_yl_trans_obj->base.id);
			tnew->tick.xb_yl_trans_obj = NULL;
			ret = CreateXBYLTransformInfo(tnew,args, num_args);
		} else if((!have_ir)&&(need_ir)) {
			NhlDestroy(tnew->tick.xb_yl_trans_obj->base.id);
			tnew->tick.xb_yl_trans_obj = NULL;
			ret = CreateXBYLTransformInfo(tnew,args, num_args);
		} else if((!have_ir)&&(!need_ir)){

			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_b_data_left>tnew->tick.x_b_data_right ? 1 : 0));
                        NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_l_data_bottom >tnew->tick.y_l_data_top? 1 : 0));

			switch(tnew->tick.y_l_style) {
			case NhlLOG:
				NhlSetSArg(&sargs[nargs++],NhlNtrYLog,1);
                                NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_l_data_min);
                                NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_l_data_max);
                                break;
                        case NhlLINEAR:
                                NhlSetSArg(&sargs[nargs++],NhlNtrYLog,0);
                                NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_l_data_min);
                                NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_l_data_max);
                                break;
                        case NhlTIME:
                        case NhlGEOGRAPHIC:
			default:
                                break;

			}
			switch(tnew->tick.x_b_style) {
                        case NhlLOG:
                                NhlSetSArg(&sargs[nargs++],NhlNtrXLog,1);
                                NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_b_data_min);
                                NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_b_data_max);
                                break;
                        case NhlLINEAR:
                                NhlSetSArg(&sargs[nargs++],NhlNtrXLog,0);
                                NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_b_data_min);
                                NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_b_data_max);
                                break;
			case NhlIRREGULAR:
                        case NhlTIME:
                        case NhlGEOGRAPHIC:
                                break;
                        }
			ret = NhlALSetValues(tnew->tick.xb_yl_trans_obj->base.id
				,sargs,nargs);

		}

	} 
	have_ir = 0;
	need_ir = 0;
	nargs = 0;
	if(tnew->tick.xt_yr_trans_obj == NULL) {
		ret = CreateXTYRTransformInfo(tnew,args, num_args);
	} else  {

		if((told->tick.y_r_style == NhlIRREGULAR)||		
			(told->tick.x_t_style == NhlIRREGULAR)) {
			have_ir = 1;
		}
		if((tnew->tick.y_r_style == NhlIRREGULAR)||
			(tnew->tick.x_t_style == NhlIRREGULAR)) {
			need_ir = 1;
		}
		if((have_ir)&&(need_ir)) {
			switch(tnew->tick.y_r_style) {
				case NhlIRREGULAR:
					if((told->tick.y_r_style != NhlIRREGULAR)||
						(tnew->tick.new_ir_yr)) {

					NhlSetSArg(&sargs[nargs++], 
						NhlNtrYCoordPoints,
                                            tnew->tick.y_r_irregular_points);
					NhlSetSArg(&sargs[nargs++],
						   NhlNtrYAxisType,
						   NhlIRREGULARAXIS);
					}
					if(told->tick.y_r_tension != 
						tnew->tick.y_r_tension) {
                                		NhlSetSArg(&sargs[nargs++],
							NhlNtrYTensionF,
                                        		tnew->tick.y_r_tension);
					}
					break;
				case NhlLOG:
					if((told->tick.y_r_style != NhlLOG)||
						( tnew->tick.ir_yrmin > 
						tnew->tick.y_r_data_min)
						||(tnew->tick.ir_yrmax < 
						tnew->tick.y_r_data_max)) {

						NhlSetSArg(&sargs[nargs++],
							   NhlNtrYAxisType,
							   NhlLOGAXIS);

						tnew->tick.ir_yrmin =
							tnew->tick.y_r_data_min;
						tnew->tick.ir_yrmax =
							tnew->tick.y_r_data_max;
					}
					break;
				case NhlLINEAR:
					if((told->tick.y_r_style != NhlLINEAR)||
						(tnew->tick.ir_yrmin >
						tnew->tick.y_r_data_min)
						||(tnew->tick.ir_yrmax >
						tnew->tick.y_r_data_max)){
						NhlSetSArg(&sargs[nargs++],
							   NhlNtrYAxisType,
							   NhlLINEARAXIS);

					tnew->tick.ir_yrmin =
						tnew->tick.y_r_data_min;
					tnew->tick.ir_yrmax =
						tnew->tick.y_r_data_max;

					}
					break;
				case NhlTIME:
				case NhlGEOGRAPHIC:
					break;
			}

			switch(tnew->tick.x_t_style) {
				case NhlIRREGULAR:
					if((told->tick.x_t_style != NhlIRREGULAR)||
						(tnew->tick.new_ir_xt)) {

					NhlSetSArg(&sargs[nargs++], 
					     NhlNtrXCoordPoints,
                                             tnew->tick.x_t_irregular_points);
					NhlSetSArg(&sargs[nargs++],
						   NhlNtrXAxisType,
						   NhlIRREGULARAXIS);
					}
					if(told->tick.x_t_tension != 
						tnew->tick.x_t_tension) {
                                		NhlSetSArg(&sargs[nargs++],
							NhlNtrXTensionF,
                                        		tnew->tick.x_t_tension);
					}
					break;
				case NhlLOG:
					if((told->tick.x_t_style != NhlLOG)||
						( tnew->tick.ir_xtmin > 
						tnew->tick.x_t_data_min)
						||(tnew->tick.ir_xtmax < 
						tnew->tick.x_t_data_max)) {

						NhlSetSArg(&sargs[nargs++],
							   NhlNtrXAxisType,
							   NhlLOGAXIS);
						tnew->tick.ir_xtmin =
							tnew->tick.x_t_data_min;
						tnew->tick.ir_xtmax =
							tnew->tick.x_t_data_max;
					}
					break;
				case NhlLINEAR:
					if((told->tick.x_t_style != NhlLINEAR)||
						(tnew->tick.ir_xtmin >
						tnew->tick.x_t_data_min)
						||(tnew->tick.ir_xtmax >
						tnew->tick.x_t_data_max)){
						NhlSetSArg(&sargs[nargs++],
							   NhlNtrXAxisType,
							   NhlLINEARAXIS);

					tnew->tick.ir_xtmin =
						tnew->tick.x_t_data_min;
					tnew->tick.ir_xtmax =
						tnew->tick.x_t_data_max;

					}
					break;
				case NhlTIME:
				case NhlGEOGRAPHIC:
					break;
			}
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_t_data_left>tnew->tick.x_t_data_right ? 1 : 0));
                        NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_r_data_bottom >tnew->tick.y_r_data_top? 1 : 0));
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_t_data_min);
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_t_data_max);
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_r_data_min);
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_r_data_max);
			ret = NhlALSetValues(tnew->tick.xt_yr_trans_obj->base.id
				,sargs,nargs);

		} else if((have_ir)&&(!need_ir)) {
			NhlDestroy(tnew->tick.xt_yr_trans_obj->base.id);
			tnew->tick.xt_yr_trans_obj = NULL;
			ret = CreateXTYRTransformInfo(tnew,args, num_args);
		} else if((!have_ir)&&(need_ir)) {
			NhlDestroy(tnew->tick.xt_yr_trans_obj->base.id);
			tnew->tick.xt_yr_trans_obj = NULL;
			ret = CreateXTYRTransformInfo(tnew,args, num_args);
		} else if((!have_ir)&&(!need_ir)){
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_t_data_left>tnew->tick.x_t_data_right ? 1 : 0));
                        NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_r_data_bottom >tnew->tick.y_r_data_top? 1 : 0));
			switch(tnew->tick.y_r_style) {
			case NhlLOG:
				NhlSetSArg(&sargs[nargs++],NhlNtrYLog,1);
                                NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_r_data_min);
                                NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_r_data_max);
                                break;
                        case NhlLINEAR:
                                NhlSetSArg(&sargs[nargs++],NhlNtrYLog,0);
                                NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_r_data_min);
                                NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_r_data_max);
                                break;
                        case NhlTIME:
                        case NhlGEOGRAPHIC:
			default:
                                break;

			}
			switch(tnew->tick.x_t_style) {
                        case NhlLOG:
                                NhlSetSArg(&sargs[nargs++],NhlNtrXLog,1);
                                NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_t_data_min);
                                NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_t_data_max);
                                break;
                        case NhlLINEAR:
                                NhlSetSArg(&sargs[nargs++],NhlNtrXLog,0);
                                NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_t_data_min);
                                NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_t_data_max);
                                break;
                        case NhlTIME:
                        case NhlGEOGRAPHIC:
			default:
                                break;
                        }
			ret = NhlALSetValues(tnew->tick.xt_yr_trans_obj->base.id
				,sargs,nargs);

		}

	} 
	return ret;
}

static NhlGenArray CreateGetValuesData
#if	NhlNeedProto
(
        NhlLayer        l,
        _NhlArg     	*arg
)
#else
(l,args,nargs)
        NhlLayer        l;
        _NhlArg		*arg;
#endif
{
	NhlString  type;
	NhlPointer data;
	int	   size;
	ng_size_t count;
	NhlTickMarkLayerPart *tmp = &((NhlTickMarkLayer)l)->tick;
	
	if(arg->quark == QXBValues) {
		data = (NhlPointer) tmp->x_b_major_data_locs; 
		count = tmp->x_b_nmajor;
		type = NhlTFloat;
		size = sizeof(float);
	}
	else if(arg->quark == QXBMinorValues) {
		data = (NhlPointer) tmp->x_b_minor_data_locs; 
		count = tmp->x_b_nminor;
		type = NhlTFloat;
		size = sizeof(float);
	}
	else if(arg->quark == QXBLabels) {
		data = tmp->x_b_major_labels;
		count = tmp->x_b_nmajor;
		type = NhlTString;
		size = sizeof(NhlString);
	}
	else if(arg->quark == QXTValues) {
		data = (NhlPointer) tmp->x_t_major_data_locs; 
		count = tmp->x_t_nmajor;
		type = NhlTFloat;
		size = sizeof(float);
	}
	else if(arg->quark == QXTMinorValues) {
		data = (NhlPointer) tmp->x_t_minor_data_locs; 
		count = tmp->x_t_nminor;
		type = NhlTFloat;
		size = sizeof(float);
	}
	else if(arg->quark == QXTLabels) {
		data = tmp->x_t_major_labels;
		count = tmp->x_t_nmajor;
		type = NhlTString;
		size = sizeof(NhlString);
	}
	else if(arg->quark == QYLValues) {
		data = (NhlPointer) tmp->y_l_major_data_locs; 
		count = tmp->y_l_nmajor;
		type = NhlTFloat;
		size = sizeof(float);
	}
	else if(arg->quark == QYLMinorValues) {
		data = (NhlPointer) tmp->y_l_minor_data_locs; 
		count = tmp->y_l_nminor;
		type = NhlTFloat;
		size = sizeof(float);
	}
	else if(arg->quark == QYLLabels) {
		data = tmp->y_l_major_labels;
		count = tmp->y_l_nmajor;
		type = NhlTString;
		size = sizeof(NhlString);
	}
	else if(arg->quark == QYRValues) {
		data = (NhlPointer) tmp->y_r_major_data_locs; 
		count = tmp->y_r_nmajor;
		type = NhlTFloat;
		size = sizeof(float);
	}
	else if(arg->quark == QYRMinorValues) {
		data = (NhlPointer) tmp->y_r_minor_data_locs; 
		count = tmp->y_r_nminor;
		type = NhlTFloat;
		size = sizeof(float);
	}
	else if(arg->quark == QYRLabels) {
		data = tmp->y_r_major_labels;
		count = tmp->y_r_nmajor;
		type = NhlTString;
		size = sizeof(NhlString);
	}
	if (! data || count == 0)
		return NULL;

	return _NhlCreateGenArray(data,type,size,1,&count,True);
}

static NhlErrorTypes TickMarkGetValues
#if	NhlNeedProto
(
        NhlLayer        l,
        _NhlArgList     args,
        int             nargs
)
#else
(l,args,nargs)
        NhlLayer        l;
        _NhlArgList     args;
        int             nargs;
#endif
{
	int		i,j;
	NhlGenArray	ga;
	NhlString	ts;
	NhlBoolean      create_ok;
	NhlTickMarkLayerPart *tmp = &((NhlTickMarkLayer)l)->tick;

	for(i = 0; i < nargs; i++) {
		create_ok = False;
		ga = NULL;
		if(args[i].quark == QXBIrregularPoints) {
			ga = tmp->x_b_irregular_points;
		}
		if(args[i].quark == QXTIrregularPoints) {
			ga = tmp->x_t_irregular_points;
		}
		if(args[i].quark == QYLIrregularPoints) {
			ga = tmp->y_l_irregular_points;
		}
		if(args[i].quark == QYRIrregularPoints) {
			ga = tmp->y_r_irregular_points;
		}
		if(args[i].quark == QXBValues) {
			ga = tmp->x_b_values;
			if (tmp->x_b_nmajor > 0) {
				create_ok = True;
			}
		}
		if(args[i].quark == QXBMinorValues) {
			ga = tmp->x_b_minor_values;
			if (tmp->x_b_nminor > 0) {
				create_ok = True;
			}
		}
		if(args[i].quark == QXTValues) {
			ga = tmp->x_t_values;
			if (tmp->x_t_nmajor > 0) {
				create_ok = True;
			}
		}
		if(args[i].quark == QXTMinorValues) {
			ga = tmp->x_t_minor_values;
			if (tmp->x_t_nminor > 0) {
				create_ok = True;
			}
		}
		if(args[i].quark == QYLValues) {
			ga = tmp->y_l_values;
			if (tmp->y_l_nmajor > 0) {
				create_ok = True;
			}
		}
		if(args[i].quark == QYLMinorValues) {
			ga = tmp->y_l_minor_values;
			if (tmp->y_l_nminor > 0) {
				create_ok = True;
			}
		}
		if(args[i].quark == QYRValues) {
			ga = tmp->y_r_values;
			if (tmp->y_r_nmajor > 0) {
				create_ok = True;
			}
		}
		if(args[i].quark == QYRMinorValues) {
			ga = tmp->y_r_minor_values;
			if (tmp->y_r_nminor > 0) {
				create_ok = True;
			}
		}
		if(args[i].quark == QXBLabels) {
			ga = tmp->x_b_labels;
			if (ga && ga->num_elements < tmp->x_b_nmajor) {
				ga = NULL;
			}
			/*
			 * return a NULL array unless at least 
			 * one string is non-NULL
			 */
			for (j = 0; j < tmp->x_b_nmajor; j++) {
				if (tmp->x_b_major_labels[j] != NULL) {
					create_ok = True;
					break;
				}
			}
		}
		if(args[i].quark == QXTLabels) {
			ga = tmp->x_t_labels;
			if (ga && ga->num_elements < tmp->x_t_nmajor) {
				ga = NULL;
			}
			/*
			 * return a NULL array unless at least 
			 * one string is non-NULL
			 */
			for (j = 0; j < tmp->x_t_nmajor; j++) {
				if (tmp->x_t_major_labels[j] != NULL) {
					create_ok = True;
					break;
				}
			}
		}
		if(args[i].quark == QYLLabels) {
			ga = tmp->y_l_labels;
			if (ga && ga->num_elements < tmp->y_l_nmajor) {
				ga = NULL;
			}
			/*
			 * return a NULL array unless at least 
			 * one string is non-NULL
			 */
			for (j = 0; j < tmp->y_l_nmajor; j++) {
				if (tmp->y_l_major_labels[j] != NULL) {
					create_ok = True;
					break;
				}
			}
		}
		if(args[i].quark == QYRLabels) {
			ga = tmp->y_r_labels;
			if (ga && ga->num_elements < tmp->y_r_nmajor) {
				ga = NULL;
			}
			/*
			 * return a NULL array unless at least 
			 * one string is non-NULL
			 */
			for (j = 0; j < tmp->y_r_nmajor; j++) {
				if (tmp->y_r_major_labels[j] != NULL) {
					create_ok = True;
					break;
				}
			}
		}

		if(ga != NULL) {
			*((NhlGenArray *)args[i].value.ptrval) = 
				_NhlCopyGenArray(ga,True);
			if(!*(NhlGenArray *)args[i].value.ptrval){
                                NhlPError(NhlFATAL,ENOMEM,
                                     "TickMarkGetValues:Unable to retrieve %s",
					  NrmQuarkToString(args[i].quark));
				return NhlFATAL;
                        }
			continue;
		}
		else if (create_ok) {
			*((NhlGenArray *)args[i].value.ptrval) = 
				CreateGetValuesData(l,&args[i]);
			if(!*(NhlGenArray *)args[i].value.ptrval){
                                NhlPError(NhlFATAL,ENOMEM,
                                     "TickMarkGetValues:Unable to retrieve %s",
					  NrmQuarkToString(args[i].quark));
				return NhlFATAL;
                        }
			continue;
		}
		ts = NULL;
		if(args[i].quark == QXBFormat){
			ts = tmp->x_b_format.fstring;
		}
		else if (args[i].quark == QXTFormat){
			ts = tmp->x_t_format.fstring;
		}
		else if (args[i].quark == QYLFormat){
			ts = tmp->y_l_format.fstring;
		}
		else if (args[i].quark == QYRFormat){
			ts = tmp->y_r_format.fstring;
		}
                if (ts != NULL) {
			*((NhlString*)(args[i].value.ptrval)) =
				NhlMalloc(strlen(ts)+1);
			if(!*((NhlString*)(args[i].value.ptrval))){
                                NhlPError(NhlFATAL,ENOMEM,
					  "%s: error retrieving %s",
					  "TickMarkGetValues",
					  NrmQuarkToString(args[i].quark));
                                return NhlFATAL;
                        }
			strcpy(*((NhlString*)(args[i].value.ptrval)),ts);
			continue;
                }
	}
	return(NhlNOERROR);
}
