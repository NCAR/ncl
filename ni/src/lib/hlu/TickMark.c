/*
 *      $Id: TickMark.c,v 1.11 1994-01-27 21:26:09 boote Exp $
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
#include <ncarg/hlu/TickMarkP.h>
#include <ncarg/hlu/TransObjI.h>
#include <ncarg/hlu/IrregularType2TransObj.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/MultiText.h>
#include <ncarg/hlu/Converters.h>


/* resource list definition */

static NhlResource resources[] = {
	{ NhlNtmSciNoteCutoff, NhlCtmSciNoteCutoff, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.sci_note_cutoff),
		NhlTImmediate,(NhlPointer)6},
	{ NhlNtmXUseBottom, NhlCtmXUseBottom, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_use_bottom),
		NhlTImmediate,(NhlPointer)True},
	{ NhlNtmXBOn, NhlCtmXBOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_on),
		NhlTImmediate,(NhlPointer)True},
	{ NhlNtmXTOn, NhlCtmXTOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_on),
		NhlTImmediate,(NhlPointer)True},
	{ NhlNtmXBLabelsOn, NhlCtmXBLabelsOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_labels_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmXTLabelsOn, NhlCtmXTLabelsOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_labels_on),
		NhlTImmediate,(NhlPointer)False },
	{ NhlNtmXBBorderOn, NhlCtmXBBorderOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_border_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmXTBorderOn, NhlCtmXTBorderOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_border_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmXBMode, NhlCtmXBMode, NhlTTickMarkModes, sizeof(NhlTickMarkModes),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_mode),
		NhlTImmediate,(NhlPointer) NhlAUTOMATIC },
	{ NhlNtmXTMode, NhlCtmXTMode, NhlTTickMarkModes, sizeof(NhlTickMarkModes),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_mode),
		NhlTImmediate,(NhlPointer) NhlAUTOMATIC }, 
	{ NhlNtmXBStyle,NhlCtmXBStyle,NhlTTickMarkStyles,sizeof(NhlTickMarkStyles),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_style),
		NhlTImmediate,(NhlPointer) NhlLINEAR },
	{ NhlNtmXBIrrTensionF,NhlCtmXBIrrTensionF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_tension),
		NhlTString,"2.0"},

	{ NhlNtmXTStyle,NhlCtmXTStyle,NhlTTickMarkStyles,sizeof(NhlTickMarkStyles),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_style),
		NhlTImmediate,(NhlPointer) NhlLINEAR },
	{ NhlNtmXTIrrTensionF,NhlCtmXTIrrTensionF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_tension),
		NhlTString, "2.0"},

	{ NhlNtmBorderThicknessF,NhlCtmBorderThicknessF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.border_thickness),
		NhlTString,"2.0" },
	{ NhlNtmBorderLineColor,NhlCtmBorderLineColor,NhlTInteger,
		sizeof(int), NhlOffset(NhlTickMarkLayerRec, tick.border_line_color),
		NhlTImmediate,(NhlPointer)1},

	{ NhlNtmXBPrecision, NhlCtmPrecisions, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_precision),
		NhlTImmediate, (NhlPointer)4 },
	{ NhlNtmXTPrecision, NhlCtmPrecisions, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_precision),
		NhlTImmediate, (NhlPointer)4 },

	{ NhlNtmXMajorGrid, NhlCtmXMajorGrid, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_major_grid),
		NhlTImmediate, (NhlPointer)False },
	{ NhlNtmXMinorGrid, NhlCtmXMinorGrid, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.x_minor_grid),
		NhlTImmediate, (NhlPointer)False },
	{ NhlNtmXMajorGridThicknessF, NhlCtmMajorGridThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_major_grid_thickness),
		NhlTString, "2.0" },
	{ NhlNtmXMajorGridLineColor, NhlCtmMajorGridLineColors, NhlTInteger,
		sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.x_major_grid_line_color),
		NhlTImmediate, (NhlPointer)1 },
	{ NhlNtmXMajorGridLineDashPattern, NhlCtmMajorGridLineDashPatterns,
		NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_major_grid_line_dash_pattern),
		NhlTImmediate, (NhlPointer)0 },

	{ NhlNtmXMinorGridThicknessF, NhlCtmMinorGridThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_minor_grid_thickness),
		NhlTString, "1.0" },
	{ NhlNtmXMinorGridLineColor, NhlCtmMinorGridLineColors, NhlTInteger,
		sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.x_minor_grid_line_color),
		NhlTImmediate, (NhlPointer)1 },
	{ NhlNtmXMinorGridLineDashPattern, NhlCtmMinorGridLineDashPatterns,
		NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_minor_grid_line_dash_pattern),
		NhlTImmediate, (NhlPointer)0 },

	{ NhlNtmXBMinorPerMajor, NhlCtmXMinorPerMajor,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_per_major),
		NhlTImmediate, (NhlPointer)3 },
	{ NhlNtmXTMinorPerMajor, NhlCtmXMinorPerMajor,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_per_major),
		NhlTImmediate, (NhlPointer)3 },
	{ NhlNtmXBMinorOn, NhlCtmXBMinorOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmXTMinorOn, NhlCtmXTMinorOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmXBLabelStride, NhlCtmXBLabelStride, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_stride),
		NhlTImmediate,(NhlPointer)0 },
	{ NhlNtmXTLabelStride, NhlCtmXTLabelStride, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_stride),
		NhlTImmediate,(NhlPointer)0 },

	{ NhlNtmXBDataLeftF, NhlCtmXBDataLeftF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_data_left),
		NhlTString,"0.0" },
	{ NhlNtmXBDataRightF, NhlCtmXBDataRightF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_data_right),
		NhlTString,"0.0" },
	{ NhlNtmXBTickStartF, NhlCtmXBTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_tick_start),
		NhlTString,"0.0" },
	{ NhlNtmXBTickEndF, NhlCtmXBTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_tick_end),
		NhlTString,"0.0" },
	{ NhlNtmXBMaxTicks, NhlCtmXBMaxTicks, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_max_ticks),
		NhlTImmediate, (NhlPointer)7 },
	{ NhlNtmXBTickSpacingF,NhlCtmXBTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_tick_spacing),
		NhlTString, "0.0" },
	{ NhlNtmXBSpacingType, NhlCtmXBSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_spacing_type),
		NhlTImmediate, (NhlPointer)0 },
	{ NhlNtmXBIrregularPoints, NhlCtmXBIrregularPoints,NhlTGenArray,
		sizeof(NhlPointer),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_irregular_points),
		NhlTImmediate, NULL},
	{ NhlNtmXBValues, NhlCtmXBValues, NhlTGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_values),
		NhlTImmediate, NULL },
	{ NhlNtmXBLabels, NhlCtmXBLabels, NhlTGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_labels),
		NhlTImmediate, NULL },
	{ NhlNtmXBMajorThicknessF, NhlCtmMajorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_major_thickness),
		NhlTString, "2.0" },
	{NhlNtmXBMajorLineColor, NhlCtmMajorLineColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_major_line_color),
		NhlTImmediate, (NhlPointer)1},
	{NhlNtmXBMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_major_length),
		NhlTString, ".02"},
	{NhlNtmXBMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_major_outward_length),
		NhlTString, "0.0"},
	{ NhlNtmXBMinorThicknessF, NhlCtmMinorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_thickness),
		NhlTString, "1.0" },
	{NhlNtmXBMinorLineColor, NhlCtmMinorLineColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_line_color),
		NhlTImmediate, (NhlPointer)1},
	{NhlNtmXBMinorLengthF, NhlCtmMinorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_minor_length),
		NhlTString, ".01"},
	{NhlNtmXBMinorOutwardLengthF, NhlCtmMinorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_minor_outward_length),
		NhlTString, "0.0"},	
	{NhlNtmXBLabelFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_label_font),
		NhlTImmediate, (NhlPointer)0 },
	{NhlNtmXBLabelFontHeightF, NhlCtmLabelFontHeightsF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_b_label_font_height),
		NhlTString, "0.02" },
	{NhlNtmXBLabelFontColor, NhlCtmLabelFontColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_font_color),
		NhlTImmediate, (NhlPointer)1 },
	{NhlNtmXBLabelFontAspectF, NhlCtmLabelFontAspectsF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_font_aspect),
		NhlTString, "1.3125" },
	{NhlNtmXBLabelJust, NhlCtmXLabelJusts,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_just),
		NhlTImmediate, (NhlPointer)3 },
	{NhlNtmXBLabelAngleF, NhlCtmXLabelAnglesF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_angle),
		NhlTString, "0.0" },
	{NhlNtmXBLabelDirection, NhlCtmXLabelDirections, NhlTTextDirection,
		sizeof(NhlTextDirection),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_direction),
		NhlTImmediate, (NhlPointer) NhlACROSS },
	{ NhlNtmXBLabelDeltaF, NhlCtmXBLabelDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_label_delta),
		NhlTString, "0.0" },
	{ NhlNtmXBAutoPrecision, NhlCtmAutoPrecision, NhlTBoolean,
		sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.x_b_auto_precision),
		NhlTImmediate, (NhlPointer)True },
	

	{ NhlNtmXTDataLeftF, NhlCtmXTDataLeftF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_data_left),
		NhlTString,"0.0" },
	{ NhlNtmXTDataRightF, NhlCtmXTDataRightF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_data_right),
		NhlTString,"0.0" },
	{ NhlNtmXTTickStartF, NhlCtmXTTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_tick_start),
		NhlTString,"0.0" },
	{ NhlNtmXTTickEndF, NhlCtmXTTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_tick_end),
		NhlTString,"0.0" },
	{ NhlNtmXTMaxTicks, NhlCtmXTMaxTicks, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_max_ticks),
		NhlTImmediate, (NhlPointer)7 },
	{ NhlNtmXTTickSpacingF, NhlCtmXTTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_tick_spacing),
		NhlTString, "0.0" },
	{ NhlNtmXTSpacingType, NhlCtmXTSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_spacing_type),
		NhlTImmediate, (NhlPointer)0 },
	{ NhlNtmXTIrregularPoints, NhlCtmXTIrregularPoints,NhlTGenArray,
		sizeof(NhlPointer),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_irregular_points),
		NhlTImmediate, NULL},
	{ NhlNtmXTValues, NhlCtmXTValues, NhlTGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_values),
		NhlTImmediate, NULL },
	{ NhlNtmXTLabels, NhlCtmXTLabels, NhlTGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_labels),
		NhlTImmediate, NULL },
	{ NhlNtmXTMajorThicknessF, NhlCtmMajorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_major_thickness),
		NhlTString, "2.0" },
	{NhlNtmXTMajorLineColor, NhlCtmMajorLineColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_major_line_color),
		NhlTImmediate, (NhlPointer)1},
	{NhlNtmXTMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_major_length),
		NhlTString, ".02"},
	{NhlNtmXTMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_major_outward_length),
		NhlTString, "0.0"},
	{ NhlNtmXTMinorThicknessF, NhlCtmMinorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_thickness),
		NhlTString, "1.0" },
	{NhlNtmXTMinorLineColor, NhlCtmMinorLineColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_line_color),
		NhlTImmediate, (NhlPointer)1},
	{NhlNtmXTMinorLengthF, NhlCtmMinorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_minor_length),
		NhlTString, ".01"},
	{NhlNtmXTMinorOutwardLengthF, NhlCtmMinorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_minor_outward_length),
		NhlTString, "0.0"},	
	{NhlNtmXTLabelFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_label_font),
		NhlTImmediate, (NhlPointer)0 },
	{NhlNtmXTLabelFontHeightF, NhlCtmLabelFontHeightsF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.x_t_label_font_height),
		NhlTString, "0.02" },
	{NhlNtmXTLabelFontColor, NhlCtmLabelFontColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_font_color),
		NhlTImmediate, (NhlPointer)1 },
	{NhlNtmXTLabelFontAspectF, NhlCtmLabelFontAspectsF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_font_aspect),
		NhlTString, "1.3125" },
	{NhlNtmXTLabelJust, NhlCtmXLabelJusts,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_just),
		NhlTImmediate, (NhlPointer)5 },
	{NhlNtmXTLabelAngleF, NhlCtmXLabelAnglesF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_angle),
		NhlTString, "0.0" },
	{NhlNtmXTLabelDirection, NhlCtmXLabelDirections, NhlTTextDirection,
		sizeof(NhlTextDirection),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_direction),
		NhlTImmediate, (NhlPointer) NhlACROSS },
	{ NhlNtmXTLabelDeltaF, NhlCtmXTLabelDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_label_delta),
		NhlTString, "0.0" },
	{ NhlNtmXTAutoPrecision, NhlCtmAutoPrecision, NhlTBoolean,
		sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.x_t_auto_precision),
		NhlTImmediate, (NhlPointer)True },

	{ NhlNtmYUseLeft, NhlCtmYUseLeft, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_use_left),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmYLOn, NhlCtmYLOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmYROn, NhlCtmYROn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmYLLabelsOn, NhlCtmYLLabelsOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_labels_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmYRLabelsOn, NhlCtmYRLabelsOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_labels_on),
		NhlTImmediate,(NhlPointer)False },
	{ NhlNtmYLBorderOn, NhlCtmYLBorderOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_border_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmYRBorderOn, NhlCtmYRBorderOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_border_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmYLMode, NhlCtmYLMode, NhlTTickMarkModes, sizeof(NhlTickMarkModes),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_mode),
		NhlTImmediate,(NhlPointer) NhlAUTOMATIC },
	{ NhlNtmYRMode, NhlCtmYRMode, NhlTTickMarkModes, sizeof(NhlTickMarkModes),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_mode),
		NhlTImmediate,(NhlPointer) NhlAUTOMATIC }, 
	{ NhlNtmYLStyle,NhlCtmYLStyle,NhlTTickMarkStyles,sizeof(NhlTickMarkStyles),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_style),
		NhlTImmediate,(NhlPointer) NhlLINEAR },
	{ NhlNtmYLIrrTensionF,NhlCtmYLIrrTensionF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_tension),
		NhlTString, "2.0"},
	{ NhlNtmYRStyle,NhlCtmYRStyle,NhlTTickMarkStyles,sizeof(NhlTickMarkStyles),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_style),
		NhlTImmediate,(NhlPointer) NhlLINEAR },
	{ NhlNtmYRIrrTensionF,NhlCtmYRIrrTensionF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_tension),
		NhlTString, "2.0"},


	{ NhlNtmYLPrecision,NhlCtmPrecisions, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_precision),
		NhlTImmediate,(NhlPointer)4 },

	{ NhlNtmYRPrecision,NhlCtmPrecisions, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_precision),
		NhlTImmediate,(NhlPointer)4 },

	{ NhlNtmYMajorGrid, NhlCtmXMajorGrid, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_major_grid),
		NhlTImmediate, (NhlPointer)False },
	{ NhlNtmYMinorGrid, NhlCtmXMinorGrid, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec, tick.y_minor_grid),
		NhlTImmediate, (NhlPointer)False },
	{ NhlNtmYMajorGridThicknessF, NhlCtmMajorGridThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_major_grid_thickness),
		NhlTString, "2.0" },
	{ NhlNtmYMajorGridLineColor, NhlCtmMajorGridLineColors, NhlTInteger,
		sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.y_major_grid_line_color),
		NhlTImmediate, (NhlPointer)1 },
	{ NhlNtmYMajorGridLineDashPattern, NhlCtmMajorGridLineDashPatterns,
		NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_major_grid_line_dash_pattern),
		NhlTImmediate, (NhlPointer)0 },

	{ NhlNtmYMinorGridThicknessF, NhlCtmMinorGridThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_minor_grid_thickness),
		NhlTString, "1.0" },
	{ NhlNtmYMinorGridLineColor, NhlCtmMinorGridLineColors, NhlTInteger,
		sizeof(int),
		NhlOffset(NhlTickMarkLayerRec, tick.y_minor_grid_line_color),
		NhlTImmediate, (NhlPointer)1 },
	{ NhlNtmYMinorGridLineDashPattern, NhlCtmMinorGridLineDashPatterns,
		NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_minor_grid_line_dash_pattern),
		NhlTImmediate, (NhlPointer)0 },

	{ NhlNtmYLMinorPerMajor, NhlCtmYMinorPerMajor,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_per_major),
		NhlTImmediate, (NhlPointer)3 },
	{ NhlNtmYRMinorPerMajor, NhlCtmYMinorPerMajor,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_per_major),
		NhlTImmediate, (NhlPointer)3 },
	{ NhlNtmYLMinorOn, NhlCtmYLMinorOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmYRMinorOn, NhlCtmYRMinorOn, NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_on),
		NhlTImmediate,(NhlPointer)True },
	{ NhlNtmYLLabelStride, NhlCtmYLLabelStride, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_stride),
		NhlTImmediate,(NhlPointer)0 },
	{ NhlNtmYRLabelStride, NhlCtmYRLabelStride, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_stride),
		NhlTImmediate,(NhlPointer)0 },

	{ NhlNtmYLDataTopF, NhlCtmYLDataTopF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_data_top),
		NhlTString,"0.0" },
	{ NhlNtmYLDataBottomF, NhlCtmYLDataBottomF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_data_bottom),
		NhlTString,"0.0" },
	{ NhlNtmYLTickStartF, NhlCtmYLTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_tick_start),
		NhlTString,"0.0" },
	{ NhlNtmYLTickEndF, NhlCtmYLTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_tick_end),
		NhlTString,"0.0" },
	{ NhlNtmYLMaxTicks, NhlCtmYLMaxTicks, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_max_ticks),
		NhlTImmediate, (NhlPointer)7 },
	{ NhlNtmYLTickSpacingF, NhlCtmYLTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_tick_spacing),
		NhlTString, "0.0" },
	{ NhlNtmYLSpacingType, NhlCtmYLSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_spacing_type),
		NhlTImmediate, (NhlPointer)0 },
	{ NhlNtmYLIrregularPoints, NhlCtmYLIrregularPoints,NhlTGenArray,
		sizeof(NhlPointer),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_irregular_points),
		NhlTImmediate, NULL},
	{ NhlNtmYLValues, NhlCtmYLValues, NhlTGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_values),
		NhlTImmediate, NULL },
	{ NhlNtmYLLabels, NhlCtmYLLabels, NhlTGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_labels),
		NhlTImmediate, NULL },
	{ NhlNtmYLMajorThicknessF, NhlCtmMajorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_major_thickness),
		NhlTString, "2.0" },
	{NhlNtmYLMajorLineColor, NhlCtmMajorLineColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_major_line_color),
		NhlTImmediate, (NhlPointer)1},
	{NhlNtmYLMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_major_length),
		NhlTString, ".02"},
	{NhlNtmYLMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_major_outward_length),
		NhlTString, "0.0"},
	{ NhlNtmYLMinorThicknessF, NhlCtmMinorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_thickness),
		NhlTString, "1.0" },
	{NhlNtmYLMinorLineColor, NhlCtmMinorLineColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_line_color),
		NhlTImmediate, (NhlPointer)1},
	{NhlNtmYLMinorLengthF, NhlCtmMinorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_minor_length),
		NhlTString, ".01"},
	{NhlNtmYLMinorOutwardLengthF, NhlCtmMinorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_minor_outward_length),
		NhlTString, "0.0"},	
	{NhlNtmYLLabelFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_label_font),
		NhlTImmediate, (NhlPointer)0 },
	{NhlNtmYLLabelFontHeightF, NhlCtmLabelFontHeightsF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_l_label_font_height),
		NhlTString, "0.02" },
	{NhlNtmYLLabelFontColor, NhlCtmLabelFontColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_font_color),
		NhlTImmediate, (NhlPointer)1 },
	{NhlNtmYLLabelFontAspectF, NhlCtmLabelFontAspectsF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_font_aspect),
		NhlTString, "1.3125" },
	{NhlNtmYLLabelJust, NhlCtmYLabelJusts,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_just),
		NhlTImmediate, (NhlPointer)7 },
	{NhlNtmYLLabelAngleF, NhlCtmYLabelAnglesF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_angle),
		NhlTString, "0.0" },
	{NhlNtmYLLabelDirection, NhlCtmYLabelDirections, NhlTTextDirection,
		sizeof(NhlTextDirection),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_direction),
		NhlTImmediate, (NhlPointer) NhlACROSS },
	{ NhlNtmYLLabelDeltaF, NhlCtmYLLabelDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_label_delta),
		NhlTString, "0.0" },
	{ NhlNtmYLAutoPrecision, NhlCtmAutoPrecision, NhlTBoolean,
		sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.y_l_auto_precision),
		NhlTImmediate, (NhlPointer)True },
	

	{ NhlNtmYRDataTopF, NhlCtmYRDataTopF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_data_top),
		NhlTString,"0.0" },
	{ NhlNtmYRDataBottomF, NhlCtmYRDataBottomF,NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_data_bottom),
		NhlTString,"0.0" },
	{ NhlNtmYRTickStartF, NhlCtmYRTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_tick_start),
		NhlTString,"0.0" },
	{ NhlNtmYRTickEndF, NhlCtmYRTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_tick_end),
		NhlTString,"0.0" },
	{ NhlNtmYRMaxTicks, NhlCtmYRMaxTicks, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_max_ticks),
		NhlTImmediate, (NhlPointer)7 },
	{ NhlNtmYRTickSpacingF, NhlCtmYRTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_tick_spacing),
		NhlTString, "0.0" },
	{ NhlNtmYRSpacingType, NhlCtmYRSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_spacing_type),
		NhlTImmediate, (NhlPointer)0 },
	{ NhlNtmYRIrregularPoints, NhlCtmYRIrregularPoints,NhlTGenArray,
		sizeof(NhlPointer),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_irregular_points),
		NhlTImmediate, NULL},
	{ NhlNtmYRValues, NhlCtmYRValues, NhlTGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_values),
		NhlTImmediate, NULL },
	{ NhlNtmYRLabels, NhlCtmYRLabels, NhlTGenArray, sizeof(NhlGenArray),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_labels),
		NhlTImmediate, NULL },
	{ NhlNtmYRMajorThicknessF, NhlCtmMajorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_major_thickness),
		NhlTString, "2.0" },
	{NhlNtmYRMajorLineColor, NhlCtmMajorLineColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_major_line_color),
		NhlTImmediate, (NhlPointer)1},
	{NhlNtmYRMajorLengthF, NhlCtmMajorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_major_length),
		NhlTString, ".02"},
	{NhlNtmYRMajorOutwardLengthF, NhlCtmMajorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_major_outward_length),
		NhlTString, "0.0"},
	{ NhlNtmYRMinorThicknessF, NhlCtmMinorThicknessesF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_thickness),
		NhlTString, "1.0" },
	{NhlNtmYRMinorLineColor, NhlCtmMinorLineColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_line_color),
		NhlTImmediate, (NhlPointer)1},
	{NhlNtmYRMinorLengthF, NhlCtmMinorLengthsF, NhlTFloat,sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_minor_length),
		NhlTString, ".01"},
	{NhlNtmYRMinorOutwardLengthF, NhlCtmMinorOutwardLengthsF, NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_minor_outward_length),
		NhlTString, "0.0"},	
	{NhlNtmYRLabelFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_label_font),
		NhlTImmediate, (NhlPointer)0 },
	{NhlNtmYRLabelFontHeightF, NhlCtmLabelFontHeightsF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec, tick.y_r_label_font_height),
		NhlTString, "0.02" },
	{NhlNtmYRLabelFontColor, NhlCtmLabelFontColors, NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_font_color),
		NhlTImmediate, (NhlPointer)1 },
	{NhlNtmYRLabelFontAspectF, NhlCtmLabelFontAspectsF,NhlTFloat,
		sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_font_aspect),
		NhlTString, "1.3125" },
	{NhlNtmYRLabelJust, NhlCtmYLabelJusts,NhlTInteger,sizeof(int),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_just),
		NhlTImmediate, (NhlPointer)1 },
	{NhlNtmYRLabelAngleF, NhlCtmYLabelAnglesF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_angle),
		NhlTString, "0.0" },
	{NhlNtmYRLabelDirection, NhlCtmYLabelDirections, NhlTTextDirection,
		sizeof(NhlTextDirection),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_direction),
		NhlTImmediate, (NhlPointer) NhlACROSS },
	{ NhlNtmYRLabelDeltaF, NhlCtmYRLabelDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_label_delta),
		NhlTString, "0.0" },
	{ NhlNtmYRAutoPrecision, NhlCtmAutoPrecision, NhlTBoolean,
		sizeof(NhlBoolean),
		NhlOffset(NhlTickMarkLayerRec,tick.y_r_auto_precision),
		NhlTImmediate, (NhlPointer)True }
};

static NhlErrorTypes DrawLabels (
#ifdef NhlNeedProto
NhlTickMarkLayer /*tlayer*/
#endif 
);
static NhlErrorTypes DrawTicks(
#ifdef NhlNeedProto
NhlTickMarkLayer /*tlayer*/
#endif 
);
static NhlErrorTypes	DrawGrid(
#ifdef NhlNeedProto
NhlTickMarkLayer /*tlayer*/
#endif 
);
static NhlErrorTypes	DrawBorder(
#ifdef NhlNeedProto
NhlTickMarkLayer /*tlayer*/
#endif 
);
static NhlErrorTypes ComputeMinorTickMarks(
#ifdef NhlNeedProto
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
	NhlTickMarkStyles style
#endif 
);
static NhlErrorTypes AutoComputeMajorTickMarks(
#ifdef NhlNeedProto 
        NhlTickMarkStyles  /* style*/,
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
        int /*cutoff*/
#endif
);
static NhlErrorTypes ManualComputeMajorTickMarks(
#ifdef NhlNeedProto
        NhlTickMarkStyles /*style*/,
        float */*array*/,
        char** /*larray*/,
        float /*dmax*/,
        float /*dmin*/,
        float /*tstart*/,
        float /*tend*/,
        int /*convert_precision*/,
        float /*spacing*/,
        int /*spacing_type*/,
        int */*nmajor*/,
        int /*cutoff*/
#endif
);
static NhlErrorTypes ChooseSpacingLin(
#ifdef NhlNeedProto
float * /*tstart*/,
float * /*tend*/,
float * /*spacing*/,
int /*convert_precision*/,
int /*max_ticks*/
#endif
);
static NhlErrorTypes ChooseSpacingLog(
#ifdef NhlNeedProto
float * /*tstart*/,
float * /*tend*/,
float * /*spacing*/,
int /* convert_precision*/,
int /* max_ticks */
#endif
);

static NhlErrorTypes ExplicitComputeMajorTickMarks(
#ifdef NhlNeedProto
NhlTickMarkStyles /*style*/,
float */*array*/,
char** /*larray*/,
float /*dmax*/,
float /*dmin*/,
float /*tstart*/,
float /*tend*/,
int /*convert_precision*/,
float */*requested_points*/,
char** /*requested_labels*/,
int */*nmajor*/,
int /* n_requested */
#endif
);



/* Method function declarations */

static NhlErrorTypes	TickMarkSetValues(
#ifdef NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes	TickMarkInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes	TickMarkDestroy(
#ifdef NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes	TickMarkClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes	TickMarkDraw(
#ifdef NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static void SetTop(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */
#endif
);
static void SetRight(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */
#endif
);
static NhlErrorTypes CheckKeyVals(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckManual(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckExplicit(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
_NhlArgList	/* args */,
int		/* num_args*/,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckNotAuto(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckLog(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckIrregular(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
_NhlArgList	/* args */,
int		/* num_args */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckTime(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes CheckGeographic(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);
static NhlErrorTypes ComputeTickInfo(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);

static NhlErrorTypes TransformLocations(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer	/* told */,
int		/* c_or_s */
#endif
);

#define CREATE 1
#define SET 0

static NhlErrorTypes ComputeAndSetLabelInfo(
#ifdef NhlNeedProto
NhlTickMarkLayer 	/* tnew */,
NhlTickMarkLayer 	/* told */,
int		/* cors */
#endif
);

static NhlErrorTypes CreateXTYRTransformInfo(
#ifdef NhlNeedProto
NhlTickMarkLayer 	tnew,
_NhlArgList	args,
int		num_args
#endif
);

static NhlErrorTypes ChangeTransformInfo(
#ifdef NhlNeedProto
NhlTickMarkLayer 	tnew,
NhlTickMarkLayer 	told,
_NhlArgList	args,
int		num_args
#endif
);

static NhlErrorTypes CreateXBYLTransformInfo(
#ifdef NhlNeedProto
NhlTickMarkLayer 	tnew,
_NhlArgList	args,
int		num_args
#endif
);

static NhlErrorTypes ScaleValuesForMove(
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
        NhlLayer          /* instance */,
        NhlBoundingBox * /*thebox*/
#endif
);

static char *ConvertToString(
#ifdef NhlNeedProto
	float		value,
	int		convert_precision,
	int		compare_precision,
	NhlTickMarkStyles	style,
	int		cutoff
#endif
);

NhlTickMarkLayerClassRec NhltickMarkLayerClassRec = {
        {
/* class_name		*/      "TickMark",
/* nrm_class		*/      NrmNULLQUARK,
/* layer_size		*/      sizeof(NhlTickMarkLayerRec),
/* class_inited		*/      False,
/* superclass		*/      (NhlLayerClass)&NhlviewLayerClassRec,

/* layer_resources	*/      resources,
/* num_resources	*/      NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize*/      NULL,
/* class_initialize	*/      TickMarkClassInitialize,
/* layer_initialize	*/      TickMarkInitialize,
/* layer_set_values	*/      TickMarkSetValues,
/* layer_set_values_hook*/      NULL,
/* layer_get_values	*/      NULL,
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


NhlLayerClass NhltickMarkLayerClass = (NhlLayerClass)&NhltickMarkLayerClassRec;

static NrmQuark Qfloat = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;

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
#if  __STDC__
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



	for(i = 0; i< MAXTICKS; i++ ) {
		tnew->tick.x_b_major_labels[i] = NULL;
		tnew->tick.x_t_major_labels[i] = NULL;
		tnew->tick.y_l_major_labels[i] = NULL;
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
* WHETHER OR NOT THESE ROUTINES NEED TO BE CALLED EVERY NhlTIME ARE NEEDED
*/

	ret = ChangeTransformInfo(tnew,told,args,num_args);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkSetValues: A fatal error was detected while setting up tranformation information,cannot continue");
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
#if	__STDC__
( NhlLayerClass class, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
	NhlLayerClass	class;
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
* WHETHER OR NOT THESE ROUTINES NEED TO BE CALLED EVERY NhlTIME ARE NEEDED
*/
/*
* At this point all resource values should be confirmed and can then procede
* with internal field generation
*/
	ret = CreateXBYLTransformInfo(tnew,args,num_args);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while setting up tranformation information,cannot continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;
	ret = CreateXTYRTransformInfo(tnew,args,num_args);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkInitialize: A fatal error was detected while setting up tranformation information,cannot continue");
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
#if	__STDC__
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

	NhlFreeGenArray(tinst->tick.y_r_irregular_points);
	NhlFreeGenArray(tinst->tick.y_r_values);

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
	NhlFree(tinst->tick.y_l_major_ndc_locs);
	NhlFree(tinst->tick.y_l_major_data_locs);
	NhlFree(tinst->tick.y_l_minor_data_locs);
	NhlFree(tinst->tick.y_l_minor_ndc_locs);

	NhlFreeGenArray(tinst->tick.y_l_irregular_points);
	NhlFreeGenArray(tinst->tick.y_l_values);

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
	NhlFree(tinst->tick.y_r_major_ndc_locs);
	NhlFree(tinst->tick.y_r_major_data_locs);
	NhlFree(tinst->tick.y_r_minor_data_locs);
	NhlFree(tinst->tick.y_r_minor_ndc_locs);
	
	return(NhlNOERROR);
}

/*
 * Function:	TickMarkClassInitialize
 *
 * Description: Just calls NrmStringToQuark to assure that the two new
 *		types NhlTTickMarkModes and NhlTTickMarkStyles are registered
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
#if	__STDC__
(void)
#else 
()
#endif
{
	NhlConvertArg	tmarkmodes[] = {
				{NhlSTRENUM,	NhlAUTOMATIC,	"automatic"},
				{NhlSTRENUM,	NhlMANUAL,	"manual"},
				{NhlSTRENUM,	NhlEXPLICIT,	"explicit"}
				};

	NhlConvertArg	tmarkstyles[] = {
				{NhlSTRENUM,	NhlLOG,		"log"},
				{NhlSTRENUM,	NhlLINEAR,	"linear"},
				{NhlSTRENUM,	NhlIRREGULAR,	"irregular"},
				{NhlSTRENUM,	NhlTIME,	"time"},
				{NhlSTRENUM,	NhlGEOGRAPHIC,	"geographic"}
				};

	_NhlInitializeLayerClass(NhlmultiTextLayerClass);

	NhlRegisterConverter(NhlTString,NhlTTickMarkModes,NhlCvtStringToEnum,
				tmarkmodes,NhlNumber(tmarkmodes),False,NULL);
	NhlRegisterConverter(NhlTString,NhlTTickMarkStyles,
					NhlCvtStringToEnum,tmarkstyles,
					NhlNumber(tmarkstyles),False,NULL);

	Qfloat = NrmStringToQuark(NhlTFloat);
	Qstring = NrmStringToQuark(NhlTString);

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
#if  __STDC__
(NhlLayer layer)
#else
(layer)
	NhlLayer layer;
#endif
{
	NhlTickMarkLayer tlayer = (NhlTickMarkLayer) layer;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes realret = NhlNOERROR;

	ret = DrawLabels(tlayer);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"TickMarkDraw: NhlFATAL error has occurred while drawing TickMark labels, can not continue");
		return(ret);
	}
	if(ret < realret)
		realret = ret;

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
#if __STDC__
(NhlTickMarkLayer tlayer)
#else
(tlayer)
NhlTickMarkLayer tlayer;
#endif
{
	float xr,xl,yt,yb;
	int i;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes ret1 = NhlNOERROR;

	ret = _NhlActivateWorkstation(tlayer->base.wkptr);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DrawGrid: An error has occured while activating the workstation, can not continue");
		return(ret);
	}
	xr = tlayer->view.x + tlayer->view.width;
	xl = tlayer->view.x;
	yt = tlayer->view.y;
	yb = tlayer->view.y - tlayer->view.height;

	c_set(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1);

	if(tlayer->tick.x_major_grid){
		if(tlayer->tick.x_b_on) {
			gset_line_colr_ind((Gint) _NhlGetGksCi(
				tlayer->base.wkptr, 
				tlayer->tick.x_major_grid_line_color));
			gset_linewidth((Gdouble)
				tlayer->tick.x_major_grid_thickness);
			for(i = 0; i < tlayer->tick.x_b_nmajor; i++) {
				c_line(tlayer->tick.x_b_major_ndc_locs[i],
					yb,
					tlayer->tick.x_b_major_ndc_locs[i],
					yt);
			}
		} else if(tlayer->tick.x_t_on) {
			gset_line_colr_ind((Gint) _NhlGetGksCi(
				tlayer->base.wkptr, 
				tlayer->tick.x_major_grid_line_color));
			gset_linewidth((Gdouble)
				tlayer->tick.x_major_grid_thickness);
			for(i = 0; i < tlayer->tick.x_t_nmajor; i++) {
				c_line(tlayer->tick.x_t_major_ndc_locs[i],
					yb,
					tlayer->tick.x_t_major_ndc_locs[i],
					yt);
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DrawGrid: Either XBOn or XTOn must be set in order to draw an XAxis grid");
			ret1 = NhlWARNING;
		}
		c_sflush();
	}
	if(tlayer->tick.x_minor_grid) {
		if((tlayer->tick.x_b_on)&&(tlayer->tick.x_b_minor_on)) {
			gset_line_colr_ind((Gint)_NhlGetGksCi(
                                tlayer->base.wkptr,
                                tlayer->tick.x_minor_grid_line_color));
                        gset_linewidth((Gdouble)
                                tlayer->tick.x_minor_grid_thickness);
			for(i=0; i<tlayer->tick.x_b_nminor; i++) {
				c_line(tlayer->tick.x_b_minor_ndc_locs[i],	
					yb,
					tlayer->tick.x_b_minor_ndc_locs[i],
					yt);
			}
		} else if((tlayer->tick.x_t_on)&&(tlayer->tick.x_t_minor_on)){
			gset_line_colr_ind((Gint)_NhlGetGksCi(
                                tlayer->base.wkptr,
                                tlayer->tick.x_minor_grid_line_color));
                        gset_linewidth((Gdouble)
                                tlayer->tick.x_minor_grid_thickness);
			for(i=0; i<tlayer->tick.x_b_nminor; i++) {
				c_line(tlayer->tick.x_b_minor_ndc_locs[i],	
					yb,
					tlayer->tick.x_b_minor_ndc_locs[i],
					yt);
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DrawGrid: Either XBOn or XTOn must be set in order to draw an XAxis minor grid");
			ret1 = NhlWARNING;
		}
		c_sflush();
	}
	if(tlayer->tick.y_major_grid){
		if(tlayer->tick.y_l_on) {
			gset_line_colr_ind((Gint) _NhlGetGksCi(
				tlayer->base.wkptr, 
				tlayer->tick.y_major_grid_line_color));
			gset_linewidth((Gdouble)
				tlayer->tick.y_major_grid_thickness);
			for(i = 0; i < tlayer->tick.y_l_nmajor; i++) {
				c_line(xl,
					tlayer->tick.y_l_major_ndc_locs[i],
					xr,
					tlayer->tick.y_l_major_ndc_locs[i]);
			}
		} else if(tlayer->tick.y_r_on) {
			gset_line_colr_ind((Gint) _NhlGetGksCi(
				tlayer->base.wkptr, 
				tlayer->tick.y_major_grid_line_color));
			gset_linewidth((Gdouble)
				tlayer->tick.y_major_grid_thickness);
			for(i = 0; i < tlayer->tick.y_r_nmajor; i++) {
				c_line(xl,
					tlayer->tick.y_r_major_ndc_locs[i],
					xr,
					tlayer->tick.y_r_major_ndc_locs[i]);
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DrawGrid: Either XBOn or XTOn must be set in order to draw an XAxis grid");
			ret1 = NhlWARNING;
		}
		c_sflush();
	}
	if(tlayer->tick.y_minor_grid) {
		if((tlayer->tick.y_l_on)&&(tlayer->tick.y_l_minor_on)) {
			gset_line_colr_ind((Gint)_NhlGetGksCi(
                                tlayer->base.wkptr,
                                tlayer->tick.y_minor_grid_line_color));
                        gset_linewidth((Gdouble)
                                tlayer->tick.y_minor_grid_thickness);
			for(i=0; i<tlayer->tick.y_l_nminor; i++) {
				c_line(xl,
					tlayer->tick.y_l_minor_ndc_locs[i],
					xr,
					tlayer->tick.y_l_minor_ndc_locs[i]);
			}
		} else if((tlayer->tick.y_r_on)&&(tlayer->tick.y_r_minor_on)){
			gset_line_colr_ind((Gint)_NhlGetGksCi(
                                tlayer->base.wkptr,
                                tlayer->tick.y_minor_grid_line_color));
                        gset_linewidth((Gdouble)
                                tlayer->tick.y_minor_grid_thickness);
			for(i=0; i<tlayer->tick.y_r_nminor; i++) {
				c_line(xl,
					tlayer->tick.y_r_minor_ndc_locs[i],
					xr,
					tlayer->tick.y_r_minor_ndc_locs[i]);
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DrawGrid: Either XBOn or XTOn must be set in order to draw an XAxis minor grid");
			ret1 = NhlWARNING;
		}
		c_sflush();
	}
	ret = _NhlDeactivateWorkstation(tlayer->base.wkptr);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DrawGrid: An error has occured while deactivating the workstation, can not continue");
	}
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
#if __STDC__
(NhlTickMarkLayer tlayer)
#else
(tlayer)
NhlTickMarkLayer tlayer;
#endif
{
	float xr,xl,yt,yb;
	int i;
	NhlErrorTypes ret = NhlNOERROR;

	ret = _NhlActivateWorkstation(tlayer->base.wkptr);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DrawTicks: An error occurred while activating the workstation, can not continue");
		return(ret);
	}
/*
* NEED SOME WAY TO KNOW WHEN A GKS ERROR HAS OCCURED
*/		
	xr = tlayer->view.x + tlayer->view.width;
	xl = tlayer->view.x;
	yt = tlayer->view.y;
	yb = tlayer->view.y - tlayer->view.height;

	c_set(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1);
	
	if(tlayer->tick.x_b_on) {
		gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,
                        tlayer->tick.x_b_major_line_color));
		gset_linewidth((Gdouble)tlayer->tick.x_b_major_thickness);
		for(i = 0; i< tlayer->tick.x_b_nmajor; i++) {
			c_line(tlayer->tick.x_b_major_ndc_locs[i],
				yb - tlayer->tick.x_b_major_outward_length,
				tlayer->tick.x_b_major_ndc_locs[i],
				(yb+tlayer->tick.x_b_major_length) 
				- tlayer->tick.x_b_major_outward_length);
		}
		c_sflush();
		if(tlayer->tick.x_b_minor_on) {
			gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr
				, tlayer->tick.x_b_minor_line_color));
			gset_linewidth((Gdouble)
				tlayer->tick.x_b_minor_thickness);
			for(i = 0; i < tlayer->tick.x_b_nminor; i++){
			c_line(tlayer->tick.x_b_minor_ndc_locs[i],
				yb - tlayer->tick.x_b_minor_outward_length,
				tlayer->tick.x_b_minor_ndc_locs[i],
				(yb + tlayer->tick.x_b_minor_length)
				- tlayer->tick.x_b_minor_outward_length);
			}
		c_sflush();
		}
	}
	if(tlayer->tick.x_t_on) {
		gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,
                        tlayer->tick.x_t_major_line_color));
		gset_linewidth((Gdouble)tlayer->tick.x_t_major_thickness);
		for(i = 0; i< tlayer->tick.x_t_nmajor; i++) {
			c_line(tlayer->tick.x_t_major_ndc_locs[i],
				yt + tlayer->tick.x_t_major_outward_length,
				tlayer->tick.x_t_major_ndc_locs[i],
				(yt-tlayer->tick.x_t_major_length) 
				+ tlayer->tick.x_t_major_outward_length);
		}
		c_sflush();
		if(tlayer->tick.x_t_minor_on) {
			gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr
				, tlayer->tick.x_t_minor_line_color));
			gset_linewidth((Gdouble)
				tlayer->tick.x_t_minor_thickness);
			for(i = 0; i < tlayer->tick.x_t_nminor; i++){
			c_line(tlayer->tick.x_t_minor_ndc_locs[i],
				yt + tlayer->tick.x_t_minor_outward_length,
				tlayer->tick.x_t_minor_ndc_locs[i],
				(yt - tlayer->tick.x_t_minor_length)
				+ tlayer->tick.x_t_minor_outward_length);
			}
		c_sflush();
		}
	}
	if(tlayer->tick.y_l_on) {
		gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,
                        tlayer->tick.y_l_major_line_color));
		gset_linewidth((Gdouble)tlayer->tick.y_l_major_thickness);
		for(i=0; i < tlayer->tick.y_l_nmajor; i++) {
			c_line(xl - tlayer->tick.y_l_major_outward_length,
				tlayer->tick.y_l_major_ndc_locs[i],
				xl + tlayer->tick.y_l_major_length 
				- tlayer->tick.y_l_major_outward_length,
				tlayer->tick.y_l_major_ndc_locs[i]);
		}
		c_sflush();
		if(tlayer->tick.y_l_minor_on) {
			gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr
				,tlayer->tick.y_l_minor_line_color));
			gset_linewidth((Gdouble)
					tlayer->tick.y_l_minor_thickness);
			for(i = 0; i < tlayer->tick.y_l_nminor; i++){
			c_line( xl - tlayer->tick.y_l_minor_outward_length,
				tlayer->tick.y_l_minor_ndc_locs[i],
				(xl + tlayer->tick.y_l_minor_length)
				- tlayer->tick.y_l_minor_outward_length,
				tlayer->tick.y_l_minor_ndc_locs[i]);
			}
		c_sflush();
		}
	}
	if(tlayer->tick.y_r_on) {
		gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,
                        tlayer->tick.y_r_major_line_color));
		gset_linewidth((Gdouble)tlayer->tick.y_r_major_thickness);
		for(i=0; i < tlayer->tick.y_r_nmajor; i++) {
			c_line(xr + tlayer->tick.y_r_major_outward_length,
				tlayer->tick.y_r_major_ndc_locs[i],
				xr - tlayer->tick.y_r_major_length 
				+ tlayer->tick.y_r_major_outward_length,
				tlayer->tick.y_r_major_ndc_locs[i]);
		}
		c_sflush();
		if(tlayer->tick.y_r_minor_on) {
			gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr
				, tlayer->tick.y_r_minor_line_color));
			gset_linewidth((Gdouble)
				tlayer->tick.y_r_minor_thickness);
			for(i = 0; i < tlayer->tick.y_r_nminor; i++){
			c_line( xr + tlayer->tick.y_r_minor_outward_length,
				tlayer->tick.y_r_minor_ndc_locs[i],
				(xr - tlayer->tick.y_r_minor_length)
				+ tlayer->tick.y_r_minor_outward_length,
				tlayer->tick.y_r_minor_ndc_locs[i]);
			}
		c_sflush();
		}
	}
	
	ret = _NhlDeactivateWorkstation(tlayer->base.wkptr);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DrawTicks: An error occurred while deactivating the workstation, can not continue");
	}
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
#if __STDC__
(NhlTickMarkLayer tlayer)
#else
(tlayer)
	NhlTickMarkLayer tlayer;
#endif
{
	int n;
	NhlErrorTypes ret = NhlNOERROR;

	ret = _NhlActivateWorkstation(tlayer->base.wkptr);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DrawBorder: An error occured while activating the workstation, can not continue");
		return(ret);
	}
	gset_linewidth((Gdouble)tlayer->tick.border_thickness);
	gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,
                       tlayer->tick.border_line_color));

	c_set(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1);
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
	c_sflush();
	ret = _NhlDeactivateWorkstation(tlayer->base.wkptr);
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DrawBorder: An error occured while deactivating the workstation, can not continue");
	}
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
#if __STDC__
(NhlTickMarkLayer tlayer)
#else
(tlayer)
NhlTickMarkLayer tlayer;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes realret = NhlNOERROR;

	if(tlayer->tick.x_b_labels_on) {
		ret = NhlDraw(tlayer->tick.xb_multi->base.id);
		if(ret < NhlWARNING) 
			return(ret);
	}
	if(ret < realret)
		realret = ret;
	if(tlayer->tick.x_t_labels_on) {
		ret = NhlDraw(tlayer->tick.xt_multi->base.id);
		if(ret < NhlWARNING)
			return(ret);
	}
	if(ret < realret)
		realret = ret;
	if(tlayer->tick.y_l_labels_on) {
		ret = NhlDraw(tlayer->tick.yl_multi->base.id);
		if(ret < NhlWARNING)
			return(ret);
	}
	if(ret < realret)
		realret = ret;
	if(tlayer->tick.y_r_labels_on) {
		ret = NhlDraw(tlayer->tick.yr_multi->base.id);
		if(ret < NhlWARNING)
			return(ret);
	}
	if(ret < realret)
		realret = ret;
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
#if	__STDC__
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlTickMarkLayer tinstance = (NhlTickMarkLayer) instance;

	if((tinstance->tick.xb_multi != NULL)&&(tinstance->tick.x_b_nmajor > 0))
	_NhlGetBB(tinstance->tick.xb_multi,thebox);
	if((tinstance->tick.xt_multi != NULL)&&(tinstance->tick.x_t_nmajor > 0))
	_NhlGetBB(tinstance->tick.xt_multi,thebox);
	if((tinstance->tick.yl_multi != NULL)&&(tinstance->tick.y_l_nmajor > 0))
	_NhlGetBB(tinstance->tick.yl_multi,thebox);
	if((tinstance->tick.yr_multi != NULL)&&(tinstance->tick.y_r_nmajor > 0))
	_NhlGetBB(tinstance->tick.yr_multi,thebox);
	
	return(NhlNOERROR);
}

static float roundit(
#if	NhlNeedProto
	float a,
	int sig_digit
#endif
);

/*
 * Function:	compare
 *
 * Description: Compare two floating point numbers to a certain number of
 *		significant digits. This was a tricky algorythm to come up
 *		with. Essentially what compare does is figure out what it
 *		takes to shift one of the numbers to the immediate left of
 *		the decimal place. It divides both numbers by this number.
 *		Then it multiplies both numbers by 10**sig_digit, rounds them
 *		then casts the resulting number into an integer. These two
 *		integers are subtracted from each other and the result returned
 *		
 *
 * In Args:	a	first floating point number
 *		b	second floating point number
 *		sig_dig	<=7 represents number of significant digits to compare.
 *
 * Out Args:	NONE
 *
 * Return Values: 0 if equal, <0 if a<b, and >0 if a>b
 *
 * Side Effects: NONE
 */
static float	compare
#if	__STDC__
(float a, float b, int sig_dig)
#else
(a,b,sig_dig)
	float a;
	float b;
	int sig_dig;
#endif
{
	float	a_final;
	float	b_final;
	long a_int;
	long b_int;
	int exp;
	int signa;
	int signb;
	float tmp;
	
/*
* If sig_dig is > 6, a_int and b_int will overflow and cause problems
*/
	if(sig_dig > 7) 
		sig_dig = 7;

/*
* Get ride of easy cases:
* These actually didn't end up being easy since large numbers compared againts
* zero cause a_int and b_int to overflow. So I added the fabs checks to make
* sure that the absolute value of non-zero numbers are at least between 
* 0 and 1.0.
*/
	if((a == 0.0)&&(b!=0.0)&&(log10(fabs(b))<=0.0)) {
		a_int = 0;
		b_final = b * (float)pow(10.0,(double)sig_dig);
		b_int = (long)b_final;
		return((float)(a_int - b_int));
	} else if((a!=0.0)&&(b==0.0)&&(log10(fabs(a))<=0.0)){
		b_int = 0;
		a_final = a * (float)pow(10.0,(double)sig_dig);
		a_int = (long)a_final;
		return((float)(a_int - b_int));
	} else if((a==0.0)&&(b==0.0)){
		return(0.0);
	}
/*
* If I get here and either a or b is zero then than means one of them is
* greater that 1 and one is 0.0
*/
	if((a==0.0)||(b==0.0)) {
		return(a - b);
	}

	
/*
* store sign info and make sure both numbers are positive so log10 can be
* used. 
*/
	signa = ((float)fabs(a))/a;
	signb = ((float)fabs(b))/b;
	a_final = fabs(a);
	b_final = fabs(b);
/*
* Now compute the exponent needed to shift a to the decimal position immediately
* right of the decimal point for the value of a
*/
	if(a_final>b_final){ 
		tmp = (float)log10(a_final);
		exp = (long)ceil(log10(a_final));
		if((float)exp == tmp)
			exp++;
	} else {
		tmp = (float)log10(b_final);
		exp = (long)ceil(log10(b_final));
		if((float)exp == tmp)
			exp++;
	}

/*
* Now divide through by the exponent determined above
*/
	a_final = a_final/(float)pow(10.0,(double)exp);
	b_final = b_final/(float)pow(10.0,(double)exp);

/*
* Since a and possibly b are now shifted to the immediate left of the decimal,
* multipling by pow(10.0,sig_dig), rounding , setting appropriate sign  and 
* truncating the decimal produces two integers that can be compared.
*/
	a_final = a_final * pow(10.0,(double)sig_dig);
	b_final = b_final * pow(10.0,(double)sig_dig);
	a_final = roundit(a_final,sig_dig);
	b_final = roundit(b_final,sig_dig);
	a_final *= signa;
	b_final *= signb;
	a_int = (long)a_final;
	b_int = (long)b_final;
	return((float)a_int-(float)b_int);
}


/*
 * Function:	roundit
 *
 * Description: Used to round a floating point number to a certain amount of
 *		significant digits. First it converts number to [0.0,1.0]
 *		exclusive and removes the sign. Then it multiplies by 
 * 		10**sig_digit. Then it adds .5 and casts the number to a long, 
 *		(i.e truncates decimal. Next, it converts the number back
 *		into its original magnitude and sign and returns. 
 *
 * In Args:	a	floating point values
 *		sig_dig	number of digit to round to must be <=7
 *
 * Out Args:	NONE
 *
 * Return Values: rounded value
 *
 * Side Effects: NONE
 */
static float roundit
#if	__STDC__
(float a,int sig_digit)
#else
(a,sig_digit)
float a;
int sig_digit;
#endif
{
	double	exp1;
	double	exp2;
	double 	a_final;
	long  ltmp;
	int sign;

/* 
* if its equal to zero just return
*/
	if(a == 0.0)
		return(a);
/*
* floats are only accurate to 7 decimal places when 32bit IEEE used
*/
	if(sig_digit>7)
		sig_digit = 7;
/*
* need to convert <a> so its not a negative, so logs can be taken
*/
	a_final = fabs(a);
	sign = (int)(a_final/a);
	

/*
* Converts number to value between 0.0 and 1.0
*/
	exp1 = (float)ceil(log10(a_final));
	a_final = a_final/pow(10.0,exp1);

/*
* Now perform significant digit computation
*/
	exp2 = pow(10.0,(double)sig_digit);
	ltmp = (long)(a_final * exp2 + .5);
	a_final = ((double)ltmp)/exp2;

/*
* Now convert back to original magnitude
*/

	a_final = (double)sign * a_final * pow(10.0,exp1);

	return((float)a_final);
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
#if   __STDC__ 
(
	NhlTickMarkStyles  style,
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
	int cutoff
)
#else
(style,array,larray,max_ticks,dmax,dmin,tstart,tend,convert_precision,spacing,nmajor,cutoff)
NhlTickMarkStyles  style;
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
#endif
{
	int done = 0,i = 0,j = 0;
	float tmploc;
	int compare_precision;
	int cnvt_precision;
	int log_spacing;
	NhlErrorTypes ret = NhlNOERROR;
	float min_compare,max_compare,min,max;

	switch(style) {
	case NhlLINEAR:	
	case NhlIRREGULAR:
/*
* Can't rember why this is necessary! In fact it seems to cause problems
		*tstart = roundit(dmin,convert_precision);
		*tend = roundit(dmax,convert_precision);
*/
		*tstart = dmin;
		*tend = dmax;
		ret = ChooseSpacingLin(tstart,tend,spacing,7,max_ticks);
		if(ret<NhlWARNING) {
			return(NhlFATAL);
		}
		min = MAX(*tstart,dmin);
		max = MIN(*tend,dmax);

		if(min != 0.0)
			min_compare = ceil(fabs(log10((double)((*spacing)/fabs(min)))))+1.0;
		else 
			min_compare = 7.0;

		if(max != 0.0)
			max_compare = ceil(fabs(log10((double)((*spacing)/fabs(max)))))+1.0;
		else
			max_compare = 7.0;

		
		if((min != 0.0)&&(max != 0.0)){
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,"AutoComputeMajorTickMarks: min and max are so close together or so far appart that arithmetic error may cause problems, proceed at own risk");
			ret = NhlWARNING;
		}
		if((cnvt_precision < max_compare)&&(cnvt_precision < min_compare)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"AutoComputeMajorTickMarks: The precision specified is smaller than the precision needed tick mark labels may not be correct");
			ret = NhlWARNING;
 
		}

		while(!done) {
			tmploc = *tstart + i * *spacing;
			if((compare(tmploc,min,7/*min_compare*/)>=0.0)
				&&(compare(tmploc,max,7/*max_compare*/) <= 0.0)) {
				array[j] = tmploc;
				larray[j] = ConvertToString(tmploc,cnvt_precision,compare_precision,NhlLINEAR,cutoff);
				j++;
				if(j == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"AutoComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			} else if(compare(tmploc,max,7/*max_compare*/) >0.0) {
				done = 1;
			} 
			i++;
		}
		*nmajor = j;
		break;
	case NhlLOG:
		if(convert_precision > 0) {
			*tstart = roundit(dmin,convert_precision);
			*tend = roundit(dmax,convert_precision);
		} else {
			*tstart = dmin;
			*tend = dmax;
		} 
		ret = ChooseSpacingLog(tstart,tend,spacing,5,max_ticks);
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
		log_spacing = (int)roundit(*spacing,2);
		while(!done) {
			tmploc = *tstart + i;
			if((compare(tmploc,log10(dmin),compare_precision)>=0.0)
				&&(compare(tmploc,log10(dmax),compare_precision) <= 0.0)) {
				array[j] = pow(10.0,tmploc);
				if(i%log_spacing == 0) {
					larray[j] = ConvertToString(array[j],cnvt_precision,compare_precision,NhlLOG,cutoff);
				} else {
					larray[j] = NULL;
				}
				j++;
				if(j == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"AutoComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			} else if(compare(tmploc,log10(dmax),compare_precision) >0.0) {
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
#if   __STDC__
(
	NhlTickMarkStyles style,
	float *array,
	char** larray,
	float dmax,
	float dmin,
	float tstart,
	float tend,
	int convert_precision,
	float spacing,
	int spacing_type,
	int *nmajor,
	int cutoff
)
#else
(style,array,larray,dmax,dmin,tstart,tend,convert_precision,spacing,spacing_type,nmajor,cutoff)
NhlTickMarkStyles  style;
float   *       array;
char	** 	larray;
float           dmax;
float           dmin;
float           tstart;
float           tend;
int		convert_precision;
float           spacing;
int             spacing_type;
int     *       nmajor;
int		cutoff;
#endif
{
	int done = 0,i=0,j=0;
	float min,max,tmploc;
	float min_compare,max_compare;
	int compare_precision;
	int cnvt_precision;
	int log_spacing;
	NhlErrorTypes ret = NhlNOERROR;


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
		log_spacing = (int) roundit(spacing,2);
		
		while(!done) {
			tmploc = tstart + i;
			if((compare(tmploc,min,compare_precision)>=0.0)
				&&(compare(tmploc,max,compare_precision) <= 0.0)) {
				array[j] = pow(10.0,tmploc);
				if(i%log_spacing == 0) {
					larray[j] = ConvertToString(array[j],cnvt_precision,compare_precision,NhlLOG,cutoff);
				} else {	
					larray[j] = NULL;
				}
				j++;
				if(j == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			} else if(compare(tmploc,max,compare_precision) >0.0) {
				done = 1;
			} 
			i++;
		}
		*nmajor = j;
		break;
	case NhlLINEAR:
	case NhlIRREGULAR:
		if(spacing <= 0.0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"ManualComputeTickMarks: Spacing is less than or equal to zero, this should not happen, can not continue");
			return(NhlFATAL);
		}
		if(min!= 0.0)
			min_compare = ceil(fabs(log10((double)(spacing/fabs(min)))))+1.0;
		else 
			min_compare = 7.0;

		if(max!= 0.0)
			max_compare = ceil(fabs(log10((double)(spacing/fabs(max)))))+1.0;
		else 
			max_compare = 7.0;

		if((max != 0.0)&&(min != 0.0)) {
			compare_precision = MAX(min_compare,max_compare);
		} else {
			compare_precision = MIN(min_compare,max_compare);
		}
		if(convert_precision !=-1) {
			cnvt_precision = convert_precision;
		} else {
			cnvt_precision = compare_precision;
		}
		if((min_compare > 7)&&(max_compare > 7)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajorTickMarks: min and max are so close together or so far appart that arithmetic error may cause problems, proceed at own risk");
			ret = NhlWARNING;
		}
		if((cnvt_precision < max_compare)&&(cnvt_precision < min_compare)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajoreTickMarks: The precision specified is smaller than the precision needed tick mark labels may not be correct");
			ret = NhlWARNING;
 
		}
		
		
		while(!done) {
			tmploc = tstart + i * spacing;
			if((compare(tmploc,min,7/*min_compare*/)>=0.0)
				&&(compare(tmploc,max,7/*max_compare*/)) <= 0.0) {
				array[j] = tmploc;
				larray[j] = ConvertToString(array[j],cnvt_precision,compare_precision,NhlLINEAR,cutoff);
				j++;
				if(j == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			} else if(compare(tmploc,max,7 /*max_compare*/) >0.0) {
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
#if   __STDC__
(NhlTickMarkStyles style,float *array,char** larray,float dmax,float dmin,float tstart,float tend,int convert_precision,float *requested_points,char** requested_labels,int *nmajor,int n_requested)
#else
(style,array,larray,dmax,dmin,tstart,tend,convert_precision,requested_points,requested_labels,nmajor,n_requested)
NhlTickMarkStyles  style;
float   *       array;
char	**	larray;
float           dmax;
float           dmin;
float           tstart;
float           tend;
int		convert_precision;
float   *       requested_points;
char	**      requested_labels;
int     *       nmajor;
int n_requested;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	float min,max;
	int i,k;
#ifdef	NOTUSED
	int	spacing_estimate,min_compare;
#endif


	switch(style) {
	case NhlLOG:
	case NhlLINEAR:
	case NhlIRREGULAR:
		max = MIN(dmax,tend);
		min = MAX(dmin,tstart);
#ifdef	NOTUSED
		spacing_estimate = (max - min)/n_requested;
		if(min != 0.0)	
			min_compare = ceil(fabs(log10((double)(spacing_estimate/fabs(min)))))+1.0;
		else 
			min_compare = 7;

		if(max != 0.0)	
			max_compare = ceil(fabs(log10((double)(spacing_estimate/fabs(max)))))+1.0;
		else 
			max_compare = 7;
#endif
		k = 0;
		for(i = 0; i< n_requested; i++) {
			if((compare(requested_points[i],min,7/*min_compare*/) >= 0.0) && (compare(requested_points[i],max,7 /*max_compare*/) <= 0.0)) {
				array[k] = requested_points[i];
				if(requested_labels != NULL)
					larray[k] = requested_labels[i];
				k++;
				if(k == MAXTICKS) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"ManualComputeMajorTickMarks: Maximum tickmarks (%d) has been reached, tickmarks may appear in complete",MAXTICKS);
					ret = NhlWARNING;
					break;	
				}
			}
			
		}
		*nmajor = k;
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
#if	__STDC__
(float *tstart,float *tend,float *spacing,int convert_precision,int max_ticks)
#else
(tstart,tend,spacing,convert_precision,max_ticks)
float *tstart;
float *tend;
float *spacing;
int	convert_precision;
int	max_ticks;
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
	table[6] = 20.0;
	table[7] = 30.0;
	table[8] = 40.0;
	table[9] = 50.0;
	npts = 10;

	if(compare(*tend,*tstart,8)<=0.0) {
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
			&&(compare((ax1-am1)/t,(double)max_ticks,convert_precision) <= 0.0))){
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
#if	__STDC__
(float *tstart,float *tend,float *spacing,int convert_precision,int max_ticks)
#else
(tstart,tend,spacing,convert_precision,max_ticks)
float *tstart;
float *tend;
float *spacing;
int	convert_precision;
int	max_ticks;
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

	if((*tstart <= 0.0)||(*tend <= 0.0)||((*tend-*tstart)<=0.0)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ChooseSpacingLog: An internal error that should not have occured has been detected, can not continue");
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
			&&(compare((ax1-am1)/t,(double)max_ticks,convert_precision) <= 0.0))){
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
#if __STDC__
(
	float		value,
	int		convert_precision,
	int		compare_precision,
	NhlTickMarkStyles	style,
	int		cutoff
)
#else
(value,convert_precision,compare_precision,style,cutoff)
float	value;
int	convert_precision;
int	compare_precision;
NhlTickMarkStyles	style;
int	cutoff;
#endif
{
	float tmp;
	float tmp2;
	long exp;
	char *tmpc;
	char buffer[_NhlMAXFNAMELEN];
	char buffer2[_NhlMAXFNAMELEN];
	char format[_NhlMAXFNAMELEN];
	int sign = 1;

	tmp = roundit(value,convert_precision);
	if(tmp != 0.0)
		sign = (long)(fabs(tmp)/tmp);
	tmp = fabs(tmp);


	switch(style) {	
	case NhlLOG:
		sprintf(buffer,"10:S:");
		sprintf(&(buffer[strlen(buffer)]),"%d:N:",(long)(float)log10((double)tmp));
		tmpc = (char*)NhlMalloc((unsigned)strlen(buffer)+1);
		strcpy(tmpc,buffer);
/*
* No Sign needed so just return
*/
		return(tmpc);
	case NhlLINEAR:
	case NhlIRREGULAR:
		if(compare(0.0,tmp,compare_precision)!= 0.0) {
			if(fabs(log10(tmp)) > cutoff) {
/*
* Scientific notation must be used
*/
				tmp2 = log10(tmp);
				exp = (long)ceil(tmp2);
				if((float)exp == tmp2)
					exp++;
				tmp = tmp * (pow(10.0,(double)(1 - exp)));
				sprintf(format,"%%.%df",convert_precision - 1);
				sprintf(buffer,format,tmp);
				if(sign < 0.0) { 
					sprintf(buffer2,"-"); 
					strcat(buffer2,buffer);
				} else {
					strcpy(buffer2,buffer);
				}
				sprintf(buffer,"x10:S:%d:N",-(1 - exp));
				strcat(buffer2,buffer);
			} else {
/*
* Normal numeric representation will be used
*/
				tmp2 = log10(tmp);
				exp = (long)ceil(tmp2);
				if((float)exp == tmp2)
					exp++;
				if(convert_precision - exp > 0.0) {
					sprintf(format,"%%.%df",convert_precision - exp);
					sprintf(buffer,format,tmp);
				} else {
					sprintf(buffer,"%d",(long)tmp);
				}
				if(sign < 0.0) {
       	        			sprintf(buffer2,"-");
       	        	               	strcat(buffer2,buffer);
               	         	} else {
               	                	strcpy(buffer2,buffer);
				}
			}
		} else {
			sprintf(buffer2,"0.0");
		}
		tmpc = (char*) NhlMalloc((unsigned)strlen(buffer2)+1);
		strcpy(tmpc,buffer2);
		return(tmpc);
	case NhlTIME:
		break;
	case NhlGEOGRAPHIC:
		break;
	}
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
 *		
 *
 * In Args:
 *		int minorpermajor,	number of minor ticks per major ticks
 *		float spacing,		major tick mark spacing
 *		float tstart,		tick mark starting
 *		float tend,		tick mark ending
 *		float dmax,		maximum data value
 *		float dmin,		minimum data value
 *		float* m_locs,		major tick mark locations
 *		int nmajor,		total numberr of major tick marks
 *		int precision,		precision used to compare two nums
 *		NhlTickMarkStyles style	style of tick marks
 *
 * Out Args:	minor_locs		locations of minor tick marks
 *		nminor		total number of minor tick marks.
 *
 * Return Values: Error Conditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes ComputeMinorTickMarks
#if __STDC__
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
	NhlTickMarkStyles style
)
#else
(minorpermajor,spacing,tstart,tend,dmax,dmin,m_locs,nmajor,minor_locs, nminor,precision,style)
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
	NhlTickMarkStyles	style;
#endif
{
	float minor_spacing,tmp;
	int i,j,k;
	int compare_precision = precision;
	float min,max,min2,min_compare,max_compare;
	float logminor;
	NhlErrorTypes ret = NhlNOERROR;


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
					while(compare(tmp,m_locs[i-1],precision) > 0.0){
						minor_locs[k] = tmp;
						k++;
						j++;
						tmp = m_locs[i] - j 
						      *pow(10.0,minor_spacing);
					}
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
				if(compare(pow(10.0,max),m_locs[nmajor-1],
					compare_precision) > 0.0) {
					minor_spacing = floor(log10(
						m_locs[nmajor-1]))  
						+ logminor;
					j = 1;
					tmp = pow(10.0,floor(log10(
						m_locs[nmajor-1])) + 1.0) - j * 
						pow(10.0,minor_spacing);
					while(compare(tmp,m_locs[nmajor-1],
						precision) > 0.0) {
						if(compare(tmp,pow(10.0,max),
							precision) < 0.0) {
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
							NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
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
				while(compare(tmp,m_locs[0],precision) > 0.0){
					if(compare(tmp,pow(10.0,max),8) < 0.0) {
						minor_locs[k] = tmp;
						k++;
					}
					j++;
					tmp = pow(10.0,ceil(max)) - j 
					      *pow(10.0,minor_spacing);
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
				
			}
			if(compare(log10(m_locs[0]),min, 8) > 0.0) {
				minor_spacing = log10(m_locs[0]);
				minor_spacing = ceil(minor_spacing);
				minor_spacing -= 1.0;
				minor_spacing += logminor;
				j = 1;
				tmp = m_locs[0] - j * pow(10.0,minor_spacing);
                                while(compare(tmp,pow(10.0,min),precision) > 0.0){
                                        minor_locs[k] = tmp;
                                        k++;
                                        j++;
                                        tmp = m_locs[0] - j * pow(10.0,minor_spacing);
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
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
				while(compare(tmp,pow(10.0,min),precision) >0.0) {
					if(compare(tmp,pow(10.0,max),precision) < 0.0) {
						minor_locs[k] = tmp;
						k++;
					}
					j++;
					tmp = pow(10.0,ceil(max)) - j *pow(10.0,minor_spacing);
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
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
			minor_spacing = roundit(spacing / (float)
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
			if(compare(min,m_locs[0],7/*min_compare*/) < 0.0) {	
				j = 1;
				tmp = m_locs[0] - j * minor_spacing;
				while(compare(min,tmp,7/*min_compare*/)<=0.0) {
					minor_locs[k] = tmp;
					k++;
					j++;
					tmp = m_locs[0] - j *minor_spacing;
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
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
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
			}
			if(compare(max,m_locs[nmajor-1],7/*compare_precision*/) > 0.0) {
				j = 1;
				tmp = m_locs[nmajor-1] + j * minor_spacing;
				while(compare(max,tmp,7/*max_compare*/)>= 0.0){
					minor_locs[k] = tmp;
					k++;
					j++;
					tmp = m_locs[nmajor-1] + j * minor_spacing;
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
						return(NhlWARNING);
					}
				}
			}
			*nminor = k;
			} else {
				k = 0;
				i = 0;
				while( compare(i*spacing + tstart,max,7/*max_compare*/) < 0.0) {
					i++;
				}
				min2 = (i-1)*spacing + tstart;
				j = 1;
				tmp = min2 + j *minor_spacing;
				while(compare(max,tmp,7/*max_compare*/) >=0.0){
					if(compare(min,tmp,7/*min_compare*/) < 0.0) {
					minor_locs[k] = tmp;
					k++;
					}
					j++;
					tmp = min2 + j * minor_spacing;
					if(k == MAXMINORTICKS) {
						*nminor = k-1;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"ComputeMinorTickMarks: Maximimum minor tick mark limit of %d ticks reached, no more minor ticks allowed",MAXMINORTICKS);
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
 *		tmXTIrrTensionF
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
#if  __STDC__
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
	tnew->tick.x_t_tick_start = tnew->tick.x_b_tick_start;
	tnew->tick.x_t_tick_end = tnew->tick.x_b_tick_end;
	tnew->tick.x_t_max_ticks = tnew->tick.x_b_max_ticks;
	tnew->tick.x_t_tick_spacing = tnew->tick.x_b_tick_spacing;
	tnew->tick.x_t_spacing_type = tnew->tick.x_b_spacing_type;
	tnew->tick.x_t_values = tnew->tick.x_b_values;
	tnew->tick.x_t_labels = tnew->tick.x_b_labels;
	tnew->tick.x_t_major_thickness = tnew->tick.x_b_major_thickness;
	tnew->tick.x_t_major_line_color = tnew->tick.x_b_major_line_color;
	tnew->tick.x_t_major_length = tnew->tick.x_b_major_length;
	tnew->tick.x_t_major_outward_length = tnew->tick.x_b_major_outward_length;
	tnew->tick.x_t_minor_thickness = tnew->tick.x_b_minor_thickness;
	tnew->tick.x_t_minor_line_color = tnew->tick.x_b_minor_line_color;
	tnew->tick.x_t_minor_length = tnew->tick.x_b_minor_length;
	tnew->tick.x_t_minor_outward_length = tnew->tick.x_b_minor_outward_length;
	tnew->tick.x_t_label_font = tnew->tick.x_b_label_font;
	tnew->tick.x_t_label_font_height = tnew->tick.x_b_label_font_height;
	tnew->tick.x_t_label_font_color = tnew->tick.x_b_label_font_color;
       	tnew->tick.x_t_label_font_aspect = tnew->tick.x_b_label_font_aspect;
       	tnew->tick.x_t_label_angle = tnew->tick.x_b_label_angle;
	tnew->tick.x_t_label_direction = tnew->tick.x_b_label_direction;
	tnew->tick.x_t_label_delta = tnew->tick.x_b_label_delta;
	tnew->tick.x_t_precision = tnew->tick.x_b_precision;
	tnew->tick.x_t_auto_precision = tnew->tick.x_b_auto_precision;
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
 *		tmYRIrrTensionF
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
#if __STDC__
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
	tnew->tick.y_r_tick_start = tnew->tick.y_l_tick_start;
	tnew->tick.y_r_tick_end = tnew->tick.y_l_tick_end;
	tnew->tick.y_r_max_ticks = tnew->tick.y_l_max_ticks;
	tnew->tick.y_r_tick_spacing = tnew->tick.y_l_tick_spacing;
	tnew->tick.y_r_spacing_type = tnew->tick.y_l_spacing_type;
	tnew->tick.y_r_values = tnew->tick.y_l_values;
	tnew->tick.y_r_labels = tnew->tick.y_l_labels;
	tnew->tick.y_r_major_thickness = tnew->tick.y_l_major_thickness;tnew->tick.y_r_major_line_color = tnew->tick.y_l_major_line_color;
	tnew->tick.y_r_major_length = tnew->tick.y_l_major_length;
	tnew->tick.y_r_major_outward_length = tnew->tick.y_l_major_outward_length;
	tnew->tick.y_r_minor_thickness = tnew->tick.y_l_minor_thickness;
	tnew->tick.y_r_minor_line_color = tnew->tick.y_l_minor_line_color;
	tnew->tick.y_r_minor_length = tnew->tick.y_l_minor_length;
	tnew->tick.y_r_minor_outward_length = tnew->tick.y_l_minor_outward_length;
	tnew->tick.y_r_label_font = tnew->tick.y_l_label_font;
	tnew->tick.y_r_label_font_height = tnew->tick.y_l_label_font_height;
	tnew->tick.y_r_label_font_color = tnew->tick.y_l_label_font_color;
	tnew->tick.y_r_label_font_aspect = tnew->tick.y_l_label_font_aspect;
	tnew->tick.y_r_label_angle = tnew->tick.y_l_label_angle;
	tnew->tick.y_r_label_direction = tnew->tick.y_l_label_direction;
	tnew->tick.y_r_label_delta = tnew->tick.y_l_label_delta;
	tnew->tick.y_r_precision = tnew->tick.y_l_precision;
	tnew->tick.y_r_auto_precision = tnew->tick.y_l_auto_precision;
	tnew->tick.y_r_irregular_points = tnew->tick.y_l_irregular_points;
	tnew->tick.y_r_label_stride = tnew->tick.y_l_label_stride;
	tnew->tick.y_r_tension = tnew->tick.y_l_tension;
}

/*
 * Function:	CheckKeyVals
 *
 * Description: Makes sure data extents set and precision are correct.
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
#if __STDC__
(NhlTickMarkLayer tnew,NhlTickMarkLayer told, int c_or_s)
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
        if(tnew->tick.x_b_precision <= 0) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Precision resource for bottom ticks is less than or equal to zero, defaulting to 1",error_lead);
                tnew->tick.x_b_precision = 1;
		ret = NhlWARNING;
        }
        if(tnew->tick.x_t_precision <= 0) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Precision resource for top ticks is less than or equal to zero, defaulting to 1",error_lead);
                tnew->tick.x_t_precision = 1;
		ret = NhlWARNING;
        }

/*
* Tickmarks must be on and have left and right(top and bottom) data values for 
* all modes
*/
        if((tnew->tick.x_b_data_left == 0.0)&&
                (tnew->tick.x_b_data_right ==0.0)&&
                (tnew->tick.x_b_on)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Neither NhlXBDataLeftF nor NhlXBDataRightF are set and NhlXBOn is set, turning off bottom ticks",error_lead);
                tnew->tick.x_b_on = 0;
		ret = NhlWARNING;
        }
        if((tnew->tick.x_t_data_left == 0.0)&&
                (tnew->tick.x_t_data_right ==0.0)&&
                (tnew->tick.x_t_on)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Neither NhlXTDataLeftF nor NhlXTDataRightF are set and NhlXTOn is set, turning off Top ticks",error_lead);
		tnew->tick.x_t_on = 0;
		ret = NhlWARNING;
        }
        if(tnew->tick.y_l_precision <= 0) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Precision resource for left ticks is less than or equal to zero, defaulting to 1",error_lead);
                tnew->tick.y_l_precision = 1;
		ret = NhlWARNING;
        }
        if(tnew->tick.y_r_precision <= 0) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Precision resource for right ticks is less than or equal to zero, defaulting to 1",error_lead);
                tnew->tick.y_r_precision = 1;
		ret = NhlWARNING;
        }

/*
* Tickmarks must be on and have left and right(top and bottom) data values for 
* all modes
*/
        if((tnew->tick.y_l_data_bottom == 0.0)&&
                (tnew->tick.y_l_data_top ==0.0)&&
                (tnew->tick.y_l_on)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Neither NhlYLDataBottomF nor NhlYLDataTopF are set and NhlYLOn is set, turning off left ticks",error_lead);
                tnew->tick.y_l_on = 0;
		ret = NhlWARNING;
        }
        if((tnew->tick.y_r_data_bottom == 0.0)&&
                (tnew->tick.y_r_data_top ==0.0)&&
                (tnew->tick.y_r_on)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Neither NhlYRDataBottomF nor NhlYRDataTopF are set and NhlYROn is set, turning off right ticks",error_lead);
                tnew->tick.y_r_on = 0;
		ret = NhlWARNING;
        }
	if(!tnew->tick.x_b_minor_on) {
		tnew->tick.x_b_nminor = 0;
	}
	if(!tnew->tick.x_t_minor_on) {
		tnew->tick.x_t_nminor = 0;
	}
	if(!tnew->tick.y_l_minor_on) {
		tnew->tick.y_r_nminor = 0;
	}
	if(!tnew->tick.y_r_minor_on) {
		tnew->tick.y_l_nminor = 0;
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
#if	__STDC__
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

        if((tnew->tick.x_b_on)&&(tnew->tick.x_b_mode == NhlMANUAL)){
                if((tnew->tick.x_b_tick_start == 0.0)&&
                        (tnew->tick.x_b_tick_end == 0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: No tick start or end set for bottom ticks in manual mode, using values of DataLeft and DataRight",error_lead);
                        tnew->tick.x_b_tick_start = MIN(tnew->tick.x_b_data_left						,tnew->tick.x_b_data_right);
                        tnew->tick.x_b_tick_end = MAX(tnew->tick.x_b_data_left,
                                                tnew->tick.x_b_data_right);
			ret = NhlWARNING;
                }
                if(tnew->tick.x_b_tick_spacing == 0.0){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: A tick mark spacing must be assigned for manual mode, defaulting bottom ticks to automatic",error_lead);
                        tnew->tick.x_b_mode = NhlAUTOMATIC;
			ret = NhlWARNING;
                }
/*
* Always use positive spacing and determine if needs to be negative latter
*/
                tnew->tick.x_b_tick_spacing = (float)
                                fabs((double)tnew->tick.x_b_tick_spacing);
        }

        if((tnew->tick.x_t_on)&&(tnew->tick.x_t_mode == NhlMANUAL)){
                if((tnew->tick.x_t_tick_start == 0.0)&&
                        (tnew->tick.x_t_tick_end == 0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: No tick start or end set for top ticks in manual mode, using values of DataLeft and DataRight",error_lead);
                        tnew->tick.x_t_tick_start = MIN(tnew->tick.x_t_data_left						,tnew->tick.x_t_data_right);
                        tnew->tick.x_t_tick_end = MAX(tnew->tick.x_t_data_left,
                                                tnew->tick.x_t_data_right);
			ret = NhlWARNING;
                }
                if(tnew->tick.x_t_tick_spacing == 0.0){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: A tick mark spacing must be assigned for manual mode, defaulting top ticks to automatic",error_lead);
                        tnew->tick.x_t_mode = NhlAUTOMATIC;
			ret = NhlWARNING;
                }
/*
* Always use positive spacing and determine if needs to be negative latter
*/
                tnew->tick.x_t_tick_spacing = (float)
                                fabs((double)tnew->tick.x_t_tick_spacing);
        }



        if((tnew->tick.y_l_on)&&(tnew->tick.y_l_mode == NhlMANUAL)){
                if((tnew->tick.y_l_tick_start == 0.0)&&
                        (tnew->tick.y_l_tick_end == 0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: No tick start or end set for left ticks in manual mode, using values of DataTop and DataBottom",error_lead);
			ret = NhlWARNING;
                        tnew->tick.y_l_tick_start = MIN(tnew->tick.y_l_data_top
                                                ,tnew->tick.y_l_data_bottom);
                        tnew->tick.y_l_tick_end = MAX(tnew->tick.y_l_data_top,
                                                tnew->tick.y_l_data_bottom);
                }
                if(tnew->tick.y_l_tick_spacing == 0.0){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: A tick mark spacing must be assigned for manual mode, defaulting left ticks to automatic",error_lead);
			ret = NhlWARNING;
                        tnew->tick.y_l_mode = NhlAUTOMATIC;
                }
/*
* Always use positive spacing and determine if needs to be negative latter
*/
                tnew->tick.y_l_tick_spacing = (float)
                                fabs((double)tnew->tick.y_l_tick_spacing);
        }
        if((tnew->tick.y_r_on)&&(tnew->tick.y_r_mode == NhlMANUAL)){
                if((tnew->tick.y_r_tick_start == 0.0)&&
                        (tnew->tick.y_r_tick_end == 0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: No tick start or end set for right ticks in manual mode, using values of DataTop and DataBottom",error_lead);
			ret = NhlWARNING;
                        tnew->tick.y_r_tick_start = MIN(tnew->tick.y_r_data_top
                                                ,tnew->tick.y_r_data_bottom);
                        tnew->tick.y_r_tick_end = MAX(tnew->tick.y_r_data_bottom                                                , tnew->tick.y_r_data_top);
                }
                if(tnew->tick.y_r_tick_spacing == 0.0){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: A tick mark spacing must be assigned for manual mode, defaulting right ticks to automatic",error_lead);
			ret = NhlWARNING;
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
#if  __STDC__
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

        if((tnew->tick.x_b_on)&&(tnew->tick.x_b_mode != NhlAUTOMATIC)) {
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
			ret = NhlINFO;
                }
        }
        if((tnew->tick.x_t_on)&&(tnew->tick.x_t_mode != NhlAUTOMATIC)) {
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
			ret = NhlINFO;
                }
        }
        if((tnew->tick.y_r_on)&&(tnew->tick.y_r_mode != NhlAUTOMATIC)) {
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
			ret = NhlINFO;
                }
        }
        if((tnew->tick.y_l_on)&&(tnew->tick.y_l_mode != NhlAUTOMATIC)) {
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
			ret = NhlINFO;
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
#if	__STDC__
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
#if  __STDC__
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
	NhlBoolean	free_xt_val=False, skip_xt_val=False;
	NhlBoolean	free_yl_val=False, skip_yl_val=False;
	NhlBoolean	free_yr_val=False, skip_yr_val=False;
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

		if(told->tick.x_t_values != tnew->tick.x_t_values)
			free_xt_val = True;
		else
			skip_xt_val = True;

		if(told->tick.y_l_values != tnew->tick.y_l_values)
			free_yl_val = True;
		else
			skip_yl_val = True;

		if(told->tick.y_r_values != tnew->tick.y_r_values)
			free_yr_val = True;
		else
			skip_yr_val = True;
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

	/*
	 * Set labels fields
	 */

	if((tnew->tick.x_b_labels != NULL) && !skip_xb_labels){

		gen = (NhlGenArray)tnew->tick.x_b_labels;

		if((gen->typeQ != Qstring) || (gen->size != sizeof(NhlString))||
			(gen->num_dimensions != 1) || (gen->num_elements < 1)){

			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be a 1 dim char* array: resetting",
						error_lead,NhlNtmXBLabels);

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

	/*
	 * Check "mode" fields
	 */

	if((tnew->tick.x_b_mode == NhlEXPLICIT) &&
					(tnew->tick.x_b_values == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlAUTOMATIC",error_lead,
			NhlNtmXBValues,NhlNtmXBMode);
		tnew->tick.x_b_mode = NhlAUTOMATIC;
		ret = MIN(ret,NhlWARNING);
	}
	if((tnew->tick.x_t_mode == NhlEXPLICIT) &&
					(tnew->tick.x_t_values == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlAUTOMATIC",error_lead,
			NhlNtmXTValues,NhlNtmXTMode);
		tnew->tick.x_t_mode = NhlAUTOMATIC;
		ret = MIN(ret,NhlWARNING);
	}
	if((tnew->tick.y_l_mode == NhlEXPLICIT) &&
					(tnew->tick.y_l_values == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlAUTOMATIC",error_lead,
			NhlNtmYLValues,NhlNtmYLMode);
		tnew->tick.y_l_mode = NhlAUTOMATIC;
		ret = MIN(ret,NhlWARNING);
	}
	if((tnew->tick.y_r_mode == NhlEXPLICIT) &&
					(tnew->tick.y_r_values == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s not set: setting %s to NhlAUTOMATIC",error_lead,
			NhlNtmYRValues,NhlNtmYRMode);
		tnew->tick.y_r_mode = NhlAUTOMATIC;
		ret = MIN(ret,NhlWARNING);
	}

	/*
	 * Give NhlINFO warning if no labels are given
	 */
	if((tnew->tick.x_b_mode == NhlEXPLICIT) &&
					(tnew->tick.x_b_labels == NULL)){
		NhlPError(NhlINFO,NhlEUNKNOWN,"%s:%s is NhlEXPLICIT but %s not set",
					error_lead,NhlNtmXBMode,NhlNtmXBLabels);
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.x_t_mode == NhlEXPLICIT) &&
					(tnew->tick.x_t_labels == NULL)){
		NhlPError(NhlINFO,NhlEUNKNOWN,"%s:%s is NhlEXPLICIT but %s not set",
					error_lead,NhlNtmXTMode,NhlNtmXTLabels);
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.y_l_mode == NhlEXPLICIT) &&
					(tnew->tick.y_l_labels == NULL)){
		NhlPError(NhlINFO,NhlEUNKNOWN,"%s:%s is NhlEXPLICIT but %s not set",
					error_lead,NhlNtmYLMode,NhlNtmYLLabels);
		ret = MIN(ret,NhlINFO);
	}
	if((tnew->tick.y_r_mode == NhlEXPLICIT) &&
					(tnew->tick.y_r_labels == NULL)){
		NhlPError(NhlINFO,NhlEUNKNOWN,"%s:%s is NhlEXPLICIT but %s not set",
					error_lead,NhlNtmYRMode,NhlNtmYRLabels);
		ret = MIN(ret,NhlINFO);
	}


	/*
	 * Check MinorOn fields
	 */
	if((tnew->tick.x_b_on)&&(tnew->tick.x_b_mode == NhlEXPLICIT)) {
                if(tnew->tick.x_b_minor_on) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s cannot be on when %s is NhlEXPLICIT:setting %s off",
				error_lead,NhlNtmXBMinorOn,NhlNtmXBMode,
							NhlNtmXBMinorOn);

			ret = NhlWARNING;
                        tnew->tick.x_b_minor_on = False;
                        tnew->tick.x_b_nminor = 0;
                }
	}
	if((tnew->tick.x_t_on)&&(tnew->tick.x_t_mode == NhlEXPLICIT)) {
                if(tnew->tick.x_t_minor_on) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s cannot be on when %s is NhlEXPLICIT:setting %s off",
				error_lead,NhlNtmXTMinorOn,NhlNtmXTMode,
							NhlNtmXTMinorOn);

			ret = NhlWARNING;
                        tnew->tick.x_t_minor_on = False;
                        tnew->tick.x_t_nminor = 0;
                }
	}
	if((tnew->tick.y_l_on)&&(tnew->tick.y_l_mode == NhlEXPLICIT)) {
                if(tnew->tick.y_l_minor_on) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s cannot be on when %s is NhlEXPLICIT:setting %s off",
				error_lead,NhlNtmYLMinorOn,NhlNtmYLMode,
							NhlNtmYLMinorOn);

			ret = NhlWARNING;
                        tnew->tick.y_l_minor_on = False;
                        tnew->tick.y_l_nminor = 0;
                }
	}
	if((tnew->tick.y_r_on)&&(tnew->tick.y_r_mode == NhlEXPLICIT)) {
                if(tnew->tick.y_r_minor_on) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s cannot be on when %s is NhlEXPLICIT:setting %s off",
				error_lead,NhlNtmYRMinorOn,NhlNtmYRMode,
							NhlNtmYRMinorOn);

			ret = NhlWARNING;
                        tnew->tick.y_r_minor_on = False;
                        tnew->tick.y_r_nminor = 0;
                }
	}


	/*
	 * Set start and end ticks
	 */
        if((tnew->tick.x_b_on)&&(tnew->tick.x_b_mode == NhlEXPLICIT)){
                if((tnew->tick.x_b_tick_start == 0.0)&&
                        (tnew->tick.x_b_tick_end == 0.0)) {
                        tnew->tick.x_b_tick_start = MIN(tnew->tick.x_b_data_left						,tnew->tick.x_b_data_right);
                        tnew->tick.x_b_tick_end = MAX(tnew->tick.x_b_data_left,
                                                tnew->tick.x_b_data_right);
			ret = NhlWARNING;
                }
        }
        if((tnew->tick.x_t_on)&&(tnew->tick.x_t_mode == NhlEXPLICIT)){
                if((tnew->tick.x_t_tick_start == 0.0)&&
                        (tnew->tick.x_t_tick_end == 0.0)) {
                        tnew->tick.x_t_tick_start = MIN(tnew->tick.x_t_data_left						,tnew->tick.x_t_data_right);
                        tnew->tick.x_t_tick_end = MAX(tnew->tick.x_t_data_left,
                                                tnew->tick.x_t_data_right);
			ret = NhlWARNING;
                }
        }
        if((tnew->tick.y_l_on)&&(tnew->tick.y_l_mode == NhlEXPLICIT)){
                if((tnew->tick.y_l_tick_start == 0.0)&&
                        (tnew->tick.y_l_tick_end == 0.0)) {
                        tnew->tick.y_l_tick_start = MIN(tnew->tick.y_l_data_top,tnew->tick.y_l_data_bottom);
                        tnew->tick.y_l_tick_end = MAX(tnew->tick.y_l_data_top,
                                                tnew->tick.y_l_data_bottom);
			ret = NhlWARNING;
                }
        }
        if((tnew->tick.y_r_on)&&(tnew->tick.y_r_mode == NhlEXPLICIT)){
                if((tnew->tick.y_r_tick_start == 0.0)&&
                        (tnew->tick.y_r_tick_end == 0.0)) {
                        tnew->tick.y_r_tick_start = MIN(tnew->tick.y_r_data_top,tnew->tick.y_r_data_bottom);
                        tnew->tick.y_r_tick_end = MAX(tnew->tick.y_r_data_top,
                                                tnew->tick.y_r_data_bottom);
			ret = NhlWARNING;
                }
        }

	return(ret);
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
#if  __STDC__
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
	
	if((tnew->tick.x_b_on)&&(tnew->tick.x_b_style == NhlLOG)) {
                if((tnew->tick.x_b_data_left <=0.0)
                        ||(tnew->tick.x_b_data_right <=0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Bottom tick style is log and either DataLeft or DataRight is negative or zero, turning bottom ticks off",error_lead);
                        tnew->tick.x_b_on = 0;
			ret = NhlWARNING;

                }
		if(tnew->tick.x_b_mode == NhlMANUAL) {
			if(tnew->tick.x_b_tick_spacing < 1.0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected but spacing is less than 1 decade reseting XBTickSpacing to 1.0",error_lead);
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG tick marks can only have 1, 4 or 8 minor tickmarks, reseting XBMinorPerMajor to 1,4 or 8",error_lead);
			ret = NhlWARNING;
		
			if(tnew->tick.x_b_minor_per_major < 3) {
				tnew->tick.x_b_minor_per_major = 1;
			} else if(tnew->tick.x_b_minor_per_major < 6) {
				tnew->tick.x_b_minor_per_major = 4;
			} else  {
                                tnew->tick.x_b_minor_per_major = 8;
			}
		}
        }
        if((tnew->tick.x_t_on)&&(tnew->tick.x_t_style == NhlLOG)) {
                if((tnew->tick.x_t_data_left <=0.0)
                        ||(tnew->tick.x_t_data_right <=0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Top tick style is log and either DataLeft or DataRight is negative or zero, turning top ticks off",error_lead);
			ret = NhlWARNING;
                        tnew->tick.x_t_on = 0;

                }
		if(tnew->tick.x_t_mode == NhlMANUAL) {
			if(tnew->tick.x_t_tick_spacing < 1.0){
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected but spacing is less than 1 decade reseting XTTickSpacing to 1.0",error_lead);
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG tick marks can only have 1, 4 or 8 minor tickmarks, reseting XTMinorPerMajor to 1,4 or 8",error_lead);
			ret = NhlWARNING;
		
			if(tnew->tick.x_t_minor_per_major < 3) {
				tnew->tick.x_t_minor_per_major = 1;
			} else if(tnew->tick.x_t_minor_per_major < 6) {
				tnew->tick.x_t_minor_per_major = 4;
			} else  {
                                tnew->tick.x_t_minor_per_major = 8;
			}
		}
        }
        if((tnew->tick.y_l_on)&&(tnew->tick.y_l_style == NhlLOG)) {
                if((tnew->tick.y_l_data_top <=0.0)
                        ||(tnew->tick.y_l_data_bottom<=0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Left tick style is log and either DataBottom or DataTop is negative or zero, turning left ticks off",error_lead);
			ret = NhlWARNING;
                        tnew->tick.y_l_on = 0;

                }
		if(tnew->tick.y_l_mode == NhlMANUAL) {
			if(tnew->tick.y_l_tick_spacing < 1.0) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected but spacing is less than 1 decade reseting YLTickSpacing to 1.0",error_lead);
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG tick marks can only have 1, 4 or 8 minor tickmarks, reseting YLMinorPerMajor to 1,4 or 8",error_lead);
			ret = NhlWARNING;
		
			if(tnew->tick.y_l_minor_per_major < 3) {
				tnew->tick.y_l_minor_per_major = 1;
			} else if(tnew->tick.y_l_minor_per_major < 6) {
				tnew->tick.y_l_minor_per_major = 4;
			} else  {
                                tnew->tick.y_l_minor_per_major = 8;
			}
		}
        }
        if((tnew->tick.y_r_on)&&(tnew->tick.y_r_style == NhlLOG)) {
                if((tnew->tick.y_r_data_top <=0.0)
                        ||(tnew->tick.y_r_data_bottom <=0.0)) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Right tick style is log and either DataBottom or DataTop is negative or zero, turning right ticks off",error_lead);
			ret = NhlWARNING;
                        tnew->tick.y_r_on = 0;

                }
		if(tnew->tick.y_r_mode == NhlMANUAL) {
			if(tnew->tick.y_r_tick_spacing < 1.0){
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG style and NhlMANUAL mode selected but spacing is less than 1 decade reseting YRTickSpacing to 1.0",error_lead);
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlLOG tick marks can only have 1, 4 or 8 minor tickmarks, reseting YLMinorPerMajor to 1,4 or 8",error_lead);
			ret = NhlWARNING;
		
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
#if  __STDC__
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

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
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

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
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

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
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

		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
		(gen->num_dimensions != 1) || (gen->len_dimensions[0] < 3)){
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
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XBDataLeftF is greater than maximum value in XBCoordPoints, resetting XBDataLeftF to maximum of XBCoordPoints");
				tnew->tick.x_b_data_left = tnew->tick.ir_xbmax;
				ret = NhlWARNING;
			} 
			if(tnew->tick.x_b_data_right < tnew->tick.ir_xbmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XBDataRightF is than minimum value in XBCoordPoints, resetting XBDataRightF to minimum of XBCoordPoints");
				tnew->tick.x_b_data_right = tnew->tick.ir_xbmin;
				ret = NhlWARNING;
			}
		} else {
			if(tnew->tick.x_b_data_left < tnew->tick.ir_xbmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XBDataLeftF is less than minimum value in XBCoordPoints, resetting XBDataLeftF to minimum of XBCoordPoints");
				tnew->tick.x_b_data_left = tnew->tick.ir_xbmin;
				ret = NhlWARNING;
			}
			if(tnew->tick.x_b_data_right > tnew->tick.ir_xbmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XBDataRightF is than maximum value in XBCoordPoints, resetting XBDataRightF to maximum of XBCoordPoints");
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
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XTDataLeftF is greater than maximum value in XTCoordPoints, resetting XTDataLeftF to maximum of XTCoordPoints");
				tnew->tick.x_t_data_left = tnew->tick.ir_xtmax;
				ret = NhlWARNING;
			}
			if(tnew->tick.x_t_data_right < tnew->tick.ir_xtmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XTDataRightF is than minimum value in XTCoordPoints, resetting XTDataRightF to minimum of XTCoordPoints");
				tnew->tick.x_t_data_right = tnew->tick.ir_xtmin;
				ret = NhlWARNING;
			}
		} else {
			if(tnew->tick.x_t_data_left < tnew->tick.ir_xtmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XTDataLeftF is less than minimum value in XTCoordPoints, resetting XTDataLeftF to minimum of XTCoordPoints");
				tnew->tick.x_t_data_left = tnew->tick.ir_xtmin;
				ret = NhlWARNING;
			}
			if(tnew->tick.x_t_data_right > tnew->tick.ir_xtmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: XTDataRightF is than maximum value in XTCoordPoints, resetting XTDataRightF to maximum of XTCoordPoints");
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
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YRDataBottomF is greater than maximum value in YRCoordPoints, resetting YRDataBottomF to maximum of YRCoordPoints");
				tnew->tick.y_r_data_bottom = tnew->tick.ir_yrmax;
				ret = NhlWARNING;
			}
			if(tnew->tick.y_r_data_top < tnew->tick.ir_yrmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YRDataTopF is than minimum value in YRCoordPoints, resetting YRDataTopF to minimum of YRCoordPoints");
				tnew->tick.y_r_data_top = tnew->tick.ir_yrmin;
				ret = NhlWARNING;
			}
		} else {
			if(tnew->tick.y_r_data_bottom < tnew->tick.ir_yrmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YRDataBottomF is less than minimum value in YRCoordPoints, resetting YRDataBottomF to minimum of YRCoordPoints");
				tnew->tick.y_r_data_bottom = tnew->tick.ir_yrmin;
				ret = NhlWARNING;
			}
			if(tnew->tick.y_r_data_top > tnew->tick.ir_yrmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YRDataTopF is than maximum value in YRCoordPoints, resetting YRDataTopF to maximum of YRCoordPoints");
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
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YLDataBottomF is greater than maximum value in YLCoordPoints, resetting YLDataBottomF to maximum of YLCoordPoints");
				tnew->tick.y_l_data_bottom = tnew->tick.ir_ylmax;
				ret = NhlWARNING;
			}
			if(tnew->tick.y_l_data_top < tnew->tick.ir_ylmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YLDataTopF is than minimum value in YLCoordPoints, resetting YLDataTopF to minimum of YLCoordPoints");
				tnew->tick.y_l_data_top= tnew->tick.ir_ylmin;
				ret = NhlWARNING;
			}
		} else {
			if(tnew->tick.y_l_data_bottom < tnew->tick.ir_ylmin) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YLDataBottomF is less than minimum value in YLCoordPoints, resetting YLDataBottomF to minimum of YLCoordPoints");
				tnew->tick.y_l_data_bottom= tnew->tick.ir_ylmin;
				ret = NhlWARNING;
			}
			if(tnew->tick.y_l_data_top > tnew->tick.ir_ylmax) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"CheckIrregular: YLDataTopF is than maximum value in YLCoordPoints, resetting YLDataTopF to maximum of YLCoordPoints");
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
#if  __STDC__
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
#if  __STDC__
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
	
	if(c_or_s == CREATE) {
		error_lead = "TickMarkInitialize";
	} else {		
		error_lead = "TickMarkSetValues";
	}
	
        if((tnew->tick.x_b_on)
                &&(tnew->tick.x_b_style == NhlGEOGRAPHIC)) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlGEOGRAPHIC tickmarks have not been implemented, default XB to NhlLINEAR",error_lead);
                tnew->tick.x_b_style = NhlLINEAR;
		ret = NhlWARNING;
        }
        if((tnew->tick.x_t_on)
                &&(tnew->tick.x_t_style == NhlGEOGRAPHIC)) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlGEOGRAPHIC tickmarks have not been implemented, default XT to NhlLINEAR",error_lead);
                tnew->tick.x_t_style = NhlLINEAR;
		ret = NhlWARNING;
        }
        if((tnew->tick.y_l_on)
                &&(tnew->tick.y_l_style == NhlGEOGRAPHIC)) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlGEOGRAPHIC tickmarks have not been implemented, default YL to NhlLINEAR",error_lead);
                tnew->tick.y_l_style = NhlLINEAR;
		ret = NhlWARNING;
        }
        if((tnew->tick.y_r_on)
                &&(tnew->tick.y_r_style == NhlGEOGRAPHIC)) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: NhlGEOGRAPHIC tickmarks have not been implemented, default YR to NhlLINEAR",error_lead);
                tnew->tick.y_r_style = NhlLINEAR;
		ret = NhlWARNING;
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
#if __STDC__
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

/*
* NEED SOME KIND OF CHECKS HERE TO SEE IF RECOMPUTATION NEEDED
*/	
        if(tnew->tick.x_b_on) {
                switch(tnew->tick.x_b_mode) {
                        case NhlAUTOMATIC:
                                majorret = AutoComputeMajorTickMarks(
                                        tnew->tick.x_b_style,
                                        tnew->tick.x_b_major_data_locs,
                                        tnew->tick.x_b_major_labels,
                                        tnew->tick.x_b_max_ticks,
                                        tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        &tnew->tick.x_b_tick_start,
                                        &tnew->tick.x_b_tick_end,
                                        (tnew->tick.x_b_auto_precision?-1:tnew->tick.x_b_precision),
                                        &tnew->tick.x_b_tick_spacing,
                                        &tnew->tick.x_b_nmajor,
                                        tnew->tick.sci_note_cutoff);
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
                                        tnew->tick.x_b_style);
                                break;
                        case NhlMANUAL:
                                majorret = ManualComputeMajorTickMarks(
                                        tnew->tick.x_b_style,
                                        tnew->tick.x_b_major_data_locs,
                                        tnew->tick.x_b_major_labels,
                                        tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        tnew->tick.x_b_tick_start,
                                        tnew->tick.x_b_tick_end,
                                        (tnew->tick.x_b_auto_precision?-1:tnew->tick.x_b_precision),
                                        tnew->tick.x_b_tick_spacing,
                                        tnew->tick.x_b_spacing_type,
                                        &tnew->tick.x_b_nmajor,
                                        tnew->tick.sci_note_cutoff);
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
                                        tnew->tick.x_b_style);
                                break;
                        case NhlEXPLICIT:
                                majorret = ExplicitComputeMajorTickMarks(
                                        tnew->tick.x_b_style,
                                        tnew->tick.x_b_major_data_locs,
                                        tnew->tick.x_b_major_labels,
                                        tnew->tick.x_b_data_max,
                                        tnew->tick.x_b_data_min,
                                        tnew->tick.x_b_tick_start,
                                        tnew->tick.x_b_tick_end,
                                        tnew->tick.x_b_precision,
                                        tnew->tick.x_b_values->data,
                                        tnew->tick.x_b_labels->data,
                                        &tnew->tick.x_b_nmajor,
					tnew->tick.x_b_values->num_elements);
                                break;
                        default:
                                break;
                }
        } else {
		tnew->tick.x_b_nmajor = 0;
		tnew->tick.x_b_nminor = 0;
	}
	if((majorret < NhlWARNING)||(minorret < NhlWARNING)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeTickInfo: A NhlFATAL error occured while computing Tick information for the X-Axis bottom ticks");
		bottom = MIN(majorret,minorret);
	}
	majorret = NhlNOERROR;
	minorret = NhlNOERROR;
        if(tnew->tick.x_t_on) {
                switch(tnew->tick.x_t_mode) {
                        case NhlAUTOMATIC:
                                majorret = AutoComputeMajorTickMarks(
                                        tnew->tick.x_t_style,
                                        tnew->tick.x_t_major_data_locs,
                                        tnew->tick.x_t_major_labels,
                                        tnew->tick.x_t_max_ticks,
                                        tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        &tnew->tick.x_t_tick_start,
                                        &tnew->tick.x_t_tick_end,
                                        (tnew->tick.x_t_auto_precision?-1:tnew->tick.x_t_precision),
                                        &tnew->tick.x_t_tick_spacing,
                                        &tnew->tick.x_t_nmajor,
                                        tnew->tick.sci_note_cutoff);
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
                                        tnew->tick.x_t_style);
                                break;
                        case NhlMANUAL:
                                majorret = ManualComputeMajorTickMarks(
                                        tnew->tick.x_t_style,
                                        tnew->tick.x_t_major_data_locs,
                                        tnew->tick.x_t_major_labels,
                                        tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        tnew->tick.x_t_tick_start,
                                        tnew->tick.x_t_tick_end,
                                        (tnew->tick.x_t_auto_precision?-1:tnew->tick.x_t_precision),
                                        tnew->tick.x_t_tick_spacing,
                                        tnew->tick.x_t_spacing_type,
                                        &tnew->tick.x_t_nmajor,
                                        tnew->tick.sci_note_cutoff);
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
                                        tnew->tick.x_t_style);
                                break;
                        case NhlEXPLICIT:
                                majorret = ExplicitComputeMajorTickMarks(
                                        tnew->tick.x_t_style,
                                        tnew->tick.x_t_major_data_locs,
                                        tnew->tick.x_t_major_labels,
                                        tnew->tick.x_t_data_max,
                                        tnew->tick.x_t_data_min,
                                        tnew->tick.x_t_tick_start,
                                        tnew->tick.x_t_tick_end,
                                        tnew->tick.x_t_precision,
                                        tnew->tick.x_t_values->data,
                                        tnew->tick.x_t_labels->data,
                                        &tnew->tick.x_t_nmajor,
					tnew->tick.x_t_values->num_elements);
                                break;
                        default:
                                break;
                }
        } else {
		tnew->tick.x_t_nmajor = 0;
		tnew->tick.x_t_nminor = 0;
	}
	if((majorret < NhlWARNING)||(minorret < NhlWARNING)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeTickInfo: A NhlFATAL error occured while computing Tick information for the X-Axis top ticks");
		top = MIN(majorret,minorret);
	}
	majorret = NhlNOERROR;
	minorret = NhlNOERROR;
        if(tnew->tick.y_l_on) {
                switch(tnew->tick.y_l_mode) {
                        case NhlAUTOMATIC:
                                majorret = AutoComputeMajorTickMarks(
                                        tnew->tick.y_l_style,
                                        tnew->tick.y_l_major_data_locs,
                                        tnew->tick.y_l_major_labels,
                                        tnew->tick.y_l_max_ticks,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        &tnew->tick.y_l_tick_start,
                                        &tnew->tick.y_l_tick_end,
                                        (tnew->tick.y_l_auto_precision?-1:tnew->tick.y_l_precision),
                                        &tnew->tick.y_l_tick_spacing,
                                        &tnew->tick.y_l_nmajor,
                                        tnew->tick.sci_note_cutoff);
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
                                        tnew->tick.y_l_style);
                                break;
                        case NhlMANUAL:
                                majorret = ManualComputeMajorTickMarks(
                                        tnew->tick.y_l_style,
                                        tnew->tick.y_l_major_data_locs,
                                        tnew->tick.y_l_major_labels,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        tnew->tick.y_l_tick_start,
                                        tnew->tick.y_l_tick_end,
                                        (tnew->tick.y_l_auto_precision?-1:tnew->tick.y_l_precision),
                                        tnew->tick.y_l_tick_spacing,
                                        tnew->tick.y_l_spacing_type,
                                        &tnew->tick.y_l_nmajor,
                                        tnew->tick.sci_note_cutoff);
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
                                        tnew->tick.y_l_style);
                                break;
                        case NhlEXPLICIT:
                                majorret = ExplicitComputeMajorTickMarks(
                                        tnew->tick.y_l_style,
                                        tnew->tick.y_l_major_data_locs,
                                        tnew->tick.y_l_major_labels,
                                        tnew->tick.y_l_data_max,
                                        tnew->tick.y_l_data_min,
                                        tnew->tick.y_l_tick_start,
                                        tnew->tick.y_l_tick_end,
                                        tnew->tick.y_l_precision,
                                        tnew->tick.y_l_values->data,
                                        tnew->tick.y_l_labels->data,
                                        &tnew->tick.y_l_nmajor,
					tnew->tick.y_l_values->num_elements);
                                break;
                }
        } else {
		tnew->tick.y_l_nmajor = 0;
		tnew->tick.y_l_nminor = 0;
	}
	if((majorret < NhlWARNING)||(minorret < NhlWARNING)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeTickInfo: A NhlFATAL error occured while computing Tick information for the Y-Axis left ticks");
		left = MIN(majorret,minorret);
	}
	majorret = NhlNOERROR;
	minorret = NhlNOERROR;
        if(tnew->tick.y_r_on) {
                switch(tnew->tick.y_r_mode) {
                        case NhlAUTOMATIC:
                                majorret = AutoComputeMajorTickMarks(
                                        tnew->tick.y_r_style,
                                        tnew->tick.y_r_major_data_locs,
                                        tnew->tick.y_r_major_labels,
                                        tnew->tick.y_r_max_ticks,
                                        tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        &tnew->tick.y_r_tick_start,
                                        &tnew->tick.y_r_tick_end,
                                        (tnew->tick.y_r_auto_precision?-1:tnew->tick.y_r_precision),
                                        &tnew->tick.y_r_tick_spacing,
                                        &tnew->tick.y_r_nmajor,
                                        tnew->tick.sci_note_cutoff);
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
                                        tnew->tick.y_r_style);
                                break;
                        case NhlMANUAL:
                                majorret = ManualComputeMajorTickMarks(
                                        tnew->tick.y_r_style,
                                        tnew->tick.y_r_major_data_locs,
                                        tnew->tick.y_r_major_labels,
                                        tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        tnew->tick.y_r_tick_start,
                                        tnew->tick.y_r_tick_end,
                                        (tnew->tick.y_r_auto_precision?-1:tnew->tick.y_r_precision),
                                        tnew->tick.y_r_tick_spacing,
                                        tnew->tick.y_r_spacing_type,
                                        &tnew->tick.y_r_nmajor,
                                        tnew->tick.sci_note_cutoff);
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
                                        tnew->tick.y_r_style);
                                break;
                        case NhlEXPLICIT:
                                majorret = ExplicitComputeMajorTickMarks(
                                        tnew->tick.y_r_style,
                                        tnew->tick.y_r_major_data_locs,
                                        tnew->tick.y_r_major_labels,
                                        tnew->tick.y_r_data_max,
                                        tnew->tick.y_r_data_min,
                                        tnew->tick.y_r_tick_start,
                                        tnew->tick.y_r_tick_end,
                                        tnew->tick.y_r_precision,
                                        tnew->tick.y_r_values->data,
                                        tnew->tick.y_r_labels->data,
                                        &tnew->tick.y_r_nmajor,
					tnew->tick.y_r_values->num_elements);
                                break;
                        default:
                                break;
                }
        } else {
		tnew->tick.y_r_nmajor = 0;
		tnew->tick.y_r_nminor = 0;
	}
	if((majorret < NhlWARNING)||(minorret < NhlWARNING)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ComputeTickInfo: A NhlFATAL error occured while computing Tick information for the Y-Axis right ticks");
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
#if  __STDC__
(NhlTickMarkLayer tnew, NhlTickMarkLayer told, int c_or_s)
#else
(tnew,told,c_or_s)
	NhlTickMarkLayer tnew;
	NhlTickMarkLayer told;
	int		c_or_s;
#endif
{
	int i,tmpi=0,status;
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

	if(((tnew->tick.x_b_on) ||(tnew->tick.y_l_on))&&(tnew->tick.xb_yl_trans_obj != NULL)) {
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
        subret = _NhlDataToWin((NhlLayer)tnew->tick.xb_yl_trans_obj,(NhlLayer)tnew,
                tnew->tick.x_b_major_data_locs,tnew->tick.y_l_major_data_locs,
                tmpi,
                tnew->tick.x_b_major_ndc_locs,tnew->tick.y_l_major_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occured while mapping the bottom and left tick marks to the window, cannot continue",error_lead);
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
        subret = _NhlWinToNDC((NhlLayer)tnew->tick.xb_yl_trans_obj,(NhlLayer)tnew,
                tnew->tick.x_b_major_ndc_locs,tnew->tick.y_l_major_ndc_locs,
                tmpi,
                tnew->tick.x_b_major_ndc_locs,tnew->tick.y_l_major_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occured while mapping the bottom and left tick marks from the window to NDC, cannot continue",error_lead);
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
        subret = _NhlDataToWin((NhlLayer)tnew->tick.xb_yl_trans_obj,(NhlLayer)tnew,
                tnew->tick.x_b_minor_data_locs,tnew->tick.y_l_minor_data_locs,
                tmpi,
                tnew->tick.x_b_minor_ndc_locs,tnew->tick.y_l_minor_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: A NhlFATAL error occured while mapping minor tickmarks to the window, cannot continue",error_lead);
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
        subret = _NhlWinToNDC((NhlLayer)tnew->tick.xb_yl_trans_obj,(NhlLayer)tnew,
                tnew->tick.x_b_minor_ndc_locs,tnew->tick.y_l_minor_ndc_locs,
                tmpi,
                tnew->tick.x_b_minor_ndc_locs,tnew->tick.y_l_minor_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: A NhlFATAL error occured while mapping minor tickmarks from the window to NDC, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
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



	if(((tnew->tick.x_t_on)||(tnew->tick.y_r_on))&&(tnew->tick.xt_yr_trans_obj != NULL)) {
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
        subret = _NhlDataToWin((NhlLayer)tnew->tick.xt_yr_trans_obj,(NhlLayer)tnew,
                tnew->tick.x_t_major_data_locs,tnew->tick.y_r_major_data_locs,
                tmpi,
                tnew->tick.x_t_major_ndc_locs,tnew->tick.y_r_major_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occured while mapping the top and right tick marks to the window, cannot continue",error_lead);
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
        subret = _NhlWinToNDC((NhlLayer)tnew->tick.xt_yr_trans_obj,(NhlLayer)tnew,
                tnew->tick.x_t_major_ndc_locs,tnew->tick.y_r_major_ndc_locs,
                tmpi,
                tnew->tick.x_t_major_ndc_locs,tnew->tick.y_r_major_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occured while mapping the top and right tick marks from the window to NDC, cannot continue",error_lead);
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
        subret = _NhlDataToWin((NhlLayer)tnew->tick.xt_yr_trans_obj,(NhlLayer)tnew,
                tnew->tick.x_t_minor_data_locs,tnew->tick.y_r_minor_data_locs,
                tmpi,
                tnew->tick.x_t_minor_ndc_locs,tnew->tick.y_r_minor_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occured while mapping the top and right minor tick marks to the window, cannot continue",error_lead);
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
        subret = _NhlWinToNDC((NhlLayer)tnew->tick.xt_yr_trans_obj,(NhlLayer)tnew,
                tnew->tick.x_t_minor_ndc_locs,tnew->tick.y_r_minor_ndc_locs,
                tmpi,
                tnew->tick.x_t_minor_ndc_locs,tnew->tick.y_r_minor_ndc_locs,
                &status,NULL,NULL);
	if(subret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: An Error occured while mapping the top and right minor tick marks from the window to NDC, cannot continue",error_lead);
		return(NhlFATAL);
	} else if(subret < ret) {
		ret = subret;
	}
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
#if  __STDC__
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
        char buffer[_NhlMAXFNAMELEN];
	float tmpy,tmpx,tmpwidth,tmpheight;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes subret = NhlNOERROR;
	char *error_lead;

	if(c_or_s == SET )
		error_lead = "TickMarkSetValues";
	else 
		error_lead = "TickMarkInitialize";
		


/*
* Now make array of locations and strings and use it to create the
* MultiTextItems . Tick must be filtered out if stride > 1
*/
        if(tnew->tick.x_b_labels_on) {
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
	if((c_or_s == CREATE)||(tnew->tick.xb_multi ==NULL)) {
                strcpy(buffer,tnew->base.name);
                strcat(buffer,".xb_Multi");
                subret = NhlVACreate(&tmpid,buffer,NhlmultiTextLayerClass,tnew->base.id,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_Y_CONST,
                        NhlNMtextConstPosF,tnew->tick.x_b_ndc_label_y,
                        NhlNMtextPosArray,locs_for_multi,
                        NhlNtxAngleF,tnew->tick.x_b_label_angle,
                        NhlNtxFont,tnew->tick.x_b_label_font,
                        NhlNtxJust,tnew->tick.x_b_label_just,
                        NhlNtxFontHeightF,tnew->tick.x_b_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.x_b_label_font_aspect,
                        NhlNtxDirection,tnew->tick.x_b_label_direction,
                        NhlNtxFontColor,tnew->tick.x_b_label_font_color,
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
                        NhlNtxAngleF,tnew->tick.x_b_label_angle,
                        NhlNtxFont,tnew->tick.x_b_label_font,
                        NhlNtxJust,tnew->tick.x_b_label_just,
                        NhlNtxFontHeightF,tnew->tick.x_b_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.x_b_label_font_aspect,
                        NhlNtxDirection,tnew->tick.x_b_label_direction,
                        NhlNtxFontColor,tnew->tick.x_b_label_font_color,
                        NULL);
		if(subret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set MultiText item values for bottom tick mark labels, cannot continue",error_lead);
			return(NhlFATAL);
		} else if (subret < ret) {
			ret = subret;
		}
	}
                subret = NhlVAGetValues(tmpid,
                        NhlNvpYF,&tmpy,NULL);
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
        if(tnew->tick.x_t_labels_on) {
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
	if((c_or_s == CREATE)||(tnew->tick.xt_multi == NULL)) {
                strcpy(buffer,tnew->base.name);
                strcat(buffer,".xt_Multi");
                subret = NhlVACreate(&tmpid,buffer,NhlmultiTextLayerClass,tnew->base.id,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_Y_CONST,
                        NhlNMtextConstPosF,tnew->tick.x_t_ndc_label_y,
                        NhlNMtextPosArray,locs_for_multi,
                        NhlNtxAngleF,tnew->tick.x_t_label_angle,
                        NhlNtxFont,tnew->tick.x_t_label_font,
                        NhlNtxJust,tnew->tick.x_t_label_just,
                        NhlNtxFontHeightF,tnew->tick.x_t_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.x_t_label_font_aspect,
                        NhlNtxDirection,tnew->tick.x_t_label_direction,
                        NhlNtxFontColor,tnew->tick.x_t_label_font_color,
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
                        NhlNtxAngleF,tnew->tick.x_t_label_angle,
                        NhlNtxFont,tnew->tick.x_t_label_font,
                        NhlNtxJust,tnew->tick.x_t_label_just,
                        NhlNtxFontHeightF,tnew->tick.x_t_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.x_t_label_font_aspect,
                        NhlNtxDirection,tnew->tick.x_t_label_direction,
                        NhlNtxFontColor,tnew->tick.x_t_label_font_color,
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
                        NhlNvpHeightF,&tmpheight,NULL);
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
        if(tnew->tick.y_l_labels_on) {
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
	
	if((c_or_s == CREATE)||(tnew->tick.yl_multi == NULL)) {
                strcpy(buffer,tnew->base.name);
                strcat(buffer,".yl_Multi");
                subret = NhlVACreate(&tmpid,buffer,NhlmultiTextLayerClass,tnew->base.id,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_X_CONST,
                        NhlNMtextConstPosF,tnew->tick.y_l_ndc_label_x,
                        NhlNMtextPosArray,locs_for_multi,
                        NhlNtxAngleF,tnew->tick.y_l_label_angle,
                        NhlNtxFont,tnew->tick.y_l_label_font,
                        NhlNtxJust,tnew->tick.y_l_label_just,
                        NhlNtxFontHeightF,tnew->tick.y_l_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.y_l_label_font_aspect,
                        NhlNtxDirection,tnew->tick.y_l_label_direction,
                        NhlNtxFontColor,tnew->tick.y_l_label_font_color,
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
                        NhlNtxAngleF,tnew->tick.y_l_label_angle,
                        NhlNtxFont,tnew->tick.y_l_label_font,
                        NhlNtxJust,tnew->tick.y_l_label_just,
                        NhlNtxFontHeightF,tnew->tick.y_l_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.y_l_label_font_aspect,
                        NhlNtxDirection,tnew->tick.y_l_label_direction,
                        NhlNtxFontColor,tnew->tick.y_l_label_font_color,
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
                        NhlNvpWidthF,&tmpwidth,NULL);
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
        if(tnew->tick.y_r_labels_on) {
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
	if((c_or_s == CREATE)||(tnew->tick.yr_multi == NULL)) {
                strcpy(buffer,tnew->base.name);
                strcat(buffer,".yr_Multi");
                subret = NhlVACreate(&tmpid,buffer,NhlmultiTextLayerClass,tnew->base.id,
                        NhlNMtextNumStrings,tmp,
                        NhlNMtextStrings,labels_for_multi,
                        NhlNMtextOrientation,NhlMTEXT_X_CONST,
                        NhlNMtextConstPosF,tnew->tick.y_r_ndc_label_x,
                        NhlNMtextPosArray,locs_for_multi,
                        NhlNtxAngleF,tnew->tick.y_r_label_angle,
                        NhlNtxFont,tnew->tick.y_r_label_font,
                        NhlNtxJust,tnew->tick.y_r_label_just,
                        NhlNtxFontHeightF,tnew->tick.y_r_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.y_r_label_font_aspect,
                        NhlNtxDirection,tnew->tick.y_r_label_direction,
                        NhlNtxFontColor,tnew->tick.y_r_label_font_color,
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
                        NhlNtxAngleF,tnew->tick.y_r_label_angle,
                        NhlNtxFont,tnew->tick.y_r_label_font,
                        NhlNtxJust,tnew->tick.y_r_label_just,
                        NhlNtxFontHeightF,tnew->tick.y_r_label_font_height,
                        NhlNtxFontAspectF,tnew->tick.y_r_label_font_aspect,
                        NhlNtxDirection,tnew->tick.y_r_label_direction,
                        NhlNtxFontColor,tnew->tick.y_r_label_font_color,
                        NULL);
		if(subret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Could not set MultiText item values for right tick mark labels, cannot continue",error_lead);
			return(NhlFATAL);
		} else if (subret < ret) {
			ret = subret;
		}
	}
                subret = NhlVAGetValues(tmpid,
                        NhlNvpXF,&tmpx,NULL);
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
 *		was needed as well as the IrregularType2TransObj. This new 
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
 *		the IrregularType2TransObj.
 */
/*ARGSUSED*/
static NhlErrorTypes CreateXBYLTransformInfo
#if __STDC__
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
	NhlLayerClass trans_class = NULL;
	float *tmpycoordpoints;
        float *tmpxcoordpoints;
	char buffer[_NhlMAXFNAMELEN];
	int tmpid;
	NhlErrorTypes ret = NhlNOERROR;


	
	if((tnew->tick.y_l_on)||(tnew->tick.x_b_on)) {
		if((tnew->tick.y_l_style == NhlIRREGULAR)||
			(tnew->tick.x_b_style == NhlIRREGULAR)) {
			switch(tnew->tick.y_l_style) {
			case NhlIRREGULAR:
/*
* THis is needed because new has already been reallocated and could possibly
* be the same as told. Also old is already freed!
*/
				NhlSetSArg(&sargs[nargs++], NhlNtrYCoordPoints,
					tnew->tick.y_l_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,
					tnew->tick.y_l_irregular_points->len_dimensions[0]);
				break;
			case NhlLOG:
				tmpycoordpoints = (float*)NhlMalloc(
					(unsigned)sizeof(float)*3);
				tmpycoordpoints[2] = tnew->tick.y_l_data_top;
				tmpycoordpoints[0] = tnew->tick.y_l_data_bottom;
				tmpycoordpoints[1] = (float)pow(10.0,
					(double)(log10(tmpycoordpoints[0]) 
					+ log10(tmpycoordpoints[2]))/2.0);

				tnew->tick.ir_ylmin = tnew->tick.y_l_data_min;
				tnew->tick.ir_ylmax = tnew->tick.y_l_data_max;

				NhlSetSArg(&sargs[nargs++],
					NhlNtrYCoordPoints,tmpycoordpoints);
				NhlSetSArg(&sargs[nargs++],	
					NhlNtrYNumPoints,3);
				NhlSetSArg(&sargs[nargs++],
					NhlNtrYUseLog,1);
				break;
			case NhlLINEAR:
				tmpycoordpoints = (float*)NhlMalloc(
					(unsigned)sizeof(float)*3);
				tmpycoordpoints[2] =
					tnew->tick.y_l_data_top;
				tmpycoordpoints[0] =
					tnew->tick.y_l_data_bottom;
				tmpycoordpoints[1] = 
					(tmpycoordpoints[0] 
					+ tmpycoordpoints[2])/2.0;

				tnew->tick.ir_ylmin = tnew->tick.y_l_data_min;
				tnew->tick.ir_ylmax = tnew->tick.y_l_data_max;

				NhlSetSArg(&sargs[nargs++],
					NhlNtrYCoordPoints,tmpycoordpoints);
				NhlSetSArg(&sargs[nargs++],	
					NhlNtrYNumPoints,3);
				NhlSetSArg(&sargs[nargs++],
					NhlNtrYUseLog,0);
				break;
			case NhlTIME:
			case NhlGEOGRAPHIC:
				break;
			}
			switch(tnew->tick.x_b_style) {
			case NhlIRREGULAR:
				NhlSetSArg(&sargs[nargs++],
					NhlNtrXCoordPoints,
					tnew->tick.x_b_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],		
					NhlNtrXNumPoints,
					tnew->tick.x_b_irregular_points->len_dimensions[0]);
				break;
			case NhlLOG:
				tmpxcoordpoints = (float*)NhlMalloc(
					(unsigned)sizeof(float)*3);
				tmpxcoordpoints[2] =
					tnew->tick.x_b_data_right;
				tmpxcoordpoints[0] =
					tnew->tick.x_b_data_left;
				tmpxcoordpoints[1] = (float)pow(10.0,
					(double)(log10(tmpxcoordpoints[0]) 
					+ log10(tmpxcoordpoints[2]))/2.0);

				tnew->tick.ir_xbmin = tnew->tick.x_b_data_min;
				tnew->tick.ir_xbmax = tnew->tick.x_b_data_max;

				NhlSetSArg(&sargs[nargs++], NhlNtrXCoordPoints,
					tmpxcoordpoints);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);
				NhlSetSArg(&sargs[nargs++], NhlNtrXUseLog,1);
				break;
			case NhlLINEAR:
				tmpxcoordpoints = (float*)NhlMalloc(
					(unsigned)sizeof(float)*3);
				tmpxcoordpoints[2] = tnew->tick.x_b_data_right;
				tmpxcoordpoints[0] = tnew->tick.x_b_data_left;
				tmpxcoordpoints[1] = (tmpxcoordpoints[0] 
						+ tmpxcoordpoints[2])/2.0;

				tnew->tick.ir_xbmin = tnew->tick.x_b_data_min;
				tnew->tick.ir_xbmax = tnew->tick.x_b_data_max;

				NhlSetSArg(&sargs[nargs++], 
					NhlNtrXCoordPoints,tmpxcoordpoints);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);
				NhlSetSArg(&sargs[nargs++], NhlNtrXUseLog,0);
				break;
			case NhlTIME:
			case NhlGEOGRAPHIC:
				break;
			}
			trans_class = NhlirregularType2TransObjLayerClass;
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_b_data_left>tnew->tick.x_b_data_right ? 1 : 0));
			NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_l_data_bottom >tnew->tick.y_l_data_top? 1 : 0));
			NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tnew->tick.x_b_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tnew->tick.x_b_data_max);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tnew->tick.y_l_data_min);
			NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tnew->tick.y_l_data_max);
			NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,tnew->tick.y_l_tension);
			NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,tnew->tick.x_b_tension);
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
				break;
			}
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_b_data_left>tnew->tick.x_b_data_right ? 1 : 0));
			NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_l_data_bottom >tnew->tick.y_l_data_top? 1 : 0));
			trans_class = NhllogLinTransObjLayerClass;
		}
		
			strcpy(buffer,tnew->base.name);
			strcat(buffer,".Trans");
			NhlALCreate(&tmpid,buffer,trans_class,tnew->base.id,sargs,nargs);
			tnew->tick.xb_yl_trans_obj = _NhlGetLayer(tmpid);
	} else {
		tnew->tick.xb_yl_trans_obj = NULL;
	}

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
#if __STDC__
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
	NhlLayerClass trans_class = NULL;
	float *tmpycoordpoints;
        float *tmpxcoordpoints;
	char buffer[_NhlMAXFNAMELEN];
	int tmpid;
	NhlErrorTypes ret = NhlNOERROR;

	if((tnew->tick.y_r_on)||(tnew->tick.x_t_on)) {
		if((tnew->tick.y_r_style == NhlIRREGULAR)||
			(tnew->tick.x_t_style == NhlIRREGULAR)) {
			switch(tnew->tick.y_r_style) {
			case NhlIRREGULAR:
				NhlSetSArg(&sargs[nargs++], NhlNtrYCoordPoints,
					tnew->tick.y_r_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,
					tnew->tick.y_r_irregular_points->len_dimensions[0]);
				break;
			case NhlLOG:
				tmpycoordpoints = (float*)NhlMalloc(
					(unsigned)sizeof(float)*3);
				tmpycoordpoints[2] = tnew->tick.y_r_data_top;
				tmpycoordpoints[0] = tnew->tick.y_r_data_bottom;
				tmpycoordpoints[1] = (float)pow(10.0,
					(double)(log10(tmpycoordpoints[0]) 
					+ log10(tmpycoordpoints[2]))/2.0);

				tnew->tick.ir_yrmin = tnew->tick.y_r_data_min;
				tnew->tick.ir_yrmax = tnew->tick.y_r_data_max;

				NhlSetSArg(&sargs[nargs++], NhlNtrYCoordPoints,
					tmpycoordpoints);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);
				NhlSetSArg(&sargs[nargs++], NhlNtrYUseLog,1);
				break;
			case NhlLINEAR:
				tmpycoordpoints = (float*)NhlMalloc(
					(unsigned)sizeof(float)*3);
				tmpycoordpoints[2] = tnew->tick.y_r_data_top;
				tmpycoordpoints[0] = tnew->tick.y_r_data_bottom;
				tmpycoordpoints[1] = (tmpycoordpoints[0] 
						+ tmpycoordpoints[2])/2.0;

				tnew->tick.ir_yrmin = tnew->tick.y_r_data_min;
				tnew->tick.ir_yrmax = tnew->tick.y_r_data_max;

				NhlSetSArg(&sargs[nargs++], NhlNtrYCoordPoints,
					tmpycoordpoints);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);
				NhlSetSArg(&sargs[nargs++], NhlNtrYUseLog,0);
				break;
			case NhlTIME:
			case NhlGEOGRAPHIC:
				break;
			}
			switch(tnew->tick.x_t_style) {
			case NhlIRREGULAR:
				NhlSetSArg(&sargs[nargs++], NhlNtrXCoordPoints,
					tnew->tick.x_t_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
					tnew->tick.x_t_irregular_points->len_dimensions[0]);
				break;
			case NhlLOG:
				tmpxcoordpoints = (float*)NhlMalloc(
					(unsigned)sizeof(float)*3);
				tmpxcoordpoints[2] = tnew->tick.x_t_data_right;
				tmpxcoordpoints[0] = tnew->tick.x_t_data_left;
				tmpxcoordpoints[1] = (float)pow(10.0, (double)
						(log10(tmpxcoordpoints[0]) 
						+log10(tmpxcoordpoints[2]))/2.0);

				tnew->tick.ir_xtmin = tnew->tick.x_t_data_min;
				tnew->tick.ir_xtmax = tnew->tick.x_t_data_max;

				NhlSetSArg(&sargs[nargs++], NhlNtrXCoordPoints,
				tmpxcoordpoints);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);
				NhlSetSArg(&sargs[nargs++], NhlNtrXUseLog,1);
				break;
			case NhlLINEAR:
				tmpxcoordpoints = (float*)NhlMalloc(
					(unsigned)sizeof(float)*3);
				tmpxcoordpoints[2] =
					tnew->tick.x_t_data_right;
				tmpxcoordpoints[0] =
					tnew->tick.x_t_data_left;
				tmpxcoordpoints[1] = 
					(tmpxcoordpoints[0] 
					+ tmpxcoordpoints[2])/2.0;

				tnew->tick.ir_xtmin = tnew->tick.x_t_data_min;
				tnew->tick.ir_xtmax = tnew->tick.x_t_data_max;

				NhlSetSArg(&sargs[nargs++],
					NhlNtrXCoordPoints,tmpxcoordpoints);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);
				NhlSetSArg(&sargs[nargs++],
					NhlNtrXUseLog,0);
				break;
			case NhlTIME:
			case NhlGEOGRAPHIC:
				break;
			}
			trans_class = NhlirregularType2TransObjLayerClass;
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
				break;
			}
			trans_class = NhllogLinTransObjLayerClass;
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,(tnew->tick.x_t_data_left>tnew->tick.x_t_data_right ? 1 : 0));
			NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,(tnew->tick.y_r_data_bottom >tnew->tick.y_r_data_top? 1 : 0));
		}
		
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Trans");
		ret = NhlALCreate(&tmpid,buffer,trans_class,tnew->base.id,sargs,nargs);
		tnew->tick.xt_yr_trans_obj = _NhlGetLayer(tmpid);
	} else {
		tnew->tick.xt_yr_trans_obj = NULL;
	}
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
#if __STDC__
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

	float deltax,deltay;

	if(c_or_s == CREATE) {
		deltax = tnew->view.width/NHL_DEFAULT_VIEW_WIDTH;
		deltay = tnew->view.height/NHL_DEFAULT_VIEW_HEIGHT;
	} else {
		deltax = tnew->view.width/told->view.width;
		deltay = tnew->view.height/told->view.height;
	}


	if(((!_NhlArgIsSet(args,num_args,NhlNvpXF))||
		(!_NhlArgIsSet(args,num_args,NhlNvpYF))||
		(!_NhlArgIsSet(args,num_args,NhlNvpWidthF))||
		(!_NhlArgIsSet(args,num_args,NhlNvpHeightF)))
		&& (compare(deltax,1.0,4) != 0.0)||
		(compare(deltay,1.0,4) != 0.0)) {
/*
* Deal with TickMarkLabels first
*/
		if((!_NhlArgIsSet(args,num_args,NhlNtmXBLabelFontHeightF))&&
			(!_NhlArgIsSet(args,num_args,NhlNtmXBLabelFontAspectF))&&
			(!_NhlArgIsSet(args,num_args,NhlNtmXBLabelAngleF)))  {
/*
* All X axis ticks scale proportional to changes in X Axis to reduce possiblity
* of text overruns
*/
			tnew->tick.x_b_label_font_height *= deltax;
		}
		if((!_NhlArgIsSet(args,num_args,NhlNtmXTLabelFontHeightF))&&
			(!_NhlArgIsSet(args,num_args,NhlNtmXTLabelFontAspectF))&&
			(!_NhlArgIsSet(args,num_args,NhlNtmXTLabelAngleF)))  {
/*
* All X axis ticks scale proportional to changes in X Axis to reduce possiblity
* of text overruns
*/
			tnew->tick.x_t_label_font_height *= deltax;
		}
		if((!_NhlArgIsSet(args,num_args,NhlNtmYLLabelFontHeightF))&&
			(!_NhlArgIsSet(args,num_args,NhlNtmYLLabelFontAspectF))&&
			(!_NhlArgIsSet(args,num_args,NhlNtmYLLabelAngleF)))  {
/*
* All Y axis ticks scale proportional to changes in Y Axis to reduce possiblity
* of text overruns
*/
			tnew->tick.y_l_label_font_height *= deltay;
		}
		if((!_NhlArgIsSet(args,num_args,NhlNtmYRLabelFontHeightF))&&
			(!_NhlArgIsSet(args,num_args,NhlNtmYRLabelFontAspectF))&&
			(!_NhlArgIsSet(args,num_args,NhlNtmYRLabelAngleF)))  {
/*
* All Y axis ticks scale proportional to changes in Y Axis to reduce possiblity
* of text overruns
*/
			tnew->tick.y_r_label_font_height *= deltay;
		}
/*
* Now deal with scalling tick mark lengths
*/
		if(!_NhlArgIsSet(args,num_args,NhlNtmXBMajorLengthF)) {
/*
* X ticks are affected by changes is height
*/
			tnew->tick.x_b_major_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmXBMinorLengthF)){
			tnew->tick.x_b_minor_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmXBMajorOutwardLengthF)){
			tnew->tick.x_b_major_outward_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmXBMinorOutwardLengthF)){
			tnew->tick.x_b_minor_outward_length *= deltay;
		}

		if(!_NhlArgIsSet(args,num_args,NhlNtmXTMajorLengthF)) {
/*
* X ticks are affected by changes is height
*/
			tnew->tick.x_t_major_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmXTMinorLengthF)){
			tnew->tick.x_t_minor_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmXTMajorOutwardLengthF)){
			tnew->tick.x_t_major_outward_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmXTMinorOutwardLengthF)){
			tnew->tick.x_t_minor_outward_length *= deltay;
		}

		if(!_NhlArgIsSet(args,num_args,NhlNtmYRMajorLengthF)) {
/*
* X ticks are affected by changes is height
*/
			tnew->tick.y_r_major_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmYRMinorLengthF)){
			tnew->tick.y_r_minor_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmYRMajorOutwardLengthF)){
			tnew->tick.y_r_major_outward_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmYRMinorOutwardLengthF)){
			tnew->tick.y_r_minor_outward_length *= deltay;
		}

		if(!_NhlArgIsSet(args,num_args,NhlNtmYLMajorLengthF)) {
/*
* X ticks are affected by changes is height
*/
			tnew->tick.y_l_major_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmYLMinorLengthF)){
			tnew->tick.y_l_minor_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmYLMajorOutwardLengthF)){
			tnew->tick.y_l_major_outward_length *= deltay;
		}
		if(!_NhlArgIsSet(args,num_args,NhlNtmYLMinorOutwardLengthF)){
			tnew->tick.y_l_minor_outward_length *= deltay;
		}
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
#if __STDC__
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
        float *tmpycoordpoints;
        float *tmpxcoordpoints;
        NhlErrorTypes ret = NhlNOERROR;



	if((tnew->tick.y_l_on)||(tnew->tick.x_b_on)) {
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
* Following is the case when an IrregularType2TransObj is already in use
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
                                        	tnew->tick.y_l_irregular_points->data);
                                	NhlSetSArg(&sargs[nargs++],
						NhlNtrYNumPoints,
                                        	tnew->tick.y_l_irregular_points->len_dimensions[0]);
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
* Since an IrregularType2TransObj is being used careful attention must be
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

						tmpycoordpoints = 
							(float*)NhlMalloc(
							(unsigned)sizeof(float)*3);
						tmpycoordpoints[2] = 
							tnew->tick.y_l_data_top;
						tmpycoordpoints[0] = 
							tnew->tick.y_l_data_bottom;
						tmpycoordpoints[1] = (float)
							pow(10.0, (double)
							(log10(tmpycoordpoints[0])
							+ log10(tmpycoordpoints[2]))/2.0);
						tnew->tick.ir_ylmin =
							tnew->tick.y_l_data_min;
						tnew->tick.ir_ylmax =
							tnew->tick.y_l_data_max;

						NhlSetSArg(&sargs[nargs++],
						NhlNtrYCoordPoints,tmpycoordpoints);
						NhlSetSArg(&sargs[nargs++],
							NhlNtrYNumPoints,3);
						NhlSetSArg(&sargs[nargs++],
		                                        NhlNtrYUseLog,1);
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
	                                tmpycoordpoints = (float*)NhlMalloc(
	                                        (unsigned)sizeof(float)*3);
	                                tmpycoordpoints[2] =
	                                        tnew->tick.y_l_data_top;
	                                tmpycoordpoints[0] =
	                                        tnew->tick.y_l_data_bottom;
	                                tmpycoordpoints[1] =
	                                        (tmpycoordpoints[0]
	                                        + tmpycoordpoints[2])/2.0;

					tnew->tick.ir_ylmin =
						tnew->tick.y_l_data_min;
					tnew->tick.ir_ylmax =
						tnew->tick.y_l_data_max;

	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrYCoordPoints,tmpycoordpoints);
	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrYNumPoints,3);
	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrYUseLog,0);

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
                                        	tnew->tick.x_b_irregular_points->data);
                                	NhlSetSArg(&sargs[nargs++],
						NhlNtrXNumPoints,
                                        	tnew->tick.x_b_irregular_points->len_dimensions[0]);
					}
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

						tmpxcoordpoints = 
							(float*)NhlMalloc(
							(unsigned)sizeof(float)*3);
						tmpxcoordpoints[2] = 
							tnew->tick.x_b_data_right;
						tmpxcoordpoints[0] = 
							tnew->tick.x_b_data_left;
						tmpxcoordpoints[1] = (float)
							pow(10.0, (double)
							(log10(tmpxcoordpoints[0])
							+ log10(tmpxcoordpoints[2]))/2.0);
						tnew->tick.ir_xbmin =
							tnew->tick.x_b_data_min;
						tnew->tick.ir_xbmax =
							tnew->tick.x_b_data_max;

						NhlSetSArg(&sargs[nargs++],
						NhlNtrXCoordPoints,tmpxcoordpoints);
						NhlSetSArg(&sargs[nargs++],
							NhlNtrXNumPoints,3);
						NhlSetSArg(&sargs[nargs++],
		                                        NhlNtrXUseLog,1);
					}
					break;
				case NhlLINEAR:
					if((told->tick.x_b_style != NhlLINEAR)||
						(tnew->tick.ir_xbmin >
						tnew->tick.x_b_data_min)
						||(tnew->tick.ir_xbmax >
						tnew->tick.x_b_data_max)){
	                                tmpxcoordpoints = (float*)NhlMalloc(
	                                        (unsigned)sizeof(float)*3);
	                                tmpxcoordpoints[2] =
	                                        tnew->tick.x_b_data_right;
	                                tmpxcoordpoints[0] =
	                                        tnew->tick.x_b_data_left;
	                                tmpxcoordpoints[1] =
	                                        (tmpxcoordpoints[0]
	                                        + tmpxcoordpoints[2])/2.0;

					tnew->tick.ir_xbmin =
						tnew->tick.x_b_data_min;
					tnew->tick.ir_xbmax =
						tnew->tick.x_b_data_max;

	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrXCoordPoints,tmpxcoordpoints);
	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrXNumPoints,3);
	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrXUseLog,0);

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
	if((tnew->tick.y_r_on)||(tnew->tick.x_t_on)) {

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
                                        	tnew->tick.y_r_irregular_points->data);
                                	NhlSetSArg(&sargs[nargs++],
						NhlNtrYNumPoints,
                                        	tnew->tick.y_r_irregular_points->len_dimensions[0]);
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

						tmpycoordpoints = 
							(float*)NhlMalloc(
							(unsigned)sizeof(float)*3);
						tmpycoordpoints[2] = 
							tnew->tick.y_r_data_top;
						tmpycoordpoints[0] = 
							tnew->tick.y_r_data_bottom;
						tmpycoordpoints[1] = (float)
							pow(10.0, (double)
							(log10(tmpycoordpoints[0])
							+ log10(tmpycoordpoints[2]))/2.0);
						tnew->tick.ir_yrmin =
							tnew->tick.y_r_data_min;
						tnew->tick.ir_yrmax =
							tnew->tick.y_r_data_max;

						NhlSetSArg(&sargs[nargs++],
						NhlNtrYCoordPoints,tmpycoordpoints);
						NhlSetSArg(&sargs[nargs++],
							NhlNtrYNumPoints,3);
						NhlSetSArg(&sargs[nargs++],
		                                        NhlNtrYUseLog,1);
					}
					break;
				case NhlLINEAR:
					if((told->tick.y_r_style != NhlLINEAR)||
						(tnew->tick.ir_yrmin >
						tnew->tick.y_r_data_min)
						||(tnew->tick.ir_yrmax >
						tnew->tick.y_r_data_max)){
	                                tmpycoordpoints = (float*)NhlMalloc(
	                                        (unsigned)sizeof(float)*3);
	                                tmpycoordpoints[2] =
	                                        tnew->tick.y_r_data_top;
	                                tmpycoordpoints[0] =
	                                        tnew->tick.y_r_data_bottom;
	                                tmpycoordpoints[1] =
	                                        (tmpycoordpoints[0]
	                                        + tmpycoordpoints[2])/2.0;

					tnew->tick.ir_yrmin =
						tnew->tick.y_r_data_min;
					tnew->tick.ir_yrmax =
						tnew->tick.y_r_data_max;

	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrYCoordPoints,tmpycoordpoints);
	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrYNumPoints,3);
	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrYUseLog,0);

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
                                        	tnew->tick.x_t_irregular_points->data);
                                	NhlSetSArg(&sargs[nargs++],
						NhlNtrXNumPoints,
                                        	tnew->tick.x_t_irregular_points->len_dimensions[0]);
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

						tmpxcoordpoints = 
							(float*)NhlMalloc(
							(unsigned)sizeof(float)*3);
						tmpxcoordpoints[2] = 
							tnew->tick.x_t_data_right;
						tmpxcoordpoints[0] = 
							tnew->tick.x_t_data_left;
						tmpxcoordpoints[1] = (float)
							pow(10.0, (double)
							(log10(tmpxcoordpoints[0])
							+ log10(tmpxcoordpoints[2]))/2.0);
						tnew->tick.ir_xtmin =
							tnew->tick.x_t_data_min;
						tnew->tick.ir_xtmax =
							tnew->tick.x_t_data_max;

						NhlSetSArg(&sargs[nargs++],
						NhlNtrXCoordPoints,tmpxcoordpoints);
						NhlSetSArg(&sargs[nargs++],
							NhlNtrXNumPoints,3);
						NhlSetSArg(&sargs[nargs++],
		                                        NhlNtrXUseLog,1);
					}
					break;
				case NhlLINEAR:
					if((told->tick.x_t_style != NhlLINEAR)||
						(tnew->tick.ir_xtmin >
						tnew->tick.x_t_data_min)
						||(tnew->tick.ir_xtmax >
						tnew->tick.x_t_data_max)){
	                                tmpxcoordpoints = (float*)NhlMalloc(
	                                        (unsigned)sizeof(float)*3);
	                                tmpxcoordpoints[2] =
	                                        tnew->tick.x_t_data_right;
	                                tmpxcoordpoints[0] =
	                                        tnew->tick.x_t_data_left;
	                                tmpxcoordpoints[1] =
	                                        (tmpxcoordpoints[0]
	                                        + tmpxcoordpoints[2])/2.0;

					tnew->tick.ir_xtmin =
						tnew->tick.x_t_data_min;
					tnew->tick.ir_xtmax =
						tnew->tick.x_t_data_max;

	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrXCoordPoints,tmpxcoordpoints);
	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrXNumPoints,3);
	                                NhlSetSArg(&sargs[nargs++],
	                                        NhlNtrXUseLog,0);

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
                                break;
                        }
			ret = NhlALSetValues(tnew->tick.xt_yr_trans_obj->base.id
				,sargs,nargs);

		}

	} 
	return ret;
}
