/*
 *      $Id: TickMark.h,v 1.21 2001-12-05 00:19:05 dbrown Exp $
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
 *	Date:		Wed Dec 2 13:55:30 MST 1992
 *
 *	Description:	
 */
#ifndef _NTickMark_h
#define	_NTickMark_h

#include <ncarg/hlu/View.h>

typedef enum { NhlAUTOMATIC, NhlMANUAL, NhlEXPLICIT } NhlTickMarkMode;
typedef enum { NhlLOG, NhlLINEAR, NhlIRREGULAR, NhlGEOGRAPHIC, NhlTIME }
							NhlTickMarkStyle;

#define NhlTTickMarkMode 	"TickMarkMode"
#define NhlTTickMarkStyle	"TickMarkStyle"

#define NhlNtmEqualizeXYSizes 	"tmEqualizeXYSizes"
#define NhlCtmEqualizeXYSizes 	"TmEqualizeXYSizes"

#define NhlNtmLabelAutoStride	"tmLabelAutoStride"

#define NhlNtmSciNoteCutoff	"tmSciNoteCutoff"
#define NhlCtmSciNoteCutoff	"TmSciNoteCutoff"

#define NhlNtmXBAutoPrecision	"tmXBAutoPrecision"
#define NhlNtmYLAutoPrecision	"tmYLAutoPrecision"
#define NhlNtmXTAutoPrecision	"tmXTAutoPrecision"
#define NhlNtmYRAutoPrecision	"tmYRAutoPrecision"
#define NhlCtmAutoPrecision	"TmAutoPrecision"

#define NhlNtmXBFormat		"tmXBFormat"
#define NhlNtmYLFormat		"tmYLFormat"
#define NhlNtmXTFormat		"tmXTFormat"
#define NhlNtmYRFormat		"tmYRFormat"

/*
* X Axis general Control resources for Bottom and Top
*/

#define NhlNtmXUseBottom	"tmXUseBottom"
#define NhlNtmXBOn		"tmXBOn"
#define NhlNtmXTOn		"tmXTOn"
#define NhlNtmXBLabelsOn	"tmXBLabelsOn"
#define NhlNtmXTLabelsOn	"tmXTLabelsOn"
#define NhlNtmXBBorderOn	"tmXBBorderOn"
#define NhlNtmXTBorderOn	"tmXTBorderOn"
#define NhlNtmXBMode		"tmXBMode"
#define NhlNtmXTMode		"tmXTMode"
#define NhlNtmXBStyle 		"tmXBStyle"
#define NhlNtmXTStyle 		"tmXTStyle"
#define NhlNtmXBIrrTensionF	"tmXBIrrTensionF"
#define NhlNtmXTIrrTensionF	"tmXTIrrTensionF"
#define NhlNtmXBPrecision	"tmXBPrecision"
#define NhlNtmXTPrecision	"tmXTPrecision"

#define NhlNtmBorderThicknessF "tmBorderThicknessF"
#define NhlNtmBorderLineColor "tmBorderLineColor"


#define NhlNtmXMajorGrid		"tmXMajorGrid"
#define NhlNtmXMinorGrid		"tmXMinorGrid"
#define NhlNtmXMajorGridThicknessF	"tmXMajorGridThicknessF"
#define NhlNtmXMajorGridLineColor	"tmXMajorGridLineColor"
#define NhlNtmXMajorGridLineDashPattern	"tmXMajorGridLineDashPattern"
#define NhlNtmXMinorGridThicknessF	"tmXMinorGridThicknessF"
#define NhlNtmXMinorGridLineColor	"tmXMinorGridLineColor"
#define NhlNtmXMinorGridLineDashPattern	"tmXMinorGridLineDashPattern"

#define NhlNtmXBMinorPerMajor	"tmXBMinorPerMajor"
#define NhlNtmXTMinorPerMajor	"tmXTMinorPerMajor"
#define NhlNtmXBMinorOn		"tmXBMinorOn"
#define NhlNtmXTMinorOn		"tmXTMinorOn"

#define NhlNtmXBLabelStride		"tmXBLabelStride"
#define NhlNtmXTLabelStride		"tmXTLabelStride"
/* 
* X Axis Bottom Resources 
*/

#define NhlNtmXBDataLeftF	"tmXBDataLeftF"
#define NhlNtmXBDataRightF	"tmXBDataRightF"
#define NhlNtmXBTickStartF	"tmXBTickStartF"
#define NhlNtmXBTickEndF	"tmXBTickEndF"
#define NhlNtmXBMaxTicks	"tmXBMaxTicks"

/*
* used for computing irregular tranformation
*/
#define NhlNtmXBIrregularPoints	"tmXBIrregularPoints"
#define NhlCtmXBIrregularPoints	"TmXBIrregularPoints"

/*
* X Axis Bottom Resources only used for manual mode
*/
#define NhlNtmXBTickSpacingF	"tmXBTickSpacingF"
#define NhlNtmXBSpacingType	"tmXBSpacingType"

/*
* XAxis Bottom Resources only used for explicit mode
*/
#define NhlNtmXBValues		"tmXBValues"
#define NhlNtmXBMinorValues		"tmXBMinorValues"
#define NhlNtmXBLabels		"tmXBLabels"

/*
* X Axis Bottom Resources for line and text attributes
*/

#define NhlNtmXBMajorThicknessF		"tmXBMajorThicknessF"
#define NhlNtmXBMajorLineColor		"tmXBMajorLineColor"
#define NhlNtmXBMajorLengthF		"tmXBMajorLengthF"
#define NhlNtmXBMajorOutwardLengthF	"tmXBMajorOutwardLengthF"

#define NhlNtmXBMinorThicknessF		"tmXBMinorThicknessF"
#define NhlNtmXBMinorLineColor		"tmXBMinorLineColor"
#define NhlNtmXBMinorLengthF		"tmXBMinorLengthF"
#define NhlNtmXBMinorOutwardLengthF	"tmXBMinorOutwardLengthF"


#define NhlNtmXBLabelFont		"tmXBLabelFont"
#define NhlNtmXBLabelFontHeightF	"tmXBLabelFontHeightF"
#define NhlNtmXBLabelFontColor		"tmXBLabelFontColor"
#define NhlNtmXBLabelFontAspectF	"tmXBLabelFontAspectF"
#define NhlNtmXBLabelJust		"tmXBLabelJust"
#define NhlNtmXBLabelAngleF		"tmXBLabelAngleF"
#define NhlNtmXBLabelDirection		"tmXBLabelDirection"
#define NhlNtmXBLabelFuncCode		"tmXBLabelFuncCode"
#define NhlNtmXBLabelFontThicknessF	"tmXBLabelFontThicknessF"
#define NhlNtmXBLabelFontQuality	"tmXBLabelFontQuality"
#define NhlNtmXBLabelConstantSpacingF	"tmXBLabelConstantSpacingF"

#define NhlNtmXBLabelDeltaF		"tmXBLabelDeltaF"

#define NhlNtmXBMaxLabelLenF		"tmXBMaxLabelLenF"
#define NhlNtmXBMinLabelSpacingF	"tmXBMinLabelSpacingF"

/* 
* X Axis Top Resources 
*/

#define NhlNtmXTTickStartF	"tmXTTickStartF"
#define NhlNtmXTTickEndF	"tmXTTickEndF"
#define NhlNtmXTDataLeftF	"tmXTDataLeftF"
#define NhlNtmXTDataRightF	"tmXTDataRightF"
#define NhlNtmXTMaxTicks	"tmXTMaxTicks"

/*
* used for computing irregular tranformation
*/
#define NhlNtmXTIrregularPoints	"tmXTIrregularPoints"
#define NhlCtmXTIrregularPoints	"TmXTIrregularPoints"

/*
* X Axis Top Resources only used for manual mode
*/
#define NhlNtmXTTickSpacingF	"tmXTTickSpacingF"
#define NhlNtmXTSpacingType	"tmXTSpacingType"

/*
* XAxis Top Resources only used for explicit mode
*/
#define NhlNtmXTValues		"tmXTValues"
#define NhlNtmXTMinorValues		"tmXTMinorValues"
#define NhlNtmXTLabels		"tmXTLabels"

/*
* X Axis Top Resources for line and text attributes
*/

#define NhlNtmXTMajorThicknessF		"tmXTMajorThicknessF"
#define NhlNtmXTMajorLineColor		"tmXTMajorLineColor"
#define NhlNtmXTMajorLengthF		"tmXTMajorLengthF"
#define NhlNtmXTMajorOutwardLengthF	"tmXTMajorOutwardLengthF"

#define NhlNtmXTMinorThicknessF		"tmXTMinorThicknessF"
#define NhlNtmXTMinorLineColor		"tmXTMinorLineColor"
#define NhlNtmXTMinorLengthF		"tmXTMinorLengthF"
#define NhlNtmXTMinorOutwardLengthF	"tmXTMinorOutwardLengthF"


#define NhlNtmXTLabelFont		"tmXTLabelFont"
#define NhlNtmXTLabelFontHeightF	"tmXTLabelFontHeightF"
#define NhlNtmXTLabelFontColor		"tmXTLabelFontColor"
#define NhlNtmXTLabelFontAspectF	"tmXTLabelFontAspectF"
#define NhlNtmXTLabelJust		"tmXTLabelJust"
#define NhlNtmXTLabelAngleF		"tmXTLabelAngleF"
#define NhlNtmXTLabelDirection		"tmXTLabelDirection"
#define NhlNtmXTLabelFuncCode		"tmXTLabelFuncCode"
#define NhlNtmXTLabelFontThicknessF	"tmXTLabelFontThicknessF"
#define NhlNtmXTLabelFontQuality	"tmXTLabelFontQuality"
#define NhlNtmXTLabelConstantSpacingF	"tmXTLabelConstantSpacingF"

#define NhlNtmXTLabelDeltaF		"tmXTLabelDeltaF"

#define NhlNtmXTMaxLabelLenF		"tmXTMaxLabelLenF"
#define NhlNtmXTMinLabelSpacingF	"tmXTMinLabelSpacingF"



/*
* Y Axis general Control resources for Left and Right 
*/

#define NhlNtmYUseLeft		"tmYUseLeft"
#define NhlNtmYLOn		"tmYLOn"
#define NhlNtmYROn		"tmYROn"
#define NhlNtmYLLabelsOn	"tmYLLabelsOn"
#define NhlNtmYRLabelsOn	"tmYRLabelsOn"
#define NhlNtmYLBorderOn	"tmYLBorderOn"
#define NhlNtmYRBorderOn	"tmYRBorderOn"
#define NhlNtmYLMode		"tmYLMode"
#define NhlNtmYRMode		"tmYRMode"
#define NhlNtmYLStyle 		"tmYLStyle"
#define NhlNtmYRStyle 		"tmYRStyle"
#define NhlNtmYLIrrTensionF	"tmYLIrrTensionF"
#define NhlNtmYRIrrTensionF	"tmYRIrrTensionF"
#define NhlNtmYLPrecision	"tmYLPrecision"
#define NhlNtmYRPrecision	"tmYRPrecision"


#define NhlNtmYMajorGrid		"tmYMajorGrid"
#define NhlNtmYMinorGrid		"tmYMinorGrid"
#define NhlNtmYMajorGridThicknessF	"tmYMajorGridThicknessF"
#define NhlNtmYMajorGridLineColor	"tmYMajorGridLineColor"
#define NhlNtmYMajorGridLineDashPattern	"tmYMajorGridLineDashPattern"
#define NhlNtmYMinorGridThicknessF	"tmYMinorGridThicknessF"
#define NhlNtmYMinorGridLineColor	"tmYMinorGridLineColor"
#define NhlNtmYMinorGridLineDashPattern	"tmYMinorGridLineDashPattern"

#define NhlNtmYLMinorPerMajor	"tmYLMinorPerMajor"
#define NhlNtmYRMinorPerMajor	"tmYRMinorPerMajor"
#define NhlNtmYLMinorOn		"tmYLMinorOn"
#define NhlNtmYRMinorOn		"tmYRMinorOn"

#define NhlNtmYLLabelStride		"tmYLLabelStride"
#define NhlNtmYRLabelStride		"tmYRLabelStride"
/* 
* X Axis LeftResources 
*/

#define NhlNtmYLDataTopF	"tmYLDataTopF"
#define NhlNtmYLDataBottomF	"tmYLDataBottomF"
#define NhlNtmYLTickStartF	"tmYLTickStartF"
#define NhlNtmYLTickEndF	"tmYLTickEndF"
#define NhlNtmYLMaxTicks	"tmYLMaxTicks"

/*
* used for computing irregular tranformation
*/
#define NhlNtmYLIrregularPoints	"tmYLIrregularPoints"
#define NhlCtmYLIrregularPoints	"TmYLIrregularPoints"
/*
* X Axis Left Resources only used for manual mode
*/
#define NhlNtmYLTickSpacingF	"tmYLTickSpacingF"
#define NhlNtmYLSpacingType	"tmYLSpacingType"

/*
* XAxis Left Resources only used for explicit mode
*/
#define NhlNtmYLValues		"tmYLValues"
#define NhlNtmYLMinorValues		"tmYLMinorValues"
#define NhlNtmYLLabels		"tmYLLabels"

/*
* Y Axis Left Resources for line and text attributes
*/

#define NhlNtmYLMajorThicknessF		"tmYLMajorThicknessF"
#define NhlNtmYLMajorLineColor		"tmYLMajorLineColor"
#define NhlNtmYLMajorLengthF		"tmYLMajorLengthF"
#define NhlNtmYLMajorOutwardLengthF	"tmYLMajorOutwardLengthF"

#define NhlNtmYLMinorThicknessF		"tmYLMinorThicknessF"
#define NhlNtmYLMinorLineColor		"tmYLMinorLineColor"
#define NhlNtmYLMinorLengthF		"tmYLMinorLengthF"
#define NhlNtmYLMinorOutwardLengthF	"tmYLMinorOutwardLengthF"


#define NhlNtmYLLabelFont		"tmYLLabelFont"
#define NhlNtmYLLabelFontHeightF	"tmYLLabelFontHeightF"
#define NhlNtmYLLabelFontColor		"tmYLLabelFontColor"
#define NhlNtmYLLabelFontAspectF	"tmYLLabelFontAspectF"
#define NhlNtmYLLabelJust		"tmYLLabelJust"
#define NhlNtmYLLabelAngleF		"tmYLLabelAngleF"
#define NhlNtmYLLabelDirection		"tmYLLabelDirection"
#define NhlNtmYLLabelFuncCode		"tmYLLabelFuncCode"
#define NhlNtmYLLabelFontThicknessF	"tmYLLabelFontThicknessF"
#define NhlNtmYLLabelFontQuality	"tmYLLabelFontQuality"
#define NhlNtmYLLabelConstantSpacingF	"tmYLLabelConstantSpacingF"

#define NhlNtmYLLabelDeltaF		"tmYLLabelDeltaF"

#define NhlNtmYLMaxLabelLenF		"tmYLMaxLabelLenF"
#define NhlNtmYLMinLabelSpacingF	"tmYLMinLabelSpacingF"


/* 
* Y Axis Right Resources 
*/

#define NhlNtmYRTickStartF	"tmYRTickStartF"
#define NhlNtmYRTickEndF	"tmYRTickEndF"
#define NhlNtmYRDataTopF	"tmYRDataTopF"
#define NhlNtmYRDataBottomF	"tmYRDataBottomF"
#define NhlNtmYRMaxTicks	"tmYRMaxTicks"

/*
* used for computing irregular tranformation
*/
#define NhlNtmYRIrregularPoints	"tmYRIrregularPoints"
#define NhlCtmYRIrregularPoints	"TmYRIrregularPoints"
/*
* Y Axis Right Resources only used for manual mode
*/
#define NhlNtmYRTickSpacingF	"tmYRTickSpacingF"
#define NhlNtmYRSpacingType	"tmYRSpacingType"

/*
* XAxis Right Resources only used for explicit mode
*/
#define NhlNtmYRValues		"tmYRValues"
#define NhlNtmYRMinorValues		"tmYRMinorValues"
#define NhlNtmYRLabels		"tmYRLabels"

/*
* X Axis Right Resources for line and text attributes
*/

#define NhlNtmYRMajorThicknessF		"tmYRMajorThicknessF"
#define NhlNtmYRMajorLineColor		"tmYRMajorLineColor"
#define NhlNtmYRMajorLengthF		"tmYRMajorLengthF"
#define NhlNtmYRMajorOutwardLengthF	"tmYRMajorOutwardLengthF"

#define NhlNtmYRMinorThicknessF		"tmYRMinorThicknessF"
#define NhlNtmYRMinorLineColor		"tmYRMinorLineColor"
#define NhlNtmYRMinorLengthF		"tmYRMinorLengthF"
#define NhlNtmYRMinorOutwardLengthF	"tmYRMinorOutwardLengthF"


#define NhlNtmYRLabelFont		"tmYRLabelFont"
#define NhlNtmYRLabelFontHeightF	"tmYRLabelFontHeightF"
#define NhlNtmYRLabelFontColor		"tmYRLabelFontColor"
#define NhlNtmYRLabelFontAspectF	"tmYRLabelFontAspectF"
#define NhlNtmYRLabelJust		"tmYRLabelJust"
#define NhlNtmYRLabelAngleF		"tmYRLabelAngleF"
#define NhlNtmYRLabelDirection		"tmYRLabelDirection"
#define NhlNtmYRLabelFuncCode		"tmYRLabelFuncCode"
#define NhlNtmYRLabelFontThicknessF	"tmYRLabelFontThicknessF"
#define NhlNtmYRLabelFontQuality	"tmYRLabelFontQuality"
#define NhlNtmYRLabelConstantSpacingF	"tmYRLabelConstantSpacingF"

#define NhlNtmYRLabelDeltaF		"tmYRLabelDeltaF"

#define NhlNtmYRMaxLabelLenF		"tmYRMaxLabelLenF"
#define NhlNtmYRMinLabelSpacingF	"tmYRMinLabelSpacingF"

/*
* -------------> Still need to come up with resources for 
* IRREGULAR STYLE
* LOG STYLE
* GEOGRAPHIC STYLE
* TIME STYLES <-------------
*/

/*
* Resource Class definitions
*
* All line/text attributes share one class.
*/
#define NhlCtmMajorThicknessesF		"TmMajorThicknessesF"
#define NhlCtmMajorLengthsF		"TmMajorLengthsF"
#define NhlCtmMajorOutwardLengthsF	"TmMajorOutwardLengthsF"

#define NhlCtmMinorThicknessesF		"TmMinorThicknessesF"
#define NhlCtmMinorLengthsF		"TmMinorLengthsF"
#define NhlCtmMinorOutwardLengthsF	"TmMinorOutwardLengthsF"

#define NhlCtmMajorGridThicknessesF	"TmMajorGridThicknessesF"
#define NhlCtmMajorGridLineDashPatterns	"TmMajorGridLineDashPatterns"
#define NhlCtmMinorGridThicknessesF	"TmMinorGridThicknessesF"
#define NhlCtmMinorGridLineDashPatterns	"TmMinorGridLineDashPatterns"

#define NhlCtmPrecisions		"TmPrecisions"

/*
* The Following shared amongst axis
*/


#define NhlCtmXMinorPerMajor	"TmXMinorPerMajor"
#define NhlCtmYMinorPerMajor	"TmYMinorPerMajor"

/*
* Normal Class names
*/


#define NhlCtmXUseBottom	"TmXUseBottom"
#define NhlCtmXBOn		"TmXBOn"
#define NhlCtmXTOn		"TmXTOn"
#define NhlCtmXBLabelsOn	"TmXBLabelsOn"
#define NhlCtmXTLabelsOn	"TmXTLabelsOn"
#define NhlCtmXBMode		"TmXBMode"
#define NhlCtmXTMode		"TmXTMode"
#define NhlCtmXBStyle 		"TmXBStyle"
#define NhlCtmXTStyle 		"TmXTStyle"
#define NhlCtmXBIrrTensionF	"TmXBIrrTensionF"
#define NhlCtmXTIrrTensionF	"TmXTIrrTensionF"
#define NhlCtmXMajorGrid	"TmXMajorGrid"
#define NhlCtmXMinorGrid	"TmXMinorGrid"
#define NhlCtmXBMinorOn		"TmXBMinorOn"
#define NhlCtmXTMinorOn		"TmXTMinorOn"
#define NhlCtmXBLabelStride	"TmXBLabelStride"
#define NhlCtmXBDataLeftF	"TmXBDataLeftF"
#define NhlCtmXBDataRightF	"TmXBDataRightF"
#define NhlCtmXBTickStartF	"TmXBTickStartF"
#define NhlCtmXBTickEndF	"TmXBTickEndF"
#define NhlCtmXBMaxTicks	"TmXBMaxTicks"
#define NhlCtmXBTickSpacingF	"TmXBTickSpacingF"
#define NhlCtmXBSpacingType	"TmXBSpacingType"
#define NhlCtmXBValues		"TmXBValues"
#define NhlCtmXBMinorValues	"TmXBMinorValues"
#define NhlCtmXBLabels		"TmXBLabels"
#define NhlCtmXBLabelDeltaF	"TmXBLabelDeltaF"
#define NhlCtmXBMaxLabelLenF		"TmXBMaxLabelLenF"
#define NhlCtmXBMinLabelSpacingF	"TmXBMinLabelSpacingF"
#define NhlCtmXTLabelStride	"TmXTLabelStride"
#define NhlCtmXTDataLeftF	"TmXTDataLeftF"
#define NhlCtmXTDataRightF	"TmXTDataRightF"
#define NhlCtmXTTickStartF	"TmXTTickStartF"
#define NhlCtmXTTickEndF	"TmXTTickEndF"
#define NhlCtmXTMaxTicks	"TmXTMaxTicks"
#define NhlCtmXTTickSpacingF	"TmXTTickSpacingF"
#define NhlCtmXTSpacingType	"TmXTSpacingType"
#define NhlCtmXTValues		"TmXTValues"
#define NhlCtmXTMinorValues	"TmXTMinorValues"
#define NhlCtmXTLabels		"TmXTLabels"
#define NhlCtmXTLabelDeltaF	"TmXLabelDeltaF"
#define NhlCtmXTMaxLabelLenF		"TmXTMaxLabelLenF"
#define NhlCtmXTMinLabelSpacingF	"TmXTMinLabelSpacingF"
#define NhlCtmYUseLeft		"TmYUseLeft"
#define NhlCtmYLOn		"TmYLOn"
#define NhlCtmYROn		"TmYROn"
#define NhlCtmYLLabelsOn	"TmYLLabelsOn"
#define NhlCtmYRLabelsOn	"TmYRLabelsOn"
#define NhlCtmYLMode		"TmYLMode"
#define NhlCtmYRMode		"TmYRMode"
#define NhlCtmYLStyle 		"TmYLStyle"
#define NhlCtmYRStyle 		"TmYRStyle"
#define NhlCtmYLIrrTensionF	"TmYLIrrTensionF"
#define NhlCtmYRIrrTensionF	"TmYRIrrTensionF"
#define NhlCtmYMajorGrid	"TmYMajorGrid"
#define NhlCtmYMinorGrid	"TmYMinorGrid"
#define NhlCtmYLMinorOn		"TmYLMinorOn"
#define NhlCtmYRMinorOn		"TmYRMinorOn"
#define NhlCtmYLLabelStride	"TmYLLabelStride"
#define NhlCtmYLDataTopF	"TmYLDataTopF"
#define NhlCtmYLDataBottomF	"TmYLDataBottomF"
#define NhlCtmYLTickStartF	"TmYLTickStartF"
#define NhlCtmYLTickEndF	"TmYLTickEndF"
#define NhlCtmYLMaxTicks	"TmYLMaxTicks"
#define NhlCtmYLTickSpacingF	"TmYLTickSpacingF"
#define NhlCtmYLSpacingType	"TmYLSpacingType"
#define NhlCtmYLValues		"TmYLValues"
#define NhlCtmYLMinorValues	"TmYLMinorValues"
#define NhlCtmYLLabels		"TmYLLabels"
#define NhlCtmYLLabelDeltaF	"TmYLLabelDeltaF"
#define NhlCtmYLMaxLabelLenF		"TmYLMaxLabelLenF"
#define NhlCtmYLMinLabelSpacingF	"TmYLMinLabelSpacingF"
#define NhlCtmYRLabelStride	"TmYRLabelStride"
#define NhlCtmYRDataTopF	"TmYRDataTopF"
#define NhlCtmYRDataBottomF	"TmYRDataBottomF"
#define NhlCtmYRTickStartF	"TmYRTickStartF"
#define NhlCtmYRTickEndF	"TmYRTickEndF"
#define NhlCtmYRMaxTicks	"TmYRMaxTicks"
#define NhlCtmYRTickSpacingF	"TmYRTickSpacingF"
#define NhlCtmYRSpacingType	"TmYRSpacingType"
#define NhlCtmYRValues		"TmYRValues"
#define NhlCtmYRMinorValues	"TmYRMinorValues"
#define NhlCtmYRLabels		"TmYRLabels"
#define NhlCtmYRLabelDeltaF	"TmYRLabelDeltaF"
#define NhlCtmYRMaxLabelLenF		"TmYRMaxLabelLenF"
#define NhlCtmYRMinLabelSpacingF	"TmYRMinLabelSpacingF"

/*
 * These class resources have been added
 */
#define NhlCtmXBLabelJust	"TmXBLabelJust"
#define NhlCtmXTLabelJust	"TmXTLabelJust"
#define NhlCtmYLLabelJust	"TmYLLabelJust"
#define NhlCtmYRLabelJust	"TmYRLabelJust"

/*
 * These class resources have been eliminated
 */
#if 0
#define NhlCtmFormat			"TmFormat"
#define NhlCtmXBBorderOn		"TmXBBorderOn"
#define NhlCtmXTBorderOn		"TmXTBorderOn"
#define NhlCtmYLBorderOn		"TmYLBorderOn"
#define NhlCtmYRBorderOn		"TmYRBorderOn"
#define NhlCtmMajorLineColors		"TmMajorLineColors"
#define NhlCtmMinorLineColors		"TmMinorLineColors"
#define NhlCtmMajorGridLineColors	"TmMajorGridLineColors"
#define NhlCtmMinorGridLineColors	"TmMinorGridLineColors"
#define NhlCtmXLabelDirections		"TmXLabelDirections"
#define NhlCtmYLabelDirections		"TmYLabelDirections"

#define NhlCtmLabelFontHeightsF		"TmLabelFontHeightsF"
#define NhlCtmLabelFontColors		"TmLabelFontColors"
#define NhlCtmLabelFontAspectsF		"TmLabelFontAspectsF"
#define NhlCtmLabelFontThicknessesF	"TmLabelFontThicknessesF"
#define NhlCtmLabelFontQualities        "TmLabelFontQualities"
#define NhlCtmLabelConstantSpacingsF	"TmLabelConstantSpacingsF"
#define NhlCtmBorderThicknessF		"TmBorderTicknessF"
#define NhlCtmBorderLineColor 		"TmBorderLineColor"
#define NhlCtmXLabelFuncCodes		"TmXLabelFuncCodes"
#define NhlCtmXLabelAnglesF		"TmXLabelAnglesF"
#define NhlCtmXLabelJusts		"TmXLabelJusts"
#define NhlCtmYLabelFuncCodes		"TmYLabelFuncCodes"
#define NhlCtmYLabelAnglesF		"TmYLabelAnglesF"
#define NhlCtmYLabelJusts		"TmYLabelJusts"

#endif

extern NhlClass NhltickMarkClass;

#endif /*_NTickMark_h */
