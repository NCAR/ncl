/*
 *      $Id: LabelBar.h,v 1.17 2005-08-24 21:12:13 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LabelBar.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	LabelBar public header file
 */
#ifndef _NLabelBar_h
#define _NLabelBar_h

#include <ncarg/hlu/View.h>
#include <ncarg/hlu/MultiText.h>


/* label alignment */

typedef enum _NhllbLabelAlignmentMode {
	NhlBOXCENTERS =0, NhlINTERIOREDGES, NhlEXTERNALEDGES
} NhllbLabelAlignmentMode;

#define NhlTlbLabelAlignmentMode "lbLabelAlignmentMode"

typedef enum _NhllbBoxSizingMode {
	NhlUNIFORMSIZING = 0, NhlEXPLICITSIZING
} NhllbBoxSizingMode;

#define NhlTlbBoxSizingMode "lbBoxSizingMode"

typedef enum _NhllbBoxEndCapStyle {
    NhlRECTANGLEENDS = 0, NhlTRIANGLELOWEND, NhlTRIANGLEHIGHEND, NhlTRIANGLEBOTHENDS
} NhllbBoxEndCapStyle;

#define NhlTlbBoxEndCapStyle "lbBoxEndCapStyle"

/* Instance resources */

#define NhlNlbLabelBarOn		"lbLabelBarOn"
#define NhlNlbOrientation		"lbOrientation"
#define NhlNlbJustification		"lbJustification"
#define NhlNlbBoxMajorExtentF		"lbBoxMajorExtentF"
#define NhlNlbBoxMinorExtentF		"lbBoxMinorExtentF"
#define NhlNlbBoxCount			"lbBoxCount"
#define NhlNlbBoxSizing			"lbBoxSizing"

#define NhlNlbAutoManage		"lbAutoManage"
#define NhlNlbLabelOffsetF		"lbLabelOffsetF"
#define NhlNlbTitleOffsetF		"lbTitleOffsetF"
#define NhlNlbLeftMarginF		"lbLeftMarginF"
#define NhlNlbRightMarginF		"lbRightMarginF"
#define NhlNlbBottomMarginF		"lbBottomMarginF"
#define NhlNlbTopMarginF		"lbTopMarginF"
#define NhlNlbMarginMode		"lbMarginMode"

/* arrays */
#define NhlNlbMonoFillColor		"lbMonoFillColor"
#define NhlNlbFillColor			"lbFillColor"
#define NhlNlbFillColors		"lbFillColors"
#define NhlNlbMonoFillPattern		"lbMonoFillPattern"
#define NhlNlbFillPattern		"lbFillPattern"
#define NhlNlbFillPatterns		"lbFillPatterns"
#define NhlNlbMonoFillScale		"lbMonoFillScale"
#define NhlNlbFillScaleF		"lbFillScaleF"
#define NhlNlbFillScales		"lbFillScales"
#define NhlNlbFillDotSizeF		"lbFillDotSizeF"
#define NhlNlbLabelStrings		"lbLabelStrings"
#define NhlNlbBoxFractions		"lbBoxFractions"

/*end of arrays */

#define NhlNlbLabelAutoStride		"lbLabelAutoStride"
#define NhlNlbLabelsOn			"lbLabelsOn"
#define NhlNlbLabelPosition		"lbLabelPosition"
#define NhlNlbLabelAngleF		"lbLabelAngleF"
#define NhlNlbLabelJust			"lbLabelJust"
#define NhlNlbLabelDirection		"lbLabelDirection"
#define NhlNlbLabelAlignment		"lbLabelAlignment"
#define NhlNlbLabelFont			"lbLabelFont"
#define NhlNlbLabelFontColor		"lbLabelFontColor"
#define NhlNlbLabelFontHeightF		"lbLabelFontHeightF"
#define NhlNlbLabelFontAspectF		"lbLabelFontAspectF" /* height/width */
#define NhlNlbLabelFontThicknessF	"lbLabelFontThicknessF"
#define NhlNlbLabelFontQuality		"lbLabelFontQuality"
#define NhlNlbLabelConstantSpacingF	"lbLabelConstantSpacingF"
#define NhlNlbLabelFuncCode		"lbLabelFuncCode"
#define NhlNlbLabelStride		"lbLabelStride"
#define NhlNlbMaxLabelLenF		"lbMaxLabelLenF"
#define NhlNlbMinLabelSpacingF		"lbMinLabelSpacingF"

#define NhlNlbTitleOn			"lbTitleOn"
#define NhlNlbTitleString		"lbTitleString"
#define NhlNlbTitlePosition		"lbTitlePosition"
#define NhlNlbTitleExtentF		"lbTitleExtentF"
#define NhlNlbTitleAngleF		"lbTitleAngleF"
#define NhlNlbTitleDirection		"lbTitleDirection"
#define NhlNlbTitleFont			"lbTitleFont"
#define NhlNlbTitleJust			"lbTitleJust"
#define NhlNlbTitleFontColor		"lbTitleFontColor"
#define NhlNlbTitleFontHeightF		"lbTitleFontHeightF"
#define NhlNlbTitleFontAspectF		"lbTitleFontAspectF" /* height/width */
#define NhlNlbTitleFontThicknessF	"lbTitleFontThicknessF"
#define NhlNlbTitleFontQuality		"lbTitleFontQuality"
#define NhlNlbTitleConstantSpacingF	"lbTitleConstantSpacingF"
#define NhlNlbTitleFuncCode		"lbTitleFuncCode"

#define NhlNlbBoxLinesOn		"lbBoxLinesOn"
#define NhlNlbBoxLineColor		"lbBoxLineColor"
#define NhlNlbBoxLineThicknessF		"lbBoxLineThicknessF"
#define NhlNlbBoxLineDashPattern	"lbBoxLineDashPattern"
#define NhlNlbBoxLineDashSegLenF	"lbBoxLineDashSegLenF"
#define NhlNlbBoxSeparatorLinesOn	"lbBoxSeparatorLinesOn"
#define NhlNlbBoxEndCapStyle            "lbBoxEndCapStyle"

#define NhlNlbPerimOn			"lbPerimOn"
#define NhlNlbPerimColor		"lbPerimColor"
#define NhlNlbPerimFill			"lbPerimFill"
#define NhlNlbPerimFillColor		"lbPerimFillColor"
#define NhlNlbPerimThicknessF		"lbPerimThicknessF"
#define NhlNlbPerimDashPattern		"lbPerimDashPattern"
#define NhlNlbPerimDashSegLenF		"lbPerimDashSegLenF"

#define NhlNlbFillBackground		"lbFillBackground"
#define NhlNlbFillLineThicknessF	"lbFillLineThicknessF"
#define NhlNlbRasterFillOn		"lbRasterFillOn"
#define NhlNlbFillOpacityF              "lbFillOpacityF"
#define NhlNlbOverrideFillOpacity       "lbOverrideFillOpacity"

/* Class resources */

#define NhlClbLabelBarOn		"LbLabelBarOn"
#define NhlClbOrientation		"LbOrientation"
#define NhlClbJustification		"LbJustification"
#define NhlClbBoxMajorExtentF		"LbBoxMajorExtentF"
#define NhlClbBoxMinorExtentF		"LbBoxMinorExtentF"
#define NhlClbAlignment			"LbAlignment"
#define NhlClbBoxCount			"LbBoxCount"
#define NhlClbBoxSizing			"LbBoxSizing"

#define NhlClbAutoManage		"LbAutoManage"
#define NhlClbLabelOffsetF		"LbLabelOffsetF"
#define NhlClbTitleOffsetF		"LbTitleOffsetF"
#define NhlClbLeftMarginF		"LbLeftMarginF"
#define NhlClbRightMarginF		"LbRightMarginF"
#define NhlClbBottomMarginF		"LbBottomMarginF"
#define NhlClbTopMarginF		"LbTopMarginF"
#define NhlClbMarginMode		"LbMarginMode"

/* arrays */
#define NhlClbMonoFillColor		"LbMonoFillColor"
#define NhlClbFillColors		"LbFillColors"
#define NhlClbMonoFillPattern		"LbMonoFillPattern"
#define NhlClbFillPatterns		"LbFillPatterns"
#define NhlClbMonoFillScale		"LbMonoFillScale"
#define NhlClbFillScales		"LbFillScales"
#define NhlClbLabelStrings		"LbLabelStrings"
#define NhlClbBoxFractions		"LbBoxFractions"
/*end of arrays */

#define NhlClbLabelsOn			"LbLabelsOn"
#define NhlClbLabelPosition		"LbLabelPosition"
#define NhlClbLabelAlignment		"LbLabelAlignment"
#define NhlClbLabelStride		"LbLabelStride"
#define NhlClbMaxLabelLenF		"LbMaxLabelLenF"
#define NhlClbMinLabelSpacingF		"LbMinLabelSpacingF"

#define NhlClbTitleOn			"LbTitleOn"
#define NhlClbTitleString		"LbTitleString"
#define NhlClbTitlePosition		"LbTitlePosition"
#define NhlClbTitleExtentF		"LbTitleExtentF"
#define NhlClbTitleFuncCode		"LbTitleFuncCode"

#define NhlClbBoxLinesOn		"LbBoxLinesOn"
#define NhlClbRasterFillOn		"LbRasterFillOn"
#define NhlClbBoxSeparatorLinesOn	"LbBoxSeparatorLinesOn"
#define NhlClbBoxEndCapStyle            "LbBoxEndCapStyle"
#define NhlClbFillOpacityF              "lbFillOpacityF"
#define NhlClbOverrideFillOpacity       "lbOverrideFillOpacity"



/* end of resources */

/*
 * These class resources have been eliminated
 */
#if 0
#define NhlClbFillColor			"LbFillColor"
#define NhlClbFillPattern		"LbFillPattern"
#define NhlClbFillScaleF		"LbFillScaleF"
#define NhlClbLabelAngleF		"LbLabelAngleF"
#define NhlClbLabelJust			"LbLabelJust"
#define NhlClbLabelDirection		"LbLabelDirection"
#define NhlClbLabelFontColor		"LbLabelFontColor"
#define NhlClbLabelFontHeightF		"LbLabelFontHeightF"
#define NhlClbLabelFontAspectF		"LbLabelFontAspectF" /* height/width */
#define NhlClbLabelFontThicknessF	"LbLabelFontThicknessF"
#define NhlClbLabelFontQuality		"LbLabelFontQuality"
#define NhlClbLabelConstantSpacingF	"LbLabelConstantSpacingF"
#define NhlClbLabelFuncCode		"LbLabelFuncCode"
#define NhlClbTitleAngleF		"LbTitleAngleF"
#define NhlClbTitleDirection		"LbTitleDirection"
#define NhlClbTitleJust			"LbTitleJust"
#define NhlClbTitleFontColor		"LbTitleFontColor"
#define NhlClbTitleFontHeightF		"LbTitleFontHeightF"
#define NhlClbTitleFontAspectF		"LbTitleFontAspectF" /* height/width */
#define NhlClbTitleFontThicknessF	"LbTitleFontThicknessF"
#define NhlClbTitleFontQuality		"LbTitleFontQuality"
#define NhlClbTitleConstantSpacingF	"LbTitleConstantSpacingF"
#define NhlClbBoxLineColor		"LbBoxLineColor"
#define NhlClbBoxLineThicknessF		"LbBoxLineThicknessF"
#define NhlClbBoxLineDashPattern	"LbBoxLineDashPattern"
#define NhlClbBoxLineDashSegLenF	"LbBoxLineDashSegLenF"
#define NhlClbPerimOn			"LbPerimOn"
#define NhlClbPerimColor		"LbPerimColor"
#define NhlClbPerimFill			"LbPerimFill"
#define NhlClbPerimFillColor		"LbPerimFillColor"
#define NhlClbPerimThicknessF		"LbPerimThicknessF"
#define NhlClbPerimDashPattern		"LbPerimDashPattern"
#define NhlClbPerimDashSegLenF		"LbPerimDashSegLenF"
#define NhlClbFillBackground		"LbFillBackground"
#define NhlClbFillLineThicknessF	"LbFillLineThicknessF"
#endif

extern NhlClass NhllabelBarClass;

#endif  /* _NLabelBar_h */
