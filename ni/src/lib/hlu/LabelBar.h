/*
 *      $Id: LabelBar.h,v 1.9 1995-04-07 10:42:23 boote Exp $
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

/* public defines */

#define NhlLB_MAX_BOXES   	256
#define NhlLB_MAX_LBL_STRINGS  	(NhlLB_MAX_BOXES + 1)
#define NhlLB_DEF_BOX_COUNT   	16
#define NhlLB_DEF_COLOR   	NhlFOREGROUND
#define NhlLB_DEF_PATTERN 	1
#define NhlLB_DEF_VALUE   	0.0
#define NhlLB_DEF_STRING  	"Label_"
#define NhlLB_DEF_BAR_MAJOR	1.0
#define NhlLB_DEF_BAR_MINOR	0.33
#define NhlLb_DEF_LABEL_MINOR	0.33
#define NhlLB_DEF_CHAR_HEIGHT	0.04
#define NhlLB_DEF_MAX_TITLE_EXT	0.15
#define NhlLB_DEF_TITLE_OFF	0.03

/* label alignment */

typedef enum _NhllbLabelAlignmentMode {
	NhlBOXCENTERS =0, NhlINTERIOREDGES, NhlEXTERNALEDGES
} NhllbLabelAlignmentMode;

#define NhlTlbLabelAlignmentMode "lblabelalignmentmode"

typedef enum _NhllbBoxSizingMode {
	NhlUNIFORMSIZING = 0, NhlEXPLICITSIZING
} NhllbBoxSizingMode;

#define NhlTlbBoxSizingMode "lbboxsizingmode"

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
#define NhlNlbLabelStrings		"lbLabelStrings"
#define NhlNlbBoxFractions		"lbBoxFractions"

/*end of arrays */

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

#define NhlNlbTitleOn			"lbTitleOn"
#define NhlNlbTitleString		"lbTitleString"
#define NhlNlbTitlePosition		"lbTitlePosition"
#define NhlNlbMaxTitleExtentF		"lbMaxTitleExtentF"
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

#define NhlNlbPerimOn			"lbPerimOn"
#define NhlNlbPerimColor		"lbPerimColor"
#define NhlNlbPerimFill			"lbPerimFill"
#define NhlNlbPerimFillColor		"lbPerimFillColor"
#define NhlNlbPerimThicknessF		"lbPerimThicknessF"
#define NhlNlbPerimDashPattern		"lbPerimDashPattern"
#define NhlNlbPerimDashSegLenF		"lbPerimDashSegLenF"

#define NhlNlbFillBackground		"lbFillBackground"
#define NhlNlbFillLineThicknessF	"lbFillLineThicknessF"

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
#define NhlClbFillColor			"LbFillColor"
#define NhlClbFillColors		"LbFillColors"
#define NhlClbMonoFillPattern		"LbMonoFillPattern"
#define NhlClbFillPattern		"LbFillPattern"
#define NhlClbFillPatterns		"LbFillPatterns"
#define NhlClbMonoFillScale		"LbMonoFillScale"
#define NhlClbFillScaleF		"LbFillScaleF"
#define NhlClbFillScales		"LbFillScales"
#define NhlClbLabelStrings		"LbLabelStrings"
#define NhlClbBoxFractions		"LbBoxFractions"
/*end of arrays */

#define NhlClbLabelsOn			"LbLabelsOn"
#define NhlClbLabelPosition		"LbLabelPosition"
#define NhlClbLabelAngleF		"LbLabelAngleF"
#define NhlClbLabelJust			"LbLabelJust"
#define NhlClbLabelAlignment		"LbLabelAlignment"
#define NhlClbLabelDirection		"LbLabelDirection"
#define NhlClbLabelFontColor		"LbLabelFontColor"
#define NhlClbLabelFontHeightF		"LbLabelFontHeightF"
#define NhlClbLabelFontAspectF		"LbLabelFontAspectF" /* height/width */
#define NhlClbLabelFontThicknessF	"LbLabelFontThicknessF"
#define NhlClbLabelFontQuality		"LbLabelFontQuality"
#define NhlClbLabelConstantSpacingF	"LbLabelConstantSpacingF"
#define NhlClbLabelFuncCode		"LbLabelFuncCode"
#define NhlClbLabelStride		"LbLabelStride"

#define NhlClbTitleOn			"LbTitleOn"
#define NhlClbTitleString		"LbTitleString"
#define NhlClbTitlePosition		"LbTitlePosition"
#define NhlClbMaxTitleExtentF		"LbMaxTitleExtentF"
#define NhlClbTitleAngleF		"LbTitleAngleF"
#define NhlClbTitleDirection		"LbTitleDirection"
#define NhlClbTitleJust			"LbTitleJust"
#define NhlClbTitleFontColor		"LbTitleFontColor"
#define NhlClbTitleFontHeightF		"LbTitleFontHeightF"
#define NhlClbTitleFontAspectF		"LbTitleFontAspectF" /* height/width */
#define NhlClbTitleFontThicknessF	"LbTitleFontThicknessF"
#define NhlClbTitleFontQuality		"LbTitleFontQuality"
#define NhlClbTitleConstantSpacingF	"LbTitleConstantSpacingF"
#define NhlClbTitleFuncCode		"LbTitleFuncCode"

#define NhlClbBoxLinesOn		"LbBoxLinesOn"
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

/* end of resources */

extern NhlClass NhllabelBarClass;

#endif  /* _NLabelBar_h */
