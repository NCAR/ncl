/*
 *      $Id: LabelBar.h,v 1.4 1994-01-27 21:23:44 boote Exp $
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

#define NhlLB_BOXCENTERS	0
#define NhlLB_INTERIOREDGES	1
#define NhlLB_EXTERNALEDGES	2

/* Box sizing */

#define NhlLB_UNIFORMSIZING	0
#define NhlLB_EXPLICITSIZING	1

/* Bar Mode */

#define NlhLB_CENTERBAR		0
#define NhlLB_SIZEBARTOFIT	1
#define NhlLB_JUSTIFYLABELSIDE	2
#define NhlLB_JUSTIFYBARSIDE	3

/* Instance resources */

#define NhlNlbLabelBar			"lbLabelBar"
#define NhlNlbOrientation		"lbOrientation"
#define NhlNlbJustification		"lbJustification"
#define NhlNlbBoxMajorExtentF		"lbBoxMajorExtentF"
#define NhlNlbBoxMinorExtentF		"lbBoxMinorExtentF"
#define NhlNlbBoxCount			"lbBoxCount"
#define NhlNlbBoxSizing			"lbBoxSizing"

#define NhlNlbAutoManage		"lbAutoManage"
#define NhlNlbBarMode			"lbBarMode"
#define NhlNlbMonoFillColor		"lbMonoFillColor"
#define NhlNlbMonoFillPattern		"lbMonoFillPattern"
#define NhlNlbMonoFillScale		"lbMonoFillScale"
#define NhlNlbMaxLabelAngleAdditionF		"lbMaxLabelAngleAdditionF"
#define NhlNlbLabelOffsetF		"lbLabelOffsetF"
#define NhlNlbTitleOffsetF		"lbTitleOffsetF"
#define NhlNlbLeftMarginF		"lbLeftMarginF"
#define NhlNlbRightMarginF		"lbRightMarginF"
#define NhlNlbBottomMarginF		"lbBottomMarginF"
#define NhlNlbTopMarginF		"lbTopMarginF"
#define NhlNlbMarginMode		"lbMarginMode"

/* arrays */
#define NhlNlbFillPatterns		"lbFillPatterns"
#define NhlNlbFillColors		"lbFillColors"
#define NhlNlbFillScales		"lbFillScales"
#define NhlNlbLabelStrings		"lbLabelStrings"
#define NhlNlbBoxFractions		"lbBoxFractions"

/*end of arrays */

#define NhlNlbDrawLabels		"lbDrawLabels"
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

#define NhlNlbDrawTitle			"lbDrawTitle"
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

#define NhlNlbDrawBoxLines		"lbDrawBoxLines"
#define NhlNlbBoxLineColor		"lbBoxLineColor"
#define NhlNlbBoxLineThicknessF		"lbBoxLineThicknessF"
#define NhlNlbBoxLineDashPattern	"lbBoxLineDashPattern"
#define NhlNlbBoxLineDashLengthF	"lbBoxLineDashLengthF"

#define NhlNlbDrawPerim			"lbDrawPerim"
#define NhlNlbPerimColor		"lbPerimColor"
#define NhlNlbPerimFill			"lbPerimFill"
#define NhlNlbPerimFillColor		"lbPerimFillColor"
#define NhlNlbPerimThicknessF		"lbPerimThicknessF"
#define NhlNlbPerimDashPattern		"lbPerimDashPattern"
#define NhlNlbPerimDashLengthF		"lbPerimDashLengthF"

#define NhlNlbFillBackground		"lbFillBackground"
#define NhlNlbFillLineThicknessF	"lbFillLineThicknessF"

/* Class resources */

#define NhlClbLabelBar			"LbLabelBar"
#define NhlClbOrientation		"LbOrientation"
#define NhlClbJustification		"LbJustification"
#define NhlClbBoxMajorExtentF		"LbBoxMajorExtentF"
#define NhlClbBoxMinorExtentF		"LbBoxMinorExtentF"
#define NhlClbAlignment			"LbAlignment"
#define NhlClbBoxCount			"LbBoxCount"
#define NhlClbBoxSizing			"LbBoxSizing"

#define NhlClbAutoManage		"LbAutoManage"
#define NhlClbBarMode			"LbBarMode"
#define NhlClbMaxLabelAngleAdditionF	"LbMaxLabelAngleAdditionF"
#define NhlClbMonoFillColor		"LbMonoFillColor"
#define NhlClbMonoFillPattern		"LbMonoFillPattern"
#define NhlClbMonoFillScale		"LbMonoFillScale"
#define NhlClbLabelOffsetF		"LbLabelOffsetF"
#define NhlClbTitleOffsetF		"LbTitleOffsetF"
#define NhlClbLeftMarginF		"LbLeftMarginF"
#define NhlClbRightMarginF		"LbRightMarginF"
#define NhlClbBottomMarginF		"LbBottomMarginF"
#define NhlClbTopMarginF		"LbTopMarginF"
#define NhlClbMarginMode		"LbMarginMode"

/* arrays */
#define NhlClbFillPatterns		"LbFillPatterns"
#define NhlClbFillColors		"LbFillColors"
#define NhlClbFillScales		"LbFillScales"
#define NhlClbValues			"LbValues"
#define NhlClbLabelStrings		"LbLabelStrings"
#define NhlClbBoxFractions		"LbBoxFractions"
/*end of arrays */

#define NhlClbDrawLabels		"LbDrawLabels"
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

#define NhlClbDrawTitle			"LbDrawTitle"
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

#define NhlClbDrawBoxLines		"LbDrawBoxLines"
#define NhlClbBoxLineColor		"LbBoxLineColor"
#define NhlClbBoxLineThicknessF		"LbBoxLineThicknessF"
#define NhlClbBoxLineDashPattern	"LbBoxLineDashPattern"
#define NhlClbBoxLineDashLengthF	"LbBoxLineDashLengthF"

#define NhlClbDrawPerim			"LbDrawPerim"
#define NhlClbPerimColor		"LbPerimColor"
#define NhlClbPerimFill			"LbPerimFill"
#define NhlClbPerimFillColor		"LbPerimFillColor"
#define NhlClbPerimThicknessF		"LbPerimThicknessF"
#define NhlClbPerimDashPattern		"LbPerimDashPattern"
#define NhlClbPerimDashLengthF		"LbPerimDashLengthF"

#define NhlClbFillBackground		"LbFillBackground"
#define NhlClbFillLineThicknessF	"LbFillLineThicknessF"

/* end of resources */

extern NhlLayerClass NhllabelBarLayerClass;

#endif  /* _NLabelBar_h */
