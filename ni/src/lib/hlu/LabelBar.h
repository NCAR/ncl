
/*
 *      $Id: LabelBar.h,v 1.1 1993-07-27 18:02:59 dbrown Exp $
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
#define NhlLB_DEF_COLOR   	0
#define NhlLB_DEF_PATTERN 	-1
#define NhlLB_DEF_VALUE   	0.0
#define NhlLB_DEF_STRING  	"MLabel_"
#define NhlLB_DEF_BAR_MAJOR	1.0
#define NhlLB_DEF_TITLE_MAJOR   0.2
#define NhlLB_DEF_BAR_MINOR	0.33
#define NhlLB_DEF_TITLE_MINOR	0.33
#define NhlLb_DEF_LABEL_MINOR	0.33

/* label alignment */

#define NhlLB_BOXCENTERS	0
#define NhlLB_INTERIOREDGES	1
#define NhlLB_EXTERNALEDGES	2

/* Box sizing */

#define NhlLB_UNIFORMSIZING	0
#define NhlLB_EXPLICITSIZING	1

/* Instance resources */

#define NhlNlbLabelBar			"lbLabelBar"
#define NhlNlbOrientation		"lbOrientation"
#define NhlNlbJustification		"lbJustification"
#define NhlNlbXF			"lbXF"
#define NhlNlbYF			"lbYF"
#define NhlNlbWidthF			"lbWidthF"
#define NhlNlbHeightF			"lbHeightF"
#define NhlNlbBoxMajorExtentF		"lbBoxMajorExtentF"
#define NhlNlbBoxMinorExtentF		"lbBoxMinorExtentF"
#define NhlNlbAlignment			"lbAlignment"
#define NhlNlbBoxCount			"lbBoxCount"
#define NhlNlbBoxMode			"lbBoxMode"
#define NhlNlbBoxSizing			"lbBoxSizing"

/* arrays */
#define NhlNlbFillPatterns		"lbFillPatterns"
#define NhlNlbColors			"lbColors"
#define NhlNlbValues			"lbValues"
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

#define NhlNlbDrawBoxLines		"lbDrawBoxLines"
#define NhlNlbBoxLineColor		"lbBoxLineColor"
#define NhlNlbBoxLineThicknessF		"lbBoxLineThicknessF"
#define NhlNlbBoxLineDashPattern	"lbBoxLineDashPattern"
#define NhlNlbBoxLineDashLengthF	"lbBoxLineDashLengthF"

#define NhlNlbDrawPerim			"lbDrawPerim"
#define NhlNlbPerimColor		"lbPerimColor"
#define NhlNlbPerimThicknessF		"lbPerimThicknessF"
#define NhlNlbPerimDashPattern		"lbPerimDashPattern"
#define NhlNlbPerimDashLengthF		"lbPerimDashLengthF"

#define NhlNlbFillLineColor		"lbFillLineColor"
#define NhlNlbFillLineThicknessF	"lbFillLineThicknessF"

/* Class resources */

#define NhlClbLabelBar			"LbLabelBar"
#define NhlClbOrientation		"LbOrientation"
#define NhlClbJustification		"LbJustification"
#define NhlClbXF			"LbXF"
#define NhlClbYF			"LbYF"
#define NhlClbWidthF			"LbWidthF"
#define NhlClbHeightF			"LbHeightF"
#define NhlClbBoxMajorExtentF		"LbBoxMajorExtentF"
#define NhlClbBoxMinorExtentF		"LbBoxMinorExtentF"
#define NhlClbAlignment			"LbAlignment"
#define NhlClbBoxCount			"LbBoxCount"
#define NhlClbBoxMode			"LbBoxMode"
#define NhlClbBoxSizing			"LbBoxSizing"

/* arrays */
#define NhlClbFillPatterns		"LbFillPatterns"
#define NhlClbColors			"LbColors"
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
#define NhlClbLabelFont			"LbLabelFont"
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
#define NhlClbTitleExtentF		"LbTitleExtentF"
#define NhlClbTitleAngleF		"LbTitleAngleF"
#define NhlClbTitleDirection		"LbTitleDirection"
#define NhlClbTitleFont			"LbTitleFont"
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
#define NhlClbPerimThicknessF		"LbPerimThicknessF"
#define NhlClbPerimDashPattern		"LbPerimDashPattern"
#define NhlClbPerimDashLengthF		"LbPerimDashLengthF"

#define NhlClbFillLineColor		"LbFillLineColor"
#define NhlClbFillLineThicknessF	"LbFillLineThicknessF"

/* end of resources */

extern LayerClass labelBarLayerClass;

typedef struct _LabelBarLayerClassRec *LabelBarLayerClass;
typedef struct _LabelBarLayerRec	*LabelBarLayer;
#endif  /* _NLabelBar_h */

