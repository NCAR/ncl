/*
 *      $Id: Legend.h,v 1.6 1994-03-02 01:44:10 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Legend.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	Legend public header file
 */
#ifndef _NLegend_h
#define _NLegend_h

#include <ncarg/hlu/View.h>
#include <ncarg/hlu/MultiText.h>

/* public defines */

#define NhlLG_MAX_ITEMS   	256
#define NhlLG_MAX_LBL_STRINGS  	(NhlLG_MAX_ITEMS)
#define NhlLG_DEF_ITEM_COUNT   	16
#define NhlLG_DEF_COLOR   	NhlFOREGROUND
#define NhlLG_DEF_LINE_INDEX	1
#define NhlLG_MIN_LINE_INDEX	0
#define NhlLG_DEF_MARKER_INDEX  1
#define NhlLG_MIN_MARKER_INDEX  0
#define NhlLG_DEF_STRING  	"Label_"
#define NhlLG_DEF_ITEM_STRING  	"L"
#define NhlLG_DEF_BAR_MAJOR	1.0
#define NhlLG_DEF_BAR_MINOR	0.33
#define NhlLg_DEF_LABEL_MINOR	0.33
#define NhlLG_DEF_CHAR_HEIGHT	0.04
#define NhlLG_DEF_MAX_TITLE_EXT	0.15
#define NhlLG_DEF_TITLE_OFF	0.03

/* label alignment */

#define NhlLG_ITEMCENTERS	0
#define NhlLG_ABOVEITEMS	1
#define NhlLG_BELOWITEMS	2

/* Box sizing */

#define NhlLG_UNIFORMPLACEMENT	0
#define NhlLG_EXPLICITPLACEMENT	1

/* Legend types */

#define NhlLG_LINES		0
#define NhlLG_MARKERS		1

/* Instance resources */

#define NhlNlgLegend			"lgLegend"
#define NhlNlgOrientation		"lgOrientation"
#define NhlNlgJustification		"lgJustification"
#define NhlNlgBoxMajorExtentF		"lgBoxMajorExtentF"
#define NhlNlgBoxMinorExtentF		"lgBoxMinorExtentF"
#define NhlNlgItemCount			"lgItemCount"
#define NhlNlgItemPlacement		"lgItemPlacement"

#define NhlNlgAutoManage		"lgAutoManage"
#define NhlNlgBarMode			"lgBarMode"
#define NhlNlgMaxLabelAngleAdditionF	"lgMaxLabelAngleAdditionF"
#define NhlNlgLabelOffsetF		"lgLabelOffsetF"
#define NhlNlgTitleOffsetF		"lgTitleOffsetF"
#define NhlNlgLeftMarginF		"lgLeftMarginF"
#define NhlNlgRightMarginF		"lgRightMarginF"
#define NhlNlgBottomMarginF		"lgBottomMarginF"
#define NhlNlgTopMarginF		"lgTopMarginF"
#define NhlNlgMarginMode		"lgMarginMode"

/* arrays */
#define NhlNlgMonoItemType		"lgMonoItemType"
#define NhlNlgItemTypes			"lgItemTypes"
#define NhlNlgItemIndexes		"lgItemIndexes"
#define NhlNlgItemStrings		"lgItemStrings"
#define NhlNlgMonoItemColor		"lgMonoItemColor"
#define NhlNlgItemColors		"lgItemColors"
#define NhlNlgMonoItemThickness		"lgMonoItemThickness"
#define NhlNlgItemThicknesses		"lgItemThicknesses"
#define NhlNlgMonoItemTextHeight	"lgMonoItemTextHeight"
#define NhlNlgItemTextHeights		"lgItemTextHeights"
#define NhlNlgLabelStrings		"lgLabelStrings"
#define NhlNlgItemPositions		"lgItemPositions"
#define NhlNlgMonoItemStringColor	"lgMonoItemStringColor"
#define NhlNlgItemStringColors		"lgItemStringColors"

/*end of arrays */

#define NhlNlgDrawLabels		"lgDrawLabels"
#define NhlNlgLabelPosition		"lgLabelPosition"
#define NhlNlgLabelAngleF		"lgLabelAngleF"
#define NhlNlgLabelJust			"lgLabelJust"
#define NhlNlgLabelDirection		"lgLabelDirection"
#define NhlNlgLabelAlignment		"lgLabelAlignment"
#define NhlNlgLabelFont			"lgLabelFont"
#define NhlNlgLabelFontColor		"lgLabelFontColor"
#define NhlNlgLabelFontHeightF		"lgLabelFontHeightF"
#define NhlNlgLabelFontAspectF		"lgLabelFontAspectF" /* height/width */
#define NhlNlgLabelFontThicknessF	"lgLabelFontThicknessF"
#define NhlNlgLabelFontQuality		"lgLabelFontQuality"
#define NhlNlgLabelConstantSpacingF	"lgLabelConstantSpacingF"
#define NhlNlgLabelFuncCode		"lgLabelFuncCode"
#define NhlNlgLabelStride		"lgLabelStride"

#define NhlNlgDrawTitle			"lgDrawTitle"
#define NhlNlgTitleString		"lgTitleString"
#define NhlNlgTitlePosition		"lgTitlePosition"
#define NhlNlgMaxTitleExtentF		"lgMaxTitleExtentF"
#define NhlNlgTitleAngleF		"lgTitleAngleF"
#define NhlNlgTitleDirection		"lgTitleDirection"
#define NhlNlgTitleFont			"lgTitleFont"
#define NhlNlgTitleJust			"lgTitleJust"
#define NhlNlgTitleFontColor		"lgTitleFontColor"
#define NhlNlgTitleFontHeightF		"lgTitleFontHeightF"
#define NhlNlgTitleFontAspectF		"lgTitleFontAspectF" /* height/width */
#define NhlNlgTitleFontThicknessF	"lgTitleFontThicknessF"
#define NhlNlgTitleFontQuality		"lgTitleFontQuality"
#define NhlNlgTitleConstantSpacingF	"lgTitleConstantSpacingF"
#define NhlNlgTitleFuncCode		"lgTitleFuncCode"

#define NhlNlgBoxBackground		"lgBoxBackground"
#define NhlNlgDrawBoxLines		"lgDrawBoxLines"
#define NhlNlgBoxLineColor		"lgBoxLineColor"
#define NhlNlgBoxLineThicknessF		"lgBoxLineThicknessF"
#define NhlNlgBoxLineDashPattern	"lgBoxLineDashPattern"
#define NhlNlgBoxLineDashLengthF	"lgBoxLineDashLengthF"

#define NhlNlgDrawPerim			"lgDrawPerim"
#define NhlNlgPerimColor		"lgPerimColor"
#define NhlNlgPerimFill			"lgPerimFill"
#define NhlNlgPerimFillColor		"lgPerimFillColor"
#define NhlNlgPerimThicknessF		"lgPerimThicknessF"
#define NhlNlgPerimDashPattern		"lgPerimDashPattern"
#define NhlNlgPerimDashLengthF		"lgPerimDashLengthF"

/* Class resources */

#define NhlClgLegend			"LgLegend"
#define NhlClgOrientation		"LgOrientation"
#define NhlClgJustification		"LgJustification"
#define NhlClgBoxMajorExtentF		"LgBoxMajorExtentF"
#define NhlClgBoxMinorExtentF		"LgBoxMinorExtentF"
#define NhlClgAlignment			"LgAlignment"
#define NhlClgItemCount			"LgItemCount"
#define NhlClgItemPlacement		"LgItemPlacement"

#define NhlClgAutoManage		"LgAutoManage"
#define NhlClgBarMode			"LgBarMode"
#define NhlClgMaxLabelAngleAdditionF	"LgMaxLabelAngleAdditionF"
#define NhlClgLabelOffsetF		"LgLabelOffsetF"
#define NhlClgTitleOffsetF		"LgTitleOffsetF"
#define NhlClgLeftMarginF		"LgLeftMarginF"
#define NhlClgRightMarginF		"LgRightMarginF"
#define NhlClgBottomMarginF		"LgBottomMarginF"
#define NhlClgTopMarginF		"LgTopMarginF"
#define NhlClgMarginMode		"LgMarginMode"

/* arrays */
#define NhlClgItemIndexes		"LgItemIndexes"
#define NhlClgItemStrings		"LgItemStrings"
#define NhlClgMonoItemType		"LgMonoItemType"
#define NhlClgItemTypes			"LgItemTypes"
#define NhlClgMonoItemColor		"LgMonoItemColor"
#define NhlClgItemColors		"LgItemColors"
#define NhlClgMonoItemThickness		"LgMonoItemThickness"
#define NhlClgItemThicknesses		"LgItemThicknesses"
#define NhlClgMonoItemTextHeight	"LgMonoItemTextHeight"
#define NhlClgItemTextHeights		"LgItemTextHeights"
#define NhlClgLabelStrings		"LgLabelStrings"
#define NhlClgItemPositions		"LgItemPositions"
#define NhlClgMonoItemStringColor	"LgMonoItemStringColor"
#define NhlClgItemStringColors		"LgItemStringColors"

/*end of arrays */

#define NhlClgDrawLabels		"LgDrawLabels"
#define NhlClgLabelPosition		"LgLabelPosition"
#define NhlClgLabelAngleF		"LgLabelAngleF"
#define NhlClgLabelJust			"LgLabelJust"
#define NhlClgLabelAlignment		"LgLabelAlignment"
#define NhlClgLabelDirection		"LgLabelDirection"
#define NhlClgLabelFontColor		"LgLabelFontColor"
#define NhlClgLabelFontHeightF		"LgLabelFontHeightF"
#define NhlClgLabelFontAspectF		"LgLabelFontAspectF" /* height/width */
#define NhlClgLabelFontThicknessF	"LgLabelFontThicknessF"
#define NhlClgLabelFontQuality		"LgLabelFontQuality"
#define NhlClgLabelConstantSpacingF	"LgLabelConstantSpacingF"
#define NhlClgLabelFuncCode		"LgLabelFuncCode"
#define NhlClgLabelStride		"LgLabelStride"

#define NhlClgDrawTitle			"LgDrawTitle"
#define NhlClgTitleString		"LgTitleString"
#define NhlClgTitlePosition		"LgTitlePosition"
#define NhlClgMaxTitleExtentF		"LgMaxTitleExtentF"
#define NhlClgTitleAngleF		"LgTitleAngleF"
#define NhlClgTitleDirection		"LgTitleDirection"
#define NhlClgTitleJust			"LgTitleJust"
#define NhlClgTitleFontColor		"LgTitleFontColor"
#define NhlClgTitleFontHeightF		"LgTitleFontHeightF"
#define NhlClgTitleFontAspectF		"LgTitleFontAspectF" /* height/width */
#define NhlClgTitleFontThicknessF	"LgTitleFontThicknessF"
#define NhlClgTitleFontQuality		"LgTitleFontQuality"
#define NhlClgTitleConstantSpacingF	"LgTitleConstantSpacingF"
#define NhlClgTitleFuncCode		"LgTitleFuncCode"

#define NhlClgBoxBackground		"LgBoxBackground"
#define NhlClgDrawBoxLines		"LgDrawBoxLines"
#define NhlClgBoxLineColor		"LgBoxLineColor"
#define NhlClgBoxLineThicknessF		"LgBoxLineThicknessF"
#define NhlClgBoxLineDashPattern	"LgBoxLineDashPattern"
#define NhlClgBoxLineDashLengthF	"LgBoxLineDashLengthF"

#define NhlClgDrawPerim			"LgDrawPerim"
#define NhlClgPerimColor		"LgPerimColor"
#define NhlClgPerimFill			"LgPerimFill"
#define NhlClgPerimFillColor		"LgPerimFillColor"
#define NhlClgPerimThicknessF		"LgPerimThicknessF"
#define NhlClgPerimDashPattern		"LgPerimDashPattern"
#define NhlClgPerimDashLengthF		"LgPerimDashLengthF"


/* end of resources */

extern NhlLayerClass NhllegendLayerClass;

#endif  /* _NLegend_h */
