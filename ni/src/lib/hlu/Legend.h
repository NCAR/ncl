/*
 *      $Id: Legend.h,v 1.22 2008-05-27 20:55:30 dbrown Exp $
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

/* label alignment */

typedef enum _NhllgLabelAlignmentMode {
	NhlITEMCENTERS = 0, NhlABOVEITEMS, NhlBELOWITEMS
} NhllgLabelAlignmentMode;

#define NhlTlgLabelAlignmentMode "lgLabelAlignmentMode"

/* Box sizing */

typedef enum _NhllgItemPlacementMode {
	NhlUNIFORMPLACEMENT = 0, NhlEXPLICITPLACEMENT
} NhllgItemPlacementMode;

#define NhlTlgItemPlacementMode "lgItemPlacementMode"

/* Instance resources */

#define NhlNlgLegendOn			"lgLegendOn"
#define NhlNlgOrientation		"lgOrientation"
#define NhlNlgJustification		"lgJustification"
#define NhlNlgBoxMajorExtentF		"lgBoxMajorExtentF"
#define NhlNlgBoxMinorExtentF		"lgBoxMinorExtentF"
#define NhlNlgItemCount			"lgItemCount"
#define NhlNlgItemPlacement		"lgItemPlacement"

#define NhlNlgAutoManage		"lgAutoManage"
#define NhlNlgLabelOffsetF		"lgLabelOffsetF"
#define NhlNlgTitleOffsetF		"lgTitleOffsetF"
#define NhlNlgLeftMarginF		"lgLeftMarginF"
#define NhlNlgRightMarginF		"lgRightMarginF"
#define NhlNlgBottomMarginF		"lgBottomMarginF"
#define NhlNlgTopMarginF		"lgTopMarginF"

/* arrays */
#define NhlNlgMonoItemType		"lgMonoItemType"
#define NhlNlgItemType			"lgItemType"
#define NhlNlgItemTypes			"lgItemTypes"

#define NhlNlgMonoDashIndex		"lgMonoDashIndex"
#define NhlNlgDashIndex			"lgDashIndex"
#define NhlNlgDashIndexes		"lgDashIndexes"

#define NhlNlgMonoMarkerIndex		"lgMonoMarkerIndex"
#define NhlNlgMarkerIndex		"lgMarkerIndex"
#define NhlNlgMarkerIndexes		"lgMarkerIndexes"

#define NhlNlgLineLabelStrings		"lgLineLabelStrings"

#define NhlNlgMonoLineColor		"lgMonoLineColor"
#define NhlNlgLineColor			"lgLineColor"
#define NhlNlgLineColors		"lgLineColors"
#define NhlNlgMonoMarkerColor		"lgMonoMarkerColor"
#define NhlNlgMarkerColor		"lgMarkerColor"
#define NhlNlgMarkerColors		"lgMarkerColors"

#define NhlNlgMonoLineThickness		"lgMonoLineThickness"
#define NhlNlgLineThicknessF		"lgLineThicknessF"
#define NhlNlgLineThicknesses		"lgLineThicknesses"
#define NhlNlgMonoMarkerThickness	"lgMonoMarkerThickness"
#define NhlNlgMarkerThicknessF		"lgMarkerThicknessF"
#define NhlNlgMarkerThicknesses		"lgMarkerThicknesses"

#define NhlNlgMonoLineLabelFontHeight	"lgMonoLineLabelFontHeight"
#define NhlNlgLineLabelFontHeightF	"lgLineLabelFontHeightF"
#define NhlNlgLineLabelFontHeights	"lgLineLabelFontHeights"
#define NhlNlgMonoMarkerSize		"lgMonoMarkerSize"
#define NhlNlgMarkerSizeF		"lgMarkerSizeF"
#define NhlNlgMarkerSizes		"lgMarkerSizes"

#define NhlNlgLabelStrings		"lgLabelStrings"
#define NhlNlgItemPositions		"lgItemPositions"
#define NhlNlgItemOrder                 "lgItemOrder"

#define NhlNlgMonoLineLabelFontColor	"lgMonoLineLabelFontColor"
#define NhlNlgLineLabelFontColor	"lgLineLabelFontColor"
#define NhlNlgLineLabelFontColors	"lgLineLabelFontColors"

#define NhlNlgMonoLineDashSegLen	"lgMonoLineDashSegLen"
#define NhlNlgLineDashSegLenF		"lgLineDashSegLenF"
#define NhlNlgLineDashSegLens		"lgLineDashSegLens"


/*end of arrays */

#define NhlNlgLineLabelsOn		"lgLineLabelsOn"

#define NhlNlgLineLabelFont		"lgLineLabelFont"
#define NhlNlgLineLabelFontAspectF	"lgLineLabelFontAspectF"
#define NhlNlgLineLabelFontThicknessF	"lgLineLabelFontThicknessF"
#define NhlNlgLineLabelFontQuality	"lgLineLabelFontQuality"
#define NhlNlgLineLabelConstantSpacingF	"lgLineLabelConstantSpacingF"
#define NhlNlgLineLabelFuncCode	"lgLineLabelFuncCode"

#define NhlNlgLabelAutoStride		"lgLabelAutoStride"
#define NhlNlgLabelsOn			"lgLabelsOn"
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

#define NhlNlgTitleOn			"lgTitleOn"
#define NhlNlgTitleString		"lgTitleString"
#define NhlNlgTitlePosition		"lgTitlePosition"
#define NhlNlgTitleExtentF		"lgTitleExtentF"
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
#define NhlNlgBoxLinesOn		"lgBoxLinesOn"
#define NhlNlgBoxLineColor		"lgBoxLineColor"
#define NhlNlgBoxLineThicknessF		"lgBoxLineThicknessF"
#define NhlNlgBoxLineDashPattern	"lgBoxLineDashPattern"
#define NhlNlgBoxLineDashSegLenF	"lgBoxLineDashSegLenF"

#define NhlNlgPerimOn			"lgPerimOn"
#define NhlNlgPerimColor		"lgPerimColor"
#define NhlNlgPerimFill			"lgPerimFill"
#define NhlNlgPerimFillColor		"lgPerimFillColor"
#define NhlNlgPerimThicknessF		"lgPerimThicknessF"
#define NhlNlgPerimDashPattern		"lgPerimDashPattern"
#define NhlNlgPerimDashSegLenF		"lgPerimDashSegLenF"

/* Class resources */

#define NhlClgLegendOn			"LgLegendOn"
#define NhlClgOrientation		"LgOrientation"
#define NhlClgJustification		"LgJustification"
#define NhlClgBoxMajorExtentF		"LgBoxMajorExtentF"
#define NhlClgBoxMinorExtentF		"LgBoxMinorExtentF"
#define NhlClgAlignment			"LgAlignment"
#define NhlClgItemCount			"LgItemCount"
#define NhlClgItemPlacement		"LgItemPlacement"

#define NhlClgAutoManage		"LgAutoManage"
#define NhlClgMaxLabelAngleAdditionF	"LgMaxLabelAngleAdditionF"
#define NhlClgLabelOffsetF		"LgLabelOffsetF"
#define NhlClgTitleOffsetF		"LgTitleOffsetF"
#define NhlClgLeftMarginF		"LgLeftMarginF"
#define NhlClgRightMarginF		"LgRightMarginF"
#define NhlClgBottomMarginF		"LgBottomMarginF"
#define NhlClgTopMarginF		"LgTopMarginF"

/* arrays */
#define NhlClgMonoItemType		"LgMonoItemType"
#define NhlClgItemType			"LgItemType"
#define NhlClgItemTypes			"LgItemTypes"
#define NhlClgMonoDashIndex		"LgMonoDashIndex"
#define NhlClgDashIndexes		"LgDashIndexes"
#define NhlClgMonoMarkerIndex		"LgMonoMarkerIndex"
#define NhlClgMarkerIndexes		"LgMarkerIndexes"
#define NhlClgLineLabelStrings		"LgLineLabelStrings"
#define NhlClgMonoLineColor		"LgMonoLineColor"
#define NhlClgLineColors		"LgLineColors"
#define NhlClgMonoMarkerColor		"LgMonoMarkerColor"
#define NhlClgMarkerColors		"LgMarkerColors"

#define NhlClgMonoLineThickness		"LgMonoLineThickness"
#define NhlClgLineThicknesses		"LgLineThicknesses"
#define NhlClgMonoMarkerThickness	"LgMonoMarkerThickness"
#define NhlClgMarkerThicknesses		"LgMarkerThicknesses"

#define NhlClgMonoLineLabelFontHeight	"LgMonoLineLabelFontHeight"
#define NhlClgLineLabelFontHeights	"LgLineLabelFontHeights"
#define NhlClgMonoMarkerSize		"LgMonoMarkerSize"
#define NhlClgMarkerSizes		"LgMarkerSizes"

#define NhlClgLabelStrings		"LgLabelStrings"
#define NhlClgItemPositions		"LgItemPositions"
#define NhlClgItemOrder                 "LgItemOrder"
#define NhlClgMonoLineLabelFontColor	"LgMonoLineLabelFontColor"
#define NhlClgLineLabelFontColors	"LgLineLabelFontColors"

#define NhlClgMonoLineDashSegLen	"LgMonoLineDashSegLen"
#define NhlClgLineDashSegLens		"LgLineDashSegLens"


/*end of arrays */

#define NhlClgLineLabelsOn		"LgLineLabelsOn"


#define NhlClgLabelsOn			"LgLabelsOn"
#define NhlClgLabelPosition		"LgLabelPosition"
#define NhlClgLabelAlignment		"LgLabelAlignment"
#define NhlClgLabelStride		"LgLabelStride"

#define NhlClgTitleOn			"LgTitleOn"
#define NhlClgTitleString		"LgTitleString"
#define NhlClgTitlePosition		"LgTitlePosition"
#define NhlClgTitleExtentF		"LgTitleExtentF"

#define NhlClgBoxLinesOn		"LgBoxLinesOn"


/*
 * These class resources have been eliminated
 */
#if 0
#define NhlClgDashIndex			"LgDashIndex"
#define NhlClgMarkerIndex		"LgMarkerIndex"
#define NhlClgLineColor			"LgLineColor"
#define NhlClgMarkerColor		"LgMarkerColor"
#define NhlClgLineThicknessF		"LgLineThicknessF"
#define NhlClgMarkerThicknessF		"LgMarkerThicknessF"
#define NhlClgLineLabelFontHeightF	"LgLineLabelFontHeightF"
#define NhlClgMarkerSizeF		"LgMarkerSizeF"
#define NhlClgLineLabelFontColor	"LgLineLabelFontColor"
#define NhlClgLineDashSegLenF		"LgLineDashSegLenF"
#define NhlClgLineLabelFont		"LgLineLabelFont"
#define NhlClgLineLabelFontAspectF	"LgLineLabelFontAspectF"
#define NhlClgLineLabelFontThicknessF	"LgLineLabelFontThicknessF"
#define NhlClgLineLabelFontQuality	"LgLineLabelFontQuality"
#define NhlClgLineLabelConstantSpacingF	"LgLineLabelConstantSpacingF"
#define NhlClgLineLabelFuncCode	"LgLineLabelFuncCode"
#define NhlClgLabelAngleF		"LgLabelAngleF"
#define NhlClgLabelJust			"LgLabelJust"
#define NhlClgLabelDirection		"LgLabelDirection"
#define NhlClgLabelFontColor		"LgLabelFontColor"
#define NhlClgLabelFontHeightF		"LgLabelFontHeightF"
#define NhlClgLabelFontAspectF		"LgLabelFontAspectF" /* height/width */
#define NhlClgLabelFontThicknessF	"LgLabelFontThicknessF"
#define NhlClgLabelFontQuality		"LgLabelFontQuality"
#define NhlClgLabelConstantSpacingF	"LgLabelConstantSpacingF"
#define NhlClgLabelFuncCode		"LgLabelFuncCode"
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
#define NhlClgBoxLineColor		"LgBoxLineColor"
#define NhlClgBoxLineThicknessF		"LgBoxLineThicknessF"
#define NhlClgBoxLineDashPattern	"LgBoxLineDashPattern"
#define NhlClgBoxLineDashSegLenF	"LgBoxLineDashSegLenF"
#define NhlClgPerimOn			"LgPerimOn"
#define NhlClgPerimColor		"LgPerimColor"
#define NhlClgPerimFill			"LgPerimFill"
#define NhlClgPerimFillColor		"LgPerimFillColor"
#define NhlClgPerimThicknessF		"LgPerimThicknessF"
#define NhlClgPerimDashPattern		"LgPerimDashPattern"
#define NhlClgPerimDashSegLenF		"LgPerimDashSegLenF"

#endif

/* end of resources */

extern NhlClass NhllegendClass;

#endif  /* _NLegend_h */
