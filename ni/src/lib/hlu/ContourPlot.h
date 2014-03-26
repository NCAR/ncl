/*
 *      $Id: ContourPlot.h,v 1.21.12.1 2010-03-17 20:47:07 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ContourPlot.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for ContourPlot class.
 */

#ifndef _NCONTOURPLOT_h
#define _NCONTOURPLOT_h

#include <ncarg/hlu/Workspace.h>
#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/ScalarField.h>

/* Level selection modes */

typedef NhlLevelSelectionMode NhlcnLevelSelectionMode;

#define NhlTcnLevelSelectionMode NhlTLevelSelectionMode


/* Level usage modes */

typedef enum _NhlcnLevelUseMode {
	NhlNOLINE, NhlLINEONLY, 
	NhlLABELONLY, NhlLINEANDLABEL
} NhlcnLevelUseMode;

#define NhlTcnLevelUseMode	"cnLevelUseMode"
#define NhlTcnLevelUseModeGenArray    NhlTcnLevelUseMode NhlTGenArray

/* Line label spacing */

typedef enum _NhlcnLineLabelPlacementMode {
	NhlCONSTANT, 
	NhlRANDOMIZED,
	NhlCOMPUTED
} NhlcnLineLabelPlacementMode;

#define NhlTcnLineLabelPlacementMode	"cnLineLabelPlacementMode"

/* Label scaling mode */

typedef NhlScalingMode NhlcnLabelScalingMode;


#define NhlTcnLabelScalingMode	NhlTScalingMode

/* label overlap flag */

typedef enum _NhlcnHighLowLabelOverlapMode {
	NhlIGNOREOVERLAP = 0,
	NhlOMITOVERINFO,
	NhlOMITOVERHL,
	NhlOMITOVERHLANDINFO,
	NhlOMITOVERVP,
	NhlOMITOVERVPANDINFO,
	NhlOMITOVERVPANDHL,
	NhlOMITOVERVPANDHLANDINFO,
	NhlADJUSTVP,
	NhlADJUSTVPOMITOVERINFO,
	NhlADJUSTVPOMITOVERHL,
	NhlADJUSTVPOMITOVERHLANDINFO
} NhlcnHighLowLabelOverlapMode;

#define NhlTcnHighLowLabelOverlapMode	"cnHighLowLabelOverlapMode"

typedef enum _NhlcnFillMode {
	NhlAREAFILL, 
	NhlRASTERFILL,
	NhlCELLFILL,
	NhlMESHFILL
} NhlcnFillMode;

#define NhlTcnFillMode	"cnFillMode"

typedef struct _NhlIsoLine {
	float level;
	int   n_segments;
        int   point_count;
	int   *start_point;
	int   *n_points;
	float *x;
	float *y;
} NhlIsoLine;

NhlIsoLine  *NhlGetIsoLines(
	int contourplot_id,
	int n_levels,
	float *levels
	);

NhlErrorTypes NhlFreeIsoLines(
	NhlIsoLine *isolines, 
	int n_levels
	);

/*
 * ContourPlot instance resources
 */

#define NhlNcnScalarFieldData		"cnScalarFieldData"
#define NhlNcnOutOfRangeValF		"cnOutOfRangeValF"

#define NhlNcnLevelCount		"cnLevelCount"		/* read-only */
#define NhlNcnLevelSelectionMode	"cnLevelSelectionMode"
#define NhlNcnMaxLevelCount		"cnMaxLevelCount"
#define NhlNcnLevelSpacingF		"cnLevelSpacingF"
#define NhlNcnLabelMasking		"cnLabelMasking"
#define NhlNcnMinLevelValF		"cnMinLevelValF"
#define NhlNcnMaxLevelValF		"cnMaxLevelValF"
#define NhlNcnLineLabelInterval		"cnLineLabelInterval"
#define NhlNcnLabelDrawOrder		"cnLabelDrawOrder"
#define NhlNcnLineDrawOrder		"cnLineDrawOrder"
#define NhlNcnFillDrawOrder		"cnFillDrawOrder"
#define NhlNcnLinesOn			"cnLinesOn"
#define NhlNcnFillOn			"cnFillOn"
#define NhlNcnFillBackgroundColor	"cnFillBackgroundColor"
#define NhlNcnFillMode			"cnFillMode"

#define NhlNcnLabelScalingMode		"cnLabelScalingMode"
#define NhlNcnLabelScaleValueF		"cnLabelScaleValueF"
#define NhlNcnLabelScaleFactorF		"cnLabelScaleFactorF" /*read-only*/
#define NhlNcnMaxDataValueFormat	"cnMaxDataValueFormat"
#define NhlNcnSmoothingOn		"cnSmoothingOn"
#define NhlNcnSmoothingTensionF		"cnSmoothingTensionF"
#define NhlNcnSmoothingDistanceF	"cnSmoothingDistanceF"
#define NhlNcnMaxPointDistanceF		"cnMaxPointDistanceF"

#define NhlNcnExplicitLineLabelsOn	"cnExplicitLineLabelsOn"
#define NhlNcnExplicitLegendLabelsOn	"cnExplicitLegendLabelsOn"
#define NhlNcnLegendLevelFlags		"cnLegendLevelFlags"
#define NhlNcnExplicitLabelBarLabelsOn	"cnExplicitLabelBarLabelsOn"
#define NhlNcnLabelBarEndLabelsOn	"cnLabelBarEndLabelsOn"
#define NhlNcnLabelBarEndStyle		"cnLabelBarEndStyle"
#define NhlNcnRasterModeOn		"cnRasterModeOn"
#define NhlNcnRasterCellSizeF		"cnRasterCellSizeF"
#define NhlNcnRasterSmoothingOn		"cnRasterSmoothingOn"
#define NhlNcnRasterSampleFactorF	"cnRasterSampleFactorF"
#define NhlNcnRasterMinCellSizeF	"cnRasterMinCellSizeF"
#define NhlNcnCyclicModeOn		"cnCyclicModeOn"

#define NhlNcnLevels			"cnLevels"
#define NhlNcnMonoLevelFlag		"cnMonoLevelFlag"
#define NhlNcnLevelFlag			"cnLevelFlag"
#define NhlNcnLevelFlags		"cnLevelFlags"
#define NhlNcnFillPalette               "cnFillPalette"
#define NhlNcnSpanFillPalette           "cnSpanFillPalette"
#define NhlNcnMonoFillColor		"cnMonoFillColor"
#define NhlNcnFillColor			"cnFillColor"
#define NhlNcnFillColors		"cnFillColors"
#define NhlNcnFillOpacityF		"cnFillOpacityF"
#define NhlNcnMonoFillPattern		"cnMonoFillPattern"
#define NhlNcnFillPattern		"cnFillPattern"
#define NhlNcnFillPatterns		"cnFillPatterns"
#define NhlNcnMonoFillScale		"cnMonoFillScale"
#define NhlNcnFillScaleF		"cnFillScaleF"
#define NhlNcnFillScales		"cnFillScales"
#define NhlNcnFillDotSizeF		"cnFillDotSizeF"
#define NhlNcnCellFillEdgeColor         "cnCellFillEdgeColor"
#define NhlNcnCellFillMissingValEdgeColor "cnCellFillMissingValEdgeColor"

#define NhlNcnMonoLineColor		"cnMonoLineColor"
#define NhlNcnLineColor			"cnLineColor"
#define NhlNcnLinePalette               "cnLinePalette"
#define NhlNcnSpanLinePalette           "cnSpanLinePalette"
#define NhlNcnLineColors		"cnLineColors"
#define NhlNcnMonoLineDashPattern	"cnMonoLineDashPattern"
#define NhlNcnLineDashPattern		"cnLineDashPattern"
#define NhlNcnLineDashPatterns		"cnLineDashPatterns"
#define NhlNcnMonoLineThickness		"cnMonoLineThickness"
#define NhlNcnLineThicknessF		"cnLineThicknessF"
#define NhlNcnLineThicknesses		"cnLineThicknesses"
#define NhlNcnMonoLineLabelFontColor	"cnMonoLineLabelFontColor"
#define NhlNcnLineLabelFontColor	"cnLineLabelFontColor"
#define NhlNcnLineLabelFontColors	"cnLineLabelFontColors"
#define NhlNcnLineLabelStrings		"cnLineLabelStrings"

#define NhlNcnLineDashSegLenF		"cnLineDashSegLenF"
#define NhlNcnLowUseHighLabelRes	"cnLowUseHighLabelRes"
#define NhlNcnHighUseLineLabelRes	"cnHighUseLineLabelRes"
#define NhlNcnConstFUseInfoLabelRes	"cnConstFUseInfoLabelRes"
#define NhlNcnLineLabelPlacementMode	"cnLineLabelPlacementMode"
#define NhlNcnLineLabelDensityF		"cnLineLabelDensityF"
#define NhlNcnLineLabelCount		"cnLineLabelCount"
#define NhlNcnHighLowLabelOverlapMode	"cnHighLowLabelOverlapMode"

#define NhlNcnLineLabelsOn		"cnLineLabelsOn"
#define NhlNcnLineLabelFormat		"cnLineLabelFormat"
#define NhlNcnLineLabelFontHeightF	"cnLineLabelFontHeightF"
#define NhlNcnLineLabelFont		"cnLineLabelFont"
#define NhlNcnLineLabelFontAspectF	"cnLineLabelFontAspectF"
#define NhlNcnLineLabelFontThicknessF	"cnLineLabelFontThicknessF"
#define NhlNcnLineLabelFontQuality	"cnLineLabelFontQuality"
#define NhlNcnLineLabelConstantSpacingF	"cnLineLabelConstantSpacingF"
#define NhlNcnLineLabelAngleF		"cnLineLabelAngleF"
#define NhlNcnLineLabelFuncCode		"cnLineLabelFuncCode"
#define NhlNcnLineLabelBackgroundColor	"cnLineLabelBackgroundColor"
#define NhlNcnLineLabelPerimOn		"cnLineLabelPerimOn"
#define NhlNcnLineLabelPerimSpaceF	"cnLineLabelPerimSpaceF"
#define NhlNcnLineLabelPerimThicknessF	"cnLineLabelPerimThicknessF"
#define NhlNcnLineLabelPerimColor	"cnLineLabelPerimColor"

#define NhlNcnHighLabelsOn		"cnHighLabelsOn"
#define NhlNcnHighLabelString		"cnHighLabelString"
#define NhlNcnHighLabelFormat		"cnHighLabelFormat"
#define NhlNcnHighLabelFontHeightF	"cnHighLabelFontHeightF"
#define NhlNcnHighLabelFont		"cnHighLabelFont"
#define NhlNcnHighLabelFontColor	"cnHighLabelFontColor"
#define NhlNcnHighLabelFontAspectF	"cnHighLabelFontAspectF"
#define NhlNcnHighLabelFontThicknessF	"cnHighLabelFontThicknessF"
#define NhlNcnHighLabelFontQuality	"cnHighLabelFontQuality"
#define NhlNcnHighLabelConstantSpacingF	"cnHighLabelConstantSpacingF"
#define NhlNcnHighLabelAngleF		"cnHighLabelAngleF"
#define NhlNcnHighLabelFuncCode		"cnHighLabelFuncCode"
#define NhlNcnHighLabelBackgroundColor	"cnHighLabelBackgroundColor"
#define NhlNcnHighLabelPerimOn		"cnHighLabelPerimOn"
#define NhlNcnHighLabelPerimSpaceF	"cnHighLabelPerimSpaceF"
#define NhlNcnHighLabelPerimThicknessF	"cnHighLabelPerimThicknessF"
#define NhlNcnHighLabelPerimColor	"cnHighLabelPerimColor"
#define NhlNcnHighLabelCount		"cnHighLabelCount"

#define NhlNcnLowLabelsOn		"cnLowLabelsOn"
#define NhlNcnLowLabelString		"cnLowLabelString"
#define NhlNcnLowLabelFormat		"cnLowLabelFormat"
#define NhlNcnLowLabelFontHeightF	"cnLowLabelFontHeightF"
#define NhlNcnLowLabelFont		"cnLowLabelFont"
#define NhlNcnLowLabelFontColor		"cnLowLabelFontColor"
#define NhlNcnLowLabelFontAspectF	"cnLowLabelFontAspectF"
#define NhlNcnLowLabelFontThicknessF	"cnLowLabelFontThicknessF"
#define NhlNcnLowLabelFontQuality	"cnLowLabelFontQuality"
#define NhlNcnLowLabelConstantSpacingF	"cnLowLabelConstantSpacingF"
#define NhlNcnLowLabelAngleF		"cnLowLabelAngleF"
#define NhlNcnLowLabelFuncCode		"cnLowLabelFuncCode"
#define NhlNcnLowLabelBackgroundColor	"cnLowLabelBackgroundColor"
#define NhlNcnLowLabelPerimOn		"cnLowLabelPerimOn"
#define NhlNcnLowLabelPerimSpaceF	"cnLowLabelPerimSpaceF"
#define NhlNcnLowLabelPerimThicknessF	"cnLowLabelPerimThicknessF"
#define NhlNcnLowLabelPerimColor	"cnLowLabelPerimColor"
#define NhlNcnLowLabelCount		"cnLowLabelCount"

#define NhlNcnInfoLabelOn		"cnInfoLabelOn"
#define NhlNcnInfoLabelString		"cnInfoLabelString"
#define NhlNcnInfoLabelFormat		"cnInfoLabelFormat"
#define NhlNcnInfoLabelFontHeightF	"cnInfoLabelFontHeightF"
#define NhlNcnInfoLabelTextDirection	"cnInfoLabelTextDirection"
#define NhlNcnInfoLabelFont		"cnInfoLabelFont"
#define NhlNcnInfoLabelFontColor	"cnInfoLabelFontColor"
#define NhlNcnInfoLabelFontAspectF	"cnInfoLabelFontAspectF"
#define NhlNcnInfoLabelFontThicknessF	"cnInfoLabelFontThicknessF"
#define NhlNcnInfoLabelFontQuality	"cnInfoLabelFontQuality"
#define NhlNcnInfoLabelConstantSpacingF	"cnInfoLabelConstantSpacingF"
#define NhlNcnInfoLabelAngleF		"cnInfoLabelAngleF"
#define NhlNcnInfoLabelFuncCode		"cnInfoLabelFuncCode"
#define NhlNcnInfoLabelBackgroundColor	"cnInfoLabelBackgroundColor"
#define NhlNcnInfoLabelPerimOn		"cnInfoLabelPerimOn"
#define NhlNcnInfoLabelPerimSpaceF	"cnInfoLabelPerimSpaceF"
#define NhlNcnInfoLabelPerimThicknessF	"cnInfoLabelPerimThicknessF"
#define NhlNcnInfoLabelPerimColor	"cnInfoLabelPerimColor"

#define NhlNcnInfoLabelZone		"cnInfoLabelZone"
#define NhlNcnInfoLabelSide		"cnInfoLabelSide"
#define NhlNcnInfoLabelJust		"cnInfoLabelJust"
#define NhlNcnInfoLabelParallelPosF	"cnInfoLabelParallelPosF"
#define NhlNcnInfoLabelOrthogonalPosF	"cnInfoLabelOrthogonalPosF"

#define NhlNcnNoDataLabelOn		"cnNoDataLabelOn"
#define NhlNcnNoDataLabelString		"cnNoDataLabelString"
#define NhlNcnConstFLabelOn		"cnConstFLabelOn"
#define NhlNcnConstFLabelString		"cnConstFLabelString"
#define NhlNcnConstFLabelFormat		"cnConstFLabelFormat"
#define NhlNcnConstFLabelFontHeightF	"cnConstFLabelFontHeightF"
#define NhlNcnConstFLabelTextDirection	"cnConstFLabelTextDirection"
#define NhlNcnConstFLabelFont		"cnConstFLabelFont"
#define NhlNcnConstFLabelFontColor	"cnConstFLabelFontColor"
#define NhlNcnConstFLabelFontAspectF	"cnConstFLabelFontAspectF"
#define NhlNcnConstFLabelFontThicknessF	"cnConstFLabelFontThicknessF"
#define NhlNcnConstFLabelFontQuality	"cnConstFLabelFontQuality"
#define NhlNcnConstFLabelConstantSpacingF "cnConstFLabelConstantSpacingF"
#define NhlNcnConstFLabelAngleF		"cnConstFLabelAngleF"
#define NhlNcnConstFLabelFuncCode	"cnConstFLabelFuncCode"
#define NhlNcnConstFLabelBackgroundColor "cnConstFLabelBackgroundColor"
#define NhlNcnConstFLabelPerimOn	"cnConstFLabelPerimOn"
#define NhlNcnConstFLabelPerimSpaceF	"cnConstFLabelPerimSpaceF"
#define NhlNcnConstFLabelPerimThicknessF "cnConstFLabelPerimThicknessF"
#define NhlNcnConstFLabelPerimColor	"cnConstFLabelPerimColor"

#define NhlNcnConstFLabelZone		"cnConstFLabelZone"
#define NhlNcnConstFLabelSide		"cnConstFLabelSide"
#define NhlNcnConstFLabelJust		"cnConstFLabelJust"
#define NhlNcnConstFLabelParallelPosF	"cnConstFLabelParallelPosF"
#define NhlNcnConstFLabelOrthogonalPosF	"cnConstFLabelOrthogonalPosF"
#define NhlNcnConstFEnableFill		"cnConstFEnableFill"

#define NhlNcnMissingValPerimOn		"cnMissingValPerimOn"
#define NhlNcnMissingValPerimGridBoundOn "cnMissingValPerimGridBoundOn"
#define NhlNcnMissingValPerimThicknessF	"cnMissingValPerimThicknessF"
#define NhlNcnMissingValPerimDashPattern "cnMissingValPerimDashPattern"
#define NhlNcnMissingValPerimColor	"cnMissingValPerimColor"
#define NhlNcnMissingValFillColor	"cnMissingValFillColor"
#define NhlNcnMissingValFillPattern	"cnMissingValFillPattern"
#define NhlNcnMissingValFillScaleF	"cnMissingValFillScaleF"

#define NhlNcnGridBoundPerimOn		"cnGridBoundPerimOn"
#define NhlNcnGridBoundPerimThicknessF	"cnGridBoundPerimThicknessF"
#define NhlNcnGridBoundPerimDashPattern "cnGridBoundPerimDashPattern"
#define NhlNcnGridBoundPerimColor	"cnGridBoundPerimColor"
#define NhlNcnGridBoundFillColor	"cnGridBoundFillColor"
#define NhlNcnGridBoundFillPattern	"cnGridBoundFillPattern"
#define NhlNcnGridBoundFillScaleF	"cnGridBoundFillScaleF"

#define NhlNcnOutOfRangePerimOn		"cnOutOfRangePerimOn"
#define NhlNcnOutOfRangePerimThicknessF	"cnOutOfRangePerimThicknessF"
#define NhlNcnOutOfRangePerimDashPattern "cnOutOfRangePerimDashPattern"
#define NhlNcnOutOfRangePerimColor	"cnOutOfRangePerimColor"
#define NhlNcnOutOfRangeFillColor	"cnOutOfRangeFillColor"
#define NhlNcnOutOfRangeFillPattern	"cnOutOfRangeFillPattern"
#define NhlNcnOutOfRangeFillScaleF	"cnOutOfRangeFillScaleF"

#define NhlNcnDumpAreaMap		"cnDumpAreaMap"
#define NhlNcnFixFillBleed		"cnFixFillBleed"
#define NhlNcnAreaMapCRange		"cnAreaMapCRange"
#define NhlNcnConpackParams		"cnConpackParams"
#define NhlNcnOutputGriddedData		"cnOutputGriddedData"
#define NhlNcnOutputFileName		"cnOutputFileName"
#define NhlNcnVerboseTriangleInfo       "cnVerboseTriangleInfo"

/*
 * ContourPlot class resources
 */

#define NhlCcnScalarFieldData		"CnScalarFieldData"
#define NhlCcnOutOfRangeValF		"CnOutOfRangeValF"

#define NhlCcnLevelCount		"CnLevelCount"		/* read-only */
#define NhlCcnLabelMasking		"CnLabelMasking"
#define NhlCcnLineLabelInterval		"CnLineLabelInterval"
#define NhlCcnLabelDrawOrder		"CnLabelDrawOrder"
#define NhlCcnLineDrawOrder		"CnLineDrawOrder"
#define NhlCcnFillDrawOrder		"CnFillDrawOrder"
#define NhlCcnLinesOn			"CnLinesOn"
#define NhlCcnFillOn			"CnFillOn"
#define NhlCcnFillMode			"CnFillMode"
#define NhlCcnCellFillEdgeColor         "CnCellFillEdgeColor"
#define NhlCcnCellFillMissingValEdgeColor "CnCellFillMissingValEdgeColor"

#define NhlCcnLabelScalingMode		"CnLabelScalingMode"
#define NhlCcnLabelScaleValueF		"CnLabelScaleValueF"
#define NhlCcnLabelScaleFactorF		"CnLabelScaleFactorF" /*read-only*/
#define NhlCcnMaxDataValueFormat	"CnMaxDataValueFormat"
#define NhlCcnSmoothingOn		"CnSmoothingOn"
#define NhlCcnSmoothingTensionF		"CnSmoothingTensionF"
#define NhlCcnSmoothingDistanceF	"CnSmoothingDistanceF"
#define NhlCcnMaxPointDistanceF		"CnMaxPointDistanceF"

#define NhlCcnExplicitLineLabelsOn	"CnExplicitLineLabelsOn"
#define NhlCcnExplicitLabelBarLabelsOn	"CnExplicitLabelBarLabelsOn"
#define NhlCcnLabelBarEndLabelsOn	"CnLabelBarEndLabelsOn"
#define NhlCcnLabelBarEndStyle		"CnLabelBarEndStyle"
#define NhlCcnExplicitLegendLabelsOn	"CnExplicitLegendLabelsOn"
#define NhlCcnLegendLevelFlags		"CnLegendLevelFlags"
#define NhlCcnRasterModeOn		"CnRasterModeOn"
#define NhlCcnRasterCellSizeF		"CnRasterCellSizeF"
#define NhlCcnRasterSmoothingOn		"CnRasterSmoothingOn"
#define NhlCcnRasterSampleFactorF	"CnRasterSampleFactorF"
#define NhlCcnRasterMinCellSizeF	"CnRasterMinCellSizeF"
#define NhlCcnCyclicModeOn		"CnCyclicModeOn"

#define NhlCcnMonoLevelFlag		"CnMonoLevelFlag"
#define NhlCcnLevelFlag			"CnLevelFlag"
#define NhlCcnLevelFlags		"CnLevelFlags"
#define NhlCcnFillPalette               "CnFillPalette"
#define NhlCcnSpanFillPalette           "CnSpanFillPalette"
#define NhlCcnMonoFillColor		"CnMonoFillColor"
#define NhlCcnFillColors		"CnFillColors"
#define NhlCcnFillOpacityF              "CnFillOpacityF"
#define NhlCcnMonoFillPattern		"CnMonoFillPattern"
#define NhlCcnFillPatterns		"CnFillPatterns"
#define NhlCcnMonoFillScale		"CnMonoFillScale"
#define NhlCcnFillScales		"CnFillScales"

#define NhlCcnMonoLineColor		"CnMonoLineColor"
#define NhlCcnLinePalette               "CnLinePalette"
#define NhlCcnSpanLinePalette           "CnSpanLinePalette"
#define NhlCcnLineColors		"CnLineColors"
#define NhlCcnMonoLineDashPattern	"CnMonoLineDashPattern"
#define NhlCcnLineDashPatterns		"CnLineDashPatterns"
#define NhlCcnMonoLineThickness		"CnMonoLineThickness"
#define NhlCcnLineThicknesses		"CnLineThicknesses"
#define NhlCcnMonoLineLabelFontColor	"CnMonoLineLabelFontColor"
#define NhlCcnLineLabelFontColors	"CnLineLabelFontColors"
#define NhlCcnLineLabelStrings		"CnLineLabelStrings"
#define NhlCcnLineLabelAngleF		"CnLineLabelAngleF"

#define NhlCcnLineLabelPlacementMode	"CnLineLabelPlacementMode"
#define NhlCcnLineLabelDensityF		"CnLineLabelDensityF"
#define NhlCcnLineLabelCount		"CnLineLabelCount"

#define NhlCcnLowUseHighLabelRes	"CnLowUseHighLabelRes"
#define NhlCcnHighUseLineLabelRes	"CnHighUseLineLabelRes"
#define NhlCcnConstFUseInfoLabelRes	"CnConstFUseInfoLabelRes"
#define NhlCcnHighLowLabelOverlapMode	"CnHighLowLabelOverlapMode"


#define NhlCcnHighLabelString		"CnHighLabelString"
#define NhlCcnHighLabelCount		"CnHighLabelCount"
#define NhlCcnLowLabelString		"CnLowLabelString"
#define NhlCcnLowLabelCount		"CnLowLabelCount"


#define NhlCcnInfoLabelString		"CnInfoLabelString"
#define NhlCcnInfoLabelSide		"CnInfoLabelSide"
#define NhlCcnInfoLabelPosition		"CnInfoLabelPosition"
#define NhlCcnInfoLabelJustification	"CnInfoLabelJusification"
#define NhlCcnInfoLabelXOffsetF		"CnInfoLabelXOffsetF"
#define NhlCcnInfoLabelYOffsetF		"CnInfoLabelYOffsetF"

#define NhlCcnInfoLabelZone		"CnInfoLabelZone"
#define NhlCcnInfoLabelSide		"CnInfoLabelSide"
#define NhlCcnInfoLabelJust		"CnInfoLabelJust"
#define NhlCcnInfoLabelParallelPosF	"CnInfoLabelParallelPosF"
#define NhlCcnInfoLabelOrthogonalPosF	"CnInfoLabelOrthogonalPosF"

#define NhlCcnNoDataLabelString		"CnNoDataLabelString"
#define NhlCcnConstFLabelString		"CnConstFLabelString"

#define NhlCcnConstFLabelZone		"CnConstFLabelZone"
#define NhlCcnConstFLabelSide		"CnConstFLabelSide"
#define NhlCcnConstFLabelJust		"CnConstFLabelJust"
#define NhlCcnConstFLabelParallelPosF	"CnConstFLabelParallelPosF"
#define NhlCcnConstFLabelOrthogonalPosF	"CnConstFLabelOrthogonalPosF"
#define NhlCcnConstFEnableFill		"CnConstFEnableFill"

#define NhlCcnDumpAreaMap		"CnDumpAreaMap"
#define NhlCcnFixFillBleed		"CnFixFillBleed"
#define NhlCcnAreaMapCRange		"CnAreaMapCRange"
#define NhlCcnConpackParams		"CnConpackParams"
#define NhlCcnOutputGriddedData		"CnOutputGriddedData"
#define NhlCcnOutputFileName		"CnOutputFileName"
#define NhlCcnVerboseTriangleInfo       "CnVerboseTriangleInfo"


/*
 * These class resources have been eliminated
 */
#if 0

#define NhlCcnLevelSelectionMode	"CnLevelSelectionMode"
#define NhlCcnMaxLevelCount		"CnMaxLevelCount"
#define NhlCcnLevelSpacingF		"CnLevelSpacingF"
#define NhlCcnMinLevelValF		"CnMinLevelValF"
#define NhlCcnMaxLevelValF		"CnMaxLevelValF"
#define NhlCcnLevels			"CnLevels"

#define NhlCcnFillBackgroundColor	"CnFillBackgroundColor"
#define NhlCcnFillColor			"CnFillColor"
#define NhlCcnFillPattern		"CnFillPattern"
#define NhlCcnFillScaleF		"CnFillScaleF"
#define NhlCcnLineColor			"CnLineColor"
#define NhlCcnLineDashPattern		"CnLineDashPattern"
#define NhlCcnLineThicknessF		"CnLineThicknessF"
#define NhlCcnLineLabelsOn		"CnLineLabelsOn"
#define NhlCcnLineLabelFontColor	"CnLineLabelFontColor"
#define NhlCcnLineDashSegLenF		"CnLineDashSegLenF"
#define NhlCcnLineLabelFontHeightF	"CnLineLabelFontHeightF"
#define NhlCcnLineLabelFont		"CnLineLabelFont"
#define NhlCcnLineLabelFontAspectF	"CnLineLabelFontAspectF"
#define NhlCcnLineLabelFontThicknessF	"CnLineLabelFontThicknessF"
#define NhlCcnLineLabelFontQuality	"CnLineLabelFontQuality"
#define NhlCcnLineLabelConstantSpacingF	"CnLineLabelConstantSpacingF"
#define NhlCcnLineLabelFuncCode		"CnLineLabelFuncCode"
#define NhlCcnLineLabelBackgroundColor	"CnLineLabelBackgroundColor"
#define NhlCcnLineLabelFormat		"CnLineLabelFormat"
#define NhlCcnLineLabelPerimSpaceF	"CnLineLabelPerimSpaceF"
#define NhlCcnLineLabelPerimOn		"CnLineLabelPerimOn"
#define NhlCcnLineLabelPerimThicknessF	"CnLineLabelPerimThicknessF"
#define NhlCcnLineLabelPerimColor	"CnLineLabelPerimColor"
#define NhlCcnHighLabelFontHeightF	"CnHighLabelFontHeightF"
#define NhlCcnHighLabelsOn		"CnHighLabelsOn"
#define NhlCcnHighLabelFont		"CnHighLabelFont"
#define NhlCcnHighLabelFontColor	"CnHighLabelFontColor"
#define NhlCcnHighLabelFontAspectF	"CnHighLabelFontAspectF"
#define NhlCcnHighLabelFontThicknessF	"CnHighLabelFontThicknessF"
#define NhlCcnHighLabelFontQuality	"CnHighLabelFontQuality"
#define NhlCcnHighLabelConstantSpacingF	"CnHighLabelConstantSpacingF"
#define NhlCcnHighLabelAngleF		"CnHighLabelAngleF"
#define NhlCcnHighLabelFuncCode		"CnHighLabelFuncCode"
#define NhlCcnHighLabelBackgroundColor	"CnHighLabelBackgroundColor"
#define NhlCcnHighLabelFormat		"CnHighLabelFormat"
#define NhlCcnHighLabelPerimSpaceF	"CnHighLabelPerimSpaceF"
#define NhlCcnHighLabelPerimOn		"CnHighLabelPerimOn"
#define NhlCcnHighLabelPerimThicknessF	"CnHighLabelPerimThicknessF"
#define NhlCcnHighLabelPerimColor	"CnHighLabelPerimColor"
#define NhlCcnLowLabelsOn		"CnLowLabelsOn"
#define NhlCcnLowLabelFontHeightF	"CnLowLabelFontHeightF"
#define NhlCcnLowLabelFont		"CnLowLabelFont"
#define NhlCcnLowLabelFontColor		"CnLowLabelFontColor"
#define NhlCcnLowLabelFontAspectF	"CnLowLabelFontAspectF"
#define NhlCcnLowLabelFontThicknessF	"CnLowLabelFontThicknessF"
#define NhlCcnLowLabelFontQuality	"CnLowLabelFontQuality"
#define NhlCcnLowLabelConstantSpacingF	"CnLowLabelConstantSpacingF"
#define NhlCcnLowLabelAngleF		"CnLowLabelAngleF"
#define NhlCcnLowLabelFuncCode		"CnLowLabelFuncCode"
#define NhlCcnLowLabelBackgroundColor	"CnLowLabelBackgroundColor"
#define NhlCcnLowLabelFormat		"CnLowLabelFormat"
#define NhlCcnLowLabelPerimSpaceF	"CnLowLabelPerimSpaceF"
#define NhlCcnLowLabelPerimOn		"CnLowLabelPerimOn"
#define NhlCcnLowLabelPerimThicknessF	"CnLowLabelPerimThicknessF"
#define NhlCcnLowLabelPerimColor	"CnLowLabelPerimColor"
#define NhlCcnInfoLabelOn		"CnInfoLabelOn"
#define NhlCcnInfoLabelFontHeightF	"CnInfoLabelFontHeightF"
#define NhlCcnInfoLabelTextDirection	"CnInfoLabelTextDirection"
#define NhlCcnInfoLabelFont		"CnInfoLabelFont"
#define NhlCcnInfoLabelFontColor	"CnInfoLabelFontColor"
#define NhlCcnInfoLabelFontAspectF	"CnInfoLabelFontAspectF"
#define NhlCcnInfoLabelFontThicknessF	"CnInfoLabelFontThicknessF"
#define NhlCcnInfoLabelFontQuality	"CnInfoLabelFontQuality"
#define NhlCcnInfoLabelConstantSpacingF	"CnInfoLabelConstantSpacingF"
#define NhlCcnInfoLabelAngleF		"CnInfoLabelAngleF"
#define NhlCcnInfoLabelFuncCode		"CnInfoLabelFuncCode"
#define NhlCcnInfoLabelBackgroundColor	"CnInfoLabelBackgroundColor"
#define NhlCcnInfoLabelFormat		"CnInfoLabelFormat"
#define NhlCcnInfoLabelPerimSpaceF	"CnInfoLabelPerimSpaceF"
#define NhlCcnInfoLabelPerimOn		"CnInfoLabelPerimOn"
#define NhlCcnInfoLabelPerimThicknessF	"CnInfoLabelPerimThicknessF"
#define NhlCcnInfoLabelPerimColor	"CnInfoLabelPerimColor"
#define NhlCcnNoDataLabelOn		"CnNoDataLabelOn"
#define NhlCcnConstFLabelOn		"CnConstFLabelOn"
#define NhlCcnConstFLabelFontHeightF	"CnConstFLabelFontHeightF"
#define NhlCcnConstFLabelTextDirection	"CnConstFLabelTextDirection"
#define NhlCcnConstFLabelFont		"CnConstFLabelFont"
#define NhlCcnConstFLabelFontColor	"CnConstFLabelFontColor"
#define NhlCcnConstFLabelFontAspectF	"CnConstFLabelFontAspectF"
#define NhlCcnConstFLabelFontThicknessF	"CnConstFLabelFontThicknessF"
#define NhlCcnConstFLabelFontQuality	"CnConstFLabelFontQuality"
#define NhlCcnConstFLabelConstantSpacingF "CnConstFLabelConstantSpacingF"
#define NhlCcnConstFLabelAngleF		"CnConstFLabelAngleF"
#define NhlCcnConstFLabelFuncCode	"CnConstFLabelFuncCode"
#define NhlCcnConstFLabelBackgroundColor "CnConstFLabelBackgroundColor"
#define NhlCcnConstFLabelFormat		"CnConstFLabelFormat"
#define NhlCcnConstFLabelPerimSpaceF	"CnConstFLabelPerimSpaceF"
#define NhlCcnConstFLabelPerimOn	"CnConstFLabelPerimOn"
#define NhlCcnConstFLabelPerimThicknessF "CnConstFLabelPerimThicknessF"
#define NhlCcnConstFLabelPerimColor	"CnConstFLabelPerimColor"

#if 0
#define NhlCcnMissingValPerimOn		"CnMissingValPerimOn"
#define NhlCcnMissingValPerimGridBoundOn "CnMissingValPerimGridBoundOn"
#define NhlCcnMissingValPerimThicknessF	"CnMissingValPerimThicknessF"
#define NhlCcnMissingValPerimDashPattern "CnMissingValPerimDashPattern"
#define NhlCcnMissingValPerimColor	"CnMissingValPerimColor"
#define NhlCcnMissingValFillColor	"CnMissingValFillColor"
#define NhlCcnMissingValFillPattern	"CnMissingValFillPattern"
#define NhlCcnMissingValFillScaleF	"CnMissingValFillScaleF"
#define NhlCcnGridBoundPerimOn		"CnGridBoundPerimOn"
#define NhlCcnGridBoundPerimThicknessF	"CnGridBoundPerimThicknessF"
#define NhlCcnGridBoundPerimDashPattern "CnGridBoundPerimDashPattern"
#define NhlCcnGridBoundPerimColor	"CnGridBoundPerimColor"
#define NhlCcnGridBoundFillColor	"CnGridBoundFillColor"
#define NhlCcnGridBoundFillPattern	"CnGridBoundFillPattern"
#define NhlCcnGridBoundFillScaleF	"CnGridBoundFillScaleF"
#define NhlCcnOutOfRangePerimOn		"CnOutOfRangePerimOn"
#define NhlCcnOutOfRangePerimThicknessF	"CnOutOfRangePerimThicknessF"
#define NhlCcnOutOfRangePerimDashPattern "CnOutOfRangePerimDashPattern"
#define NhlCcnOutOfRangePerimColor	"CnOutOfRangePerimColor"
#define NhlCcnOutOfRangeFillColor	"CnOutOfRangeFillColor"
#define NhlCcnOutOfRangeFillPattern	"CnOutOfRangeFillPattern"
#define NhlCcnOutOfRangeFillScaleF	"CnOutOfRangeFillScaleF"
#endif

#endif

extern NhlClass			NhlcontourPlotClass;

#endif /*_NCONTOURPLOT_h */
