/*
 *      $Id: ContourPlot.h,v 1.3 1995-04-07 00:39:48 dbrown Exp $
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

typedef enum _NhlcnLevelSelectionMode { 
	NhlAUTOMATICLEVELS, NhlMANUALLEVELS, 
	NhlEXPLICITLEVELS, NhlEQUALSPACEDLEVELS } NhlcnLevelSelectionMode;

#define NhlTcnLevelSelectionMode "cnlevelselectionmode"

/* Level usage modes */

typedef enum _NhlcnLevelUseMode {
	NhlNOLINE, NhlLINEONLY, 
	NhlLABELONLY, NhlLINEANDLABEL
} NhlcnLevelUseMode;

#define NhlTcnLevelUseMode	"cnlevelusemode"
#define NhlTcnLevelUseModeGenArray    NhlTcnLevelUseMode NhlTGenArray

/* Line label spacing */

typedef enum _NhlcnLineLabelPlacementMode {
	NhlCONSTANT, 
	NhlRANDOMIZED,
	NhlCOMPUTED
} NhlcnLineLabelPlacementMode;

#define NhlTcnLineLabelPlacementMode	"cnLineLabelPlacementmode"

/* Label scaling mode */

typedef enum _NhlcnLabelScalingMode {
	NhlSCALEFACTOR, NhlCONFINETORANGE,
	NhlTRIMZEROS,NhlMAXSIGDIGITSLEFT,
        NhlINTEGERLINELABELS
} NhlcnLabelScalingMode;

#define NhlTcnLabelScalingMode	"cnlabelscalinggmode"

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

#define NhlTcnHighLowLabelOverlapMode	"cnhighlowlabeloverlapmode"

/*
 * DataDep objects resources
 */

#define NhlNcnExplicitLabels		"cnExplicitLabels"
#define NhlCcnExplicitLabels		"CnExplicitLabels"

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

#define NhlNcnLabelScalingMode		"cnLabelScalingMode"
#define NhlNcnLabelScaleValueF		"cnLabelScaleValueF"
#define NhlNcnLabelScaleFactorF		"cnLabelScaleFactorF" /*read-only*/
#define NhlNcnMaxDataValueFormat	"cnMaxDataValueFormat"
#define NhlNcnSmoothingOn		"cnSmoothingOn"
#define NhlNcnSmoothingTensionF		"cnSmoothingTensionF"
#define NhlNcnSmoothingDistanceF	"cnSmoothingDistanceF"
#define NhlNcnCheckPointDistance	"cnCheckPointDistance"
#define NhlNcnMaxPointDistanceF		"cnMaxPointDistanceF"


#define NhlNcnLevels			"cnLevels"
#define NhlNcnMonoLevelFlag		"cnMonoLevelFlag"
#define NhlNcnLevelFlag			"cnLevelFlag"
#define NhlNcnLevelFlags		"cnLevelFlags"
#define NhlNcnMonoFillColor		"cnMonoFillColor"
#define NhlNcnFillColor			"cnFillColor"
#define NhlNcnFillColors		"cnFillColors"
#define NhlNcnMonoFillPattern		"cnMonoFillPattern"
#define NhlNcnFillPattern		"cnFillPattern"
#define NhlNcnFillPatterns		"cnFillPatterns"
#define NhlNcnMonoFillScale		"cnMonoFillScale"
#define NhlNcnFillScaleF		"cnFillScaleF"
#define NhlNcnFillScales		"cnFillScales"

#define NhlNcnMonoLineColor		"cnMonoLineColor"
#define NhlNcnLineColor			"cnLineColor"
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

#define NhlNcnConstFLabelOn		"cnConstFLabelOn"
#define NhlNcnConstFLabelString		"cnConstFLabelString"
#define NhlNcnConstFLabelFormat		"cnconstFLabelFormat"
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

#define NhlNcnMissingValPerimOn		"cnMissingValPerimOn"
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
#define NhlNcnAreaMapCRange		"cnAreaMapCRange"

/*
 * ContourPlot class resources
 */

#define NhlCcnScalarFieldData		"CnScalarFieldData"
#define NhlCcnOutOfRangeValF		"CnOutOfRangeValF"

#define NhlCcnLevelCount		"CnLevelCount"		/* read-only */
#define NhlCcnLevelSelectionMode	"CnLevelSelectionMode"
#define NhlCcnMaxLevelCount		"CnMaxLevelCount"
#define NhlCcnLevelSpacingF		"CnLevelSpacingF"
#define NhlCcnLabelMasking		"CnLabelMasking"
#define NhlCcnMinLevelValF		"CnMinLevelValF"
#define NhlCcnMaxLevelValF		"CnMaxLevelValF"
#define NhlCcnLineLabelInterval		"CnLineLabelInterval"
#define NhlCcnLabelDrawOrder		"CnLabelDrawOrder"
#define NhlCcnLineDrawOrder		"CnLineDrawOrder"
#define NhlCcnFillDrawOrder		"CnFillDrawOrder"
#define NhlCcnLinesOn			"CnLinesOn"
#define NhlCcnFillOn			"CnFillOn"
#define NhlCcnFillBackgroundColor	"CnFillBackgroundColor"

#define NhlCcnLabelScalingMode		"CnLabelScalingMode"
#define NhlCcnLabelScaleValueF		"CnLabelScaleValueF"
#define NhlCcnLabelScaleFactorF		"CnLabelScaleFactorF" /*read-only*/
#define NhlCcnMaxDataValueFormat	"CnMaxDataValueFormat"
#define NhlCcnSmoothingOn		"CnSmoothingOn"
#define NhlCcnSmoothingTensionF		"CnSmoothingTensionF"
#define NhlCcnSmoothingDistanceF	"CnSmoothingDistanceF"
#define NhlCcnCheckPointDistance	"CnCheckPointDistance"
#define NhlCcnMaxPointDistanceF		"CnMaxPointDistanceF"

#define NhlCcnLevels			"CnLevels"
#define NhlCcnMonoLevelFlag		"CnMonoLevelFlag"
#define NhlCcnLevelFlag			"CnLevelFlag"
#define NhlCcnLevelFlags		"CnLevelFlags"
#define NhlCcnMonoFillColor		"CnMonoFillColor"
#define NhlCcnFillColor			"CnFillColor"
#define NhlCcnFillColors		"CnFillColors"
#define NhlCcnMonoFillPattern		"CnMonoFillPattern"
#define NhlCcnFillPattern		"CnFillPattern"
#define NhlCcnFillPatterns		"CnFillPatterns"
#define NhlCcnMonoFillScale		"CnMonoFillScale"
#define NhlCcnFillScaleF		"CnFillScaleF"
#define NhlCcnFillScales		"CnFillScales"

#define NhlCcnMonoLineColor		"CnMonoLineColor"
#define NhlCcnLineColor			"CnLineColor"
#define NhlCcnLineColors		"CnLineColors"
#define NhlCcnMonoLineDashPattern	"CnMonoLineDashPattern"
#define NhlCcnLineDashPattern		"CnLineDashPattern"
#define NhlCcnLineDashPatterns		"CnLineDashPatterns"
#define NhlCcnMonoLineThickness		"CnMonoLineThickness"
#define NhlCcnLineThicknessF		"CnLineThicknessF"
#define NhlCcnLineThicknesses		"CnLineThicknesses"
#define NhlCcnMonoLineLabelFontColor	"CnMonoLineLabelFontColor"
#define NhlCcnLineLabelFontColor	"CnLineLabelFontColor"
#define NhlCcnLineLabelFontColors	"CnLineLabelFontColors"
#define NhlCcnLineLabelStrings		"CnLineLabelStrings"

#define NhlCcnLineDashSegLenF		"CnLineDashSegLenF"
#define NhlCcnLineLabelPlacementMode	"CnLineLabelPlacementMode"

#define NhlCcnLowUseHighLabelRes	"CnLowUseHighLabelRes"
#define NhlCcnHighUseLineLabelRes	"CnHighUseLineLabelRes"
#define NhlCcnConstFUseInfoLabelRes	"CnConstFUseInfoLabelRes"
#define NhlCcnHighLowLabelOverlapMode	"CnHighLowLabelOverlapMode"

#define NhlCcnLineLabelsOn		"CnLineLabelsOn"
#define NhlCcnLineLabelFormat		"CnLineLabelFormat"
#define NhlCcnLineLabelFontHeightF	"CnLineLabelFontHeightF"
#define NhlCcnLineLabelFont		"CnLineLabelFont"
#define NhlCcnLineLabelFontAspectF	"CnLineLabelFontAspectF"
#define NhlCcnLineLabelFontThicknessF	"CnLineLabelFontThicknessF"
#define NhlCcnLineLabelFontQuality	"CnLineLabelFontQuality"
#define NhlCcnLineLabelConstantSpacingF	"CnLineLabelConstantSpacingF"
#define NhlCcnLineLabelAngleF		"CnLineLabelAngleF"
#define NhlCcnLineLabelFuncCode		"CnLineLabelFuncCode"
#define NhlCcnLineLabelBackgroundColor	"CnLineLabelBackgroundColor"
#define NhlCcnLineLabelPerimOn		"CnLineLabelPerimOn"
#define NhlCcnLineLabelPerimSpaceF	"CnLineLabelPerimSpaceF"
#define NhlCcnLineLabelPerimThicknessF	"CnLineLabelPerimThicknessF"
#define NhlCcnLineLabelPerimColor	"CnLineLabelPerimColor"

#define NhlCcnHighLabelsOn		"CnHighLabelsOn"
#define NhlCcnHighLabelString		"CnHighLabelString"
#define NhlCcnHighLabelFormat		"CnHighLabelFormat"
#define NhlCcnHighLabelFontHeightF	"CnHighLabelFontHeightF"
#define NhlCcnHighLabelFont		"CnHighLabelFont"
#define NhlCcnHighLabelFontColor	"CnHighLabelFontColor"
#define NhlCcnHighLabelFontAspectF	"CnHighLabelFontAspectF"
#define NhlCcnHighLabelFontThicknessF	"CnHighLabelFontThicknessF"
#define NhlCcnHighLabelFontQuality	"CnHighLabelFontQuality"
#define NhlCcnHighLabelConstantSpacingF	"CnHighLabelConstantSpacingF"
#define NhlCcnHighLabelAngleF		"CnHighLabelAngleF"
#define NhlCcnHighLabelFuncCode		"CnHighLabelFuncCode"
#define NhlCcnHighLabelBackgroundColor	"CnHighLabelBackgroundColor"
#define NhlCcnHighLabelPerimOn		"CnHighLabelPerimOn"
#define NhlCcnHighLabelPerimSpaceF	"CnHighLabelPerimSpaceF"
#define NhlCcnHighLabelPerimThicknessF	"CnHighLabelPerimThicknessF"
#define NhlCcnHighLabelPerimColor	"CnHighLabelPerimColor"

#define NhlCcnLowLabelsOn		"CnLowLabelsOn"
#define NhlCcnLowLabelString		"CnLowLabelString"
#define NhlCcnLowLabelFormat		"CnLowLabelFormat"
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
#define NhlCcnLowLabelPerimOn		"CnLowLabelPerimOn"
#define NhlCcnLowLabelPerimSpaceF	"CnLowLabelPerimSpaceF"
#define NhlCcnLowLabelPerimThicknessF	"CnLowLabelPerimThicknessF"
#define NhlCcnLowLabelPerimColor	"CnLowLabelPerimColor"


#define NhlCcnInfoLabelOn		"CnInfoLabelOn"
#define NhlCcnInfoLabelString		"CnInfoLabelString"
#define NhlCcnInfoLabelFormat		"CnInfoLabelFormat"
#define NhlCcnInfoLabelSide		"CnInfoLabelSide"
#define NhlCcnInfoLabelPosition		"CnInfoLabelPosition"
#define NhlCcnInfoLabelJustification	"CnInfoLabelJusification"
#define NhlCcnInfoLabelXOffsetF		"CnInfoLabelXOffsetF"
#define NhlCcnInfoLabelYOffsetF		"CnInfoLabelYOffsetF"
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
#define NhlCcnInfoLabelPerimOn		"CnInfoLabelPerimOn"
#define NhlCcnInfoLabelPerimSpaceF	"CnInfoLabelPerimSpaceF"
#define NhlCcnInfoLabelPerimThicknessF	"CnInfoLabelPerimThicknessF"
#define NhlCcnInfoLabelPerimColor	"CnInfoLabelPerimColor"

#define NhlCcnInfoLabelZone		"CnInfoLabelZone"
#define NhlCcnInfoLabelSide		"CnInfoLabelSide"
#define NhlCcnInfoLabelJust		"CnInfoLabelJust"
#define NhlCcnInfoLabelParallelPosF	"CnInfoLabelParallelPosF"
#define NhlCcnInfoLabelOrthogonalPosF	"CnInfoLabelOrthogonalPosF"

#define NhlCcnConstFLabelOn		"CnConstFLabelOn"
#define NhlCcnConstFLabelString		"CnConstFLabelString"
#define NhlCcnConstFLabelFormat		"CnConstFLabelFormat"
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
#define NhlCcnConstFLabelPerimOn	"CnConstFLabelPerimOn"
#define NhlCcnConstFLabelPerimSpaceF	"CnConstFLabelPerimSpaceF"
#define NhlCcnConstFLabelPerimThicknessF "CnConstFLabelPerimThicknessF"
#define NhlCcnConstFLabelPerimColor	"CnConstFLabelPerimColor"

#define NhlCcnConstFLabelZone		"CnConstFLabelZone"
#define NhlCcnConstFLabelSide		"CnConstFLabelSide"
#define NhlCcnConstFLabelJust		"CnConstFLabelJust"
#define NhlCcnConstFLabelParallelPosF	"CnConstFLabelParallelPosF"
#define NhlCcnConstFLabelOrthogonalPosF	"CnConstFLabelOrthogonalPosF"

#define NhlCcnMissingValPerimOn		"CnMissingValPerimOn"
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

#define NhlCcnDumpAreaMap		"CnDumpAreaMap"
#define NhlCcnAreaMapCRange		"CnAreaMapCRange"

extern NhlLayerClass			NhlcontourPlotLayerClass;

#endif /*_NCONTOURPLOT_h */
