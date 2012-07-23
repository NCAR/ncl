/*
 *      $Id: VectorPlot.h,v 1.9.12.1 2010-03-17 20:47:07 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VectorPlot.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 28 11:44:30 MDT 1995
 *
 *	Description:	Public header for VectorPlot class.
 */

#ifndef _NVECTORPLOT_h
#define _NVECTORPLOT_h

#include <ncarg/hlu/Workspace.h>
#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/VectorField.h>
#include <ncarg/hlu/ScalarField.h>

typedef enum _NhlVectorPositionMode {
	NhlARROWHEAD,
	NhlARROWTAIL,
	NhlARROWCENTER
} NhlVectorPositionMode;
#define NhlTVectorPositionMode	"VectorPositionMode"

typedef enum _NhlVectorGlyphStyle {
	NhlLINEARROW,
	NhlFILLARROW,
	NhlWINDBARB,
	NhlCURLYVECTOR
} NhlVectorGlyphStyle;
#define NhlTVectorGlyphStyle		"VectorGlyphStyle"

/*
 * VectorPlot instance resources
 */

#define NhlNvcVectorFieldData		"vcVectorFieldData"
#define NhlNvcScalarFieldData		"vcScalarFieldData"
#define NhlNvcMapDirection		"vcMapDirection"
#define NhlNvcPositionMode		"vcPositionMode"
#define NhlNvcVectorDrawOrder		"vcVectorDrawOrder"
#define NhlNvcGlyphStyle		"vcGlyphStyle"
#define NhlNvcGlyphOpacityF     "vcGlyphOpacityF"

#define NhlNvcMinDistanceF		"vcMinDistanceF"
#define NhlNvcMinMagnitudeF		"vcMinMagnitudeF"
#define NhlNvcMaxMagnitudeF		"vcMaxMagnitudeF"
#define NhlNvcRefMagnitudeF		"vcRefMagnitudeF"
#define NhlNvcRefLengthF		"vcRefLengthF" 
#define NhlNvcMinFracLengthF		"vcMinFracLengthF"

#define NhlNvcLevels			"vcLevels"
#define NhlNvcLevelCount		"vcLevelCount"		/* read-only */
#define NhlNvcLevelSelectionMode	"vcLevelSelectionMode"
#define NhlNvcMaxLevelCount		"vcMaxLevelCount"
#define NhlNvcLevelSpacingF		"vcLevelSpacingF"
#define NhlNvcMinLevelValF		"vcMinLevelValF"
#define NhlNvcMaxLevelValF		"vcMaxLevelValF"
#define NhlNvcLevelPalette              "vcLevelPalette"
#define NhlNvcSpanLevelPalette          "vcSpanLevelPalette"
#define NhlNvcLevelColors		"vcLevelColors"
#define NhlNvcUseScalarArray		"vcUseScalarArray"
#define NhlNvcScalarMissingValColor	"vcScalarMissingValColor"

#define NhlNvcLineArrowThicknessF	"vcLineArrowThicknessF"
#define NhlNvcMonoLineArrowColor	"vcMonoLineArrowColor"
#define NhlNvcLineArrowColor		"vcLineArrowColor"
#define NhlNvcLineArrowHeadMinSizeF	"vcLineArrowHeadMinSizeF"
#define NhlNvcLineArrowHeadMaxSizeF	"vcLineArrowHeadMaxSizeF"

#define NhlNvcFillArrowsOn		"vcFillArrowsOn"
#define NhlNvcMonoFillArrowFillColor	"vcMonoFillArrowFillColor"
#define NhlNvcFillArrowFillColor	"vcFillArrowFillColor"
#define NhlNvcFillOverEdge		"vcFillOverEdge"
#define NhlNvcMonoFillArrowEdgeColor	"vcMonoFillArrowEdgeColor"
#define NhlNvcFillArrowEdgeColor	"vcFillArrowEdgeColor"
#define NhlNvcFillArrowEdgeThicknessF	"vcFillArrowEdgeThicknessF"
#define NhlNvcFillArrowWidthF		"vcFillArrowWidthF"
#define NhlNvcFillArrowMinFracWidthF    "vcFillArrowMinFracWidthF"
#define NhlNvcFillArrowHeadXF		"vcFillArrowHeadXF"
#define NhlNvcFillArrowHeadMinFracXF 	"vcFillArrowHeadMinFracXF"
#define NhlNvcFillArrowHeadInteriorXF	"vcFillArrowHeadInteriorXF"
#define NhlNvcFillArrowHeadYF		"vcFillArrowHeadYF"
#define NhlNvcFillArrowHeadMinFracYF	"vcFillArrowHeadMinFracYF"

#define NhlNvcMonoWindBarbColor		"vcMonoWindBarbColor"
#define NhlNvcWindBarbColor		"vcWindBarbColor"
#define NhlNvcWindBarbLineThicknessF	"vcWindBarbLineThicknessF"
#define NhlNvcWindBarbTickAngleF	"vcWindBarbTickAngleF"
#define NhlNvcWindBarbTickLengthF	"vcWindBarbTickLengthF"
#define NhlNvcWindBarbTickSpacingF	"vcWindBarbTickSpacingF"
#define NhlNvcWindBarbCalmCircleSizeF   "vcWindBarbCalmCircleSizeF"
#define NhlNvcWindBarbScaleFactorF	"vcWindBarbScaleFactorF"

#define NhlNvcUseRefAnnoRes		"vcUseRefAnnoRes"
#define NhlNvcRefAnnoOn 		"vcRefAnnoOn"
#define NhlNvcRefAnnoOrientation 	"vcRefAnnoOrientation"
#define NhlNvcRefAnnoExplicitMagnitudeF "vcRefAnnoExplicitMagnitudeF"
#define NhlNvcRefAnnoArrowLineColor	"vcRefAnnoArrowLineColor"
#define NhlNvcRefAnnoArrowFillColor	"vcRefAnnoArrowFillColor"
#define NhlNvcRefAnnoArrowEdgeColor	"vcRefAnnoArrowEdgeColor"
#define NhlNvcRefAnnoArrowUseVecColor	"vcRefAnnoArrowUseVecColor"
#define NhlNvcRefAnnoArrowAngleF	"vcRefAnnoArrowAngleF"
#define NhlNvcRefAnnoArrowSpaceF	"vcRefAnnoArrowSpaceF"
#define NhlNvcRefAnnoArrowMinOffsetF	"vcRefAnnoArrowMinOffsetF"

#define NhlNvcRefAnnoString1On		"vcRefAnnoString1On"
#define NhlNvcRefAnnoString1		"vcRefAnnoString1"
#define NhlNvcRefAnnoString2On		"vcRefAnnoString2On"
#define NhlNvcRefAnnoString2		"vcRefAnnoString2"
#define NhlNvcRefAnnoFormat		"vcRefAnnoFormat"
#define NhlNvcRefAnnoFontHeightF	"vcRefAnnoFontHeightF"
#define NhlNvcRefAnnoTextDirection	"vcRefAnnoTextDirection"
#define NhlNvcRefAnnoFont		"vcRefAnnoFont"
#define NhlNvcRefAnnoFontColor		"vcRefAnnoFontColor"
#define NhlNvcRefAnnoFontAspectF	"vcRefAnnoFontAspectF"
#define NhlNvcRefAnnoFontThicknessF	"vcRefAnnoFontThicknessF"
#define NhlNvcRefAnnoFontQuality	"vcRefAnnoFontQuality"
#define NhlNvcRefAnnoConstantSpacingF	"vcRefAnnoConstantSpacingF"
#define NhlNvcRefAnnoAngleF		"vcRefAnnoAngleF"
#define NhlNvcRefAnnoFuncCode		"vcRefAnnoFuncCode"
#define NhlNvcRefAnnoBackgroundColor	"vcRefAnnoBackgroundColor"
#define NhlNvcRefAnnoPerimOn		"vcRefAnnoPerimOn"
#define NhlNvcRefAnnoPerimSpaceF	"vcRefAnnoPerimSpaceF"
#define NhlNvcRefAnnoPerimThicknessF	"vcRefAnnoPerimThicknessF"
#define NhlNvcRefAnnoPerimColor		"vcRefAnnoPerimColor"

#define NhlNvcRefAnnoZone		"vcRefAnnoZone"
#define NhlNvcRefAnnoSide		"vcRefAnnoSide"
#define NhlNvcRefAnnoJust		"vcRefAnnoJust"
#define NhlNvcRefAnnoParallelPosF	"vcRefAnnoParallelPosF"
#define NhlNvcRefAnnoOrthogonalPosF	"vcRefAnnoOrthogonalPosF"

#define NhlNvcMinAnnoOn			"vcMinAnnoOn"
#define NhlNvcMinAnnoOrientation 	"vcMinAnnoOrientation"
#define NhlNvcMinAnnoExplicitMagnitudeF "vcMinAnnoExplicitMagnitudeF"
#define NhlNvcMinAnnoArrowLineColor	"vcMinAnnoArrowLineColor"
#define NhlNvcMinAnnoArrowFillColor	"vcMinAnnoArrowFillColor"
#define NhlNvcMinAnnoArrowEdgeColor	"vcMinAnnoArrowEdgeColor"
#define NhlNvcMinAnnoArrowUseVecColor	"vcMinAnnoArrowUseVecColor"
#define NhlNvcMinAnnoArrowAngleF	"vcMinAnnoArrowAngleF"
#define NhlNvcMinAnnoArrowSpaceF	"vcMinAnnoArrowSpaceF"
#define NhlNvcMinAnnoArrowMinOffsetF	"vcMinAnnoArrowMinOffsetF"
#define NhlNvcMinAnnoString1On		"vcMinAnnoString1On"
#define NhlNvcMinAnnoString1		"vcMinAnnoString1"
#define NhlNvcMinAnnoString2On		"vcMinAnnoString2On"
#define NhlNvcMinAnnoString2		"vcMinAnnoString2"
#define NhlNvcMinAnnoFormat		"vcMinAnnoFormat"
#define NhlNvcMinAnnoFontHeightF	"vcMinAnnoFontHeightF"
#define NhlNvcMinAnnoTextDirection	"vcMinAnnoTextDirection"
#define NhlNvcMinAnnoFont		"vcMinAnnoFont"
#define NhlNvcMinAnnoFontColor		"vcMinAnnoFontColor"
#define NhlNvcMinAnnoFontAspectF	"vcMinAnnoFontAspectF"
#define NhlNvcMinAnnoFontThicknessF	"vcMinAnnoFontThicknessF"
#define NhlNvcMinAnnoFontQuality	"vcMinAnnoFontQuality"
#define NhlNvcMinAnnoConstantSpacingF	"vcMinAnnoConstantSpacingF"
#define NhlNvcMinAnnoAngleF		"vcMinAnnoAngleF"
#define NhlNvcMinAnnoFuncCode		"vcMinAnnoFuncCode"
#define NhlNvcMinAnnoBackgroundColor	"vcMinAnnoBackgroundColor"
#define NhlNvcMinAnnoPerimOn		"vcMinAnnoPerimOn"
#define NhlNvcMinAnnoPerimSpaceF	"vcMinAnnoPerimSpaceF"
#define NhlNvcMinAnnoPerimThicknessF	"vcMinAnnoPerimThicknessF"
#define NhlNvcMinAnnoPerimColor		"vcMinAnnoPerimColor"

#define NhlNvcMinAnnoZone		"vcMinAnnoZone"
#define NhlNvcMinAnnoSide		"vcMinAnnoSide"
#define NhlNvcMinAnnoJust		"vcMinAnnoJust"
#define NhlNvcMinAnnoParallelPosF	"vcMinAnnoParallelPosF"
#define NhlNvcMinAnnoOrthogonalPosF	"vcMinAnnoOrthogonalPosF"

#define NhlNvcNoDataLabelOn		"vcNoDataLabelOn"
#define NhlNvcNoDataLabelString		"vcNoDataLabelString"
#define NhlNvcZeroFLabelOn		"vcZeroFLabelOn"
#define NhlNvcZeroFLabelString		"vcZeroFLabelString"
#define NhlNvcZeroFLabelFormat		"vcZeroFLabelFormat"
#define NhlNvcZeroFLabelFontHeightF	"vcZeroFLabelFontHeightF"
#define NhlNvcZeroFLabelTextDirection	"vcZeroFLabelTextDirection"
#define NhlNvcZeroFLabelFont		"vcZeroFLabelFont"
#define NhlNvcZeroFLabelFontColor	"vcZeroFLabelFontColor"
#define NhlNvcZeroFLabelFontAspectF	"vcZeroFLabelFontAspectF"
#define NhlNvcZeroFLabelFontThicknessF	"vcZeroFLabelFontThicknessF"
#define NhlNvcZeroFLabelFontQuality	"vcZeroFLabelFontQuality"
#define NhlNvcZeroFLabelConstantSpacingF "vcZeroFLabelConstantSpacingF"
#define NhlNvcZeroFLabelAngleF		"vcZeroFLabelAngleF"
#define NhlNvcZeroFLabelFuncCode	"vcZeroFLabelFuncCode"
#define NhlNvcZeroFLabelBackgroundColor "vcZeroFLabelBackgroundColor"
#define NhlNvcZeroFLabelPerimOn		"vcZeroFLabelPerimOn"
#define NhlNvcZeroFLabelPerimSpaceF	"vcZeroFLabelPerimSpaceF"
#define NhlNvcZeroFLabelPerimThicknessF "vcZeroFLabelPerimThicknessF"
#define NhlNvcZeroFLabelPerimColor	"vcZeroFLabelPerimColor"

#define NhlNvcZeroFLabelZone		"vcZeroFLabelZone"
#define NhlNvcZeroFLabelSide		"vcZeroFLabelSide"
#define NhlNvcZeroFLabelJust		"vcZeroFLabelJust"
#define NhlNvcZeroFLabelParallelPosF	"vcZeroFLabelParallelPosF"
#define NhlNvcZeroFLabelOrthogonalPosF	"vcZeroFLabelOrthogonalPosF"

#define NhlNvcMagnitudeScalingMode	"vcMagnitudeScalingMode"
#define NhlNvcMagnitudeScaleValueF	"vcMagnitudeScaleValueF"
#define NhlNvcMagnitudeScaleFactorF	"vcMagnitudeScaleFactorF" /*ro*/
#define NhlNvcMagnitudeFormat		"vcMagnitudeFormat"

#define NhlNvcScalarValueScalingMode	"vcScalarValueScalingMode"
#define NhlNvcScalarValueScaleValueF	"vcScalarValueScaleValueF"
#define NhlNvcScalarValueScaleFactorF 	"vcScalarValueScaleFactorF" /*ro*/
#define NhlNvcScalarValueFormat		"vcScalarValueFormat"

#define NhlNvcExplicitLabelBarLabelsOn	"vcExplicitLabelBarLabelsOn"
#define NhlNvcLabelBarEndLabelsOn	"vcLabelBarEndLabelsOn"

#define NhlNvcLabelsOn			"vcLabelsOn"
#define NhlNvcLabelsUseVectorColor	"vcLabelsUseVectorColor"
#define NhlNvcLabelFontColor		"vcLabelFontColor"
#define NhlNvcLabelFontHeightF		"vcLabelFontHeightF"

/*class resources */

#define NhlCvcVectorFieldData		"VcVectorFieldData"
#define NhlCvcScalarFieldData		"VcScalarFieldData"
#define NhlCvcMapDirection		"VcMapDirection"
#define NhlCvcPositionMode		"VcPositionMode"
#define NhlCvcVectorDrawOrder		"VcVectorDrawOrder"
#define NhlCvcGlyphStyle		"VcGlyphStyle"
#define NhlCvcGlyphOpacityF     "VcGlyphOpacityF"

#define NhlCvcMinDistanceF		"VcMinDistanceF"
#define NhlCvcMinMagnitudeF		"VcMinMagnitudeF"
#define NhlCvcMaxMagnitudeF		"VcMaxMagnitudeF"
#define NhlCvcRefMagnitudeF		"VcRefMagnitudeF"
#define NhlCvcRefLengthF		"VcRefLengthF" 
#define NhlCvcMinFracLengthF		"VcMinFracLengthF"

#define NhlCvcLevelCount		"VcLevelCount"		/* read-only */
#define NhlCvcLevelPalette              "VcLevelPalette"
#define NhlCvcSpanLevelPalette          "VcSpanLevelPalette"
#define NhlCvcLevelColors		"VcLevelColors"
#define NhlCvcUseScalarArray		"VcUseScalarArray"
#define NhlCvcScalarMissingValColor	"VcScalarMissingValColor"

#define NhlCvcMonoLineArrowColor	"VcMonoLineArrowColor"
#define NhlCvcLineArrowHeadMinSizeF	"VcLineArrowHeadMinSizeF"
#define NhlCvcLineArrowHeadMaxSizeF	"VcLineArrowHeadMaxSizeF"

#define NhlCvcFillArrowsOn		"VcFillArrowsOn"
#define NhlCvcMonoFillArrowFillColor	"VcMonoFillArrowFillColor"
#define NhlCvcFillOverEdge		"VcFillOverEdge"
#define NhlCvcMonoFillArrowEdgeColor	"VcMonoFillArrowEdgeColor"
#define NhlCvcFillArrowWidthF		"VcFillArrowWidthF"
#define NhlCvcFillArrowMinFracWidthF    "VcFillArrowMinFracWidthF"
#define NhlCvcFillArrowHeadXF		"VcFillArrowHeadXF"
#define NhlCvcFillArrowHeadMinFracXF 	"VcFillArrowHeadMinFracXF"
#define NhlCvcFillArrowHeadInteriorXF	"VcFillArrowHeadInteriorXF"
#define NhlCvcFillArrowHeadYF		"VcFillArrowHeadYF"
#define NhlCvcFillArrowHeadMinFracYF	"VcFillArrowHeadMinFracYF"

#define NhlCvcMonoWindBarbColor		"VcMonoWindBarbColor"
#define NhlCvcWindBarbTickAngleF	"VcWindBarbTickAngleF"
#define NhlCvcWindBarbTickLengthF	"VcWindBarbTickLengthF"
#define NhlCvcWindBarbTickSpacingF	"VcWindBarbTickSpacingF"
#define NhlCvcWindBarbCalmCircleSizeF   "VcWindBarbCalmCircleSizeF"
#define NhlCvcWindBarbScaleFactorF	"VcWindBarbScaleFactorF"

#define NhlCvcUseRefAnnoRes		"VcUseRefAnnoRes"
#define NhlCvcRefAnnoOn 		"VcRefAnnoOn"
#define NhlCvcRefAnnoOrientation 	"VcRefAnnoOrientation"
#define NhlCvcRefAnnoExplicitMagnitudeF "VcRefAnnoExplicitMagnitudeF"
#define NhlCvcRefAnnoArrowUseVecColor	"VcRefAnnoArrowUseVecColor"
#define NhlCvcRefAnnoArrowAngleF	"VcRefAnnoArrowAngleF"
#define NhlCvcRefAnnoArrowSpaceF	"VcRefAnnoArrowSpaceF"
#define NhlCvcRefAnnoArrowMinOffsetF	"VcRefAnnoArrowMinOffsetF"

#define NhlCvcRefAnnoString1On		"VcRefAnnoString1On"
#define NhlCvcRefAnnoString1		"VcRefAnnoString1"
#define NhlCvcRefAnnoString2On		"VcRefAnnoString2On"
#define NhlCvcRefAnnoString2		"VcRefAnnoString2"

#define NhlCvcRefAnnoZone		"VcRefAnnoZone"
#define NhlCvcRefAnnoSide		"VcRefAnnoSide"
#define NhlCvcRefAnnoJust		"VcRefAnnoJust"
#define NhlCvcRefAnnoParallelPosF	"VcRefAnnoParallelPosF"
#define NhlCvcRefAnnoOrthogonalPosF	"VcRefAnnoOrthogonalPosF"

#define NhlCvcMinAnnoOn			"VcMinAnnoOn"
#define NhlCvcMinAnnoOrientation 	"VcMinAnnoOrientation"
#define NhlCvcMinAnnoExplicitMagnitudeF "VcMinAnnoExplicitMagnitudeF"
#define NhlCvcMinAnnoArrowUseVecColor	"VcMinAnnoArrowUseVecColor"
#define NhlCvcMinAnnoArrowAngleF	"VcMinAnnoArrowAngleF"
#define NhlCvcMinAnnoArrowSpaceF	"VcMinAnnoArrowSpaceF"
#define NhlCvcMinAnnoArrowMinOffsetF	"VcMinAnnoArrowMinOffsetF"
#define NhlCvcMinAnnoString1On		"VcMinAnnoString1On"
#define NhlCvcMinAnnoString1		"VcMinAnnoString1"
#define NhlCvcMinAnnoString2On		"VcMinAnnoString2On"
#define NhlCvcMinAnnoString2		"VcMinAnnoString2"

#define NhlCvcMinAnnoZone		"VcMinAnnoZone"
#define NhlCvcMinAnnoSide		"VcMinAnnoSide"
#define NhlCvcMinAnnoJust		"VcMinAnnoJust"
#define NhlCvcMinAnnoParallelPosF	"VcMinAnnoParallelPosF"
#define NhlCvcMinAnnoOrthogonalPosF	"VcMinAnnoOrthogonalPosF"

#define NhlCvcNoDataLabelOn		"VcNoDataLabelOn"
#define NhlCvcNoDataLabelString		"VcNoDataLabelString"
#define NhlCvcZeroFLabelOn		"VcZeroFLabelOn"
#define NhlCvcZeroFLabelString		"VcZeroFLabelString"

#define NhlCvcZeroFLabelZone		"VcZeroFLabelZone"
#define NhlCvcZeroFLabelSide		"VcZeroFLabelSide"
#define NhlCvcZeroFLabelJust		"VcZeroFLabelJust"
#define NhlCvcZeroFLabelParallelPosF	"VcZeroFLabelParallelPosF"
#define NhlCvcZeroFLabelOrthogonalPosF	"VcZeroFLabelOrthogonalPosF"

#define NhlCvcMagnitudeScalingMode	"VcMagnitudeScalingMode"
#define NhlCvcMagnitudeScaleValueF	"VcMagnitudeScaleValueF"
#define NhlCvcMagnitudeScaleFactorF	"VcMagnitudeScaleFactorF" /*ro*/

#define NhlCvcScalarValueScalingMode	"VcScalarValueScalingMode"
#define NhlCvcScalarValueScaleValueF	"VcScalarValueScaleValueF"
#define NhlCvcScalarValueScaleFactorF 	"VcScalarValueScaleFactorF" /*ro*/

#define NhlCvcExplicitLabelBarLabelsOn	"VcExplicitLabelBarLabelsOn"
#define NhlCvcLabelBarEndLabelsOn	"VcLabelBarEndLabelsOn"

#define NhlCvcLabelsOn			"VcLabelsOn"
#define NhlCvcLabelsUseVectorColor	"VcLabelsUseVectorColor"

/*
 * These class resources have been eliminated
 */
#if 0
#define NhlCvcLevels			"VcLevels"
#define NhlCvcLevelSelectionMode	"VcLevelSelectionMode"
#define NhlCvcMaxLevelCount		"VcMaxLevelCount"
#define NhlCvcLevelSpacingF		"VcLevelSpacingF"
#define NhlCvcMinLevelValF		"VcMinLevelValF"
#define NhlCvcMaxLevelValF		"VcMaxLevelValF"

#define NhlCvcLineArrowThicknessF	"VcLineArrowThicknessF"
#define NhlCvcLineArrowColor		"VcLineArrowColor"
#define NhlCvcFillArrowFillColor	"VcFillArrowFillColor"
#define NhlCvcFillArrowEdgeColor	"VcFillArrowEdgeColor"
#define NhlCvcFillArrowEdgeThicknessF	"VcFillArrowEdgeThicknessF"
#define NhlCvcWindBarbColor		"VcWindBarbColor"
#define NhlCvcWindBarbLineThicknessF	"VcWindBarbLineThicknessF"
#define NhlCvcRefAnnoFormat		"VcRefAnnoFormat"
#define NhlCvcRefAnnoPerimOn		"VcRefAnnoPerimOn"
#define NhlCvcRefAnnoPerimSpaceF	"VcRefAnnoPerimSpaceF"
#define NhlCvcRefAnnoArrowLineColor	"VcRefAnnoArrowLineColor"
#define NhlCvcRefAnnoArrowFillColor	"VcRefAnnoArrowFillColor"
#define NhlCvcRefAnnoArrowEdgeColor	"VcRefAnnoArrowEdgeColor"
#define NhlCvcRefAnnoFontHeightF	"VcRefAnnoFontHeightF"
#define NhlCvcRefAnnoTextDirection	"VcRefAnnoTextDirection"
#define NhlCvcRefAnnoFont		"VcRefAnnoFont"
#define NhlCvcRefAnnoFontColor		"VcRefAnnoFontColor"
#define NhlCvcRefAnnoFontAspectF	"VcRefAnnoFontAspectF"
#define NhlCvcRefAnnoFontThicknessF	"VcRefAnnoFontThicknessF"
#define NhlCvcRefAnnoFontQuality	"VcRefAnnoFontQuality"
#define NhlCvcRefAnnoConstantSpacingF	"VcRefAnnoConstantSpacingF"
#define NhlCvcRefAnnoFuncCode		"VcRefAnnoFuncCode"
#define NhlCvcRefAnnoAngleF		"VcRefAnnoAngleF"
#define NhlCvcRefAnnoBackgroundColor	"VcRefAnnoBackgroundColor"
#define NhlCvcMinAnnoPerimOn		"VcMinAnnoPerimOn"
#define NhlCvcMinAnnoPerimSpaceF	"VcMinAnnoPerimSpaceF"
#define NhlCvcRefAnnoPerimThicknessF	"VcRefAnnoPerimThicknessF"
#define NhlCvcRefAnnoPerimColor		"VcRefAnnoPerimColor"
#define NhlCvcMinAnnoFormat		"VcMinAnnoFormat"
#define NhlCvcMinAnnoArrowLineColor	"VcMinAnnoArrowLineColor"
#define NhlCvcMinAnnoArrowFillColor	"VcMinAnnoArrowFillColor"
#define NhlCvcMinAnnoArrowEdgeColor	"VcMinAnnoArrowEdgeColor"
#define NhlCvcMinAnnoFontHeightF	"VcMinAnnoFontHeightF"
#define NhlCvcMinAnnoTextDirection	"VcMinAnnoTextDirection"
#define NhlCvcMinAnnoFont		"VcMinAnnoFont"
#define NhlCvcMinAnnoFontColor		"VcMinAnnoFontColor"
#define NhlCvcMinAnnoFontAspectF	"VcMinAnnoFontAspectF"
#define NhlCvcMinAnnoFontThicknessF	"VcMinAnnoFontThicknessF"
#define NhlCvcMinAnnoFontQuality	"VcMinAnnoFontQuality"
#define NhlCvcMinAnnoConstantSpacingF	"VcMinAnnoConstantSpacingF"
#define NhlCvcMinAnnoAngleF		"VcMinAnnoAngleF"
#define NhlCvcMinAnnoFuncCode		"VcMinAnnoFuncCode"
#define NhlCvcMinAnnoBackgroundColor	"VcMinAnnoBackgroundColor"
#define NhlCvcMinAnnoPerimThicknessF	"VcMinAnnoPerimThicknessF"
#define NhlCvcMinAnnoPerimColor		"VcMinAnnoPerimColor"
#define NhlCvcZeroFLabelFormat		"VcZeroFLabelFormat"
#define NhlCvcZeroFLabelFontHeightF	"VcZeroFLabelFontHeightF"
#define NhlCvcZeroFLabelTextDirection	"VcZeroFLabelTextDirection"
#define NhlCvcZeroFLabelFont		"VcZeroFLabelFont"
#define NhlCvcZeroFLabelFontColor	"VcZeroFLabelFontColor"
#define NhlCvcZeroFLabelFontAspectF	"VcZeroFLabelFontAspectF"
#define NhlCvcZeroFLabelFontThicknessF	"VcZeroFLabelFontThicknessF"
#define NhlCvcZeroFLabelFontQuality	"VcZeroFLabelFontQuality"
#define NhlCvcZeroFLabelConstantSpacingF "VcZeroFLabelConstantSpacingF"
#define NhlCvcZeroFLabelAngleF		"VcZeroFLabelAngleF"
#define NhlCvcZeroFLabelFuncCode	"VcZeroFLabelFuncCode"
#define NhlCvcZeroFLabelBackgroundColor "VcZeroFLabelBackgroundColor"
#define NhlCvcZeroFLabelPerimOn		"VcZeroFLabelPerimOn"
#define NhlCvcZeroFLabelPerimSpaceF	"VcZeroFLabelPerimSpaceF"
#define NhlCvcZeroFLabelPerimThicknessF "VcZeroFLabelPerimThicknessF"
#define NhlCvcZeroFLabelPerimColor	"VcZeroFLabelPerimColor"
#define NhlCvcLabelFontColor		"VcLabelFontColor"
#define NhlCvcLabelFontHeightF		"VcLabelFontHeightF"
#define NhlCvcMagnitudeFormat		"VcMagnitudeFormat"
#define NhlCvcScalarValueFormat		"VcScalarValueFormat"

#endif

extern NhlClass			NhlvectorPlotClass;

#endif /*_NVECTORPLOT_h */
