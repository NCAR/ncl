/*
 *      $Id: VectorPlot.h,v 1.1 1995-11-21 20:19:08 dbrown Exp $
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
#define NhlTVectorPositionMode	"vectorpositionmode"

/*
 * VectorPlot instance resources
 */

#define NhlNvcVectorFieldData		"vcVectorFieldData"
#define NhlNvcScalarFieldData		"vcScalarFieldData"
#define NhlNvcMapVectorDirection	"vcMapVectorDirection"
#define NhlNvcPositionMode		"vcPositionMode"

#define NhlNvcMinFractionalLenF		"vcMinFractionalLenF"
#define NhlNvcMinMagnitudeF		"vcMinMagnitudeF"
#define NhlNvcMaxMagnitudeF		"vcMaxMagnitudeF"
#define NhlNvcRefMagnitudeF		"vcRefMagnitudeF"
#define NhlNvcRefLengthF		"vcRefLengthF" 

#define NhlNvcLevels			"vcLevels"
#define NhlNvcLevelCount		"vcLevelCount"		/* read-only */
#define NhlNvcLevelSelectionMode	"vcLevelSelectionMode"
#define NhlNvcMaxLevelCount		"vcMaxLevelCount"
#define NhlNvcLevelSpacingF		"vcLevelSpacingF"
#define NhlNvcMinLevelValF		"vcMinLevelValF"
#define NhlNvcMaxLevelValF		"vcMaxLevelValF"

#define NhlNvcArrowHeadMinSizeF		"vcArrowHeadMinSizeF"
#define NhlNvcArrowHeadMaxSizeF		"vcArrowHeadMaxSizeF"
#define NhlNvcVectorLineThicknessF	"vcVectorLineThicknessF"
#define NhlNvcUseScalarArray		"vcUseScalarArray"
#define NhlNvcMonoVectorColor		"vcMonoVectorColor"
#define NhlNvcVectorColor		"vcVectorColor"
#define NhlNvcVectorColors		"vcVectorColors"
#define NhlNvcScalarMissingValColor	"vcScalarMissingValColor"
#define NhlNvcVectorDrawOrder		"vcVectorDrawOrder"

#define NhlNvcUseRefAnnoRes		"vcUseRefAnnoRes"
#define NhlNvcRefAnnoOn 		"vcRefAnnoOn"
#define NhlNvcRefAnnoOrientation 	"vcRefAnnoOrientation"
#define NhlNvcRefAnnoExplicitMagnitudeF "vcRefAnnoExplicitMagnitudeF"
#define NhlNvcRefAnnoArrowColor		"vcRefAnnoArrowColor"
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
#define NhlNvcMinAnnoArrowColor		"vcMinAnnoArrowColor"
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
#define NhlCvcMapVectorDirection	"VcMapVectorDirection"
#define NhlCvcPositionMode		"VcPositionMode"

#define NhlCvcMinFractionalLenF		"VcMinFractionalLenF"
#define NhlCvcMinMagnitudeF		"VcMinMagnitudeF"
#define NhlCvcMaxMagnitudeF		"VcMaxMagnitudeF"
#define NhlCvcRefMagnitudeF		"VcRefMagnitudeF"
#define NhlCvcRefLengthF		"VcRefLengthF" 

#define NhlCvcLevels			"VcLevels"
#define NhlCvcLevelCount		"VcLevelCount"		/* read-only */
#define NhlCvcLevelSelectionMode	"VcLevelSelectionMode"
#define NhlCvcMaxLevelCount		"VcMaxLevelCount"
#define NhlCvcLevelSpacingF		"VcLevelSpacingF"
#define NhlCvcMinLevelValF		"VcMinLevelValF"
#define NhlCvcMaxLevelValF		"VcMaxLevelValF"

#define NhlCvcArrowHeadMinSizeF		"VcArrowHeadMinSizeF"
#define NhlCvcArrowHeadMaxSizeF		"VcArrowHeadMaxSizeF"
#define NhlCvcVectorMinSizeF		"VcMinVectorSizeF"
#define NhlCvcVectorMaxSizeF		"VcMaxVectorSizeF"
#define NhlCvcVectorLineThicknessF	"VcVectorLineThicknessF"
#define NhlCvcUseScalarArray		"VcUseScalarArray"
#define NhlCvcMonoVectorColor		"VcMonoVectorColor"
#define NhlCvcVectorColor		"VcVectorColor"
#define NhlCvcVectorColors		"VcVectorColors"
#define NhlCvcScalarMissingValColor	"VcScalarMissingValColor"
#define NhlCvcVectorDrawOrder		"VcVectorDrawOrder"

#define NhlCvcUseRefAnnoRes		"VcUseRefAnnoRes"
#define NhlCvcRefAnnoOn			"VcRefAnnoOn"
#define NhlCvcRefAnnoOrientation 	"VcRefAnnoOrientation"
#define NhlCvcRefAnnoExplicitMagnitudeF "VcRefAnnoExplicitMagnitudeF"
#define NhlCvcRefAnnoArrowColor		"VcRefAnnoArrowColor"
#define NhlCvcRefAnnoArrowUseVecColor	"VcRefAnnoArrowUseVecColor"
#define NhlCvcRefAnnoArrowAngleF	"VcRefAnnoArrowAngleF"
#define NhlCvcRefAnnoArrowSpaceF	"VcRefAnnoArrowSpaceF"
#define NhlCvcRefAnnoArrowMinOffsetF	"VcRefAnnoArrowMinOffsetF"
#define NhlCvcRefAnnoString1On		"VcRefAnnoString1On"
#define NhlCvcRefAnnoString1		"VcRefAnnoString1"
#define NhlCvcRefAnnoString2On		"VcRefAnnoString2On"
#define NhlCvcRefAnnoString2		"VcRefAnnoString2"
#define NhlCvcRefAnnoFormat		"VcRefAnnoFormat"
#define NhlCvcRefAnnoFontHeightF	"VcRefAnnoFontHeightF"
#define NhlCvcRefAnnoTextDirection	"VcRefAnnoTextDirection"
#define NhlCvcRefAnnoFont		"VcRefAnnoFont"
#define NhlCvcRefAnnoFontColor		"VcRefAnnoFontColor"
#define NhlCvcRefAnnoFontAspectF	"VcRefAnnoFontAspectF"
#define NhlCvcRefAnnoFontThicknessF	"VcRefAnnoFontThicknessF"
#define NhlCvcRefAnnoFontQuality	"VcRefAnnoFontQuality"
#define NhlCvcRefAnnoConstantSpacingF	"VcRefAnnoConstantSpacingF"
#define NhlCvcRefAnnoAngleF		"VcRefAnnoAngleF"
#define NhlCvcRefAnnoFuncCode		"VcRefAnnoFuncCode"
#define NhlCvcRefAnnoBackgroundColor	"VcRefAnnoBackgroundColor"
#define NhlCvcRefAnnoPerimOn		"VcRefAnnoPerimOn"
#define NhlCvcRefAnnoPerimSpaceF	"VcRefAnnoPerimSpaceF"
#define NhlCvcRefAnnoPerimThicknessF	"VcRefAnnoPerimThicknessF"
#define NhlCvcRefAnnoPerimColor		"VcRefAnnoPerimColor"

#define NhlCvcRefAnnoZone		"VcRefAnnoZone"
#define NhlCvcRefAnnoSide		"VcRefAnnoSide"
#define NhlCvcRefAnnoJust		"VcRefAnnoJust"
#define NhlCvcRefAnnoParallelPosF	"VcRefAnnoParallelPosF"
#define NhlCvcRefAnnoOrthogonalPosF	"VcRefAnnoOrthogonalPosF"

#define NhlCvcMinAnnoOn			"VcMinAnnoOn"
#define NhlCvcMinAnnoOrientation 	"VcMinAnnoOrientation"
#define NhlCvcMinAnnoExplicitMagnitudeF "VcMinAnnoExplicitMagnitudeF"
#define NhlCvcMinAnnoArrowColor		"VcMinAnnoArrowColor"
#define NhlCvcMinAnnoArrowUseVecColor	"VcMinAnnoArrowUseVecColor"
#define NhlCvcMinAnnoArrowAngleF	"VcMinAnnoArrowAngleF"
#define NhlCvcMinAnnoArrowSpaceF	"VcMinAnnoArrowSpaceF"
#define NhlCvcMinAnnoArrowMinOffsetF	"VcMinAnnoArrowMinOffsetF"
#define NhlCvcMinAnnoString1On		"VcMinAnnoString1On"
#define NhlCvcMinAnnoString1		"VcMinAnnoString1"
#define NhlCvcMinAnnoString2On		"VcMinAnnoString2On"
#define NhlCvcMinAnnoString2		"VcMinAnnoString2"
#define NhlCvcMinAnnoFormat		"VcMinAnnoFormat"
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
#define NhlCvcMinAnnoPerimOn		"VcMinAnnoPerimOn"
#define NhlCvcMinAnnoPerimSpaceF	"VcMinAnnoPerimSpaceF"
#define NhlCvcMinAnnoPerimThicknessF	"VcMinAnnoPerimThicknessF"
#define NhlCvcMinAnnoPerimColor		"VcMinAnnoPerimColor"

#define NhlCvcMinAnnoZone		"VcMinAnnoZone"
#define NhlCvcMinAnnoSide		"VcMinAnnoSide"
#define NhlCvcMinAnnoJust		"VcMinAnnoJust"
#define NhlCvcMinAnnoParallelPosF	"VcMinAnnoParallelPosF"
#define NhlCvcMinAnnoOrthogonalPosF	"VcMinAnnoOrthogonalPosF"

#define NhlCvcNoDataLabelOn		"VcNoDataLabelOn"
#define NhlCvcNoDataLabelString		"VcNoDataLabelString"
#define NhlCvcZeroFLabelOn		"VcZeroFLabelOn"
#define NhlCvcZeroFLabelString		"VcZeroFLabelString"
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

#define NhlCvcZeroFLabelZone		"VcZeroFLabelZone"
#define NhlCvcZeroFLabelSide		"VcZeroFLabelSide"
#define NhlCvcZeroFLabelJust		"VcZeroFLabelJust"
#define NhlCvcZeroFLabelParallelPosF	"VcZeroFLabelParallelPosF"
#define NhlCvcZeroFLabelOrthogonalPosF	"VcZeroFLabelOrthogonalPosF"

#define NhlCvcMagnitudeScalingMode	"VcMagnitudeScalingMode"
#define NhlCvcMagnitudeScaleValueF	"VcMagnitudeScaleValueF"
#define NhlCvcMagnitudeScaleFactorF	"VcMagnitudeScaleFactorF" /*ro*/
#define NhlCvcMagnitudeFormat		"VcMagnitudeFormat"

#define NhlCvcScalarValueScalingMode	"VcScalarValueScalingMode"
#define NhlCvcScalarValueScaleValueF	"VcScalarValueScaleValueF"
#define NhlCvcScalarValueScaleFactorF 	"VcScalarValueScaleFactorF" /*ro*/
#define NhlCvcScalarValueFormat		"VcScalarValueFormat"

#define NhlCvcExplicitLabelBarLabelsOn	"VcExplicitLabelBarLabelsOn"
#define NhlCvcLabelBarEndLabelsOn	"VcLabelBarEndLabelsOn"

#define NhlCvcLabelsOn			"VcLabelsOn"
#define NhlCvcLabelsUseVectorColor	"VcLabelsUseVectorColor"
#define NhlCvcLabelFontColor		"VcLabelFontColor"
#define NhlCvcLabelFontHeightF		"VcLabelFontHeightF"

extern NhlClass			NhlvectorPlotClass;

#endif /*_NVECTORPLOT_h */
