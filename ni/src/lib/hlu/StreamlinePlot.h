/*
 *      $Id: StreamlinePlot.h,v 1.2 1996-02-06 19:59:19 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		StreamlinePlot.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 28 11:44:30 MDT 1995
 *
 *	Description:	Public header for StreamlinePlot class.
 */

#ifndef _NSTREAMLINEPLOT_h
#define _NSTREAMLINEPLOT_h

#include <ncarg/hlu/Workspace.h>
#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/VectorField.h>
#include <ncarg/hlu/ScalarField.h>

/*
 * StreamlinePlot instance resources
 */

#define NhlNstVectorFieldData		"stVectorFieldData"
#define NhlNstScalarFieldData		"stScalarFieldData"
#define NhlNstMapDirection		"stMapDirection"	/* TRT */

#define NhlNstLineThicknessF		"stLineThicknessF"	/* LWD */
#define NhlNstLineColor			"stLineColor"
#define NhlNstArrowLengthF		"stArrowLengthF"	/* ARL */
#define NhlNstStepSizeF			"stStepSizeF"		/* DFM */
#define NhlNstMinStepFactorF		"stMinStepFactorF"    	/* CDS */
#define NhlNstLengthCheckCount		"stLengthCheckCount" 	/* CKP */
#define NhlNstCrossoverCheckCount	"stCrossoverCheckCount" /* CKX */

#define NhlNstMinLineLengthF		"stMinLineLengthF"	
#define NhlNstMinLineSpacingF		"stMinLineSpacingF"	/* SSP */
#define NhlNstLineStartStride		"stLineStartStride"     /* SGD */
#define NhlNstArrowStartStride		"stArrowStartStride"    /* AGD */


#define NhlNstLevels			"stLevels"
#define NhlNstLevelCount		"stLevelCount"		/* read-only */
#define NhlNstLevelSelectionMode	"stLevelSelectionMode"
#define NhlNstMaxLevelCount		"stMaxLevelCount"
#define NhlNstLevelSpacingF		"stLevelSpacingF"
#define NhlNstMinLevelValF		"stMinLevelValF"
#define NhlNstMaxLevelValF		"stMaxLevelValF"

#define NhlNstUseScalarArray		"stUseScalarArray"
#define NhlNstMonoStreamlineLineColor	"stMonoStreamlineLineColor"
#define NhlNstStreamlineLineColor	"stStreamlineLineColor"
#define NhlNstMonoStreamlineFillColor	"stMonoStreamlineFillColor"
#define NhlNstStreamlineFillColor		"stStreamlineFillColor"
#define NhlNstStreamlineColors		"stStreamlineColors"
#define NhlNstScalarMissingValColor	"stScalarMissingValColor"
#define NhlNstStreamlineDrawOrder		"stStreamlineDrawOrder"



#define NhlNstNoDataLabelOn		"stNoDataLabelOn"
#define NhlNstNoDataLabelString		"stNoDataLabelString"
#define NhlNstZeroFLabelOn		"stZeroFLabelOn"
#define NhlNstZeroFLabelString		"stZeroFLabelString"
#define NhlNstZeroFLabelFormat		"stZeroFLabelFormat"
#define NhlNstZeroFLabelFontHeightF	"stZeroFLabelFontHeightF"
#define NhlNstZeroFLabelTextDirection	"stZeroFLabelTextDirection"
#define NhlNstZeroFLabelFont		"stZeroFLabelFont"
#define NhlNstZeroFLabelFontColor	"stZeroFLabelFontColor"
#define NhlNstZeroFLabelFontAspectF	"stZeroFLabelFontAspectF"
#define NhlNstZeroFLabelFontThicknessF	"stZeroFLabelFontThicknessF"
#define NhlNstZeroFLabelFontQuality	"stZeroFLabelFontQuality"
#define NhlNstZeroFLabelConstantSpacingF "stZeroFLabelConstantSpacingF"
#define NhlNstZeroFLabelAngleF		"stZeroFLabelAngleF"
#define NhlNstZeroFLabelFuncCode	"stZeroFLabelFuncCode"
#define NhlNstZeroFLabelBackgroundColor "stZeroFLabelBackgroundColor"
#define NhlNstZeroFLabelPerimOn		"stZeroFLabelPerimOn"
#define NhlNstZeroFLabelPerimSpaceF	"stZeroFLabelPerimSpaceF"
#define NhlNstZeroFLabelPerimThicknessF "stZeroFLabelPerimThicknessF"
#define NhlNstZeroFLabelPerimColor	"stZeroFLabelPerimColor"

#define NhlNstZeroFLabelZone		"stZeroFLabelZone"
#define NhlNstZeroFLabelSide		"stZeroFLabelSide"
#define NhlNstZeroFLabelJust		"stZeroFLabelJust"
#define NhlNstZeroFLabelParallelPosF	"stZeroFLabelParallelPosF"
#define NhlNstZeroFLabelOrthogonalPosF	"stZeroFLabelOrthogonalPosF"

#define NhlNstMagnitudeScalingMode	"stMagnitudeScalingMode"
#define NhlNstMagnitudeScaleValueF	"stMagnitudeScaleValueF"
#define NhlNstMagnitudeScaleFactorF	"stMagnitudeScaleFactorF" /*ro*/
#define NhlNstMagnitudeFormat		"stMagnitudeFormat"

#define NhlNstScalarValueScalingMode	"stScalarValueScalingMode"
#define NhlNstScalarValueScaleValueF	"stScalarValueScaleValueF"
#define NhlNstScalarValueScaleFactorF 	"stScalarValueScaleFactorF" /*ro*/
#define NhlNstScalarValueFormat		"stScalarValueFormat"

#define NhlNstExplicitLabelBarLabelsOn	"stExplicitLabelBarLabelsOn"
#define NhlNstLabelBarEndLabelsOn	"stLabelBarEndLabelsOn"

#define NhlNstLabelsOn			"stLabelsOn"
#define NhlNstLabelsUseStreamlineColor	"stLabelsUseStreamlineColor"
#define NhlNstLabelFontColor		"stLabelFontColor"
#define NhlNstLabelFontHeightF		"stLabelFontHeightF"

/*class resources */

#define NhlCstVectorFieldData		"StVectorFieldData"
#define NhlCstScalarFieldData		"StScalarFieldData"
#define NhlCstMapDirection		"StMapDirection"

#define NhlCstLineThicknessF		"StLineThicknessF"	/* LWD */
#define NhlCstLineColor			"StLineColor"
#define NhlCstArrowLengthF		"StArrowLengthF"	/* ARL */
#define NhlCstStepSizeF			"StStepSizeF"		/* DFM */
#define NhlCstMinStepFactorF		"StMinStepFactorF"    	/* CDS */
#define NhlCstLengthCheckCount		"StLengthCheckCount" 	/* CKP */
#define NhlCstCrossoverCheckCount	"StCrossoverCheckCount" /* CKX */

#define NhlCstMinLineLengthF		"StMinLineLengthF"	
#define NhlCstMinLineSpacingF		"StMinLineSpacingF"	/* SSP */
#define NhlCstLineStartStride		"StLineStartStride"     /* SGD */
#define NhlCstArrowStartStride		"StArrowStartStride"    /* AGD */

#define NhlCstLevels			"StLevels"
#define NhlCstLevelCount		"StLevelCount"		/* read-only */
#define NhlCstLevelSelectionMode	"StLevelSelectionMode"
#define NhlCstMaxLevelCount		"StMaxLevelCount"
#define NhlCstLevelSpacingF		"StLevelSpacingF"
#define NhlCstMinLevelValF		"StMinLevelValF"
#define NhlCstMaxLevelValF		"StMaxLevelValF"

#define NhlCstStreamlineLineThicknessF	"StStreamlineLineThicknessF"
#define NhlCstUseScalarArray		"StUseScalarArray"
#define NhlCstMonoStreamlineLineColor	"StMonoStreamlineLineColor"
#define NhlCstStreamlineLineColor		"StStreamlineLineColor"
#define NhlCstMonoStreamlineFillColor	"StMonoStreamlineFillColor"
#define NhlCstStreamlineFillColor		"StStreamlineFillColor"
#define NhlCstStreamlineColors		"StStreamlineColors"
#define NhlCstScalarMissingValColor	"StScalarMissingValColor"
#define NhlCstStreamlineDrawOrder		"StStreamlineDrawOrder"

#define NhlCstNoDataLabelOn		"StNoDataLabelOn"
#define NhlCstNoDataLabelString		"StNoDataLabelString"
#define NhlCstZeroFLabelOn		"StZeroFLabelOn"
#define NhlCstZeroFLabelString		"StZeroFLabelString"
#define NhlCstZeroFLabelFormat		"StZeroFLabelFormat"
#define NhlCstZeroFLabelFontHeightF	"StZeroFLabelFontHeightF"
#define NhlCstZeroFLabelTextDirection	"StZeroFLabelTextDirection"
#define NhlCstZeroFLabelFont		"StZeroFLabelFont"
#define NhlCstZeroFLabelFontColor	"StZeroFLabelFontColor"
#define NhlCstZeroFLabelFontAspectF	"StZeroFLabelFontAspectF"
#define NhlCstZeroFLabelFontThicknessF	"StZeroFLabelFontThicknessF"
#define NhlCstZeroFLabelFontQuality	"StZeroFLabelFontQuality"
#define NhlCstZeroFLabelConstantSpacingF "StZeroFLabelConstantSpacingF"
#define NhlCstZeroFLabelAngleF		"StZeroFLabelAngleF"
#define NhlCstZeroFLabelFuncCode	"StZeroFLabelFuncCode"
#define NhlCstZeroFLabelBackgroundColor "StZeroFLabelBackgroundColor"
#define NhlCstZeroFLabelPerimOn		"StZeroFLabelPerimOn"
#define NhlCstZeroFLabelPerimSpaceF	"StZeroFLabelPerimSpaceF"
#define NhlCstZeroFLabelPerimThicknessF "StZeroFLabelPerimThicknessF"
#define NhlCstZeroFLabelPerimColor	"StZeroFLabelPerimColor"

#define NhlCstZeroFLabelZone		"StZeroFLabelZone"
#define NhlCstZeroFLabelSide		"StZeroFLabelSide"
#define NhlCstZeroFLabelJust		"StZeroFLabelJust"
#define NhlCstZeroFLabelParallelPosF	"StZeroFLabelParallelPosF"
#define NhlCstZeroFLabelOrthogonalPosF	"StZeroFLabelOrthogonalPosF"

#define NhlCstMagnitudeScalingMode	"StMagnitudeScalingMode"
#define NhlCstMagnitudeScaleValueF	"StMagnitudeScaleValueF"
#define NhlCstMagnitudeScaleFactorF	"StMagnitudeScaleFactorF" /*ro*/
#define NhlCstMagnitudeFormat		"StMagnitudeFormat"

#define NhlCstScalarValueScalingMode	"StScalarValueScalingMode"
#define NhlCstScalarValueScaleValueF	"StScalarValueScaleValueF"
#define NhlCstScalarValueScaleFactorF 	"StScalarValueScaleFactorF" /*ro*/
#define NhlCstScalarValueFormat		"StScalarValueFormat"

#define NhlCstExplicitLabelBarLabelsOn	"StExplicitLabelBarLabelsOn"
#define NhlCstLabelBarEndLabelsOn	"StLabelBarEndLabelsOn"

#define NhlCstLabelsOn			"StLabelsOn"
#define NhlCstLabelsUseStreamlineColor	"StLabelsUseStreamlineColor"
#define NhlCstLabelFontColor		"StLabelFontColor"
#define NhlCstLabelFontHeightF		"StLabelFontHeightF"

extern NhlClass			NhlstreamlinePlotClass;

#endif /*_NSTREAMLINEPLOT_h */
