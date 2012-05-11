/*
 *      $Id: StreamlinePlot.h,v 1.7.12.1 2010-03-17 20:47:07 brownrig Exp $
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
#define NhlNstStreamlineDrawOrder	"stStreamlineDrawOrder"

#define NhlNstLineThicknessF		"stLineThicknessF"	/* LWD */
#define NhlNstMonoLineColor		"stMonoLineColor"
#define NhlNstLineColor			"stLineColor"
#define NhlNstLineOpacityF      "stLineOpacityF"
#define NhlNstArrowLengthF		"stArrowLengthF"	/* ARL */
#define NhlNstStepSizeF			"stStepSizeF"		/* DFM */
#define NhlNstMinStepFactorF		"stMinStepFactorF"    	/* CDS */
#define NhlNstLengthCheckCount		"stLengthCheckCount" 	/* CKP */
#define NhlNstCrossoverCheckCount	"stCrossoverCheckCount" /* CKX */

#define NhlNstMinArrowSpacingF		"stMinArrowSpacingF"	/* AMD */
#define NhlNstMinLineSpacingF		"stMinLineSpacingF"	/* SSP */
#define NhlNstLineStartStride		"stLineStartStride"     /* SGD */
#define NhlNstArrowStride		"stArrowStride"    	/* AGD */

#define NhlNstCurlyVectorMode           "stCurlyVectorMode"     /* STM */
#define NhlNstRefMagnitudeF		"stRefMagnitudeF"       /* VRM */
#define NhlNstRefLengthF		"stRefLengthF"          /* VRL */
#define NhlNstMinFracLengthF		"stMinFracLengthF"      /* VRF */
#define NhlNstPositionMode		"stPositionMode"        /* VPO */
#define NhlNstArrowFracLengthF          "stArrowFracLengthF"    /* AFR */
#define NhlNstMinDistanceF		"stMinDistanceF"        /* SMD */
#define NhlNstMinMagnitudeF		"stMinMagnitudeF"       

#define NhlNstLevels			"stLevels"
#define NhlNstLevelCount		"stLevelCount"		/* read-only */
#define NhlNstLevelSelectionMode	"stLevelSelectionMode"
#define NhlNstMaxLevelCount		"stMaxLevelCount"
#define NhlNstLevelSpacingF		"stLevelSpacingF"
#define NhlNstMinLevelValF		"stMinLevelValF"
#define NhlNstMaxLevelValF		"stMaxLevelValF"
#define NhlNstLevelPalette              "stLevelPalette"
#define NhlNstSpanLevelPalette          "stSpanLevelPalette"
#define NhlNstLevelColors		"stLevelColors"
#define NhlNstUseScalarArray		"stUseScalarArray"
#define NhlNstScalarMissingValColor	"stScalarMissingValColor"

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

#define NhlNstExplicitLabelBarLabelsOn	"stExplicitLabelBarLabelsOn"
#define NhlNstLabelBarEndLabelsOn	"stLabelBarEndLabelsOn"
#define NhlNstLabelFormat		"stLabelFormat"

/*class resources */

#define NhlCstVectorFieldData		"StVectorFieldData"
#define NhlCstScalarFieldData		"StScalarFieldData"
#define NhlCstMapDirection		"StMapDirection"
#define NhlCstStreamlineDrawOrder		"StStreamlineDrawOrder"

#define NhlCstArrowLengthF		"StArrowLengthF"	/* ARL */
#define NhlCstStepSizeF			"StStepSizeF"		/* DFM */
#define NhlCstMinStepFactorF		"StMinStepFactorF"    	/* CDS */
#define NhlCstLengthCheckCount		"StLengthCheckCount" 	/* CKP */
#define NhlCstCrossoverCheckCount	"StCrossoverCheckCount" /* CKX */

#define NhlCstMonoLineColor		"StMonoLineColor"
#define NhlCstMinArrowSpacingF		"StMinArrowSpacingF"	/* AMD */
#define NhlCstMinLineSpacingF		"StMinLineSpacingF"	/* SSP */
#define NhlCstLineStartStride		"StLineStartStride"     /* SGD */
#define NhlCstArrowStride		"StArrowStride"         /* AGD */

#define NhlCstCurlyVectorMode           "StCurlyVectorMode"     /* STM */
#define NhlCstRefMagnitudeF		"StRefMagnitudeF"       /* VRM */
#define NhlCstRefLengthF		"StRefLengthF"          /* VRL */
#define NhlCstMinFracLengthF		"StMinFracLengthF"      /* VRF */
#define NhlCstPositionMode		"StPositionMode"        /* VPO */
#define NhlCstArrowFracLengthF          "StArrowFracLengthF"    /* AFR */
#define NhlCstMinDistanceF		"StMinDistanceF"        /* SMD */
#define NhlCstMinMagnitudeF		"StMinMagnitudeF"       

#define NhlCstLevelCount		"StLevelCount"	/* read-only */
#define NhlCstLevelPalette              "StLevelPalette"
#define NhlCstSpanLevelPalette          "StSpanLevelPalette"
#define NhlCstLevelColors		"StLevelColors"
#define NhlCstUseScalarArray		"StUseScalarArray"
#define NhlCstScalarMissingValColor	"StScalarMissingValColor"

#define NhlCstNoDataLabelString		"StNoDataLabelString"
#define NhlCstZeroFLabelString		"StZeroFLabelString"

#define NhlCstZeroFLabelZone		"StZeroFLabelZone"
#define NhlCstZeroFLabelSide		"StZeroFLabelSide"
#define NhlCstZeroFLabelJust		"StZeroFLabelJust"
#define NhlCstZeroFLabelParallelPosF	"StZeroFLabelParallelPosF"
#define NhlCstZeroFLabelOrthogonalPosF	"StZeroFLabelOrthogonalPosF"

#define NhlCstExplicitLabelBarLabelsOn	"StExplicitLabelBarLabelsOn"
#define NhlCstLabelBarEndLabelsOn	"StLabelBarEndLabelsOn"
#define NhlCstLabelFormat		"StLabelFormat"

/*
 * These class resources have been eliminated
 */
#if 0
#define NhlCstLineThicknessF		"StLineThicknessF"	/* LWD */
#define NhlCstLineColor			"StLineColor"
#define NhlCstNoDataLabelOn		"StNoDataLabelOn"
#define NhlCstZeroFLabelOn		"StZeroFLabelOn"
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
#define NhlCstZeroFLabelPerimSpaceF	"StZeroFLabelPerimSpaceF"
#define NhlCstZeroFLabelPerimOn		"StZeroFLabelPerimOn"
#define NhlCstZeroFLabelBackgroundColor "StZeroFLabelBackgroundColor"
#define NhlCstZeroFLabelPerimThicknessF "StZeroFLabelPerimThicknessF"
#define NhlCstZeroFLabelPerimColor	"StZeroFLabelPerimColor"

#endif

/*
 * Not yet implemented
 */
#if 0

#define NhlNstScalarFieldData		"stScalarFieldData"
#define NhlNstUseScalarArray		"stUseScalarArray"
#define NhlNstMonoStreamlineLineColor	"stMonoStreamlineLineColor"
#define NhlNstStreamlineLineColor	"stStreamlineLineColor"
#define NhlNstMonoStreamlineFillColor	"stMonoStreamlineFillColor"
#define NhlNstStreamlineFillColor		"stStreamlineFillColor"
#define NhlNstStreamlineColors		"stStreamlineColors"
#define NhlNstScalarMissingValColor	"stScalarMissingValColor"

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

#define NhlCstScalarFieldData		"StScalarFieldData"
#define NhlCstUseScalarArray		"StUseScalarArray"
#define NhlCstStreamlineColors		"StStreamlineColors"
#define NhlCstScalarMissingValColor	"StScalarMissingValColor"

#define NhlCstMagnitudeScalingMode	"StMagnitudeScalingMode"
#define NhlCstMagnitudeScaleValueF	"StMagnitudeScaleValueF"
#define NhlCstMagnitudeScaleFactorF	"StMagnitudeScaleFactorF" /*ro*/

#define NhlCstScalarValueScalingMode	"StScalarValueScalingMode"
#define NhlCstScalarValueScaleValueF	"StScalarValueScaleValueF"
#define NhlCstScalarValueScaleFactorF 	"StScalarValueScaleFactorF" /*ro*/

#define NhlCstExplicitLabelBarLabelsOn	"StExplicitLabelBarLabelsOn"
#define NhlCstLabelBarEndLabelsOn	"StLabelBarEndLabelsOn"

#define NhlCstLabelsOn			"StLabelsOn"
#define NhlCstLabelsUseStreamlineColor	"StLabelsUseStreamlineColor"
#define NhlCstLabelFontColor		"StLabelFontColor"
#define NhlCstLabelFontHeightF		"StLabelFontHeightF"

#define NhlCstStreamlineLineThicknessF	"StStreamlineLineThicknessF"
#define NhlCstMonoStreamlineLineColor	"StMonoStreamlineLineColor"
#define NhlCstStreamlineLineColor		"StStreamlineLineColor"
#define NhlCstMonoStreamlineFillColor	"StMonoStreamlineFillColor"
#define NhlCstStreamlineFillColor		"StStreamlineFillColor"
#endif

extern NhlClass			NhlstreamlinePlotClass;

#endif /*_NSTREAMLINEPLOT_h */
