/*
 *      $Id: MapPlot.h,v 1.10 1995-04-01 00:04:08 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapPlot.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for MapPlot class.
 */

#ifndef _NMAPPLOT_h
#define _NMAPPLOT_h

#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/PlotManager.h>

#define NhlTMapBoundarySets	"mapboundarysets"
typedef enum _NhlMapBoundarySets {
	NhlmpNOBOUNDARIES = 0,
        NhlmpGEOPHYSICAL,
	NhlmpNATIONAL,
        NhlmpUSSTATES,
	NhlmpGEOPHYSICALANDUSSTATES,
        NhlmpALLBOUNDARIES
} NhlMapBoundarySets;

#define NhlTSpecifiedFillPriority	"specifiedfillpriority"
typedef enum _NhlSpecifiedFillPriority {
	NhlmpGEOPHYSICALPRIORITY = 0,
        NhlmpPOLITICALPRIORITY
} NhlSpecifiedFillPriority;

#define NhlTMapGridMaskMode	"mapgridmaskmode"
typedef enum _NhlMapGridMaskMode {
	NhlmpMASKNONE = 0,
        NhlmpMASKOCEAN,
	NhlmpMASKNOTOCEAN,
	NhlmpMASKLAND,
	NhlmpMASKNOTLAND,
	NhlmpMASKFILLAREA,
	NhlmpMASKMASKAREA
} NhlMapGridMaskMode;

#define NhlTMapShapeMode	"mapshapemode"
typedef enum _NhlMapShapeMode {
	NhlmpFREEASPECT = 0,
	NhlmpFIXEDASPECTFITBB,
	NhlmpFIXEDASPECTNOFITBB
} NhlMapShapeMode;

#define NhlmpNULLAREA		"nullarea"
#define NhlmpALLNATIONAL	"allnational"
#define NhlmpALLGEOPHYSICAL	"allgeophysical"
#define NhlmpLAND		"land"
#define NhlmpWATER		"water"
#define NhlmpINLANDWATER	"inlandwater"
#define NhlmpOCEANS		"oceans"
#define NhlmpCONTINENTS		"continents"
#define NhlmpISLANDS		"islands"
#define NhlmpLARGEISLANDS	"largeislands"
#define NhlmpSMALLISLANDS	"smallislands"
#define NhlmpALLUSSTATES	"allusstates"
#define NhlmpUSSTATESLAND	"usstatesland"
#define NhlmpUSSTATESWATER	"usstateswater"

#define NhlmpUNSETCOLOR		NhlNULLCOLOR
#define NhlmpUNSETFILLPATTERN	NhlNULLFILL
#define NhlmpUNSETFILLSCALE	0.0

#define NhlmpDEFAULTGROUPINDEX		0
#define NhlmpOCEANGROUPINDEX		1
#define NhlmpLANDGROUPINDEX		2
#define NhlmpINLANDWATERGROUPINDEX	3

/*
 * MapPlot instance resources
 */

#define NhlNmpShapeMode			"mpShapeMode"
#define NhlNmpAreaNames			"mpAreaNames"
#define NhlNmpAreaTypes			"mpAreaTypes"
#define NhlNmpFixedAreaGroups		"mpFixedAreaGroups"
#define NhlNmpDynamicAreaGroups		"mpDynamicAreaGroups"
#define NhlNmpDataBaseVersion		"mpDataBaseVersion"

#define NhlNmpOutlineOn			"mpOutlineOn"
#define NhlNmpOutlineDrawOrder		"mpOutlineDrawOrder"
#define NhlNmpOutlineBoundarySets	"mpOutlineBoundarySets"
#define NhlNmpOutlineSpecifiers		"mpOutlineSpecifiers"
#define NhlNmpGeophysicalLineColor	"mpGeophysicalLineColor"
#define NhlNmpGeophysicalLineDashPattern	"mpGeophysicalLineDashPattern"
#define NhlNmpGeophysicalLineDashSegLenF	"mpGeophysicalLineDashSegLenF"
#define NhlNmpGeophysicalLineThicknessF	"mpGeophysicalLineThicknessF"
#define NhlNmpUSStateLineColor		"mpUSStateLineColor"
#define NhlNmpUSStateLineDashPattern	"mpUSStateLineDashPattern"
#define NhlNmpUSStateLineDashSegLenF	"mpUSStateLineDashSegLenF"
#define NhlNmpUSStateLineThicknessF	"mpUSStateLineThicknessF"
#define NhlNmpNationalLineColor		"mpNationalLineColor"
#define NhlNmpNationalLineDashPattern	"mpNationalLineDashPattern"
#define NhlNmpNationalLineDashSegLenF	"mpNationalLineDashSegLenF"
#define NhlNmpNationalLineThicknessF	"mpNationalLineThicknessF"

#define NhlNmpAreaMaskingOn		"mpAreaMaskingOn"
#define NhlNmpMaskAreaSpecifiers	"mpMaskAreaSpecifiers"

#define NhlNmpFillOn			"mpFillOn"
#define NhlNmpFillDrawOrder		"mpFillDrawOrder"
#define NhlNmpFillPatternBackground	"mpFillPatternBackground"
#define NhlNmpFillBoundarySets		"mpFillBoundarySets"

#define NhlNmpFillAreaSpecifiers	"mpFillAreaSpecifiers"
#define NhlNmpSpecifiedFillPriority	"mpSpecifiedFillPriority"
#define NhlNmpSpecifiedFillDirectIndexing "mpSpecifiedFillDirectIndexing"
#define NhlNmpSpecifiedFillColors	"mpSpecifiedFillColors"
#define NhlNmpSpecifiedFillPatterns	"mpSpecifiedFillPatterns"
#define NhlNmpSpecifiedFillScales	"mpSpecifiedFillScales"

#define NhlNmpAreaGroupCount		"mpAreaGroupCount"
#define NhlNmpMonoFillColor		"mpMonoFillColor"
#define NhlNmpFillColor			"mpFillColor"
#define NhlNmpFillColors		"mpFillColors"
#define NhlNmpMonoFillPattern		"mpMonoFillPattern"
#define NhlNmpFillPattern		"mpFillPattern"
#define NhlNmpFillPatterns		"mpFillPatterns"
#define NhlNmpMonoFillScale		"mpMonoFillScale"
#define NhlNmpFillScale			"mpFillScale"
#define NhlNmpFillScales		"mpFillScales"

#define NhlNmpDefaultFillColor		"mpDefaultFillColor"
#define NhlNmpDefaultFillPattern	"mpDefaultFillPattern"
#define NhlNmpDefaultFillScaleF		"mpDefaultFillScaleF"
#define NhlNmpOceanFillColor		"mpOceanFillColor"
#define NhlNmpOceanFillPattern		"mpOceanFillPattern"
#define NhlNmpOceanFillScaleF		"mpOceanFillScaleF"
#define NhlNmpLandFillColor		"mpLandFillColor"
#define NhlNmpLandFillPattern		"mpLandFillPattern"
#define NhlNmpLandFillScaleF		"mpLandFillScaleF"
#define NhlNmpInlandWaterFillColor	"mpInlandWaterFillColor"
#define NhlNmpInlandWaterFillPattern	"mpInlandWaterFillPattern"
#define NhlNmpInlandWaterFillScaleF	"mpInlandWaterFillScaleF"

#define NhlNmpRelativeGridSpacing	"mpRelativeGridSpacing"
#define NhlNmpGridSpacingF		"mpGridSpacingF"
#define NhlNmpGridMaskMode		"mpGridMaskMode"
#define NhlNmpGridAndLimbOn		"mpGridAndLimbOn"
#define NhlNmpGridAndLimbDrawOrder	"mpGridAndLimbDrawOrder"
#define NhlNmpGridLineColor		"mpGridLineColor"
#define NhlNmpGridLineDashPattern	"mpGridLineDashPattern"
#define NhlNmpGridLineDashSegLenF	"mpGridLineDashSegLenF"
#define NhlNmpGridLineThicknessF	"mpGridLineThicknessF"

#define NhlNmpLimbLineColor		"mpLimbLineColor"
#define NhlNmpLimbLineDashPattern	"mpLimbLineDashPattern"
#define NhlNmpLimbLineDashSegLenF	"mpLimbLineDashSegLenF"
#define NhlNmpLimbLineThicknessF	"mpLimbLineThicknessF"

#define NhlNmpPerimOn			"mpPerimOn"
#define NhlNmpPerimDrawOrder		"mpPerimDrawOrder"
#define NhlNmpPerimLineColor		"mpPerimLineColor"
#define NhlNmpPerimLineDashPattern	"mpPerimLineDashPattern"
#define NhlNmpPerimLineDashSegLenF	"mpPerimLineDashSegLenF"
#define NhlNmpPerimLineThicknessF	"mpPerimLineThicknessF"

#define NhlNmpLabelsOn			"mpLabelsOn"
#define NhlNmpLabelDrawOrder		"mpLabelDrawOrder"
#define NhlNmpLabelFontHeightF		"mpLabelFontHeightF"
#define NhlNmpLabelFontColor		"mpLabelFontColor"

/**************** NOT-IMPLEMENTED**********/

#define NhlNmpLabelTextDirection	"mpLabelTextDirection"
#define NhlNmpLabelFont			"mpLabelFont"
#define NhlNmpLabelFontAspectF		"mpLabelFontAspectF"
#define NhlNmpLabelFontThicknessF	"mpLabelFontThicknessF"
#define NhlNmpLabelFontQuality		"mpLabelFontQuality"
#define NhlNmpLabelConstantSpacingF	"mpLabelConstantSpacingF"
#define NhlNmpLabelAngleF		"mpLabelAngleF"
#define NhlNmpLabelFuncCode		"mpLabelFuncCode"
#define NhlNmpLabelBackgroundColor	"mpLabelBackgroundColor"

#define NhlNmpLabelPerimOn		"mpLabelPerimOn"
#define NhlNmpLabelPerimSpaceF		"mpLabelPerimSpaceF"
#define NhlNmpLabelPerimThicknessF	"mpLabelPerimThicknessF"
#define NhlNmpLabelPerimColor		"mpLabelPerimColor"

/*
 * MapPlot class resources
 */


#define NhlCmpShapeMode			"MpShapeMode"
#define NhlCmpAreaNames			"MpAreaNames"
#define NhlCmpAreaTypes			"MpAreaTypes"
#define NhlCmpFixedAreaGroups		"MpFixedAreaGroups"
#define NhlCmpDynamicAreaGroups		"MpDynamicAreaGroups"
#define NhlCmpDataBaseVersion		"MpDataBaseVersion"

#define NhlCmpOutlineOn			"MpOutlineOn"
#define NhlCmpOutlineDrawOrder		"MpOutlineDrawOrder"
#define NhlCmpOutlineBoundarySets	"MpOutlineBoundarySets"
#define NhlCmpOutlineSpecifiers		"MpOutlineSpecifiers"
#define NhlCmpGeophysicalLineColor	"MpGeophysicalLineColor"
#define NhlCmpGeophysicalLineDashPattern	"MpGeophysicalLineDashPattern"
#define NhlCmpGeophysicalLineDashSegLenF	"MpGeophysicalLineDashSegLenF"
#define NhlCmpGeophysicalLineThicknessF	"MpGeophysicalLineThicknessF"
#define NhlCmpUSStateLineColor		"MpUSStateLineColor"
#define NhlCmpUSStateLineDashPattern	"MpUSStateLineDashPattern"
#define NhlCmpUSStateLineDashSegLenF	"MpUSStateLineDashSegLenF"
#define NhlCmpUSStateLineThicknessF	"MpUSStateLineThicknessF"
#define NhlCmpNationalLineColor		"MpNationalLineColor"
#define NhlCmpNationalLineDashPattern	"MpNationalLineDashPattern"
#define NhlCmpNationalLineDashSegLenF	"MpNationalLineDashSegLenF"
#define NhlCmpNationalLineThicknessF	"MpNationalLineThicknessF"

#define NhlCmpAreaMaskingOn		"MpAreaMaskingOn"
#define NhlCmpMaskAreaSpecifiers	"MpMaskAreaSpecifiers"

#define NhlCmpFillOn			"MpFillOn"
#define NhlCmpFillDrawOrder		"MpFillDrawOrder"
#define NhlCmpFillPatternBackground	"MpFillPatternBackground"
#define NhlCmpSpecifiedFillPriority	"MpSpecifiedFillPriority"
#define NhlCmpFillBoundarySets		"MpFillBoundarySets"

#define NhlCmpFillAreaSpecifiers	"MpFillAreaSpecifiers"
#define NhlCmpSpecifiedFillDirectIndexing "MpSpecifiedFillDirectIndexing"
#define NhlCmpSpecifiedFillColors	"MpSpecifiedFillColors"
#define NhlCmpSpecifiedFillPatterns	"MpSpecifiedFillPatterns"
#define NhlCmpSpecifiedFillScales	"MpSpecifiedFillScales"

#define NhlCmpAreaGroupCount		"MpAreaGroupCount"
#define NhlCmpMonoFillColor		"MpMonoFillColor"
#define NhlCmpFillColor			"MpFillColor"
#define NhlCmpFillColors		"MpFillColors"
#define NhlCmpMonoFillPattern		"MpMonoFillPattern"
#define NhlCmpFillPattern		"MpFillPattern"
#define NhlCmpFillPatterns		"MpFillPatterns"
#define NhlCmpMonoFillScale		"MpMonoFillScale"
#define NhlCmpFillScale			"MpFillScale"
#define NhlCmpFillScales		"MpFillScales"

#define NhlCmpDefaultFillColor		"MpDefaultFillColor"
#define NhlCmpDefaultFillPattern	"MpDefaultFillPattern"
#define NhlCmpDefaultFillScaleF		"MpDefaultFillScaleF"
#define NhlCmpOceanFillColor		"MpOceanFillColor"
#define NhlCmpOceanFillPattern		"MpOceanFillPattern"
#define NhlCmpOceanFillScaleF		"MpOceanFillScaleF"
#define NhlCmpLandFillColor		"MpLandFillColor"
#define NhlCmpLandFillPattern		"MpLandFillPattern"
#define NhlCmpLandFillScaleF		"MpLandFillScaleF"
#define NhlCmpInlandWaterFillColor	"MpInlandWaterFillColor"
#define NhlCmpInlandWaterFillPattern	"MpInlandWaterFillPattern"
#define NhlCmpInlandWaterFillScaleF	"MpInlandWaterFillScaleF"

#define NhlCmpRelativeGridSpacing	"MpRelativeGridSpacing"
#define NhlCmpGridSpacingF		"MpGridSpacingF"
#define NhlCmpGridMaskMode		"MpGridMaskMode"
#define NhlCmpGridAndLimbOn		"MpGridAndLimbOn"
#define NhlCmpGridAndLimbDrawOrder	"MpGridAndLimbDrawOrder"
#define NhlCmpGridLineColor		"MpGridLineColor"
#define NhlCmpGridLineDashPattern	"MpGridLineDashPattern"
#define NhlCmpGridLineDashSegLenF	"MpGridLineDashSegLenF"
#define NhlCmpGridLineThicknessF	"MpGridLineThicknessF"

#define NhlCmpLimbLineColor		"MpLimbLineColor"
#define NhlCmpLimbLineDashPattern	"MpLimbLineDashPattern"
#define NhlCmpLimbLineDashSegLenF	"MpLimbLineDashSegLenF"
#define NhlCmpLimbLineThicknessF	"MpLimbLineThicknessF"

#define NhlCmpPerimOn			"MpPerimOn"
#define NhlCmpPerimDrawOrder		"MpPerimDrawOrder"
#define NhlCmpPerimLineColor		"MpPerimLineColor"
#define NhlCmpPerimLineDashPattern	"MpPerimLineDashPattern"
#define NhlCmpPerimLineDashSegLenF	"MpPerimLineDashSegLenF"
#define NhlCmpPerimLineThicknessF	"MpPerimLineThicknessF"

#define NhlCmpLabelsOn			"MpLabelsOn"
#define NhlCmpLabelDrawOrder		"MpLabelDrawOrder"
#define NhlCmpLabelFontHeightF		"MpLabelFontHeightF"
#define NhlCmpLabelFontColor		"MpLabelFontColor"

/**************** NOT-IMPLEMENTED**********/

#define NhlCmpLabelTextDirection	"MpLabelTextDirection"
#define NhlCmpLabelFont			"MpLabelFont"
#define NhlCmpLabelFontAspectF		"MpLabelFontAspectF"
#define NhlCmpLabelFontThicknessF	"MpLabelFontThicknessF"
#define NhlCmpLabelFontQuality		"MpLabelFontQuality"
#define NhlCmpLabelConstantSpacingF	"MpLabelConstantSpacingF"
#define NhlCmpLabelAngleF		"MpLabelAngleF"
#define NhlCmpLabelFuncCode		"MpLabelFuncCode"
#define NhlCmpLabelBackgroundColor	"MpLabelBackgroundColor"

#define NhlCmpLabelPerimOn		"MpLabelPerimOn"
#define NhlCmpLabelPerimSpaceF		"MpLabelPerimSpaceF"
#define NhlCmpLabelPerimThicknessF	"MpLabelPerimThicknessF"
#define NhlCmpLabelPerimColor		"MpLabelPerimColor"

extern NhlLayerClass NhlmapPlotLayerClass;

#endif /*_NMAPPLOT_h */
