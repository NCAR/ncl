/*
 *      $Id: MapPlot.h,v 1.23 2006-06-15 16:45:56 dbrown Exp $
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

#define NhlTMapBoundarySets	"MapBoundarySets"
typedef enum _NhlMapBoundarySets {
	NhlNOBOUNDARIES = 0,
        NhlGEOPHYSICAL,
	NhlNATIONAL,
        NhlUSSTATES,
	NhlGEOPHYSICALANDUSSTATES,
        NhlALLBOUNDARIES
} NhlMapBoundarySets;

#define NhlTSpecifiedFillPriority	"SpecifiedFillPriority"
typedef enum _NhlSpecifiedFillPriority {
	NhlGEOPHYSICALPRIORITY = 0,
        NhlPOLITICALPRIORITY
} NhlSpecifiedFillPriority;

#define NhlTMapGridMaskMode	"MapGridMaskMode"
typedef enum _NhlMapGridMaskMode {
	NhlMASKNONE = 0,
        NhlMASKOCEAN,
	NhlMASKNOTOCEAN,
	NhlMASKLAND,
	NhlMASKNOTLAND,
	NhlMASKFILLAREA,
	NhlMASKMASKAREA
} NhlMapGridMaskMode;

#define NhlTMapShapeMode	"MapShapeMode"
typedef enum _NhlMapShapeMode {
	NhlFREEASPECT = 0,
	NhlFIXEDASPECTFITBB,
	NhlFIXEDASPECTNOFITBB
} NhlMapShapeMode;

#define NhlTMapDataBaseVersion	"MapDataBaseVersion"
typedef enum _NhlMapDataBaseVersion {
	NhlNCARG4_0 = 0,
	NhlNCARG4_1,
	NhlRANGS_GSHHS,
        NhlDYNAMIC_MAPS
} NhlMapDataBaseVersion;

#define NhlTMapDataResolution "MapDataResolution"
typedef enum _NhlMapDataResolution {
	NhlUNSPECIFIEDRESOLUTION = -1,
	NhlFINESTRESOLUTION,
	NhlFINERESOLUTION,
	NhlMEDIUMRESOLUTION,
	NhlCOARSERESOLUTION,
	NhlCOARSESTRESOLUTION
} NhlMapDataResolution;

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
#define NhlNmpDataSetName		"mpDataSetName"
#define NhlNmpDataResolution		"mpDataResolution"

#define NhlNmpOutlineMaskingOn		"mpOutlineMaskingOn"
#define NhlNmpMaskOutlineSpecifiers	"mpMaskOutlineSpecifiers"

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
/* "Provincial" resources are intended to be aliases for USState resources */
#define NhlNmpProvincialLineColor	"mpProvincialLineColor"
#define NhlNmpProvincialLineDashPattern	"mpProvincialLineDashPattern"
#define NhlNmpProvincialLineDashSegLenF	"mpProvincialLineDashSegLenF"
#define NhlNmpProvincialLineThicknessF	"mpProvincialLineThicknessF"
#define NhlNmpNationalLineColor		"mpNationalLineColor"
#define NhlNmpNationalLineDashPattern	"mpNationalLineDashPattern"
#define NhlNmpNationalLineDashSegLenF	"mpNationalLineDashSegLenF"
#define NhlNmpNationalLineThicknessF	"mpNationalLineThicknessF"
#define NhlNmpCountyLineColor		"mpCountyLineColor"
#define NhlNmpCountyLineDashPattern	"mpCountyLineDashPattern"
#define NhlNmpCountyLineDashSegLenF	"mpCountyLineDashSegLenF"
#define NhlNmpCountyLineThicknessF	"mpCountyLineThicknessF"

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
#define NhlNmpFillScaleF		"mpFillScaleF"
#define NhlNmpFillScales		"mpFillScales"
#define NhlNmpFillDotSizeF		"mpFillDotSizeF"

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

#define NhlNmpGridSpacingF		"mpGridSpacingF"
#define NhlNmpGridLatSpacingF           "mpGridLatSpacingF"
#define NhlNmpGridLonSpacingF           "mpGridLonSpacingF"
#define NhlNmpGridMaxLatF      		"mpGridMaxLatF"
#define NhlNmpGridPolarLonSpacingF	"mpGridPolarLonSpacingF"
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

/*
 * MapPlot class resources
 */


#define NhlCmpShapeMode			"MpShapeMode"
#define NhlCmpAreaNames			"MpAreaNames"
#define NhlCmpAreaTypes			"MpAreaTypes"
#define NhlCmpFixedAreaGroups		"MpFixedAreaGroups"
#define NhlCmpDynamicAreaGroups		"MpDynamicAreaGroups"
#define NhlCmpDataBaseVersion		"MpDataBaseVersion"
#define NhlCmpDataSetName		"MpDataSetName"
#define NhlCmpDataResolution		"MpDataResolution"

#define NhlCmpOutlineMaskingOn		"MpOutlineMaskingOn"
#define NhlCmpMaskOutlineSpecifiers	"MpMaskOutlineSpecifiers"

#define NhlCmpOutlineOn			"MpOutlineOn"
#define NhlCmpOutlineDrawOrder		"MpOutlineDrawOrder"
#define NhlCmpOutlineBoundarySets	"MpOutlineBoundarySets"
#define NhlCmpOutlineSpecifiers		"MpOutlineSpecifiers"

#define NhlCmpAreaMaskingOn		"MpAreaMaskingOn"
#define NhlCmpMaskAreaSpecifiers	"MpMaskAreaSpecifiers"

#define NhlCmpFillOn			"MpFillOn"
#define NhlCmpFillDrawOrder		"MpFillDrawOrder"
#define NhlCmpSpecifiedFillPriority	"MpSpecifiedFillPriority"
#define NhlCmpFillBoundarySets		"MpFillBoundarySets"

#define NhlCmpFillAreaSpecifiers	"MpFillAreaSpecifiers"
#define NhlCmpSpecifiedFillDirectIndexing "MpSpecifiedFillDirectIndexing"
#define NhlCmpSpecifiedFillColors	"MpSpecifiedFillColors"
#define NhlCmpSpecifiedFillPatterns	"MpSpecifiedFillPatterns"
#define NhlCmpSpecifiedFillScales	"MpSpecifiedFillScales"

#define NhlCmpAreaGroupCount		"MpAreaGroupCount"
#define NhlCmpMonoFillColor		"MpMonoFillColor"
#define NhlCmpFillColors		"MpFillColors"
#define NhlCmpMonoFillPattern		"MpMonoFillPattern"
#define NhlCmpFillPatterns		"MpFillPatterns"
#define NhlCmpMonoFillScale		"MpMonoFillScale"
#define NhlCmpFillScales		"MpFillScales"


#define NhlCmpGridSpacingF		"MpGridSpacingF"
#define NhlCmpGridLatSpacingF           "MpGridLatSpacingF"
#define NhlCmpGridLonSpacingF           "MpGridLonSpacingF"
#define NhlCmpGridMaxLatF      		"MpGridMaxLatF"
#define NhlCmpGridPolarLonSpacingF	"MpGridPolarLonSpacingF"
#define NhlCmpGridMaskMode		"MpGridMaskMode"
#define NhlCmpGridAndLimbOn		"MpGridAndLimbOn"
#define NhlCmpGridAndLimbDrawOrder	"MpGridAndLimbDrawOrder"
#define NhlCmpGridLineColor		"MpGridLineColor"

#define NhlCmpPerimDrawOrder		"MpPerimDrawOrder"

#define NhlCmpLabelDrawOrder		"MpLabelDrawOrder"

/*
 * These class resources have been eliminated
 */
#if 0
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
#define NhlCmpFillPatternBackground	"MpFillPatternBackground"
#define NhlCmpFillColor			"MpFillColor"
#define NhlCmpFillPattern		"MpFillPattern"
#define NhlCmpFillScaleF		"MpFillScaleF"
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
#define NhlCmpGridLineDashPattern	"MpGridLineDashPattern"
#define NhlCmpGridLineDashSegLenF	"MpGridLineDashSegLenF"
#define NhlCmpGridLineThicknessF	"MpGridLineThicknessF"
#define NhlCmpLimbLineColor		"MpLimbLineColor"
#define NhlCmpLimbLineDashPattern	"MpLimbLineDashPattern"
#define NhlCmpLimbLineDashSegLenF	"MpLimbLineDashSegLenF"
#define NhlCmpLimbLineThicknessF	"MpLimbLineThicknessF"
#define NhlCmpPerimOn			"MpPerimOn"
#define NhlCmpPerimLineColor		"MpPerimLineColor"
#define NhlCmpPerimLineDashPattern	"MpPerimLineDashPattern"
#define NhlCmpPerimLineDashSegLenF	"MpPerimLineDashSegLenF"
#define NhlCmpPerimLineThicknessF	"MpPerimLineThicknessF"
#define NhlCmpLabelsOn			"MpLabelsOn"
#define NhlCmpLabelFontHeightF		"MpLabelFontHeightF"
#define NhlCmpLabelFontColor		"MpLabelFontColor"

#endif

extern NhlClass NhlmapPlotClass;

#endif /*_NMAPPLOT_h */
