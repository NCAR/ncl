/*
 *      $Id: MapPlot.h,v 1.6 1994-09-08 01:34:21 dbrown Exp $
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

#ifndef _NMapPlot_h
#define _NMapPlot_h

#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/Overlay.h>

#define NhlTMapBoundarySets	"mapboundarysets"
typedef enum _NhlMapBoundarySets {
	NhlmpNOBOUNDARIES = 0,
        NhlmpGEOPHYSICAL,
	NhlmpNATIONAL,
        NhlmpUSSTATES,
	NhlmpGEOPHYSICALANDUSSTATES,
        NhlmpALLBOUNDARIES
} NhlMapBoundarySets;

#define NhlTAreaGroupPriority	"areagrouppriority"
typedef enum _NhlAreaGroupPriority {
	NhlmpGEOPHYSICALPRIORITY = 0,
        NhlmpPOLITICALPRIORITY
} NhlAreaGroupPriority;

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

#define NhlmpUNSETCOLOR		-999
#define NhlmpUNSETFILLPATTERN	-999
#define NhlmpUNSETFILLSCALE	-1.0

#define NhlmpDEFAULTGROUPINDEX		0
#define NhlmpOCEANGROUPINDEX		1
#define NhlmpLANDGROUPINDEX		2
#define NhlmpINLANDWATERGROUPINDEX	3

/*
 * MapPlot instance resources
 */

#define NhlNmpOutlineOn			"mpOutlineOn"
#define NhlNmpOutlineBoundarySets	"mpOutlineBoundarySets"
#define NhlNmpOutlineDrawOrder		"mpOutlineDrawOrder"
#define NhlNmpFillOn			"mpFillOn"
#define NhlNmpFillBoundarySets		"mpFillBoundarySets"
#define NhlNmpFillDrawOrder		"mpFillDrawOrder"
#define NhlNmpFillGroupCount		"mpFillGroupCount"
#define NhlNmpFillAreaSpecifiers	"mpFillAreaSpecifiers"
#define NhlNmpMaskAreaSpecifiers	"mpMaskAreaSpecifiers"
#define NhlNmpOutlineSpecifiers		"mpOutlineSpecifiers"

#define NhlNmpFillPatternBackground	"mpFillPatternBackground"

#define NhlNmpAreaNames			"mpAreaNames"
#define NhlNmpAreaTypes			"mpAreaTypes"
#define NhlNmpAreaGroups		"mpAreaGroups"
#define NhlNmpFillAreaColors		"mpFillAreaColors"
#define NhlNmpDirectFillAreaColor	"mpDirectFillAreaColor"

#define NhlNmpAreaGroupPriority		"mpAreaGroupPriority"

#define NhlNmpMonoFillGroupColor	"mpMonoFillGroupColor"
#define NhlNmpMonoFillGroupPattern	"mpMonoFillGroupPattern"
#define NhlNmpMonoFillGroupScale	"mpMonoFillGroupScale"
#define NhlNmpFillGroupColors		"mpFillGroupColors"
#define NhlNmpFillGroupPatterns		"mpFillGroupPatterns"
#define NhlNmpFillGroupScales		"mpFillGroupScales"

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
#define NhlNmpLabelTextHeightF		"mpLabelTextHeightF"
#define NhlNmpLabelTextDirection	"mpLabelTextDirection"
#define NhlNmpLabelFont			"mpLabelFont"
#define NhlNmpLabelFontColor		"mpLabelFontColor"
#define NhlNmpLabelFontAspectF		"mpLabelFontAspectF"
#define NhlNmpLabelFontThicknessF	"mpLabelFontThicknessF"
#define NhlNmpLabelFontQuality		"mpLabelFontQuality"
#define NhlNmpLabelConstantSpacingF	"mpLabelConstantSpacingF"
#define NhlNmpLabelAngleF		"mpLabelAngleF"
#define NhlNmpLabelFuncCode		"mpLabelFuncCode"
#define NhlNmpLabelBackgroundColor	"mpLabelBackgroundColor"
#define NhlNmpLabelPerim		"mpLabelPerim"
#define NhlNmpLabelPerimSpaceF		"mpLabelPerimSpaceF"
#define NhlNmpLabelPerimThicknessF	"mpLabelPerimThicknessF"
#define NhlNmpLabelPerimColor		"mpLabelPerimColor"

/*
 * MapPlot class resources
 */

#define NhlCmpOutlineOn			"MpOutlineOn"
#define NhlCmpOutlineBoundarySets	"MpOutlineBoundarySets"
#define NhlCmpOutlineDrawOrder		"MpOutlineDrawOrder"
#define NhlCmpFillOn			"MpFillOn"
#define NhlCmpFillBoundarySets		"MpFillBoundarySets"
#define NhlCmpFillDrawOrder		"MpFillDrawOrder"
#define NhlCmpFillGroupCount		"MpFillGroupCount"
#define NhlCmpInverseFill		"MpInverseFill"
#define NhlCmpInverseFillDrawOrder	"MpInverseFillDrawOrder"
#define NhlCmpFillAreaSpecifiers	"MpFillAreaSpecifiers"
#define NhlCmpMaskAreaSpecifiers	"MpMaskAreaSpecifiers"
#define NhlCmpOutlineSpecifiers		"MpOutlineSpecifiers"

#define NhlCmpFillPatternBackground	"MpFillPatternBackground"

#define NhlCmpAreaNames			"MpAreaNames"
#define NhlCmpAreaTypes			"MpAreaTypes"
#define NhlCmpAreaGroups		"MpAreaGroups"
#define NhlCmpFillAreaColors		"MpFillAreaColors"
#define NhlCmpDirectFillAreaColor	"MpDirectFillAreaColor"

#define NhlCmpAreaGroupPriority		"MpAreaGroupPriority"

#define NhlCmpMonoFillGroupColor	"MpMonoFillGroupColor"
#define NhlCmpMonoFillGroupPattern	"MpMonoFillGroupPattern"
#define NhlCmpMonoFillGroupScale	"MpMonoFillGroupScale"
#define NhlCmpFillGroupColors		"MpFillGroupColors"
#define NhlCmpFillGroupPatterns		"MpFillGroupPatterns"
#define NhlCmpFillGroupScales		"MpFillGroupScales"

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
#define NhlCmpLabelTextHeightF		"MpLabelTextHeightF"
#define NhlCmpLabelTextDirection	"MpLabelTextDirection"
#define NhlCmpLabelFont			"MpLabelFont"
#define NhlCmpLabelFontColor		"MpLabelFontColor"
#define NhlCmpLabelFontAspectF		"MpLabelFontAspectF"
#define NhlCmpLabelFontThicknessF	"MpLabelFontThicknessF"
#define NhlCmpLabelFontQuality		"MpLabelFontQuality"
#define NhlCmpLabelConstantSpacingF	"MpLabelConstantSpacingF"
#define NhlCmpLabelAngleF		"MpLabelAngleF"
#define NhlCmpLabelFuncCode		"MpLabelFuncCode"
#define NhlCmpLabelBackgroundColor	"MpLabelBackgroundColor"
#define NhlCmpLabelPerim		"MpLabelPerim"
#define NhlCmpLabelPerimSpaceF		"MpLabelPerimSpaceF"
#define NhlCmpLabelPerimThicknessF	"MpLabelPerimThicknessF"
#define NhlCmpLabelPerimColor		"MpLabelPerimColor"

extern NhlLayerClass NhlmapPlotLayerClass;

#endif /*_NMapPlot_h */
