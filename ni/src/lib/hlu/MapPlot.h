/*
 *      $Id: MapPlot.h,v 1.5 1994-06-24 00:39:37 dbrown Exp $
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

#include <ncarg/hlu/Overlay.h>

typedef enum _NhlMapOutlineType {
	NhlNOOUTLINES = 0, NhlCONTINENTS, 
	NhlUSSTATES, NhlALLOUTLINES,
	NhlCONTINENTSANDCOUNTRIES, 
	NhlSIMPLIFIEDCONTINENTS } NhlMapOutlineType;

#define NhlTMapOutlineType	"mapoutlinetype"

/*
 * MapPlot instance resources
 */

#define NhlNmpMonoFillColor		"mpMonoFillColor"
#define NhlNmpMonoFillPattern		"mpMonoFillPattern"
#define NhlNmpMonoFillScale		"mpMonoFillScale"
#define NhlNmpFillColors		"mpFillColors"
#define NhlNmpFillPatterns		"mpFillPatterns"
#define NhlNmpFillScales		"mpFillScales"

#define NhlNmpOutlineType		"mpOutlineType"
#define NhlNmpDelayOutline		"mpDelayOutline"
#define NhlNmpContinentLineColor	"mpContinentLineColor"
#define NhlNmpContinentLineDashPattern	"mpContinentLineDashPattern"
#define NhlNmpContinentLineDashSegLenF	"mpContinentLineDashSegLenF"
#define NhlNmpContinentLineThicknessF	"mpContinentLineThicknessF"
#define NhlNmpUSStateLineColor		"mpUSStateLineColor"
#define NhlNmpUSStateLineDashPattern	"mpUSStateLineDashPattern"
#define NhlNmpUSStateLineDashSegLenF	"mpUSStateLineDashSegLenF"
#define NhlNmpUSStateLineThicknessF	"mpUSStateLineThicknessF"
#define NhlNmpCountryLineColor		"mpCountryLineColor"
#define NhlNmpCountryLineDashPattern	"mpCountryLineDashPattern"
#define NhlNmpCountryLineDashSegLenF	"mpCountryLineDashSegLenF"
#define NhlNmpCountryLineThicknessF	"mpCountryLineThicknessF"

#define NhlNmpGridOn			"mpGridOn"
#define NhlNmpDelayGrid			"mpDelayGrid"
#define NhlNmpGridSpacingF		"mpGridSpacingF"
#define NhlNmpGridLineColor		"mpGridLineColor"
#define NhlNmpGridLineDashPattern	"mpGridLineDashPattern"
#define NhlNmpGridLineDashSegLenF	"mpGridLineDashSegLenF"
#define NhlNmpGridLineThicknessF	"mpGridLineThicknessF"

#define NhlNmpLimbOn			"mpLimbOn"
#define NhlNmpDelayLimb			"mpDelayLimb"
#define NhlNmpLimbLineColor		"mpLimbLineColor"
#define NhlNmpLimbLineDashPattern	"mpLimbLineDashPattern"
#define NhlNmpLimbLineDashSegLenF	"mpLimbLineDashSegLenF"
#define NhlNmpLimbLineThicknessF	"mpLimbLineThicknessF"

#define NhlNmpPerimOn			"mpPerimOn"
#define NhlNmpDelayPerim		"mpDelayPerim"
#define NhlNmpPerimLineColor		"mpPerimLineColor"
#define NhlNmpPerimLineDashPattern	"mpPerimLineDashPattern"
#define NhlNmpPerimLineDashSegLenF	"mpPerimLineDashSegLenF"
#define NhlNmpPerimLineThicknessF	"mpPerimLineThicknessF"

#define NhlNmpLabelsOn			"mpLabelsOn"
#define NhlNmpDelayLabels		"mpDelayLabels"
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


#define NhlCmpMonoFillColor		"MpMonoFillColor"
#define NhlCmpMonoFillPattern		"MpMonoFillPattern"
#define NhlCmpMonoFillScale		"MpMonoFillScale"
#define NhlCmpFillColors		"MpFillColors"
#define NhlCmpFillPatterns		"MpFillPatterns"
#define NhlCmpFillScales		"MpFillScales"

#define NhlCmpOutlineType		"MpOutlineType"
#define NhlCmpDelayOutline		"MpDelayOutline"
#define NhlCmpContinentLineColor	"MpContinentLineColor"
#define NhlCmpContinentLineDashPattern	"MpContinentLineDashPattern"
#define NhlCmpContinentLineDashSegLenF	"MpContinentLineDashSegLenF"
#define NhlCmpContinentLineThicknessF	"MpContinentLineThicknessF"
#define NhlCmpUSStateLineColor		"MpUSStateLineColor"
#define NhlCmpUSStateLineDashPattern	"MpUSStateLineDashPattern"
#define NhlCmpUSStateLineDashSegLenF	"MpUSStateLineDashSegLenF"
#define NhlCmpUSStateLineThicknessF	"MpUSStateLineThicknessF"
#define NhlCmpCountryLineColor		"MpCountryLineColor"
#define NhlCmpCountryLineDashPattern	"MpCountryLineDashPattern"
#define NhlCmpCountryLineDashSegLenF	"MpCountryLineDashSegLenF"
#define NhlCmpCountryLineThicknessF	"MpCountryLineThicknessF"

#define NhlCmpGridOn			"MpGridOn"
#define NhlCmpDelayGrid			"MpDelayGrid"
#define NhlCmpGridSpacingF		"MpGridSpacingF"
#define NhlCmpGridLineColor		"MpGridLineColor"
#define NhlCmpGridLineDashPattern	"MpGridLineDashPattern"
#define NhlCmpGridLineDashSegLenF	"MpGridLineDashSegLenF"
#define NhlCmpGridLineThicknessF	"MpGridLineThicknessF"

#define NhlCmpLimbOn			"MpLimbOn"
#define NhlCmpDelayLimb			"MpDelayLimb"
#define NhlCmpLimbLineColor		"MpLimbLineColor"
#define NhlCmpLimbLineDashPattern	"MpLimbLineDashPattern"
#define NhlCmpLimbLineDashSegLenF	"MpLimbLineDashSegLenF"
#define NhlCmpLimbLineThicknessF	"MpLimbLineThicknessF"

#define NhlCmpPerimOn			"MpPerimOn"
#define NhlCmpDelayPerim		"MpDelayPerim"
#define NhlCmpPerimLineColor		"MpPerimLineColor"
#define NhlCmpPerimLineDashPattern	"MpPerimLineDashPattern"
#define NhlCmpPerimLineDashSegLenF	"MpPerimLineDashSegLenF"
#define NhlCmpPerimLineThicknessF	"MpPerimLineThicknessF"

#define NhlCmpLabelsOn			"MpLabelsOn"
#define NhlCmpDelayLabels		"MpDelayLabels"
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
