/*
 *      $Id: XyPlot.h,v 1.7 1995-01-11 00:46:58 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		XyPlot.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Dec 30 13:07:27 MST 1992
 *
 *	Description:	public header for xyplotter
 */
#ifndef _NXyPlot_h
#define _NXyPlot_h

#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>

/*
 * Resource names
 */

/*
 * DataDep objects resources
 */
#define	NhlNxyColor	"xyColor"
#define	NhlCxyColor	"XyColor"
#define NhlNxyColors	"xyColors"
#define NhlCxyColors	"XyColors"
#define	NhlNxyMonoColor	"xyMonoColor"
#define	NhlCxyMonoColor	"XyMonoColor"

#define	NhlNxyDashPattern	"xyDashPattern"
#define	NhlCxyDashPattern	"XyDashPattern"
#define NhlNxyDashPatterns	"xyDashPatterns"
#define NhlCxyDashPatterns	"XyDashPatterns"
#define	NhlNxyMonoDashPattern	"xyMonoDashPattern"
#define	NhlCxyMonoDashPattern	"XyMonoDashPattern"


#define NhlNxyMarkerMode	"xyMarkerMode"
#define NhlCxyMarkerMode	"XyMarkerMode"
#define NhlNxyMarkerModes	"xyMarkerModes"
#define NhlCxyMarkerModes	"XyMarkerModes"
#define NhlNxyMonoMarkerMode	"xyMonoMarkerMode"
#define NhlCxyMonoMarkerMode	"XyMonoMarkerMode"

#define NhlNxyMarker		"xyMarker"
#define NhlCxyMarker		"XyMarker"
#define NhlNxyMarkers		"xyMarkers"
#define NhlCxyMarkers		"XyMarkers"
#define NhlNxyMonoMarker	"xyMonoMarker"
#define NhlCxyMonoMarker	"XyMonoMarker"

#define NhlNxyMarkerColor	"xyMarkerColor"
#define NhlCxyMarkerColor	"XyMarkerColor"
#define NhlNxyMarkerColors	"xyMarkerColors"
#define NhlCxyMarkerColors	"XyMarkerColors"
#define NhlNxyMonoMarkerColor	"xyMonoMarkerColor"
#define NhlCxyMonoMarkerColor	"XyMonoMarkerColor"

#define NhlNxyMarkerSizeF	"xyMarkerSizeF"
#define NhlCxyMarkerSizeF	"XyMarkerSizeF"
#define NhlNxyMarkerSizes	"xyMarkerSizes"
#define NhlCxyMarkerSizes	"XyMarkerSizes"
#define NhlNxyMonoMarkerSize	"xyMonoMarkerSize"
#define NhlCxyMonoMarkerSize	"XyMonoMarkerSize"

#define NhlNxyLabelMode		"xyLabelMode"
#define NhlCxyLabelMode		"XyLabelMode"

#define NhlNxyExplicitLabels	"xyExplicitLabels"
#define NhlCxyExplicitLabels	"XyExplicitLabels"

/*
 * XyPlot's resource names
 */

#define NhlNxyCurveData		"xyCurveData"
#define NhlCxyCurveData		"XyCurveData"

#define NhlNxyCurveThicknessF	"xyCurveThicknessF"
#define NhlCxyCurveThicknessF	"XyCurveThicknessF"

#define NhlNxyXStyle		"xyXStyle"
#define NhlCxyXStyle		"XyXStyle"

#define NhlNxyYStyle		"xyYStyle"
#define NhlCxyYStyle		"XyYStyle"

#define NhlNxyXIrregularPoints	"xyXIrregularPoints"
#define NhlCxyXIrregularPoints	"XyXIrregularPoints"

#define NhlNxyYIrregularPoints	"xyYIrregularPoints"
#define NhlCxyYIrregularPoints	"XyYIrregularPoints"

#define NhlNxyXReverse		"xyXReverse"
#define NhlCxyXReverse		"XyXReverse"

#define NhlNxyYReverse		"xyYReverse"
#define NhlCxyYReverse		"XyYReverse"

#define	NhlNxyComputeXMin	"xyComputeXMin"
#define	NhlCxyComputeXMin	"XyComputeXMin"

#define NhlNxyXMinF		"xyXMinF"
#define NhlCxyXMinF		"XyXMinF"

#define	NhlNxyComputeXMax	"xyComputeXMax"
#define	NhlCxyComputeXMax	"XyComputeXMax"

#define NhlNxyXMaxF		"xyXMaxF"
#define NhlCxyXMaxF		"XyXMaxF"

#define	NhlNxyComputeYMax	"xyComputeYMax"
#define	NhlCxyComputeYMax	"XyComputeYMax"

#define NhlNxyYMaxF		"xyYMaxF"
#define NhlCxyYMaxF		"XyYMaxF"

#define	NhlNxyComputeYMin	"xyComputeYMin"
#define	NhlCxyComputeYMin	"XyComputeYMin"

#define NhlNxyYMinF		"xyYMinF"
#define NhlCxyYMinF		"XyYMinF"

#define NhlNxyTitles		"xyTitles"
#define NhlCxyTitles		"XyTitles"

#define NhlNxyXAlternate	"xyXAlternate"
#define NhlCxyXAlternate	"XyXAlternate"

#define NhlNxyYAlternate	"xyYAlternate"
#define NhlCxyYAlternate	"XyYAlternate"

#define NhlNxyXAlternateCoords	"xyXAlternateCoords"
#define NhlCxyXAlternateCoords	"XyXAlternateCoords"

#define NhlNxyXOriginalCoords	"xyXOriginalCoords"
#define NhlCxyXOriginalCoords	"XyXOriginalCoords"

#define NhlNxyYAlternateCoords	"xyYAlternateCoords"
#define NhlCxyYAlternateCoords	"XyYAlternateCoords"

#define NhlNxyYOriginalCoords	"xyYOriginalCoords"
#define NhlCxyYOriginalCoords	"XyYOriginalCoords"

#define NhlNxyDashSegmentLengthF	"xyDashSegmentLengthF"
#define NhlCxyDashSegmentLengthF	"XyDashSegmentLengthF"

#define NhlNxyLineLabelFontHeightF	"xyLineLabelFontHeightF"
#define NhlCxyLineLabelFontHeightF	"XyLineLabelFontHeightF"

#define NhlNxyXIrrTensionF		"xyXIrrTensionF"
#define NhlCxyXIrrTensionF		"XyXIrrTensionF"

#define NhlNxyYIrrTensionF		"xyYIrrTensionF"
#define NhlCxyYIrrTensionF		"XyYIrrTensionF"
/*
 * Names for new types.
 */
#define NhlTAlternatePlace	"alternatePlace"
#define NhlTLineLabelMode	"lineLabelMode"
#define NhlTMarkerMode		"markerMode"
#define NhlTMarkerModeGenArray 	"markerModeGenArray"

/*
 * New types
 */
typedef enum _NhlAlternatePlace{
	NhlNONE,
	NhlLEFTAXIS,
	NhlRIGHTAXIS,
	NhlTOPAXIS,
	NhlBOTTOMAXIS
} NhlAlternatePlace;

typedef enum _NhlMarkerMode{
	NhlNOMARKERS,
	NhlMARKERSONLY,
	NhlMARKLINES
} NhlMarkerMode;

typedef enum _NhlLineLabelMode{
	NhlNOLABELS,
	NhlLETTERED,
	NhlCUSTOM
} NhlLineLabelMode;

extern NhlLayerClass NhlxyPlotLayerClass;
extern NhlLayerClass NhlxyDataDepLayerClass;

#endif /* _NXyPlot_h */
