/*
 *      $Id: XyPlot.h,v 1.18 1999-03-24 19:09:51 dbrown Exp $
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
#ifndef _NXYPLOT_h
#define _NXYPLOT_h

#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/LogLinTransObj.h>

/*
 * Resource names
 */

/*
 * DataDep objects resources
 */
#define	NhlNxyDashPattern	"xyDashPattern"
#define NhlNxyDashPatterns	"xyDashPatterns"
#define NhlCxyDashPatterns	"XyDashPatterns"
#define	NhlNxyMonoDashPattern	"xyMonoDashPattern"
#define	NhlCxyMonoDashPattern	"XyMonoDashPattern"

#define NhlNxyMarkLineMode	"xyMarkLineMode"
#define NhlCxyMarkLineMode	"XyMarkLineMode"
#define NhlNxyMarkLineModes	"xyMarkLineModes"
#define NhlCxyMarkLineModes	"XyMarkLineModes"
#define NhlNxyMonoMarkLineMode	"xyMonoMarkLineMode"
#define NhlCxyMonoMarkLineMode	"XyMonoMarkLineMode"

#define NhlNxyExplicitLegendLabels	"xyExplicitLegendLabels"
#define NhlCxyExplicitLegendLabels	"XyExplicitLegendLabels"

#define	NhlNxyLineColor		"xyLineColor"
#define NhlNxyLineColors	"xyLineColors"
#define NhlCxyLineColors	"XyLineColors"
#define	NhlNxyMonoLineColor	"xyMonoLineColor"
#define	NhlCxyMonoLineColor	"XyMonoLineColor"
#define NhlNxyLineOpacityF      "xyLineOpacityF"
#define NhlCxyLineOpacityF      "XyLineOpacityF"
#define NhlNxyLineOpacities     "xyLineOpacities"
#define NhlCxyLineOpacities     "XyLineOpacities"

#define NhlNxyLineLabelFontColor	"xyLineLabelFontColor"
#define NhlNxyLineLabelFontColors	"xyLineLabelFontColors"
#define NhlCxyLineLabelFontColors	"XyLineLabelFontColors"
#define NhlNxyMonoLineLabelFontColor	"xyMonoLineLabelFontColor"
#define NhlCxyMonoLineLabelFontColor	"XyMonoLineLabelFontColor"

#define NhlNxyLabelMode		"xyLabelMode"
#define NhlCxyLabelMode		"XyLabelMode"

#define NhlNxyExplicitLabels	"xyExplicitLabels"
#define NhlCxyExplicitLabels	"XyExplicitLabels"

#define NhlNxyLineThicknessF	"xyLineThicknessF"
#define NhlNxyLineThicknesses	"xyLineThicknesses"
#define NhlCxyLineThicknesses	"XyLineThicknesses"
#define NhlNxyMonoLineThickness	"xyMonoLineThickness"
#define NhlCxyMonoLineThickness	"XyMonoLineThickness"

#define NhlNxyMarker		"xyMarker"
#define NhlNxyMarkers		"xyMarkers"
#define NhlCxyMarkers		"XyMarkers"
#define NhlNxyMonoMarker	"xyMonoMarker"
#define NhlCxyMonoMarker	"XyMonoMarker"

#define NhlNxyMarkerColor	"xyMarkerColor"
#define NhlNxyMarkerColors	"xyMarkerColors"
#define NhlCxyMarkerColors	"XyMarkerColors"
#define NhlNxyMonoMarkerColor	"xyMonoMarkerColor"
#define NhlCxyMonoMarkerColor	"XyMonoMarkerColor"
#define NhlNxyMarkerOpacityF    "xyMarkerOpacityF"
#define NhlCxyMarkerOpacityF    "XyMarkerOpacityF"
#define NhlNxyMarkerOpacities    "xyMarkerOpacities"
#define NhlCxyMarkerOpacities    "XyMarkerOpacities"

#define NhlNxyMarkerSizeF	"xyMarkerSizeF"
#define NhlNxyMarkerSizes	"xyMarkerSizes"
#define NhlCxyMarkerSizes	"XyMarkerSizes"
#define NhlNxyMonoMarkerSize	"xyMonoMarkerSize"
#define NhlCxyMonoMarkerSize	"XyMonoMarkerSize"

#define NhlNxyMarkerThicknessF		"xyMarkerThicknessF"
#define NhlNxyMarkerThicknesses		"xyMarkerThicknesses"
#define NhlCxyMarkerThicknesses		"XyMarkerThicknesses"
#define NhlNxyMonoMarkerThickness	"xyMonoMarkerThickness"
#define NhlCxyMonoMarkerThickness	"XyMonoMarkerThickness"

#define NhlNxyLineDashSegLenF		"xyLineDashSegLenF"
#define NhlNxyLineLabelFontHeightF	"xyLineLabelFontHeightF"

#define NhlNxyLineLabelFont		"xyLineLabelFont"
#define NhlNxyLineLabelFontAspectF	"xyLineLabelFontAspectF"
#define NhlNxyLineLabelFontThicknessF	"xyLineLabelFontThicknessF"
#define NhlNxyLineLabelFontQuality	"xyLineLabelFontQuality"
#define NhlNxyLineLabelConstantSpacingF	"xyLineLabelConstantSpacingF"
#define NhlNxyLineLabelFuncCode		"xyLineLabelFuncCode"

#if 0
/* 
 * these have been replaced by more general class resources
 */
#define	NhlCxyLineColor		"XyLineColor"
#define	NhlCxyDashPattern	"XyDashPattern"
#define NhlCxyMarker		"XyMarker"
#define NhlCxyMarkerColor	"XyMarkerColor"
#define NhlCxyMarkerSizeF	"XyMarkerSizeF"
#define NhlCxyMarkerThicknessF		"XyMarkerThicknessF"
#define NhlCxyLineDashSegLenF		"XyLineDashSegLenF"
#define NhlCxyLineThicknessF	"XyLineThicknessF"
#define NhlCxyLineLabelFontHeightF	"XyLineLabelFontHeightF"
#define NhlCxyLineLabelFontColor	"XyLineLabelFontColor"

#endif

/*
 * XyPlot's resource names
 */

#define NhlNxyCoordData		"xyCoordData"
#define NhlCxyCoordData		"XyCoordData"

#define NhlNxyCoordDataSpec	"xyCoordDataSpec"
#define NhlCxyCoordDataSpec	"XyCoordDataSpec"

#define NhlNxyXStyle		"xyXStyle"
#define NhlCxyXStyle		"XyXStyle"

#define NhlNxyYStyle		"xyYStyle"
#define NhlCxyYStyle		"XyYStyle"

#define NhlNxyXIrregularPoints	"xyXIrregularPoints"
#define NhlCxyXIrregularPoints	"XyXIrregularPoints"

#define NhlNxyYIrregularPoints	"xyYIrregularPoints"
#define NhlCxyYIrregularPoints	"XyYIrregularPoints"

#define	NhlNxyComputeXMin	"xyComputeXMin"
#define	NhlNxyComputeXMax	"xyComputeXMax"
#define	NhlNxyComputeYMin	"xyComputeYMin"
#define	NhlNxyComputeYMax	"xyComputeYMax"
#define	NhlCxyComputeExtent	"XyComputeExtent"
#define NhlNxyCurveDrawOrder	"xyCurveDrawOrder"
#define NhlCxyCurveDrawOrder	"XyCurveDrawOrder"

/*
 * These resources have not been implimented yet.
 */
#ifdef	NOT
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

#define NhlTAlternatePlace	"AlternatePlace"

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
#endif	/* NOT */


/*
 * Names for new types.
 */
#define NhlTLineLabelMode	"LineLabelMode"

typedef enum _NhlLineLabelMode{
	NhlNOLABELS,
	NhlLETTERED,
	NhlCUSTOM
} NhlLineLabelMode;

extern NhlClass NhlxyPlotClass;

#endif /* _NXYPLOT_h */
