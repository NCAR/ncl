/*
 *      $Id: XyPlot.h,v 1.3 1993-11-10 01:19:46 ethan Exp $
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
#define NhlNxyColors	"xyColors"
#define NhlCxyColors	"XyColors"

#define	NhlNxyColor	"xyColor"
#define	NhlCxyColor	"XyColor"

#define NhlNxyDashPatterns "xyDashPatterns"
#define NhlCxyDashPatterns "XyDashPatterns"

#define	NhlNxyDashPattern	"xyDashPattern"
#define	NhlCxyDashPattern	"XyDashPattern"

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
#define NhlTLineLabelModes	"lineLabelModes"

/*
 * New types
 */
typedef enum { NONE, LEFTAXIS, RIGHTAXIS, TOPAXIS, BOTTOMAXIS } AlternatePlace;
typedef enum { NOLABELS, LETTERED, CUSTOM } LineLabelModes;

extern LayerClass xyPlotLayerClass;

typedef struct _XyPlotLayerClassRec *XyPlotLayerClass;
typedef struct _XyPlotLayerRec *XyPlotLayer;
#endif /* _NXyPlot_h */
