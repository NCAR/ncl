
/*
 *      $Id: XyPlot.h,v 1.1 1993-04-30 17:26:31 boote Exp $
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

#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>

#define NhlNxyNumCurves	"xyNumCurves"
#define NhlNxyXValues	"xyXValues"
#define NhlNxyYValues	"xyYValues"
#define NhlNxyCurveThicknessF	"xyCurveThicknessF"
#define NhlNxyCurveColors	"xyCurveColors"
#define NhlNxyCurveLengths	"xyCurveLengths"
#define NhlNxyCurveDashPatterns "xyCurveDashPatterns"
#define NhlNxyCurveLineLabels	"xyCurveLineLabels"
#define NhlNxyCurveLineLabelMode	"xyCurveLineLabelMode"
#define NhlCxyCurveLineLabelMode	"xyCurveLineLabelMode"
#define NhlNxyXMissingF	"xyXMissingF"
#define NhlNxyYMissingF	"xyYMissingF"
#define NhlNxyXStyle	"xyXStyle"
#define NhlNxyYStyle	"xyYStyle"
#define NhlNxyClip	"xyClip"
#define NhlNxyXIrregularPoints	"xyXIrregularPoints"
#define NhlNxyYIrregularPoints	"xyYIrregularPoints"
#define NhlNxyXNumIrregularPoints "xyXNumIrregularPoints"
#define NhlNxyYNumIrregularPoints "xyYNumIrregularPoints"
#define NhlNxyXReverse	"xyXReverse"
#define NhlNxyYReverse	"xyYReverse"
#define NhlNxyXLeftF	"xyXLeftF"
#define NhlNxyXRightF	"xyXRightF"
#define NhlNxyYTopF	"xyYTopF"
#define NhlNxyYBottomF	"xyYBottomF"
#define NhlNxyTitles	"xyTitles"
#define NhlNxyXAlternate "xyXAlternate"
#define NhlNxyYAlternate "xyYAlternate"
#define NhlNxyXNumAlternateCoords "xyXNumAlternateCoords"
#define NhlNxyYNumAlternateCoords "xyYNumAlternateCoords"
#define NhlNxyXAlternateCoords	"xyXAlternateCoords"
#define NhlNxyXOriginalCoords	"xyXOriginalCoords"
#define NhlNxyYAlternateCoords	"xyYAlternateCoords"
#define NhlNxyYOriginalCoords	"xyYOriginalCoords"
#define NhlNxyDashSegmentLengthF "xyDashSegmentLengthF"
#define NhlNxyLineLabelFontHeightF "xyLineLabelFontHeightF"

#define NhlCxyNumCurves	"XyNumCurves"
#define NhlCxyXValues	"XyXValues"
#define NhlCxyYValues	"XyYValues"
#define NhlCxyCurveColors	"XyColors"
#define NhlCxyCurveLengths	"XyLengths"
#define NhlCxyCurveDashPatterns "XyDashPatterns"
#define NhlCxyCurveLineLabels	"XyLineLabels"
#define NhlCxyCurveThicknessF	"XyCurveThicknessF"
#define NhlCxyXMissingF	"XyXMissingF"
#define NhlCxyYMissingF	"XyYMissingF"
#define NhlCxyXStyle	"XyXStyle"
#define NhlCxyYStyle	"XyYStyle"
#define NhlCxyClip	"XyClip"
#define NhlCxyXIrregularPoints "XyXIrregularPoints"
#define NhlCxyYIrregularPoints "XyYIrregularPoints"
#define NhlCxyXNumIrregularPoints "XyXNumIrregularPoints"
#define NhlCxyYNumIrregularPoints "XyYNumIrregularPoints"
#define NhlCxyXReverse	"XyXReverse"
#define NhlCxyYReverse	"XyYReverse"
#define NhlCxyXLeftF	"XyXLeftF"
#define NhlCxyXRightF	"XyXRightF"
#define NhlCxyYTopF	"XyYTopF"
#define NhlCxyYBottomF	"XyYBottomF"
#define NhlCxyTitles	"XyTitles"
#define NhlCxyXAlternate "XyXAlternate"
#define NhlCxyYAlternate "XyYAlternate"
#define NhlCxyXNumAlternateCoords "XyXNumAlternateCoords"
#define NhlCxyYNumAlternateCoords "XyYNumAlternateCoords"
#define NhlCxyXAlternateCoords	"XyXAlternateCoords"
#define NhlCxyXOriginalCoords	"XyXOriginalCoords"
#define NhlCxyYAlternateCoords	"XyYAlternateCoords"
#define NhlCxyYOriginalCoords	"XyYOriginalCoords"
#define NhlCxyDashSegmentLengthF "XyDashSegmentLengthF"
#define NhlCxyLineLabelFontHeightF "XyLineLabelFontHeightF"

typedef enum { NONE, LEFTAXIS, RIGHTAXIS, TOPAXIS, BOTTOMAXIS } AlternatePlace;
typedef enum { NOLABELS, LETTERED, CUSTOM } LineLabelModes;
#define NhlTAlternatePlace "alternatePlace"
#define NhlTLineLabelModes "lineLabelModes"

extern LayerClass xyPlotLayerClass;

typedef struct _XyPlotLayerClassRec *XyPlotLayerClass;
typedef struct _XyPlotLayerRec *XyPlotLayer;
#endif /* _NXyPlot_h */

