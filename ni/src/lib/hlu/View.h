/*
 *      $Id: View.h,v 1.18.12.1 2010-03-17 20:47:07 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		View.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 1 09:59:16 MDT 1992
 *
 *	Description:	Public header file for the ViewClass.
 */
#ifndef _NVIEW_h
#define _NVIEW_h

#include <ncarg/hlu/Base.h>

/* types used by Legend and LabelBar */
#define NhlTOrientation "Orientation"
typedef enum _NhlOrientation {
	NhlHORIZONTAL = 0,
	NhlVERTICAL = 1
} NhlOrientation;

#define NhlTDrawOrder "DrawOrder"
typedef enum _NhlDrawOrder {
	NhlPREDRAW,
	NhlDRAW,
	NhlPOSTDRAW
} NhlDrawOrder;

NhlDOCTAG(NhlCoord)
typedef struct _NhlCoord {
	float x;
	float y;
} NhlCoord;

#define	NhlNvpOn	"vpOn"
#define	NhlCvpOn	"VpOn"
#define NhlNvpXF "vpXF"
#define NhlCvpXF "VpXF"
#define NhlNvpYF "vpYF"
#define NhlCvpYF "VpYF"
#define NhlNvpWidthF "vpWidthF"
#define NhlCvpWidthF "VpWidthF"
#define NhlNvpHeightF "vpHeightF"
#define NhlCvpHeightF "VpHeightF"
#define NhlNvpKeepAspect	"vpKeepAspect"
#define NhlCvpKeepAspect	"VpKeepAspect"
#define NhlNvpUseSegments	"vpUseSegments"
#define NhlCvpUseSegments	"VpUseSegments"
#define NhlNvpAnnoManagerId	"vpAnnoManagerId"
#define NhlCvpAnnoManagerId	"VpAnnoManagerId"
#define NhlNvpClipOn            "vpClipOn"
#define NhlCvpClipOn	        "VpClipOn"

NhlDOCTAG(NhlBoundingBox)
typedef struct _NhlBoundingBox {
        int     set;
        float   t;
        float   b;
        float   l;
        float   r;
} NhlBoundingBox;

extern NhlErrorTypes NhlGetBB(
#if	NhlNeedProto
	int,		/* pid */
	NhlBoundingBox*	/* thebox */
#endif
);

extern NhlBoolean NhlIsView(
#if	NhlNeedProto
	int	pid
#endif
);

/* Common class resources */

#define NhlCLineDashPattern		"LineDashPattern"
#define NhlCLineDashSegLenF		"LineDashSegLenF"
#define NhlCLineColor			"LineColor"
#define NhlCLineOpacityF		"LineOpacityF"
#define NhlCLineThicknessF		"LineThicknessF"
#define NhlCLineLabelString		"LineLabelString"
#define NhlCFillPattern			"FillPattern"
#define NhlCFillColor			"FillColor"
#define NhlCFillOpacityF			"FillOpacityF"
#define NhlCFillBackgroundColor		"FillBackgroundColor"
#define NhlCFillScaleF			"FillScaleF"
#define NhlCFillLineThicknessF		"FillLineThicknessF"
#define NhlCFillDotSizeF		"FillDotSizeF"
#define NhlCEdgesOn			"EdgesOn"
#define NhlCEdgeDashPattern		"EdgeDashPattern"
#define NhlCEdgeThicknessF		"EdgeThicknessF"
#define NhlCEdgeDashSegLenF		"EdgeDashSegLenF"
#define NhlCEdgeColor			"EdgeColor"
#define NhlCEdgeBorderWidthF		"EdgeBorderWidthF"
#define NhlCMarkerIndex			"MarkerIndex"
#define NhlCMarkerColor			"MarkerColor"
#define NhlCMarkerOpacityF		"MarkerOpacityF"
#define NhlCMarkerSizeF			"MarkerSizeF"
#define NhlCMarkerThicknessF		"MarkerThicknessF"

#define NhlCTextAngleF 			"TextAngleF"
#define NhlCTextJustification		"TextJustification"
#define NhlCTextDirection		"TextDirection"
#define NhlCFont			"Font"
#define NhlCFontColor			"FontColor"
#define NhlCFontOpacityF		"FontOpacityF"
#define NhlCFontHeightF			"FontHeightF"
#define NhlCFontAspectF			"FontAspectF"
#define NhlCFontThicknessF		"FontThicknessF"
#define NhlCFontQuality			"FontQuality"
#define NhlCTextConstantSpacingF	"TextConstantSpacingF"
#define NhlCTextFuncCode		"TextFuncCode"
#define NhlCYAxisTextDirection		"YAxisTextDirection"
#define NhlCYAxisTextAngleF		"YAxisTextAngleF"
#define NhlCYAxisTextJustification	"YAxisTextJustification"

#define NhlCNumberFormat		"NumberFormat"
#define NhlCPlotLabelsOn		"PlotLabelsOn"
#define NhlCAnnotationLabelsOn		"AnnotationLabelsOn"
#define NhlCLabelAutoStride		"LabelAutoStride"

extern NhlClass NhlviewClass;

#endif /*_NVIEW_h*/
