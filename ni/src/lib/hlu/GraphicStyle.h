/*
 *      $Id: GraphicStyle.h,v 1.1 1996-02-26 21:45:54 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		GraphicStyle.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 20 18:10:29 MST 1996
 *
 *	Description:	GraphicStyle public header file
 */
#ifndef _NGRAPHICSTYLE_h
#define _NGRAPHICSTYLE_h

#include <ncarg/hlu/Style.h>

/* line style resources */

#define NhlNgsLineDashPattern		"gsLineDashPattern"
#define NhlNgsLineDashSegLenF		"gsLineDashSegLenF"
#define NhlNgsLineColor			"gsLineColor"
#define NhlNgsLineThicknessF		"gsLineThicknessF"

#define NhlNgsLineLabelString		"gsLineLabelString"
#define NhlNgsLineLabelFont		"gsLineLabelFont"
#define NhlNgsLineLabelFontColor	"gsLineLabelFontColor"
#define NhlNgsLineLabelFontHeightF	"gsLineLabelFontHeightF"
#define NhlNgsLineLabelFontAspectF	"gsLineLabelFontAspectF"
#define NhlNgsLineLabelFontThicknessF	"gsLineLabelFontThicknessF"
#define NhlNgsLineLabelFontQuality	"gsLineLabelFontQuality"
#define NhlNgsLineLabelConstantSpacingF	"gsLineLabelConstantSpacingF"
#define NhlNgsLineLabelFuncCode		"gsLineLabelFuncCode"

/* fill resources */

#define NhlNgsFillIndex			"gsFillIndex"
#define NhlNgsFillColor         	"gsFillColor"
#define NhlNgsFillBackgroundColor    	"gsFillBackgroundColor"
#define NhlNgsFillScaleFactorF		"gsFillScaleFactorF"
#define NhlNgsFillLineThicknessF 	"gsFillLineThicknessF"

/* edge resources */

#define NhlNgsEdgesOn			"gsEdgesOn"
#define NhlNgsEdgeDashPattern   	"gsEdgeDashPattern"
#define NhlNgsEdgeThicknessF    	"gsEdgeThicknessF"
#define NhlNgsEdgeDashSegLenF   	"gsEdgeDashSegLenF"
#define NhlNgsEdgeColor         	"gsEdgeColor"

/* marker resources */

#define NhlNgsMarkerIndex		"gsMarkerIndex"
#define NhlNgsMarkerColor       	"gsMarkerColor"
#define NhlNgsMarkerSizeF		"gsMarkerSizeF"
#define NhlNgsMarkerThicknessF  	"gsMarkerThicknessF"

/* text resources */

#define NhlNgsTextAngleF		"gsTextAngleF"
#define NhlNgsTextJustification		"gsTextJustication"
#define NhlNgsDirection			"gsDirection"
#define NhlNgsFont			"gsFont"
#define NhlNgsFontColor			"gsFontColor"
#define NhlNgsFontHeightF		"gsFontHeightF"
#define NhlNgsFontAspectF		"gsFontAspectF"
#define NhlNgsFontThicknessF		"gsFontThicknessF"
#define NhlNgsFontQuality		"gsFontQuality"
#define NhlNgsConstantSpacingF		"gsConstantSpacingF"
#define NhlNgsFuncCode			"gsFuncCode"

/* class resources */

#define NhlCLineDashPattern		"LineDashPattern"
#define NhlCLineDashSegLenF		"LineDashSegLenF"
#define NhlCLineColor			"LineColor"
#define NhlCLineThicknessF		"LineThicknessF"
#define NhlCLineLabelString		"LineLabelString"
#define NhlCLineLabelFont		"LineLabelFont"
#define NhlCLineLabelFontColor		"LineLabelFontColor"
#define NhlCLineLabelFontHeightF	"LineLabelFontHeightF"
#define NhlCLineLabelFontAspectF	"LineLabelFontAspectF"
#define NhlCLineLabelFontThicknessF	"LineLabelFontThicknessF"
#define NhlCLineLabelFontQuality	"LineLabelFontQuality"
#define NhlCLineLabelConstantSpacingF	"LineLabelConstantSpacingF"
#define NhlCLineLabelFuncCode		"LineLabelFuncCode"
#define NhlCFillIndex			"FillIndex"
#define NhlCFillColor			"FillColor"
#define NhlCFillBackgroundColor		"FillBackgroundColor"
#define NhlCFillScaleFactorF		"FillScaleFactorF"
#define NhlCFillLineThicknessF		"FillLineThicknessF"
#define NhlCEdgesOn			"EdgesOn"
#define NhlCEdgeDashPattern		"EdgeDashPattern"
#define NhlCEdgeThicknessF		"EdgeThicknessF"
#define NhlCEdgeDashSegLenF		"EdgeDashSegLenF"
#define NhlCEdgeColor			"EdgeColor"
#define NhlCMarkerIndex			"MarkerIndex"
#define NhlCMarkerColor			"MarkerColor"
#define NhlCMarkerSizeF			"MarkerSizeF"
#define NhlCMarkerThicknessF		"MarkerThicknessF"

#define NhlCTextAngleF 			"TextAngleF"
#define NhlCTextJustification		"TextJustification"
#define NhlCTextDirection		"TextDirection"
#define NhlCFont			"Font"
#define NhlCFontColor			"FontColor"
#define NhlCFontHeightF			"FontHeightF"
#define NhlCFontAspectF			"FontAspectF"
#define NhlCFontThicknessF		"FontThicknessF"
#define NhlCFontQuality			"FontQuality"
#define NhlCConstantSpacingF		"ConstantSpacingF"
#define NhlCFuncCode			"FuncCode"

extern NhlClass NhlgraphicStyleClass;


#endif  /* _NGRAPHICSTYLE_h */
