/*
 *      $Id: GraphicStyle.h,v 1.6.12.1 2010-03-17 20:47:07 brownrig Exp $
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

/* shared resources */

#define NhlNgsColors		"gsColors"
#define NhlNgsSegments		"gsSegments"
#define NhlCgsColors		"GsColors"
#define NhlCgsSegments		"GsSegments"


/* line style resources */

#define NhlNgsLineDashPattern		"gsLineDashPattern"
#define NhlNgsLineDashSegLenF		"gsLineDashSegLenF"
#define NhlNgsLineColor			"gsLineColor"
#define NhlNgsLineOpacityF			"gsLineOpacityF"
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
#define NhlNgsFillOpacityF      "gsFillOpacityF"
#define NhlNgsFillBackgroundColor    	"gsFillBackgroundColor"
#define NhlNgsFillScaleF		"gsFillScaleF"
#define NhlNgsFillLineThicknessF 	"gsFillLineThicknessF"
#define NhlNgsFillDotSizeF 		"gsFillDotSizeF"

/* edge resources */

#define NhlNgsEdgesOn			"gsEdgesOn"
#define NhlNgsEdgeDashPattern   	"gsEdgeDashPattern"
#define NhlNgsEdgeThicknessF    	"gsEdgeThicknessF"
#define NhlNgsEdgeDashSegLenF   	"gsEdgeDashSegLenF"
#define NhlNgsEdgeColor         	"gsEdgeColor"

/* marker resources */

#define NhlNgsMarkerIndex		"gsMarkerIndex"
#define NhlNgsMarkerColor       	"gsMarkerColor"
#define NhlNgsMarkerOpacityF       	"gsMarkerOpacityF"
#define NhlNgsMarkerSizeF		"gsMarkerSizeF"
#define NhlNgsMarkerThicknessF  	"gsMarkerThicknessF"

/* text resources */

#define NhlNgsTextAngleF		"gsTextAngleF"
#define NhlNgsTextJustification		"gsTextJustification"
#define NhlNgsTextDirection		"gsTextDirection"
#define NhlNgsFont			"gsFont"
#define NhlNgsFontColor			"gsFontColor"
#define NhlNgsFontOpacityF		"gsFontOpacityF"
#define NhlNgsFontHeightF		"gsFontHeightF"
#define NhlNgsFontAspectF		"gsFontAspectF"
#define NhlNgsFontThicknessF		"gsFontThicknessF"
#define NhlNgsFontQuality		"gsFontQuality"
#define NhlNgsTextConstantSpacingF	"gsTextConstantSpacingF"
#define NhlNgsTextFuncCode		"gsTextFuncCode"

/* class resources */

#if 0
/* 
 * Replaced by generic class font resources
 */
#define NhlCLineLabelFont		"LineLabelFont"
#define NhlCLineLabelFontColor		"LineLabelFontColor"
#define NhlCLineLabelFontHeightF	"LineLabelFontHeightF"
#define NhlCLineLabelFontAspectF	"LineLabelFontAspectF"
#define NhlCLineLabelFontThicknessF	"LineLabelFontThicknessF"
#define NhlCLineLabelFontQuality	"LineLabelFontQuality"
#define NhlCLineLabelConstantSpacingF	"LineLabelConstantSpacingF"
#define NhlCLineLabelFuncCode		"LineLabelFuncCode"
#endif
extern NhlClass NhlgraphicStyleClass;


#endif  /* _NGRAPHICSTYLE_h */
