/*
 *      $Id: TextItem.h,v 1.6 1995-04-04 06:48:00 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TextItem.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 10 12:53:44 MST 1992
 *
 *	Description:	TextItem public header file
 */
#ifndef _NTextItem_h
#define _NTextItem_h

#include <ncarg/hlu/View.h>

#define NhlNtxString	"txString"
#define NhlCtxString	"TxString"
#define NhlNtxPosXF	"txPosXF"
#define NhlCtxPosXF	"TxPosXF"
#define NhlNtxPosYF	"txPosYF"
#define NhlCtxPosYF	"TxPosYF"
#define NhlNtxAngleF	"txAngleF"
#define NhlCtxAngleF 	"TxAngleF"
#define NhlNtxJust	"txJust"
#define NhlCtxJust	"TxJust"
#define NhlNtxDirection	"txDirection"
#define NhlCtxDirection	"TxDirection"
#define NhlNtxFont	"txFont"
#define NhlCFont	"Font"
#define NhlNtxFontColor	"txFontColor"
#define NhlCtxFontColor	"TxFontColor"
#define NhlNtxFontHeightF	"txFontHeightF"
#define NhlCtxFontHeightF	"TxFontHeightF"
#define NhlNtxFontAspectF	"txFontAspectF"
#define NhlCtxFontAspectF	"TxFontAspectF"
/*
* Aspect is height/width
*/
#define NhlNtxFontThicknessF	"txFontThicknessF"
#define NhlCtxFontThicknessF	"TxFontThicknessF"
#define NhlNtxFontQuality	"txFontQuality"
#define NhlCtxFontQuality	"TxFontQuality"
#define NhlNtxConstantSpacingF	"txConstantSpacingF"
#define NhlCtxConstantSpacingF	"TxConstantSpacingF"
#define NhlNtxFuncCode		"txFuncCode"
#define NhlCtxFuncCode		"TxFuncCode"

#define NhlNtxPerimOn			"txPerimOn"
#define NhlNtxPerimColor		"txPerimColor"
#define NhlNtxPerimThicknessF		"txPerimThicknessF"
#define NhlNtxPerimDashPattern		"txPerimDashPattern"
#define NhlNtxPerimDashLengthF		"txPerimDashLengthF"
#define NhlNtxPerimSpaceF		"txPerimSpaceF"
#define NhlNtxBackgroundFillColor	"txBackgroundFillColor"

#define NhlCtxPerimOn			"TxPerimOn"
#define NhlCtxPerimColor		"TxPerimColor"
#define NhlCtxPerimThicknessF		"TxPerimThicknessF"
#define NhlCtxPerimDashPattern		"TxPerimDashPattern"
#define NhlCtxPerimDashLengthF		"TxPerimDashLengthF"
#define NhlCtxPerimSpaceF		"TxPerimSpaceF"
#define NhlCtxBackgroundFillColor	"TxBackgroundFillColor"

typedef enum {NhlHIGH,NhlMEDIUM,NhlLOW} NhlFontQuality;
typedef enum {NhlDOWN,NhlACROSS} NhlTextDirection;

#define NhlTFontQuality		"FontQuality"
#define NhlTTextDirection	"TextDirection"

extern NhlLayerClass NhltextItemLayerClass;
#endif  /* _NTextItem_h */
