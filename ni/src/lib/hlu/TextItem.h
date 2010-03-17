/*
 *      $Id: TextItem.h,v 1.9.12.1 2010-03-17 20:47:07 brownrig Exp $
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
#define NhlNtxJust	"txJust"
#define NhlNtxDirection	"txDirection"
#define NhlNtxFont	"txFont"
#define NhlNtxFontColor	"txFontColor"
#define NhlNtxFontOpacityF	"txFontOpacityF"
#define NhlNtxFontHeightF	"txFontHeightF"
#define NhlNtxFontAspectF	"txFontAspectF"
/*
* Aspect is height/width
*/
#define NhlNtxFontThicknessF	"txFontThicknessF"
#define NhlNtxFontQuality	"txFontQuality"
#define NhlNtxConstantSpacingF	"txConstantSpacingF"
#define NhlNtxFuncCode		"txFuncCode"

#define NhlNtxPerimOn			"txPerimOn"
#define NhlNtxPerimColor		"txPerimColor"
#define NhlNtxPerimThicknessF		"txPerimThicknessF"
#define NhlNtxPerimDashPattern		"txPerimDashPattern"
#define NhlNtxPerimDashLengthF		"txPerimDashLengthF"
#define NhlNtxPerimSpaceF		"txPerimSpaceF"
#define NhlNtxBackgroundFillColor	"txBackgroundFillColor"

#define NhlCtxPerimSpaceF		"TxPerimSpaceF"

/*
 * These class resources have been eliminated
 */
#if 0

#define NhlCtxAngleF 	"TxAngleF"
#define NhlCtxJust	"TxJust"
#define NhlCtxDirection	"TxDirection"
#define NhlCtxFontColor	"TxFontColor"
#define NhlCtxFontHeightF	"TxFontHeightF"
#define NhlCtxFontAspectF	"TxFontAspectF"
#define NhlCtxFontThicknessF	"TxFontThicknessF"
#define NhlCtxFontQuality	"TxFontQuality"
#define NhlCtxConstantSpacingF	"TxConstantSpacingF"
#define NhlCtxFuncCode		"TxFuncCode"
#define NhlCtxPerimOn			"TxPerimOn"
#define NhlCtxPerimColor		"TxPerimColor"
#define NhlCtxPerimThicknessF		"TxPerimThicknessF"
#define NhlCtxPerimDashPattern		"TxPerimDashPattern"
#define NhlCtxPerimDashLengthF		"TxPerimDashLengthF"
#define NhlCtxBackgroundFillColor	"TxBackgroundFillColor"

#endif

extern NhlClass NhltextItemClass;
#endif  /* _NTextItem_h */
