
/*
 *      $Id: Legend.h,v 1.1 1993-07-27 18:03:06 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Legend.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	Legend public header file
 */
#ifndef _NLegend_h
#define _NLegend_h

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
#define NhlCtxFont	"TxFont"
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
#define NhlNtxXCorners		".txXCorners"
#define NhlCtxXCorners		".TxXCorners"
#define NhlNtxYCorners		".txYCorners"
#define NhlCtxYCorners		".TxYCorners"


typedef enum {HIGH,MEDIUM,LOW} FontQuality;
typedef enum {DOWN,UP,ACROSS} TextDirection;

#define NhlTFQuality "fquality"
#define NhlTTextDirection "tdirection"

extern LayerClass legendLayerClass;

typedef struct _LegendLayerClassRec *LegendLayerClass;
typedef struct _LegendLayerRec	*LegendLayer;
#endif  /* _NLegend_h */

