/*
 *      $Id: Title.h,v 1.2 1994-01-27 21:26:37 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Title.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Nov 18 17:01:12 MST 1992
 *
 *	Description:	
 */

#ifndef _NTitle_h
#define	_NTitle_h

#include <ncarg/hlu/View.h>

#define NhlNtiDeltaF	"tiDeltaF"
#define NhlCtiDeltaF	"TiDeltaF"
#define NhlNtiMainString	"tiMainString"
#define NhlCtiMainString	"TiMainString"
#define NhlNtiMainJust	"tiMainJust"
#define NhlNtiMainFont	"tiMainFont"
#define NhlNtiMainFontColor	"tiMainFontColor"
#define NhlNtiMainOffsetXF	"tiMainOffsetXF"
#define NhlCtiMainOffsetXF	"TiMainOffsetXF"
#define NhlNtiMainOffsetYF	"tiMainOffsetYF"
#define NhlCtiMainOffsetYF	"TiMainOffsetYF"
#define NhlNtiMainFontHeightF "tiMainFontHeightF"
#define NhlNtiMainFontAspectF "tiMainFontAspectF"
#define NhlNtiMainFontThicknessF "tiMainFontThicknessF"
#define NhlNtiMainAngleF	"tiMainAngleF"
#define NhlNtiMainDirection "tiMainDirection"
#define NhlCtiMainDirection "TiMainDirection"
#define NhlNtiMainPosition  "tiMainPosition"
#define NhlCtiMainPosition  "TiMainPosition"
#define NhlNtiMainOn		"tiMainOn"
#define NhlCtiMainOn		"TiMainOn"
#define NhlNtiMainSide		"tiMainSide"
#define NhlCtiMainSide		"TiMainSide"
#define NhlNtiMainConstantSpacingF	"tiMainConstantSpacingF"
#define NhlNtiUseMainAttributes		"tiUseMainAttributes"
#define NhlCtiUseMainAttributes		"TiUseMainAttributes"
#define NhlNtiMainFuncCode		"tiMainFuncCode"
#define NhlNtiMainFontQuality		"tiMainFontQuality"

#define NhlNtiXAxisFuncCode		"tiXAxisFuncCode"
#define NhlNtiXAxisString "tiXAxisString"
#define NhlCtiXAxisString "TiXAxisString"
#define NhlNtiXAxisJust	"tiXAxisJust"
#define NhlNtiXAxisFont	"tiXAxisFont"
#define NhlNtiXAxisFontColor	"tiXAxisFontColor"
#define NhlNtiXAxisOffsetXF	"tiXAxisOffsetXF"
#define NhlCtiXAxisOffsetXF	"TiXAxisOffsetXF"
#define NhlNtiXAxisOffsetYF	"tiXAxisOffsetYF"
#define NhlCtiXAxisOffsetYF	"TiXAxisOffsetYF"
#define NhlNtiXAxisFontHeightF "tiXAxisFontHeightF"
#define NhlNtiXAxisFontAspectF "tiXAxisFontAspectF"
#define NhlNtiXAxisFontThicknessF "tiXAxisFontThicknessF"
#define NhlNtiXAxisAngleF	"tiXAxisAngleF"
#define NhlNtiXAxisDirection "tiXAxisDirection"
#define NhlCtiXAxisDirection "TiXAxisDirection"
#define NhlNtiXAxisPosition  "tiXAxisPosition"
#define NhlCtiXAxisPosition  "TiXAxisPosition"
#define NhlNtiXAxisOn		"tiXAxisOn"
#define NhlCtiXAxisOn		"TiXAxisOn"
#define NhlNtiXAxisSide		"tiXAxisSide"
#define NhlCtiXAxisSide		"TiXAxisSide"
#define NhlNtiXAxisConstantSpacingF	"tiXAxisConstantSpacingF"
#define NhlNtiXAxisFontQuality		"tiXAxisFontQuality"

#define NhlNtiYAxisFuncCode		"tiYAxisFuncCode"
#define NhlNtiYAxisString "tiYAxisString"
#define NhlCtiYAxisString "TiYAxisString"
#define NhlNtiYAxisJust	"tiYAxisJust"
#define NhlNtiYAxisFont	"tiYAxisFont"
#define NhlNtiYAxisFontColor "tiYAxisFontColor"
#define NhlNtiYAxisOffsetXF	"tiYAxisOffsetXF"
#define NhlCtiYAxisOffsetXF	"TiYAxisOffsetXF"
#define NhlNtiYAxisOffsetYF	"tiYAxisOffsetYF"
#define NhlCtiYAxisOffsetYF	"TiYAxisOffsetYF"
#define NhlNtiYAxisFontHeightF "tiYAxisFontHeightF"
#define NhlNtiYAxisFontAspectF "tiYAxisFontAspectF"
#define NhlNtiYAxisFontThicknessF "tiYAxisFontThicknessF"
#define NhlNtiYAxisAngleF	"tiYAxisAngleF"
#define NhlNtiYAxisDirection "tiYAxisDirection"
#define NhlCtiYAxisDirection "TiYAxisDirection"
#define NhlNtiYAxisPosition  "tiYAxisPosition"
#define NhlCtiYAxisPosition  "TiYAxisPosition"
#define NhlNtiYAxisOn		"tiYAxisOn"
#define NhlCtiYAxisOn		"TiYAxisOn"
#define NhlNtiYAxisSide		"tiYAxisSide"
#define NhlCtiYAxisSide		"TiYAxisSide"
#define NhlNtiYAxisConstantSpacingF	"tiYAxisConstantSpacingF"
#define NhlNtiYAxisFontQuality		"tiYAxisFontQuality"
/*
* only defining one class for each font resource so users can default
* all titles instead of each
*/
#define NhlCtiTitleJust	"TiTitleJust"
#define NhlCtiTitleFontHeightsF "TiTitleFontHeightsF"
#define NhlCtiTitleFontAspectsF "TiTitleFontAspectsF"
#define NhlCtiTitleFontThicknessF "TiTitleFontThicknessF"
#define NhlCtiTitleAnglesF	"TiTitleAnglesF"
#define NhlCtiTitleConstantSpacingsF	"TiTitleConstantSpacingsF"
#define NhlCtiTitleFuncCodes		"TiTitleFuncCodes"
#define NhlCtiTitleFontQualities	"TiTitleFontQuailties"
#define NhlCtiTitleFontColors		"TiTitleFontColors"



typedef NhlPosition	NhlTitlePositions;
#define NhlTTitlePositions	"TitlePositions"

extern NhlLayerClass NhltitleLayerClass;

#endif /*_NTitle_h */
