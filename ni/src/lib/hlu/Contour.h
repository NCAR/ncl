/*
 *      $Id: Contour.h,v 1.7 1994-04-05 00:51:07 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Contour.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for Contour class.
 */

#ifndef _NContour_h
#define _NContour_h

#include <ncarg/hlu/Workspace.h>
#include <ncarg/hlu/Overlay.h>

/* Level selection modes */

#define Nhl_cnMANUAL		0
#define Nhl_cnEQUALSPACING	1
#define Nhl_cnAUTOMATIC		2
#define Nhl_cnEXPLICIT		3

/* Level usage modes */

#define Nhl_cnNOLINE		0
#define Nhl_cnLINEONLY		1
#define Nhl_cnLABELONLY		2
#define Nhl_cnLINEANDLABEL	3

/* Line label position */

#define Nhl_cnNOLABELS		0
#define Nhl_cnCONSTANT		1
#define Nhl_cnRANDOMIZED	2
#define Nhl_cnSMART		3

/* Line label mask modes */

#define Nhl_cnNOMASK		0
#define Nhl_cnSOLIDMASK		1
#define Nhl_cnSOFTMASK		2

/*
 * Contour instance resources
 */

#define NhlNcnOutOfRangeValF		"cnOutOfRangeValF"
#define NhlNcnSpecialValF		"cnSpecialValF"

#define NhlNcnLevelCount		".cnLevelCount"		/* read-only */
#define NhlNcnLevelSelectionMode	"cnLevelSelectionMode"
#define NhlNcnMaxLevelCount		"cnMaxLevelCount"
#define NhlNcnLevelSpacingF		"cnLevelSpacingF"
#define NhlNcnLabelMasking		"cnLabelMasking"
#define NhlNcnMinLevelValF		"cnMinLevelValF"
#define NhlNcnMaxLevelValF		"cnMaxLevelValF"
#define NhlNcnLineLabelInterval		"cnLineLabelInterval"

#define NhlNcnMonoLevelFlag		"cnMonoLevelFlag"
#define NhlNcnMonoFillColor		"cnMonoFillColor"
#define NhlNcnMonoFillPattern		"cnMonoFillPattern"
#define NhlNcnMonoFillScale		"cnMonoFillScale"
#define NhlNcnMonoLineColor		"cnMonoLineColor"
#define NhlNcnMonoLineDashPattern	"cnMonoLineDashPattern"
#define NhlNcnMonoLineThickness		"cnMonoLineThickness"
#define NhlNcnMonoLineLabelColor	"cnMonoLineLabelColor"

#define NhlNcnLevels			"cnLevels"
#define NhlNcnLevelFlags		"cnLevelFlags"
#define NhlNcnFillColors		"cnFillColors"
#define NhlNcnFillPatterns		"cnFillPatterns"
#define NhlNcnFillScales		"cnFillScales"

#define NhlNcnLineColors		"cnLineColors"
#define NhlNcnLineDashPatterns		"cnLineDashPatterns"
#define NhlNcnLineThicknesses		"cnLineThicknesses"
#define NhlNcnLineLabelStrings		"cnLineLabelStrings"
#define NhlNcnLineLabelColors		"cnLineLabelColors"

#define NhlNcnLineDashSegLenF		"cnLineDashSegLenF"
#define NhlNcnLowUseHighLabelRes	"cnLowUseHighLabelRes"
#define NhlNcnHighUseLineLabelRes	"cnHighUseLineLabelRes"
#define NhlNcnLineUseInfoLabelRes	"cnLineUseInfoLabelRes"

#define NhlNcnLineLabelPosition		"cnLineLabelPosition"
#define NhlNcnLineLabelAngleF		"cnLineLabelAngleF"

#define NhlNcnLineLabelsOn		"cnLineLabelsOn"
#define NhlNcnLineLabelTextHeightF	"cnLineLabelTextHeightF"
#define NhlNcnLineLabelFont		"cnLineLabelFont"
#define NhlNcnLineLabelFontAspectF	"cnLineLabelFontAspectF"
#define NhlNcnLineLabelFontThicknessF	"cnLineLabelFontThicknessF"
#define NhlNcnLineLabelFontQuality	"cnLineLabelFontQuality"
#define NhlNcnLineLabelConstantSpacingF	"cnLineLabelConstantSpacingF"
#define NhlNcnLineLabelFuncCode		"cnLineLabelFuncCode"
#define NhlNcnLineLabelBackgroundColor	"cnLineLabelBackgroundColor"
#define NhlNcnLineLabelPerim		"cnLineLabelPerim"
#define NhlNcnLineLabelPerimSpaceF	"cnLineLabelPerimSpaceF"
#define NhlNcnLineLabelPerimThicknessF	"cnLineLabelPerimThicknessF"
#define NhlNcnLineLabelPerimColor	"cnLineLabelPerimColor"

#define NhlNcnInfoLabelOn		"cnInfoLabelOn"
#define NhlNcnInfoLabelTextHeightF	"cnInfoLabelTextHeightF"
#define NhlNcnInfoLabelFont		"cnInfoLabelFont"
#define NhlNcnInfoLabelFontColor	"cnInfoLabelFontColor"
#define NhlNcnInfoLabelFontAspectF	"cnInfoLabelFontAspectF"
#define NhlNcnInfoLabelFontThicknessF	"cnInfoLabelFontThicknessF"
#define NhlNcnInfoLabelFontQuality	"cnInfoLabelFontQuality"
#define NhlNcnInfoLabelConstantSpacingF	"cnInfoLabelConstantSpacingF"
#define NhlNcnInfoLabelFuncCode		"cnInfoLabelFuncCode"
#define NhlNcnInfoLabelBackgroundColor	"cnInfoLabelBackgroundColor"
#define NhlNcnInfoLabelPerim		"cnInfoLabelPerim"
#define NhlNcnInfoLabelPerimSpaceF	"cnInfoLabelPerimSpaceF"
#define NhlNcnInfoLabelPerimThicknessF	"cnInfoLabelPerimThicknessF"
#define NhlNcnInfoLabelPerimColor	"cnInfoLabelPerimColor"

#define NhlNcnHighLabelsOn		"cnHighLabelsOn"
#define NhlNcnHighLabelTextHeightF	"cnHighLabelTextHeightF"
#define NhlNcnHighLabelFont		"cnHighLabelFont"
#define NhlNcnHighLabelFontColor	"cnHighLabelFontColor"
#define NhlNcnHighLabelFontAspectF	"cnHighLabelFontAspectF"
#define NhlNcnHighLabelFontThicknessF	"cnHighLabelFontThicknessF"
#define NhlNcnHighLabelFontQuality	"cnHighLabelFontQuality"
#define NhlNcnHighLabelConstantSpacingF	"cnHighLabelConstantSpacingF"
#define NhlNcnHighLabelFuncCode		"cnHighLabelFuncCode"
#define NhlNcnHighLabelBackgroundColor	"cnHighLabelBackgroundColor"
#define NhlNcnHighLabelPerim		"cnHighLabelPerim"
#define NhlNcnHighLabelPerimSpaceF	"cnHighLabelPerimSpaceF"
#define NhlNcnHighLabelPerimThicknessF	"cnHighLabelPerimThicknessF"
#define NhlNcnHighLabelPerimColor	"cnHighLabelPerimColor"

#define NhlNcnLowLabelsOn		"cnLowLabelsOn"
#define NhlNcnLowLabelTextHeightF	"cnLowLabelTextHeightF"
#define NhlNcnLowLabelFont		"cnLowLabelFont"
#define NhlNcnLowLabelFontColor		"cnLowLabelFontColor"
#define NhlNcnLowLabelFontAspectF	"cnLowLabelFontAspectF"
#define NhlNcnLowLabelFontThicknessF	"cnLowLabelFontThicknessF"
#define NhlNcnLowLabelFontQuality	"cnLowLabelFontQuality"
#define NhlNcnLowLabelConstantSpacingF	"cnLowLabelConstantSpacingF"
#define NhlNcnLowLabelFuncCode		"cnLowLabelFuncCode"
#define NhlNcnLowLabelBackgroundColor	"cnLowLabelBackgroundColor"
#define NhlNcnLowLabelPerim		"cnLowLabelPerim"
#define NhlNcnLowLabelPerimSpaceF	"cnLowLabelPerimSpaceF"
#define NhlNcnLowLabelPerimThicknessF	"cnLowLabelPerimThicknessF"
#define NhlNcnLowLabelPerimColor	"cnLowLabelPerimColor"

/*
 * Contour class resources
 */

#define NhlCcnOutOfRangeValF		"CnOutOfRangeValF"

#define NhlCcnSpecialValF		"CnSpecialValF"

#define NhlCcnLevelCount		".CnLevelCount"		/* read-only */
#define NhlCcnLevelSelectionMode	"CnLevelSelectionMode"
#define NhlCcnMaxLevelCount		"CnMaxLevelCount"
#define NhlCcnLevelSpacingF		"CnLevelSpacingF"
#define NhlCcnLabelMasking		"CnLabelMasking"
#define NhlCcnMinLevelValF		"CnMinLevelValF"
#define NhlCcnMaxLevelValF		"CnMaxLevelValF"
#define NhlCcnLineLabelInterval		"CnLineLabelInterval"

#define NhlCcnMonoLevelFlag		"CnMonoLevelFlag"
#define NhlCcnMonoFillColor		"CnMonoFillColor"
#define NhlCcnMonoFillPattern		"CnMonoFillPattern"
#define NhlCcnMonoFillScale		"CnMonoFillScale"
#define NhlCcnMonoLineColor		"CnMonoLineColor"
#define NhlCcnMonoLineDashPattern	"CnMonoLineDashPattern"
#define NhlCcnMonoLineThickness		"CnMonoLineThickness"
#define NhlCcnMonoLineLabelColor	"CnMonoLineLabelColor"

#define NhlCcnLevels			"CnLevels"
#define NhlCcnLevelFlags		"CnLevelFlags"
#define NhlCcnFillColors		"CnFillColors"
#define NhlCcnFillPatterns		"CnFillPatterns"
#define NhlCcnFillScales		"CnFillScales"

#define NhlCcnLineColors		"CnLineColors"
#define NhlCcnLineDashPatterns		"CnLineDashPatterns"
#define NhlCcnLineThicknesses		"CnLineThicknesses"
#define NhlCcnLineLabelStrings		"CnLineLabelStrings"
#define NhlCcnLineLabelColors		"CnLineLabelColors"

#define NhlCcnLineDashSegLenF		"CnLineDashSegLenF"
#define NhlCcnLineLabelPosition		"CnLineLabelPosition"
#define NhlCcnLineLabelAngleF		"CnLineLabelAngleF"

#define NhlCcnLowUseHighLabelRes	"CnLowUseHighLabelRes"
#define NhlCcnHighUseLineLabelRes	"CnHighUseLineLabelRes"
#define NhlCcnLineUseInfoLabelRes	"CnLineUseInfoLabelRes"

#define NhlCcnLineLabelsOn		"CnLineLabelsOn"
#define NhlCcnLineLabelTextHeightF	"CnLineLabelTextHeightF"
#define NhlCcnLineLabelFont		"CnLineLabelFont"
#define NhlCcnLineLabelFontAspectF	"CnLineLabelFontAspectF"
#define NhlCcnLineLabelFontThicknessF	"CnLineLabelFontThicknessF"
#define NhlCcnLineLabelFontQuality	"CnLineLabelFontQuality"
#define NhlCcnLineLabelConstantSpacingF	"CnLineLabelConstantSpacingF"
#define NhlCcnLineLabelFuncCode		"CnLineLabelFuncCode"
#define NhlCcnLineLabelBackgroundColor	"CnLineLabelBackgroundColor"
#define NhlCcnLineLabelPerim		"CnLineLabelPerim"
#define NhlCcnLineLabelPerimSpaceF	"CnLineLabelPerimSpaceF"
#define NhlCcnLineLabelPerimThicknessF	"CnLineLabelPerimThicknessF"
#define NhlCcnLineLabelPerimColor	"CnLineLabelPerimColor"

#define NhlCcnInfoLabelOn		"CnInfoLabelOn"
#define NhlCcnInfoLabelTextHeightF	"CnInfoLabelTextHeightF"
#define NhlCcnInfoLabelFont		"CnInfoLabelFont"
#define NhlCcnInfoLabelFontColor	"CnInfoLabelFontColor"
#define NhlCcnInfoLabelFontAspectF	"CnInfoLabelFontAspectF"
#define NhlCcnInfoLabelFontThicknessF	"CnInfoLabelFontThicknessF"
#define NhlCcnInfoLabelFontQuality	"CnInfoLabelFontQuality"
#define NhlCcnInfoLabelConstantSpacingF	"CnInfoLabelConstantSpacingF"
#define NhlCcnInfoLabelFuncCode		"CnInfoLabelFuncCode"
#define NhlCcnInfoLabelBackgroundColor	"CnInfoLabelBackgroundColor"
#define NhlCcnInfoLabelPerim		"CnInfoLabelPerim"
#define NhlCcnInfoLabelPerimSpaceF	"CnInfoLabelPerimSpaceF"
#define NhlCcnInfoLabelPerimThicknessF	"CnInfoLabelPerimThicknessF"
#define NhlCcnInfoLabelPerimColor	"CnInfoLabelPerimColor"

#define NhlCcnHighLabelsOn		"CnHighLabelsOn"
#define NhlCcnHighLabelTextHeightF	"CnHighLabelTextHeightF"
#define NhlCcnHighLabelFont		"CnHighLabelFont"
#define NhlCcnHighLabelFontColor	"CnHighLabelFontColor"
#define NhlCcnHighLabelFontAspectF	"CnHighLabelFontAspectF"
#define NhlCcnHighLabelFontThicknessF	"CnHighLabelFontThicknessF"
#define NhlCcnHighLabelFontQuality	"CnHighLabelFontQuality"
#define NhlCcnHighLabelConstantSpacingF	"CnHighLabelConstantSpacingF"
#define NhlCcnHighLabelFuncCode		"CnHighLabelFuncCode"
#define NhlCcnHighLabelBackgroundColor	"CnHighLabelBackgroundColor"
#define NhlCcnHighLabelPerim		"CnHighLabelPerim"
#define NhlCcnHighLabelPerimSpaceF	"CnHighLabelPerimSpaceF"
#define NhlCcnHighLabelPerimThicknessF	"CnHighLabelPerimThicknessF"
#define NhlCcnHighLabelPerimColor	"CnHighLabelPerimColor"

#define NhlCcnLowLabelsOn		"CnLowLabelsOn"
#define NhlCcnLowLabelTextHeightF	"CnLowLabelTextHeightF"
#define NhlCcnLowLabelFont		"CnLowLabelFont"
#define NhlCcnLowLabelFontColor		"CnLowLabelFontColor"
#define NhlCcnLowLabelFontAspectF	"CnLowLabelFontAspectF"
#define NhlCcnLowLabelFontThicknessF	"CnLowLabelFontThicknessF"
#define NhlCcnLowLabelFontQuality	"CnLowLabelFontQuality"
#define NhlCcnLowLabelConstantSpacingF	"CnLowLabelConstantSpacingF"
#define NhlCcnLowLabelFuncCode		"CnLowLabelFuncCode"
#define NhlCcnLowLabelBackgroundColor	"CnLowLabelBackgroundColor"
#define NhlCcnLowLabelPerim		"CnLowLabelPerim"
#define NhlCcnLowLabelPerimSpaceF	"CnLowLabelPerimSpaceF"
#define NhlCcnLowLabelPerimThicknessF	"CnLowLabelPerimThicknessF"
#define NhlCcnLowLabelPerimColor	"CnLowLabelPerimColor"

extern NhlLayerClass			NhlcontourLayerClass;

#endif /*_NContour_h */
