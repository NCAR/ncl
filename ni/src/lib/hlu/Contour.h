/*
 *      $Id: Contour.h,v 1.6 1994-03-18 02:18:05 dbrown Exp $
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
#define NhlNcnLineLabelTextHeightF	"cnLineLabelTextHeightF"
#define NhlNcnLineLabelPosition		"cnLineLabelPosition"
#define NhlNcnLineLabelAngleF		"cnLineLabelAngleF"
#define NhlNcnLineLabelBackgroundColor	"cnLineLabelBackgroundColor"
#define NhlNcnLineLabelPerim		"cnLineLabelPerim"
#define NhlNcnLineLabelPerimColor	"cnLineLabelPerimColor"

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
#define NhlCcnLineLabelTextHeightF	"CnLineLabelTextHeightF"
#define NhlCcnLineLabelPosition		"CnLineLabelPosition"
#define NhlCcnLineLabelAngleF		"CnLineLabelAngleF"
#define NhlCcnLineLabelBackgroundColor	"CnLineLabelBackgroundColor"
#define NhlCcnLineLabelPerim		"CnLineLabelPerim"
#define NhlCcnLineLabelPerimColor	"CnLineLabelPerimColor"

extern NhlLayerClass			NhlcontourLayerClass;

#endif /*_NContour_h */
