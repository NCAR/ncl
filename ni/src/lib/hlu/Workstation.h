
/*
 *      $Id: Workstation.h,v 1.2 1993-10-19 17:53:18 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Workstation.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 9 09:51:36 MDT 1992
 *
 *	Description:	Main workstation class from which all GKS workstations
 *			are subclassed. This main class manages the children
 *			of the workstation, a color map, the workstation ID, and
 *			the workstation type.
 */


#ifndef _NWorkstation_h
#define _NWorkstation_h

#include <ncarg/hlu/Base.h>

#define NhlWK_INIT_FILL_TABLE_LEN	16
#define NhlWK_INIT_DASH_TABLE_LEN	16
#define NhlWK_INIT_MARKER_TABLE_LEN	16
#define NhlWK_DEF_MARKER		3

#define NhlNwkColorMap		"wkColorMap"
#define NhlCwkColorMap		"WkColorMap"
#define NhlNwkColorMapLen	"wkColorMapLen"
#define NhlCwkColorMapLen	"WkColorMapLen"
#define NhlNwkBkgndColor	"wkBkgndColor"
#define NhlCwkBkgndColor	"WkBkgndColor"
#define NhlNwkDashPattern       "wkDashPattern"
#define NhlNwkLineLabel         "wkLineLabel"
#define NhlNwkLineThicknessF    "wkLineThicknessF"
#define NhlNwkLineLabelFontHeightF      "wkLineLabelFontHeightF"
#define NhlNwkLineDashSegLenF   "wkLineDashSegLenF"
#define NhlNwkLineColor         "wkLineColor"
#define NhlNwkDashTableLength	".wkDashTableLength" /* read-only */

#define NhlCwkDashPattern       "WkDashPattern"
#define NhlCwkLineLabel         "WkLineLabel"
#define NhlCwkLineThicknessF    "WkLineThicknessF"
#define NhlCwkLineLabelFontHeightF      "WkLineLabelFontHeightF"
#define NhlCwkLineDashSegLenF   "WkLineDashSegLenF"
#define NhlCwkLineColor         "WkLineColor"
#define NhlCwkDashTableLength	".WkDashTableLength" /* read-only */

#define NhlNwkFillIndex		"wkFillIndex"
#define NhlNwkFillColor         "wkFillColor"
#define NhlNwkFillBackground    "wkFillBackground"
#define NhlNwkFillScaleFactorF	"wkFillScaleFactorF"
#define NhlNwkFillLineThicknessF "wkFillLineThicknessF"
#define NhlNwkFillTableLength	".wkFillTableLength" /* read-only */

#define NhlNwkDrawEdges		"wkDrawEdges"
#define NhlNwkEdgeDashPattern   "wkEdgeDashPattern"
#define NhlNwkEdgeThicknessF    "wkEdgeThicknessF"
#define NhlNwkEdgeDashSegLenF   "wkEdgeDashSegLenF"
#define NhlNwkEdgeColor         "wkEdgeColor"

#define NhlCwkFillIndex		"WkFillIndex"
#define NhlCwkFillColor         "WkFillColor"
#define NhlCwkFillBackground    "WkFillBackground"
#define NhlCwkFillScaleFactorF	"WkFillScaleFactorF"
#define NhlCwkFillLineThicknessF "WkFillLineThicknessF"
#define NhlCwkFillTableLength	".WkFillTableLength" /* read-only */

#define NhlCwkDrawEdges		"WkDrawEdges"
#define NhlCwkEdgeDashPattern   "WkEdgeDashPattern"
#define NhlCwkEdgeThicknessF    "WkEdgeThicknessF"
#define NhlCwkEdgeDashSegLenF   "WkEdgeDashSegLenF"
#define NhlCwkEdgeColor         "WkEdgeColor"

#define NhlNwkMarkerTableLength	".wkMarkerTableLength" /* read-only */
#define NhlNwkMarkerTableStrings ".wkMarkerTableStrings" /* read-only */
#define NhlNwkMarkerTableParams  ".wkMarkerTableParams"  /* read-only */
#define NhlNwkMarkerIndex	"wkMarkerIndex"
#define NhlNwkMarkerString	"wkMarkerString"
#define NhlNwkMarkerColor       "wkMarkerColor"
#define NhlNwkMarkerSizeF	"wkMarkerSizeF"
#define NhlNwkMarkerXOffsetF    "wkMarkerXOffset"
#define NhlNwkMarkerYOffsetF    "wkMarkerYOffset"
#define NhlNwkMarkerThicknessF  "wkMarkerThicknessF"

#define NhlNwkDrawMarkerLines		"wkDrawMarkerLines"
#define NhlNwkMarkerLineDashPattern	"wkMarkerLineDashPattern"
#define NhlNwkMarkerLineThicknessF	"wkMarkerLineThicknessF"
#define NhlNwkMarkerLineDashSegLenF	"wkMarkerLineDashSegLenF"
#define NhlNwkMarkerLineColor     	"wkMarkerLineColor"

#define NhlCwkMarkerTableLength	".WkMarkerTableLength" /* read-only */
#define NhlCwkMarkerTableStrings ".WkMarkerTableStrings" /* read-only */
#define NhlCwkMarkerTableParams  ".WkMarkerTableParams"  /* read-only */
#define NhlCwkMarkerIndex	"WkMarkerIndex"
#define NhlCwkMarkerString	"WkMarkerString"
#define NhlCwkMarkerColor       "WkMarkerColor"
#define NhlCwkMarkerSizeF	"WkMarkerSizeF"
#define NhlCwkMarkerXOffsetF    "WkMarkerXOffset"
#define NhlCwkMarkerYOffsetF    "WkMarkerYOffset"
#define NhlCwkMarkerThicknessF  "WkMarkerThicknessF"

#define NhlCwkDrawMarkerLines		"WkDrawMarkerLines"
#define NhlCwkMarkerLineDashPattern	"WkMarkerLineDashPattern"
#define NhlCwkMarkerLineThicknessF	"WkMarkerLineThicknessF"
#define NhlCwkMarkerLineDashSegLenF	"WkMarkerLineDashSegLenF"
#define NhlCwkMarkerLineColor     	"WkMarkerLineColor"

typedef struct _NhlColor {
	float	red;
	float	green;
	float	blue;
} NhlColor;

#define NhlTColorPtr "colorptr"
#define NhlTColor    "color"

/* Workstation Fill stuff */

#define NhlWK_INITIAL_FILL_BUFSIZE 128

typedef struct _NhlFillSpec {
	int angle;
	float spacing;
	int dots_on;
	int *dots_p;
	int glyph;
	int type;
	int ici;
} NhlFillSpec;

typedef struct _NhlMarkerSpec {
	char *marker;
	float x_off;
	float y_off;
	float aspect_adj;
	float size_adj;
	NhlBoolean dynamic;
} NhlMarkerSpec;

typedef NhlMarkerSpec ** NhlMarkerTable;

/* 
 * The marker table params contains all floats, and therefore can
 * be converted into a 2-d Fortran array. It is used only in the 
 * WorkstationGetValues function.
 */

typedef struct _NhlMarkerTableParams {
	float x_off;
	float y_off;
	float aspect_adj;
	float size_adj;
} NhlMarkerTableParams;

#define NhlTMarkerTableParamsPtr "marker_table_params_ptr"
#define NhlTMarkerTableParams    "marker_table_params"

extern LayerClass workstationLayerClass;

typedef struct _WorkstationLayerRec *WorkstationLayer;
typedef struct _WorkstationLayerClassRec *WorkstationLayerClass;

#endif	/* _NWorkstation_h */
