/*
 *      $Id: Workstation.h,v 1.11 1995-01-24 01:25:16 boote Exp $
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


#define NhlNwkColorMap		"wkColorMap"
#define NhlCwkColorMap		"WkColorMap"
#define NhlNwkColorMapLen	".wkColorMapLen"
#define NhlCwkColorMapLen	".WkColorMapLen"
#define NhlNwkBackgroundColor	"wkBackgroundColor"
#define NhlCwkBackgroundColor	"WkBackgroundColor"
#define NhlNwkForegroundColor	"wkForegroundColor"
#define NhlCwkForegroundColor	"WkForegroundColor"
#define NhlNwkDashPattern       "wkDashPattern"
#define NhlNwkLineLabel         "wkLineLabel"
#define NhlNwkLineLabelColor	"wLineLabelColor"
#define NhlNwkLineThicknessF    "wkLineThicknessF"
#define NhlNwkLineLabelFontHeightF      "wkLineLabelFontHeightF"
#define NhlNwkLineDashSegLenF   "wkLineDashSegLenF"
#define NhlNwkLineColor         "wkLineColor"
#define NhlNwkDashTableLength	".wkDashTableLength" /* read-only */
#define NhlNwkDashTable 	".wkDashTable" 		/* now read-only */

#define NhlCwkDashPattern       "WkDashPattern"
#define NhlCwkLineLabel         "WkLineLabel"
#define NhlCwkLineLabelColor	"WkLineLabelColor"
#define NhlCwkLineThicknessF    "WkLineThicknessF"
#define NhlCwkLineLabelFontHeightF      "WkLineLabelFontHeightF"
#define NhlCwkLineDashSegLenF   "WkLineDashSegLenF"
#define NhlCwkLineColor         "WkLineColor"
#define NhlCwkDashTableLength	".WkDashTableLength" /* read-only */
#define NhlCwkDashTable 	".WkDashTable" 		/* now read-only */

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
#define NhlNwkMarkerTableStrings "wkMarkerTableStrings" /* read-only */
#define NhlNwkMarkerTableParams  "wkMarkerTableParams"  /* read-only */
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
#define NhlCwkMarkerTableStrings "WkMarkerTableStrings" /* read-only */
#define NhlCwkMarkerTableParams  "WkMarkerTableParams"  /* read-only */
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

/* Define for dash pattern index 0 */

typedef	int	NhlDashIndex;
#define	NhlTDashIndex	"dashIndex"
#define	NhlTDashIndexGenArray	"dashIndexGenArray"
#define NhlSOLIDLINE	0

/* Colormap stuff */

typedef	int	NhlColorIndex;
#define	NhlTColorIndex	"colorIndex"
#define	NhlTColorIndexGenArray	"colorIndexGenArray"
#define NhlBACKGROUND 0
#define NhlFOREGROUND 1
#define NhlTRANSPARENT -1

typedef float NhlColor[3];

/* Workstation Fill stuff */

typedef	int	NhlFillIndex;
#define	NhlTFillIndex	"fillIndex"
#define	NhlTFillIndexGenArray	"fillIndexGenArray"
#define NhlHOLLOWFILL	-1
#define NhlSOLIDFILL	0
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

/* Workstation marker stuff */

typedef	int	NhlMarkerIndex;
#define	NhlTMarkerIndex	"markerIndex"
#define	NhlTMarkerIndexGenArray	"markerIndexGenArray"
#define NhlWK_DEF_MARKER	3

typedef struct _NhlMarkerSpec {
	char *marker;
	float x_off;
	float y_off;
	float aspect_adj;
	float size_adj;
	NhlBoolean dynamic;
} NhlMarkerSpec;

typedef NhlMarkerSpec **NhlMarkerTable;

typedef float NhlMarkerTableParams[4];

/*
 * Public access functions to support Workstation Class
 */

extern NhlErrorTypes NhlUpdateWorkstation(
#if	NhlNeedProto
	int	workid	/* workid of workstation to update	*/
#endif
);

extern NhlErrorTypes NhlClearWorkstation(
#if	NhlNeedProto
	int	workid	/* workid of workstation to clear	*/
#endif
);

extern NhlErrorTypes   NhlFrame(
#if	NhlNeedProto
	int /*wid*/
#endif
);


extern NhlErrorTypes NhlSetColor(
#if	NhlNeedProto
int 	/* pid */,
int     /* ci */,
float   /* red */,
float   /* green */,
float   /* blue */
#endif
);

extern NhlErrorTypes NhlFreeColor(
#if	NhlNeedProto
        int 	/* pid */,
        int     /* ci */
#endif
);

extern int NhlNewColor(
#if	NhlNeedProto
        int     /* pid*/,
        float   /* red */,
        float   /* green */,
        float   /* blue */
#endif
);

extern int NhlGetGksCi(
#if	NhlNeedProto
        int     /* pid */,
        int     /* ci   */
#endif
);

extern int NhlIsAllocatedColor(
#if	NhlNeedProto
        int     /* pid */,
        int     /* ci   */
#endif
);

extern int NhlGetGksWorkId(
#if	NhlNeedProto
int /* workid */
#endif
);

extern int NhlNewMarker(
#if	NhlNeedProto
	int instance, 
	char *marker_string, 
	float x_off, 
	float y_off,
	float aspect_adj,
	float size_adj
#endif
);

extern NhlErrorTypes NhlSetMarker(
#if	NhlNeedProto
	int instance, 
	int	index,
	char	*marker_string, 
	float	x_off, 
	float	y_off,
	float	aspect_adj,
	float	size_adj
#endif
);

extern NhlLayerClass NhlworkstationLayerClass;

#endif	/* _NWorkstation_h */
