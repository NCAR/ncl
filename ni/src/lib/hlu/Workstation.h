/*
 *      $Id: Workstation.h,v 1.19 1996-03-16 21:37:46 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Workstation.h
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
#include <ncarg/hlu/GraphicStyle.h>

/*
 * Public Workstation Resources are defined here.
 */

#define NhlNwkColorMap		"wkColorMap"
#define NhlCwkColorMap		"WkColorMap"
#define NhlNwkColorMapLen	"wkColorMapLen"
#define NhlCwkColorMapLen	"WkColorMapLen"
#define NhlNwkBackgroundColor	"wkBackgroundColor"
#define NhlCwkBackgroundColor	"WkBackgroundColor"
#define NhlNwkForegroundColor	"wkForegroundColor"
#define NhlCwkForegroundColor	"WkForegroundColor"

/*
 * Public resources for setting Line and Marker attributes for
 * Public Polyline interface.
 */
#define NhlNwkDashPattern	"wkDashPattern"
#define NhlCwkDashPattern	"WkDashPattern"
#define NhlNwkLineDashSegLenF	"wkLineDashSegLenF"
#define NhlCwkLineDashSegLenF	"WkLineDashSegLenF"
#define NhlNwkLineColor		"wkLineColor"
#define NhlCwkLineColor		"WkLineColor"
#define NhlNwkLineThicknessF	"wkLineThicknessF"
#define NhlCwkLineThicknessF	"WkLineThicknessF"

#define NhlNwkLineLabel		"wkLineLabel"
#define NhlCwkLineLabel		"WkLineLabel"
#define NhlNwkLineLabelFont	"wkLineLabelFont"
#define NhlCwkLineLabelFont	"WkLineLabelFont"
#define NhlNwkLineLabelFontColor	"wkLineLabelFontColor"
#define NhlCwkLineLabelFontColor	"WkLineLabelFontColor"
#define NhlNwkLineLabelFontHeightF	"wkLineLabelFontHeightF"
#define NhlCwkLineLabelFontHeightF	"WkLineLabelFontHeightF"
#define NhlNwkLineLabelFontAspectF	"wkLineLabelFontAspectF"
#define NhlCwkLineLabelFontAspectF	"WkLineLabelFontAspectF"
#define NhlNwkLineLabelFontThicknessF	"wkLineLabelFontThicknessF"
#define NhlCwkLineLabelFontThicknessF	"WkLineLabelFontThicknessF"
#define NhlNwkLineLabelFontQuality	"wkLineLabelFontQuality"
#define NhlCwkLineLabelFontQuality	"WkLineLabelFontQuality"
#define NhlNwkLineLabelConstantSpacingF	"wkLineLabelConstantSpacingF"
#define NhlCwkLineLabelConstantSpacingF	"WkLineLabelConstantSpacingF"
#define NhlNwkLineLabelFuncCode		"wkLineLabelFuncCode"
#define NhlCwkLineLabelFuncCode		"WkLineLabelFuncCode"

#define NhlNwkMarkerIndex	"wkMarkerIndex"
#define NhlCwkMarkerIndex	"WkMarkerIndex"
#define NhlNwkMarkerColor       "wkMarkerColor"
#define NhlCwkMarkerColor       "WkMarkerColor"
#define NhlNwkMarkerSizeF	"wkMarkerSizeF"
#define NhlCwkMarkerSizeF	"WkMarkerSizeF"
#define NhlNwkMarkerXOffsetF    "wkMarkerXOffset"
#define NhlCwkMarkerXOffsetF    "WkMarkerXOffset"
#define NhlNwkMarkerYOffsetF    "wkMarkerYOffset"
#define NhlCwkMarkerYOffsetF    "WkMarkerYOffset"
#define NhlNwkMarkerThicknessF  "wkMarkerThicknessF"
#define NhlCwkMarkerThicknessF  "WkMarkerThicknessF"

/*
 * GetValues ONLY resources
 */
#define NhlNwkDashTableLength	"wkDashTableLength"
#define NhlCwkDashTableLength	"WkDashTableLength"
#define NhlNwkFillTableLength	"wkFillTableLength"
#define NhlCwkFillTableLength	"WkFillTableLength"
#define NhlNwkMarkerTableLength	"wkMarkerTableLength"
#define NhlCwkMarkerTableLength	"WkMarkerTableLength"
#define NhlNwkGksWorkId		"wkGksWorkId"
#define NhlCwkGksWorkId		"WkGksWorkId"
#define NhlNwkDefGraphicStyleId	"wkDefGraphicStyleId"
#define NhlCwkDefGraphicStyleId	"WkDefGraphicStyleId"

/* Define for dash pattern index 0 */

typedef	int	NhlDashIndex;
#define	NhlTDashIndex	"DashIndex"
#define	NhlTDashIndexGenArray	"DashIndexGenArray"
#define NhlSOLIDLINE	0

/* Colormap stuff */
typedef	int	NhlColorIndex;
#define	NhlTColorIndex	"ColorIndex"
#define	NhlTColorIndexGenArray	"ColorIndexGenArray"
#define NhlBACKGROUND 0
#define NhlFOREGROUND 1
#define NhlTRANSPARENT -1
#define NhlNULLCOLOR   -1

typedef float NhlColor[3];

/* Workstation Fill stuff */

typedef	int	NhlFillIndex;
#define	NhlTFillIndex	"FillIndex"
#define	NhlTFillIndexGenArray	"FillIndexGenArray"
#define NhlHOLLOWFILL	-1
#define NhlNULLFILL	-1
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
#define	NhlTMarkerIndex	"MarkerIndex"
#define	NhlTMarkerIndexGenArray	"MarkerIndexGenArray"
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

#define	NhlTMarkLineMode		"MarkLineMode"
#define	NhlTMarkLineModeGenArray	"MarkLineModeGenArray"

typedef enum _NhlMarkLineMode{
	NhlLINES = 0,
	NhlMARKERS = 1,
	NhlMARKLINES = 2
} NhlMarkLineMode;

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

/*
 * Returns the actual hlu ci used - values less then 0 indicate
 * an error.
 */
extern int NhlGetColor(
#if	NhlNeedProto
        int		pid,
	int		ci,
        float		*red,
        float		*green,
        float		*blue
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

extern NhlBoolean NhlIsAllocatedColor(
#if	NhlNeedProto
        int     /* pid */,
        int     /* ci   */
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

extern NhlBoolean NhlIsWorkstation(
#if	NhlNeedProto
	int	pid
#endif
);

extern NhlClass NhlworkstationClass;

#endif	/* _NWorkstation_h */
