/*
 *      $Id: Workstation.h,v 1.39.2.1 2010-03-17 20:47:07 brownrig Exp $
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
 *			of the workstation, a color map, the workstation ID,
 *			and the workstation type.
 */


#ifndef _NWorkstation_h
#define _NWorkstation_h

#include <ncarg/hlu/Base.h>
#include <ncarg/hlu/GraphicStyle.h>

#define NhlwkMAX_COLORS	(256)

/*
 * Public Workstation Resources are defined here.
 */

#define NhlNwkColorMap		"wkColorMap"
#define NhlCwkColorMap		"WkColorMap"
#define NhlNwkColorMapLen	"wkColorMapLen"
#define NhlCwkColorMapLen	"WkColorMapLen"
#define NhlNwkBackgroundOpacityF "wkBackgroundOpacityF"
#define NhlCBackgroundOpacityF	"BackgroundOpacityF"
#define NhlNwkBackgroundColor	"wkBackgroundColor"
#define NhlCBackgroundColor	"BackgroundColor"
#define NhlNwkForegroundColor	"wkForegroundColor"
#define NhlCForegroundColor	"ForegroundColor"
#define NhlNwkVSWidthDevUnits	"wkVSWidthDevUnits"
#define NhlCwkVSWidthDevUnits	"WkVSWidthDevUnits"
#define NhlNwkViews		"wkViews"
#define NhlCwkViews		"WkViews"
#define NhlNwkTopLevelViews	"wkTopLevelViews"
#define NhlCwkTopLevelViews	"WkTopLevelViews"
#define NhlNwkAntiAlias         "wkAntiAlias"
#define NhlCwkAntiAlias 	"WkAntiAlias"

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
#define NhlNwkLineOpacityF	"wkLineOpacityF"
#define NhlCwkLineOpacityF	"WkLineOpacityF"
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
#define NhlNwkMarkerOpacityF    "wkMarkerOpacityF"
#define NhlCwkMarkerOpacityF    "WkMarkerOpacityF"
#define NhlNwkMarkerSizeF	"wkMarkerSizeF"
#define NhlCwkMarkerSizeF	"WkMarkerSizeF"
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

/* Dash pattern stuff */

typedef	int				NhlDashIndexFullEnum;
#define	NhlTDashIndexFullEnum		"DashIndexFullEnum"
#define	NhlTDashIndexFullEnumGenArray	"DashIndexFullEnumGenArray"
#define NhlUNSPECIFIEDLINE		-1

typedef	int			NhlDashIndex;
#define	NhlTDashIndex		"DashIndex"
#define	NhlTDashIndexGenArray	"DashIndexGenArray"
#define NhlSOLIDLINE		0


/* Color stuff */

typedef	int				NhlColorIndexFullEnum;
#define	NhlTColorIndexFullEnum		"ColorIndexFullEnum"
#define	NhlTColorIndexFullEnumGenArray	"ColorIndexFullEnumGenArray"
#define NhlUNSPECIFIEDCOLOR		-2

typedef	int	NhlColorIndex;
#define	NhlTColorIndex	"ColorIndex"
#define	NhlTColorIndexGenArray	"ColorIndexGenArray"
#define NhlBACKGROUND 0
#define NhlFOREGROUND 1
#define NhlTRANSPARENT -1
#define NhlNULLCOLOR   -1

#define NhlTColorDefinitionGenArray	"ColorDefinitionGenArray"

typedef float NhlColor[3];

/* Fill stuff */

typedef	int				NhlFillIndexFullEnum;
#define	NhlTFillIndexFullEnum		"FillIndexFullEnum"
#define	NhlTFillIndexFullEnumGenArray	"FillIndexFullEnumGenArray"
#define NhlUNSPECIFIEDFILL		-2

typedef	int	NhlFillIndex;
#define	NhlTFillIndex	"FillIndex"
#define	NhlTFillIndexGenArray	"FillIndexGenArray"
#define NhlHOLLOWFILL	-1
#define NhlNULLFILL	-1
#define NhlSOLIDFILL	0
#define NhlWK_INITIAL_FILL_BUFSIZE 128


/* Workstation marker stuff */

typedef	int	NhlMarkerIndex;
#define	NhlTMarkerIndex	"MarkerIndex"
#define	NhlTMarkerIndexGenArray	"MarkerIndexGenArray"
#define NhlWK_DEF_MARKER	3


typedef float NhlMarkerTableParams[4];

#define	NhlTMarkLineMode		"MarkLineMode"
#define	NhlTMarkLineModeGenArray	"MarkLineModeGenArray"

typedef enum _NhlMarkLineMode{
	NhlLINES = 0,
	NhlMARKERS = 1,
	NhlMARKLINES = 2
} NhlMarkLineMode;


/* font/text stuff */
typedef enum {NhlHIGH,NhlMEDIUM,NhlLOW,NhlWORKSTATION} NhlFontQuality;
typedef enum {NhlDOWN,NhlACROSS} NhlTextDirection;

#define NhlTFontQuality		"FontQuality"
#define NhlTTextDirection	"TextDirection"

/* antialiasing control */
#define NhlTAntiAlias         "AntiAlias"
typedef enum {
    NhlANTIALIAS_OFF, 
    NhlANTIALIAS_ON,
    NhlANTIALIAS_TEXTONLY
} NhlwkAntiAlias;

typedef enum {
    NhlTEXT_ANTIALIAS_MODE,
    NhlNON_TEXT_ANTIALIAS_MODE
} NhlAntiAliasMode;

/*
 * Common/shared resources for PostScript (PS) and Portable
 * Document Format (PDF) workstation types.
 */
#define    NhlTVisualType  "VisualType"
typedef enum _NhlVisualType {
    NhlCOLOR = 0,
    NhlMONOCHROME = 3
} NhlVisualType;

#define    NhlTWorkOrientation     "WorkOrientation"
typedef enum _NhlWorkOrientation {
    NhlPORTRAIT = 0,
    NhlLANDSCAPE = 6
} NhlWorkOrientation;

#define    NhlTColorModel      "ColorModel"
typedef enum _NhlColorModel {
    NhlCMYK = 0,
    NhlRGB  = 1
} NhlColorModel;

#define	NhlNwkVisualType	"wkVisualType"
#define	NhlCwkVisualType	"WkVisualType"

#define	NhlNwkOrientation	"wkOrientation"
#define	NhlCwkOrientation	"WkOrientation"

#define	NhlNwkFullBackground	"wkFullBackground"
#define	NhlCwkFullBackground	"WkFullBackground"

#define	NhlNwkDeviceLowerX	"wkDeviceLowerX"
#define	NhlCwkDeviceLowerX	"WkDeviceLowerX"

#define	NhlNwkDeviceLowerY	"wkDeviceLowerY"
#define	NhlCwkDeviceLowerY	"WkDeviceLowerY"

#define	NhlNwkDeviceUpperX	"wkDeviceUpperX"
#define	NhlCwkDeviceUpperX	"WkDeviceUpperX"

#define	NhlNwkDeviceUpperY	"wkDeviceUpperY"
#define	NhlCwkDeviceUpperY	"WkDeviceUpperY"

#define	NhlNwkColorModel	"wkColorModel"
#define	NhlCwkColorModel	"WkColorModel"

#define	NhlNwkSuppressBackground	"wkSuppressBackground"
#define	NhlCwkSuppressBackground	"WkSuppressBackground"

#define	NhlNwkSuppressBBInfo	"wkSuppressBBInfo"
#define	NhlCwkSuppressBBInfo	"WkSuppressBBInfo"

/* Resources shared by the PS, PDF, and cairo-based document workstations */
#define NhlNwkPaperWidthF   "wkPaperWidthF"
#define NhlCwkPaperWidthF   "WkPaperWidthF"

#define NhlNwkPaperHeightF  "wkPaperHeightF"
#define NhlCwkPaperHeightF  "WkPaperHeightF"

#define NhlNwkPaperSize     "wkPaperSize"
#define NhlCwkPaperSize     "WkPaperSize"

/* Resources shared by the XWorkstation and Cairo-based ImageWorkstation */
#define NhlNwkWidth         "wkWidth"
#define NhlCwkWidth         "WkWidth"

#define NhlNwkHeight        "wkHeight"
#define NhlCwkHeight        "WkHeight"

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
NhlColorIndex     /* ci */,
float   /* red */,
float   /* green */,
float   /* blue */
#endif
);

extern NhlErrorTypes NhlFreeColor(
#if	NhlNeedProto
        int 	/* pid */,
        NhlColorIndex     /* ci */
#endif
);

/*
 * Returns the actual hlu ci used - values less then 0 indicate
 * an error.
 */
extern NhlColorIndex NhlGetColor(
#if	NhlNeedProto
        int		pid,
	NhlColorIndex	ci,
        float		*red,
        float		*green,
        float		*blue
#endif
);

extern NhlColorIndex NhlNewColor(
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
        NhlColorIndex     /* ci   */
#endif
);

extern int NhlGetNamedColorIndex(
#if	NhlNeedProto
	int		pid,
	Const char	*name
#endif
);

extern NhlBoolean NhlIsAllocatedColor(
#if	NhlNeedProto
        int     /* pid */,
        NhlColorIndex     /* ci   */
#endif
);

extern int NhlNewMarker(
#if	NhlNeedProto
	int instance,
	char *marker_string,
	int  font,
	float x_off,
	float y_off,
	float aspect_adj,
	float size_adj,
	float angle
#endif
);

extern NhlErrorTypes NhlSetMarker(
#if	NhlNeedProto
	int instance,
	int	index,
	char	*marker_string,
	int     font,
	float	x_off,
	float	y_off,
	float	aspect_adj,
	float	size_adj,
	float   angle
#endif
);

extern int NhlNewDashPattern(
#if  NhlNeedProto
	int	wid,
	char	*dash_string
#endif
);

extern NhlErrorTypes NhlSetDashPattern(
#if	NhlNeedProto
	int instance,
	int	index,
	char	*dash_string
#endif
);

extern NhlBoolean NhlIsWorkstation(
#if	NhlNeedProto
	int	pid
#endif
);

extern NhlClass NhlworkstationClass;

/*
 * Palette declarations
 */
/*
 * New Types
 */

#define	NhlTColorMap	"ColorMap"


/*
 * Global API
 */

extern int NhlPalGetDefined(
#if	NhlNeedProto
	NhlClass	wc,
	NhlString	**names
#endif
);

extern NhlErrorTypes NhlPalGetColormap(
#if	NhlNeedProto
	NhlClass	wc,
	NhlString	name,
	NhlColor	**map,
	int		*len
#endif
);

extern NhlErrorTypes NhlPalSetColormap(
#if	NhlNeedProto
	NhlClass	wc,
	NhlString	name,
	NhlColor	*map,
	int		len
#endif
);

extern NhlErrorTypes NhlPalLoadColormapFiles(
#if	NhlNeedProto
	NhlClass	lc,
	NhlBoolean	suppress_path_message
#endif
);

#endif	/* _NWorkstation_h */
