/*
 *      $Id: WorkstationI.h,v 1.15.12.1 2010-03-17 20:47:07 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		WorkstationI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jan 26 17:39:56 MST 1994
 *
 *	Description:	Used to declare "Private" access functions to the
 *			Workstation Class.
 */
#ifndef	_NWORKSTATIONI_H
#define	_NWORKSTATIONI_H

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/Workstation.h>

typedef struct _NhlworkColorChangeDataRec
			_NhlworkColorChangeDataRec, *_NhlworkColorChangeData;

struct _NhlworkColorChangeDataRec{
	int		pid;	/* wks layer id */
	NhlColorIndex	ci;
	float		red;
	float		green;
	float		blue;
};

/*
 * The "Index" version is to get changes on a "specific" color index.  Set
 * the selector.lngval = to the index of interest.
 *
 * The non-"Index" version is a callback that gets called for each and every
 * index that changes - so a complete colormap set will cause that CB function
 * to be called 256 times.
 *
 * cbdata.ptrval is _NhlworkColorChangeData for both of these
 */
/*
 * changed the non-"Index" version: now it is called ColorMapChange, and
 * is called *once* any time the colormap is altered in any way. 
 * Actually the indexed color map callback will be called 256 times for
 * a complete color map change. The callback user only sees calls that he
 * for indexes he has registered however.
 */
#define _NhlCBworkColorIndexChange	"CBworkColorIndexChange"
#define _NhlCBworkColorMapChange	"CBworkColorMapChange"

/*cbdata.ptrval is workstation NhlLayer */
#define	_NhlCBworkPreOpen	"CBworkPreOpen"

/*
 * Private Workstation Resources are defined here.
 * These resources are only to be used by other objects
 * in the HLU library, they are not part of the API.
 */

/*
 * This resource must be set by itself!!!
 */
#define	_NhlNwkReset		".wkReset"
#define	_NhlCwkReset		".WkReset"
/*
 * This resource must be set by itself!!!
 */
#define	_NhlNwkSetPublic		".wkSetPublic"
#define	_NhlCwkSetPublic		".WkSetPublic"

#define _NhlNwkGraphicStyle	".wkGraphicStyle"
#define _NhlCwkGraphicStyle	".WkGraphicStyle"

#define _NhlNwkDashPattern	".wkDashPattern"
#define _NhlCwkDashPattern	".WkDashPattern"
#define _NhlNwkLineDashSegLenF	".wkLineDashSegLenF"
#define _NhlCwkLineDashSegLenF	".WkLineDashSegLenF"
#define _NhlNwkLineColor	".wkLineColor"
#define _NhlCwkLineColor	".WkLineColor"
#define _NhlNwkLineOpacityF	".wkLineOpacityF"
#define _NhlCwkLineOpacityF	".WkLineOpacityF"
#define _NhlNwkLineThicknessF	".wkLineThicknessF"
#define _NhlCwkLineThicknessF	".WkLineThicknessF"

#define _NhlNwkLineLabel	".wkLineLabel"
#define _NhlCwkLineLabel	".WkLineLabel"
#define _NhlNwkLineLabelFont	".wkLineLabelFont"
#define _NhlCwkLineLabelFont	".WkLineLabelFont"
#define _NhlNwkLineLabelFontColor	".wkLineLabelFontColor"
#define _NhlCwkLineLabelFontColor	".WkLineLabelFontColor"
#define _NhlNwkLineLabelFontHeightF	".wkLineLabelFontHeightF"
#define _NhlCwkLineLabelFontHeightF	".WkLineLabelFontHeightF"
#define _NhlNwkLineLabelFontAspectF	".wkLineLabelFontAspectF"
#define _NhlCwkLineLabelFontAspectF	".WkLineLabelFontAspectF"
#define _NhlNwkLineLabelFontThicknessF	".wkLineLabelFontThicknessF"
#define _NhlCwkLineLabelFontThicknessF	".WkLineLabelFontThicknessF"
#define _NhlNwkLineLabelFontQuality	".wkLineLabelFontQuality"
#define _NhlCwkLineLabelFontQuality	".WkLineLabelFontQuality"
#define _NhlNwkLineLabelConstantSpacingF	".wkLineLabelConstantSpacingF"
#define _NhlCwkLineLabelConstantSpacingF	".WkLineLabelConstantSpacingF"
#define _NhlNwkLineLabelFuncCode	".wkLineLabelFuncCode"
#define _NhlCwkLineLabelFuncCode	".WkLineLabelFuncCode"

#define _NhlNwkMarkerIndex	".wkMarkerIndex"
#define _NhlCwkMarkerIndex	".WkMarkerIndex"
#define _NhlNwkMarkerColor      ".wkMarkerColor"
#define _NhlCwkMarkerColor      ".WkMarkerColor"
#define _NhlNwkMarkerOpacityF   ".wkMarkerOpacityF"
#define _NhlCwkMarkerOpacityF   ".WkMarkerOpacityF"
#define _NhlNwkMarkerSizeF	".wkMarkerSizeF"
#define _NhlCwkMarkerSizeF	".WkMarkerSizeF"
#define _NhlNwkMarkerXOffsetF   ".wkMarkerXOffset"
#define _NhlCwkMarkerXOffsetF   ".WkMarkerXOffset"
#define _NhlNwkMarkerYOffsetF   ".wkMarkerYOffset"
#define _NhlCwkMarkerYOffsetF   ".WkMarkerYOffset"
#define _NhlNwkMarkerThicknessF ".wkMarkerThicknessF"
#define _NhlCwkMarkerThicknessF ".WkMarkerThicknessF"

#define _NhlNwkDashTable 	".wkDashTable"
#define _NhlCwkDashTable 	".WkDashTable"

#define _NhlNwkFillIndex		".wkFillIndex"
#define _NhlCwkFillIndex		".WkFillIndex"

#define _NhlNwkFillColor         ".wkFillColor"
#define _NhlCwkFillColor         ".WkFillColor"

#define _NhlNwkFillOpacityF      ".wkFillOpacityF"
#define _NhlCwkFillOpacityF      ".WkFillOpacityF"

#define _NhlNwkFillBackground    ".wkFillBackground"
#define _NhlCwkFillBackground    ".WkFillBackground"

#define _NhlNwkFillScaleFactorF	".wkFillScaleFactorF"
#define _NhlCwkFillScaleFactorF	".WkFillScaleFactorF"

#define _NhlNwkFillLineThicknessF ".wkFillLineThicknessF"
#define _NhlCwkFillLineThicknessF ".WkFillLineThicknessF"

#define _NhlNwkFillDotSizeF 	  ".wkFillDotSizeF"
#define _NhlCwkFillDotSizeF 	  ".WkFillDotSizeF"

#define _NhlNwkEdgesOn		 ".wkEdgesOn"
#define _NhlCwkEdgesOn		 ".WkEdgesOn"

#define _NhlNwkEdgeDashPattern   ".wkEdgeDashPattern"
#define _NhlCwkEdgeDashPattern   ".WkEdgeDashPattern"

#define _NhlNwkEdgeThicknessF    ".wkEdgeThicknessF"
#define _NhlCwkEdgeThicknessF    ".WkEdgeThicknessF"

#define _NhlNwkEdgeDashSegLenF   ".wkEdgeDashSegLenF"
#define _NhlCwkEdgeDashSegLenF   ".WkEdgeDashSegLenF"

#define _NhlNwkEdgeColor         ".wkEdgeColor"
#define _NhlCwkEdgeColor         ".WkEdgeColor"

#define _NhlNwkMarkerTableStrings ".wkMarkerTableStrings" /* read-only */
#define _NhlCwkMarkerTableStrings ".WkMarkerTableStrings" /* read-only */

#define _NhlNwkMarkerTableParams  ".wkMarkerTableParams"  /* read-only */
#define _NhlCwkMarkerTableParams  ".WkMarkerTableParams"  /* read-only */

/*
 * Resources Disabled for 4.0
 */
#define _NhlNwkDrawMarkerLines		".wkDrawMarkerLines"
#define _NhlCwkDrawMarkerLines		".WkDrawMarkerLines"

#define _NhlNwkMarkerLineDashPattern	".wkMarkerLineDashPattern"
#define _NhlCwkMarkerLineDashPattern	".WkMarkerLineDashPattern"

#define _NhlNwkMarkerLineThicknessF	".wkMarkerLineThicknessF"
#define _NhlCwkMarkerLineThicknessF	".WkMarkerLineThicknessF"

#define _NhlNwkMarkerLineDashSegLenF	".wkMarkerLineDashSegLenF"
#define _NhlCwkMarkerLineDashSegLenF	".WkMarkerLineDashSegLenF"

#define _NhlNwkMarkerLineColor     	".wkMarkerLineColor"
#define _NhlCwkMarkerLineColor     	".WkMarkerLineColor"

typedef struct _NhlWorkstationLayerRec *NhlWorkstationLayer;
typedef struct _NhlWorkstationClassRec *NhlWorkstationClass;

/*
 * Private Functions to support Workstation Class Objects
 */
extern void _NhlSetLineInfo(
#if	NhlNeedProto
        NhlLayer   /* instance */,
        NhlLayer   /* plot */
#endif
);

extern NhlErrorTypes _NhlWorkstationLineTo(
#if	NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern void _NhlSetFillInfo(
#if	NhlNeedProto
NhlLayer instance,
NhlLayer plot
#endif
);

extern NhlErrorTypes _NhlWorkstationFill(
#if	NhlNeedProto
NhlLayer   /* instance */,
float*   /* x */,
float*   /* y */,
int     /* num_points */
#endif
);

extern NhlErrorTypes _NhlWorkstationCellFill(
#if	NhlNeedProto
NhlLayer	l,
float		xp1,
float		yp1,
float           xp2,
float           yp2,
int             nx,
int             ny,
int             *clrixs
#endif
);

extern void _NhlSetMarkerInfo(
#if	NhlNeedProto
NhlLayer instance,
NhlLayer plot
#endif
);

extern NhlErrorTypes _NhlWorkstationMarker(
#if	NhlNeedProto
NhlLayer   /* instance */,
float*   /* x */,
float*   /* y */,
int     /* num_points */
#endif
);

extern void _NhlSetAntialiasingMode(
NhlLayer l, 
NhlAntiAliasMode mode
);

extern NhlErrorTypes _NhlActivateWorkstation(
#if	NhlNeedProto
        NhlLayer   /* layer*/
#endif
);

extern NhlErrorTypes _NhlDeactivateWorkstation(
#if	NhlNeedProto
        NhlLayer   /* layer*/
#endif
);

extern NhlErrorTypes _NhlCloseWorkstation(
#if	NhlNeedProto
        NhlLayer   /*layer*/
#endif
);

extern NhlErrorTypes _NhlOpenWorkstation(
#if	NhlNeedProto
        NhlLayer   /*layer*/
#endif
);

extern  int  _NhlWorkstationId(
#if	NhlNeedProto
        NhlLayer   /*instance */
#endif
);

extern int _NhlGetGksCi(
#if	NhlNeedProto
        NhlLayer   /* workstation*/,
        int /* ci*/
#endif
);

extern NhlBoolean _NhlIsAllocatedColor(
#if	NhlNeedProto
        NhlLayer   /* workstation*/,
        int /* ci*/
#endif
);

extern NhlErrorTypes _NhlSetColor(
#if	NhlNeedProto
NhlLayer   /* inst */,
int     /* ci */,
float   /* red */,
float   /* green */,
float   /* blue */
#endif
);

extern NhlErrorTypes _NhlFreeColor(
#if	NhlNeedProto
        NhlLayer   /* inst */,
        int     /* ci */
#endif
);


extern int _NhlGetColor(
#if	NhlNeedProto
        NhlLayer	l,
	int		ci,
        float		*red,
        float		*green,
        float		*blue
#endif
);

extern int _NhlNewColor(
#if	NhlNeedProto
        NhlLayer   /* inst */,
        float   /* red */,
        float   /* green */,
        float   /* blue */
#endif
);


extern NhlErrorTypes _NhlSpanColorPalette(
	NhlGenArray palga, 
	int count, 
	int *colors
);

extern NhlErrorTypes _NhlSetColorsFromPalette(
	NhlLayer vl,
	NhlGenArray palette_ga, 
	int color_count, 
	int span_palette, 
	NhlGenArray *color_ga, 
	char *entry_name
);

extern NhlErrorTypes   _NhlSetColorsFromIndexAndPalette(
	NhlLayer	       vl,
	NhlGenArray            index_ga,
	NhlGenArray            palette_ga,
	char *                 entry_name
);

extern NhlErrorTypes   _NhlSetColorsFromWorkstationColorMap(
	NhlLayer	       vl,
	NhlGenArray            *index_ga,
	ng_size_t              count,
	int		       span_palette, 
	char *                 entry_name
);

extern NhlGenArray _NhlGetWorkstationPalette(
#if	NhlNeedProto
        NhlLayer  l
#endif
);

extern NhlLayer _NhlDefaultPlot(
#if	NhlNeedProto
        NhlLayer  l
#endif
);

extern NhlErrorTypes _NhlUpdateDrawBB(
#if	NhlNeedProto
	NhlLayer	vl
#endif
);

#endif	/* _NWORKSTATIONI_H */
