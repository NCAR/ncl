/*
 *      $Id: WorkstationI.h,v 1.4 1995-02-19 08:19:21 boote Exp $
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
#include <ncarg/hlu/Workstation.h>

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

#define _NhlNwkDashPattern	".wkDashPattern"
#define _NhlCwkDashPattern	".WkDashPattern"
#define _NhlNwkLineDashSegLenF	".wkLineDashSegLenF"
#define _NhlCwkLineDashSegLenF	".WkLineDashSegLenF"
#define _NhlNwkLineColor	".wkLineColor"
#define _NhlCwkLineColor	".WkLineColor"
#define _NhlNwkLineThicknessF	".wkLineThicknessF"
#define _NhlCwkLineThicknessF	".WkLineThicknessF"

#define _NhlNwkLineLabel	".wkLineLabel"
#define _NhlCwkLineLabel	".WkLineLabel"
#define _NhlNwkLineLabelFont	".wkLineLabelFont"
#define _NhlCwkLineLabelFont	".WkLineLabelFont"
#define _NhlNwkLineLabelColor	".wkLineLabelColor"
#define _NhlCwkLineLabelColor	".WkLineLabelColor"
#define _NhlNwkLineLabelFontHeightF	".wkLineLabelFontHeightF"
#define _NhlCwkLineLabelFontHeightF	".WkLineLabelFontHeightF"

#define _NhlNwkMarkerIndex	".wkMarkerIndex"
#define _NhlCwkMarkerIndex	".WkMarkerIndex"
#define _NhlNwkMarkerColor      ".wkMarkerColor"
#define _NhlCwkMarkerColor      ".WkMarkerColor"
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

#define _NhlNwkFillBackground    ".wkFillBackground"
#define _NhlCwkFillBackground    ".WkFillBackground"

#define _NhlNwkFillScaleFactorF	".wkFillScaleFactorF"
#define _NhlCwkFillScaleFactorF	".WkFillScaleFactorF"

#define _NhlNwkFillLineThicknessF ".wkFillLineThicknessF"
#define _NhlCwkFillLineThicknessF ".WkFillLineThicknessF"

#define _NhlNwkDrawEdges		".wkDrawEdges"
#define _NhlCwkDrawEdges		".WkDrawEdges"

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

extern int _NhlIsAllocatedColor(
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

extern int _NhlNewColor(
#if	NhlNeedProto
        NhlLayer   /* inst */,
        float   /* red */,
        float   /* green */,
        float   /* blue */
#endif
);

#endif	/* _NWORKSTATIONI_H */
