/*
 *      $Id: Overlay.h,v 1.14 1995-03-13 21:47:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Overlay.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for Overlay class.
 */

#ifndef _NOverlay_h
#define _NOverlay_h

#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/Legend.h>
#include <ncarg/hlu/Annotation.h>

typedef enum _NhlAnnotationDisplayMode {
	NhlNOCREATE = -1,NhlNEVER = 0, NhlALWAYS = 1, NhlCONDITIONAL
} NhlAnnotationDisplayMode;

#define NhlTAnnotationDisplayMode        "annotationdisplaymode"

/*
 * Overlay instance resources
 */

#define NhlNovOverlayIds	".ovOverlayIds"
#define NhlNovPreDrawOrder	".ovPreDrawOrder"
#define NhlNovPostDrawOrder	".ovPostDrawOrder"

#define NhlNovAnnoViews		"ovAnnoViews"
#define NhlNovAnnotations	">ovAnnotations"
#define NhlNovFitToBB		"ovFitToBB"
#define NhlNovBBLeftF		"ovBBLeftF"
#define NhlNovBBRightF		"ovBBRightF"
#define NhlNovBBBottomF		"ovBBBottomF"
#define NhlNovBBTopF		"ovBBTopF"

#define NhlNovTitleDisplayMode	"ovTitleDisplayMode"
#define NhlNovTitleZone		"ovTitleZone"
#define NhlNovTickMarkDisplayMode	"ovTickMarkDisplayMode"
#define NhlNovTickMarkZone	"ovTickMarkZone"
#define NhlNovLabelBarDisplayMode	"ovLabelBarDisplayMode"
#define NhlNovLabelBarZone	"ovLabelBarZone"
#define NhlNovLegendDisplayMode	"ovLegendDisplayMode"
#define NhlNovLegendZone	"ovLegendZone"

#define NhlNovLabelBarWidthF	"ovLabelBarWidthF"
#define NhlNovLabelBarHeightF	"ovLabelBarHeightF"
#define NhlNovLabelBarSide	"ovLabelBarSide"
#define NhlNovLabelBarParallelPosF	"ovLabelBarParallelPosF"
#define NhlNovLabelBarOrthogonalPosF	"ovLabelBarOrthogonalPosF"

#define NhlNovLabelBarXOffsetF	"ovLabelBarXOffset"
#define NhlNovLabelBarYOffsetF	"ovLabelBarYOffset"
#define NhlNovLabelBarPosition	"ovLabelBarPosition"
#define NhlNovLegendWidthF	"ovLegendWidthF"
#define NhlNovLegendHeightF	"ovLegendHeightF"
#define NhlNovLegendSide	"ovLegendSide"
#define NhlNovLegendParallelPosF	"ovLegendParallelPosF"
#define NhlNovLegendOrthogonalPosF	"ovLegendOrthogonalPosF"

#define NhlNovLegendXOffsetF	"ovLegendXOffset"
#define NhlNovLegendYOffsetF	"ovLegendYOffset"
#define NhlNovLegendPosition	"ovLegendPosition"

/*
 * Overlay class resources
 */

#define NhlCovOverlayIds	".OvOverlayIds"
#define NhlCovPreDrawOrder	".OvPreDrawOrder"
#define NhlCovPostDrawOrder	".OvPostDrawOrder"

#define NhlCovAnnoViews		"OvAnnoViews"
#define NhlCovAnnotations	".OvAnnotations"
#define NhlCovFitToBB		"OvFitToBB"
#define NhlCovBBLeftF		"OvBBLeftF"
#define NhlCovBBRightF		"OvBBRightF"
#define NhlCovBBBottomF		"OvBBBottomF"
#define NhlCovBBTopF		"OvBBTopF"

#define NhlCovTitleDisplayMode	"OvTitleDisplayMode"
#define NhlCovTitleZone		"OvTitleZone"
#define NhlCovTickMarkDisplayMode	"OvTickMarkDisplayMode"
#define NhlCovTickMarkZone	"OvTickMarkZone"
#define NhlCovLabelBarDisplayMode	"OvLabelBarDisplayMode"
#define NhlCovLabelBarZone	"OvLabelBarZone"
#define NhlCovLegendDisplayMode	"OvLegendDisplayMode"
#define NhlCovLegendZone	"OvLegendZone"
#define NhlCovLabelBarWidthF	"OvLabelBarWidth"
#define NhlCovLabelBarHeightF	"OvLabelBarHeight"
#define NhlCovLabelBarSide	"OvLabelBarSide"
#define NhlCovLabelBarParallelPosF	"OvLabelBarParallelPosF"
#define NhlCovLabelBarOrthogonalPosF	"OvLabelBarOrthogonalPosF"

#define NhlCovLabelBarXOffsetF	"OvLabelBarXOffset"
#define NhlCovLabelBarYOffsetF	"OvLabelBarYOffset"
#define NhlCovLabelBarPosition	"OvLabelBarPosition"

#define NhlCovLegendWidthF	"OvLegendWidth"
#define NhlCovLegendHeightF	"OvLegendHeight"
#define NhlCovLegendSide	"OvLegendSide"
#define NhlCovLegendParallelPosF	"OvLegendParallelPosF"
#define NhlCovLegendOrthogonalPosF	"OvLegendOrthogonalPosF"

#define NhlCovLegendXOffsetF	"OvLegendXOffset"
#define NhlCovLegendYOffsetF	"OvLegendYOffset"
#define NhlCovLegendPosition	"OvLegendPosition"

extern NhlLayerClass NhloverlayLayerClass;

/* Public functions defined by the Overlay Class	*/
extern NhlErrorTypes NhlAddToOverlay(
#if	NhlNeedProto
        int		base_id,
	int		plot_id,
	int		after_id
#endif
);

extern NhlErrorTypes NhlRemoveFromOverlay(
#if	NhlNeedProto
        int		base_id,
	int		plot_id,
	NhlBoolean	restore
#endif
);

extern int NhlAddAnnotation(
#if	NhlNeedProto
        int	overlay_base_id,
	int	anno_view_id
#endif
);

extern NhlErrorTypes NhlRemoveAnnotation(
#if	NhlNeedProto
        int	overlay_base_id,
	int	anno_id
#endif
);

int NhlGetAnnotationId(
#if	NhlNeedProto
        int	overlay_base_id,
	int	anno_view_id
#endif
);


#endif /*_NOverlay_h */
