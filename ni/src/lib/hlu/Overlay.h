/*
 *      $Id: Overlay.h,v 1.7 1994-06-07 18:54:20 dbrown Exp $
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

/*
 * defines for Display resources (temporary, should be enums)
 */

#define Nhl_ovNoCreate		0
#define Nhl_ovNever		1
#define Nhl_ovConditionally	2
#define Nhl_ovAlways		3


/*
 * Overlay instance resources
 */

#define NhlNovOverlayIds	".ovOverlayIds"
#define NhlNovPreDrawOrder	".ovPreDrawOrder"
#define NhlNovPostDrawOrder	".ovPostDrawOrder"
#define NhlNovDisplayTitles	"ovDisplayTitles"
#define NhlNovTitleZone		"ovTitleZone"
#define NhlNovDisplayTickMarks	"ovDisplayTickMarks"
#define NhlNovTickMarkZone	"ovTickMarkZone"
#define NhlNovDisplayLabelBar	"ovDisplayLabelBar"
#define NhlNovLabelBarZone	"ovLabelBarZone"
#define NhlNovDisplayLegend	"ovDisplayLegend"
#define NhlNovLegendZone	"ovLegendZone"

#define NhlNovLabelBarWidthF	"ovLabelBarWidth"
#define NhlNovLabelBarHeightF	"ovLabelBarHeight"
#define NhlNovLabelBarSide	"ovLabelBarSide"
#define NhlNovLabelBarParallelPosF	"ovLabelBarParallelPosF"
#define NhlNovLabelBarOrthogonalPosF	"ovLabelBarOrthogonalPosF"

#define NhlNovLabelBarXOffsetF	"ovLabelBarXOffset"
#define NhlNovLabelBarYOffsetF	"ovLabelBarYOffset"
#define NhlNovLabelBarPosition	"ovLabelBarPosition"
#define NhlNovLegendWidthF	"ovLegendWidth"
#define NhlNovLegendHeightF	"ovLegendHeight"
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
#define NhlCovDisplayTitles	"OvDisplayTitles"
#define NhlCovTitleZone		"OvTitleZone"
#define NhlCovDisplayTickMarks	"OvDisplayTickMarks"
#define NhlCovTickMarkZone	"OvTickMarkZone"
#define NhlCovDisplayLabelBar	"OvDisplayLabelBar"
#define NhlCovLabelBarZone	"OvLabelBarZone"
#define NhlCovDisplayLegend	"OvDisplayLegend"
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
NhlErrorTypes NhlAddToOverlay(
#ifdef NhlNeedProto
        int		base_id,
	int		plot_id,
	int		after_id
#endif
);

NhlErrorTypes NhlRemoveFromOverlay(
#ifdef NhlNeedProto
        int		base_id,
	int		plot_id,
	NhlBoolean	restore
#endif
);

NhlErrorTypes NhlRegisterAnnotation(
#ifdef NhlNeedProto
        int	overlay_base_id,
	int	annotation_id
#endif
);

NhlErrorTypes NhlUnregisterAnnotation(
#ifdef NhlNeedProto
        int	overlay_base_id,
	int	annotation_id
#endif
);

#endif /*_NOverlay_h */
