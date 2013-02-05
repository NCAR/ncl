/*
 *      $Id: PlotManager.h,v 1.7 2010-03-19 22:44:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		PlotManager.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for PlotManager class.
 */

#ifndef _NPLOTMANAGER_h
#define _NPLOTMANAGER_h

#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/Legend.h>
#include <ncarg/hlu/AnnoManager.h>

typedef enum _NhlAnnotationDisplayMode {
	NhlNOCREATE = -1,NhlNEVER = 0, NhlALWAYS = 1, NhlCONDITIONAL,NhlFORCEALWAYS
} NhlAnnotationDisplayMode;

#define NhlTAnnotationDisplayMode        "AnnotationDisplayMode"

/*
 * PlotManager instance resources
 */

#define NhlNpmOverlaySequenceIds	"pmOverlaySequenceIds"
#define NhlNpmPreDrawOrder	"pmPreDrawOrder"
#define NhlNpmPostDrawOrder	"pmPostDrawOrder"

#define NhlNpmAnnoViews		"pmAnnoViews"
#define NhlNpmAnnoManagers	"pmAnnoManagers"
#define NhlNpmFitToBB		"pmFitToBB"
#define NhlNpmBBLeftF		"pmBBLeftF"
#define NhlNpmBBRightF		"pmBBRightF"
#define NhlNpmBBBottomF		"pmBBBottomF"
#define NhlNpmBBTopF		"pmBBTopF"

#define NhlNpmTitleDisplayMode	"pmTitleDisplayMode"
#define NhlNpmTitleZone		"pmTitleZone"
#define NhlNpmTickMarkDisplayMode	"pmTickMarkDisplayMode"
#define NhlNpmTickMarkZone	"pmTickMarkZone"
#define NhlNpmLabelBarDisplayMode	"pmLabelBarDisplayMode"
#define NhlNpmLabelBarZone	"pmLabelBarZone"
#define NhlNpmLegendDisplayMode	"pmLegendDisplayMode"
#define NhlNpmLegendZone	"pmLegendZone"

#define NhlNpmLabelBarWidthF	"pmLabelBarWidthF"
#define NhlNpmLabelBarHeightF	"pmLabelBarHeightF"
#define NhlNpmLabelBarSide	"pmLabelBarSide"
#define NhlNpmLabelBarKeepAspect	"pmLabelBarKeepAspect"
#define NhlNpmLabelBarParallelPosF	"pmLabelBarParallelPosF"
#define NhlNpmLabelBarOrthogonalPosF	"pmLabelBarOrthogonalPosF"

#define NhlNpmLabelBarXOffsetF	"pmLabelBarXOffset"
#define NhlNpmLabelBarYOffsetF	"pmLabelBarYOffset"
#define NhlNpmLabelBarPosition	"pmLabelBarPosition"
#define NhlNpmLegendWidthF	"pmLegendWidthF"
#define NhlNpmLegendHeightF	"pmLegendHeightF"
#define NhlNpmLegendKeepAspect	"pmLegendKeepAspect"
#define NhlNpmLegendSide	"pmLegendSide"
#define NhlNpmLegendParallelPosF	"pmLegendParallelPosF"
#define NhlNpmLegendOrthogonalPosF	"pmLegendOrthogonalPosF"

#define NhlNpmLegendXOffsetF	"pmLegendXOffset"
#define NhlNpmLegendYOffsetF	"pmLegendYOffset"
#define NhlNpmLegendPosition	"pmLegendPosition"

/*
 * PlotManager class resources
 */

#define NhlCpmOverlaySequenceIds	"PmOverlaySequenceIds"
#define NhlCpmPreDrawOrder	"PmPreDrawOrder"
#define NhlCpmPostDrawOrder	"PmPostDrawOrder"

#define NhlCpmAnnoViews		"PmAnnoViews"
#define NhlCpmAnnoManagers	"PmAnnoManagers"
#define NhlCpmFitToBB		"PmFitToBB"
#define NhlCpmBBLeftF		"PmBBLeftF"
#define NhlCpmBBRightF		"PmBBRightF"
#define NhlCpmBBBottomF		"PmBBBottomF"
#define NhlCpmBBTopF		"PmBBTopF"

#define NhlCpmTitleDisplayMode	"PmTitleDisplayMode"
#define NhlCpmTitleZone		"PmTitleZone"
#define NhlCpmTickMarkDisplayMode	"PmTickMarkDisplayMode"
#define NhlCpmTickMarkZone	"PmTickMarkZone"
#define NhlCpmLabelBarDisplayMode	"PmLabelBarDisplayMode"
#define NhlCpmLabelBarZone	"PmLabelBarZone"
#define NhlCpmLegendDisplayMode	"PmLegendDisplayMode"
#define NhlCpmLegendZone	"PmLegendZone"
#define NhlCpmLabelBarWidthF	"PmLabelBarWidth"
#define NhlCpmLabelBarHeightF	"PmLabelBarHeight"
#define NhlCpmLabelBarKeepAspect	"PmLabelBarKeepAspect"
#define NhlCpmLabelBarSide	"PmLabelBarSide"
#define NhlCpmLabelBarParallelPosF	"PmLabelBarParallelPosF"
#define NhlCpmLabelBarOrthogonalPosF	"PmLabelBarOrthogonalPosF"

#define NhlCpmLabelBarXOffsetF	"PmLabelBarXOffset"
#define NhlCpmLabelBarYOffsetF	"PmLabelBarYOffset"
#define NhlCpmLabelBarPosition	"PmLabelBarPosition"

#define NhlCpmLegendWidthF	"PmLegendWidth"
#define NhlCpmLegendHeightF	"PmLegendHeight"
#define NhlCpmLegendKeepAspect	"PmLegendKeepAspect"
#define NhlCpmLegendSide	"PmLegendSide"
#define NhlCpmLegendParallelPosF	"PmLegendParallelPosF"
#define NhlCpmLegendOrthogonalPosF	"PmLegendOrthogonalPosF"

#define NhlCpmLegendXOffsetF	"PmLegendXOffset"
#define NhlCpmLegendYOffsetF	"PmLegendYOffset"
#define NhlCpmLegendPosition	"PmLegendPosition"

extern NhlClass NhlplotManagerClass;

#endif /*_NPLOTMANAGER_h */
