/*
 *      $Id: Contour.h,v 1.1 1993-11-20 01:05:49 dbrown Exp $
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
 *	Description:	public header for Contour object
 */
#ifndef _NContour_h
#define _NContour_h

#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>

/*
 * Resource names
 */

/*
 * DataDep objects resources
 */
#define NhlNcnColors	"cnColors"
#define NhlCcnColors	"CnColors"

#define	NhlNcnColor	"cnColor"
#define	NhlCcnColor	"CnColor"

#define NhlNcnDashPatterns "cnDashPatterns"
#define NhlCcnDashPatterns "CnDashPatterns"

#define	NhlNcnDashPattern	"cnDashPattern"
#define	NhlCcnDashPattern	"CnDashPattern"

#define NhlNcnLabelMode		"cnLabelMode"
#define NhlCcnLabelMode		"CnLabelMode"

#define NhlNcnExplicitLabels	"cnExplicitLabels"
#define NhlCcnExplicitLabels	"CnExplicitLabels"

/*
 * Contour's resource names
 */

#define NhlNcnCurveData		"cnCurveData"
#define NhlCcnCurveData		"CnCurveData"

#define NhlNcnCurveThicknessF	"cnCurveThicknessF"
#define NhlCcnCurveThicknessF	"CnCurveThicknessF"

#define NhlNcnXStyle		"cnXStyle"
#define NhlCcnXStyle		"CnXStyle"

#define NhlNcnYStyle		"cnYStyle"
#define NhlCcnYStyle		"CnYStyle"

#define NhlNcnXIrregularPoints	"cnXIrregularPoints"
#define NhlCcnXIrregularPoints	"CnXIrregularPoints"

#define NhlNcnYIrregularPoints	"cnYIrregularPoints"
#define NhlCcnYIrregularPoints	"CnYIrregularPoints"

#define NhlNcnXReverse		"cnXReverse"
#define NhlCcnXReverse		"CnXReverse"

#define NhlNcnYReverse		"cnYReverse"
#define NhlCcnYReverse		"CnYReverse"

#define	NhlNcnComputeXMin	"cnComputeXMin"
#define	NhlCcnComputeXMin	"CnComputeXMin"

#define NhlNcnXMinF		"cnXMinF"
#define NhlCcnXMinF		"CnXMinF"

#define	NhlNcnComputeXMax	"cnComputeXMax"
#define	NhlCcnComputeXMax	"CnComputeXMax"

#define NhlNcnXMaxF		"cnXMaxF"
#define NhlCcnXMaxF		"CnXMaxF"

#define	NhlNcnComputeYMax	"cnComputeYMax"
#define	NhlCcnComputeYMax	"CnComputeYMax"

#define NhlNcnYMaxF		"cnYMaxF"
#define NhlCcnYMaxF		"CnYMaxF"

#define	NhlNcnComputeYMin	"cnComputeYMin"
#define	NhlCcnComputeYMin	"CnComputeYMin"

#define NhlNcnYMinF		"cnYMinF"
#define NhlCcnYMinF		"CnYMinF"

#define NhlNcnTitles		"cnTitles"
#define NhlCcnTitles		"CnTitles"

#define NhlNcnXAlternate	"cnXAlternate"
#define NhlCcnXAlternate	"CnXAlternate"

#define NhlNcnYAlternate	"cnYAlternate"
#define NhlCcnYAlternate	"CnYAlternate"

#define NhlNcnXAlternateCoords	"cnXAlternateCoords"
#define NhlCcnXAlternateCoords	"CnXAlternateCoords"

#define NhlNcnXOriginalCoords	"cnXOriginalCoords"
#define NhlCcnXOriginalCoords	"CnXOriginalCoords"

#define NhlNcnYAlternateCoords	"cnYAlternateCoords"
#define NhlCcnYAlternateCoords	"CnYAlternateCoords"

#define NhlNcnYOriginalCoords	"cnYOriginalCoords"
#define NhlCcnYOriginalCoords	"CnYOriginalCoords"

#define NhlNcnDashSegmentLengthF	"cnDashSegmentLengthF"
#define NhlCcnDashSegmentLengthF	"CnDashSegmentLengthF"

#define NhlNcnLineLabelFontHeightF	"cnLineLabelFontHeightF"
#define NhlCcnLineLabelFontHeightF	"CnLineLabelFontHeightF"

#define NhlNcnXIrrTensionF		"cnXIrrTensionF"
#define NhlCcnXIrrTensionF		"CnXIrrTensionF"

#define NhlNcnYIrrTensionF		"cnYIrrTensionF"
#define NhlCcnYIrrTensionF		"CnYIrrTensionF"
/*
 * Names for new types.
 */
#define NhlTAlternatePlace	"alternatePlace"
#define NhlTLineLabelModes	"lineLabelModes"

/*
 * New types
 */
typedef enum { NONE, LEFTAXIS, RIGHTAXIS, TOPAXIS, BOTTOMAXIS } AlternatePlace;
typedef enum { NOLABELS, LETTERED, CUSTOM } LineLabelModes;

extern LayerClass contourLayerClass;

typedef struct _ContourLayerClassRec *ContourLayerClass;
typedef struct _ContourLayerRec *ContourLayer;
#endif /* _NContour_h */



