/*
 *      $Id: Contour.h,v 1.2 1993-12-22 00:55:43 dbrown Exp $
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
 *	Description:	Public header for Contour class.
 */

#ifndef _NContour_h
#define _NContour_h

#include <ncarg/hlu/Overlay.h>

/*
 * Contour instance resources
 */

#define NhlNcnOutOfRangeValF	"cnOutOfRangeValF"
#define NhlNcnXMinF		"cnXMinF"
#define NhlNcnXMaxF		"cnXMaxF"
#define NhlNcnXLog		"cnXLog"
#define NhlNcnXReverse		"cnXReverse"

#define NhlNcnYMinF		"cnYMinF"
#define NhlNcnYMaxF		"cnYMaxF"
#define NhlNcnYLog		"cnYLog"
#define NhlNcnYReverse		"cnYReverse"

/*
 * Contour class resources
 */


#define NhlCcnOutOfRangeValF	"CnOutOfRangeValF"
#define NhlCcnXMinF		"CnXMinF"
#define NhlCcnXMaxF		"CnXMaxF"
#define NhlCcnXLog		"CnXLog"
#define NhlCcnXReverse		"CnXReverse"

#define NhlCcnYMinF		"CnYMinF"
#define NhlCcnYMaxF		"CnYMaxF"
#define NhlCcnYLog		"CnYLog"
#define NhlCcnYReverse		"CnYReverse"

typedef struct _ContourLayerClassRec	*ContourLayerClass;
typedef struct _ContourLayerRec		*ContourLayer;

extern LayerClass			contourLayerClass;

#endif /*_NContour_h */
