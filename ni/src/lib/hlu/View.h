/*
 *      $Id: View.h,v 1.12 1996-01-04 21:47:56 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		View.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 1 09:59:16 MDT 1992
 *
 *	Description:	Public header file for the ViewClass.
 */
#ifndef _NVIEW_h
#define _NVIEW_h

#include <ncarg/hlu/Base.h>

/* types used by Legend and LabelBar */
#define NhlTOrientation "Orientation"
typedef enum _NhlOrientation {
	NhlHORIZONTAL = 0,
	NhlVERTICAL = 1
} NhlOrientation;

#define NhlTDrawOrder "DrawOrder"
typedef enum _NhlDrawOrder {
	NhlPREDRAW,
	NhlDRAW,
	NhlPOSTDRAW
} NhlDrawOrder;

NhlDOCTAG(NhlCoord)
typedef struct _NhlCoord {
	float x;
	float y;
} NhlCoord;


#define NhlNvpXF "vpXF"
#define NhlCvpXF "VpXF"
#define NhlNvpYF "vpYF"
#define NhlCvpYF "VpYF"
#define NhlNvpWidthF "vpWidthF"
#define NhlCvpWidthF "VpWidthF"
#define NhlNvpHeightF "vpHeightF"
#define NhlCvpHeightF "VpHeightF"
#define NhlNvpKeepAspect	"vpKeepAspect"
#define NhlCvpKeepAspect	"VpKeepAspect"
#define NhlNvpUseSegments	"vpUseSegments"
#define NhlCvpUseSegments	"VpUseSegments"
#define NhlNvpAnnoManagerId	"vpAnnoManagerId"
#define NhlCvpAnnoManagerId	"VpAnnoManagerId"

NhlDOCTAG(NhlBoundingBox)
typedef struct _NhlBoundingBox {
        int     set;
        float   t;
        float   b;
        float   l;
        float   r;
} NhlBoundingBox;

extern NhlErrorTypes NhlGetBB(
#if	NhlNeedProto
	int,		/* pid */
	NhlBoundingBox*	/* thebox */
#endif
);

extern NhlBoolean NhlIsView(
#if	NhlNeedProto
	int	pid
#endif
);

extern NhlClass NhlviewClass;

#endif /*_NVIEW_h*/
