/*
 *      $Id: View.h,v 1.9 1995-03-21 22:37:06 dbrown Exp $
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
 *	Description:	Public header file for the ViewLayerClass.
 */
#ifndef _NView_h
#define _NView_h

#include <ncarg/hlu/Base.h>

/* types used by Legend and LabelBar */
#define NhlTOrientation "Orientation"
typedef enum _NhlOrientation {
	NhlHORIZONTAL = 0,
	NhlVERTICAL = 1
} NhlOrientation;

/* position enumeration */

#define NhlTPosition "Position"
typedef enum _NhlPosition {
	NhlTOP,
	NhlBOTTOM,
	NhlRIGHT,
	NhlLEFT,
	NhlCENTER
} NhlPosition;

/* justification enumeration */

#define NhlTJustification "Justification"
typedef enum _NhlJustification {
	NhlTOPLEFT,
	NhlCENTERLEFT,
	NhlBOTTOMLEFT,
	NhlTOPCENTER,
	NhlCENTERCENTER,
	NhlBOTTOMCENTER,
	NhlTOPRIGHT,
	NhlCENTERRIGHT,
	NhlBOTTOMRIGHT
} NhlJustification;

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
#define NhlNvpAnnotationId	"vpAnnotationId"
#define NhlCvpAnnotationId	"VpAnnotationId"

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

extern NhlLayerClass NhlviewLayerClass;

#endif /*_NView_h*/
