
/*
 *      $Id: MapTransObj.h,v 1.1 1993-04-30 17:22:56 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Oct 28 09:21:14 MST 1992
 *
 *	Description:	
 */

#ifndef _NMapTransObj_h
#define _NMapTransObj_h

#include <ncarg/hlu/TransObj.h>

#define	NhlNmpProjection	"mpProjection"
#define NhlNmpOutlineType	"mpOutlineType"
#define NhlNmpCenterLatF		"mpCenterLatF"
#define NhlNmpCenterLonF		"mpCenterLonF"
#define NhlNmpCenterRotF		"mpCenterRotF"
#define NhlNmpRectLimitType		"mpRectLimitType"
#define NhlNmpRectLimit1		"mpRectLimit1"
#define NhlNmpRectLimit2		"mpRectLimit2"
#define NhlNmpRectLimit3		"mpRectLimit3"
#define NhlNmpRectLimit4		"mpRectLimit4"
#define NhlNmpLambertParallel1F		"mpLambertParallel1F"
#define NhlNmpLambertParallel2F		"mpLambertParallel2F"
#define NhlNmpLambertMeridianF		"mpLambertMeridianF"
#define NhlNmpSatelliteDistF		"mpSatelliteDistF"
#define NhlNmpSatelliteAngle1F		"mpSatelliteAngle1F"
#define NhlNmpSatelliteAngle2F		"mpSatelliteAngle2F"
#define NhlNmpEllipticalBoundary	"mpEllipticalBoundary"
/*
#define NhlNmpMapPosLF			"mpMapPosLF"
#define NhlNmpMapPosRF			"mpMapPosRF"
#define NhlNmpMapPosTF			"mpMapPosTF"
#define NhlNmpMapPosBF			"mpMapPosBF"
*/


#define	NhlCmpProjection		"MpProjection"
#define NhlCmpOutlineType		"MpOutlineType"
#define NhlCmpCenterLatF		"MpCenterLatF"
#define NhlCmpCenterLonF		"MpCenterLonF"
#define NhlCmpCenterRotF		"MpCenterRotF"
#define NhlCmpRectLimitType		"MpRectLimitType"
#define NhlCmpRectLimit1		"MpRectLimit1"
#define NhlCmpRectLimit2		"MpRectLimit2"
#define NhlCmpRectLimit3		"MpRectLimit3"
#define NhlCmpRectLimit4		"MpRectLimit4"
#define NhlCmpLambertParallel1F		"MpLambertParallel1F"
#define NhlCmpLambertParallel2F		"MpLambertParallel2F"
#define NhlCmpLambertMeridianF		"MpLambertMeridianF"
#define NhlCmpSatelliteDistF		"MpSatelliteDistF"
#define NhlCmpSatelliteAngle1F		"MpSatelliteAngle1F"
#define NhlCmpSatelliteAngle2F		"MpSatelliteAngle2F"
#define NhlCmpEllipticalBoundary	"MpEllipticalBoundary"
/*
#define NhlCmpMapPosLF			"MpMapPosLF"
#define NhlCmpMapPosRF			"MpMapPosRF"
#define NhlCmpMapPosTF			"MpMapPosTF"
#define NhlCmpMapPosBF			"MpMapPosBF"
*/



extern LayerClass mapTransObjLayerClass;

typedef struct _MapTransObjLayerRec *MapTransObjLayer;
typedef struct _MapTransObjLayerClassRec *MapTransObjLayerClass;

#endif /*_NMapTranObj_h */
