/*
 *      $Id: MapTransObj.h,v 1.4 1994-09-08 01:34:27 dbrown Exp $
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

typedef enum _NhlMapLimitMode {
	NhlMAXIMALAREA = 0, NhlLATLON, NhlANGLES, NhlCORNERS,
	NhlWINDOW } NhlMapLimitMode;

#define NhlTMapLimitMode	"maplimitmode"

typedef enum _NhlProjection {
	NhlORTHOGRAPHIC, NhlSTEREOGRAPHIC, NhlLAMBERTEQUALAREA,
	NhlGNOMONIC, NhlAZIMUTHALEQUIDISTANT, NhlSATELLITE,
	NhlMOLLWEIDE, NhlMERCATOR, NhlCYLINDRICALEQUIDISTANT,
	NhlLAMBERTCONFORMAL } NhlProjection;

#define NhlTProjection	"projection"

#include <ncarg/hlu/TransObj.h>

#define	NhlNmpProjection		"mpProjection"
#define NhlNmpCenterLatF		"mpCenterLatF"
#define NhlNmpCenterLonF		"mpCenterLonF"
#define NhlNmpCenterRotF		"mpCenterRotF"

#define NhlNmpMapLimitMode		"mpMapLimitMode"
#define NhlNmpMinLatF			"mpMinLatF"
#define NhlNmpMaxLatF			"mpMaxLatF"
#define NhlNmpMinLonF			"mpMinLonF"
#define NhlNmpMaxLonF			"mpMaxLonF"
#define NhlNmpLeftAngleF		"mpLeftAngleF"
#define NhlNmpRightAngleF		"mpRightAngleF"
#define NhlNmpBottomAngleF		"mpBottomAngleF"
#define NhlNmpTopAngleF			"mpTopAngleF"
#define NhlNmpActualMinLatF		"mpActualMinLatF"
#define NhlNmpActualMaxLatF		"mpActualMaxLatF"
#define NhlNmpActualMinLonF		"mpActualMinLonF"
#define NhlNmpActualMaxLonF		"mpActualMaxLonF"

#define NhlNmpLeftCornerLatF		"mpLeftCornerLatF"
#define NhlNmpLeftCornerLonF		"mpLeftCornerLonF"
#define NhlNmpRightCornerLatF		"mpRightCornerLatF"
#define NhlNmpRightCornerLonF		"mpRightCornerLonF"
#define NhlNmpLeftWindowF		"mpLeftWindowF"
#define NhlNmpRightWindowF		"mpRightWindowF"
#define NhlNmpBottomWindowF		"mpBottomWindowF"
#define NhlNmpTopWindowF		"mpTopWindowF"

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

/* resource classes */

#define	NhlCmpProjection		"MpProjection"
#define NhlCmpCenterLatF		"MpCenterLatF"
#define NhlCmpCenterLonF		"MpCenterLonF"
#define NhlCmpCenterRotF		"MpCenterRotF"

#define NhlCmpMapLimitMode		"MpMapLimitMode"
#define NhlCmpMinLatF			"MpMinLatF"
#define NhlCmpMaxLatF			"MpMaxLatF"
#define NhlCmpMinLonF			"MpMinLonF"
#define NhlCmpMaxLonF			"MpMaxLonF"
#define NhlCmpLeftAngleF		"MpLeftAngleF"
#define NhlCmpRightAngleF		"MpRightAngleF"
#define NhlCmpBottomAngleF		"MpBottomAngleF"
#define NhlCmpTopAngleF			"MpTopAngleF"
#define NhlCmpActualMinLatF		"MpActualMinLatF"
#define NhlCmpActualMaxLatF		"MpActualMaxLatF"
#define NhlCmpActualMinLonF		"MpActualMinLonF"
#define NhlCmpActualMaxLonF		"MpActualMaxLonF"

#define NhlCmpLeftCornerLatF		"MpLeftCornerLatF"
#define NhlCmpLeftCornerLonF		"MpLeftCornerLonF"
#define NhlCmpRightCornerLatF		"MpRightCornerLatF"
#define NhlCmpRightCornerLonF		"MpRightCornerLonF"
#define NhlCmpLeftWindowF		"MpLeftWindowF"
#define NhlCmpRightWindowF		"MpRightWindowF"
#define NhlCmpBottomWindowF		"MpBottomWindowF"
#define NhlCmpTopWindowF		"MpTopWindowF"

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

extern NhlLayerClass NhlmapTransObjLayerClass;

#endif /*_NMapTranObj_h */
