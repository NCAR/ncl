/*
 *      $Id: MapTransObj.h,v 1.13 2008-09-05 00:31:51 dbrown Exp $
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
	NhlMAXIMALAREA = 0, NhlLATLON, NhlANGLES, NhlNPC, NhlNDC,
	NhlCORNERS, NhlPOINTS, NhlWINDOW } NhlMapLimitMode;

#define NhlTMapLimitMode	"MapLimitMode"

typedef enum _NhlProjection {
	NhlORTHOGRAPHIC = 0, NhlSTEREOGRAPHIC, NhlLAMBERTEQUALAREA,
	NhlGNOMONIC, NhlAZIMUTHALEQUIDISTANT, NhlSATELLITE,
	NhlPSEUDOMOLLWEIDE, NhlMERCATOR, NhlCYLINDRICALEQUIDISTANT,
	NhlLAMBERTCONFORMAL, NhlROBINSON, NhlCYLINDRICALEQUALAREA, NhlROTATEDMERCATOR,
        NhlAITOFF, NhlHAMMER, NhlMOLLWEIDE, NhlWINKELTRIPEL} NhlProjection;

#define NhlTProjection	"Projection"

typedef enum _NhlMapPolyMode {
	NhlAUTOPOLY = 0, NhlFASTPOLY, NhlSTANDARDPOLY
	 } NhlMapPolyMode;

#define NhlTMapPolyMode	"MapPolyMode"

#include <ncarg/hlu/TransObj.h>

#define	NhlNmpProjection		"mpProjection"
#define NhlNmpCenterLatF		"mpCenterLatF"
#define NhlNmpCenterLonF		"mpCenterLonF"
#define NhlNmpCenterRotF		"mpCenterRotF"
#define NhlNmpRelativeCenterLat		"mpRelativeCenterLat"
#define NhlNmpRelativeCenterLon		"mpRelativeCenterLon"
#define NhlNmpPreserveAspectRatio	"mpPreserveAspectRatio"

#define NhlNmpLeftMapPosF		"mpLeftMapPosF"
#define NhlNmpRightMapPosF		"mpRightMapPosF"
#define NhlNmpBottomMapPosF		"mpBottomMapPosF"
#define NhlNmpTopMapPosF		"mpTopMapPosF"

#define NhlNmpLimitMode			"mpLimitMode"

#define NhlNmpMinLatF			"mpMinLatF"
#define NhlNmpMaxLatF			"mpMaxLatF"
#define NhlNmpMinLonF			"mpMinLonF"
#define NhlNmpMaxLonF			"mpMaxLonF"

#define NhlNmpLeftAngleF		"mpLeftAngleF"
#define NhlNmpRightAngleF		"mpRightAngleF"
#define NhlNmpBottomAngleF		"mpBottomAngleF"
#define NhlNmpTopAngleF			"mpTopAngleF"

#define NhlNmpLeftNPCF			"mpLeftNPCF"
#define NhlNmpRightNPCF			"mpRightNPCF"
#define NhlNmpBottomNPCF		"mpBottomNPCF"
#define NhlNmpTopNPCF			"mpTopNPCF"

#define NhlNmpLeftNDCF			"mpLeftNDCF"
#define NhlNmpRightNDCF			"mpRightNDCF"
#define NhlNmpBottomNDCF		"mpBottomNDCF"
#define NhlNmpTopNDCF			"mpTopNDCF"

#define NhlNmpActualMinLatF		"mpActualMinLatF"
#define NhlNmpActualMaxLatF		"mpActualMaxLatF"
#define NhlNmpActualMinLonF		"mpActualMinLonF"
#define NhlNmpActualMaxLonF		"mpActualMaxLonF"

#define NhlNmpLeftCornerLatF		"mpLeftCornerLatF"
#define NhlNmpLeftCornerLonF		"mpLeftCornerLonF"
#define NhlNmpRightCornerLatF		"mpRightCornerLatF"
#define NhlNmpRightCornerLonF		"mpRightCornerLonF"

#define NhlNmpLeftPointLonF		"mpLeftPointLonF"
#define NhlNmpLeftPointLatF		"mpLeftPointLatF"
#define NhlNmpRightPointLonF		"mpRightPointLonF"
#define NhlNmpRightPointLatF		"mpRightPointLatF"
#define NhlNmpBottomPointLonF		"mpBottomPointLonF"
#define NhlNmpBottomPointLatF		"mpBottomPointLatF"
#define NhlNmpTopPointLonF		"mpTopPointLonF"
#define NhlNmpTopPointLatF		"mpTopPointLatF"

#define NhlNmpLeftWindowF		"mpLeftWindowF"
#define NhlNmpRightWindowF		"mpRightWindowF"
#define NhlNmpBottomWindowF		"mpBottomWindowF"
#define NhlNmpTopWindowF		"mpTopWindowF"

#define NhlNmpLambertParallel1F		"mpLambertParallel1F"
#define NhlNmpLambertParallel2F		"mpLambertParallel2F"
#define NhlNmpLambertMeridianF		"mpLambertMeridianF"
#define NhlNmpSatelliteDistF		"mpSatelliteDistF"
#define NhlNmpSatelliteAngle1F		"mpSatelliteAngle1F"
#define NhlNmpSatelliteAngle2F		"mpSatelliteAngle2F"
#define NhlNmpEllipticalBoundary	"mpEllipticalBoundary"
#define NhlNmpGreatCircleLinesOn	"mpGreatCircleLinesOn"
#define NhlNmpPolyMode	                "mpPolyMode"

#define NhlNmpDataMinLonF		"mpDataMinLonF"
#define NhlNmpDataMaxLonF		"mpDataMaxLonF"

/* resource classes */

#define	NhlCmpProjection		"MpProjection"
#define NhlCmpCenterLatF		"MpCenterLatF"
#define NhlCmpCenterLonF		"MpCenterLonF"
#define NhlCmpCenterRotF		"MpCenterRotF"
#define NhlCmpRelativeCenterLat		"MpRelativeCenterLat"
#define NhlCmpRelativeCenterLon		"MpRelativeCenterLon"
#define NhlCmpPreserveAspectRatio	"MpPreserveAspectRatio"

#define NhlCmpLimitMode			"MpLimitMode"

#define NhlCmpMinLatF			"MpMinLatF"
#define NhlCmpMaxLatF			"MpMaxLatF"
#define NhlCmpMinLonF			"MpMinLonF"
#define NhlCmpMaxLonF			"MpMaxLonF"

#define NhlCmpLeftAngleF		"MpLeftAngleF"
#define NhlCmpRightAngleF		"MpRightAngleF"
#define NhlCmpBottomAngleF		"MpBottomAngleF"
#define NhlCmpTopAngleF			"MpTopAngleF"

#define NhlCmpLeftNPCF			"MpLeftNPCF"
#define NhlCmpRightNPCF			"MpRightNPCF"
#define NhlCmpBottomNPCF		"MpBottomNPCF"
#define NhlCmpTopNPCF			"MpTopNPCF"

#define NhlCmpLeftNDCF			"MpLeftNDCF"
#define NhlCmpRightNDCF			"MpRightNDCF"
#define NhlCmpBottomNDCF		"MpBottomNDCF"
#define NhlCmpTopNDCF			"MpTopNDCF"

#define NhlCmpLeftMapPosF		"MpLeftMapPosF"
#define NhlCmpRightMapPosF		"MpRightMapPosF"
#define NhlCmpBottomMapPosF		"MpBottomMapPosF"
#define NhlCmpTopMapPosF		"MpTopMapPosF"

#define NhlCmpActualMinLatF		"MpActualMinLatF"
#define NhlCmpActualMaxLatF		"MpActualMaxLatF"
#define NhlCmpActualMinLonF		"MpActualMinLonF"
#define NhlCmpActualMaxLonF		"MpActualMaxLonF"

#define NhlCmpLeftCornerLatF		"MpLeftCornerLatF"
#define NhlCmpLeftCornerLonF		"MpLeftCornerLonF"
#define NhlCmpRightCornerLatF		"MpRightCornerLatF"
#define NhlCmpRightCornerLonF		"MpRightCornerLonF"

#define NhlCmpLeftPointLonF		"MpLeftPointLonF"
#define NhlCmpLeftPointLatF		"MpLeftPointLatF"
#define NhlCmpRightPointLonF		"MpRightPointLonF"
#define NhlCmpRightPointLatF		"MpRightPointLatF"
#define NhlCmpBottomPointLonF		"MpBottomPointLonF"
#define NhlCmpBottomPointLatF		"MpBottomPointLatF"
#define NhlCmpTopPointLonF		"MpTopPointLonF"
#define NhlCmpTopPointLatF		"MpTopPointLatF"

#define NhlCmpLeftWindowF		"MpLeftWindowF"
#define NhlCmpRightWindowF		"MpRightWindowF"
#define NhlCmpBottomWindowF		"MpBottomWindowF"
#define NhlCmpTopWindowF		"MpTopWindowF"

#define NhlCmpLambertParallel1F		"MpLambertParallel1F"
#define NhlCmpLambertParallel2F		"MpLambertParallel2F"
#define NhlCmpLambertMeridianF		"MpLambertMeridianF"
#define NhlCmpSatelliteDistF		"MpSatelliteDistF"
#define NhlCmpSatelliteAngle1F		"MpSatelliteAngle1F"
#define NhlCmpSatelliteAngle2F		"MpSatelliteAngle2F"
#define NhlCmpEllipticalBoundary	"MpEllipticalBoundary"
#define NhlCmpGreatCircleLinesOn	"MpGreatCircleLinesOn"
#define NhlCmpPolyMode	                "MpPolyMode"

#define NhlCmpDataMinLonF		"MpDataMinLonF"
#define NhlCmpDataMaxLonF		"MpDataMaxLonF"

extern NhlClass NhlmapTransObjClass;

#endif /*_NMapTranObj_h */
