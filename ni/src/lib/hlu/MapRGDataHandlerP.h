/*
 *      $Id: MapRGDataHandlerP.h,v 1.2 2001-12-05 00:19:04 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapRGDataHandlerP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Oct  3 13:17:25 MDT 2001
 *
 *	Description:	
 */
#ifndef _NMapRGDataHandlerP_h
#define  _NMapRGDataHandlerP_h

#include <ncarg/hlu/MapDataHandlerP.h>
#include <ncarg/hlu/MapRGDataHandler.h>

#define Nhl_mpMAPDATAFILE	"NhlMapData"
#define rgALLOC_UNIT		64
#define mpRG_FLT_REQ_SIZE	122000 * 4
#define mpRG_AREAMAP_REQ_SIZE	665000 * 4

typedef struct _NhlMapRGDataHandlerLayerPart {

	NhlMapDataResolution data_resolution;
        
	int		aws_id;
	int		fws_id;
        NhlBoolean	new_amap_req;
	NhlMapDataResolution real_data_resolution;
}NhlMapRGDataHandlerLayerPart;

typedef struct _NhlMapRGDataHandlerLayerRec {
	NhlObjLayerPart			base;
	NhlMapDataHandlerLayerPart	mapdh;
	NhlMapRGDataHandlerLayerPart	maprgdh;
}NhlMapRGDataHandlerLayerRec;

typedef struct _NhlMapRGDataHandlerClassPart {
	int  foo;
} NhlMapRGDataHandlerClassPart;

typedef struct _NhlMapRGDataHandlerClassRec {
	NhlObjClassPart			base_class;
	NhlMapDataHandlerClassPart	mapdh_class;
	NhlMapRGDataHandlerClassPart	maprgdh_class;
} NhlMapRGDataHandlerClassRec;

typedef struct _NhlMapRGDataHandlerClassRec *NhlMapRGDataHandlerClass;
typedef struct _NhlMapRGDataHandlerLayerRec *NhlMapRGDataHandlerLayer;

extern NhlMapRGDataHandlerClassRec NhlmapRGDataHandlerClassRec;

#endif  /*_NMapRGDataHandlerP_h*/
