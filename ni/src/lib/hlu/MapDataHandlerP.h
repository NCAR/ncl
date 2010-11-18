/*
 *      $Id: MapDataHandlerP.h,v 1.2 1998-11-12 21:40:02 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapDataHandlerP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 23 12:01:04 MDT 1998
 *
 *	Description:	
 */
#ifndef _NMapDataHandlerP_h
#define  _NMapDataHandlerP_h

#include <ncarg/hlu/MapDataHandler.h>

typedef enum _mdhCompType {
		mdhSTRCMP,mdhSTRNCMP,mdhSTRSTR,mdhSTRSTREND
} mdhCompType;

typedef struct _NhlMapDataHandlerLayerPart {
        NhlGenArray	area_names;
        NhlGenArray	area_types;
        NhlGenArray	dynamic_groups;
        NhlGenArray	fixed_groups;
        int		area_group_count;
}NhlMapDataHandlerLayerPart;

typedef struct _NhlMapDataHandlerLayerRec {
	NhlObjLayerPart			base;
	NhlMapDataHandlerLayerPart	mapdh;
}NhlMapDataHandlerLayerRec;

typedef NhlErrorTypes (*NhlUpdateDrawList)(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlBoolean  		init,
        NhlMapPlotLayer 	newmp,
        NhlMapPlotLayer 	oldmp,
        _NhlArgList		args,
        int             	num_args
#endif
);

typedef NhlErrorTypes (*NhlDrawMapList)(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlMapPlotLayer 	mp,
        mpDrawOp		draw_op,
	NhlBoolean		init_draw
#endif
);

typedef struct _NhlMapDataHandlerClassPart {
        NhlUpdateDrawList	update_draw_list;
        NhlDrawMapList		draw_map_list;
} NhlMapDataHandlerClassPart;

typedef struct _NhlMapDataHandlerClassRec {
	NhlObjClassPart			base_class;
	NhlMapDataHandlerClassPart	mapdh_class;
} NhlMapDataHandlerClassRec;

typedef struct _NhlMapDataHandlerClassRec *NhlMapDataHandlerClass;
typedef struct _NhlMapDataHandlerLayerRec *NhlMapDataHandlerLayer;

extern NhlMapDataHandlerClassRec NhlmapDataHandlerClassRec;

#endif  /*_NMapDataHandlerP_h*/
