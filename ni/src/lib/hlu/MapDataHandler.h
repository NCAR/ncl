/*
 *      $Id: MapDataHandler.h,v 1.1 1998-05-22 01:59:08 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapDataHandler.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 23 12:01:04 MDT 1998
 *
 *	Description:	This is the header file for the MapDataHandler class.
 *                      MapDataHandler is a private class to be used by
 *                      MapPlot internally. It has access to MapPlot's
 *                      internal data structures; the reverse is not true
 *                      however.
 */
#ifndef _NMapDataHandler_h
#define  _NMapDataHandler_h

#include <ncarg/hlu/MapPlotP.h>

extern NhlErrorTypes _NhlUpdateDrawList(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlBoolean  		init,
        NhlMapPlotLayer 	newmp,
        NhlMapPlotLayer 	oldmp,
        _NhlArgList		args,
        int             	num_args
#endif
);

extern NhlErrorTypes _NhlDrawMapList(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlMapPlotLayer 	mp,
        mpDrawOp		draw_op,
        NhlBoolean		init_draw
#endif
);

extern NhlClass NhlmapDataHandlerClass;


#endif  /*_NMapDataHandler_h*/
