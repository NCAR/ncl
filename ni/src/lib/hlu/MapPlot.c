/*
 *      $Id: MapPlot.c,v 1.1 1993-11-20 01:06:12 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapPlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a generic Map plot that
 *			can serve as an overlay base plot or be used for
 *			general map plots that do not use other hlu
 *			plot objects.
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/MapPlotP.h>


MapPlotLayerClassRec mapPlotLayerClassRec = {
        {
/* class_name			*/      "MapPlot",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(MapPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (LayerClass)&transformLayerClassRec,

/* layer_resources		*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL,

/* child_resources		*/	NULL,

/* layer_draw			*/      NULL,

/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	NULL
	},
	{
/* handles_overlays 		*/	True,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL
	},
	{
/* foo				*/	NULL
	}
};
	
LayerClass mapPlotLayerClass = (LayerClass)&mapPlotLayerClassRec;

