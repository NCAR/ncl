/*
 *      $Id: Overlay.c,v 1.1 1993-11-20 01:06:18 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Overlay.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Manages the drawing for plot objects that must
 *			share a common overlay transformation
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/OverlayP.h>


OverlayLayerClassRec overlayLayerClassRec = {
        {
/* class_name			*/      "Overlay",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(OverlayLayerRec),
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
	
LayerClass overlayLayerClass = (LayerClass)&overlayLayerClassRec;

