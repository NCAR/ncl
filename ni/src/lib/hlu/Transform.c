/*
 *      $Id: Transform.c,v 1.2 1993-10-19 17:52:59 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Transform.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 16:40:50 MDT 1992
 *
 *	Description:	Provides all subclasses of this class a generic hook
 *			into which are placed functions for the forward and
 *			reverse transformations to support point-n-click.
 *			
 *			LevelOne implies a linear transformation NDC<==>WINDOW
 *			LevelTwo implies a transformation  from  WINDOW<==>DATA
 *			
 *			Level two is used when maps or odd transformations are
 *			used.
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransformP.h>


TransformLayerClassRec transformLayerClassRec = {
        {
/* class_name */        "Transform",
/* nrm_class */         NrmNULLQUARK,
/* layer_size */        sizeof(TransformLayerRec),
/* class_inited */      False,
/* superclass*/         (LayerClass)&viewLayerClassRec,

/* layer_resources */   NULL,
/* num_resources */     0,
/* all_resources		*/	NULL,

/* class_part_initialize */     NULL,
/* class_initialize */  NULL,
/* layer_initialize */  NULL,
/* layer_set_values */  NULL,
/* layer_set_values_hook */  NULL,
/* layer_get_values */  NULL,
/* layer_reparent */  NULL,
/* layer_destroy */     NULL,

/* child_resources */	NULL,

/* layer_draw */        NULL,

/* layer_pre_draw */        NULL,
/* layer_draw_segonly */    NULL,
/* layer_post_draw */        NULL,
/* layer_clear */       NULL

        },
	{
/* segment_wkid */		0,
/* get_bb */			NULL
	},
	{
/* data_to_ndc */	NULL,
/* ndc_to_data */	NULL
	}
};
	
LayerClass transformLayerClass = (LayerClass)&transformLayerClassRec;

