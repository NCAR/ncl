
/*
 *      $Id: TransObj.c,v 1.1 1993-04-30 17:25:15 boote Exp $
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
 *	Date:		Fri Oct 16 11:46:05 MDT 1992
 *
 *	Description:	
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>

#include <ncarg/hlu/TransObjP.h>


TransObjLayerClassRec transObjLayerClassRec = {
	{
/* superclass*/         (LayerClass)&baseLayerClassRec,
/* class_name */        "TransObj",
/* nrm_class */         NrmNULLQUARK,
/* layer_size */        sizeof(TransObjLayerRec),
/* layer_resources */   NULL,
/* num_resources */     0,
/* class_part_initialize */     NULL,
/* class_inited */      False,
/* class_initialize */  NULL,
/* layer_initialize */  NULL,
/* layer_set_values */  NULL,
/* layer_set_values_not */  NULL,
/* layer_get_values */  NULL,
/* layer_pre_draw */    NULL,
/* layer_draw */        NULL,
/* layer_draw_segonly */NULL,
/* layer_post_draw */   NULL,
/* layer_clear */       NULL,
/* layer_destroy */    NULL 
	},
	{
/* set_trans */		NULL,
/* trans_type */	NULL,
/* win_to_ndc */	NULL,
/* ndc_to_win */	NULL,
/* data_to_win */	NULL,
/* win_to_data */	NULL,
/* data_to_compc */	NULL,
/* compc_to_data */	NULL,
/* win_to_compc */	NULL,
/* compc_to_win */	NULL
	}
};

LayerClass transObjLayerClass = (LayerClass)&transObjLayerClassRec;

