/*
 *      $Id: nioBase.c,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Base.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:30:30 MDT 1992
 *
 *	Description:	This file contains all the functions and definitions
 *			neccessary to create an instance of the Base class
 *			layer.
 */
#include <stdio.h>
#include <limits.h>
#include "niohlu.h"
#include "nioConvertP.h"
#include "nioBaseP.h"

static _NhlRawObjCB bcallbacks[] = {
	{_NhlCBobjDestroy,NhlOffset(NhlBaseLayerRec,base.destroycb),
		 0,NULL,NULL,NULL},
	{_NhlCBobjChildChange,NhlOffset(NhlBaseLayerRec,base.cchildcb),
		 0,NULL,NULL,NULL},
};
static _NhlRawObjCB ocallbacks[] = {
	{_NhlCBobjDestroy,NhlOffset(NhlBaseLayerRec,base.destroycb),
		 0,NULL,NULL,NULL},
	{_NhlCBobjChildChange,NhlOffset(NhlBaseLayerRec,base.cchildcb),
		 0,NULL,NULL,NULL},
};

static NhlResource bresources[] = {
	{NhlNobjAppObj,NhlCobjAppObj,NhlTInteger,sizeof(int),
		NhlOffset(NhlBaseLayerRec,base.appid),
		NhlTImmediate,_NhlUSET(NhlDEFAULT_APP),
         	_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNnclData,_NhlCnclData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.ncl_data),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
	{_NhlNguiData,_NhlCguiData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
	{_NhlNguiData2,_NhlCguiData2,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data2),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
};

static NhlResource oresources[] = {
	{NhlNobjAppObj,NhlCobjAppObj,NhlTInteger,sizeof(int),
		NhlOffset(NhlBaseLayerRec,base.appid),
		NhlTImmediate,_NhlUSET(NhlDEFAULT_APP),
         	_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNnclData,_NhlCnclData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.ncl_data),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
	{_NhlNguiData,_NhlCguiData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
	{_NhlNguiData2,_NhlCguiData2,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data2),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL}
};

NhlObjClassRec NhlobjClassRec = {
	{
/* class_name			*/	"objClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)NULL,
/* cvt_table			*/	_NhlDefHashTable,

/* resources			*/	oresources,
/* num_resources		*/	NhlNumber(oresources),
/* all_resources		*/	NULL,
/* callbacks			*/	ocallbacks,
/* num_callbacks		*/	NhlNumber(ocallbacks),
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	}
};

NhlClassRec NhllayerClassRec = {
	{
/* class_name			*/	"baseClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)NULL,
/* cvt_table			*/	_NhlDefHashTable,

/* resources			*/	bresources,
/* num_resources		*/	NhlNumber(bresources),
/* all_resources		*/	NULL,
/* callbacks			*/	bcallbacks,
/* num_callbacks		*/	NhlNumber(bcallbacks),
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	}
};

NhlClass NhllayerClass = (NhlClass)&NhllayerClassRec;
NhlClass NhlbaseClass = (NhlClass)&NhllayerClassRec;
NhlClass NhlobjClass = (NhlClass)&NhlobjClassRec;
