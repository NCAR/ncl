/*
 *      $Id: DataMgrP.h,v 1.3 1994-01-27 21:22:49 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataMgrP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jun 24 10:26:35 MDT 1993
 *
 *	Description:	Private declarations for DataMgr class.
 */
#ifndef _NDataMgrP_h
#define _NDataMgrP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/DataMgrF.h>

typedef struct _NhlDHandleRec _NhlDHandleRec;
typedef struct _NhlDCacheRec _NhlDCacheRec, *_NhlDCache;
typedef struct _NhlDSpecRec _NhlDSpecRec, *_NhlDSpec;

struct	_NhlDCacheRec{
	NrmQuark		type;
	NhlBoolean		uptodate;
	int			ref_count;
	NhlLayer		dataset;
	_NhlConvertContext	cvt_context;
	_NhlDCache		next;
};

struct	_NhlDHandleRec{
	int		datacommid;
	NrmQuark	res_name;
	NrmQuark	type;
	_NhlDCache	cache;
	_NhlDHandle	next;
};

struct	_NhlDSpecRec{
	int		dspec_id;
	_NhlDSpec	next;
};

typedef struct _NhlDataMgrLayerPart{
	/* User setable resource fields */
	/* Private Fields */
	NhlBoolean	uptodate;
	_NhlDHandle	connection_list;
	_NhlDCache	data_list;
	_NhlDSpec	dspec_list;
} NhlDataMgrLayerPart;

typedef struct _NhlDataMgrLayerRec{
	NhlObjLayerPart		base;
	NhlDataMgrLayerPart	datamgr;
} NhlDataMgrLayerRec;

typedef struct _NhlDataMgrLayerClassPart{
	int	foo;
} NhlDataMgrLayerClassPart;

typedef struct _NhlDataMgrLayerClassRec{
	NhlObjLayerClassPart		base_class;
	NhlDataMgrLayerClassPart	datamgr_class;
} NhlDataMgrLayerClassRec;

/* Exported Layer and LayerClass */
typedef struct _NhlDataMgrLayerClassRec *NhlDataMgrLayerClass;
typedef struct _NhlDataMgrLayerRec *NhlDataMgrLayer;

extern NhlDataMgrLayerClassRec NhldataMgrLayerClassRec;

#endif  /* _NDataMgrP_h */
