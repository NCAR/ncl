/*
 *      $Id: DataMgrP.h,v 1.5 1995-04-07 10:41:40 boote Exp $
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

typedef struct _NhlDataMgrLayerPart{
	/* User setable resource fields */
	/* Private Fields */
	NhlBoolean	uptodate;
	_NhlDHandle	connection_list;
	_NhlDCache	data_list;
} NhlDataMgrLayerPart;

typedef struct _NhlDataMgrLayerRec{
	NhlObjLayerPart		base;
	NhlDataMgrLayerPart	datamgr;
} NhlDataMgrLayerRec;

typedef struct _NhlDataMgrClassPart{
	int	foo;
} NhlDataMgrClassPart;

typedef struct _NhlDataMgrClassRec{
	NhlObjClassPart		base_class;
	NhlDataMgrClassPart	datamgr_class;
} NhlDataMgrClassRec;

/* Exported Layer and Class */
typedef struct _NhlDataMgrClassRec *NhlDataMgrClass;
typedef struct _NhlDataMgrLayerRec *NhlDataMgrLayer;

extern NhlDataMgrClassRec NhldataMgrClassRec;

#endif  /* _NDataMgrP_h */
