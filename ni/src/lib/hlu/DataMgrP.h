/*
 *      $Id: DataMgrP.h,v 1.2 1993-10-19 17:50:36 boote Exp $
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
	NrmQuark	type;
	NhlBoolean	uptodate;
	int		ref_count;
	Layer		dataset;
	_NhlConvertContext	cvt_context;
	_NhlDCache	next;
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

typedef struct _DataMgrLayerPart{
	/* User setable resource fields */
	/* Private Fields */
	NhlBoolean	uptodate;
	_NhlDHandle	connection_list;
	_NhlDCache	data_list;
	_NhlDSpec	dspec_list;
} DataMgrLayerPart;

typedef struct _DataMgrLayerRec{
	ObjLayerPart		base;
	DataMgrLayerPart	datamgr;
} DataMgrLayerRec;

typedef struct _DataMgrLayerClassPart{
	int	foo;
} DataMgrLayerClassPart;

typedef struct _DataMgrLayerClassRec{
	ObjLayerClassPart	base_class;
	DataMgrLayerClassPart	datamgr_class;
} DataMgrLayerClassRec;

extern DataMgrLayerClassRec dataMgrLayerClassRec;

#endif  /* _NDataMgrP_h */
