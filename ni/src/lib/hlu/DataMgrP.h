/*
 *      $Id: DataMgrP.h,v 1.1 1993-07-12 22:36:19 boote Exp $
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

struct	_NhlDCacheRec{
	NrmQuark	type;
	NhlBoolean	uptodate;
	int		ref_count;
	int		dataset_id;
	_NhlConvertContext	cvt_context;
	_NhlDCache	next;
};

struct	_NhlDHandleRec{
	NrmQuark	type;
	_NhlDCache	cache;
	_NhlDHandle	next;
};

typedef struct _DataMgrLayerPart{
	/* User setable resource fields */
	/* Private Fields */
	NhlBoolean	uptodate;
	_NhlDHandle	connection_list;
	_NhlDCache	data_list;
} DataMgrLayerPart;

typedef struct _DataMgrLayerRec{
	BaseLayerPart		base;
	DataMgrLayerPart	datamgr;
} DataMgrLayerRec;

typedef struct _DataMgrLayerClassPart{
	int	foo;
} DataMgrLayerClassPart;

typedef struct _DataMgrLayerClassRec{
	BaseLayerClassPart	base_class;
	DataMgrLayerClassPart	datamgr_class;
} DataMgrLayerClassRec;

extern DataMgrLayerClassRec dataMgrLayerClassRec;

#endif  /* _NDataMgrP_h */
