/*
 *      $Id: DataCommP.h,v 1.3 1994-01-27 21:22:33 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataCommP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 16 17:47:55 MDT 1993
 *
 *	Description:	This is the Private Header file for the DataComm
 *			class.
 */
#ifndef _NDataCommP_h
#define _NDataCommP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/DataCommF.h>
#include <ncarg/hlu/DataMgr.h>

#define _NhlTDListCompiled	".List.Compiled."
#define _NhlTDataList		".Data.List"
#define _NhlTAddData		".Add.Data"
#define _NhlTRemoveData		".Rm.Data"

typedef struct _NhlDataSpecLayerClassRec *NhlDataSpecLayerClass;
typedef struct _NhlDataSpecLayerRec *NhlDataSpecLayer;

typedef struct _NhlDataCommLayerClassRec *NhlDataCommLayerClass;
typedef struct _NhlDataCommLayerRec *NhlDataCommLayer;

typedef struct _NhlDataOffsetRec _NhlDataOffsetRec, *_NhlDataOffset;
typedef struct _NhlDataNodeRec _NhlDataNodeRec, *_NhlDataNodePtr;
typedef struct _NhlInternalDataListRec _NhlInternDataListRec, *_NhlInternDataList;

struct _NhlDataNodeRec {
	int		id;		/* id used to add/remove	*/
	NhlDataSpecLayer	dataspec;	/* DataSpec object	*/
	NrmQuark	type;		/* Type Data will convert to	*/
	int		item;		/* Data Item			*/
	_NhlDHandle	dhandle;	/* connection to data Item	*/
};

struct _NhlInternalDataListRec{
	int			num_items;
	_NhlDataNodePtr		*list;
	_NhlDataNodePtr		extra;
	NhlBoolean		shared_list;
	NhlDataCommLayer	dcl;
	_NhlDataOffset		oset;
};

struct _NhlDataOffsetRec{
	NrmQuark	res_name;
	unsigned int	offset;
	NhlLayerClass	dataspec_class;
	NrmQuarkList	qlist;
	_NhlDataOffset	next;
};

typedef struct _NhlDataCommLayerPart{
	/* User setable resource fields */
	NhlBoolean	 delay_compute;
	/* Private Fields */
	NhlBoolean	 data_changed;
} NhlDataCommLayerPart;

typedef struct _NhlDataCommLayerRec{
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlDataCommLayerPart	datacomm;
} NhlDataCommLayerRec;

typedef	NhlErrorTypes (*_NhlUpdateDataProc)(
#if	NhlNeedProto
	NhlDataCommLayer	new,
	NhlDataCommLayer	old
#endif
);

typedef struct _NhlDataCommLayerClassPart{
	_NhlDataOffset		data_offsets;
	_NhlUpdateDataProc	update_data;
} NhlDataCommLayerClassPart;

typedef struct _NhlDataCommLayerClassRec{
	NhlBaseLayerClassPart	base_class;
	NhlViewLayerClassPart	view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlDataCommLayerClassPart	datacomm_class;
} NhlDataCommLayerClassRec;

extern NhlDataCommLayerClassRec NhldataCommLayerClassRec;

/*
 * DataSpec definitions
 */

typedef struct _NhlDCommListRec _NhlDCommListRec, *_NhlDCommList;

struct _NhlDCommListRec{
	int		dcommid;
	NrmQuark	res_name;
	_NhlDCommList	next;
};

typedef struct _NhlDataSpecLayerPart{
	/* User setable resource fields */
	int		 data_item;
	/* Private Fields */
	_NhlDCommList	dcomm_list;
} NhlDataSpecLayerPart;

typedef struct _NhlDataSpecLayerRec{
	NhlObjLayerPart		base;
	NhlDataSpecLayerPart	dataspec;
} NhlDataSpecLayerRec;

typedef struct _NhlDataSpecLayerClassPart{
	int	foo;
} NhlDataSpecLayerClassPart;

typedef struct _NhlDataSpecLayerClassRec{
	NhlObjLayerClassPart	base_class;
	NhlDataSpecLayerClassPart	dataspec_class;
} NhlDataSpecLayerClassRec;

extern NhlDataSpecLayerClassRec NhldataSpecLayerClassRec;

/*
 * Private API to be used by Sub-Classes for handling Data
 */

/*VARARGS3*/
extern NhlErrorTypes _NhlRegisterDataRes(
#if	NeedVarArgProto
	NhlDataCommLayerClass	dc,		/* DataComm sub-class	*/
	NrmString		res_name,	/* name of data res	*/
	NhlLayerClass		dataspec,	/* DataSpecific object	*/
	...					/* types requested	*/
#endif
);

extern int _NhlGetDataInfo(
#if	NhlNeedProto
	NhlGenArray		data,		/* pointer to datalist	*/
	_NhlDataNodePtr		**dinfo		/* data info RET	*/
#endif
);

extern NhlLayer _NhlGetDataSet(
#if	NhlNeedProto
	_NhlDataNodePtr		datanode,	/* data node	*/
	NhlBoolean		*new_ret	/* is data new?	*/
#endif
);

#endif  /* _NDataCommP_h */
