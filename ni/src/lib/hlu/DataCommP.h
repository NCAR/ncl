/*
 *      $Id: DataCommP.h,v 1.2 1993-10-19 17:50:23 boote Exp $
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

typedef struct _DataOffsetRec _NhlDataOffsetRec, *_NhlDataOffset;
typedef struct _DataNodeRec _NhlDataNodeRec, *_NhlDataNodePtr;
typedef struct _InternalDataListRec _NhlInternDataListRec, *_NhlInternDataList;

struct _DataNodeRec {
	int		id;		/* id used to add/remove	*/
	DataSpecLayer	dataspec;	/* DataSpec object		*/
	NrmQuark	type;		/* Type Data will convert to	*/
	int		item;		/* Data Item			*/
	_NhlDHandle	dhandle;	/* connection to data Item	*/
};

struct _InternalDataListRec{
	int		num_items;
	_NhlDataNodePtr	*list;
	_NhlDataNodePtr	extra;
	NhlBoolean	shared_list;
	DataCommLayer	dcl;
	_NhlDataOffset	oset;
};

struct _DataOffsetRec{
	NrmQuark	res_name;
	unsigned int	offset;
	LayerClass	dataspec_class;
	NrmQuarkList	qlist;
	_NhlDataOffset	next;
};

typedef struct _DataCommLayerPart{
	/* User setable resource fields */
	NhlBoolean	 delay_compute;
	/* Private Fields */
	NhlBoolean	 data_changed;
} DataCommLayerPart;

typedef struct _DataCommLayerRec{
	BaseLayerPart		base;
	ViewLayerPart		view;
	TransformLayerPart	trans;
	DataCommLayerPart	datacomm;
} DataCommLayerRec;

typedef	NhlErrorTypes (*_NhlUpdateDataProc)(
#if	NhlNeedProto
	DataCommLayer	new,
	DataCommLayer	old
#endif
);

typedef struct _DataCommLayerClassPart{
	_NhlDataOffset		data_offsets;
	_NhlUpdateDataProc	update_data;
} DataCommLayerClassPart;

typedef struct _DataCommLayerClassRec{
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TransformLayerClassPart	trans_class;
	DataCommLayerClassPart	datacomm_class;
} DataCommLayerClassRec;

extern DataCommLayerClassRec dataCommLayerClassRec;

/*
 * DataSpec definitions
 */

typedef struct _NhlDCommListRec _NhlDCommListRec, *_NhlDCommList;

struct _NhlDCommListRec{
	int		dcommid;
	NrmQuark	res_name;
	_NhlDCommList	next;
};

typedef struct _DataSpecLayerPart{
	/* User setable resource fields */
	int		 data_item;
	/* Private Fields */
	_NhlDCommList	dcomm_list;
} DataSpecLayerPart;

typedef struct _DataSpecLayerRec{
	ObjLayerPart		base;
	DataSpecLayerPart	dataspec;
} DataSpecLayerRec;

typedef struct _DataSpecLayerClassPart{
	int	foo;
} DataSpecLayerClassPart;

typedef struct _DataSpecLayerClassRec{
	ObjLayerClassPart	base_class;
	DataSpecLayerClassPart	dataspec_class;
} DataSpecLayerClassRec;

extern DataSpecLayerClassRec dataSpecLayerClassRec;

/*
 * Private API to be used by Sub-Classes for handling Data
 */

/*VARARGS3*/
extern NhlErrorTypes _NhlRegisterDataRes(
#if	NeedVarArgProto
	DataCommLayerClass	dc,		/* DataComm sub-class	*/
	NrmString		res_name,	/* name of data res	*/
	LayerClass		dataspec,	/* DataSpecific object	*/
	...					/* types requested	*/
#endif
);

extern int _NhlGetDataInfo(
#if	NhlNeedProto
	NhlGenArray		data,		/* pointer to datalist	*/
	_NhlDataNodePtr		**dinfo		/* data info RET	*/
#endif
);

extern Layer _NhlGetDataSet(
#if	NhlNeedProto
	_NhlDataNodePtr		datanode,	/* data node	*/
	NhlBoolean		*new_ret	/* is data new?	*/
#endif
);

#endif  /* _NDataCommP_h */
