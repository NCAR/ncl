/*
 *      $Id: DataCommP.h,v 1.8 1995-04-07 10:41:31 boote Exp $
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
#define _NhlTDataSpecList	".Data.Spec.List"
#define _NhlTAddData		".Add.Data"
#define _NhlTRemoveData		".Rm.Data"

typedef struct _NhlDataSpecClassRec *NhlDataSpecClass;
typedef struct _NhlDataSpecLayerRec *NhlDataSpecLayer;

typedef struct _NhlDataCommClassRec *NhlDataCommClass;
typedef struct _NhlDataCommLayerRec *NhlDataCommLayer;

typedef struct _NhlDataOffsetRec _NhlDataOffsetRec, *_NhlDataOffset;
typedef struct _NhlDataNodeRec _NhlDataNodeRec, *_NhlDataNodePtr;
typedef struct _NhlInternalDataListRec _NhlInternDataListRec, *_NhlInternDataList;

typedef struct _NhlDCommListRec _NhlDCommListRec, *_NhlDCommList;

struct _NhlDataNodeRec {
	int			item;		/* Data Item		*/
	NhlDataSpecLayer	dataspec;	/* DataSpec object	*/
	NrmQuark		type;		/* Type to convert to	*/
	_NhlDHandle		dhandle;	/* connection to datamgr*/
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
	NrmQuark	dsres_name;
	unsigned int	dsoffset;
	NhlClass	dataspec_class;
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

typedef struct _NhlDataCommClassPart{
	_NhlDataOffset		data_offsets;
	_NhlUpdateDataProc	update_data;
} NhlDataCommClassPart;

typedef struct _NhlDataCommClassRec{
	NhlBaseClassPart	base_class;
	NhlViewClassPart	view_class;
	NhlTransformClassPart	trans_class;
	NhlDataCommClassPart	datacomm_class;
} NhlDataCommClassRec;

extern NhlDataCommClassRec NhldataCommClassRec;

/*
 * DataSpec definitions
 */


struct _NhlDCommListRec{
	int		dcommid;
	NrmQuark	res_name;
	_NhlDCommList	next;
};

typedef struct _NhlDataSpecLayerPart{
	/* Private Fields */
	int		destroying;
} NhlDataSpecLayerPart;

typedef struct _NhlDataSpecLayerRec{
	NhlBaseLayerPart	base;
	NhlDataSpecLayerPart	dataspec;
} NhlDataSpecLayerRec;

typedef struct _NhlDataSpecClassPart{
	int	foo;
} NhlDataSpecClassPart;

typedef struct _NhlDataSpecClassRec{
	NhlBaseClassPart		base_class;
	NhlDataSpecClassPart	dataspec_class;
} NhlDataSpecClassRec;

extern NhlDataSpecClassRec NhldataSpecClassRec;

/*
 * Private API to be used by Sub-Classes for handling Data
 */

/*VARARGS3*/
extern NhlErrorTypes _NhlRegisterDataRes(
#if	NhlNeedVarArgProto
	NhlDataCommClass	dc,		/* DataComm sub-class	*/
	NrmString		data_res,	/* name of data res	*/
	NrmString		dataspec_res,	/* name of data res	*/
	NhlClass		dataspec,	/* DataSpecific class	*/
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
