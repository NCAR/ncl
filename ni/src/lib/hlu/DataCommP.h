/*
 *      $Id: DataCommP.h,v 1.1 1993-07-12 22:36:03 boote Exp $
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
#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/DataMgr.h>

typedef struct _DataListRec _NhlDataListRec, *_NhlDataList;

struct _DataListRec {
	int		item;
	_NhlDHandle	dhandle;
	_NhlDataList	next;
};

typedef struct _DataCommLayerPart{
	/* User setable resource fields */
	/* Private Fields */
	_NhlDataList data_list;
} DataCommLayerPart;

typedef struct _DataCommLayerRec{
	BaseLayerPart		base;
	ViewLayerPart		view;
	TransformLayerPart	trans;
	DataCommLayerPart	datacomm;
} DataCommLayerRec;

typedef struct _DataCommLayerClassPart{
	int	foo;
} DataCommLayerClassPart;

typedef struct _DataCommLayerClassRec{
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TransformLayerClassPart	trans_class;
	DataCommLayerClassPart	datacomm_class;
} DataCommLayerClassRec;

extern DataCommLayerClassRec dataCommLayerClassRec;

/*
 * Private API to be used by Sub-Classes for handling Data
 */

extern NrmQuark	_NhlDataConverterType(
#if	NeedVarArgProto
	int	item_id,/* data item to convert				*/
	...		/* types requested NrmNULLQUARK terminated	*/
#endif
);

extern _NhlDHandle _NhlRegisterData(
#if	NhlNeedProto
	Layer		self,		/* DataComm sub-class 		*/
	int		item_id,	/* DataItem sub-class		*/
	NrmQuark	type_requested	/* Q of type wanted		*/
#endif
);

extern NhlBoolean _NhlGetDataSet(
#if	NhlNeedProto
	int		item_id,	/* DataItem sub-class	*/
	_NhlDHandle	dhandle,	/* data id		*/
	NhlBoolean	*new_ret,	/* is data new?		*/
	int		*dset_ret	/* actual data object	*/
#endif
);

extern void _NhlReleaseData(
#if	NhlNeedProto
	Layer		self,		/* DataComm sub-class	*/
	int		item_id,	/* DataItem sub-class	*/
	_NhlDHandle	dhandle		/* data id		*/
#endif
);

#endif  /* _NDataCommP_h */
