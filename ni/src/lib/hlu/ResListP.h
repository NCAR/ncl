/*
 *      $Id: ResListP.h,v 1.5.4.1 2008-03-28 20:37:37 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ResListP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 15 11:14:34 MST 1994
 *
 *	Description:	
 */
#ifndef	_NResListP_H
#define	_NResListP_H

#include <ncarg/hlu/ResList.h>

#define	_NhlTExpMDArray		"_Export_MDArray_"
#define	_NhlTExpMDTypeArray	"_Export_MDTypeArray_"
#define	_NhlTExpArray		"_Export_Array_"
#define	_NhlTExpTypeArray	"_Export_TypeArray_"

typedef struct _NhlGArgExtraRec{
	NrmQuark	type_ret;
	unsigned int	size_ret;
	_NhlArgVal	value_ret;
	_NhlFreeFunc	free_func;
	NhlClass	chld_class;
} _NhlGArgExtra, *_NhlGArgExtraList;

typedef struct _NhlRLNode_ _NhlRLNodeRec, *_NhlRLNode;
typedef struct _NhlRLHead_ _NhlRLHeadRec, *_NhlRLHead;

struct _NhlRLNode_ {
	NrmQuark		nameQ;
	NrmQuark		typeQ;
	_NhlArgVal		value;
	unsigned int		size;
	_NhlFreeFunc		free_func;
	_NhlRLNode		left;
	_NhlRLNode		right;
};

struct _NhlRLHead_ {
	int		num;
	NhlRLType	list_type;
	_NhlRLNode	list;
};

typedef struct _NhlExpArrayRec_ _NhlExpArrayRec, *_NhlExpArray;
struct	_NhlExpArrayRec_ {
	int		*num_dimensions;
	ng_size_t		**len_dimensions;
	NhlString	*type;
	unsigned int	*size;
	NhlPointer	*data;

/*	int		*num_elements;*/
	ng_size_t		*num_elements;
	int		num_dim_req;
	NrmQuark	type_req;
	unsigned int	size_req;
};

extern NhlBoolean _NhlRLToArgList(
#if	NhlNeedProto
	int		id,	/* RL list			*/
	NhlRLType	action,	/* type of RL action		*/
	_NhlArgList	args,	/* args <return>		*/
	int		*nargs	/* number of args <return>	*/
#endif
);

extern void _NhlDestroyRLList(
#if	NhlNeedProto
	void
#endif
);

extern NhlBoolean _NhlRLInsert(
#if	NhlNeedProto
	int		id,
	NhlRLType	type_action,
	int		nameQ,
	int		typeQ,
	_NhlArgVal	value,
	unsigned int	size,
	_NhlFreeFunc	free_func
#endif
);

#endif	/* _NResListP_H */
