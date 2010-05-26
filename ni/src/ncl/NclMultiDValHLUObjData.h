
/*
 *      $Id: NclMultiDValHLUObjData.h,v 1.8 2008-12-10 20:12:17 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jan 13 14:55:46 MST 1994
 *
 *	Description:	
 */
#ifndef NclMultiDValHLUObjData_h
#define NclMultiDValHLUObjData_h
#ifdef NIO_LIB_ONLY
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/Callbacks.h>
#endif
#include "defs.h"
#include "NclMultiDValData.h"

typedef struct _HLUMDCalRec {
        int parent_id;
        int index;
}HLUMDCalRec;

typedef struct _HLURefNameListNode {
	NclQuark vname;
	NclQuark aname;
	int count;
	struct _HLURefNameListNode *next;
}HLURefNameListNode;

typedef struct _HLURefTableNode {
	int id;
	struct _HLURefNameListNode *thelist;
	struct _HLURefTableNode *next;
}HLURefTableNode;

typedef struct _NclMultiDValHLUObjDataPart {
	_NhlCB *cbs;
	HLUMDCalRec **crecs;
}NclMultiDValHLUObjDataPart;

typedef struct _NclMultiDValHLUObjDataRec {
	NclObjPart	obj;
	NclDataPart	data;
	NclMultiDValDataPart multidval;
	NclMultiDValHLUObjDataPart multi_obj;
}NclMultiDValHLUObjDataRec;

typedef struct _NclMultiDValHLUObjDataClassPart {
	HLURefTableNode *ref_table;
}NclMultiDValHLUObjDataClassPart;

typedef struct _NclMultiDValHLUObjDataClassRec {
	NclObjClassPart	obj_class;
	NclDataClassPart data_class;
	NclMultiDValDataClassPart multid_class;
	NclMultiDValHLUObjDataClassPart multid_obj_class;
}NclMultiDValHLUObjDataClassRec;

typedef struct _NclMultiDValHLUObjDataRec* NclMultiDValHLUObjData;
typedef struct _NclMultiDValHLUObjDataClassRec* NclMultiDValHLUObjDataClass;


extern NclObjClass nclMultiDValHLUObjDataClass;
extern NclMultiDValHLUObjDataClassRec nclMultiDValHLUObjDataClassRec;

extern struct _NclMultiDValDataRec *_NclMultiDValHLUObjDataCreate(
#if	NhlNeedProto
NclObj          /* inst */,
NclObjClass     /* theclass */,
NclObjTypes     /* obj_type */,
unsigned int    /* obj_type_mask */,
void *          /* val */,
NclScalar *     /*missing_value*/,
int             /*n_dims*/,
ng_size_t *           /*dim_sizes*/,
NclStatus       /*status*/,
NclSelectionRecord * /*sel_rec*/
#endif
);



#endif /*NclMultiDValHLUObjData_h */
