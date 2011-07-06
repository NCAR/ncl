

/*
 *      $Id: NclList.h,v 1.4 2010-04-14 21:29:47 huangwei Exp $
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
 *	Date:		Thu Jan 13 15:04:41 MST 1994
 *
 *	Description:	
 */
#ifndef NclList_h
#define NclList_h
#include "NclData.h"

typedef struct _NclListRec *NclList;
typedef struct _NclListClassRec *NclListClass;

extern struct _NclObjRec *_NclListCreate(
#if	NhlNeedProto
	struct _NclObjRec *	/* int */,
	struct _NclObjClassRec *	/* theclass */,
	NclObjTypes 	/* obj_type */,
	unsigned int 	/* obj_type_mask */,
	int 		/* list_type */
#endif
);


typedef NhlErrorTypes (*NclListPushFunction)(
#if     NhlNeedProto
NclObj /*thelistobj*/,
NclObj /*theobj*/
#endif
);

typedef NclObj (*NclListPopFunction)(
#if     NhlNeedProto
NclObj /*thelistobj*/
#endif
);

typedef NclList  (*NclListSelectFunction)(
#if     NhlNeedProto
NclObj /*thelistobj*/,
NclSelection* /*sel_ptr*/
#endif
);

typedef NhlErrorTypes (*NclListSetTypeFunction)(
#if     NhlNeedProto
NclObj /*thelistobj*/,
int	/*new_type*/
#endif
);
typedef int (*NclListGetTypeFunction)(
#if     NhlNeedProto
NclObj /*thelistobj*/
#endif
);

typedef int (*NclListGetNextFunction)(
#if	NhlNeedProto
NclObj /*thelistobj*/
#endif
);


typedef struct _NclListClassPart {
	NclListSetTypeFunction	set_type;
	NclListGetTypeFunction	get_type;
	NclListPushFunction	push;
	NclListPopFunction	pop;
	NclListSelectFunction	select;
	NclListGetNextFunction  get_next;
} NclListClassPart;

#define NCL_FIFO 01
#define NCL_LIFO 02
#define NCL_CONCAT 04
#define NCL_JOIN 010
#define NCL_VLEN 020
#define NCL_ITEM 040
#define NCL_STRUCT 0100
#define NCL_COMPOUND NCL_STRUCT

#define NCL_LIST_IDLE 1
#define NCL_LIST_SEQUENCING 0

typedef struct _NclListObjList {
        int obj_id;
	NclObjTypes orig_type;
	_NhlCB cb;
        struct _NclListObjList *next;
        struct _NclListObjList *prev;
}NclListObjList;

typedef struct _NclListPart {
        int     list_quark;
	int	list_type;
        struct _NclSymbol* thesym;
	int 	state;
	NclListObjList *current_item;
	NclListObjList *first;
	NclListObjList *last;
	int nelem;
	NclSelection *agg_sel_ptr;
}NclListPart;
 
typedef struct _NclListClassRec{
	NclObjClassPart	obj_class;
	NclListClassPart list_class;
}NclListClassRec;

typedef struct _NclListRec {
	NclObjPart      obj;
	NclListPart	list;
}NclListRec;

NhlErrorTypes ListPush(NclObj list,NclObj theobj);

extern NclObjClass nclListClass;

extern NclListClassRec nclListClassRec;

extern void ListItemDestroyNotify(NhlArgVal cbdata, NhlArgVal udata);
#endif /* NclList_h */
