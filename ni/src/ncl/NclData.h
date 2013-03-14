
/*
 *      $Id: NclData.h,v 1.14 2008-12-10 20:12:16 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
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
 *	Date:		Fri Oct 29 14:00:51 MDT 1993
 *
 *	Description:	
 */
#ifndef NclData_h
#define NclData_h

#include "NclDataDefs.h"
#ifdef NIO_LIB_ONLY
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/Callbacks.h>
#endif

typedef NhlErrorTypes (*NclDelParentFunction) (
#if NhlNeedProto
struct _NclObjRec *     /*theobj*/,
struct _NclObjRec * 	/*parent*/
#endif
);

typedef NhlErrorTypes (*NclAddParentFunction) (
#if NhlNeedProto
struct _NclObjRec *     /*theobj*/,
struct _NclObjRec *	/*parent*/
#endif
);

#define CREATED		01
#define DESTROYED	02
#define MODIFIED	04
#define MISSINGNOTIFY	3
#define HLUDESTROYED	5
#define ATTDESTROYED	6
#define COORDDESTROYED	7
#define HLUVALCHANGE	8


typedef struct _NclObjPart {
	struct _NclObjRec*	self;
	struct _NclObjClassRec *class_ptr;
	NclObjTypes obj_type;
	unsigned int obj_type_mask; /* some kind of bit mask to id object*/
	int	storage; /* tag to determine whether object belongs to a symbol or is expression result */
	int id;
	int is_constant;
	NclStatus   status;
	NclRefList                      *parents;
	int 	ref_count;
	 _NhlCBList                      cblist;
} NclObjPart;

typedef struct _NclObjRec {
	NclObjPart	obj;
}NclObjRec;



typedef void (*NclGenericFunction)(
#if	NhlNeedProto
NclObj	/*self*/
#endif
);


typedef NhlErrorTypes (*NclInitPartFunction)(
#if	NhlNeedProto
NclObjClass	/*self*/
#endif
);

typedef NhlErrorTypes (*NclInitClassFunction)(
#if	NhlNeedProto
void
#endif
);

typedef int (*NclSetStatusFunction)(
#if	NhlNeedProto
NclObj	/*self*/,
NclStatus /*requested*/
#endif
);

typedef NhlErrorTypes (*NclPrintSummaryFunction)
                      (NclObj obj, FILE	*fp);

typedef NhlErrorTypes (*NclPrintFunction)(
#if	NhlNeedProto
NclObj /*self*/,
FILE	*		/*fp*/
#endif
);
typedef struct _NclDataRec *(*NclCopyFunction)(
#if	NhlNeedProto
	struct _NclDataRec * /*self*/,
	NclScalar * /*new_missing*/
#endif
);

typedef NhlErrorTypes (*NclCallBack)(
#if	NhlNeedProto
void* 	/*obj_ref*/, /*   */
void*	/*user_data*/
#endif
);

typedef struct _NclCallBackList {
	NclCallBack	func;
	void*		user_data;
	struct _NclCallBackList *next;
}NclCallBackList;

typedef void *(*NclObtainCallDataFunc)(
#if NhlNeedProto
NclObj	/* obj */,
unsigned int /* type */
#endif
);

typedef struct _NclObjClassPart {	
	char 	*class_name;
	unsigned int obj_size;
	struct _NclObjClassRec *super_class;
	int	inited;
	
	NclGenericFunction	destroy;
	NclSetStatusFunction	set_status;
	NclInitPartFunction	initialize_part;
	NclInitClassFunction	initialize_class;

        NclAddParentFunction            add_parent;
        NclDelParentFunction            del_parent;
	NclPrintSummaryFunction print_summary;
	NclPrintFunction        print;
	NclCallBackList		*create_callback;
	NclCallBackList		*delete_callback;
	NclCallBackList		*modify_callback;
	NclObtainCallDataFunc	obtain_calldata;
}NclObjClassPart;

typedef struct _NclObjClassRec{
	NclObjClassPart	obj_class;
}NclObjClassRec;

typedef struct _NclDataPart {
	unsigned int data_type_mask; /* diffent from obj_type_mask which only
					has entries for obj's class heirarchy,
					This field communicates what type the
					object wants to be perceived as with
					respect to data operations.  Variables
					fill this in with the type of their 
					value field's obj_type and the variable
					mask*/  

}NclDataPart;

typedef struct _NclDataRec {
	NclObjPart      obj;
	NclDataPart	data;
}NclDataRec;


typedef struct _NclDataRec *(*NclSingleOperatorFunction)(
#if	NhlNeedProto
	NclData	/*self*/,
	NclData 	/*result*/
#endif
);

typedef void (*NclResetMissingValueFunction) (
#if	NhlNeedProto
	NclData	/*self*/,
	NclScalar * /*missing*/
#endif
);

typedef struct _NclDataRec *(*NclOperatorFunction)(
#if	NhlNeedProto
	NclData	/*self*/,
	NclData 	/*other*/,
	NclData	/*result*/
#endif
);
typedef struct _NclDataRec *(*NclCoerceFunction)(
#if	NhlNeedProto
	NclData	/*self*/,
	NclObjTypes 		/*coerce_to_obj*/,
	NclScalar *		/* new_missing*/
#endif
);
typedef struct _NclDataRec *(*NclReadSubSecFunction)(
#if	NhlNeedProto
	NclData	/*self*/,
	struct _NclSelectionRecord * /*selection*/,
	NclScalar* /*missing*/
#endif
);

typedef NhlErrorTypes (*NclReadThenWriteFunc)(
#if	NhlNeedProto
	NclData /*to_data*/,
	struct _NclSelectionRecord* /*to_selctions*/,
	NclData /*from_data*/,
	struct _NclSelectionRecord* /*from_selection*/
#endif
);

typedef NhlErrorTypes (*NclWriteSubSecFunction)(
#if	NhlNeedProto
	NclData	/*self*/,
	struct _NclSelectionRecord * /*selection*/,
	NclData 	/*value*/
#endif
);

typedef int (*NclIsMissingFunction) (
#if	NhlNeedProto
	NclData /*self*/,
	void *v_one
#endif
);

typedef int (*NclScalarFindFunction)(
#if 	NhlNeedProto
	NclData /*self*/,
	void * /*value*/
#endif
);

typedef int (*NclMonotonicTestFunction)(
#if	NhlNeedProto
	NclData /*self*/
#endif
);


typedef struct _NclDataClassPart {
	NclCopyFunction		dup;
	NclResetMissingValueFunction 	reset_mis;
	NclReadSubSecFunction   r_subsection;
	NclWriteSubSecFunction  w_subsection[2];
	NclReadThenWriteFunc 	r_then_w_subsection;
	NclCoerceFunction  	coerce[2];
	NclOperatorFunction	multiply[4];
	NclOperatorFunction	plus[4];
	NclOperatorFunction	minus[4];
	NclOperatorFunction	divide[4];	
	NclOperatorFunction	exponent[4];
	NclOperatorFunction	mod[4];
	NclOperatorFunction	mat[4];
	NclOperatorFunction	sel_lt[4];
	NclOperatorFunction	sel_gt[4];
	NclSingleOperatorFunction	ncl_not[2];
	NclSingleOperatorFunction	neg[2];
	NclOperatorFunction	gt[4];
	NclOperatorFunction	lt[4];
	NclOperatorFunction	ge[4];
	NclOperatorFunction	le[4];
	NclOperatorFunction	ne[4];
	NclOperatorFunction	eq[4];
	NclOperatorFunction	ncl_and[4];
	NclOperatorFunction	ncl_or[4];
	NclOperatorFunction	ncl_xor[4];
	NclIsMissingFunction	is_mis;
} NclDataClassPart;
 
typedef struct _NclDataClassRec{
	NclObjClassPart	obj_class;
	NclDataClassPart data_class;
}NclDataClassRec;

typedef NclDataClassRec *NclDataClass;

extern NclObjClass nclObjClass;
extern NclObjClass nclDataClass;

extern NclObjClassRec nclObjClassRec;
extern NclDataClassRec nclDataClassRec;

extern NhlErrorTypes _NclInitClass(
#if     NhlNeedProto
	NclObjClass	/* oc */
#endif
);
extern void _NclDestroyObj(
#if	NhlNeedProto
	NclObj	/*obj*/
#endif
);
extern int _NclSizeOf(
#if     NhlNeedProto
NclBasicDataTypes /*data_type*/
#endif
);

extern int _NclSetStatus(
#if 	NhlNeedProto
NclObj	/* obj */,
NclStatus /* requested*/
#endif
);

extern struct _NclObjRec * _NclObjCreate(
#if	NhlNeedProto
NclObj	/* inst */,
NclObjClass /* theclass */,
NclObjTypes /* obj_type */,
unsigned int /* obj_type_mask*/,
NclStatus /* status */
#endif
);
extern struct _NclDataRec * _NclDataCreate(
#if	NhlNeedProto
NclObj	/* inst */,
NclObjClass /* theclass */,
NclObjTypes /* obj_type */,
unsigned int /* obj_type_mask*/,
NclStatus /* status */
#endif
);

extern struct _NclObjRec *_NclGetObj(
#if  NhlNeedProto
int /*id*/
#endif
);
extern void _NclNumGetObjCals(
#if NhlNeedProto
FILE *fp
#endif
);
extern int _NclRegisterObj(
#if	NhlNeedProto
NclObj /*self*/
#endif
);
extern void _NclUnRegisterObj(
#if	NhlNeedProto
NclObj /*self*/
#endif
);

extern int _NclNumObjs(
#if     NhlNeedProto
void
#endif
);

extern void _NclObjsSize(
#if NhlNeedProto
FILE *fp
#endif
);

extern void _NclPrintUnfreedObjs(
#if     NhlNeedProto
FILE * /*fp*/
#endif
);


#endif /* NclData_h */
