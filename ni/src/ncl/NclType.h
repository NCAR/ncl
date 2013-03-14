/*
 *      $Id$
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
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
 *	Date:		Fri Jan 27 18:24:21 MST 1995
 *
 *	Description:	
 */
#ifndef NclType_h
#define NclType_h
#include "NclData.h"

typedef NhlErrorTypes (*NclMatTypeOp)  (
#if	NhlNeedProto
void * /*result*/,
void * /* lhs */,
void * /* rhs */,
NclScalar * /* lhs_m */, 
NclScalar * /* rhs_m */,
int         /* ndimslhs */, 
ng_size_t * /* dimsizeslhs */, 
int         /* ndimsrhs */,
ng_size_t * /* dimsizesrhs */ 
#endif
);
typedef NhlErrorTypes (*NclTypeOp)  (
#if	NhlNeedProto
void * /*result*/,
void * /* lhs */,
void * /* rhs */,
NclScalar * /* lhs_m */, 
NclScalar * /* rhs_m */,
ng_size_t /* nlhs */, 
ng_size_t /* nrhs */
#endif
);

typedef NhlErrorTypes (*NclNumScalarCompareFunc) (
#if	NhlNeedProto
void * /*lhs */,
void * /*rhs */,
NclScalar * /*lhs_m*/,
NclScalar * /*rhs_m*/,
int /* digits */,
double* /* result */
#endif
);

typedef enum {
	NclNONMONO = 0,
	NclINCREASING = 01,
	NclDECREASING = 02
} NclMonoTypes;

typedef NclMonoTypes (*NclTypeMonotonicTestFunction) (
#if	NhlNeedProto
void * /*val*/,
NclScalar * /*val_m*/,
ng_size_t	/*nval*/
#endif
);

typedef struct _NclTypeClassRec * (*NclTypeOutType) (
#if	NhlNeedProto
void
#endif
);

/*
* Coercion is called on the target type
* and passed the from type. In this
* way old types do not need to be modified
* when new types are add. However, new types
* must "know" the types it can coerce from.
*/

typedef NhlErrorTypes (*NclTypeCoerceFunction) (
#if	NhlNeedProto
void*	/*result*/,
void*	/*from*/,
ng_size_t  /*n*/,
NclScalar* /*from_m*/,
NclScalar* /*to_m*/,
struct _NclTypeClassRec */*from*/
#endif
);

typedef NhlErrorTypes (*NclTypeResetMissing) (
#if	NhlNeedProto
void 	* /*val*/,
NclScalar * /*old_m*/,
NclScalar * /*new_m*/,
ng_size_t   /* nval */
#endif
);

typedef NhlErrorTypes (*NclTypePrint) (
#if	NhlNeedProto
FILE * /*fp */,
void	* /*val*/
#endif
);



typedef struct _NclTypeClassPart {
	NclObjTypes		type;
	NclBasicDataTypes	data_type;
	ng_size_t		size;
	char*			hlu_type_rep[2];
	NclScalar		default_mis;
	char*			format;
	NclTypePrint		print;
	NclTypeResetMissing	reset_mis;
	NclTypeCoerceFunction   coerce;
	NclTypeOp		multiply;
	NclTypeOutType		multiply_type;
	NclTypeOp		plus;
	NclTypeOutType		plus_type;
	NclTypeOp		minus;
	NclTypeOutType		minus_type;
	NclTypeOp		divide;
	NclTypeOutType		divide_type;
	NclTypeOp		exponent;
	NclTypeOutType		exponent_type;
	NclTypeOp		mod;
	NclTypeOutType		mod_type;
	NclMatTypeOp		mat;
	NclTypeOutType		mat_type;
	NclTypeOp		sel_lt;
	NclTypeOutType		sel_lt_type;
	NclTypeOp		sel_gt;
	NclTypeOutType		sel_gt_type;
	NclTypeOp		ncl_not;
	NclTypeOutType		not_type;
	NclTypeOp		neg;
	NclTypeOutType		neg_type;
	NclTypeOp		gt;
	NclTypeOutType		gt_type;
	NclTypeOp		lt;
	NclTypeOutType		lt_type;
	NclTypeOp		ge;
	NclTypeOutType		ge_type;
	NclTypeOp		le;
	NclTypeOutType		le_type;
	NclTypeOp		ne;
	NclTypeOutType		ne_type;
	NclTypeOp		eq;
	NclTypeOutType		eq_type;
	NclTypeOp		ncl_and;
	NclTypeOutType		and_type;
	NclTypeOp		ncl_or;
	NclTypeOutType		or_type;
	NclTypeOp		ncl_xor;
	NclTypeOutType		xor_type;
	NclNumScalarCompareFunc	cmpf;
	NclTypeMonotonicTestFunction	is_mono;
} NclTypeClassPart;

typedef struct _NclTypePart {
	char * foo;
} NclTypePart;

typedef struct _NclTypeClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
}NclTypeClassRec;

typedef struct _NclTypeRec {
	NclObjPart	obj;
	NclTypePart	type;
}NclTypeRec;

typedef NclTypeRec *NclType;
typedef NclTypeClassRec *NclTypeClass;
extern NclObjClass nclTypeClass;
extern NclTypeClassRec nclTypeClassRec;

NclType _NclTypeCreate(
#if     NhlNeedProto
NclObj /*inst*/,
NclObjClass /*theclass*/,
NclObjTypes /*obj_type*/, 
unsigned int /*obj_type_mask*/, 
NclStatus /*status*/
#endif
);

#endif /* NclType_h */
