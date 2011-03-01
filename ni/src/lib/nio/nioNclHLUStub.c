

/*
 *      $Id: nioNclHLUStub.c,v 1.1 2009-05-15 00:49:27 dbrown Exp $
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
 *	Date:		Thu Jan 13 15:04:25 MST 1994
 *
 *	Description:	
 */
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "NclHLUVar.h"
#include "NclHLUObj.h"
#include "NclCallBacksI.h"
#include "HLUSupport.h"

NclHLUVarClassRec nclHLUVarClassRec = {
	{
		"NclHLUVarClass",
		sizeof(NclHLUVarRec),
		(NclObjClass)&nclVarClassRec,
		0,
		(NclGenericFunction)NULL,
		(NclSetStatusFunction)NULL,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)NULL,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
/* NclPrintFunction print */	NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclRepValueFunc rep_val */		NULL,
/* NclGetValFunc get_val*/		NULL,
/* NclVarCoerceFunc */			NULL,
/* NclCopyVarFunc */			NULL,

/* NclAssignFunction write_func */	NULL,
/* NclAssignVarToVarFunction write_vv_func */	NULL,
/* NclReadFunction read_func */		NULL,
/* NclReadValueFunction read_func */	NULL,

/* NclReadAttribute read_att_func*/	NULL,
/* NclIsA is_att_func*/			NULL,
/* NclWriteAttribute write_att_func*/	NULL,

/* NclIsAFunc is_dim_func */		NULL,
/* NclReadDimension read_dim_func*/	NULL,
/* NclGetDimInfo read_dim_func*/	NULL,
/* NclWriteDimension write_dim_func*/	NULL,

/* NclIsAFunc is_coord_func */		NULL,
/* NclReadCoordinate read_coordinate*/	NULL,
/* NclWriteCoordinate write_coordinate*/ NULL
	},
	{
		NULL
	}
};

NclObjClass nclHLUVarClass = (NclObjClass)&nclHLUVarClassRec;


struct _NclVarRec *_NclHLUVarCreate
#if	NhlNeedProto
(NclVar inst,
NclObjClass theclass,
NclObjTypes obj_type,
unsigned int obj_type_mask,
struct _NclSymbol *thesym,
struct _NclMultiDValDataRec * value,
NclDimRec *dim_info,
int att_id,
int *coords,
NclVarTypes var_type,
char *var_name,
NclStatus status)
#else
(inst,theclass,obj_type,obj_type_mask,thesym,value,dim_info,att_id,coords,var_type,var_name,status)
	NclVar inst;
	NclObjClass theclass;
	NclObjTypes obj_type;
	unsigned int obj_type_mask;
	struct _NclSymbol *thesym;
	struct _NclMultiDValDataRec *value;
	NclDimRec *dim_info;
	int att_id;
	int *coords;
	NclVarTypes var_type;
	char *var_name;
	NclStatus status;
#endif
{
	NclHLUVar hvar = NULL;
	NclObjClass	cptr = (theclass ? theclass : nclHLUVarClass);
	int *ncl_hlu_ids;
	NhlArgVal udata;
	int i;
	NclMultiDValData tmp_md;

	return(NULL);
}

struct _NclHLUObjRec * _NclHLUObjCreate
#if	NhlNeedProto
(NclObj inst, 
 NclObjClass theclass, 
 NclObjTypes obj_type, 
 unsigned int obj_type_mask, 
 NclStatus status, 
 int id,
 int parentid,
 NhlClass class_ptr)
#else
(inst , theclass , obj_type ,obj_type_mask, status,id,parentid,class_ptr)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
int id;
int parentid;
NhlClass class_ptr;
#endif
{
	NclHLUObj tmp,ptmp;

	return(NULL);
}


NclMultiDValHLUObjDataClassRec nclMultiDValHLUObjDataClassRec = {
	{
/* char *class_name; 		*/	"MultiDValHLUObjData",
/* unsigned int obj_size;	*/	sizeof(NclMultiDValHLUObjDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclMultiDValDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	NULL,
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	NULL,
	(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
	/* NclPrintFunction print; 	*/	NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclGenericFunction dup; 	*/	NULL,
/* NclResetMissingValueFuction dup;	*/	NULL,
/* NclReadSubSecFunction r_subsection */ NULL,
/* NclReadSubSecFunction w_subsection */{
					NULL,NULL
					},
/* NclReadThenWriteSubFunc w_subsection */ NULL,
/* NclDataFunction coerce; 	*/	{NULL,NULL},
/* NclDataFunction multiply; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction plus; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction minus; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction divide; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction exponent; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction mod; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction mat; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction sel_lt; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction sel_gt; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction not; 	*/	{NULL,NULL},
/* NclDataFunction neg; 	*/	{NULL,NULL},
/* NclDataFunction gt; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction lt; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction ge; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction le; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction ne; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction eq; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction and;	 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction or; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction xor;		*/	{NULL,NULL,NULL,NULL},
/* NclIsMissingFunction    is_mis; */	NULL

	},
	{	
		NULL
	},
	{		
		NULL
	}
};

NclObjClass nclMultiDValHLUObjDataClass = (NclObjClass)&nclMultiDValHLUObjDataClassRec;

struct _NclMultiDValDataRec * _NclMultiDValHLUObjDataCreate
#if	NhlNeedProto
(NclObj inst,
 NclObjClass theclass,
 NclObjTypes obj_type,
 unsigned int obj_type_mask,
 void *val,
 NclScalar *missing_value,
 int n_dims, 
 ng_size_t *dim_sizes,
 NclStatus status,
 NclSelectionRecord *sel_rec)
#else
(inst,theclass,obj_type,obj_type_mask, val,missing_value,n_dims,dim_sizes,status,sel_rec)
NclObj inst ;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
ng_size_t *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
#endif
{
	
	return(NULL);
}
