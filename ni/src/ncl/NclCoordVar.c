

/*
 *      $Id: NclCoordVar.c,v 1.12 2008-12-10 20:12:16 dbrown Exp $
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
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "NclCoordVar.h"
#include "NclOneDValCoordData.h"

static NhlErrorTypes InitializeCoordVarClass(
#if NhlNeedProto
void
#endif
);
NclCoordVarClassRec nclCoordVarClassRec = {
	{
		"NclCoordVarClass",
		sizeof(NclCoordVarRec),
		(NclObjClass)&nclVarClassRec,
		0,
		(NclGenericFunction)NULL,
		(NclSetStatusFunction)NULL /*VarSetStatus*/,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)InitializeCoordVarClass,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
/* NclPrintSummaryFunction print_summary */ NULL,
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

NclObjClass nclCoordVarClass = (NclObjClass)&nclCoordVarClassRec;

static NhlErrorTypes InitializeCoordVarClass
#if NhlNeedProto
(void)
#else
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_CoordVar,
		(NclObjClass)&nclCoordVarClassRec
	);
	return(NhlNOERROR);
}



struct _NclVarRec *_NclCoordVarCreate
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
	NclMultiDValData tmp_md;
	void *tmp_val;
	NclCoordVar tmp_var;
	NclObjClass  the_class;

	if(inst == NULL) {
		tmp_var = (NclCoordVar)NclMalloc(sizeof(NclCoordVarRec));
	} else {
		tmp_var = (NclCoordVar)inst;
	}
	if(theclass != NULL) {
		the_class = theclass;
	} else {
		the_class = (NclObjClass)&nclCoordVarClassRec;
	}

	if(!(value->obj.obj_type_mask & NCL_COORD_MASK )) {
		tmp_val = (void*)NclMalloc(value->multidval.type->type_class.size * value->multidval.dim_sizes[0]);
		memcpy(tmp_val,value->multidval.val,value->multidval.type->type_class.size * value->multidval.dim_sizes[0]);
		tmp_md = _NclOneDValCoordDataCreate(
			NULL,
			nclOneDValCoordDataClass,
			Ncl_OneDValCoordData,
			Ncl_OneDValCoordData,
			tmp_val,
			(value->multidval.missing_value.has_missing ? &value->multidval.missing_value.value:NULL),
			value->multidval.n_dims,
			value->multidval.dim_sizes,
			TEMPORARY,
			value->multidval.sel_rec,
			value->multidval.type
		);
		if(value->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)value);
		}
		if(tmp_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclCoordVarCreate: Could not create coordinate variable");
			return(NULL);
		}
	} else if(value->obj.status == PERMANENT) {
		tmp_md = _NclCopyVal(value,NULL);
	} else {
		tmp_md = value;
	}

	return(_NclVarCreate((void*)tmp_var,the_class,obj_type,(obj_type_mask | Ncl_CoordVar),thesym,tmp_md,dim_info,att_id,coords,var_type,var_name,status));
}

