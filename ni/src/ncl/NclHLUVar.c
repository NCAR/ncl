

/*
 *      $Id: NclHLUVar.c,v 1.5 1995-06-03 00:45:44 ethan Exp $
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
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include "DataSupport.h"
#include "NclHLUVar.h"
#include "NclCallBacksI.h"

static NhlErrorTypes InitializeHLUVarClass(
#if  NhlNeedProto
void
#endif
);

static void * HLUVarObtainCallData(
#if NhlNeedProto
NclObj /* obj */, 
unsigned int /* type */
#endif
);

NclHLUVarClassRec nclHLUVarClassRec = {
	{
		"NclHLUVarClass",
		sizeof(NclHLUVarRec),
		(NclObjClass)&nclVarClassRec,
		0,
		(NclGenericFunction)NULL,
		(NclSetStatusFunction)NULL /*VarSetStatus*/,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)InitializeHLUVarClass,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
/* NclPrintFunction print */	NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   HLUVarObtainCallData
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

static void *HLUVarObtainCallData
#if NhlNeedProto
(NclObj obj, unsigned int type)
#else
(obj, type)
NclObj obj;
unsigned int type;
#endif
{
        NclHLUVarClassInfo  *tmp = NclMalloc(sizeof(NclHLUVarClassInfo));
        NclHLUVar var = (NclHLUVar)obj;
        int i;
        
        tmp->obj.obj_id = obj->obj.id;
        tmp->obj.obj_type = NCLHLUVar;
        tmp->var.var_type = (NclApiVarTypes)var->var.var_type;
        tmp->var.var_quark = var->var.var_quark;
        tmp->var.n_dims = var->var.n_dims;
        for ( i = 0; i < var->var.n_dims; i++) {
                tmp->var.dim_sizes[i] = var->var.dim_info[i].dim_size;
                tmp->var.dim_quarks[i] = var->var.dim_info[i].dim_quark;
        }
	tmp->hlu.foo = 0;
        return((void*)tmp);
}


static NhlErrorTypes InitializeHLUVarClass
#if NhlNeedProto
(void)
#else 
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_HLUVar,
		(NclObjClass)&nclHLUVarClassRec
	);
}


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

	if(inst != NULL) {
		hvar = (NclHLUVar) inst;
	} else {
		hvar = (NclHLUVar) NclMalloc(sizeof(NclHLUVarRec));
	}
	if(theclass != NULL) {
		_NclVarCreate((NclVar)hvar,theclass,obj_type,obj_type_mask | Ncl_HLUVar,thesym,value,dim_info,att_id,coords,var_type,var_name,status);
	} else {
		_NclVarCreate((NclVar)hvar,(NclObjClass)&nclHLUVarClassRec,obj_type,obj_type_mask | Ncl_HLUVar,thesym,value,dim_info,att_id,coords,var_type,var_name,status);
	}
	if(theclass == NULL) {
		_NclCallCallBacks((NclObj)hvar,CREATED);
	}
	return((NclVar)hvar);
}

