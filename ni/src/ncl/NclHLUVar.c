

/*
 *      $Id: NclHLUVar.c,v 1.16 2010-01-11 21:36:19 dbrown Exp $
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
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include "DataSupport.h"
#include "NclHLUVar.h"
#include "NclHLUObj.h"
#include "NclCallBacksI.h"
#include "HLUSupport.h"

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
static void HLUVarDestroy
#if     NhlNeedProto
(struct _NclObjRec*     self)
#else
(self)
struct _NclObjRec*      self;
#endif
{
	int i;
	NclHLUVar self_var = (NclHLUVar)self;
	NclMultiDValData tmp_md = (NclMultiDValData)_NclGetObj(self_var->var.thevalue_id);

	if((self_var->var.var_type == NORMAL)||(self_var->var.var_type == HLUOBJ)) {
		_NhlCBDelete(self_var->hvar.cb);
		NhlFree(self_var->hvar.udata);
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(self_var->var.thesym != NULL) {
				_NclDelHLURef(((obj*)tmp_md->multidval.val)[i],self_var->var.var_quark,-1,i,self_var->var.thesym->level);
			} else {
				_NclDelHLURef(((obj*)tmp_md->multidval.val)[i],self_var->var.var_quark,-1,i,-1);
			}
		}
	}
	(*(self_var->obj.class_ptr->obj_class.super_class->obj_class.destroy))(self);

	
}

NclHLUVarClassRec nclHLUVarClassRec = {
	{
		"NclHLUVarClass",
		sizeof(NclHLUVarRec),
		(NclObjClass)&nclVarClassRec,
		0,
		(NclGenericFunction)HLUVarDestroy,
		(NclSetStatusFunction)NULL,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)InitializeHLUVarClass,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
/* NclPrintSummaryFunction print_summary */ NULL,
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
	NclMultiDValHLUObjData tmp_md;
	NclHLUObj tmp_ho;
        int i;
        
        tmp->obj.obj_id = obj->obj.id;
        tmp->obj.obj_type = NCLHLUVar;
	if((var->var.thesym != NULL)&&(var->var.thesym->level != 1)) {
                if(var->var.var_type == NORMAL) {
                        tmp->var.var_type = (NclApiVarTypes)NclAPIFUNCNORMAL;
                } else {
                        tmp->var.var_type = (NclApiVarTypes)var->var.var_type;
                }
        } else {
                tmp->var.var_type = (NclApiVarTypes)var->var.var_type;
        }
        tmp->var.var_quark = var->var.var_quark;
        tmp->var.n_dims = var->var.n_dims;
        for ( i = 0; i < var->var.n_dims; i++) {
                tmp->var.dim_sizes[i] = var->var.dim_info[i].dim_size;
                tmp->var.dim_quarks[i] = var->var.dim_info[i].dim_quark;
        }
	if(var->var.thevalue_id == -1) {
		NclFree(tmp);
		return(NULL);
	}
	tmp_md = (NclMultiDValHLUObjData)_NclGetObj(var->var.thevalue_id);
	if(tmp_md == NULL) {
		NclFree(tmp);
		return(NULL);
	}
	
	tmp->hlu.the_hlu_info = (NclHLUObjInfoRec*)NclMalloc(sizeof(NclHLUObjInfoRec)*tmp_md->multidval.totalelements);
	for(i = 0; i < tmp_md->multidval.totalelements;i++) {
		tmp_ho = (NclHLUObj)_NclGetObj(((int*)tmp_md->multidval.val)[i]);
		if(tmp_ho != NULL) {
			tmp->hlu.the_hlu_info[i].hlu_id = tmp_ho->hlu.hlu_id;
			tmp->hlu.the_hlu_info[i].hlu_name = tmp_ho->hlu.hlu_name;
			tmp->hlu.the_hlu_info[i].parent_hluobj_id = tmp_ho->hlu.parent_hluobj_id;
			tmp->hlu.the_hlu_info[i].class_ptr = tmp_ho->hlu.class_ptr;	
		} else {
			tmp->hlu.the_hlu_info[i].hlu_id = -1;	
			tmp->hlu.the_hlu_info[i].hlu_name = -1;	
			tmp->hlu.the_hlu_info[i].parent_hluobj_id= -1;	
			tmp->hlu.the_hlu_info[i].class_ptr = NULL;	
			
		}
	}
	
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
	return NhlNOERROR;
}

void _NclHLUVarValChange
#if     NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata,udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
	NclHLUUData *ud = (NclHLUUData*)udata.ptrval;
	NclHLUCbData *cb = (NclHLUCbData*)cbdata.ptrval;

	if (cb->prev_id > 0 && _NclGetObj(cb->prev_id) != NULL)
		_NclDelHLURef(cb->prev_id,ud->vq,ud->aq,cb->off,ud->level);
	if(!cb->kind) {
		_NclAddHLURef(cb->ncl_id,ud->vq,ud->aq,cb->off,ud->level);
	}
	
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
	NclObjClass	cptr = (theclass ? theclass : nclHLUVarClass);
	NhlArgVal udata;
	int i;
	NclMultiDValData tmp_md;

	if(inst != NULL) {
		hvar = (NclHLUVar) inst;
	} else {
		hvar = (NclHLUVar) NclMalloc(sizeof(NclHLUVarRec));
	}
	_NclVarCreate((NclVar)hvar,cptr,obj_type,obj_type_mask | Ncl_HLUVar,thesym,value,dim_info,att_id,coords,var_type,var_name,status);


	tmp_md = (NclMultiDValData)_NclGetObj(hvar->var.thevalue_id);
	if(((thesym != NULL)||(var_name != NULL))&&((var_type == NORMAL)||(var_type == HLUOBJ))) {
		udata.ptrval = NclMalloc(sizeof(NclHLUUData));
		if(thesym != NULL) {
			((NclHLUUData*)udata.ptrval)->vq =NrmStringToQuark(thesym->name);
		} else {
			((NclHLUUData*)udata.ptrval)->vq =NrmStringToQuark(var_name);
		}
	
		((NclHLUUData*)udata.ptrval)->aq = -1;
		if(thesym != NULL) {
			((NclHLUUData*)udata.ptrval)->level = thesym->level;
		} else {
			((NclHLUUData*)udata.ptrval)->level = -1;
		}
		hvar->hvar.cb = _NclAddCallback((NclObj)tmp_md,NULL,_NclHLUVarValChange,HLUVALCHANGE,&udata);
		hvar->hvar.udata = udata.ptrval;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(thesym != NULL) {
				_NclAddHLURef(((obj*)tmp_md->multidval.val)[i],((NclHLUUData*)udata.ptrval)->vq,-1,i,thesym->level);
			} else {
				_NclAddHLURef(((obj*)tmp_md->multidval.val)[i],((NclHLUUData*)udata.ptrval)->vq,-1,i,-1);
			}
		}
	}
	if(cptr == nclHLUVarClass) {
		_NclCallCallBacks((NclObj)hvar,CREATED);
	}
	return((NclVar)hvar);
}

