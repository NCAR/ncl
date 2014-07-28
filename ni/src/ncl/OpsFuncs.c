#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/Callbacks.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/ncargC.h>
#include "defs.h"
#include <errno.h>
#include "Symbol.h"
#include "NclList.h"
#include "NclDataDefs.h"
#include "Machine.h"
#include "NclFile.h"
#include "NclVar.h"
#include "NclCoordVar.h"
#include "NclHLUObj.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "ListSupport.h"
#include "HLUSupport.h"
#include "NclMdInc.h"
#include "parser.h"
#include "OpsList.h"
#include "ApiRecords.h"
#include "TypeSupport.h"
#include "NclBuiltInSupport.h"


int defaultapp_hluobj_id = -1;

static void DefaultAppChangeCB
#if     NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata,udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
	int hlu_def_parent_id = (int)cbdata.lngval;
	int nclhluobj_id = (int)udata.lngval;
	NclHLUObj hlu_obj;

	hlu_obj = (NclHLUObj)_NclGetObj(nclhluobj_id);

	if(hlu_obj == NULL)
		return;

	/*
	 * if this hlu_obj is the HLU Default Parent, then save the
	 * ncl id of this hlu_obj in defaultapp_hluobj_id.
	 */
	if(hlu_obj->hlu.hlu_id == hlu_def_parent_id)
		defaultapp_hluobj_id = nclhluobj_id;
	/*
	 * This hlu_obj is not the HLU Default Parent, but is currently
	 * saved in the defaultapp_hluobj_id - so reset defaultapp_hluobj_id.
	 */
	else if(nclhluobj_id == defaultapp_hluobj_id) {
		defaultapp_hluobj_id = -1;
#if 0
		if(hlu_obj->obj.status != PERMANENT) 
			hlu_obj->obj.status = TEMPORARY;
#endif

	}

	return;
}



NhlErrorTypes _NclDualOp
#if	NhlNeedProto
(NclStackEntry lhs, NclStackEntry rhs,NclStackEntry *result,int operation)
#else
(lhs, rhs,result,operation)
NclStackEntry lhs;
NclStackEntry rhs;
NclStackEntry *result;
int operation;
#endif
{
	NclMultiDValData lhs_data_obj = NULL;
	NclMultiDValData rhs_data_obj = NULL;
	NclMultiDValData coerce_res = NULL;
	NhlErrorTypes ret = NhlNOERROR;
	int lhs_type;
	int rhs_type;


	if(lhs.kind == NclStk_VAL) {
		lhs_type = lhs.u.data_obj->multidval.type->type_class.type;
		lhs_data_obj = lhs.u.data_obj;
	} else if(lhs.kind == NclStk_VAR) {
		lhs_data_obj = _NclVarValueRead(lhs.u.data_var,NULL,NULL);
		lhs_type = lhs_data_obj->multidval.type->type_class.type;
	} else {
		return(NhlFATAL);
	}

	if(rhs.kind == NclStk_VAL) {
		rhs_type = rhs.u.data_obj->multidval.type->type_class.type;
		rhs_data_obj = rhs.u.data_obj;
	} else if(rhs.kind == NclStk_VAR) {
		rhs_data_obj = _NclVarValueRead(rhs.u.data_var,NULL,NULL);
		rhs_type = rhs_data_obj->multidval.type->type_class.type;
	} else {
		return(NhlFATAL);
	}

	if(lhs_type < rhs_type) {

/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
		coerce_res = _NclCoerceData(rhs_data_obj,(NclObjTypes)((int)lhs_type & NCL_VAL_TYPE_MASK),NULL);
		if(coerce_res == NULL) {
/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
			coerce_res = _NclCoerceData(lhs_data_obj,(NclObjTypes)((int)rhs_type & NCL_VAL_TYPE_MASK),NULL);
			if(coerce_res == NULL) {
/*
* Error message needed
*/
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce values for operation");
					return(NhlFATAL);
			} else {
				lhs_data_obj = coerce_res;
			}
		} else {
			rhs_data_obj = coerce_res;
		}
	} else if(lhs_type > rhs_type) {
/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
		coerce_res = _NclCoerceData(lhs_data_obj,(NclObjTypes)(rhs_type & NCL_VAL_TYPE_MASK),NULL);
		if(coerce_res == NULL) {
/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
			coerce_res = _NclCoerceData(rhs_data_obj,(NclObjTypes)(lhs_type & NCL_VAL_TYPE_MASK),NULL);
			if(coerce_res == NULL) {
/*
* Error message needed
*/
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce values for operation");
					return(NhlFATAL);
			} else {
				rhs_data_obj = coerce_res;
			}
		} else {
			lhs_data_obj = coerce_res;
		}
	}
	if((lhs_data_obj != NULL)&&(rhs_data_obj != NULL)) {

		if(result->u.data_obj == NULL) {
			if(((lhs_data_obj->multidval.kind != SCALAR)||(lhs_data_obj->multidval.kind == rhs_data_obj->multidval.kind))&&((lhs_data_obj->obj.status == TEMPORARY)||(lhs.u.data_obj->obj.status == TEMPORARY))) {
				result->u.data_obj = lhs_data_obj;
			} else if(((rhs_data_obj->multidval.kind != SCALAR)||(lhs_data_obj->multidval.kind == rhs_data_obj->multidval.kind))&&((rhs_data_obj->obj.status == TEMPORARY)||(rhs.u.data_obj->obj.status == TEMPORARY))) {
				result->u.data_obj = rhs_data_obj;
			} else {
				result->u.data_obj = NULL;
			}
		} 
		ret = _NclCallDualOp(lhs_data_obj,rhs_data_obj,operation,(NclObj*)&(result->u.data_obj));
		if(result->u.data_obj != NULL)
			result->kind = NclStk_VAL;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unexpected error in operation");
		return(NhlFATAL);
	}



        if((lhs.kind == NclStk_VAL)&&(result->u.data_obj != NULL)) {
		if(lhs_data_obj->obj.id != result->u.data_obj->obj.id) {
			if(lhs_data_obj != lhs.u.data_obj) {
				if(lhs_data_obj->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)lhs_data_obj);
				}
			}
			if(lhs.u.data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)lhs.u.data_obj);
			}
		} else if(lhs_data_obj != lhs.u.data_obj) {
			if(lhs.u.data_obj->obj.status != PERMANENT) {
                                _NclDestroyObj((NclObj)lhs.u.data_obj);
                        }
		}	
        } else if((lhs.kind == NclStk_VAR)&&(result->u.data_obj != NULL)) {
		if(lhs_data_obj->obj.id != result->u.data_obj->obj.id) {
			if(lhs_data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)lhs_data_obj);
			}
			if(lhs.u.data_var->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)lhs.u.data_var);
			}
		} else {
			if(lhs.u.data_var->obj.status != PERMANENT) {
				if(result->u.data_obj->obj.id == lhs.u.data_var->var.thevalue_id) {
					(void)_NclStripVarData(lhs.u.data_var);
					_NclDestroyObj((NclObj)lhs.u.data_var);
				} else {
					_NclDestroyObj((NclObj)lhs.u.data_var);
				}
			}
		}
        } else if(result->u.data_obj == NULL){
		if(lhs.kind == NclStk_VAR) {
			if(lhs_data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)lhs_data_obj);
			}
			if(lhs.u.data_var->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)lhs.u.data_var);
			} 
		} else {
			if(lhs_data_obj != lhs.u.data_obj) {
				if(lhs_data_obj->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)lhs_data_obj);
				}
			}
			if(lhs.u.data_obj->obj.status != PERMANENT) {
                                _NclDestroyObj((NclObj)lhs.u.data_obj);
                        }
		}
	}
        if((rhs.kind == NclStk_VAL)&&(result->u.data_obj != NULL)) {
		if(rhs_data_obj->obj.id != result->u.data_obj->obj.id) {
			if(rhs_data_obj != rhs.u.data_obj) {
				if(rhs_data_obj->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)rhs_data_obj);
				}
			}
			if(rhs.u.data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs.u.data_obj);
			}
		} else if(rhs_data_obj != rhs.u.data_obj) {
			if(rhs.u.data_obj->obj.status != PERMANENT) {
                                _NclDestroyObj((NclObj)rhs.u.data_obj);
                        }
		}	
        } else if((rhs.kind == NclStk_VAR)&&(result->u.data_obj != NULL)) {
		if(rhs_data_obj->obj.id != result->u.data_obj->obj.id) {
			if(rhs_data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_data_obj);
			}
			if(rhs.u.data_var->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs.u.data_var);
			}
		} else {
			if(rhs.u.data_var->obj.status != PERMANENT) {
				if(result->u.data_obj->obj.id == rhs.u.data_var->var.thevalue_id) {
					(void)_NclStripVarData(rhs.u.data_var);
					_NclDestroyObj((NclObj)rhs.u.data_var);
				} else {
					_NclDestroyObj((NclObj)rhs.u.data_var);
				}
			}
		}
        } else if(result->u.data_obj == NULL){
		if(rhs.kind == NclStk_VAR) {
			if(rhs_data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_data_obj);
			}
			if(rhs.u.data_var->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs.u.data_var);
			} 
		} else {
			if(rhs_data_obj != rhs.u.data_obj) {
				if(rhs_data_obj->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)rhs_data_obj);
				}
			}
			if(rhs.u.data_obj->obj.status != PERMANENT) {
                                _NclDestroyObj((NclObj)rhs.u.data_obj);
                        }
		}
	}


	return(ret);
}


NhlErrorTypes _NclMonoOp
#if	NhlNeedProto
(NclStackEntry operand, NclStackEntry *result, int operation)
#else
(operand, result,operation)
NclStackEntry operand;
NclStackEntry *result;
int operation;
#endif
{
	NclMultiDValData operand_md;
	NhlErrorTypes ret = NhlNOERROR;
	
	
        if(operand.kind == NclStk_VAL) {
		operand_md = operand.u.data_obj;
	} else if(operand.kind == NclStk_VAR) {
		operand_md = _NclVarValueRead(operand.u.data_var,NULL,NULL);
        } else {
                return(NhlFATAL);
        }

	ret = _NclCallMonoOp(operand_md,(NclObj*)&(result->u.data_obj),operation);
	if(result->u.data_obj != NULL)
		result->kind = NclStk_VAL;

	if(operand_md->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)operand_md);
	} else if((operand.kind == NclStk_VAR)&&(operand.u.data_var->obj.status != PERMANENT)) {
		_NclDestroyObj((NclObj)operand_md);
	}

        return(ret);
}


NhlErrorTypes _NclBuildArray
#if	NhlNeedProto
(int n_items,NclStackEntry *result)
#else
(n_items,result)
	int n_items;
	NclStackEntry *result;
#endif
{
	NclStackEntry data;
	int partsize;
	int items_left = n_items;
	logical tmp;
	void *value;
	char *ptr;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	NclMultiDValData theobj,coerce_res = NULL;
	NclStackEntry *data_ptr;
	NclObjTypes result_type ;
	NclObjTypes obj_type ;
	int must_be_numeric = 1,i,j;
	ng_size_t ndims;
	NclScalar *mis_ptr = NULL,themissing;
	int still_no_missing = 1;

	
	if( n_items == 1) {
		data = _NclPop();
		switch(data.kind) {
		case NclStk_VAL:
			result->kind = NclStk_VAL;
			result->u.data_obj = data.u.data_obj;
			return(NhlNOERROR);
		case NclStk_VAR:
			result->kind = NclStk_VAL;
			if(data.u.data_var->obj.status != PERMANENT) {
				result->u.data_obj = _NclStripVarData(data.u.data_var);
				_NclDestroyObj((NclObj)data.u.data_var);
			} else {
				result->u.data_obj = _NclCopyVal(_NclVarValueRead(data.u.data_var,NULL,NULL),NULL);
			}
			return(NhlNOERROR);
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
			return (NhlFATAL);
		}
	}
/*
* First element determines whether the type of the result array is numerci
* or textual
*/
	data_ptr = _NclPeek(0);
	if(data_ptr->kind == NclStk_VAL) {	
		obj_type = data_ptr->u.data_obj->multidval.type->type_class.type;
		ndims = data_ptr->u.data_obj->multidval.n_dims;
		for(i = 0; i < ndims; i++ ) {
			dim_sizes[i] = data_ptr->u.data_obj->multidval.dim_sizes[i];
		} 
	} else if(data_ptr->kind == NclStk_VAR) {
		obj_type = _NclGetVarRepValue(data_ptr->u.data_var);
		ndims = data_ptr->u.data_var->var.n_dims;
		for(i = 0; i < ndims; i++ ) {
			dim_sizes[i] = data_ptr->u.data_var->var.dim_info[i].dim_size;
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type, can't continue");
		_NclCleanUpStack(n_items);
		return(NhlFATAL);
	}
	if(obj_type & (NCL_SNUMERIC_TYPE_MASK | Ncl_Typelogical)) {
		must_be_numeric =1;
		result_type = (NclObjTypes)((int)obj_type & (NCL_SNUMERIC_TYPE_MASK | (int)Ncl_Typelogical));
	} else if(obj_type & NCL_CHARSTR_TYPE_MASK) {
		must_be_numeric =0;
		result_type = (NclObjTypes)((int)obj_type & NCL_CHARSTR_TYPE_MASK);
	} else if(obj_type & Ncl_Typeobj) {
		must_be_numeric =-1;
		result_type = Ncl_Typeobj;
	} else if(obj_type & Ncl_Typegroup) {
		must_be_numeric =-2;
		result_type = Ncl_Typegroup;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type or undefined element, can't continue");
		_NclCleanUpStack(n_items);
		return(NhlFATAL);
	}

/*
* The following loop figures out what the final type of the result array
* should be so the appropriate space can be allocated and the elements not
* of this type can be coerced into the correct type
*/	
	for(i = 1; i< n_items; i++) {
		data_ptr = _NclPeek(i);
		if(data_ptr->kind == NclStk_VAL) {	
			obj_type = data_ptr->u.data_obj->multidval.type->type_class.type;
			if(ndims == data_ptr->u.data_obj->multidval.n_dims) {
				for(j = 0; j < ndims; j++) {
					if(dim_sizes[j] != data_ptr->u.data_obj->multidval.dim_sizes[j]) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: each element of a literal array must have the same dimension sizes, at least one item doesn't");
						_NclCleanUpStack(n_items);
						return(NhlFATAL);
					}
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: each element of a literal array must have the same number of dimensions");
				_NclCleanUpStack(n_items);
				return(NhlFATAL);
			}
		} else if(data_ptr->kind == NclStk_VAR) {
			obj_type = _NclGetVarRepValue(data_ptr->u.data_var);
			if(ndims == data_ptr->u.data_var->var.n_dims) {
				for(j = 0; j < ndims; j++) {
					if(dim_sizes[j] != data_ptr->u.data_var->var.dim_info[j].dim_size) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: each element of a literal array must have the same dimension sizes, at least one item doesn't");
						_NclCleanUpStack(n_items);
						return(NhlFATAL);
					}
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: each element of a literal array must have the same number of dimensions");
				_NclCleanUpStack(n_items);
				return(NhlFATAL);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type, can't continue");
			_NclCleanUpStack(n_items);
			return(NhlFATAL);
		}
		if((must_be_numeric==1)&&
			( obj_type & (NCL_SNUMERIC_TYPE_MASK | Ncl_Typelogical))) {
			if ((result_type & Ncl_Typelogical) || (obj_type & Ncl_Typelogical)) {
				result_type = Ncl_Typelogical;
			}
			else if (result_type > (NclObjTypes)((int)obj_type & (NCL_SNUMERIC_TYPE_MASK | Ncl_Typelogical))) {
				result_type = (NclObjTypes)((int)obj_type & (NCL_SNUMERIC_TYPE_MASK | (int)Ncl_Typelogical));
			}
		} else if((must_be_numeric == 0)&&
			(obj_type & NCL_CHARSTR_TYPE_MASK)) {
			if(result_type > (obj_type & NCL_CHARSTR_TYPE_MASK)) {
				result_type = (NclObjTypes)((int)obj_type & NCL_CHARSTR_TYPE_MASK);
			}
		} else if((must_be_numeric == -1)&&
			(obj_type & Ncl_Typeobj )) {
			result_type = obj_type;
		} else if((must_be_numeric == -2)&&
			(obj_type & Ncl_Typegroup )) {
			result_type = obj_type;
		} else {
/*
* May need to say something different now that objects are done in this
* way too
*/
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: can not combine character or string types with numeric types, can't continue");
			_NclCleanUpStack(n_items);
			return(NhlFATAL);
		}
	}
/*
* By the time you get here you know the stack is guarenteed to contain either 
* numeric values that can be coerced or textual values that can be coerced. 
* Still the  possibility that dimensions numbers and sizes of elements don't 
* match
*/
/*
	data = _NclPop();
*/
	data_ptr = _NclPeek(0);
	items_left--;
	if(data_ptr->kind == NclStk_VAL) {
		theobj = (NclMultiDValData)data_ptr->u.data_obj;
		if(!(theobj->multidval.type->type_class.type & result_type)) {
			coerce_res = _NclCoerceData(theobj,result_type,NULL);
			if(coerce_res == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
			} else if(coerce_res->multidval.missing_value.has_missing) {
				still_no_missing = 0;
/*
* I do this so I can just pass mis_ptr in regardless later on. It
* will be NULL if none of the input has missing values. other wise
* it will be set to the correct missing value.
*/
				mis_ptr = &themissing;
				themissing = coerce_res->multidval.missing_value.value;
			}
/*
			if(theobj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)theobj);
			}
*/
			theobj = coerce_res;
		} else if(theobj->multidval.missing_value.has_missing) {
			still_no_missing = 0;
			mis_ptr = &themissing;
			themissing = theobj->multidval.missing_value.value;
		}
	} else if(data_ptr->kind == NclStk_VAR){
		obj_type = _NclGetVarRepValue(data_ptr->u.data_var);	
		if(!(obj_type & result_type)) {
			theobj = _NclCoerceVar(data_ptr->u.data_var,result_type,NULL);
			if(theobj == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
				_NclCleanUpStack(items_left);
				return(NhlFATAL);
			} else if(theobj->multidval.missing_value.has_missing){
				still_no_missing = 0;
/*
* I do this so I can just pass mis_ptr in regardless later on. It
* will be NULL if none of the input has missing values. other wise
* it will be set to the correct missing value.
*/
				mis_ptr = &themissing;
				themissing = theobj->multidval.missing_value.value;
			}
/*
			if(data_ptr->u.data_var->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)data_ptr->u.data_var);
			}
*/
		} else {
/*
			if(data_ptr->u.data_var->obj.status == PERMANENT) {
*/
				theobj = _NclVarValueRead(data_ptr->u.data_var,NULL,NULL);
/*
			} else {
				theobj = _NclStripVarData(data_ptr->u.data_var);
				_NclDestroyObj((NclObj)data_ptr->u.data_var);
			}
*/
			if(theobj == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
				_NclCleanUpStack(items_left);
				return(NhlFATAL);
			} else if(theobj->multidval.missing_value.has_missing){
/*
* I do this so I can just pass mis_ptr in regardless later on. It
* will be NULL if none of the input has missing values. other wise
* it will be set to the correct missing value.
*/
				still_no_missing = 0;
				mis_ptr = &themissing;
				themissing = theobj->multidval.missing_value.value;
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: unknown stack data type");
		_NclCleanUpStack(items_left);
		return(NhlFATAL);
	}
	if(theobj->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: Arrays of files are not yet supported");
		result->kind = NclStk_NOVAL;
		result->u.data_obj = NULL; 
		_NclCleanUpStack(items_left);
		return(NhlFATAL);
	}


	partsize = theobj->multidval.totalsize;

/*
* ------------->Need check for exceeding maximum dimensions <----------
*/
	for(i =0; i< theobj->multidval.n_dims; i++) {
/*
* -------------> Also need to rearrage dimension info <---------------
*/
		dim_sizes[i+1] = theobj->multidval.dim_sizes[i];
	}
	dim_sizes[0] = n_items;
	if(theobj->multidval.kind == MULTID) {
		ndims = theobj->multidval.n_dims +1;
	} else {
		ndims = 1;
	}
	value = (void*)NclMalloc((unsigned)partsize*n_items);
	if(value == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray : Memory allocation failure\n");
		result->kind = NclStk_NOVAL;
		result->u.data_obj = NULL; 
		NclFree(value);
		_NclCleanUpStack(items_left);
		return(NhlFATAL);
	}
	ptr = (char*)value;
	memcpy(ptr,(char*)theobj->multidval.val,partsize);
	ptr += partsize;
/*
	if(theobj->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)theobj);
	}
*/

	coerce_res = NULL;
	for(i = 1; i< n_items; i++) {
		data_ptr = _NclPeek(i);
/*
		data = _NclPop();
*/
		items_left--;
		if(data_ptr->kind == NclStk_VAL) {
			theobj = (NclMultiDValData)data_ptr->u.data_obj;
			if(theobj->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: Arrays of files are not yet supported");
				result->kind = NclStk_NOVAL;
				result->u.data_obj = NULL; 
				NclFree(value);
				_NclCleanUpStack(items_left);
				return(NhlFATAL);
			}
			if(!(theobj->multidval.type->type_class.type & result_type)) {
				coerce_res = _NclCoerceData(theobj,result_type,mis_ptr);
				if(coerce_res == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
					_NclCleanUpStack(items_left);
					NclFree(value);
					return(NhlFATAL);
				} else if((still_no_missing)	
					&&(coerce_res->multidval.missing_value.has_missing)) {
					still_no_missing = 0;
					mis_ptr = &themissing;
					themissing = theobj->multidval.missing_value.value;
				}
/*
				if(theobj->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)theobj);
				}
*/
				theobj = coerce_res;
			} else if((theobj->multidval.missing_value.has_missing)&&(still_no_missing)) {
				still_no_missing = 0;
				mis_ptr = &themissing;
				themissing = theobj->multidval.missing_value.value;
			} else if((theobj->multidval.missing_value.has_missing)&&(!still_no_missing)){
/*
* This is the case where the object is already the correct type but
* needs to have the missing value reset. ResetMissingValue should only be
* called if the object is TEMPORARY otherwise CoerceData Needs to be
* used to convert the missing values.
*/
				if(theobj->obj.status != TEMPORARY) {
/*
* Since theobj is not TEMPORARY then it is referenced somewhere else and the
* following will not cause the pointer to be lost.
*/
					_Ncleq(theobj->multidval.type,&tmp,&theobj->multidval.missing_value.value,mis_ptr,NULL,NULL,1,1);
					if(tmp) {
						coerce_res = _NclCopyVal(theobj,mis_ptr);
/*
					if(theobj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)theobj);
					}
*/
						theobj = coerce_res;
					}
				} else {
					if(mis_ptr != NULL) {
						_NclResetMissingValue(theobj,mis_ptr);
					}
				}
			}
		} else if(data_ptr->kind == NclStk_VAR){
			obj_type = _NclGetVarRepValue(data_ptr->u.data_var);	
			if(!(obj_type & result_type)) {
				theobj = _NclCoerceVar(data_ptr->u.data_var,result_type,mis_ptr);
/*
				if(data_ptr->u.data_var->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)data_ptr->u.data_var);
				}
*/
				if(theobj == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
					_NclCleanUpStack(items_left);
					NclFree(value);
					return(NhlFATAL);
				} else if((still_no_missing)&&
					(theobj->multidval.missing_value.has_missing)) {
					still_no_missing = 0;
					mis_ptr = &themissing;
					themissing = theobj->multidval.missing_value.value;
				}
			} else {
/*
				if(data_ptr->u.data_var->obj.status == PERMANENT) {
*/
					theobj = _NclVarValueRead(data_ptr->u.data_var,NULL,NULL);
/*
				} else {
					theobj = _NclStripVarData(data_ptr->u.data_var);
					_NclDestroyObj((NclObj)data_ptr->u.data_var);
				}
*/
				if(theobj == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
					_NclCleanUpStack(items_left);
					NclFree(value);
					return(NhlFATAL);
				} else if((still_no_missing)&&
					(theobj->multidval.missing_value.has_missing)) {
					still_no_missing = 0;
					mis_ptr = &themissing;
					themissing = theobj->multidval.missing_value.value;
				} else if((theobj->multidval.missing_value.has_missing)&&(!still_no_missing)) {
					if(theobj->obj.status == TEMPORARY) {
						if(mis_ptr != NULL) {
							_NclResetMissingValue(theobj,mis_ptr);
						}
					} else {
						_Ncleq(theobj->multidval.type,&tmp,&theobj->multidval.missing_value.value,mis_ptr,NULL,NULL,1,1);

						if(tmp) {
							coerce_res = _NclCopyVal(theobj,mis_ptr);
/*
						if(theobj->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)theobj);
						}
*/
							theobj = coerce_res;
						}
					}
				}
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: unknown stack data type");
			_NclCleanUpStack(items_left);
			NclFree(value);
			return(NhlFATAL);
		}
		memcpy(ptr,(char*)theobj->multidval.val,partsize);
		ptr += partsize;
		if((coerce_res != NULL)&&(coerce_res->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)coerce_res);
			coerce_res  = NULL;
		}
			
/*
		if(theobj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)theobj);
		} 
*/
	}
	result->kind = NclStk_VAL;
/*
*
* ------------> stilll need to handle dim info
*/
	if((result_type & Ncl_Typeobj)&&(must_be_numeric == -1)) {
		result->u.data_obj = _NclMultiDValHLUObjDataCreate(NULL,NULL,Ncl_MultiDValHLUObjData,0,value,mis_ptr,ndims,dim_sizes,TEMPORARY,NULL);
	} else {
		result->u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,value,mis_ptr,ndims,dim_sizes,TEMPORARY,NULL,_NclTypeEnumToTypeClass(result_type));
	}
	for(i = 0; i< n_items; i++) {
		data = _NclPop();
		if(data.u.data_obj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)data.u.data_obj);
		}
	}
	if(result->u.data_obj != NULL) 
		return(NhlNOERROR);
	else 
		return(NhlFATAL);
}

NhlErrorTypes _NclBuildListVar
#if	NhlNeedProto
(int n_items,NclStackEntry *result)
#else
(n_items,result)
	int n_items;
	NclStackEntry *result;
#endif
{
	NclStackEntry data;
	int i;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	int ndims = 1;

	NclList thelist;
	obj *id;

	thelist =(NclList)_NclListCreate(NULL,NULL,0,0,NCL_FIFO);
	id = (obj*)NclMalloc(sizeof(obj));
	*id = thelist->obj.id;
	_NclListSetType((NclObj)thelist,NCL_FIFO);

        dim_sizes[0] = 1;
	result->kind = NclStk_VAL;
	result->u.data_obj = _NclMultiDVallistDataCreate(NULL,NULL,Ncl_MultiDVallistData,
				(Ncl_List | Ncl_MultiDVallistData | Ncl_ListVar | Ncl_Typelist),id,NULL,
				ndims,dim_sizes,TEMPORARY,NULL);

	for(i = 0; i < n_items; i++)
	{
		data = _NclPop();
		ListAppend((NclObj)thelist, (NclObj)(data.u.data_obj)); 
	}

	_NclPlaceReturn(*result);

	if(result->u.data_obj != NULL) 
		return(NhlNOERROR);
	else 
		return(NhlFATAL);
}

NclStackEntry _NclCreateAList(const char *buffer)
{
	NclStackEntry data;

	NclList tmp_list;
	obj *id;
	ng_size_t one = 1;
	int list_type = (int) (NCL_FIFO);
	
	data.kind = NclStk_VAL;

        if(0 == strcmp("join",buffer))
	{
		list_type = (int) (NCL_JOIN | NCL_FIFO);
	}
        else if(0 == strcmp("concat",buffer))
	{
		list_type = (int) (NCL_CONCAT | NCL_FIFO);
	}
        else if(0 == strcmp("fifo",buffer))
	{
		list_type = (int) (NCL_FIFO);
	}
        else if(0 == strcmp("lifo",buffer))
	{
		list_type = (int) (NCL_LIFO);
	}
        else
	{
		fprintf(stderr, "\nin file: %s, at line: %d, in function: <_NclCreateAList>\n",
                                   __FILE__, __LINE__);
		fprintf(stderr, "Unable to create list type: <%s>\n", buffer);
                fprintf(stderr, "Use FIFO as the default list type.\n");

		list_type = (int) (NCL_FIFO);
	}

	tmp_list =(NclList)_NclListCreate(NULL,NULL,0,0,list_type);
	id = (obj*)NclMalloc(sizeof(obj));
	*id = tmp_list->obj.id;
	_NclListSetType((NclObj)tmp_list,list_type);

	data.u.data_obj = _NclMultiDVallistDataCreate(NULL,NULL,Ncl_MultiDVallistData,
				(Ncl_List | Ncl_MultiDVallistData | Ncl_ListVar | Ncl_Typelist),id,NULL,
				1,&one,TEMPORARY,NULL);
	
	return(data);
}

NhlErrorTypes _NclBuildConcatArray
#if	NhlNeedProto
(int n_items,NclStackEntry *result)
#else
(n_items,result)
	int n_items;
	NclStackEntry *result;
#endif
{
	NclStackEntry data;
	int partsize;
	int items_left = n_items;
	logical tmp;
	void *value;
	char *ptr;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	NclMultiDValData theobj,coerce_res = NULL;
	NclStackEntry *data_ptr;
	NclObjTypes result_type ;
	NclTypeClass result_type_class;
	NclObjTypes obj_type ;
	int must_be_numeric = 1,i,j;
	ng_size_t ndims;
	NclScalar *mis_ptr = NULL,themissing;
	int still_no_missing = 1;

	
	if( n_items == 1) {
		data = _NclPop();
		switch(data.kind) {
		case NclStk_VAL:
			result->kind = NclStk_VAL;
			result->u.data_obj = data.u.data_obj;
			return(NhlNOERROR);
		case NclStk_VAR:
			result->kind = NclStk_VAL;
			if(data.u.data_var->obj.status != PERMANENT) {
				result->u.data_obj = _NclStripVarData(data.u.data_var);
				_NclDestroyObj((NclObj)data.u.data_var);
			} else {
				result->u.data_obj = _NclCopyVal(_NclVarValueRead(data.u.data_var,NULL,NULL),NULL);
			}
			return(NhlNOERROR);
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
			return (NhlFATAL);
		}
	}
/*
* First element determines whether the type of the result array is numerci
* or textual
*/
	data_ptr = _NclPeek(0);
	if(data_ptr->kind == NclStk_VAL) {	
		obj_type = data_ptr->u.data_obj->multidval.type->type_class.type;
		ndims = data_ptr->u.data_obj->multidval.n_dims;
		dim_sizes[0] = data_ptr->u.data_obj->multidval.dim_sizes[0];
		partsize = 1;
		for(i = 1; i < ndims; i++ ) {
			dim_sizes[i] = data_ptr->u.data_obj->multidval.dim_sizes[i];
			partsize *= dim_sizes[i];
		} 
	} else if(data_ptr->kind == NclStk_VAR) {
		obj_type = _NclGetVarRepValue(data_ptr->u.data_var);
		ndims = data_ptr->u.data_var->var.n_dims;
		dim_sizes[0] = data_ptr->u.data_var->var.dim_info[0].dim_size;
		partsize = 1;
		for(i = 1; i < ndims; i++ ) {
			dim_sizes[i] = data_ptr->u.data_var->var.dim_info[i].dim_size;
			partsize *= dim_sizes[i];
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: attempt to build array out of illegal data type, can't continue");
		_NclCleanUpStack(n_items);
		return(NhlFATAL);
	}
	if(obj_type & (NCL_SNUMERIC_TYPE_MASK | Ncl_Typelogical)) {
		must_be_numeric =1;
		result_type = (NclObjTypes)((int)obj_type & (NCL_SNUMERIC_TYPE_MASK | (int)Ncl_Typelogical));
	} else if(obj_type & NCL_CHARSTR_TYPE_MASK) {
		must_be_numeric =0;
		result_type = (NclObjTypes)((int)obj_type & NCL_CHARSTR_TYPE_MASK);
	} else if(obj_type & Ncl_Typeobj) {
		must_be_numeric =-1;
		result_type = Ncl_Typeobj;
	} else if(obj_type & Ncl_Typegroup) {
		must_be_numeric =-2;
		result_type = Ncl_Typegroup;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: attempt to build array out of illegal data type or undefined element, can't continue");
		_NclCleanUpStack(n_items);
		return(NhlFATAL);
	}

/*
* The following loop figures out what the final type of the result array
* should be so the appropriate space can be allocated and the elements not
* of this type can be coerced into the correct type
*/	
	for(i = 1; i< n_items; i++) {
		data_ptr = _NclPeek(i);
		if(data_ptr->kind == NclStk_VAL) {	
			obj_type = data_ptr->u.data_obj->multidval.type->type_class.type;
			if(ndims == data_ptr->u.data_obj->multidval.n_dims) {
				dim_sizes[0] += data_ptr->u.data_obj->multidval.dim_sizes[0];
				for(j = 1; j < ndims; j++) {
					if(dim_sizes[j] != data_ptr->u.data_obj->multidval.dim_sizes[j]) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: each element of a literal array must have the same dimension sizes, at least one item doesn't");
						_NclCleanUpStack(n_items);
						return(NhlFATAL);
					}
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: each element of a literal array must have the same number of dimensions");
				_NclCleanUpStack(n_items);
				return(NhlFATAL);
			}
		} else if(data_ptr->kind == NclStk_VAR) {
			obj_type = _NclGetVarRepValue(data_ptr->u.data_var);
			if(ndims == data_ptr->u.data_var->var.n_dims) {
				dim_sizes[0] += data_ptr->u.data_var->var.dim_info[0].dim_size;
				for(j = 1; j < ndims; j++) {
					if(dim_sizes[j] != data_ptr->u.data_var->var.dim_info[j].dim_size) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: each element of a literal array must have the same dimension sizes, at least one item doesn't");
						_NclCleanUpStack(n_items);
						return(NhlFATAL);
					}
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: each element of a literal array must have the same number of dimensions");
				_NclCleanUpStack(n_items);
				return(NhlFATAL);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: attempt to build array out of illegal data type, can't continue");
			_NclCleanUpStack(n_items);
			return(NhlFATAL);
		}
		if((must_be_numeric==1)&&
			( obj_type & (NCL_SNUMERIC_TYPE_MASK | Ncl_Typelogical))) {
			if(result_type > (NclObjTypes)((int)obj_type & (NCL_SNUMERIC_TYPE_MASK | Ncl_Typelogical))) {
				result_type = (NclObjTypes)((int)obj_type & (NCL_SNUMERIC_TYPE_MASK | (int)Ncl_Typelogical));
			}
		} else if((must_be_numeric == 0)&&
			(obj_type & NCL_CHARSTR_TYPE_MASK)) {
			if(result_type > (obj_type & NCL_CHARSTR_TYPE_MASK)) {
				result_type = (NclObjTypes)((int)obj_type & NCL_CHARSTR_TYPE_MASK);
			}
		} else if((must_be_numeric == -1)&&
			(obj_type & Ncl_Typeobj )) {
			result_type = obj_type;
		} else if((must_be_numeric == -2)&&
			(obj_type & Ncl_Typegroup )) {
			result_type = obj_type;
		} else {
/*
* May need to say something different now that objects are done in this
* way too
*/
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: can not combine character or string types with numeric types, can't continue");
			_NclCleanUpStack(n_items);
			return(NhlFATAL);
		}
	}
/*
* By the time you get here you know the stack is guarenteed to contain either 
* numeric values that can be coerced or textual values that can be coerced. 
* Still the  possibility that dimensions numbers and sizes of elements don't 
* match
*/
/*
	data = _NclPop();
*/
	data_ptr = _NclPeek(0);
	items_left--;
	if(data_ptr->kind == NclStk_VAL) {
		theobj = (NclMultiDValData)data_ptr->u.data_obj;
		if(!(theobj->multidval.type->type_class.type & result_type)) {
			coerce_res = _NclCoerceData(theobj,result_type,NULL);
			if(coerce_res == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
			} else if(coerce_res->multidval.missing_value.has_missing) {
				still_no_missing = 0;
/*
* I do this so I can just pass mis_ptr in regardless later on. It
* will be NULL if none of the input has missing values. other wise
* it will be set to the correct missing value.
*/
				mis_ptr = &themissing;
				themissing = coerce_res->multidval.missing_value.value;
			}
/*
			if(theobj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)theobj);
			}
*/
			theobj = coerce_res;
		} else if(theobj->multidval.missing_value.has_missing) {
			still_no_missing = 0;
			mis_ptr = &themissing;
			themissing = theobj->multidval.missing_value.value;
		}
	} else if(data_ptr->kind == NclStk_VAR){
		obj_type = _NclGetVarRepValue(data_ptr->u.data_var);	
		if(!(obj_type & result_type)) {
			theobj = _NclCoerceVar(data_ptr->u.data_var,result_type,NULL);
			if(theobj == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
				_NclCleanUpStack(items_left);
				return(NhlFATAL);
			} else if(theobj->multidval.missing_value.has_missing){
				still_no_missing = 0;
/*
* I do this so I can just pass mis_ptr in regardless later on. It
* will be NULL if none of the input has missing values. other wise
* it will be set to the correct missing value.
*/
				mis_ptr = &themissing;
				themissing = theobj->multidval.missing_value.value;
			}
/*
			if(data_ptr->u.data_var->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)data_ptr->u.data_var);
			}
*/
		} else {
/*
			if(data_ptr->u.data_var->obj.status == PERMANENT) {
*/
				theobj = _NclVarValueRead(data_ptr->u.data_var,NULL,NULL);
/*
			} else {
				theobj = _NclStripVarData(data_ptr->u.data_var);
				_NclDestroyObj((NclObj)data_ptr->u.data_var);
			}
*/
			if(theobj == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
				_NclCleanUpStack(items_left);
				return(NhlFATAL);
			} else if(theobj->multidval.missing_value.has_missing){
/*
* I do this so I can just pass mis_ptr in regardless later on. It
* will be NULL if none of the input has missing values. other wise
* it will be set to the correct missing value.
*/
				still_no_missing = 0;
				mis_ptr = &themissing;
				themissing = theobj->multidval.missing_value.value;
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: unknown stack data type");
		_NclCleanUpStack(items_left);
		return(NhlFATAL);
	}
	if(theobj->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: Arrays of files are not yet supported");
		result->kind = NclStk_NOVAL;
		result->u.data_obj = NULL; 
		_NclCleanUpStack(items_left);
		return(NhlFATAL);
	}



	if(theobj->multidval.kind == MULTID) {
		ndims = theobj->multidval.n_dims ;
	} else {
		ndims = 1;
	}
	result_type_class = _NclTypeEnumToTypeClass(result_type);
	value = (void*)NclMalloc((unsigned)dim_sizes[0]*partsize*result_type_class->type_class.size);
	if(value == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray : Memory allocation failure\n");
		result->kind = NclStk_NOVAL;
		result->u.data_obj = NULL; 
		NclFree(value);
		_NclCleanUpStack(items_left);
		return(NhlFATAL);
	}
	ptr = (char*)value;
	memcpy(ptr,(char*)theobj->multidval.val,theobj->multidval.totalsize);
	ptr += theobj->multidval.totalsize;
/*
	if(theobj->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)theobj);
	}
*/

	coerce_res = NULL;
	for(i = 1; i< n_items; i++) {
		data_ptr = _NclPeek(i);
/*
		data = _NclPop();
*/
		items_left--;
		if(data_ptr->kind == NclStk_VAL) {
			theobj = (NclMultiDValData)data_ptr->u.data_obj;
			if(theobj->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: Arrays of files are not yet supported");
				result->kind = NclStk_NOVAL;
				result->u.data_obj = NULL; 
				NclFree(value);
				_NclCleanUpStack(items_left);
				return(NhlFATAL);
			}
			if(!(theobj->multidval.type->type_class.type & result_type)) {
				coerce_res = _NclCoerceData(theobj,result_type,mis_ptr);
				if(coerce_res == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
					_NclCleanUpStack(items_left);
					NclFree(value);
					return(NhlFATAL);
				} else if((still_no_missing)	
					&&(coerce_res->multidval.missing_value.has_missing)) {
					still_no_missing = 0;
					mis_ptr = &themissing;
					themissing = theobj->multidval.missing_value.value;
				}
/*
				if(theobj->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)theobj);
				}
*/
				theobj = coerce_res;
			} else if((theobj->multidval.missing_value.has_missing)&&(still_no_missing)) {
				still_no_missing = 0;
				mis_ptr = &themissing;
				themissing = theobj->multidval.missing_value.value;
			} else if((theobj->multidval.missing_value.has_missing)&&(!still_no_missing)){
/*
* This is the case where the object is already the correct type but
* needs to have the missing value reset. ResetMissingValue should only be
* called if the object is TEMPORARY otherwise CoerceData Needs to be
* used to convert the missing values.
*/
				if(theobj->obj.status != TEMPORARY) {
/*
* Since theobj is not TEMPORARY then it is referenced somewhere else and the
* following will not cause the pointer to be lost.
*/
					_Ncleq(theobj->multidval.type,&tmp,&theobj->multidval.missing_value.value,mis_ptr,NULL,NULL,1,1);
					if(tmp) {
						coerce_res = _NclCopyVal(theobj,mis_ptr);
/*
					if(theobj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)theobj);
					}
*/
						theobj = coerce_res;
					}
				} else {
					if(mis_ptr != NULL) {
						_NclResetMissingValue(theobj,mis_ptr);
					}
				}
			}
		} else if(data_ptr->kind == NclStk_VAR){
			obj_type = _NclGetVarRepValue(data_ptr->u.data_var);	
			if(!(obj_type & result_type)) {
				theobj = _NclCoerceVar(data_ptr->u.data_var,result_type,mis_ptr);
/*
				if(data_ptr->u.data_var->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)data_ptr->u.data_var);
				}
*/
				if(theobj == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
					_NclCleanUpStack(items_left);
					NclFree(value);
					return(NhlFATAL);
				} else if((still_no_missing)&&
					(theobj->multidval.missing_value.has_missing)) {
					still_no_missing = 0;
					mis_ptr = &themissing;
					themissing = theobj->multidval.missing_value.value;
				}
			} else {
/*
				if(data_ptr->u.data_var->obj.status == PERMANENT) {
*/
					theobj = _NclVarValueRead(data_ptr->u.data_var,NULL,NULL);
/*
				} else {
					theobj = _NclStripVarData(data_ptr->u.data_var);
					_NclDestroyObj((NclObj)data_ptr->u.data_var);
				}
*/
				if(theobj == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occurred that should not have happened");
					_NclCleanUpStack(items_left);
					NclFree(value);
					return(NhlFATAL);
				} else if((still_no_missing)&&
					(theobj->multidval.missing_value.has_missing)) {
					still_no_missing = 0;
					mis_ptr = &themissing;
					themissing = theobj->multidval.missing_value.value;
				} else if((theobj->multidval.missing_value.has_missing)&&(!still_no_missing)) {
					if(theobj->obj.status == TEMPORARY) {
						if(mis_ptr != NULL) {
							_NclResetMissingValue(theobj,mis_ptr);
						}
					} else {
						_Ncleq(theobj->multidval.type,&tmp,&theobj->multidval.missing_value.value,mis_ptr,NULL,NULL,1,1);

						if(tmp) {
							coerce_res = _NclCopyVal(theobj,mis_ptr);
/*
						if(theobj->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)theobj);
						}
*/
							theobj = coerce_res;
						}
					}
				}
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildConcatArray: unknown stack data type");
			_NclCleanUpStack(items_left);
			NclFree(value);
			return(NhlFATAL);
		}
		memcpy(ptr,(char*)theobj->multidval.val,theobj->multidval.totalsize);
		ptr += theobj->multidval.totalsize;
		if((coerce_res != NULL)&&(coerce_res->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)coerce_res);
			coerce_res  = NULL;
		}
			
/*
		if(theobj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)theobj);
		} 
*/
	}
	result->kind = NclStk_VAL;
/*
*
* ------------> stilll need to handle dim info
*/
	if((result_type & Ncl_Typeobj)&&(must_be_numeric == -1)) {
		result->u.data_obj = _NclMultiDValHLUObjDataCreate(NULL,NULL,Ncl_MultiDValHLUObjData,0,value,mis_ptr,ndims,dim_sizes,TEMPORARY,NULL);
	} else {
		result->u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,value,mis_ptr,ndims,dim_sizes,TEMPORARY,NULL,_NclTypeEnumToTypeClass(result_type));
	}
	for(i = 0; i< n_items; i++) {
		data = _NclPop();
		if(data.u.data_obj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)data.u.data_obj);
		}
	}
	if(result->u.data_obj != NULL) 
		return(NhlNOERROR);
	else 
		return(NhlFATAL);
}
NhlErrorTypes _NclProcCallOp
#if	NhlNeedProto
(NclSymbol *proc,int caller_level)
#else
(proc,caller_level)
	NclSymbol *proc;
	int caller_level;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclExecuteReturnStatus eret;
	void* previous_fp = NULL;

/*
* By the time you get here all of the arguments should've been checked against
* the templates and converted to the appropriate type and the sizes checked
*/
	if(proc->u.procfunc== NULL) {
		return(NhlFATAL);
	}

	if(proc->u.procfunc->mach_rec_ptr != NULL) {	
		_NclPushMachine(proc->u.procfunc->mach_rec_ptr);
		eret = _NclExecute(0);
		switch(eret) {
		case Ncl_ERRORS:
			ret = NhlFATAL;
			break;
		case Ncl_STOPS:
		default:
		ret = NhlNOERROR;
			break;
		}
		(void)_NclPopMachine();
	} else {
		ret = NhlFATAL;
	}
/*
* Temporary stack management code
*/
	if(ret != NhlFATAL) {
		previous_fp = _NclLeaveFrame(caller_level);
		(void)_NclPopScope();
		_NclRemapParameters(proc->u.procfunc->nargs,proc->u.procfunc->thescope->cur_offset,previous_fp,PROC_CALL_OP);
		_NclUndefSymbolsInScope(proc->u.procfunc);
		_NclPopFrame(PROC_CALL_OP);
	}  else {
		_NclClearToStackBase(caller_level);
	}
	return(ret);
}
NhlErrorTypes _NclFuncCallOp
#if	NhlNeedProto
(NclSymbol *func,int caller_level)
#else
(func,caller_level)
	NclSymbol *func;
	int caller_level;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclExecuteReturnStatus eret;
	void *previous_fp= NULL;

/*
* By the time you get here all of the arguments should've been checked against
* the templates and converted to the appropriate type and the sizes checked
*/
	if(func->u.procfunc == NULL) {
		return(NhlFATAL);
	}

	if(func->u.procfunc->mach_rec_ptr != NULL) {	
		_NclPushMachine(func->u.procfunc->mach_rec_ptr);
		eret = _NclExecute(0);
		switch(eret) {
		case Ncl_ERRORS:
			ret = NhlFATAL;
			break;
		case Ncl_STOPS:
		default:
			ret = NhlNOERROR;
			break;
		}
		(void)_NclPopMachine();
	} else {
		ret = NhlFATAL;
	}

	if(ret != NhlFATAL) {
		previous_fp = _NclLeaveFrame(caller_level);
		(void)_NclPopScope();
		_NclRemapParameters(func->u.procfunc->nargs,func->u.procfunc->thescope->cur_offset,previous_fp,FUNC_CALL_OP);
		_NclUndefSymbolsInScope(func->u.procfunc);
		_NclPopFrame(FUNC_CALL_OP);
	}   else {
		_NclClearToStackBase(caller_level);
	}
/*
* Doesn't leave return value on stack if an error has occurred. Probably
* should check it to see if it needs to be freed
	if(ret < NhlWARNING) {
		(void)_NclPop();
	}
* No longer needed this since _NclAbortFrame takes care of everything
*/

	return(ret);
}

static int ncl_private_rl_list = 0;

int _NclGetExtRLList
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int	rl = ncl_private_rl_list;

	ncl_private_rl_list = 0;

	return rl;
}

NhlErrorTypes _NclIPSetRL
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg;
	NclMultiDValData tmp_md;

	arg = _NclGetArg(0,1,DONT_CARE);
	switch(arg.kind) {
	case NclStk_VAL:
		tmp_md = arg.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg.u.data_var,
				NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	ncl_private_rl_list = *(int*)(tmp_md->multidval.val);
	if(ncl_private_rl_list < 0) {
		ncl_private_rl_list = 0;
	}
	return(NhlNOERROR);
}



NclStackEntry _NclCreateHLUObjOp
#if	NhlNeedProto
(int nres,char *the_hlu_obj, NclSymbol *the_hlu_obj_class, NclMultiDValData parent)
#else
(nres,the_hlu_obj, the_hlu_obj_class, parent)
	int nres;
	NclSymbol *the_hlu_obj;
	NclSymbol *the_hlu_obj_class;
	NclMultiDValData parent;
#endif
{
	int i,j;
	ng_size_t m;
	NclStackEntry *data,*resname;
	NclStackEntry data_out;
	int rl_list;
	static int local_rl_list = 0;
	NhlGenArray *gen_array;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData tmp2_md = NULL;
	NclHLUObj tmp_ho = NULL;
	int *tmp_id = NULL,tmp_ho_id;
	ng_size_t dim_size = 1;
	int parent_id = -1;
	int parent_hluobj_id = -1;
	int *ids;
	int appd_id = 0;
	NhlArgVal sel,udata;

	NhlINITVAR(data_out);

	if(parent != NULL) 	 {
		if(parent->multidval.totalelements > 1) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Objects only accept one object as their parent, a multi-element of array objects, using the first element");
		}
		tmp_ho = (NclHLUObj)_NclGetObj(*(int*)parent->multidval.val);
		if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)) {
			parent_id = tmp_ho->hlu.hlu_id;
			parent_hluobj_id = *(int*)parent->multidval.val;
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclCreateHLUObjOp: Parent value is not an hlu object or is undefined");
			_NclCleanUpStack(2*nres);
			data_out.kind = NclStk_NOVAL;
			data_out.u.data_obj = NULL;
			return(data_out);
		}
	} else if((defaultapp_hluobj_id != -1)&&(the_hlu_obj_class->u.obj_class_ptr != NhlappClass)) {
		tmp_ho = (NclHLUObj)_NclGetObj(defaultapp_hluobj_id);
		if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)) {
                        parent_id = tmp_ho->hlu.hlu_id;
			parent_hluobj_id = tmp_ho->obj.id;
                } else {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclCreateHLUObjOp: Parent value is not an hlu object or is undefined");
                        _NclCleanUpStack(2*nres);
                        data_out.kind = NclStk_NOVAL;
                        data_out.u.data_obj = NULL;
                        return(data_out);
                }
	} 

	if(local_rl_list == 0) {
		local_rl_list = NhlRLCreate(NhlSETRL);	
	} 

	rl_list = _NclGetExtRLList();	
	if(rl_list == 0) {
		rl_list = local_rl_list;
	}

	gen_array = NclMalloc((unsigned)sizeof(NhlGenArray)*nres);
	for(i = 0; i < nres; i++) {
/*
* Need to peek because I have to keep the stack values arround until after
* the Create call
*/
		data = _NclPeek(2*i);
		resname = _NclPeek(2*i+1);
		if(((data->kind != NclStk_VAL)&&(data->kind != NclStk_NOVAL)&&(data->kind != NclStk_VAR))||((resname->kind != NclStk_VAL)&&(resname->kind != NclStk_VAR))) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in resource list. type mismatch, resource names must be strings and \nresource values must be either file variables, variables or expression results.\n");

			_NclCleanUpStack(2*nres);
			data_out.kind = NclStk_NOVAL;
			data_out.u.data_obj = NULL;
			return(data_out);
		} 
		switch(data->kind) {
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data->u.data_obj;
		break;
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data->u.data_var,NULL,NULL);
		break;
		case NclStk_NOVAL:
			tmp_md = NULL;
		break;
		default:
			_NclCleanUpStack(2*nres);
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
			data_out.kind = NclStk_NOVAL;
			data_out.u.data_obj = NULL;
			return(data_out);
		}
		switch(resname->kind) {
		case NclStk_VAL:
			tmp2_md = (NclMultiDValData)resname->u.data_obj;
		break;
		case NclStk_VAR:
			tmp2_md = _NclVarValueRead(resname->u.data_var,NULL,NULL);
		break;
		default:
			_NclCleanUpStack(2*nres);
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
			data_out.kind = NclStk_NOVAL;
			data_out.u.data_obj = NULL;
			return(data_out);
		}
		if((tmp2_md->multidval.type != (NclTypeClass)nclTypestringClass)||(tmp2_md->multidval.kind != SCALAR)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in resource name. Resources must be scalar strings\n");

			_NclCleanUpStack(2*nres);
			data_out.kind = NclStk_NOVAL;
			data_out.u.data_obj = NULL;
			return(data_out);
		}
		if(tmp_md == NULL) {
			NhlRLSet(rl_list,NrmQuarkToString(
				*(NclQuark *)(tmp2_md->multidval.val)),
				NrmNULLQUARK,
				NULL);
			gen_array[i] = NULL;
		} else if(tmp_md->multidval.hlu_type_rep[0] != NULL) {
			gen_array[i] = _NhlCreateGenArray(
					(NhlPointer)tmp_md->multidval.val,
					tmp_md->multidval.hlu_type_rep[0],
					(int)(tmp_md->multidval.totalsize/tmp_md->multidval.totalelements),
					tmp_md->multidval.n_dims,
					tmp_md->multidval.dim_sizes,
					1);
			NhlRLSet(rl_list,NrmQuarkToString(
				*(NclQuark *)(tmp2_md->multidval.val)),
				NhlTGenArray,
				gen_array[i]);
		} else {
			ids = (int*)NclMalloc((unsigned)sizeof(int)*tmp_md->multidval.totalelements);
			m = 0;
			for(j = 0; j < tmp_md->multidval.totalelements;j++) {
				if(tmp_md->multidval.missing_value.has_missing) {
					if(((int*)tmp_md->multidval.val)[j] != tmp_md->multidval.missing_value.value.objval) {
						tmp_ho = (NclHLUObj)_NclGetObj(((int*)tmp_md->multidval.val)[j]);
					} else {
						tmp_ho = NULL;
					}
				} else {
					tmp_ho = (NclHLUObj)_NclGetObj(((int*)tmp_md->multidval.val)[j]);
				}
				if(tmp_ho != NULL) {
					ids[m++] = tmp_ho->hlu.hlu_id;
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"create: Bad HLU id passed to create, ignoring it");
			
				}
			}
			if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK){
				gen_array[i] = _NhlCreateGenArray(
					(NhlPointer)ids,
					NhlTInteger,
					sizeof(int),
					1,
					&m,
					1);
				NhlRLSet(rl_list,NrmQuarkToString(
					*(NclQuark *)(tmp2_md->multidval.val)),
					NhlTGenArray,
					gen_array[i]);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"The value associated with (%s) does not have an HLU representation",
						NrmQuarkToString(*(NclQuark *)(tmp2_md->multidval.val)));
				gen_array[i] = NULL;
			}
			NclFree(ids);
		}
/*
*-----> Need to deal with NULL hlu_type_rep
*/
	}

	tmp_id = (int*)NclMalloc((unsigned)sizeof(int));

	NhlCreate(&tmp_ho_id,the_hlu_obj,the_hlu_obj_class->u.obj_class_ptr,(parent_id == -1? NhlDEFAULT_APP:parent_id),rl_list);
	tmp_ho = _NclHLUObjCreate(NULL,nclHLUObjClass,Ncl_HLUObj,0,TEMPORARY,tmp_ho_id,parent_hluobj_id,the_hlu_obj_class->u.obj_class_ptr); 
	*tmp_id = tmp_ho->obj.id;
	tmp2_md = _NclMultiDValHLUObjDataCreate(
		NULL,
		NULL,
		Ncl_MultiDValHLUObjData,
		0,
		tmp_id,
		NULL,
		1,
		&dim_size,
		TEMPORARY,
		NULL
	); 
	if(NhlIsApp(tmp_ho->hlu.hlu_id)) {
		appd_id = NhlAppGetDefaultParentId();
		if(tmp_ho->hlu.hlu_id == appd_id) {	
			defaultapp_hluobj_id = tmp_ho->obj.id;
			/* since it is the default app we cannot allow it to be deleted */
			tmp_ho->obj.status = STATIC;
		}
		NhlINITVAR(sel);
		NhlINITVAR(udata);
		udata.lngval = tmp_ho->obj.id;
		tmp_ho->hlu.apcb = _NhlAddClassCallback(NhlappClass,_NhlCBappDefParentChange,sel,DefaultAppChangeCB,udata);
	}

	
	for(i = 0; i < nres; i++) {
		data = _NclPeek(2*i);
		switch(data->kind) {
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data->u.data_obj;
		break;
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data->u.data_var,NULL,NULL);
		break;
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
			_NclCleanUpStack(2*nres);
			data_out.kind = NclStk_NOVAL;
			data_out.u.data_obj = NULL;
			return(data_out);
		}
		if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData) {
			for(j = 0; j < tmp_md->multidval.totalelements;j++) {
				if(tmp_md->multidval.missing_value.has_missing) {
					if(((int*)tmp_md->multidval.val)[j] != tmp_md->multidval.missing_value.value.objval) {
						tmp_ho = (NclHLUObj)_NclGetObj(((int*)tmp_md->multidval.val)[j]);
					} else {
						tmp_ho = NULL;
					}
				} else {
					tmp_ho = (NclHLUObj)_NclGetObj(((int*)tmp_md->multidval.val)[j]);
				}
				if(tmp_ho != NULL) 
					_NclAddHLUToExpList((NclHLUObj)_NclGetObj(*tmp_id),tmp_ho->obj.id);
			}
		}
	}
	if(tmp2_md != NULL) {
		data_out.u.data_obj = tmp2_md;
		data_out.kind = NclStk_VAL;
	} else {
		data_out.u.data_obj = NULL;
		data_out.kind = NclStk_NOVAL;
	}
	for(i = 0; i < nres; i++) {
		if(gen_array[i])
		NhlFreeGenArray(gen_array[i]);
	}
	NclFree(gen_array);
	NhlRLClear(rl_list);
	_NclCleanUpStack(2*nres);
	return(data_out);
}

NclStackEntry _NclGetHLUObjOp
#if	NhlNeedProto
(NclMultiDValData the_hlu_data_obj,NclQuark res_name)
#else
(the_hlu_data_obj,res_name)
NclMultiDValData the_hlu_data_obj;
NclQuark res_name;
#endif
{
	NclStackEntry out_data;
	int *obj_ids = NULL;
	int *child_ids = NULL;
	NclHLUObj hlu_ptr;
	int i,n_items = 0;
	NclMultiDValData tmp_md= NULL;
	NclStackEntry tmp_data;
	NhlErrorTypes ret = NhlNOERROR;
	int rl_list;
	int k;

	rl_list = NhlRLCreate(NhlGETRL);	



	obj_ids = (int*)the_hlu_data_obj->multidval.val;
	if(the_hlu_data_obj->multidval.totalelements > 1) {
		NhlRLGet(rl_list,NrmQuarkToString(res_name),NhlTNclData,&tmp_md);
		for(i = 0; i < the_hlu_data_obj->multidval.totalelements; i++ ) {
			hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
			if((hlu_ptr != NULL) &&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
/*
* Problem here when hlu_ptr is null positions of return values may be intractable since they won't corespond
* with hlu id in input array. Probably need to keep track of sizes and types and use the new function to
* create object containing missing values.
*/
				NhlGetValues(hlu_ptr->hlu.hlu_id,rl_list);
				if(tmp_md != NULL) {	
					if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData) {
						child_ids = (int*)tmp_md->multidval.val;
						for(k = 0; k < tmp_md->multidval.totalelements; k++) {
							if(tmp_md->multidval.missing_value.has_missing) {
								if(child_ids[k] != tmp_md->multidval.missing_value.value.objval) {
									_NclAddHLUChild(hlu_ptr,child_ids[k]);
								}
							} else {
								_NclAddHLUChild(hlu_ptr,child_ids[k]);
							}
						}
					}
					tmp_data.kind = NclStk_VAL;
					tmp_data.u.data_obj = tmp_md;
					if(_NclPush(tmp_data) == NhlFATAL) {
						out_data.kind = NclStk_NOVAL;
						out_data.u.data_obj = NULL;
						return(out_data);
					}
					n_items++;	
				}
			}
			if(n_items != 0) {
				ret = _NclBuildConcatArray(n_items,&out_data);
				if(ret > NhlWARNING) {
					out_data = _NclPop();
					return(out_data);
				} else {
					out_data.kind = NclStk_NOVAL;
					out_data.u.data_obj = NULL;
					return(out_data);
				}
			}
		}
		NhlRLDestroy(rl_list);
	} else {
		hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[0]);
		NhlRLGet(rl_list,NrmQuarkToString(res_name),NhlTNclData,&tmp_md);
		if((hlu_ptr != NULL) &&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
			NhlGetValues(hlu_ptr->hlu.hlu_id,rl_list);
			if(tmp_md != NULL) {
				if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData) {
					child_ids = (int*)tmp_md->multidval.val;
					for(k = 0; k < tmp_md->multidval.totalelements; k++) {
						if(tmp_md->multidval.missing_value.has_missing) {
							if(child_ids[k] != tmp_md->multidval.missing_value.value.objval) {
								_NclAddHLUChild(hlu_ptr,child_ids[k]);
							}
						} else {
							_NclAddHLUChild(hlu_ptr,child_ids[k]);
						}
					}
				}
				out_data.kind = NclStk_VAL;
				out_data.u.data_obj = tmp_md;
				return(out_data);
			} else {
				out_data.kind = NclStk_NOVAL;
				out_data.u.data_obj = NULL;
				return(out_data);
			}
		}
		NhlRLDestroy(rl_list);
	} 
	out_data.kind = NclStk_NOVAL;
	out_data.u.data_obj = NULL;
	return(out_data);
}

NhlErrorTypes _NclSetHLUObjOp
#if	NhlNeedProto
(NclMultiDValData the_hlu_data_obj, int nres)
#else
(the_hlu_data_obj,nres)
NclMultiDValData the_hlu_data_obj;
int nres;
#endif
{
	int i,j,k;
    ng_size_t m;
	NclStackEntry *data,*resname;
	int rl_list;
	static int local_rl_list = 0;
	NhlGenArray *gen_array;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData tmp2_md = NULL;
	int *obj_ids = NULL;
	NclHLUObj hlu_ptr,tmp_ho;
	int *ids;
	



	if(local_rl_list == 0 ) {
		local_rl_list = NhlRLCreate(NhlSETRL);	
	}
	rl_list = _NclGetExtRLList();
	if(rl_list == 0) {
		rl_list = local_rl_list;
	}
	gen_array = NclMalloc((unsigned)sizeof(NhlGenArray)*nres);
	for(i = 0; i < nres; i++) {
/*
* Need to peek because I have to keep the stack values arround until after
* the Create call
*/
		data = _NclPeek(2*i);
		resname = _NclPeek(2*i+1);
		if(((data->kind != NclStk_VAL)&&(data->kind!= NclStk_NOVAL)&&(data->kind != NclStk_VAR))||((resname->kind != NclStk_VAL)&&(resname->kind != NclStk_VAR))) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in resource list. type mismatch, resource names must be strings and \nresource values must be either file variables, variables or expression results.\n");

			_NclCleanUpStack(2*nres);
			return(NhlFATAL);
		} 
		switch(data->kind) {
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data->u.data_obj;
		break;
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data->u.data_var,NULL,NULL);
			break;
		case NclStk_NOVAL:
			tmp_md = NULL;
		break;
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
			return (NhlFATAL);
		}
		switch(resname->kind) {
		case NclStk_VAL:
			tmp2_md = (NclMultiDValData)resname->u.data_obj;
		break;
		case NclStk_VAR:
			tmp2_md = _NclVarValueRead(resname->u.data_var,NULL,NULL);
		break;
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
			return (NhlFATAL);
		}
		if((tmp2_md->multidval.type != (NclTypeClass)nclTypestringClass)||(tmp2_md->multidval.kind != SCALAR)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in resource name. Resources must be scalar strings\n");

			_NclCleanUpStack(2*nres);
			return(NhlFATAL);
		}
		if(tmp_md == NULL) {
			NhlRLSet(rl_list,NrmQuarkToString(
				*(NclQuark *)(tmp2_md->multidval.val)),
				NrmNULLQUARK,
				NULL);
			gen_array[i] = NULL;
		} else if(tmp_md->multidval.hlu_type_rep[0] != NULL) {
			gen_array[i] = _NhlCreateGenArray(
					(NhlPointer)tmp_md->multidval.val,
					tmp_md->multidval.hlu_type_rep[0],
					(int)(tmp_md->multidval.totalsize/tmp_md->multidval.totalelements),
					tmp_md->multidval.n_dims,
					tmp_md->multidval.dim_sizes,
					0);
			NhlRLSet(rl_list,NrmQuarkToString(
				*(NclQuark *)(tmp2_md->multidval.val)),
				NhlTGenArray,
				gen_array[i]);
		} else {
			ids = (int*)NclMalloc((unsigned)sizeof(int)*tmp_md->multidval.totalelements);
			m = 0;
			for(j = 0; j < tmp_md->multidval.totalelements;j++) {
				if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData ) {
                                	tmp_ho = (NclHLUObj)_NclGetObj(((int*)tmp_md->multidval.val)[j]);
					if(tmp_ho != NULL) {
                                		ids[m++] = tmp_ho->hlu.hlu_id;
						obj_ids = (int*)the_hlu_data_obj->multidval.val;
						for(k = 0; k < the_hlu_data_obj->multidval.totalelements; k++) {
							if((!the_hlu_data_obj->multidval.missing_value.has_missing)||
								(obj_ids[k]!= the_hlu_data_obj->multidval.missing_value.value.objval))  {
								hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[k]);
								if((hlu_ptr != NULL) &&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
									_NclAddHLUToExpList(hlu_ptr,tmp_ho->obj.id);
								}
							} 
						}
					} else {
						NhlPError(NhlWARNING,NhlEUNKNOWN,"setvalues: Bad HLU id passed to setvalues, ignoring it");
					}
					
				}
                        }
			if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK){
                                gen_array[i] = _NhlCreateGenArray(
                                        (NhlPointer)ids,
                                        NhlTInteger,
                                        sizeof(int),
                                        1,
                                        &m,
                                        1);
                                NhlRLSet(rl_list,NrmQuarkToString(
                                        *(NclQuark *)(tmp2_md->multidval.val)),
                                        NhlTGenArray,
                                        gen_array[i]);
				NclFree(ids);
                        } else {
				NclFree(ids);
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"The value associated with (%s) does not have an HLU representation",
                                                NrmQuarkToString(*(NclQuark *)(tmp2_md->multidval.val)));
                                gen_array[i] = NULL;
                        }



		}
	}


/* 
* stuff like this makes me think that NclMultiDValHLUObjData.c should have 
* methods added to the HLUObj to perform HLU specific functions.
*/
	obj_ids = (int*)the_hlu_data_obj->multidval.val;
	for(i = 0; i < the_hlu_data_obj->multidval.totalelements; i++ ) {
		if((!the_hlu_data_obj->multidval.missing_value.has_missing)||
				(obj_ids[i]!= the_hlu_data_obj->multidval.missing_value.value.objval))  {
			hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
			if((hlu_ptr != NULL) &&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
				NhlSetValues(hlu_ptr->hlu.hlu_id,rl_list);
			}
		}
	}
	for(i = 0; i < nres; i++) {
		if(gen_array[i])
			NhlFreeGenArray(gen_array[i]);
	}
	NclFree(gen_array);
	NhlRLClear(rl_list);
	_NclCleanUpStack(2*nres);
	return(NhlNOERROR);
}

NhlErrorTypes _NclNewOp
#if	NhlNeedProto
(NclSymbol*  data_type, NclStackEntry size_expr, NclStackEntry missing_expr)
#else
(data_type, size_expr, missing_expr)
NclSymbol*  data_type;
NclStackEntry size_expr;
NclStackEntry missing_expr;
#endif  
{
	NclObjTypes the_obj_type;
	NclStackEntry data;
	NclBasicDataTypes the_type;
	NclScalar missing_val;
	NclMultiDValData missing_md,tmp_md,size_md,tmp1_md;
	void *tmp_val;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	long long *dim_size_list;
	ng_size_t total;
	long long ll_total;
	ng_size_t i,j;
	char *tp;
	NclTypeClass typec = NULL;
	int tsize;
	NrmQuark qnofill = NrmStringToQuark("No_FillValue");
	int fill = 1;
	
	the_type = _NclKeywordToDataType(data_type);
	the_obj_type = _NclKeywordToObjType(data_type);
	typec = (NclTypeClass)_NclTypeEnumToTypeClass(the_obj_type);
	if(the_obj_type == NCL_NUMERIC_TYPE_MASK) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"New: Keyword numeric is too general, can't determine the size of data");
		return(NhlFATAL);
	} else if(the_obj_type == NCL_SNUMERIC_TYPE_MASK) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"New: Keyword snumeric is too general, can't determine the size of data");
		return(NhlFATAL);
	} else if(the_obj_type == Ncl_Var) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"New: Can not make arrays of vars without values and variable names");
		return(NhlFATAL);
	}

	if((missing_expr.kind == NclStk_VAL)||(missing_expr.kind == NclStk_VAR)) {
		if(missing_expr.kind == NclStk_VAL) {
			missing_md = missing_expr.u.data_obj;
		} else {
			missing_md = _NclVarValueRead(missing_expr.u.data_var,NULL,NULL);
		}
		if(missing_md->multidval.kind != SCALAR) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"New: the missing value provided has more than one element, using the first one as the _FillValue");
		}
		if (missing_md->multidval.data_type == NCL_string && *(NrmQuark*)missing_md->multidval.val == qnofill) {
			if (the_obj_type != Ncl_MultiDValnclfileData) {
				fill = 0;
			}
			else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"New: file variables cannot be created as an undefined value, setting default _FillValue");
				fill = 1;
				missing_val.objval = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
			}
		}
		else if(missing_md->multidval.type->type_class.type != the_obj_type) {
			tmp_md = _NclCoerceData(missing_md,the_obj_type,NULL);
			if(tmp_md == NULL) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"New: Could not coerce missing value parameter into appropriate type, using default");
				missing_val = typec->type_class.default_mis;
			} else {
				memcpy((void*)&missing_val,(void*)tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
				_NclDestroyObj((NclObj)tmp_md);
			}
		} else {
			memcpy((void*)&missing_val,(void*)missing_md->multidval.val,missing_md->multidval.type->type_class.size);
		}
	} else {
		if(the_obj_type & NCL_VAL_TYPE_MASK) {
			dim_sizes[0] = 1;
			missing_val = typec->type_class.default_mis;
		} else if(the_obj_type & NCL_MD_MASK) {
			
			missing_val.objval = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"New: Incorrect type passed in to be created");
			return(NhlFATAL);
		}
	}
	if(size_expr.kind == NclStk_VAL) {
		size_md = size_expr.u.data_obj;
	} else if(size_expr.kind == NclStk_VAR) {
		size_md = _NclVarValueRead(size_expr.u.data_var,NULL,NULL);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"New: Incorrect type passed as dimension size array");
		return(NhlFATAL);

	}
	if(size_md != NULL) {
		if(!(size_md->multidval.type->type_class.type & Ncl_Typeint64)) {
			tmp1_md = _NclCoerceData(size_md,Ncl_Typeint64,NULL);
			if(tmp1_md == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"New: the dimension size parameter is the wrong type an integer value was expected");
				return(NhlFATAL);
			}
		} else {
			tmp1_md = size_md;
		}
		dim_size_list = (long long *)tmp1_md->multidval.val;
		if(tmp1_md->multidval.missing_value.has_missing) {
			for(i = 0; i < tmp1_md->multidval.totalelements; i++ ) {
				if(tmp1_md->multidval.missing_value.value.longval == dim_size_list[i]) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"New: The dimension size list contains missing values, can't determine size");
					return(NhlFATAL);
				}
			}
		}
		ll_total = 1;
		j = 0;
		if((tmp1_md->multidval.dim_sizes[0] == 1)&&(dim_size_list[0] > 0)) {
			ll_total *= dim_size_list[0];
			dim_sizes[0] = (ng_size_t)dim_size_list[0];
			j++;
		} else {
			for(i = 0; i< tmp1_md->multidval.dim_sizes[0]; i++) {
				if(dim_size_list[i] < 1) {	
#if 0
					printf("_NclNewOp: i is %ld; dim_size_list[%ld] is %zd, tmp1_md->multidval.dim_sizes[0] is %zd\n",
					       (long)i, (long)i, dim_size_list[i], tmp1_md->multidval.dim_sizes[0]);
#endif
					NhlPError(NhlFATAL,NhlEUNKNOWN,"New: a zero or negative value has been passed in in the dimension size parameter");
					return(NhlFATAL);
				} else {
					ll_total *= dim_size_list[i];
					dim_sizes[j] = (ng_size_t)dim_size_list[i];
					j++;
				}
			}
		}
		if(j == 0) {
			dim_sizes[0] = 1;
			j = 1;
		}
		total = (ng_size_t) ll_total;
		ll_total *= _NclSizeOf(the_type);
#ifdef NG32BIT
		if (ll_total > INT_MAX) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"New: requested size of variable (%lld bytes) exceeds the current maximum %d allowed on this system\n",ll_total,INT_MAX));
#else
		if (ll_total > LONG_MAX) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"New: requested size of variable (%lld bytes) exceeds the current maximum %d allowed on this system\n",ll_total,LONG_MAX));
#endif
			return(NhlFATAL);
		}

		tmp_val = (void*)NclMalloc((ng_usize_t)ll_total);
		if (! tmp_val) {
			NhlPError(NhlFATAL,ENOMEM,"New: could not create new array");
			return(NhlFATAL);
		}
		if (fill) {
			if(NCL_list == the_type)
			{
				_NclBuildArrayOfList(tmp_val, j, dim_sizes);

				tmp_md = _NclCreateVal(NULL,NULL,((the_obj_type & NCL_VAL_TYPE_MASK) ?
						Ncl_MultiDValData:the_obj_type),0,tmp_val,
						NULL,j,dim_sizes,TEMPORARY,NULL,
						(NclObjClass)((the_obj_type & NCL_VAL_TYPE_MASK) ?
						_NclTypeEnumToTypeClass(the_obj_type):NULL));
			}
			else
			{
				tp = (char *) tmp_val;
				i = 1;
				tsize = _NclSizeOf(the_type);
				memcpy((void*)tp,(void*)&missing_val,tsize);
				while (i <= total / 2) {
					memcpy(tp+i*tsize,tp,tsize * i);
					i *= 2;
				}
				if (total - i > 0) {
					memcpy(tp+i*tsize,tp,tsize * (total - i));
				}

				tmp_md = _NclCreateVal(NULL,NULL,((the_obj_type & NCL_VAL_TYPE_MASK) ?
						Ncl_MultiDValData:the_obj_type),0,tmp_val,
						&missing_val,j,dim_sizes,TEMPORARY,NULL,
						(NclObjClass)((the_obj_type & NCL_VAL_TYPE_MASK) ?
						_NclTypeEnumToTypeClass(the_obj_type):NULL));
			}
		}
		else if (the_obj_type == Ncl_MultiDValHLUObjData) {
			/* this type must have a defined value: if no missing value is supplied then set them to -1, which is out of the range of NCL object ids. */
			int fill_val = -1;
			tp = (char *) tmp_val;
			i = 1;
			tsize = _NclSizeOf(the_type);
			memcpy((void*)tp,(void*)&fill_val,tsize);
			while (i <= total / 2) {
				memcpy(tp+i*tsize,tp,tsize * i);
				i *= 2;
			}
			if (total - i > 0) {
				memcpy(tp+i*tsize,tp,tsize * (total - i));
			}

			tmp_md = _NclCreateVal(NULL,NULL,((the_obj_type & NCL_VAL_TYPE_MASK) ?
							  Ncl_MultiDValData:the_obj_type),0,tmp_val,
					       NULL,j,dim_sizes,TEMPORARY,NULL,
					       (NclObjClass)((the_obj_type & NCL_VAL_TYPE_MASK) ?
							     _NclTypeEnumToTypeClass(the_obj_type):NULL));
		}
		else {
			tmp_md = _NclCreateVal(NULL,NULL,((the_obj_type & NCL_VAL_TYPE_MASK) ?
							  Ncl_MultiDValData:the_obj_type),0,tmp_val,
					       NULL,j,dim_sizes,TEMPORARY,NULL,
					       (NclObjClass)((the_obj_type & NCL_VAL_TYPE_MASK) ?
							     _NclTypeEnumToTypeClass(the_obj_type):NULL));
		}
		if((tmp1_md != size_md)&&(tmp1_md->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)tmp1_md);
		}
		if(tmp_md != NULL) {
			data.kind = NclStk_VAL;
			data.u.data_obj = tmp_md;
			return(_NclPush(data));
		} else{
			NhlPError(NhlFATAL,NhlEUNKNOWN,"New: Could not create new array");
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"New: Could not create new array");
		return(NhlFATAL);
	}
}



#ifdef __cplusplus
}
#endif
