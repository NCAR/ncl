#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/ncargC.h>
#include "defs.h"
#include <errno.h>
#include "Symbol.h"
#include "NclDataDefs.h"
#include "Machine.h"
#include "NclFile.h"
#include "NclVar.h"
#include "NclCoordVar.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclMdInc.h"
#include "NclHLUObj.h"
#include "parser.h"
#include "OpsList.h"
#include "ApiRecords.h"
#include "TypeSupport.h"
#include "NclBuiltInSupport.h"





















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
	} else if(lhs.kind == NclStk_VAR) {
		lhs_type = _NclGetVarRepValue(lhs.u.data_var);
	} else {
		return(NhlFATAL);
	}

	if(rhs.kind == NclStk_VAL) {
		rhs_type = rhs.u.data_obj->multidval.type->type_class.type;
	} else if(rhs.kind == NclStk_VAR) {
		rhs_type = _NclGetVarRepValue(rhs.u.data_var);
	} else {
		return(NhlFATAL);
	}

	if(lhs_type != rhs_type) {

		if(rhs.kind == NclStk_VAL) {
/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
			coerce_res = _NclCoerceData(rhs.u.data_obj,
					lhs_type,NULL);
		} else {
/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
			coerce_res = _NclCoerceVar(rhs.u.data_var,lhs_type,NULL);
		}
		if(coerce_res == NULL) {
			if(lhs.kind == NclStk_VAL) {
/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
			coerce_res = _NclCoerceData(lhs.u.data_obj,
				rhs_type & NCL_VAL_TYPE_MASK,NULL);
				if(coerce_res == NULL) {
/*
* Error message needed
*/
					return(NhlFATAL);
				} else {
					lhs_data_obj = coerce_res;
					if(rhs.kind == NclStk_VAL) {
						rhs_data_obj = rhs.u.data_obj;
					} else {
						rhs_data_obj = _NclVarValueRead(rhs.u.data_var,NULL,NULL);
					}
				}
			} else {
/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
				coerce_res = _NclCoerceVar(lhs.u.data_var,
					rhs_type,NULL);
				if(coerce_res == NULL) {
/*
* Error message needed
*/
					return(NhlFATAL);
				} else {
					lhs_data_obj = coerce_res;
					if(rhs.kind == NclStk_VAL) {
						rhs_data_obj = rhs.u.data_obj;
					} else {
						rhs_data_obj = _NclVarValueRead(rhs.u.data_var,NULL,NULL);
					}
				}
			}
		} else {
			rhs_data_obj = coerce_res;
			if(lhs.kind == NclStk_VAL) {
				lhs_data_obj = lhs.u.data_obj;
			} else {
				lhs_data_obj = _NclVarValueRead(lhs.u.data_var,NULL,NULL);
			}
		}
	} else {
		if(lhs.kind == NclStk_VAL) {
			lhs_data_obj = lhs.u.data_obj;
		} else {
			lhs_data_obj = _NclVarValueRead(lhs.u.data_var,NULL,NULL);
		}
		if(rhs.kind == NclStk_VAL) {
			rhs_data_obj = rhs.u.data_obj;
		} else {
			rhs_data_obj = _NclVarValueRead(rhs.u.data_var,NULL,NULL);
		}
	}
	if((lhs_data_obj != NULL)&&(rhs_data_obj != NULL)) {
		ret = _NclCallDualOp(lhs_data_obj,rhs_data_obj,operation,(NclObj*)&(result->u.data_obj));
		if(result->u.data_obj != NULL)
			result->kind = NclStk_VAL;
	} else {
		return(NhlFATAL);
	}




        if(lhs.kind == NclStk_VAL) {
		if(lhs_data_obj != lhs.u.data_obj) {
			if(lhs_data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)lhs_data_obj);
			}
		}
		if(lhs.u.data_obj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)lhs.u.data_obj);
		}
        } else if(lhs.kind == NclStk_VAR) {
		if(lhs_data_obj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)lhs_data_obj);
		}
		if(lhs.u.data_var->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)lhs.u.data_var);
		}
        } 
        if(rhs.kind == NclStk_VAL) {
		if(rhs_data_obj != rhs.u.data_obj) {
			if(rhs_data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_data_obj);
			}
		}
		if(rhs.u.data_obj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)rhs.u.data_obj);
		}
        } else if(rhs.kind == NclStk_VAR) {
		if(rhs_data_obj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)rhs_data_obj);
		}
		if(rhs.u.data_var->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)rhs.u.data_var);
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
	void *value;
	char *ptr;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	NclMultiDValData theobj,coerce_res;
	NclStackEntry *data_ptr;
	NclObjTypes result_type ;
	int obj_type ;
	int must_be_numeric = 1,i,j;
	int ndims;
	NclScalar *mis_ptr = NULL,themissing;
	int still_no_missing = 1;

	

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
	if(obj_type & (NCL_VAL_NUMERIC_MASK | Ncl_Typelogical)) {
		must_be_numeric =1;
		result_type = obj_type & (NCL_VAL_NUMERIC_MASK | Ncl_Typelogical);
	} else if(obj_type & NCL_VAL_CHARSTR_MASK) {
		must_be_numeric =0;
		result_type = obj_type & NCL_VAL_CHARSTR_MASK;
	} else if(obj_type & Ncl_Typeobj) {
		must_be_numeric =-1;
		result_type = Ncl_Typeobj;
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
			( obj_type & (NCL_VAL_NUMERIC_MASK | Ncl_Typelogical))) {
			if(result_type > (obj_type & (NCL_VAL_NUMERIC_MASK | Ncl_Typelogical))) {
				result_type = (obj_type & (NCL_VAL_NUMERIC_MASK | Ncl_Typelogical));
			}
		} else if((must_be_numeric == 0)&&
			(obj_type & NCL_VAL_CHARSTR_MASK)) {
			if(result_type > (obj_type & NCL_VAL_CHARSTR_MASK)) {
				result_type = (obj_type & NCL_VAL_CHARSTR_MASK);
			}
		} else if((must_be_numeric == -1)&&
			(obj_type & Ncl_Typeobj )) {
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

	data = _NclPop();
	items_left--;
	if(data.kind == NclStk_VAL) {
		theobj = (NclMultiDValData)data.u.data_obj;
		if(!(theobj->multidval.type->type_class.type & result_type)) {
			coerce_res = _NclCoerceData(theobj,result_type,NULL);
			if(coerce_res == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occured that should not have happend");
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
			if(theobj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)theobj);
			}
			theobj = coerce_res;
		} else if(theobj->multidval.missing_value.has_missing) {
			still_no_missing = 0;
			mis_ptr = &themissing;
			themissing = theobj->multidval.missing_value.value;
		}
	} else if(data.kind == NclStk_VAR){
		obj_type = _NclGetVarRepValue(data.u.data_var);	
		if(!(obj_type & result_type)) {
			theobj = _NclCoerceVar(data.u.data_var,result_type,NULL);
			if(theobj == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occured that should not have happend");
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
			if(data.u.data_var->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)data.u.data_var);
			}
		} else {
			theobj = _NclVarValueRead(data.u.data_var,NULL,NULL);
			if(theobj == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occured that should not have happend");
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
	if(theobj->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)theobj);
	}


	while(items_left) {
		data = _NclPop();
		items_left--;
		if(data.kind == NclStk_VAL) {
			theobj = (NclMultiDValData)data.u.data_obj;
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occured that should not have happend");
					_NclCleanUpStack(items_left);
					NclFree(value);
					return(NhlFATAL);
				} else if((still_no_missing)	
					&&(coerce_res->multidval.missing_value.has_missing)) {
					still_no_missing = 0;
					mis_ptr = &themissing;
					themissing = theobj->multidval.missing_value.value;
				}
				if(theobj->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)theobj);
				}
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
					coerce_res = _NclCopyVal(theobj,mis_ptr);
					if(theobj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)theobj);
					}
					theobj = coerce_res;
				} else {
					_NclResetMissingValue(theobj,mis_ptr);
				}
			}
		} else if(data.kind == NclStk_VAR){
			obj_type = _NclGetVarRepValue(data.u.data_var);	
			if(!(obj_type & result_type)) {
				theobj = _NclCoerceVar(data.u.data_var,result_type,mis_ptr);
				if(theobj == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occured that should not have happend");
					_NclCleanUpStack(items_left);
					NclFree(value);
					return(NhlFATAL);
				} else if((still_no_missing)&&
					(theobj->multidval.missing_value.has_missing)) {
					still_no_missing = 0;
					mis_ptr = &themissing;
					themissing = theobj->multidval.missing_value.value;
				}
				if(data.u.data_var->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)data.u.data_var);
				}
			} else {
				theobj = _NclVarValueRead(data.u.data_var,NULL,NULL);
				if(theobj == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occured that should not have happend");
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
						_NclResetMissingValue(theobj,mis_ptr);
					} else {
						coerce_res = _NclCopyVal(theobj,mis_ptr);
						if(theobj->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)theobj);
						}
						theobj = coerce_res;
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
		if(theobj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)theobj);
		}
	}
	result->kind = NclStk_VAL;
/*
*
* ------------> stilll need to handle dim info
*/
	if((result_type & Ncl_Typeobj)&&(must_be_numeric == -1)) {
		result->u.data_obj = _NclMultiDValHLUObjDataCreate(NULL,NULL,Ncl_MultiDValHLUObjData,0,value,NULL,ndims,dim_sizes,TEMPORARY,NULL);
	} else {
		result->u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,value,NULL,ndims,dim_sizes,TEMPORARY,NULL,_NclTypeEnumToTypeClass(result_type));
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
	NclStackEntry data;
	void* previous_fp = NULL;
	int i;

/*
* By the time you get here all of the arguments should've been checked against
* the templates and converted to the appropriate type and the sizes checked
*/
	if(proc->u.procfunc== NULL) {
		return(NhlFATAL);
	}
	
	_NclPushMachine(proc->u.procfunc->mach_rec_ptr);
	eret = _NclExecute(0);
	switch(eret) {
	case Ncl_ERRORS:
		ret = NhlFATAL;
		break;
	case Ncl_STOPS:
	case Ncl_BREAKS:
	case Ncl_CONTINUES:
	default:
		ret = NhlNOERROR;
		break;
	}
	(void)_NclPopMachine();
	previous_fp = _NclLeaveFrame(caller_level);
/*
* Temporary stack management code
*/
	if(ret != NhlFATAL) {
		_NclRemapParameters(proc->u.procfunc->nargs,proc->u.procfunc->thescope->cur_offset,previous_fp,PROC_CALL_OP);
	} else {
		for(i = 0; i<proc->u.procfunc->thescope->cur_offset; i++) {
			data = _NclPop();
			switch(data.kind) {
			case NclStk_VAL: {
				_NclDestroyObj((NclObj)data.u.data_obj);
			}
				break;
			case NclStk_VAR: {
				_NclDestroyObj((NclObj)data.u.data_var);
			}
				break;
			default:
				break;
			}
		}
	}
	
	_NclPopFrame(PROC_CALL_OP);
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
	NclStackEntry data;
	NclExecuteReturnStatus eret;
	void *previous_fp= NULL;
	
	int i;

/*
* By the time you get here all of the arguments should've been checked against
* the templates and converted to the appropriate type and the sizes checked
*/
	if(func->u.procfunc == NULL) {
		return(NhlFATAL);
	}
	
	_NclPushMachine(func->u.procfunc->mach_rec_ptr);
	eret = _NclExecute(0);
	switch(eret) {
	case Ncl_ERRORS:
		ret = NhlFATAL;
		break;
	case Ncl_STOPS:
	case Ncl_BREAKS:
	case Ncl_CONTINUES:
	default:
		ret = NhlNOERROR;
		break;
	}
	(void)_NclPopMachine();
	previous_fp = _NclLeaveFrame(caller_level);

	if(ret != NhlFATAL) {
		_NclRemapParameters(func->u.procfunc->nargs,func->u.procfunc->thescope->cur_offset,previous_fp,FUNC_CALL_OP);
	} else {
		for(i = 0; i<func->u.procfunc->thescope->cur_offset; i++) {
			data = _NclPop();
			switch(data.kind) {
			case NclStk_VAL: {
				_NclDestroyObj((NclObj)data.u.data_obj);
			}
				break;
			case NclStk_VAR: {
				_NclDestroyObj((NclObj)data.u.data_var);
			}
				break;
			default:
				break;
			}
		}
	}
	_NclPopFrame(FUNC_CALL_OP);
/*
* Doesn't leave return value on stack if an error has occured. Probably
* should check it to see if it needs to be freed
*/
	if(ret < NhlWARNING) {
		(void)_NclPop();
	}

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
	NclStackEntry *data,*resname;
	NclStackEntry data_out;
	int rl_list;
	static int local_rl_list = 0;
	NhlGenArray *gen_array;
	NclMultiDValData tmp_md = NULL;
	NclHLUObj tmp_ho = NULL;
	int *tmp_id = NULL,tmp_ho_id;
	int dim_size = 1;
	int parent_id = -1;
	int *ids;

	if(parent != NULL) 	 {
		if(parent->multidval.totalelements > 1) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Objects only accept one object as their parent, a multi-element of array objects, using the first element");
		}
		tmp_ho = (NclHLUObj)_NclGetObj(*(int*)parent->multidval.val);
		if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)) {
			parent_id = tmp_ho->hlu.hlu_id;
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
		if(((data->kind != NclStk_VAL)&&(data->kind != NclStk_VAR))||(resname->kind != NclStk_VAL)) {
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
		}
		if(tmp_md->multidval.hlu_type_rep[0] != NULL) {
			gen_array[i] = _NhlCreateGenArray(
					(NhlPointer)tmp_md->multidval.val,
					tmp_md->multidval.hlu_type_rep[0],
					(int)(tmp_md->multidval.totalsize/tmp_md->multidval.totalelements),
					tmp_md->multidval.n_dims,
					tmp_md->multidval.dim_sizes,
					0);
			NhlRLSet(rl_list,NrmQuarkToString(
				*(string*)(((NclMultiDValData)resname->u.data_obj)->multidval.val)),
				NhlTGenArray,
				gen_array[i]);
		} else {
/*
* Totally temporary code 6/21
*/
			ids = (int*)NclMalloc((unsigned)sizeof(int)*tmp_md->multidval.totalelements);
			for(j = 0; j < tmp_md->multidval.totalelements;j++) {
				tmp_ho = (NclHLUObj)_NclGetObj(((int*)tmp_md->multidval.val)[j]);
				ids[j] = tmp_ho->hlu.hlu_id;
			}
			if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK){
				gen_array[i] = _NhlCreateGenArray(
					(NhlPointer)ids,
					NhlTInteger,
					sizeof(int),
					1,
					&tmp_md->multidval.totalelements,
					1);
				NhlRLSet(rl_list,NrmQuarkToString(
					*(string*)(((NclMultiDValData)resname->u.data_obj)->multidval.val)),
					NhlTGenArray,
					gen_array[i]);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"The value associated with (%s) does not have an HLU representation",
						NrmQuarkToString(*(string*)(((NclMultiDValData)resname->u.data_obj)->multidval.val)));
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
	tmp_ho = _NclHLUObjCreate(NULL,nclHLUObjClass,Ncl_HLUObj,0,TEMPORARY,tmp_ho_id,(parent_id == -1 ? -1 : *(int*)parent->multidval.val),the_hlu_obj_class->u.obj_class_ptr); 
	*tmp_id = tmp_ho->obj.id;
	tmp_md = _NclMultiDValHLUObjDataCreate(
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
	if(tmp_md != NULL) {
		data_out.u.data_obj = tmp_md;
		data_out.kind = NclStk_VAL;
	} else {
		data_out.u.data_obj = NULL;
		data_out.kind = NclStk_NOVAL;
	}
	for(i = 0; i < nres; i++) {
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
	NclHLUObj hlu_ptr;
	int i,n_items = 0;
	NclMultiDValData tmp_md= NULL;
	NclStackEntry tmp_data;
	NhlErrorTypes ret = NhlNOERROR;
	int rl_list;

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
				ret = _NclBuildArray(n_items,&out_data);
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
	int i,j;
	NclStackEntry *data,*resname;
	int rl_list;
	static int local_rl_list = 0;
	NhlGenArray *gen_array;
	NclMultiDValData tmp_md = NULL;
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
		if(((data->kind != NclStk_VAL)&&(data->kind != NclStk_VAR))||(resname->kind != NclStk_VAL)) {
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
		}
		if(tmp_md->multidval.hlu_type_rep[0] != NULL) {
			gen_array[i] = _NhlCreateGenArray(
					(NhlPointer)tmp_md->multidval.val,
					tmp_md->multidval.hlu_type_rep[0],
					(int)(tmp_md->multidval.totalsize/tmp_md->multidval.totalelements),
					tmp_md->multidval.n_dims,
					tmp_md->multidval.dim_sizes,
					0);
			NhlRLSet(rl_list,NrmQuarkToString(
				*(string*)(((NclMultiDValData)resname->u.data_obj)->multidval.val)),
				NhlTGenArray,
				gen_array[i]);
		} else {
			ids = (int*)NclMalloc((unsigned)sizeof(int)*tmp_md->multidval.totalelements);
			for(j = 0; j < tmp_md->multidval.totalelements;j++) {
                                tmp_ho = (NclHLUObj)_NclGetObj(((int*)tmp_md->multidval.val)[j]);
                                ids[j] = tmp_ho->hlu.hlu_id;
                        }
			if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK){
                                gen_array[i] = _NhlCreateGenArray(
                                        (NhlPointer)ids,
                                        NhlTInteger,
                                        sizeof(int),
                                        1,
                                        &tmp_md->multidval.totalelements,
                                        0);
                                NhlRLSet(rl_list,NrmQuarkToString(
                                        *(string*)(((NclMultiDValData)resname->u.data_obj)->multidval.val)),
                                        NhlTGenArray,
                                        gen_array[i]);
                        } else {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"The value associated with (%s) does not have an HLU representation",
                                                NrmQuarkToString(*(string*)(((NclMultiDValData)resname->u.data_obj)->multidval.val)));
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
		hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
		if((hlu_ptr != NULL) &&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
			NhlSetValues(hlu_ptr->hlu.hlu_id,rl_list);
		}
	}
	for(i = 0; i < nres; i++) {
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
/*
	unsigned int allowed_types = (NCL_VAL_NUMERIC_MASK | NCL_VAL_CHARSTR_MASK | NCL_HLU_MASK);
*/
	NclScalar missing_val;
	NclMultiDValData missing_md,tmp_md,size_md,tmp1_md;
	void *tmp_val;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	short tmp_missing = NCL_DEFAULT_MISSING_VALUE;
	long *dim_size_list,total;
	int i;
	NclTypeClass typec = NULL;
	

	the_type = _NclKeywordToDataType(data_type);
	the_obj_type = _NclKeywordToObjType(data_type);
	typec = (NclTypeClass)_NclTypeEnumToTypeClass(the_obj_type);
	if(the_obj_type == NCL_VAL_NUMERIC_MASK) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"New: Keyword numeric is too general, can't determine the size of data");
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
		if(missing_md->multidval.type->type_class.type != the_obj_type) {
			tmp_md = _NclCoerceData(missing_md,the_obj_type,NULL);
			if(tmp_md == NULL) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"New: Could not coerce missing value parameter into appropriate type, using default");
				missing_val = typec->type_class.default_mis;
			} else {
				if(missing_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)missing_md);
				}
				missing_md = tmp_md;
				memcpy((void*)&missing_val,(void*)missing_md->multidval.val,missing_md->multidval.type->type_class.size);
			}
		} else {
			memcpy((void*)&missing_val,(void*)missing_md->multidval.val,missing_md->multidval.type->type_class.size);
		}
	} else {
		if(the_obj_type & NCL_VAL_TYPE_MASK) {
			dim_sizes[0] = 1;
			missing_val = typec->type_class.default_mis;
		} else if(the_obj_type & NCL_MD_MASK) {
			
			missing_val.objval = (obj)tmp_missing;
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
		if(size_md->multidval.missing_value.has_missing) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"New: The dimension size list contains missing values, can't determine size");
			return(NhlFATAL);
		}
		if(!(size_md->multidval.type->type_class.type & Ncl_Typelong)) {
			tmp1_md = _NclCoerceData(size_md,Ncl_Typelong,NULL);
			if(tmp1_md == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"New: the dimension size parameter is the wrong type an integer value was expected");
				return(NhlFATAL);
			}
		} else {
			tmp1_md = size_md;
		}
		dim_size_list = (long*)tmp1_md->multidval.val;
		total = 1;
		for(i = 0; i< tmp1_md->multidval.dim_sizes[0]; i++) {
			if(dim_size_list[i] < 1) {	
				NhlPError(NhlFATAL,NhlEUNKNOWN,"New: a zero or negative value has been passed in in the dimension size parameter");
				return(NhlFATAL);
			}
			total *= dim_size_list[i];
			dim_sizes[i] = (int)dim_size_list[i];
		}
		tmp_val = (void*)NclMalloc((unsigned int)total*_NclSizeOf(the_type));
		for(i = 0; i< total*_NclSizeOf(the_type); i+=_NclSizeOf(the_type)) {
			memcpy((void*)&(((char*)tmp_val)[i]),(void*)&missing_val,_NclSizeOf(the_type));
			
		}
		tmp_md = _NclCreateVal(NULL,NULL,((the_obj_type & NCL_VAL_TYPE_MASK) ? Ncl_MultiDValData:the_obj_type),0,tmp_val,&missing_val,tmp1_md->multidval.totalelements,dim_sizes,TEMPORARY,NULL,(NclObjClass)((the_obj_type & NCL_VAL_TYPE_MASK) ?_NclTypeEnumToTypeClass(the_obj_type):NULL));
		if(tmp1_md != size_md) {
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
