#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <data_objs/NclMultiDValdoubleData.h>
#include <data_objs/NclMultiDValfloatData.h>
#include <data_objs/NclMultiDVallongData.h>
#include <data_objs/NclMultiDValintData.h>
#include <data_objs/NclMultiDValshortData.h>
#include <data_objs/NclMultiDValstringData.h>
#include <defs.h>
#include <Symbol.h>
#include <errno.h>
#include <OpsList.h>
#include <Machine.h>
#include <Execute.h>
#include <OpsFuncs.h>
#include <data_objs/NclVar.h>
#include <data_objs/DataSupport.h>
#include <y.tab.h>


void _NclIPrint
#if  __STDC__
(void)
#else
()
#endif
{
	NclStackEntry data;
	FILE *fp;
	

	data = _NclGetArg(0,1);
	fp = stdout;
/*_NclGetOutputStream();*/

	switch(data.kind) {
	case NclStk_VAL:
		_NclMultiDValPrint(data.u.data_obj,fp);
		break;
	case NclStk_VAR:
		_NclVarPrint(data.u.data_var,fp);
		break;
	default:
		break;
	}
	return;
}

void _NclIDelete
#if  __STDC__
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclStackEntry* var;
	NclSymbol *thesym;
	

	data = _NclGetArg(0,1);

	switch(data.kind) {
	case NclStk_VAL:
		_NclDestroyObj((NclObj)data.u.data_obj);
		break;
	case NclStk_VAR:
		if((data.u.data_var != NULL)&&(data.u.data_var->var.thesym != NULL)) {
			var = _NclRetrieveRec(data.u.data_var->var.thesym);
			thesym = data.u.data_var->var.thesym;
			if(data.u.data_var->var.var_type == NORMAL) {
/*
* Can't destroy symbol since it may be referenced from the instruction
* sequence. Changing it to UNDEF should do the trick though
*/
				_NclChangeSymbolType(thesym,UNDEF);
			}
		} else {
			var = NULL;
		}
		_NclDestroyObj((NclObj)data.u.data_var);
		if(var != NULL) {
			var->u.data_var = NULL;
			var->kind = NclStk_NOVAL;
		}
		break;
	default:
		break;
	}
	data.kind = NclStk_NOVAL;
	data.u.data_obj = NULL;
	_NclPutArg(data,0,1);
	
	return;
}
NhlErrorTypes _NclDualOp
#if  __STDC__
(NclStackEntry lhs, NclStackEntry rhs,NclStackEntry *result,int operation)
#else
(lhs, rhs,result,operation)
NclStackEntry lhs;
NclStackEntry rhs;
NclStackEntry *result;
int operation ;
#endif
{
	NclMultiDValData lhs_data_obj = NULL;
	NclMultiDValData rhs_data_obj = NULL;
	NclMultiDValData coerce_res = NULL;
	NhlErrorTypes ret = NhlNOERROR;
	int lhs_type;
	int rhs_type;


	if(lhs.kind == NclStk_VAL) {
		lhs_type = lhs.u.data_obj->obj.obj_type_mask & NCL_VAL_TYPE_MASK;
	} else if(lhs.kind == NclStk_VAR) {
		lhs_type = _NclGetVarRepValue(lhs.u.data_var);
	} else {
		return(NhlFATAL);
	}

	if(rhs.kind == NclStk_VAL) {
		rhs_type = rhs.u.data_obj->obj.obj_type_mask & NCL_VAL_TYPE_MASK;
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
			} else {
/*
* No need to pass in missing value since it will be used appropriately
* by the operator's function
*/
			coerce_res = _NclCoerceVar(lhs.u.data_var,
				rhs_type,NULL);
			}
			if(coerce_res == NULL) {
/*
* Error message needed
*/
				return(NhlFATAL);
			} else {
				if(lhs.u.data_obj->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)lhs.u.data_obj);
				}
				lhs_data_obj = coerce_res;
				if(rhs.kind == NclStk_VAL) {
					rhs_data_obj = rhs.u.data_obj;
				} else {
					rhs_data_obj = _NclVarValueRead(rhs.u.data_var,NULL,NULL);
				}

			}
		} else {
			if(rhs.u.data_obj->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs.u.data_obj);
			}
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
		ret = _NclCallDualOp(lhs_data_obj,rhs_data_obj,operation,result);
	} else {
		return(NhlFATAL);
	}

	if(lhs_data_obj->obj.status !=PERMANENT ) {
		_NclDestroyObj((NclObj)lhs_data_obj);
	}
	if(rhs_data_obj->obj.status !=PERMANENT ) {
		_NclDestroyObj((NclObj)rhs_data_obj);
	}

	return(ret);
}


NhlErrorTypes _NclMonoOp
#if  __STDC__
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

	ret = _NclCallMonoOp(operand_md,result,operation);

	if(operand_md->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)operand_md);
	}

        return(ret);
}


NhlErrorTypes _NclBuildArray
#if __STDC__
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
	int must_be_numeric = 1,i;
	int ndims;
	NclScalar *mis_ptr = NULL,themissing;
	int still_no_missing = 1;

	

/*
* First element determines whether the type of the result array is numerci
* or textual
*/
	data_ptr = _NclPeek(0);
	if(data_ptr->kind == NclStk_VAL) {	
		obj_type = data_ptr->u.data_obj->obj.obj_type_mask;
	} else if(data_ptr->kind == NclStk_VAR) {
		obj_type = _NclGetVarRepValue(data_ptr->u.data_var);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type, can't continue");
		return(NhlFATAL);
	}
	if(obj_type & NCL_VAL_NUMERIC_MASK) {
		must_be_numeric =1;
		result_type = obj_type & NCL_VAL_NUMERIC_MASK;
	} else if(obj_type & NCL_VAL_CHARSTR_MASK) {
		must_be_numeric =0;
		result_type = obj_type & NCL_VAL_CHARSTR_MASK;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type or undefined element, can't continue");
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
			obj_type = data_ptr->u.data_obj->obj.obj_type_mask;
		} else if(data_ptr->kind == NclStk_VAR) {
			obj_type = _NclGetVarRepValue(data_ptr->u.data_var);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type, can't continue");
			return(NhlFATAL);
		}
		if((must_be_numeric)&&
			( obj_type &NCL_VAL_NUMERIC_MASK)) {
			if(result_type > (obj_type & NCL_VAL_NUMERIC_MASK)) {
				result_type = (obj_type & NCL_VAL_NUMERIC_MASK);
			}
		} else if((!must_be_numeric)&&
			(obj_type & NCL_VAL_CHARSTR_MASK)) {
			if(result_type > (obj_type & NCL_VAL_CHARSTR_MASK)) {
				result_type = (obj_type & NCL_VAL_CHARSTR_MASK);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclBuildArray: can not combine character or string types with numeric types, can't continue");
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
		if(!(theobj->obj.obj_type_mask & result_type)) {
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
		return(NhlFATAL);
	}
/*
****** HEY THIS DOESN"T WORK FOR STRINGS 
*/
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
			if(!(theobj->obj.obj_type_mask & result_type)) {
				coerce_res = _NclCoerceData(theobj,result_type,mis_ptr);
				if(coerce_res == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
					NhlPError(NhlFATAL,NhlEUNKNOWN,"An Error occured that should not have happend");
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
* ------------__> stilll need to handle dim info
*/
	result->u.data_obj = _NclCreateVal(NULL,NULL,result_type,0,value,NULL,ndims,dim_sizes,TEMPORARY,NULL);
	if(result->u.data_obj != NULL) 
		return(NhlNOERROR);
	else 
		return(NhlFATAL);
}


#ifdef __cplusplus
}
#endif
