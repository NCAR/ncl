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


void _NclPrint
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
		(*((NclDataClass)(data.u.data_obj)->obj.class_ptr)->data_class.print)((NclData)(data.u.data_obj),fp);
		break;
	case NclStk_VAR:
/*
		(*((NclVarClass)(data.u.data_var)->obj.class_ptr)->var_class.print)((NclVar)(data.u.data_var),fp);
		break;
*/
	default:
		break;
	}
	return;
}
NclStackEntry _NclRetrieveRec
#if  __STDC__
(NclSymbol* the_sym)
#else
(the_sym)
NclSymbol* the_sym;
#endif
{
	NclStackEntry data;
	return (data);
}

NclMultiDValData _NclCoerceData
#if  __STDC__
(NclMultiDValData obj, NclObjTypes coerce_to)
#else
(obj, coerce_to)
NclMultiDValData obj;
NclObjTypes coerce_to;
#endif
{
	int f_selection;

	f_selection = (int)obj->multidval.kind;
	if( ((NclDataClass)obj->obj.class_ptr)->data_class.coerce[f_selection] != NULL) {
		return((NclMultiDValData)(*((NclDataClass)obj->obj.class_ptr)->data_class.coerce[f_selection])((NclData)obj,coerce_to));
	} else {
		return(NULL);
	}
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
	NclMultiDValData lhs_data_obj;
	NclMultiDValData rhs_data_obj;
	NclMultiDValData coerce_res;
	NclDataClassPart *data_part;
	int f_selection;
	NhlErrorTypes ret = NOERROR;

	if(lhs.kind == NclStk_VAL) {
		lhs_data_obj = (NclMultiDValData)lhs.u.data_obj;
/*
	} else if(lhs.kind == NclStk_VAR) {
		lhs_data_obj = (NclMultiDValData)(*(NclMultiDValData*)lhs.u.data_var);
*/
	} else {
		return(FATAL);
	}

	if(rhs.kind == NclStk_VAL) {
		rhs_data_obj = (NclMultiDValData)rhs.u.data_obj;
/*
	} else if(rhs.kind == NclStk_VAR) {
		rhs_data_obj = (NclMultiDValData)(*(NclMultiDValData*)rhs.u.data_var);
*/
	} else {
		return(FATAL);
	}

	if((lhs_data_obj->obj.obj_type_mask & NCL_VAL_TYPE_MASK) !=
		(rhs_data_obj->obj.obj_type_mask & NCL_VAL_TYPE_MASK)) {

		coerce_res = _NclCoerceData(rhs_data_obj,
				lhs_data_obj->obj.obj_type_mask & NCL_VAL_TYPE_MASK);
		if(coerce_res == NULL) {
			coerce_res = _NclCoerceData(lhs_data_obj,
				rhs_data_obj->obj.obj_type_mask & NCL_VAL_TYPE_MASK);
			if(coerce_res == NULL) {
/*
* Error message needed
*/
				return(FATAL);
			} else {
				if(lhs_data_obj->obj.status != PERMANENT) {
					(*lhs_data_obj->obj.class_ptr->obj_class.destroy)((NclData)lhs_data_obj);
				}
				lhs_data_obj = coerce_res;
			}
		} else {
			if(rhs_data_obj->obj.status != PERMANENT) {
				(*rhs_data_obj->obj.class_ptr->obj_class.destroy)((NclData)rhs_data_obj);
			}
			rhs_data_obj = coerce_res;
		}
	}
	data_part = (NclDataClassPart*)
		&(((NclDataClass)lhs_data_obj->obj.class_ptr)->data_class);

	f_selection = (int)
		((lhs_data_obj->multidval.kind<< 1)
		|(rhs_data_obj->multidval.kind));
	switch(operation) {
	case MOD_OP:
	if(data_part->mod[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->mod[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case OR_OP:
	if(data_part->or[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->or[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case AND_OP:
	if(data_part->and[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->and[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case XOR_OP:
	if(data_part->xor[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->xor[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case LTSEL_OP:
	if(data_part->sel_lt[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->sel_lt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case GTSEL_OP:
	if(data_part->sel_gt[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->sel_gt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case PLUS_OP:
	if(data_part->plus[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->plus[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case MINUS_OP:
	if(data_part->minus[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->minus[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case MUL_OP:
	if(data_part->multiply[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->multiply[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case MAT_OP:
	if(data_part->mat[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->mat[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case DIV_OP:
	if(data_part->divide[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->divide[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case EXP_OP:
	if(data_part->exponent[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->exponent[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case LE_OP:
	if(data_part->le[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->le[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case GE_OP:
	if(data_part->ge[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->ge[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case GT_OP:
	if(data_part->gt[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->gt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case LT_OP:
	if(data_part->lt[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->lt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case EQ_OP:
	if(data_part->eq[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->eq[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	case NE_OP:
	if(data_part->ne[f_selection] != NULL) {
		result->u.data_obj = (NclData)((*data_part->ne[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		result->kind = NclStk_VAL;
		if(result->u.data_obj != NULL) {
			ret = NOERROR;
		} else {
			ret = FATAL;
		}
	}
	break;
	default:
		return(FATAL);
	}
	if(lhs_data_obj->obj.status !=PERMANENT ) {
		(*lhs_data_obj->obj.class_ptr->obj_class.destroy)((NclData)lhs_data_obj);
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
        NclStackEntry data;
	switch(operation) {
	case NOT_OP:
	break;
	case NEG_OP:
	break;
	}

        return(NOERROR);
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
	int kind;
	NclBasicDataTypes type;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	NclMultiDValData theobj,coerce_res;
	NclStackEntry *data_ptr;
	NclObjTypes result_type ;
	NclBasicDataTypes result_data_type;
	int must_be_numeric = 1,i;
	int ndims;

	

/*
* First element determines whether the type of the result array is numerci
* or textual
*/
	data_ptr = _NclPeek(0);
	if(data_ptr->kind == NclStk_VAL) {	
		theobj = (NclMultiDValData)data_ptr->u.data_obj;
/*
	} else if(data_ptr->kind == NclStk_VAR) {
		theobj = (NclMultiDValData)*(data_ptr->u.data_var);
*/
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type, can't continue");
		return(FATAL);
	}
	if(theobj->obj.obj_type_mask & NCL_VAL_NUMERIC_MASK) {
		must_be_numeric =1;
		result_type = theobj->obj.obj_type_mask & NCL_VAL_NUMERIC_MASK;
		result_data_type = theobj->multidval.data_type;
	} else if(theobj->obj.obj_type_mask & NCL_VAL_CHARSTR_MASK) {
		must_be_numeric =0;
		result_type = theobj->obj.obj_type_mask & NCL_VAL_CHARSTR_MASK;
		result_data_type = theobj->multidval.data_type;
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type, can't continue");
		return(FATAL);
	}

/*
* The following loop figures out what the final type of the result array
* should be so the appropriate space can be allocated and the elements not
* of this type can be coerced into the correct type
*/	
	for(i = 1; i< n_items; i++) {
		data_ptr = _NclPeek(i);
		if(data_ptr->kind == NclStk_VAL) {	
			theobj = (NclMultiDValData)data_ptr->u.data_obj;
/*
		} else if(data_ptr->kind == NclStk_VAR) {
			theobj = (NclMultiDValData)*(data_ptr->u.data_var);
*/
		} else {
			NhlPError(FATAL,E_UNKNOWN,"_NclBuildArray: attempt to build array out of illegal data type, can't continue");
			return(FATAL);
		}
		if((must_be_numeric)&&
			(theobj->obj.obj_type_mask&NCL_VAL_NUMERIC_MASK)) {
			if(result_type > (theobj->obj.obj_type_mask&NCL_VAL_NUMERIC_MASK)) {
				result_type = (theobj->obj.obj_type_mask&NCL_VAL_NUMERIC_MASK);
				result_data_type = theobj->multidval.data_type;
			}
		} else if((!must_be_numeric)&&
			(theobj->obj.obj_type_mask & NCL_VAL_CHARSTR_MASK)) {
			if(result_type > (theobj->obj.obj_type_mask & NCL_VAL_CHARSTR_MASK)) {
				result_type = (theobj->obj.obj_type_mask&NCL_VAL_CHARSTR_MASK);
				result_data_type = theobj->multidval.data_type;
			}
		} else {
			NhlPError(FATAL,E_UNKNOWN,"_NclBuildArray: can not combine character or string types with numeric types, can't continue");
			return(FATAL);
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
/*
	} else if(data.kind == NclStk_VAR){
		theobj =(NclMultiDValData) *data.u.data_var;	
*/
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NclBuildArray: unknown stack data type");
		return(FATAL);
	}
	partsize = theobj->multidval.totalelements * _NclSizeOf(result_data_type);

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
		NhlPError(FATAL,E_UNKNOWN,"_NclBuildArray : Memory allocation failure\n");
		result->kind = NclStk_NOVAL;
		result->u.data_obj = NULL; 
		return(FATAL);
	}
	ptr = (char*)value;
	if(!(theobj->obj.obj_type_mask & result_type)) {
		coerce_res = _NclCoerceData(theobj,result_type);
		if(coerce_res == NULL) {
			NhlPError(FATAL,E_UNKNOWN,"An Error occured that should not have happend");
		}
		if(theobj->obj.status != PERMANENT) {
		 	(*theobj->obj.class_ptr->obj_class.destroy)((NclData)theobj);
		}
		theobj = coerce_res;
	}
	memcpy(ptr,(char*)theobj->multidval.val,partsize);
	ptr += partsize;
	if(theobj->obj.status != PERMANENT) {
	 	(*theobj->obj.class_ptr->obj_class.destroy)((NclData)theobj);
	}
	while(items_left) {
		data = _NclPop();
		items_left--;
		if(data.kind == NclStk_VAL) {
			theobj = (NclMultiDValData)data.u.data_obj;
/*
		} else if(data.kind == NclStk_VAR){
			theobj = (NclMultiDValData)*data.u.data_var;	
*/
		} else {
			NhlPError(FATAL,E_UNKNOWN,"_NclBuildArray: unknown stack data type");
			return(FATAL);
		}
		if(!(theobj->obj.obj_type_mask & result_type)) {
			coerce_res = _NclCoerceData(theobj,result_type);
			if(coerce_res == NULL) {
/*
* This should not happen because the beginning loops assure that all elements
* are coercible to result_type.
*/
				NhlPError(FATAL,E_UNKNOWN,"An Error occured that should not have happend");
			}
			if(theobj->obj.status != PERMANENT) {
			 	(*theobj->obj.class_ptr->obj_class.destroy)((NclData)theobj);
			}
			theobj = coerce_res;
		}
		memcpy(ptr,(char*)theobj->multidval.val,partsize);
		ptr += partsize;
		if(theobj->obj.status != PERMANENT) {
		 	(*theobj->obj.class_ptr->obj_class.destroy)((NclData)theobj);
		}
	}
	result->kind = NclStk_VAL;
/*
*
* ------------__> stilll need to handle dim info
*/
	switch(result_data_type) {
	case NCL_double:
	result->u.data_obj = (NclData)_NclMultiDValdoubleCreate(NULL,value,NULL,ndims,dim_sizes,NULL,TEMPORARY,NULL);
	break;
	case NCL_float:
	result->u.data_obj = (NclData)_NclMultiDValfloatCreate(NULL,value,NULL,ndims,dim_sizes,NULL,TEMPORARY,NULL);
	break;
	case NCL_long:
	result->u.data_obj = (NclData)_NclMultiDVallongCreate(NULL,value,NULL,ndims,dim_sizes,NULL,TEMPORARY,NULL);
	break;
	case NCL_int:
	result->u.data_obj =(NclData) _NclMultiDValintCreate(NULL,value,NULL,ndims,dim_sizes,NULL,TEMPORARY,NULL);
	break;
	case NCL_short:
	result->u.data_obj = (NclData)_NclMultiDValshortCreate(NULL,value,NULL,ndims,dim_sizes,NULL,TEMPORARY,NULL);
	break;
	case NCL_string:
	result->u.data_obj = (NclData)_NclMultiDValstringCreate(NULL,value,NULL,ndims,dim_sizes,NULL,TEMPORARY,NULL);
	break;
	case NCL_char:
	default:
		return(FATAL);
	}
	return(NOERROR);
}


#ifdef __cplusplus
}
#endif
