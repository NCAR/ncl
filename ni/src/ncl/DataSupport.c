/*
 *      $Id: DataSupport.c,v 1.9 1994-12-23 01:17:19 ethan Exp $
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
 *	Date:		Thu Jan 13 14:52:04 MST 1994
 *
 *	Description:	
 */

#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "parser.h"
#include "OpsList.h"

extern void _NclInitDataClasses
#if     NhlNeedProto
(void)
#else
()
#endif
{
	_NclInitClass(nclMultiDValdoubleDataClass);
	_NclInitClass(nclMultiDValfloatDataClass);
	_NclInitClass(nclMultiDValintDataClass);
	_NclInitClass(nclMultiDValshortDataClass);
	_NclInitClass(nclMultiDVallongDataClass);
	_NclInitClass(nclMultiDVallogicalDataClass);
	_NclInitClass(nclMultiDValbyteDataClass);
	_NclInitClass(nclMultiDValcharDataClass);
	_NclInitClass(nclMultiDValstringDataClass);
}



NclMultiDValData _NclStringMdToCharMd
#if	NhlNeedProto
(NclMultiDValData str_md)
#else
(str_md)
NclMultiDValData str_md;
#endif
{
	char **buffer;
	int i,n_dims;
	string *value;
	int max_len = 0,tmp_len =0,to = 0;
	char *val = NULL;
	int dim_sizes[NCL_MAX_DIMENSIONS];

	buffer = (char**)NclMalloc((unsigned)str_md->multidval.totalelements*sizeof(char*));
	value = (string*)str_md->multidval.val;
	for(i = 0; i < str_md->multidval.totalelements; i++) {
		buffer[i] = NrmQuarkToString(value[i]);
		tmp_len = strlen(buffer[i]) + 1;
		if((buffer[i] != NULL) &&(tmp_len > max_len)) {
			max_len = tmp_len;
		}
	}
	if(str_md->multidval.kind == SCALAR) {
		dim_sizes[0] = max_len;
		n_dims = 1;
	} else {
		for(i = 0; i < str_md->multidval.n_dims; i++) {
			dim_sizes[i] = str_md->multidval.dim_sizes[i];
		}
		dim_sizes[str_md->multidval.n_dims] = max_len;
		n_dims = str_md->multidval.n_dims +1;
	}
	val = (char*)NclMalloc(max_len * str_md->multidval.totalelements);
	for(i = 0; i < str_md->multidval.totalelements; i++) {
		strcpy(&(val[to]),buffer[i]);
		to += max_len;
	}
	return(_NclMultiDValcharCreate(
		NULL,
		NULL,
		Ncl_MultiDValcharData,
		0,
		val,
		NULL,
		n_dims,
		dim_sizes,
		TEMPORARY,
		NULL));
}
NclMultiDValData _NclCharMdToStringMd
#if	NhlNeedProto
(NclMultiDValData char_md)
#else
(char_md)
NclMultiDValData char_md;
#endif
{
	int i;
	int n_strings = 1;
	char *buffer = NULL;
	int len =0;
	int from = 0;
	string *value = NULL;
	char *val = NULL;
	
	len = char_md->multidval.dim_sizes[char_md->multidval.n_dims -1];

	for( i = 0; i < char_md->multidval.n_dims - 1; i++) {
		n_strings *= char_md->multidval.dim_sizes[i];
	}
	buffer = (char*)NclMalloc((unsigned)len + 1);
	buffer[len] = '\0';
	value = (string*) NclMalloc((unsigned)n_strings*sizeof(string));
	val = (char*)char_md->multidval.val;
	for(i = 0; i < n_strings ; i++) {
		memcpy((void*)buffer,&(val[from]),len);
		value[i] = NrmStringToQuark(buffer);
	}
	NclFree(buffer);
	if(char_md->multidval.n_dims != 1) {
		return(_NclMultiDValstringCreate(
			NULL,
			NULL,
			NCL_string,
			0,
			(void*)value,
			NULL,
			char_md->multidval.n_dims -1,
			char_md->multidval.dim_sizes,
			TEMPORARY,
			NULL));
	} else {
		return(_NclMultiDValstringCreate(
			NULL,
			NULL,
			NCL_string,
			0,
			(void*)value,
			NULL,
			char_md->multidval.n_dims,
			&(char_md->multidval.n_dims),
			TEMPORARY,
			NULL));
	}
}

/*
* Probably should put stuff like this in the actual data object to avoid having to
* add objects here later
*/

long _NclObjTypeToName
#if	NhlNeedProto
(NclObjTypes obj)
#else
(obj)
	NclObjTypes obj;
#endif
{
	switch(obj) {
	case Ncl_MultiDValintData:
		return(NrmStringToQuark("Ncl_MultiDValintData"));
	case Ncl_MultiDValdoubleData:
		return(NrmStringToQuark("Ncl_MultiDValdoubleData"));
	case Ncl_MultiDValbyteData:
		return(NrmStringToQuark("Ncl_MultiDValbyteData"));
	case Ncl_MultiDVallongData:
		return(NrmStringToQuark("Ncl_MultiDVallongData"));
	case Ncl_MultiDValshortData:
		return(NrmStringToQuark("Ncl_MultiDValshortData"));
	case Ncl_MultiDValfloatData:
		return(NrmStringToQuark("Ncl_MultiDValfloatData"));
	case Ncl_MultiDValcharData:
		return(NrmStringToQuark("Ncl_MultiDValcharData"));
	case Ncl_MultiDValstringData:
		return(NrmStringToQuark("Ncl_MultiDValstringData"));
	case Ncl_MultiDValHLUObjData:
		return(NrmStringToQuark("Ncl_MultiDValHLUObjData"));
	case Ncl_MultiDVallogicalData:
		return(NrmStringToQuark("Ncl_MultiDVallogicalData"));
	case Ncl_MultiDValnclfileData:
		return(NrmStringToQuark("Ncl_MultiDValnclfileData"));
	default:
		return(-1);
	}
}
unsigned int _NclKeywordToObjType
#if	NhlNeedProto
(struct _NclSymbol *keywd)
#else
(keywd)
	struct _NclSymbol *keywd;
#endif
{
	if(keywd != NULL) {
		switch(keywd->type) {
		case INTEGER:
			return((unsigned int)Ncl_MultiDValintData);
		case DOUBLE:
			return((unsigned int)Ncl_MultiDValdoubleData);
		case BYTE:
			return((unsigned int)Ncl_MultiDValbyteData);
		case LONG:
			return((unsigned int)Ncl_MultiDVallongData);
		case SHORT:
			return((unsigned int)Ncl_MultiDValshortData);
		case FLOAT:
			return((unsigned int)Ncl_MultiDValfloatData);
		case CHARACTER:
			return((unsigned int)Ncl_MultiDValcharData);
		case STRNG:
			return((unsigned int)Ncl_MultiDValstringData);
		case NUMERIC:
			return(NCL_VAL_NUMERIC_MASK);
		case GRAPHIC:
			return((unsigned int)Ncl_MultiDValHLUObjData);
		case LOGICAL:
			return((unsigned int)Ncl_MultiDVallogicalData);
		case FILETYPE:
			return((unsigned int)Ncl_MultiDValnclfileData);
		default:
			return(Ncl_Obj);
		}
	} else {
		return(Ncl_Obj);
	}
}

NclObjTypes _NclBasicDataTypeToObjType
#if	NhlNeedProto
(NclBasicDataTypes dt)
#else
(dt)
	NclBasicDataTypes dt;
#endif
{
	switch(dt) {
	case NCL_short:
		return(Ncl_MultiDValshortData);
	case NCL_int:
		return(Ncl_MultiDValintData);
	case NCL_long:
		return(Ncl_MultiDVallongData);
	case NCL_float:
		return(Ncl_MultiDValfloatData);
	case NCL_double:
		return(Ncl_MultiDValdoubleData);
	case NCL_char:
		return(Ncl_MultiDValcharData);
	case NCL_byte:
		return(Ncl_MultiDValbyteData);
	case NCL_string:
		return(Ncl_MultiDValstringData);
	case NCL_logical:
		return(Ncl_MultiDVallogicalData);
	case NCL_nclfile:
		return(Ncl_MultiDValnclfileData);
	default:
		return(Ncl_Obj);
	}
}

NclBasicDataTypes _NclKeywordToDataType
#if	NhlNeedProto
(struct _NclSymbol *keywd)
#else
(keywd)
	struct _NclSymbol *keywd;
#endif
{

	if(keywd != NULL) {
		switch(keywd->type) {
		case INTEGER:
		case GRAPHIC:
			return(NCL_int);
		case DOUBLE:
			return(NCL_double);
		case BYTE:
			return(NCL_byte);
		case LONG:
			return(NCL_long);
		case SHORT:
			return(NCL_short);
		case FLOAT:
			return(NCL_float);
		case CHARACTER:
			return(NCL_char);
		case STRNG:
			return(NCL_string);
		case NUMERIC:
			return(NCL_numeric);
		case LOGICAL:
			return(NCL_logical);
		case FILETYPE:
			return(NCL_nclfile);
		default:
			return(NCL_none);
		}
	} else {
		return(NCL_none);
	}
}


int _NclSizeOf
#if	NhlNeedProto
(NclBasicDataTypes data_type)
#else 
(data_type)
NclBasicDataTypes data_type;
#endif
{
	switch(data_type) {
		case NCL_short:
			return(sizeof(short));
		case NCL_int:
			return(sizeof(int));
		case NCL_long:
			return(sizeof(long));
		case NCL_float:
			return(sizeof(float));
		case NCL_double:
			return(sizeof(double));
		case NCL_char:
			return(sizeof(char));
		case NCL_byte:
			return(sizeof(char));
		case NCL_string:
			return(sizeof(int));	
		case NCL_logical:
			return(sizeof(int));	
		default:
			return(-1);
	}
}

void _NclDestroyObj
#if	NhlNeedProto
(NclObj obj)
#else
(obj)
	NclObj obj;
#endif
{
	NclObjClass oc;

	if(obj == NULL)  {
		return;
	} else {
		oc = obj->obj.class_ptr;
	}

	while(oc != NULL) {
		if(oc->obj_class.destroy != NULL)  {
			(*(oc->obj_class.destroy))(obj);
			return;
		} else {
			oc = oc->obj_class.super_class;
		}
	} 
	NclFree((void*)obj);
	return;
}


int _NclScalarCoerce
#if	NhlNeedProto
(void *from,NclBasicDataTypes frtype,void *to,NclBasicDataTypes totype)
#else
(from,frtype,to,totype)
void *from;
NclBasicDataTypes frtype;
void *to;
NclBasicDataTypes totype;
#endif
{
	switch(frtype) {
	case NCL_short:
		switch(totype) {
		case NCL_short:
			*(short*)to= *(short*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(short*)from;
			return(1);
		case NCL_int:
			*(int*)to = *(short*)from;
			return(1);
		case NCL_float:
			*(float*)to = *(short*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(short*)from;
			return(1);
		case NCL_logical:
			*(int*)to = *(short*)from;
			return(1);
		default:
			return(0);
		}
	case NCL_int:
		switch(totype) {
		case NCL_long:
			*(long*)to = *(int*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(int*)from;
			return(1);
                case NCL_float:
			*(float*)to = *(int*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(int*)from;
			return(1);
                case NCL_logical:
			*(int*)to = *(int*)from;
			return(1);
		default:
			return(0);
		}
	case NCL_long:
		switch(totype) {
		case NCL_long:
			*(long*)to = *(long*)from;
			return(1);
		case NCL_float:
			*(float*)to = *(long*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(long*)from;
			return(1);
		case NCL_logical:
			*(int*)to = *(long*)from;
			return(1);
		default:
			return(0);
		}
	case NCL_float:
		switch(totype) {
		case NCL_float:
			*(float*)to = *(float*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(float*)from;
			return(1);
		default:
			return(0);
		}
	case NCL_double:
		switch(totype) {
		case NCL_double:
			*(double*)to = *(double*)from;
			return(1);
		default:
			return(0);
		}
	case NCL_char:
	case NCL_byte:
		switch(totype) {
		case NCL_byte:
		case NCL_char:
			*(char*) to = *(char*)from;
			return(1);
		default:
			return(0);
		}
	case NCL_string:
		switch(totype) {
		case NCL_string:
			*(string*)to = *(string*)from;
			return(1);
		default:
			return(0);
		}
	case NCL_logical:
	default:
		return(0);
	}
}

void _NclPrint
#if	NhlNeedProto
(NclObj obj,FILE *fp)
#else 
(obj,fp)
NclObj obj;
FILE *fp;
#endif
{
	NclObjClass oc;

	if(obj == NULL)  {
		return;
	} else {
		oc = obj->obj.class_ptr;
	}

	while(oc != NULL) {
		if(oc->obj_class.print != NULL)  {
			(*(oc->obj_class.print))(obj,fp);
			return;
		} else {
			oc = oc->obj_class.super_class;
		}
	} 
	return;
}


NclMultiDValData _NclCoerceData
#if	NhlNeedProto
(NclMultiDValData obj, NclObjTypes coerce_to,NclScalar *new_missing)
#else
(obj, coerce_to,new_missing)
NclMultiDValData obj;
NclObjTypes coerce_to;
NclScalar *new_missing;
#endif
{
	NclDataClass oc;
        int f_selection;

	if((obj == NULL)||!(obj->obj.obj_type_mask & NCL_MD_MASK)) {
                return(NULL);
	} else {
		oc = (NclDataClass)obj->obj.class_ptr;
	}

        f_selection = (int)obj->multidval.kind;
	while((NclObjClass)oc != nclObjClass) {
        	if( oc->data_class.coerce[f_selection] != NULL) {
                	return((NclMultiDValData)((*oc->data_class.coerce[f_selection])((NclData)obj,coerce_to,new_missing)));
        	} else {
			oc = (NclDataClass)oc->obj_class.super_class;
		}
	}
}

NhlErrorTypes _NclCallDualOp
#if	NhlNeedProto
(NclMultiDValData lhs_data_obj, NclMultiDValData rhs_data_obj, int operation, NclObj *result)
#else
(lhs_data_obj, rhs_data_obj, operation,result)
NclMultiDValData lhs_data_obj;
NclMultiDValData rhs_data_obj;
int operation;
NclObj *result;
#endif
{
	NclDataClass data_part;
	int f_selection;

	data_part = (NclDataClass)
		((NclDataClass)lhs_data_obj->obj.class_ptr);

	f_selection = (int)
		((lhs_data_obj->multidval.kind<< 1)
		|(rhs_data_obj->multidval.kind));
	*result = NULL;
	while(((NclObjClass) data_part != nclObjClass)&&(*result == NULL)) {
		switch(operation) {
		case MOD_OP:
		if(data_part->data_class.mod[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.mod[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case OR_OP:
		if(data_part->data_class.or[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.or[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case AND_OP:
		if(data_part->data_class.and[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.and[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case XOR_OP:
		if(data_part->data_class.xor[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.xor[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case LTSEL_OP:
		if(data_part->data_class.sel_lt[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.sel_lt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case GTSEL_OP:
		if(data_part->data_class.sel_gt[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.sel_gt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case PLUS_OP:
		if(data_part->data_class.plus[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.plus[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case MINUS_OP:
		if(data_part->data_class.minus[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.minus[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case MUL_OP:
		if(data_part->data_class.multiply[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.multiply[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case MAT_OP:
		if(data_part->data_class.mat[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.mat[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case DIV_OP:
		if(data_part->data_class.divide[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.divide[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case EXP_OP:
		if(data_part->data_class.exponent[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.exponent[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case LE_OP:
		if(data_part->data_class.le[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.le[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case GE_OP:
		if(data_part->data_class.ge[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.ge[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case GT_OP:
		if(data_part->data_class.gt[f_selection] != NULL) {
			*result =(NclObj) ((*data_part->data_class.gt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case LT_OP:
		if(data_part->data_class.lt[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.lt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case EQ_OP:
		if(data_part->data_class.eq[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.eq[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		case NE_OP:
		if(data_part->data_class.ne[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.ne[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,NULL));
		}
		break;
		default:
			return(NhlFATAL);
		}
		data_part = (NclDataClass)data_part->obj_class.super_class;
	}
	if(*result != NULL) {
		return(NhlNOERROR);
	} else {
		return(NhlFATAL);
	}

}


NhlErrorTypes _NclCallMonoOp
#if	NhlNeedProto
(NclMultiDValData operand, NclObj *result, int operation)
#else
(operand, result,operation)
NclMultiDValData operand;
NclObj *result;
int operation;
#endif
{
	NclDataClass data_part;
	int f_selection;

	data_part = (NclDataClass)((NclDataClass)operand->obj.class_ptr);

	f_selection = (int)operand->multidval.kind;

	while(((NclObjClass)data_part != nclObjClass)&&(*result == NULL)) { 
		switch(operation) {
		case NOT_OP:
		if(data_part->data_class.not[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.not[f_selection])((NclData)operand,NULL));
		}
		break;
		case NEG_OP:
		if(data_part->data_class.neg[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.neg[f_selection])((NclData)operand,NULL));
		}
		break;
		}
		data_part = (NclDataClass)data_part->obj_class.super_class;
	}

	if(*result != NULL) {
		return(NhlNOERROR);
	} else {
		*result = NULL;
		return(NhlFATAL); 
	}
}



int _NclIsMissing 
#if	NhlNeedProto
(NclMultiDValData self, void* val)
#else 
(self, val)
NclMultiDValData self;
void* val;
#endif
{
	NclDataClass dc;
	
	if(self == NULL) {
		return(0);
	}  else {
		dc = (NclDataClass)self->obj.class_ptr;
	}
	while((NclObjClass)dc != nclObjClass){
		if(dc->data_class.dup != NULL) {
			return((*dc->data_class.is_mis)((NclData)self,val));
		} else {
			dc = (NclDataClass)dc->obj_class.super_class;
		}
	}
	return(0);
}
struct _NclMultiDValDataRec* _NclCopyVal
#if	NhlNeedProto
(struct _NclMultiDValDataRec * self,NclScalar *new_missing)
#else
(self,new_missing)
struct _NclMultiDValDataRec * self;
NclScalar *new_missing;
#endif
{
	NclDataClass dc;
	
	if(self == NULL) {
		return(NULL);
	}  else {
		dc = (NclDataClass)self->obj.class_ptr;
	}
	while((NclObjClass)dc != nclObjClass){
		if(dc->data_class.dup != NULL) {
			return((NclMultiDValData)(*dc->data_class.dup)((NclData)self,new_missing));
		} else {
			dc = (NclDataClass)dc->obj_class.super_class;
		}
	}
	return(NULL);
}

void _NclResetMissingValue
#if	NhlNeedProto
(struct _NclMultiDValDataRec * self,NclScalar *missing)
#else
(self,missing)
	struct _NclMultiDValDataRec *self;
	NclScalar *missing;
#endif
{
	NclDataClass dc ;
	
	if((self == NULL)||(missing == NULL)) {
		return;
	} else {
		dc  = (NclDataClass)self->obj.class_ptr;
	}
	while((NclObjClass)dc != nclObjClass) {

		if(dc->data_class.reset_mis != NULL) {
			(*dc->data_class.reset_mis)((NclData)self,missing);
			return;
		} else {
			dc = (NclDataClass)dc->obj_class.super_class;
		}
	}
	return;
}


struct _NclMultiDValDataRec * _NclCreateVal
#if	NhlNeedProto
( NclObj inst, NclObjClass theclass, NclObjTypes obj_type, unsigned int obj_type_mask, void *val, NclScalar *missing_value, int n_dims, int *dim_sizes, NclStatus status, NclSelectionRecord *sel_rec)
#else 
( inst, theclass, obj_type, obj_type_mask, val, missing_value, n_dims, dim_sizes, status, sel_rec)
NclObj inst;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
int *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
#endif
{

	switch(obj_type) {
        case Ncl_MultiDValdoubleData:
	        return(_NclMultiDValdoubleCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
        case Ncl_MultiDValfloatData:
        	return(_NclMultiDValfloatCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
        case Ncl_MultiDVallongData:
	        return(_NclMultiDVallongCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
        case Ncl_MultiDValintData:
        	return(_NclMultiDValintCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
        case Ncl_MultiDValshortData:
        	return(_NclMultiDValshortCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
        case Ncl_MultiDValstringData:
       		return(_NclMultiDValstringCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
	case Ncl_MultiDValHLUObjData:
       		return(_NclMultiDValHLUObjDataCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
	case Ncl_MultiDVallogicalData:
       		return(_NclMultiDVallogicalCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
	case Ncl_MultiDValnclfileData:
       		return(_NclMultiDValnclfileCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
	case Ncl_MultiDValcharData:
       		return(_NclMultiDValcharCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
	case Ncl_MultiDValbyteData:
       		return(_NclMultiDValbyteCreate(
			inst,
			theclass,
			obj_type,
			obj_type_mask,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec));
        default:
                return(NULL);
        }
}


NhlErrorTypes _NclAddParent
#if	NhlNeedProto
(struct _NclObjRec * theobj, struct _NclObjRec* parent)
#else
( theobj,  parent)
struct _NclObjRec * theobj;
struct _NclObjRec* parent;
#endif
{
	NclObjClass class_ptr;


	if((theobj == NULL)||(parent == NULL)) 
		return(NhlFATAL);

	class_ptr = theobj->obj.class_ptr;

	while((class_ptr != NULL)&&(class_ptr->obj_class.add_parent == NULL)) {
		class_ptr = class_ptr->obj_class.super_class;
	}
	if(class_ptr == NULL) {
		return(NhlFATAL);
	} else {
		return((*class_ptr->obj_class.add_parent)(theobj,parent));
	}
}

NhlErrorTypes _NclDelParent
#if	NhlNeedProto
(struct _NclObjRec * theobj, struct _NclObjRec* parent)
#else
(theobj, parent)
struct _NclObjRec * theobj;
struct _NclObjRec* parent;
#endif
{
	NclObjClass class_ptr;

	if((theobj == NULL)||(parent == NULL)) 
		return(NhlFATAL);

	class_ptr = theobj->obj.class_ptr;

	while((class_ptr != NULL)&&(class_ptr->obj_class.del_parent == NULL)) {
		class_ptr = class_ptr->obj_class.super_class;
	}
	if(class_ptr == NULL) {
		return(NhlFATAL);
	} else {
		return((*class_ptr->obj_class.del_parent)(theobj,parent));
	}
}


extern int _NclGetObjRefCount
#if	NhlNeedProto
(int the_id)
#else
(the_id)
	int the_id;
#endif
{
	struct _NclObjRec * the_obj;

	the_obj = _NclGetObj(the_id);
	if(the_obj != NULL) {
		return(the_obj->obj.ref_count);
	} else {
		return(-1);
	}
}


extern char* _NclBasicDataTypeToName
#if	NhlNeedProto
(NclBasicDataTypes dt)
#else
(dt)
NclBasicDataTypes dt;
#endif
{
	static int first = 1;
	static NclQuark quarks[9];

	if(first) {
		first = 0;
		quarks[0] = NrmStringToQuark("double");
		quarks[1] = NrmStringToQuark("float");
		quarks[2] = NrmStringToQuark("long");
		quarks[3] = NrmStringToQuark("integer");
		quarks[4] = NrmStringToQuark("short");
		quarks[5] = NrmStringToQuark("string");
		quarks[6] = NrmStringToQuark("char");
		quarks[7] = NrmStringToQuark("byte");
		quarks[8] = NrmStringToQuark("logical");
	}	

	switch(dt) {
	case NCL_double:
		return(NrmQuarkToString(quarks[0]));
	case NCL_float:
		return(NrmQuarkToString(quarks[1]));
	case NCL_long:
		return(NrmQuarkToString(quarks[2]));
	case NCL_int:
		return(NrmQuarkToString(quarks[3]));
	case NCL_short:
		return(NrmQuarkToString(quarks[4]));
	case NCL_string:
		return(NrmQuarkToString(quarks[5]));
	case NCL_char:
		return(NrmQuarkToString(quarks[6]));
	case NCL_byte:
		return(NrmQuarkToString(quarks[7]));
	case NCL_logical:
		return(NrmQuarkToString(quarks[8]));
	}
	return(NULL);
}

extern NclBasicDataTypes _NclPromoteType
#if	NhlNeedProto
(NclBasicDataTypes dt)
#else
(dt)
NclBasicDataTypes dt;
#endif
{
	switch(dt) {
	case NCL_double:
		return(NCL_double);
	case NCL_float:
		return(NCL_double);
	case NCL_long:
		return(NCL_double);
	case NCL_int:
		return(NCL_long);
	case NCL_short:
		return(NCL_int);
	case NCL_string:
		return(NCL_string);
	case NCL_char:
		return(NCL_string);
	case NCL_byte:
		return(NCL_short);
	}
	return(NCL_none);
}

