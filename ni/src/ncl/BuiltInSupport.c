
/*
 *      $Id: BuiltInSupport.c,v 1.2 1995-02-17 01:00:39 ethan Exp $
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
 *	Date:		Tue Jan 31 15:18:21 MST 1995
 *
 *	Description:	
 */
/*

#include <ncarg/hlu/hluP.h>
#include "defs.h"
#include "NclDataDefs.h"
#include "Symbol.h"
#include "Machine.h"
#include "NclMultiDValData.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclType.h"
#include "TypeSupport.h"
*/


#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "NclVar.h"
#include "Machine.h"
#include "DataSupport.h"
#include "VarSupport.h"
#include "TypeSupport.h"







void *NclGetArgValue
#if NhlNeedProto
(int arg_num, int n_args,int* n_dims, int* dimsizes, NclScalar* missing, int * has_missing, NclBasicDataTypes *type)
#else
(arg_num, n_args,n_dims, dimsizes, missing, has_missing, type)
int arg_num;
int n_args;
int* n_dims;
int* dimsizes;
NclScalar* missing;
int * has_missing;
NclBasicDataTypes *type;
#endif
{
	NclStackEntry val;
	NclMultiDValData tmp_md = NULL;

	val = _NclGetArg(arg_num,n_args);
	
	switch(val.kind) {
	case NclStk_VAL:
		tmp_md = val.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
		break;
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGetArgValue: In correct argument type passed to function");
		return(NULL);
	}
	if(n_dims != NULL) {
		*n_dims = tmp_md->multidval.n_dims;
	}
	if(dimsizes != NULL) {
		memcpy((void*)dimsizes,(void*)tmp_md->multidval.dim_sizes,tmp_md->multidval.n_dims * sizeof(int));
	}
	if((missing != NULL)&&(has_missing != NULL)){
		if(tmp_md->multidval.missing_value.has_missing) {
			*has_missing = 1;
			*missing = tmp_md->multidval.missing_value.value;
		} else {	
			*has_missing = 0;
			*missing = tmp_md->multidval.missing_value.value;
		}
	} else if(has_missing != NULL) {
		*has_missing = tmp_md->multidval.missing_value.has_missing;
	} else if(missing != NULL) {
		*missing = tmp_md->multidval.missing_value.value;
	}
	if(type != NULL) {
		*type = tmp_md->multidval.data_type;
	}
	return(tmp_md->multidval.val);
}

extern NhlErrorTypes NclReturnValue
#if NhlNeedProto
(void *value, int n_dims, int* dimsizes, NclScalar* missing, NclBasicDataTypes type, int copy_data)
#else
(value, n_dims, dimsizes, missing, type, copy_data)
void *value;
int n_dims;
int* dimsizes;
NclScalar* missing;
NclBasicDataTypes type;
int copy_data;
#endif
{
	NclStackEntry data;
	void *tmp;
	NclObjTypes obj_type;
	NclTypeClass tc;
	int i,total=1;
	NclMultiDValData tmp_md;

	if((value != NULL)&&(n_dims > 0)&&(dimsizes != NULL)&&((int)type>0)) {
		obj_type = _NclBasicDataTypeToObjType(type);
		tc = _NclTypeEnumToTypeClass(obj_type);
		if(tc != NULL)  {
			if(copy_data) {
				for(i = 0; i < n_dims; i ++) {
					total *= dimsizes[i];
				}
				tmp = NclMalloc(total*tc->type_class.size);
				memcpy(tmp,value,total*tc->type_class.size);
			} else {
				tmp = value;
			}
			tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,tmp,missing,n_dims,dimsizes,TEMPORARY,NULL,(NclObjClass)tc);
			data.kind = NclStk_VAL;
			data.u.data_obj = tmp_md;
			_NclPlaceReturn(data);
			return(NhlNOERROR);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not determine type class. Incorrect BasicType requested by builtin function");
			return(NhlFATAL);
		}
	} else {

		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclReturnValue: Incomplete information to create return value from builtin function");
		return(NhlFATAL);
	}
}
