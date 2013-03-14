/*
 *      $Id$
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
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
 *	Date:		Fri Oct 29 11:36:10 MDT 1993
 *
 *	Description:	
 */

#include <stdio.h>
#ifdef NIO_LIB_ONLY
#include "niohluP.h"
#include "nioConvert.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/Convert.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include "Symbol.h"
#include <errno.h>
#include <math.h>
#include "NclTypeint.h"
#include "NclTypelogical.h"
#include "NclTypestring.h"
#include "NclMultiDValData.h"


static NhlErrorTypes CvtNhlTStringGenArrayToNclData
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
        NhlGenArray gen;
        char func[] = "CvtNhlTStringGenArrayToNclData";
        NclQuark *val;
        NclMultiDValData tmp_md;
        ng_size_t len_dimensions = 1;
	char **strar;
	int i;
 
 
        if(nargs != 0) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong number of args",func);
                to->size =0;
                return(NhlFATAL);
        }
        gen = (NhlGenArray)from->data.ptrval;
        if(gen != NULL) {
                if(!_NhlIsSubtypeQ(NrmStringToQuark(NhlTStringGenArray),from->typeQ)) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong input type",func);
                        to->size =0;
                        return(NhlFATAL);
                }
		val = NclMalloc(sizeof(NclQuark)* gen->num_elements);
	        strar = (char**)gen->data;
        	for(i = 0; i < gen->num_elements; i++) {
                	val[i] = NrmStringToQuark(strar[i]);
        	}

                tmp_md = _NclCreateMultiDVal(
                        NULL,NULL, Ncl_MultiDValData,
                        0,val,NULL,gen->num_dimensions,
                        gen->len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
                if(to->size < sizeof(NclMultiDValData)) {
                        return(NhlFATAL);
                } else {
                        *((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
                        return(NhlNOERROR);
                }
        } else {
                val = NclMalloc((unsigned)nclTypestringClassRec.type_class.size);
                *(NclQuark *)(val) = nclTypestringClassRec.type_class.default_mis.stringval;
                tmp_md = _NclCreateMultiDVal(
                        NULL,NULL, Ncl_MultiDValData,
                        0,val,&nclTypestringClassRec.type_class.default_mis,1,
                        &len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
                if(to->size < sizeof(NclMultiDValData)) {
                        return(NhlFATAL);
                } else {
                       *((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
                       return(NhlNOERROR);
                }
        }
}

/*ARGSUSED*/
static NhlErrorTypes CvtNhlTStringToNclData
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	NclQuark *tmp;
	NclMultiDValData tmp_md;
	int n_dims = 1;
	ng_size_t len_dims = 1;

	tmp = NclMalloc((unsigned)sizeof(NclQuark));
	*tmp = NrmStringToQuark((char*)(from->data.ptrval));
	tmp_md = _NclCreateMultiDVal(
		NULL,NULL, Ncl_MultiDValData,
		0,(void*)tmp,NULL,n_dims,
		&len_dims,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}

static NhlErrorTypes Ncl_Type_string_InitClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
        NhlRegisterConverter(NhlbaseClass,NhlTStringGenArray,NhlTNclData,
		CvtNhlTStringGenArrayToNclData,NULL,0,False,NULL);
        NhlRegisterConverter(NhlbaseClass,NhlTString,NhlTNclData,
		CvtNhlTStringToNclData,NULL,0,False,NULL);
	nclTypestringClassRec.type_class.default_mis.stringval = NrmStringToQuark("missing");
	return(NhlNOERROR);
}


/* 0 on match, non zero on non-match just like real strcmp */
/*
static int my_strcmp
#if	NhlNeedProto
(NclQuark s1,NclQuark s2)
#else 
(s1,s2)
NclQuark s1;
NclQuark s2;
#endif
{
	if((s1 < 1)&&(s2 < 1)) {
		return(0);
	} else if((s1 < 1)||(s2 < 1)) {
		return(1);
	} else {
		return(strcmp(NrmQuarkToString(s1),NrmQuarkToString(s2)));
	}
}
*/

static NclQuark combine_strings
#if	NhlNeedProto
(NclQuark ls,NclQuark rs,NclQuark mis_ls, NclQuark mis_rs,NclQuark mis)
#else
(ls,rs,mis_ls,mis_rs,mis)
NclQuark ls;
NclQuark rs;
NclQuark mis_ls;
NclQuark mis_rs;
NclQuark mis;
#endif
{
	char *tmp;
	char * ls_ptr;
	char * rs_ptr;
	NclQuark ret_val;

#if 0
	if(mis_ls == ls) {
		return(mis);
	} else if(mis_rs == rs) {
		return(mis);
	}
#endif
	if (mis_ls == ls && mis_rs == rs) {
		return (mis);
	}
	else {
		ls_ptr = NrmQuarkToString(ls);
		rs_ptr = NrmQuarkToString(rs);
		tmp = (char*)NclMalloc(strlen(ls_ptr)+strlen(rs_ptr)+1);
		strcpy(tmp,ls_ptr);
		strcat(tmp,rs_ptr);
		ret_val = NrmStringToQuark(tmp);
		NclFree(tmp);
		return(ret_val);
	}
}

static NclQuark select_string_lt
#if	NhlNeedProto
(NclQuark ls,NclQuark rs,NclQuark mis_ls, NclQuark mis_rs,NclQuark mis)
#else
(ls,rs,mis_ls,mis_rs,mis)
NclQuark ls;
NclQuark rs;
NclQuark mis_ls;
NclQuark mis_rs;
NclQuark mis;
#endif
{
	int len;
	int i;
	char *lptr;
	char *rptr;
	char *save_l;
	char *save_r;

	if(ls == mis_ls) {
		return(mis);
	} else if(rs == mis_rs) {
		return(mis);
	}


		
	save_l = lptr = NrmQuarkToString(ls);
	save_r = rptr = NrmQuarkToString(rs);
	len = MIN(strlen(lptr),strlen(rptr));	

	for(i = 0; i<len; i++) {
		if((int)(*lptr) < (int)(*rptr)) {
			return(ls);
		} else if((int)(*lptr) > (int)(*rptr)){
			return(rs);
		}
		lptr++;
		rptr++;
	}
	if(strlen(save_l) < strlen(save_r)) {
		return(ls);
	} else {
		return(rs);
	}
}
static NclQuark select_string_gt
#if	NhlNeedProto
(NclQuark ls,NclQuark rs,NclQuark mis_ls, NclQuark mis_rs,NclQuark mis)
#else
(ls,rs,mis_ls,mis_rs,mis)
NclQuark ls;
NclQuark rs;
NclQuark mis_ls;
NclQuark mis_rs;
NclQuark mis;
#endif
{
	int len;
	int i;
	char *lptr;
	char *rptr;
	char *save_l;
	char *save_r;
	
	if(ls == mis_ls) {
		return(mis);
	} else if(rs == mis_rs) {
		return(mis);
	}

	save_l = lptr = NrmQuarkToString(ls);
	save_r = rptr = NrmQuarkToString(rs);
	len = MIN(strlen(lptr),strlen(rptr));	

	for(i = 0; i<len; i++) {
		if((int)(*lptr) > (int)(*rptr)) {
			return(ls);
		} else if((int)(*lptr) < (int)(*rptr)){
			return(rs);
		}
		lptr++;
		rptr++;
	}
	if(strlen(save_l) > strlen(save_r)) {
		return(ls);
	} else {
		return(rs);
	}
}
static int cmp_string_lt
#if	NhlNeedProto
(NclQuark ls,NclQuark rs)
#else
(ls,rs)
NclQuark ls;
NclQuark rs;
#endif
{
	int len;
	int i;
	char *lptr;
	char *rptr;
	char *save_l;
	char *save_r;
	
	if(rs <1 ) {
		return(0);
	} else if(ls <1) {
		return(1);
	}
	
	save_l = lptr = NrmQuarkToString(ls);
	save_r = rptr = NrmQuarkToString(rs);
	len = MIN(strlen(lptr),strlen(rptr));	

	for(i = 0; i<len; i++) {
		if((int)(*lptr) < (int)(*rptr)) {
			return(1);
		} else if((int)(*lptr) > (int)(*rptr)) {
			return(0);
		}
		lptr++;	
		rptr++;
	}
	if(strlen(save_l) < strlen(save_r)) {
		return(1);
	} else {
		return(0);
	}
}

static int cmp_string_gt
#if	NhlNeedProto
(NclQuark ls,NclQuark rs)
#else
(ls,rs)
NclQuark ls;
NclQuark rs;
#endif
{
	int len;
	int i;
	char *lptr;
	char *rptr;
	char *save_l;
	char *save_r;

	if(ls <1) {
		return(0);
	} else if(rs <1) {
		return(1);
	}
	
	save_l = lptr = NrmQuarkToString(ls);
	save_r = rptr = NrmQuarkToString(rs);
	len = MIN(strlen(lptr),strlen(rptr));	

	for(i = 0; i<len; i++) {
		if((int)(*lptr) > (int)(*rptr)) {
			return(1);
		} else if((int)(*lptr) < (int)(*rptr)) {
			return(0);
		}
		lptr++;	
		rptr++;
	}
	if(strlen(save_l) > strlen(save_r)) {
		return(1);
	} else {
		return(0);
	}
}

static int cmp_string_le
#if	NhlNeedProto
(NclQuark ls,NclQuark rs)
#else
(ls,rs)
NclQuark ls;
NclQuark rs;
#endif
{
	int len;
	int i;
	char *lptr;
	char *rptr;
	char *save_l;
	char *save_r;

	if(ls <1) {
		return(1);
	} else if(rs <1) {
		return(0);
	}
	
	save_l = lptr = NrmQuarkToString(ls);
	save_r = rptr = NrmQuarkToString(rs);
	len = MIN(strlen(lptr),strlen(rptr));	

	for(i = 0; i<len; i++) {
		if((int)(*lptr) < (int)(*rptr)) {
			return(1);
		} else if((int)(*lptr) > (int)(*rptr)) {
			return(0);
		}
		lptr++;	
		rptr++;
	}
	if(strlen(save_l) <= strlen(save_r)) {
		return(1);
	} else {
		return(0);
	}
}
static int cmp_string_ge
#if	NhlNeedProto
(NclQuark ls,NclQuark rs)
#else
(ls,rs)
NclQuark ls;
NclQuark rs;
#endif
{
	int len;
	int i;
	char *lptr;
	char *rptr;
	char *save_l;
	char *save_r;

	if(rs <1) {
		return(1);
	} else if(ls <1) {
		return(0);
	}

	save_l = lptr = NrmQuarkToString(ls);
	save_r = rptr = NrmQuarkToString(rs);
	len = MIN(strlen(lptr),strlen(rptr));	

	for(i = 0; i<len; i++) {
		if((int)(*lptr) > (int)(*rptr)) {
			return(1);
		} else if((int)(*lptr) < (int)(*rptr)) {
			return(0);
		}
		lptr++;	
		rptr++;
	}
	if(strlen(save_l) >= strlen(save_r)) {
		return(1);
	} else {
		return(0);
	}
}

static NclMonoTypes Ncl_Type_string_is_mono
#if	NhlNeedProto
(void *val,NclScalar* val_m,ng_size_t nval)
#else
(val, val_m, nval)
void *val;
NclScalar* val_m;
ng_size_t nval;
#endif
{
	NclQuark *value = (NclQuark*)val;
	ng_size_t i = 0,j = 1;

	if(nval == 1) 
		return(1);
	if(val_m != NULL) {
		i = 0;
		j = 0;
		while((i<nval)&&(value[i] == val_m->stringval))i++;
		if(i >= nval-1) return(NclNONMONO);
		j = i + 1;
		while((j<nval)&&(value[j] == val_m->stringval)) j++;
		if(j == nval) return(NclNONMONO);
/*
* i is first non-missing value and j is second guarenteed
*/
		if(cmp_string_gt(value[i],value[j])) {
			while(cmp_string_gt(value[i],value[j])) {
				i = j;
				j++;
				while((j<nval)&&(value[j] == val_m->stringval)) {
					j++;
				}
				if(j >= nval)
					break;
			}
			if(j >= nval) {
				return(NclDECREASING);
			} else {
				return(NclNONMONO);
			}
		} else if(cmp_string_lt(value[i], value[j])) {
			while(cmp_string_lt(value[i],value[j])) {
				i = j;
				j++;
				while((j<nval)&&(value[j] == val_m->stringval)) {
					j++;
				}
				if(j >= nval)
					break;
			}
			if(j >= nval) {
				return(NclINCREASING);
			} else {
				return(NclNONMONO);
			}
		}
	} else {
		i = 0;
		if(cmp_string_gt(value[0], value[1])) {
			while((i<nval-1)&&(cmp_string_gt(value[i],value[i+1]))) i++;
			if(i == nval-1) {
				return(NclDECREASING);
			} else {
				return(NclNONMONO);
			}
		} else if(cmp_string_lt(value[0],value[1])) {
			while((i<nval-1)&&(cmp_string_lt(value[i],value[i+1]))) i++;
			if(i == nval-1) {
				return(NclINCREASING);
			} else {
				return(NclNONMONO);
			}
		} 
	}
	return(NclNONMONO);
}

static NhlErrorTypes Ncl_Type_string_cmpf
#if NhlNeedProto
(void *lhs, void* rhs, NclScalar* lhs_m, NclScalar *rhs_m,int digits, double* result)
#else
(lhs, rhs, lhs_m, rhs_m, digits, result)
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar *rhs_m;
int digits;
double * result;
#endif
{
        if((lhs_m != NULL)&&(lhs_m->intval == *(int*)lhs)) {
                return(NhlFATAL);
        } else if((rhs_m != NULL)&&(rhs_m->intval == *(int*)rhs)) {
                return(NhlFATAL);
        } else {
		if(cmp_string_gt(*(NclQuark*)lhs,*(NclQuark*)rhs)) {
			*result = 1;
		} else if(cmp_string_lt(*(NclQuark*)lhs,*(NclQuark*)rhs)) {
			*result = -1;
		} else {
			*result = 0;
		}
                return(NhlNOERROR);
        }
}



INSERTTMPSTRING


NhlErrorTypes Ncl_Type_string_plus
#if	NhlNeedProto
(void *result,void *lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m, ng_size_t nlhs, ng_size_t nrhs)
#else
(result,lhs,rhs,lhs_m,rhs_m,nlhs,nrhs)
void *result;
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar* rhs_m;
ng_size_t nlhs;
ng_size_t nrhs;
#endif
{
        NclQuark *ls,*rs;
	NclQuark *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (NclQuark*)lhs;
	rs = (NclQuark*)rhs;
	res = (NclQuark*)result;

	if(nlhs > nrhs) 
		stopi = nlhs;
	else
		stopi = nrhs;
	if(nlhs > 1) {
		linc = 1;
	}
	if(nrhs > 1) {
		rinc = 1;
	}
	

	if((lhs_m == NULL)&&(rhs_m == NULL)) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = combine_strings(*ls,*rs,-1,-1,-1);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = combine_strings(*ls,*rs, lhs_m->stringval, -1,lhs_m->stringval);
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = combine_strings(*ls,*rs,-1, rhs_m->stringval, rhs_m->stringval);
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = combine_strings(*ls,*rs, lhs_m->stringval, rhs_m->stringval, lhs_m->stringval);
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_string_plus_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypestringClass);
}

NhlErrorTypes Ncl_Type_string_eq
#if	NhlNeedProto
(void *result,void *lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m, ng_size_t nlhs, ng_size_t nrhs)
#else
(result,lhs,rhs,lhs_m,rhs_m,nlhs,nrhs)
void *result;
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar* rhs_m;
ng_size_t nlhs;
ng_size_t nrhs;
#endif
{
        NclQuark *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (NclQuark*)lhs;
	rs = (NclQuark*)rhs;
	res = (logical*)result;

	if(nlhs > nrhs) 
		stopi = nlhs;
	else
		stopi = nrhs;
	if(nlhs > 1) {
		linc = 1;
	}
	if(nrhs > 1) {
		rinc = 1;
	}
	

	if((lhs_m == NULL)&&(rhs_m == NULL)) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = *ls == *rs;
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((lhs_m->stringval == *ls) ? ((logical)((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval) : *ls == *rs);
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((rhs_m->stringval == *rs) ? ( (logical)((logical)((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval)) : *ls == *rs);
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(((lhs_m->stringval == *ls)||( rhs_m->stringval==*rs)) ? ((logical)((logical)((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval)) : *ls == *rs);
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_string_eq_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}
NhlErrorTypes Ncl_Type_string_ne
#if	NhlNeedProto
(void *result,void *lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m, ng_size_t nlhs, ng_size_t nrhs)
#else
(result,lhs,rhs,lhs_m,rhs_m,nlhs,nrhs)
void *result;
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar* rhs_m;
ng_size_t nlhs;
ng_size_t nrhs;
#endif
{
        NclQuark *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (NclQuark*)lhs;
	rs = (NclQuark*)rhs;
	res = (logical*)result;

	if(nlhs > nrhs) 
		stopi = nlhs;
	else
		stopi = nrhs;
	if(nlhs > 1) {
		linc = 1;
	}
	if(nrhs > 1) {
		rinc = 1;
	}


	if((lhs_m == NULL)&&(rhs_m == NULL)) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = !((*ls==*rs));
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->stringval == *ls) ? ( (logical)((logical)((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval)) : !((*ls==*rs)));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((rhs_m->stringval == *rs)?((logical)((logical)((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval)):!(*ls==*rs));
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->stringval == *ls) ||( rhs_m->stringval==*rs)) ? ( (logical)((logical)((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval)) : !((*ls==*rs)));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_string_ne_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

NclTypestringClassRec nclTypestringClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclTypeClassRec,
		0,
		NULL,
		NULL,
		NULL,
		Ncl_Type_string_InitClass,
		NULL,
		NULL,
		NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL
	},
	{
/* NclObjTypes type 			*/ Ncl_Typestring,
/* NclBasicDataTypes data_type		*/ NCL_string,
/* int size 				*/ sizeof(NclQuark),
/* char *hlu_rep_type			*/ {NhlTQuark,NhlTQuarkGenArray},
/* NclScalar 				*/ {(NclQuark)-1},
/* char *format ; 			*/ "%s",
/* NclTypePrint print ; 		*/ Ncl_Type_string_print,
/* NclTypeResetMissing reset_mis; 	*/ Ncl_Type_string_reset_mis,
/* NclTypeCoerceFunction coerce; 	*/ Ncl_Type_string_coerce,
/* NclTypeOp multiply; 			*/ NULL,
/* NclTypeOutSize multiply_type;        */ NULL,
/* NclTypeOp plus; 			*/ Ncl_Type_string_plus,
/* NclTypeOutSize plus_type;            */ Ncl_Type_string_plus_type,
/* NclTypeOp minus; 			*/ NULL,
/* NclTypeOutSize minus_type;           */ NULL,
/* NclTypeOp divide; 			*/ NULL,
/* NclTypeOutSize divide_type;          */ NULL,
/* NclTypeOp exponent; 			*/ NULL,
/* NclTypeOutSize exponent_type;        */ NULL,
/* NclTypeOp mod; 			*/ NULL,
/* NclTypeOutSize mod_type;             */ NULL,
/* NclTypeOp mat; 			*/ NULL,
/* NclTypeOutSize mat_type;             */ NULL,
/* NclTypeOp sel_lt; 			*/ Ncl_Type_string_sel_lt,
/* NclTypeOutSize sel_lt_type;          */ Ncl_Type_string_sel_lt_type,
/* NclTypeOp sel_gt; 			*/ Ncl_Type_string_sel_gt,
/* NclTypeOutSize sel_gt_type;          */ Ncl_Type_string_sel_gt_type,
/* NclTypeOp not; 			*/ NULL,
/* NclTypeOutSize not_type;             */ NULL,
/* NclTypeOp neg; 			*/ NULL,
/* NclTypeOutSize neg_type;             */ NULL,
/* NclTypeOp gt; 			*/ Ncl_Type_string_gt,
/* NclTypeOutSize gt_type;              */ Ncl_Type_string_gt_type,
/* NclTypeOp lt; 			*/ Ncl_Type_string_lt,
/* NclTypeOutSize lt_type;              */ Ncl_Type_string_lt_type,
/* NclTypeOp ge; 			*/ Ncl_Type_string_ge,
/* NclTypeOutSize ge_type;              */ Ncl_Type_string_ge_type,
/* NclTypeOp le; 			*/ Ncl_Type_string_le,
/* NclTypeOutSize le_type;              */ Ncl_Type_string_le_type,
/* NclTypeOp ne; 			*/ Ncl_Type_string_ne,
/* NclTypeOutSize ne_type;              */ Ncl_Type_string_ne_type,
/* NclTypeOp eq; 			*/ Ncl_Type_string_eq,
/* NclTypeOutSize eq_type;              */ Ncl_Type_string_eq_type,
/* NclTypeOp and; 			*/ NULL,
/* NclTypeOutSize and_type;             */ NULL,
/* NclTypeOp or; 			*/ NULL,
/* NclTypeOutSize or_type;              */ NULL,
/* NclTypeOp xor; 			*/ NULL,
/* NclTypeOp xor;                       */ NULL,
/* NclNumScalarCompareFunc cmpf; 	*/ Ncl_Type_string_cmpf,
/* NclMonotonicTestFunction is_mono; 	*/ Ncl_Type_string_is_mono
	},
	{
		NULL
	}
};

NclObjClass nclTypestringClass = (NclObjClass)&nclTypestringClassRec;

NclType _NclTypestringCreate
#if	NhlNeedProto
(NclObj inst , NclObjClass theclass , NclObjTypes obj_type , unsigned int obj_type_mask, NclStatus status)
#else
(inst , theclass , obj_type ,obj_type_mask, status)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
#endif
{
	return((NclType)_NclTypeCreate(inst,theclass,obj_type,(obj_type_mask | Ncl_Typestring), status));
}
