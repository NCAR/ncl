
/*
 *      $Id: string_ops.c.sed,v 1.1 1994-07-14 20:47:54 ethan Exp $
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
#include <ncarg/hlu/hluP.h>
#include "defs.h"
#include "Symbol.h"
#include <errno.h>
#include <math.h>
#include "NclMultiDValintData.h"
#include "NclMultiDValstringData.h"
#include "DataSupport.h"




static void NclMultiDValstringPrint(
#ifdef NhlNeedProto
	NclObj  /* self */,
	FILE *	/*fp*/
#endif
);
static void MultiDValDestroy(
#ifdef NhlNeedProto
NclObj	self
#endif
);
/* 0 on match, non zero on non-match just like real strcmp */
/*
static int my_strcmp
#if  __STDC__
(string s1,string s2)
#else 
(s1,s2)
string s1;
string s2;
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

static string combine_strings
#if  __STDC__
(string ls,string rs,string mis_ls, string mis_rs,string mis)
#else
(ls,rs,mis_ls,mis_rs,mis)
string ls;
string rs;
string mis_ls;
string mis_rs;
string mis;
#endif
{
	char *tmp;
	char * ls_ptr;
	char * rs_ptr;
	string ret_val;

	if(ls == -1) {
		return(rs);
	} else if(rs == -1){
		return(ls);
	}
	if((mis_ls > 0)&&(mis_ls == ls)) {
		return(mis);
	} else if((mis_rs > 0)&&(mis_rs == rs)) {
		return(mis);
	} else {
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

static string select_string_lt
#if  __STDC__
(string ls,string rs,string mis_ls, string mis_rs,string mis)
#else
(ls,rs,mis_ls,mis_rs,mis)
string ls;
string rs;
string mis_ls;
string mis_rs;
string mis;
#endif
{
	int len;
	int i;
	char *lptr;
	char *rptr;
	char *save_l;
	char *save_r;
	
	if(ls < 1) {
		return(-1);
	} else if(rs < 1) {
		return(ls);
	}

		
	save_l = lptr = NrmQuarkToString(ls);
	save_r = rptr = NrmQuarkToString(rs);
	len = MIN(strlen(lptr),strlen(rptr));	

	if((mis_ls > 0)&&(mis_ls == ls)) {
		return(mis);
	} else if((mis_rs > 0)&&(mis_rs == rs)) {
		return(mis);
	} else {
		for(i = 0; i<len; i++) {
			if((int)tolower(*lptr) < (int)tolower(*rptr)) {
				return(ls);
			} else if((int)tolower(*lptr) > (int)tolower(*rptr)){
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
}
static string select_string_gt
#if  __STDC__
(string ls,string rs,string mis_ls, string mis_rs,string mis)
#else
(ls,rs,mis_ls,mis_rs,mis)
string ls;
string rs;
string mis_ls;
string mis_rs;
string mis;
#endif
{
	int len;
	int i;
	char *lptr;
	char *rptr;
	char *save_l;
	char *save_r;
	
	if(ls < 1) {
		return(rs);
	} else if(rs < 1) {
		return(ls);
	}

	save_l = lptr = NrmQuarkToString(ls);
	save_r = rptr = NrmQuarkToString(rs);
	len = MIN(strlen(lptr),strlen(rptr));	

	if((mis_ls > 0)&&(mis_ls==ls)) {
		return(mis);
	} else if((mis_rs > 0)&&(mis_rs == rs)) {
		return(mis);
	} else {
		for(i = 0; i<len; i++) {
			if((int)tolower(*lptr) > (int)tolower(*rptr)) {
				return(ls);
			} else if((int)tolower(*lptr) < (int)tolower(*rptr)){
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
}
static int cmp_string_lt
#if  __STDC__
(string ls,string rs)
#else
(ls,rs)
string ls;
string rs;
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
		if((int)tolower(*lptr) < (int)tolower(*rptr)) {
			return(1);
		} else if((int)tolower(*lptr) > (int)tolower(*rptr)) {
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
#if  __STDC__
(string ls,string rs)
#else
(ls,rs)
string ls;
string rs;
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
		if((int)tolower(*lptr) > (int)tolower(*rptr)) {
			return(1);
		} else if((int)tolower(*lptr) < (int)tolower(*rptr)) {
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
#if  __STDC__
(string ls,string rs)
#else
(ls,rs)
string ls;
string rs;
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
		if((int)tolower(*lptr) < (int)tolower(*rptr)) {
			return(1);
		} else if((int)tolower(*lptr) > (int)tolower(*rptr)) {
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
#if  __STDC__
(string ls,string rs)
#else
(ls,rs)
string ls;
string rs;
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
		if((int)tolower(*lptr) > (int)tolower(*rptr)) {
			return(1);
		} else if((int)tolower(*lptr) < (int)tolower(*rptr)) {
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

INSERTTMPSTRING

static NclData NclMultiDValstringDup
#if  __STDC__
(NclData self,NclScalar *new_missing)
#else
(self,new_missing)
NclData self;
NclScalar *new_missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData) self;
	string *toval;
	string *frval;
	string missing;
	NclScalar themissing;
	int i;

	toval = (string*)NclMalloc(self_md->multidval.totalsize);
	frval = (string*)self_md->multidval.val;
	if(toval == NULL) {
		return(NULL);
	}
	if((new_missing == NULL)||(!self_md->multidval.missing_value.has_missing)) {
		memcpy((char*)toval,(char*)frval,self_md->multidval.totalsize);
		themissing = self_md->multidval.missing_value.value;
	} else {
		missing = self_md->multidval.missing_value.value.stringval;
		for(i = 0; i < self_md->multidval.totalelements; i++) {
			toval[i] = (frval[i] == missing ?
					new_missing->stringval :
					frval[i]);
		}
		themissing = *new_missing;
	}
	return((NclData)_NclMultiDValstringCreate(
		NULL,
		NULL,
		Ncl_MultiDValstringData,
		0,
		(void*)toval,
		(self_md->multidval.missing_value.has_missing ? &themissing : NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,
		NULL));
}
static NclData MultiDVal_string_mdmd_Plus
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
        NclMultiDValData self_md = (NclMultiDValData)self;
        NclMultiDValData other_md = (NclMultiDValData)other;
        NclMultiDValData result_md = (NclMultiDValData)result;
        NclData output_md;
	NclMissingRec themissing;
        void *result_val;
        int i,totalelements= 1;

        string *ls,*rs;
	string *res;

        ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));
        ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));
	if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		return(NULL);
	}

	for(i = 0; i< self_md->multidval.n_dims; i++) {
		if(self_md->multidval.dim_sizes[i] 
			!= other_md->multidval.dim_sizes[i]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
			return(NULL);
		} 
	}
	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements * sizeof(string));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Could not allocate memory for result type, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if((self_md->multidval.missing_value.has_missing)&&(self_md->multidval.missing_value.value.stringval != -1)) {

               	themissing.value.stringval = self_md->multidval.missing_value.value.stringval;
               	themissing.has_missing = 1;

        } else if((other_md->multidval.missing_value.has_missing)&&(other_md->multidval.missing_value.value.stringval != -1)) {

                themissing.value.stringval = other_md->multidval.missing_value.value.stringval;
                themissing.has_missing = 1;

        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValstringCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValstringData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing)) {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = combine_strings(ls[i],rs[i],-1,-1,-1);
		}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = combine_strings(ls[i],rs[i],
			self_md->multidval.missing_value.value.stringval,
			-1,
			self_md->multidval.missing_value.value.stringval);
		}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements ; i++) {
			res[i] = combine_strings(ls[i],rs[i],-1,
			other_md->multidval.missing_value.value.stringval,
			other_md->multidval.missing_value.value.stringval);
		}
	} else {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = combine_strings(ls[i],rs[i],
			self_md->multidval.missing_value.value.stringval, 
			other_md->multidval.missing_value.value.stringval,
			self_md->multidval.missing_value.value.stringval);
		}
	}
	return((NclData)output_md);
}
static NclData MultiDVal_string_mds_Plus
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	void *result_val;
	NclMissingRec themissing;
	int i,totalelements = 1;
	string *ls,*rs;
	string *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));



	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements*sizeof(string));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if((self_md->multidval.missing_value.has_missing)&&
		(self_md->multidval.missing_value.value.stringval != -1)) {

                themissing.value.stringval =  self_md->multidval.missing_value.value.stringval;
                themissing.has_missing = 1;

        } else if((other_md->multidval.missing_value.has_missing)&&
		(other_md->multidval.missing_value.value.stringval != -1)) {

		themissing.value.stringval = other_md->multidval.missing_value.value.stringval;
                themissing.has_missing = 1;

        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValstringCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValstringData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing ) &&
		!(other_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = combine_strings(ls[i],*rs,
					-1,-1,-1);
			}
	} else if(!(other_md->multidval.missing_value.has_missing )) {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = combine_strings(ls[i],*rs,
			self_md->multidval.missing_value.value.stringval,-1,
			self_md->multidval.missing_value.value.stringval);
		}
	} else if(!(self_md->multidval.missing_value.has_missing )) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)other_md->multidval.val==other_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = other_md->multidval.missing_value.value.stringval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = combine_strings(ls[i],*rs,
						-1,-1,-1);
				}
			}
	} else {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)other_md->multidval.val == other_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = self_md->multidval.missing_value.value.stringval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = combine_strings(ls[i],*rs,
					self_md->multidval.missing_value.value.stringval,
					-1, 
					self_md->multidval.missing_value.value.stringval);
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_smd_Plus
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	NclMissingRec themissing;
	void *result_val;
	int i,totalelements= 1;

	string *ls,*rs;
	string *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));

	totalelements  = other_md->multidval.totalelements;
	result_val = (void*)NclMalloc(other_md->multidval.totalelements * sizeof(string));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if((self_md->multidval.missing_value.has_missing)&&
		(self_md->multidval.missing_value.value.stringval > 0)) {
		themissing.value.stringval = self_md->multidval.missing_value.value.stringval;
                themissing.has_missing = 1;
        } else if((other_md->multidval.missing_value.has_missing)&&
		(other_md->multidval.missing_value.value.stringval > 0)) {
		themissing.value.stringval = other_md->multidval.missing_value.value.stringval;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValstringCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValstringData,0,result_val,
		(themissing.has_missing ? &(themissing.value):NULL),
		other_md->multidval.n_dims,
		other_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing )) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = combine_strings(*ls,rs[i],
					-1,-1,-1);
			}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = combine_strings(*ls,rs[i],-1,
				other_md->multidval.missing_value.value.stringval,
				other_md->multidval.missing_value.value.stringval);
			}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
				rs = (string*)other_md->multidval.val;
				if(*(string*)self_md->multidval.val == self_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = self_md->multidval.missing_value.value.stringval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] =  combine_strings(*ls,rs[i],
							-1,-1,-1);
				}
			}
	} else {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val == self_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = self_md->multidval.missing_value.value.stringval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = combine_strings(*ls,rs[i],
						-1,
						other_md->multidval.missing_value.value.stringval,
						self_md->multidval.missing_value.value.stringval);
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_ss_Plus
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	NclMissingRec themissing;
	void *result_val;

	string *ls,*rs;
	string *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));


	result_val = (void*)NclMalloc(sizeof(string));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
		themissing.value.stringval = self_md->multidval.missing_value.value.stringval;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
		themissing.value.stringval = other_md->multidval.missing_value.value.stringval;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValstringCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValstringData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		other_md->multidval.n_dims,
		other_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing )) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			*res = combine_strings(*ls,*rs,-1,-1,-1);
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			*res = combine_strings(*ls,*rs,-1,
				other_md->multidval.missing_value.value.stringval,
				other_md->multidval.missing_value.value.stringval);
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				*res = self_md->multidval.missing_value.value.stringval;
			} else {
				*res =  combine_strings(*ls,*rs,-1,-1,-1);
			}
	} else {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val == self_md->multidval.missing_value.value.stringval) {
				*res = (string)self_md->multidval.missing_value.value.stringval;
			} else {
				*res = combine_strings(*ls,*rs,-1,other_md->multidval.missing_value.value.stringval,self_md->multidval.missing_value.value.stringval);
			}
	}
	return((NclData)output_md);
} 

static NclData MultiDVal_string_mdmd_Eq
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
        NclMultiDValData self_md = (NclMultiDValData)self;
        NclMultiDValData other_md = (NclMultiDValData)other;
        NclMultiDValData result_md = (NclMultiDValData)result;
        NclData output_md;
	NclMissingRec themissing;
        void *result_val;
        int i,totalelements= 1;

        string *ls,*rs;
	int *res;

        ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));
        ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));
	if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		return(NULL);
	}

	for(i = 0; i< self_md->multidval.n_dims; i++) {
		if(self_md->multidval.dim_sizes[i] 
			!= other_md->multidval.dim_sizes[i]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
			return(NULL);
		} 
	}
	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements * sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result type, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValintCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing)) {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = ls[i] == rs[i];
		}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (int)((
			self_md->multidval.missing_value.value.stringval == ls[i]) ? (
			(int)NCL_DEFAULT_MISSING_VALUE)
			: ls[i] == rs[i]);
		}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements ; i++) {
			res[i] = (int)((other_md->multidval.missing_value.value.stringval == rs[i]) 
			? (
			(int)NCL_DEFAULT_MISSING_VALUE )
			: ls[i] == rs[i]);
		}
	} else {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (int)(((
				self_md->multidval.missing_value.value.stringval == ls[i])
				||( other_md->multidval.missing_value.value.stringval==rs[i])) ? (
			(int)NCL_DEFAULT_MISSING_VALUE)
			: ls[i] == rs[i]);
		}
	}
	return((NclData)output_md);
}
static NclData MultiDVal_string_mds_Eq
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	void *result_val;
	NclMissingRec themissing;
	int i,totalelements = 1;
	string *ls,*rs;
	int *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));



	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements*sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValintCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing ) &&
		!(other_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)(ls[i] == *rs);
			}
	} else if(!(other_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)((
				self_md->multidval.missing_value.value.stringval== ls[i]) ? (
				(int)NCL_DEFAULT_MISSING_VALUE)
				: (ls[i]==*rs));
			}
	} else if(!(self_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)other_md->multidval.val == other_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)(ls[i]==*rs);
				}
			}
	} else {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)other_md->multidval.val == other_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)((
						self_md->multidval.missing_value.value.stringval == ls[i]) 
						? (int)NCL_DEFAULT_MISSING_VALUE 
						: (ls[i]==*rs));
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_smd_Eq
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	NclMissingRec themissing;
	void *result_val;
	int i,totalelements= 1;

	string *ls,*rs;
	int *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));

	totalelements  = other_md->multidval.totalelements;
	result_val = (void*)NclMalloc(other_md->multidval.totalelements * sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValintCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,result_val,
		(themissing.has_missing ? &(themissing.value):NULL),
		other_md->multidval.n_dims,
		other_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)(*ls==rs[i]);
			}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)((
				other_md->multidval.missing_value.value.stringval == rs[i]) ? (
				(int)NCL_DEFAULT_MISSING_VALUE)
				: (*ls==rs[i]));
			}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
				rs = (string*)other_md->multidval.val;
				if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] =  (int)(*ls==rs[i]);
				}
			}
	} else {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)(((
						other_md->multidval.missing_value.value.stringval==rs[i])) 
						? ((int)NCL_DEFAULT_MISSING_VALUE)  
						: (*ls==rs[i]));
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_ss_Eq
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	NclMissingRec themissing;
	void *result_val;

	string *ls,*rs;
	int *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));


	result_val = (void*)NclMalloc(sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValintCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		other_md->multidval.n_dims,
		other_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			*res = (int)(*ls==*rs);
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			*res = (int)((
			other_md->multidval.missing_value.value.stringval == *rs) ? (
			NCL_DEFAULT_MISSING_VALUE)
			: (*ls==*rs));
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				*res = (int)NCL_DEFAULT_MISSING_VALUE;
			} else {
				*res =  (int)(*ls==*rs);
			}
	} else {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				*res = (int)NCL_DEFAULT_MISSING_VALUE;
			} else {
				*res = (int)(((
					other_md->multidval.missing_value.value.stringval==*rs)) 
					? ((int)NCL_DEFAULT_MISSING_VALUE ) 
					: (*ls==*rs));
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_mdmd_Ne
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
        NclMultiDValData self_md = (NclMultiDValData)self;
        NclMultiDValData other_md = (NclMultiDValData)other;
        NclMultiDValData result_md = (NclMultiDValData)result;
        NclData output_md;
	NclMissingRec themissing;
        void *result_val;
        int i,totalelements= 1;

        string *ls,*rs;
	int *res;

        ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));
        ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));
	if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		return(NULL);
	}

	for(i = 0; i< self_md->multidval.n_dims; i++) {
		if(self_md->multidval.dim_sizes[i] 
			!= other_md->multidval.dim_sizes[i]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
			return(NULL);
		} 
	}
	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements * sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result type, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValintCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing)) {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = !((ls[i]==rs[i]));
		}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (int)((
			self_md->multidval.missing_value.value.stringval == ls[i]) ? (
			(int)NCL_DEFAULT_MISSING_VALUE)
			: !((ls[i]==rs[i])));
		}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements ; i++) {
			res[i] = (int)((
			other_md->multidval.missing_value.value.stringval == rs[i]) ? (
			(int)NCL_DEFAULT_MISSING_VALUE )
			: !(ls[i]==rs[i]));
		}
	} else {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (int)(((
				self_md->multidval.missing_value.value.stringval == ls[i])
				||( other_md->multidval.missing_value.value.stringval==rs[i])) ? (
			(int)NCL_DEFAULT_MISSING_VALUE)
			: !((ls[i]==rs[i])));
		}
	}
	return((NclData)output_md);
}
static NclData MultiDVal_string_mds_Ne
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	void *result_val;
	NclMissingRec themissing;
	int i,totalelements = 1;
	string *ls,*rs;
	int *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));



	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements*sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValintCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing ) &&
		!(other_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)!((ls[i]==*rs));
			}
	} else if(!(other_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)((
				self_md->multidval.missing_value.value.stringval== ls[i]) ? (
				(int)NCL_DEFAULT_MISSING_VALUE)
				: !((ls[i]==*rs)));
			}
	} else if(!(self_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)other_md->multidval.val==other_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)!((ls[i]==*rs));
				}
			}
	} else {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)other_md->multidval.val==other_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)(((
						self_md->multidval.missing_value.value.stringval==ls[i])) 
						? (int)NCL_DEFAULT_MISSING_VALUE 
						: !((ls[i]==*rs)));
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_smd_Ne
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	NclMissingRec themissing;
	void *result_val;
	int i,totalelements= 1;

	string *ls,*rs;
	int *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));

	totalelements  = other_md->multidval.totalelements;
	result_val = (void*)NclMalloc(other_md->multidval.totalelements * sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValintCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,result_val,
		(themissing.has_missing ? &(themissing.value):NULL),
		other_md->multidval.n_dims,
		other_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)!((*ls==rs[i]));
			}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)((
				other_md->multidval.missing_value.value.stringval== rs[i]) ? (
				(int)NCL_DEFAULT_MISSING_VALUE)
				: !((*ls==rs[i])));
			}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
				rs = (string*)other_md->multidval.val;
				if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] =  (int)!((*ls==rs[i]));
				}
			}
	} else {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)(((
						other_md->multidval.missing_value.value.stringval==rs[i])) 
						? ((int)NCL_DEFAULT_MISSING_VALUE)  
						: !((*ls==rs[i])));
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_ss_Ne
#if __STDC__
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	NclMissingRec themissing;
	void *result_val;

	string *ls,*rs;
	int *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));


	result_val = (void*)NclMalloc(sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.intval = (int)NCL_DEFAULT_MISSING_VALUE;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValintCreate(
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		other_md->multidval.n_dims,
		other_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			*res = (int)!((*ls==*rs));
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			*res = (int)((
			other_md->multidval.missing_value.value.stringval== *rs) ? (
			NCL_DEFAULT_MISSING_VALUE)
			: !((*ls==*rs)));
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				*res = (int)NCL_DEFAULT_MISSING_VALUE;
			} else {
				*res =  (int)!((*ls==*rs));
			}
	} else {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				*res = (int)NCL_DEFAULT_MISSING_VALUE;
			} else {
				*res = (int)(((
					other_md->multidval.missing_value.value.stringval==*rs)) 
					? ((int)NCL_DEFAULT_MISSING_VALUE ) 
					:!((*ls==*rs)));
			}
	}
	return((NclData)output_md);
} 
NclMultiDValstringDataClassRec nclMultiDValstringDataClassRec = {
	{
/* char *class_name; 		*/	"MultiDValstringData",
/* unsigned int obj_size;	*/	sizeof(NclMultiDValstringDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclMultiDValDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	MultiDValDestroy, 
/* NclSetStatusFunciton set_status; 	*/	NULL, 
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	NULL,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
/* NclPrintFunction print; 	*/	NclMultiDValstringPrint,
	},
	{
/* NclCopyFunction dup */	 		NclMultiDValstringDup,
/* NclResetMissingValueFunction reset_mis*/	MultiDVal_string_ResetMissing,
/* NclReadSubSecFunction r_subsection */ 	MultiDVal_string_ReadSection,
/* NclWriteSubSecFunction w_subsection */ 	{
						MultiDVal_string_md_WriteSection,
						MultiDVal_string_s_WriteSection
},
/* NclReadSubSecFunction r_then_w_subsection */ MultiDVal_string_ReadWriteSection,
/* NclDataFunction coerce; 	*/	{ 
						MultiDVal_string_md_Coerce,
						MultiDVal_string_s_Coerce
					},
/* NclDataFunction multiply; 	*/	{
						NULL,
						NULL,
						NULL,
						NULL
					},
/* NclDataFunction plus; 	*/	{
						MultiDVal_string_mdmd_Plus,
						MultiDVal_string_mds_Plus,
						MultiDVal_string_smd_Plus,
						MultiDVal_string_ss_Plus
					},
/* NclDataFunction minus; 	*/	{
						NULL,
						NULL,
						NULL,
						NULL	
					},
/* NclDataFunction divide; 	*/	{
						NULL,
						NULL,
						NULL,
						NULL
					},
/* NclDataFunction exponent; 	*/	{
						NULL,
						NULL,
						NULL,
						NULL
					},
/* NclDataFunction mod; 	*/	{
						NULL,
						NULL,
						NULL,
						NULL
					},
/* NclDataFunction mat; 	*/	{
						NULL,
						NULL,
						NULL,
						NULL
					},
/* NclDataFunction sel_lt; 	*/	{
						MultiDVal_string_mdmd_SelLt,
						MultiDVal_string_mds_SelLt,
						MultiDVal_string_smd_SelLt,
						MultiDVal_string_ss_SelLt
					},
/* NclDataFunction sel_gt; 	*/	{
						MultiDVal_string_mdmd_SelGt,
						MultiDVal_string_mds_SelGt,
						MultiDVal_string_smd_SelGt,
						MultiDVal_string_ss_SelGt
					},
/* NclDataFunction not; 	*/	{
						NULL,
						NULL	
						
					},
/* NclDataFunction neg; 	*/	{
						NULL,
						NULL
					},
/* NclDataFunction gt; 		*/	{
						MultiDVal_string_mdmd_Gt,
						MultiDVal_string_mds_Gt,
						MultiDVal_string_smd_Gt,
						MultiDVal_string_ss_Gt
					},
/* NclDataFunction lt; 		*/	{
						MultiDVal_string_mdmd_Lt,
						MultiDVal_string_mds_Lt,
						MultiDVal_string_smd_Lt,
						MultiDVal_string_ss_Lt	
					},
/* NclDataFunction ge; 		*/	{
						MultiDVal_string_mdmd_Ge,
						MultiDVal_string_mds_Ge,
						MultiDVal_string_smd_Ge,
						MultiDVal_string_ss_Ge
					},
/* NclDataFunction le; 		*/	{
						MultiDVal_string_mdmd_Le,
						MultiDVal_string_mds_Le,
						MultiDVal_string_smd_Le,
						MultiDVal_string_ss_Le
					},
/* NclDataFunction ne; 		*/	{
						MultiDVal_string_mdmd_Ne,
						MultiDVal_string_mds_Ne,
						MultiDVal_string_smd_Ne,
						MultiDVal_string_ss_Ne
					},
/* NclDataFunction eq; 		*/	{
						MultiDVal_string_mdmd_Eq,
						MultiDVal_string_mds_Eq,
						MultiDVal_string_smd_Eq,
						MultiDVal_string_ss_Eq
					},
/* NclDataFunction and;	 	*/	{
						NULL,
						NULL,
						NULL,
						NULL	
					},
/* NclDataFunction or; 		*/	{
						NULL,
						NULL,
						NULL,
						NULL
					},
/* NclDataFunction xor;		*/	{
						NULL,
						NULL,
						NULL,
						NULL
					}
	},
	{
		NULL
	},
	{
		NULL
	}
	
};

NclObjClass nclMultiDValstringDataClass = (NclObjClass)&nclMultiDValstringDataClassRec;


NclMultiDValData _NclMultiDValstringCreate
#if __STDC__
(NclObj inst,NclObjClass theclass,NclObjTypes obj_type,unsigned int obj_type_mask,void *val,NclScalar *missing_value,int n_dims, int *dim_sizes,NclStatus status,NclSelectionRecord *sel_rec)
#else
(inst,theclass,obj_type,obj_type_mask,val,missing_value,n_dims,dim_sizes,status,sel_rec)
NclObj inst ;
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
	NclMultiDValstringData thevalobj;
	NclObjClass class_ptr = nclMultiDValstringDataClass;
	int i;
	NhlErrorTypes ret1= NhlNOERROR;
	int nelem;

	ret1 = _NclInitClass(nclMultiDValstringDataClass);
	if(ret1 < NhlWARNING) {
		return(NULL);
	}
	if(inst == NULL ) {
		thevalobj = (NclMultiDValstringData)NclMalloc(
				(unsigned)nclMultiDValstringDataClassRec.obj_class.obj_size);
	} else {
		thevalobj = (NclMultiDValstringData)inst;
	}
	if(theclass != NULL) {
		class_ptr = theclass;
	}
/*
* Since no initialize functions exist for Obj and Data (meaningless because
* data has not instance record) fields must be assign manually here
*/

	_NclMultiDValDataCreate((NclObj)thevalobj,class_ptr,obj_type,(obj_type_mask | Ncl_MultiDValstringData),status);


	thevalobj->multidval.data_type = NCL_string;
	thevalobj->multidval.val = val;

	if(missing_value != NULL ) {
		thevalobj->multidval.missing_value.has_missing = 1;
			thevalobj->multidval.missing_value.value = *missing_value;
	} else {
		thevalobj->multidval.missing_value.has_missing = 0;
	}



	thevalobj->multidval.n_dims = n_dims;
	nelem = 1;
	if((n_dims == 1) &&(dim_sizes[0]== 1)) {
		thevalobj->multidval.kind = SCALAR;
		thevalobj->multidval.dim_sizes[0] = 1;
	} else {
		thevalobj->multidval.kind = MULTID;
		for(i = 0; i<n_dims; i++) {
			thevalobj->multidval.dim_sizes[i] = dim_sizes[i];
			nelem *= dim_sizes[i];
		}
	}
	thevalobj->multidval.totalelements = nelem;
	thevalobj->multidval.totalsize = nelem * _NclSizeOf(NCL_string);
	if(sel_rec != NULL) {
		thevalobj->multidval.sel_rec = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
		memcpy((char*)thevalobj->multidval.sel_rec,(char*)sel_rec,sizeof(NclSelectionRecord));
	} else {
		thevalobj->multidval.sel_rec = NULL;
	}

	thevalobj->multidval.hlu_type_rep = HLUTYPEREP;
	return((NclMultiDValData)thevalobj);
}

static void NclMultiDValstringPrint
#if __STDC__
(NclObj self, FILE *fp)
#else 
(self,fp)
NclObj self;
FILE *fp;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	int i[NCL_MAX_DIMENSIONS];
	int j[NCL_MAX_DIMENSIONS];
	int k,where,done = 0;
	int ndims = self_md->multidval.n_dims;

	
	for(k = 0; k < self_md->multidval.n_dims; k++) {
		i[k] = 0;
		j[k] = self_md->multidval.dim_sizes[k];
	}
	while(!done) {
		where = 0;
		fprintf(fp,"(");
		for(k = 0; k < ndims - 1; k++) {
			fprintf(fp,"%d,",i[k]);
			where = (where + i[k]) * j[k+1];
		}
		fprintf(fp,"%d)\t",i[ndims-1]);
		where = where + i[ndims - 1];

		if(((string*)(self_md->multidval.val))[where] != -1) {
			fprintf(fp,"%s\n",NrmQuarkToString(((string*)(self_md->multidval.val))[where]));
		} else {
			fprintf(fp,"<NULL>\n");
		}
		i[ndims - 1]++;
		if(i[ndims - 1] == j[ndims - 1]) {
			for(k=ndims - 1;k >0;k--) {
				if(i[k] == j[k]) {
					i[k] = 0;
					i[k-1]++;
				}
			}
			if(i[0] == j[0]) done = 1;
		}
	}
	return;
}

static void MultiDValDestroy
#if  __STDC__
(NclObj	self)
#else
(self)
	NclObj  self;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;

	_NclUnRegisterObj(self);
	if(self_md->multidval.sel_rec != NULL) {
		NclFree(self_md->multidval.sel_rec);
	}
	if(self_md->obj.status != STATIC) {
		NclFree(self_md->multidval.val);
	}
	NclFree(self);
/*
* handling of dimension information records goes here
*/
	return;
}

