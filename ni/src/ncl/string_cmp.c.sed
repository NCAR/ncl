
/*
 *      $Id: string_cmp.c.sed,v 1.2 1994-12-23 01:19:28 ethan Exp $
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
 *	Date:		Thu Jan 13 15:10:31 MST 1994
 *
 *	Description:	
 */
static NclData MultiDVal_string_mdmd_FUNCNAME
#if	NhlNeedProto
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
			return(NULL);
		} 
	}
	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements * sizeof(int));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result type, can't continue\n");
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
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,
		result_val,
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
			res[i] = (CMPFUNC(ls[i],rs[i]));
		}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (int)((
                        self_md->multidval.missing_value.value.stringval ==
                        ls[i]) ? (
                        (int)NCL_DEFAULT_MISSING_VALUE)
                        : CMPFUNC(ls[i],rs[i]));

		}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements ; i++) {
			res[i] = (int)((
                        other_md->multidval.missing_value.value.stringval
                        ==rs[i]) ? (
                        (int)NCL_DEFAULT_MISSING_VALUE )
                        : CMPFUNC(ls[i],rs[i]));
		}
	} else {
		res = (int*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (int)(((
				self_md->multidval.missing_value.value.stringval==ls[i])
				||(other_md->multidval.missing_value.value.stringval==rs[i])) ? (
			(int)NCL_DEFAULT_MISSING_VALUE)
			: CMPFUNC(ls[i],rs[i]));
		}
	}
	return((NclData)output_md);
}
static NclData MultiDVal_string_mds_FUNCNAME
#if	NhlNeedProto
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
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
		(NclObj)result_md,NULL,Ncl_MultiDValintData,0,
		result_val,
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
				res[i] = CMPFUNC(ls[i],*rs);
			}
	} else if(!(other_md->multidval.missing_value.has_missing )) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)((
				self_md->multidval.missing_value.value.stringval == ls[i]) ? (
				(int)NCL_DEFAULT_MISSING_VALUE)
				: CMPFUNC(ls[i],*rs));
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
					res[i] = (int)CMPFUNC(ls[i],*rs);
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
						: CMPFUNC(ls[i],*rs));
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_smd_FUNCNAME
#if	NhlNeedProto
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
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
		(NclObj)result_md,
		NULL,Ncl_MultiDValintData,0,
		result_val,
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
				res[i] = (int)CMPFUNC(*ls,rs[i]);
			}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (int)((
				other_md->multidval.missing_value.value.stringval == rs[i]) ? (
				(int)NCL_DEFAULT_MISSING_VALUE)
				: CMPFUNC(*ls,rs[i]));
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
					res[i] =  (int)CMPFUNC(*ls,rs[i]);
				}
			}
	} else {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val == self_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)NCL_DEFAULT_MISSING_VALUE;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (int)(((
						other_md->multidval.missing_value.value.stringval==rs[i])) 
						? ((int)NCL_DEFAULT_MISSING_VALUE)  
						: CMPFUNC(*ls,rs[i]));
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_ss_FUNCNAME
#if	NhlNeedProto
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
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
			*res = (int)CMPFUNC(*ls,*rs);
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			*res = (int)((
			other_md->multidval.missing_value.value.stringval == *rs) ? (
			NCL_DEFAULT_MISSING_VALUE)
			: CMPFUNC(*ls,*rs));
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (int*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				*res = (int)NCL_DEFAULT_MISSING_VALUE;
			} else {
				*res =  (int)CMPFUNC(*ls,*rs);
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
					: CMPFUNC(*ls,*rs));
			}
	}
	return((NclData)output_md);
} 
