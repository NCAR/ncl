
/*
 *      $Id: string_sel.c.sed,v 1.2 1994-09-01 17:42:24 ethan Exp $
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
 *	Date:		Thu Jan 13 15:10:46 MST 1994
 *
 *	Description:	
 */
static NclData MultiDVal_string_mdmd_FUNCNAME
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
			return(NULL);
		} 
	}
	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements * sizeof(string));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result type, can't continue\n");
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
			res[i] = SELFUNC(ls[i],rs[i],-1,-1,-1);
		}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = SELFUNC(ls[i],rs[i],
			self_md->multidval.missing_value.value.stringval,
			-1,
			self_md->multidval.missing_value.value.stringval);
		}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements ; i++) {
			res[i] = SELFUNC(ls[i],rs[i],-1,
			other_md->multidval.missing_value.value.stringval,
			other_md->multidval.missing_value.value.stringval);
		}
	} else {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = SELFUNC(ls[i],rs[i],
			self_md->multidval.missing_value.value.stringval, 
			other_md->multidval.missing_value.value.stringval,
			self_md->multidval.missing_value.value.stringval);
		}
	}
	return((NclData)output_md);
}
static NclData MultiDVal_string_mds_FUNCNAME
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if((self_md->multidval.missing_value.has_missing)&&
		(self_md->multidval.missing_value.value.stringval != -1)) {

		themissing.value.stringval = self_md->multidval.missing_value.value.stringval;
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
				res[i] = SELFUNC(ls[i],*rs,
					-1,-1,-1);
			}
	} else if(!(other_md->multidval.missing_value.has_missing )) {
		res = (string*)result_val;
		ls = (string*)self_md->multidval.val;
		rs = (string*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = SELFUNC(ls[i],*rs,
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
					res[i] = SELFUNC(ls[i],*rs,
						-1,-1,-1);
				}
			}
	} else {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)other_md->multidval.val==other_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = self_md->multidval.missing_value.value.stringval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = SELFUNC(ls[i],*rs,
					self_md->multidval.missing_value.value.stringval,
					-1, 
					self_md->multidval.missing_value.value.stringval);
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_smd_FUNCNAME
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if((self_md->multidval.missing_value.has_missing)&&
		(self_md->multidval.missing_value.value.stringval!=-1)) {
		themissing.value.stringval = self_md->multidval.missing_value.value.stringval;
                themissing.has_missing = 1;
        } else if((other_md->multidval.missing_value.has_missing)&&
		(other_md->multidval.missing_value.value.stringval)) {
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
				res[i] = SELFUNC(*ls,rs[i],
					-1,-1,-1);
			}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = SELFUNC(*ls,rs[i],-1,
				other_md->multidval.missing_value.value.stringval,
				other_md->multidval.missing_value.value.stringval);
			}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
				rs = (string*)other_md->multidval.val;
				if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = self_md->multidval.missing_value.value.stringval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] =  SELFUNC(*ls,rs[i],
						-1,
						other_md->multidval.missing_value.value.stringval,
						self_md->multidval.missing_value.value.stringval);
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_string_ss_FUNCNAME
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
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
			*res = SELFUNC(*ls,*rs,-1,-1,-1);
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			*res = SELFUNC(*ls,*rs,-1,
				other_md->multidval.missing_value.value.stringval,
				other_md->multidval.missing_value.value.stringval);
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				*res = self_md->multidval.missing_value.value.stringval;
			} else {
				*res =  SELFUNC(*ls,*rs,-1,-1,-1);
			}
	} else {
			res = (string*)result_val;
			ls = (string*)self_md->multidval.val;
			rs = (string*)other_md->multidval.val;
			if(*(string*)self_md->multidval.val==self_md->multidval.missing_value.value.stringval) {
				*res = (string)self_md->multidval.missing_value.value.stringval;
			} else {
				*res = SELFUNC(*ls,*rs,-1,other_md->multidval.missing_value.value.stringval,self_md->multidval.missing_value.value.stringval);
			}
	}
	return((NclData)output_md);
} 

