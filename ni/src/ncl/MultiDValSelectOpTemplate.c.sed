/*
 *      $Id: MultiDValSelectOpTemplate.c.sed,v 1.1 1994-07-14 20:46:14 ethan Exp $
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
 *	Date:		Thu Jan 13 14:54:40 MST 1994
 *
 *	Description:	
 */
static NclData MultiDVal_DATATYPE_mdmd_FUNCNAME
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

        DATATYPE *ls,*rs;
	OUTDATATYPE *res;

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
	totalelements= self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements*sizeof(OUTDATATYPE));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result type, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.OUTDATATYPEval = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.OUTDATATYPEval = (OUTDATATYPE)other_md->multidval.missing_value.value.DATATYPEval;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValOUTDATATYPECreate(
		(NclObj)result_md,NULL,
                Ncl_MultiDValOUTDATATYPEData,
                0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing)) {
		res = (OUTDATATYPE*)result_val;
		ls = (DATATYPE*)self_md->multidval.val;
		rs = (DATATYPE*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (OUTDATATYPE)(ls[i] THEOP rs[i])? ls[i]:rs[i];
		}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
		res = (OUTDATATYPE*)result_val;
		ls = (DATATYPE*)self_md->multidval.val;
		rs = (DATATYPE*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (OUTDATATYPE)((
			self_md->multidval.missing_value.value.DATATYPEval 
			== ls[i]) ? (
			self_md->multidval.missing_value.value.DATATYPEval)
			: (ls[i] THEOP rs[i])? ls[i] : rs[i]);
		}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
		res = (OUTDATATYPE*)result_val;
		ls = (DATATYPE*)self_md->multidval.val;
		rs = (DATATYPE*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (OUTDATATYPE)((
			other_md->multidval.missing_value.value.DATATYPEval 
			== rs[i]) ? (
			other_md->multidval.missing_value.value.DATATYPEval)
			: (ls[i] THEOP rs[i])? ls[i] : rs[i]);
		}
	} else {
		res = (OUTDATATYPE*)result_val;
		ls = (DATATYPE*)self_md->multidval.val;
		rs = (DATATYPE*)other_md->multidval.val;
		for(i=0 ; i< totalelements; i++) {
			res[i] = (OUTDATATYPE)(((
			self_md->multidval.missing_value.value.DATATYPEval 
			== ls[i])|| (
                                      other_md->multidval.missing_value.value.DATATYPEval
                                      == rs[i])) ? (
			self_md->multidval.missing_value.value.DATATYPEval)
			: (ls[i] THEOP rs[i])? ls[i] : rs[i]);
		}
	}
	return((NclData)output_md);
}
static NclData MultiDVal_DATATYPE_mds_FUNCNAME
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
	int i,totalelements= 1;
	DATATYPE *ls,*rs;
	OUTDATATYPE *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));



	totalelements = self_md->multidval.totalelements;
	result_val = (void*)NclMalloc(self_md->multidval.totalelements * sizeof(OUTDATATYPE));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.OUTDATATYPEval = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.OUTDATATYPEval = (OUTDATATYPE)other_md->multidval.missing_value.value.DATATYPEval;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValOUTDATATYPECreate(
		(NclObj)result_md,NULL,
                Ncl_MultiDValOUTDATATYPEData,
                0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing ) &&
		!(other_md->multidval.missing_value.has_missing)) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (OUTDATATYPE)(ls[i] THEOP *rs)? ls[i] : *rs;
			}
	} else if(!(other_md->multidval.missing_value.has_missing )) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (OUTDATATYPE)((
				self_md->multidval.missing_value.value.DATATYPEval 
				== ls[i]) ? (
				self_md->multidval.missing_value.value.DATATYPEval)
				: (ls[i] THEOP *rs)?ls[i]:*rs);
			}
	} else if(!(self_md->multidval.missing_value.has_missing )) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			if(*(DATATYPE*)other_md->multidval.val == other_md->multidval.missing_value.value.DATATYPEval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (OUTDATATYPE)other_md->multidval.missing_value.value.DATATYPEval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (OUTDATATYPE)(ls[i] THEOP *rs)?ls[i]:*rs;
				}
			}
	} else {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			if(*(DATATYPE*)other_md->multidval.val == other_md->multidval.missing_value.value.DATATYPEval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (OUTDATATYPE)(((
						self_md->multidval.missing_value.value.DATATYPEval == ls[i])) 
						? (self_md->multidval.missing_value.value.DATATYPEval) 
						: (ls[i] THEOP *rs)?ls[i]:*rs);
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_DATATYPE_smd_FUNCNAME
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
	int i,totalelements = 1;
	DATATYPE *ls,*rs;
	OUTDATATYPE *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));


	totalelements = other_md->multidval.totalelements;
	result_val = (void*)NclMalloc(other_md->multidval.totalelements*sizeof(OUTDATATYPE));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.OUTDATATYPEval = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.OUTDATATYPEval = (OUTDATATYPE)other_md->multidval.missing_value.value.DATATYPEval;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValOUTDATATYPECreate(
		(NclObj)result_md,NULL,
                Ncl_MultiDValOUTDATATYPEData,
                0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		other_md->multidval.n_dims,
		other_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing )) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (OUTDATATYPE)(*ls THEOP rs[i])?*ls:rs[i];
			}
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			for(i=0 ; i< totalelements; i++) {
				res[i] = (OUTDATATYPE)((
				other_md->multidval.missing_value.value.DATATYPEval 
				== rs[i]) ? (
				other_md->multidval.missing_value.value.DATATYPEval)
				: (*ls THEOP rs[i])?*ls:rs[i]);
			}
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
				rs = (DATATYPE*)other_md->multidval.val;
				if(*(DATATYPE*)self_md->multidval.val == self_md->multidval.missing_value.value.DATATYPEval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] =  (OUTDATATYPE)(*ls THEOP rs[i])?*ls:rs[i];
				}
			}
	} else {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			if(*(DATATYPE*)self_md->multidval.val == self_md->multidval.missing_value.value.DATATYPEval) {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
				}
			} else {
				for(i=0 ; i< totalelements; i++) {
					res[i] = (OUTDATATYPE)(((
						other_md->multidval.missing_value.value.DATATYPEval == rs[i])) 
						? ( self_md->multidval.missing_value.value.DATATYPEval) 
						: (*ls THEOP rs[i])?*ls:rs[i]);
				}
			}
	}
	return((NclData)output_md);
} 
static NclData MultiDVal_DATATYPE_ss_FUNCNAME
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

	DATATYPE *ls,*rs;
	OUTDATATYPE *res;

	ASSERT((other_md->obj.obj_type_mask&(unsigned int)Ncl_MultiDValData));	
	ASSERT((self_md->multidval.data_type==other_md->multidval.data_type));


	result_val = (void*)NclMalloc(sizeof(OUTDATATYPE));
	if(result_val == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result, can't continue\n");
			return(NULL);
	}
/*
* When here obj types match, data_types match, n_dims match , dim_sizes match
* and memory has been allocated.
*/
	if(self_md->multidval.missing_value.has_missing) {
                themissing.value.OUTDATATYPEval = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value.OUTDATATYPEval = (OUTDATATYPE)other_md->multidval.missing_value.value.DATATYPEval;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	output_md = (NclData)_NclMultiDValOUTDATATYPECreate(
		(NclObj)result_md,NULL,
                Ncl_MultiDValOUTDATATYPEData,
                0,result_val,
		(themissing.has_missing? &(themissing.value):NULL),
		other_md->multidval.n_dims,
		other_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing) &&
		!(other_md->multidval.missing_value.has_missing )) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			*res = (OUTDATATYPE)(*ls THEOP *rs)?*ls :*rs ;
	} else if(!(self_md->multidval.missing_value.has_missing)) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			*res = ((
			other_md->multidval.missing_value.value.DATATYPEval 
			== *rs) ? (
			other_md->multidval.missing_value.value.DATATYPEval)
			: (*ls THEOP *rs)?*ls:*rs);
	} else if(!(other_md->multidval.missing_value.has_missing)) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			if(*(DATATYPE*)self_md->multidval.val == self_md->multidval.missing_value.value.DATATYPEval) {
				*res = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
			} else {
				*res =  (OUTDATATYPE)(*ls THEOP *rs)?*ls:*rs;
			}
	} else {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			rs = (DATATYPE*)other_md->multidval.val;
			if(*(DATATYPE*)self_md->multidval.val == self_md->multidval.missing_value.value.DATATYPEval) {
				*res = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
			} else {
				*res = (OUTDATATYPE)(((
					other_md->multidval.missing_value.value.DATATYPEval == *rs)) 
					? ( self_md->multidval.missing_value.value.DATATYPEval) 
					: (*ls THEOP *rs)?*ls:*rs);
			}
	}
	return((NclData)output_md);
} 
