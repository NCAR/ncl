
/*
 *      $Id: MultiDValMonoOpTemplate.c.sed,v 1.1 1994-07-14 20:46:09 ethan Exp $
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
 *	Date:		Thu Jan 13 14:54:08 MST 1994
 *
 *	Description:	
 */
static NclData MultiDVal_DATATYPE_md_FUNCNAME
#if __STDC__
(NclData self, NclData result)
#else
(self,result)
NclData self;
NclData result;
#endif
{
        NclMultiDValData self_md = (NclMultiDValData)self;
        NclMultiDValData result_md = (NclMultiDValData)result;
        NclData output_md;
        void *result_val;
	NclMissingRec themissing;
        int i;

        DATATYPE *ls;
	OUTDATATYPE *res;


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
        } else {
                themissing.has_missing = 0;
        }

	
	output_md = (NclData)_NclMultiDValOUTDATATYPECreate(
		(NclObj)result_md,NULL,
                Ncl_MultiDValOUTDATATYPEData,
                0,result_val,
		(themissing.has_missing?&(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);

	if(!(self_md->multidval.missing_value.has_missing)) { 
		res = (OUTDATATYPE*)result_val;
		ls = (DATATYPE*)self_md->multidval.val;
		for(i=0 ; i< self_md->multidval.totalelements; i++) {
			res[i] = (OUTDATATYPE)THEOP ls[i];
		}
	} else {
		res = (OUTDATATYPE*)result_val;
		ls = (DATATYPE*)self_md->multidval.val;
		for(i=0 ; i< self_md->multidval.totalelements; i++) {
			res[i] = (OUTDATATYPE)((
			self_md->multidval.missing_value.value.DATATYPEval 
			== ls[i]) ? (
			self_md->multidval.missing_value.value.DATATYPEval)
			: (THEOP ls[i]));
		}
	} 
	return((NclData)output_md);
}
static NclData MultiDVal_DATATYPE_s_FUNCNAME
#if __STDC__
(NclData self, NclData result)
#else
(self,result)
NclData self;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclData output_md;
	void *result_val;
	NclMissingRec themissing;


	DATATYPE *ls;
	OUTDATATYPE *res;



	result_val = (void*)NclMalloc(_NclSizeOf(self_md->multidval.data_type));
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
        } else {
                themissing.has_missing = 0;
        }
	output_md = (NclData)_NclMultiDValOUTDATATYPECreate(
		(NclObj)result_md,NULL,
                Ncl_MultiDValOUTDATATYPEData,
                0,result_val,
		(themissing.has_missing ? &(themissing.value):NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,NULL);


	if(!(self_md->multidval.missing_value.has_missing)) {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			*res = (OUTDATATYPE)THEOP *ls ;
	} else {
			res = (OUTDATATYPE*)result_val;
			ls = (DATATYPE*)self_md->multidval.val;
			if(*(DATATYPE*)self_md->multidval.val == self_md->multidval.missing_value.value.DATATYPEval) {
				*res = (OUTDATATYPE)self_md->multidval.missing_value.value.DATATYPEval;
			} else {
				*res =  (OUTDATATYPE)(THEOP *ls);
			}
	} 
	return((NclData)output_md);
} 
