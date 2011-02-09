
/*
 *      $Id: MatMulDValOp.c.sed,v 1.1.4.1 2008-03-28 20:37:48 grubin Exp $
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
 *	Date:		Fri Jan 27 18:22:08 MST 1995
 *
 *	Description:	
 */

static NclData MultiDVal_mdmd_Mat
#if	NhlNeedProto
(NclData self, NclData other, NclData result)
#else
(self,other,result)
NclData	self;
NclData other;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData other_md = (NclMultiDValData)other;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclMultiDValData output_md = NULL;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	int n_dims =0;
	ng_size_t dims[2];
	ng_size_t total;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims > 2)||(self_md->multidval.n_dims > 2)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Mat: One of the operands exceeds the rank of 2, can't continue");
		return(NULL);
        }
	if((other_md->multidval.n_dims == 2)&&(self_md->multidval.n_dims == 2)) {
		if(self_md->multidval.dim_sizes[1] != other_md->multidval.dim_sizes[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Mat: Dimension size mismatch");
			return(NULL);
		}
		n_dims = 2;
		dims[0] = self_md->multidval.dim_sizes[0];
		dims[1] = other_md->multidval.dim_sizes[1];
		total = dims[0]*dims[1];
	} else if(self_md->multidval.n_dims == 2) {
		if(self_md->multidval.dim_sizes[1] != other_md->multidval.dim_sizes[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Mat: Dimension size mismatch");
			return(NULL);
		}
		n_dims = 1;
		dims[0] = self_md->multidval.dim_sizes[0];
		total = dims[0];
	} else if(other_md->multidval.n_dims == 2) {
		if(self_md->multidval.dim_sizes[0] != other_md->multidval.dim_sizes[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Mat: Dimension size mismatch");
			return(NULL);
		}
		n_dims = 1;
		dims[0] = other_md->multidval.dim_sizes[1];
		total = dims[0];
	} else {
		if(self_md->multidval.dim_sizes[0] != other_md->multidval.dim_sizes[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Mat: Dimension size mismatch");
			return(NULL);
		}
		n_dims = 1;
		dims[0] = 1;
		total = 1;
	}



	operand_type = self_md->multidval.type;
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.mat != NULL)) {

/*
* the_type is not null since requirement if mat != NULL mat_type != NULL
*/

		the_type = _Nclmat_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md!=self_md)&&(result_md!=other_md)&&(result_md->multidval.data_type== the_type->type_class.data_type)&&(total*result_md->multidval.type->type_class.size <= result_md->multidval.totalsize)) {
			result_val = result_md->multidval.val;
			result_md->multidval.totalsize = total*result_md->multidval.type->type_class.size;
			result_md->multidval.n_dims = n_dims;
			if(n_dims == 2 ) {
				result_md->multidval.dim_sizes[0] = dims[0];
				result_md->multidval.dim_sizes[1] = dims[1];
			} else {
				result_md->multidval.dim_sizes[0] = dims[0];
			}
		} else if((result_md != NULL)&&(result_md!=self_md)&&(result_md!=other_md)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)&&(total*result_md->multidval.type->type_class.size <= result_md->multidval.totalsize)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.n_dims = n_dims;
			result_md->multidval.totalsize = total* the_type->type_class.size;
			if(n_dims == 2 ) {
				result_md->multidval.dim_sizes[0] = dims[0];
				result_md->multidval.dim_sizes[1] = dims[1];
			} else {
				result_md->multidval.dim_sizes[0] = dims[0];
			}
		} else {
			if((result_md != NULL)&&(result_md!=self_md)&&(result_md!=other_md))  {
				_NclDestroyObj((NclObj)result_md);
				result_md = NULL;
			} else {
				result_md = NULL;
			}
			result_val = (void*)NclMalloc(total * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclmat(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.n_dims,
			self_md->multidval.dim_sizes,
			other_md->multidval.n_dims,
			other_md->multidval.dim_sizes) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) ) {

					themissing.value = the_type->type_class.default_mis;
				}
			}
			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dims,
					TEMPORARY,
					NULL,
					the_type
				);
				return((NclData)output_md);
			} else {
				if(themissing.has_missing)
					result_md->multidval.missing_value = themissing;
				return((NclData)result_md);
			}
		} else {
			if(result_md == NULL) {
				NclFree(result_val);
			}
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Mat: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

