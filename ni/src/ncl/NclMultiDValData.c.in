
/*
 *      $Id: NclMultiDValData.c.sed,v 1.46 2009-07-02 23:17:36 dbrown Exp $
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
#include "niohlu.h"
#include "nioNresDB.h"
short    NCLnoPrintElem;  
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
extern short    NCLnoPrintElem;     /* for multiple element print() stmts */
#endif
#include "defs.h"
#include <errno.h>
#include "NclMultiDValData.h"
#include "TypeSupport.h"
#include "DataSupport.h"
#include <math.h>
#include <stdlib.h>
#include "NclTypestring.h"
#include "NclTypechar.h"



/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Mul
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Mul: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Mul: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.multiply != NULL)) {

/*
* the_type is not null since requirement if multiply != NULL multiply_type != NULL
*/

		the_type = _Nclmultiply_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Mul: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclmultiply(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Mul: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Mul: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Mul
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"multiply for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.multiply != NULL)) {

/*
* the_type is not null since requirement if multiply != NULL multiply_type != NULL
*/

		the_type = _Nclmultiply_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Mul: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclmultiply(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Mul: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Mul: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Div
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Div: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Div: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.divide != NULL)) {

/*
* the_type is not null since requirement if divide != NULL divide_type != NULL
*/

		the_type = _Ncldivide_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Div: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Ncldivide(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Div: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Div: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Div
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"divide for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.divide != NULL)) {

/*
* the_type is not null since requirement if divide != NULL divide_type != NULL
*/

		the_type = _Ncldivide_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Div: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Ncldivide(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Div: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Div: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Plus
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.plus != NULL)) {

/*
* the_type is not null since requirement if plus != NULL plus_type != NULL
*/

		the_type = _Nclplus_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclplus(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Plus
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"plus for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.plus != NULL)) {

/*
* the_type is not null since requirement if plus != NULL plus_type != NULL
*/

		the_type = _Nclplus_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclplus(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Plus: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Minus
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Minus: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Minus: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.minus != NULL)) {

/*
* the_type is not null since requirement if minus != NULL minus_type != NULL
*/

		the_type = _Nclminus_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Minus: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclminus(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Minus: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Minus: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Minus
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"minus for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.minus != NULL)) {

/*
* the_type is not null since requirement if minus != NULL minus_type != NULL
*/

		the_type = _Nclminus_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Minus: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclminus(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Minus: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Minus: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Exp
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Exp: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Exp: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.exponent != NULL)) {

/*
* the_type is not null since requirement if exponent != NULL exponent_type != NULL
*/

		the_type = _Nclexponent_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Exp: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclexponent(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Exp: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Exp: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Exp
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"exponent for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.exponent != NULL)) {

/*
* the_type is not null since requirement if exponent != NULL exponent_type != NULL
*/

		the_type = _Nclexponent_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Exp: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclexponent(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Exp: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Exp: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Mod
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Mod: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Mod: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.mod != NULL)) {

/*
* the_type is not null since requirement if mod != NULL mod_type != NULL
*/

		the_type = _Nclmod_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Mod: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclmod(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Mod: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Mod: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Mod
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"mod for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.mod != NULL)) {

/*
* the_type is not null since requirement if mod != NULL mod_type != NULL
*/

		the_type = _Nclmod_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Mod: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclmod(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Mod: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Mod: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_SelLt
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SelLt: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"SelLt: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.sel_lt != NULL)) {

/*
* the_type is not null since requirement if sel_lt != NULL sel_lt_type != NULL
*/

		the_type = _Nclsel_lt_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"SelLt: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclsel_lt(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"SelLt: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SelLt: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_SelLt
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"sel_lt for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.sel_lt != NULL)) {

/*
* the_type is not null since requirement if sel_lt != NULL sel_lt_type != NULL
*/

		the_type = _Nclsel_lt_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"SelLt: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclsel_lt(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"SelLt: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SelLt: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_SelGt
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SelGt: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"SelGt: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.sel_gt != NULL)) {

/*
* the_type is not null since requirement if sel_gt != NULL sel_gt_type != NULL
*/

		the_type = _Nclsel_gt_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"SelGt: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclsel_gt(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"SelGt: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SelGt: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_SelGt
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"sel_gt for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.sel_gt != NULL)) {

/*
* the_type is not null since requirement if sel_gt != NULL sel_gt_type != NULL
*/

		the_type = _Nclsel_gt_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"SelGt: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclsel_gt(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"SelGt: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SelGt: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Gt
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Gt: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Gt: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.gt != NULL)) {

/*
* the_type is not null since requirement if gt != NULL gt_type != NULL
*/

		the_type = _Nclgt_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Gt: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclgt(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Gt: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Gt: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Gt
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"gt for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.gt != NULL)) {

/*
* the_type is not null since requirement if gt != NULL gt_type != NULL
*/

		the_type = _Nclgt_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Gt: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclgt(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Gt: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Gt: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Lt
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Lt: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Lt: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.lt != NULL)) {

/*
* the_type is not null since requirement if lt != NULL lt_type != NULL
*/

		the_type = _Ncllt_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Lt: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Ncllt(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Lt: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Lt: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Lt
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"lt for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.lt != NULL)) {

/*
* the_type is not null since requirement if lt != NULL lt_type != NULL
*/

		the_type = _Ncllt_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Lt: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Ncllt(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Lt: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Lt: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Ge
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Ge: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Ge: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ge != NULL)) {

/*
* the_type is not null since requirement if ge != NULL ge_type != NULL
*/

		the_type = _Nclge_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Ge: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclge(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Ge: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Ge: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Ge
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ge for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ge != NULL)) {

/*
* the_type is not null since requirement if ge != NULL ge_type != NULL
*/

		the_type = _Nclge_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Ge: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclge(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Ge: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Ge: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Le
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Le: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Le: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.le != NULL)) {

/*
* the_type is not null since requirement if le != NULL le_type != NULL
*/

		the_type = _Nclle_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Le: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclle(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Le: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Le: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Le
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"le for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.le != NULL)) {

/*
* the_type is not null since requirement if le != NULL le_type != NULL
*/

		the_type = _Nclle_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Le: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclle(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Le: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Le: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Ne
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Ne: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Ne: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ne != NULL)) {

/*
* the_type is not null since requirement if ne != NULL ne_type != NULL
*/

		the_type = _Nclne_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Ne: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclne(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Ne: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Ne: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Ne
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ne for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ne != NULL)) {

/*
* the_type is not null since requirement if ne != NULL ne_type != NULL
*/

		the_type = _Nclne_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Ne: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclne(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Ne: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Ne: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Eq
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.eq != NULL)) {

/*
* the_type is not null since requirement if eq != NULL eq_type != NULL
*/

		the_type = _Ncleq_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Ncleq(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Eq
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"eq for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.eq != NULL)) {

/*
* the_type is not null since requirement if eq != NULL eq_type != NULL
*/

		the_type = _Ncleq_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Ncleq(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Eq: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_And
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"And: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"And: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ncl_and != NULL)) {

/*
* the_type is not null since requirement if ncl_and != NULL and_type != NULL
*/

		the_type = _Ncland_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"And: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclncl_and(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"And: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"And: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_And
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ncl_and for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ncl_and != NULL)) {

/*
* the_type is not null since requirement if ncl_and != NULL and_type != NULL
*/

		the_type = _Ncland_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"And: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclncl_and(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"And: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"And: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Or
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Or: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Or: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ncl_or != NULL)) {

/*
* the_type is not null since requirement if ncl_or != NULL or_type != NULL
*/

		the_type = _Nclor_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Or: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclncl_or(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Or: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Or: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Or
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ncl_or for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ncl_or != NULL)) {

/*
* the_type is not null since requirement if ncl_or != NULL or_type != NULL
*/

		the_type = _Nclor_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Or: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclncl_or(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Or: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Or: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

/*
 *      $Id: MultiDValOp.c.sed,v 1.13.4.1 2008-03-28 20:37:49 grubin Exp $
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

static NclData MultiDVal_mdmd_Xor
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
	ng_size_t i;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

        if((other_md->multidval.n_dims != self_md->multidval.n_dims)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Xor: Number of dimensions do not match, can't continue");
                return(NULL);
        }
	operand_type = self_md->multidval.type;
	for(i = 0; i< self_md->multidval.n_dims; i++) {
                if(self_md->multidval.dim_sizes[i]
                        != other_md->multidval.dim_sizes[i]) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Xor: Dimension size, for dimension number %d, of operands does not match, can't continue\n",i);
                        return(NULL);
                }
        }
        if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else if(other_md->multidval.missing_value.has_missing) {
                themissing.value = other_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ncl_xor != NULL)) {

/*
* the_type is not null since requirement if ncl_xor != NULL xor_type != NULL
*/

		the_type = _Nclxor_type(self_md->multidval.type);
		if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
			result_val = result_md->multidval.val;
		} else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
			result_val = result_md->multidval.val;
			result_md->multidval.type = the_type;
			result_md->multidval.data_type = the_type->type_class.data_type;
			result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
			result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
			result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
		} else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
				_NclDestroyObj((NclObj)result_md);
			}
			result_md = NULL;
			result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
			if(result_val == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Xor: Could not allocate memory for result type, can't continue\n");
				return(NULL);
			}
		}
		if(_Nclncl_xor(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if((the_type != operand_type)&&(themissing.has_missing)) {
				if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {

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
					self_md->multidval.n_dims,
					self_md->multidval.dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Xor: operator failed, can't continue");
			return(NULL);
		}
			
			
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Xor: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static NclData MultiDVal_ss_Xor
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
	NclMultiDValData output_md;
	NclTypeClass the_type = NULL;
	NclTypeClass operand_type = NULL;
	NclMissingRec themissing;
	void *result_val;
	ng_size_t total;
	ng_size_t *dim_sizes = NULL;
	int n_dims = 1;
	

	if((other_md == NULL)||(self_md == NULL))
		return(NULL);

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
	if(!((self_md->multidval.kind == SCALAR)||(other_md->multidval.kind == SCALAR))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ncl_xor for scalar values was called with non scalar value, this should not happen");
		return(NULL);
	}

	total = self_md->multidval.totalelements;
	n_dims = self_md->multidval.n_dims;
	dim_sizes = self_md->multidval.dim_sizes;
	if(total < other_md->multidval.totalelements) {
		total = other_md->multidval.totalelements;
		n_dims = other_md->multidval.n_dims;
		dim_sizes = other_md->multidval.dim_sizes;
	}

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ncl_xor != NULL)) {

/*
* the_type is not null since requirement if ncl_xor != NULL xor_type != NULL
*/

		the_type = _Nclxor_type(self_md->multidval.type);

                if((result_md != NULL)&&(result_md->multidval.data_type== the_type->type_class.data_type)) {
                        result_val = result_md->multidval.val;
                } else if((result_md != NULL)&&(result_md->multidval.type->type_class.size >= the_type->type_class.size)) {
                        result_val = result_md->multidval.val;
                        result_md->multidval.type = the_type;
                        result_md->multidval.data_type = the_type->type_class.data_type;
                        result_md->multidval.hlu_type_rep[0] = the_type->type_class.hlu_type_rep[0];
                        result_md->multidval.hlu_type_rep[1] = the_type->type_class.hlu_type_rep[1];
                        result_md->multidval.totalsize = result_md->multidval.totalelements * the_type->type_class.size;
                } else {
			if((result_md != NULL) &&((result_md != self_md)&&(result_md != other_md))) {
                                _NclDestroyObj((NclObj)result_md);
                        }
                        result_md = NULL;
                        result_val = (void*)NclMalloc(total * the_type->type_class.size);
                        if(result_val == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Xor: Could not allocate memory for result type, can't continue\n");
                                return(NULL);
                        }
                }
		if((the_type != operand_type)&&(themissing.has_missing)) {
			if((the_type->type_class.data_type == NCL_logical)||(!_NclScalarCoerce(&themissing.value,operand_type->type_class.data_type,&themissing.value,the_type->type_class.data_type) )) {
				themissing.value = the_type->type_class.default_mis;
			}
		}

		if(_Nclncl_xor(
			operand_type,
			result_val,
			self_md->multidval.val,
			other_md->multidval.val,
			(self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
			(other_md->multidval.missing_value.has_missing?(void*)&other_md->multidval.missing_value.value:NULL),
			self_md->multidval.totalelements,
			other_md->multidval.totalelements) != NhlFATAL) {

			if(result_md == NULL) {
				output_md = _NclCreateMultiDVal(
					(NclObj)result_md,
					NULL,
					Ncl_MultiDValData,
					0,
					result_val,
					(themissing.has_missing? &(themissing.value):NULL),
					n_dims,
					dim_sizes,
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Xor: operator failed, can't continue");
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Xor: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

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
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Mat: Could not allocate memory for result type, can't continue\n");
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




static struct _NclDataRec *MultiDValReadSection
#if	NhlNeedProto
(NclData self, NclSelectionRecord * sel,NclScalar *missing)
#else
(self,sel,missing)
	NclData self;
	NclSelectionRecord *sel;
	NclScalar *missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclData output_md;
	NclSelection *sel_ptr;
	void *val;
	ng_size_t i,k,from,to;

	ng_size_t current_index[NCL_MAX_DIMENSIONS];
	ng_size_t multiplier[NCL_MAX_DIMENSIONS];
	ng_size_t compare_sel[NCL_MAX_DIMENSIONS];
	ng_size_t strider[NCL_MAX_DIMENSIONS];
	ng_size_t keeper[NCL_MAX_DIMENSIONS];
	ng_size_t output_dim_sizes[NCL_MAX_DIMENSIONS];

	ng_size_t total_elements = 1;
	int n_dims_input = self_md->multidval.n_dims;
	ng_size_t n_elem=0;
	int done = 0;
	int inc_done = 0;
	ng_size_t el_size = 0;
	ng_size_t rem_count;
	ng_size_t cpy_count = 1;
	int last = 0;

/*
* Pre Conditions: 	number entries in selection record == n_dims
*			object is not scalar
* 			if it is a reorder only one reference to a values 
*				dimension is allowed (no duplicates!!)
*			if a missing value is passed in it is the same
* 			type as the value being selected
*
* First check dimension ranges.
*
* Check to see if vector subscripting is used.
*
* Compute total number of elements in output array.
* 
* Allocate array and perform selection, three types 
* (only normal subs,contains vector subs,reordering selection)
* 
* if no vector subscripting is used then add selection record to
* output object.
*/
	if(sel!= NULL){
		sel_ptr	= sel->selection;
	}
	for(i = 0 ; i < n_dims_input; i++) {
		switch(sel_ptr->sel_type) {
		case Ncl_SUB_ALL:                             
			sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			sel_ptr->u.sub.finish = (long)self_md->multidval.dim_sizes[sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
* Note all cases fall through to here (no break statement)
*/
			sel_ptr->sel_type = Ncl_SUBSCR;
		case Ncl_SUBSCR:
			if(sel_ptr->u.sub.is_single) {
				keeper[i] = 0;
			} else {
				keeper[i] = 1;
			}
			if(sel_ptr->u.sub.finish < sel_ptr->u.sub.start) {
				

				if(sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
					sel_ptr->u.sub.stride = 1;
				}

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				if(sel_ptr->u.sub.stride < 0){
					current_index[i] = sel_ptr->u.sub.finish + (sel_ptr->u.sub.start - sel_ptr->u.sub.finish) % abs(sel_ptr->u.sub.stride);
					sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
					sel_ptr->u.sub.start = current_index[i];
					compare_sel[i] = -2;
					strider[i] = -(sel_ptr->u.sub.stride);
					sel_ptr->u.sub.stride = strider[i];

				} else {
					compare_sel[i] = -1;
					current_index[i] = sel_ptr->u.sub.start;
					strider[i] = -(sel_ptr->u.sub.stride);
				}
			} else {
				if(sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
					sel_ptr->u.sub.stride = 1;

				}

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;
				
				if(sel_ptr->u.sub.stride < 0){
					compare_sel[i] = -1;
					current_index[i] = sel_ptr->u.sub.finish - (sel_ptr->u.sub.finish - sel_ptr->u.sub.start) % abs(sel_ptr->u.sub.stride);
					sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
					sel_ptr->u.sub.start = current_index[i];
					strider[i] = sel_ptr->u.sub.stride;
					sel_ptr->u.sub.stride = -strider[i];

				} else {
					compare_sel[i] = -2;
					current_index[i] = sel_ptr->u.sub.start;
					strider[i] = sel_ptr->u.sub.stride;
				}
			}
			if((sel_ptr->u.sub.start > self_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}

			if((sel_ptr->u.sub.finish > self_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			keeper[i] = 1;
			if((sel_ptr->u.vec.min < 0)|| (sel_ptr->u.vec.min > self_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}
			if((sel_ptr->u.vec.max < 0)|| (sel_ptr->u.vec.max > self_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}
			n_elem = sel_ptr->u.vec.n_ind;
			strider[i] = 0;
			current_index[i] = sel_ptr->u.vec.ind[0]; 
			compare_sel[i] = 0;
		} 
		if(sel_ptr->dim_num != n_dims_input -1) {
			multiplier[i] = 1;

			for(k = sel_ptr->dim_num + 1; k < n_dims_input ;k++) {
				multiplier[i] *= (long)self_md->multidval.dim_sizes[k];
			}
		} else {
			multiplier[i] = 1;
		}
		output_dim_sizes[i] = n_elem;
		total_elements = total_elements * n_elem;
		sel_ptr++;
	}
	sel_ptr = sel->selection;
/*
* All subscript ranges are valid. whether or not it is a reorder, and
* whether or not a vector subscript is present are known.
* also multiplier contains the appropriate multiplier by which to multiply
* an index to get the actual index in the value vector.
* the output arrays dimension sizes are known and the total number of 
* elements is known 
* the current_index array is inialized to the starting location
*
* compare_sel contains either -2 or -1 for normal subscripts or the
* index >= 0 into the integer vector array.
*/

/*
 * Now determine the amount of continguous data that may be copied in a single 
 * call to memcpy. Conditions are as follows:
 * Testing proceeds from right to left (fastest dimension to slowest);
 * If a dimension is a proper subset, dimensions to the left must be handled individually ('last' test)
 * For each dimension, its elements must not be reversed (positive stride + compare_sel[i] == -2 ensures this)
 * For each dimension stride must be 1 (strider[i] == 1)
 * The dimension cannot be reordered (its dim_num should equal its position)
 * The dimension cannot be a vector subscript (compare_sel[i] should not be positive)
 * Any dimension that does not meet these requirements ensures that dimensions to the left cannot be contiguous either. 
 */
	for (i = n_dims_input - 1; i >= 0; i--) {
		if (last || compare_sel[i] != -2 || strider[i] != 1 || (i != sel_ptr[i].dim_num))
			break;
		if (output_dim_sizes[i] < self_md->multidval.dim_sizes[sel_ptr[i].dim_num])
			last = 1;
		cpy_count *= output_dim_sizes[i];
	}
	
	el_size = self_md->multidval.type->type_class.size;
	val = (void*)NclMalloc(total_elements * el_size);
	to = 0;
	while(!done) {
		rem_count = cpy_count;
		from = 0;
		for(i = 0; i < n_dims_input;i++) {
			from = from + (current_index[i] * multiplier[i]);
		}
		memcpy((void*)((char*)val + to * el_size),(void*)((char*)self_md->multidval.val + from * el_size),el_size * cpy_count);

		if (rem_count > 1) {
			current_index[n_dims_input -1 ] += output_dim_sizes[n_dims_input -1];
			rem_count /= output_dim_sizes[n_dims_input -1];
		}
		else if (compare_sel[n_dims_input-1] <0) {
			current_index[n_dims_input -1 ] += strider[n_dims_input-1];
		} 
		else {
			compare_sel[n_dims_input-1]++;
		}
		
		for(k = n_dims_input-1; k >0; k--) {
			switch(compare_sel[k]) {
			case -2: 
				if(current_index[k] > sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if (rem_count > 1) {
						current_index[k -1 ] += output_dim_sizes[k -1];
						rem_count /= output_dim_sizes[k -1];
					}
					else if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			case -1:
				if(current_index[k] < sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			default:
/*
* Only falls through to here when vector is present because compare_sel
* is only positive when a vector is present
*/
				if(compare_sel[k] >= sel_ptr[k].u.vec.n_ind) {
					compare_sel[k] = 0;
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}

				} else {
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					inc_done = 1;
				}
				break;
			}

			if(inc_done) {
				inc_done = 0;
				break;
			}
		}
		switch(compare_sel[0]) {
		case -2:
			if(current_index[0] > sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		case -1:
			if(current_index[0] < sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		default:
			if(compare_sel[0] >= sel_ptr[0].u.vec.n_ind) {
					done = 1;
			} else {
				current_index[0] = sel_ptr[0].u.vec.ind[compare_sel[0]];
			}
			break;
		}
		to +=cpy_count;
	}
	i =0;
	while(i < n_dims_input) {
		if((output_dim_sizes[i] < 2)&&(!keeper[i])) {
			for(k=i; k< n_dims_input-1;k++){
				output_dim_sizes[k] = output_dim_sizes[k+1];
				keeper[k] = keeper[k+1];
			}
			n_dims_input--;
		} else {
			i++;
		}
	}
	if(n_dims_input == 0) {
/*
* This means every one of the output_dim_sizes was one hence SCALAR 
* subsection
*/
		n_dims_input = 1;
	}
	if((self_md->multidval.missing_value.has_missing)&&
		(missing != NULL)) {
		_Nclreset_mis(self_md->multidval.type,self_md->multidval.val,&(self_md->multidval.missing_value.value),missing,total_elements);
		output_md = (NclData)_NclCreateVal(NULL,
			NULL,
			self_md->obj.obj_type,
			0,(void*)val,	
			missing,
			n_dims_input,output_dim_sizes,
			TEMPORARY,sel,(NclObjClass)self_md->multidval.type);
	} else if(self_md->multidval.missing_value.has_missing) {
		output_md = (NclData)_NclCreateVal(NULL,
			NULL,
			self_md->obj.obj_type,
			0,(void*)val,	
			&self_md->multidval.missing_value.value,
			n_dims_input,output_dim_sizes,
			TEMPORARY,sel,(NclObjClass)self_md->multidval.type);
	} else {
		output_md = (NclData)_NclCreateVal(NULL,
			NULL,
			self_md->obj.obj_type,
			0,(void*)val,	
			NULL,
			n_dims_input,output_dim_sizes,
			TEMPORARY,sel,(NclObjClass)self_md->multidval.type);
	}

	return(output_md);		
}


static NhlErrorTypes MultiDVal_md_WriteSection
#if	NhlNeedProto
(NclData target, NclSelectionRecord * sel, struct _NclDataRec* value)
#else
(target,sel,value)
	NclData target;
	NclSelectionRecord *sel;
	NclData value;
#endif
{
	NclMultiDValData target_md = (NclMultiDValData)target;
	NclMultiDValData value_md = (NclMultiDValData)value;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/
	NclSelection *sel_ptr = NULL;
	void *val;
	ng_size_t i,k;
	ng_size_t from,to;

	ng_size_t current_index[NCL_MAX_DIMENSIONS];
	ng_size_t multiplier[NCL_MAX_DIMENSIONS];
	ng_size_t compare_sel[NCL_MAX_DIMENSIONS];
	ng_size_t strider[NCL_MAX_DIMENSIONS];
	ng_size_t output_dim_sizes[NCL_MAX_DIMENSIONS];

	ng_size_t dim_sizes_value [NCL_MAX_DIMENSIONS];
	int n_dims_value = 0;
	int n_dims_sel = 0;
	ng_size_t total_elements = 1;
	int n_dims_target = target_md->multidval.n_dims;
	ng_size_t n_elem=0;
	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	logical tmpe;
	ng_size_t el_size;
	ng_size_t rem_count;
	ng_size_t cpy_count = 1;
	int last = 0;

/*
* preconditions:
*	target and value are same type of object
*	sel is a valid selection record with no duplicate indices and
* 	contains a selection entry for each dimension in target or it
* 	is null implying a direct copy.
*
* 	*****Value is a multidimensional array***** 
*	number of dimensions >= 1 && size > 1
*/

	if((target_md->multidval.missing_value.has_missing)&&
		(value_md->multidval.missing_value.has_missing)) {
		_Ncleq(target_md->multidval.type,(void*)&tmpe,(void*)&(target_md->multidval.missing_value.value),(void*)&(value_md->multidval.missing_value.value),NULL,NULL,1,1);
		if(tmpe) {
/*
* No need to check when missing values are equal
*/
			chckmiss = 0;
		} else {
			chckmiss = 1;
		}
	} else {
		chckmiss = 0;
	}
	el_size = target_md->multidval.type->type_class.size;
	
	if(sel != NULL) {
		sel_ptr = sel->selection;
	} else {
		if(target_md->multidval.totalsize == value_md->multidval.totalsize) {
			memcpy(target_md->multidval.val,value_md->multidval.val,value_md->multidval.totalsize);
			if(chckmiss) {
				_Nclreset_mis(target_md->multidval.type,target_md->multidval.val,&value_md->multidval.missing_value.value,&target_md->multidval.missing_value.value,target_md->multidval.totalelements);
			} 
			return(NhlNOERROR);
		} else {
			return(NhlFATAL);
		}
	}
        for(i = 0; i < value_md->multidval.n_dims; i++) {
                if(value_md->multidval.dim_sizes[i] > 1) {
                        dim_sizes_value[n_dims_value] = value_md->multidval.dim_sizes[i];
                        n_dims_value++;
                }
        }

	for(i = 0 ; i < n_dims_target; i++) {
		switch(sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			sel_ptr->u.sub.finish = (long)target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
* Note all cases fall through to here (no break statement)
*/
			sel_ptr->sel_type = Ncl_SUBSCR;
		case Ncl_SUBSCR:
			if(sel_ptr->u.sub.finish < sel_ptr->u.sub.start) {

				if(sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be a positive non-zero integer ");
					sel_ptr->u.sub.stride = 1;

				}

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;
				

				 if(sel_ptr->u.sub.stride < 0){
                                        current_index[i] = sel_ptr->u.sub.finish + (sel_ptr->u.sub.start - sel_ptr->u.sub.finish) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        compare_sel[i] = -2;
                                        strider[i] = -(sel_ptr->u.sub.stride);
					sel_ptr->u.sub.stride = strider[i];
                                } else {
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = -(sel_ptr->u.sub.stride);
                                }


/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/

			} else {
				if(sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be a positive non-zero integer ");
					sel_ptr->u.sub.stride = 1;

				}

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;

				if(sel_ptr->u.sub.stride < 0){
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.finish - (sel_ptr->u.sub.finish - sel_ptr->u.sub.start) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        strider[i] = sel_ptr->u.sub.stride;
					sel_ptr->u.sub.stride = -strider[i];

                                } else {
                                        compare_sel[i] = -2;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = sel_ptr->u.sub.stride;
                                }


			}
			if((sel_ptr->u.sub.start > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((sel_ptr->u.vec.min < 0)|| (sel_ptr->u.vec.min > target_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			if((sel_ptr->u.vec.max < 0)|| (sel_ptr->u.vec.max > target_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			n_elem = sel_ptr->u.vec.n_ind;
			strider[i] = 0;
			current_index[i] = sel_ptr->u.vec.ind[0]; 
			compare_sel[i] = 0;
		} 
/*
* This check should really be done from above before calling this function
* But I put it here just in case.
*/
		if(sel_ptr->dim_num != n_dims_target -1) {
			multiplier[i] = 1;

			for(k = sel_ptr->dim_num + 1; k < n_dims_target;k++) {
				multiplier[i] *= (long)target_md->multidval.dim_sizes[k];
			}
		} else {
			multiplier[i] = 1;
		}
		output_dim_sizes[i] = n_elem;
		total_elements = total_elements * n_elem;
		sel_ptr++;
		if(n_elem != 1) {
			if(n_elem != dim_sizes_value[n_dims_sel]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension size mismatch on subscript #%d, left-hand and right-hand side dimensions do not match",i);
				return(NhlFATAL);
			}
			n_dims_sel++;
		}
	}
	if(total_elements != value_md->multidval.totalelements) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension sizes on right hand side of assignment do not match dimension sizes of left hand side");
		return(NhlFATAL);
	}
	if(n_dims_sel != n_dims_value) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Left-hand side and right-hand side of assignment do not have the same number of dimensions");
		return(NhlFATAL);
		
	}
	sel_ptr = sel->selection;
/*
* all dimsizes between value and selection target match. 
* all dimsizes are in valid ranges of target dimensions.
* All subscript ranges are valid.vector subscript is present is known.
* also multiplier contains the appropriate multiplier by which to multiply
* an index to get the actual index in the target vector.
* the output arrays dimension sizes are known and the total number of 
* elements is known 
* the current_index array is inialized to the starting location
*
* compare_sel contains either -2 or -1 for normal subscripts or the
* index >= 0 into the integer vector array.
*/

/*
 * Now determine the amount of continguous data that may be copied in a single 
 * call to memcpy
 */
	for (i = n_dims_target - 1; i >= 0; i--) {
		if (last || compare_sel[i] != -2 || strider[i] != 1 || (i != sel_ptr[i].dim_num))
			break;
		if (output_dim_sizes[i] < target_md->multidval.dim_sizes[sel_ptr[i].dim_num])
			last = 1;
		cpy_count *= output_dim_sizes[i];
	}

	val = value_md->multidval.val;
	from = 0;
	while(!done) {
		rem_count = cpy_count;
		to = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (current_index[i] * multiplier[i]);
		}
		memcpy((void*)((char*)target_md->multidval.val + (to * el_size)),
		       (void*)((char*)val + (from * el_size)),el_size * cpy_count);
		if (rem_count > 1) {
			current_index[n_dims_target -1 ] += output_dim_sizes[n_dims_target -1];
			rem_count /= output_dim_sizes[n_dims_target -1];
		}
		else if(compare_sel[n_dims_target-1] <0) {
			current_index[n_dims_target -1 ] += strider[n_dims_target-1];
		} 
		else {
			compare_sel[n_dims_target-1]++;
		}
		for(k = n_dims_target-1; k >0; k--) {
			switch(compare_sel[k]) {
			case -2: 
				if(current_index[k] > sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if (rem_count > 1) {
						current_index[k -1 ] += output_dim_sizes[k -1];
						rem_count /= output_dim_sizes[k -1];
					}
					else if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} 
					else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			case -1:
				if(current_index[k] < sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			default:
/*
* Only falls through to here when vector is present because compare_sel
* is only positive when a vector is present
*/
				if(compare_sel[k] >= sel_ptr[k].u.vec.n_ind) {
					compare_sel[k] = 0;
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}

				} else {
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					inc_done = 1;
				}
				break;
			}

			if(inc_done) {
				inc_done = 0;
				break;
			}
		}
		switch(compare_sel[0]) {
		case -2:
			if(current_index[0] > sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		case -1:
			if(current_index[0] < sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		default:
			if(compare_sel[0] >= sel_ptr[0].u.vec.n_ind) {
					done = 1;
			} else {
				current_index[0] = sel_ptr[0].u.vec.ind[compare_sel[0]];
			}
			break;
		}
		from += cpy_count;
	}

	if(chckmiss) {
		_Nclreset_mis(target_md->multidval.type,target_md->multidval.val,&value_md->multidval.missing_value.value,&target_md->multidval.missing_value.value,target_md->multidval.totalelements);
	} else if(value_md->multidval.missing_value.has_missing && !chckmiss) {
		target_md->multidval.missing_value = value_md->multidval.missing_value;
	}
	return(NhlNOERROR);
}
static NhlErrorTypes MultiDVal_s_WriteSection
#if	NhlNeedProto
(NclData target, NclSelectionRecord * sel, struct _NclDataRec* value)
#else
(target,sel,value)
	NclData target;
	NclSelectionRecord *sel;
	NclData value;
#endif
{
	NclMultiDValData target_md = (NclMultiDValData)target;
	NclMultiDValData value_md = (NclMultiDValData)value;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/
	NclSelection *sel_ptr = NULL;
	void *val;
	ng_size_t i,k,to;

	ng_size_t current_index[NCL_MAX_DIMENSIONS];
	ng_size_t multiplier[NCL_MAX_DIMENSIONS];
	ng_size_t compare_sel[NCL_MAX_DIMENSIONS];
	ng_size_t strider[NCL_MAX_DIMENSIONS];
	ng_size_t output_dim_sizes[NCL_MAX_DIMENSIONS];

	ng_size_t total_elements = 1;
	int n_dims_target = target_md->multidval.n_dims;
	long long n_elem=0;
	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	ng_size_t el_size;
	logical tmpe;
	ng_size_t rem_count;
	ng_size_t cpy_count = 1;
	int last = 0;
	int first = 1;
	char *savep;

/*
* preconditions:
*	target and value are same type of object
*	sel is a valid selection record with no duplicate indices and
* 	contains a selection entry for each dimension in target.
*
* 	*****Value is a SCALAR array***** 
*	number of dimensions == 1 && size == 1
*/
	if((target_md->multidval.missing_value.has_missing)&&
		(value_md->multidval.missing_value.has_missing)) {
		_Ncleq(target_md->multidval.type,(void*)&tmpe,(void*)&(target_md->multidval.missing_value.value),(void*)&(value_md->multidval.missing_value.value),NULL,NULL,1,1);
		if(tmpe) {
/*
* No need to check when missing values are equal
*/
			chckmiss = 0;
		} else {
			chckmiss = 1;
		}
	} else {
		chckmiss = 0;
	}
	el_size = target_md->multidval.type->type_class.size;
	if(sel != NULL) {
		sel_ptr = sel->selection;
	} else {
		/*
		 * DIB -- this seems wrong: if the value_md has only one element it can't possible work.
		 * The totalsizes will not in general be equal, so it will return a FATAL.
		 * I suppose this branch never gets taken.
		 */
		if(target_md->multidval.totalsize == value_md->multidval.totalsize) {
			memcpy(target_md->multidval.val,value_md->multidval.val,value_md->multidval.totalsize);
                        if(chckmiss) {
                                _Nclreset_mis(target_md->multidval.type,target_md->multidval.val,&value_md->multidval.missing_value.value,&target_md->multidval.missing_value.value,target_md->multidval.totalelements);
                        }
                        return(NhlNOERROR);
                } else {
			return(NhlFATAL);
                }
        }


	for(i = 0 ; i < n_dims_target; i++) {
		switch(sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			sel_ptr->u.sub.finish = target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
* Note all cases fall through to here (no break statement)
*/
			sel_ptr->sel_type = Ncl_SUBSCR;
		case Ncl_SUBSCR:
			if(sel_ptr->u.sub.finish < sel_ptr->u.sub.start) {

				if(sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
					sel_ptr->u.sub.stride = 1;
				}

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				if(sel_ptr->u.sub.stride < 0){
                                        current_index[i] = sel_ptr->u.sub.finish + (sel_ptr->u.sub.start - sel_ptr->u.sub.finish) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        compare_sel[i] = -2;
                                        strider[i] = -(sel_ptr->u.sub.stride);
					sel_ptr->u.sub.stride = strider[i];
                                } else {
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = -(sel_ptr->u.sub.stride);
                                }
			} else {
				if(sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
					sel_ptr->u.sub.stride = 1;
				}

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;

				if(sel_ptr->u.sub.stride < 0){
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.finish - (sel_ptr->u.sub.finish - sel_ptr->u.sub.start) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        strider[i] = sel_ptr->u.sub.stride;
					sel_ptr->u.sub.stride = -strider[i];

                                } else {
                                        compare_sel[i] = -2;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = sel_ptr->u.sub.stride;
                                }


			}
			if((sel_ptr->u.sub.start > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((sel_ptr->u.vec.min < 0)|| (sel_ptr->u.vec.min > target_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			if((sel_ptr->u.vec.max < 0)|| (sel_ptr->u.vec.max > target_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			n_elem = sel_ptr->u.vec.n_ind;
			strider[i] = 0;
			current_index[i] = sel_ptr->u.vec.ind[0]; 
			compare_sel[i] = 0;
		} 
/*
* This check should really be done from above before calling this function
* But I put it here just in case.
*/
		if(sel_ptr->dim_num != n_dims_target -1) {
			multiplier[i] = 1;

			for(k = sel_ptr->dim_num + 1; k < n_dims_target;k++) {
				multiplier[i] *= target_md->multidval.dim_sizes[k];
			}
		} else {
			multiplier[i] = 1;
		}
		output_dim_sizes[i] = n_elem;
		total_elements = total_elements * n_elem;
		sel_ptr++;
	}
	sel_ptr = sel->selection;
/*
* all dimsizes are in valid ranges of target dimensions.
* All subscript ranges are valid. if vector subscript is present is known.
* also multiplier contains the appropriate multiplier by which to multiply
* an index to get the actual index in the target vector.
* the output arrays dimension sizes are known and the total number of 
* elements is known 
* the current_index array is inialized to the starting location
*
* compare_sel contains either -2 or -1 for normal subscripts or the
* index >= 0 into the integer vector array.
*/

	val = value_md->multidval.val;
	tmpe = 0;
	if(value_md->multidval.missing_value.has_missing)
		_Ncleq(value_md->multidval.type,(void*)&tmpe,(void*)&(value_md->multidval.missing_value.value),val,NULL,NULL,1,1);
/*
* chckmis is true only if *both* md's contain missing values
*/
	if((tmpe) &&(chckmiss)) {
/*
* case where val is missing value and target has missing values also
*/
		val = (void*)&(target_md->multidval.missing_value.value);
	} else if(tmpe){
/*
* case where val is missing value and target has none val is already 
* equal to missing value but target needs to be changed
*/
		target_md->multidval.missing_value.has_missing = 1;
		target_md->multidval.missing_value.value = value_md->multidval.missing_value.value;
		
	}
	for (i = n_dims_target - 1; i >= 0; i--) {
		if (last || compare_sel[i] != -2 || strider[i] != 1 || (i != sel_ptr[i].dim_num))
			break;
		if (output_dim_sizes[i] < target_md->multidval.dim_sizes[sel_ptr[i].dim_num])
			last = 1;
		cpy_count *= output_dim_sizes[i];
	}
	while(!done) {
		char *tp;
		rem_count = cpy_count;
		to = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (current_index[i] * multiplier[i]);
		}
		tp = ((char*)target_md->multidval.val + (to * el_size));
		/*
		 * Propagate the scalar value to contiguous elements of the array
		 */
		if (first) {
			i = 1;
			memcpy(tp,val,el_size);
			while (i <= cpy_count / 2) {
				memcpy(tp+i*el_size,tp,el_size * i);
				i *= 2;
			}
			if (cpy_count - i > 0) {
				memcpy(tp+i*el_size,tp,el_size * (cpy_count - i));
			}
			savep = tp;
			first = 0;
		}
		else {
			memcpy(tp,savep,el_size * cpy_count);
		}

		if (rem_count > 1) {
			current_index[n_dims_target -1 ] += output_dim_sizes[n_dims_target -1];
			rem_count /= output_dim_sizes[n_dims_target -1];
		}
		else if (compare_sel[n_dims_target-1] <0) {
			current_index[n_dims_target -1 ] += strider[n_dims_target-1];
		} 
		else {
			compare_sel[n_dims_target-1]++;
		}
		for(k = n_dims_target-1; k >0; k--) {
			switch(compare_sel[k]) {
			case -2: 
				if(current_index[k] > sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if (rem_count > 1) {
						current_index[k -1 ] += output_dim_sizes[k -1];
						rem_count /= output_dim_sizes[k -1];
					}
					else if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			case -1:
				if(current_index[k] < sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			default:
/*
* Only falls through to here when vector is present because compare_sel
* is only positive when a vector is present
*/
				if(compare_sel[k] >= sel_ptr[k].u.vec.n_ind) {
					compare_sel[k] = 0;
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}

				} else {
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					inc_done = 1;
				}
				break;
			}

			if(inc_done) {
				inc_done = 0;
				break;
			}
		}
		switch(compare_sel[0]) {
		case -2:
			if(current_index[0] > sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		case -1:
			if(current_index[0] < sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		default:
			if(compare_sel[0] >= sel_ptr[0].u.vec.n_ind) {
					done = 1;
			} else {
				current_index[0] = sel_ptr[0].u.vec.ind[compare_sel[0]];
			}
			break;
		}
	}
	return(NhlNOERROR);
}




static NhlErrorTypes MultiDVal_ReadWriteSection
#if	NhlNeedProto
(NclData to_data, NclSelectionRecord * to_selection, NclData from_data, NclSelectionRecord *from_selection)
#else
(to_data,to_selection,from_data,from_selection)
NclData to_data;
NclSelectionRecord * to_selection;
NclData from_data;
NclSelectionRecord *from_selection;
#endif
{
	NclMultiDValData target_md = (NclMultiDValData)to_data;
	NclMultiDValData value_md = (NclMultiDValData)from_data;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/

	ng_size_t i,j,k;
	ng_size_t from,to;
	NclSelection *to_sel_ptr = NULL;
	void *to_val;
	NclSelection *from_sel_ptr = NULL;
	void *from_val;

	ng_size_t to_current_index[NCL_MAX_DIMENSIONS];
	ng_size_t to_multiplier[NCL_MAX_DIMENSIONS];
	ng_size_t to_compare_sel[NCL_MAX_DIMENSIONS];
	ng_size_t to_strider[NCL_MAX_DIMENSIONS];
	ng_size_t to_output_dim_sizes[NCL_MAX_DIMENSIONS];
	ng_size_t from_current_index[NCL_MAX_DIMENSIONS];
	ng_size_t from_multiplier[NCL_MAX_DIMENSIONS];
	ng_size_t from_compare_sel[NCL_MAX_DIMENSIONS];
	ng_size_t from_strider[NCL_MAX_DIMENSIONS];
	ng_size_t from_output_dim_sizes[NCL_MAX_DIMENSIONS];

	int n_dims_value = 0;
	ng_size_t total_elements_value = 1;
	ng_size_t total_elements_target = 1;
	int n_dims_target = 0;
	int n_elem_target=0;
	int n_elem_value=0;

	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	ng_size_t el_size;
	logical tmpe =0 ;
	ng_size_t to_rem_count,from_rem_count;
	ng_size_t from_cpy_count = 1,cpy_count = 1;
	int last = 0;
	ng_size_t from_dim_count, to_dim_count;
	ng_size_t from_left_dim, to_left_dim, jstart;
	int scalar;
	
	for(i = 0; i < NCL_MAX_DIMENSIONS; i++) {
		to_current_index[i] = 0;
		to_multiplier[i] = 0;
		to_compare_sel[i] = 0;
		to_strider[i]=0;
		to_output_dim_sizes[i]=0;
		from_current_index[i]=0;
		from_multiplier[i]=0;
		from_compare_sel[i]=0;
		from_strider[i]=0;
		from_output_dim_sizes[i]=0;
	}

	if((target_md == NULL)||(value_md == NULL) ) {
		return(NhlFATAL);
	}

	n_dims_value = value_md->multidval.n_dims;
	n_dims_target = target_md->multidval.n_dims;
	
	

/*
* preconditions:
*	target and value are same type of object
*	sel is a valid selection record with no duplicate indices and
* 	contains a selection entry for each dimension in target or it
* 	is null implying a direct copy.
*
* 	*****Value is a multidimensional array***** 
*	number of dimensions >= 1 && size > 1
*/
	if((target_md->multidval.missing_value.has_missing)&&
		(value_md->multidval.missing_value.has_missing)) {
		_Ncleq(target_md->multidval.type,(void*)&tmpe,(void*)&(target_md->multidval.missing_value.value),(void*)&(value_md->multidval.missing_value.value),NULL,NULL,1,1);
		if(tmpe) {
/*
* No need to check when missing values are equal
*/
			chckmiss = 0;
		} else {
			chckmiss = 1;
		}
	} else {
		chckmiss = 0;
	}
	el_size = target_md->multidval.type->type_class.size;

	
	if(to_selection != NULL) {
		to_sel_ptr = to_selection->selection;
	} 
	if(from_selection != NULL) {
		from_sel_ptr = from_selection->selection;
	} 
	for(i = 0 ; i < n_dims_target; i++) {
		switch(to_sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			to_sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			to_sel_ptr->u.sub.finish = (long)target_md->multidval.dim_sizes[to_sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(to_sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				to_sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
* Note all cases fall through to here (no break statement)
*/
			to_sel_ptr->sel_type = Ncl_SUBSCR;
		case Ncl_SUBSCR:
			if(to_sel_ptr->u.sub.finish < to_sel_ptr->u.sub.start) {

				if(to_sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be a positive non-zero integer");
					to_sel_ptr->u.sub.stride = 1;
				}

				n_elem_target = (ng_size_t) labs((to_sel_ptr->u.sub.start - to_sel_ptr->u.sub.finish)
					 	          / to_sel_ptr->u.sub.stride) + 1L;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				if(to_sel_ptr->u.sub.stride < 0){
                                        to_current_index[i] = to_sel_ptr->u.sub.finish + (to_sel_ptr->u.sub.start - to_sel_ptr->u.sub.finish) % abs(to_sel_ptr->u.sub.stride);
                                        to_sel_ptr->u.sub.finish = to_sel_ptr->u.sub.start;
                                        to_sel_ptr->u.sub.start = to_current_index[i];
                                        to_compare_sel[i] = -2;
                                        to_strider[i] = -(to_sel_ptr->u.sub.stride);
					to_sel_ptr->u.sub.stride = to_strider[i];
                                } else {
                                        to_compare_sel[i] = -1;
                                        to_current_index[i] = to_sel_ptr->u.sub.start;
                                        to_strider[i] = -(to_sel_ptr->u.sub.stride);
                                }


			} else {
				if(to_sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be a positive non-zero integer");
					to_sel_ptr->u.sub.stride = 1;

				}

				n_elem_target = (ng_size_t) labs((to_sel_ptr->u.sub.start - to_sel_ptr->u.sub.finish)
					 	          / to_sel_ptr->u.sub.stride) + 1L;

				if(to_sel_ptr->u.sub.stride < 0){
                                        to_compare_sel[i] = -1;
                                        to_current_index[i] = to_sel_ptr->u.sub.finish - (to_sel_ptr->u.sub.finish - to_sel_ptr->u.sub.start) % abs(to_sel_ptr->u.sub.stride);
                                        to_sel_ptr->u.sub.finish = to_sel_ptr->u.sub.start;
                                        to_sel_ptr->u.sub.start = to_current_index[i];
                                        to_strider[i] = to_sel_ptr->u.sub.stride;
					to_sel_ptr->u.sub.stride = -to_strider[i];

                                } else {
                                        to_compare_sel[i] = -2;
                                        to_current_index[i] = to_sel_ptr->u.sub.start;
                                        to_strider[i] = to_sel_ptr->u.sub.stride;
                                }

				
			}
			if((to_sel_ptr->u.sub.start > target_md->multidval.dim_sizes[to_sel_ptr->dim_num] - 1)||(to_sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((to_sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[to_sel_ptr->dim_num] - 1)||(to_sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((to_sel_ptr->u.vec.min < 0)|| (to_sel_ptr->u.vec.min > target_md->multidval.dim_sizes[to_sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			if((to_sel_ptr->u.vec.max < 0)|| (to_sel_ptr->u.vec.max > target_md->multidval.dim_sizes[to_sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			n_elem_target = to_sel_ptr->u.vec.n_ind;
			to_strider[i] = 0;
			to_current_index[i] = to_sel_ptr->u.vec.ind[0]; 
			to_compare_sel[i] = 0;
		} 
/*
* This check should really be done from above before calling this function
* But I put it here just in case.
*/
		if(to_sel_ptr->dim_num != n_dims_target -1) {
			to_multiplier[i] = 1;

			for(k = to_sel_ptr->dim_num + 1; k < n_dims_target;k++) {
				to_multiplier[i] *= (long)target_md->multidval.dim_sizes[k];
			}
		} else {
			to_multiplier[i] = 1;
		}
		to_output_dim_sizes[i] = n_elem_target < 1 ? 1 : n_elem_target;
		total_elements_target = total_elements_target * n_elem_target;
		to_sel_ptr++;
	}

	for(i = 0 ; i < n_dims_value; i++) {
		switch(from_sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			from_sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			from_sel_ptr->u.sub.finish = (long)value_md->multidval.dim_sizes[from_sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(from_sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				from_sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
* Note all cases fall through to here (no break statement)
*/
			from_sel_ptr->sel_type = Ncl_SUBSCR;
		case Ncl_SUBSCR:
			if(from_sel_ptr->u.sub.finish < from_sel_ptr->u.sub.start) {

				if(from_sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be a positive non-zero integer");
					from_sel_ptr->u.sub.stride = 1;

				}

				n_elem_value = (ng_size_t) labs((from_sel_ptr->u.sub.start - from_sel_ptr->u.sub.finish)
					 	          / from_sel_ptr->u.sub.stride) + 1L;

/*
* Need from be able from determine which type of comparision < or > is needed from
* determine whether the finish has been passed up
*/
				if(from_sel_ptr->u.sub.stride < 0){
                                        from_current_index[i] = from_sel_ptr->u.sub.finish + (from_sel_ptr->u.sub.start - from_sel_ptr->u.sub.finish) % abs(from_sel_ptr->u.sub.stride);
                                        from_sel_ptr->u.sub.finish = from_sel_ptr->u.sub.start;
                                        from_sel_ptr->u.sub.start = from_current_index[i];
                                        from_compare_sel[i] = -2;
                                        from_strider[i] = -(from_sel_ptr->u.sub.stride);
					from_sel_ptr->u.sub.stride = from_strider[i];
                                } else {
                                        from_compare_sel[i] = -1;
                                        from_current_index[i] = from_sel_ptr->u.sub.start;
                                        from_strider[i] = -(from_sel_ptr->u.sub.stride);
                                }


			} else {
				if(from_sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be a positive non-zero integer");
					from_sel_ptr->u.sub.stride = 1;
				}

				n_elem_value = (ng_size_t) labs((from_sel_ptr->u.sub.start - from_sel_ptr->u.sub.finish)
					 	          / from_sel_ptr->u.sub.stride) + 1L;
				
				if(from_sel_ptr->u.sub.stride < 0){
                                        from_compare_sel[i] = -1;
                                        from_current_index[i] = from_sel_ptr->u.sub.finish - (from_sel_ptr->u.sub.finish - from_sel_ptr->u.sub.start) % abs(from_sel_ptr->u.sub.stride);
                                        from_sel_ptr->u.sub.finish = from_sel_ptr->u.sub.start;
                                        from_sel_ptr->u.sub.start = from_current_index[i];
                                        from_strider[i] = from_sel_ptr->u.sub.stride;
					from_sel_ptr->u.sub.stride = -from_strider[i];
                                } else {
                                        from_compare_sel[i] = -2;
                                        from_current_index[i] = from_sel_ptr->u.sub.start;
                                        from_strider[i] = from_sel_ptr->u.sub.stride;
                                }

			}
			if((from_sel_ptr->u.sub.start > value_md->multidval.dim_sizes[from_sel_ptr->dim_num] - 1)||(from_sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((from_sel_ptr->u.sub.finish > value_md->multidval.dim_sizes[from_sel_ptr->dim_num] - 1)||(from_sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((from_sel_ptr->u.vec.min < 0)|| (from_sel_ptr->u.vec.min > value_md->multidval.dim_sizes[from_sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			if((from_sel_ptr->u.vec.max < 0)|| (from_sel_ptr->u.vec.max > value_md->multidval.dim_sizes[from_sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			n_elem_value = from_sel_ptr->u.vec.n_ind;
			from_strider[i] = 0;
			from_current_index[i] = from_sel_ptr->u.vec.ind[0]; 
			from_compare_sel[i] = 0;
		} 
/*
* This check should really be done from above before calling this function
* But I put it here just in case.
*/
		if(from_sel_ptr->dim_num != n_dims_value -1) {
			from_multiplier[i] = 1;

			for(k = from_sel_ptr->dim_num + 1; k < n_dims_value;k++) {
				from_multiplier[i] *= (long)value_md->multidval.dim_sizes[k];
			}
		} else {
			from_multiplier[i] = 1;
		}
		from_output_dim_sizes[i] = n_elem_value < 1 ? 1 : n_elem_value;
		total_elements_value =total_elements_value * n_elem_value;
		from_sel_ptr++;
	}

	to_sel_ptr = to_selection->selection;
	from_sel_ptr = from_selection->selection;
	to_val = target_md->multidval.val;
	from_val = value_md->multidval.val;

/*
 * find the dim count after subscripting and the left most non-unitary dimension
 */


	from_dim_count = 0;
	from_left_dim = -1;
	for (i = 0; i < n_dims_value; i++) {
		if (from_output_dim_sizes[i] > 1) {
			if (from_left_dim == -1) {
				from_left_dim = i;
			}
			from_dim_count++;
		}
	}
	if (from_dim_count == 0) {
		from_dim_count = 1;
		from_left_dim = 0;
	}

	to_dim_count = 0;
	to_left_dim = -1;
	for (i = 0; i < n_dims_target; i++) {
		if (to_output_dim_sizes[i] > 1) {
			if (to_left_dim == -1) {
				to_left_dim = i;
			}
			to_dim_count++;
		}
	}
	if (to_dim_count == 0) {
		to_dim_count = 1;
		to_left_dim = 0;
	}
/*
 * if from_dim_count == 1 and 	from_output_dim_sizes[from_left_dim] == 1, it turns into a 
 * scalar assignment
 */
	scalar = from_dim_count == 1 && from_output_dim_sizes[from_left_dim] == 1;

/*
 * if not scalar then check that the number of dimensions after subscripting is equal,
 * and that the non-unitary dimensions match in number of elements proceeding left to right.
 */   

	if (! scalar) {	
		if(to_dim_count != from_dim_count) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Right hand side of assignment has (%d) dimensions and left hand side has (%d), dimension mismatch",from_dim_count,to_dim_count);
			return(NhlFATAL);
		}

		k = 0; /* counts dims after subscripting */
		jstart = from_left_dim;
		for (i = to_left_dim; i <  n_dims_target; i++) {
			if (to_output_dim_sizes[i] <= 1) /* should never be less than 1 */
				continue;
			for (j = jstart; j < n_dims_value; j++) {
				if (from_output_dim_sizes[j] <= 1)
					continue;
				if (to_output_dim_sizes[i] == from_output_dim_sizes[j]) {
					jstart = j+1;
					k++;
					break;
				}
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension size mismatch, dimension (%d) of left hand side reference does not have the same size as the right hand side reference after subscripting.",k);
				return(NhlFATAL);
			}
		}
	}

/*
 * Now determine the amount of continguous data that may be copied in a single 
 * call to memcpy
 */
	if (! chckmiss) {
		last = 0;
		for (i = n_dims_value - 1; i >= 0; i--) {
			if (last || from_compare_sel[i] != -2 || from_strider[i] != 1 || (i != from_sel_ptr[i].dim_num))
				break;
			if (from_output_dim_sizes[i] < value_md->multidval.dim_sizes[from_sel_ptr[i].dim_num])
				last = 1;
			from_cpy_count *= from_output_dim_sizes[i];
		}
		last = 0;
		for (i = n_dims_target - 1; i >= 0; i--) {
			if (last || to_compare_sel[i] != -2 || to_strider[i] != 1 || (i != to_sel_ptr[i].dim_num))
				break;
			if (to_output_dim_sizes[i] < target_md->multidval.dim_sizes[to_sel_ptr[i].dim_num])
				last = 1;
			cpy_count *= to_output_dim_sizes[i];
		}
		cpy_count = cpy_count > from_cpy_count ? from_cpy_count : cpy_count;
	}
/*
 * copy the data
 */

	while(!done) {
		to_rem_count = from_rem_count = cpy_count;
		to = 0;
		from = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (to_current_index[i] * to_multiplier[i]);
		}
		for(i = 0; i < n_dims_value;i++) {
			from = from + (from_current_index[i] * from_multiplier[i]);
		}
		if(chckmiss) {
			tmpe = 0;
			_Ncleq(target_md->multidval.type,(void*)&tmpe,(void*)((char*)from_val + (from * el_size)),(void*)&(value_md->multidval.missing_value.value),NULL,NULL,1,1);	
			if(tmpe) {
				memcpy((void*)((char*)to_val + (to*el_size)),(void*)&(target_md->multidval.missing_value.value),el_size);
			} else {
				memcpy((void*)((char*)to_val + (to*el_size)),(void*)((char*)from_val + (from * el_size)),el_size);
			}
		} else {
			memcpy((void*)((char*)to_val + (to*el_size)),(void*)((char*)from_val + (from * el_size)),el_size * cpy_count);
		}
		if (to_rem_count > 1) {
			to_current_index[n_dims_target -1 ] += to_output_dim_sizes[n_dims_target -1];
			to_rem_count /= to_output_dim_sizes[n_dims_target -1];
		}
		else if (to_compare_sel[n_dims_target-1] <0) {
			to_current_index[n_dims_target -1 ] += to_strider[n_dims_target-1];
		} else {
			to_compare_sel[n_dims_target-1]++;
		}
		if (! scalar) {
			if (from_rem_count > 1) {
				from_current_index[n_dims_value -1 ] += from_output_dim_sizes[n_dims_value -1];
				from_rem_count /= from_output_dim_sizes[n_dims_value -1];
			}
			else if (from_compare_sel[n_dims_value -1] <0) {
				from_current_index[n_dims_value -1 ] += from_strider[n_dims_value-1];
			} else {
				from_compare_sel[n_dims_value-1]++;
			}
		}
		for(k = n_dims_target-1; k >0; k--) {
			switch(to_compare_sel[k]) {
			case -2: 
				if(to_current_index[k] > to_sel_ptr[k].u.sub.finish) {
					to_current_index[k] = to_sel_ptr[k].u.sub.start;
					if (to_rem_count > 1) {
						to_current_index[k -1 ] += to_output_dim_sizes[k -1];
						to_rem_count /= to_output_dim_sizes[k -1];
					}
					else if(to_compare_sel[k-1] < 0) {
						to_current_index[k-1] += to_strider[k-1];
					} else {
						to_compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			case -1:
				if(to_current_index[k] < to_sel_ptr[k].u.sub.finish) {
					to_current_index[k] = to_sel_ptr[k].u.sub.start;
					if(to_compare_sel[k-1] < 0) {
						to_current_index[k-1] += to_strider[k-1];
					} else {
						to_compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			default:
				if(to_compare_sel[k] >= to_sel_ptr[k].u.vec.n_ind) {
					to_compare_sel[k] = 0;
					to_current_index[k] = to_sel_ptr[k].u.vec.ind[to_compare_sel[k]];
					if(to_compare_sel[k-1] < 0) {
						to_current_index[k-1] += to_strider[k-1];
					} else {
						to_compare_sel[k-1]++;
					}

				} else {
					to_current_index[k] = to_sel_ptr[k].u.vec.ind[to_compare_sel[k]];
					inc_done = 1;
				}
				break;
			}

			if(inc_done) {
				inc_done = 0;
				break;
			}
		}
		switch(to_compare_sel[0]) {
		case -2:
			if(to_current_index[0] > to_sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		case -1:
			if(to_current_index[0] < to_sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		default:
			if(to_compare_sel[0] >= to_sel_ptr[0].u.vec.n_ind) {
					done = 1;
			} else {
				to_current_index[0] = to_sel_ptr[0].u.vec.ind[to_compare_sel[0]];
			}
			break;
		}
		if (! scalar) {
			for(k = n_dims_value -1; k >0; k--) {
				switch(from_compare_sel[k]) {
				case -2: 
					if(from_current_index[k] > from_sel_ptr[k].u.sub.finish) {
						from_current_index[k] = from_sel_ptr[k].u.sub.start;
						if (from_rem_count > 1) {
							from_current_index[k -1 ] += from_output_dim_sizes[k -1];
							from_rem_count /= from_output_dim_sizes[k -1];
						}
						else if(from_compare_sel[k-1] < 0) {
							from_current_index[k-1] += from_strider[k-1];
						} else {
							from_compare_sel[k-1]++;
						}
					} else {
						inc_done = 1;
					}
					break;
				case -1:
					if(from_current_index[k] < from_sel_ptr[k].u.sub.finish) {
						from_current_index[k] = from_sel_ptr[k].u.sub.start;
						if(from_compare_sel[k-1] < 0) {
							from_current_index[k-1] += from_strider[k-1];
						} else {
							from_compare_sel[k-1]++;
						}
					} else {
						inc_done = 1;
					}
					break;
				default:
					if(from_compare_sel[k] >= from_sel_ptr[k].u.vec.n_ind) {
						from_compare_sel[k] = 0;
						from_current_index[k] = from_sel_ptr[k].u.vec.ind[from_compare_sel[k]];
						if(from_compare_sel[k-1] < 0) {
							from_current_index[k-1] += from_strider[k-1];
						} else {
							from_compare_sel[k-1]++;
						}
	
					} else {
						from_current_index[k] = from_sel_ptr[k].u.vec.ind[from_compare_sel[k]];
						inc_done = 1;
					}
					break;
				}
	
				if(inc_done) {
					inc_done = 0;
					break;
				}
			}
			switch(from_compare_sel[0]) {
			case -2:
				if(from_current_index[0] > from_sel_ptr[0].u.sub.finish)
						done = 1;
				break;
			case -1:
				if(from_current_index[0] < from_sel_ptr[0].u.sub.finish)
						done = 1;
				break;
			default:
				if(from_compare_sel[0] >= from_sel_ptr[0].u.vec.n_ind) {
						done = 1;
				} else {
					from_current_index[0] = from_sel_ptr[0].u.vec.ind[from_compare_sel[0]];
				}
				break;
			}
		}
	}
	if(value_md->multidval.missing_value.has_missing&&!chckmiss) {
/*
* This condition is when value has missing value that could've been
* assigned and target doesn't have a missing_value record.
*/
		target_md->multidval.missing_value.has_missing = 1;
		target_md->multidval.missing_value.value = value_md->multidval.missing_value.value;
	}
	return(NhlNOERROR);
}

static NhlErrorTypes MultiDValAddParent
#if	NhlNeedProto
(NclObj theobj, NclObj parent)
#else 
(theobj, parent)
NclObj theobj;
NclObj parent;
#endif
{
	NclRefList * tmp = NULL;

	tmp = theobj->obj.parents;
	theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
	theobj->obj.parents->next = tmp;
	theobj->obj.parents->pid= parent->obj.id;
	theobj->obj.ref_count++;
	return(NhlNOERROR);
}

static NhlErrorTypes MultiDValDelParent
#if	NhlNeedProto
(NclObj theobj, NclObj parent)
#else 
(theobj, parent)
NclObj theobj;
NclObj parent;
#endif
{
	NclRefList *tmp,*tmp1;
	int found = 0;

	if(theobj->obj.parents == NULL) {
		if(0 == strcmp("MultiDVallistData", parent->obj.class_ptr->obj_class.class_name))
		{
			return(NhlNOERROR);
		}
		else
		{
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"MultiDValDelParent: Attempt to delete parent from empty list"));
			return(NhlFATAL);
		}
	} 

	tmp = theobj->obj.parents;	
	while((tmp!=NULL)&&(tmp->pid == parent->obj.id)) {
		theobj->obj.parents = theobj->obj.parents->next;
		NclFree(tmp);
		tmp = theobj->obj.parents;
		found = 1;
		theobj->obj.ref_count--;
	}
	if((tmp == NULL)&&(found)) {
		_NclDestroyObj(theobj);
		return(NhlNOERROR);
	}
	while(tmp->next != NULL) {
		if(tmp->next->pid == parent->obj.id) {
			found = 1;
			tmp1 = tmp->next;
			tmp->next = tmp->next->next;
			NclFree(tmp1);
			theobj->obj.ref_count--;
		} else {
			tmp = tmp->next;
		}
	}
	if(found) {
		if(theobj->obj.ref_count <= 0) 
			_NclDestroyObj(theobj);
		return(NhlNOERROR);
	} else {
		return(NhlWARNING);
	}
}

static NclData MultiDValDup
#if	NhlNeedProto
(NclData self,NclScalar *new_missing)
#else
(self,new_missing)
NclData self;
NclScalar *new_missing;
#endif
{
	void *new_val;
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData output_md = NULL;

	new_val = (void*)NclMalloc(self_md->multidval.totalsize);
	memcpy(new_val,self_md->multidval.val,self_md->multidval.totalsize);

	output_md = _NclCreateVal(
		NULL,
		NULL,
		self_md->obj.obj_type,
		self_md->obj.obj_type_mask,
		new_val,
		((self_md->multidval.missing_value.has_missing)? ((new_missing != NULL) ? new_missing : &(self_md->multidval.missing_value.value)) : NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,
		NULL,
		(NclObjClass)self_md->multidval.type
	);	
	if(output_md != NULL) {
		if((new_missing != NULL)&&(self_md->multidval.missing_value.has_missing)) {
			_Nclreset_mis(output_md->multidval.type,output_md->multidval.val,&(self_md->multidval.missing_value.value),new_missing,self_md->multidval.totalelements);
			output_md->multidval.missing_value.has_missing = 1;
			output_md->multidval.missing_value.value = *new_missing;
		} 	
	}
	return((NclData)output_md);
}

NclData MultiDVal_md_Not
#if	NhlNeedProto
(NclData self,NclData result)
#else
(self,result)
NclData self;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclMissingRec themissing;
	NclMultiDValData output_md = NULL;
	NclTypeClass the_type;
	void *result_val;

	if(self_md == NULL) return(NULL);

	if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }	
	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.ncl_not != NULL)) {

/*
* the_type is not null since requirement if not != NULL not_type != NULL
*/

                the_type = _Nclnot_type(self_md->multidval.type);
		if((the_type->type_class.data_type == NCL_logical)&&(themissing.has_missing)) {
			themissing.value = the_type->type_class.default_mis;
		}
                result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
                if(result_val == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Not: Could not allocate memory for result type, can't continue\n");
                        return(NULL);
                }
                if(_Nclncl_not(
                        self_md->multidval.type,
                        result_val,
                        self_md->multidval.val,
                        (self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
                        self_md->multidval.totalelements) != NhlFATAL) {

                        output_md = _NclCreateMultiDVal(
                                (NclObj)result_md,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                result_val,
                                (themissing.has_missing? &(themissing.value):NULL),
                                self_md->multidval.n_dims,
                                self_md->multidval.dim_sizes,
                                TEMPORARY,
                                NULL,
                                the_type
                        );
                        return((NclData)output_md);
                } else {
                        NclFree(result_val);
                        return(NULL);
                }
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Not: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}
static NclData MultiDVal_md_Neg
#if	NhlNeedProto
(NclData self,NclData result)
#else
(self,result)
NclData self;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclMissingRec themissing;
	NclMultiDValData output_md = NULL;
	NclTypeClass the_type;
	void *result_val;

	if(self_md == NULL) return(NULL);

	if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }	

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.neg != NULL)) {

/*
* the_type is not null since requirement if neg != NULL neg_type != NULL
*/

                the_type = _Nclneg_type(self_md->multidval.type);
                result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
                if(result_val == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Neg: Could not allocate memory for result type, can't continue\n");
                        return(NULL);
                }
                if(_Nclneg(
                        self_md->multidval.type,
                        result_val,
                        self_md->multidval.val,
                        (self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
                        self_md->multidval.totalelements) != NhlFATAL) {

                        output_md = _NclCreateMultiDVal(
                                (NclObj)result_md,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                result_val,
                                (themissing.has_missing? &(themissing.value):NULL),
                                self_md->multidval.n_dims,
                                self_md->multidval.dim_sizes,
                                TEMPORARY,
                                NULL,
                                the_type
                        );
                        return((NclData)output_md);
                } else {
                        NclFree(result_val);
                        return(NULL);
                }
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Neg: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static int MultiDVal_IsMis
#if	NhlNeedProto
(NclData self, void* v_one)
#else
(self,v_one)
NclData self_md;
void *v_one;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	logical res;

	if((self_md != NULL)&&(self_md->multidval.type != NULL)&&(self_md->multidval.missing_value.has_missing)) {
		if(_Ncleq(self_md->multidval.type,(void*)&res,(void*)&self_md->multidval.missing_value.value,v_one,NULL,NULL,1,1) != NhlFATAL) {
			return((int)res);
		}
	}
	return(0);
}

static NclData MultiDVal_md_Coerce
#if	NhlNeedProto
(NclData self, NclObjTypes coerce_to_obj, NclScalar *new_missing)
#else
(self,coerce_to_obj,new_missing)
NclData self;
NclObjTypes coerce_to_obj;
NclScalar *new_missing;
#endif
{
	NclTypeClass to_type;
	NclTypeClass from_type;
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData output_md = NULL;
	void *result_val = NULL;
	int limit = 1,from = 0;
	int n_dims = 0;
	int step = 0;
	ng_size_t i;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclScalar tmp_missing;

	if((self_md != NULL)&&(self_md->multidval.type != NULL)) {
		from_type = self_md->multidval.type;
		to_type = _NclTypeEnumToTypeClass(coerce_to_obj);
		if(self_md->multidval.type == to_type) {
			if((self_md->multidval.missing_value.has_missing)&&(new_missing != NULL)) 
				_NclResetMissingValue(self_md,new_missing);
			return(self);
		} else {
			if(to_type != (NclTypeClass)nclTypeClass) {
				if((from_type == (NclTypeClass)nclTypecharClass)&&(to_type == (NclTypeClass)nclTypestringClass)){
					for(i = 0; i < self_md->multidval.n_dims - 1; i++) {
						limit *= self_md->multidval.dim_sizes[i];
						dimsizes[i] = self_md->multidval.dim_sizes[i];
					}
					n_dims = self_md->multidval.n_dims - 1;
					if(n_dims == 0) {
						n_dims = 1;
						dimsizes[i] = 1;
					}
					step = self_md->multidval.dim_sizes[self_md->multidval.n_dims - 1];
					result_val = (void*)NclMalloc(to_type->type_class.size*limit);
					if(result_val != NULL) {
						for(i = 0; i < limit; i++) {
							if(_Nclcoerce(
								to_type,
								(void*)&((NclQuark*)result_val)[i],
								(void*)&((char*)self_md->multidval.val)[from],
								self_md->multidval.totalelements/limit,
								(self_md->multidval.missing_value.has_missing?&self_md->multidval.missing_value.value:NULL),
								new_missing,
								from_type) == NhlFATAL) {
								
								NclFree(result_val);
							}
							from += step;
						}
						output_md = _NclCreateVal(
							NULL,
							NULL,
							self_md->obj.obj_type,
							self_md->obj.obj_type_mask,
							result_val,
							new_missing,
							n_dims,
							(ng_size_t*)dimsizes,
							TEMPORARY,
							NULL,
							(NclObjClass)to_type
							);
						return((NclData)output_md);
					}
				} else {
					result_val = (void*)NclMalloc(to_type->type_class.size*self_md->multidval.totalelements);
					if(result_val != NULL) {
						if(_Nclcoerce(
							to_type,
							result_val,
							self_md->multidval.val,
							self_md->multidval.totalelements,
							(self_md->multidval.missing_value.has_missing?&self_md->multidval.missing_value.value:NULL),
							new_missing,
							from_type) != NhlFATAL) {
							if((self_md->multidval.missing_value.has_missing)&&(new_missing == NULL)) {
								/* dont try to convert other type missing values to logical */
					                        if((to_type->type_class.data_type == NCL_logical)||
								   (!_NclScalarCoerce(&(self_md->multidval.missing_value.value),self_md->multidval.data_type,&tmp_missing, to_type->type_class.data_type))) {
					                                tmp_missing = to_type->type_class.default_mis;
					                        }
								output_md = _NclCreateVal(
									NULL,
									NULL,
									self_md->obj.obj_type,
									self_md->obj.obj_type_mask,
									result_val,
									&tmp_missing,
									self_md->multidval.n_dims,
									self_md->multidval.dim_sizes,
									TEMPORARY,
									NULL,
									(NclObjClass)to_type
									);
							}  else {
								output_md = _NclCreateVal(
									NULL,
									NULL,
									self_md->obj.obj_type,
									self_md->obj.obj_type_mask,
									result_val,
									new_missing,
									self_md->multidval.n_dims,
									self_md->multidval.dim_sizes,
									TEMPORARY,
									NULL,
									(NclObjClass)to_type
									);
							}
							return((NclData)output_md);
						} else {
							NclFree(result_val);
						}
					}
				}
			}
		} 
	}
	return(NULL);
}

static void MultiDValResetMissing
#if	NhlNeedProto
(NclData self, NclScalar * missing)
#else
(self, missing)
NclData self;
NclScalar * missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	
	if(self_md == NULL) return;

	if(missing == NULL) {
		self_md->multidval.missing_value.has_missing = 0;
		return;
	}

	if(!self_md->multidval.missing_value.has_missing) {
		if (self_md->multidval.data_type == NCL_logical) {
			self_md->multidval.missing_value.has_missing = 1;
			self_md->multidval.missing_value.value.logicalval = -1;
                }
		else {
			self_md->multidval.missing_value.has_missing = 1;
			self_md->multidval.missing_value.value = *missing;
			return;
               }
	}

	if(self_md->multidval.type == NULL) return;

	if(_Nclreset_mis(self_md->multidval.type,self_md->multidval.val,&(self_md->multidval.missing_value.value),missing,self_md->multidval.totalelements) != NhlFATAL) {
		self_md->multidval.missing_value.value = *missing;
	}
	return;
}

static NhlErrorTypes MultiDValPrint
#if     NhlNeedProto
(NclObj self, FILE *fp)
#else
(self,fp)
NclObj self;
FILE *fp;
#endif
{
    NclMultiDValData self_md = (NclMultiDValData)self;
    ng_size_t i[NCL_MAX_DIMENSIONS];
    ng_size_t j[NCL_MAX_DIMENSIONS];
    ng_size_t where;
    int done = 0;
    ng_size_t k;
    ng_size_t ndims = self_md->multidval.n_dims;
    ng_size_t el_size;
    int ret = 0;
    NhlErrorTypes ret0 = NhlNOERROR;

    if (self_md->multidval.totalelements == 0) {
        ret = nclfprintf(fp,"(0) no elements\n");
        if(ret < 0) {
            return(NhlWARNING);
        }
	return NhlNOERROR;
    }	

    el_size = self_md->multidval.type->type_class.size;
    for(k = 0; k < self_md->multidval.n_dims; k++) {
        i[k] = 0;
        j[k] = self_md->multidval.dim_sizes[k];
    }

    while(!done) {
        where = 0;
        if (!NCLnoPrintElem) {
            ret = nclfprintf(fp,"(");
            if(ret < 0) {
                return(NhlWARNING);
            }
            for(k = 0; k < ndims - 1; k++) {
                ret = nclfprintf(fp,"%zd,",i[k]);
                if(ret < 0) {
                    return(NhlWARNING);
                }
                where = (where + i[k]) * j[k+1];
            }
            ret = nclfprintf(fp,"%zd)\t",i[ndims-1]);
            if(ret < 0) {
                return(NhlWARNING);
            }
        }
	else {
 	    for(k = 0; k < ndims - 1; k++) {
	       where = (where + i[k]) * j[k+1];
	    }
	}	

        where = where + i[ndims - 1];

        ret0 = _Nclprint(self_md->multidval.type,fp,(void*)((char*)self_md->multidval.val + (where * el_size)));
        if(ret0 < NhlINFO) {
            return(NhlWARNING);
        }
        ret = nclfprintf(fp,"\n");
        if(ret < 0) {
            return(NhlWARNING);
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
    return(NhlNOERROR);
}

static void MultiDValDestroy
#if     NhlNeedProto
(NclObj self)
#else
(self)
        NclObj  self;
#endif
{
        NclMultiDValData self_md = (NclMultiDValData)self;
	NhlArgVal selector;
	NhlArgVal cbdata;
	ng_size_t i;

	cbdata.intval = self->obj.id;
	selector.lngval = DESTROYED;

	if(self->obj.cblist != NULL) {
		_NhlCBCallCallbacks(self->obj.cblist,selector,cbdata);
	}
        _NclUnRegisterObj(self);

        if(self_md->multidval.sel_rec != NULL) {
		for(i = 0; i <  self_md->multidval.sel_rec->n_entries; i++) {
			if(self_md->multidval.sel_rec->selection[i].sel_type == Ncl_VECSUBSCR){
				NclFree(self_md->multidval.sel_rec->selection[i].u.vec.ind);
			}
		}
		NclFree(self_md->multidval.sel_rec);
        }

        if((self_md->obj.status != STATIC)&&(self_md->multidval.val != NULL)) {
                NclFree(self_md->multidval.val);
        }
	if(self->obj.cblist != NULL) {
		_NhlCBDestroy(self->obj.cblist);
	}
        NclFree(self);
        return;
}

static NhlErrorTypes MultiDValClassInitialize(
#if NhlNeedProto
void
#endif
);

NclMultiDValDataClassRec nclMultiDValDataClassRec = {
	{
/* char *class_name; 		*/	"MultiDValData",
/* unsigned int obj_size;	*/	sizeof(NclMultiDValDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	MultiDValDestroy,
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	MultiDValClassInitialize,
		(NclAddParentFunction)MultiDValAddParent,
                (NclDelParentFunction)MultiDValDelParent,
/* NclPrintSummaryFunction print_summary; */    NULL,
/* NclPrintFunction print; 	*/	MultiDValPrint	,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCallData  obtain_calldata*/   NULL
	},
	{
/* NclGenericFunction dup; 	*/	MultiDValDup,
/* NclResetMissingValueFuction dup;	*/	MultiDValResetMissing,
/* NclReadSubSecFunction r_subsection */ 	MultiDValReadSection,
/* NclReadSubSecFunction w_subsection */ {	
						MultiDVal_md_WriteSection,
						MultiDVal_s_WriteSection
					},
/* NclReadThenWriteSubFunc w_subsection */ MultiDVal_ReadWriteSection,
/* NclDataFunction coerce; 	*/	{
						MultiDVal_md_Coerce,
						MultiDVal_md_Coerce
					},
/* NclDataFunction multiply; 	*/	{
						MultiDVal_mdmd_Mul,
						MultiDVal_ss_Mul,
						MultiDVal_ss_Mul,
						MultiDVal_ss_Mul
					},
/* NclDataFunction plus; 	*/	{
						MultiDVal_mdmd_Plus,
						MultiDVal_ss_Plus,
						MultiDVal_ss_Plus,
						MultiDVal_ss_Plus
					},
/* NclDataFunction minus; 	*/	{	
						MultiDVal_mdmd_Minus,	
						MultiDVal_ss_Minus,	
						MultiDVal_ss_Minus,	
						MultiDVal_ss_Minus
					},
/* NclDataFunction divide; 	*/	{
						MultiDVal_mdmd_Div,
						MultiDVal_ss_Div,
						MultiDVal_ss_Div,
						MultiDVal_ss_Div
					},
/* NclDataFunction exponent; 	*/	{
                                                MultiDVal_mdmd_Exp,
                                                MultiDVal_ss_Exp,
                                                MultiDVal_ss_Exp,
                                                MultiDVal_ss_Exp
					},
/* NclDataFunction mod; 	*/	{
                                                MultiDVal_mdmd_Mod,
                                                MultiDVal_ss_Mod,
                                                MultiDVal_ss_Mod,
                                                MultiDVal_ss_Mod
					},
/* NclDataFunction mat; 	*/	{
                                                MultiDVal_mdmd_Mat,
                                                MultiDVal_ss_Mul,
                                                MultiDVal_ss_Mul,
                                                MultiDVal_ss_Mul 
					},
/* NclDataFunction sel_lt; 	*/	{
                                                MultiDVal_mdmd_SelLt,
                                                MultiDVal_ss_SelLt,
                                                MultiDVal_ss_SelLt,
                                                MultiDVal_ss_SelLt
					},
/* NclDataFunction sel_gt; 	*/	{
                                                MultiDVal_mdmd_SelGt,
                                                MultiDVal_ss_SelGt,
                                                MultiDVal_ss_SelGt,
                                                MultiDVal_ss_SelGt
					},
/* NclDataFunction not; 	*/	{
						MultiDVal_md_Not,
						MultiDVal_md_Not
					},
/* NclDataFunction neg; 	*/	{	
						MultiDVal_md_Neg,
						MultiDVal_md_Neg
					},
/* NclDataFunction gt; 		*/	{
                                                MultiDVal_mdmd_Gt,
                                                MultiDVal_ss_Gt,
                                                MultiDVal_ss_Gt,
                                                MultiDVal_ss_Gt
					},
/* NclDataFunction lt; 		*/	{
                                                MultiDVal_mdmd_Lt,
                                                MultiDVal_ss_Lt,
                                                MultiDVal_ss_Lt,
                                                MultiDVal_ss_Lt
					},
/* NclDataFunction ge; 		*/	{
                                                MultiDVal_mdmd_Ge,
                                                MultiDVal_ss_Ge,
                                                MultiDVal_ss_Ge,
                                                MultiDVal_ss_Ge
					},
/* NclDataFunction le; 		*/	{
                                                MultiDVal_mdmd_Le,
                                                MultiDVal_ss_Le,
                                                MultiDVal_ss_Le,
                                                MultiDVal_ss_Le
					},
/* NclDataFunction ne; 		*/	{
                                                MultiDVal_mdmd_Ne,
                                                MultiDVal_ss_Ne,
                                                MultiDVal_ss_Ne,
                                                MultiDVal_ss_Ne
					},
/* NclDataFunction eq; 		*/	{
                                                MultiDVal_mdmd_Eq,
                                                MultiDVal_ss_Eq,
                                                MultiDVal_ss_Eq,
                                                MultiDVal_ss_Eq
					},
/* NclDataFunction and;	 	*/	{
                                                MultiDVal_mdmd_And,
                                                MultiDVal_ss_And,
                                                MultiDVal_ss_And,
                                                MultiDVal_ss_And
					},
/* NclDataFunction or; 		*/	{
                                                MultiDVal_mdmd_Or,
                                                MultiDVal_ss_Or,
                                                MultiDVal_ss_Or,
                                                MultiDVal_ss_Or
					},
/* NclDataFunction xor;		*/	{
                                                MultiDVal_mdmd_Xor,
                                                MultiDVal_ss_Xor,
                                                MultiDVal_ss_Xor,
                                                MultiDVal_ss_Xor
					},
/* NclIsMissingFuncion is_mis   */	MultiDVal_IsMis
	},
	{
		NULL
	}
};

NclObjClass nclMultiDValDataClass = (NclObjClass)&nclMultiDValDataClassRec;

static NhlErrorTypes MultiDValClassInitialize
#if NhlNeedProto
(void)
#else
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_MultiDValData,
		(NclObjClass)&nclMultiDValDataClassRec
	);
	return(NhlNOERROR);
}




struct _NclMultiDValDataRec * _NclCreateMultiDVal
#if     NhlNeedProto
(NclObj inst, NclObjClass theclass, NclObjTypes obj_type, unsigned int obj_type_mask, void *val, NclScalar *missing_value, int n_dims, ng_size_t *dim_sizes, NclStatus status, NclSelectionRecord *sel_rec, NclTypeClass type)
#else
(inst, theclass, obj_type, obj_type_mask, val, missing_value, n_dims, dim_sizes, status, sel_rec, type)
NclObj inst;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
ng_size_t *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
NclTypeClass type;
#endif
{

	NclMultiDValData thevalobj;
	NclObjClass class_ptr= nclMultiDValDataClass;
	ng_size_t i;
	NhlErrorTypes ret1= NhlNOERROR;
	ng_size_t nelem;

	ret1 = _NclInitClass(nclMultiDValDataClass);
	if(ret1 < NhlWARNING) {
		return(NULL);
	}
	if(inst == NULL ) {
		thevalobj = (NclMultiDValData)NclMalloc(
				(unsigned)nclMultiDValDataClassRec.obj_class.obj_size);
	} else {
		thevalobj = (NclMultiDValData)inst;
	}
	if(theclass != NULL) {
		class_ptr = theclass;
	}
/*
* Since no initialize functions exist for Obj and Data (meaningless because
* data has not instance record) fields must be assign manually here
*/
	_NclDataCreate((NclObj)thevalobj,class_ptr,obj_type,(obj_type_mask | Ncl_MultiDValData),status);


	thevalobj->multidval.data_type = type->type_class.data_type;
	thevalobj->multidval.val = val;
	thevalobj->multidval.type= type;
	thevalobj->multidval.hlu_type_rep[0] = type->type_class.hlu_type_rep[0];
	thevalobj->multidval.hlu_type_rep[1] = type->type_class.hlu_type_rep[1];

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
	thevalobj->multidval.totalsize = nelem * type->type_class.size;
	if(sel_rec != NULL) {
		thevalobj->multidval.sel_rec = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
		memcpy((char*)thevalobj->multidval.sel_rec,(char*)sel_rec,sizeof(NclSelectionRecord));
		for(i = 0; i <  sel_rec->n_entries; i++) {
                 	if(sel_rec->selection[i].sel_type == Ncl_VECSUBSCR){
                        	thevalobj->multidval.sel_rec->selection[i].u.vec.ind = (long*) NclMalloc(sizeof(long)*(sel_rec->selection[i].u.vec.n_ind));
				memcpy(thevalobj->multidval.sel_rec->selection[i].u.vec.ind,sel_rec->selection[i].u.vec.ind,sizeof(long)*(sel_rec->selection[i].u.vec.n_ind));
        	        }
		}
	} else {
		thevalobj->multidval.sel_rec = NULL;
	}
	return((NclMultiDValData)thevalobj);
}
