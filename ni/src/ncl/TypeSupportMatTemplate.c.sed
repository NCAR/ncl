
/*
 *      $Id: TypeSupportMatTemplate.c.sed,v 1.1.4.1 2008-03-28 20:37:53 grubin Exp $
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
 *	Date:		Fri Jan 27 18:30:15 MST 1995
 *
 *	Description:	
 */





NhlErrorTypes _NclTFUNC
#if	NhlNeedProto
(NclTypeClass the_type, void * result, void* lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m, int nlhs_dims,ng_size_t* lhs_dimsizes, int nrhs_dims, ng_size_t* rhs_dimsizes)
#else
(the_type, result, lhs, rhs, lhs_m, rhs_m, nlhs_dims,lhs_dimsizes, nrhs_dims,rhs_dimsizes)
NclTypeClass the_type;
void * result;
void* lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar* rhs_m;
int nlhs_dims;
ng_size_t *lhs_dimsizes;
int nrhs_dims;
ng_size_t *rhs_dimsizes;
#endif
{
	NclTypeClass tmp;

	if(the_type->type_class.TFUNC!= NULL) {
		return((*(the_type->type_class.TFUNC))(result, lhs, rhs, lhs_m, rhs_m, nlhs_dims,lhs_dimsizes, nrhs_dims,rhs_dimsizes));
	} else {
		tmp = (NclTypeClass)the_type->obj_class.super_class;
		while(tmp != (NclTypeClass)nclTypeClass) {
			if(tmp->type_class.TFUNC != NULL) {
				return((*(the_type->type_class.TFUNC))(result, lhs, rhs, lhs_m, rhs_m, nlhs_dims,lhs_dimsizes, nrhs_dims,rhs_dimsizes));
			} else {
				tmp = (NclTypeClass)tmp->obj_class.super_class;
			}
		}
		return(NhlFATAL);
	}
}


NclTypeClass _NclTFUNC_type
#if	NhlNeedProto
(NclTypeClass the_type)
#else
(the_type)
NclTypeClass the_type;
#endif
{
	NclTypeClass tmp;

	if(the_type->type_class.TFUNC_type!= NULL) {
		return((*(the_type->type_class.TFUNC_type))());
	} else {
		tmp = (NclTypeClass)the_type->obj_class.super_class;
		while(tmp != (NclTypeClass)nclTypeClass) {
			if(tmp->type_class.TFUNC_type != NULL) {
				return((*(tmp->type_class.TFUNC_type))());
			} else {
				tmp = (NclTypeClass)tmp->obj_class.super_class;
			}
		}
		return(NULL);
	}
}

