
/*
 *      $Id: TypeMatMulOpTemplate.c.sed,v 1.3 2009-07-10 19:54:06 huangwei Exp $
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
 *	Date:		Fri Jan 27 18:29:35 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_DATATYPE_mat
#if	NhlNeedProto
(void *result,void *lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m, int nlhs_dims,ng_size_t* lhs_dimsizes, int nrhs_dims,ng_size_t*rhs_dimsizes)
#else
(result,lhs,rhs,lhs_m,rhs_m,nlhs,lhs_dimsizes,nrhs,rhs_dimsizes)
void *result;
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar* rhs_m;
int nlhs;
ng_size_t* lhs_dimsizes;
int nrhs;
ng_size_t*rhs_dimsizes;
#endif
{
        LOCALTYPE *ls,*rs;
	LOCALOUTTYPE *res;
	ng_size_t stopi = 1;
	ng_size_t stopk = 1;
	ng_size_t stopj = 1;
	ng_size_t i,j,k;

	ls = (LOCALTYPE*)lhs;
	rs = (LOCALTYPE*)rhs;
	res = (LOCALOUTTYPE*)result;

	if((nrhs_dims == 2)&&(nlhs_dims ==2))  {
		stopi = lhs_dimsizes[0];
		stopj = rhs_dimsizes[1];
		stopk = rhs_dimsizes[0];
	} else if(nrhs_dims ==2) {
		stopi = 1;
		stopj = rhs_dimsizes[1];
		stopk = rhs_dimsizes[0];
	} else if(nlhs_dims ==2) {
		stopi = lhs_dimsizes[0];
		stopj = 1;
		stopk = rhs_dimsizes[0];
	} else {
		stopi = 1;
		stopj = 1;
		stopk = rhs_dimsizes[0];
	}
	

	if((lhs_m == NULL)&&(rhs_m == NULL)) {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (LOCALTYPE*)rhs + j;
				ls = &(((LOCALTYPE*)lhs)[i * stopk]);
				*res = (LOCALOUTTYPE)(*ls++ * *rs);
				rs += stopj;
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					*res = *res + (LOCALOUTTYPE)(*ls * *rs);
				}
				res++;
			}
		}
	} else if(rhs_m == NULL) {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (LOCALTYPE*)rhs + j;
				ls = &(((LOCALTYPE*)lhs)[i * stopk]);
				if( lhs_m->DATATYPEval == *ls) {
					*res = LEFTMISSING;
					res++;
					continue;
				} else {
					*res = (LOCALOUTTYPE)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if( lhs_m->DATATYPEval == *ls) {
						*res = LEFTMISSING;
						break;
					} else 	{
						*res = *res + (LOCALOUTTYPE)(*ls * *rs);
					}
				}
				res++;
			}
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (LOCALTYPE*)rhs + j;
				ls = &(((LOCALTYPE*)lhs)[i * stopk]);
				if( rhs_m->DATATYPEval == *rs) {
					*res = RIGHTMISSING;
					res++;
					continue;
				} else {
					*res = (LOCALOUTTYPE)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if( rhs_m->DATATYPEval == *rs) {
						*res = RIGHTMISSING;
						break;
					} else 	{
						*res = *res + (LOCALOUTTYPE)(*ls * *rs);
					}
				}
				res++;
			}
		}
	} else {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (LOCALTYPE*)rhs + j;
				ls = &(((LOCALTYPE*)lhs)[i * stopk]);
				if((rhs_m->DATATYPEval == *rs)||( lhs_m->DATATYPEval == *ls)) {
					*res = LEFTMISSING;
					res++;
					continue;
				} else {
					*res = (LOCALOUTTYPE)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if(( rhs_m->DATATYPEval == *rs)||( lhs_m->DATATYPEval == *ls)) {
						*res = LEFTMISSING;
						break;
					} else 	{
						*res = *res + (LOCALOUTTYPE)(*ls * *rs);
					}
				}
				res++;
			}
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_DATATYPE_mat_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypeOUTDATATYPEClass);
}
