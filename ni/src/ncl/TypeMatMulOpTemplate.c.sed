
/*
 *      $Id: TypeMatMulOpTemplate.c.sed,v 1.2 1998-12-23 18:31:36 ethan Exp $
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
(void *result,void *lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m, int nlhs_dims,int* lhs_dimsizes, int nrhs_dims,int*rhs_dimsizes)
#else
(result,lhs,rhs,lhs_m,rhs_m,nlhs,nrhs)
void *result;
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar* rhs_m;
int nlhs;
int nrhs;
#endif
{
        DATATYPE *ls,*rs;
	OUTDATATYPE *res;
	int stopi = 1;
	int stopk = 1;
	int stopj = 1;
	int i,j,k;

	ls = (DATATYPE*)lhs;
	rs = (DATATYPE*)rhs;
	res = (OUTDATATYPE*)result;

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
				rs = (DATATYPE*)rhs + j;
				ls = &(((DATATYPE*)lhs)[i * stopk]);
				*res = (OUTDATATYPE)(*ls++ * *rs);
				rs += stopj;
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					*res = *res + (OUTDATATYPE)(*ls * *rs);
				}
				res++;
			}
		}
	} else if(rhs_m == NULL) {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (DATATYPE*)rhs + j;
				ls = &(((DATATYPE*)lhs)[i * stopk]);
				if( lhs_m->DATATYPEval == *ls) {
					*res = LEFTMISSING;
					res++;
					continue;
				} else {
					*res = (OUTDATATYPE)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if( lhs_m->DATATYPEval == *ls) {
						*res = LEFTMISSING;
						break;
					} else 	{
						*res = *res + (OUTDATATYPE)(*ls * *rs);
					}
				}
				res++;
			}
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (DATATYPE*)rhs + j;
				ls = &(((DATATYPE*)lhs)[i * stopk]);
				if( rhs_m->DATATYPEval == *rs) {
					*res = RIGHTMISSING;
					res++;
					continue;
				} else {
					*res = (OUTDATATYPE)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if( rhs_m->DATATYPEval == *rs) {
						*res = RIGHTMISSING;
						break;
					} else 	{
						*res = *res + (OUTDATATYPE)(*ls * *rs);
					}
				}
				res++;
			}
		}
	} else {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (DATATYPE*)rhs + j;
				ls = &(((DATATYPE*)lhs)[i * stopk]);
				if((rhs_m->DATATYPEval == *rs)||( lhs_m->DATATYPEval == *ls)) {
					*res = LEFTMISSING;
					res++;
					continue;
				} else {
					*res = (OUTDATATYPE)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if(( rhs_m->DATATYPEval == *rs)||( lhs_m->DATATYPEval == *ls)) {
						*res = LEFTMISSING;
						break;
					} else 	{
						*res = *res + (OUTDATATYPE)(*ls * *rs);
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
