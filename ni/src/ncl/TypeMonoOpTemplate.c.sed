
/*
 *      $Id$
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
 *	Date:		Fri Jan 27 18:28:58 MST 1995
 *
 *	Description:	
 */
/*ARGSUSED*/
NhlErrorTypes Ncl_Type_DATATYPE_FUNCNAME
#if	NhlNeedProto
(void *result,void *lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m, ng_size_t nlhs, ng_size_t nrhs)
#else
(result,lhs,rhs,lhs_m,rhs_m,nlhs,nrhs)
void *result;
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar* rhs_m;
ng_size_t nlhs;
ng_size_t nrhs;
#endif
{
        LOCALTYPE *ls,*rs;
	LOCALOUTTYPE *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;
	
	rs = NULL;
	ls = (LOCALTYPE*)lhs;
	res = (LOCALOUTTYPE*)result;	

	if(nlhs > nrhs) 
		stopi = nlhs;
	else
		stopi = nrhs;
	if(nlhs > 1) {
		linc = 1;
	}
	if(nrhs > 1) {
		rinc = 1;
	}
	

	if((lhs_m == NULL)&&(rhs_m == NULL)) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (LOCALOUTTYPE)(THEOP *ls);
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (LOCALOUTTYPE)(( lhs_m->DATATYPEval == *ls) ? ( OUTMISSING ) : (THEOP *ls));
		}
	} 
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_DATATYPE_FUNCNAME_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypeOUTDATATYPEClass);
}
