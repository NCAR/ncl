
/*
 *      $Id: TypestringCompareOpTemplate.c.sed,v 1.2 1995-03-28 00:05:10 ethan Exp $
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
 *	Date:		Fri Jan 27 18:31:56 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_string_FUNCNAME
#if	NhlNeedProto
(void *result,void *lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m, int nlhs, int nrhs)
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
        string *ls,*rs;
	logical *res;
	int stopi = 1;
	int linc = 0;
	int rinc = 0;
	int i;

	ls = (string*)lhs;
	rs = (string*)rhs;
	res = (logical*)result;

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
			*res = (logical)(CMPFUNC(*ls,*rs));
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->stringval == *ls) ? (((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval) : (CMPFUNC(*ls,*rs)));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->stringval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval) : (CMPFUNC(*ls,*rs)));
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->stringval == *ls)|| ( rhs_m->stringval == *rs)) ? (((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval) : (CMPFUNC(*ls,*rs)));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_string_FUNCNAME_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}
