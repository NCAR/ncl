
/*
 *      $Id: TypestringSelectOpTemplate.c.sed,v 1.1 1995-01-28 01:53:21 ethan Exp $
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
 *	Date:		Fri Jan 27 18:32:08 MST 1995
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
	string *res;
	int stopi = 1;
	int linc = 0;
	int rinc = 0;
	int i;

	ls = (string*)lhs;
	rs = (string*)rhs;
	res = (string*)result;

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
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (string)SELFUNC(*ls,*rs,-1,-1,-1);
		}
	} else if(rhs_m == NULL) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res= (string)SELFUNC(*ls,*rs, lhs_m->stringval, -1, lhs_m->stringval);
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (string)SELFUNC(*ls,*rs,-1, rhs_m->stringval,rhs_m->stringval);
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (string)SELFUNC(*ls,*rs, lhs_m->stringval, rhs_m->stringval,lhs_m->stringval);
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
	return((NclTypeClass)nclTypestringClass);
}
