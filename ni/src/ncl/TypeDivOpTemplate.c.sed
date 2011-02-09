
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
 *	Date:		Fri Jan 27 18:28:35 MST 1995
 *
 *	Description:	
 */
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

        rs = (LOCALTYPE*)rhs;
	ls = (LOCALTYPE*)lhs;
	res = (LOCALOUTTYPE*)result;
        for(i = 0; i< nrhs ; i++) {
                if((rs[i] == (LOCALTYPE)0)&&((rhs_m == NULL) || (rhs_m->DATATYPEval != rs[i]))) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: Division by 0, Can't continue");
                        return(NhlFATAL);
                }
        }


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
			*res = (LOCALOUTTYPE)(*ls THEOP *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (LOCALOUTTYPE)(( lhs_m->DATATYPEval == *ls) ? ( LEFTMISSING ) : (*ls THEOP *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (LOCALOUTTYPE)(( rhs_m->DATATYPEval == *rs) ? ( RIGHTMISSING ) : (*ls THEOP *rs));
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (LOCALOUTTYPE)((( lhs_m->DATATYPEval == *ls)|| ( rhs_m->DATATYPEval == *rs)) ? ( LEFTMISSING ) : (*ls THEOP *rs));
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
