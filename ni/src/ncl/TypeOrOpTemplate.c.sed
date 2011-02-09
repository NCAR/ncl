
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
 *	Author:		Dave Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 17 15:36:20 MDT 2009
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
	OUTDATATYPE *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (LOCALTYPE*)lhs;
	rs = (LOCALTYPE*)rhs;
	res = (OUTDATATYPE*)result;

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
			*res = (OUTDATATYPE)(*ls || *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (OUTDATATYPE)(( lhs_m->DATATYPEval == *ls) ? ( LEFTMISSING ) : (*ls || *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
                        if ((OUTDATATYPE) *ls) {
                        	*res = (OUTDATATYPE) (*ls || 1);
			}
			else {
				*res = (OUTDATATYPE)(( rhs_m->DATATYPEval == *rs) ? ( RIGHTMISSING ) : (*ls || *rs));
			}
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			if (lhs_m->DATATYPEval == *ls) {
				*res = (OUTDATATYPE) ( LEFTMISSING );
			}
			else if ((OUTDATATYPE) *ls) {
                        	*res = (OUTDATATYPE) (*ls || 1);
			}
			else {
				*res = (OUTDATATYPE)(( rhs_m->DATATYPEval == *rs) ? ( LEFTMISSING ) : (*ls || *rs));
			}
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
