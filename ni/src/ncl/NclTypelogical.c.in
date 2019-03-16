
/*
 *      $Id: NclType.c.sed,v 1.10 2009-07-10 19:54:05 huangwei Exp $
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
 *	Date:		Fri Jan 27 18:23:52 MST 1995
 *
 *	Description:	
 */

#include "NclTypelogical.h"

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
 *	Date:		Fri Jan 27 18:26:35 MST 1995
 *
 *	Description:	
 */
#include "NclTypelogical.h"
#include "NclTypefloat.h"

static NhlErrorTypes Ncl_Type_logical_print
#if     NhlNeedProto
(FILE *fp, void * val)
#else
(fp,val)
FILE *fp;
void *val;
#endif
{
        logical *lp = (logical*)val;
	int ret;

	if(*lp == -1) {
		ret =nclfprintf(fp,"Missing");
	} else if(*lp == 0) {
		ret =nclfprintf(fp,"False");
	} else {
        	ret =nclfprintf(fp,"True");
	}
	if(ret < 0) {
                return(NhlWARNING);
        } else {
                return(NhlNOERROR);
        }
}









static NhlErrorTypes Ncl_Type_logical_coerce
#if	NhlNeedProto
(void * result, void* from, ng_size_t n, NclScalar* from_m, NclScalar *to_m,NclTypeClass fc)
#else
(result, from, n, from_m, to_m, fc)
void * result;
void* from;
ng_size_t n;
NclScalar* from_m;
NclScalar *to_m;
NclTypeClass fc;
#endif
{
	ng_size_t i;
	logical *res = (logical*)result;
	logical tmp;
	NclTypeOp eq;
	NclScalar tmp_mis;
	

        if((fc == NULL)||(fc->type_class.eq == NULL)
                ||(result == NULL)
                ||(from==NULL))
                return(NhlFATAL);
        else
                eq = fc->type_class.eq;

        if(to_m == NULL){  
		tmp_mis.logicalval = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
        } else {
		tmp_mis = *to_m;
	}


	switch(fc->type_class.type) {
	case Ncl_Typedouble: {
		double *fl = (double*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		}
		else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typefloat: {
		float *fl = (float*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typelogical: {
		logical *fl = (logical *)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = *fl;
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typelong: {
		long *fl = (long *)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeint: {
		int *fl = (int*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeshort: {
		short *fl = (short*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typebyte: {
		byte *fl = (byte*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeulong: {
		unsigned long *fl = (unsigned long *)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeuint: {
		unsigned int *fl = (unsigned int*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeint64: {
		long long *fl = (long long*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeuint64: {
		unsigned long long *fl = (unsigned long long*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeubyte: {
		unsigned char *fl = (unsigned char*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeushort: {
		unsigned short *fl = (unsigned short*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typechar: {
		char *fl = (char*)from;
		if((from_m == NULL)&&(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (logical)(*fl?1:0);
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.logicalval;
				} else {
					*res = (logical)(*fl?1:0);
				}
			}
		}
		return(NhlNOERROR);
	}
	default:
		return(NhlFATAL);
	}
}

/*
 *      $Id: TypeResetMissing.c.sed,v 1.3 2009-07-10 19:54:06 huangwei Exp $
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
 *	Date:		Fri Jan 27 18:29:04 MST 1995
 *
 *	Description:	
 */


static NhlErrorTypes Ncl_Type_logical_reset_mis
#if	NhlNeedProto
(void	*val,NclScalar * old_m,NclScalar * new_m, ng_size_t nval)
#else
(val,old_m,new_m,nval)
void *val;
NclScalar * old_m;
NclScalar * new_m;
ng_size_t nval;
#endif
{
	logical *value = (logical*)val;
	ng_size_t i;

	if((old_m == NULL)||(new_m == NULL))
		return(NhlFATAL);

	if (old_m->logicalval == new_m->logicalval) {
		/* nothing to do */
		return NhlNOERROR;	
	}

	for(i = 0; i < nval; i++,value++ ) {
		if(*value == old_m->logicalval) {
			*value = new_m->logicalval;
		}	
	}
	return(NhlNOERROR);
}

/*
 *      $Id: TypeInitClassTemplate.c.sed,v 1.6 2009-07-10 19:54:06 huangwei Exp $
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
 *	Date:		Fri Jan 27 18:35:14 MST 1995
 *
 *	Description:	
 */

/*ARGSUSED*/
static NhlErrorTypes CvtNhlTBooleanGenArrayToNclData
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	NhlGenArray gen;
	char func[] = "CvtNhlTBooleanGenArrayToNclData";
	void *val;
	NclMultiDValData tmp_md;
	ng_size_t len_dimensions = 1;

	if(nargs != 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong number of args",func);
		to->size =0;
		return(NhlFATAL);
	}
	gen = (NhlGenArray)from->data.ptrval;
	if(gen != NULL) {
		if(!_NhlIsSubtypeQ(NrmStringToQuark(((NclTypelogicalClass)nclTypelogicalClass)->type_class.hlu_type_rep[1]),from->typeQ)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong input type",func);
			to->size =0;
			return(NhlFATAL);
		}
		if(gen->my_data) {
			val = gen->data;
			gen->my_data = False;
		} else {
			val = NclMalloc((unsigned)gen->size * gen->num_elements);
			memcpy(val,gen->data,(unsigned)gen->size * gen->num_elements);
		}
		tmp_md = _NclCreateMultiDVal(
			NULL,NULL, Ncl_MultiDValData,
			0,val,NULL,gen->num_dimensions,
			gen->len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
		if(to->size < sizeof(NclMultiDValData)) {
			return(NhlFATAL);
		} else {
			*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
			return(NhlNOERROR);
		}
	} else {
                val = NclMalloc((unsigned)nclTypelogicalClassRec.type_class.size);
                *(logical*)(val) = nclTypelogicalClassRec.type_class.default_mis.logicalval;
                tmp_md = _NclCreateMultiDVal(
                        NULL,NULL, Ncl_MultiDValData,
                        0,val,&nclTypelogicalClassRec.type_class.default_mis,1,
                        &len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
                if(to->size < sizeof(NclMultiDValData)) {
                        return(NhlFATAL);
                } else {
                        *((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
                        return(NhlNOERROR);
                }
	}
}
/*ARGSUSED*/
static NhlErrorTypes CvtNhlTBooleanToNclData
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	NhlArgVal * tmp;
	NclMultiDValData tmp_md;
	int n_dims = 1;
	ng_size_t len_dims = 1;

	tmp = NclMalloc((unsigned)sizeof(NhlArgVal));
	memcpy((void*)tmp,(void*)&from->data,sizeof(NhlArgVal));
	tmp_md = _NclCreateMultiDVal(
		NULL,NULL, Ncl_MultiDValData,
		0,(void*)tmp,NULL,n_dims,
		&len_dims,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}

static NhlErrorTypes Ncl_Type_logical_InitClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
        NhlRegisterConverter(NhlbaseClass,NhlTBooleanGenArray,NhlTNclData,
		CvtNhlTBooleanGenArrayToNclData,NULL,0,False,NULL);
        NhlRegisterConverter(NhlbaseClass,NhlTBoolean,NhlTNclData,
		CvtNhlTBooleanToNclData,NULL,0,False,NULL);
	nclTypelogicalClassRec.type_class.default_mis.logicalval = -1;
	return(NhlNOERROR);
}


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
NhlErrorTypes Ncl_Type_logical_and
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
        logical *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (logical*)lhs;
	rs = (logical*)rhs;
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
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(*ls && *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->logicalval == *ls) ? ( lhs_m->logicalval ) : (*ls && *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
                        if (!(logical) *ls) {
                        	*res = (logical) (*ls && 0);
			}
			else {
				*res = (logical)(( rhs_m->logicalval == *rs) ? ( rhs_m->logicalval ) : (*ls && *rs));
			}
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			if (lhs_m->logicalval == *ls) {
				*res = (logical) ( lhs_m->logicalval );
			}
			else if (! (logical) *ls) {
                        	*res = (logical) (*ls && 0);
			}
			else {
				*res = (logical)(( rhs_m->logicalval == *rs) ? ( lhs_m->logicalval ) : (*ls && *rs));
			}
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_logical_and_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

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
NhlErrorTypes Ncl_Type_logical_not
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
        logical *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;
	
	rs = NULL;
	ls = (logical*)lhs;
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
			*res = (logical)(! *ls);
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->logicalval == *ls) ? ( lhs_m->logicalval ) : (! *ls));
		}
	} 
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_logical_not_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

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
NhlErrorTypes Ncl_Type_logical_or
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
        logical *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (logical*)lhs;
	rs = (logical*)rhs;
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
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(*ls || *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->logicalval == *ls) ? ( lhs_m->logicalval ) : (*ls || *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
                        if ((logical) *ls) {
                        	*res = (logical) (*ls || 1);
			}
			else {
				*res = (logical)(( rhs_m->logicalval == *rs) ? ( rhs_m->logicalval ) : (*ls || *rs));
			}
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			if (lhs_m->logicalval == *ls) {
				*res = (logical) ( lhs_m->logicalval );
			}
			else if ((logical) *ls) {
                        	*res = (logical) (*ls || 1);
			}
			else {
				*res = (logical)(( rhs_m->logicalval == *rs) ? ( lhs_m->logicalval ) : (*ls || *rs));
			}
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_logical_or_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

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
 *	Date:		Fri Jan 27 18:31:50 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_logical_xor
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
        logical *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (logical*)lhs;
	rs = (logical*)rhs;
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
			*res = (logical)(!*ls&&*rs)||(*ls&&!*rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->logicalval == *ls) ? ( lhs_m->logicalval ) : (!*ls&&*rs)||(*ls&&!*rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->logicalval == *rs) ? ( rhs_m->logicalval ) : (!*ls&&*rs)||(*ls&&!*rs));
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->logicalval == *ls)|| ( lhs_m->logicalval == *rs)) ? ( lhs_m->logicalval) : (!*ls&&*rs)||(*ls&&!*rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_logical_xor_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

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
 *	Date:		Fri Jan 27 18:29:35 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_logical_eq
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
        logical *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (logical*)lhs;
	rs = (logical*)rhs;
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
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(*ls == *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->logicalval == *ls) ? ( lhs_m->logicalval ) : (*ls == *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->logicalval == *rs) ? ( rhs_m->logicalval ) : (*ls == *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->logicalval == *ls)|| ( rhs_m->logicalval == *rs)) ? ( lhs_m->logicalval ) : (*ls == *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_logical_eq_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

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
 *	Date:		Fri Jan 27 18:29:35 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_logical_ne
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
        logical *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (logical*)lhs;
	rs = (logical*)rhs;
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
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(*ls != *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->logicalval == *ls) ? ( lhs_m->logicalval ) : (*ls != *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->logicalval == *rs) ? ( rhs_m->logicalval ) : (*ls != *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->logicalval == *ls)|| ( rhs_m->logicalval == *rs)) ? ( lhs_m->logicalval ) : (*ls != *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_logical_ne_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

NclTypelogicalClassRec nclTypelogicalClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclTypeClassRec,
		0,
		NULL,
		NULL,
		NULL,
		Ncl_Type_logical_InitClass,
		NULL,
		NULL,
		NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclObjTypes type 			*/ Ncl_Typelogical,
/* NclBasicDataTypes 			*/ NCL_logical,
/* int size 				*/ sizeof(logical),
/* char * hlu_rep_type			*/ {NhlTBoolean,NhlTBooleanGenArray},
/* NclScalar	default_mis		*/ {-1},
/* NclTypePrint print			*/ "DEFAULT_FORMAT",
/* NclTypePrint print			*/ Ncl_Type_logical_print,
/* NclTypeResetMissing reset_mis; 	*/ Ncl_Type_logical_reset_mis,
/* NclTypeCoerceFunction coerce; 	*/ Ncl_Type_logical_coerce,
/* NclTypeOp multiply; 			*/ NULL,
/* NclTypeOutType multiply_type;        */ NULL,
/* NclTypeOp plus; 			*/ NULL,
/* NclTypeOutType plus_type;            */ NULL,
/* NclTypeOp minus; 			*/ NULL,
/* NclTypeOutType minus_type;           */ NULL,
/* NclTypeOp divide; 			*/ NULL,
/* NclTypeOutType divide_type;          */ NULL,
/* NclTypeOp exponent; 			*/ NULL,
/* NclTypeOutType exponent_type;        */ NULL,
/* NclTypeOp mod; 			*/ NULL,
/* NclTypeOutType mod_type;             */ NULL,
/* NclTypeOp mat; 			*/ NULL,
/* NclTypeOutType mat_type;             */ NULL,
/* NclTypeOp sel_lt; 			*/ NULL,
/* NclTypeOutType sel_lt_type;          */ NULL,
/* NclTypeOp sel_gt; 			*/ NULL,
/* NclTypeOutType sel_gt_type;          */ NULL,
/* NclTypeOp not; 			*/ Ncl_Type_logical_not,
/* NclTypeOutType not_type;             */ Ncl_Type_logical_not_type,
/* NclTypeOp neg; 			*/ NULL,
/* NclTypeOutType neg_type;             */ NULL,
/* NclTypeOp gt; 			*/ NULL,
/* NclTypeOutType gt_type;              */ NULL,
/* NclTypeOp lt; 			*/ NULL,
/* NclTypeOutType lt_type;              */ NULL,
/* NclTypeOp ge; 			*/ NULL,
/* NclTypeOutType ge_type;              */ NULL,
/* NclTypeOp le; 			*/ NULL,
/* NclTypeOutType le_type;              */ NULL,
/* NclTypeOp ne; 			*/ Ncl_Type_logical_ne,
/* NclTypeOutType ne_type;              */ Ncl_Type_logical_ne_type,
/* NclTypeOp eq; 			*/ Ncl_Type_logical_eq,
/* NclTypeOutType eq_type;              */ Ncl_Type_logical_eq_type,
/* NclTypeOp and; 			*/ Ncl_Type_logical_and,
/* NclTypeOutType and_type;             */ Ncl_Type_logical_and_type,
/* NclTypeOp or; 			*/ Ncl_Type_logical_or,
/* NclTypeOutType or_type;              */ Ncl_Type_logical_or_type,
/* NclTypeOp xor; 			*/ Ncl_Type_logical_xor,
/* NclTypeOp xor;                       */ Ncl_Type_logical_xor_type,
/* NclNumScalarCompareFunc cmpf; 	*/ NULL,
/* NclMonotonicTestFunction is_mono; 	*/ NULL
	},
	{
		NULL
	}
};

NclObjClass nclTypelogicalClass = (NclObjClass)&nclTypelogicalClassRec;

NclType _NclTypelogicalCreate
#if	NhlNeedProto
(NclObj inst , NclObjClass theclass , NclObjTypes obj_type , unsigned int obj_type_mask, NclStatus status)
#else
(inst , theclass , obj_type ,obj_type_mask, status)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
#endif
{
	return((NclType)_NclTypeCreate(inst,theclass,obj_type,(obj_type_mask | Ncl_Typelogical), status));
}
