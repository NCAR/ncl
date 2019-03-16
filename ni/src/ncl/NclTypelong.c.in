
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

#include "NclTypelong.h"

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
 *	Date:		Fri Jan 27 18:26:41 MST 1995
 *
 *	Description:	
 */
#include "NclTypelogical.h"
#include "NclTypefloat.h"
#include "NclTypeint.h"

static NhlErrorTypes Ncl_Type_long_print
#if     NhlNeedProto
(FILE *fp, void * val)
#else
(fp,val)
FILE *fp;
void *val;
#endif
{
        long *lp = (long*)val;
	int ret;

        ret = nclfprintf(fp,"%ld",*lp);
	if(ret < 0) {
                return(NhlWARNING);
        } else {
                return(NhlNOERROR);
        }

}






static NhlErrorTypes Ncl_Type_long_coerce
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
	long *res = (long*)result;
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
		tmp_mis.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
        } else {
		tmp_mis.longval = to_m->longval;
	}

	switch(fc->type_class.type) {
        case Ncl_Typeulong: {
                unsigned long *fl = (unsigned long*)from;
                if((from_m == NULL)||(to_m == NULL)) {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                *res = (long)*fl;
                        }
                } else {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                tmp = 0;
                                (*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
                                if(tmp) {
                                        *res = tmp_mis.longval;
                                } else {
                                        *res = *fl;
                                }
                        }
                }
                return(NhlNOERROR);
        }
	case Ncl_Typelong: {
		long *fl = (long*)from;
		if((from_m == NULL)||(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (long)*fl;
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.longval;
				} else {
					*res = *fl;
				}
			}
		}
		return(NhlNOERROR);
	}
	case Ncl_Typeint: {
		int *fl = (int*)from;
		if((from_m == NULL)||(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (long)*fl;
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.longval;
				} else {
					*res = (long)*fl;
				}
			}
		}
		return(NhlNOERROR);
	}
        case Ncl_Typeuint: {
                unsigned int *fl = (unsigned int*)from;
                if((from_m == NULL)||(to_m == NULL)) {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                *res = (long)*fl;
                        }
                } else {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                tmp = 0;
                                (*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
                                if(tmp) {
                                        *res = tmp_mis.longval;
                                } else {
                                        *res = (long)*fl;
                                }
                        }
                }
                return(NhlNOERROR);
        }
	case Ncl_Typeshort: {
		short *fl = (short*)from;
		if((from_m == NULL)||(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (long)*fl;
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.longval;
				} else {
					*res = (long)*fl;
				}
			}
		}
		return(NhlNOERROR);
	}
        case Ncl_Typeushort: {
                unsigned short *fl = (unsigned short*)from;
                if((from_m == NULL)||(to_m == NULL)) {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                *res = (long)*fl;
                        }
                } else {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                tmp = 0;
                                (*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
                                if(tmp) {
                                        *res = tmp_mis.longval;
                                } else {
                                        *res = (long)*fl;
                                }
                        }
                }
                return(NhlNOERROR);
        }
	case Ncl_Typebyte: {
		byte *fl = (byte*)from;

		if((from_m == NULL)||(to_m == NULL)) {
			for(i = 0; i < n;i++,res++,fl++)  {
				*res = (long)*fl;
			}
		} else {
			for(i = 0; i < n;i++,res++,fl++)  {
				tmp = 0;
				(*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
				if(tmp) {
					*res = tmp_mis.longval;
				} else {
					*res = (long)*fl;
				}
			}
		}
		return(NhlNOERROR);
	}
        case Ncl_Typeubyte: {
                unsigned char *fl = (unsigned char *)from;
                if((from_m == NULL)||(to_m == NULL)) {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                *res = (long)*fl;
                        }
                } else {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                tmp = 0;
                                (*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
                                if(tmp) {
                                        *res = tmp_mis.longval;
                                } else {
                                        *res = (long)*fl;
                                }
                        }
                }
                return(NhlNOERROR);
        }
	default:
		return(NhlFATAL);
	}
}
static NhlErrorTypes Ncl_Type_long_cmpf
#if     NhlNeedProto
(void *lhs, void* rhs, NclScalar* lhs_m, NclScalar *rhs_m,int digits, double* result)
#else
(lhs, rhs, lhs_m, rhs_m, digits, result)
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar *rhs_m;
int digits;
double * result;
#endif
{
        
        if((lhs_m != NULL)&&(lhs_m->longval == *(long*)lhs)) {
                return(NhlFATAL);
        } else if((rhs_m != NULL)&&(rhs_m->longval == *(long*)rhs)) {
                return(NhlFATAL);
        } else {
                *result = (double)(*(long*)lhs-*(long*)rhs);
                return(NhlNOERROR);
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


static NhlErrorTypes Ncl_Type_long_reset_mis
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
	long *value = (long*)val;
	ng_size_t i;

	if((old_m == NULL)||(new_m == NULL))
		return(NhlFATAL);

	if (old_m->longval == new_m->longval) {
		/* nothing to do */
		return NhlNOERROR;	
	}

	for(i = 0; i < nval; i++,value++ ) {
		if(*value == old_m->longval) {
			*value = new_m->longval;
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
static NhlErrorTypes CvtNhlTLongGenArrayToNclData
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
	char func[] = "CvtNhlTLongGenArrayToNclData";
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
		if(!_NhlIsSubtypeQ(NrmStringToQuark(((NclTypelongClass)nclTypelongClass)->type_class.hlu_type_rep[1]),from->typeQ)) {
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
			gen->len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypelongClass);
		if(to->size < sizeof(NclMultiDValData)) {
			return(NhlFATAL);
		} else {
			*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
			return(NhlNOERROR);
		}
	} else {
                val = NclMalloc((unsigned)nclTypelongClassRec.type_class.size);
                *(long*)(val) = nclTypelongClassRec.type_class.default_mis.longval;
                tmp_md = _NclCreateMultiDVal(
                        NULL,NULL, Ncl_MultiDValData,
                        0,val,&nclTypelongClassRec.type_class.default_mis,1,
                        &len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypelongClass);
                if(to->size < sizeof(NclMultiDValData)) {
                        return(NhlFATAL);
                } else {
                        *((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
                        return(NhlNOERROR);
                }
	}
}
/*ARGSUSED*/
static NhlErrorTypes CvtNhlTLongToNclData
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
		&len_dims,TEMPORARY,NULL,(NclTypeClass)nclTypelongClass);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}

static NhlErrorTypes Ncl_Type_long_InitClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
        NhlRegisterConverter(NhlbaseClass,NhlTLongGenArray,NhlTNclData,
		CvtNhlTLongGenArrayToNclData,NULL,0,False,NULL);
        NhlRegisterConverter(NhlbaseClass,NhlTLong,NhlTNclData,
		CvtNhlTLongToNclData,NULL,0,False,NULL);
	nclTypelongClassRec.type_class.default_mis.longval = -2147483647;
	return(NhlNOERROR);
}


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
NhlErrorTypes Ncl_Type_long_mat
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t stopk = 1;
	ng_size_t stopj = 1;
	ng_size_t i,j,k;

	ls = (long*)lhs;
	rs = (long*)rhs;
	res = (long*)result;

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
				rs = (long*)rhs + j;
				ls = &(((long*)lhs)[i * stopk]);
				*res = (long)(*ls++ * *rs);
				rs += stopj;
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					*res = *res + (long)(*ls * *rs);
				}
				res++;
			}
		}
	} else if(rhs_m == NULL) {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (long*)rhs + j;
				ls = &(((long*)lhs)[i * stopk]);
				if( lhs_m->longval == *ls) {
					*res = lhs_m->longval;
					res++;
					continue;
				} else {
					*res = (long)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if( lhs_m->longval == *ls) {
						*res = lhs_m->longval;
						break;
					} else 	{
						*res = *res + (long)(*ls * *rs);
					}
				}
				res++;
			}
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (long*)rhs + j;
				ls = &(((long*)lhs)[i * stopk]);
				if( rhs_m->longval == *rs) {
					*res = rhs_m->longval;
					res++;
					continue;
				} else {
					*res = (long)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if( rhs_m->longval == *rs) {
						*res = rhs_m->longval;
						break;
					} else 	{
						*res = *res + (long)(*ls * *rs);
					}
				}
				res++;
			}
		}
	} else {
		for(i = 0; i < stopi; i++) {
			for(j = 0; j < stopj; j++){
				rs = (long*)rhs + j;
				ls = &(((long*)lhs)[i * stopk]);
				if((rhs_m->longval == *rs)||( lhs_m->longval == *ls)) {
					*res = lhs_m->longval;
					res++;
					continue;
				} else {
					*res = (long)(*ls++ * *rs);
					rs += stopj;
				}
				for(k = 1; k < stopk; k++,ls++,rs+=stopj) {
					if(( rhs_m->longval == *rs)||( lhs_m->longval == *ls)) {
						*res = lhs_m->longval;
						break;
					} else 	{
						*res = *res + (long)(*ls * *rs);
					}
				}
				res++;
			}
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_mat_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
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
NhlErrorTypes Ncl_Type_long_plus
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
	res = (long*)result;

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
			*res = (long)(*ls + *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( lhs_m->longval == *ls) ? ( lhs_m->longval ) : (*ls + *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( rhs_m->longval == *rs) ? ( rhs_m->longval ) : (*ls + *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( lhs_m->longval ) : (*ls + *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_plus_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
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
NhlErrorTypes Ncl_Type_long_minus
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
	res = (long*)result;

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
			*res = (long)(*ls - *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( lhs_m->longval == *ls) ? ( lhs_m->longval ) : (*ls - *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( rhs_m->longval == *rs) ? ( rhs_m->longval ) : (*ls - *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( lhs_m->longval ) : (*ls - *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_minus_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
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
NhlErrorTypes Ncl_Type_long_multiply
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
	res = (long*)result;

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
			*res = (long)(*ls * *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( lhs_m->longval == *ls) ? ( lhs_m->longval ) : (*ls * *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( rhs_m->longval == *rs) ? ( rhs_m->longval ) : (*ls * *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( lhs_m->longval ) : (*ls * *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_multiply_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
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
NhlErrorTypes Ncl_Type_long_lt
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(*ls < *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls < *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls < *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls < *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_lt_type
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
NhlErrorTypes Ncl_Type_long_gt
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(*ls > *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls > *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls > *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls > *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_gt_type
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
NhlErrorTypes Ncl_Type_long_le
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(*ls <= *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls <= *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls <= *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls <= *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_le_type
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
NhlErrorTypes Ncl_Type_long_ge
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(*ls >= *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls >= *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls >= *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls >= *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_ge_type
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
NhlErrorTypes Ncl_Type_long_eq
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_eq_type
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
NhlErrorTypes Ncl_Type_long_ne
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_ne_type
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
NhlErrorTypes Ncl_Type_long_and
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls && *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
                        if (!(logical) *ls) {
                        	*res = (logical) (*ls && 0);
			}
			else {
				*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls && *rs));
			}
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			if (lhs_m->longval == *ls) {
				*res = (logical) ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval );
			}
			else if (! (logical) *ls) {
                        	*res = (logical) (*ls && 0);
			}
			else {
				*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls && *rs));
			}
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_and_type
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
NhlErrorTypes Ncl_Type_long_or
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls || *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
                        if ((logical) *ls) {
                        	*res = (logical) (*ls || 1);
			}
			else {
				*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls || *rs));
			}
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			if (lhs_m->longval == *ls) {
				*res = (logical) ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval );
			}
			else if ((logical) *ls) {
                        	*res = (logical) (*ls || 1);
			}
			else {
				*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls || *rs));
			}
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_or_type
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
 *	Date:		Fri Jan 27 18:28:50 MST 1995
 *
 *	Description:	
 */
static NclMonoTypes Ncl_Type_long_is_mono
#if	NhlNeedProto
(void *val,NclScalar* val_m,ng_size_t nval)
#else
(val, val_m, nval)
void *val;
NclScalar* val_m;
ng_size_t nval;
#endif
{
	long *value = (long*)val;
	ng_size_t i = 0,j = 1;

	if(nval == 1) 
		return(NclINCREASING);
	if(val_m != NULL) {
		i = 0;
		j = 0;
		while((i<nval)&&(value[i] == val_m->longval))i++;
		if(i >= nval-1) return(NclNONMONO);
		j = i + 1;
		while((j<nval)&&(value[j] == val_m->longval)) j++;
		if(j == nval) return(NclNONMONO);
/*
* i is first non-missing value and j is second guarenteed
*/
		if(value[i] > value[j]) {
			while(value[i] > value[j]) {
				i = j;
				j++;
				while((j<nval)&&(value[j] == val_m->longval)) {
					j++;
				}
				if(j >= nval)
					break;
			}
			if(j >= nval) {
				return(NclDECREASING);
			} else {
				return(NclNONMONO);
			}
		} else if(value[i] < value[j]) {
			while(value[i] < value[j]) {
				i = j;
				j++;
				while((j<nval)&&(value[j] == val_m->longval)) {
					j++;
				}
				if(j >= nval)
					break;
			}
			if(j >= nval) {
				return(NclINCREASING);
			} else {
				return(NclNONMONO);
			}
		}
	} else {
		i = 0;
		if(value[0] > value[1]) {
			while((i<nval-1)&&(value[i] > value[i+1])) i++;
			if(i == nval-1) {
				return(NclDECREASING);
			} else {
				return(NclNONMONO);
			}
		} else if(value[0] < value[1]) {
			while((i<nval-1)&&(value[i] < value[i+1])) i++;
			if(i == nval-1) {
				return(NclINCREASING);
			} else {
				return(NclNONMONO);
			}
		} 
	}
	return(NclNONMONO);
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
 *	Date:		Fri Jan 27 18:28:42 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_long_exponent
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
        long *ls,*rs;
	float *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
	res = (float*)result;

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
			*res = (float)(pow((double)*ls,(double)*rs));
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (float)((lhs_m->longval == *ls) ? ( lhs_m->longval ) : (pow((double)(*ls),(double)(*rs))));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (float)(( rhs_m->longval == *rs) ? ( rhs_m->longval ) : (pow((double)(*ls),(double)(*rs))));
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (float)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( lhs_m->longval) : (pow((double)(*ls),(double)(*rs))));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_exponent_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypefloatClass);
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
NhlErrorTypes Ncl_Type_long_not
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;
	
	rs = NULL;
	ls = (long*)lhs;
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
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (! *ls));
		}
	} 
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_not_type
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
NhlErrorTypes Ncl_Type_long_neg
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;
	
	rs = NULL;
	ls = (long*)lhs;
	res = (long*)result;	

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
			*res = (long)(- *ls);
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( lhs_m->longval == *ls) ? ( lhs_m->longval ) : (- *ls));
		}
	} 
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_neg_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
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
 *	Date:		Fri Jan 27 18:29:29 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_long_sel_lt
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
	res= (long*)result;

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
			*res = (long)((*ls < *rs)? *ls:*rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( lhs_m->longval == *ls) ? ( lhs_m->longval) : (*ls < *rs)? *ls : *rs);
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( rhs_m->longval == *rs) ? ( rhs_m->longval) : (*ls < *rs)? *ls : *rs);
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( lhs_m->longval) : (*ls < *rs)? *ls : *rs);
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_sel_lt_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
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
 *	Date:		Fri Jan 27 18:29:29 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_long_sel_gt
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
	res= (long*)result;

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
			*res = (long)((*ls > *rs)? *ls:*rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( lhs_m->longval == *ls) ? ( lhs_m->longval) : (*ls > *rs)? *ls : *rs);
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( rhs_m->longval == *rs) ? ( rhs_m->longval) : (*ls > *rs)? *ls : *rs);
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( lhs_m->longval) : (*ls > *rs)? *ls : *rs);
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_sel_gt_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
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
NhlErrorTypes Ncl_Type_long_xor
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
        long *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (long*)lhs;
	rs = (long*)rhs;
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
			*res = (logical)(( lhs_m->longval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (!*ls&&*rs)||(*ls&&!*rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->longval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (!*ls&&*rs)||(*ls&&!*rs));
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->longval == *ls)|| ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval == *rs)) ? ( lhs_m->longval) : (!*ls&&*rs)||(*ls&&!*rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_xor_type
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
 *	Date:		Fri Jan 27 18:28:35 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_long_divide
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

        rs = (long*)rhs;
	ls = (long*)lhs;
	res = (long*)result;
        for(i = 0; i< nrhs ; i++) {
                if((rs[i] == (long)0)&&((rhs_m == NULL) || (rhs_m->longval != rs[i]))) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"divide: Division by 0, Can't continue");
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
			*res = (long)(*ls / *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( lhs_m->longval == *ls) ? ( lhs_m->longval ) : (*ls / *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( rhs_m->longval == *rs) ? ( rhs_m->longval ) : (*ls / *rs));
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( lhs_m->longval ) : (*ls / *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_divide_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
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
 *	Date:		Fri Jan 27 18:28:35 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_long_mod
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
        long *ls,*rs;
	long *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

        rs = (long*)rhs;
	ls = (long*)lhs;
	res = (long*)result;
        for(i = 0; i< nrhs ; i++) {
                if((rs[i] == (long)0)&&((rhs_m == NULL) || (rhs_m->longval != rs[i]))) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"mod: Division by 0, Can't continue");
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
			*res = (long)(*ls % *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( lhs_m->longval == *ls) ? ( lhs_m->longval ) : (*ls % *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)(( rhs_m->longval == *rs) ? ( rhs_m->longval ) : (*ls % *rs));
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (long)((( lhs_m->longval == *ls)|| ( rhs_m->longval == *rs)) ? ( lhs_m->longval ) : (*ls % *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_long_mod_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelongClass);
}

NclTypelongClassRec nclTypelongClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclTypeClassRec,
		0,
		NULL,
		NULL,
		NULL,
		Ncl_Type_long_InitClass,
		NULL,
		NULL,
		NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclObjTypes type 			*/ Ncl_Typelong,
/* NclBasicDataTypes 			*/ NCL_long,
/* int size 				*/ sizeof(long),
/* char * hlu_rep_type			*/ {NhlTLong,NhlTLongGenArray},
/* NclScalar	default_mis		*/ {-2147483647},
/* NclTypePrint print			*/ "%ld",
/* NclTypePrint print			*/ Ncl_Type_long_print,
/* NclTypeResetMissing reset_mis; 	*/ Ncl_Type_long_reset_mis,
/* NclTypeCoerceFunction coerce; 	*/ Ncl_Type_long_coerce,
/* NclTypeOp multiply; 			*/ Ncl_Type_long_multiply,
/* NclTypeOutType multiply_type;        */ Ncl_Type_long_multiply_type,
/* NclTypeOp plus; 			*/ Ncl_Type_long_plus,
/* NclTypeOutType plus_type;            */ Ncl_Type_long_plus_type,
/* NclTypeOp minus; 			*/ Ncl_Type_long_minus,
/* NclTypeOutType minus_type;           */ Ncl_Type_long_minus_type,
/* NclTypeOp divide; 			*/ Ncl_Type_long_divide,
/* NclTypeOutType divide_type;          */ Ncl_Type_long_divide_type,
/* NclTypeOp exponent; 			*/ Ncl_Type_long_exponent,
/* NclTypeOutType exponent_type;        */ Ncl_Type_long_exponent_type,
/* NclTypeOp mod; 			*/ Ncl_Type_long_mod,
/* NclTypeOutType mod_type;             */ Ncl_Type_long_mod_type,
/* NclTypeOp mat; 			*/ Ncl_Type_long_mat,
/* NclTypeOutType mat_type;             */ Ncl_Type_long_mat_type,
/* NclTypeOp sel_lt; 			*/ Ncl_Type_long_sel_lt,
/* NclTypeOutType sel_lt_type;          */ Ncl_Type_long_sel_lt_type,
/* NclTypeOp sel_gt; 			*/ Ncl_Type_long_sel_gt,
/* NclTypeOutType sel_gt_type;          */ Ncl_Type_long_sel_gt_type,
/* NclTypeOp not; 			*/ Ncl_Type_long_not,
/* NclTypeOutType not_type;             */ Ncl_Type_long_not_type,
/* NclTypeOp neg; 			*/ Ncl_Type_long_neg,
/* NclTypeOutType neg_type;             */ Ncl_Type_long_neg_type,
/* NclTypeOp gt; 			*/ Ncl_Type_long_gt,
/* NclTypeOutType gt_type;              */ Ncl_Type_long_gt_type,
/* NclTypeOp lt; 			*/ Ncl_Type_long_lt,
/* NclTypeOutType lt_type;              */ Ncl_Type_long_lt_type,
/* NclTypeOp ge; 			*/ Ncl_Type_long_ge,
/* NclTypeOutType ge_type;              */ Ncl_Type_long_ge_type,
/* NclTypeOp le; 			*/ Ncl_Type_long_le,
/* NclTypeOutType le_type;              */ Ncl_Type_long_le_type,
/* NclTypeOp ne; 			*/ Ncl_Type_long_ne,
/* NclTypeOutType ne_type;              */ Ncl_Type_long_ne_type,
/* NclTypeOp eq; 			*/ Ncl_Type_long_eq,
/* NclTypeOutType eq_type;              */ Ncl_Type_long_eq_type,
/* NclTypeOp and; 			*/ Ncl_Type_long_and,
/* NclTypeOutType and_type;             */ Ncl_Type_long_and_type,
/* NclTypeOp or; 			*/ Ncl_Type_long_or,
/* NclTypeOutType or_type;              */ Ncl_Type_long_or_type,
/* NclTypeOp xor; 			*/ Ncl_Type_long_xor,
/* NclTypeOp xor;                       */ Ncl_Type_long_xor_type,
/* NclNumScalarCompareFunc cmpf; 	*/ Ncl_Type_long_cmpf,
/* NclMonotonicTestFunction is_mono; 	*/ Ncl_Type_long_is_mono
	},
	{
		NULL
	}
};

NclObjClass nclTypelongClass = (NclObjClass)&nclTypelongClassRec;

NclType _NclTypelongCreate
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
	return((NclType)_NclTypeCreate(inst,theclass,obj_type,(obj_type_mask | Ncl_Typelong), status));
}
