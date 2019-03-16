
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

#include "NclTypechar.h"

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
 *	Date:		Fri Jan 27 18:25:51 MST 1995
 *
 *	Description:	
 */

#include "NclTypelogical.h"
#include "NclTypefloat.h"
#include <ctype.h>

static NhlErrorTypes Ncl_Type_char_print
#if     NhlNeedProto
(FILE *fp, void * val)
#else
(fp,val)
FILE *fp;
void *val;
#endif
{
        unsigned char *sp = (unsigned char*)val;
	int ret;

	if (isprint(*sp) || isspace(*sp))
	        ret = nclfprintf(fp,"%c",*sp);
        else
	        ret = nclfprintf(fp,"0x%02x",*sp);
	if(ret < 0) {
                return(NhlWARNING);
        } else {
                return(NhlNOERROR);
        }
}



static NhlErrorTypes Ncl_Type_char_coerce
#if     NhlNeedProto
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
        unsigned char *res = (unsigned char*)result;
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
                tmp_mis.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
        } else {
                tmp_mis.charval = to_m->charval;
        }

        switch(fc->type_class.type) {
        case Ncl_Typebyte: {
                byte *fl = (byte*)from;
                if((from_m == NULL)||(to_m == NULL)) {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                *res = *fl;
                        }
                } else {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                tmp = 0;
                                (*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
                                if(tmp) {
                                        *res = tmp_mis.charval;
                                } else {
                                        *res = (unsigned char)*fl;
                                }
                        }
                }
                return(NhlNOERROR);
        }	
        case Ncl_Typechar: {
                unsigned char *fl = (unsigned char*)from;
                if((from_m == NULL)||(to_m == NULL)) {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                *res = *fl;
                        }
                } else {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                tmp = 0;
                                (*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
                                if(tmp) {
                                        *res = tmp_mis.charval;
                                } else {
                                        *res = (unsigned char)*fl;
                                }
                        }
                }
                return(NhlNOERROR);
        }	
        case Ncl_Typeubyte: {
                unsigned char *fl = (unsigned char *)from;
                if((from_m == NULL)||(to_m == NULL)) {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                *res = (char)*fl;
                        }
                } else {
                        for(i = 0; i < n;i++,res++,fl++)  {
                                tmp = 0;
                                (*eq)((void*)&tmp,(void*)fl,(void*)from_m,NULL,NULL,1,1);
                                if(tmp) {
                                        *res = tmp_mis.charval;
                                } else {
                                        *res = (char)*fl;
                                }
                        }
                }
                return(NhlNOERROR);
        }
	default:
		return(NhlFATAL);
	}
}


static NhlErrorTypes Ncl_Type_char_cmpf
#if     NhlNeedProto
(void *lhs, void* rhs, NclScalar* lhs_m, NclScalar *rhs_m,int digits, double* result)
#else
(lhs, rhs, lhs_m, rhs_m, digits, result)
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar *rhs_m;
int digits;
double* result;
#endif
{
        
        if((lhs_m != NULL)&&(lhs_m->charval == *(unsigned char*)lhs)) {
                return(NhlFATAL);
        } else if((rhs_m != NULL)&&(rhs_m->charval == *(unsigned char*)rhs)) {
                return(NhlFATAL);
        } else {
                *result = (double)(*(unsigned char*)lhs-*(unsigned char*)rhs);
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


static NhlErrorTypes Ncl_Type_char_reset_mis
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
	char *value = (char*)val;
	ng_size_t i;

	if((old_m == NULL)||(new_m == NULL))
		return(NhlFATAL);

	if (old_m->charval == new_m->charval) {
		/* nothing to do */
		return NhlNOERROR;	
	}

	for(i = 0; i < nval; i++,value++ ) {
		if(*value == old_m->charval) {
			*value = new_m->charval;
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
static NhlErrorTypes CvtNhlTCharacterGenArrayToNclData
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
	char func[] = "CvtNhlTCharacterGenArrayToNclData";
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
		if(!_NhlIsSubtypeQ(NrmStringToQuark(((NclTypecharClass)nclTypecharClass)->type_class.hlu_type_rep[1]),from->typeQ)) {
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
			gen->len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypecharClass);
		if(to->size < sizeof(NclMultiDValData)) {
			return(NhlFATAL);
		} else {
			*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
			return(NhlNOERROR);
		}
	} else {
                val = NclMalloc((unsigned)nclTypecharClassRec.type_class.size);
                *(char*)(val) = nclTypecharClassRec.type_class.default_mis.charval;
                tmp_md = _NclCreateMultiDVal(
                        NULL,NULL, Ncl_MultiDValData,
                        0,val,&nclTypecharClassRec.type_class.default_mis,1,
                        &len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypecharClass);
                if(to->size < sizeof(NclMultiDValData)) {
                        return(NhlFATAL);
                } else {
                        *((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
                        return(NhlNOERROR);
                }
	}
}
/*ARGSUSED*/
static NhlErrorTypes CvtNhlTCharacterToNclData
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
		&len_dims,TEMPORARY,NULL,(NclTypeClass)nclTypecharClass);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}

static NhlErrorTypes Ncl_Type_char_InitClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
        NhlRegisterConverter(NhlbaseClass,NhlTCharacterGenArray,NhlTNclData,
		CvtNhlTCharacterGenArrayToNclData,NULL,0,False,NULL);
        NhlRegisterConverter(NhlbaseClass,NhlTCharacter,NhlTNclData,
		CvtNhlTCharacterToNclData,NULL,0,False,NULL);
	nclTypecharClassRec.type_class.default_mis.charval = (char)0;
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
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jan 27 18:29:35 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_char_plus
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
        char *ls,*rs;
	char *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
	res = (char*)result;

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
			*res = (char)(*ls + *rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)(( lhs_m->charval == *ls) ? ( lhs_m->charval ) : (*ls + *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)(( rhs_m->charval == *rs) ? ( rhs_m->charval ) : (*ls + *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( lhs_m->charval ) : (*ls + *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_plus_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypecharClass);
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
NhlErrorTypes Ncl_Type_char_lt
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
        char *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
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
			*res = (logical)(( lhs_m->charval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls < *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->charval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls < *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls < *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_lt_type
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
NhlErrorTypes Ncl_Type_char_gt
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
        char *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
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
			*res = (logical)(( lhs_m->charval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls > *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->charval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls > *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls > *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_gt_type
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
NhlErrorTypes Ncl_Type_char_le
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
        char *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
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
			*res = (logical)(( lhs_m->charval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls <= *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->charval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls <= *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls <= *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_le_type
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
NhlErrorTypes Ncl_Type_char_ge
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
        char *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
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
			*res = (logical)(( lhs_m->charval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls >= *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->charval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls >= *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls >= *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_ge_type
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
NhlErrorTypes Ncl_Type_char_eq
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
        char *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
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
			*res = (logical)(( lhs_m->charval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->charval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_eq_type
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
NhlErrorTypes Ncl_Type_char_ne
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
        char *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
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
			*res = (logical)(( lhs_m->charval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->charval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_ne_type
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
 *	Date:		Fri Jan 27 18:29:29 MST 1995
 *
 *	Description:	
 */
NhlErrorTypes Ncl_Type_char_sel_lt
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
        char *ls,*rs;
	char *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
	res= (char*)result;

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
			*res = (char)((*ls < *rs)? *ls:*rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)(( lhs_m->charval == *ls) ? ( lhs_m->charval) : (*ls < *rs)? *ls : *rs);
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)(( rhs_m->charval == *rs) ? ( rhs_m->charval) : (*ls < *rs)? *ls : *rs);
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( lhs_m->charval) : (*ls < *rs)? *ls : *rs);
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_sel_lt_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypecharClass);
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
NhlErrorTypes Ncl_Type_char_sel_gt
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
        char *ls,*rs;
	char *res;
	ng_size_t stopi = 1;
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
	ng_size_t i;

	ls = (char*)lhs;
	rs = (char*)rhs;
	res= (char*)result;

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
			*res = (char)((*ls > *rs)? *ls:*rs);
		}
	} else if(rhs_m == NULL) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)(( lhs_m->charval == *ls) ? ( lhs_m->charval) : (*ls > *rs)? *ls : *rs);
		}
	} else if(lhs_m == NULL ) {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)(( rhs_m->charval == *rs) ? ( rhs_m->charval) : (*ls > *rs)? *ls : *rs);
		}
	} else {
		for(i = 0 ; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (char)((( lhs_m->charval == *ls)|| ( rhs_m->charval == *rs)) ? ( lhs_m->charval) : (*ls > *rs)? *ls : *rs);
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_char_sel_gt_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypecharClass);
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
static NclMonoTypes Ncl_Type_char_is_mono
#if	NhlNeedProto
(void *val,NclScalar* val_m,ng_size_t nval)
#else
(val, val_m, nval)
void *val;
NclScalar* val_m;
ng_size_t nval;
#endif
{
	char *value = (char*)val;
	ng_size_t i = 0,j = 1;

	if(nval == 1) 
		return(NclINCREASING);
	if(val_m != NULL) {
		i = 0;
		j = 0;
		while((i<nval)&&(value[i] == val_m->charval))i++;
		if(i >= nval-1) return(NclNONMONO);
		j = i + 1;
		while((j<nval)&&(value[j] == val_m->charval)) j++;
		if(j == nval) return(NclNONMONO);
/*
* i is first non-missing value and j is second guarenteed
*/
		if(value[i] > value[j]) {
			while(value[i] > value[j]) {
				i = j;
				j++;
				while((j<nval)&&(value[j] == val_m->charval)) {
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
				while((j<nval)&&(value[j] == val_m->charval)) {
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

NclTypecharClassRec nclTypecharClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclTypeClassRec,
		0,
		NULL,
		NULL,
		NULL,
		Ncl_Type_char_InitClass,
		NULL,
		NULL,
		NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclObjTypes type 			*/ Ncl_Typechar,
/* NclBasicDataTypes 			*/ NCL_char,
/* int size 				*/ sizeof(char),
/* char * hlu_rep_type			*/ {NhlTCharacter,NhlTCharacterGenArray},
/* NclScalar	default_mis		*/ {0},
/* NclTypePrint print			*/ "%c",
/* NclTypePrint print			*/ Ncl_Type_char_print,
/* NclTypeResetMissing reset_mis; 	*/ Ncl_Type_char_reset_mis,
/* NclTypeCoerceFunction coerce; 	*/ Ncl_Type_char_coerce,
/* NclTypeOp multiply; 			*/ NULL,
/* NclTypeOutType multiply_type;        */ NULL,
/* NclTypeOp plus; 			*/ Ncl_Type_char_plus,
/* NclTypeOutType plus_type;            */ Ncl_Type_char_plus_type,
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
/* NclTypeOp sel_lt; 			*/ Ncl_Type_char_sel_lt,
/* NclTypeOutType sel_lt_type;          */ Ncl_Type_char_sel_lt_type,
/* NclTypeOp sel_gt; 			*/ Ncl_Type_char_sel_gt,
/* NclTypeOutType sel_gt_type;          */ Ncl_Type_char_sel_gt_type,
/* NclTypeOp not; 			*/ NULL,
/* NclTypeOutType not_type;             */ NULL,
/* NclTypeOp neg; 			*/ NULL,
/* NclTypeOutType neg_type;             */ NULL,
/* NclTypeOp gt; 			*/ Ncl_Type_char_gt,
/* NclTypeOutType gt_type;              */ Ncl_Type_char_gt_type,
/* NclTypeOp lt; 			*/ Ncl_Type_char_lt,
/* NclTypeOutType lt_type;              */ Ncl_Type_char_lt_type,
/* NclTypeOp ge; 			*/ Ncl_Type_char_ge,
/* NclTypeOutType ge_type;              */ Ncl_Type_char_ge_type,
/* NclTypeOp le; 			*/ Ncl_Type_char_le,
/* NclTypeOutType le_type;              */ Ncl_Type_char_le_type,
/* NclTypeOp ne; 			*/ Ncl_Type_char_ne,
/* NclTypeOutType ne_type;              */ Ncl_Type_char_ne_type,
/* NclTypeOp eq; 			*/ Ncl_Type_char_eq,
/* NclTypeOutType eq_type;              */ Ncl_Type_char_eq_type,
/* NclTypeOp and; 			*/ NULL,
/* NclTypeOutType and_type;             */ NULL,
/* NclTypeOp or; 			*/ NULL,
/* NclTypeOutType or_type;              */ NULL,
/* NclTypeOp xor; 			*/ NULL,
/* NclTypeOp xor;                       */ NULL,
/* NclNumScalarCompareFunc cmpf; 	*/ Ncl_Type_char_cmpf,
/* NclMonotonicTestFunction is_mono; 	*/ Ncl_Type_char_is_mono
	},
	{
		NULL
	}
};

NclObjClass nclTypecharClass = (NclObjClass)&nclTypecharClassRec;

NclType _NclTypecharCreate
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
	return((NclType)_NclTypeCreate(inst,theclass,obj_type,(obj_type_mask | Ncl_Typechar), status));
}
