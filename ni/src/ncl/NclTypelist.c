
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
 *	Date:		Fri Jan 27 18:23:52 MST 1995
 *
 *	Description:	
 */

#ifdef NIO_LIB_ONLY
#include "niohluP.h"
#include "nioNresDB.h"
#include "nioConvert.h"
#else
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Convert.h>
#endif
#include "defs.h"
#include "NclTypedouble.h"
#include "NclTypelist.h"
#include <math.h>
#include "NclMultiDValData.h"
#include "DataSupport.h"

#ifdef NIO_LIB_ONLY
#include "nioBaseP.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Callbacks.h>
#endif

#include "NclTypelogical.h"
#include "NclHLUObj.h"
#include "NclMultiDValHLUObjData.h"

static NhlErrorTypes Ncl_Type_list_print
#if     NhlNeedProto
(FILE *fp, void * val)
#else
(fp,val)
FILE *fp;
void *val;
#endif
{

	NclObj object;
	int ret;


	object = _NclGetObj(*(obj*)val);

	if(object == NULL) {
		ret = nclfprintf(fp,"%d",*(obj*)val);
		if(ret < 0) {
                	return(NhlWARNING);
        	} else {
                	return(NhlNOERROR);
        	}
	} else {
		return(_NclPrint(object,fp));
	}
}



static NhlErrorTypes Ncl_Type_list_InitClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
	nclTypelistClassRec.type_class.default_mis.listval = -1;
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
 *	Date:		Fri Jan 27 18:29:04 MST 1995
 *
 *	Description:	
 */


static NhlErrorTypes Ncl_Type_list_reset_mis
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
	obj *value = (obj*)val;
	ng_size_t i;

	if((old_m == NULL)||(new_m == NULL))
		return(NhlFATAL);


	for(i = 0; i < nval; i++,value++ ) {
		if(*value == old_m->objval) {
			*value = new_m->objval;
		}	
	}
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
NhlErrorTypes Ncl_Type_list_eq
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
        obj *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	int linc = 0;
	int rinc = 0;
	ng_size_t i;

	ls = (obj*)lhs;
	rs = (obj*)rhs;
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
			*res = (logical)(( lhs_m->objval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->objval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->objval == *ls)|| ( rhs_m->objval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls == *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_list_eq_type
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
NhlErrorTypes Ncl_Type_list_ne
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
        obj *ls,*rs;
	logical *res;
	ng_size_t stopi = 1;
	int linc = 0;
	int rinc = 0;
	ng_size_t i;

	ls = (obj*)lhs;
	rs = (obj*)rhs;
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
			*res = (logical)(( lhs_m->objval == *ls) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	} else if(lhs_m == NULL ) {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)(( rhs_m->objval == *rs) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	} else {
		for(i = 0; i < stopi; i++, res++, ls += linc, rs += rinc) {
			*res = (logical)((( lhs_m->objval == *ls)|| ( rhs_m->objval == *rs)) ? ( ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval ) : (*ls != *rs));
		}
	}
	return(NhlNOERROR);
}

NclTypeClass Ncl_Type_list_ne_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

NclTypelistClassRec nclTypelistClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclTypeClassRec,
		0,
		NULL,
		NULL,
		NULL,
		Ncl_Type_list_InitClass,
		NULL,
		NULL,
		NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclObjTypes type 			*/ Ncl_Typelist,
/* NclBasicDataTypes 			*/ NCL_list,
/* int size 				*/ sizeof(obj),
/* char * hlu_rep_type			*/ {NULL,NULL},
/* NclScalar	default_mis		*/ {-1},
/* NclTypePrint print			*/ "%d",
/* NclTypePrint print			*/ Ncl_Type_list_print,
/* NclTypeResetMissing reset_mis; 	*/ Ncl_Type_list_reset_mis,
/* NclTypeCoerceFunction coerce; 	*/ NULL,
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
/* NclTypeOp not; 			*/ NULL,
/* NclTypeOutType not_type;             */ NULL,
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
/* NclTypeOp ne; 			*/ Ncl_Type_list_ne,
/* NclTypeOutType ne_type;              */ Ncl_Type_list_ne_type,
/* NclTypeOp eq; 			*/ Ncl_Type_list_eq,
/* NclTypeOutType eq_type;              */ Ncl_Type_list_eq_type,
/* NclTypeOp and; 			*/ NULL,
/* NclTypeOutType and_type;             */ NULL,
/* NclTypeOp or; 			*/ NULL,
/* NclTypeOutType or_type;              */ NULL,
/* NclTypeOp xor; 			*/ NULL,
/* NclTypeOp xor;                       */ NULL,
/* NclNumScalarCompareFunc cmpf; 	*/ NULL,
/* NclMonotonicTestFunction is_mono; 	*/ NULL
	},
	{
		NULL
	}
};

NclObjClass nclTypelistClass = (NclObjClass)&nclTypelistClassRec;

NclType _NclTypelistCreate
#if	NhlNeedProto
(NclObj inst , NclObjClass theclass , NclObjTypes list_type , unsigned int list_type_mask, NclStatus status)
#else
(inst , theclass , list_type ,list_type_mask, status)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes list_type ;
unsigned int list_type_mask;
NclStatus status;
#endif
{
	return((NclType)_NclTypeCreate(inst,theclass,list_type,(list_type_mask | Ncl_Typelist), status));
}
