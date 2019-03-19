
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

#include "NclTypeobj.h"

/*
 *      $Id: NclTypeobj.c.specific,v 1.11 2008-12-10 20:12:17 dbrown Exp $
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
 *	Date:		Fri Jan 27 18:34:41 MST 1995
 *
 *	Description:	
 */
#ifdef NIO_LIB_ONLY
#include "nioBaseP.h"
#include "nioCallbacks.h"
#include "nioConvertP.h"
#else
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Callbacks.h>
#include <ncarg/hlu/ConvertP.h>
#endif
#include "NclTypelogical.h"
#include "NclHLUObj.h"
#include "NclMultiDValHLUObjData.h"

static NhlErrorTypes Ncl_Type_obj_print
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


/*ARGSUSED*/
static NhlErrorTypes CvtNhlTObjIdGenArrayToNclData
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
	char func[] = "CvtNhlTObjIdGenArrayToNclData";
	void *val;
	NclMultiDValData tmp_md;
	ng_size_t i;
	int id;
	NclHLUObj tmp_hlu;
	NhlLayer tmp_layer;
	ng_size_t len_dimensions = 1;
	

	if(nargs != 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong number of args",func);
		to->size =0;
		return(NhlFATAL);
	}
	gen = (NhlGenArray)from->data.ptrval;
	if(gen != NULL) {
		if(!_NhlIsSubtypeQ(NrmStringToQuark(NhlTIntegerGenArray),from->typeQ)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong input type",func);
			to->size =0;
			return(NhlFATAL);
		}
		val = NclMalloc((unsigned)sizeof(int) * gen->num_elements);
		for(i = 0; i < gen->num_elements; i++ ){
			id = ((int*)gen->data)[i];
			if(id >0 ) {
				tmp_layer = _NhlGetLayer(id);
				tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,id,-1,tmp_layer->base.layer_class);
				if(tmp_hlu != NULL) {
					((int*)val)[i] = tmp_hlu->obj.id;
				} else {
					((int*)val)[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
				}
			} else {
				((int*)val)[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
			}
		}
		tmp_md = _NclMultiDValHLUObjDataCreate(
			NULL,NULL, Ncl_MultiDValHLUObjData,
			0,val,NULL,gen->num_dimensions,
			gen->len_dimensions,TEMPORARY,NULL);
		if(to->size < sizeof(NclMultiDValData)) {
			return(NhlFATAL);
		} else {
			*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
			return(NhlNOERROR);
		}
	} else {
		val = NclMalloc((unsigned)sizeof(int) );
		*(obj*)(val) = nclTypeobjClassRec.type_class.default_mis.objval;
		tmp_md = _NclMultiDValHLUObjDataCreate(
			NULL,NULL, Ncl_MultiDValHLUObjData,
			0,val,&nclTypeobjClassRec.type_class.default_mis,1,
			&len_dimensions,TEMPORARY,NULL);
		if(to->size < sizeof(NclMultiDValData)) {
			return(NhlFATAL);
		} else {
			*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
			return(NhlNOERROR);
		}
	}
}
/*ARGSUSED*/
static NhlErrorTypes CvtNhlTObjIdToNclData
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
	void* tmp;
	NclMultiDValData tmp_md;
	int n_dims = 1;
	ng_size_t len_dims = 1;
	NhlLayer tmp_layer;
	NclHLUObj tmp_hlu;
	NclScalar missing;

	tmp = NclMalloc((unsigned)sizeof(int));
	tmp_layer = _NhlGetLayer(*(int*)&(from->data));
	missing = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis;
	if(tmp_layer != NULL) {
		tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,*(int*)&(from->data),-1,tmp_layer->base.layer_class);
		*(int*)tmp = tmp_hlu->obj.id;
	} else {
		*(int*)tmp = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval; 
	}
	
	
	tmp_md = _NclMultiDValHLUObjDataCreate(
		NULL,NULL, Ncl_MultiDValHLUObjData,
		0,(void*)tmp,&missing,n_dims,
		&len_dims,TEMPORARY,NULL);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}

static NhlErrorTypes Ncl_Type_obj_InitClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
        NhlRegisterConverter(NhlbaseClass,NhlTObjIdGenArray,NhlTNclData,
		CvtNhlTObjIdGenArrayToNclData,NULL,0,False,NULL);
        NhlRegisterConverter(NhlbaseClass,NhlTObjId,NhlTNclData,
		CvtNhlTObjIdToNclData,NULL,0,False,NULL);
	nclTypeobjClassRec.type_class.default_mis.objval = -1;
	return(NhlNOERROR);
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


static NhlErrorTypes Ncl_Type_obj_reset_mis
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

	if (old_m->objval == new_m->objval) {
		/* nothing to do */
		return NhlNOERROR;	
	}

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
NhlErrorTypes Ncl_Type_obj_eq
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
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
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

NclTypeClass Ncl_Type_obj_eq_type
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
NhlErrorTypes Ncl_Type_obj_ne
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
	ng_size_t linc = 0;
	ng_size_t rinc = 0;
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

NclTypeClass Ncl_Type_obj_ne_type
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return((NclTypeClass)nclTypelogicalClass);
}

NclTypeobjClassRec nclTypeobjClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclTypeClassRec,
		0,
		NULL,
		NULL,
		NULL,
		Ncl_Type_obj_InitClass,
		NULL,
		NULL,
		NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclObjTypes type 			*/ Ncl_Typeobj,
/* NclBasicDataTypes 			*/ NCL_obj,
/* int size 				*/ sizeof(obj),
/* char * hlu_rep_type			*/ {NULL,NULL},
/* NclScalar	default_mis		*/ {-1},
/* NclTypePrint print			*/ "%d",
/* NclTypePrint print			*/ Ncl_Type_obj_print,
/* NclTypeResetMissing reset_mis; 	*/ Ncl_Type_obj_reset_mis,
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
/* NclTypeOp ne; 			*/ Ncl_Type_obj_ne,
/* NclTypeOutType ne_type;              */ Ncl_Type_obj_ne_type,
/* NclTypeOp eq; 			*/ Ncl_Type_obj_eq,
/* NclTypeOutType eq_type;              */ Ncl_Type_obj_eq_type,
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

NclObjClass nclTypeobjClass = (NclObjClass)&nclTypeobjClassRec;

NclType _NclTypeobjCreate
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
	return((NclType)_NclTypeCreate(inst,theclass,obj_type,(obj_type_mask | Ncl_Typeobj), status));
}
