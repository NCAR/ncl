
/*
 *      $Id: NclMultiDValData.c.sed,v 1.4 1994-10-29 00:57:49 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
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
 *	Date:		Fri Oct 29 11:36:10 MDT 1993
 *
 *	Description:	
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Convert.h>
#include "defs.h"
#include <errno.h>
#include "NclMultiDValDATATYPEData.h"


static NclData MultiDVal_DATATYPE_md_Coerce(
#ifdef NhlNeedProto
NclData /* self */,
NclObjTypes /* coerce_to_obj */,
NclScalar * /*new_missing*/
#endif
);

static NclData MultiDVal_DATATYPE_s_Coerce(
#ifdef NhlNeedProto
NclData /* self */,
NclObjTypes /* coerce_to_obj */,
NclScalar * /*new_missing*/
#endif
);


static void NclMultiDValDATATYPEPrint(
#ifdef NhlNeedProto
	NclObj /* self */,
	FILE *	/*fp*/
#endif
);
static void MultiDValDestroy(
#ifdef NhlNeedProto
NclObj	self
#endif
);

static NclData NclMultiDValDATATYPEDup (
#ifdef NhlNeedProto
NclData /*self*/,
NclScalar * /*new_missing*/
#endif
);

DSPECIFIC
REPLACE

NclMultiDValDATATYPEDataClassRec nclMultiDValDATATYPEDataClassRec = {
	{
/* char *class_name; 		*/	"MultiDValDATATYPEData",
/* unsigned int obj_size;	*/	sizeof(NclMultiDValDATATYPEDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclMultiDValDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	MultiDValDestroy, 
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	MultiDVal_DATATYPE_InitClass,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
/* NclPrintFunction print; 	*/	NclMultiDValDATATYPEPrint,
	},
	{
/* NclPrintFunction dup; 	*/	NclMultiDValDATATYPEDup,
/* NclResetMissingValueFuction rest_mis; 	*/	MultiDVal_DATATYPE_ResetMissing,
/* NclReadSubSecFunction r_subsection */	MultiDVal_DATATYPE_ReadSection,
/* NclWriteSubSecFunction w_subsection */{
						MultiDVal_DATATYPE_md_WriteSection,
						MultiDVal_DATATYPE_s_WriteSection
					},
/* NclReadThenWriteSubFunc r_then_w_subsection*/ MultiDVal_DATATYPE_ReadWriteSection,
/* NclDataFunction coerce; 	*/	{ 
						MultiDVal_DATATYPE_md_Coerce,
						MultiDVal_DATATYPE_s_Coerce
					},
/* NclDataFunction multiply; 	*/	{
						MultiDVal_DATATYPE_mdmd_Mul,
						MultiDVal_DATATYPE_mds_Mul,
						MultiDVal_DATATYPE_smd_Mul,
						MultiDVal_DATATYPE_ss_Mul
					},
/* NclDataFunction plus; 	*/	{
						MultiDVal_DATATYPE_mdmd_Plus,
						MultiDVal_DATATYPE_mds_Plus,
						MultiDVal_DATATYPE_smd_Plus,
						MultiDVal_DATATYPE_ss_Plus
					},
/* NclDataFunction minus; 	*/	{
						MultiDVal_DATATYPE_mdmd_Minus,
						MultiDVal_DATATYPE_mds_Minus,
						MultiDVal_DATATYPE_smd_Minus,
						MultiDVal_DATATYPE_ss_Minus
					},
/* NclDataFunction divide; 	*/	{
						MultiDVal_DATATYPE_mdmd_Div,
						MultiDVal_DATATYPE_mds_Div,
						MultiDVal_DATATYPE_smd_Div,
						MultiDVal_DATATYPE_ss_Div
					},
/* NclDataFunction exponent; 	*/	{
						MultiDVal_DATATYPE_mdmd_Exp,
						MultiDVal_DATATYPE_mds_Exp,
						MultiDVal_DATATYPE_smd_Exp,
						MultiDVal_DATATYPE_ss_Exp
					},
/* NclDataFunction mod; 	*/	{
						MultiDVal_DATATYPE_mdmd_Mod,
						MultiDVal_DATATYPE_mds_Mod,
						MultiDVal_DATATYPE_smd_Mod,
						MultiDVal_DATATYPE_ss_Mod
					},
/* NclDataFunction mat; 	*/	{
						NULL,
						NULL,
						NULL,
						NULL
					},
/* NclDataFunction sel_lt; 	*/	{
						MultiDVal_DATATYPE_mdmd_SelLt,
						MultiDVal_DATATYPE_mds_SelLt,
						MultiDVal_DATATYPE_smd_SelLt,
						MultiDVal_DATATYPE_ss_SelLt
					},
/* NclDataFunction sel_gt; 	*/	{
						MultiDVal_DATATYPE_mdmd_SelGt,
						MultiDVal_DATATYPE_mds_SelGt,
						MultiDVal_DATATYPE_smd_SelGt,
						MultiDVal_DATATYPE_ss_SelGt
					},
/* NclDataFunction not; 	*/	{
						MultiDVal_DATATYPE_md_Not,
						MultiDVal_DATATYPE_s_Not
						
					},
/* NclDataFunction neg; 	*/	{
						MultiDVal_DATATYPE_md_Neg,
						MultiDVal_DATATYPE_s_Neg
					},
/* NclDataFunction gt; 		*/	{
						MultiDVal_DATATYPE_mdmd_Gt,
						MultiDVal_DATATYPE_mds_Gt,
						MultiDVal_DATATYPE_smd_Gt,
						MultiDVal_DATATYPE_ss_Gt
					},
/* NclDataFunction lt; 		*/	{
						MultiDVal_DATATYPE_mdmd_Lt,
						MultiDVal_DATATYPE_mds_Lt,
						MultiDVal_DATATYPE_smd_Lt,
						MultiDVal_DATATYPE_ss_Lt
					},
/* NclDataFunction ge; 		*/	{
						MultiDVal_DATATYPE_mdmd_Ge,
						MultiDVal_DATATYPE_mds_Ge,
						MultiDVal_DATATYPE_smd_Ge,
						MultiDVal_DATATYPE_ss_Ge
					},
/* NclDataFunction le; 		*/	{
						MultiDVal_DATATYPE_mdmd_Le,
						MultiDVal_DATATYPE_mds_Le,
						MultiDVal_DATATYPE_smd_Le,
						MultiDVal_DATATYPE_ss_Le
					},
/* NclDataFunction ne; 		*/	{
						MultiDVal_DATATYPE_mdmd_Ne,
						MultiDVal_DATATYPE_mds_Ne,
						MultiDVal_DATATYPE_smd_Ne,
						MultiDVal_DATATYPE_ss_Ne
					},
/* NclDataFunction eq; 		*/	{
						MultiDVal_DATATYPE_mdmd_Eq,
						MultiDVal_DATATYPE_mds_Eq,
						MultiDVal_DATATYPE_smd_Eq,
						MultiDVal_DATATYPE_ss_Eq
					},
/* NclDataFunction and;	 	*/	{
						MultiDVal_DATATYPE_mdmd_And,
						MultiDVal_DATATYPE_mds_And,
						MultiDVal_DATATYPE_smd_And,
						MultiDVal_DATATYPE_ss_And
					},
/* NclDataFunction or; 		*/	{
						MultiDVal_DATATYPE_mdmd_Or,
						MultiDVal_DATATYPE_mds_Or,
						MultiDVal_DATATYPE_smd_Or,
						MultiDVal_DATATYPE_ss_Or
					},
/* NclDataFunction xor;		*/	{
						MultiDVal_DATATYPE_mdmd_Xor,
						MultiDVal_DATATYPE_mds_Xor,
						MultiDVal_DATATYPE_smd_Xor,
						MultiDVal_DATATYPE_ss_Xor
					},
/* NclIsMissingFunction is_mis */	MultiDVal_DATATYPE_is_mis
	},
	{
		HLUGENTYPEREP
	},
	{
		NULL
	}
	
};

NclObjClass nclMultiDValDATATYPEDataClass = (NclObjClass)&nclMultiDValDATATYPEDataClassRec;


NclMultiDValData _NclMultiDValDATATYPECreate
#if __STDC__
(NclObj inst,NclObjClass theclass,NclObjTypes obj_type,unsigned int obj_type_mask,void *val,NclScalar *missing_value,int n_dims, int *dim_sizes,NclStatus status,NclSelectionRecord *sel_rec)
#else
(inst,theclass,obj_type,obj_type_mask, val,missing_value,n_dims,dim_sizes,status,sel_rec)
NclObj inst ;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
int *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
#endif
{
	NclMultiDValDATATYPEData thevalobj;
	NclObjClass class_ptr= nclMultiDValDATATYPEDataClass;
	int i;
	NhlErrorTypes ret1= NhlNOERROR;
	int nelem;

	ret1 = _NclInitClass(nclMultiDValDATATYPEDataClass);
	if(ret1 < NhlWARNING) {
		return(NULL);
	}
	if(inst == NULL ) {
		thevalobj = (NclMultiDValDATATYPEData)NclMalloc(
				(unsigned)nclMultiDValDATATYPEDataClassRec.obj_class.obj_size);
	} else {
		thevalobj = (NclMultiDValDATATYPEData)inst;
	}
	if(theclass != NULL) {
		class_ptr = theclass;
	}
/*
* Since no initialize functions exist for Obj and Data (meaningless because
* data has not instance record) fields must be assign manually here
*/
	_NclMultiDValDataCreate((NclObj)thevalobj,class_ptr,obj_type,(obj_type_mask | Ncl_MultiDValDATATYPEData),status);


	thevalobj->multidval.data_type = NCL_DATATYPE;
	thevalobj->multidval.val = val;

	if(missing_value != NULL ) {
		thevalobj->multidval.missing_value.has_missing = 1;
			thevalobj->multidval.missing_value.value = *missing_value;
	} else {
		thevalobj->multidval.missing_value.has_missing = 0;
	}



	thevalobj->multidval.n_dims = n_dims;
	nelem = 1;
	if((n_dims == 1) &&(dim_sizes[0]== 1)) {
		thevalobj->multidval.kind = SCALAR;
		thevalobj->multidval.dim_sizes[0] = 1;
	} else {
		thevalobj->multidval.kind = MULTID;
		for(i = 0; i<n_dims; i++) {
			thevalobj->multidval.dim_sizes[i] = dim_sizes[i];
			nelem *= dim_sizes[i];
		}
	}
	thevalobj->multidval.totalelements = nelem;
	thevalobj->multidval.totalsize = nelem * _NclSizeOf(NCL_DATATYPE);
	if(sel_rec != NULL) {
		thevalobj->multidval.sel_rec = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
		memcpy((char*)thevalobj->multidval.sel_rec,(char*)sel_rec,sizeof(NclSelectionRecord));
	} else {
		thevalobj->multidval.sel_rec = NULL;
	}
	thevalobj->multidval.hlu_type_rep[0] = HLUTYPEREP;
	thevalobj->multidval.hlu_type_rep[1] = HLUGENTYPEREP;
	return((NclMultiDValData)thevalobj);
}

static NclData NclMultiDValDATATYPEDup
#if  __STDC__
(NclData self,NclScalar *new_missing)
#else
(self,new_missing)
NclData self;
NclScalar *new_missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData) self;
	DATATYPE *toval;
	DATATYPE *frval;
	DATATYPE missing;
	NclScalar themissing;
	int i;

	toval = (DATATYPE*)NclMalloc(self_md->multidval.totalsize);
	frval = (DATATYPE*)self_md->multidval.val;
	if(toval == NULL) {
		return(NULL);
	}
	if((new_missing == NULL)||(!self_md->multidval.missing_value.has_missing)) {
		memcpy((char*)toval,(char*)frval,self_md->multidval.totalsize);
		themissing = self_md->multidval.missing_value.value;
	} else {
		missing = self_md->multidval.missing_value.value.DATATYPEval;
		for(i = 0; i < self_md->multidval.totalelements; i++) {
			toval[i] = (frval[i] == missing ?
					new_missing->DATATYPEval :
					frval[i]);
		}
		themissing = *new_missing;
	}
	return((NclData)_NclMultiDValDATATYPECreate(
		NULL,
		NULL,
		Ncl_MultiDValDATATYPEData,
		0,
		(void*)toval,
		(self_md->multidval.missing_value.has_missing ? &themissing : NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,
		NULL));
}

static void NclMultiDValDATATYPEPrint
#if __STDC__
(NclObj self, FILE *fp)
#else 
(self,fp)
NclObj self;
FILE *fp;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	int i[NCL_MAX_DIMENSIONS];
	int j[NCL_MAX_DIMENSIONS];
	int k,where,done = 0;
	int ndims = self_md->multidval.n_dims;

	
	for(k = 0; k < self_md->multidval.n_dims; k++) {
		i[k] = 0;
		j[k] = self_md->multidval.dim_sizes[k];
	}
	while(!done) {
		where = 0;
		nclfprintf(fp,"(");
		for(k = 0; k < ndims - 1; k++) {
			nclfprintf(fp,"%d,",i[k]);
			where = (where + i[k]) * j[k+1];
		}
		nclfprintf(fp,"%d)\t",i[ndims-1]);
		where = where + i[ndims - 1];

		nclfprintf(fp,"PRINTFORMAT",((DATATYPE*)(self_md->multidval.val))[where]);
		i[ndims - 1]++;
		if(i[ndims - 1] == j[ndims - 1]) {
			for(k=ndims - 1;k >0;k--) {
				if(i[k] == j[k]) {
					i[k] = 0;
					i[k-1]++;
				}
			}
			if(i[0] == j[0]) done = 1;
		}
	}
	return;
}

static void MultiDValDestroy
#if  __STDC__
(NclObj	self)
#else
(self)
	NclObj  self;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;

	_NclUnRegisterObj(self);

	if(self_md->multidval.sel_rec != NULL) {
		NclFree(self_md->multidval.sel_rec);
	}
	if((self_md->obj.status != STATIC)&&(self_md->multidval.val != NULL)){
		NclFree(self_md->multidval.val);
	}
	NclFree(self);
/*
* handling of dimension information records goes here
*/
	return;
}

