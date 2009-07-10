
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

#include "NclTypeDATATYPE.h"
DSPECIFIC
REPLACE

NclTypeDATATYPEClassRec nclTypeDATATYPEClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclTypeClassRec,
		0,
		NULL,
		NULL,
		NULL,
		Ncl_Type_DATATYPE_InitClass,
		NULL,
		NULL,
		NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclObjTypes type 			*/ Ncl_TypeDATATYPE,
/* NclBasicDataTypes 			*/ NCL_DATATYPE,
/* int size 				*/ sizeof(LOCALTYPE),
/* char * hlu_rep_type			*/ {HLUTYPEREP,HLUGENTYPEREP},
/* NclScalar	default_mis		*/ {DEFAULT_MISS},
/* NclTypePrint print			*/ "DEFAULT_FORMAT",
/* NclTypePrint print			*/ Ncl_Type_DATATYPE_print,
/* NclTypeResetMissing reset_mis; 	*/ Ncl_Type_DATATYPE_reset_mis,
/* NclTypeCoerceFunction coerce; 	*/ Ncl_Type_DATATYPE_coerce,
/* NclTypeOp multiply; 			*/ Ncl_Type_DATATYPE_multiply,
/* NclTypeOutType multiply_type;        */ Ncl_Type_DATATYPE_multiply_type,
/* NclTypeOp plus; 			*/ Ncl_Type_DATATYPE_plus,
/* NclTypeOutType plus_type;            */ Ncl_Type_DATATYPE_plus_type,
/* NclTypeOp minus; 			*/ Ncl_Type_DATATYPE_minus,
/* NclTypeOutType minus_type;           */ Ncl_Type_DATATYPE_minus_type,
/* NclTypeOp divide; 			*/ Ncl_Type_DATATYPE_divide,
/* NclTypeOutType divide_type;          */ Ncl_Type_DATATYPE_divide_type,
/* NclTypeOp exponent; 			*/ Ncl_Type_DATATYPE_exponent,
/* NclTypeOutType exponent_type;        */ Ncl_Type_DATATYPE_exponent_type,
/* NclTypeOp mod; 			*/ Ncl_Type_DATATYPE_mod,
/* NclTypeOutType mod_type;             */ Ncl_Type_DATATYPE_mod_type,
/* NclTypeOp mat; 			*/ Ncl_Type_DATATYPE_mat,
/* NclTypeOutType mat_type;             */ Ncl_Type_DATATYPE_mat_type,
/* NclTypeOp sel_lt; 			*/ Ncl_Type_DATATYPE_sel_lt,
/* NclTypeOutType sel_lt_type;          */ Ncl_Type_DATATYPE_sel_lt_type,
/* NclTypeOp sel_gt; 			*/ Ncl_Type_DATATYPE_sel_gt,
/* NclTypeOutType sel_gt_type;          */ Ncl_Type_DATATYPE_sel_gt_type,
/* NclTypeOp not; 			*/ Ncl_Type_DATATYPE_not,
/* NclTypeOutType not_type;             */ Ncl_Type_DATATYPE_not_type,
/* NclTypeOp neg; 			*/ Ncl_Type_DATATYPE_neg,
/* NclTypeOutType neg_type;             */ Ncl_Type_DATATYPE_neg_type,
/* NclTypeOp gt; 			*/ Ncl_Type_DATATYPE_gt,
/* NclTypeOutType gt_type;              */ Ncl_Type_DATATYPE_gt_type,
/* NclTypeOp lt; 			*/ Ncl_Type_DATATYPE_lt,
/* NclTypeOutType lt_type;              */ Ncl_Type_DATATYPE_lt_type,
/* NclTypeOp ge; 			*/ Ncl_Type_DATATYPE_ge,
/* NclTypeOutType ge_type;              */ Ncl_Type_DATATYPE_ge_type,
/* NclTypeOp le; 			*/ Ncl_Type_DATATYPE_le,
/* NclTypeOutType le_type;              */ Ncl_Type_DATATYPE_le_type,
/* NclTypeOp ne; 			*/ Ncl_Type_DATATYPE_ne,
/* NclTypeOutType ne_type;              */ Ncl_Type_DATATYPE_ne_type,
/* NclTypeOp eq; 			*/ Ncl_Type_DATATYPE_eq,
/* NclTypeOutType eq_type;              */ Ncl_Type_DATATYPE_eq_type,
/* NclTypeOp and; 			*/ Ncl_Type_DATATYPE_and,
/* NclTypeOutType and_type;             */ Ncl_Type_DATATYPE_and_type,
/* NclTypeOp or; 			*/ Ncl_Type_DATATYPE_or,
/* NclTypeOutType or_type;              */ Ncl_Type_DATATYPE_or_type,
/* NclTypeOp xor; 			*/ Ncl_Type_DATATYPE_xor,
/* NclTypeOp xor;                       */ Ncl_Type_DATATYPE_xor_type,
/* NclNumScalarCompareFunc cmpf; 	*/ Ncl_Type_DATATYPE_cmpf,
/* NclMonotonicTestFunction is_mono; 	*/ Ncl_Type_DATATYPE_is_mono
	},
	{
		NULL
	}
};

NclObjClass nclTypeDATATYPEClass = (NclObjClass)&nclTypeDATATYPEClassRec;

NclType _NclTypeDATATYPECreate
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
	return((NclType)_NclTypeCreate(inst,theclass,obj_type,(obj_type_mask | Ncl_TypeDATATYPE), status));
}
