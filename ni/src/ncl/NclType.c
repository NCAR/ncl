
/*
 *      $Id: NclType.c,v 1.4 1995-05-23 15:54:15 ethan Exp $
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
 *	Date:		Fri Jan 27 18:23:46 MST 1995
 *
 *	Description:	
 */

#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclType.h"


NclTypeClassRec nclTypeClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclObjClassRec,
		0,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL
	},
	{
/* NclObjTypes type;			*/ Ncl_Type,
/* NclBasicDataTypes data_type;		*/ NCL_none,
/* int size;				*/ 0,
/* char* hlu_rep_type[2];		*/ {NULL,NULL},
/* NclScalar default_mis;		*/ {0},
/* char* format		;	*/ NULL,
/* NclTypePrint            print;	*/ NULL,
/* NclTypeResetMissing reset_mis; 	*/ NULL,
/* NclTypeCoerceFunction coerce; 	*/ NULL,
/* NclTypeOp multiply; 			*/ NULL,
/* NclTypeOutSize multiply_size;	*/ NULL,
/* NclTypeOp plus; 			*/ NULL,
/* NclTypeOutSize plus_size;		*/ NULL,
/* NclTypeOp minus; 			*/ NULL,
/* NclTypeOutSize minus_size;		*/ NULL,
/* NclTypeOp divide; 			*/ NULL,
/* NclTypeOutSize divide_size;		*/ NULL,
/* NclTypeOp exponent; 			*/ NULL,
/* NclTypeOutSize exponent_size;	*/ NULL,
/* NclTypeOp mod; 			*/ NULL,
/* NclTypeOutSize mod_size;		*/ NULL,
/* NclTypeOp mat; 			*/ NULL,
/* NclTypeOutSize mat_size;		*/ NULL,
/* NclTypeOp sel_lt; 			*/ NULL,
/* NclTypeOutSize sel_lt_size;		*/ NULL,
/* NclTypeOp sel_gt; 			*/ NULL,
/* NclTypeOutSize sel_gt_size;		*/ NULL,
/* NclTypeOp not; 			*/ NULL,
/* NclTypeOutSize not_size;		*/ NULL,
/* NclTypeOp neg; 			*/ NULL,
/* NclTypeOutSize neg_size;		*/ NULL,
/* NclTypeOp gt; 			*/ NULL,
/* NclTypeOutSize gt_size;		*/ NULL,
/* NclTypeOp lt; 			*/ NULL,
/* NclTypeOutSize lt_size;		*/ NULL,
/* NclTypeOp ge; 			*/ NULL,
/* NclTypeOutSize ge_size;		*/ NULL,
/* NclTypeOp le; 			*/ NULL,
/* NclTypeOutSize le_size;		*/ NULL,
/* NclTypeOp ne; 			*/ NULL,
/* NclTypeOutSize ne_size;		*/ NULL,
/* NclTypeOp eq; 			*/ NULL,
/* NclTypeOutSize eq_size;		*/ NULL,
/* NclTypeOp and; 			*/ NULL,
/* NclTypeOutSize and_size;		*/ NULL,
/* NclTypeOp or; 			*/ NULL,
/* NclTypeOutSize or_size;		*/ NULL,
/* NclTypeOp xor; 			*/ NULL,
/* NclTypeOutSize xor_size;		*/ NULL,
/* NclNumScalarCompareFunc cmpf; 	*/ NULL,
/* NclMonotonicTestFunction is_mono; 	*/ NULL
	}
};

NclObjClass nclTypeClass = (NclObjClass)&nclTypeClassRec;

NclType _NclTypeCreate
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
	return((NclType)_NclObjCreate(inst,theclass,obj_type,(obj_type_mask | Ncl_Type), status));
}
