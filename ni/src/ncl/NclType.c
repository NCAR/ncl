
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
 *	Date:		Fri Jan 27 18:23:46 MST 1995
 *
 *	Description:	
 */

#ifdef NIO_LIB_ONLY
#include "niohluP.h"
#include "nioNresDB.h"
#include "nioConvertP.h"
#else
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/ConvertP.h>
#endif
#include "defs.h"
#include "NclType.h"
/*
 * This macro is used because most of the converters end the same way.
 */
#define _NhlSetVal(type,sz,value)                               \
{                                                               \
        if((to->size > 0) && (to->data.ptrval != NULL)){        \
                                                                \
                /* caller provided space */                     \
                                                                \
                if(to->size < sz){                              \
                        /* Not large enough */                  \
                        to->size = (unsigned int)sz;            \
                        return(NhlFATAL);                       \
                }                                               \
                                                                \
                /* give caller copy */                          \
                                                                \
                to->size = (unsigned int)sz;                    \
                *((type *)(to->data.ptrval)) = value;           \
                return(ret);                                    \
        }                                                       \
        else{                                                   \
                                                                \
        /* caller didn't provide space - give pointer   */      \
        /* into static data - if they modify it they    */      \
        /* may die.                                     */      \
                                                                \
                static type val;                                \
                                                                \
                to->size = sz;                                  \
                val = value;                                    \
                to->data.ptrval = &val;                         \
                return(ret);                                    \
        }                                                       \
}


static NhlErrorTypes NhlCvtGenArrayToGenArray
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from,to,args,nargs)                                                    
        NrmValue                *from;                                  
        NrmValue                *to;                                    
        NhlConvertArgList       args;                                   
        int                     nargs;
#endif
{
        NhlGenArray     gen;
        char            func[] = "NhlCvtGenArrayToGenArray";
        char            buff[_NhlMAXRESNAMLEN];
        NrmQuark        newfromQ;
        NhlErrorTypes   ret = NhlNOERROR;
	NrmQuark	varQ  = NrmStringToQuark(NhlTVariable) ;
	NrmQuark	genQ = NrmStringToQuark(NhlTGenArray);
 
        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "%s:called with wrong number of args",func);
                to->size = 0;
                return NhlFATAL;
        }
 
        gen = from->data.ptrval;
 
        /*
         * if to GenArray, then all specialized GenArrays are valid and
         * no conversion is really necessary.
         */
        if(to->typeQ == genQ){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
        }
 
        /*
         * if from is not a GenArray, then this converter was already called
         * to get the more specific name.  This ends the recursion.
         */
        if((from->typeQ != genQ ) && (from->typeQ != varQ)){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "%s:Need a converter from %s to %s",func,
                                                NrmQuarkToString(from->typeQ),
                                                NrmQuarkToString(to->typeQ));
                return NhlFATAL;
        }
 
        /*
         * We need a more specific name for the from GenArray so the
         * specific converters can be called.
         */
	if(gen == NULL) {
        	strcpy(buff,"Float");
        	strcat(buff,NhlTGenArray);
        	newfromQ = NrmStringToQuark(buff);
	} else {
	        strcpy(buff,NrmQuarkToString(gen->typeQ));
        	strcat(buff,NhlTGenArray);
        	newfromQ = NrmStringToQuark(buff);
	}
        /*
         * If they are now equal, then just set.
         */
        if(newfromQ == to->typeQ){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
        }
        return _NhlReConvertData(newfromQ,to->typeQ,from,to);
}

static NhlErrorTypes Ncl_Type_InitClass
#if     NhlNeedProto
(void)
#else
()
#endif
{
        NhlRegisterConverter(NhlbaseClass,NhlTGenArray,NhlTNclData,
                NhlCvtGenArrayToGenArray,NULL,0,False,NULL);
        return(NhlNOERROR);
}


NclTypeClassRec nclTypeClassRec = {
	{
		"NclTypeClass",
		sizeof(NclTypeRec),
		(NclObjClass)&nclObjClassRec,
		0,
		NULL,
		NULL,
		NULL,
		Ncl_Type_InitClass,
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
/* ng_size_t size;			*/ 0,
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
