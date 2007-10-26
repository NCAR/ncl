

/*
 *      $Id: NclHLUVar.h,v 1.4 2007-10-26 18:39:18 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
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
 *	Date:		Thu Jan 13 15:04:41 MST 1994
 *
 *	Description:	
 */
#ifndef NclHLUVar_h
#define NclHLUVar_h
#include "NclVar.h"

typedef struct _NclHLUVarClassPart {
	char *foo;
} NclHLUVarClassPart;

typedef struct _NclHLUVarPart {
	_NhlCB cb;
        void  *udata;
}NclHLUVarPart;
 
typedef struct _NclHLUVarClassRec{
	NclObjClassPart	obj_class;
	NclVarClassPart var_class;
	NclHLUVarClassPart hvar_class;
}NclHLUVarClassRec;

typedef struct _NclHLUVarRec {
	NclObjPart      obj;
	NclVarPart	var;
	NclHLUVarPart	hvar;
}NclHLUVarRec;

typedef NclHLUVarRec *NclHLUVar;
typedef NclHLUVarClassRec *NclHLUVarClass;

extern NclObjClass nclHLUVarClass;

extern NclHLUVarClassRec nclHLUVarClassRec;

extern struct _NclVarRec *_NclHLUVarCreate(
#if     NhlNeedProto
        struct _NclVarRec *     /* inst */,
        struct _NclObjClassRec *        /* theclass */,
        NclObjTypes     /* obj_type */,
        unsigned int    /* obj_type_mask */,
        struct _NclSymbol  * /* thesym */,
        struct _NclMultiDValDataRec * /* value */,
        struct _NclDimRec * /*dim_info*/,
        int             /*att_id*/,
        int*            /*coords*/,
        NclVarTypes /* var_type */,
        char * /*var_name*/,
	NclStatus /*status*/
#endif
);

#endif /* NclHLUVar_h */
