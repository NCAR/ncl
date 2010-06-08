

/*
 *      $Id: NclFileVar.h,v 1.4 2010-04-14 21:29:47 huangwei Exp $
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
#ifndef NclFileVar_h
#define NclFileVar_h
#include "NclVar.h"

typedef struct _NclFileVarClassPart {
	char *foo;
} NclFileVarClassPart;

typedef struct _NclFileVarPart {
	char *foo;
}NclFileVarPart;
 
typedef struct _NclFileVarClassRec{
	NclObjClassPart	obj_class;
	NclVarClassPart var_class;
	NclFileVarClassPart fvar_class;
}NclFileVarClassRec;

typedef struct _NclFileVarRec {
	NclObjPart      obj;
	NclVarPart	var;
	NclFileVarPart	fvar;
}NclFileVarRec;

typedef NclFileVarRec *NclFileVar;
typedef NclFileVarClassRec *NclFileVarClass;

extern NclObjClass nclFileVarClass;

extern NclFileVarClassRec nclFileVarClassRec;

extern struct _NclVarRec *_NclFileVarCreate(
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

extern NhlErrorTypes FileVarPrint(NclObj theobj,FILE *fp);
extern NhlErrorTypes FileVarPrintVarSummary(NclObj theobj,FILE *fp);

#endif /* NclFileVar_h */
