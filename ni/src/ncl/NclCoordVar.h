

/*
 *      $Id: NclCoordVar.h,v 1.2 1995-01-28 01:51:14 ethan Exp $
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
#ifndef NclCoordVar_h
#define NclCoordVar_h
#include "NclVar.h"

typedef struct _NclCoordVarClassPart {
	char *foo;
} NclCoordVarClassPart;

typedef struct _NclCoordVarPart {
	char *foo;
}NclCoordVarPart;
 
typedef struct _NclCoordVarClassRec{
	NclObjClassPart	obj_class;
	NclVarClassPart var_class;
	NclCoordVarClassPart hvar_class;
}NclCoordVarClassRec;

typedef struct _NclCoordVarRec {
	NclObjPart      obj;
	NclVarPart	var;
	NclCoordVarPart	hvar;
}NclCoordVarRec;

typedef NclCoordVarRec *NclCoordVar;
typedef NclCoordVarClassRec *NclCoordVarClass;

extern NclObjClass nclCoordVarClass;

extern NclCoordVarClassRec nclCoordVarClassRec;

extern struct _NclVarRec *_NclCoordVarCreate(
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
	NclStatus /* status */
#endif
);

#endif /* NclCoordVar_h */
