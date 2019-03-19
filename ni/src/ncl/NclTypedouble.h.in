
/*
 *      $Id: NclType.h.sed,v 1.2 2008-12-10 20:12:17 dbrown Exp $
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
 *	Date:		Fri Jan 27 18:25:39 MST 1995
 *
 *	Description:	
 */
#ifndef NclTypedouble_h
#define NclTypedouble_h
#ifdef NIO_LIB_ONLY
#include "nioConvertP.h"
#else
#include <ncarg/hlu/ConvertP.h>
#endif
#include "defs.h"
#include "NclType.h"
#include "NclTypedouble.h"
#include <math.h>
#include "NclMultiDValData.h"
#include "DataSupport.h"


typedef struct _NclTypedoubleClassPart {
	char * foo;
} NclTypedoubleClassPart;

typedef struct _NclTypedoublePart {
	char * foo;
} NclTypedoublePart;

typedef struct _NclTypedoubleClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypedoubleClassPart doubleval_class;
}NclTypedoubleClassRec;

typedef struct _NclTypedoubleRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypedoublePart	doubleval;
}NclTypedoubleRec;

typedef NclTypedoubleRec *NclTypedouble;
typedef NclTypedoubleClassRec *NclTypedoubleClass;

extern NclObjClass nclTypedoubleClass;
extern NclTypedoubleClassRec nclTypedoubleClassRec;

NclType _NclTypedoubleCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypedouble_h*/
