
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
#ifndef NclTypeobj_h
#define NclTypeobj_h
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


typedef struct _NclTypeobjClassPart {
	char * foo;
} NclTypeobjClassPart;

typedef struct _NclTypeobjPart {
	char * foo;
} NclTypeobjPart;

typedef struct _NclTypeobjClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeobjClassPart objval_class;
}NclTypeobjClassRec;

typedef struct _NclTypeobjRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeobjPart	objval;
}NclTypeobjRec;

typedef NclTypeobjRec *NclTypeobj;
typedef NclTypeobjClassRec *NclTypeobjClass;

extern NclObjClass nclTypeobjClass;
extern NclTypeobjClassRec nclTypeobjClassRec;

NclType _NclTypeobjCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeobj_h*/
