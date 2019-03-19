
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
#ifndef NclTypeuint_h
#define NclTypeuint_h
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


typedef struct _NclTypeuintClassPart {
	char * foo;
} NclTypeuintClassPart;

typedef struct _NclTypeuintPart {
	char * foo;
} NclTypeuintPart;

typedef struct _NclTypeuintClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeuintClassPart uintval_class;
}NclTypeuintClassRec;

typedef struct _NclTypeuintRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeuintPart	uintval;
}NclTypeuintRec;

typedef NclTypeuintRec *NclTypeuint;
typedef NclTypeuintClassRec *NclTypeuintClass;

extern NclObjClass nclTypeuintClass;
extern NclTypeuintClassRec nclTypeuintClassRec;

NclType _NclTypeuintCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeuint_h*/
