
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
#ifndef NclTypelogical_h
#define NclTypelogical_h
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


typedef struct _NclTypelogicalClassPart {
	char * foo;
} NclTypelogicalClassPart;

typedef struct _NclTypelogicalPart {
	char * foo;
} NclTypelogicalPart;

typedef struct _NclTypelogicalClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypelogicalClassPart logicalval_class;
}NclTypelogicalClassRec;

typedef struct _NclTypelogicalRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypelogicalPart	logicalval;
}NclTypelogicalRec;

typedef NclTypelogicalRec *NclTypelogical;
typedef NclTypelogicalClassRec *NclTypelogicalClass;

extern NclObjClass nclTypelogicalClass;
extern NclTypelogicalClassRec nclTypelogicalClassRec;

NclType _NclTypelogicalCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypelogical_h*/
