
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
#ifndef NclTypechar_h
#define NclTypechar_h
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


typedef struct _NclTypecharClassPart {
	char * foo;
} NclTypecharClassPart;

typedef struct _NclTypecharPart {
	char * foo;
} NclTypecharPart;

typedef struct _NclTypecharClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypecharClassPart charval_class;
}NclTypecharClassRec;

typedef struct _NclTypecharRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypecharPart	charval;
}NclTypecharRec;

typedef NclTypecharRec *NclTypechar;
typedef NclTypecharClassRec *NclTypecharClass;

extern NclObjClass nclTypecharClass;
extern NclTypecharClassRec nclTypecharClassRec;

NclType _NclTypecharCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypechar_h*/
