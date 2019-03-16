
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
#ifndef NclTypestring_h
#define NclTypestring_h
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


typedef struct _NclTypestringClassPart {
	char * foo;
} NclTypestringClassPart;

typedef struct _NclTypestringPart {
	char * foo;
} NclTypestringPart;

typedef struct _NclTypestringClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypestringClassPart stringval_class;
}NclTypestringClassRec;

typedef struct _NclTypestringRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypestringPart	stringval;
}NclTypestringRec;

typedef NclTypestringRec *NclTypestring;
typedef NclTypestringClassRec *NclTypestringClass;

extern NclObjClass nclTypestringClass;
extern NclTypestringClassRec nclTypestringClassRec;

NclType _NclTypestringCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypestring_h*/
