
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
#ifndef NclTypeshort_h
#define NclTypeshort_h
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


typedef struct _NclTypeshortClassPart {
	char * foo;
} NclTypeshortClassPart;

typedef struct _NclTypeshortPart {
	char * foo;
} NclTypeshortPart;

typedef struct _NclTypeshortClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeshortClassPart shortval_class;
}NclTypeshortClassRec;

typedef struct _NclTypeshortRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeshortPart	shortval;
}NclTypeshortRec;

typedef NclTypeshortRec *NclTypeshort;
typedef NclTypeshortClassRec *NclTypeshortClass;

extern NclObjClass nclTypeshortClass;
extern NclTypeshortClassRec nclTypeshortClassRec;

NclType _NclTypeshortCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeshort_h*/
