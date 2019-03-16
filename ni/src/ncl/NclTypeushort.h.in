
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
#ifndef NclTypeushort_h
#define NclTypeushort_h
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


typedef struct _NclTypeushortClassPart {
	char * foo;
} NclTypeushortClassPart;

typedef struct _NclTypeushortPart {
	char * foo;
} NclTypeushortPart;

typedef struct _NclTypeushortClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeushortClassPart ushortval_class;
}NclTypeushortClassRec;

typedef struct _NclTypeushortRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeushortPart	ushortval;
}NclTypeushortRec;

typedef NclTypeushortRec *NclTypeushort;
typedef NclTypeushortClassRec *NclTypeushortClass;

extern NclObjClass nclTypeushortClass;
extern NclTypeushortClassRec nclTypeushortClassRec;

NclType _NclTypeushortCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeushort_h*/
