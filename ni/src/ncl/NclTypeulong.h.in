
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
#ifndef NclTypeulong_h
#define NclTypeulong_h
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


typedef struct _NclTypeulongClassPart {
	char * foo;
} NclTypeulongClassPart;

typedef struct _NclTypeulongPart {
	char * foo;
} NclTypeulongPart;

typedef struct _NclTypeulongClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeulongClassPart ulongval_class;
}NclTypeulongClassRec;

typedef struct _NclTypeulongRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeulongPart	ulongval;
}NclTypeulongRec;

typedef NclTypeulongRec *NclTypeulong;
typedef NclTypeulongClassRec *NclTypeulongClass;

extern NclObjClass nclTypeulongClass;
extern NclTypeulongClassRec nclTypeulongClassRec;

NclType _NclTypeulongCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeulong_h*/
