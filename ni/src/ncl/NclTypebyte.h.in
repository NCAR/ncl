
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
#ifndef NclTypebyte_h
#define NclTypebyte_h
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


typedef struct _NclTypebyteClassPart {
	char * foo;
} NclTypebyteClassPart;

typedef struct _NclTypebytePart {
	char * foo;
} NclTypebytePart;

typedef struct _NclTypebyteClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypebyteClassPart byteval_class;
}NclTypebyteClassRec;

typedef struct _NclTypebyteRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypebytePart	byteval;
}NclTypebyteRec;

typedef NclTypebyteRec *NclTypebyte;
typedef NclTypebyteClassRec *NclTypebyteClass;

extern NclObjClass nclTypebyteClass;
extern NclTypebyteClassRec nclTypebyteClassRec;

NclType _NclTypebyteCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypebyte_h*/
