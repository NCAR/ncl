
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
#ifndef NclTypeubyte_h
#define NclTypeubyte_h
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


typedef struct _NclTypeubyteClassPart {
	char * foo;
} NclTypeubyteClassPart;

typedef struct _NclTypeubytePart {
	char * foo;
} NclTypeubytePart;

typedef struct _NclTypeubyteClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeubyteClassPart ubyteval_class;
}NclTypeubyteClassRec;

typedef struct _NclTypeubyteRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeubytePart	ubyteval;
}NclTypeubyteRec;

typedef NclTypeubyteRec *NclTypeubyte;
typedef NclTypeubyteClassRec *NclTypeubyteClass;

extern NclObjClass nclTypeubyteClass;
extern NclTypeubyteClassRec nclTypeubyteClassRec;

NclType _NclTypeubyteCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeubyte_h*/
