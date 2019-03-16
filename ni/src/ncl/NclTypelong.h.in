
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
#ifndef NclTypelong_h
#define NclTypelong_h
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


typedef struct _NclTypelongClassPart {
	char * foo;
} NclTypelongClassPart;

typedef struct _NclTypelongPart {
	char * foo;
} NclTypelongPart;

typedef struct _NclTypelongClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypelongClassPart longval_class;
}NclTypelongClassRec;

typedef struct _NclTypelongRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypelongPart	longval;
}NclTypelongRec;

typedef NclTypelongRec *NclTypelong;
typedef NclTypelongClassRec *NclTypelongClass;

extern NclObjClass nclTypelongClass;
extern NclTypelongClassRec nclTypelongClassRec;

NclType _NclTypelongCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypelong_h*/
