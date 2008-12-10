
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
#ifndef NclTypeDATATYPE_h
#define NclTypeDATATYPE_h
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


typedef struct _NclTypeDATATYPEClassPart {
	char * foo;
} NclTypeDATATYPEClassPart;

typedef struct _NclTypeDATATYPEPart {
	char * foo;
} NclTypeDATATYPEPart;

typedef struct _NclTypeDATATYPEClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeDATATYPEClassPart FIELDNAME_class;
}NclTypeDATATYPEClassRec;

typedef struct _NclTypeDATATYPERec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeDATATYPEPart	FIELDNAME;
}NclTypeDATATYPERec;

typedef NclTypeDATATYPERec *NclTypeDATATYPE;
typedef NclTypeDATATYPEClassRec *NclTypeDATATYPEClass;

extern NclObjClass nclTypeDATATYPEClass;
extern NclTypeDATATYPEClassRec nclTypeDATATYPEClassRec;

NclType _NclTypeDATATYPECreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeDATATYPE_h*/
