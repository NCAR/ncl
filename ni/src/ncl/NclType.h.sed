
/*
 *      $Id: NclType.h.sed,v 1.1 1995-01-28 01:52:09 ethan Exp $
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
#include "NclType.h"


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
