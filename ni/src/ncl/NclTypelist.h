
/*
 *      $Id: NclTypelist.h,v 1.1 1999-11-12 18:36:42 ethan Exp $
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
#ifndef NclTypelist_h
#define NclTypelist_h
#include "NclType.h"


typedef struct _NclTypelistClassPart {
	char * foo;
} NclTypelistClassPart;

typedef struct _NclTypelistPart {
	char * foo;
} NclTypelistPart;

typedef struct _NclTypelistClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypelistClassPart listval_class;
}NclTypelistClassRec;

typedef struct _NclTypelistRec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypelistPart	listval;
}NclTypelistRec;

typedef NclTypelistRec *NclTypelist;
typedef NclTypelistClassRec *NclTypelistClass;

extern NclObjClass nclTypelistClass;
extern NclTypelistClassRec nclTypelistClassRec;

NclType _NclTypelistCreate(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* list_type */, 
unsigned int /* list_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypelist_h*/
