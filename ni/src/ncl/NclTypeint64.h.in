
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
#ifndef NclTypeint64_h
#define NclTypeint64_h
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


typedef struct _NclTypeint64ClassPart {
	char * foo;
} NclTypeint64ClassPart;

typedef struct _NclTypeint64Part {
	char * foo;
} NclTypeint64Part;

typedef struct _NclTypeint64ClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeint64ClassPart int64val_class;
}NclTypeint64ClassRec;

typedef struct _NclTypeint64Rec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeint64Part	int64val;
}NclTypeint64Rec;

typedef NclTypeint64Rec *NclTypeint64;
typedef NclTypeint64ClassRec *NclTypeint64Class;

extern NclObjClass nclTypeint64Class;
extern NclTypeint64ClassRec nclTypeint64ClassRec;

NclType _NclTypeint64Create(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeint64_h*/
