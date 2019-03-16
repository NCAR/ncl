
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
#ifndef NclTypeuint64_h
#define NclTypeuint64_h
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


typedef struct _NclTypeuint64ClassPart {
	char * foo;
} NclTypeuint64ClassPart;

typedef struct _NclTypeuint64Part {
	char * foo;
} NclTypeuint64Part;

typedef struct _NclTypeuint64ClassRec {
	NclObjClassPart	obj_class;
	NclTypeClassPart type_class;
	NclTypeuint64ClassPart uint64val_class;
}NclTypeuint64ClassRec;

typedef struct _NclTypeuint64Rec {
	NclObjPart	obj;
	NclTypePart	type;
	NclTypeuint64Part	uint64val;
}NclTypeuint64Rec;

typedef NclTypeuint64Rec *NclTypeuint64;
typedef NclTypeuint64ClassRec *NclTypeuint64Class;

extern NclObjClass nclTypeuint64Class;
extern NclTypeuint64ClassRec nclTypeuint64ClassRec;

NclType _NclTypeuint64Create(
#if	NhlNeedProto
NclObj /* inst */, 
NclObjClass /* theclass */,
NclObjTypes /* obj_type */, 
unsigned int /* obj_type_mask */, 
NclStatus /* status*/
#endif
);

#endif /* NclTypeuint64_h*/
