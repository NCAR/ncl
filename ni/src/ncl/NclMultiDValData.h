
/*
 *      $Id: NclMultiDValData.h,v 1.6.4.1 2008-03-28 20:37:51 grubin Exp $
 */
/************************************************************************
*                                                                       *
*                   Copyright (C)  1994                                 *
*           University Corporation for Atmospheric Research             *
*                   All Rights Reserved                                 *
*                                                                       *
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jan 13 14:55:46 MST 1994
 *
 *	Description:	
 */
#ifndef NclMultiDValData_h
#define NclMultiDValData_h

#include    <stddef.h>

#include "NclData.h"
#include "NclType.h"

typedef enum {
MULTID = 0,
SCALAR = 01
} NclScalarOrMd;

typedef struct _NclMissingRec {
	int has_missing;
	NclScalar value;
}NclMissingRec;

typedef struct _NclMultiDValDataPart {
	NclBasicDataTypes data_type;
	NclScalarOrMd kind;
	void *val;
	NclMissingRec missing_value;
	int n_dims;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	ng_size_t  totalsize;      /* bytes */
	ng_size_t  totalelements;  /* number of values*/
	NclSelectionRecord *sel_rec; /* Set only when creating data as a 
					subsection */
	NhlString hlu_type_rep[2];
	NclTypeClass type;
}NclMultiDValDataPart;

typedef struct _NclMultiDValDataRec {
	NclObjPart	obj;
	NclDataPart	data;
	NclMultiDValDataPart multidval;
}NclMultiDValDataRec;

typedef struct _NclMultiDValDataClassPart {
	NhlString hlu_gen_type_rep;
}NclMultiDValDataClassPart;

typedef struct _NclMultiDValDataClassRec {
	NclObjClassPart	obj_class;
	NclDataClassPart data_class;
	NclMultiDValDataClassPart multid_class;
}NclMultiDValDataClassRec;

typedef struct _NclMultiDValDataClassRec* NclMultiDValDataClass;


extern NclObjClass nclMultiDValDataClass;
extern NclMultiDValDataClassRec nclMultiDValDataClassRec;

extern struct _NclMultiDValDataRec *_NclCreateMultiDVal(
#if	NhlNeedProto
NclObj  /* inst */,
NclObjClass  /* theclass */,
NclObjTypes  /* obj_type */,
unsigned int  /* obj_type_mask */,
void * /* val */,
NclScalar * /* missing_value */,
int  /* n_dims */,
ng_size_t * /*dim_sizes */,
NclStatus  /* status */,
NclSelectionRecord * /* sel_rec */,
NclTypeClass /* type */
#endif
);



#endif /*NclMultiDValData_h */
