
/*
 *      $Id: NclMultiDVallistData.h,v 1.2 2010/04/14 21:29:47 huangwei Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
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
 *	Date:		Thu Jan 13 14:55:46 MST 1994
 *
 *	Description:	
 */
#ifndef NclMultiDVallistData_h
#define NclMultiDVallistData_h
#include "defs.h"
#include "NclMultiDValData.h"



typedef struct _NclMultiDVallistDataPart {
	char *space_holder;
}NclMultiDVallistDataPart;

typedef struct _NclMultiDVallistDataRec {
	NclObjPart	obj;
	NclDataPart	data;
	NclMultiDValDataPart multidval;
	NclMultiDVallistDataPart multid_list;
}NclMultiDVallistDataRec;

typedef struct _NclMultiDVallistDataClassPart {
	char *foo;
}NclMultiDVallistDataClassPart;

typedef struct _NclMultiDVallistDataClassRec {
	NclObjClassPart	obj_class;
	NclDataClassPart data_class;
	NclMultiDValDataClassPart multid_class;
	NclMultiDVallistDataClassPart multid_list_class;
}NclMultiDVallistDataClassRec;

typedef struct _NclMultiDVallistDataRec* NclMultiDVallistData;
typedef struct _NclMultiDVallistDataClassRec* NclMultiDVallistDataClass;


extern NclObjClass nclMultiDVallistDataClass;
extern NclMultiDVallistDataClassRec nclMultiDVallistDataClassRec;

extern struct _NclMultiDValDataRec *_NclMultiDVallistDataCreate(
#if	NhlNeedProto
NclObj          /* inst */,
NclObjClass     /* theclass */,
NclObjTypes     /* obj_type */,
unsigned int    /* obj_type_mask */,
void *          /* val */,
NclScalar *     /*missing_value*/,
int             /*n_dims*/,
ng_size_t *           /*dim_sizes*/,
NclStatus       /*status*/,
NclSelectionRecord * /*sel_rec*/
#endif
);

#endif /*NclMultiDVallistData_h */
