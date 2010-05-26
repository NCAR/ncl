
/*
 *      $Id: NclMultiDValnclfileData.h,v 1.1.24.1 2008-03-28 20:37:51 grubin Exp $
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
#ifndef NclMultiDValnclfileData_h
#define NclMultiDValnclfileData_h
#include "defs.h"
#include "NclMultiDValData.h"



typedef struct _NclMultiDValnclfileDataPart {
	char *space_holder;
}NclMultiDValnclfileDataPart;

typedef struct _NclMultiDValnclfileDataRec {
	NclObjPart	obj;
	NclDataPart	data;
	NclMultiDValDataPart multidval;
	NclMultiDValnclfileDataPart multid_file;
}NclMultiDValnclfileDataRec;

typedef struct _NclMultiDValnclfileDataClassPart {
	char *foo;
}NclMultiDValnclfileDataClassPart;

typedef struct _NclMultiDValnclfileDataClassRec {
	NclObjClassPart	obj_class;
	NclDataClassPart data_class;
	NclMultiDValDataClassPart multid_class;
	NclMultiDValnclfileDataClassPart multid_file_class;
}NclMultiDValnclfileDataClassRec;

typedef struct _NclMultiDValnclfileDataRec* NclMultiDValnclfileData;
typedef struct _NclMultiDValnclfileDataClassRec* NclMultiDValnclfileDataClass;


extern NclObjClass nclMultiDValnclfileDataClass;
extern NclMultiDValnclfileDataClassRec nclMultiDValnclfileDataClassRec;

extern struct _NclMultiDValDataRec *_NclMultiDValnclfileDataCreate(
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



#endif /*NclMultiDValnclfileData_h */
