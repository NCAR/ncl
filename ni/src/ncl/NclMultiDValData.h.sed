
/*
 *      $Id: NclMultiDValData.h.sed,v 1.4 1994-12-23 01:18:33 ethan Exp $
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
 *	Date:		Thu Jan 13 14:55:57 MST 1994
 *
 *	Description:	
 */
#ifndef NclMultiDValDATATYPEData_h
#define NclMultiDValDATATYPEData_h
#include "NclMultiDValData.h"
#include "DataSupport.h"


typedef struct _NclMultiDValDATATYPEDataPart {
	char *foo;
}NclMultiDValDATATYPEDataPart;

typedef struct _NclMultiDValDATATYPEDataRec {
	NclObjPart	obj;
	NclDataPart	data;
	NclMultiDValDataPart multidval;
	NclMultiDValDATATYPEDataPart FIELDNAME;
}NclMultiDValDATATYPEDataRec;

typedef struct _NclMultiDValDATATYPEDataClassPart {
	char *foo;
}NclMultiDValDATATYPEDataClassPart;

typedef struct _NclMultiDValDATATYPEDataClassRec {
	NclObjClassPart	obj_class;
	NclDataClassPart data_class;
	NclMultiDValDataClassPart multid_class;
	NclMultiDValDATATYPEDataClassPart FIELDNAME_class;
}NclMultiDValDATATYPEDataClassRec;

typedef struct _NclMultiDValDATATYPEDataRec* NclMultiDValDATATYPEData;
typedef struct _NclMultiDValDATATYPEDataClassRec* NclMultiDValDATATYPEDataClass;


extern NclObjClass nclMultiDValDATATYPEDataClass;

NclMultiDValData _NclMultiDValDATATYPECreate(
#if	NhlNeedProto
NclObj 		/* inst */,
NclObjClass     /* theclass */,
NclObjTypes     /* obj_type */,
unsigned int    /* obj_type_mask */,
void *          /* val */,
NclScalar *     /*missing_value*/,
int             /*n_dims*/,
int *           /*dim_sizes*/,
NclStatus       /*status*/,
NclSelectionRecord * /*sel_rec*/
#endif
);


DSPECIFIC

#endif /*NclMultiDValDATATYPEData_h */
