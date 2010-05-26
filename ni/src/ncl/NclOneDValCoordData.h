
/*
 *      $Id: NclOneDValCoordData.h,v 1.1.24.1 2008-03-28 20:37:51 grubin Exp $
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
#ifndef NclOneDValCoordData_h
#define NclOneDValCoordData_h
#include "defs.h"
#include "NclMultiDValData.h"



typedef struct _NclOneDValCoordDataPart {
	NclMonoTypes mono_type;
}NclOneDValCoordDataPart;

typedef struct _NclOneDValCoordDataRec {
	NclObjPart	obj;
	NclDataPart	data;
	NclMultiDValDataPart multidval;
	NclOneDValCoordDataPart onedval;
}NclOneDValCoordDataRec;

typedef NhlErrorTypes (*NclCoordClosestFunction)(
#if 	NhlNeedProto
	NclMultiDValData /* self_md */,
	void *		/* val_md*/,
	long	*	 /* ind */
#endif
);

typedef NhlErrorTypes (*NclCoordRangeFunction)(
#if 	NhlNeedProto
	NclMultiDValData /* self_md */,
	void *		/* start_md*/,
	void *		/* finish_md*/,
	long	*	 /* start*/,
	long	* 	 /* finsh*/
#endif
);
typedef struct _NclOneDValCoordDataClassPart {
	NclCoordRangeFunction get_range_ind;
	NclCoordClosestFunction get_closest_ind;
}NclOneDValCoordDataClassPart;

typedef struct _NclOneDValCoordDataClassRec {
	NclObjClassPart	obj_class;
	NclDataClassPart data_class;
	NclMultiDValDataClassPart multid_class;
	NclOneDValCoordDataClassPart oned_class;
}NclOneDValCoordDataClassRec;

typedef struct _NclOneDValCoordDataRec* NclOneDValCoordData;
typedef struct _NclOneDValCoordDataClassRec* NclOneDValCoordDataClass;


extern NclObjClass nclOneDValCoordDataClass;
extern NclOneDValCoordDataClassRec nclOneDValCoordDataClassRec;

extern struct _NclMultiDValDataRec *_NclOneDValCoordDataCreate(
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
NclSelectionRecord * /*sel_rec*/,
NclTypeClass 	/*type*/
#endif
);



#endif /*NclOneDValCoordData_h */
