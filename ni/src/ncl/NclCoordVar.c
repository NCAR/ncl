

/*
 *      $Id: NclCoordVar.c,v 1.2 1994-12-23 01:18:09 ethan Exp $
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
 *	Date:		Thu Jan 13 15:04:25 MST 1994
 *
 *	Description:	
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include "DataSupport.h"
#include "NclCoordVar.h"


NclCoordVarClassRec nclCoordVarClassRec = {
	{
		"NclCoordVarClass",
		sizeof(NclCoordVarRec),
		(NclObjClass)&nclVarClassRec,
		0,
		(NclGenericFunction)NULL,
		(NclSetStatusFunction)NULL /*VarSetStatus*/,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)NULL,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
/* NclPrintFunction print */	NULL
	},
	{
/* NclRepValueFunc rep_val */		NULL,
/* NclGetValFunc get_val*/		NULL,
/* NclVarCoerceFunc */			NULL,
/* NclCopyVarFunc */			NULL,

/* NclAssignFunction write_func */	NULL,
/* NclAssignVarToVarFunction write_vv_func */	NULL,
/* NclReadFunction read_func */		NULL,
/* NclReadValueFunction read_func */	NULL,

/* NclReadAttribute read_att_func*/	NULL,
/* NclIsA is_att_func*/			NULL,
/* NclWriteAttribute write_att_func*/	NULL,

/* NclIsAFunc is_dim_func */		NULL,
/* NclReadDimension read_dim_func*/	NULL,
/* NclGetDimInfo read_dim_func*/	NULL,
/* NclWriteDimension write_dim_func*/	NULL,

/* NclIsAFunc is_coord_func */		NULL,
/* NclReadCoordinate read_coordinate*/	NULL,
/* NclWriteCoordinate write_coordinate*/ NULL
	},
	{
		NULL
	}
};

NclObjClass nclCoordVarClass = (NclObjClass)&nclCoordVarClassRec;





struct _NclVarRec *_NclCoordVarCreate
#if	NhlNeedProto
(NclVar inst,
NclObjClass theclass,
NclObjTypes obj_type,
unsigned int obj_type_mask,
struct _NclSymbol *thesym,
struct _NclMultiDValDataRec * value,
NclDimRec *dim_info,
int att_id,
int *coords,
NclVarTypes var_type,
char *var_name)
#else
(inst,theclass,obj_type,obj_type_mask,thesym,value,dim_info,att_id,coords,var_type,var_name)
	NclVar inst;
	NclObjClass theclass;
	NclObjTypes obj_type;
	unsigned int obj_type_mask;
	struct _NclSymbol *thesym;
	struct _NclMultiDValDataRec *value;
	NclDimRec *dim_info;
	int att_id;
	int *coords;
	NclVarTypes var_type;
	char *var_name;
#endif
{
	NclCoordVar cvar = NULL;
	
	return(_NclVarCreate(inst,theclass,obj_type,obj_type_mask,thesym,value,dim_info,att_id,coords,var_type,var_name));
}

