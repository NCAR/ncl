
/*
 *      $Id: NclMultiDValData.c,v 1.3 1994-12-23 01:18:30 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
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
 *	Date:		Fri Oct 29 11:36:10 MDT 1993
 *
 *	Description:	
 */

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <errno.h>
#include "NclMultiDValData.h"

static NhlErrorTypes MultiDValAddParent
#if	NhlNeedProto
(NclObj theobj, NclObj parent)
#else 
(theobj, parent)
NclObj theobj;
NclObj parent;
#endif
{
	NclRefList * tmp = NULL;

	tmp = theobj->obj.parents;
	theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
	theobj->obj.parents->next = tmp;
	theobj->obj.parents->pptr = parent;
	theobj->obj.ref_count++;
	return(NhlNOERROR);
}

static NhlErrorTypes MultiDValDelParent
#if	NhlNeedProto
(NclObj theobj, NclObj parent)
#else 
(theobj, parent)
NclObj theobj;
NclObj parent;
#endif
{
	NclRefList *tmp,*tmp1;
	int found = 0;

	if(theobj->obj.parents == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"MultiDValDelParent: Attempt to delete parent from empty list");
		return(NhlFATAL);
	} 

	tmp = theobj->obj.parents;	
	while((tmp!=NULL)&&(tmp->pptr->obj.id == parent->obj.id)) {
		theobj->obj.parents = theobj->obj.parents->next;
		NclFree(tmp);
		tmp = theobj->obj.parents;
		found = 1;
		theobj->obj.ref_count--;
	}
	if((tmp == NULL)&&(found)) {
		_NclDestroyObj(theobj);
		return(NhlNOERROR);
	}
	while(tmp->next != NULL) {
		if(tmp->next->pptr->obj.id == parent->obj.id) {
			found = 1;
			tmp1 = tmp->next;
			tmp->next = tmp->next->next;
			NclFree(tmp1);
			theobj->obj.ref_count--;
		} else {
			tmp = tmp->next;
		}
	}
	if(found) {
		if(theobj->obj.ref_count <= 0) 
			_NclDestroyObj(theobj);
		return(NhlNOERROR);
	} else {
		return(NhlWARNING);
	}
}

NclMultiDValDataClassRec nclMultiDValDataClassRec = {
	{
/* char *class_name; 		*/	"MultiDValData",
/* unsigned int obj_size;	*/	sizeof(NclMultiDValDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	NULL /* MultiDValDestroy */,
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	NULL,
		(NclAddParentFunction)MultiDValAddParent,
                (NclDelParentFunction)MultiDValDelParent,
	/* NclPrintFunction print; 	*/	NULL
	},
	{
/* NclGenericFunction dup; 	*/	NULL,
/* NclResetMissingValueFuction dup;	*/	NULL,
/* NclReadSubSecFunction r_subsection */ NULL,
/* NclReadSubSecFunction w_subsection */ {NULL,NULL},
/* NclReadThenWriteSubFunc w_subsection */ NULL,
/* NclDataFunction coerce; 	*/	{NULL,NULL},
/* NclDataFunction multiply; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction plus; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction minus; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction divide; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction exponent; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction mod; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction mat; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction sel_lt; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction sel_gt; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction not; 	*/	{NULL,NULL},
/* NclDataFunction neg; 	*/	{NULL,NULL},
/* NclDataFunction gt; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction lt; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction ge; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction le; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction ne; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction eq; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction and;	 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction or; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction xor;		*/	{NULL,NULL,NULL,NULL},
/* NclIsMissingFuncion is_mis   */	NULL
	},
	{
		NULL
	}
	
};

NclObjClass nclMultiDValDataClass = (NclObjClass)&nclMultiDValDataClassRec;


struct _NclMultiDValDataRec * _NclMultiDValDataCreate
#if	NhlNeedProto
(NclObj inst , NclObjClass theclass , NclObjTypes obj_type , unsigned int obj_type_mask, NclStatus status)
#else
(inst , theclass , obj_type ,obj_type_mask, status)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
#endif
{
        return((NclMultiDValData)_NclDataCreate(inst , theclass , obj_type ,(obj_type_mask | Ncl_MultiDValData), status));

}
