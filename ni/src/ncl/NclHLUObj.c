

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <errno.h>
#include "NclHLUObj.h"
static NhlErrorTypes HLUObjAddParent
#if __STDC__
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

static NhlErrorTypes HLUObjDelParent
#if __STDC__
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,"HLUObjDelParent: Attempt to delete parent from empty list");
		return(NhlFATAL);
	} 

	tmp = theobj->obj.parents;	
	if((tmp!=NULL)&&(tmp->pptr->obj.id == parent->obj.id)) {
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
			if(theobj->obj.ref_count <= 0) 
				_NclDestroyObj(theobj);
			return(NhlNOERROR);
		} else {
			tmp = tmp->next;
		}
	}
	return(NhlWARNING);
}

static void HLUObjDestroy
#if  __STDC__
(NclObj  self)
#else
(self)
	NclObj self;
#endif
{
	NclHLUObj hlu_obj = (NclHLUObj) self;
	if(hlu_obj != NULL) {
		NhlDestroy(hlu_obj->hlu.hlu_id);
		_NclUnRegisterObj(self);
		NclFree(self);
	}
}


NclHLUObjClassRec nclHLUObjClassRec = {
	{
/* char *class_name; 		*/	"HLUObj",
/* unsigned int obj_size;	*/	sizeof(NclHLUObjRec),
/* NclObjClass 			*/	(NclObjClass)&nclObjClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	HLUObjDestroy,
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	NULL,
		(NclAddParentFunction)HLUObjAddParent,
                (NclDelParentFunction)HLUObjDelParent,
	/* NclPrintFunction print; 	*/	NULL
	},
	{
/* foo; 	*/	NULL
	}
};

NclObjClass nclHLUObjClass = (NclObjClass)&nclHLUObjClassRec;


struct _NclHLUObjRec * _NclHLUObjCreate
#if     __STDC__
(NclObj inst , NclObjClass theclass , NclObjTypes obj_type , unsigned int obj_type_mask, NclStatus status, int id)
#else
(inst , theclass , obj_type ,obj_type_mask, status,id)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
int id;
#endif
{
	NclHLUObj tmp;

	if(inst == NULL) {
		tmp = (NclHLUObj)NclMalloc((unsigned)sizeof(NclHLUObjRec));
	} else {
		tmp = (NclHLUObj)inst;
	}
	tmp->hlu.hlu_id = id;
        return((NclHLUObj)_NclObjCreate((NclObj)tmp , theclass , obj_type ,(obj_type_mask | Ncl_HLUObj), status));
}
