

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <errno.h>
#include "NclHLUObj.h"
#ifdef MAKEAPI
extern void _NclAddToDelList(
#ifdef NhlNeedProto
int /*id*/,
NclQuark /*name*/,
NhlLayerClass /*cl_ptr*/
#endif
);
extern void _NclAddToNewList(
#ifdef NhlNeedProto
int /*id*/,
NclQuark /*name*/,
NhlLayerClass /*cl_ptr*/
#endif
);
#endif /*MAKEAPI*/
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
	NclHLUObj hlu_obj = (NclHLUObj) self,ptmp;
	NclHLUChildList *step,*tmp;
 
	NclObj tmp_obj;
	if(hlu_obj != NULL) {
/*
* All of the HLU objects children will be destroyed by the NhlDestroy call
* Therefore, all NclHLUObjs that point to children must be deleted.
*/
		if(hlu_obj->hlu.c_list != NULL) {
			while(hlu_obj->hlu.c_list != NULL) {
				tmp_obj = _NclGetObj(hlu_obj->hlu.c_list->child_id);
				if(tmp_obj != NULL){
					_NclDestroyObj(tmp_obj);
				}
			}
		}
		if(hlu_obj->hlu.parent_hluobj_id > -1) {
			ptmp = (NclHLUObj)_NclGetObj(hlu_obj->hlu.parent_hluobj_id);
			if(ptmp != NULL) {
				_NclDelHLUChild(ptmp,self->obj.id);
			}
		}
#ifdef MAKEAPI
		_NclAddToDelList(hlu_obj->hlu.hlu_id,NrmStringToQuark(NhlName(hlu_obj->hlu.hlu_id)),hlu_obj->hlu.class_ptr);
#endif /* MAKEAPI */
		NhlDestroy(hlu_obj->hlu.hlu_id);
		_NclUnRegisterObj(self);
		NclFree(self);
	}
}

static NhlErrorTypes DelHLUChild
#if __STDC__
(NclHLUObj self, int child_id)
#else
(self, child_id)
NclHLUObj self;
NclQuark child_id;
#endif
{
	NclHLUChildList *step,*tmp;

	if(self->hlu.c_list != NULL) {
		while((self->hlu.c_list != NULL)&&(self->hlu.c_list->child_id == child_id)) {
			tmp = self->hlu.c_list;
			self->hlu.c_list = self->hlu.c_list->next;
			NclFree(tmp);
		}
		if(self->hlu.c_list != NULL ) {
		step = self->hlu.c_list;
			while(step->next != NULL) {
				if(step->next->child_id == child_id) {
					tmp = step->next;
					step->next = step->next->next;
					NclFree(tmp);
				} else {
					step = step->next;
				}
			}
		}
		return(NhlNOERROR);
	} else {
		return(NhlNOERROR);
	}
}

static NhlErrorTypes AddHLUChild
#if  __STDC__
(NclHLUObj self, int child_id)
#else
(self, child_id)
NclHLUObj self;
NclQuark child_id;
#endif
{
	NclHLUChildList *tmp;

	tmp = self->hlu.c_list;
	self->hlu.c_list = (NclHLUChildList*)NclMalloc((unsigned)sizeof(NclHLUChildList));
	self->hlu.c_list->next = tmp;
	self->hlu.c_list->child_id = child_id;
	return(NhlNOERROR);
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
/* foo; 	*/	DelHLUChild,
/* foo; 	*/	AddHLUChild
	}
};

NclObjClass nclHLUObjClass = (NclObjClass)&nclHLUObjClassRec;


struct _NclHLUObjRec * _NclHLUObjCreate
#if     __STDC__
(NclObj inst , NclObjClass theclass , NclObjTypes obj_type , unsigned int obj_type_mask, NclStatus status, int id,int parentid,NhlLayerClass class_ptr)
#else
(inst , theclass , obj_type ,obj_type_mask, status,id,parentid,class_ptr)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
int id;
int parentid;
NhlLayerClass class_ptr;
#endif
{
	NclHLUObj tmp,ptmp;

	if(inst == NULL) {
		tmp = (NclHLUObj)NclMalloc((unsigned)sizeof(NclHLUObjRec));
	} else {
		tmp = (NclHLUObj)inst;
	}
	tmp->hlu.parent_hluobj_id = parentid;
	tmp->hlu.hlu_id = id;
	tmp->hlu.c_list = NULL;
	tmp->hlu.class_ptr = class_ptr;
#ifdef MAKEAPI
	_NclAddToNewList(tmp->hlu.hlu_id,NrmStringToQuark(NhlName(tmp->hlu.hlu_id)),tmp->hlu.class_ptr);
#endif /*MAKEAPI*/
        (void)_NclObjCreate((NclObj)tmp , theclass , obj_type ,(obj_type_mask | Ncl_HLUObj), status);
	if(parentid > -1) {
		ptmp = (NclHLUObj)_NclGetObj(parentid);
		_NclAddHLUChild(ptmp,tmp->obj.id);
	}
	return(tmp);
}
