

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <errno.h>
#include "NclHLUObj.h"
#include "HLUSupport.h"
#include "NclCallBacksI.h"

#ifdef MAKEAPI
extern void _NclAddToDelList(
#if	NhlNeedProto
int /*id*/,
NclQuark /*name*/,
NhlClass /*cl_ptr*/
#endif
);
extern void _NclAddToNewList(
#if	NhlNeedProto
int /*id*/,
NclQuark /*name*/,
NhlClass /*cl_ptr*/
#endif
);
#endif /*MAKEAPI*/
static NhlErrorTypes HLUObjAddParent
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

static NhlErrorTypes HLUObjDelParent
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
#if	NhlNeedProto
(NclObj  self)
#else
(self)
	NclObj self;
#endif
{
	NclHLUObj hlu_obj = (NclHLUObj) self,ptmp;
 
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
				hlu_obj->hlu.c_list = hlu_obj->hlu.c_list->next;
			}
		}
		if(hlu_obj->hlu.parent_hluobj_id > -1) {
			ptmp = (NclHLUObj)_NclGetObj(hlu_obj->hlu.parent_hluobj_id);
			if(ptmp != NULL) {
				_NclDelHLUChild(ptmp,self->obj.id);
			}
		}
		if(hlu_obj->obj.status != STATIC) {
#ifdef MAKEAPI
		_NclAddToDelList(hlu_obj->hlu.hlu_id,NrmStringToQuark(NhlName(hlu_obj->hlu.hlu_id)),hlu_obj->hlu.class_ptr);
#endif /* MAKEAPI */
			NhlDestroy(hlu_obj->hlu.hlu_id);
		}
		_NclUnRegisterObj(self);
		NclFree(self);
	}
}

static NhlErrorTypes DelHLUChild
#if	NhlNeedProto
(NclHLUObj self,  int child_id)
#else
(self, child_id)
NclHLUObj self;
int child_id;
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
#if	NhlNeedProto
(NclHLUObj self, int child_id)
#else
(self, child_id)
NclHLUObj self;
int child_id;
#endif
{
	NclHLUChildList *tmp;
	NclHLUObj chi = (NclHLUObj)_NclGetObj(child_id);

	tmp = self->hlu.c_list;
	self->hlu.c_list = (NclHLUChildList*)NclMalloc((unsigned)sizeof(NclHLUChildList));
	self->hlu.c_list->next = tmp;
	self->hlu.c_list->child_id = child_id;
	if(chi->hlu.parent_hluobj_id != self->obj.id) {
		chi->hlu.parent_hluobj_id  = self->obj.id;
	}
	return(NhlNOERROR);
}

static void HLUObjPrint
#if	NhlNeedProto
(NclObj self,FILE *fp)
#else
(self,fp)
NclObj self;
FILE * fp;
#endif
{
	NclHLUObj hlu_ptr = (NclHLUObj)self;
	Const char *name;
	Const char *cname;

	name = NhlName(hlu_ptr->hlu.hlu_id);
	cname = NhlClassName(hlu_ptr->hlu.hlu_id);

	nclfprintf(fp,"%s\t%s\t%d", name,cname,hlu_ptr->hlu.hlu_id);
}

static NhlErrorTypes InitializeHLUObjClass(
#if	NhlNeedProto
void
#endif
);


static void *HLUObjObtainCallData
#if NhlNeedProto
(NclObj obj, unsigned int type)
#else
(obj, type)
NclObj obj;
unsigned int type;
#endif
{
	NclHLUObjClassInfo *tmp = NclMalloc(sizeof(NclHLUObjClassInfo));
	NclHLUObj hlu = (NclHLUObj)obj;
	NclHLUObj parent = NULL;

	tmp->obj.obj_id = obj->obj.id;
	tmp->obj.obj_type = NCLHLUObj;
	tmp->hluobj.hlu_id = hlu->hlu.hlu_id;
	if(hlu->hlu.parent_hluobj_id > 0) {
		parent = (NclHLUObj)_NclGetObj(hlu->hlu.parent_hluobj_id);
		tmp->hluobj.parent_hluobj_id = parent->hlu.hlu_id;
	}
	tmp->hluobj.class_ptr = hlu->hlu.class_ptr;
	tmp->hluobj.hlu_name = NrmStringToQuark(NhlName(hlu->hlu.hlu_id));

	return((void*)tmp);
	
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
/* NclInitClassFunction initialize_class; 	*/	InitializeHLUObjClass,
		(NclAddParentFunction)HLUObjAddParent,
                (NclDelParentFunction)HLUObjDelParent,
	/* NclPrintFunction print; 	*/	HLUObjPrint,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   HLUObjObtainCallData
	},
	{
/* foo; 	*/	DelHLUChild,
/* foo; 	*/	AddHLUChild
	}
};

NclObjClass nclHLUObjClass = (NclObjClass)&nclHLUObjClassRec;

static NhlErrorTypes InitializeHLUObjClass
#if  NhlNeedProto
(void)
#else
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_HLUObj,
		(NclObjClass)&nclHLUObjClassRec		
	);
	return(NhlNOERROR);
}
struct _NclHLUObjRec * _NclHLUObjCreate
#if	NhlNeedProto
(NclObj inst , NclObjClass theclass , NclObjTypes obj_type , unsigned int obj_type_mask, NclStatus status, int id,int parentid,NhlClass class_ptr)
#else
(inst , theclass , obj_type ,obj_type_mask, status,id,parentid,class_ptr)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
int id;
int parentid;
NhlClass class_ptr;
#endif
{
	NclHLUObj tmp,ptmp;
	NclObjClass	cptr = (theclass ? theclass : nclHLUObjClass);

	if(inst == NULL) {
		tmp = (NclHLUObj)NclMalloc((unsigned)sizeof(NclHLUObjRec));
	} else {
		tmp = (NclHLUObj)inst;
	}
	tmp->hlu.parent_hluobj_id = -1;
	tmp->hlu.hlu_id = id;
	tmp->hlu.c_list = NULL;
	tmp->hlu.class_ptr = class_ptr;
#ifdef MAKEAPI
	_NclAddToNewList(tmp->hlu.hlu_id,NrmStringToQuark(NhlName(tmp->hlu.hlu_id)),tmp->hlu.class_ptr);
#endif /*MAKEAPI*/
        (void)_NclObjCreate((NclObj)tmp , cptr , obj_type ,(obj_type_mask | Ncl_HLUObj), status);
	tmp->hlu.parent_hluobj_id = parentid;
	if(parentid > -1) {
		ptmp = (NclHLUObj)_NclGetObj(parentid);
		_NclAddHLUChild(ptmp,tmp->obj.id);
	}
	if(cptr == nclHLUObjClass) {
		_NclCallCallBacks((NclObj)tmp,CREATED);
	}
	return(tmp);
}
