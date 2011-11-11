#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include <errno.h>
#include "NclHLUObj.h"
#include "DataSupport.h"
#include "HLUSupport.h"
#include "NclCallBacksI.h"
#include "NclMultiDValHLUObjData.h"

extern int defaultapp_hluobj_id;
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
	theobj->obj.parents->pid = parent->obj.id;
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
	if((tmp!=NULL)&&(tmp->pid == parent->obj.id)) {
		theobj->obj.parents = theobj->obj.parents->next;
		NclFree(tmp);
		tmp = theobj->obj.parents;
		found = 1;
		theobj->obj.ref_count--;
	}
	if((tmp == NULL)&&(found)) {
		_NclDestroyObj(theobj);
		return(NhlNOERROR);
	} else if(found) {
		return(NhlNOERROR);
	}
	while(tmp->next != NULL) {
		if(tmp->next->pid == parent->obj.id) {
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
	NclHLUChildList *tmp1,*tmp2;
	NclHLUExpChildList *etmp1,*etmp2;
	NclObj tmp_obj,pobj;
	NhlArgVal cbdata;
	NhlArgVal selector;
	NclRefList *parents,*tmpptr,*tmpptr2;
	int id;

	NhlINITVAR(cbdata);
	NhlINITVAR(selector);

	
	if(hlu_obj != NULL) {
/*
* All of the HLU objects children will be destroyed by the NhlDestroy call
* Therefore, all NclHLUObjs that point to children must be deleted.
*/
		id = hlu_obj->hlu.hlu_id;
		if(hlu_obj->obj.ref_count > 0) {
			parents = hlu_obj->obj.parents;
			while(parents != NULL) {
				tmpptr = parents;
				while(tmpptr->next != NULL) {
					if(tmpptr->next->pid == parents->pid) {
						tmpptr2 = tmpptr->next->next;
						NclFree(tmpptr->next);
						tmpptr->next = tmpptr2;
					} else {
						tmpptr = tmpptr->next;
					}
				}
				pobj = _NclGetObj(parents->pid);
				if(pobj != NULL) {
					if(pobj->obj.obj_type_mask & Ncl_MultiDValHLUObjData) {
						cbdata.lngval = hlu_obj->obj.id;
						selector.lngval = HLUDESTROYED;
						_NhlCBCallCallbacks(((NclMultiDValHLUObjData)pobj)->obj.cblist,selector,cbdata);
					} else if(pobj->obj.obj_type_mask & Ncl_HLUObj) {
						cbdata.lngval = hlu_obj->obj.id;
						selector.lngval = 0;
						_NhlCBCallCallbacks(((NclHLUObj)pobj)->hlu.cblist,selector,cbdata);
					}
				}
				tmpptr = parents;
				parents = parents->next;
				NclFree(tmpptr);
			}
			
		}
		if(hlu_obj->hlu.c_list != NULL) {
/*
* exp_list could change as a result of Destroy call
*/
			while(hlu_obj->hlu.c_list != NULL) {
				tmp1 = hlu_obj->hlu.c_list;
				tmp_obj = _NclGetObj(tmp1->child_id);
				tmp2 = tmp1;
				hlu_obj->hlu.c_list = tmp1->next;
/*
* This line keeps HLUObjDestroy from calling DelHLUChild on subsequent 
* this object
*/
				if(tmp_obj != NULL){
					if(((NclHLUObj)tmp_obj)->hlu.parent_hluobj_id == hlu_obj->obj.id)
						((NclHLUObj)tmp_obj)->hlu.parent_hluobj_id = -1;
					_NclDestroyObj(tmp_obj);
				}
				NclFree(tmp2);
			}
		}
		if(hlu_obj->hlu.exp_list != NULL) {
/*
* exp_list could change as a result of DelParent call
*/

			while(hlu_obj->hlu.exp_list != NULL) {
				etmp1 = hlu_obj->hlu.exp_list;
				tmp_obj = _NclGetObj(etmp1->child_id);
				etmp2 = etmp1;
				hlu_obj->hlu.exp_list = etmp1->next;
/*
* This line keeps HLUObjDestroy from calling DelHLUChild on subsequent 
* this object
*/
				_NhlCBDelete(etmp2->cb);
				NclFree(etmp2->crec);
#ifdef NCLDEBUG
					if(((NclHLUObj)tmp_obj)->hlu.parent_hluobj_id == hlu_obj->obj.id)
						((NclHLUObj)tmp_obj)->hlu.parent_hluobj_id = -1;
				if(tmp_obj != NULL){
#else
				if(tmp_obj != NULL){
					if(((NclHLUObj)tmp_obj)->hlu.parent_hluobj_id == hlu_obj->obj.id)
						((NclHLUObj)tmp_obj)->hlu.parent_hluobj_id = -1;
#endif
					_NclDelParent(tmp_obj,(NclObj)hlu_obj);
				}
				NclFree(etmp2);
			}
		}
		if(hlu_obj->hlu.parent_hluobj_id > -1) {
			ptmp = (NclHLUObj)_NclGetObj(hlu_obj->hlu.parent_hluobj_id);
			if(ptmp != NULL) {
				_NclDelHLUChild(ptmp,self->obj.id);
			}
		}
		if(hlu_obj->hlu.apcb != NULL) {
			_NhlCBDelete(hlu_obj->hlu.apcb);
		}
		_NhlCBDestroy(hlu_obj->hlu.cblist);
		if(defaultapp_hluobj_id == hlu_obj->hlu.hlu_id) 
			defaultapp_hluobj_id = -1;
		if(hlu_obj->obj.status != STATIC) {
#ifdef MAKEAPI
		_NclAddToDelList(hlu_obj->hlu.hlu_id,NrmStringToQuark(NhlName(hlu_obj->hlu.hlu_id)),hlu_obj->hlu.class_ptr);
#endif /* MAKEAPI */
			_NclRemoveAllRefs(hlu_obj->obj.id);
			NhlDestroy(hlu_obj->hlu.hlu_id);
		}
		if(self->obj.cblist != NULL) {
			_NhlCBDestroy(self->obj.cblist);
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
static void ExpDestroyNotify
#if     NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata,udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
	HLUObjCalRec *crec;
	NclHLUObj self;
	NclHLUExpChildList *tmp,*tmp2;

	crec = (HLUObjCalRec*)udata.ptrval;
	if(crec->child_id == cbdata.lngval) {
		self = (NclHLUObj)_NclGetObj(crec->parent_id);
		if(self != NULL) {
			tmp = self->hlu.exp_list;
			if(tmp->child_id == crec->child_id) {
				self->hlu.exp_list = self->hlu.exp_list->next;
				_NhlCBDelete(tmp->cb);
				NclFree(tmp->crec);
				NclFree(tmp);
				return;
			} else {
				while(tmp->next != NULL) {
					if(tmp->next->child_id == crec->child_id) {
						tmp2 = tmp->next;
						tmp->next = tmp->next->next;
					 	_NhlCBDelete(tmp2->cb);
						NclFree(tmp2->crec);
						NclFree(tmp2);	
						return;
					}
					tmp = tmp->next;
				}
			}
		}
	}
}

static NhlErrorTypes AddHLUExpChild
#if	NhlNeedProto
(NclHLUObj self, int child_id)
#else
(self, child_id)
NclHLUObj self;
int child_id;
#endif
{
	NclHLUExpChildList *tmp;
	NclHLUObj chi = (NclHLUObj)_NclGetObj(child_id);
	NhlArgVal selector;
	NhlArgVal udata;
	
	NhlINITVAR(selector);
	NhlINITVAR(udata);

	selector.lngval = 0;

	
	if(self->obj.id != child_id) {
		tmp = self->hlu.exp_list;
		while(tmp != NULL) {
			if(tmp->child_id == child_id) {
				return(NhlNOERROR);
			}
			tmp = tmp->next;
		}
		tmp = self->hlu.exp_list;
		self->hlu.exp_list = (NclHLUExpChildList*)NclMalloc((unsigned)sizeof(NclHLUExpChildList));
		self->hlu.exp_list->next = tmp;
		self->hlu.exp_list->child_id = child_id;
		self->hlu.exp_list->crec =  (HLUObjCalRec*)NclMalloc(sizeof(HLUObjCalRec));
		self->hlu.exp_list->crec->child_id = child_id;
		self->hlu.exp_list->crec->parent_id = self->obj.id;
		udata.ptrval = (NhlPointer)self->hlu.exp_list->crec;
		self->hlu.exp_list->cb = _NhlCBAdd(self->hlu.cblist,selector,ExpDestroyNotify,udata);
		_NclAddParent((NclObj)chi,(NclObj)self);
	}	

	return(NhlNOERROR);
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

	if(self->obj.id != child_id) {
		tmp = self->hlu.c_list;
		self->hlu.c_list = (NclHLUChildList*)NclMalloc((unsigned)sizeof(NclHLUChildList));
		self->hlu.c_list->next = tmp;
		self->hlu.c_list->child_id = child_id;
		if(chi->hlu.parent_hluobj_id != self->obj.id) {
			chi->hlu.parent_hluobj_id  = self->obj.id;
		}
	}
	return(NhlNOERROR);
}

static NhlErrorTypes HLUObjPrint
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
	int ret=0;

	if (hlu_ptr->hlu.hlu_id <= 0) {
		name = "no name";
		cname = "no class";
	}
	else {
		name = NhlName(hlu_ptr->hlu.hlu_id);
		cname = NhlClassName(hlu_ptr->hlu.hlu_id);
	}

	ret = nclfprintf(fp,"%s\t%s\t%d", name,cname,hlu_ptr->hlu.hlu_id);
	if(ret < 0) {
		return(NhlWARNING);
	} else {
		return(NhlNOERROR);
	}
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
	else
		tmp->hluobj.parent_hluobj_id = -1;
	tmp->hluobj.class_ptr = hlu->hlu.class_ptr;
	tmp->hluobj.hlu_name = hlu->hlu.hlu_name;

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
/* NclPrintSummaryFunction print_summary */ NULL,
/* NclPrintFunction print; 	*/	HLUObjPrint,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   HLUObjObtainCallData
	},
	{
/* foo; 	*/	DelHLUChild,
/* foo; 	*/	AddHLUChild,
			AddHLUExpChild
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

	tmp = _NclLookUpHLU(id);
	if(tmp == NULL) {
		if(inst == NULL) {
			tmp = (NclHLUObj)NclMalloc((unsigned)sizeof(NclHLUObjRec));
		} else {
			tmp = (NclHLUObj)inst;
		}
		tmp->hlu.parent_hluobj_id = -1;
		tmp->hlu.hlu_id = id;
		tmp->hlu.hlu_name = NrmStringToQuark(NhlName(tmp->hlu.hlu_id));
		tmp->hlu.c_list = NULL;
		tmp->hlu.exp_list = NULL;
		tmp->hlu.cblist = _NhlCBCreate(0,NULL,NULL,NULL,NULL);
		tmp->hlu.apcb = NULL;
		tmp->hlu.class_ptr = class_ptr;
#ifdef MAKEAPI
		_NclAddToNewList(tmp->hlu.hlu_id,tmp->hlu.hlu_name,tmp->hlu.class_ptr);
#endif /*MAKEAPI*/
		(void)_NclObjCreate((NclObj)tmp , cptr , obj_type ,(obj_type_mask | Ncl_HLUObj), status);
		tmp->hlu.parent_hluobj_id = parentid;
	}
	if(parentid > -1) {
		ptmp = (NclHLUObj)_NclGetObj(parentid);
		_NclAddHLUChild(ptmp,tmp->obj.id);
	}
	if(cptr == nclHLUObjClass) {
		_NclCallCallBacks((NclObj)tmp,CREATED);
	}
	return(tmp);
}
