#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include "defs.h"
#include "NclDataDefs.h"
#include "Symbol.h"
#include "NclList.h"
#include "NclMultiDValData.h"
#include "DataSupport.h"
#include "ListSupport.h"
#include "NclVar.h"
#include "NclHLUVar.h"
#include "NclFileVar.h"
#include "VarSupport.h"

static NhlErrorTypes ListPrintSummary(NclObj theobj, FILE *fp)
{
	NclList tmp_list = (NclList) theobj;
	NhlErrorTypes ret = -1;

        if(NCL_STRUCT & tmp_list->list.list_type)
	{
		ret = nclfprintf(fp,"Type: list <struct>\n");
	}
        else if(NCL_CONCAT & tmp_list->list.list_type)
	{
        	if(NCL_FIFO & tmp_list->list.list_type)
			ret = nclfprintf(fp,"Type: list <concat | fifo>\n");
        	else if(NCL_LIFO & tmp_list->list.list_type)
			ret = nclfprintf(fp,"Type: list <concat | lifo>\n");
        	else
			ret = nclfprintf(fp,"Type: list <concat>\n");
	}
        else if(NCL_JOIN & tmp_list->list.list_type)
	{
        	if(NCL_FIFO & tmp_list->list.list_type)
			ret = nclfprintf(fp,"Type: list <join | fifo>\n");
        	else if(NCL_LIFO & tmp_list->list.list_type)
			ret = nclfprintf(fp,"Type: list <join | lifo>\n");
        	else
			ret = nclfprintf(fp,"Type: list <join>\n");
	}
	else if(NCL_FIFO & tmp_list->list.list_type)
		ret = nclfprintf(fp,"Type: list <fifo>\n");
	else if(NCL_LIFO & tmp_list->list.list_type)
		ret = nclfprintf(fp,"Type: list <lifo>\n");
	else
		ret = nclfprintf(fp,"Type: list <unknown: 0%x>\n", tmp_list->list.list_type);

	if(ret < 0)
	{
		return(NhlWARNING);
	}
	ret = nclfprintf(fp,"Total items: %ld\n",(long)tmp_list->list.nelem);
	if(ret < 0)
	{
		return(NhlWARNING);
	}

	nclfprintf(fp,"\n");

	return(NhlNOERROR);
}

static NhlErrorTypes ListPrint
#if     NhlNeedProto
(NclObj list, FILE *fp)
#else
(list,fp)
NclObj list;
FILE *fp;
#endif
{
	NclList tmp_list = (NclList) list;
	NclListObjList *step;
	NclObjClass oc;
	NclObj cur_obj;
	int nv = 0;
	NhlErrorTypes ret = -1;

	ret = ListPrintSummary(list, fp);

	step = tmp_list->list.first;
	
	while(step != NULL)
	{
	    cur_obj = (NclObj)_NclGetObj(step->obj_id);
	   /* orig_type is a mask it wont work with this func 
	   *oc = _NclObjTypeToPointer(step->orig_type);
           */

	    oc = _NclObjTypeToPointer(cur_obj->obj.obj_type);

	    if(oc != NULL)
	    {
	 	ret = nclfprintf(fp,"List Item %d:\t%s", nv, oc->obj_class.class_name);
	    }
	    else
	    {
	        ret = NhlWARNING;
	    }

            if(ret < 0)
	    {
                return(NhlWARNING);
            }

	    switch(cur_obj->obj.obj_type)
	    {
	        NclObj obj;
	        case Ncl_Var:
	        case Ncl_FileVar:
			obj = _NclGetObj(cur_obj->obj.id);
			_NclPrintVarSummary((NclVar)obj);
			break;
	        case Ncl_MultiDVallistData:
	        case Ncl_List:
	 		ret = nclfprintf(fp,"\tList\n");
			break;
	        case Ncl_MultiDValData:
			/*
	 		ret = nclfprintf(fp,"\tMultiDValData\n");
			*/
			break;
	        default:
		    fprintf(stderr, "\tin file: %s, line: %d\n", __FILE__, __LINE__);
		    fprintf(stderr, "\tUNRECOGANIZED cur_obj->obj.obj_type %d: %o\n", nv, cur_obj->obj.obj_type);
	    }

	    step = step->next;
	    nv++;
 	    nclfprintf(fp,"\n");
	}

	nclfprintf(fp,"\n");

	return(NhlNOERROR);
}


static void ListDestroy
#if     NhlNeedProto
(NclObj list)
#else
(list)
NclObj list;
#endif
{
	NclList thelist = (NclList)list;
	NclListObjList  *tmp,*tmp0;


	_NclUnRegisterObj(list);
	tmp = thelist->list.first;
	while(tmp != NULL) {
		tmp0 = tmp;
		tmp = tmp->next;
		_NhlCBDelete(tmp0->cb);
		_NclDelParent(_NclGetObj(tmp0->obj_id),list);
		NclFree(tmp0);
	}
	NclFree(list);
	return;
}

static int ListGetNext
#if     NhlNeedProto
(NclObj list)
#else
(list)
NclObj list;
#endif
{
	NclList thelist = (NclList)list;
	int tmp_id;

	if(thelist != NULL) {
		if(thelist->list.state == NCL_LIST_IDLE) {
			thelist->list.state = NCL_LIST_SEQUENCING;
			thelist->list.current_item= thelist->list.last;
		} 
		if(thelist->list.current_item != NULL) {
			tmp_id = thelist->list.current_item->obj_id;
			thelist->list.current_item = thelist->list.current_item->prev;
			return(tmp_id);
		} else {
			thelist->list.state = NCL_LIST_IDLE;
			return(-1);
		}
	} else {
		return(-1);
	}
}
void ListItemDestroyNotify
#if     NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata,udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
	NclList list;
	NclListObjList *step,*tmp;
	int list_id = udata.intval;
	int obj_id = cbdata.intval;

	list = (NclList)_NclGetObj(list_id);
	

	step = list->list.first;
	if(step == NULL) return;

	while(step->obj_id == obj_id) {
		tmp = list->list.first;
		step = list->list.first->next;
		list->list.nelem--;
		list->list.first = list->list.first->next;
		if (tmp == list->list.last) {
			list->list.last = list->list.last->prev;
		}
		NclFree(tmp);
		if(list->list.first != NULL) {
			list->list.first->prev = NULL;
		} else {
			list->list.last = NULL;
			return;
		}
		if(step == NULL) {
			return;
		}
	}
	
	while(step->next != NULL) {
		if(step->next->obj_id == obj_id) {
			list->list.nelem--;
			tmp = step->next;
			if(step->next->next != NULL) {
				step->next->next->prev = step;
			} 
			step->next = step->next->next;
			if (tmp == list->list.last) {
				list->list.last = list->list.last->prev;
			}
			NhlFree(tmp);
		} else {
			step = step->next;
		}
	}

	return;
	

	
}

static void ListAttDestroyNotify
#if     NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata, udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
	NclVar listatt;

	listatt = (NclVar)_NclGetObj(udata.intval);
	if(listatt == NULL) {
		return;
	}
	listatt->var.thevalue_id = -1;
	_NclDestroyObj((NclObj)listatt);

	return;
}

NhlErrorTypes ListPush
#if     NhlNeedProto
(NclObj list,NclObj theobj)
#else
(list,theobj)
NclObj list;
NclObj theobj;
#endif
{
	NclList thelist = (NclList)list;
	NclListObjList *tmp = (NclListObjList*)NclMalloc(sizeof(NclListObjList));
	NhlErrorTypes  ret = NhlNOERROR;
	NclObj tmp_obj;
	if((thelist!=NULL)&&(theobj != NULL)) {
		tmp->orig_type = theobj->obj.obj_type_mask;
		if(theobj->obj.obj_type_mask & Ncl_Var)
		{
			tmp_obj = theobj;
		}
		else if(theobj->obj.obj_type_mask & Ncl_MultiDValnclfileData)
		{
			tmp_obj= (NclObj)_NclFileVarCreate(NULL,NULL,Ncl_FileVar,0,NULL,
							   (NclMultiDValData)theobj,NULL,-1,NULL,NORMAL,NULL,PERMANENT);
		}
		else if (theobj->obj.obj_type_mask & Ncl_MultiDValHLUObjData)
		{
			tmp_obj= (NclObj)_NclHLUVarCreate(NULL,NULL,Ncl_HLUVar,0,NULL,
							  (NclMultiDValData)theobj,NULL,-1,NULL,NORMAL,NULL,PERMANENT);
		}
		else
		{
			NclObj tmp_parent_obj;
			NclRefList *p;
			if (theobj->obj.parents) {  
				tmp_obj = theobj;
				for (p = theobj->obj.parents; p; p = p->next) {
					tmp_parent_obj = _NclGetObj(p->pid);
					if (tmp_parent_obj->obj.obj_type_mask & Ncl_Att) {
						tmp_obj= (NclObj)_NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,(NclMultiDValData)theobj, 
									       NULL,-1,NULL,ATTVALLINK,NULL,PERMANENT);
						((NclVar)tmp_obj)->var.att_cb = _NclAddCallback((NclObj)theobj,(NclObj)tmp_obj,
												ListAttDestroyNotify,DESTROYED,NULL);
						break;
					}
				}
			}
			else {
				tmp_obj= (NclObj)_NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,
							       (NclMultiDValData)theobj, NULL,-1,NULL,NORMAL,NULL,PERMANENT);
			}
		}

		/*
		 * These lines cause problems when individual files from a list are accessed:
                 * i.e. isfilevar(f[0],var) returns an error rather than True or False
                 *
		     tmp_obj->obj.obj_type_mask = theobj->obj.obj_type;
		     tmp->orig_type = theobj->obj.obj_type;
                */
			
		ret = _NclAddParent(tmp_obj,list);
		tmp->cb = _NclAddCallback( tmp_obj, list, ListItemDestroyNotify,DESTROYED,NULL);

		if(tmp_obj->obj.status == TEMPORARY) {
			_NclSetStatus(tmp_obj,PERMANENT);
		}
		if(ret != NhlNOERROR) {
			return(ret);
		}
		tmp->obj_id = tmp_obj->obj.id;

		/*if(thelist->list.list_type & NCL_FIFO) {*/
			tmp->next = thelist->list.first;
			if(thelist->list.first == NULL) {
				thelist->list.first = tmp;
				thelist->list.first->prev = NULL;	
				thelist->list.last = tmp;
			} else {
				thelist->list.first->prev = tmp;
				thelist->list.first = tmp;
				tmp->prev = NULL;
			}
		/*} else {
			if(thelist->list.last == NULL) {
				thelist->list.first = tmp;
				thelist->list.first->prev = NULL;	
				thelist->list.last = tmp;
				tmp->next = NULL;
			} else {
				if(thelist->list.last == thelist->list.first) 
					thelist->list.first->next = tmp;
				tmp->prev = thelist->list.last;
				thelist->list.last = tmp;
				tmp->next = NULL;
			}
		}*/
		thelist->list.nelem++;
		return(NhlNOERROR);
	} else {
		return(NhlFATAL);
	}
}
static NclObj ListPop
#if     NhlNeedProto
(NclObj list)
#else
(list)
NclObj list;
#endif
{
	NclList thelist = (NclList)list;
	NclObj tmp,ret_obj;
	NclListObjList *tmp_list;

	if((thelist != NULL)&&(thelist->list.first != NULL)&&(thelist->list.last != NULL) ) {
		if(thelist->list.list_type & NCL_FIFO) {
			tmp_list = thelist->list.last;
			if(tmp_list == thelist->list.current_item) {
				thelist->list.current_item = tmp_list->next;
			}
			if(thelist->list.last->prev != NULL) {
				thelist->list.last->prev->next = NULL;
			}
			thelist->list.last = thelist->list.last->prev;
			if(thelist->list.nelem == 1)  {
				thelist->list.first = thelist->list.last;
			}
		} else {
			tmp_list = thelist->list.first;
			if(tmp_list == thelist->list.current_item) {
				thelist->list.current_item = tmp_list->prev;
			}
			if(thelist->list.first->next != NULL) {
				thelist->list.first->next->prev = NULL;
			}
			thelist->list.first = thelist->list.first->next;
			if(thelist->list.nelem == 1) {
				thelist->list.last = thelist->list.first;
			}
		}
		thelist->list.nelem--;
		tmp = _NclGetObj(tmp_list->obj_id);
		if(tmp != NULL) {
			_NhlCBDelete(tmp_list->cb);
			if(tmp_list->orig_type & Ncl_MultiDValData) {
				tmp->obj.status = TEMPORARY;
				ret_obj = (NclObj)_NclStripVarData((NclVar)tmp);

				_NclDestroyObj(tmp);
			} else {
				_NclDelParent(tmp,list);
				ret_obj =  _NclGetObj(tmp_list->obj_id);
			}
		}
		NclFree(tmp_list);
		return(ret_obj);
	} else {
		return(NULL);
	}
}

NhlErrorTypes ListAppend
#if     NhlNeedProto
(NclObj list,NclObj theobj)
#else
(list,theobj)
NclObj list;
NclObj theobj;
#endif
{
	NclList thelist = (NclList)list;
	NclListObjList *tmp = (NclListObjList*)NclMalloc(sizeof(NclListObjList));
	NhlErrorTypes  ret = NhlNOERROR;
	NclObj tmp_obj;
	if((thelist!=NULL)&&(theobj != NULL)) {
		tmp->orig_type = theobj->obj.obj_type_mask;
		if(theobj->obj.obj_type_mask & Ncl_Var)
		{
			tmp_obj = theobj;
		}
		else if(theobj->obj.obj_type_mask & Ncl_MultiDValnclfileData)
		{
			tmp_obj= (NclObj)_NclFileVarCreate(NULL,NULL,Ncl_FileVar,0,NULL,
							   (NclMultiDValData)theobj,NULL,-1,NULL,NORMAL,NULL,PERMANENT);
		}
		else if (theobj->obj.obj_type_mask & Ncl_MultiDValHLUObjData)
		{
			tmp_obj= (NclObj)_NclHLUVarCreate(NULL,NULL,Ncl_HLUVar,0,NULL,
							  (NclMultiDValData)theobj,NULL,-1,NULL,NORMAL,NULL,PERMANENT);
		}
#if 0
                else if (theobj->obj.obj_type_mask & Ncl_MultiDVallistData)
                {
                        tmp_obj = theobj;
                }
                else if (theobj->obj.obj_type_mask & Ncl_MultiDValData)
                {
                        tmp_obj = (NclObj)_NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,(NclMultiDValData)theobj,
                                                         NULL,-1,NULL,NORMAL,NULL,PERMANENT);
                }
#endif
		else
		{
			NclObj tmp_parent_obj;
			NclRefList *p;
			if (theobj->obj.parents) {  
				tmp_obj = theobj;
				for (p = theobj->obj.parents; p; p = p->next) {
					tmp_parent_obj = _NclGetObj(p->pid);
					if (tmp_parent_obj->obj.obj_type_mask & Ncl_Att) {
						tmp_obj= (NclObj)_NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,(NclMultiDValData)theobj, 
									       NULL,-1,NULL,ATTVALLINK,NULL,PERMANENT);
						((NclVar)tmp_obj)->var.att_cb = _NclAddCallback((NclObj)theobj,(NclObj)tmp_obj,
												ListAttDestroyNotify,DESTROYED,NULL);
						break;
					}
				}
			}
			else {
				tmp_obj= (NclObj)_NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,
							       (NclMultiDValData)theobj, NULL,-1,NULL,NORMAL,NULL,PERMANENT);
			}
		}

		ret = _NclAddParent(tmp_obj,list);
		tmp->cb = _NclAddCallback( tmp_obj, list, ListItemDestroyNotify,DESTROYED,NULL);

		if(tmp_obj->obj.status == TEMPORARY) {
			_NclSetStatus(tmp_obj,PERMANENT);
		}
		if(ret != NhlNOERROR) {
			return(ret);
		}
		tmp->obj_id = tmp_obj->obj.id;

		tmp->prev = thelist->list.last;
		if(thelist->list.last == NULL) {
			thelist->list.last = tmp;
			thelist->list.last->next = NULL;	
			thelist->list.first = tmp;
		} else {
			thelist->list.last->next = tmp;
			thelist->list.last = tmp;
			tmp->next = NULL;
		}

		thelist->list.nelem++;
		return(NhlNOERROR);
	} else {
		return(NhlFATAL);
	}
}

static int ListGetType
#if     NhlNeedProto
(NclObj list)
#else
(list)
NclObj list;
#endif
{
	NclList thelist = (NclList)list;
	if(list!=NULL) {
		return(thelist->list.list_type);
	} else {
		return(-1);
	}
	
}

static NhlErrorTypes ListSetType
#if     NhlNeedProto
(NclObj list,int new_type)
#else
(list,new_type)
NclObj list;
int new_type;
#endif
{
	NclList thelist = (NclList)list;

	int fo_mask =  ~(NCL_LIFO | NCL_FIFO);
	int jo_mask =  ~(NCL_JOIN | NCL_CONCAT);

	if((new_type & NCL_LIFO)||(new_type & NCL_FIFO)) {
		thelist->list.list_type = (new_type & jo_mask) | (thelist->list.list_type & fo_mask);
	}
	else if((new_type & NCL_JOIN )||(new_type & NCL_CONCAT)) {
		thelist->list.list_type = (new_type & fo_mask) | (thelist->list.list_type & jo_mask);
	}
	else {
		thelist->list.list_type = new_type;
	}
	return(NhlNOERROR) ;
}
static NclList ReverseList
#if     NhlNeedProto
(NclList thelist)
#else
(thelist)
NclList thelist;
#endif
{

	NclListObjList *step,*tmp_prev;

	step = thelist->list.first;
	tmp_prev = thelist->list.first;
	thelist->list.first = thelist->list.last;
	thelist->list.last = tmp_prev;
	while(step!= NULL) {
		tmp_prev = step->prev;
		step->prev = step->next;
		step->next = tmp_prev;
		step = step->prev;
	}
	return thelist;
	
}
static NclList ListSelect
#if     NhlNeedProto
(NclObj list,NclSelection * sel_ptr)
#else
(list,sel_ptr)
NclObj list;
int new_type;
#endif
{
	NclList thelist = (NclList)list;
	NclListObjList *tmp,*step;
	NclObj tmp_obj;
	int tmp_stride;
	int i,j;
	NclList outlist= (NclList)_NclListCreate(NULL,NULL,Ncl_List,0,thelist->list.list_type);
	long *ind;
	
	if(sel_ptr == NULL) {
/*
* List select must copy entire list because list has to be sequenced 
* which require unique storage.
*/
		step = thelist->list.last;
		while(step != NULL) {
			tmp = (NclListObjList*)NclMalloc(sizeof(NclListObjList));
			tmp->prev = NULL;
			tmp->next = NULL;
			if(outlist->list.last == NULL){
				outlist->list.last = tmp;	
			}
			tmp->obj_id = step->obj_id;
			tmp_obj = _NclGetObj(step->obj_id);
			tmp->orig_type = tmp_obj->obj.obj_type_mask;
			_NclAddParent(tmp_obj,(NclObj)outlist);
			tmp->cb = _NclAddCallback(tmp_obj ,(NclObj) outlist, ListItemDestroyNotify,DESTROYED,NULL);
			tmp->next = outlist->list.first;
			if(outlist->list.first != NULL) {
				outlist->list.first->prev = tmp;
			}
			outlist->list.first = tmp;
			step = step->prev;
			outlist->list.nelem++;
		}
		return(outlist);
	} else if(sel_ptr->sel_type == Ncl_VECSUBSCR) {

		ind = sel_ptr->u.vec.ind;
		for(i = sel_ptr->u.vec.n_ind-1; i >= 0 ; i--) {
			tmp = (NclListObjList*)NclMalloc(sizeof(NclListObjList));
			tmp->prev = NULL;
			tmp->next = NULL;
			if(outlist->list.last == NULL) {
				outlist->list.last = tmp;
			}
			step = thelist->list.first;
 			if (ind[i] < 0 || ind[i] >= thelist->list.nelem) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSelect: Index out of range");
                                _NclDestroyObj((NclObj)outlist);
                                NclFree(tmp);
                                return(NULL);
                        }
			for(j = 0; j < ind[i];j++) {
				if(step != NULL) {
					step = step->next;
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSelect: Index out of range");
					_NclDestroyObj((NclObj)outlist);
					NclFree(tmp);
					return(NULL);
				}
			}
			tmp->obj_id = step->obj_id;
			tmp_obj = _NclGetObj(step->obj_id);
			tmp->orig_type = tmp_obj->obj.obj_type_mask;
			_NclAddParent(tmp_obj,(NclObj)outlist);
			tmp->cb = _NclAddCallback(tmp_obj, (NclObj)outlist, ListItemDestroyNotify,DESTROYED,NULL);
			tmp->next = outlist->list.first;
			if(outlist->list.first != NULL) {
				outlist->list.first->prev = tmp;
			}
			outlist->list.first = tmp;
			step = step->prev;
			outlist->list.nelem++;
		}
		return(outlist);
	} else {
		switch(sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			sel_ptr->u.sub.start = 0;
			sel_ptr->u.sub.finish = thelist->list.nelem-1;
			break;
		case Ncl_SUB_VAL_DEF:
			sel_ptr->u.sub.finish = thelist->list.nelem-1;
			break;
		case Ncl_SUB_DEF_VAL:
			sel_ptr->u.sub.start = 0;
			break;
		case Ncl_SUBSCR:
			break;
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
			return(NULL);
		}

		tmp_stride = abs(sel_ptr->u.sub.stride);
		if(sel_ptr->u.sub.start <= sel_ptr->u.sub.finish) {
                        if (sel_ptr->u.sub.start < 0 || sel_ptr->u.sub.finish >= thelist->list.nelem) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSelect: Index out of range");
				_NclDestroyObj((NclObj)outlist);
				return(NULL);
			}
			if(sel_ptr->u.sub.start != 0) {
				step = thelist->list.first;
				for(i = 0; i < sel_ptr->u.sub.start; i++) {
					if(step == NULL) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSelect: Subscript out of range\n");
/*
* NEED TO FREE!
*/
						_NclDestroyObj((NclObj)outlist);
						return(NULL);
					}
					step = step->next;
				}
			} else {
				step = thelist->list.first;
			}
			for(i = sel_ptr->u.sub.start; i <= sel_ptr->u.sub.finish;i+=tmp_stride) {
				if(step == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSelect: Subscript out of range\n");
/*
* NEED TO FREE!
*/
					_NclDestroyObj((NclObj)outlist);
					return(NULL);
				}
				tmp = (NclListObjList*)NclMalloc(sizeof(NclListObjList));
				tmp->prev = NULL;
				tmp->next = NULL;
				if(outlist->list.first == NULL){
					outlist->list.first = tmp;	
					if(outlist->list.last == NULL) {
						outlist->list.last = tmp;
					}
				}
				tmp->obj_id = step->obj_id;
				tmp_obj = _NclGetObj(step->obj_id);
				tmp->orig_type = tmp_obj->obj.obj_type_mask;


				_NclAddParent(tmp_obj,(NclObj)outlist);
				tmp->cb = _NclAddCallback(tmp_obj , (NclObj)outlist, ListItemDestroyNotify,DESTROYED,NULL);
				if(outlist->list.last != tmp) {
					tmp->prev = outlist->list.last;
					outlist->list.last->next = tmp;
					outlist->list.last = tmp;
				}  
				
				outlist->list.nelem++;
				for(j = 0; j < tmp_stride; j++) {
					if(step == NULL) {
						break;
					}
					step = step->next;
				}
			}
		} else {
                        if (sel_ptr->u.sub.finish < 0 || sel_ptr->u.sub.start >= thelist->list.nelem) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSelect: Index out of range");
				_NclDestroyObj((NclObj)outlist);
				return(NULL);
			}
			if(sel_ptr->u.sub.start != thelist->list.nelem-1) {
				step = thelist->list.last;
				for(i = thelist->list.nelem-1; i > sel_ptr->u.sub.start; i--) {
					if(step == NULL) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSelect: Subscript out of range\n");
/*
* NEED TO FREE!
*/		
						_NclDestroyObj((NclObj)outlist);
						return(NULL);
					}
					step = step->prev;
				}
				
			} else {
				step = thelist->list.last;
			}
			for(i = sel_ptr->u.sub.start; i >= sel_ptr->u.sub.finish;i-=tmp_stride) {
				if(step == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSelect: Subscript out of range\n");
/*
* NEED TO FREE!
*/
					_NclDestroyObj((NclObj)outlist);
					return(NULL);
				}
				tmp = (NclListObjList*)NclMalloc(sizeof(NclListObjList));
				tmp->prev = NULL;
				tmp->next = NULL;
				if(outlist->list.first== NULL){
					outlist->list.first= tmp;	
					if(outlist->list.last== NULL) {
						outlist->list.last= tmp;
					}
				}
				tmp->obj_id = step->obj_id;
				tmp_obj = _NclGetObj(step->obj_id);
				tmp->orig_type = tmp_obj->obj.obj_type_mask;
				_NclAddParent(tmp_obj,(NclObj)outlist);
				tmp->cb = _NclAddCallback(tmp_obj, (NclObj)outlist, ListItemDestroyNotify,DESTROYED,NULL);
				if(outlist->list.last != tmp) {
					tmp->prev= outlist->list.last;
					outlist->list.last->next= tmp;
					outlist->list.last= tmp;
				}  
				
				outlist->list.nelem++;
				for(j = 0; j < tmp_stride; j++) {
					if(step == NULL) {
						break;
					}
					step = step->prev;
				}
			}
		}
		if(sel_ptr->u.sub.stride < 0)  {
			ReverseList(outlist);
		}
		return(outlist);
	}
		
}

static NhlErrorTypes ListAddParent
#if     NhlNeedProto
(NclObj theobj,NclObj  parent)
#else
(theobj,parent)
NclObj theobj;
NclObj parent;
#endif
{
	NclRefList *tmp = NULL;

	theobj->obj.ref_count++;
	tmp = theobj->obj.parents;
	theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
	theobj->obj.parents->next = tmp;
	theobj->obj.parents->pid = parent->obj.id;
 
	return(NhlNOERROR);

}

static NhlErrorTypes ListDelParent
#if     NhlNeedProto
(NclObj theobj,NclObj  parent)
#else
(theobj,parent)
NclObj theobj;
NclObj parent;
#endif
{
        NclRefList *tmp,*tmp1;
        int found = 0;
	
	if(theobj->obj.parents == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"VarDelParent: Attempt to delete parent from empty list");
                return(NhlFATAL);
        }
	tmp = theobj->obj.parents;
	while((tmp!=NULL)&&(tmp->pid == parent->obj.id)){
		theobj->obj.parents = theobj->obj.parents->next;
		NclFree(tmp);
		tmp = theobj->obj.parents;
		theobj->obj.ref_count--;
		found = 1;
	}
	if((tmp == NULL) &&(found) ) {
		_NclDestroyObj(theobj);
		return(NhlNOERROR);
	}
        while(tmp->next != NULL) {
                if(tmp->next->pid == parent->obj.id) {
                        found = 1;
                        theobj->obj.ref_count--;
                        tmp1 = tmp->next;
                        tmp->next = tmp->next->next;
                        NclFree(tmp1);
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

static NhlErrorTypes InitializeListClass
#if NhlNeedProto
(void)
#else
()
#endif
{
        _NclRegisterClassPointer(
                Ncl_List,
                (NclObjClass)&nclListClassRec
        );
        return(NhlNOERROR);
}

NclListClassRec nclListClassRec = {
	{
		"NclListClass",
		sizeof(NclListRec),
		(NclObjClass)&nclObjClassRec,
		0,
		(NclGenericFunction)ListDestroy,
		(NclSetStatusFunction)NULL,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)InitializeListClass,
/* NclAddParentFunction add_parent */   ListAddParent,
/* NclDelParentFunction del_parent */   ListDelParent,
/* NclPrintSummaryFunction print_summary */ ListPrintSummary,
/* NclPrintFunction print */            ListPrint,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclListSetTypeFunction  set_type;*/ ListSetType,
/* NclListGetTypeFunction  get_type;*/ ListGetType,
/* NclListAppendFunction   append;*/ ListAppend,
/* NclListPushFunction     push;*/ ListPush,
/* NclListPopFunction      pop;*/ ListPop,
/* NclListSelectFunction   select;*/ ListSelect,
/* NclListGetNextFunction  get_next;*/ ListGetNext
	}
};

NclObjClass nclListClass = (NclObjClass)&nclListClassRec;


struct _NclObjRec *_NclListCreate(
        struct _NclObjRec *     inst,
        struct _NclObjClassRec *        theclass ,
        NclObjTypes      obj_type,
        unsigned int    obj_type_mask,
	int list_type
)
{
	NclList my_inst;
	NclObjClass class_ptr;
	NhlErrorTypes ret;

	ret = _NclInitClass(nclListClass);
        if(ret < NhlWARNING)
                return(NULL);
	if(inst == NULL) {
		my_inst = (NclList)NclMalloc((unsigned)sizeof(NclListRec));
	} else {
		my_inst = (NclList)inst;
	}
	if(theclass == NULL) {
		class_ptr = nclListClass;
	} else {
		class_ptr = theclass;
	}
	if(obj_type == 0) {
		obj_type = Ncl_List;
	}

	(void)_NclObjCreate((NclObj)my_inst,class_ptr,obj_type,(obj_type_mask | Ncl_List),PERMANENT);

	my_inst->list.list_type = list_type;
	my_inst->list.list_quark = 0;
	my_inst->list.thesym = NULL;
	my_inst->list.nelem = 0;
	my_inst->list.state = NCL_LIST_IDLE;
	my_inst->list.first = NULL;
	my_inst->list.last= NULL;
	my_inst->list.current_item= NULL;
	my_inst->list.agg_sel_ptr = NULL;

	return((NclObj)my_inst);
}

