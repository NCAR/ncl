/*
 * $ID$
 */

#include "NclAdvancedList.h"
#include <assert.h>
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

struct _NclObjRec *_NclAdvancedListCreate(struct _NclObjRec      *inst,
                                     struct _NclObjClassRec *theclass,
                                     NclObjTypes             obj_type,
                                     unsigned int            obj_type_mask,
                                     ng_size_t               listsize,
                                     int                     list_type)
{
    NclAdvancedList my_inst;
    NclObjClass class_ptr;
    NhlErrorTypes ret;

    ret = _NclInitClass(nclAdvancedListClass);
    if(ret < NhlWARNING)
       return(NULL);

    if(inst == NULL)
    {
        my_inst = (NclAdvancedList)NclCalloc(1, (unsigned)sizeof(NclAdvancedListRec));
    }
    else
    {
        my_inst = (NclAdvancedList)inst;
    }

    if(theclass == NULL)
    {
        class_ptr = nclAdvancedListClass;
    }
    else
    {
        class_ptr = theclass;
    }

    if(obj_type == 0)
    {
        obj_type = Ncl_List;
    }

    (void)_NclObjCreate((NclObj)my_inst,class_ptr,obj_type,
                        (obj_type_mask | Ncl_List),PERMANENT);

    my_inst->advancedlist.type = list_type;
    my_inst->advancedlist.name = 0;
    my_inst->advancedlist.thesym = NULL;

    if(listsize < 1)
        my_inst->advancedlist.max_elem = NCL_MIN_LIST_ITEMS;
    else
        my_inst->advancedlist.max_elem = listsize;
    my_inst->advancedlist.n_elem = 0;
    my_inst->advancedlist.state = NCL_LIST_IDLE;

    my_inst->advancedlist.item = (NclListObjList **)NclCalloc(my_inst->advancedlist.max_elem,
                                                        sizeof(NclListObjList *));
    assert(my_inst->advancedlist.item);

    return((NclObj)my_inst);
}

static NhlErrorTypes InitializeAdvancedListClass
#if NhlNeedProto
(void)
#else
()
#endif
{
        _NclRegisterClassPointer(
                Ncl_List,
                (NclObjClass)&nclAdvancedListClassRec
        );
        return(NhlNOERROR);
}

static NhlErrorTypes AdvancedListPrintSummary(NclObj theobj, FILE *fp)
{
    NclAdvancedList new_list = NULL;
    NhlErrorTypes ret;

    new_list = (NclAdvancedList) theobj;

    ret = nclfprintf(fp,"Type: %s <list>\n",
                         NrmQuarkToString(new_list->advancedlist.type));
    ret = nclfprintf(fp,"Total items: %ld\n",
                         (long)new_list->advancedlist.n_elem);

    return(NhlNOERROR);
}

static NhlErrorTypes AdvancedListPrint(NclObj theobj, FILE *fp)
{
    NclAdvancedList new_list = (NclAdvancedList) theobj;
    NhlErrorTypes ret;

    ret = nclfprintf(fp,"Type: %s <list>\n",
                         NrmQuarkToString(new_list->advancedlist.type));
    ret = nclfprintf(fp,"Total items: %ld\n",
                         (long)new_list->advancedlist.n_elem);

    return(NhlNOERROR);
}

NhlErrorTypes ListAppend(NclObj list,NclObj theobj)
{
    NclAdvancedList thelist = (NclAdvancedList)list;
    NclListObjList *tmp = (NclListObjList*)NclCalloc(1, sizeof(NclListObjList));
    NhlErrorTypes  ret = NhlNOERROR;
    NclObj tmp_obj;
    int n;

    if(NULL == theobj)
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
            "ListAppend: Nothing have done for append a NULL object to list.\n"));
        return(NhlWARNING);
    }
       
    if(NULL == thelist)
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
            "ListAppend: Could not append a object to NULL list.\n"));
        return(NhlFATAL);
    }

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
		    for (p = theobj->obj.parents; p; p = p->next) {
			    tmp_parent_obj = _NclGetObj(p->pid);
			    if (tmp_parent_obj->obj.obj_type_mask & Ncl_Att) {
				    tmp_obj= (NclObj)_NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,(NclMultiDValData)theobj, 
								   NULL,-1,NULL,ATTVALLINK,NULL,PERMANENT);
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
    tmp->cb = _NclAddCallback(tmp_obj, list, ListItemDestroyNotify,DESTROYED,NULL);

    if(tmp_obj->obj.status == TEMPORARY)
    {
        _NclSetStatus(tmp_obj,PERMANENT);
    }

    if(ret != NhlNOERROR)
    {
        return(ret);
    }

    tmp->obj_id = tmp_obj->obj.id;

    tmp->prev = NULL;
    tmp->next = NULL;

    n = thelist->advancedlist.n_elem;

    if(n >= thelist->advancedlist.max_elem)
    {
        while(thelist->advancedlist.max_elem <= n)
            thelist->advancedlist.max_elem *= 2;

        thelist->advancedlist.item = (NclListObjList **)NclRealloc(thelist->advancedlist.item,
                                thelist->advancedlist.max_elem * sizeof(NclListObjList *));
        assert(thelist->advancedlist.item);
    }

    thelist->advancedlist.item[n] = tmp;

    thelist->advancedlist.n_elem++;
    return(NhlNOERROR);
}

static NclList AdvancedListSelect(NclObj list, NclSelection *sel_ptr)
{
    NclAdvancedList thelist = (NclAdvancedList)list;
    NclListObjList *ori;
    NclObj tmp_obj;
    int tmp_stride;
    int i,j,n;
    NclAdvancedList outlist;
    long *ind;

    if(sel_ptr == NULL)
    {
        outlist = (NclAdvancedList)_NclAdvancedListCreate(NULL,NULL,Ncl_List,0,
                                                thelist->advancedlist.n_elem,
                                                thelist->advancedlist.type);
        assert(outlist);
        outlist->advancedlist.name = thelist->advancedlist.name;
        outlist->advancedlist.type = thelist->advancedlist.type;
        outlist->obj.obj_type = Ncl_List;

        for(n = 0; n < thelist->advancedlist.n_elem; n++)
        {
            ori = thelist->advancedlist.item[n];
            tmp_obj = _NclGetObj(ori->obj_id);
            ListAppend((NclObj)outlist, tmp_obj);
        }
        return((NclList)outlist);
    }
    else if(sel_ptr->sel_type == Ncl_VECSUBSCR)
    {
        outlist = (NclAdvancedList)_NclAdvancedListCreate(NULL,NULL,Ncl_List,0,
                                                0,
                                                thelist->advancedlist.type);
        assert(outlist);
        outlist->advancedlist.name = thelist->advancedlist.name;
        outlist->advancedlist.type = thelist->advancedlist.type;
        outlist->obj.obj_type = Ncl_List;

        n = 0;
        ind = sel_ptr->u.vec.ind;
        for(i = sel_ptr->u.vec.n_ind-1; i >= 0 ; i--)
        {
            if(ind[i] < 0 || ind[i] >= thelist->advancedlist.n_elem)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"AdvancedListSelect: Index out of range");
                _NclDestroyObj((NclObj)outlist);
                return(NULL);
            }

            n += ind[i];
            ori = thelist->advancedlist.item[n];
            tmp_obj = _NclGetObj(ori->obj_id);
            ListAppend((NclObj)outlist, tmp_obj);
        }
        return((NclList)outlist);
    }
    else
    {
        switch(sel_ptr->sel_type)
        {
        case Ncl_SUB_ALL:
            sel_ptr->u.sub.start = 0;
            sel_ptr->u.sub.finish = thelist->advancedlist.n_elem-1;
            break;
        case Ncl_SUB_VAL_DEF:
            sel_ptr->u.sub.finish = thelist->advancedlist.n_elem-1;
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
        if(sel_ptr->u.sub.start <= sel_ptr->u.sub.finish)
        {
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tsel_ptr->u.sub.start = %d\n", sel_ptr->u.sub.start);
           *fprintf(stderr, "\tsel_ptr->u.sub.finish = %d\n", sel_ptr->u.sub.finish);
           *fprintf(stderr, "\ttmp_stride = %d\n", tmp_stride);
           */

            if(sel_ptr->u.sub.start < 0 || sel_ptr->u.sub.finish >= thelist->advancedlist.n_elem)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"AdvancedListSelect: Index out of range");
                _NclDestroyObj((NclObj)outlist);
                return(NULL);
            }

            n = (sel_ptr->u.sub.finish - sel_ptr->u.sub.start + 1) / tmp_stride;

            outlist = (NclAdvancedList)_NclAdvancedListCreate(NULL,NULL,Ncl_List,0,
                                                    n, thelist->advancedlist.type);
            assert(outlist);
            outlist->advancedlist.name = thelist->advancedlist.name;
            outlist->advancedlist.type = thelist->advancedlist.type;
            outlist->obj.obj_type = Ncl_List;

            for(i = sel_ptr->u.sub.start; i <= sel_ptr->u.sub.finish; i += tmp_stride)
            {
                ori = thelist->advancedlist.item[i];
                tmp_obj = _NclGetObj(ori->obj_id);
                ListAppend((NclObj)outlist, tmp_obj);
            }
        }
        else
        {
            fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
            fprintf(stderr, "\tsel_ptr->u.sub.start = %d\n", sel_ptr->u.sub.start);
            fprintf(stderr, "\tsel_ptr->u.sub.finish = %d\n", sel_ptr->u.sub.finish);
            fprintf(stderr, "\ttmp_stride = %d\n", tmp_stride);

            if(sel_ptr->u.sub.finish < 0 || sel_ptr->u.sub.start >= thelist->advancedlist.n_elem)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"AdvancedListSelect: Index out of range");
                _NclDestroyObj((NclObj)outlist);
                return(NULL);
            }

            n = (sel_ptr->u.sub.start - sel_ptr->u.sub.finish + 1) / tmp_stride;

            outlist = (NclAdvancedList)_NclAdvancedListCreate(NULL,NULL,Ncl_List,0,
                                                    n, thelist->advancedlist.type);
            assert(outlist);
            outlist->advancedlist.name = thelist->advancedlist.name;
            outlist->advancedlist.type = thelist->advancedlist.type;
            outlist->obj.obj_type = Ncl_List;

            for(i = sel_ptr->u.sub.start; i >= sel_ptr->u.sub.finish; i -= tmp_stride)
            {
                ori = thelist->advancedlist.item[i];
                tmp_obj = _NclGetObj(ori->obj_id);
                ListAppend((NclObj)outlist, tmp_obj);
            }
        }

        return((NclList)outlist);
    }
}

/* This always returns the first item's id */
static int AdvancedListGetNext(NclObj list)
{
    NclAdvancedList thelist = (NclAdvancedList)list;
    int tmp_id = -1;

    if(NULL != thelist)
    {
        if(NCL_LIST_IDLE == thelist->advancedlist.state)
        {
            thelist->advancedlist.state = NCL_LIST_SEQUENCING;
            if(NULL != thelist->advancedlist.item[0])
            {
                tmp_id = thelist->advancedlist.item[0]->obj_id;
            }
        } 
        else
        {
            thelist->list.state = NCL_LIST_IDLE;;
            tmp_id = -1;
        } 
    }

    return (tmp_id);
}

NclAdvancedListClassRec nclAdvancedListClassRec =
{
    {
        "NclAdvancedListClass",
        sizeof(NclAdvancedListRec),
        (NclObjClass)&nclObjClassRec,
        0,
        (NclGenericFunction)   NULL,
        (NclSetStatusFunction) NULL,
        (NclInitPartFunction)  NULL,
        (NclInitClassFunction) InitializeAdvancedListClass,
        /* NclAddParentFunction add_parent */   NULL,
        /* NclDelParentFunction del_parent */   NULL,
        /* NclPrintSummaryFunction print_summary */ AdvancedListPrintSummary,
        /* NclPrintFunction print */            AdvancedListPrint,
        /* NclCallBackList* create_callback */  NULL,
        /* NclCallBackList* delete_callback */  NULL,
        /* NclCallBackList* modify_callback */  NULL,
        /* NclObtainCall obtain_calldata */     NULL
    },

    {
        /* NclListSetTypeFunction  set_type */ NULL,
        /* NclListGetTypeFunction  get_type */ NULL,
        /* NclListPushFunction     push */     NULL,
        /* NclListPopFunction      pop */      NULL,
        /* NclListSelectFunction   select */   AdvancedListSelect,
        /* NclListGetNextFunction  get_next */ AdvancedListGetNext
    },

    {
        /* NclListAppendFunction append */     ListAppend
    }
};

NclObjClass nclAdvancedListClass = (NclObjClass)&nclAdvancedListClassRec;

