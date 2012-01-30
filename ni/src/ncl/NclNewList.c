/*
 * $ID$
 */

#include "NclNewList.h"
#include <assert.h>

#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include "NclDataDefs.h"

struct _NclObjRec *_NclNewListCreate(struct _NclObjRec      *inst,
                                     struct _NclObjClassRec *theclass,
                                     NclObjTypes             obj_type,
                                     unsigned int            obj_type_mask,
                                     ng_size_t               listsize,
                                     int                     list_type)
{
    NclNewList my_inst;
    NclObjClass class_ptr;
    NhlErrorTypes ret;

    ret = _NclInitClass(nclNewListClass);
    if(ret < NhlWARNING)
       return(NULL);

    if(inst == NULL)
    {
        my_inst = (NclNewList)NclCalloc(1, (unsigned)sizeof(NclNewListRec));
    }
    else
    {
        my_inst = (NclNewList)inst;
    }

    if(theclass == NULL)
    {
        class_ptr = nclNewListClass;
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

    my_inst->newlist.type = list_type;
    my_inst->newlist.name = 0;
    my_inst->newlist.thesym = NULL;

    if(listsize < 1)
        my_inst->newlist.max_elem = NCL_MIN_LIST_ITEMS;
    else
        my_inst->newlist.max_elem = listsize;
    my_inst->newlist.n_elem = 0;
    my_inst->newlist.state = NCL_LIST_IDLE;

    my_inst->newlist.item = (NclListObjList **)NclCalloc(my_inst->newlist.max_elem,
                                                        sizeof(NclListObjList *));
    assert(my_inst->newlist.item);

    return((NclObj)my_inst);
}

static NhlErrorTypes InitializeNewListClass
#if NhlNeedProto
(void)
#else
()
#endif
{
        _NclRegisterClassPointer(
                Ncl_List,
                (NclObjClass)&nclNewListClassRec
        );
        return(NhlNOERROR);
}

static NhlErrorTypes NewListPrintSummary(NclObj theobj, FILE *fp)
{
    NclNewList new_list = NULL;
    NhlErrorTypes ret;

    new_list = (NclNewList) theobj;

    ret = nclfprintf(fp,"Type: %s <list>\n",
                         NrmQuarkToString(new_list->newlist.type));
    ret = nclfprintf(fp,"Total items: %ld\n",
                         (long)new_list->newlist.n_elem);

    return(NhlNOERROR);
}

static NhlErrorTypes NewListPrint(NclObj theobj, FILE *fp)
{
    NclNewList new_list = (NclNewList) theobj;
    NhlErrorTypes ret;

    ret = nclfprintf(fp,"Type: %s <list>\n",
                         NrmQuarkToString(new_list->newlist.type));
    ret = nclfprintf(fp,"Total items: %ld\n",
                         (long)new_list->newlist.n_elem);

    return(NhlNOERROR);
}

NhlErrorTypes ListAppend(NclObj list,NclObj theobj)
{
    NclNewList thelist = (NclNewList)list;
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

    if(!(theobj->obj.obj_type_mask & Ncl_Var))
    {
        if(theobj->obj.obj_type_mask & Ncl_MultiDValnclfileData)
        {
            tmp_obj = (NclObj)_NclFileVarCreate(NULL,NULL,Ncl_FileVar,0,NULL,
                      (NclMultiDValData)theobj,NULL,-1,NULL,NORMAL,NULL,PERMANENT);
        }
        else if(theobj->obj.obj_type_mask & Ncl_MultiDValHLUObjData)
        {
            tmp_obj = (NclObj)_NclHLUVarCreate(NULL,NULL,Ncl_HLUVar,0,NULL,
                      (NclMultiDValData)theobj,NULL,-1,NULL,NORMAL,NULL,PERMANENT);
        }
        else
        {
            tmp_obj = (NclObj)_NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,
                      (NclMultiDValData)theobj, NULL,-1,NULL,NORMAL,NULL,PERMANENT);
        }
    }
    else
    {
        tmp_obj = theobj;
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

    n = thelist->newlist.n_elem;

    if(n >= thelist->newlist.max_elem)
    {
        while(thelist->newlist.max_elem <= n)
            thelist->newlist.max_elem *= 2;

        thelist->newlist.item = (NclListObjList **)NclRealloc(thelist->newlist.item,
                                thelist->newlist.max_elem * sizeof(NclListObjList *));
        assert(thelist->newlist.item);
    }

    thelist->newlist.item[n] = tmp;

    thelist->newlist.n_elem++;
    return(NhlNOERROR);
}

static NclList NewListSelect(NclObj list, NclSelection *sel_ptr)
{
    NclNewList thelist = (NclNewList)list;
    NclListObjList *ori;
    NclObj tmp_obj;
    int tmp_stride;
    int i,j,n;
    NclNewList outlist;
    long *ind;

    if(sel_ptr == NULL)
    {
        outlist = (NclNewList)_NclNewListCreate(NULL,NULL,Ncl_List,0,
                                                thelist->newlist.n_elem,
                                                thelist->newlist.type);
        assert(outlist);
        outlist->newlist.name = thelist->newlist.name;
        outlist->newlist.type = thelist->newlist.type;
        outlist->obj.obj_type = Ncl_List;

        for(n = 0; n < thelist->newlist.n_elem; n++)
        {
            ori = thelist->newlist.item[n];
            tmp_obj = _NclGetObj(ori->obj_id);
            ListAppend((NclObj)outlist, tmp_obj);
        }
        return((NclList)outlist);
    }
    else if(sel_ptr->sel_type == Ncl_VECSUBSCR)
    {
        outlist = (NclNewList)_NclNewListCreate(NULL,NULL,Ncl_List,0,
                                                0,
                                                thelist->newlist.type);
        assert(outlist);
        outlist->newlist.name = thelist->newlist.name;
        outlist->newlist.type = thelist->newlist.type;
        outlist->obj.obj_type = Ncl_List;

        n = 0;
        ind = sel_ptr->u.vec.ind;
        for(i = sel_ptr->u.vec.n_ind-1; i >= 0 ; i--)
        {
            if(ind[i] < 0 || ind[i] >= thelist->newlist.n_elem)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"NewListSelect: Index out of range");
                _NclDestroyObj((NclObj)outlist);
                return(NULL);
            }

            n += ind[i];
            ori = thelist->newlist.item[n];
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
            sel_ptr->u.sub.finish = thelist->newlist.n_elem-1;
            break;
        case Ncl_SUB_VAL_DEF:
            sel_ptr->u.sub.finish = thelist->newlist.n_elem-1;
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

            if(sel_ptr->u.sub.start < 0 || sel_ptr->u.sub.finish >= thelist->newlist.n_elem)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"NewListSelect: Index out of range");
                _NclDestroyObj((NclObj)outlist);
                return(NULL);
            }

            n = (sel_ptr->u.sub.finish - sel_ptr->u.sub.start + 1) / tmp_stride;

            outlist = (NclNewList)_NclNewListCreate(NULL,NULL,Ncl_List,0,
                                                    n, thelist->newlist.type);
            assert(outlist);
            outlist->newlist.name = thelist->newlist.name;
            outlist->newlist.type = thelist->newlist.type;
            outlist->obj.obj_type = Ncl_List;

            for(i = sel_ptr->u.sub.start; i <= sel_ptr->u.sub.finish; i += tmp_stride)
            {
                ori = thelist->newlist.item[i];
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

            if(sel_ptr->u.sub.finish < 0 || sel_ptr->u.sub.start >= thelist->newlist.n_elem)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"NewListSelect: Index out of range");
                _NclDestroyObj((NclObj)outlist);
                return(NULL);
            }

            n = (sel_ptr->u.sub.start - sel_ptr->u.sub.finish + 1) / tmp_stride;

            outlist = (NclNewList)_NclNewListCreate(NULL,NULL,Ncl_List,0,
                                                    n, thelist->newlist.type);
            assert(outlist);
            outlist->newlist.name = thelist->newlist.name;
            outlist->newlist.type = thelist->newlist.type;
            outlist->obj.obj_type = Ncl_List;

            for(i = sel_ptr->u.sub.start; i >= sel_ptr->u.sub.finish; i -= tmp_stride)
            {
                ori = thelist->newlist.item[i];
                tmp_obj = _NclGetObj(ori->obj_id);
                ListAppend((NclObj)outlist, tmp_obj);
            }
        }

        return((NclList)outlist);
    }
}

/* This always returns the first item's id */
static int NewListGetNext(NclObj list)
{
    NclNewList thelist = (NclNewList)list;
    int tmp_id = -1;

    if(NULL != thelist)
    {
        if(NCL_LIST_IDLE == thelist->newlist.state)
        {
            thelist->newlist.state = NCL_LIST_SEQUENCING;
            if(NULL != thelist->newlist.item[0])
            {
                tmp_id = thelist->newlist.item[0]->obj_id;
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

NclNewListClassRec nclNewListClassRec =
{
    {
        "NclNewListClass",
        sizeof(NclNewListRec),
        (NclObjClass)&nclObjClassRec,
        0,
        (NclGenericFunction)   NULL,
        (NclSetStatusFunction) NULL,
        (NclInitPartFunction)  NULL,
        (NclInitClassFunction) InitializeNewListClass,
        /* NclAddParentFunction add_parent */   NULL,
        /* NclDelParentFunction del_parent */   NULL,
        /* NclPrintSummaryFunction print_summary */ NewListPrintSummary,
        /* NclPrintFunction print */            NewListPrint,
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
        /* NclListSelectFunction   select */   NewListSelect,
        /* NclListGetNextFunction  get_next */ NewListGetNext
    },

    {
        /* NclListAppendFunction append */     ListAppend
    }
};

NclObjClass nclNewListClass = (NclObjClass)&nclNewListClassRec;

