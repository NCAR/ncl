/*
 * $Id: ListSupport.c,v 1.3 2004-04-28 17:02:12 grubin Exp $
 */

/************************************************************************
*                                                                       *
*                            Copyright (C)  1993                        *
*            University Corporation for Atmospheric Research            *
*                            All Rights Reserved                        *
*                                                                       *
************************************************************************/
/*
 *      Author:         Ethan Alpert
 *                      National Center for Atmospheric Research
 *                      POB 3000, Boulder, Colorado
 *
 *      $Date: 2004-04-28 17:02:12 $
 */

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
#include "Symbol.h"
#include "NclList.h"
#include "ListSupport.h"

int _NclListGetNext(NclObj thelist)
{
	NclListClass lc;
	if(thelist == NULL) {
		return(-1);
	} else {
		lc = (NclListClass)thelist->obj.class_ptr;
	}
	while((NclObjClass)lc != nclObjClass) {
		if(lc->list_class.get_next != NULL) {
			return((*lc->list_class.get_next)(thelist));
		} else {
			lc = (NclListClass)lc->obj_class.super_class;
		}
	}
	return(-1);
}

struct _NclObjRec* _NclListPop(NclObj thelist)
{
	NclListClass lc;
	if(thelist == NULL) {
		return(NULL);
	} else {
		lc = (NclListClass)thelist->obj.class_ptr;
	}
	while((NclObjClass)lc != nclObjClass) {
		if(lc->list_class.pop!= NULL) {
			return((NclObj)(*lc->list_class.pop)(thelist));
		} else {
			lc = (NclListClass)lc->obj_class.super_class;
		}
	}
	return(NULL);
}
NhlErrorTypes _NclListPush(NclObj thelist, NclObj theobj) 
{
	NclListClass lc;
	if(thelist == NULL) {
		return(NhlFATAL);
	} else {
		lc = (NclListClass)thelist->obj.class_ptr;
	}
	while((NclObjClass)lc != nclObjClass) {
		if(lc->list_class.push!= NULL) {
			return((*lc->list_class.push)(thelist,theobj));
		} else {
			lc = (NclListClass)lc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
NhlErrorTypes _NclListSetType(NclObj thelist, int new_type)
{
	NclListClass lc;
	if(thelist == NULL) {
		return(NhlFATAL);
	} else {
		lc = (NclListClass)thelist->obj.class_ptr;
	}
	while((NclObjClass)lc != nclObjClass) {
		if(lc->list_class.set_type!= NULL) {
			return((*lc->list_class.set_type)(thelist,new_type));
		} else {
			lc = (NclListClass)lc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
int _NclListGetType(NclObj thelist)
{
	NclListClass lc;
	if(thelist == NULL) {
		return(NhlFATAL);
	} else {
		lc = (NclListClass)thelist->obj.class_ptr;
	}
	while((NclObjClass)lc != nclObjClass) {
		if(lc->list_class.set_type!= NULL) {
			return((*lc->list_class.get_type)(thelist));
		} else {
			lc = (NclListClass)lc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
NclList _NclListSelect(NclList thelist, NclSelection* sel_ptr)
{
	NclListClass lc;
	if(thelist == NULL) {
		return(NULL);
	} else {
		lc = (NclListClass)thelist->obj.class_ptr;
	}
	while((NclObjClass)lc != nclObjClass) {
		if(lc->list_class.select!= NULL) {
			return((*lc->list_class.select)((NclObj)thelist,sel_ptr));
		} else {
			lc = (NclListClass)lc->obj_class.super_class;
		}
	}
	return(NULL);
}


void  _NclListDestroy(NclObj thelist)
{
	NclListClass lc;
	if(thelist == NULL) {
		return;
	} else {
		lc = (NclListClass)thelist->obj.class_ptr;
	}
	while((NclObjClass)lc != nclObjClass) {
		if(lc->obj_class.destroy != NULL) {
			(*lc->obj_class.destroy)(thelist);
		} else {
			lc = (NclListClass)lc->obj_class.super_class;
		}
	}
	return;
}

NhlErrorTypes _NclListAppend(NclObj thelist, NclObj theobj) 
{
	NclListClass lc;
	if(thelist == NULL) {
		return(NhlFATAL);
	} else {
		lc = (NclListClass)thelist->obj.class_ptr;
	}
	while((NclObjClass)lc != nclObjClass) {
		if(NULL != lc->list_class.append) {
			return((*lc->list_class.append)(thelist,theobj));
		} else {
			lc = (NclListClass)lc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}


void _NclBuildArrayOfList(void *tmp_val, int ndims, ng_size_t *dim_sizes)
{
        obj *id = (obj *)tmp_val;
        NclList tmp_list;
        ng_size_t i;
        ng_size_t n_items = 1;
        int list_type = (int) (NCL_FIFO);

        for(i = 0; i < ndims; i++)
                n_items *= dim_sizes[i];

        for(i = 0; i < n_items; i++)
        {
                tmp_list =(NclList)_NclListCreate(NULL,NULL,0,0,list_type);
                _NclListSetType((NclObj)tmp_list,list_type);
                id[i] = tmp_list->obj.id;
        }
}

