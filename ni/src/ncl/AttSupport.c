/*
 *      $Id: AttSupport.c,v 1.6 2008-12-10 20:12:16 dbrown Exp $
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
 *	Date:		Thu Jan 13 14:52:04 MST 1994
 *
 *	Description:	
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
#include "NclAtt.h"
#include "NclMultiDValData.h"


void _NclAddAttParent
#if	NhlNeedProto
(int id , NclObj parent)
#else
(id , parent)
int id;
NclObj parent;
#endif
{
	NclObj theattobj;
	NclAttClass ac;


	theattobj = (NclObj)_NclGetObj(id);
	if(theattobj == NULL) {
		return;
	} else {
		ac = (NclAttClass)theattobj->obj.class_ptr;
	}

	while((NclObjClass)ac != NULL) {	
		if(ac->obj_class.add_parent!= NULL) {
			(*ac->obj_class.add_parent)(theattobj,parent);
		}  else {
			ac = (NclAttClass)ac->obj_class.super_class;
		}
	}
	return;
}

NhlErrorTypes _NclAddAtt
#if	NhlNeedProto
(int id, char * attname, struct _NclMultiDValDataRec  *value, NclSelectionRecord * sel_ptr)
#else
(id,attname,value,sel_ptr)
int id;
char * attname;
struct _NclMultiDValDataRec  *value;
NclSelectionRecord * sel_ptr;
#endif
{
	struct _NclAttRec *theattobj;
	NclAttClass ac;

	theattobj = (NclAtt)_NclGetObj(id);
	if(theattobj == NULL) {
		return(NhlFATAL);
	} else {
		ac = (NclAttClass)theattobj->obj.class_ptr;
	}
	while((NclObjClass) ac != NULL) {
		if(ac->att_class.add_att!= NULL) {
			return((*ac->att_class.add_att)(theattobj,attname,value,sel_ptr));
		}   else {
			ac = (NclAttClass)ac->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

void _NclDeleteAttParent
#if	NhlNeedProto
(int id, NclObj parent)
#else
(id, parent)
int id;
NclObj parent;
#endif
{
	NclAtt theattobj;
	NclAttClass ac;


	theattobj = (NclAtt)_NclGetObj(id);
	if(theattobj == NULL) {
                return;
        } else {
                ac = (NclAttClass)theattobj->obj.class_ptr;
        }
        while((NclObjClass) ac != NULL) {

		if(ac->obj_class.del_parent!= NULL) {
			(*ac->obj_class.del_parent)((NclObj)theattobj,parent);
		} else {
			ac = (NclAttClass) ac->obj_class.super_class;
		}
	}
	return;

}

int _NclIsAtt
#if	NhlNeedProto
(int id, char *name)
#else
(id, name)
int id;
char *name;
#endif
{
	NclAtt theattobj;
	NclAttClass ac;

	theattobj = (NclAtt)_NclGetObj(id);
	if(theattobj == NULL) {
		return(0);
	} else {
		ac = (NclAttClass)theattobj->obj.class_ptr;
	}
	while((NclObjClass)ac != NULL) {
		if(ac->att_class.is_att!= NULL) {
			return((*ac->att_class.is_att)(theattobj,name));
		} else {
			ac = (NclAttClass) ac->obj_class.super_class;
		}
	} 
	return(0);
}

struct _NclMultiDValDataRec* _NclGetAtt
#if	NhlNeedProto
(int id, char *attname, NclSelectionRecord *sel_ptr)
#else
(id, attname, sel_ptr)
int id;
char *attname;
NclSelectionRecord *sel_ptr;
#endif
{
	NclAtt theattobj;
	NclAttClass ac;

	theattobj = (NclAtt)_NclGetObj(id);
	if(theattobj == NULL) {
		return(NULL);
	} else {
		ac = (NclAttClass)theattobj->obj.class_ptr;
	}
	while((NclObjClass)ac != NULL) {
		if(ac->att_class.get_att!= NULL) {
			return((*ac->att_class.get_att)(theattobj,attname,sel_ptr));
		} else {
			ac = (NclAttClass)ac->obj_class.super_class;
		}
	} 
	return(NULL);
	
}
void _NclDeleteAttMDID
#if	NhlNeedProto
(int id, int md_id)
#else
(id, md_id)
int id;
int md_id;
#endif
{
	NclAtt theattobj;
	NclAttClass ac;
	char *attname;
	NclAttList *tmp;
	NclMultiDValData tmp_md;

	theattobj = (NclAtt)_NclGetObj(id);
	if(theattobj == NULL) {
		return;
	} else {
		ac = (NclAttClass)theattobj->obj.class_ptr;
	}
	tmp = theattobj->att.att_list;
	attname = NULL;
	while(tmp != NULL) {
		tmp_md = tmp->attvalue;
		if(tmp_md->obj.id == md_id) {
			attname = NrmQuarkToString(tmp->quark);
			break;
		} else {
			tmp= tmp->next;
		}
	}
	if(tmp == NULL) {
		return;
	}
	
	while(ac != NULL) {
		if(ac->att_class.del_att!= NULL) {
			(*ac->att_class.del_att)(theattobj,attname);
			return;
		} else {
			ac = (NclAttClass)ac->obj_class.super_class;
		}
	}
	return;
}



void _NclDeleteAtt
#if	NhlNeedProto
(int id, char * attname)
#else
(id, attname)
int id;
char * attname;
#endif
{
	NclAtt theattobj;
	NclAttClass ac;

	theattobj = (NclAtt)_NclGetObj(id);
	if(theattobj == NULL) {
		return;
	} else {
		ac = (NclAttClass)theattobj->obj.class_ptr;
	}
	while(ac != NULL) {
		if(ac->att_class.del_att!= NULL) {
			(*ac->att_class.del_att)(theattobj,attname);
			return;
		} else {
			ac = (NclAttClass)ac->obj_class.super_class;
		}
	}
	return;
}


struct _NclAttRec * _NclCopyAtt
#if	NhlNeedProto
(struct _NclAttRec * theattobj, struct _NclAttRec * storage)
#else
(theattobj, storage)
struct _NclAttRec * theattobj;
struct _NclAttRec * storage;
#endif
{
	NclAttClass ac;

	if(theattobj == NULL) {
		return(NULL);
	} else {
		ac = (NclAttClass)theattobj->obj.class_ptr;
	}
	while((NclObjClass) ac != nclObjClass) {
		if(ac->att_class.copy_att != NULL) {
                	return((NclAtt)(*ac->att_class.copy_att)(theattobj,storage));
        	} else {
			ac = (NclAttClass) ac->obj_class.super_class;
        	}
	}
        return(NULL);

}

