/*
 *      $Id: NclAtt.c,v 1.12 1996-06-17 22:15:12 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		NclAtt.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Feb 3 12:05:08 MST 1994
 *
 *	Description:	
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include "Symbol.h"
#include "NclAtt.h"
#include "NclMultiDValData.h"
#include "DataSupport.h"
#include "AttSupport.h"
extern void _NclVarMissingNotify(
#if     NhlNeedProto
NhlArgVal,
NhlArgVal
#endif
);

static void AttDestroyObj
#if	NhlNeedProto
(NclObj att)
#else
(att)
NclObj att;
#endif
{
	NclAttList *tmp,*tmp1;

	_NclUnRegisterObj((NclObj)att);
	
	tmp = ((NclAtt)att)->att.att_list;
	
	while(tmp != NULL) {
		if(tmp->attname != NULL) {
			NclFree(tmp->attname);
		}
		if(tmp->attvalue != NULL) {
			_NclDelParent((NclObj)tmp->attvalue,(NclObj)att);
		}
		tmp1 = tmp;
		tmp = tmp->next;
		NclFree(tmp1);
	}
	_NhlCBDestroy(((NclAtt)att)->att.cblist);
	NclFree(att);
	return;
}
static int AttIsAttFunction 
#if	NhlNeedProto
(NclAtt theattobj, char* attname)
#else 
(theattobj,attname)
	NclAtt theattobj;
	char *attname;
#endif
{
	int att_quark = NrmStringToQuark(attname);
        NclAttList *thelist;

        if(theattobj->att.att_list == NULL) {
                return(0);
        } else {
                thelist = theattobj->att.att_list;
                while(thelist != NULL) {
                        if(thelist->quark == att_quark) {
                                return(1);
                        }
                        thelist = thelist->next;
                }
                return(0);
        }
}


static struct _NclMultiDValDataRec * AttGetFunction
#if	NhlNeedProto
(NclAtt theattobj, char* attname,NclSelectionRecord * sel_ptr)
#else
(theattobj, attname,sel_ptr)
NclAtt theattobj;
char* attname;
NclSelectionRecord * sel_ptr;
#endif
{
        int att_quark = NrmStringToQuark(attname);
        NclAttList *thelist;
        NclMultiDValData tmp_md;

        thelist = theattobj->att.att_list;
        while(thelist != NULL) {
                if(thelist->quark == att_quark) {
                        break;
                } else {
                        thelist = thelist->next;
                }
         }
        if(thelist == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"Attribute (%s) is undefined",attname);
                return(NULL);
        } else {
                if(sel_ptr != NULL) {
			tmp_md = (NclMultiDValData)_NclReadSubSection((NclData)thelist->attvalue,sel_ptr,NULL);

                        if(tmp_md == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not read attribute (%s)",attname);
                                return(NULL);
                        } else {
                                return(tmp_md);
                        }
                } else {
                        return(thelist->attvalue);
                }
        }
}
static NhlErrorTypes AttAddFunction
#if	NhlNeedProto
(NclAtt theattobj, char* attname, NclMultiDValData value, NclSelectionRecord * sel_ptr)
#else
(theattobj, attname , value,  sel_ptr)
NclAtt theattobj;
char * attname;
NclMultiDValData value;
NclSelectionRecord * sel_ptr;
#endif
{
	NclAttList *thelist;
	NclMultiDValData targetdat,tmp_md;
	int lhs_type,rhs_type;
	int att_quark;
	NhlErrorTypes ret;
	NhlArgVal cbdata;
	NhlArgVal selector;
	NclScalar tmp_scalar;
	
	selector.lngval = 0;
	cbdata.ptrval = NULL;

	att_quark = NrmStringToQuark(attname);

	if(att_quark == NrmStringToQuark(NCL_MISSING_VALUE_ATT)) {
		memcpy((void*)&tmp_scalar,value->multidval.val,value->multidval.type->type_class.size);
		cbdata.ptrval = &tmp_scalar;
		_NhlCBCallCallbacks(theattobj->att.cblist,selector,cbdata);
	}
        thelist = theattobj->att.att_list;
        while(thelist != NULL) {
                if(thelist->quark == att_quark) {
                        break;
                } else {
                        thelist = thelist->next;
                }
        }
        if(thelist == NULL) {
                thelist = (NclAttList*)NclMalloc((unsigned)
                        sizeof(NclAttList));
                thelist->quark = att_quark;
                thelist->attname = (char*)NclMalloc((unsigned)
                strlen(attname)+1);
                        strcpy(thelist->attname, attname);
                if(_NclSetStatus((NclObj)value,PERMANENT)){
                        thelist->attvalue = value;
                } else {
                        thelist->attvalue = _NclCopyVal(value,NULL);
                        _NclSetStatus((NclObj)thelist->attvalue,PERMANENT);
                }
		_NclAddParent((NclObj)thelist->attvalue,(NclObj)theattobj);
                thelist->next = theattobj->att.att_list;
                theattobj->att.att_list = thelist;
                theattobj->att.n_atts++;
                return(NhlNOERROR);
        } else {
                targetdat = thelist->attvalue;
                if(value->multidval.n_dims > 1) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to assign value with more than one dimension to attribute, attributes are restricted to having only one dimension");
                        return(NhlFATAL);
                } else if(sel_ptr == NULL) {
                        lhs_type = targetdat->multidval.type->type_class.type;
                        rhs_type = value->multidval.type->type_class.type;
                        if(lhs_type != rhs_type) {
                                tmp_md = _NclCoerceData(value,targetdat->multidval.type->type_class.type ,(targetdat->multidval.missing_value.has_missing?&targetdat->multidval.missing_value.value:NULL));
                                if(tmp_md == NULL) {
                                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Attribute assignment type mismatch");
                                        return(NhlFATAL);
                                } else {
                                        if((value->obj.status != PERMANENT)&&(value != tmp_md)) {
/*
* value_md is either equal to value or had to be coerced. In the event it
* was coerced it must be freed. Therefore it will faile value != value_md
* conditional
*/
                                                _NclDestroyObj((NclObj)tmp_md);
                                        }
                                }
                        } else {
                                tmp_md = value;
                        }
                        if(tmp_md->multidval.dim_sizes[0] != targetdat->multidval.dim_sizes[0]) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension size of attribute and right-hand side of assignment do not match");
                                return(NhlFATAL);
                        }
                        if(_NclSetStatus((NclObj)tmp_md,PERMANENT)) {
                                thelist->attvalue = tmp_md;
				_NclDelParent((NclObj)targetdat,(NclObj)theattobj);
				_NclAddParent((NclObj)thelist->attvalue,(NclObj)theattobj);
                        } else {
                                thelist->attvalue = _NclCopyVal(tmp_md,NULL);
                                if(thelist->attvalue != NULL) {
                                        _NclSetStatus((NclObj)thelist->attvalue,PERMANENT);
					_NclAddParent((NclObj)thelist->attvalue,(NclObj)theattobj);
/*
* This destroys the old attribute value. This is ok since this is
* strogage that is internal to the NclVar object
*/
					_NclDelParent((NclObj)targetdat,(NclObj)theattobj);

                                } else {
/*
* Resets attvalue fied to previous value and returns
*/
                                        thelist->attvalue = targetdat;
                                        NhlPError(NhlFATAL,NhlEUNKNOWN,"An internal error occurred, could not create attribute");
                                        return(NhlFATAL);
                                }
                        }
			ret = NhlNOERROR;
                } else {
/*
* subscript exists
*/
                        lhs_type = targetdat->multidval.type->type_class.type & NCL_VAL_TYPE_MASK;
                        rhs_type = value->multidval.type->type_class.type & NCL_VAL_TYPE_MASK;
                        if(lhs_type != rhs_type) {
                                tmp_md = _NclCoerceData(value,targetdat->multidval.type->type_class.type,(targetdat->multidval.missing_value.has_missing?&targetdat->multidval.missing_value.value:NULL));
                                if(tmp_md == NULL) {
                                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Attribute assignment type mismatch");
                                        return(NhlFATAL);
                                } else {
                                        if((value->obj.status != PERMANENT)&&(value != tmp_md)) {
/*
* value_md is either equal to value or had to be coerced. In the event it
* was coerced it must be freed. Therefore it will faile value != value_md
* conditional
*/
                                                _NclDestroyObj((NclObj)tmp_md);
                                        }
                                }
                        } else {
                                tmp_md = value;
                        }
			ret = _NclWriteSubSection((NclData)targetdat,sel_ptr,(NclData)tmp_md);

                        if((tmp_md->obj.status != PERMANENT)&&(tmp_md != value)) {
                                _NclDestroyObj((NclObj)tmp_md);
                        }
                }
                return(ret);
        }
}

static NhlErrorTypes AttPrint
#if	NhlNeedProto
(NclObj theobj,FILE *fp)
#else
(theobj,fp)
NclObj theobj;
FILE *fp;
#endif
{
	NclAtt theattobj = (NclAtt)theobj;
	NclAttList *tmp;
	int ret = 0;
	NhlErrorTypes ret1 = NhlNOERROR;
	
	tmp = theattobj->att.att_list;
	ret = nclfprintf(fp,"Number Of Attributes: %d\n",theattobj->att.n_atts);
	while((tmp != NULL)&&(ret>=0)) {
		ret = nclfprintf(fp,"  %s :\t",tmp->attname);
		if(ret < 0) {
			return(NhlWARNING);
		}
		if(tmp->attvalue->multidval.totalelements ==1) {
			ret1 = _Nclprint(tmp->attvalue->multidval.type,fp,tmp->attvalue->multidval.val);
			if(ret1 < NhlINFO) {
				return(ret1);
			}
		} else {
			ret = nclfprintf(fp,"<ARRAY>",tmp->attname);
			if(ret < 0) {
				return(NhlWARNING);
			}
		}
		ret = nclfprintf(fp,"\n",tmp->attname);
		if(ret < 0) {
			return(NhlWARNING);
		}
		tmp = tmp->next;
	}
	return(NhlNOERROR);
}

static NhlErrorTypes AttAddParent
#if	NhlNeedProto
(struct _NclObjRec * theobj, NclObj parent)
#else
( theobj, parent)
struct _NclObjRec * theobj;
NclObj parent;
#endif
{
/* Preconditions: parent better only be add once */
	NclRefList *tmp;
	NclAttCBRec *tmpcb;
	NclAtt theattobj = (NclAtt) theobj;
	NhlArgVal selector;
        NhlArgVal udata;

	selector.lngval = 0;
	udata.intval = parent->obj.id;


	if(theattobj->obj.parents == NULL) {
		theattobj->obj.parents = (NclRefList*)NclMalloc(sizeof(NclRefList));
		theattobj->obj.parents->next = NULL;
		theattobj->obj.parents->pptr = parent;
		theattobj->obj.ref_count = 1;
	} else {	
		tmp = theattobj->obj.parents;
		theattobj->obj.parents = (NclRefList*)NclMalloc(sizeof(NclRefList));
		theattobj->obj.parents->next = tmp;
		theattobj->obj.parents->pptr = parent;
		theattobj->obj.ref_count++;
	}
	if(parent->obj.obj_type_mask & Ncl_Var) {
		if(theattobj->att.cbr_list == NULL) {
			theattobj->att.cbr_list = (NclAttCBRec*)NclMalloc(sizeof(NclAttCBRec));
			theattobj->att.cbr_list->pid = parent->obj.id;
			theattobj->att.cbr_list->next = NULL;
			theattobj->att.cbr_list->cb = _NhlCBAdd(theattobj->att.cblist,selector,_NclVarMissingNotify,udata);
		} else {
			tmpcb = theattobj->att.cbr_list;
			theattobj->att.cbr_list = (NclAttCBRec*)NclMalloc(sizeof(NclAttCBRec));
                	theattobj->att.cbr_list->pid = parent->obj.id;
                	theattobj->att.cbr_list->next = tmpcb;
               		theattobj->att.cbr_list->cb = _NhlCBAdd(theattobj->att.cblist,selector,_NclVarMissingNotify,udata);
		}
	}
	return(NhlNOERROR);
}

static NhlErrorTypes AttDelParent
#if	NhlNeedProto
(struct _NclObjRec * theobj, NclObj parent)
#else
( theobj, parent)
struct _NclObjRec * theobj;
NclObj parent;
#endif
{
	NclAtt theattobj = (NclAtt)theobj;
	NclRefList *tmp,*tmp1;
	NclAttCBRec *tmpcb, *tmpcb1;

	tmp = theattobj->obj.parents;
	if((tmp != NULL)&&(tmp->pptr == parent)){
		theattobj->obj.parents = theattobj->obj.parents->next;
		NclFree(tmp);
	} else {
		if(tmp == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"AttDelParent: Attempt to delete element from empty list");
			return(NhlFATAL);
		}
		while(tmp->next != NULL) {
			if(tmp->next->pptr == parent) {
				tmp1 = tmp->next;
				tmp->next = tmp->next->next;
				NclFree(tmp1);
			} else {
				tmp = tmp->next;
			}
		}
	}
	theattobj->obj.ref_count--;
	if(parent->obj.obj_type_mask & Ncl_Var) {
		tmpcb = theattobj->att.cbr_list;
		if((tmpcb != NULL)&&(tmpcb->pid == parent->obj.id)){
			_NhlCBDelete(theattobj->att.cbr_list->cb);
			theattobj->att.cbr_list = theattobj->att.cbr_list->next;
			NclFree(tmpcb);
		} else {
			if(tmpcb == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"AttDelParent: Attempt to delete element from empty list");
				return(NhlFATAL);
			}
			while(tmpcb->next != NULL) {
				if(tmpcb->next->pid== parent->obj.id) {
					tmpcb1 = tmpcb->next;
					tmpcb->next = tmpcb->next->next;
				 	_NhlCBDelete(tmpcb1->cb);
					NclFree(tmpcb1);
				} else {
					tmpcb = tmpcb->next;
				}
			}
		}
	}
	if(theattobj->obj.parents == NULL) {
		_NclDestroyObj((NclObj)theattobj);
	}
	return(NhlNOERROR);
}

static void AttDelFunction
#if	NhlNeedProto
(struct _NclAttRec * theattobj, char *attname)
#else
(theattobj, attname)
struct _NclAttRec * theattobj;
char *attname;
#endif
{
	NclAttList *tmp,*tmp1;
	NclAttCBRec *tmpcb, *tmpcb1;
	NhlArgVal cbdata;
	NhlArgVal selector;
	int att_quark = NrmStringToQuark(attname);

	cbdata.ptrval = NULL;
	selector.lngval = 0;

	tmp = theattobj->att.att_list;
	if((tmp!= NULL)&&(tmp->quark == att_quark)) {
		theattobj->att.att_list = theattobj->att.att_list->next;
		theattobj->att.n_atts--;
		if(NrmStringToQuark(NCL_MISSING_VALUE_ATT)==att_quark) {
			_NhlCBCallCallbacks(theattobj->att.cblist,selector,cbdata);
		}
		if(tmp->attname != NULL) {
			NclFree(tmp->attname);
		}
		_NclDelParent((NclObj)tmp->attvalue,(NclObj)theattobj);
		NclFree(tmp);
		return;
	}
	while(tmp->next != NULL) {
		if(tmp->next->quark == att_quark) {
			if(NrmStringToQuark(NCL_MISSING_VALUE_ATT)==att_quark) {
				_NhlCBCallCallbacks(theattobj->att.cblist,selector,cbdata);
			}
			tmp1 = tmp->next;
			tmp->next = tmp->next->next;
			if(tmp1->attname != NULL) {
				NclFree(tmp1->attname);
			}
			_NclDelParent((NclObj)tmp1->attvalue,(NclObj)theattobj);
			NclFree(tmp1);
			theattobj->att.n_atts--;
			return;
		} else {
			tmp = tmp->next;
		}
	}
	return;
}

static NclAtt AttCopy
#if	NhlNeedProto
(struct _NclAttRec * theattobj, struct _NclAttRec * storage)
#else
(theattobj, storage)
struct _NclAttRec * theattobj;
struct _NclAttRec * storage;
#endif
{
	NclAtt tmp = NULL;
	NclAttList *thelist;

	if(theattobj == NULL) {
		return(NULL);
	}

	tmp = (NclAtt)_NclGetObj(_NclAttCreate((NclObj)storage,NULL,Ncl_Att,0,NULL));

	if(tmp == NULL){
		return(NULL);
	}

	thelist = theattobj->att.att_list;
	
	while(thelist != NULL) {
		_NclAddAtt(tmp->obj.id,thelist->attname,_NclCopyVal(thelist->attvalue,NULL),NULL); 
		thelist = thelist->next;
	}

	return(tmp);
	
}
static NhlErrorTypes InitializeAttClass(
#if NhlNeedProto
	void
#endif
);
NclAttClassRec nclAttClassRec = {
	{
		"NclAttClass",
		sizeof(NclAttRec),
		(NclObjClass)&nclObjClassRec,
		1,
		(NclGenericFunction)AttDestroyObj,
		(NclSetStatusFunction)NULL,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)InitializeAttClass,
/* NclAddParentFunction add_parent */	AttAddParent,
/* NclDelParentFunction del_parent */	AttDelParent,
/* NclAttPrintFunction print */		AttPrint,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclAddAttFunction add_att */		AttAddFunction,
/* NclGetAttFunction get_att */		AttGetFunction,
/* NclDelAttFunction del_att */		AttDelFunction,
/* NclIsAttFunction is_att */		AttIsAttFunction,
/* NclCopyAttFunction copy_att */	AttCopy
	}
};

NclObjClass nclAttClass = (NclObjClass)&nclAttClassRec;

static NhlErrorTypes InitializeAttClass
#if NhlNeedProto
(void)
#else
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_Att,
		(NclObjClass)&nclAttClassRec
	);
	return(NhlNOERROR);
}

int _NclAttCreate
#if	NhlNeedProto
(struct _NclObjRec *inst, struct _NclObjClassRec *theclass, NclObjTypes obj_type, unsigned int obj_type_mask, struct _NclObjRec *parent)
#else 
(inst, theclass, obj_type, obj_type_mask, parent)
struct _NclObjRec *inst;
struct _NclObjClassRec *theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
struct _NclObjRec *parent;
#endif
{
	NclAtt my_inst;
	NclObjClass class_ptr;

	if(inst == NULL) {
		my_inst = (NclAtt)NclMalloc((unsigned)sizeof(NclAttRec));
	} else {
		my_inst = (NclAtt)inst; 
	}
	if(theclass == NULL) {
		class_ptr = nclAttClass;
	} else {
		class_ptr = theclass;
	}
	(void)_NclObjCreate((NclObj)my_inst,class_ptr,obj_type,(obj_type_mask | Ncl_Att),PERMANENT);

	my_inst->att.n_atts = 0;
	my_inst->att.att_list  = NULL;
	my_inst->att.cbr_list = NULL;
	my_inst->att.cblist = _NhlCBCreate(0,NULL,NULL);
	if(parent != NULL) {
		AttAddParent((NclObj)my_inst,parent);
	}

	return(my_inst->obj.id);
}

