
/*
 *      $Id: NclData.c,v 1.2 1994-08-25 18:00:40 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
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
 *	Date:		Fri Oct 29 16:19:58 MDT 1993
 *
 *	Description:	
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclData.h"
#include "DataSupport.h"


static int ObjSetStatus(
#if  NhlNeedProto
NclObj /*self*/,
NclStatus /* requested */
#endif
);

NclObjClassRec nclObjClassRec = {
	{
		"NclObjClass",
		sizeof(NclObjRec),
		(NclObjClass) NULL,
		0,
		(NclGenericFunction)NULL,
		(NclSetStatusFunction)ObjSetStatus,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)NULL,
		(NclAddParentFunction)NULL,
		(NclDelParentFunction)NULL,
/* NclPrintFunction      print; 	*/ 	NULL,
	}
};

NclObjClass nclObjClass = (NclObjClass)&nclObjClassRec;


static NhlErrorTypes NclDataInitializePart(
#ifdef NhlNeedProto
NclObjClass	/*self*/
#endif
);

NclDataClassRec nclDataClassRec = {
	{
		"NclDataClass",
		sizeof(NclDataRec),
		(NclObjClass)&nclObjClassRec,
		0,
		(NclGenericFunction)NULL,
		(NclSetStatusFunction)NULL,
		NclDataInitializePart,
		NULL,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
                (NclPrintFunction)NULL,
	},
	{
/* NclCopyFunction      dup; 	 	*/ 	NULL,
/* NclResetMissingValueFunctin reset_mis; 	 	*/ 	NULL,
/* NclReadSubSecFunction  r_subsection;*/	NULL,
/* NclWriteSubSecFunction  w_subsection;*/	{NULL,NULL},
/* NclReadThenWriteSecFunc r_then_wsubsection;*/	NULL,
/* NclDataFunction         coerce; 	*/ 	{NULL,NULL},
/* NclOperatorFunction     multiply; 	*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     plus; 	*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     minus; 	*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     divide; 	*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     exponent; 	*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     mod; 	*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     mat; 	*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     sel_lt; 	*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     sel_gt; 	*/	{NULL,NULL,NULL,NULL},
/* NclSingleOperatorFunction  not;	*/	{NULL,NULL},
/* NclSingleOperatorFunction  neg;	*/	{NULL,NULL},
/* NclOperatorFunction     gt;		*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     lt;		*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     ge;		*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     le;		*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     ne;		*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     eq;		*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     and;		*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     or;		*/	{NULL,NULL,NULL,NULL},
/* NclOperatorFunction     xor;		*/	{NULL,NULL,NULL,NULL},
/* NclIsMissingFunction    is_mis;	*/	{NULL}
	}
};

NclObjClass nclDataClass = (NclObjClass)&nclDataClassRec;

static NhlErrorTypes NclDataInitializePart
#if  __STDC__
(NclObjClass self)
#else
(self)
	NclObjClass self;
#endif
{
	NclDataClass	par = (NclDataClass)self->obj_class.super_class;
	NclDataClass	me = (NclDataClass)self;
	if((self->obj_class.super_class != nclDataClass)&&
		(self->obj_class.super_class != nclObjClass)) {
		par = (NclDataClass)self->obj_class.super_class;

/*
* Coerce
*/
		if(me->data_class.coerce[0] == NULL) {
			me->data_class.coerce[0] = par->data_class.coerce[0];
		}
		if(me->data_class.coerce[1] == NULL) {
			me->data_class.coerce[1] = par->data_class.coerce[1];
		}

/*
* Assign
*/
/*
		if(me->data_class.assign[0] == NULL) {
			me->data_class.assign[0] = par->data_class.assign[0];
		}
		if(me->data_class.assign[1] == NULL) {
			me->data_class.assign[1] = par->data_class.assign[1];
		}
		if(me->data_class.assign[2] == NULL) {
			me->data_class.assign[2] = par->data_class.assign[2];
		}
		if(me->data_class.assign[3] == NULL) {
			me->data_class.assign[3] = par->data_class.assign[3];
		}
*/
/*
* Multiply
*/
		if(me->data_class.multiply[0] == NULL) {
			me->data_class.multiply[0] =par->data_class.multiply[0];
		}
		if(me->data_class.multiply[1] == NULL) {
			me->data_class.multiply[1] =par->data_class.multiply[1];
		}
		if(me->data_class.multiply[2] == NULL) {
			me->data_class.multiply[2] =par->data_class.multiply[2];
		}
		if(me->data_class.multiply[3] == NULL) {
			me->data_class.multiply[3] =par->data_class.multiply[3];
		}
/*
* Plus
*/
		if(me->data_class.plus[0] == NULL) {
			me->data_class.plus[0] = par->data_class.plus[0];
		}
		if(me->data_class.plus[1] == NULL) {
			me->data_class.plus[1] = par->data_class.plus[1];
		}
		if(me->data_class.plus[2] == NULL) {
			me->data_class.plus[2] = par->data_class.plus[2];
		}
		if(me->data_class.plus[3] == NULL) {
			me->data_class.plus[3] = par->data_class.plus[3];
		}
/*
* Minus
*/
		if(me->data_class.minus[0] == NULL) {
			me->data_class.minus[0] = par->data_class.minus[0];
		}
		if(me->data_class.minus[1] == NULL) {
			me->data_class.minus[1] = par->data_class.minus[1];
		}
		if(me->data_class.minus[2] == NULL) {
			me->data_class.minus[2] = par->data_class.minus[2];
		}
		if(me->data_class.minus[3] == NULL) {
			me->data_class.minus[3] = par->data_class.minus[3];
		}
/*
* Divide
*/
		if(me->data_class.divide[0] == NULL) {
			me->data_class.divide[0] = par->data_class.divide[0];
		}
		if(me->data_class.divide[1] == NULL) {
			me->data_class.divide[1] = par->data_class.divide[1];
		}
		if(me->data_class.divide[2] == NULL) {
			me->data_class.divide[2] = par->data_class.divide[2];
		}
		if(me->data_class.divide[3] == NULL) {
			me->data_class.divide[3] = par->data_class.divide[3];
		}
/*
* Exponent
*/
		if(me->data_class.exponent[0] == NULL) {
			me->data_class.exponent[0] =par->data_class.exponent[0];
		}
		if(me->data_class.exponent[1] == NULL) {
			me->data_class.exponent[1] =par->data_class.exponent[1];
		}
		if(me->data_class.exponent[2] == NULL) {
			me->data_class.exponent[2] =par->data_class.exponent[2];
		}
		if(me->data_class.exponent[3] == NULL) {
			me->data_class.exponent[3] =par->data_class.exponent[3];
		}
/*
* Mod
*/
		if(me->data_class.mod[0] == NULL) {
			me->data_class.mod[0] = par->data_class.mod[0];
		}
		if(me->data_class.mod[1] == NULL) {
			me->data_class.mod[1] = par->data_class.mod[1];
		}
		if(me->data_class.mod[2] == NULL) {
			me->data_class.mod[2] = par->data_class.mod[2];
		}
		if(me->data_class.mod[3] == NULL) {
			me->data_class.mod[3] = par->data_class.mod[3];
		}
/*
* Mat
*/
		if(me->data_class.mat[0] == NULL) {
			me->data_class.mat[0] = par->data_class.mat[0];
		}
		if(me->data_class.mat[1] == NULL) {
			me->data_class.mat[1] = par->data_class.mat[1];
		}
		if(me->data_class.mat[2] == NULL) {
			me->data_class.mat[2] = par->data_class.mat[2];
		}
		if(me->data_class.mat[3] == NULL) {
			me->data_class.mat[3] = par->data_class.mat[3];
		}

/*
* SEL_LT
*/
		if(me->data_class.sel_lt[0] == NULL) {
			me->data_class.sel_lt[0] = par->data_class.sel_lt[0];
		}
		if(me->data_class.sel_lt[1] == NULL) {
			me->data_class.sel_lt[1] = par->data_class.sel_lt[1];
		}
		if(me->data_class.sel_lt[2] == NULL) {
			me->data_class.sel_lt[2] = par->data_class.sel_lt[2];
		}
		if(me->data_class.sel_lt[3] == NULL) {
			me->data_class.sel_lt[3] = par->data_class.sel_lt[3];
		}
/*
* SEL_GT
*/
		if(me->data_class.sel_gt[0] == NULL) {
			me->data_class.sel_gt[0] = par->data_class.sel_gt[0];
		}
		if(me->data_class.sel_gt[1] == NULL) {
			me->data_class.sel_gt[1] = par->data_class.sel_gt[1];
		}
		if(me->data_class.sel_gt[2] == NULL) {
			me->data_class.sel_gt[2] = par->data_class.sel_gt[2];
		}
		if(me->data_class.sel_gt[3] == NULL) {
			me->data_class.sel_gt[3] = par->data_class.sel_gt[3];
		}
/*
* NOT
*/

		if(me->data_class.not[0] == NULL) {
			me->data_class.not[0] = par->data_class.not[0];
		}
		if(me->data_class.not[1] == NULL) {
			me->data_class.not[1] = par->data_class.not[1];
		}
/*
* Neg
*/

		if(me->data_class.neg[0] == NULL) {
			me->data_class.neg[0] = par->data_class.neg[0];
		}
		if(me->data_class.neg[1] == NULL) {
			me->data_class.neg[1] = par->data_class.neg[1];
		}
/*
* GT
*/
		if(me->data_class.gt[0] == NULL) {
			me->data_class.gt[0] = par->data_class.gt[0];
		}
		if(me->data_class.gt[1] == NULL) {
			me->data_class.gt[1] = par->data_class.gt[1];
		}
		if(me->data_class.gt[2] == NULL) {
			me->data_class.gt[2] = par->data_class.gt[2];
		}
		if(me->data_class.gt[3] == NULL) {
			me->data_class.gt[3] = par->data_class.gt[3];
		}
/*
* Lt
*/
		if(me->data_class.lt[0] == NULL) {
			me->data_class.lt[0] = par->data_class.lt[0];
		}
		if(me->data_class.lt[1] == NULL) {
			me->data_class.lt[1] = par->data_class.lt[1];
		}
		if(me->data_class.lt[2] == NULL) {
			me->data_class.lt[2] = par->data_class.lt[2];
		}
		if(me->data_class.lt[3] == NULL) {
			me->data_class.lt[3] = par->data_class.lt[3];
		}
/*
* Ge
*/

		if(me->data_class.ge[0] == NULL) {
			me->data_class.ge[0] = par->data_class.ge[0];
		}
		if(me->data_class.ge[1] == NULL) {
			me->data_class.ge[1] = par->data_class.ge[1];
		}
		if(me->data_class.ge[2] == NULL) {
			me->data_class.ge[2] = par->data_class.ge[2];
		}
		if(me->data_class.ge[3] == NULL) {
			me->data_class.ge[3] = par->data_class.ge[3];
		}

/*
* Le
*/
		if(me->data_class.le[0] == NULL) {
			me->data_class.le[0] = par->data_class.le[0];
		}
		if(me->data_class.le[1] == NULL) {
			me->data_class.le[1] = par->data_class.le[1];
		}
		if(me->data_class.le[2] == NULL) {
			me->data_class.le[2] = par->data_class.le[2];
		}
		if(me->data_class.le[3] == NULL) {
			me->data_class.le[3] = par->data_class.le[3];
		}
/*
*Ne
*/
		if(me->data_class.ne[0] == NULL) {
			me->data_class.ne[0] = par->data_class.ne[0];
		}
		if(me->data_class.ne[1] == NULL) {
			me->data_class.ne[1] = par->data_class.ne[1];
		}
		if(me->data_class.ne[2] == NULL) {
			me->data_class.ne[2] = par->data_class.ne[2];
		}
		if(me->data_class.ne[3] == NULL) {
			me->data_class.ne[3] = par->data_class.ne[3];
		}

/*
* eq
*/
		if(me->data_class.eq[0] == NULL) {
			me->data_class.eq[0] = par->data_class.eq[0];
		}
		if(me->data_class.eq[1] == NULL) {
			me->data_class.eq[1] = par->data_class.eq[1];
		}
		if(me->data_class.eq[2] == NULL) {
			me->data_class.eq[2] = par->data_class.eq[2];
		}
		if(me->data_class.eq[3] == NULL) {
			me->data_class.eq[3] = par->data_class.eq[3];
		}
/*
* And
*/
		if(me->data_class.and[0] == NULL) {
			me->data_class.and[0] = par->data_class.and[0];
		}
		if(me->data_class.and[1] == NULL) {
			me->data_class.and[1] = par->data_class.and[1];
		}
		if(me->data_class.and[2] == NULL) {
			me->data_class.and[2] = par->data_class.and[2];
		}
		if(me->data_class.and[3] == NULL) {
			me->data_class.and[3] = par->data_class.and[3];
		}
/*
* Or
*/
		if(me->data_class.or[0] == NULL) {
			me->data_class.or[0] = par->data_class.or[0];
		}
		if(me->data_class.or[1] == NULL) {
			me->data_class.or[1] = par->data_class.or[1];
		}
		if(me->data_class.or[2] == NULL) {
			me->data_class.or[2] = par->data_class.or[2];
		}
		if(me->data_class.or[3] == NULL) {
			me->data_class.or[3] = par->data_class.or[3];
		}
/*
* XOR
*/
		if(me->data_class.xor[0] == NULL) {
			me->data_class.xor[0] = par->data_class.xor[0];
		}
		if(me->data_class.xor[1] == NULL) {
			me->data_class.xor[1] = par->data_class.xor[1];
		}
		if(me->data_class.xor[2] == NULL) {
			me->data_class.xor[2] = par->data_class.xor[2];
		}
		if(me->data_class.xor[3] == NULL) {
			me->data_class.xor[3] = par->data_class.xor[3];
		}

	}
	return(NhlNOERROR);
}

/*
* NclSetStatus is a little different than the functions that went into
* the DataSupport file so I put it here. Instead of just directly calling
* the set_status field of the class pointer this one walks up the class
* heirarchy
*/
int _NclSetStatus
#if     __STDC__
(NclObj  obj , NclStatus requested)
#else 
(obj , requested)
NclObj  obj;
NclStatus requested;
#endif
{
	NclObjClass obc;

	obc = obj->obj.class_ptr;
	
	while((obc != NULL)&&(obc->obj_class.set_status == NULL)) {
		obc = obc->obj_class.super_class;
	}
	if(obc != NULL) {
		return((*obc->obj_class.set_status)(obj,requested));
	} else {
		return(0);
	}
}

struct _NclDataRec * _NclDataCreate
#if     __STDC__
(NclObj inst , NclObjClass theclass , NclObjTypes obj_type , unsigned int obj_type_mask, NclStatus status)
#else
(inst , theclass , obj_type ,obj_type_mask, status)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
#endif
{
 	return((NclData)_NclObjCreate(inst , theclass , obj_type ,(obj_type_mask | Ncl_Data), status));
}

struct _NclObjRec * _NclObjCreate
#if     __STDC__
(NclObj inst , NclObjClass theclass , NclObjTypes obj_type , unsigned int obj_type_mask, NclStatus status)
#else
(inst , theclass , obj_type ,obj_type_mask, status)
NclObj inst ;
NclObjClass theclass ;
NclObjTypes obj_type ;
unsigned int obj_type_mask;
NclStatus status;
#endif
{
	NclObj theinst;

	if(inst != NULL) {
		theinst = inst;
	} else {
		theinst = (NclObj)NclMalloc(sizeof(NclObjRec));
	}
	theinst->obj.self = inst;
	theinst->obj.class_ptr = theclass;
	theinst->obj.obj_type = obj_type;
	theinst->obj.obj_type_mask = (Ncl_Obj | obj_type_mask);
	theinst->obj.status = status;
	theinst->obj.id = _NclRegisterObj(theinst);
	theinst->obj.parents = NULL;
	theinst->obj.ref_count = 0;
	return(theinst);
}




static int ObjSetStatus
#if  __STDC__
(NclObj self, NclStatus requested )
#else
(self, requested )
NclObj self;
NclStatus requested;
#endif
{
	if(self->obj.status == TEMPORARY) {
		self->obj.status = requested;
		return(1);
	} else {
		return(0);
	}
}

static int current_id = 0;
static struct _NclObjList *objs  = NULL;
#define  OBJ_LIST_START_SIZE 512
static int current_size = OBJ_LIST_START_SIZE;


void _NclUnRegisterObj
#if	__STDC__
(NclObj self)
#else
(self)
NclObj self;
#endif
{
	objs[self->obj.id].freed = 1;
}

static char *_NclStatusString
#if __STDC__
(NclStatus status)
#else
(status)
NclStatus status;
#endif
{
	switch(status) {
	case PERMANENT:
		return("PERMANENT");
	case TEMPORARY:
		return("TEMPORARY");
	case STATIC:
		return("STATIC");
	default:
		return("Unknown status");
	}
}
void _NclPrintUnfreedObjs
#if	__STDC__
(FILE *fp)
#else
(fp)
FILE *fp;
#endif
{
	int i;
	
	for(i = 0; i < current_id; i++) {
		if(!objs[i].freed) {
			fprintf(fp,"\n------%d------\n",i);
			fprintf(fp,"Index: %d\n",i);
			fprintf(fp,"Object Class: %s\n",objs[i].theobj->obj.class_ptr->obj_class.class_name);
			fprintf(fp,"Object Status: %s\n",_NclStatusString(objs[i].theobj->obj.status));

			_NclPrint(objs[i].theobj,fp);
		}
	}
}
int _NclNumObjs
#if 	__STDC__
(void)
#else
()
#endif
{
	int i;
	int total = 0;
	
	for(i = 0; i < current_id; i++) {
		if(!objs[i].freed) {
			total++;
		}
	}
	return(total);
}

struct _NclObjRec *_NclGetObj 
#if __STDC__
(int id)
#else 
(id)
	int id;
#endif
{
	if((id <0)||(id > current_id)||objs[id].freed) {
		return(NULL);
	} else {
		return(objs[id].theobj);
	}
}
int _NclRegisterObj
#if	__STDC__
(NclObj self)
#else
(self)
NclObj self;
#endif
{
	int tmp;
	
	if(objs == NULL) {
		objs = (NclObjList*)NclMalloc((unsigned)sizeof(NclObjList)*current_size);
	} else if(current_id >= current_size) {
		objs = (NclObjList*)NclRealloc((char*)objs,sizeof(NclObjList)*current_size * 2);
		current_size *= 2;
	}

	
	
	tmp = current_id;
	objs[current_id].freed = 0;
	objs[current_id].obj_type = self->obj.obj_type;
	objs[current_id].obj_type_mask = self->obj.obj_type_mask;
	objs[current_id].theobj = self;
	current_id++;
	return(tmp);
}
