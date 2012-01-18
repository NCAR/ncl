
/*
 *      $Id: NclData.c,v 1.25 2009-07-09 22:40:47 dbrown Exp $
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
#include <limits.h>
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include "NclData.h"
#include "DataSupport.h"


static int ObjSetStatus(
#if  NhlNeedProto
NclObj /*self*/,
NclStatus /* requested */
#endif
);

static NhlErrorTypes InitializeObjClass(
#if NhlNeedProto
void
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
		(NclInitClassFunction)InitializeObjClass,
		(NclAddParentFunction)NULL,
		(NclDelParentFunction)NULL,
/* NclPrintSummaryFunction print_summary */ NULL,
/* NclPrintFunction      print; 	*/  NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	}
};

NclObjClass nclObjClass = (NclObjClass)&nclObjClassRec;

static NhlErrorTypes InitializeObjClass
#if  NhlNeedProto
(void)
#else
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_Obj,
		(NclObjClass)&nclObjClassRec
	);
	return(NhlNOERROR);
}

static NhlErrorTypes NclDataInitializeClass(
#if	NhlNeedProto
void
#endif
);
static NhlErrorTypes NclDataInitializePart(
#if	NhlNeedProto
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
		NclDataInitializeClass,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
                (NclPrintFunction)NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
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
/* NclIsMissingFunction    is_mis;	*/	NULL
	}
};

NclObjClass nclDataClass = (NclObjClass)&nclDataClassRec;

static NhlErrorTypes NclDataInitializeClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_Data,
		(NclObjClass)&nclDataClassRec
	);
	return(NhlNOERROR);
}
static NhlErrorTypes NclDataInitializePart
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
	theinst->obj.is_constant = -1;
	theinst->obj.parents = NULL;
	theinst->obj.ref_count = 0;
	theinst->obj.cblist =  NULL;
	return(theinst);
}




static int ObjSetStatus
#if	NhlNeedProto
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

int debug_obj_table = 0;
static int current_id = 0;
#define  OBJ_LIST_START_SIZE 8192
static struct _NclObjList objs[OBJ_LIST_START_SIZE];
static int current_size = OBJ_LIST_START_SIZE;
static long long total_obj_count = 0;
static int recycled = 0;
/*  
 * If we have to recycle go back to an id well above the low number that will tend
 * to be permanently assigned, but also well below the max id which will keep things
 * separate from recently assigned ids.
 */

#define RECYCLE_START_ID 100000000
#define MAX_ID INT_MAX - 10

/*
#define MAX_ID 1000000
#define RECYCLE_START_ID 250000
*/


void _NclUnRegisterObj
#if	NhlNeedProto
(NclObj self)
#else
(self)
NclObj self;
#endif
{
	int tmp;
	NclObjList *step,*temp;

#if 0
        if (self->obj.id + 1 == self->obj.is_constant) {
		printf("not unregistering constant object of type %d\n", (int) self->obj.obj_type);
		/*return*/;
	}
	if (debug_obj_table) 
		printf("%d obj unregistered: obj_type %d status %d ref_count %d\n",self->obj.id,(int) self->obj.obj_type,(int)self->obj.status,self->obj.ref_count);
#endif
	tmp = self->obj.id % current_size;
	if(objs[tmp].id == -1) {
		return;
	}
	if(objs[tmp].id == self->obj.id) {
		objs[tmp].id = -1;
		objs[tmp].theobj = NULL;
		if(objs[tmp].next != NULL) {
			step = objs[tmp].next;
			objs[tmp] = *objs[tmp].next;
			NclFree(step);
		}
	} else {
		step = &objs[tmp];
		while((step->next != NULL)&&(step->next->id != self->obj.id)) {
			step= step->next;
		}
		if((step->next != NULL)&&(step->next->id == self->obj.id)) {
			temp = step->next;
			step->next = step->next->next;
			NclFree(temp);
		}
	}
}

static char *_NclStatusString
#if	NhlNeedProto
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
void _NclFreeConstants
#if	NhlNeedProto
(int num)
#else
(num)
int num;
#endif
{
	int i;
	NclObjList *tmp;

	if(current_id > 0) {	
		for(i = 0; i < num; i++) {
			if(objs[i % current_size].id == i) {
				_NclDestroyObj((NclObj)objs[i%current_size].theobj);
			} else {
				tmp = objs[i%current_size].next;
				while(tmp != NULL) {
					if(tmp->id == i) {
						_NclDestroyObj((NclObj)tmp->theobj);
						break;
					}
					tmp = tmp->next;
				}
				
			}
		}
	}
}

void _NclPrintUnfreedObjs
#if	NhlNeedProto
(FILE *fp)
#else
(fp)
FILE *fp;
#endif
{
	int i;
	NclObjList *tmp;
	int count;
	int total = 0;
	int min_count,max_count;

	if(current_id > 0) {	
		min_count = 1000000000;
		max_count = 0;
		for(i = 0; i < current_size; i++) {
			count = 0;
			if(objs[i].id != -1) {
				fprintf(fp,"\n------%d------\n",objs[i].id);
				fprintf(fp,"Index: %d\n",i);
				fprintf(fp,"Object Class: %s\n",objs[i].theobj->obj.class_ptr->obj_class.class_name);
				fprintf(fp,"Object Status: %s\n",_NclStatusString(objs[i].theobj->obj.status));

				/*(void) _NclPrint(objs[i].theobj,fp);*/
				count++;
				total++;
			}
			tmp = objs[i].next;
			while(tmp != NULL) {
				fprintf(fp,"\n------%d------\n",tmp->id);
				fprintf(fp,"Index: %d\n",i);
				fprintf(fp,"Object Class: %s\n",tmp->theobj->obj.class_ptr->obj_class.class_name);
				fprintf(fp,"Object Status: %s\n",_NclStatusString(tmp->theobj->obj.status));
	
				/*(void)_NclPrint(tmp->theobj,fp);*/
				count++;
				total++;
				tmp = tmp->next;
			}
			fprintf(fp,"Entries for this hash index: %d\n",count);
			if (count > max_count) max_count = count;
			if (count < min_count) min_count = count;
			
		}
		fprintf(fp,"Min entries per index: %d; max entries per index: %d\n",min_count,max_count);
		fprintf(fp,"Current object total: %d\n", total);
		
/*
		NclFree(objs);
*/
	}
}
int _NclNumObjs
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int i;
	int total = 0;
	NclObjList *tmp;

	if(objs != NULL) {	
		for(i = 0; i < current_size; i++) {
			if(objs[i].id != -1) {
				total++;
			}
			tmp = objs[i].next ;
			while(tmp != NULL) {
				total++;
				tmp = tmp->next;
			}
		}
	}
	return(total);
}
int num_get_obj = 0;
void _NclNumGetObjCals
#if	NhlNeedProto
(FILE *fp)
#else
(fp)
FILE *fp;
#endif
{
	fprintf(fp,"The number of _NclGetObj calls was %d\n",num_get_obj);
}

static struct _NclObjRec *find_in_list
#if     NhlNeedProto
(NclObjList *ptr,int id)
#else
(NclObjList *ptr,int id)
#endif
{
	while((ptr != NULL)&&(ptr->id != id)) {
		ptr = ptr->next;
	}
	if((ptr == NULL) || (ptr->id != id)) {
		return(NULL);
	} else {
		return(ptr->theobj);
	}
}
struct _NclObjRec *_NclGetObj 
#if	NhlNeedProto
(int id)
#else 
(id)
	int id;
#endif
{
	int tmp = abs(id % current_size);
	NclObjList *ptr =  &objs[tmp]; 
#ifdef NCLDEBUG
	num_get_obj++;
#endif
	return((objs[tmp].id == id) ? objs[tmp].theobj : find_in_list(ptr,id));
}

int _NclRegisterObj
#if	NhlNeedProto
(NclObj self)
#else
(self)
NclObj self;
#endif
{
	int tmp;
	NclObjList *ptr;
	static int first = 1;

	if(first) {
		first  = 0;
		/*
		  if(objs == NULL) {
		  objs = (NclObjList*)NclMalloc((unsigned)sizeof(NclObjList)*current_size);
		*/
		for(tmp = 0; tmp < current_size; tmp++) {
			objs[tmp].id = -1;
			objs[tmp].obj_type = Ncl_None;
			objs[tmp].obj_type_mask = 0;
			objs[tmp].theobj = NULL;
			objs[tmp].next = NULL;
		}
	} 

	switch(current_id) {
	case 40:
	case 191:
	case 170:
		tmp = 0;
		break;
	default:
		break;
	}

	
	
	if (recycled) {
		/* 
		 * Make sure we don't use an object that exists;
		 */
		while (_NclGetObj(current_id)) 
			current_id++;
	}
	
	tmp = current_id % current_size;
	if (objs[tmp].id == current_id) {
		NhlPError(NhlFATAL, NhlEUNKNOWN, 
			  "_NclRegisterObj: internal error; invalid duplication of object id %d\n",current_id);
		return (NhlFATAL);
	}

	if(objs[tmp].id == -1) {
		ptr = &(objs[tmp]);
	}
	else if(objs[tmp].next == NULL) {
		objs[tmp].next = (NclObjList*)NclMalloc(sizeof(NclObjList));
		ptr = objs[tmp].next;
		ptr->next = NULL;
	} 
	else {
		ptr = objs[tmp].next;
		while (ptr->next != NULL) {
			if (ptr->id == current_id) {
				NhlPError(NhlFATAL, NhlEUNKNOWN, 
					  "_NclRegisterObj: internal error; invalid duplication of object id %d\n",current_id);
				return (NhlFATAL);
			}
			ptr = ptr->next;
		}
		ptr->next = (NclObjList*)NclMalloc(sizeof(NclObjList));
		ptr = ptr->next;
		ptr->next = NULL;
	}

	ptr->id = current_id;
	ptr->obj_type = self->obj.obj_type;
	ptr->obj_type_mask = self->obj.obj_type_mask;
	ptr->theobj = self;
#if 0
	if (debug_obj_table) 
		printf("%d obj registered: obj_type %d status %d ref_count %d\n",current_id,(int) self->obj.obj_type,(int)self->obj.status,self->obj.ref_count);
#endif
	current_id++;
	if (current_id == MAX_ID) {
		recycled = 1;
		current_id = RECYCLE_START_ID;
	}
	total_obj_count++;   /* the count of all objects created */
	return(ptr->id);
}

void _NclObjsSize
#if	NhlNeedProto
(FILE *fp)
#else
(fp)
	FILE *fp;
#endif
{
	fprintf(fp,"The size of objs list is %d elements of size %ld; %lld total objects have been created\n",current_size,sizeof(NclObjList),total_obj_count);
}
