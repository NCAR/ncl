#ifndef NclHLUObj_h
#define NclHLUObj_h

#include "NclData.h"

typedef struct _HLUObjCalRec {
        int parent_id;
        int child_id;
}HLUObjCalRec;

typedef struct _NclHLUObjRec *NclHLUObj;
typedef NhlErrorTypes (*NclAddHLUChild)(
#if  NhlNeedProto
NclHLUObj	/* self */,
int 	/*child_id*/
#endif
);

typedef NhlErrorTypes (*NclDelHLUChild)(
#if  NhlNeedProto
NclHLUObj	/* self */,
int /*child_id*/
#endif
);

typedef _NhlCB (*NclAddDestroyNotify)(
#if NhlNeedProto
NclHLUObj	/*self*/,		
_NhlCBFunc      /*cbfunc*/,
int		/*obj_id*/
#endif
);

typedef struct _NclHLUObjClassPart {
	NclDelHLUChild  del_hlu_child;
	NclAddHLUChild	add_hlu_child;
	NclAddHLUChild	add_exp_child;
} NclHLUObjClassPart;

struct _NclHLUChildList {
	NclQuark child_id;
	struct _NclHLUChildList * next;
};
struct _NclHLUExpChildList {
	NclQuark child_id;
	_NhlCB cb;
	HLUObjCalRec *crec;
	struct _NclHLUExpChildList * next;
};

typedef struct _NclHLUChildList NclHLUChildList;
typedef struct _NclHLUExpChildList NclHLUExpChildList;


typedef struct _NclHLUObjPart {
	int hlu_id;
	NclQuark hlu_name;
	int parent_hluobj_id;
	NhlClass class_ptr;
	NclHLUChildList *c_list;
	NclHLUExpChildList *exp_list;
	_NhlCBList	cblist;
	_NhlCB	apcb;
}NclHLUObjPart;
 
typedef struct _NclHLUObjClassRec{
	NclObjClassPart	obj_class;
	NclHLUObjClassPart hlu_class;
}NclHLUObjClassRec;

typedef struct _NclHLUObjRec {
	NclObjPart      obj;
	NclHLUObjPart	hlu;
}NclHLUObjRec;

typedef NclHLUObjClassRec *NclHLUObjClass;

extern NclObjClass nclHLUObjClass;

extern NclHLUObjClassRec nclHLUObjClassRec;

extern struct _NclHLUObjRec * _NclHLUObjCreate(
#if  NhlNeedProto
NclObj /*inst*/ , 
NclObjClass /*theclass */,
NclObjTypes /*obj_type */, 
unsigned int /*obj_type_mask*/, 
NclStatus /*status*/, 
int /*id*/,
int /* pid*/,
NhlClass /* class_ptr */
#endif
);


#endif /* NclHLUObj_h */
