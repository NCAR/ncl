#ifndef NclHLUObj_h
#define NclHLUObj_h

#include "NclData.h"

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

typedef struct _NclHLUObjClassPart {
	NclDelHLUChild  del_hlu_child;
	NclAddHLUChild	add_hlu_child;
} NclHLUObjClassPart;

struct _NclHLUChildList {
	NclQuark child_id;
	struct _NclHLUChildList * next;
};

typedef struct _NclHLUChildList NclHLUChildList;


typedef struct _NclHLUObjPart {
	int hlu_id;
	int parent_hluobj_id;
	NhlLayerClass class_ptr;
	NclHLUChildList *c_list;
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
NhlLayerClass /* class_ptr */
#endif
);


#endif /* NclHLUObj_h */
