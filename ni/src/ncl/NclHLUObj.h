#ifndef NclHLUObj_h
#define NclHLUObj_h

#include "NclData.h"

typedef struct _NclHLUObjClassPart {
	char *foo;
} NclHLUObjClassPart;

typedef struct _NclHLUObjPart {
	int hlu_id;
}NclHLUObjPart;
 
typedef struct _NclHLUObjClassRec{
	NclObjClassPart	obj_class;
	NclHLUObjClassPart hlu_class;
}NclHLUObjClassRec;

typedef struct _NclHLUObjRec {
	NclObjPart      obj;
	NclHLUObjPart	hlu;
}NclHLUObjRec;

typedef NclHLUObjRec *NclHLUObj;
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
int /*id*/
#endif
);


#endif /* NclHLUObj_h */
