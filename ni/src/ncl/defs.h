
/*
 *      $Id: defs.h,v 1.13 1994-07-08 21:31:59 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		defs.h 
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jun 29 15:40:27 MDT 1993
 *
 *	Description:	Contains definitions for ncl
 */
#ifdef __cplusplus
extern "C" {
#endif
#ifndef _NCdefs_h
#define _NCdefs_h



#define NCL_MAX_DIMENSIONS 32
#define NCL_MAX_FVARS 128
#define NCL_MAX_STRING 256
#define NCL_MAX_ATTRIBUTES 32
#define NCL_MAX_SYMS_PER_STMNT 300
#define NCL_SRC_TREE_NODE_LIST_SIZE 1000
#define NCL_MISSING_VALUE_ATT "_FillValue"

/*
* Maximum number of error messages to be printed
* for a single statement includes blocks.
*/
#define NCL_MAX_ERROR 15
/*
* The following must be a PRIME number
*/
#define NCL_SYM_TAB_SIZE 211

extern void *NclMalloc(
#ifdef NhlFuncProto
unsigned  int	/* size */
#endif
);
extern void *NclCalloc(
#ifdef NhlFuncProto
unsigned int	/* num */,
unsigned int	/* size */
#endif
);

extern void *NclRealloc(
#ifdef NhlFuncProto
void 	*  /* ptr */	,
unsigned int	/* size */
#endif
);

extern NhlErrorTypes NclFree(
#ifdef NhlFuncProto
void * /* size */
#endif
);

extern void _NclFreeSubRec(
#ifdef NhlFuncProto
struct _NclSubRec	* /*sub_rec*/;
#endif
);

typedef NrmQuark NclQuark;

typedef struct _NclGenericVal {
	int kind;
	char *name;
} NclGenericVal;

typedef enum stack_value_types { 
	NclStk_NOVAL = 0, NclStk_OFFSET = 01, 
	NclStk_VAL = 02,NclStk_VAR = 04, NclStk_SUBREC = 010,
	NclStk_PARAMLIST = 020, NclStk_RANGEREC = 040,
	NclStk_VECREC = 0100, NclStk_FILE = 0200, NclStk_GRAPHIC = 0400,
	NclStk_RETURNVAL = 01000, NclStk_STATIC_LINK = 02000, 
	NclStk_DYNAMIC_LINK = 04000, NclStk_RET_OFFSET = 010000
	} NclStackValueTypes;
/*
static char *stack_element_names[] = { 
	"NclStk_NOVAL", 
	"NclStk_OFFSET", 
	"NclStk_VAL",
	"NclStk_VAR", 
	"NclStk_SUBREC" ,
	"NclStk_PARAMLIST" , 
	"NclStk_RANGEREC" ,
	"NclStk_VECREC" , 
	"NclStk_FILE" , 
	"NclStk_GRAPHIC" ,
	"NclStk_RETURNVAL",
	"NclStk_STATIC_LINK",
	"NclStk_DYNAMIC_LINK",
	"NclStk_RET_OFFSET"
	};
*/




typedef long NclValue;


typedef struct _NclStackEntry{
	NclStackValueTypes kind;
	union {
		unsigned long   offset;
/*
* All of the following must be pointers to pointers so changes
* made such as allocating a new record can propagte to copies
* an example is an array passed to a function with two parameters
* twice.
*/
		struct _NclRangeRec	*range_rec;
		struct _NclVecRec	*vec_rec;
		struct _NclSubRec	*sub_rec;
		struct _NclParamRecList *the_list;
		struct _NclVarRec	*data_var;
		struct _NclMultiDValDataRec 	*data_obj;
	}u;
}NclStackEntry;

typedef enum { 
	COORD_VECT,
	COORD_RANGE,
	INT_VECT,
	INT_RANGE
} NclSubTypes;

typedef struct _NclSubRec {
	NclSubTypes sub_type; 
	char *name;
	union {
		struct _NclRangeRec *range;
		struct _NclVecRec *vec;
	}u;
} NclSubRec;

typedef struct _NclRangeRec {
	struct _NclMultiDValDataRec *start;
	struct _NclMultiDValDataRec *finish;
	struct _NclMultiDValDataRec *stride;
}NclRangeRec;

typedef struct _NclVecRec {
	struct _NclMultiDValDataRec *vec;
}NclVecRec;

typedef struct _NclFrame{
	NclStackEntry	func_ret_value;
	NclStackEntry	static_link;
	NclStackEntry	dynamic_link;
	NclStackEntry	return_pcoffset;
	NclStackEntry	parameter_map;
}NclFrame;

typedef struct _NclVectorSelection{
        int n_ind;
        long *ind;
	long min;
	long max;
}NclVectorSelection;

typedef struct _NclSubscriptSelection{
        long start;
        long finish;
        long stride;
}NclSubscriptSelection;

typedef enum {	
		Ncl_SUBSCR, 
		Ncl_VECSUBSCR, 
		Ncl_SUB_ALL, 
		Ncl_SUB_VAL_DEF, 
		Ncl_SUB_DEF_VAL
} NclSelectionTypes;

typedef struct _NclSelection{
        NclSelectionTypes sel_type;
	long dim_num;
        union {
                struct _NclSubscriptSelection sub;
                struct _NclVectorSelection  vec;
        }u;
} NclSelection;

typedef struct _NclSelectionRecord {
	
	struct _NclSymbol *selected_from_sym;
	struct _NclVarRec *selected_from_var;
	int n_entries;
	NclSelection selection[NCL_MAX_DIMENSIONS];
} NclSelectionRecord;

typedef enum {NONE_P, VALUE_P, VAR_P} NclParamTypes;

typedef struct _NclParamRec {
	NclParamTypes p_type;
	int is_modified;
	struct _NclSymbol *var_sym;
	struct _NclVarRec *var_ptr;
	NclSelectionRecord *rec;
} NclParamRec;
typedef struct _NclParamRecList {
	int n_elements;
	struct _NclSymbol * fpsym;
	struct _NclParamRec *the_elements;
}NclParamRecList;

extern int _NclTranslate(
#ifdef NhlNeedProto
void* 	/*root*/,
FILE*   /*fp*/
#endif
);
extern void _NclTransTerminate(
#ifdef NhlNeedProto
void
#endif
);

#endif /*_NCdefs.h*/
#ifdef __cplusplus
}
#endif 
