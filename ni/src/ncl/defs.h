
/*
 *      $Id: defs.h,v 1.5 1993-12-21 19:18:22 ethan Exp $
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
#define NCL_MAX_STRING 256
#define NCL_MAX_ATTRIBUTES 32
#define NCL_MAX_SYMS_PER_STMNT 300
#define NCL_SRC_TREE_NODE_LIST_SIZE 1000

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

typedef struct _NclGenericVal {
	int kind;
	char *name;
} NclGenericVal;

typedef enum stack_value_types { 
	NclStk_NOVAL = 01, NclStk_OFFSET = 02, 
	NclStk_VAL = 04,NclStk_VAR = 010
	} NclStackValueTypes;




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
		struct _NclParamRecList *the_list;
		struct _NclVarRec	*data_var;
		struct _NclDataRec 	*data_obj;
	}u;
}NclStackEntry;

typedef struct _NclFrame{
	NclStackEntry	func_ret_value;
	NclStackEntry	static_link;
	NclStackEntry	dynamic_link;
	NclStackEntry	return_pcoffset;
	NclStackEntry	parameter_map;
}NclFrame;

typedef struct _NclVectorSelection{
        int n_ind;
        int *ind;
	int min;
	int max;
}NclVectorSelection;

typedef struct _NclSubscriptSelection{
        int start;
        int finish;
        int stride;
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
	int dim_num;
        union {
                struct _NclSubscriptSelection sub;
                struct _NclVectorSelection  vec;
        }u;
} NclSelection;

typedef struct _NclSelectionRecord {
	
	struct _NclSymbol *selected_from_sym;
	struct _NclVarRec *selected_from_var;
	NclSelection selection[NCL_MAX_DIMENSIONS];
} NclSelectionRecord;

typedef struct _NclParamRecList {
	struct _NclSymbol *var;
	NclSelectionRecord *rec;
	struct _NclParamRecList *next;
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
