
/*
 *      $Id: Machine.h,v 1.19 1996-12-20 00:42:09 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Machine.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 7 09:04:42 MDT 1993
 *
 *	Description:	contains necessary extern definitions so other
 *			parts of NCL can access pc,sp and fp.
 */
#ifndef _NCMachine_h
#define _NCMachine_h
#ifdef __cplusplus
extern "C" {
#endif
/*
* This is dynamically allocated so that ReAlloc can be used to grow the
* machine incase of overflow. The reason for making the machine a stack
* is so that functions and procedures won't fragment the instruction sequences.
* procedures and functions have to stick arround until the end of the program
* or until they are removed. Where regular instructions generated from a s
* statment  can be removed once they are executed.
*/

typedef struct mach_rec {
        NclValue *themachine;
        char **thefiles;
        int     *thelines;
        unsigned int pcoffset;
        NclValue *pc;
        char **fn;
        int *lc;
        unsigned int current_machine_size;
}_NclMachineRec;

typedef struct mach_stack {
	struct mach_rec *the_rec;
        struct mach_stack *next;
} _NclMachineStack;

typedef struct _NclFrameList {
	int level;
	unsigned int fp;
	unsigned int sb;
	struct _NclFrameList * next;
}NclFrameList;

typedef enum {
			Ncl_ERRORS, 
			Ncl_STOPS, 
			Ncl_BREAKS, 
			Ncl_CONTINUES }NclExecuteReturnStatus ;

typedef enum stack_value_types { 
	NclStk_NOVAL = 0, NclStk_OFFSET = 01, 
	NclStk_VAL = 02,NclStk_VAR = 04, NclStk_SUBREC = 010,
	NclStk_PARAMLIST = 020, NclStk_RANGEREC = 040,
	NclStk_VECREC = 0100, NclStk_FILE = 0200, NclStk_GRAPHIC = 0400,
	NclStk_RETURNVAL = 01000, NclStk_STATIC_LINK = 02000, 
	NclStk_DYNAMIC_LINK = 04000, NclStk_RET_OFFSET = 010000
	} NclStackValueTypes;

typedef enum {NONE_P, VALUE_P, VAR_P} NclParamTypes;

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
		struct _NclRangeRec {
        		struct _NclMultiDValDataRec *start;
        		struct _NclMultiDValDataRec *finish;
        		struct _NclMultiDValDataRec *stride;
		} range_rec;
		struct _NclVecRec {
			struct _NclMultiDValDataRec *vec;
		} vec_rec;
		struct _NclSubRec {
			NclSubTypes sub_type;
			char *name;
			int tolerence;  /* applies only to coordinate variables */
			union {
				struct _NclRangeRec range;
				struct _NclVecRec vec;
			}u;
		} sub_rec;
		struct _NclParamRecList *the_list;
		struct _NclVarRec	*data_var;
		struct _NclMultiDValDataRec 	*data_obj;
	}u;
}NclStackEntry;


typedef struct _NclFrame{
        NclStackEntry   func_ret_value;
        NclStackEntry   static_link;
        NclStackEntry   dynamic_link;
        NclStackEntry   return_pcoffset;
        NclStackEntry   parameter_map;
}NclFrame;
typedef struct _NclSubRec NclSubRec;

typedef struct _NclRangeRec NclRangeRec;

typedef struct _NclVecRec NclVecRec;

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

extern void _NclFreeSubRec(
#ifdef NhlFuncProto
struct _NclSubRec       * /*sub_rec*/;
#endif
);

NclExecuteReturnStatus _NclExecute(
#if NhlNeedProto
	unsigned long start_offset
#endif
);
extern NhlErrorTypes _NclPush(
#if	NhlNeedProto
NclStackEntry /*data*/
#endif
);

extern NclStackEntry _NclPop(
#if	NhlNeedProto
void 
#endif
);

extern void _NclResetMachine(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclInitMachine(
#if	NhlNeedProto
void
#endif
);
extern int _NclPutIntInstr(
#if	NhlNeedProto
int /* val */,
int	/* line */,
char * /* file*/
#endif
);

extern int _NclPutInstr(
#if	NhlNeedProto
NclValue /*val*/,
int	/* line */,
char * /* file*/
#endif
);

extern int _NclGetCurrentOffset(
#if	NhlNeedProto
void
#endif
);

extern int _NclPutInstrAt(
#if	NhlNeedProto
int 	/* offset */,
NclValue /*val*/,
int	/* line */,
char * /* file*/
#endif
);

extern void _NclPrintMachine(
#if	NhlNeedProto
int	/* from */,
int	/* to */,
FILE*	/* fp */
#endif
);

extern void _NclNewMachine(
#if	NhlNeedProto
void
#endif
);

extern void *_NclPopMachine(
#if	NhlNeedProto
void
#endif
);

extern void _NclPushMachine(
#if	NhlNeedProto
void * /*the_mach_rec */
#endif
);


extern NclValue *_NclGetCurrentMachine(
#if	NhlNeedProto
void
#endif
);

extern int *_NclGetCurrentLineRec(
#if	NhlNeedProto
void
#endif
);

extern char **_NclGetCurrentFileNameRec(
#if	NhlNeedProto
void
#endif
);

extern NclStackEntry _NclGetArg(
#if	NhlNeedProto
int  /*arg_num*/,
int  /*total_arg*/,
int  /*access_type*/
#endif
);

extern NhlErrorTypes _NclPutArg(
#if	NhlNeedProto
NclStackEntry /* data */,
int  /*arg_num*/,
int  /*total_arg*/
#endif
);

extern NclStackEntry *_NclPeek(
#if	NhlNeedProto
int  /*offset*/
#endif
);


extern NhlErrorTypes _NclPutRec(
#if	NhlNeedProto
struct _NclSymbol * /*n_items*/,
NclStackEntry * /*therec*/
#endif
);

extern NhlErrorTypes _NclPutLevel1Var(
#if	NhlNeedProto
int	/*offset*/,
NclStackEntry * /*therec*/
#endif
);
#define DONT_CARE 0
#define WRITE_IT  1
#define READ_IT	2
extern NclStackEntry *_NclRetrieveRec(
#if	NhlNeedProto
NclSymbol * /*n_items*/,
int 	/*access_type*/
#endif
);

extern NclStackEntry *_NclGetLevel1Var(
#if	NhlNeedProto
int	/*offset*/
#endif
);

extern void *_NclLeaveFrame(
#if	NhlNeedProto
int /* caller_level */
#endif
);

extern NhlErrorTypes _NclPushFrame(
#if	NhlNeedProto
struct _NclSymbol * /* thesym*/,
unsigned long /* offset */
#endif
);

extern void _NclPopFrame(
#if	NhlNeedProto
int /*popping_from*/
#endif
);

extern int _NclFinishFrame(
#if	NhlNeedProto
void
#endif
);

extern void _NclAddObjToParamList(
#if	NhlNeedProto
struct _NclObjRec * /* obj */,
int /* arg_num */
#endif
);

extern void _NclRemapParameters(
#if	NhlNeedProto
int /* nargs*/,
int /* cur_off */,
void * /*previous_fp*/,
int /* from */
#endif
);

extern void _NclRemapIntrParameters(
#if	NhlNeedProto
int /*nargs*/,
void * /*previous_fp*/,
int /*from*/
#endif
);

extern void _NclDumpStack(
#if	NhlNeedProto
FILE * /*fp*/,
int /*off*/
#endif
);

extern NhlErrorTypes _NclPlaceReturn(
#if	NhlNeedProto
struct _NclStackEntry data
#endif
);

extern void _NclCleanUpStack(
#if	NhlNeedProto
int /*n*/
#endif
);

extern void _NclAbortFrame(
#if	NhlNeedProto
void
#endif
);

extern void _NclClearToStackBase(
#if	NhlNeedProto
void
#endif
);

extern int _NclPutRealInstr(
#if	NhlNeedProto
float	/*val*/,
int	/* line */,
char * /*file*/
#endif
);
#ifdef __cplusplus
}
#endif

#endif
