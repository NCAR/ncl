
/*
 *      $Id: Machine.h,v 1.10 1994-05-28 00:12:53 ethan Exp $
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
typedef struct mach_stack {
        NclValue *themachine;
        char **thefiles;
        int     *thelines;
        unsigned int pcoffset;
        NclValue *pc;
        char **fn;
        int *lc;
        unsigned int current_machine_size;
        struct mach_stack *next;
} _NclMachineStack;

typedef struct _NclFrameList {
	int level;
	struct _NclFrame *fp;
	struct _NclStackEntry *sb;
	struct _NclFrameList * next;
}NclFrameList;

extern void _NclPush(
#ifdef NhlNeedProto
NclStackEntry /*data*/
#endif
);

extern NclStackEntry _NclPop(
#ifdef NhlNeedProto
void 
#endif
);

extern void _NclResetMachine(
#ifdef NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclInitMachine(
#ifdef NhlNeedProto
void
#endif
);

extern int _NclPutIntInstr(
#ifdef NhlNeedProto
int /* val */,
int	/* line */,
char * /* file*/
#endif
);
extern int _NclPutRealInstr(
#ifdef NhlNeedProto
float /* val */,
int	/* line */,
char * /* file*/
#endif
);

extern int _NclPutInstr(
#ifdef NhlNeedProto
NclValue /*val*/,
int	/* line */,
char * /* file*/
#endif
);

extern int _NclGetCurrentOffset(
#ifdef NhlNeedProto
void
#endif
);

extern int _NclPutInstrAt(
#ifdef NhlNeedProto
int 	/* offset */,
NclValue /*val*/,
int	/* line */,
char * /* file*/
#endif
);

extern void _NclPrintMachine(
#ifdef NhlNeedProto
int	/* from */,
int	/* to */,
FILE*	/* fp */
#endif
);

extern void _NclNewMachine(
#ifdef NhlNeedProto
void
#endif
);

extern void *_NclPopMachine(
#ifdef NhlNeedProto
void
#endif
);

extern void _NclPushMachine(
#ifdef NhlNeedProto
void * /*the_mach_rec */
#endif
);

extern int _NclGetCurrentOffset(
#ifdef NhlNeedProto 
void
#endif
);


extern NclValue *_NclGetCurrentMachine(
#ifdef NhlNeedProto
void
#endif
);

extern int *_NclGetCurrentLineRec(
#ifdef NhlNeedProto
void
#endif
);

extern char **_NclGetCurrentFileNameRec(
#ifdef NhlNeedProto
void
#endif
);

extern NclStackEntry _NclGetArg(
#ifdef NhlNeedProto
int  /*arg_num*/,
int  /*total_arg*/
#endif
);

extern void _NclPutArg(
#ifdef NhlNeedProto
NclStackEntry /* data */,
int  /*arg_num*/,
int  /*total_arg*/
#endif
);

extern NclStackEntry *_NclPeek(
#ifdef NhlNeedProto
int  /*offset*/
#endif
);


extern NhlErrorTypes _NclPutRec(
#ifdef NhlNeedProto
NclSymbol * /*n_items*/,
NclStackEntry * /*therec*/
#endif
);

extern NhlErrorTypes _NclPutLevel1Var(
#ifdef NhlNeedProto
int	/*offset*/,
NclStackEntry * /*therec*/
#endif
);
#define DONT_CARE 0
#define WRITE_IT  1
#define READ_IT	2
extern NclStackEntry *_NclRetrieveRec(
#ifdef NhlNeedProto
NclSymbol * /*n_items*/,
int 	/*access_type*/
#endif
);

extern NclStackEntry *_NclGetLevel1Var(
#ifdef NhlNeedProto
int	/*offset*/
#endif
);

extern void *_NclLeaveFrame(
#ifdef NhlNeedProto
int /* caller_level */
#endif
);

extern void _NclPushFrame(
#ifdef NhlNeedProto
struct _NclSymbol * /* thesym*/,
unsigned long /* offset */
#endif
);

extern void _NclPopFrame(
#ifdef NhlNeedProto
int /*popping_from*/
#endif
);

extern int _NclFinishFrame(
#ifdef NhlNeedProto
void
#endif
);

extern void _NclAddObjToParamList(
#ifdef NhlNeedProto
struct _NclObjRec * /* obj */,
int /* arg_num */
#endif
);

extern void _NclRemapParameters(
#ifdef NhlNeedProto
int /* nargs*/,
int /* cur_off */,
void * /*previous_fp*/,
int /* from */
#endif
);

extern void _NclRemapIntrParameters(
#ifdef NhlNeedProto
int /*nargs*/,
void * /*previous_fp*/,
int /*from*/
#endif
);

extern void _NclDumpStack(
#ifdef NhlNeedProto
FILE * /*fp*/,
int /*off*/
#endif
);

extern NhlErrorTypes _NclPlaceReturn(
#ifdef NhlNeedProto
struct _NclStackEntry data
#endif
);

extern void _NclCleanUpStack(
#ifdef NhlNeedProto
int /*n*/
#endif
);

extern void _NclAbortFrame(
#ifdef NhlNeedProto
void
#endif
);

extern void _NclClearToStackBase(
#ifdef NhlNeedProto
void
#endif
);
#ifdef __cplusplus
}
#endif

#endif
