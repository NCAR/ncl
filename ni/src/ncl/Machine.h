
/*
 *      $Id: Machine.h,v 1.2 1993-10-06 22:54:25 ethan Exp $
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

extern void _NclPush(
#ifdef NhlNeedProto
void * /*data*/
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

extern int _NclGetCurrentOFfset(
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

extern int _NclPrintMachine(
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
