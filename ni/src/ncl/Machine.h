
/*
 *      $Id: Machine.h,v 1.1 1993-09-24 23:40:37 ethan Exp $
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
* Program conter
*/
extern void *pc;

/*
* Stack pointer
*/
extern void *sp;

/*
* Frame pointer
*/
extern void *fp;

extern void _NclPush(
#ifdef NhlNeedProto
void * /*data*/
#endif
);

extern void * _NclPop(
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




