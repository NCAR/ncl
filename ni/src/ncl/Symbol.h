
/*
 *      $Id: Symbol.h,v 1.6 1994-03-03 21:54:38 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Symbol.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jun 28 10:12:49 MDT 1993
 *
 *	Description:	Type definitions for symbol table entries.
 */
#ifdef __cplusplus
extern "C" {
#endif

#ifndef _NCSymbol_h
#define _NCSymbol_h

#include <Files.h>
#include <Variable.h>
#include <Literal.h>
#include <ProcFuncs.h>
#include <Graphics.h>


typedef struct _NclSymbol {
	int type;
	int ind;
	int level;
	char name[NCL_MAX_STRING];
	unsigned int offset;
	union {
		struct _NclVarInfo		*var;
		struct _NclFileInfo		*file;
		struct _NclFileVarInfo		*fvar;
		struct _NclVisBlkInfo		*visblk;
		struct _NclProcFuncInfo		*procfunc;
		struct _NclBuiltInProcInfo	*bproc;
		struct _NclBuiltInFuncInfo	*bfunc;
/*
		struct _NclLiteral		*lit;
*/
	} u;
	struct _NclSymbol *symnext;
	struct _NclSymbol *sympre;
} NclSymbol;

typedef struct _NclSymTableElem {
	int nelem;
	struct _NclSymbol *thelist;
} NclSymTableElem;

typedef struct _NclSymTableListNode {
	int level;
	int cur_offset;
	struct _NclSymTableElem *this_scope;
	struct _NclSymTableListNode *previous;
}NclSymTableListNode;


extern NclSymbol *_NclLookUpInScope(
#ifdef NhlNeedProto
NclSymTableListNode	* /*thetable*/,
char			* /*name*/
#endif
);

extern NclSymbol *_NclAddInScope(
#ifdef NhlNeedProto
	NclSymTableListNode     * /*thetable*/,
	char			* /* name */,
	int			  /* type */
#endif
);

extern void _NclDeleteSymInScope(
#ifdef NhlNeedProto
NclSymTableListNode * /*thetable*/,
NclSymbol * /*sym*/
#endif
);

extern NclSymbol *_NclLookUp(
#ifdef NhlNeedProto
char  * /* name */
#endif
);

extern NclSymbol *_NclAddSym(
#ifdef NhlNeedProto
char * /* name */,
int  /* type */
#endif
);

extern void _NclDeleteSym(
#ifdef NhlNeedProto
NclSymbol * /*sym*/
#endif
);

extern int _NclInitSymbol (
#ifdef NhlNeedProto 
void 
#endif
);

extern int _NclNewScope(
#ifdef NhlNeedProto
void
#endif
);

extern NclSymTableListNode *_NclPopScope(
#ifdef NhlNeedProto
void
#endif
);

extern void _NclPrintSymbol(
#ifdef NhlNeedProto
NclSymbol * /*sym*/,
FILE  * /*fp*/
#endif
);

extern NclSymbol *_NclChangeSymbolType(
#ifdef NhlNeedProto
NclSymbol * /*thesym*/,
int	/* type */
#endif
);

extern int _NclGetCurrentScopeLevel(
#ifdef NhlNeedProto
void
#endif
);

void _NclResetNewSymStack(
#ifdef NhlNeedProto
void
#endif
);

void _NclDeleteNewSymStack(
#ifdef NhlNeedProto
void
#endif
);

void _NclAddIntrinsics(
#ifdef NhlNeedProto
void
#endif
);

void _NclAddBuiltIns(
#ifdef NhlNeedProto
void
#endif
);

void _NclRegisterFunc(
#ifdef NhlNeedProto
NclBuiltInFuncWrapper /* thefunctptr */,
NclArgTemplate *      /* args */,
char *                /* fname */,
int                   /* nargs */,
int 		      /* ftype */
#endif
);

void _NclRegisterProc(
#ifdef NhlNeedProto
NclBuiltInProcWrapper /* theproctptr */,
NclArgTemplate *      /* args */,
char *                /* fname */,
int                   /* nargs */,
int                   /* ftype */
#endif
);

#endif /*_NCSymbol_h*/
#ifdef __cplusplus
}
#endif
