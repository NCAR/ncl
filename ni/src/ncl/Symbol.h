
/*
 *      $Id: Symbol.h,v 1.10 1994-07-27 20:30:35 ethan Exp $
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

typedef struct _NclFileInfo {
	char filename[NCL_MAX_STRING];
	int level;
	unsigned int offset;
	struct _NclSymTableListNode *filescope;
} NclFileInfo;

typedef struct _NclFileVarInfo {
	char fvarname[NCL_MAX_STRING];
	int level;
	unsigned int offset;
	struct _NclSymbol *parent_file;
} NclFileVarInfo;


typedef struct _NclVarInfo {
	char	varname[NCL_MAX_STRING];
	int level;
	int datatype;
	unsigned int offset;
}NclVarInfo; 


#define ANYDIMSIZE = -1

typedef NhlErrorTypes (*NclBuiltInProcWrapper)(
#if	NhlNeedProto
	void
#endif
);
typedef NhlErrorTypes (*NclIntrinsicProcWrapper)(
#if	NhlNeedProto
	void	
#endif
);

typedef NhlErrorTypes (*NclBuiltInFuncWrapper)(
#if	NhlNeedProto
	void
#endif
);


typedef struct _NclGenProcFuncInfo {
	int nargs;
	struct _NclArgTemplate * theargs;
	struct _NclSymbol *thesym;
	struct _NclSymTableListNode* thescope;
} NclGenProcFuncInfo;

typedef struct _NclProcFuncInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	struct _NclSymTableListNode* thescope;
	void *mach_rec_ptr;
} NclProcFuncInfo;

typedef struct _NclBuiltInFuncInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	struct _NclSymTableListNode* thescope;
	NclBuiltInFuncWrapper thefunc;
} NclBuiltInFuncInfo;

typedef struct _NclBuiltInProcInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	struct _NclSymTableListNode* thescope;
	NclBuiltInProcWrapper theproc;
} NclBuiltInProcInfo;


typedef struct _NclArgTemplate {
	int n_dims;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	struct _NclSymbol *arg_data_type; /* use symbol table keyword entries */
	struct _NclSymbol *arg_sym;
	int is_dimsizes;
} NclArgTemplate;

typedef struct _NclVisBlkInfo {
	struct _NclSymbol*  obj_type;
} NclVisBlkInfo;

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
		struct _NclBuiltInFuncInfo	*bfunc;
		struct _NclBuiltInProcInfo 	*bproc;
		struct _NhlLayerClassRec 	*obj_class_ptr;
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

extern NclSymbol *_NclAddUniqueSym(
#ifdef NhlNeedProto
char * /*name */,
int /* type */
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

void _NclAddHLUObjs(
#ifdef NhlNeedProto
void
#endif
);

struct _NhlLayerClassRec* _NclGetClassPtr(
#ifdef  NhlNeedProto
int /* class_q */
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
