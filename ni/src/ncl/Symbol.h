

/*
 *      $Id: Symbol.h,v 1.33 2009-09-03 06:41:18 dbrown Exp $
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

#if defined(HPUX)
#include <dl.h>
#endif

typedef struct _NclFileInfo {
	char filename[NCL_MAX_STRING];
	int level;
	unsigned int offset;
	struct _NclScopeRec* filescope;
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

typedef struct _NclSharedLibraryInfo {
	struct _NclScopeRec* scope;
#if defined(HPUX)
	shl_t so_handle;
#else
	void *so_handle;
#endif
}NclSharedLibraryInfo;


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
	struct _NclScopeRec* thescope;
} NclGenProcFuncInfo;

typedef struct _NclProcFuncInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	struct _NclScopeRec* thescope;
	void *mach_rec_ptr;
} NclProcFuncInfo;

typedef struct _NclBuiltInFuncInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	struct _NclScopeRec* thescope;
	NclBuiltInFuncWrapper thefunc;
} NclBuiltInFuncInfo;

typedef struct _NclBuiltInProcInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	struct _NclScopeRec* thescope;
	NclBuiltInProcWrapper theproc;
} NclBuiltInProcInfo;


typedef struct _NclArgTemplate {
	int n_dims;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
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
		struct _NclSharedLibraryInfo		*package;
		struct _NhlClassRec 	*obj_class_ptr;
	} u;
	struct _NclSymbol *symnext;
	struct _NclSymbol *sympre;
} NclSymbol;

typedef struct _NclSymTableElem {
	int nelem;
	struct _NclSymbol *thelist;
} NclSymTableElem;

typedef struct _NclScopeRec {
	int level;
	int cur_offset;
	struct _NclSymTableElem *this_scope;
} NclScopeRec; 
typedef struct _NclSymTableListNode {
	struct _NclScopeRec *sr;
	struct _NclSymTableListNode *previous;
}NclSymTableListNode;


extern NclSymbol *_NclLookUpInScope(
#if	NhlNeedProto
NclScopeRec* /*thetable*/,
char			* /*name*/
#endif
);

extern NclSymbol *_NclAddInScope(
#if	NhlNeedProto
	NclScopeRec */*thetable*/,
	char			* /* name */,
	int			  /* type */
#endif
);

extern void _NclDeleteSymInScope(
#if	NhlNeedProto
NclScopeRec* /*thetable*/,
NclSymbol * /*sym*/
#endif
);

extern NclSymbol *_NclLookUp(
#if	NhlNeedProto
char  * /* name */
#endif
);

extern NclSymbol *_NclAddSym(
#if	NhlNeedProto
char * /* name */,
int  /* type */
#endif
);

extern NclSymbol *_NclAddUniqueSym(
#if	NhlNeedProto
char * /*name */,
int /* type */
#endif
);

extern void _NclDeleteSym(
#if	NhlNeedProto
NclSymbol * /*sym*/
#endif
);

extern int _NclInitSymbol (
#if	NhlNeedProto
void 
#endif
);

extern int _NclFinalizeSymbol();

extern int _NclNewScope(
#if	NhlNeedProto
void
#endif
);

extern void _NclPushScope(
#if	NhlNeedProto
NclScopeRec * /* new_scope */
#endif
);
extern NclScopeRec *_NclPopScope(
#if	NhlNeedProto
void
#endif
);

extern void _NclPrintSymbol(
#if	NhlNeedProto
NclSymbol * /*sym*/,
FILE  * /*fp*/
#endif
);

extern NclSymbol *_NclChangeSymbolType(
#if	NhlNeedProto
NclSymbol * /*thesym*/,
int	/* type */
#endif
);

extern int _NclGetCurrentScopeLevel(
#if	NhlNeedProto
void
#endif
);

extern void _NclUndefSymbolsInScope(
#if	NhlNeedProto
NclProcFuncInfo *procfunc_info
#endif
);

void _NclResetNewSymStack(
#if	NhlNeedProto
void
#endif
);

void _NclDeleteNewSymStack(
#if	NhlNeedProto
void
#endif
);

void _NclAddIntrinsics(
#if	NhlNeedProto
void
#endif
);

void _NclFreeProcFuncInfo(
#if	NhlNeedProto
NclSymbol * /*sym*/
#endif
);

void _NclAddBuiltIns(
#if	NhlNeedProto
void
#endif
);
struct _NclExtValueRec *_NclGetHLUObjId(
#if NhlNeedProto
char * /* var_name */
#endif
);
void _NclAddHLUObjs(
#if	NhlNeedProto
void
#endif
);

struct _NhlClassRec* _NclGetClassPtr(
#if	NhlNeedProto
int /* class_q */
#endif
);


void _NclRegisterFunc(
#if	NhlNeedProto
NclBuiltInFuncWrapper /* thefunctptr */,
NclArgTemplate *      /* args */,
char *                /* fname */,
int                   /* nargs */,
int 		      /* ftype */
#endif
);

void _NclRegisterProc(
#if	NhlNeedProto
NclBuiltInProcWrapper /* theproctptr */,
NclArgTemplate *      /* args */,
char *                /* fname */,
int                   /* nargs */,
int                   /* ftype */
#endif
);


extern struct _NclApiDataList *_NclGetDefinedVarInfo(
#if NhlNeedProto
void
#endif
);

extern struct _NclApiDataList *_NclGetDefinedProcFuncInfo(
#if NhlNeedProto
void
#endif
);

extern struct _NclApiDataList *_NclGetDefinedFileInfo(
#if NhlNeedProto
void
#endif
);
extern struct _NclApiDataList *_NclGetFileVarInfoList(
#if NhlNeedProto
NclQuark /*file_var*/
#endif
);

extern struct _NclApiDataList *_NclGetDefinedHLUInfo(
#if NhlNeedProto
void 
#endif
);

extern void _NclFreeApiDataList(
#if NhlNeedProto
void* /*list*/
#endif
);

extern struct _NclExtValueRec *_NclReadVarValue(
#if NhlNeedProto
NclSymbol * /*the_sym*/,
long * /*start*/,
long * /*finish*/,
long * /*stride*/
#endif
);
extern struct _NclExtValueRec *_NclGetVarValue(
#if NhlNeedProto
NclSymbol * /*the_sym*/,
int /*copy_data*/
#endif
);

extern NclQuark *_NclGetFileSymNames(
#if     NhlNeedProto
int    *num_names
#endif
);

extern NclQuark *_NclGetProcFuncSymNames(
#if     NhlNeedProto
int    *num_names
#endif
);
extern NhlClass *_NclGetHLUClassPtrs(
#if     NhlNeedProto
int    *num_names
#endif
);

extern NclQuark *_NclGetVarSymNames(
#if     NhlNeedProto
int    *num_names
#endif
);

extern NclQuark *_NclGetHLUVarSymNames(
#if     NhlNeedProto
int    *num_names
#endif
);

extern struct _NclApiDataList *_NclGetFileInfo(
#if	NhlNeedProto
NclQuark /*file_sym_name*/
#endif
);

extern struct _NclExtValueRec * _NclReadFileAtt(
#if     NhlNeedProto
NclQuark /*file_sym_name*/,
NclQuark /*attname*/
#endif
);

extern NclQuark *_NclGetFileVarNames(
#if     NhlNeedProto
NclQuark /* file_sym_name*/,
int * /*num_names*/
#endif
);

extern struct _NclApiDataList *_NclGetFileVarInfo(
#if     NhlNeedProto
NclQuark /*file_sym_name*/,
NclQuark /*file_var_name*/
#endif
);

extern struct _NclApiDataList *_NclGetFileVarCoordInfo(
#if     NhlNeedProto
NclQuark /*file_sym_name*/,
NclQuark /*file_var_name*/,
NclQuark /*coord_name*/
#endif
);

extern struct _NclExtValueRec *_NclReadFileVar(
#if     NhlNeedProto
NclQuark /*file_sym_name*/,
NclQuark /*file_var_name*/,
long    * /*start*/,
long    * /*finish*/,
long    * /*stride*/
#endif
);

extern struct _NclExtValueRec *_NclReadFileVarAtt(
#if     NhlNeedProto
NclQuark /*file_sym_name*/,
NclQuark /*file_var_name*/,
NclQuark /*attname*/
#endif
);

extern NclQuark *_NclGetFileCompoundVarComponentInfo(NclQuark file_sym_name,
							NclQuark file_var_name,
							ng_size_t* num_components);

extern struct _NclExtValueRec *_NclReadFileVarCoord(
#if     NhlNeedProto
NclQuark /*file_sym_name*/,
NclQuark /*file_var_name*/,
NclQuark /*coordname*/,
long    * /*start*/,
long    * /*finish*/,
long    * /*stride*/
#endif
);

extern struct _NclApiDataList *_NclGetVarInfo(
#if	NhlNeedProto
NclQuark /*var_sym_name*/
#endif
);

extern struct _NclApiDataList *_NclGetVarCoordInfo(
#if	NhlNeedProto
NclQuark /*var_sym_name*/,
NclQuark /*coordname*/
#endif
);


extern struct _NclExtValueRec *_NclReadVarAtt(
#if     NhlNeedProto
NclQuark /*var_sym_name*/,
NclQuark /*attname*/
#endif
);

extern struct _NclExtValueRec *_NclReadVarCoord(
#if     NhlNeedProto
NclQuark /*var_sym_name*/,
NclQuark /*coordname*/,
long    * /*start*/,
long    * /*finish*/,
long    * /*stride*/
#endif
);

extern struct _NclExtValueRec *_NclReadVarCoordAtt(
#if     NhlNeedProto
NclQuark /*var_sym_name*/,
NclQuark /*coordname*/,
NclQuark /*attname*/
#endif
);

extern NhlErrorTypes _NclWalkSymTable(
#if	NhlNeedProto
void
#endif
);

extern void _NclFileCleanUp (
#if NhlNeedProto
void
#endif
);

extern void _NclExit(
#if NhlNeedProto
int status
#endif
);

extern char ncl_getc(
#if	NhlNeedProto
FILE *fp
#endif
);

#ifdef YY_CURRENT_BUFFER_LVALUE
#define NCL_YY_CURRENT_BUFFER YY_CURRENT_BUFFER_LVALUE
#else
#define NCL_YY_CURRENT_BUFFER yy_current_buffer
#endif

#ifndef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
        if ( NCL_YY_CURRENT_BUFFER->yy_is_interactive ) \
                { \
                int c = '*', n; \
                for ( n = 0; n < max_size && \
                             (c = ncl_getc( yyin )) != EOF && c != '\n'; ++n ) \
                        buf[n] = (char) c; \
                if ( c == '\n' ) \
                        buf[n++] = (char) c; \
                if ( c == EOF && ferror( yyin ) ) \
                        YY_FATAL_ERROR( "input in flex scanner failed" ); \
                result = n; \
                } \
        else if ( ((result = fread( buf, 1, max_size, yyin )) == 0) \
                  && ferror( yyin ) ) \
                YY_FATAL_ERROR( "input in flex scanner failed" );\
	else if (feof(yyin) && result > 0 && result < max_size - 1 && buf[result-1] != '\n') \
		buf[result++] = '\n';
		
#endif

#endif /*_NCSymbol_h*/
#ifdef __cplusplus
}
#endif

