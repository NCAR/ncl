
/*
 *      $Id: defs.h,v 1.27 2010-04-14 21:29:48 huangwei Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
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

#ifndef NIO_LIB_ONLY
#include "ncarg/hlu/NgSizeT.h"
#endif
#include    <stddef.h>

#define NCL_MAX_DIMENSIONS 32
#define NCL_MAX_FVARS 16
#define NCL_MAX_GVARS 2
#define NCL_MAX_STRING 256
#define NCL_MAX_COMPOUND_COMPONETS 256
#define NCL_MAX_ATTRIBUTES 32
#define NCL_MAX_SYMS_PER_STMNT 300
#define NCL_SRC_TREE_NODE_LIST_SIZE 1000
#define NCL_MISSING_VALUE_ATT "_FillValue"
#define NclANY NULL
#define NhlTNclData "nclData"

extern int use_new_hlfs;

typedef enum {	NORMAL = 0,
		VARSUBSEL = 1,
		COORD = 2,
		COORDSUBSEL = 3,
		FILEVAR = 4,
		FILEVARSUBSEL = 5,
		PARAM = 6,
		RETURNVAR = 7,
		HLUOBJ = 8,
		FILEGROUP = 9,
		RETURNGROUP = 10
} NclVarTypes;

typedef struct _NclDimRec {
        int   dim_quark;
        int   dim_num;
        ng_size_t   dim_size;
} NclDimRec;


typedef void (*NclPromptFunc)(
#if     NhlNeedProto
void * /*user_data*/,
int /*arg*/
#endif
);



/*
* Maximum number of error messages to be printed
* for a single statement includes blocks.
*/
#define NCL_MAX_ERROR 15
/*
* The following must be a PRIME number
*/
#define NCL_SYM_TAB_SIZE 211

#ifndef NclQuarkIsDef
typedef NrmQuark NclQuark;
#define NclQuarkIsDef
#endif

typedef size_t  NclValue;

typedef struct _NclGenericVal {
	size_t  kind;
	char *name;
} NclGenericVal;

extern void *NclMalloc(
#if	NhlNeedProto
ng_usize_t	/* size */
#endif
);

extern void *NclCalloc(
#if	NhlNeedProto
ng_usize_t	/* num */,
ng_usize_t	/* size */
#endif
);

extern void *NclRealloc(
#if	NhlNeedProto
void 	*	/* ptr */	,
ng_usize_t	/* size */
#endif
);

extern NhlErrorTypes NclFree(
#if	NhlNeedProto
void * /* size */
#endif
);

extern FILE* _NclGetOutputStream(
#if	NhlNeedProto
void
#endif
);
extern FILE* _NclGetInputStream(
#if	NhlNeedProto
void
#endif
);
extern FILE* _NclGetErrorStream(
#if	NhlNeedProto
void
#endif
);

void _NclPushNewInputFile(
#if NhlNeedProto
FILE * /*fp*/,
const char * /*name*/,
int /*cline_number*/
#endif
);

void _NclPushNewInputStr(
#if NhlNeedProto
char* /*tmp_input*/,
const char* /*name*/,
int /*size*/,
int /*cline_number*/
#endif
);

char *_NclPopInputStr(
#if NhlNeedProto
void
#endif
);

FILE *_NclPopInputFile(
#if NhlNeedProto
void
#endif
);


typedef int (*NclVaPrintFunc)(
#if	NhlNeedProto
FILE* /*fp*/,
const char* /*fmt*/,
va_list /*arg*/
#endif
);

extern int nclfprintf(
#if	NhlNeedVarArgProto
	FILE * /*fp*/,
	char * /*fmt*/,
	...
#endif
);

NclVaPrintFunc _NclSetPrintFunc(
#if NhlNeedProto
NclVaPrintFunc thepit
#endif
);

void _NclStartCmdLinePager(
#if NhlNeedProto
void
#endif
);
void _NclEndCmdLinePager(
#if NhlNeedProto
void
#endif
);
typedef struct _ext_stack {
        struct _NclSymbol*tmp_sym;
        struct _ext_stack * next;
} ExtStack;

#endif /*_NCdefs.h*/
#ifdef __cplusplus
}
#endif 
