
/*
 *      $Id: defs.h,v 1.17 1994-11-07 03:02:33 ethan Exp $
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
#define NCL_MAX_FVARS 512
#define NCL_MAX_STRING 256
#define NCL_MAX_ATTRIBUTES 32
#define NCL_MAX_SYMS_PER_STMNT 300
#define NCL_SRC_TREE_NODE_LIST_SIZE 1000
#define NCL_MISSING_VALUE_ATT "_FillValue"

#define NhlTNclData "nclData"
typedef enum { NORMAL, VARSUBSEL , COORD, COORDSUBSEL, FILEVAR, FILEVARSUBSEL,PARAM,RETURNVAR,HLUOBJ } NclVarTypes;
typedef struct _NclDimRec {
        int   dim_quark;
        long   dim_num;
        int   dim_size;
} NclDimRec;


/*
* Maximum number of error messages to be printed
* for a single statement includes blocks.
*/
#define NCL_MAX_ERROR 15
/*
* The following must be a PRIME number
*/
#define NCL_SYM_TAB_SIZE 211

typedef NrmQuark NclQuark;
typedef long NclValue;

typedef struct _NclGenericVal {
	int kind;
	char *name;
} NclGenericVal;

extern void *NclMalloc(
#ifdef NhlNeedProto
unsigned  int	/* size */
#endif
);

extern void *NclCalloc(
#ifdef NhlNeedProto
unsigned int	/* num */,
unsigned int	/* size */
#endif
);

extern void *NclRealloc(
#ifdef NhlNeedProto
void 	*  /* ptr */	,
unsigned int	/* size */
#endif
);

extern NhlErrorTypes NclFree(
#ifdef NhlNeedProto
void * /* size */
#endif
);

extern FILE* _NclGetOutputStream(
#ifdef NhlNeedProto
void
#endif
);
extern FILE* _NclGetInputStream(
#ifdef NhlNeedProto
void
#endif
);
extern FILE* _NclGetErrorStream(
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
FILE* /*fp*/,
const char* /*fmt*/,
va_list /*arg*/
#endif
);

extern void nclfprintf(
#if	NhlNeedVarArgProto
	FILE * /*fp*/,
	char * /*fmt*/,
	...
#endif
);

void _NclSetPrintFunc(
#if NhlNeedProto
NclVaPrintFunc thepit
#endif
);


#endif /*_NCdefs.h*/
#ifdef __cplusplus
}
#endif 
