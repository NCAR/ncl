
/*
 *      $Id: defs.h,v 1.14 1994-07-14 20:47:44 ethan Exp $
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
#define NCL_MAX_FVARS 128
#define NCL_MAX_STRING 256
#define NCL_MAX_ATTRIBUTES 32
#define NCL_MAX_SYMS_PER_STMNT 300
#define NCL_SRC_TREE_NODE_LIST_SIZE 1000
#define NCL_MISSING_VALUE_ATT "_FillValue"

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

#endif /*_NCdefs.h*/
#ifdef __cplusplus
}
#endif 
