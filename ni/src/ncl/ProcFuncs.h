
/*
 *      $Id: ProcFuncs.h,v 1.5 1994-04-07 16:48:24 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 30 10:15:51 MDT 1993
 *
 *	Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif
#ifndef _NCProcFuncs_h
#define _NCProcFuncs_h

#define ANYDIMSIZE = -1

typedef struct _NclGenProcFuncInfo {
	int nargs;
	struct _NclArgTemplate * theargs;
	struct _NclSymbol *thesym;
} NclGenProcFuncInfo;

typedef struct _NclProcFuncInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	struct _NclSymTableListNode* thescope;
	void *mach_rec_ptr;
} NclProcFuncInfo;

typedef struct _NclArgTemplate {
	int n_dims;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	struct _NclSymbol *arg_data_type; /* use symbol table keyword entries */
	struct _NclSymbol *arg_sym;
	int is_dimsizes;
} NclArgTemplate;

typedef void (*NclBuiltInProcWrapper)(
#if	NhlNeedProto
	void
#endif
);
typedef void (*NclIntrinsicProcWrapper)(
#if	NhlNeedProto
	int
#endif
);

typedef struct _NclBuiltInProcInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	NclBuiltInProcWrapper theproc;
} NclBuiltInProcInfo;

typedef void (*NclBuiltInFuncWrapper)(
#if	NhlNeedProto
	NclStackEntry * /*return_data*/
#endif
);

typedef struct _NclBuiltInFuncInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	NclBuiltInFuncWrapper thefunc;
} NclBuiltInFuncInfo;


#endif	/* _NCProcFuncs_h */
#ifdef __cplusplus
}
#endif
