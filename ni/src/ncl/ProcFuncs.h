
/*
 *      $Id: ProcFuncs.h,v 1.7 1994-07-08 21:31:49 ethan Exp $
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



#endif	/* _NCProcFuncs_h */
#ifdef __cplusplus
}
#endif
