
/*
 *      $Id: ProcFuncs.h,v 1.2 1993-10-18 16:10:55 ethan Exp $
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

#ifndef _NCProcFuncs_h
#define _NCProcFuncs_h

#define ANYDIMSIZE = -1

typedef struct _NclProcFuncInfo {
	int nargs;
	struct _NclArgTemplate *theargs;
	struct _NclSymbol *thesym;
	void *mach_rec_ptr;
} NclProcFuncInfo;

typedef struct _NclArgTemplate {
	int dim_sizes[NCL_MAX_DIMENSIONS];
	struct _NclSymbol *arg_data_type; /* use symbol table keyword entries */
	int is_dimsizes;
} NclArgTemplate;

typedef void (*NclBuiltInProcWrapper)(
#if	NhlNeedProto
	void
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
