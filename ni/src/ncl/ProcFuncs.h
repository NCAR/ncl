
/*
 *      $Id: ProcFuncs.h,v 1.1 1993-10-06 22:54:34 ethan Exp $
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

#endif	/* _NCProcFuncs_h */
