
/*
 *      $Id: Procedures.h,v 1.1 1993-09-24 23:40:56 ethan Exp $
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

#ifndef _NCProcedures_h
#define _NCProcedures_h

typedef struct _NclProcInfo {
	int nargs;
	int *arg_types;
	struct _NclSymbol *syms;
} NclProcInfo;

typedef struct _NclProcRec {
	NclValue addr;
	struct _NclSymbol *entry;
} NclProcRec;

#endif	/* _NCProcedures_h */
