
/*
 *      $Id: Files.h,v 1.2 1993-12-21 19:17:33 ethan Exp $
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
 *	Date:		Wed Jun 30 09:15:45 MDT 1993
 *
 *	Description:	
 */
#ifndef _NCFiles_h
#define _NCFiles_h

#ifdef __cplusplus 
extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif

#endif /*_NCFiles_h*/
