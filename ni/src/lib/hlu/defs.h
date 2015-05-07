/*
 *      $Id: defs.h,v 1.9 1995-02-17 10:23:49 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		defs.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Aug 28 08:32:34 MDT 1992
 *
 *	Description:	This file contains all the #define constants that
 *			may need to change in the future.  They are all
 *			placed here for easy of updating.
 */
#ifndef _DEFS_H_
#define _DEFS_H_

#include "ncarg/hlu/NgSizeT.h"
/*
 * These macros are defined to be NULL.  They are not used by the code, but
 * so that http daemons using the code2html script can produce anchors in
 * the code.
 */
#define NhlDOCTAG(tagname)
#define NhlDOCREF(url,anchortext)
#define NhlSRCREF(srcfileurl)

/* max characters in a "line" */
#define _NhlMAXLINELEN (256)

/* max characters in a filename */
#define _NhlMAXFNAMELEN	(1024)

/* max characters allowed for an LLU graphics output file name */
#define _NhlMAXLLUPATHLEN (1024)

/* path delimiter - "/" in unix "\" in dos and I'm sure there are others */
#define _NhlPATHDELIMITER	"/"

/* max depth of parent -> sub-parent -> child */
#define	_NhlMAXTREEDEPTH	(128)

/* max number of arguments that can be set in a single call*/
/* The actual number is one less than this because the last one */
/* gets filled with null to determine the length		*/
#define	_NhlMAXARGLIST	(512)

/* maximuim number of resources a single object can have */
#define	_NhlMAXRESLIST	(1024)

/* maximuim resource name length - each name not the full name.name. string */
#define _NhlMAXRESNAMLEN	(320)   /* 256 + 64 */

/* how many layer pointers to alloc at a time when more are needed */
#define _NhlLAYERLISTINC	(128)

/* usefull defines for specifying a layer has no parent or default app parent */
#define	NhlDEFAULT_APP	0


/* defines for the converter hash table */

/*
 * HASHSIZE must be a power of 2 - I have found the hash function works best
 * with a HASHMULT = to the power of 2 used for HASHSIZE
 * ie:        POW(2,HASHMULT) = HASHSIZE
 * right now: POW(2,8) = 256
 */
#define _NhlHASHSIZE	(256)
#define _NhlHASHMASK	(_NhlHASHSIZE - 1)
#define _NhlHASHMULT	(8)


#endif /*_DEFS_H_*/
