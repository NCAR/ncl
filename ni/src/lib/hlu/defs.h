/*
 *      $Id: defs.h,v 1.3 1993-12-14 21:44:25 boote Exp $
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

/*
 * These macros are defined to be NULL.  They are not used by the code, but
 * so that http daemons using the code2html script can produce anchors in
 * the code.
 */
#define NhlDOCTAG(tagname)
#define NhlDOCREF(url,anchortext)
#define NhlSRCREF(srcfileurl)

/* environment variable names for resource file locations */
/* The sysresfile is kept in $NCARG_LIB/ncarg/hluresfile for now */
#ifndef HLUSYSRESENVNAME
#define SYSRESENVNAME	"NCARG_SYSRESFILE"
#else
#define SYSRESENVNAME HLUSYSRESENVNAME
#endif
#ifndef DEFSYSRESFNAME
#define DEFSYSRESFNAME	"hluresfile"
#endif

#ifndef HLUUSRRESENVNAME
#define USRRESENVNAME	"NCARG_USRRESFILE"
#else
#define USRRESENVNAME HLUUSRRESENVNAME
#endif
#ifndef DEFUSRRESFNAME
#define DEFUSRRESFNAME	"~/.hluresfile"
#endif

/* max characters in a filename */
#define MAXFNAMELEN	(256)

/* path delimiter - "/" in unix "\" in dos and I'm sure there are others */
#define PATHDELIMITER	"/"

/* maximuim depth of parent -> sub-parent -> child */
#define	MAXTREEDEPTH	(128)

/* maximuin number of arguments that can be set in a single call*/
/* The actual number is one less than this because the last one */
/* gets filled with null to determine the length		*/
#define	MAXARGLIST	(128)

/* maximuim number of resources a single object can have */
#define	MAXRESLIST	(1024)

/* maximuim resource name length - each name not the full name.name. string */
#define MAXRESNAMLEN	(128)

/* how many layer pointers to alloc at a time when more are needed */
#define LAYERLISTINC	(128)

/* usefull defines for specifying a layer has no parent */
#define NULL_LAYER	(-100)
#define NULL_PARENT	NULL_LAYER
#define NOPARENT	NULL_LAYER


/* defines for the converter hash table */

/*
 * HASHSIZE must be a power of 2 - I have found the hash function works best
 * with a HASHMULT = to the power of 2 used for HASHSIZE
 * ie:        POW(2,HASHMULT) = HASHSIZE
 * right now: POW(2,8) = 256
 */
#define HASHSIZE	(256)
#define HASHMASK	(HASHSIZE - 1)
#define HASHMULT	(8)


#endif /*_DEFS_H_*/
