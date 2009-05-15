/*
 *      $Id: nioNresDB.h,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		NresDB.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Dec 14 14:39:40 MST 1992
 *
 *	Description:	This file was taken from the mit X distribution
 *			and has been modified to support the hlu's. The
 *			above copyright is for the changes to the code
 *			while the copyright listed below is for the
 *			original code that still exists in this file.
 */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifndef _NRESOURCE_H_
#define _NRESOURCE_H_


/****************************************************************
 ****************************************************************
 ***                                                          ***
 ***                                                          ***
 ***          N Resource Manager Intrinsics                   ***
 ***                                                          ***
 ***                                                          ***
 ****************************************************************
 ****************************************************************/

/****************************************************************
 *								*
 * File management macros - may need to be redefined for OS	*
 *								*
 ****************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define	OpenFile(name)		open((name), O_RDONLY)
#define	CloseFile(fd)		close((fd))
#define	ReadFile(fd,buf,size)	read((fd), (buf), (size))
#define	GetSizeOfFile(name,size)			\
{							\
	struct stat status_buffer;			\
	if ( (stat((name), &status_buffer)) == -1 )	\
		size = -1;				\
	else						\
		size = status_buffer.st_size;		\
}

/****************************************************************
 *
 * Memory Management
 *
 ****************************************************************/

extern char *Npermalloc(
#if NhlNeedProto
    unsigned int	/* size */
#endif
);

/****************************************************************
 *
 * Quark Management
 *
 ****************************************************************/

typedef long	NrmQuark, *NrmQuarkList;
#define NrmNULLQUARK ((NrmQuark) 0)

typedef char *NrmString;
#define NrmNULLSTRING ((NrmString) 0)

/* find quark for string, create new quark if none already exists */
extern NrmQuark NrmStringToQuark(
#if NhlNeedProto
    Const char* 	/* string */
#endif
);

extern NrmQuark NrmPermStringToQuark(
#if NhlNeedProto
    Const char* 	/* string */
#endif
);

/* find string for quark */
extern NrmString NrmQuarkToString(
#if NhlNeedProto
    NrmQuark 		/* quark */
#endif
);

extern NrmQuark NrmUniqueQuark(
#if NhlNeedProto
    void
#endif
);

#define NrmStringsEqual(a1, a2) (strcmp(a1, a2) == 0)


/****************************************************************
 *
 * Conversion of Strings to Lists
 *
 ****************************************************************/

typedef enum {NrmBindTightly, NrmBindLoosely} NrmBinding, *NrmBindingList;

extern void NrmStringToQuarkList(
#if NhlNeedProto
    Const char*	/* string */,
    NrmQuarkList	/* quarks_return */
#endif
);

extern void NrmStringToBindingQuarkList(
#if NhlNeedProto
    Const char*	/* string */,
    NrmBindingList	/* bindings_return */,
    NrmQuarkList	/* quarks_return */
#endif
);

/****************************************************************
 *
 * Name and Class lists.
 *
 ****************************************************************/

typedef NrmQuark     NrmName;
typedef NrmQuarkList NrmNameList;
#define NrmNameToString(name)		NrmQuarkToString(name)
#define NrmStringToName(string)		NrmStringToQuark(string)
#define NrmStringToNameList(str, name)	NrmStringToQuarkList(str, name)

typedef NrmQuark     NrmClass;
typedef NrmQuarkList NrmClassList;
#define NrmClassToString(class)		NrmQuarkToString(class)
#define NrmStringToClass(class)		NrmStringToQuark(class)
#define NrmStringToClassList(str,class)	NrmStringToQuarkList(str, class)



/****************************************************************
 *
 * Resource Representation Types and Values
 *
 ****************************************************************/

typedef NrmQuark     NrmRepresentation;
#define NrmStringToRepresentation(string)   NrmStringToQuark(string)
#define	NrmRepresentationToString(type)   NrmQuarkToString(type)

typedef struct _NrmValue{
    unsigned int	size;
    NhlArgVal		data;
    NrmQuark		typeQ;	/* This is only valid inside converter funcs */
} NrmValue, *NrmValuePtr;


/****************************************************************
 *
 * Resource Manager Functions
 *
 ****************************************************************/

typedef struct _NrmHashBucketRec *NrmHashBucket;
typedef NrmHashBucket *NrmHashTable;
typedef NrmHashTable NrmSearchList[];
typedef struct _NrmHashBucketRec *NrmDatabase;


extern void NrmDestroyDB(
#if NhlNeedProto
    NrmDatabase		/* database */    
#endif
);

extern void NrmQPutResource(
#if NhlNeedProto
    NrmDatabase*	/* database */,
    NrmBindingList	/* bindings */,
    NrmQuarkList	/* quarks */,
    NrmRepresentation	/* type */,
    NrmValue*		/* value */
#endif
);

extern void NrmPutResource(
#if NhlNeedProto
    NrmDatabase*	/* database */,
    Const char*	/* specifier */,
    Const char*	/* type */,
    NrmValue*		/* value */
#endif
);

extern void NrmQPutStringResource(
#if NhlNeedProto
    NrmDatabase*	/* database */,
    NrmBindingList      /* bindings */,
    NrmQuarkList	/* quarks */,
    Const char*	/* value */
#endif
);

extern void NrmPutStringRes(
#if	NhlNeedProto
    NrmDatabase*,	/* database */
    Const char*,	/* specifier */
    Const char*		/* value */
#endif
);

extern void NrmPutLineResource(
#if NhlNeedProto
    NrmDatabase*	/* database */,
    Const char*	/* line */
#endif
);

extern NhlBoolean NrmQGetResource(
#if NhlNeedProto
    NrmDatabase		/* database */,
    NrmNameList		/* quark_name */,
    NrmClassList	/* quark_class */,
    NrmRepresentation*	/* quark_type_return */,
    NrmValue*		/* value_return */
#endif
);

extern NhlBoolean NrmGetResource(
#if NhlNeedProto
    NrmDatabase		/* database */,
    Const char*	/* str_name */,
    Const char*	/* str_class */,
    char**		/* str_type_return */,
    NrmValue*		/* value_return */
#endif
);

extern NhlBoolean NrmQGetSearchList(
#if NhlNeedProto
    NrmDatabase		/* database */,
    NrmNameList		/* names */,
    NrmClassList	/* classes */,
    NrmSearchList	/* list_return */,
    int			/* list_length */
#endif
);

extern NhlBoolean NrmGetQResFromList(
#if NhlNeedProto
    NrmSearchList	/* list */,
    NrmName		/* name */,
    NrmClass		/* class */,
    NrmRepresentation*	/* type_return */,
    NrmValue*		/* value_return */
#endif
);

/****************************************************************
 *
 * Resource Database Management
 *
 ****************************************************************/

extern NrmDatabase NrmGetFileDB(
#if NhlNeedProto
    Const char*	/* filename */
#endif
);

extern int NrmCombineFileDB(
#if NhlNeedProto
    Const char* 	/* filename */,
    NrmDatabase*	/* target */,
    NhlBoolean		/* override */
#endif
);

extern NrmDatabase NrmGetStringDatabase(
#if NhlNeedProto
    Const char*	/* data */  /*  null terminated string */
#endif
);

extern void NrmPutFileDatabase(
#if NhlNeedProto
    NrmDatabase		/* database */,
    Const char*	/* filename */
#endif
);

extern void NrmMergeDatabases(
#if NhlNeedProto
    NrmDatabase		/* source_db */,
    NrmDatabase*	/* target_db */
#endif
);

extern void NrmCombineDatabase(
#if NhlNeedProto
    NrmDatabase		/* source_db */,
    NrmDatabase*	/* target_db */,
    NhlBoolean		/* override */
#endif
);

#define NrmEnumAllLevels 0
#define NrmEnumOneLevel  1

typedef NhlBoolean (*NrmDBEnumProc)(
#if NhlNeedProto
	NrmDatabase		*db,
	NrmBindingList		bindings,
	NrmQuarkList		quarks,
	NrmRepresentation	*type,
	NrmValue		*value,
	NhlPointer		closure
#endif
);

extern NhlBoolean NrmEnumerateDatabase(
#if	NhlNeedProto
    NrmDatabase		/* db */,
    NrmNameList		/* name_prefix */,
    NrmClassList	/* class_prefix */,
    int			/* mode */,
    NrmDBEnumProc	/* proc */,
    NhlPointer		/* closure */
#endif
);

extern char *NrmLocaleOfDatabase(
#if NhlNeedProto
    NrmDatabase 	/* database */
#endif
);


/****************************************************************
 *
 * Command line option mapping to resource entries
 *
 ****************************************************************/

typedef enum {
    NrmoptionNoArg,	/* Value is specified in OptionDescRec.value	    */
    NrmoptionIsArg,     /* Value is the option string itself		    */
    NrmoptionStickyArg, /* Value is characters immediately following option */
    NrmoptionSepArg,    /* Value is next argument in argv		    */
    NrmoptionResArg,	/* Resource and value in next argument in argv      */
    NrmoptionSkipArg,   /* Ignore this option and the next argument in argv */
    NrmoptionSkipLine,  /* Ignore this option and the rest of argv	    */
    NrmoptionSkipNArgs	/* Ignore this option and the next 
			   OptionDescRes.value arguments in argv */
} NrmOptionKind;

typedef struct {
    char	    *option;	    /* Option abbreviation in argv	    */
    char	    *specifier;     /* Resource specifier		    */
    NrmOptionKind   argKind;	    /* Which style of option it is	    */
    NhlPointer	    value;	    /* Value to provide if NrmoptionNoArg   */
} NrmOptionDescRec, *NrmOptionDescList;


extern void NrmParseCommand(
#if NhlNeedProto
    NrmDatabase*	/* database */,
    NrmOptionDescList	/* table */,
    Const char*		/* name */,
    int*		/* argc_in_out */,
    char**		/* argv_in_out */		     
#endif
);

extern void _NrmInitialize(
#if	NhlNeedProto
	void
#endif
);

extern NhlBoolean NrmQinQList(
#if	NhlNeedProto
	NrmQuarkList,
	NrmQuark
#endif
);

typedef unsigned long Signature;

extern NrmQuark _NrmInternalStringToQuark(
#if	NhlNeedProto
	Const char	*name,
	int		len,
	Signature	sig,
	NhlBoolean	permstring
#endif
);

#endif /* _NRESOURCE_H_ */
/* DON'T ADD STUFF AFTER THIS #endif */
