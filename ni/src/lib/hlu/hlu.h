/*
 *      $Id: hlu.h,v 1.13 1994-03-23 15:27:35 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hlu.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 09:09:12 MDT 1992
 *
 *	Description:	This file contains all the public information neccessary
 *			to write a program using the hlu library.
 */
#ifndef _NHLU_h
#define _NHLU_h

#include <stdio.h>
#include <string.h>

#include <ncarg/hlu/defs.h>

#ifdef	__STDC__
#define NeedVarArgProto         True 
#include <stdlib.h>
#else
#define NeedVarArgProto		False
#endif

#if	__STDC__
#define Const const
#define NhlNeedProto 1
#else
#ifdef	__STDC__
#define Const const
#else
#define Const
#endif
#endif

/*
 * Macro:	_NHLCALLF
 *
 * Description:	This macro is used whenever a Fortran fuction is being called
 *		from C.  It is used to deal with different calling conventions.
 */
#ifndef	_NHLCALLF
#if	defined(Cray2) || defined(Cray)
/* Brain dead cray's */
#define	_NHLCALLF(reg,caps)	caps

#elif	defined(RS6000) || defined(_HPUX_SOURCE)
/* No munging of names - wow how unique */
#define	_NHLCALLF(reg,caps)	reg

#else	/* Regular old BSD */
#ifdef	__STDC__
#define	_NHLCALLF(reg,caps)	reg##_
#else
#define	_NHLCALLF(reg,caps)	reg/**/_
#endif	/* __STDC__ */
#endif	/* defined... */
#endif	/* _NHLCALLF	*/

#define True 1
#define False 0
#define BIGNUMBER 99e99
#define LITTLENUMBER -99e99

#define NhlTCharacter	"Character"
#define NhlTString	"String"
#define NhlTStringPtr	"StringPtr"
#define NhlTLong	"Long"
#define NhlTShort	"Short"
#define NhlTInteger	"Int"
#define NhlTBoolean	"Boolean"
#define NhlTFloat	"Float"
#define NhlTDouble	"Double"
#define NhlTLongPtr	"LongPtr"
#define NhlTShortPtr	"ShortPtr"
#define NhlTIntegerPtr	"IntPtr"
#define NhlTFloatPtr	"FloatPtr"
#define NhlTFloatPtrPtr	"FloatPtrPtr"
#define NhlTDoublePtr	"DoublePtr"
#define NhlTPointer	"Pointer"
#define NhlTImmediate	"Immediate"
#define NhlTProcedure	"Procedure"
#define	NhlTGenArray	"GenArray"
#define	NhlTQuark	"Quark"
#define	NhlTArray1D	"Array1D"
#define NhlTNull	"NULL"
#define NhlTExtraLayer	"ExtraLayer"
#define	NhlTFont	"Font"

typedef	char	*NhlString;
typedef	void	*NhlPointer;
typedef	int	NhlBoolean;
typedef	int	NhlFont;

/* This declaration will hopefully move to hluP.h */
typedef	struct NhlGenArrayRec_ *NhlGenArray;

typedef enum _NhlErrType{
	NhlFATAL	= -4,
	NhlWARNING	= -3,
	NhlINFO		= -2,
	NhlNOERROR	= -1
} NhlErrorTypes;


#define NhlOffset(p_type,field) \
        ((unsigned int) (((char *) (&(((p_type*)NULL)->field))) - ((char *) NULL)))
#define NhlNumber(arr)           ((unsigned int) (sizeof(arr) / sizeof(arr[0])))

union _NhlType_ {
	NhlPointer	ptrval;
	char		charval;
	short		shrtval;
	int		intval;
	long		lngval;
	float		fltval;
	NhlString	strval;
	double		dblval;
};

typedef	union _NhlType_	NhlArgVal;

NhlDOCTAG(NhlSArg)
typedef struct NhlSArgRec{
	NhlString	name;
	NhlArgVal	value;
} NhlSArg, *NhlSArgList;

NhlDOCTAG(NhlGArg)
typedef struct NhlGArgRec{
	NhlString	resname;
	NhlArgVal	value;
} NhlGArg, *NhlGArgList;

NhlDOCTAG(NhlResource)
typedef struct _NhlResource {
	NhlString	resource_name;
	NhlString	resource_class;
	NhlString	resource_type;
	unsigned int	resource_size;
	unsigned int	resource_offset;
	/* stuff for dealling with defaults */
	NhlString	default_type;
	NhlArgVal	default_val;
} NhlResource, *NhlResourceList;

typedef struct _NhlLayerClassRec *NhlLayerClass;

/* These are here because they needs defs from above. */
#include <ncarg/hlu/Error.h>
#include <ncarg/hlu/ResList.h>

/*
 * These functions are used to create and destroy NhlGenArray description
 * records.
 */
NhlDOCTAG(NhlCreateGenArray)
NhlSRCREF(hlu/hlu.c#NhlCreateGenArray)
extern NhlGenArray NhlCreateGenArray(
#ifdef	NhlNeedProto
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	int		*len_dimensions	/* number of dimensions	*/
#endif
);

NhlDOCTAG(NhlFreeGenArray)
NhlSRCREF(hlu/hlu.c#NhlFreeGenArray)
extern void NhlFreeGenArray(
#ifdef	NhlNeedProto
	NhlGenArray	gen	/* GenArray description record to free	*/
#endif
);

/*
 * These functions will allow us to impliment our own malloc if we need to
 */

extern void *NhlMalloc(
#ifdef	NhlNeedProto
	unsigned int	size
#endif
);

extern void *NhlCalloc( 
#ifdef  NhlNeedProto 
	unsigned int    num, 
	unsigned int    size 
#endif 
);

extern void *NhlRealloc(
#ifdef	NhlNeedProto
	void*,		/* pointer to copy		*/
	unsigned int	/* size of requested memory	*/
#endif
);

extern NhlErrorTypes NhlFree(
#ifdef	NhlNeedProto
	void*		/* pointer to memory to free	*/
#endif
);

extern Const char *NhlName(
#ifdef	NhlNeedProto
	int	pid	/* id of an object	*/
#endif
);

extern Const char *NhlClassName(
#if	NhlNeedProto
	int	pid	/* id of object whose class name is requested */
#endif
);

NhlDOCTAG(NhlSetSArg)
NhlSRCREF(hlu/hlu.c#NhlSetSArg)
/*VARARGS2*/
extern void NhlSetSArg(
#if	NeedVarArgProto
	NhlSArg		*arg,		/* arg to set		*/
	NhlString	resname,	/* resource to set	*/
	...				/* value to set arg to	*/
#endif
);

NhlDOCTAG(NhlSetGArg)
NhlSRCREF(hlu/hlu.c#NhlSetGArg)
/*VARARGS2*/
extern void NhlSetGArg(
#if	NeedVarArgProto
	NhlGArg		*arg,		/* arg to set		*/
	NhlString	resname,	/* resource to set	*/
	...				/* value to set arg to	*/
#endif
);

NhlDOCTAG(NhlVACreate)
NhlSRCREF(hlu/hlu.c#NhlSetGArg)
/*VARARGS4*/
extern NhlErrorTypes NhlVACreate(
#if	NeedVarArgProto
	int*,			/* return plot id			*/
	Const char*,		/* name					*/
	NhlLayerClass,		/* requested class			*/
	int,			/* parent's id				*/
	...			/* res names/values - NULL terminated	*/
#endif
);

NhlDOCTAG(NhlALCreate)
extern NhlErrorTypes NhlALCreate(
#if	NhlNeedProto
	int*,			/* return plot id	*/
	Const char*,		/* name			*/
	NhlLayerClass,		/* requested class	*/
	int,			/* parent's id		*/
	NhlSArgList,		/* setarg list		*/
	int			/* number of Sarg's	*/
#endif
);

extern NhlErrorTypes NhlCreate(
#if	NhlNeedProto
	int*,			/* return plot id	*/
	Const char*,		/* name			*/
	NhlLayerClass,		/* requested class	*/
	int,			/* parent's id		*/
	int			/* RL list id		*/
#endif
);

NhlDOCTAG(NhlDestroy)
extern NhlErrorTypes NhlDestroy(
#if	NhlNeedProto
	int		/* plot id	*/
#endif
);

NhlDOCTAG(NhlDraw)
extern NhlErrorTypes NhlDraw(
#ifdef	NhlNeedProto
	int	/* id */
#endif
);

NhlDOCTAG(NhlOpen)
extern void NhlOpen(
#ifdef	NhlNeedProto
	void
#endif
);

NhlDOCTAG(NhlClose)
extern void NhlClose(
#ifdef	NhlNeedProto 
	void 
#endif 
); 

NhlDOCTAG(NhlVASetValues)
/*VARARGS1*/
extern NhlErrorTypes NhlVASetValues( 
#if	NeedVarArgProto 
	int,		/* id		*/
	...		/* resource names and values - NULL terminated	*/
#endif 
);

NhlDOCTAG(NhlALSetValues)
extern NhlErrorTypes NhlALSetValues(
#if	NhlNeedProto
	int		pid,	/* id		*/
	NhlSArgList	args,	/* args to set	*/
	int		nargs	/* num args	*/
#endif
);

extern NhlErrorTypes NhlSetValues(
#if	NhlNeedProto
	int	pid,	/* id of object	*/
	int	rlid	/* RL list id	*/
#endif
);

NhlDOCTAG(NhlVAGetValues)
/*VARARGS1*/
extern NhlErrorTypes NhlVAGetValues(
#if	NeedVarArgProto
	int,		/* id		*/
	...		/* resource names and values - NULL terminated	*/
#endif
);

extern NhlErrorTypes NhlALGetValues(
#if	NhlNeedProto
	int		pid,	/* id			*/
	NhlGArgList	args,	/* args to retrieve	*/
	int		nargs	/* num args		*/
#endif
);

extern NhlErrorTypes NhlGetValues(
#if	NhlNeedProto
	int	pid,	/* id of object	*/
	int	rlid	/* RL list id	*/
#endif
);

#endif /* _NHLU_h */
