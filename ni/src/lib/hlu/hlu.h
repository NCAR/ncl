/*
 *      $Id: hlu.h,v 1.4 1993-10-23 00:35:12 dbrown Exp $
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

typedef	char	*NhlString;
typedef	void	*NhlPointer;
typedef	int	NhlBoolean;

typedef	struct NhlGenArrayRec_ *NhlGenArray;

typedef enum _NhlErrType{
	FATAL	= -4,
	WARNING	= -3,
	INFO	= -2,
	NOERROR	= -1
} NhlErrorTypes;


/* pseudo Boolean types */

#define NhlTOrientation NhlTBoolean
#define NhlHORIZONTAL	0
#define NhlVERTICAL	1
typedef NhlBoolean NhlOrientation;

/* position enumeration */

#define NhlTPosition "Position"
typedef enum _NhlPosition {
	NhlTOP,
	NhlBOTTOM,
	NhlRIGHT,
	NhlLEFT,
	NhlCENTER,
	NhlBOTH       /* LabelBar needs this */
} NhlPosition;

/* justification enumeration */

#define NhlTJustification "Justification"
typedef enum _NhlJustification {
	NhlTOPLEFT,
	NhlCENTERLEFT,
	NhlBOTTOMLEFT,
	NhlTOPCENTER,
	NhlCENTERCENTER,
	NhlBOTTOMCENTER,
	NhlTOPRIGHT,
	NhlCENTERRIGHT,
	NhlBOTTOMRIGHT
} NhlJustification;


#define NhlOffset(p_type,field) \
        ((unsigned int) (((char *) (&(((p_type*)NULL)->field))) - ((char *) NULL)))
#define NhlNumber(arr)           ((unsigned int) (sizeof(arr) / sizeof(arr[0])))

typedef	long	NhlArgVal;

typedef struct NhlSArgRec{
	NhlString	name;
	NhlArgVal	value;
} NhlSArg, *NhlSArgList;

typedef struct NhlGArgRec{
	NhlString	resname;
	NhlArgVal	value;
} NhlGArg, *NhlGArgList;

typedef struct _NhlResource {
	NhlString	resource_name;
	NhlString	resource_class;
	NhlString	resource_type;
	unsigned int	resource_size;
	unsigned int	resource_offset;
	/* stuff for dealling with defaults */
	NhlString	default_type;
	NhlPointer	default_addr;
} NhlResource, *NhlResourceList;

typedef struct _NhlBoundingBox {
        int     set;
        float   t;
        float   b;
        float   l;
        float   r;
} NhlBoundingBox;

typedef struct _NhlCoord {
	float x;
	float y;
} NhlCoord;

typedef struct _LayerClassRec *LayerClass;
typedef struct _LayerRec *Layer;
typedef struct _LayerList {
	struct _LayerRec *layer;
	struct _LayerList *next;
} LayerListNode, *LayerList;

/*
 * Error.h is included here because everything needs it - and it needs the
 * declarations of Layer and LayerClass before it.
 */
#include <ncarg/hlu/Error.h>

/*
 * These functions are used to create and destroy NhlGenArray description
 * records.
 */
extern NhlGenArray NhlCreateGenArray(
#ifdef	NhlNeedProto
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	int		*len_dimensions	/* number of dimensions	*/
#endif
);

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

/*VARARGS2*/
extern void NhlSetSArg(
#if	NeedVarArgProto
	NhlSArg		*arg,		/* arg to set		*/
	NhlString	resname,	/* resource to set	*/
	...				/* value to set arg to	*/
#endif
);

/*VARARGS2*/
extern void NhlSetGArg(
#if	NeedVarArgProto
	NhlGArg		*arg,		/* arg to set		*/
	NhlString	resname,	/* resource to set	*/
	...				/* value to set arg to	*/
#endif
);

/*VARARGS4*/
extern NhlErrorTypes NhlCreate(
#if	NeedVarArgProto
	int*,			/* return plot id			*/
	Const char*,		/* name					*/
	LayerClass,		/* requested class			*/
	int,			/* parent's id				*/
	...			/* res names/values - NULL terminated	*/
#endif
);

extern NhlErrorTypes NhlALCreate(
#if	NhlNeedProto
	int*,			/* return plot id	*/
	Const char*,		/* name			*/
	LayerClass,		/* requested class	*/
	int,			/* parent's id		*/
	NhlSArgList,		/* setarg list		*/
	int			/* number of Sarg's	*/
#endif
);

extern NhlErrorTypes NhlDestroy(
#if	NhlNeedProto
	int		/* plot id	*/
#endif
);

extern NhlErrorTypes NhlDraw(
#ifdef	NhlNeedProto
	int	/* id */
#endif
);

extern void NhlOpen(
#ifdef	NhlNeedProto
	void
#endif
);

extern void NhlClose(
#ifdef	NhlNeedProto 
	void 
#endif 
); 

/*VARARGS1*/
extern NhlErrorTypes NhlSetValues( 
#if	NeedVarArgProto 
	int,		/* id		*/
	...		/* resource names and values - NULL terminated	*/
#endif 
);

extern NhlErrorTypes NhlALSetValues(
#if	NhlNeedProto
	int		pid,	/* id		*/
	NhlSArgList	args,	/* args to set	*/
	int		nargs	/* num args	*/
#endif
);

/*VARARGS1*/
extern NhlErrorTypes NhlGetValues(
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

extern NhlErrorTypes NhlChangeWorkstation(
#if	NhlNeedProto
	int	plotid,		/* plotid to move to new workstation	*/
	int	workid		/* workid of workstation		*/
#endif
);

extern NhlErrorTypes NhlUpdateWorkstation(
#if	NhlNeedProto
	int	workid	/* workid of workstation to update	*/
#endif
);

extern NhlErrorTypes NhlClearWorkstation(
#if	NhlNeedProto
	int	workid	/* workid of workstation to clear	*/
#endif
);

extern NhlErrorTypes   NhlFrame(
#if	NhlNeedProto
	int /*wid*/
#endif
);


extern NhlErrorTypes NhlSetColor(
#ifdef NhlNeedProto
int 	/* pid */,
int     /* ci */,
float   /* red */,
float   /* green */,
float   /* blue */
#endif
);

extern NhlErrorTypes NhlFreeColor(
#ifdef NhlNeedProto
        int 	/* pid */,
        int     /* ci */
#endif
);

extern int NhlNewColor(
#ifdef NhlNeedProto
        int     /* pid*/,
        float   /* red */,
        float   /* green */,
        float   /* blue */
#endif
);

extern int NhlGetGksCi(
#ifdef NhlNeedProto
        int     /* pid */,
        int     /* ci   */
#endif
);
int NhlGetGksWorkId(
#ifdef NhlNeedProto
int /* workid */
#endif
);

int NhlNewMarker(
#ifdef NhlNeedProto
	Layer instance, 
	char *marker_string, 
	float x_off, 
	float y_off,
	float aspect_adj,
	float size_adj
#endif
);

NhlErrorTypes NhlSetMarker(
#ifdef NhlNeedProto
	Layer instance, 
	int	index,
	char	*marker_string, 
	float	x_off, 
	float	y_off,
	float	aspect_adj,
	float	size_adj
#endif
);

extern NhlErrorTypes NhlGetBB(
#ifdef NhlNeedProto
	int,		/* pid */
	NhlBoundingBox*	/* thebox */
#endif
);

extern NhlErrorTypes NhlNDCToData(
#ifdef NhlNeedProto
	int	/*pid*/,
	float* /*x*/,
	float* /*y*/,
	int	/*n*/,
	float* /*xout*/,
	float* /*yout*/,
	float * /* xmissing */,
	float * /* ymissing */
#endif
);

extern NhlErrorTypes NhlDataToNDC(
#ifdef NhlNeedProto
	int	/*pid*/,
	float* /*x*/,
	float* /*y*/,
	int	/*n*/,
	float* /*xout*/,
	float* /*yout*/,
	float * /* xmissing */,
	float * /* ymissing */
#endif
);


#endif /* _NHLU_h */
