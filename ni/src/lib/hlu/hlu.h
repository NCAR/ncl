/*
 *      $Id: hlu.h,v 1.1 1993-04-30 17:26:44 boote Exp $
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

#include <ncarg/hlu/defs.h>

#if	__STDC__ && !(__CENTERLINE__ && sun) 
#define NeedVarArgProto         True 
#else
#define NeedVarArgProto		False
#endif

#if	__STDC__
#define Const const
#define NhlNeedProto 1
#else
#ifdef	__STDC__
#define Const const
#endif
#define Const
#endif

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
#define NhlTPtr		"Ptr"
#define NhlTImmediate	"Immediate"
#define NhlTProcedure	"Procedure"
#define NhlTNull	"NULL"

typedef	char	*NhlString;
typedef	void	*NhlPointer;
typedef	int	NhlBoolean;

typedef enum _NhlErrType{
	FATAL	= -4,
	WARNING	= -3,
	INFO	= -2,
	NOERROR	= -1
} NhlErrorTypes;

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
 * These functions will allow us to impliment our on malloc if we need to
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
	int*,		/* return plot id				*/
	NhlString,	/* name						*/
	LayerClass,	/* requested class				*/
	int,		/* parent's id					*/
	...		/* resource names and values - NULL terminated	*/
#endif
);

extern NhlErrorTypes NhlALCreate(
#if	NhlNeedProto
	int*,		/* return plot id				*/
	NhlString,	/* name						*/
	LayerClass,	/* requested class				*/
	int,		/* parent's id					*/
	NhlSArgList,	/* setarg list					*/
	int		/* number of Sarg's				*/
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
