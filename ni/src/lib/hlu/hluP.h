/*
 *      $Id: hluP.h,v 1.1 1993-04-30 17:26:50 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hluP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:41:38 MDT 1992
 *
 *	Description:	This file contains all the external definitions and
 *			declarations needed by hlu writers that are not
 *			needed by hlu users.
 */
#ifndef HLUP_H
#define HLUP_H

#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Segments.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
* In order to recognize class types from the intrinsics the private header
* and a one bit flag need to be included here and a new conditional stmnt
* added to the InitializeLayerClass function in Create.c
*/


#define BaseLayerClassFlag 		0x02
#define _NhlIsBase(instance) \
    (((Layer)(instance))->base.layer_class->base_class.class_inited & 0x02)

#define WorkstationLayerClassFlag 	0x04
#define _NhlIsWorkstation(instance) \
    (((Layer)(instance))->base.layer_class->base_class.class_inited & 0x04)

#define ViewLayerClassFlag 	0x08
#define _NhlIsView(instance) \
    (((Layer)(instance))->base.layer_class->base_class.class_inited & 0x08)

#define TransformLayerClassFlag 	0x010
#define _NhlIsTransform(instance) \
    (((Layer)(instance))->base.layer_class->base_class.class_inited & 0x010)

#define ErrorLayerClassFlag	0x020
#define _NhlIsError(instance) \
	(((Layer)(instance))->base.layer_class->base_class.class_inited & \
							ErrorLayerClassFlag)

#define TransObjLayerClassFlag		0x040
#define _NhlIsTransObj(instance) \
    (((Layer)(instance))->base.layer_class->base_class.class_inited & 0x040)


#define MIN(a,b)	(((a)<(b))?(a):(b))
#define MAX(a,b)	(((a)>(b))?(a):(b))

typedef	NhlArgVal	_NhlArgVal;

typedef struct _NhlArgRec{
	NrmQuark	quark;
	_NhlArgVal	value;
} _NhlArg, *_NhlArgList;

typedef struct _NhlChildArgRec _NhlChildArgNode, *_NhlChildArgList; 
  
struct _NhlChildArgRec{ 
	LayerClass		class; 
	NhlBoolean		autosetval;
	_NhlArgList		args; 
	int			nargs; 
	_NhlChildArgList	next; 
}; 

/*
 * This function is used as an inheritance constant.
 */
extern void _NhlInherit(
#if	NhlNeedProto
	void
#endif
);

extern void _NhlSArgToSetArgList(
#if	NhlNeedProto
	_NhlArgList	*args,	/* args <return>	*/
	NhlSArgList	sargs,	/* args to set		*/
	int		nargs	/* number of args	*/
#endif
);

extern void _NhlGArgToGetArgList(
#if	NhlNeedProto
	_NhlArgList	*args,	/* args <return>	*/
	NhlGArgList	gargs,	/* args to retrieve	*/
	int		nargs	/* number of args	*/
#endif
);

/*VARARGS4*/
extern NhlErrorTypes _NhlCreateChild(
#if	NeedVarArgProto
	int		*pid,	/* pid return		*/
	NhlString	name,	/* name of child	*/
	LayerClass	class,	/* class to create	*/
	Layer		parent,	/* parent of child	*/
	...			/* args to set in child	*/
#endif
);

extern NhlErrorTypes _NhlALCreateChild(
#if	NhlNeedProto
	int		*pid,		/* pid return		*/
	NhlString	name,		/* name of child	*/
	LayerClass	class,		/* class to create	*/
	Layer		parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args		*/
#endif
);

/*VARARGS2*/
extern NhlErrorTypes _NhlSetValuesChild(
#if	NeedVarArgProto
	int		pid,	/* pid return		*/
	Layer		parent,	/* parent of child	*/
	...			/* args to set in child	*/
#endif
);

extern NhlErrorTypes _NhlALSetValuesChild(
#if	NhlNeedProto
	int		pid,		/* pid return		*/
	Layer		parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args in	*/
#endif
);

extern NhlErrorTypes _NhlDestroyChild(
#if	NhlNeedProto
	int	pid,	/* pid of child to destroy	*/
	Layer	parent	/* parent of child to destroy	*/
#endif
);

/*VARARGS4*/
extern NhlErrorTypes _NhlRegisterChildClass(
#if	NeedVarArgProto
	LayerClass	parent,		/* parent class			*/
	LayerClass	child,		/* child class			*/
	NhlBoolean	autosetval,	/* SetValue im/ex plicite	*/
	NhlBoolean	forward,	/* T-frwd listed F-excl listed	*/
	...				/* resource names		*/
#endif
);

extern LayerClass _NhlClass(
#if	NhlNeedProto
	Layer	/* layer	*/
#endif
);

extern Const char * _NhlClassName(
#if	NhlNeedProto
	LayerClass  /* lc */
#endif
);

NhlBoolean _NhlIsFloatRes ( 
#if	NhlNeedProto 
	NhlString	res_name	/* resource name        */ 
#endif
); 

extern int _NhlAddLayer(
#if	NhlNeedProto
	Layer	l		/* layer to add to list	*/
#endif
);

extern NhlErrorTypes _NhlRemoveLayer(
#if	NhlNeedProto
	Layer	l		/* layer to remove from list	*/
#endif
);

extern Layer _NhlGetLayer(
#if	NhlNeedProto
	int	id		/* id of layer to retrieve	*/
#endif
);


/*
* Globally callable functions from Workstation.c
*/
extern NhlErrorTypes _NhlActivateWorkstation(
#if	NhlNeedProto
        Layer   /* layer*/
#endif
);

extern NhlErrorTypes _NhlDeactivateWorkstation(
#if	NhlNeedProto
        Layer   /* layer*/
#endif
);

extern NhlErrorTypes _NhlCloseWorkstation(
#if	NhlNeedProto
        Layer   /*layer*/
#endif
);

extern NhlErrorTypes _NhlOpenWorkstation(
#ifdef NhlNeedProto
        Layer   /*layer*/
#endif
);

extern  int  _NhlWorkstationId(
#ifdef NhlNeedProto
        Layer   /*instance */
#endif
);

extern Layer _NhlGetWorkstationLayer(
#ifdef NhlNeedProto
	Layer	/*parent */
#endif
);

extern int _NhlGetGksCi(
#ifdef NhlNeedProto
        Layer   /* workstation*/,
        int /* ci*/
#endif
);

extern NhlErrorTypes _NhlSetColor(
#ifdef NhlNeedProto
Layer   /* inst */,
int     /* ci */,
float   /* red */,
float   /* green */,
float   /* blue */
#endif
);

extern NhlErrorTypes _NhlFreeColor(
#ifdef NhlNeedProto
        Layer   /* inst */,
        int     /* ci */
#endif
);
extern int _NhlNewColor(
#ifdef NhlNeedProto
        Layer   /* inst */,
        float   /* red */,
        float   /* green */,
        float   /* blue */
#endif
);

/*
* END of functions from Workstation.c
*/


/*
* END of functions from view.c
*/

/*
* Globally callable functions from Segments.c
*/
extern void _NhlDestroySegTransDat(
#ifdef NhlNeedProto
NhlTransDat*    /* transdat */
#endif
);

extern NhlTransDat      *_NhlInitSegTransDat(
#ifdef NhlNeedProto
float*, /* x */
float*  /* y */
#endif
);

extern void _NhlResetSegTransDat(
#ifdef NhlNeedProto
NhlTransDat*,   /* transdat */
float*,         /* x */
float*          /* y */
#endif
);
extern void _NhlComputeSegTrans(
#ifdef NhlNeedProto
NhlTransDat*,   /* transdat */
float   *,      /* transform */
float   *,      /* xprime */
float   *       /* yprime */
#endif
);

extern NhlErrorTypes _NhlDrawSegment(
#ifdef NhlNeedProto
NhlTransDat*,   /* transdat */
int             /* wksid */
#endif
);
extern void _NhlEvalTrans(
#ifdef NhlNeedProto
float *,        /*transform */
float,          /* x */
float,          /* y */
float *,        /* xprime */
float *         /* yprime */
#endif
);

extern void _NhlStartSegment(
#ifdef NhlNeedProto
NhlTransDat*    /* transdat */
#endif
);

extern void _NhlSetSegTrans(
#ifdef NhlNeedProto
NhlTransDat*,   /* transdat */
float*          /* transform */
#endif
);

extern void _NhlEndSegment();

extern void _NhlConvertersInitialize(
#if	NhlNeedProto
	void
#endif
);

extern void _NhlInitResDatabase(
#if	NhlNeedProto
	void
#endif
);

extern Const char *_NhlGetSysResFile(
#if	NhlNeedProto
	void
#endif
);

extern Const char *_NhlGetUsrResFile(
#if	NhlNeedProto
	void
#endif
);

extern Const char *_NhlResolvePath(
#if	NhlNeedProto
	Const char *	/* raw path name	*/
#endif
);

extern NhlErrorTypes _NhlGetBB(
#ifdef NhlNeedProto
	Layer	instance,
	NhlBoundingBox* /* thebox */
#endif
);


extern NhlErrorTypes _NhlDataToWin(
#ifdef NhlNeedProto
	Layer /* instance */,
	Layer /* parent */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans */,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);

extern NhlErrorTypes _NhlWinToData(
#ifdef NhlNeedProto
	Layer /* instance */,
	Layer /* parent */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);

extern NhlErrorTypes _NhlWinToNDC(
#ifdef NhlNeedProto
	Layer /* instance */,
	Layer /* parent */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes _NhlNDCToWin(
#ifdef NhlNeedProto
	Layer /* instance */,
	Layer /* parent */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes _NhlDataToCompc(
#ifdef NhlNeedProto
	Layer /* instance */,
	Layer /* parent */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes _NhlCompcToData(
#ifdef NhlNeedProto
	Layer /* instance */,
	Layer /* parent */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes _NhlCompcToWin(
#ifdef NhlNeedProto
	Layer /* instance */,
	Layer /* parent */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);

extern NhlErrorTypes _NhlWinToCompc(
#ifdef NhlNeedProto
	Layer /* instance */,
	Layer /* parent */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
NhlErrorTypes   _NhlSetTrans(
#ifdef NhlNeedProto
Layer /* instance*/,
Layer  parent
#endif
);

NhlErrorTypes _NhlInitializeLayerClass(
#ifdef NhlNeedProto
LayerClass /* lc */
#endif
);

#endif /* HLUP_H */
