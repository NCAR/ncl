/*
 *      $Id: hluP.h,v 1.15 1994-05-05 18:17:56 ethan Exp $
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
 * For initializing the union ArgVal in the NhlResource struct
 * (works around problem with loading CodeCenter source)
 */

#ifdef CODECENTER
#define _NhlUSET(uset) uset
#else
#define _NhlUSET(uset) {(uset)}
#endif

typedef struct _NhlLayerRec *NhlLayer;

/*
* In order to recognize class types from the intrinsics the private header
* and a one bit flag need to be included here and a new conditional stmnt
* added to the InitializeLayerClass function in Create.c
*/

#define _NhlObjLayerClassFlag		0x02
#define _NhlIsObj(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlObjLayerClassFlag)

#define _NhlBaseLayerClassFlag 		0x04
#define _NhlIsBase(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlBaseLayerClassFlag)

#define _NhlWorkstationLayerClassFlag 	0x08
#define _NhlIsWorkstation(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
						_NhlWorkstationLayerClassFlag)

#define _NhlViewLayerClassFlag 	0x010
#define _NhlIsView(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlViewLayerClassFlag)

#define _NhlTransformLayerClassFlag 	0x020
#define _NhlIsTransform(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
						_NhlTransformLayerClassFlag)

#define _NhlErrorLayerClassFlag	0x040
#define _NhlIsError(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlErrorLayerClassFlag)

#define _NhlTransObjLayerClassFlag		0x080
#define _NhlIsTransObj(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlTransObjLayerClassFlag)

#define	_NhlDataCommLayerClassFlag	0x0100
#define	_NhlIsDataComm(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlDataCommLayerClassFlag)

#define	_NhlDataItemLayerClassFlag	0x0200
#define	_NhlIsDataItem(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlDataItemLayerClassFlag)

#define	_NhlDataMgrLayerClassFlag	0x0400
#define	_NhlIsDataMgr(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlDataMgrLayerClassFlag)

#define	_NhlDataSpecLayerClassFlag	0x0800
#define	_NhlIsDataSpec(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlDataSpecLayerClassFlag)

#define MIN(a,b)	(((a)<(b))?(a):(b))
#define MAX(a,b)	(((a)>(b))?(a):(b))

typedef	NhlArgVal	_NhlArgVal;

#define _NhlFreeFunc NhlFreeFunc

typedef enum _NhlC_OR_F_{
	_NhlCLIB,
	_NhlFLIB,
	_NhlFCLIB
} _NhlC_OR_F;

/*
 * type_ret, size_ret and free_func are for the get_values method to use - they
 * are not valid fields for set_values or create.
 */
typedef struct _NhlArgRec{
	NrmQuark		quark;		/* resname Q		*/
	_NhlArgVal		value;		/* val or ptr		*/
	NrmQuark		type;		/* type of *value	*/
	NrmQuark		*type_ret;
	unsigned int		*size_ret;
	_NhlFreeFunc		*free_func;
} _NhlArg, *_NhlArgList;

typedef struct _NhlChildArgRec _NhlChildArgNode, *_NhlChildArgList; 
  
struct _NhlChildArgRec{ 
	NhlLayerClass		class; 
	NhlBoolean		autosetval;
	_NhlArgList		args; 
	int			nargs; 
	NhlBoolean		**args_used;
	_NhlChildArgList	next; 
}; 

/*
 * The len_dimensions member of the following struct points to an array
 * containing the length of each dimension of the data array. It should
 * have num_dimensions elements. The fastest varying dimension (rightmost
 * dimension in C) corresponds to the highest numbered element of the 
 * len_dimensions array. The num_elements member is equal to the 
 * product of the value in each element of len_dimensions, and so contains
 * the total number of elements of the array viewed as a single
 * dimensional array.
 */
typedef struct NhlGenArrayRec_ NhlGenArrayRec;
struct NhlGenArrayRec_{
	int		num_dimensions;
	int		*len_dimensions;
	int		num_elements;
	NrmQuark	typeQ;
	unsigned int	size;
	NhlPointer	data;
	NhlBoolean	my_data;
};

/*
 * This function is used as an inheritance constant.
 */
extern void _NhlInherit(
#if	NhlNeedProto
	void
#endif
);

extern void _NhlInitGetValues(
#if	NhlNeedProto
	void
#endif
);

extern NhlGenArray _NhlCreateGenArray(
#if	NhlNeedProto
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	int		*len_dimensions,/* number of dimensions	*/
	NhlBoolean	copy_data	/* copy data pointer?	*/
#endif
);

extern NhlGenArray _NhlCopyGenArray(
#if	NhlNeedProto
	NhlGenArray	gen,		/* public gen array	*/
	NhlBoolean	copy_data	/* copy data part?	*/
#endif
);

extern NhlErrorTypes _NhlValidatedGenArrayCopy(
#ifdef NhlNeedProto
	 NhlGenArray	*gto, 		/* destination gen array */
	 NhlGenArray	gfrom,		/* source gen array */
	 int		max_el,	      /* maximum number of elements allowed */
	 NhlBoolean	copy_data,	/* copy data part? */
	 NhlBoolean	exact_count,    /* ensure dest counts match source */
	 char		*res_name,	/* associated resource name */
	 char		*caller		/* the user-level calling function */
#endif
);

extern FILE *_NhlTmpFile(
#if	NhlNeedProto
	void
#endif
);

extern void _NhlSArgToSetArgList(
#if	NhlNeedProto
	_NhlArgList	args,	/* args <return>	*/
	NhlSArgList	sargs,	/* args to set		*/
	int		nargs	/* number of args	*/
#endif
);

/*VARARGS4*/
extern NhlErrorTypes _NhlCreateChild(
#if	NeedVarArgProto
	int		*pid,	/* pid return		*/
	Const char	*name,	/* name of child	*/
	NhlLayerClass	class,	/* class to create	*/
	NhlLayer	parent,	/* parent of child	*/
	...			/* args to set in child	*/
#endif
);

extern NhlErrorTypes _NhlALCreateChild(
#if	NhlNeedProto
	int		*pid,		/* pid return		*/
	Const char	*name,		/* name of child	*/
	NhlLayerClass	class,		/* class to create	*/
	NhlLayer	parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args		*/
#endif
);

/*VARARGS2*/
extern NhlErrorTypes _NhlSetValuesChild(
#if	NeedVarArgProto
	int		pid,	/* pid return		*/
	NhlLayer	parent,	/* parent of child	*/
	...			/* args to set in child	*/
#endif
);

extern NhlErrorTypes _NhlALSetValuesChild(
#if	NhlNeedProto
	int		pid,		/* pid return		*/
	NhlLayer		parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args in	*/
#endif
);

extern NhlErrorTypes _NhlDestroyChild(
#if	NhlNeedProto
	int		pid,	/* pid of child to destroy	*/
	NhlLayer	parent	/* parent of child to destroy	*/
#endif
);

/*VARARGS4*/
extern NhlErrorTypes _NhlRegisterChildClass(
#if	NeedVarArgProto
	NhlLayerClass	parent,		/* parent class			*/
	NhlLayerClass	child,		/* child class			*/
	NhlBoolean	autosetval,	/* SetValue im/ex plicite	*/
	NhlBoolean	forward,	/* T-frwd listed F-excl listed	*/
	...				/* resource names		*/
#endif
);

extern NhlLayerClass _NhlClass(
#if	NhlNeedProto
	NhlLayer	/* layer	*/
#endif
);

extern Const char * _NhlClassName(
#if	NhlNeedProto
	NhlLayerClass  /* lc */
#endif
);

NhlBoolean _NhlIsFloatRes ( 
#if	NhlNeedProto 
	NhlString	res_name	/* resource name        */ 
#endif
); 

extern int _NhlAddLayer(
#if	NhlNeedProto
	NhlLayer	l		/* layer to add to list	*/
#endif
);

extern NhlErrorTypes _NhlRemoveLayer(
#if	NhlNeedProto
	NhlLayer	l		/* layer to remove from list	*/
#endif
);

extern NhlLayer _NhlGetLayer(
#if	NhlNeedProto
	int	id		/* id of layer to retrieve	*/
#endif
);

extern void _NhlDestroyLayerTable(
#if	NhlNeedProto
	void
#endif
);

extern NhlErrorTypes _NhlSetValues(
#if	NhlNeedProto
	NhlLayer	l,		/* layer instance	*/
	_NhlArgList	args,		/* args to change	*/
	int		nargs		/* number of args	*/
#endif
);

extern NhlErrorTypes _NhlReparent(
#if	NhlNeedProto
	NhlLayer	l,
	NhlLayer	new_parent
#endif
);

extern NhlLayer _NhlGetWorkstationLayer(
#ifdef NhlNeedProto
	NhlLayer	/*parent */
#endif
);

/*
 * Globally callable functions from Draw.c
 */

extern NhlErrorTypes _NhlPreDraw(
#ifdef NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

extern NhlErrorTypes _NhlDraw(
#ifdef NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

extern NhlErrorTypes _NhlPostDraw(
#ifdef NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

extern NhlErrorTypes _NhlSegDraw(
#ifdef NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

/*
 * End of Draw.c functions
 */

extern void _NhlConvertersInitialize(
#if	NhlNeedProto
	_NhlC_OR_F	init_type
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

extern  NhlErrorTypes _NhlInitializeLayerClass(
#ifdef NhlNeedProto
NhlLayerClass /* lc */
#endif
);

extern NhlBoolean _NhlArgIsSet(
#ifdef NhlNeedProto
        _NhlArgList 	/* args */,
        int    		/* num_args */,
        char* 	 	/*resource_name*/
#endif
);

extern void _NhlCopyToVal(
#if	NhlNeedProto
	NhlPointer	src,
	_NhlArgVal	*dst,
	unsigned int	size
#endif
);

#endif /* HLUP_H */
