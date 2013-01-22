/*
 *      $Id: hluP.h,v 1.42.4.1 2008-03-28 20:37:38 grubin Exp $
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
#include <ncarg/hlu/Callbacks.h>
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

#define	_NhlRES_DEFAULT		(0)
#define	_NhlRES_NORACCESS	(0x01)
#define	_NhlRES_NOCACCESS	(0x02)
#define	_NhlRES_NOSACCESS	(0x04)
#define	_NhlRES_NOGACCESS	(0x08)
#define _NhlRES_PRIVATE		(0x10)
#define _NhlRES_INTERCEPTED	(0x20)

#define	_NhlRES_RONLY	(_NhlRES_NOCACCESS|_NhlRES_NOSACCESS|_NhlRES_NOGACCESS)
#define	_NhlRES_CONLY	(_NhlRES_NORACCESS|_NhlRES_NOSACCESS|_NhlRES_NOGACCESS)
#define	_NhlRES_SONLY	(_NhlRES_NORACCESS|_NhlRES_NOCACCESS|_NhlRES_NOGACCESS)
#define	_NhlRES_GONLY	(_NhlRES_NORACCESS|_NhlRES_NOCACCESS|_NhlRES_NOSACCESS)
#define	_NhlRES_RCONLY	(_NhlRES_NOSACCESS|_NhlRES_NOGACCESS)
#define	_NhlRES_SGONLY	(_NhlRES_NORACCESS|_NhlRES_NOCACCESS)
#define	_NhlRES_CGONLY	(_NhlRES_NORACCESS|_NhlRES_NOSACCESS)
#define _NhlRES_NOACCESS	(_NhlRES_NORACCESS|_NhlRES_NOCACCESS| \
				_NhlRES_NOSACCESS|_NhlRES_NOGACCESS)

typedef struct _NhlResource {
	NhlString	resource_name;
	NhlString	resource_class;
	NhlString	resource_type;
	unsigned int	resource_size;
	unsigned int	resource_offset;
	/* stuff for dealling with defaults */
	NhlString	default_type;
	NhlArgVal	default_val;
	unsigned int	res_info;
	NhlFreeFunc	free_func;
	NhlClass	nhlclass;
} NhlResource, *NhlResourceList;

typedef struct _NhlLayerRec *NhlLayer;

/*
* In order to recognize class types from the intrinsics the private header
* and a one bit flag need to be included here and a new conditional stmnt
* added to the InitializeClass function in Create.c
*/

#define _NhlObjClassFlag		0x02
#define _NhlIsObj(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlObjClassFlag)

#define _NhlBaseClassFlag 		0x04
#define _NhlIsBase(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlBaseClassFlag)

#define _NhlWorkstationClassFlag 	0x08
#define _NhlIsWorkstation(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
						_NhlWorkstationClassFlag)

#define _NhlViewClassFlag 	0x010
#define _NhlIsView(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlViewClassFlag)

#define _NhlTransformClassFlag 	0x020
#define _NhlIsTransform(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
						_NhlTransformClassFlag)

#define _NhlErrorClassFlag	0x040
#define _NhlIsError(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlErrorClassFlag)

#define _NhlTransObjClassFlag		0x080
#define _NhlIsTransObj(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlTransObjClassFlag)

#define	_NhlDataCommClassFlag	0x0100
#define	_NhlIsDataComm(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlDataCommClassFlag)

#define	_NhlDataItemClassFlag	0x0200
#define	_NhlIsDataItem(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlDataItemClassFlag)

#define	_NhlDataMgrClassFlag	0x0400
#define	_NhlIsDataMgr(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlDataMgrClassFlag)

#define	_NhlDataSpecClassFlag	0x0800
#define	_NhlIsDataSpec(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlDataSpecClassFlag)

#define	_NhlAppClassFlag	0x1000
#define	_NhlIsApp(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlAppClassFlag)
#define	_NhlStyleClassFlag	0x2000
#define	_NhlIsStyle(instance) \
	(((NhlLayer)(instance))->base.layer_class->base_class.class_inited & \
							_NhlStyleClassFlag)

#define	_NhlXWorkstationClassFlag	0x4000
#define	_NhlIsXWorkstation(inst) \
	(((NhlLayer)(inst))->base.layer_class->base_class.class_inited & \
						_NhlXWorkstationClassFlag)

typedef	NhlArgVal	_NhlArgVal;

typedef NhlFreeFunc _NhlFreeFunc;

typedef NhlPointer (*_NhlAllocFunc)(
#if     NhlNeedProto
	ng_usize_t	size
#endif
);

typedef enum _NhlC_OR_F_{
	_NhlCLIB,
	_NhlFLIB,
	_NhlNONE
} _NhlC_OR_F;

/*
 * type_ret, size_ret and free_func are for the get_values method to use - they
 * are not valid fields for set_values or create.
 */
typedef struct _NhlArgRec{
	NrmQuark		quark;		/* resname Q		*/
	_NhlArgVal		value;		/* val or ptr		*/
	NrmQuark		type;		/* type of *value	*/
	unsigned int		size;		/* size of *value	*/
	NrmQuark		*type_ret;
	unsigned int		*size_ret;
	_NhlFreeFunc		*free_func;
	NhlClass		*chld_class;
} _NhlArg, *_NhlArgList;

typedef struct _NhlChildArgRec _NhlChildArgNode, *_NhlChildArgList; 
  
struct _NhlChildArgRec{ 
	NhlClass		theclass; 
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
	int	num_dimensions;
	ng_size_t	*len_dimensions;
	ng_size_t	num_elements;
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

extern NhlGenArray _NhlAllocCreateGenArray(
#if	NhlNeedProto
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	ng_size_t	*len_dimensions,/* number of dimensions	*/
	NhlBoolean	copy_data,	/* copy data pointer?	*/
	_NhlAllocFunc	alloc_func	/* alloc func to use	*/
#endif
);

extern NhlGenArray _NhlCreateGenArray(
#if	NhlNeedProto
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	ng_size_t		*len_dimensions,/* number of dimensions	*/
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
#if	NhlNeedProto
	 NhlGenArray	*gto, 		/* destination gen array */
	 NhlGenArray	gfrom,		/* source gen array */
	 ng_size_t	max_el,	        /* maximum number of elements allowed */
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

extern int _NhlCompareArg(
#if	NhlNeedProto
	Const void	*ov,
	Const void	*tv
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
extern NhlErrorTypes _NhlVACreateChild(
#if	NhlNeedVarArgProto
	int		*pid,	/* pid return		*/
	Const char	*name,	/* name of child	*/
	NhlClass	theclass,	/* class to create	*/
	NhlLayer	parent,	/* parent of child	*/
	...			/* args to set in child	*/
#endif
);

extern NhlErrorTypes _NhlALCreateChild(
#if	NhlNeedProto
	int		*pid,		/* pid return		*/
	Const char	*name,		/* name of child	*/
	NhlClass	theclass,		/* class to create	*/
	NhlLayer	parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args		*/
#endif
);

/*VARARGS2*/
extern NhlErrorTypes _NhlVASetValuesChild(
#if	NhlNeedVarArgProto
	int		pid,	/* pid return		*/
	NhlLayer	parent,	/* parent of child	*/
	...			/* args to set in child	*/
#endif
);

extern NhlErrorTypes _NhlALSetValuesChild(
#if	NhlNeedProto
	int		pid,		/* pid return		*/
	NhlLayer	parent,		/* parent of child	*/
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
#if	NhlNeedVarArgProto
	NhlClass	parent,		/* parent class			*/
	NhlClass	child,		/* child class			*/
	NhlBoolean	autosetval,	/* SetValue im/ex plicite	*/
	NhlBoolean	forward,	/* T-frwd listed F-excl listed	*/
	...				/* resource names		*/
#endif
);

extern NhlClass _NhlClass(
#if	NhlNeedProto
	NhlLayer	/* layer	*/
#endif
);

extern Const char * _NhlClassName(
#if	NhlNeedProto
	NhlClass  /* lc */
#endif
);

extern NhlBoolean _NhlIsClass(
#if	NhlNeedProto
	NhlLayer	l,
	NhlClass	cl
#endif
);

extern NhlBoolean _NhlIsFloatRes( 
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

extern NhlErrorTypes _NhlReparent(
#if	NhlNeedProto
	NhlLayer	l,
	NhlLayer	new_parent
#endif
);

extern NhlLayer _NhlGetWorkstationLayer(
#if	NhlNeedProto
	NhlLayer	/*parent */
#endif
);

/*
 * Globally callable functions from Draw.c
 */

extern NhlErrorTypes _NhlPlotManagerDraw(
#if	NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

extern NhlErrorTypes _NhlPreDraw(
#if	NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

extern NhlErrorTypes _NhlDraw(
#if	NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

extern NhlErrorTypes _NhlPostDraw(
#if	NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

extern NhlErrorTypes _NhlSegDraw(
#if	NhlNeedProto
	NhlLayer	layer	/* layer of object to draw	*/
#endif
);

/*
 * End of Draw.c functions
 */

extern  NhlErrorTypes _NhlInitializeClass(
#if	NhlNeedProto
NhlClass /* lc */
#endif
);

extern int _NhlArgIsSet(
#if	NhlNeedProto
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

extern NhlBoolean _NhlLLErrCheckPrnt(
#if	NhlNeedProto
	NhlErrorTypes	level,
	NhlString	calling_func
#endif
);

extern NhlErrorTypes _NhlResUnset(
#if	NhlNeedProto
	NrmName		name,
	NrmClass	cname,
	NhlPointer	base,
	unsigned int	offset
#endif
);

extern _NhlCB _NhlAddObjCallback(
	NhlLayer	l,
	NhlString	cbname,
	NhlArgVal	sel,
	_NhlCBFunc	func,
	NhlArgVal	udata
);

extern void _NhlCallObjCallbacks(
	NhlLayer	l,
	NhlString	cbname,
	NhlArgVal	sel,
	NhlArgVal	cbdata
);

extern void _NhlIterateObjCallbacks(
	NhlLayer	l,
	NhlString	cbname,
	_NhlCBTask	task,
	NhlArgVal	cbdata
);

extern _NhlCB _NhlAddClassCallback(
	NhlClass	lc,
	NhlString	cbname,
	NhlArgVal	sel,
	_NhlCBFunc	f,
	NhlArgVal	udata
);

extern void _NhlCallClassCallbacks(
	NhlClass	lc,
	NhlString	cbname,
	NhlArgVal	sel,
	NhlArgVal	cbdata
);

extern void _NhlIterateClassCallbacks(
	NhlClass	lc,
	NhlString	cbname,
	_NhlCBTask	task,
	NhlArgVal	cbdata
);

#endif /* HLUP_H */
