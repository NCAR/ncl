/*
 *      $Id: BaseP.h,v 1.17 1997-08-11 23:16:10 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		BaseP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:37:49 MDT 1992
 *
 *	Description:	This file makes all the declarations neccessary to
 *			sub-class the BaseClass.
 */
#ifndef _NBaseP_h
#define _NBaseP_h

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/BaseI.h>

/*
 * Class name for resources that are used to determine which programming lang.
 * mode to use.
 */
#define	_NhlClangMode	"Lang.Mode"

typedef struct _NhlObjLayerRec *NhlObjLayer;
typedef struct _NhlObjClassRec *NhlObjClass;

typedef struct _NhlBaseLayerRec *NhlBaseLayer;
typedef struct _NhlBaseClassRec *NhlBaseClass;

typedef struct _NhlChildRec _NhlChildNode, *_NhlChildList;

struct _NhlChildRec {
	int			pid;
	NhlBoolean		svalscalled;
	NhlClass		theclass;
	NrmNameList		resources;
	_NhlChildList		next;
};

typedef struct _NhlAllChildRec _NhlAllChildNode, *_NhlAllChildList;

struct _NhlAllChildRec {
	int			pid;
	_NhlAllChildList	next;
};

/*
 * This structure is a simplified version of the BaseLayerPart.
 * It is used to create objects that don't do graphics and stuff.
 */
typedef struct _NhlObjLayerPart {
	int		id;		/* index into global layer Table*/
	NhlLayer	self;		/* pointer to self		*/
	NhlClass	layer_class;	/* pointer to ClassRec		*/
	NhlLayer	parent;		/* parent Layer			*/
	NrmName		nrm_name;	/* Layer resource name quarkified*/
	Const char	*name;		/* Layer resource name		*/
	NhlLayer	appobj;		/* App Class Object		*/
	NhlBoolean	being_destroyed;
	_NhlCBList	destroycb;
	_NhlCBList	resvaluesetcb;
	_NhlCBList	cchildcb;
	_NhlAllChildList	all_children;
	int		appid;
	_NhlCB		app_destroy;
	NhlPointer	ncl_data;
	NhlPointer	gui_data;
	NhlPointer	gui_data2;
} NhlObjLayerPart;


NhlDOCREF(/design/hlu/Base.html,Base Object Design)
NhlDOCTAG(NhlBaseLayerPart)
typedef struct _NhlBaseLayerPart {
	int		id;		/* index into global layer Table*/
	NhlLayer	self;		/* pointer to self		*/
	NhlClass	layer_class;	/* pointer to ClassRec		*/
	NhlLayer	parent;		/* parent Layer			*/
	NrmName		nrm_name;	/* Layer resource name quarkified*/
	Const char	*name;		/* Layer resource name		*/
	NhlLayer	appobj;		/* App Class Object		*/
	NhlBoolean	being_destroyed;
	_NhlCBList	destroycb;
	_NhlCBList	resvaluesetcb;
	_NhlCBList	cchildcb;
	_NhlAllChildList	all_children;
	int		appid;
	_NhlCB		app_destroy;
	NhlPointer	ncl_data;
	NhlPointer	gui_data;
	NhlPointer	gui_data2;

/* NOTHING CAN BE ADDED BEFORE HERE UNLESS IT IS ALSO ADDED IN ObjLayerPart */

	NhlBoolean		in_init;	/* NhlCreateChild valid call */
	_NhlChildList		children;
	_NhlChildArgList	child_args;

	NhlLayer		wkptr;
	/* import Values */
} NhlBaseLayerPart;

typedef struct _NhlObjRec {
	NhlObjLayerPart	base;
} NhlObjLayerRec;

typedef struct _NhlLayerRec {
	NhlBaseLayerPart	base;
} NhlBaseLayerRec, NhlLayerRec;

typedef struct _NhlChildResRec _NhlChildResNode, *_NhlChildResList;

struct _NhlChildResRec {
	NhlClass		theclass;
	NhlBoolean		autosetval;
	NrmNameList		resources;
	_NhlChildResList	next;
};

typedef struct _NhlRawObjCBRec{
	NhlString	cbname;
	unsigned int	offset;
	int		hash_mult;
	_NhlCBAddHash	add_hash;
	_NhlCBCallHash	call_hash;
	_NhlCBTaskProc	task_proc;
} _NhlRawObjCB, *_NhlRawObjCBList;

typedef struct _NhlCookedObjCBRec{
	NrmQuark	cbquark;
	unsigned int	offset;
	int		hash_mult;
	_NhlCBAddHash	add_hash;
	_NhlCBCallHash	call_hash;
	_NhlCBTaskProc	task_proc;
} _NhlCookedObjCB, *_NhlCookedObjCBList;

typedef struct _NhlRawClassCBRec{
	NhlString	cbname;
	_NhlCBList	cblist;
	int		hash_mult;
	_NhlCBAddHash	add_hash;
	_NhlCBCallHash	call_hash;
	_NhlCBTaskProc	task_proc;
} _NhlRawClassCB, *_NhlRawClassCBList;

typedef struct _NhlCookedClassCBRec{
	NrmQuark	cbquark;
	_NhlCBList	cblist;
	int		hash_mult;
	_NhlCBAddHash	add_hash;
	_NhlCBCallHash	call_hash;
	_NhlCBTaskProc	task_proc;
} _NhlCookedClassCB, *_NhlCookedClassCBList;

typedef NhlErrorTypes (*NhlClassPartInitProc)(
#if	NhlNeedProto
	NhlClass	/* lc to initialize */
#endif
);

typedef NhlErrorTypes (*NhlClassInitProc)(
#if	NhlNeedProto
	void
#endif
);

typedef NhlErrorTypes (*NhlInitProc)(
#if	NhlNeedProto
	NhlClass,	/* class of instance to init	*/
	NhlLayer,	/* request layer		*/
	NhlLayer,	/* new layer			*/
	_NhlArgList,	/* args to set in layer		*/
	int		/* nargs			*/
#endif
);

typedef NhlErrorTypes (*NhlSetValuesProc)(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

typedef NhlErrorTypes (*NhlGetValuesProc)(
#if	NhlNeedProto
	NhlLayer,	/* layer	*/
	_NhlArgList,	/* args to get	*/
	int		/* nargs	*/
#endif
);

typedef NhlErrorTypes (*NhlReparentProc)(
#if	NhlNeedProto
	NhlLayer	l,
	NhlLayer	parent
#endif
);

typedef NhlErrorTypes (*NhlDrawProc)(
#if	NhlNeedProto
	NhlLayer		/* layer to draw	*/
#endif
);

typedef NhlErrorTypes (*NhlDestroyProc)(
#if	NhlNeedProto
	NhlLayer		/* layer to destroy	*/
#endif
);

typedef struct _NhlObjClassPart {
	NhlString		class_name;
	NrmClass		nrm_class;
	unsigned int		layer_size;
	int			class_inited;
	NhlClass 		superclass;
	NhlConvertPtr		*cvt_table;

	NhlResourceList		resources;
	int			num_resources;
	NrmNameList		all_resources;
	_NhlRawObjCBList	callbacks;
	int			num_callbacks;
	_NhlRawClassCBList	class_callbacks;
	int			num_class_callbacks;

	NhlClassPartInitProc	class_part_initialize;
	NhlClassInitProc	class_initialize;
	NhlInitProc		layer_initialize;
	NhlSetValuesProc	layer_set_values;
	NhlSetValuesProc	layer_set_values_hook;
	NhlGetValuesProc	layer_get_values;
	NhlReparentProc		layer_reparent;
	NhlDestroyProc		layer_destroy;
} NhlObjClassPart;

/*
 * NhlDOCREF(/design/hlu/Base.html,Base Object Design)
 */
NhlDOCTAG(NhlBaseClassPart)
typedef struct _NhlBaseClassPart {
	NhlString		class_name;
	NrmClass		nrm_class;
	unsigned int		layer_size;
	int			class_inited;
	NhlClass 		superclass;
	NhlConvertPtr		*cvt_table;

	NhlResourceList		resources;
	int			num_resources;
	NrmNameList		all_resources;
	_NhlRawObjCBList	callbacks;
	int			num_callbacks;
	_NhlRawClassCBList	class_callbacks;
	int			num_class_callbacks;

	NhlClassPartInitProc	class_part_initialize;
	NhlClassInitProc	class_initialize;
	NhlInitProc		layer_initialize;
	NhlSetValuesProc	layer_set_values;
	NhlSetValuesProc	layer_set_values_hook;
	NhlGetValuesProc	layer_get_values;
	NhlReparentProc		layer_reparent;
	NhlDestroyProc		layer_destroy;
/* NOTHING CAN BE ADDED ABOVE HERE WITHOUT ADDING IT TO ObjClassPart */

	_NhlChildResList	child_resources;

	NhlDrawProc		layer_draw;

	NhlDrawProc		layer_pre_draw;
	NhlDrawProc		layer_draw_segonly;
	NhlDrawProc		layer_post_draw;
	NhlDrawProc		layer_clear;
} NhlBaseClassPart;

typedef struct _NhlObjClassRec {
	NhlObjClassPart	base_class;
} NhlObjClassRec;

typedef struct _NhlClassRec {
	NhlBaseClassPart	base_class;
} NhlBaseClassRec, NhlClassRec ;

extern NhlObjClassRec NhlobjClassRec;
extern NhlClassRec NhllayerClassRec;

#define NhlbaseClassRec NhllayerClassRec

#define _NhlName(instance) ((instance!=NULL)?(((NhlLayer)instance)->base.name):"(null)")

extern void _NhlBaseAppDestroyCB(
#if	NhlNeedProto
	NhlArgVal	cbdata,
	NhlArgVal	udata
#endif
);

extern NhlBoolean _NhlBaseAddChild(
#if	NhlNeedProto
	NhlLayer	parent,
	int		child
#endif
);

extern void _NhlBaseRemoveChild(
#if	NhlNeedProto
	NhlLayer	l
#endif
);

extern NhlBoolean _NhlBaseMoveChild(
#if	NhlNeedProto
	NhlLayer	parent,	/* new parent	*/
	NhlLayer	child	/* child	*/
#endif
);

#endif /* _NBaseP_h */	
