/*
 *      $Id: BaseP.h,v 1.6 1994-10-28 03:13:40 boote Exp $
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
 *			sub-class the BaseLayerClass.
 */
#ifndef _NBaseP_h
#define _NBaseP_h

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/Base.h>

/*
 * Class name for resources that are used to determine which programming lang.
 * mode to use.
 */
#define	_NhlClangMode	"Lang.Mode"

typedef struct _NhlObjLayerRec *NhlObjLayer;
typedef struct _NhlObjLayerClassRec *NhlObjLayerClass;

typedef struct _NhlBaseLayerRec *NhlBaseLayer;
typedef struct _NhlBaseLayerClassRec *NhlBaseLayerClass;

typedef struct _NhlChildRec _NhlChildNode, *_NhlChildList;

struct _NhlChildRec {
	int			pid;
	NhlBoolean		svalscalled;
	NhlLayerClass		class;
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
	NhlLayerClass	layer_class;	/* pointer to ClassRec		*/
	NhlLayer	parent;		/* parent Layer			*/
	NrmName		nrm_name;	/* Layer resource name quarkified*/
	Const char	*name;		/* Layer resource name		*/
	NhlLayer	appobj;		/* App Class Object		*/
	_NhlAllChildList	all_children;
} NhlObjLayerPart;


NhlDOCREF(/design/hlu/Base.html,Base Object Design)
NhlDOCTAG(NhlBaseLayerPart)
typedef struct _NhlBaseLayerPart {
	int		id;		/* index into global layer Table*/
	NhlLayer	self;		/* pointer to self		*/
	NhlLayerClass	layer_class;	/* pointer to ClassRec		*/
	NhlLayer	parent;		/* parent Layer			*/
	NrmName		nrm_name;	/* Layer resource name quarkified*/
	Const char	*name;		/* Layer resource name		*/
	NhlLayer	appobj;		/* App Class Object		*/
	_NhlAllChildList	all_children;

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
	NhlLayerClass		class;
	NhlBoolean		autosetval;
	NrmNameList		resources;
	_NhlChildResList	next;
};

typedef NhlErrorTypes (*NhlClassPartInitProc)(
#if	NhlNeedProto
	NhlLayerClass	/* lc to initialize */
#endif
);

typedef NhlErrorTypes (*NhlClassInitProc)(
#if	NhlNeedProto
	void
#endif
);

typedef NhlErrorTypes (*NhlInitProc)(
#if	NhlNeedProto
	NhlLayerClass,	/* class of instance to init	*/
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

typedef struct _NhlObjLayerClassPart {
	NhlString		class_name;
	NrmClass		nrm_class;
	unsigned int		layer_size;
	int			class_inited;
	NhlLayerClass 		superclass;

	NhlResourceList		resources;
	int			num_resources;
	NrmNameList		all_resources;

	NhlClassPartInitProc	class_part_initialize;
	NhlClassInitProc	class_initialize;
	NhlInitProc		layer_initialize;
	NhlSetValuesProc	layer_set_values;
	NhlSetValuesProc	layer_set_values_hook;
	NhlGetValuesProc	layer_get_values;
	NhlReparentProc		layer_reparent;
	NhlDestroyProc		layer_destroy;
} NhlObjLayerClassPart;

/*
 * NhlDOCREF(/design/hlu/Base.html,Base Object Design)
 */
NhlDOCTAG(NhlBaseLayerClassPart)
typedef struct _NhlBaseLayerClassPart {
	NhlString		class_name;
	NrmClass		nrm_class;
	unsigned int		layer_size;
	int			class_inited;
	NhlLayerClass 		superclass;

	NhlResourceList		resources;
	int			num_resources;
	NrmNameList		all_resources;

	NhlClassPartInitProc	class_part_initialize;
	NhlClassInitProc	class_initialize;
	NhlInitProc		layer_initialize;
	NhlSetValuesProc	layer_set_values;
	NhlSetValuesProc	layer_set_values_hook;
	NhlGetValuesProc	layer_get_values;
	NhlReparentProc		layer_reparent;
	NhlDestroyProc		layer_destroy;
/* NOTHING CAN BE ADDED ABOVE HERE WITHOUT ADDING IT TO ObjLayerClassPart */

	_NhlChildResList	child_resources;

	NhlDrawProc		layer_draw;

/* The following are not currently used	*/

	NhlDrawProc		layer_pre_draw;
	NhlDrawProc		layer_draw_segonly;
	NhlDrawProc		layer_post_draw;
	NhlDrawProc		layer_clear;
} NhlBaseLayerClassPart;

typedef struct _NhlObjClassRec {
	NhlObjLayerClassPart	base_class;
} NhlObjLayerClassRec;

typedef struct _NhlLayerClassRec {
	NhlBaseLayerClassPart	base_class;
} NhlBaseLayerClassRec, NhlLayerClassRec ;

extern NhlObjLayerClassRec NhlobjLayerClassRec;
extern NhlLayerClassRec NhllayerClassRec;

#define NhlbaseLayerClassRec NhllayerClassRec

#define _NhlName(instance) (((NhlLayer)instance)->base.name)
#endif /* _NBaseP_h */	
