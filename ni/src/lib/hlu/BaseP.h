/*
 *      $Id: BaseP.h,v 1.2 1993-10-19 17:49:44 boote Exp $
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

typedef struct _NhlChildRec _NhlChildNode, *_NhlChildList;

struct _NhlChildRec {
	int			pid;
	NhlBoolean		svalscalled;
	LayerClass		class;
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
typedef struct _ObjLayerPart {
	int		id;		/* index into global layer Table*/
	Layer		self;		/* pointer to self		*/
	LayerClass	layer_class;	/* pointer to ClassRec		*/
	Layer		parent;		/* parent Layer			*/
	NrmName		nrm_name;	/* Layer resource name quarkified*/
	Const char	*name;		/* Layer resource name		*/
} ObjLayerPart;


NhlDOCREF(/design/hlu/Base.html,Base Object Design)
NhlDOCTAG(BaseLayerPart)
typedef struct _BaseLayerPart {
	int		id;		/* index into global layer Table*/
	Layer		self;		/* pointer to self		*/
	LayerClass	layer_class;	/* pointer to ClassRec		*/
	Layer		parent;		/* parent Layer			*/
	NrmName		nrm_name;	/* Layer resource name quarkified*/
	Const char	*name;		/* Layer resource name		*/

/* NOTHING CAN BE ADDED BEFORE HERE UNLESS IT IS ALSO ADDED IN ObjLayerPart */

	NhlBoolean		in_init;	/* NhlCreateChild valid call */
	_NhlAllChildList	all_children;
	_NhlChildList		children;
	_NhlChildArgList	child_args;

	Layer			wkptr;
	/* import Values */
} BaseLayerPart;

typedef struct _ObjRec {
	ObjLayerPart	base;
} ObjLayerRec;

typedef struct _LayerRec {
	BaseLayerPart	base;
} BaseLayerRec, LayerRec;

typedef struct _NhlChildResRec _NhlChildResNode, *_NhlChildResList;

struct _NhlChildResRec {
	LayerClass		class;
	NhlBoolean		autosetval;
	NrmNameList		resources;
	_NhlChildResList	next;
};

typedef NhlErrorTypes (*NhlClassPartInitProc)(
#if	NhlNeedProto
	LayerClass	/* lc to initialize */
#endif
);

typedef NhlErrorTypes (*NhlClassInitProc)(
#if	NhlNeedProto
	void
#endif
);

typedef NhlErrorTypes (*NhlInitProc)(
#if	NhlNeedProto
	LayerClass,	/* class of instance to init	*/
	Layer,		/* request layer		*/
	Layer,		/* new layer			*/
	_NhlArgList,	/* args to set in layer		*/
	int		/* nargs			*/
#endif
);

typedef NhlErrorTypes (*NhlSetValuesProc)(
#if	NhlNeedProto
	Layer		old,		/* old		*/
	Layer		req,		/* requested	*/
	Layer		new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

typedef NhlErrorTypes (*NhlGetValuesProc)(
#if	NhlNeedProto
	Layer,		/* layer	*/
	_NhlArgList,	/* args to get	*/
	int		/* nargs	*/
#endif
);

typedef NhlErrorTypes (*NhlReparentProc)(
#if	NhlNeedProto
	void
#endif
);

typedef NhlErrorTypes (*NhlDrawProc)(
#if	NhlNeedProto
	Layer		/* layer to draw	*/
#endif
);

typedef NhlErrorTypes (*NhlDestroyProc)(
#if	NhlNeedProto
	Layer		/* layer to destroy	*/
#endif
);

typedef struct _ObjLayerClassPart {
	NhlString		class_name;
	NrmClass		nrm_class;
	unsigned int		layer_size;
	int			class_inited;
	LayerClass 		superclass;

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
} ObjLayerClassPart;

typedef struct _BaseLayerClassPart {
	NhlString		class_name;
	NrmClass		nrm_class;
	unsigned int		layer_size;
	int			class_inited;
	LayerClass 		superclass;

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
} BaseLayerClassPart;

typedef struct _ObjClassRec {
	ObjLayerClassPart	base_class;
} ObjLayerClassRec;

typedef struct _LayerClassRec {
	BaseLayerClassPart	base_class;
} BaseLayerClassRec, LayerClassRec ;

extern ObjLayerClassRec objLayerClassRec;
extern LayerClassRec layerClassRec;

#define baseLayerClassRec layerClassRec

#define _NhlName(instance) (((Layer)instance)->base.name)
#endif /* _NBaseP_h */	
