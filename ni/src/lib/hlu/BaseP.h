/*
 *      $Id: BaseP.h,v 1.1 1993-04-30 17:21:15 boote Exp $
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

typedef struct _BaseLayerPart {
	/* User setable resource fields */

	/* Internal private fields */

	Layer			wkptr;
	_NhlAllChildList	all_children;

	_NhlChildList		children;
	_NhlChildArgList	child_args;

	/* export  Values */
	
	Layer		self;
	LayerClass	layer_class;	/* pointer to Widgets ClassRec	  */
	Layer		parent;		/* parent Layer			  */
	int		id;		/* index into global layer Table  */
	NrmName		nrm_name;	/* Layer resource name quarkified */
	Const char	*name;		/* Layer resource name		  */

	/* import Values */
} BaseLayerPart;

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

typedef struct _BaseLayerClassPart {
	LayerClass 		superclass;
	NhlString		class_name;
	NrmClass		nrm_class;
	int			layer_size;
	NhlResourceList		resources;
	int			num_resources;
	_NhlChildResList	child_resources;
	NrmNameList		all_resources;
	NhlClassPartInitProc	class_part_initialize;
	int			class_inited;
	NhlClassInitProc	class_initialize;
	NhlInitProc		layer_initialize;
	NhlSetValuesProc	layer_set_values;
	NhlErrorTypes		(*layer_set_values_not)();
	NhlGetValuesProc	layer_get_values;
	NhlDrawProc		layer_pre_draw;
	NhlDrawProc		layer_draw;
	NhlDrawProc		layer_draw_segonly;
	NhlDrawProc		layer_post_draw;
	NhlDrawProc		layer_clear;
	NhlDestroyProc		layer_destroy;
} BaseLayerClassPart;

typedef struct _LayerClassRec {
	BaseLayerClassPart	base_class;
} BaseLayerClassRec, LayerClassRec ;

extern LayerClassRec layerClassRec;

#define baseLayerClassRec layerClassRec

#endif /* _NBaseP_h */	
