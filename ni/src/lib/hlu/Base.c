/*
 *      $Id: Base.c,v 1.3 1994-01-10 19:48:25 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Base.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:30:30 MDT 1992
 *
 *	Description:	This file contains all the functions and definitions
 *			neccessary to create an instance of the Base class
 *			layer.
 */
#include <stdio.h>

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>

static NhlErrorTypes BaseLayerClassPartInitialize(
#if	NhlNeedProto
	LayerClass
#endif
);

static NhlErrorTypes BaseLayerDestroy(
#if	NhlNeedProto
	Layer
#endif
);

static NhlErrorTypes BaseLayerReparent(
#if	NhlNeedProto
	Layer	instance,
	Layer	new_parent
#endif
);

ObjLayerClassRec objLayerClassRec = {
	{
/* class_name			*/	"Obj",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(ObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)NULL,

/* resources			*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,

/* class_part_initialize	*/	BaseLayerClassPartInitialize,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	}
};

LayerClassRec layerClassRec = {
	{
/* class_name			*/	"Base",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(LayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)NULL,

/* resources			*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,

/* class_part_initialize	*/	BaseLayerClassPartInitialize,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	BaseLayerReparent,
/* layer_destroy		*/	BaseLayerDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	}
};

LayerClass layerClass = (LayerClass)&layerClassRec;
LayerClass baseLayerClass = (LayerClass)&layerClassRec;
LayerClass objLayerClass = (LayerClass)&objLayerClassRec;

/*
 * Function:	BaseLayerClassPartInitialize
 *
 * Description:	This function is called to initialize the base_class
 *		part of every layer class record.  It basically initializes
 *		the resources list of each class to include the resources
 *		of it's super classes and also converts the entire list
 *		into a quarked list so strcmp's don't have to occur in
 *		the rest of the library.
 *
 * In Args:	
 *		LayerClass	lc	pointer to class structure to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
BaseLayerClassPartInitialize
#if	__STDC__
(
	LayerClass	lc	/* pointer to class structure to update	*/
)
#else
(lc)
	LayerClass	lc;	/* pointer to class structure to update	*/
#endif
{
	lc->base_class.nrm_class = NrmStringToName(lc->base_class.class_name);

	if(lc->base_class.resources)
		_NhlCompileResourceList(lc->base_class.resources,
					lc->base_class.num_resources);

	_NhlGroupResources(lc);
	return(NOERROR);
}

/*
 * Function:	FreeAndDestroyChildList
 *
 * Description:	This function is used to recursively free the children
 *		list passed to it.  Including destroying the children
 *		associated with the pid part of each node.
 *
 * In Args:	
 *		_NhlChildList	list	list to free
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
FreeAndDestroyChildList
#if	__STDC__
(
	_NhlChildList	list	/* list to free	*/
)
#else
(list)
	_NhlChildList	list;	/* list to free	*/
#endif
{
	NhlErrorTypes ret=NOERROR,lret=NOERROR;

	if(list == NULL)
		return ret;

	ret = FreeAndDestroyChildList(list->next);
	lret = NhlDestroy(list->pid);
	(void)NhlFree(list);

	return MIN(ret,lret);
}

/*
 * Function:	ReparentChildren
 *
 * Description:	This function parses all the children of the given object
 *		and notifies them that there parent has changed.  There
 *		direct parent didn't change so the parent arg is passed
 *		with the current parent - but this allows there Reparent
 *		method to be called.
 *
 * In Args:	
 *		Layer	l
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
ReparentChildren
#if	__STDC__
(
	Layer	l	/* layer 	*/
)
#else
(l)
	Layer	l;	/* layer 	*/
#endif
{
	_NhlAllChildList	tnode;
	NhlErrorTypes		ret = NOERROR, lret = NOERROR;

	tnode = l->base.all_children;

	while(tnode != (_NhlAllChildList)NULL){
		Layer	child = NULL;

		child = _NhlGetLayer(tnode->pid);
		lret = _NhlReparent(child,l);

		ret = MIN(lret,ret);

		tnode = tnode->next;
	}

	return ret;
}

/*
 * Function:	BaseLayerReparent
 *
 * Description:	This function is used to reset the Workstation pointer in
 *		this object and in this objects children.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
BaseLayerReparent
#if	__STDC__
(
	Layer	l,	/* layer to reparent	*/
	Layer	parent	/* new parent		*/
)
#else
(l,parent)
	Layer	l;	/* layer to reparent	*/
	Layer	parent;	/* new parent		*/
#endif
{
	if(_NhlIsWorkstation(parent))
		l->base.wkptr = parent;
	else
		l->base.wkptr = parent->base.wkptr;

	return ReparentChildren(l);
}

/*
 * Function:	BaseLayerDestroy
 *
 * Description:	This function is used to clean up any memory that has
 *		been allocated in the base part of the layer. It is called
 *		from the NhlDestroy method.
 *
 * In Args:	
 *		Layer	l	layer to destroy
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
BaseLayerDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
{
	NhlErrorTypes	ret=NOERROR,lret=NOERROR;
	/*
	 * First free register children left behind by sub-classes
	 */
	ret = FreeAndDestroyChildList(l->base.children);

	/*
	 * Now free any children that are still around
	 */
	while(l->base.all_children != NULL){
		lret = NhlDestroy(l->base.all_children->pid);
		ret = MIN(ret,lret);
	}

	return ret;
}

/*
 * Function:	_NhlGetWorkstationLayer
 *
 * Description:	This function is used to retrieve the Workstation pointer
 *		from a layer object.
 *
 * In Args:	
 *		Layer	layer	Layer to retrieve workstation pointer from
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	Layer
 * Side Effect:	
 */
Layer
_NhlGetWorkstationLayer
#if	__STDC__
(
	Layer	layer	/* Layer to retrieve workstation pointer from	*/
)
#else
(layer)
	Layer layer;	/* Layer to retrieve workstation pointer from	*/
#endif
{
	return(layer->base.wkptr);
}
