/*
 *      $Id: Base.c,v 1.5 1994-06-03 16:37:54 ethan Exp $
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
	NhlLayerClass
#endif
);

static NhlErrorTypes BaseLayerDestroy(
#if	NhlNeedProto
	NhlLayer
#endif
);

static NhlErrorTypes BaseLayerReparent(
#if	NhlNeedProto
	NhlLayer	instance,
	NhlLayer	new_parent
#endif
);

NhlObjLayerClassRec NhlobjLayerClassRec = {
	{
/* class_name			*/	"Obj",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)NULL,

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

NhlLayerClassRec NhllayerClassRec = {
	{
/* class_name			*/	"Base",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)NULL,

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

NhlLayerClass NhllayerClass = (NhlLayerClass)&NhllayerClassRec;
NhlLayerClass NhlbaseLayerClass = (NhlLayerClass)&NhllayerClassRec;
NhlLayerClass NhlobjLayerClass = (NhlLayerClass)&NhlobjLayerClassRec;

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
 *		NhlLayerClass	lc	pointer to class structure to update
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
	NhlLayerClass	lc	/* pointer to class structure to update	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* pointer to class structure to update	*/
#endif
{
	lc->base_class.nrm_class = NrmStringToName(lc->base_class.class_name);

	if(lc->base_class.resources)
		_NhlCompileResourceList(lc->base_class.resources,
					lc->base_class.num_resources);

	_NhlGroupResources(lc);
	return(NhlNOERROR);
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
	NhlErrorTypes ret=NhlNOERROR,lret=NhlNOERROR;

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
 *		NhlLayer	l
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
	NhlLayer	l	/* layer 	*/
)
#else
(l)
	NhlLayer	l;	/* layer 	*/
#endif
{
	_NhlAllChildList	tnode;
	NhlErrorTypes		ret = NhlNOERROR, lret = NhlNOERROR;

	tnode = l->base.all_children;

	while(tnode != (_NhlAllChildList)NULL){
		NhlLayer	child = NULL;

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
	NhlLayer	l,	/* layer to reparent	*/
	NhlLayer	parent	/* new parent		*/
)
#else
(l,parent)
	NhlLayer	l;	/* layer to reparent	*/
	NhlLayer	parent;	/* new parent		*/
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
 *		NhlLayer	l	layer to destroy
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
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlErrorTypes	ret=NhlNOERROR,lret=NhlNOERROR;
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
 *		NhlLayer	layer	Layer to get workstation pointer from
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlLayer
 * Side Effect:	
 */
NhlLayer
_NhlGetWorkstationLayer
#if	__STDC__
(
	NhlLayer	layer	/* Layer to get workstation pointer from*/
)
#else
(layer)
	NhlLayer layer;	/* NhlLayer to retrieve workstation pointer from	*/
#endif
{
	return(layer->base.wkptr);
}

int NhlGetParentWorkstation
#if	__STDC__
(int plotid)
#else
(plotid)
	int plotid;
#endif
{
	NhlLayer plot_ptr = _NhlGetLayer(plotid);

	if(plot_ptr == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NhlGetParentWorkstation: invalid plotid can't get workstation id");
		return(-1);
	}

	if(plot_ptr->base.wkptr !=NULL) {
		return(plot_ptr->base.wkptr->base.id);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NhlGetParentWorkstation: plot does not have parent workstation");
		return(-1);
	}
}
