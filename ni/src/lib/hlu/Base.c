/*
 *      $Id: Base.c,v 1.1 1993-04-30 17:21:08 boote Exp $
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

LayerClassRec layerClassRec = {
	{
/* superclass			*/	(LayerClass)NULL,
/* class_name			*/	"Base",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(LayerRec),
/* layer_resources		*/	NULL,
/* num_resources		*/	0,
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	BaseLayerClassPartInitialize,
/* class_inited			*/	False,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	BaseLayerDestroy
	}
};

LayerClass layerClass = (LayerClass)&layerClassRec;
LayerClass baseLayerClass = (LayerClass)&layerClassRec;

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
 * Returns:	void
 * Side Effect:	
 */
static void
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
	if(list == NULL)
		return;

	FreeAndDestroyChildList(list->next);
	(void)NhlDestroy(list->pid);
	(void)NhlFree(list);
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
	FreeAndDestroyChildList(l->base.children);

	return NOERROR;
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
