/*
 *      $Id: Base.c,v 1.17 1996-10-10 17:57:54 boote Exp $
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
#include <limits.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/BaseP.h>

static _NhlRawObjCB bcallbacks[] = {
	{_NhlCBobjDestroy,NhlOffset(NhlBaseLayerRec,base.destroycb),0,NULL,NULL}
};
static _NhlRawObjCB ocallbacks[] = {
	{_NhlCBobjDestroy,NhlOffset(NhlObjLayerRec,base.destroycb),0,NULL,NULL}
};

static NhlResource bresources[] = {
	{_NhlNnclData,_NhlCnclData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.ncl_data),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_NORACCESS,NULL},
	{_NhlNguiData,_NhlCguiData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_NORACCESS,NULL},
};

static NhlResource oresources[] = {
	{_NhlNnclData,_NhlCnclData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlObjLayerRec,base.ncl_data),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_NORACCESS,NULL},
	{_NhlNguiData,_NhlCguiData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlObjLayerRec,base.gui_data),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_NORACCESS,NULL},
};

static NhlErrorTypes BaseClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes BaseClassPartInitialize(
#if	NhlNeedProto
	NhlClass
#endif
);

static NhlErrorTypes BaseInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes ObjLayerDestroy(
#if	NhlNeedProto
	NhlLayer
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

NhlObjClassRec NhlobjClassRec = {
	{
/* class_name			*/	"objClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)NULL,
/* cvt_table			*/	_NhlDefHashTable,

/* resources			*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,
/* callbacks			*/	ocallbacks,
/* num_callbacks		*/	NhlNumber(ocallbacks),

/* class_part_initialize	*/	BaseClassPartInitialize,
/* class_initialize		*/	BaseClassInitialize,
/* layer_initialize		*/	BaseInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ObjLayerDestroy
	}
};

NhlClassRec NhllayerClassRec = {
	{
/* class_name			*/	"baseClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)NULL,
/* cvt_table			*/	_NhlDefHashTable,

/* resources			*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,
/* callbacks			*/	bcallbacks,
/* num_callbacks		*/	NhlNumber(bcallbacks),

/* class_part_initialize	*/	BaseClassPartInitialize,
/* class_initialize		*/	BaseClassInitialize,
/* layer_initialize		*/	BaseInitialize,
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

NhlClass NhllayerClass = (NhlClass)&NhllayerClassRec;
NhlClass NhlbaseClass = (NhlClass)&NhllayerClassRec;
NhlClass NhlobjClass = (NhlClass)&NhlobjClassRec;

/*
 * Function:	BaseClassInitialize
 *
 * Description:	
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
BaseClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, lret = NhlNOERROR;


	_NhlEnumVals	objidvals[] = {
		{NhlNULLOBJID,	"nullobjid"}
	};
	NhlConvertArg	objidargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMIN)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlNULLOBJID)}
	};


        _NhlEnumVals   positionlist[] = {
        {NhlTOP,	"top"},
        {NhlBOTTOM,	"bottom"},
        {NhlRIGHT, 	"right"},
        {NhlLEFT,	"left"},
        {NhlCENTER,	"center"}
        };

	_NhlEnumVals justificationlist[] =  {
	{NhlTOPLEFT,		"topleft"},
	{NhlCENTERLEFT,		"centerleft"},
	{NhlBOTTOMLEFT,		"bottomleft"},
	{NhlTOPCENTER,		"topcenter"},
	{NhlCENTERCENTER,	"centercenter"},
	{NhlBOTTOMCENTER,	"bottomcenter"},
	{NhlTOPRIGHT,		"topright"},
	{NhlCENTERRIGHT,	"centerright"},
	{NhlBOTTOMRIGHT,	"bottomright"}
	};

	(void)_NhlRegisterEnumType(NhlbaseClass,NhlTObjId,objidvals,
							NhlNumber(objidvals));
	_NhlRegisterEnumType(NhlobjClass,NhlTPosition,positionlist,
			     NhlNumber(positionlist));
	_NhlRegisterEnumType(NhlobjClass,NhlTJustification,justificationlist,
			     NhlNumber(justificationlist));

	(void)NhlRegisterConverter(NhlbaseClass,NhlTScalar,NhlTObjId,
		_NhlCvtScalarToIndex,objidargs,NhlNumber(objidargs),False,NULL);
	(void)NhlRegisterConverter(NhlbaseClass,NhlTGenArray,NhlTObjIdGenArray,
		_NhlCvtGenArrayToIndexGenArray,objidargs,NhlNumber(objidargs),
		False,NULL);

	return MIN(ret,lret);
}

static void
CompileObjCallbackList
(
	_NhlRawObjCBList	cbls,
	int			num_cbls
)
{
	register int	i;
	register _NhlCookedObjCB	*cooked=(_NhlCookedObjCB *)&cbls[0];

	for(i=0;i<num_cbls;cbls++,cooked++,i++)
		cooked->cbquark = NrmPermStringToQuark(cbls->cbname);

	return;
}

static void
GroupObjCallbacks
(
	NhlClass	lc
)
{
	_NhlCookedObjCBList	local;
	_NhlCookedObjCBList	clist;
	int			num_clist,i,j,k;
	NhlClass		sc = lc->base_class.superclass;

	if(!sc || sc->base_class.num_callbacks < 1)
		return;

	if(lc->base_class.num_callbacks < 1){
		lc->base_class.callbacks = sc->base_class.callbacks;
		lc->base_class.num_callbacks = sc->base_class.num_callbacks;

		return;
	}

	num_clist = lc->base_class.num_callbacks + sc->base_class.num_callbacks;
	clist = (_NhlCookedObjCBList)NhlMalloc(
					sizeof(_NhlCookedObjCB)*num_clist);

	if(!clist){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	memcpy((char*)clist,(char*)sc->base_class.callbacks,
			sizeof(_NhlCookedObjCB)*sc->base_class.num_callbacks);

	local = (_NhlCookedObjCBList)lc->base_class.callbacks;
	k = sc->base_class.num_callbacks;

	for(i=0;i<lc->base_class.num_callbacks;i++){
		NhlBoolean	override;

		override = False;

		if(local[i].offset < sc->base_class.layer_size){

			for(j=0;j<sc->base_class.num_callbacks;j++){
				if(local[i].offset == clist[j].offset){
					/*
					 * Over-ride cb definition.
					 */
					clist[j] = local[i];
					num_clist--;
					override = True;
					break;
				}
			}
		}

		if(!override)
			clist[k++] = local[i];
	}

	lc->base_class.callbacks = (_NhlRawObjCBList)clist;
	lc->base_class.num_callbacks = num_clist;

	return;
}

/*
 * Function:	BaseClassPartInitialize
 *
 * Description:	This function is called to initialize the base_class
 *		part of every layer class record.  It basically initializes
 *		the resources list of each class to include the resources
 *		of it's super classes and also converts the entire list
 *		into a quarked list so strcmp's don't have to occur in
 *		the rest of the library.
 *
 * In Args:	
 *		NhlClass	lc	pointer to class structure to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
BaseClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class structure to update	*/
)
#else
(lc)
	NhlClass	lc;	/* pointer to class structure to update	*/
#endif
{
	lc->base_class.nrm_class = NrmStringToName(lc->base_class.class_name);

	if(lc->base_class.resources)
		_NhlCompileResourceList(lc->base_class.resources,
					lc->base_class.num_resources);

	_NhlGroupResources(lc);

	if(lc->base_class.callbacks)
		CompileObjCallbackList(lc->base_class.callbacks,
						lc->base_class.num_callbacks);
	GroupObjCallbacks(lc);

	return(NhlNOERROR);
}

/*
 * Function:	BaseInitialize
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
BaseInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	NhlClass		cc = new->base.layer_class;
	_NhlCookedObjCBList	cbs =
				(_NhlCookedObjCBList)cc->base_class.callbacks;
	int			ncbs = cc->base_class.num_callbacks;
	int			i;

	/*
	 * Initialize all object callbacks to NULL.
	 */
	for(i=0;i<ncbs;i++)
		*(_NhlCBList*)((char*)new + cbs[i].offset) = NULL;

	return NhlNOERROR;
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
 * Function:	ObjLayerDestroy
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
ObjLayerDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlErrorTypes		ret=NhlNOERROR,lret=NhlNOERROR;
	NhlClass		lc = l->base.layer_class;
	_NhlCookedObjCBList	cbs =
				(_NhlCookedObjCBList)lc->base_class.callbacks;
	int			ncbs = lc->base_class.num_callbacks;
	int			i;

	/*
	 * Destroy all object callbacks
	 */
	for(i=0;i<ncbs;i++)
		_NhlCBDestroy(*(_NhlCBList*)((char*)l + cbs[i].offset));

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
#if	NhlNeedProto
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
	 * Now do regular destroy stuff
	 */
	lret = ObjLayerDestroy(l);

	return MIN(ret,lret);
}
