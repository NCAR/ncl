/*
 *      $Id: Base.c,v 1.29 1998-11-06 22:16:02 dbrown Exp $
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
#include <ncarg/hlu/AppI.h>

static _NhlRawObjCB bcallbacks[] = {
	{_NhlCBobjDestroy,NhlOffset(NhlBaseLayerRec,base.destroycb),
		 0,NULL,NULL,NULL},
	{_NhlCBobjValueSet,NhlOffset(NhlBaseLayerRec,base.resvaluesetcb),
		 1,NULL,NULL,_NhlResValueSetCBTask},
	{_NhlCBobjChildChange,NhlOffset(NhlBaseLayerRec,base.cchildcb),
		 0,NULL,NULL,NULL},
};
static _NhlRawObjCB ocallbacks[] = {
	{_NhlCBobjDestroy,NhlOffset(NhlBaseLayerRec,base.destroycb),
		 0,NULL,NULL,NULL},
	{_NhlCBobjValueSet,NhlOffset(NhlBaseLayerRec,base.resvaluesetcb),
		 0,NULL,NULL,_NhlResValueSetCBTask},
	{_NhlCBobjChildChange,NhlOffset(NhlBaseLayerRec,base.cchildcb),
		 0,NULL,NULL,NULL},
};

static NhlResource bresources[] = {
	{NhlNobjAppObj,NhlCobjAppObj,NhlTInteger,sizeof(int),
		NhlOffset(NhlBaseLayerRec,base.appid),
		NhlTImmediate,_NhlUSET(NhlDEFAULT_APP),
         	_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNnclData,_NhlCnclData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.ncl_data),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
	{_NhlNguiData,_NhlCguiData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
	{_NhlNguiData2,_NhlCguiData2,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data2),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
};

static NhlResource oresources[] = {
	{NhlNobjAppObj,NhlCobjAppObj,NhlTInteger,sizeof(int),
		NhlOffset(NhlBaseLayerRec,base.appid),
		NhlTImmediate,_NhlUSET(NhlDEFAULT_APP),
         	_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNnclData,_NhlCnclData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.ncl_data),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
	{_NhlNguiData,_NhlCguiData,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL},
	{_NhlNguiData2,_NhlCguiData2,NhlTPointer,sizeof(NhlPointer),
		NhlOffset(NhlBaseLayerRec,base.gui_data2),
		NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_NORACCESS|_NhlRES_PRIVATE,NULL}
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

/* resources			*/	oresources,
/* num_resources		*/	NhlNumber(oresources),
/* all_resources		*/	NULL,
/* callbacks			*/	ocallbacks,
/* num_callbacks		*/	NhlNumber(ocallbacks),
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

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

/* resources			*/	bresources,
/* num_resources		*/	NhlNumber(bresources),
/* all_resources		*/	NULL,
/* callbacks			*/	bcallbacks,
/* num_callbacks		*/	NhlNumber(bcallbacks),
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

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
		{NhlNULLOBJID,	"NullObjId"}
	};
	NhlConvertArg	objidargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMIN)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlNULLOBJID)}
	};


        _NhlEnumVals   positionlist[] = {
        {NhlTOP,	"Top"},
        {NhlBOTTOM,	"Bottom"},
        {NhlRIGHT, 	"Right"},
        {NhlLEFT,	"Left"},
        {NhlCENTER,	"Center"}
        };

	_NhlEnumVals justificationlist[] =  {
	{NhlTOPLEFT,		"TopLeft"},
	{NhlCENTERLEFT,		"CenterLeft"},
	{NhlBOTTOMLEFT,		"BottomLeft"},
	{NhlTOPCENTER,		"TopCenter"},
	{NhlCENTERCENTER,	"CenterCenter"},
	{NhlBOTTOMCENTER,	"BottomCenter"},
	{NhlTOPRIGHT,		"TopRight"},
	{NhlCENTERRIGHT,	"CenterRight"},
	{NhlBOTTOMRIGHT,	"BottomRight"}
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

static void
CompileClassCallbackList
(
	_NhlRawClassCBList	cbls,
	int			num_cbls
)
{
	register int	i;
	register _NhlCookedClassCB	*cooked=(_NhlCookedClassCB *)&cbls[0];

	for(i=0;i<num_cbls;cbls++,cooked++,i++){
		cooked->cbquark = NrmPermStringToQuark(cbls->cbname);
		cooked->cblist = NULL;
	}

	return;
}

static void
GroupClassCallbacks
(
	NhlClass	lc
)
{
	_NhlCookedClassCBList	local;
	_NhlCookedClassCBList	clist;
	int			num_clist,i,j,k;
	NhlClass		sc = lc->base_class.superclass;

	if(!sc || sc->base_class.num_class_callbacks < 1)
		return;

	num_clist = lc->base_class.num_class_callbacks +
					sc->base_class.num_class_callbacks;
	clist = (_NhlCookedClassCBList)NhlMalloc(
					sizeof(_NhlCookedClassCB)*num_clist);

	if(!clist){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	memcpy((char*)clist,(char*)sc->base_class.class_callbacks,
				sizeof(_NhlCookedClassCB) *
					sc->base_class.num_class_callbacks);

	local = (_NhlCookedClassCBList)lc->base_class.class_callbacks;
	k = sc->base_class.num_class_callbacks;

	for(i=0;i<lc->base_class.num_class_callbacks;i++){

		for(j=0;j<sc->base_class.num_class_callbacks;j++){
			if(local[i].cbquark == clist[j].cbquark){
				/*
				 * Over-ride cb definition.
				 */
				clist[j] = local[i];
				num_clist--;
				goto OVERRIDE;
			}
		}

		clist[k++] = local[i];
OVERRIDE:
		;
	}

	for(i=0;i<num_clist;i++)
		clist[i].cblist = NULL;
	lc->base_class.class_callbacks = (_NhlRawClassCBList)clist;
	lc->base_class.num_class_callbacks = num_clist;

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
		_NhlCompileResourceList(lc,lc->base_class.resources,
					lc->base_class.num_resources);

	_NhlGroupResources(lc);

	if(lc->base_class.callbacks)
		CompileObjCallbackList(lc->base_class.callbacks,
						lc->base_class.num_callbacks);
	GroupObjCallbacks(lc);

	if(lc->base_class.class_callbacks)
		CompileClassCallbackList(lc->base_class.class_callbacks,
					lc->base_class.num_class_callbacks);
	GroupClassCallbacks(lc);

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

	/*
	 * _NhlNguiData should be inherited...
	 */
	if(new->base.parent && !_NhlArgIsSet(args,nargs,_NhlNguiData))
		new->base.gui_data = new->base.parent->base.gui_data;

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
 *		with the current parent - but this allows the Reparent
 *		method to be called for every child and sub-child in
 *		the hierarchy.
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

		lret = _NhlReparent(_NhlGetLayer(tnode->pid),l);

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
	NhlLayer	p	/* new parent		*/
)
#else
(l,p)
	NhlLayer	l;	/* layer to reparent	*/
	NhlLayer	p;	/* new parent		*/
#endif
{
	if(_NhlIsWorkstation(p))
		l->base.wkptr = p;
	else
		l->base.wkptr = p->base.wkptr;

	if(!l->base.app_destroy){
		l->base.appobj = p->base.appobj;
		if(p->base.app_destroy){
			NhlArgVal	dummy,udata;

			NhlINITVAR(dummy);
			NhlINITVAR(udata);
			dummy.lngval = 0;
			udata.ptrval = l;
			l->base.app_destroy = _NhlAddObjCallback(
					l->base.appobj,_NhlCBobjDestroy,dummy,
					_NhlBaseAppDestroyCB,udata);
		}
	}

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

	_NhlCBDelete(l->base.app_destroy);

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

/*
 * Function:	AppDestroyCB
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
void
_NhlBaseAppDestroyCB
#if	NhlNeedProto
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
#else
(cbdata,udata)
	NhlArgVal	cbdata;
	NhlArgVal	udata;
#endif
{
	NhlLayer	app = (NhlLayer)cbdata.ptrval;
	NhlLayer	l = (NhlLayer)udata.ptrval;
	NhlLayer	p = l->base.parent;

	_NhlCBDelete(l->base.app_destroy);
	l->base.app_destroy = NULL;

	while(p && p->base.appobj == app)
		p = p->base.parent;

	if(p){
		l->base.appobj = p->base.appobj;
		if(p->base.app_destroy){
			NhlArgVal	dummy,udata;

			NhlINITVAR(dummy);
			NhlINITVAR(udata);
			dummy.lngval = 0;
			udata.ptrval = l;
			l->base.app_destroy = _NhlAddObjCallback(
					l->base.appobj,_NhlCBobjDestroy,dummy,
					_NhlBaseAppDestroyCB,udata);
		}
	}
	else{
		l->base.appid = _NhlGetDefaultApp();
		l->base.appobj = _NhlGetLayer(l->base.appid);
	}

	return;
}

NhlBoolean
_NhlBaseAddChild
#if	NhlNeedProto
(
	NhlLayer	parent,
	int		child
)
#else
(parent,child)
	NhlLayer	parent;
	int		child;
#endif
{
	_NhlAllChildList	node,*nptr;
	_NhlobjChangeChildRec	cc;
	NhlArgVal		cbdata;
	NhlArgVal		sel;

	if(!parent)
		return True;

 
	node = (_NhlAllChildList)NhlMalloc(sizeof(_NhlAllChildNode));

	if(!node){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return False;
	}

	node->pid = child;
	node->next = NULL;
	for (nptr = &parent->base.all_children; *nptr; nptr = &(*nptr)->next)
		;
	*nptr = node;

	NhlINITVAR(cbdata);
	NhlINITVAR(sel);
	cc.reason = _NhlobjCCAdd;
	cc.new = parent->base.id;
	cc.child = child;
	cbdata.ptrval = &cc;
	sel.lngval = 0; /* ignored */
	_NhlCallObjCallbacks(parent,_NhlCBobjChildChange,sel,cbdata);

	return True;
}

void
_NhlBaseRemoveChild
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	_NhlAllChildList	*nptr;
	_NhlAllChildList	node = NULL;
	NhlBoolean		found = False;
	_NhlobjChangeChildRec	cc;
	NhlArgVal		cbdata;
	NhlArgVal		sel;

	if(!l->base.parent)
		return;

	nptr = &l->base.parent->base.all_children;

	while(*nptr){
		if((*nptr)->pid == l->base.id){
			found = True;
			node = *nptr;
			*nptr = (*nptr)->next;
			(void)NhlFree(node);
			break;
		}
		nptr = &(*nptr)->next;
	}

	if(!found){
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"Unable to remove PID#%d from Parent's list",
								l->base.id));
		return;
	}

        if (! l->base.parent->base.being_destroyed) {
                NhlINITVAR(cbdata);
                NhlINITVAR(sel);
                cc.reason = _NhlobjCCRemove;
                cc.old = l->base.parent->base.id;
                cc.child = l->base.id;
                cbdata.ptrval = &cc;
                sel.lngval = 0; /* ignored */
                _NhlCallObjCallbacks
                        (l->base.parent,_NhlCBobjChildChange,sel,cbdata);
        }

	return;
}

NhlBoolean
_NhlBaseMoveChild
#if	NhlNeedProto
(
	NhlLayer	parent,
	NhlLayer	child
)
#else
(parent,child)
	NhlLayer	parent;
	NhlLayer	child;
#endif
{
	_NhlAllChildList	*nptr,node=NULL;
	_NhlobjChangeChildRec	cc;
	NhlArgVal		cbdata;
	NhlArgVal		sel;
	NhlLayer		oldp = child->base.parent;

	/*
	 * It is not this child's immediate parent that is changing - 
	 * the change is higher up the instance hierarchy.
	 */
	if(parent == child->base.parent)
		return True;

	nptr = &child->base.parent->base.all_children;

	/*
	 * remove child node from parent's list.
	 */
	while(*nptr){
		if((*nptr)->pid == child->base.id){
			node = *nptr;
			*nptr = (*nptr)->next;
			break;
		}
		nptr = &(*nptr)->next;
	}

	if(!node){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"Unable to remove PID#%d from parent",child->base.id));
		return False;
	}

	/*
	 * add child node into new parent's list.
	 */

	node->next = NULL;
	for (nptr = &parent->base.all_children; *nptr; nptr = &(*nptr)->next)
		;
	*nptr = node;

	/*
	 * Change child's parent pointer
	 */
	child->base.parent = parent;

	NhlINITVAR(cbdata);
	NhlINITVAR(sel);
	cc.reason = _NhlobjCCMove;
	cc.new = parent->base.id;
	cc.old = oldp->base.id;
	cc.child = child->base.id;
	cbdata.ptrval = &cc;
	sel.lngval = 0; /* ignored */
	_NhlCallObjCallbacks(oldp,_NhlCBobjChildChange,sel,cbdata);
	_NhlCallObjCallbacks(parent,_NhlCBobjChildChange,sel,cbdata);

	return True;
}
