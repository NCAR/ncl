/*
 *      $Id: Create.c,v 1.6 1994-02-18 02:54:03 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Create.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:32:38 MDT 1992
 *
 *	Description:	This file contains all the functions and definitions
 *			neccessary to create layer instances and to initialize
 *			layer Classes. The Design documentation is
 *			NhlDOCREF(/design/hlu/Create.html,here).
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TransObj.h>
#include <ncarg/hlu/Error.h>
#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/DataMgr.h>
#include <ncarg/hlu/DataItem.h>

/*
 * Function:	CallClassPartInit
 *
 * Description:	This function is used to call the top to bottom ClassPartInit
 *		chained method.
 *
 * In Args:	NhlLayerClass	ancestor:
 *		NhlLayerClass	lc;
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	
 * Side Effect:	The lc structure has been updated by each of it's ansestors
 *		ClassPartInit functions.
 */
static NhlErrorTypes
CallClassPartInit
#if	__STDC__
(
	NhlLayerClass	ancestor,	/* ancestor		*/
	NhlLayerClass	lc		/* class to update	*/
)
#else
(ancestor, lc)
	NhlLayerClass	ancestor;	/* ancestor		*/
	NhlLayerClass	lc;		/* class to update	*/
#endif
{
	NhlErrorTypes ancestorerr = NhlNOERROR, thisclass = NhlNOERROR;

	if(ancestor->base_class.superclass != NULL) {
		ancestorerr =
			CallClassPartInit(ancestor->base_class.superclass,lc);

		if(ancestorerr < NhlWARNING)
			return(ancestorerr);
	}

	if(ancestor->base_class.class_part_initialize != NULL )
		thisclass = (*(ancestor->base_class.class_part_initialize))(lc);

	return(MIN(ancestorerr,thisclass));
}

/*
 * Function:	InitializeLayerClass
 *
 * Description:	This function is used to Initialize the Class structure
 *		of the given class.  It calls the Initialize function
 *		within the given class to do this.  Also, It initializes
 *		the given Classes superclass before doing itself.
 *
 * In Args:	NhlLayerClass	lc;	Class to initialize
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	The structure pointed to by lc is initialized.
 */
static NhlErrorTypes
InitializeLayerClass
#if	__STDC__
(
	NhlLayerClass	lc	/* pointer to class to be initalized	*/
) 
#else
(lc) 
	NhlLayerClass	lc;	/* pointer to class to be initalized	*/
#endif
{
	NhlErrorTypes ansestor, thisclass, classpart;
	NhlLayerClass step;
	int inited = 0x01;

	if(lc->base_class.class_inited) return(NhlNOERROR);

	step = lc;
	while(step != NULL) {
		if(step == NhlobjLayerClass) {
			inited |= (_NhlObjLayerClassFlag);
			break;
		}
		else if(step == NhlbaseLayerClass) {
			inited |= ( _NhlBaseLayerClassFlag );  
			break;
		}
		else if(step == NhlworkstationLayerClass)
			inited |= (_NhlWorkstationLayerClassFlag);  
		else if(step == NhlviewLayerClass)
			inited |= (_NhlViewLayerClassFlag);
		else if(step == NhltransformLayerClass)
			inited |= (_NhlTransformLayerClassFlag);
		else if(step == NhlerrorLayerClass)
			inited |= (_NhlErrorLayerClassFlag);
		else if(step == NhltransObjLayerClass)
			inited |= (_NhlTransObjLayerClassFlag);
		else if(step == NhldataCommLayerClass)
			inited |= (_NhlDataCommLayerClassFlag);
		else if(step == NhldataMgrLayerClass)
			inited |= (_NhlDataMgrLayerClassFlag);
		else if(step == NhldataItemLayerClass)
			inited |= (_NhlDataItemLayerClassFlag);
		else if(step == NhldataSpecLayerClass)
			inited |= (_NhlDataSpecLayerClassFlag);
		step = step->base_class.superclass;
	}

	if((lc->base_class.superclass != NULL)
		&&(!(lc->base_class.superclass->base_class.class_inited))){

		ansestor = InitializeLayerClass(lc->base_class.superclass);
		if(ansestor < NhlWARNING)
			return(ansestor);
	}
	else
		ansestor = NhlNOERROR;

	if(lc->base_class.class_initialize != NULL){
		thisclass = (*(lc->base_class.class_initialize))();
		if(thisclass < NhlWARNING)
			return(thisclass);
	}
	else
		thisclass = NhlNOERROR;

	classpart = CallClassPartInit(lc,lc);
	if(classpart < NhlWARNING)
		return(classpart);

	lc->base_class.class_inited = inited;

	return(MIN((MIN(ansestor,thisclass)),classpart));
}

/*
 * Function:	_NhlInitializeLayerClass
 *
 * Description:	Global function to init a layer class. This is needed if
 *		The class you are writing depends upon a type defined
 *		in another class.  The converters for the type need to
 *		be installed.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlDOCTAG(_NhlInitializeLayerClass)
NhlErrorTypes
_NhlInitializeLayerClass
#if	__STDC__
(
	NhlLayerClass	lc	/* pointer to class to be initalized	*/
) 
#else
(lc) 
	NhlLayerClass	lc;	/* pointer to class to be initalized	*/
#endif
{
	return(InitializeLayerClass(lc));
}

/*
 * Function:	CallInitialize
 *
 * Description:	This function is used to initialize an instance of the given
 *		NhlLayer Class.  It calls the top to bottom initialize method
 *		of the classes.
 *
 * In Args:	NhlLayerClass	lc	Class of NhlLayer being initialized
 *		NhlLayer	req	NhlLayer with requested values in it
 *		_NhlArgList	args	res names and values requested
 *		int		nargs	number of args
 *
 * Out Args:	NhlLayer	new	NhlLayer to update
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallInitialize
#if	__STDC__
(
	NhlLayerClass	lc,	/* Class of NhlLayer being initialized	*/
	NhlLayer	req,	/* NhlLayer with requested values in it	*/
	NhlLayer	new,	/* NhlLayer to update			*/
	_NhlArgList	args,	/* res names and values requested	*/
	int		nargs	/* number of args			*/
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass	lc;	/* Class of NhlLayer being initialized	*/
	NhlLayer	req;	/* NhlLayer with requested values in it	*/
	NhlLayer	new;	/* NhlLayer to update			*/
	_NhlArgList	args;	/* res names and values requested	*/
	int		nargs;	/* number of args			*/
#endif
{
	NhlErrorTypes ancestor=NhlNOERROR, class=NhlNOERROR;

	if(lc->base_class.superclass != NULL){
		ancestor = CallInitialize(lc->base_class.superclass,req,new,
								args,nargs);
		if(ancestor < NhlWARNING)
			return(ancestor);
	}

	if(lc->base_class.layer_initialize != NULL )
		class = (*(lc->base_class.layer_initialize))(lc,req,new,args,
									nargs);
	return(MIN(ancestor,class));

}

/*
 * Function:	_NhlCreate
 *
 * Description:	This function is the actual create function, it is called
 *		by the public varargs NhlCreate function after it has
 *		packed the varargs into arglists and by the public AL
 *		NhlALCreate function after it has packed it's SArg's
 *		into the arglists.
 *
 * In Args:	Const NhlString	name		name to identify instance
 *		NhlLayerClass	lc		class of instance to create
 *		int		parentid	id# of parent
 *		NhlErrorTypes	*ret		return code
 *		_NhlArgList	args		resources to set in instance
 *		int		nargs		number of args
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:
 */
NhlDOCTAG(_NhlCreate)
static NhlErrorTypes
_NhlCreate
#if	__STDC__
(
	int		*pid,		/* return plot id		*/
	Const char	*name,		/* name to identify instance	*/
	NhlLayerClass	lc,		/* class of instance to create	*/
	int		parentid,	/* id# of parent		*/
	_NhlArgList	args,		/* resources to set in instance	*/
	int		nargs,		/* number of args		*/
	NrmQuarkList	child		/* used to create managed child	*/
)
#else
(pid, name, lc, parentid, args, nargs, child)
	int		*pid;		/* return plot id		*/
	Const char	*name;		/* name to identify instance	*/
	NhlLayerClass	lc;		/* class of instance to create	*/
	int		parentid;	/* id# of parent		*/
	_NhlArgList	args;		/* resources to set in instance	*/
	int		nargs;		/* number of args		*/
	NrmQuarkList	child;		/* used to create managed child	*/
#endif
{
	NhlLayer		parent = _NhlGetLayer(parentid);
	NhlLayer		layer;
	NhlLayer		request;
	NhlErrorTypes		ret=NhlNOERROR, lret=NhlNOERROR;
	_NhlArg			stackargs[_NhlMAXARGLIST];
	_NhlArgList		largs=stackargs;
	int			nlargs;
	_NhlChildArgList	chld_args=NULL;
	NhlBoolean		chld_args_used[_NhlMAXARGLIST];
	_NhlConvertContext	context=NULL;

	if((parent != NULL) && _NhlIsObj(parent)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"NhlObjLayer sub-classes cannot have children");
		return NhlFATAL;
	}

	if(!(lc->base_class.class_inited)){
		ret = InitializeLayerClass(lc);
		if(ret < NhlWARNING)
			return(ret);
	}

	layer = (NhlLayer)NhlMalloc((unsigned)(lc->base_class.layer_size));

	if(layer == (NhlLayer)NULL){
		NhlPError(NhlFATAL,ENOMEM,"Unable to Create");
		return(NhlFATAL);
	}

/*
 * Set things that are identical in Obj's and NhlLayer's.
 */
	layer->base.self = layer;
	layer->base.parent = parent;
	layer->base.layer_class = lc;
	layer->base.nrm_name = NrmStringToName((name != NULL) ? name : "");
	layer->base.name = (Const NhlString)
					NrmNameToString(layer->base.nrm_name);

/*
 * context is a structure that remembers the memory that is allocated
 * by any converters on behalf of this object.  It needs to be free'd -
 * along with all that memory after the Initailize methode has had a chance
 * to copy the memory.
 */
	context = _NhlCreateConvertContext();
	if(context == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to create Convert Context - Can't Create");
		(void)NhlFree(layer);
		return NhlFATAL;
	}

/*
 * Check for valid parent for Obj sub-classes
 */
	if(_NhlIsObj(layer)){
		if(_NhlIsError(layer)){
			if(parent != NULL){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
"Error objects should not have a parent - Resetting parent to NOPARENT");
				layer->base.parent = NULL;
			}
		}
		else if(_NhlIsDataMgr(layer)){
			if((parent == NULL) || !_NhlIsDataItem(parent)){
				NhlPError(NhlFATAL,NhlEUNKNOWN,"DataMgr objects can only be created as a child of a DataItem Class");
				(void)NhlFree(layer);
				return(NhlFATAL);
			}
		}
	/*
	 * Resource forwarding is not supported by Obj sub-classes
	 * so need to set largs to args.
	 */
	largs = args;
	nlargs = nargs;
	}

/*
 * It is a NhlLayer - not an Obj so set the additional fields
 */
	else {

		layer->base.in_init = True;
		layer->base.all_children = NULL;
		layer->base.children = NULL;

/*
 * Set Workstation Pointer in NhlLayer
 *
 * All layer types that should not have parents should be handled on a case
 * by case bases first, and then the default case of getting the workstation
 * from the parent should be done.
 */
		if( _NhlIsWorkstation(layer)){
			layer->base.wkptr = layer;
			if(parent != NULL){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
"Workstation objects should not have a parent - Resetting parent to NOPARENT");
				layer->base.parent = NULL;
				layer->base.wkptr = NULL;
			}
		}
		else if(_NhlIsDataItem(layer)){
			if(parent != NULL){
				NhlPError(NhlWARNING,NhlEUNKNOWN,"Data objects should not have a parent - Resetting parent to NOPARENT");
				layer->base.parent = NULL;
			}
		}
		else if(parent != NULL) {
			layer->base.wkptr = _NhlGetWorkstationLayer(parent);
		}
		else{
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"This NhlLayer must have a Parent");
			(void)NhlFree(layer);
			return(NhlFATAL);
		}

/*
 * _NhlSortChildArgs sorts the args into seperate arg lists for the parent
 * and for the children.  The children args are returned as a linked list
 * that get's assigned into child_args of the instance during init and
 * then get's free'd soon after, that way they are available for any
 * _NhlCreateChild calls that happen during initialize.
 * Resource forwarding is not supported for Obj's.
 */
		lret = _NhlSortChildArgs(layer,args,nargs,&largs,&nlargs,
					&chld_args,chld_args_used,False);
		if(lret < NhlWARNING){
			NhlPError(lret,NhlEUNKNOWN,
				"Unable to Create Arg Lists - Can't Create");
			(void)NhlFree(layer);
			return lret;
		}
		ret = MIN(lret,ret);

		layer->base.child_args = chld_args;
	}

/*
 * Gets Resources from args and default files and sets them in layer
 */	
	lret = _NhlGetResources(context,layer,largs,nlargs,child);
	if(lret < NhlWARNING){
		NhlPError(lret,NhlEUNKNOWN,
				"Unable to retrieve resources-Can't Create");
		_NhlFreeChildArgs(chld_args);
		_NhlFreeConvertContext(context);
		(void)NhlFree(layer);
		return lret;
	}
	ret = MIN(lret,ret);
	
	request = (NhlLayer)NhlMalloc((unsigned)(lc->base_class.layer_size));
	memcpy((char*)request,(char*)layer,lc->base_class.layer_size);

/*
 * If AddNhlLayer can't add it, it will return an ErrorCode - less than 0
 */
	*pid = _NhlAddLayer(layer);
	if(*pid < 0){
		lret = (NhlErrorTypes)*pid;
		if(lret < NhlWARNING){
			NhlPError(lret,NhlEUNKNOWN,"Unable to add layer to internal table-Can't Create");
			_NhlFreeChildArgs(chld_args);
			_NhlFreeConvertContext(context);
			(void)NhlFree(layer);
			(void)NhlFree(request);
			return(lret);
		}
		ret = MIN(lret,ret);
	}

	lret = CallInitialize(lc,request,layer,largs,nlargs); 
	if(lret < NhlWARNING){
		NhlPError(lret,NhlEUNKNOWN,
				"Unable to initialize layer-Can't Create");
		_NhlRemoveLayer(layer);
		_NhlFreeConvertContext(context);
		_NhlFreeChildArgs(chld_args);
		(void)NhlFree(layer);
		(void)NhlFree(request);
		*pid = lret;
		return(lret);
	}
	ret = MIN(ret,lret);

	/*
	 * Free the child_args and context fields of layer and set them
	 * to NULL.  They were only needed during Initialize.
	 */
	if(!_NhlIsObj(layer)){
		int i;
		layer->base.in_init = False;
		_NhlFreeChildArgs(chld_args);
		layer->base.child_args = NULL;
		for(i=0;i<nargs;i++){
			if(!chld_args_used[i]){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s is not a valid resource in %s at this time",
						NrmNameToString(args[i].quark),
							_NhlName(layer));
				ret = MIN(ret,NhlWARNING);
			}
		}
	}

	_NhlFreeConvertContext(context);

	if(_NhlIsWorkstation(layer)) {
		lret = _NhlOpenWorkstation(layer);
		ret = MIN(ret,lret);
		if(ret < NhlWARNING) {
			NhlPError(ret,NhlEUNKNOWN,
				"Unable to open Workstation-Can't Create");
			_NhlRemoveLayer(layer);
			(void)NhlFree(layer); 
			(void)NhlFree(request); 
			*pid = ret; 
			return(ret);
		}
	}

	/*
	 * Add this layer to it's parents all_children list, if it has
	 * a parent.
	 */
	if(parent != NULL){
		_NhlAllChildList	tcnode;

		tcnode = (_NhlAllChildList)NhlMalloc(sizeof(_NhlAllChildNode));

		tcnode->pid = *pid;
		tcnode->next = parent->base.all_children;
		parent->base.all_children = tcnode;
	}

	(void)NhlFree(request);

	return(ret);
}

/*
 * Function:	NhlVACreate
 *
 * Description:	This function is used to create an instance of the given
 *		NhlLayerClass.  It takes it's varargs and packs them into
 *		the internal arglist format needed - and then calls
 *		_NhlCreate. NhlDOCREF(#_NhlCreate,_NhlCreate is here.)
 *
 * In Args:	Const NhlString	name;		name of instance
 *		NhlLayerClass	lc;		NhlLayerClass to create
 *		int		parentid;	id of parent
 *		...				resource name/value pairs
 *
 * Out Args:	int		*pid;		return plot id
 *
 * Scope:	Global Public
 * Returns:	valid id of created layer instance - a negative number on error.
 * Side Effect:	
 */
/*VARARGS4*/
NhlDOCTAG(NhlCreate)
NhlErrorTypes
NhlVACreate
#if	NeedVarArgProto
(
	int		*pid,		/* return plot id		*/
	Const char	*name,		/* name of instance		*/
	NhlLayerClass	lc,		/* NhlLayerClass to create	*/
	int		parentid,	/* id of parent			*/
	...				/* resource name/value pairs	*/
)
#else
(pid, name, lc, parentid, va_alist)
	int		*pid;		/* return plot id		*/
	Const char	*name;		/* name of instance		*/
	NhlLayerClass	lc;		/* NhlLayerClass to create	*/
	int		parentid;	/* id of parent			*/
	va_dcl				/* resource name/value pairs	*/
#endif	/* NeedVarArgProto */
{
	va_list		ap;
	int		num_args;
	_NhlArg		args[_NhlMAXARGLIST];
	NhlErrorTypes	ret;

	if(pid == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"NhlCreate:arg pid must not be NULL");
		return NhlFATAL;
	}

	VA_START(ap,parentid); 
	num_args = _NhlVarToSetArgList(ap,args);
	va_end(ap);

/*
 * Initialize pid in case user didn't
 */
	*pid = NhlNULL_LAYER;

	ret = _NhlCreate(pid, name, lc, parentid, args, num_args, NULL);

	return(ret);
}

/*
 * Function:	NhlCreate
 *
 * Description:	This function is used to create an instance of the given
 *		NhlLayerClass.  This function is identical to NhlVACreate
 *		except that it takes an RL list instead of using varargs.
 *
 * In Args:	Const NhlString	name;		name of instance
 *		NhlLayerClass	lc;		NhlLayerClass to create
 *		int		parentid;	id of parent
 *		NhlArgList	args,
 *		int		nargs
 *
 * Out Args:	int		*pid,		return plot id
 *
 * Scope:	Global Public
 * Returns:	valid id of created layer instance - a negative number on error.
 * Side Effect:	
 */
NhlErrorTypes
NhlCreate
#if	__STDC__
(
	int		*pid,		/* plot id <return>		*/
	Const char	*name,		/* name of instance		*/
	NhlLayerClass	lc,		/* NhlLayerClass to create	*/
	int		parentid,	/* parent's id			*/
	int		rlid		/* RL list of resources		*/
)
#else
(pid,name,lc,parentid,rlid)
	int		*pid;		/* plot id <return>		*/
	Const char	*name;		/* name of instance		*/
	NhlLayerClass	lc;		/* NhlLayerClass to create	*/
	int		parentid;	/* parent's id			*/
	int		rlid;		/* RL list of resources		*/
#endif
{
	_NhlArg		args[_NhlMAXARGLIST];
	int		nargs;

	if(pid == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"NhlCreate:arg pid must not be NULL");
		return NhlFATAL;
	}

	if(!_NhlRLToArgList(rlid,NhlSETRL,args,&nargs)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"NhlCreate:Invalid RL id %d",rlid);
		return NhlFATAL;
	}

/*
 * Initialize pid in case user didn't
 */
	*pid = NhlNULL_LAYER;

	return _NhlCreate(pid, name, lc, parentid, args, nargs, NULL);
}

/*
 * Function:	NhlALCreate
 *
 * Description:	This function is used to create an instance of the given
 *		NhlLayerClass.  This function is identical to NhlCreate
 *		except that it takes an arglist instead of using varargs.
 *
 * In Args:	Const NhlString	name;		name of instance
 *		NhlLayerClass	lc;		NhlLayerClass to create
 *		int		parentid;	id of parent
 *		NhlArgList	args,
 *		int		nargs
 *
 * Out Args:	int		*pid,		return plot id
 *
 * Scope:	Global Public
 * Returns:	valid id of created layer instance - a negative number on error.
 * Side Effect:	
 */
NhlDOCTAG(NhlALCreate)
NhlErrorTypes
NhlALCreate
#if	__STDC__
(
	int		*pid,		/* plot id <return>		*/
	Const char	*name,		/* name of instance		*/
	NhlLayerClass	lc,		/* NhlLayerClass to create	*/
	int		parentid,	/* parent's id			*/
	NhlSArgList	args_in,	/* resource name/values		*/
	int		nargs		/* number of resources		*/
)
#else
(pid, name, lc, parentid, args_in, nargs)
	int		*pid;		/* plot id <return>		*/
	Const char	*name;		/* name of instance		*/
	NhlLayerClass	lc;		/* NhlLayerClass to create	*/
	int		parentid;	/* parent's id			*/
	NhlSArgList	args_in;	/* resource name/values		*/
	int		nargs;		/* number of resources		*/
#endif	/* NeedVarArgProto */
{
	_NhlArg		args[_NhlMAXARGLIST];

	if(pid == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"NhlALCreate:arg pid must not be NULL");
		return NhlFATAL;
	}

	if(nargs > _NhlMAXARGLIST){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlALCreate:Only %d args can be passed in at a time",
								_NhlMAXARGLIST);
		return NhlFATAL;
	}

	_NhlSArgToSetArgList(args,args_in,nargs);

/*
 * Initialize pid in case user didn't
 */
	*pid = NhlNULL_LAYER;

	return _NhlCreate(pid, name, lc, parentid, args, nargs, NULL);
}

/*
 * Function:	InitAllResources
 *
 * Description:	This function initializes the all_resources field for the
 *		given class.  This is done here instead of in initialize
 *		layer class because it is not needed for the majority of
 *		classes.  So, we only do it when it's needed. The class
 *		must be initialized before this function is called.
 *		It is only neccessary if the Class is being used as
 *		an automatically managed child of another class ie.  is
 *		being registered as a childclass of another class.
 *
 *
 * In Args:	
 *		NhlLayerClass	lc	Class to init all_resources
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
InitAllResources
#if	__STDC__
(
	NhlLayerClass	lc	/* Class to init all_resources 	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* Class to init all_resources 	*/
#endif
{
	NrmQuark		list[_NhlMAXRESLIST+1];
	NrmQuark		tquark;
	int			i;
	NrmResourceList		parentlist =
				(NrmResourceList)lc->base_class.resources;
	int			num_all_res = 0;
	_NhlChildResList	tnode;
	
	if(lc->base_class.class_inited & _NhlObjLayerClassFlag)
		tnode = NULL;
	else
		tnode = lc->base_class.child_resources;

	/*
	 * Initialize list
	 */
	memset((char*)list,0,sizeof(list));

	/*
	 * If the class uses children add there resources to the list
	 * but only add each resource to the list once even if it occurs
	 * in more then one child.
	 */
	while(tnode != NULL){

		i = 0;
		while((tquark = tnode->resources[i++]) != NrmNULLQUARK)
			if(!NrmQinQList(list,tquark))
				list[num_all_res++] = tquark;
		tnode = tnode->next;
	}

	/*
	 * Add resources for the actual class - don't need to check
	 * for duplicates because the children lists are not allowed to
	 * have any of the resources that the parent has.
	 */

	for(i= 0;i < lc->base_class.num_resources;i++)
		list[i + num_all_res] = parentlist[i].nrm_name;

	num_all_res += lc->base_class.num_resources;

	lc->base_class.all_resources = (NrmQuarkList)NhlMalloc((unsigned)
					((num_all_res + 1) * sizeof(NrmQuark)));
	if(lc->base_class.all_resources == NULL){
		NHLPERROR((NhlFATAL,12,"Unable to initialize all_resources of %s",
							_NhlClassName(lc)));
		return NhlFATAL;
	}

	memcpy((char*)lc->base_class.all_resources,(char*)list,
					(num_all_res+1)*sizeof(NrmQuark));

	return NhlNOERROR;
}

/*
 * Function:	_NhlRegisterChildClass
 *
 * Description:	This function is used by object writers to register a class
 *		that they may end up using as a child within the class they
 *		are writing. If they register the child's class in there
 *		class_initialize proc, and then create the child using
 *		_NhlCreateChild to create the child - the hlu support
 *		routines will take care of forwarding resources set in
 *		the parent to the child. In effect making the fact that
 *		there is a child in use transparent to the App writer. The
 *		autosetval param is used to determine if the NhlSetValues
 *		function should automatically call setvalues on children
 *		or if it should let the parent call _NhlSetChildValues
 *		to control exactly when the child's resources are set.
 *
 * In Args:	
 *		NhlLayerClass	parent,		parent class
 *		NhlLayerClass	child,		child class
 *		NhlBoolean	autosetval,	SetValue im/ex plicite
 *		NhlBoolean	forward,	T-frwd listed F-excl listed
 *		...
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(_NhlRegisterChildClass)
/*VARARGS3*/
NhlErrorTypes
_NhlRegisterChildClass
#if	NeedVarArgProto
(
	NhlLayerClass	parent,		/* parent class			*/
	NhlLayerClass	child,		/* child class			*/
	NhlBoolean	autosetval,	/* SetValue im/ex plicite	*/
	NhlBoolean	forward,	/* T-frwd listed F-excl listed	*/
	...				/* resource names		*/
)
#else
(parent,child,autosetval,forward,va_alist)
	NhlLayerClass	parent;		/* parent class			*/
	NhlLayerClass	child;		/* child class			*/
	NhlBoolean	autosetval;	/* SetValue im/ex plicite	*/
	NhlBoolean	forward;	/* T-frwd listed F-excl listed	*/
	va_dcl				/* resource names		*/
#endif
{
	va_list			ap;
	int			i;
	NhlString		name=NULL;
	NrmQuark		varqlist[_NhlMAXRESLIST];
	NrmQuark		forwardqlist[_NhlMAXRESLIST];
	NrmQuark		tquark;
	NhlErrorTypes		ret=NhlNOERROR;
	NhlErrorTypes		lret=NhlNOERROR;
	int			num_frwd;
	_NhlChildResList	chld_node=NULL;

	/*
	 * initialize varqlist with NrmNULLQUARKS
	 */
	memset((char*)varqlist,0,sizeof(varqlist));

	/*
	 * make sure the child class has been initialized so it knows what
	 * it's resources are, and so it's resource list has been compiled.
	 */
	if(!(child->base_class.class_inited)){
		ret = InitializeLayerClass(child);
		if(ret < NhlWARNING)
			return(ret);
	}

	/*
	 * ObjNhlLayer's do not support resource forwarding so they can not
	 * have children or be children.
	 */
	if(parent->base_class.class_inited & _NhlObjLayerClassFlag){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
"_NhlRegisterChildClass:%s is a sub-class of Obj so it can't register children",
						parent->base_class.class_name);
		return NhlFATAL;
	}

	/*
	 * Fill childs all_resources field with all the resources that
	 * child is interested in.
	 */
	if(child->base_class.all_resources == NULL){
		lret = InitAllResources(child);
		if(lret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Unable to determine resources for %s",
							_NhlClassName(child));
			return(ret);
		}
		ret = MIN(lret,ret);
	}

	/*
	 * make a list of the resources the parent is interested in forwarding
	 * or not forwarding.
	 */
	i=0;
	VA_START(ap,forward);
	for(name = (NhlString)(va_arg(ap, NhlString)); name != NULL;
				name = (NhlString)(va_arg(ap, NhlString)))
		varqlist[i++] = NrmStringToName(name);
	va_end(ap);

	/*
	 * Fill resources list with the resources that should be forwarded
	 * on to children of this class.
	 */
	i=0;
	num_frwd = 0;
	while((tquark = child->base_class.all_resources[i++]) != NrmNULLQUARK){

		if(_NhlResInClass(parent,tquark))
			continue;

		/*
		 * if varqlist contains resources that should be forwarded
		 * then forward the resoure if it's in the list.
		 * if varqlist contains resources that shouldn't be forwarded
		 * then forward the resoure if it's not in the list.
		 */
		if(forward == NrmQinQList(varqlist,tquark))
			forwardqlist[num_frwd++] = tquark;
	}
	forwardqlist[num_frwd] = NrmNULLQUARK;

	/*
	 * allocate space for the child resource node to be placed in
	 * the parent.
	 */
	chld_node = (_NhlChildResList)NhlMalloc(sizeof(_NhlChildResNode));
	if(chld_node == NULL){
		NHLPERROR((NhlFATAL,12,NULL));
		return(NhlFATAL);
	}

	chld_node->class = child;

	chld_node->autosetval = autosetval;

	/*
	 * allocate space for the resources to forward to child
	 */
	chld_node->resources = (NrmQuarkList)NhlMalloc((unsigned)
					((num_frwd + 1) * sizeof(NrmQuark)));
	if(chld_node->resources == NULL){
		(void)NhlFree(chld_node);
		NHLPERROR((NhlFATAL,12,NULL));
		return(NhlFATAL);
	}

	memcpy((char*)chld_node->resources,(char*)forwardqlist,
						(num_frwd+1)*sizeof(NrmQuark));

	/*
	 * Insert chld_node into child_resources list of parent
	 */
	chld_node->next = parent->base_class.child_resources;
	parent->base_class.child_resources = chld_node;

	return ret;
}

/*
 * Function:	CreateChild
 *
 * Description:	This is the function that actually does the CreateChild
 *		stuff after the args are parsed off in the varargs interface
 *		or in the AL interface.  It puts the management information
 *		in the parent of the child - to set it up for automatic
 *		resource forwarding.
 *
 * In Args:	
 *		int		*pid,		pid return
 *		Const NhlString	name,		name of child
 *		NhlLayerClass	class,		class to create
 *		NhlLayer		parent,		parent of child
 *		_NhlArgList	sargs,		resources to set
 *		int		num_sargs	number of res to set
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(CreateChild)
static NhlErrorTypes
CreateChild
#if	__STDC__
(
	int		*pid,		/* pid return		*/
	Const char	*name,		/* name of child	*/
	NhlLayerClass	class,		/* class to create	*/
	NhlLayer	parent,		/* parent of child	*/
	_NhlArgList	sargs,		/* resources to set	*/
	int		num_sargs	/* number of res to set	*/
)
#else
(pid,name,class,parent,sargs,num_sargs)
	int		*pid;		/* pid return		*/
	Const char	*name;		/* name of child	*/
	NhlLayerClass	class;		/* class to create	*/
	NhlLayer	parent;		/* parent of child	*/
	_NhlArgList	sargs;		/* resources to set	*/
	int		num_sargs;	/* number of res to set	*/
#endif
{
	int			i;
	int			num_pargs=0;
	_NhlArgList		pargs = NULL;
	int			num_args=0;
	_NhlArg			args[_NhlMAXARGLIST];
	NrmQuarkList		child=NULL;
	NhlLayerClass		plc = _NhlClass(parent);
	_NhlChildResList	tresnode=NULL;
	_NhlChildArgList	targnode=NULL;
	NhlErrorTypes		ret=NhlNOERROR;
	_NhlChildList		tchldnode=NULL;

	if(_NhlIsObj(parent)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"CreateChild:%s is an Obj sub-class so it cannot have children",
						NhlName(parent->base.id));
		return NhlFATAL;
	}

	if(!parent->base.in_init){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"CreateChild:%s is not in Initialize so CreateChild is invalid",
						_NhlName(parent));
		return NhlFATAL;
	}

	if(pid == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"CreateChild:argument pid can not be NULL");
		return NhlFATAL;
	}

	/*
	 * set child to the list of resources that should be forwarded
	 */
	tresnode = plc->base_class.child_resources;

	while(tresnode != NULL){
		if(tresnode->class == class){
			child = tresnode->resources;
			break;
		}
		tresnode = tresnode->next;
	}

	if(child == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s NhlLayerClass is not a registered child class of %s",
				_NhlClassName(class),_NhlClassName(class));
		return NhlFATAL;
	}

	/*
	 * retrieve create args passed by parent
	 */
	targnode = parent->base.child_args;

	while(targnode != NULL){
		if(targnode->class == class){
			pargs = targnode->args;
			num_pargs = targnode->nargs;
			/*
			 * Mark each arg as used
			 */
			for(i=0;i<targnode->nargs;i++)
				*(targnode->args_used[i]) = True;
			break;
		}
		targnode = targnode->next;
	}

	/*
	 * merge the pargs and sargs into a single args list for _NhlCreate
	 */
	_NhlMergeArgLists(args,&num_args,sargs,num_sargs,pargs,num_pargs);

	/*
	 * Initailize pid in case user didn't
	 */

	*pid = NhlNULL_LAYER;

	/*
	 * Create a child node to place child info in.
	 * Done here so we don't waste time creating the child when there
	 * isn't enough memory to save it anyway.
	 */
	tchldnode = (_NhlChildList)NhlMalloc(sizeof(_NhlChildNode));
	if(tchldnode == NULL){
		NHLPERROR((NhlFATAL,12,"Unable to Create Child"));
		return NhlFATAL;
	}

	/*
	 * Create the child
	 */
	ret = _NhlCreate(pid,name,class,parent->base.id,args,num_args,child);

	/*
	 * fill in the child node infomation and add it into the children
	 * list of the parent
	 */
	tchldnode->pid = *pid;
	tchldnode->svalscalled = False;
	tchldnode->class = class;
	tchldnode->resources = child;
	tchldnode->next = parent->base.children;
	parent->base.children = tchldnode;

	return ret;
}

/*
 * Function:	_NhlCreateChild
 *
 * Description:	This function is used from within a layer's methode to
 *		create a child layer instance that has resources automatically
 *		forwarded to it.  Therefore the parent doesn't have to know
 *		about all of the resources in the child - only the one's
 *		it is interested in controling. It really only parses of
 *		the varargs and puts them in an internal arglist format
 *		for the NhlDOCREF(#CreateChild,CreateChild) function.
 *
 * In Args:	
 *		int		*pid,	pid return
 *		Const NhlString	name,	name of child
 *		NhlLayerClass	class,	class to create
 *		NhlLayer		parent,	parent of child
 *		...			args to set in child
 *
 * Out Args:	
 *
 * Scope:	Global NhlLayer writer
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(_NhlCreateChild)
/*VARARGS4*/
NhlErrorTypes
_NhlCreateChild
#if	NeedVarArgProto
(
	int		*pid,	/* pid return		*/
	Const char	*name,	/* name of child	*/
	NhlLayerClass	class,	/* class to create	*/
	NhlLayer	parent,	/* parent of child	*/
	...			/* args to set in child	*/
)
#else
(pid,name,class,parent,va_alist)
	int		*pid;	/* pid return		*/
	Const char	*name;	/* name of child	*/
	NhlLayerClass	class;	/* class to create	*/
	NhlLayer	parent;	/* parent of child	*/
	va_dcl
#endif
{
	va_list			ap;
	int			num_vargs;
	_NhlArg			vargs[_NhlMAXARGLIST];
	NhlErrorTypes		ret;

	/*
	 * retrieve the var arg list
	 */
	VA_START(ap,parent);
	num_vargs = _NhlVarToSetArgList(ap,vargs);
	va_end(ap);

	/*
	 * Create the child
	 */
	ret = CreateChild(pid,name,class,parent,vargs,num_vargs);


	return ret;
}

/*
 * Function:	_NhlALCreateChild
 *
 * Description:	This function is used from within a layer's methode to
 *		create a child layer instance that has resources automatically
 *		forwarded to it.  Therefore the parent doesn't have to know
 *		about all of the resources in the child - only the one's
 *		it is interested in controling. It really only parses the
 *		SArgs and creates an internal arglist for the
 *		NhlDOCREF(#CreateChild,CreateChild) function to do all the work.
 *
 * In Args:	
 *		int		*pid,		pid return
 *		Const NhlString	name,		name of child
 *		NhlLayerClass	class,		class to create
 *		NhlLayer	parent,		parent of child
 *		NhlSArgList	args_in,	args in
 *		int		nargs		number args
 *
 * Out Args:	
 *
 * Scope:	Global NhlLayer writer
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(_NhlALCreateChild)
NhlErrorTypes
_NhlALCreateChild
#if	__STDC__
(
	int		*pid,		/* pid return		*/
	Const char	*name,		/* name of child	*/
	NhlLayerClass	class,		/* class to create	*/
	NhlLayer	parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args		*/
)
#else
(pid,name,class,parent,args_in,nargs)
	int		*pid;		/* pid return		*/
	Const char	*name;		/* name of child	*/
	NhlLayerClass	class;		/* class to create	*/
	NhlLayer	parent;		/* parent of child	*/
	NhlSArgList	args_in;	/* args in		*/
	int		nargs;		/* number args		*/
#endif
{
	_NhlArg			args[_NhlMAXARGLIST];
	NhlErrorTypes		ret;

	_NhlSArgToSetArgList(args,args_in,nargs);

	/*
	 * Create the child
	 */
	ret = CreateChild(pid,name,class,parent,args,nargs);

	return ret;
}
