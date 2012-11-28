/*
 *      $Id: Create.c,v 1.41 2004-05-28 20:45:26 dbrown Exp $
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
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TransObj.h>
#include <ncarg/hlu/Error.h>
#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/DataMgr.h>
#include <ncarg/hlu/DataItem.h>
#include <ncarg/hlu/XWorkstation.h>

/*
 * Function:	CallClassPartInit
 *
 * Description:	This function is used to call the top to bottom ClassPartInit
 *		chained method.
 *
 * In Args:	NhlClass	ancestor:
 *		NhlClass	lc;
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
#if	NhlNeedProto
(
	NhlClass	ancestor,	/* ancestor		*/
	NhlClass	lc		/* class to update	*/
)
#else
(ancestor, lc)
	NhlClass	ancestor;	/* ancestor		*/
	NhlClass	lc;		/* class to update	*/
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
 * Function:	InitializeClass
 *
 * Description:	This function is used to Initialize the Class structure
 *		of the given class.  It calls the Initialize function
 *		within the given class to do this.  Also, It initializes
 *		the given Classes superclass before doing itself.
 *
 * In Args:	NhlClass	lc;	Class to initialize
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	The structure pointed to by lc is initialized.
 */
static NhlErrorTypes
InitializeClass
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class to be initalized	*/
) 
#else
(lc) 
	NhlClass	lc;	/* pointer to class to be initalized	*/
#endif
{
	NhlErrorTypes ansestor, thisclass, classpart;
	NhlClass step;
	int inited = 0x01;

	if(lc->base_class.class_inited) return(NhlNOERROR);

	step = lc;
	while(step != NULL) {
		if(step == NhlobjClass)
			inited |= (_NhlObjClassFlag);
		else if(step == NhlbaseClass)
			inited |= ( _NhlBaseClassFlag );  
		else if(step == NhlworkstationClass)
			inited |= (_NhlWorkstationClassFlag);  
		else if(step == NhlviewClass)
			inited |= (_NhlViewClassFlag);
		else if(step == NhltransformClass)
			inited |= (_NhlTransformClassFlag);
		else if(step == NhlerrorClass)
			inited |= (_NhlErrorClassFlag);
		else if(step == NhltransObjClass)
			inited |= (_NhlTransObjClassFlag);
		else if(step == NhldataCommClass)
			inited |= (_NhlDataCommClassFlag);
		else if(step == NhldataMgrClass)
			inited |= (_NhlDataMgrClassFlag);
		else if(step == NhldataItemClass)
			inited |= (_NhlDataItemClassFlag);
		else if(step == NhldataSpecClass)
			inited |= (_NhlDataSpecClassFlag);
		else if(step == NhlappClass)
			inited |= (_NhlAppClassFlag);
		else if(step == NhlstyleClass)
			inited |= (_NhlStyleClassFlag);
		else if(step == NhlxWorkstationClass)
			inited |= (_NhlXWorkstationClassFlag);
		step = step->base_class.superclass;
	}

	if((lc->base_class.superclass != NULL)
		&&(!(lc->base_class.superclass->base_class.class_inited))){

		ansestor = InitializeClass(lc->base_class.superclass);
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

	lc->base_class.class_inited = inited;

	classpart = CallClassPartInit(lc,lc);
	if(classpart < NhlWARNING){
		lc->base_class.class_inited = False;
		return(classpart);
	}

	return(MIN((MIN(ansestor,thisclass)),classpart));
}

/*
 * Function:	_NhlInitializeClass
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
NhlDOCTAG(_NhlInitializeClass)
NhlErrorTypes
_NhlInitializeClass
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class to be initalized	*/
) 
#else
(lc) 
	NhlClass	lc;	/* pointer to class to be initalized	*/
#endif
{
	return(InitializeClass(lc));
}

/*
 * Function:	CallInitialize
 *
 * Description:	This function is used to initialize an instance of the given
 *		NhlLayer Class.  It calls the top to bottom initialize method
 *		of the classes.
 *
 * In Args:	NhlClass	lc	Class of NhlLayer being initialized
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
#if	NhlNeedProto
(
	NhlClass	lc,	/* Class of NhlLayer being initialized	*/
	NhlLayer	req,	/* NhlLayer with requested values in it	*/
	NhlLayer	new,	/* NhlLayer to update			*/
	_NhlArgList	args,	/* res names and values requested	*/
	int		nargs	/* number of args			*/
)
#else
(lc,req,new,args,nargs)
	NhlClass	lc;	/* Class of NhlLayer being initialized	*/
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

static void CleanUp
(
	NhlLayer l
)
{
	_NhlAllChildNode *child;

	if (! l)
		return;
	/*
	 * get rid of the app destroy callback
	 */

	if (l->base.app_destroy) {
		_NhlCBDelete(l->base.app_destroy);
	}
	/*
	 * get rid of any children that have been successfully created
	 */
	for (child = l->base.all_children; child; child = child->next) {
		NhlLayer cl = _NhlGetLayer(child->pid);
		if (! cl) 
			continue;
		/*
		 * don't want the child doing anything to the parent, since
		 * it doesn't exist yet
		 */
		cl->base.parent = NULL;

		NhlDestroy(child->pid);
	}
	return;
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
 *		NhlClass	lc		class of instance to create
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
#if	NhlNeedProto
(
	int		*pid,		/* return plot id		*/
	Const char	*name,		/* name to identify instance	*/
	NhlClass	lc,		/* class of instance to create	*/
	int		parentid,	/* id# of parent		*/
	_NhlArgList	args,		/* resources to set in instance	*/
	int		nargs,		/* number of args		*/
	NrmQuarkList	*child		/* used to create managed child	*/
)
#else
(pid, name, lc, parentid, args, nargs, child)
	int		*pid;		/* return plot id		*/
	Const char	*name;		/* name to identify instance	*/
	NhlClass	lc;		/* class of instance to create	*/
	int		parentid;	/* id# of parent		*/
	_NhlArgList	args;		/* resources to set in instance	*/
	int		nargs;		/* number of args		*/
	NrmQuarkList	*child;		/* used to create managed child	*/
#endif
{
	char			func[] = "_NhlCreate";
	NhlLayer		parent;
	NhlLayer		layer;
	NhlLayer		request;
	NhlErrorTypes		ret=NhlNOERROR, lret=NhlNOERROR;
	_NhlArg			stackargs[_NhlMAXARGLIST];
	_NhlArgList		largs=stackargs;
	int			nlargs;
	_NhlChildArgList	chld_args=NULL;
	NhlBoolean		chld_args_used[_NhlMAXARGLIST];
	_NhlConvertContext	context=NULL;
	int			i;
	NrmValue		from,to;
	static NrmQuark		Qint = NrmNULLQUARK;

	*pid = -4;

	if(Qint == NrmNULLQUARK)
		Qint = NrmPermStringToQuark(NhlTInteger);

	if(!(lc->base_class.class_inited)){
		ret = InitializeClass(lc);
		if(ret < NhlWARNING)
			return(ret);
	}

	if(lc->base_class.class_inited & _NhlAppClassFlag)
		parent = NULL;
	else if(parentid == NhlDEFAULT_APP){
		parent = _NhlGetCurrentApp();
		if(parent == NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to access \"default\" App object",
				func);
			return NhlFATAL;
		}
	}
	else{
		parent = _NhlGetLayer(parentid);
		if((parent == NULL) || (_NhlIsObj(parent) &&
						!(lc->base_class.class_inited &
						_NhlObjClassFlag))){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Invalid Parent id #%d",func,parentid);
				return NhlFATAL;
		}
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
	layer->base.nrm_name = NrmStringToName((name)?name:"");
	layer->base.name = (NhlString) NrmNameToString(layer->base.nrm_name);

/*
 * context is a structure that remembers the memory that is allocated
 * by any converters on behalf of this object.  It needs to be free'd -
 * along with all that memory after the Initailize methode has had a chance
 * to copy the memory.
 */
	context = _NhlCreateConvertContext(layer);
	if(context == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to create Convert Context - Can't Create");
		(void)NhlFree(layer);
		return NhlFATAL;
	}

/*
 * Find app object to use for resource database.
 */
	layer->base.appobj = NULL;
	layer->base.app_destroy = NULL;
	if(parent == NULL){
		layer->base.appobj = layer;
	}
	else{
		if((i = _NhlArgIsSet(args,nargs,NhlNobjAppObj))){
			int		appid;
	
			from.size = args[i-1].size;
			from.data = args[i-1].value;
			to.size = sizeof(int);
			to.data.ptrval = &appid;
	
			lret = _NhlConvertData(context,args[i-1].type,Qint,
								&from,&to);
	
			layer->base.app_destroy = NULL;
			layer->base.appobj = _NhlGetLayer(appid);
		}
	
		if(!layer->base.appobj || !_NhlIsApp(layer->base.appobj))
			layer->base.appobj = parent->base.appobj;
	
		if((layer->base.appobj != parent->base.appobj) ||
			parent->base.app_destroy){
			NhlArgVal	dummy,udata;
	
			NhlINITVAR(dummy);
			NhlINITVAR(udata);

			/* INSTALL CB to handle destroy of appobj */
			dummy.lngval = 0;
			udata.ptrval = layer;
			layer->base.app_destroy = _NhlAddObjCallback(
						layer->base.appobj,
						_NhlCBobjDestroy,dummy,
						_NhlBaseAppDestroyCB,udata);
		}
	}

	layer->base.being_destroyed = False;
	layer->base.all_children = NULL;

/*
 * Check for valid parent for Obj sub-classes
 */
	if(_NhlIsObj(layer)){
		if(_NhlIsDataMgr(layer) && !_NhlIsDataItem(parent)){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
"%s:DataMgr objects can only be created as a child of a DataItem Class",func);
			(void)NhlFree(layer);
			return NhlFATAL;
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
		layer->base.children = NULL;

/*
 * Set Workstation Pointer in NhlLayer
 *
 * All layer types that should not have parents should be handled on a case
 * by case bases first, and then the default case of getting the workstation
 * from the parent should be done.
 */
		layer->base.wkptr = NULL;
		if( _NhlIsWorkstation(layer)){
			layer->base.wkptr = layer;
			if(!_NhlIsApp(parent)){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
"%s:Workstation objects must have an App class object for their parent",func);
				NhlFree(layer);
				return NhlFATAL;
			}
		}
		else if(_NhlIsError(layer)){
			if(!_NhlIsApp(parent)){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
	"%s:The Error object must have an App class object as a parent",func);
				NhlFree(layer);
				return NhlFATAL;
			}
		}
		else if(_NhlIsDataItem(layer)){
			if(!_NhlIsApp(parent)){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
"%s:DataItem objects must have an App class object for their parent",func);
				NhlFree(layer);
				return NhlFATAL;
			}
		}
		else if(_NhlIsStyle(layer)){
			/* a workstation is okay but deprecated */
			layer->base.wkptr = _NhlGetWorkstationLayer(parent);
			if(! (layer->base.wkptr || _NhlIsApp(parent) ||
			      _NhlIsStyle(parent))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
"%s:Style objects must have an App class object for their parent",func);
				NhlFree(layer);
				return NhlFATAL;
			}
		}
		else if(parent != NULL) {
			layer->base.wkptr = _NhlGetWorkstationLayer(parent);
			if(layer->base.wkptr == NULL){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:This object must have a Workstation Ancestor",func);
				NhlFree(layer);
				return NhlFATAL;
			}
		}

		if(_NhlIsApp(layer)){
			/*
			 * This function just assumes that any resource that
			 * is not in the "App" class, is an Application defined
			 * one. An error message should be reported when the
			 * App object tries to apply the arg to it's Application
			 * defined resources, if it is not actually defined.
			 */
			lret = _NhlSortAppArgs(layer,args,nargs,&largs,&nlargs);
			if(lret < NhlWARNING){
				NhlPError(lret,NhlEUNKNOWN,
				"%s:Unable to Create Arg Lists - Can't Create",
				func);
				(void)NhlFree(layer);
				return lret;
			}
			layer->base.child_args = NULL;
			for(i=0;i<nargs;i++)
				chld_args_used[i] = True;
		}
		else{
/*
 * _NhlSortChildArgs sorts the args into seperate arg lists for the parent
 * and for the children.  The children args are returned as a linked list
 * that get's assigned into child_args of the instance during init and
 * then get's free'd soon after, that way they are available for any
 * _NhlVACreateChild calls that happen during initialize.
 * Resource forwarding is not supported for Obj's.
 */
			lret = _NhlSortChildArgs(layer,args,nargs,&largs,
				&nlargs,&chld_args,chld_args_used,False);
			if(lret < NhlWARNING){
				NhlPError(lret,NhlEUNKNOWN,
				"Unable to Create Arg Lists - Can't Create");
				(void)NhlFree(layer);
				return lret;
			}
			ret = MIN(lret,ret);

			layer->base.child_args = chld_args;
		}
	}

/*
 * Gets Resources from args and default files and sets them in layer
 */	
	lret = _NhlGetLayerResources(context,layer,largs,nlargs,child);
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
	(void)NhlFree(request);
	_NhlFreeConvertContext(context);
	_NhlFreeChildArgs(chld_args);
	if(lret < NhlWARNING){
		NhlPError(lret,NhlEUNKNOWN,
				"Unable to initialize layer-Can't Create");
		CleanUp(layer);
		_NhlRemoveLayer(layer);
		(void)NhlFree(layer);
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

	/*
	 * Add this layer to it's parents all_children list.
	 */
	if(!_NhlBaseAddChild(parent,*pid)){
		_NhlRemoveLayer(layer);
		(void)NhlFree(layer);
		*pid = NhlFATAL;
		ret = NhlFATAL;
	}

	if(_NhlIsWorkstation(layer)) {
		lret = _NhlOpenWorkstation(layer);
		ret = MIN(ret,lret);
		if(ret < NhlWARNING) {
			NhlPError(ret,NhlEUNKNOWN,
				"Unable to open Workstation-Can't Create");
			NhlDestroy(layer->base.id);
			*pid = ret; 
			return(ret);
		}
	}


	return(ret);
}

/*
 * Function:	NhlVACreate
 *
 * Description:	This function is used to create an instance of the given
 *		NhlClass.  It takes it's varargs and packs them into
 *		the internal arglist format needed - and then calls
 *		_NhlCreate. NhlDOCREF(#_NhlCreate,_NhlCreate is here.)
 *
 * In Args:	Const NhlString	name;		name of instance
 *		NhlClass	lc;		NhlClass to create
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
NhlDOCTAG(NhlVACreate)
NhlErrorTypes
NhlVACreate
#if	NhlNeedVarArgProto
(
	int		*pid,		/* return plot id		*/
	Const char	*name,		/* name of instance		*/
	NhlClass	lc,		/* NhlClass to create	*/
	int		parentid,	/* id of parent			*/
	...				/* resource name/value pairs	*/
)
#else
(pid, name, lc, parentid, va_alist)
	int		*pid;		/* return plot id		*/
	Const char	*name;		/* name of instance		*/
	NhlClass	lc;		/* NhlClass to create	*/
	int		parentid;	/* id of parent			*/
	va_dcl				/* resource name/value pairs	*/
#endif	/* NhlNeedVarArgProto */
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

	ret = _NhlCreate(pid, name, lc, parentid, args, num_args, NULL);

	return(ret);
}

/*
 * Function:	NhlCreate
 *
 * Description:	This function is used to create an instance of the given
 *		NhlClass.  This function is identical to NhlVACreate
 *		except that it takes an RL list instead of using varargs.
 *
 * In Args:	Const NhlString	name;		name of instance
 *		NhlClass	lc;		NhlClass to create
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
NhlDOCTAG(NhlCreate)
NhlErrorTypes
NhlCreate
#if	NhlNeedProto
(
	int		*pid,		/* plot id <return>		*/
	Const char	*name,		/* name of instance		*/
	NhlClass	lc,		/* NhlClass to create	*/
	int		parentid,	/* parent's id			*/
	int		rlid		/* RL list of resources		*/
)
#else
(pid,name,lc,parentid,rlid)
	int		*pid;		/* plot id <return>		*/
	Const char	*name;		/* name of instance		*/
	NhlClass	lc;		/* NhlClass to create	*/
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

	return _NhlCreate(pid, name, lc, parentid, args, nargs, NULL);
}

static sigjmp_buf	flc_env;

#if	!defined(IRIX) && !defined(SIG_PF)

typedef void (*vfunc)(
#if	NhlNeedProto
	int
#endif
);
#define	SIG_PF	vfunc
#endif

static void
handle_ill
#if	NhlNeedProto
(void)
#else
()
#endif
{
	siglongjmp(flc_env,1);
}

/*
 * Function:	nhlpfcreate
 *
 * Description:	This function is used to create an instance of the given
 *		classfunc.  This function is identical to NhlCreate
 *		except that it is called from fortran and uses a classfunc
 *		instead of the NhlClass pointer.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Fortran
 * Returns:	id is a negative number if the create failed.
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfcreate,NHLPFCREATE)
#if	NhlNeedProto
(
	int		*pid,		/* plot id <return>		*/
	_NhlFString	fname,		/* name of instance		*/
	_NhlClassFunc	lc_func,	/* NhlClass to create	*/
	int		*parentid,	/* parent's id			*/
	int		*rlid,		/* RL list of resources		*/
	int		*fname_len,	/* length of "name" char array	*/
	int		*err_ret	/* error return			*/
)
#else
(pid,fname,lc_func,parentid,rlid,fname_len,err_ret)
	int		*pid;		/* plot id <return>		*/
	_NhlFString	fname;		/* name of instance		*/
	_NhlClassFunc	lc_func;	/* NhlClass to create	*/
	int		*parentid;	/* parent's id			*/
	int		*rlid;		/* RL list of resources		*/
	int		*fname_len;	/* length of "name" char array	*/
	int		*err_ret;	/* error return			*/
#endif
{
	_NhlArg		args[_NhlMAXARGLIST];
	int		nargs;
	char		cname[_NhlMAXRESNAMLEN];
	char		*name;
	NhlClass	lc = NULL;
	struct sigaction	sact;
	
	/*
	 * Catch SIGILL to try and detect a bad class func
	 * (A bad class func would happen if the user forgot to declare
	 * the function external, or if they misspelled it.  I recommend
	 * using "implicit none", which would correct these stupid errors.)
	 */

	/*
	 * Set the jump point - this is where we come back to if the
	 * signal handler gets called. (sigsetjmp will return 1 if it
	 * is coming from the handler and 0 if it is from this direct
	 * call.)
	 */
	if(sigsetjmp(flc_env,1)){
		/*
		 * Reset SIGILL since it doesn't happen automatically.
		 */
		sact.sa_flags = 0;
		sact.sa_handler = SIG_DFL;
		sigemptyset(&sact.sa_mask);
		sigaction(SIGILL,&sact,NULL);
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"nhlfcreate:Invalid Class Function");
		*pid = *err_ret = NhlFATAL;
		return;
	}

	/*
	 * Catch SIGILL with handle_ill.
	 */
	sact.sa_flags = 0;
	sact.sa_handler = (SIG_PF)handle_ill;
	sigemptyset(&sact.sa_mask);
	sigaction(SIGILL,&sact,NULL);
	lc = (*lc_func)();

	/*
	 * reset SIGILL to SIG_DFL
	 */
	sact.sa_handler = SIG_DFL;
	sigaction(SIGILL,&sact,NULL);

	name = _NhlFstrToCstr(cname,NhlNumber(cname),fname,*fname_len);

	if(!_NhlRLToArgList(*rlid,NhlSETRL,args,&nargs)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"nhlfcreate:Invalid RL id %d",*rlid);
		*pid = *err_ret = NhlFATAL;
		return;
	}


	*err_ret = _NhlCreate(pid, name, lc, *parentid, args, nargs, NULL);
}

/*
 * Function:	NhlALCreate
 *
 * Description:	This function is used to create an instance of the given
 *		NhlClass.  This function is identical to NhlCreate
 *		except that it takes an arglist instead of using varargs.
 *
 * In Args:	Const NhlString	name;		name of instance
 *		NhlClass	lc;		NhlClass to create
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
#if	NhlNeedProto
(
	int		*pid,		/* plot id <return>		*/
	Const char	*name,		/* name of instance		*/
	NhlClass	lc,		/* NhlClass to create	*/
	int		parentid,	/* parent's id			*/
	NhlSArgList	args_in,	/* resource name/values		*/
	int		nargs		/* number of resources		*/
)
#else
(pid, name, lc, parentid, args_in, nargs)
	int		*pid;		/* plot id <return>		*/
	Const char	*name;		/* name of instance		*/
	NhlClass	lc;		/* NhlClass to create	*/
	int		parentid;	/* parent's id			*/
	NhlSArgList	args_in;	/* resource name/values		*/
	int		nargs;		/* number of resources		*/
#endif	/* NhlNeedVarArgProto */
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

	return _NhlCreate(pid, name, lc, parentid, args, nargs, NULL);
}

static int
qcompare
#if	NhlNeedProto
(
	Const void	*ov,
	Const void	*tv
)
#else
(ov,tv)
	Const void	*ov;
	Const void	*tv;
#endif
{
	NrmQuark	*one = (NrmQuark*)ov;
	NrmQuark	*two = (NrmQuark*)tv;

	if(*one < *two)
		return -1;
	if(*one > *two)
		return 1;
	return 0;
}

/*
 * Function:	_NhlInitAllResources
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
 *		NhlClass	lc	Class to init all_resources
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
int
_NhlInitAllResources
#if	NhlNeedProto
(
	NhlClass	lc	/* Class to init all_resources 	*/
)
#else
(lc)
	NhlClass	lc;	/* Class to init all_resources 	*/
#endif
{
	NrmQuark		list[_NhlMAXRESLIST+1];
	NrmQuark		tlist[_NhlMAXRESLIST+1];
	int			i,j,k=0;
	NrmResourceList		parentlist =
				(NrmResourceList)lc->base_class.resources;
	int			num_all_res = 0;
	_NhlChildResList	tnode;
	
	if(lc->base_class.class_inited & _NhlObjClassFlag)
		tnode = NULL;
	else
		tnode = lc->base_class.child_resources;

	/*
	 * Initialize list
	 */
	memset((char*)list,0,sizeof(list));

	/*
	 * If the class uses children add their resources to the list
	 * but only add each resource to the list once even if it occurs
	 * in more then one child.
	 */
	while(tnode != NULL){

		memcpy((char*)tlist,list,sizeof(NrmQuark)*(k+1));
		i=0;j=0;k=0;
		while((tlist[i] != NrmNULLQUARK) &&
					(tnode->resources[j] != NrmNULLQUARK)){
			if(tnode->resources[j] < tlist[i])
				list[k++] = tnode->resources[j++];
			else if(tnode->resources[j] > tlist[i])
				list[k++] = tlist[i++];
			else
				j++;
		}
		while(tlist[i] != NrmNULLQUARK)
			list[k++] = tlist[i++];
		while(tnode->resources[j] != NrmNULLQUARK)
			list[k++] = tnode->resources[j++];

		tnode = tnode->next;
	}

	/*
	 * Add resources for the actual class - don't need to check
	 * for duplicates because the children lists are not allowed to
	 * have any of the resources that the parent has.
	 */

	memcpy((char*)tlist,list,sizeof(NrmQuark)*(k+1));
	i=0;j=0;k=0;
	while((i < lc->base_class.num_resources) && (tlist[j] != NrmNULLQUARK)){
		if(parentlist[i].nrm_name < tlist[j])
			list[k++] = parentlist[i++].nrm_name;
		else
			list[k++] = tlist[j++];
	}
	while(tlist[j] != NrmNULLQUARK)
		list[k++] = tlist[j++];
	while(i < lc->base_class.num_resources)
		list[k++] = parentlist[i++].nrm_name;

	num_all_res = k;

	lc->base_class.all_resources = (NrmQuarkList)NhlMalloc((unsigned)
					((num_all_res + 1) * sizeof(NrmQuark)));
	if(lc->base_class.all_resources == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,
			"Unable to initialize all_resources of %s",
							_NhlClassName(lc)));
		return NhlFATAL;
	}

	memcpy((char*)lc->base_class.all_resources,(char*)list,
					(num_all_res+1)*sizeof(NrmQuark));

	return num_all_res;
}

/*
 * Function:	_NhlRegisterChildClass
 *
 * Description:	This function is used by object writers to register a class
 *		that they may end up using as a child within the class they
 *		are writing. If they register the child's class in there
 *		class_initialize proc, and then create the child using
 *		_NhlVACreateChild to create the child - the hlu support
 *		routines will take care of forwarding resources set in
 *		the parent to the child. In effect making the fact that
 *		there is a child in use transparent to the App writer. The
 *		autosetval param is used to determine if the NhlSetValues
 *		function should automatically call setvalues on children
 *		or if it should let the parent call _NhlSetChildValues
 *		to control exactly when the child's resources are set.
 *
 * In Args:	
 *		NhlClass	parent,		parent class
 *		NhlClass	child,		child class
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
#if	NhlNeedVarArgProto
(
	NhlClass	parent,		/* parent class			*/
	NhlClass	child,		/* child class			*/
	NhlBoolean	autosetval,	/* SetValue im/ex plicite	*/
	NhlBoolean	forward,	/* T-frwd listed F-excl listed	*/
	...				/* resource names		*/
)
#else
(parent,child,autosetval,forward,va_alist)
	NhlClass	parent;		/* parent class			*/
	NhlClass	child;		/* child class			*/
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
		ret = InitializeClass(child);
		if(ret < NhlWARNING)
			return(ret);
	}

	/*
	 * ObjLayer's do not support resource forwarding so they can not
	 * have children or be children.
	 */
	if(parent->base_class.class_inited & _NhlObjClassFlag){
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
		int count = _NhlInitAllResources(child);
		if (count < 0)
			lret = (NhlErrorTypes) count;
		ret = MIN(lret,ret);
		if(ret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Unable to determine resources for %s",
							_NhlClassName(child));
			return(ret);
		}
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

	qsort(varqlist,i,sizeof(NrmQuark),qcompare);

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

	chld_node->theclass = child;

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
 *		NhlClass	class,		class to create
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
#if	NhlNeedProto
(
	int		*pid,		/* pid return		*/
	Const char	*name,		/* name of child	*/
	NhlClass	theclass,	/* class to create	*/
	NhlLayer	parent,		/* parent of child	*/
	_NhlArgList	sargs,		/* resources to set	*/
	int		num_sargs	/* number of res to set	*/
)
#else
(pid,name,theclass,parent,sargs,num_sargs)
	int		*pid;		/* pid return		*/
	Const char	*name;		/* name of child	*/
	NhlClass	theclass;	/* class to create	*/
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
	NrmQuarkList		child_lists[_NhlMAXTREEDEPTH+1];
	NhlClass		plc = _NhlClass(parent);
	_NhlChildResList	tresnode=NULL;
	_NhlChildArgList	targnode=NULL;
	NhlErrorTypes		ret=NhlNOERROR;
	_NhlChildList		tchldnode=NULL;
	NhlClass		tplc,tclc;
	NhlLayer		tpl;

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
	 * set child_lists to the lists of resources that should be forwarded
	 * Need to travers full instance tree until we reach the first
	 * parent that doesn't forward resources.  That way, resources that
	 * are forwarded threw multiple objects can be found.
	 */
	tplc = plc;
	tclc = theclass;
	tpl = parent;
	child = NULL;
	for(i=0;i < _NhlMAXTREEDEPTH;i++){
		tresnode = tplc->base_class.child_resources;

		while(tresnode != NULL){
			if(tresnode->theclass == tclc){
				child = tresnode->resources;
				break;
			}
			tresnode = tresnode->next;
		}

		child_lists[i] = child;

		/*
		 * If child is Null, then we have reached the top of
		 * the forwarded resources.
		 */
		if(!child)
			break;

		tpl = tpl->base.parent;
		tclc = tplc;
		tplc = tpl->base.layer_class;
		child = NULL;
	}
	child_lists[_NhlMAXTREEDEPTH] = NULL;

	if(child_lists[0] == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s NhlClass is not a registered child class of %s",
				_NhlClassName(theclass),_NhlClassName(plc));
		return NhlFATAL;
	}

	/*
	 * retrieve create args passed by parent
	 */
	targnode = parent->base.child_args;

	while(targnode != NULL){
		if(targnode->theclass == theclass){
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
	ret = _NhlCreate(pid,name,theclass,parent->base.id,args,num_args,
								child_lists);

	/*
	 * fill in the child node infomation and add it into the children
	 * list of the parent
	 */
	tchldnode->pid = *pid;
	tchldnode->svalscalled = False;
	tchldnode->theclass = theclass;
	tchldnode->resources = child;
	tchldnode->next = parent->base.children;
	parent->base.children = tchldnode;

	return ret;
}

/*
 * Function:	_NhlVACreateChild
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
 *		NhlClass	theclass,	class to create
 *		NhlLayer		parent,	parent of child
 *		...			args to set in child
 *
 * Out Args:	
 *
 * Scope:	Global NhlLayer writer
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(_NhlVACreateChild)
/*VARARGS4*/
NhlErrorTypes
_NhlVACreateChild
#if	NhlNeedVarArgProto
(
	int		*pid,	/* pid return		*/
	Const char	*name,	/* name of child	*/
	NhlClass	theclass,	/* class to create	*/
	NhlLayer	parent,	/* parent of child	*/
	...			/* args to set in child	*/
)
#else
(pid,name,theclass,parent,va_alist)
	int		*pid;	/* pid return		*/
	Const char	*name;	/* name of child	*/
	NhlClass	theclass;	/* class to create	*/
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
	ret = CreateChild(pid,name,theclass,parent,vargs,num_vargs);


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
 *		NhlClass	theclass,		class to create
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
#if	NhlNeedProto
(
	int		*pid,		/* pid return		*/
	Const char	*name,		/* name of child	*/
	NhlClass	theclass,		/* class to create	*/
	NhlLayer	parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args		*/
)
#else
(pid,name,theclass,parent,args_in,nargs)
	int		*pid;		/* pid return		*/
	Const char	*name;		/* name of child	*/
	NhlClass	theclass;		/* class to create	*/
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
	ret = CreateChild(pid,name,theclass,parent,args,nargs);

	return ret;
}
