/*
 *      $Id: Create.c,v 1.1 1993-04-30 17:21:39 boote Exp $
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
 *			layer Classes.
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TransObj.h>
#include <ncarg/hlu/Error.h>

/*
 * Function:	CallClassPartInit
 *
 * Description:	This function is used to call the top to bottom ClassPartInit
 *		chained method.
 *
 * In Args:	LayerClass	ancestor:
 *		LayerClass	lc;
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
	LayerClass	ancestor,	/* ancestor		*/
	LayerClass	lc		/* class to update	*/
)
#else
(ancestor, lc)
	LayerClass	ancestor;	/* ancestor		*/
	LayerClass	lc;		/* class to update	*/
#endif
{
	NhlErrorTypes ancestorerr = NOERROR, thisclass = NOERROR;

	if(ancestor->base_class.superclass != NULL) {
		ancestorerr =
			CallClassPartInit(ancestor->base_class.superclass,lc);

		if(ancestorerr < WARNING)
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
 * In Args:	LayerClass	lc;	Class to initialize
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
	LayerClass	lc	/* pointer to class to be initalized	*/
) 
#else
(lc) 
	LayerClass	lc;	/* pointer to class to be initalized	*/
#endif
{
	NhlErrorTypes ansestor, thisclass, classpart;
	LayerClass step;
	int inited = 0x01;

	if(lc->base_class.class_inited) return(NOERROR);

	step = lc;
	while(step != NULL) {
		if(step == workstationLayerClass) {
			inited |= (WorkstationLayerClassFlag 
					| BaseLayerClassFlag);  
			break;
		}
		if(step == baseLayerClass) {
			inited |= ( BaseLayerClassFlag );  
			break;
		}
		if(step == viewLayerClass) {
			inited |= ( ViewLayerClassFlag 
					| BaseLayerClassFlag);
			break;
		}
		if(step == transformLayerClass) {
			inited |= ( TransformLayerClassFlag
					| ViewLayerClassFlag
					| BaseLayerClassFlag);
			break;
		}
		if(step == errorLayerClass){
			inited |= ( ErrorLayerClassFlag | BaseLayerClassFlag);
			break;
		}
		if(step == transObjLayerClass) {
			inited |= (TransObjLayerClassFlag | BaseLayerClassFlag);
			break;
		}
		step = step->base_class.superclass;
	}

	if((lc->base_class.superclass != NULL)
		&&(!(lc->base_class.superclass->base_class.class_inited))){

		ansestor = InitializeLayerClass(lc->base_class.superclass);
		if(ansestor < WARNING)
			return(ansestor);
	}
	else
		ansestor = NOERROR;

	if(lc->base_class.class_initialize != NULL){
		thisclass = (*(lc->base_class.class_initialize))();
		if(thisclass < WARNING)
			return(thisclass);
	}
	else
		thisclass = NOERROR;

	classpart = CallClassPartInit(lc,lc);
	if(classpart < WARNING)
		return(classpart);

	lc->base_class.class_inited = inited;

	return(MIN((MIN(ansestor,thisclass)),classpart));
}
NhlErrorTypes _NhlInitializeLayerClass
#if	__STDC__
(
	LayerClass	lc	/* pointer to class to be initalized	*/
) 
#else
(lc) 
	LayerClass	lc;	/* pointer to class to be initalized	*/
#endif
{
	return(InitializeLayerClass(lc));
}

/*
 * Function:	CallInitialize
 *
 * Description:	This function is used to initialize an instance of the given
 *		Layer Class.  It calls the top to bottom initialize method
 *		of the classes.
 *
 * In Args:	LayerClass	lc	Class of Layer being initialized
 *		Layer		req	Layer with requested values in it
 *		_NhlArgList	args	res names and values requested
 *		int		nargs	number of args
 *
 * Out Args:	Layer		new	Layer to update
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallInitialize
#if	__STDC__
(
	LayerClass	lc,	/* Class of Layer being initialized	*/
	Layer		req,	/* Layer with requested values in it	*/
	Layer		new,	/* Layer to update			*/
	_NhlArgList	args,	/* res names and values requested	*/
	int		nargs	/* number of args			*/
)
#else
(lc,req,new,args,nargs)
	LayerClass	lc;	/* Class of Layer being initialized	*/
	Layer		req;	/* Layer with requested values in it	*/
	Layer		new;	/* Layer to update			*/
	_NhlArgList	args;	/* res names and values requested	*/
	int		nargs;	/* number of args			*/
#endif
{
	NhlErrorTypes ancestor=NOERROR, class=NOERROR;

	if(lc->base_class.superclass != NULL){
		ancestor = CallInitialize(lc->base_class.superclass,req,new,
								args,nargs);
		if(ancestor < WARNING)
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
 *		into the arlists.
 *
 * In Args:	NhlString	name		name to identify instance
 *		LayerClass	lc		class of instance to create
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
static NhlErrorTypes
_NhlCreate
#if	__STDC__
(
	int		*pid,		/* return plot id		*/
	NhlString	name,		/* name to identify instance	*/
	LayerClass	lc,		/* class of instance to create	*/
	int		parentid,	/* id# of parent		*/
	_NhlArgList	args,		/* resources to set in instance	*/
	int		nargs,		/* number of args		*/
	NrmQuarkList	child		/* used to create managed child	*/
)
#else
(pid, name, lc, parentid, args, nargs, child)
	int		*pid;		/* return plot id		*/
	NhlString	name;		/* name to identify instance	*/
	LayerClass	lc;		/* class of instance to create	*/
	int		parentid;	/* id# of parent		*/
	_NhlArgList	args;		/* resources to set in instance	*/
	int		nargs;		/* number of args		*/
	NrmQuarkList	child;		/* used to create managed child	*/
#endif
{
	Layer			parent = _NhlGetLayer(parentid);
	Layer			layer;
	Layer			request;
	NhlErrorTypes		ret=NOERROR, lret=NOERROR;
	_NhlArg			largs[MAXARGLIST];
	int			nlargs;
	_NhlChildArgList	chld_args=NULL;

	if(!(lc->base_class.class_inited)){
		ret = InitializeLayerClass(lc);
		if(ret < WARNING)
			return(ret);
	}

	layer = (Layer)NhlMalloc((unsigned)(lc->base_class.layer_size));

	if(layer == (Layer)NULL){
		NhlPError(FATAL,12,"Unable to Create");
		return(FATAL);
	}

	(void)memset((char*)layer,(int)~(0x01),(int)lc->base_class.layer_size);
	layer->base.self = layer;
	layer->base.parent = parent;
	layer->base.all_children = NULL;
	layer->base.children = NULL;
	layer->base.layer_class = lc;
	layer->base.nrm_name = NrmStringToName((name != NULL) ? name : "");
	layer->base.name = NrmNameToString(layer->base.nrm_name);

/*
 * Set Workstation Pointer in Layer
 *
 * All layer types that should not have parents should be handled on a case
 * by case bases first, and then the default case of getting the workstation
 * from the parent should be done.
 */
	if( _NhlIsWorkstation(layer)){
		layer->base.wkptr = layer;
		if(parent != NULL){
			NhlPError(WARNING,E_UNKNOWN,"Workstation objects should not have a parent - Resetting parent to NOPARENT");
			layer->base.parent = NULL;
		}
	}
	else if(_NhlIsError(layer)){
		layer->base.wkptr = NULL;
		if(parent != NULL){
			NhlPError(WARNING,E_UNKNOWN,"Error objects should not have a parent - Resetting parent to NOPARENT");
			layer->base.parent = NULL;
		}
	}
#ifdef	NOTYET
	else if(_NhlIsDataObject(layer)){
		layer->base.wkptr = NULL;
		if(parent != NULL){
			NhlPError(WARNING,E_UNKNOWN,"Data objects should not have a parent - Resetting parent to NOPARENT");
			layer->base.parent = NULL;
		}
	}
#endif	/*NOTYET*/
	else if(parent != NULL) {
		layer->base.wkptr = _NhlGetWorkstationLayer(parent);
	}
	else{
		NhlPError(FATAL,E_UNKNOWN,"This Layer must have a Parent");
		(void)NhlFree(layer);
		return(FATAL);
	}

/*
 * _NhlSortChildArgs sorts the args into seperate arg lists for the parent
 * and for the children.  The children args are returned as a linked list
 * that get's assigned into child_args of the instance during init and
 * then get's free'd soon after, that way they are available for any
 * _NhlCreateChild calls that happen during initialize.
 */
	lret = _NhlSortChildArgs(layer,args,nargs,largs,&nlargs,&chld_args,
									False);
	if(lret < WARNING){
		NhlPError(lret,E_UNKNOWN,
				"Unable to Create Arg Lists - Can't Create");
		(void)NhlFree(layer);
		return lret;
	}
	ret = MIN(lret,ret);

	layer->base.child_args = chld_args;

/*
 * Gets Resources from args and default files and sets them in layer
 */	
	lret = _NhlGetResources(layer,largs,nlargs,child);
	if(lret < WARNING){
		NhlPError(lret,E_UNKNOWN,
				"Unable to retrieve resources-Can't Create");
		_NhlFreeChildArgs(chld_args);
		(void)NhlFree(layer);
		return lret;
	}
	ret = MIN(lret,ret);
	
	request = (Layer)NhlMalloc((unsigned)(lc->base_class.layer_size));
	bcopy((char*)layer,(char*)request,lc->base_class.layer_size);

/*
 * If AddLayer can't add it, it will return an ErrorCode - less than 0
 */
	*pid = _NhlAddLayer(layer);
	if(*pid < 0){
		lret = (NhlErrorTypes)*pid;
		if(lret < WARNING){
			NhlPError(lret,E_UNKNOWN,"Unable to add layer to internal table-Can't Create");
			_NhlFreeChildArgs(chld_args);
			(void)NhlFree(layer);
			(void)NhlFree(request);
			return(lret);
		}
		ret = MIN(lret,ret);
	}

	lret = CallInitialize(lc,request,layer,largs,nlargs); 
	if(lret < WARNING){
		NhlPError(lret,E_UNKNOWN,
				"Unable to initialize layer-Can't Create");
		_NhlRemoveLayer(layer);
		_NhlFreeChildArgs(chld_args);
		(void)NhlFree(layer);
		(void)NhlFree(request);
		*pid = lret;
		return(lret);
	}
	ret = MIN(ret,lret);

	/*
	 * Free the child_args field of layer and set it to NULL
	 * It was only needed during Initialize
	 */
	_NhlFreeChildArgs(chld_args);
	layer->base.child_args = NULL;

	if(_NhlIsWorkstation(layer)) {
		lret = _NhlOpenWorkstation(layer);
		ret = MIN(ret,lret);
		if(ret < WARNING) {
			NhlPError(ret,E_UNKNOWN,
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
 * Function:	NhlCreate
 *
 * Description:	This function is used to create an instance of the given
 *		LayerClass.  It takes it's varargs and packs them into
 *		the internal arglist format needed - and then calls
 *		_NhlCreate.
 *
 * In Args:	NhlString	name;		name of instance
 *		LayerClass	lc;		LayerClass to create
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
NhlErrorTypes
NhlCreate
#if	NeedVarArgProto
(
	int		*pid,		/* return plot id		*/
	NhlString	name,		/* name of instance		*/
	LayerClass	lc,		/* LayerClass to create		*/
	int		parentid,	/* id of parent			*/
	...				/* resource name/value pairs	*/
)
#else
(pid, name, lc, parentid, va_alist)
	int		*pid;		/* return plot id		*/
	NhlString	name;		/* name of instance		*/
	LayerClass	lc;		/* LayerClass to create		*/
	int		parentid;	/* id of parent			*/
	va_dcl				/* resource name/value pairs	*/
#endif	/* NeedVarArgProto */
{
	va_list		ap;
	int		num_args;
	_NhlArgList	args = NULL;
	NhlErrorTypes	ret;

	if(pid == NULL){
		NhlPError(FATAL,E_UNKNOWN,"NhlCreate:arg pid must not be NULL");
		return FATAL;
	}

	VA_START(ap,parentid);
	num_args = _NhlCountSetVarList(ap);
	va_end(ap);

	VA_START(ap,parentid); 
	_NhlVarToSetArgList(ap,&args,num_args);
	va_end(ap);

/*
 * Initialize pid in case user didn't
 */
	*pid = NULL_LAYER;

	ret = _NhlCreate(pid, name, lc, parentid, args, num_args, NULL);

	(void)NhlFree(args);

	return(ret);
}

/*
 * Function:	NhlALCreate
 *
 * Description:	This function is used to create an instance of the given
 *		LayerClass.  This function is identical to NhlCreate
 *		except that it takes an arglist instead of using varargs.
 *
 * In Args:	NhlString	name;		name of instance
 *		LayerClass	lc;		LayerClass to create
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
NhlALCreate
#if	__STDC__
(
	int		*pid,		/* plot id <return>	*/
	NhlString	name,		/* name of instance	*/
	LayerClass	lc,		/* LayerClass to create	*/
	int		parentid,	/* parent's id		*/
	NhlSArgList	args_in,	/* resource name/values	*/
	int		nargs		/* number of resources	*/
)
#else
(pid, name, lc, parentid, args_in, nargs)
	int		*pid;		/* plot id <return>	*/
	NhlString	name;		/* name of instance	*/
	LayerClass	lc;		/* LayerClass to create	*/
	int		parentid;	/* parent's id		*/
	NhlSArgList	args_in;	/* resource name/values	*/
	int		nargs;		/* number of resources	*/
#endif	/* NeedVarArgProto */
{
	_NhlArgList	args = NULL;
	NhlErrorTypes	ret;

	if(pid == NULL){
		NhlPError(FATAL,E_UNKNOWN,
					"NhlALCreate:arg pid must not be NULL");
		return FATAL;
	}

	_NhlSArgToSetArgList(&args,args_in,nargs);

/*
 * Initialize pid in case user didn't
 */
	*pid = NULL_LAYER;

	ret = _NhlCreate(pid, name, lc, parentid, args, nargs, NULL);

	(void)NhlFree(args);

	return(ret);
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
 *		LayerClass	lc	Class to init all_resources
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
	LayerClass	lc	/* Class to init all_resources 	*/
)
#else
(lc)
	LayerClass	lc;	/* Class to init all_resources 	*/
#endif
{
	NrmQuark		list[MAXRESLIST];
	NrmQuark		tquark;
	int			i;
	NrmResourceList		parentlist =
				(NrmResourceList)lc->base_class.resources;
	int			num_all_res = 0;
	_NhlChildResList	tnode = lc->base_class.child_resources;

	/*
	 * Initialize list
	 */
	bzero(list,sizeof(list));

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
		NHLPERROR((FATAL,12,"Unable to initialize all_resources of %s",
							_NhlClassName(lc)));
		return FATAL;
	}

	bcopy(list,lc->base_class.all_resources,
					(num_all_res+1)*sizeof(NrmQuark));

	return NOERROR;
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
 *		LayerClass	parent,		parent class
 *		LayerClass	child,		child class
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
/*VARARGS3*/
NhlErrorTypes
_NhlRegisterChildClass
#if	NeedVarArgProto
(
	LayerClass	parent,		/* parent class			*/
	LayerClass	child,		/* child class			*/
	NhlBoolean	autosetval,	/* SetValue im/ex plicite	*/
	NhlBoolean	forward,	/* T-frwd listed F-excl listed	*/
	...				/* resource names		*/
)
#else
(parent,child,autosetval,forward,va_alist)
	LayerClass	parent;		/* parent class			*/
	LayerClass	child;		/* child class			*/
	NhlBoolean	autosetval;	/* SetValue im/ex plicite	*/
	NhlBoolean	forward;	/* T-frwd listed F-excl listed	*/
	va_dcl				/* resource names		*/
#endif
{
	va_list			ap;
	int			i;
	NhlString		name=NULL;
	NrmQuark		varqlist[MAXRESLIST];
	NrmQuark		forwardqlist[MAXRESLIST];
	NrmQuark		tquark;
	NhlErrorTypes		ret=NOERROR;
	NhlErrorTypes		lret=NOERROR;
	int			num_frwd;
	_NhlChildResList	chld_node=NULL;

	/*
	 * initialize varqlist with NrmNULLQUARKS
	 */
	bzero(varqlist,sizeof(varqlist));

	/*
	 * make sure the child class has been initialized so it knows what
	 * it's resources are, and so it's resource list has been compiled.
	 */
	if(!(child->base_class.class_inited)){
		ret = InitializeLayerClass(child);
		if(ret < WARNING)
			return(ret);
	}

	/*
	 * Fill childs all_resources field with all the resources that
	 * child is interested in.
	 */
	if(child->base_class.all_resources == NULL){
		lret = InitAllResources(child);
		if(lret < WARNING){
			NhlPError(FATAL,E_UNKNOWN,
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
		NHLPERROR((FATAL,12,NULL));
		return(FATAL);
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
		NHLPERROR((FATAL,12,NULL));
		return(FATAL);
	}

	bcopy(forwardqlist,chld_node->resources,(num_frwd+1)*sizeof(NrmQuark));

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
 *		NhlString	name,		name of child
 *		LayerClass	class,		class to create
 *		Layer		parent,		parent of child
 *		_NhlArgList	sargs,		resources to set
 *		int		num_sargs	number of res to set
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CreateChild
#if	__STDC__
(
	int		*pid,		/* pid return		*/
	NhlString	name,		/* name of child	*/
	LayerClass	class,		/* class to create	*/
	Layer		parent,		/* parent of child	*/
	_NhlArgList	sargs,		/* resources to set	*/
	int		num_sargs	/* number of res to set	*/
)
#else
(pid,name,class,parent,sargs,num_sargs)
	int		*pid;		/* pid return		*/
	NhlString	name;		/* name of child	*/
	LayerClass	class;		/* class to create	*/
	Layer		parent;		/* parent of child	*/
	_NhlArgList	sargs;		/* resources to set	*/
	int		num_sargs;	/* number of res to set	*/
#endif
{
	int			num_pargs=0;
	_NhlArgList		pargs = NULL;
	int			num_args=0;
	_NhlArgList		args = NULL;
	NrmQuarkList		child=NULL;
	LayerClass		plc = _NhlClass(parent);
	_NhlChildResList	tresnode=NULL;
	_NhlChildArgList	targnode=NULL;
	NhlErrorTypes		ret=NOERROR;
	_NhlChildList		tchldnode=NULL;

	if(pid == NULL){
		NhlPError(FATAL,E_UNKNOWN,"_NhlCreateChild:argument pid can not be NULL");
		return FATAL;
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
		NhlPError(FATAL,E_UNKNOWN,"%s LayerClass is not a registered child class of %s",_NhlClassName(class),
							_NhlClassName(class));
		return FATAL;
	}

	/*
	 * retrieve create args passed by parent
	 */
	targnode = parent->base.child_args;

	while(targnode != NULL){
		if(targnode->class == class){
			pargs = targnode->args;
			num_pargs = targnode->nargs;
			break;
		}
		targnode = targnode->next;
	}

	/*
	 * merge the pargs and sargs into a single args list for _NhlCreate
	 */
	_NhlMergeArgLists(&args,&num_args,sargs,num_sargs,pargs,num_pargs);

	/*
	 * Initailize pid in case user didn't
	 */

	*pid = NULL_LAYER;

	/*
	 * Create a child node to place child info in.
	 * Done here so we don't waste time creating the child when there
	 * isn't enough memory to save it anyway.
	 */
	tchldnode = (_NhlChildList)NhlMalloc(sizeof(_NhlChildNode));
	if(tchldnode == NULL){
		NHLPERROR((FATAL,12,"Unable to Create Child"));
		return FATAL;
	}

	/*
	 * Create the child
	 */
	ret = _NhlCreate(pid,name,class,parent->base.id,args,num_args,child);

	(void)NhlFree(args);

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
 *		for the CreateChild function.
 *
 * In Args:	
 *		int		*pid,	pid return
 *		NhlString	name,	name of child
 *		LayerClass	class,	class to create
 *		Layer		parent,	parent of child
 *		...			args to set in child
 *
 * Out Args:	
 *
 * Scope:	Global Layer writer
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*VARARGS4*/
NhlErrorTypes
_NhlCreateChild
#if	NeedVarArgProto
(
	int		*pid,	/* pid return		*/
	NhlString	name,	/* name of child	*/
	LayerClass	class,	/* class to create	*/
	Layer		parent,	/* parent of child	*/
	...			/* args to set in child	*/
)
#else
(pid,name,class,parent,va_alist)
	int		*pid;	/* pid return		*/
	NhlString	name;	/* name of child	*/
	LayerClass	class;	/* class to create	*/
	Layer		parent;	/* parent of child	*/
	va_dcl
#endif
{
	va_list			ap;
	int			num_vargs;
	_NhlArgList		vargs = NULL;
	NhlErrorTypes		ret;

	/*
	 * retrieve the var arg list
	 */
	VA_START(ap,parent);
	num_vargs = _NhlCountSetVarList(ap);
	va_end(ap);

	VA_START(ap,parent);
	_NhlVarToSetArgList(ap,&vargs,num_vargs);
	va_end(ap);

	/*
	 * Create the child
	 */
	ret = CreateChild(pid,name,class,parent,vargs,num_vargs);

	(void)NhlFree(vargs);

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
 *		SArgs and creates an internal arglist for the CreateChild
 *		function to do all the work.
 *
 * In Args:	
 *		int		*pid,		pid return
 *		NhlString	name,		name of child
 *		LayerClass	class,		class to create
 *		Layer		parent,		parent of child
 *		NhlSArgList	args_in,	args in
 *		int		nargs		number args
 *
 * Out Args:	
 *
 * Scope:	Global Layer writer
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlALCreateChild
#if	__STDC__
(
	int		*pid,		/* pid return		*/
	NhlString	name,		/* name of child	*/
	LayerClass	class,		/* class to create	*/
	Layer		parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args		*/
)
#else
(pid,name,class,parent,args_in,nargs)
	int		*pid;		/* pid return		*/
	NhlString	name;		/* name of child	*/
	LayerClass	class;		/* class to create	*/
	Layer		parent;		/* parent of child	*/
	NhlSArgList	args_in;	/* args in		*/
	int		nargs;		/* number args		*/
#endif
{
	_NhlArgList		args = NULL;
	NhlErrorTypes		ret;

	_NhlSArgToSetArgList(&args,args_in,nargs);

	/*
	 * Create the child
	 */
	ret = CreateChild(pid,name,class,parent,args,nargs);

	(void)NhlFree(args);

	return ret;
}
