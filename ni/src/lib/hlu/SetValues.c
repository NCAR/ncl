/*
 *      $Id: SetValues.c,v 1.1 1993-04-30 17:24:12 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SetValues.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 10:54:22 MDT 1992
 *
 *	Description:	This file contains the functions neccessary to update
 *			a Layer instance via the SetValues call.
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>

/*
 * Function:	CallSetValues
 *
 * Description:	This function is used to call the SetValues methods of the
 *		given layer.
 *
 * In Args:	
 *		LayerClass	class,		Class of Layer being set
 *		Layer		oldl,		Layer w/ old values
 *		Layer		reql,		Layer w/ requested values
 *		Layer		newl,		Layer to update
 *		_NhlArgList	args,		res names and values to set
 *		int		num_args	num of resources to set
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallSetValues
#if	__STDC__
(
	LayerClass	class,		/* Class of Layer being set	*/
	Layer		oldl,		/* Layer w/ old values		*/
	Layer		reql,		/* Layer w/ requested values	*/
	Layer		newl,		/* Layer to update		*/
	_NhlArgList	args,		/* res names and values to set	*/
	int		num_args	/* num of resources to set	*/
)
#else
(class,oldl,reql,newl,args,num_args) 
	LayerClass	class;		/* Class of Layer being set	*/
	Layer		oldl;		/* Layer w/ old values		*/
	Layer		reql;		/* Layer w/ requested values	*/
	Layer		newl;		/* Layer to update		*/
	_NhlArgList	args;		/* res names and values to set	*/
	int		num_args;	/* num of resources to set	*/
#endif
{
	NhlErrorTypes ansestorerr=NOERROR, thisclasserr=NOERROR;

	if(class->base_class.superclass != NULL)
		ansestorerr = CallSetValues(class->base_class.superclass,
						oldl,reql,newl,args,num_args);

	if(ansestorerr < WARNING)
		return(ansestorerr);

	if(class->base_class.layer_set_values != NULL)
		thisclasserr = (*(class->base_class.layer_set_values))
						(oldl,reql,newl,args,num_args);

	return(MIN(ansestorerr,thisclasserr));
}

/*
 * Function:	SetValues
 *
 * Description:	This function sets resource values addressed by base + resource
 *		offset.  It sets the resources specified by the arg names
 *		and values in the arglist.
 *
 * In Args:	char*		base;		base address to write values to
 *		NrmResourceList	resources;	resource list with offsets
 *		int		num_res;	number of resources
 *		_NhlArgList	args;		names and values of resources
 *		int		nargs;		number of args
 *
 * Out Args:	base + resource offsets are set to values stored in args
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes 
SetValues
#if	__STDC__
(
	char*		base,		/* base address to write values to*/
	NrmResourceList	resources,	/* resource list with offsets	*/
	int		num_res,	/* number of resources		*/
	_NhlArgList	args,		/* names and values of resources*/
	int		nargs		/* number of args		*/
)
#else
(base, resources, num_res, args, nargs)
	char*		base;		/* base address to write values to*/
	NrmResourceList	resources;	/* resource list with offsets	*/
	int		num_res;	/* number of resources		*/
	_NhlArgList	args;		/* names and values of resources*/
	int		nargs;		/* number of args		*/
#endif
{
	register int	i,j;
	NhlBoolean	argfound[MAXARGLIST];
	NhlErrorTypes	retcode = NOERROR;

	/*
	 * all args could have been used in children
	 */
	if(nargs == 0)
		return NOERROR;

	/* Mark each arg as not found */ 
	bzero((char *) argfound, (int)(nargs * sizeof(NhlBoolean))); 
		 
	for(i=0; i < nargs; i++){
		for(j=0; j < num_res; j++){
			if(args[i].quark == resources[j].nrm_name){
				/* SUPPRESS 112 */
				_NhlCopyFromArg(args[i].value,
					(char*)(base + resources[j].nrm_offset),
					resources[j].nrm_size); 
				argfound[i] = True;
				break;
			}
		}

		if(!argfound[i]){
			NhlPError(INFO,E_UNKNOWN,
				"%s is not a resource in the given object",
						NrmNameToString(args[i].quark));
			retcode = MIN(retcode,INFO);
		}
	}

	return(retcode);
}

/*
 * Function:	_NhlSetValues
 *
 * Description:	This function sets the resources specified by the args passed
 *		in the layer passed.
 *
 * In Args:	_NhlArgList	args;	resource names and values to change
 *		int		nargs;	length of arg array
 *
 * Out Args:	Layer		l;	layer to set values in.
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	The layer is modified to set the requested values
 */
static NhlErrorTypes
_NhlSetValues
#if	__STDC__
(
	Layer		l,		/* layer instance	*/
	_NhlArgList	args,		/* args to change	*/
	int		nargs		/* number of args	*/
)
#else
(l,args,nargs)
	Layer		l;		/* layer instance	*/
	_NhlArgList	args;		/* args to change	*/
	int		nargs;		/* number of args	*/
#endif
{
	Layer			oldl,
				reql;
	int			layersize;
	LayerClass		lc = _NhlClass(l);
	NhlErrorTypes		ret=NOERROR, lret=NOERROR;
	_NhlArg			largs[MAXARGLIST];
	int			nlargs;
	_NhlChildArgList	targnode=NULL;
	_NhlChildList		tchldnode=NULL;

	if(l == NULL){
		NHLPERROR((FATAL,E_UNKNOWN,
				"_NhlSetValues was passed a NULL layer"));
		return FATAL;
	}

	if(nargs == 0)
		return NOERROR;

	/*
	 * Sort the args into args for this instance and for it's children
	 * If there are no children it just copies the args to largs
	 */
	lret = _NhlSortChildArgs(l,args,nargs,largs,&nlargs,&l->base.child_args,
									False);

	if(lret < WARNING){
		NhlPError(lret,E_UNKNOWN,
				"Unable to Create Arg Lists - Can't SetValues");
		return lret;
	}
	ret = MIN(ret,lret);

	/*
	 * If this layer has children forward args to them if autosetval = True.
	 */
	if(l->base.children != NULL){
		tchldnode = l->base.children;

		while(tchldnode != NULL){

			targnode = l->base.child_args;

			while((targnode != NULL) &&
					(tchldnode->class != targnode->class))
				targnode = targnode->next;
			
			if(targnode == NULL){
				NHLPERROR((FATAL,E_UNKNOWN,
					"SetValues can't find args to set child's resources %s",_NhlClassName(tchldnode->class)));
				tchldnode = tchldnode->next;
				continue;
			}

			if(targnode->autosetval){
				lret = _NhlSetValues(
					_NhlGetLayer(tchldnode->pid),
						targnode->args,targnode->nargs);
				if(lret < WARNING){
					NHLPERROR((lret,E_UNKNOWN,
				"SetValues can't set values of hidden child %s",
						NhlName(tchldnode->pid)));
					tchldnode = tchldnode->next;
					continue;
				}
				ret = MIN(lret,ret);

				tchldnode->svalscalled = True;
			}

			tchldnode = tchldnode->next;
		}
	}

	layersize = lc->base_class.layer_size;
	oldl = (Layer)NhlMalloc((unsigned)layersize);
	reql = (Layer)NhlMalloc((unsigned)layersize);

	if((oldl == NULL) || (reql == NULL)){
		NhlPError(FATAL,12,"Unable to set values of PID #%d",
								l->base.id);
		(void)NhlFree(oldl);
		(void)NhlFree(reql);
		return FATAL;
	}

	bcopy((char*)l,(char*)oldl,(int)layersize);

	lret = SetValues((char*)l,(NrmResourceList)(lc->base_class.resources),
				lc->base_class.num_resources, largs,nlargs);

	if (lret < WARNING) {
/*
 * When fatal error occurs destroy l - it is in an unpredictable state.
 */
		NhlPError(FATAL,E_UNKNOWN,"Unable to set values of PID #%d",
							l->base.id);
		NhlPError(FATAL,E_UNKNOWN,
			"PID #%d Destroyed to recover from errors",l->base.id);
		_NhlFreeChildArgs(l->base.child_args);
		(void)NhlDestroy(l->base.id);
		(void)_NhlRemoveLayer(l);
		(void)NhlFree(l);
		(void)NhlFree(oldl);
		(void)NhlFree(reql);
	} 
	ret = MIN(ret,lret);

	bcopy((char*)l,(char*)reql,(int)layersize);

	lret = CallSetValues(lc,oldl,reql,l,largs,nlargs);

	ret = MIN(lret,ret);

/*
 * LOOP threw child_list and make sure they have all had setvalues called
 * on them - if not print out an error. - Also reset svalscalled to False
 * for next SetValues call.
 */

	tchldnode = l->base.children;
	while(tchldnode != NULL){
		if(!tchldnode->svalscalled){
			NHLPERROR((WARNING,E_UNKNOWN,
				"SetValues never occured on %s: Error in %s",
						NhlName(tchldnode->pid),
						_NhlClassName(_NhlClass(l))));
		}
		tchldnode->svalscalled = False;

		tchldnode = tchldnode->next;
	}

	_NhlFreeChildArgs(l->base.child_args);
	l->base.child_args = NULL;

	(void)NhlFree(oldl);
	(void)NhlFree(reql);

	return ret;
}

/*
 * Function:	NhlSetValues
 *
 * Description:	This function sets the resources specified by the name/value
 *		pairs passed in threw the varargs.
 *
 * In Args:	int		id;	Index into list of layers
 *		...			resource name/value pairs
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	The layer indexed by id, is modified to set the requested values
 */
/*VARARGS1*/
NhlErrorTypes
NhlSetValues
#if	NeedVarArgProto
(
	int		id,		/* plot id		*/
	...
)
#else
(id,va_alist)
	int		id;		/* plot id		*/
	va_dcl
#endif	/* NeedVarArgProto */
{
        va_list         ap; 
	int             num_args; 
	_NhlArgList     args = NULL; 
	NhlErrorTypes	ret;
	Layer		l = NULL;

	/* count the variable args */
	VA_START(ap,id); 
	num_args = _NhlCountSetVarList(ap); 
	va_end(ap); 

	/* create an arglist from the varargs */
	VA_START(ap,id); 
	_NhlVarToSetArgList(ap,&args,num_args); 
	va_end(ap); 

	l = _NhlGetLayer(id);
	if(l == NULL){
		NhlPError(FATAL,E_UNKNOWN,
				"PID #%d can't be found in NhlSetValues",id);
		return(FATAL);
	}
	ret = _NhlSetValues(l, args, num_args);

	(void)NhlFree(args);

	return(ret);
}

/*
 * Function:	NhlALSetValues
 *
 * Description:	This function sets the resources specified by the SArgList
 *		passed to it.
 *
 * In Args:	int		id;	Index into list of layers
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	The layer indexed by id, is modified to set the requested values
 */
/*VARARGS1*/
NhlErrorTypes
NhlALSetValues
#if	__STDC__
(
	int		id,		/* plot id		*/
	NhlSArgList	args_in,	/* SArg's to set	*/
	int		nargs		/* num SArg's		*/
)
#else
(id,args_in,nargs)
	int		id;		/* plot id		*/
	NhlSArgList	args_in;	/* SArg's to set	*/
	int		nargs;		/* num SArg's		*/
#endif
{
	_NhlArgList     args = NULL; 
	NhlErrorTypes	ret;
	Layer		l = NULL;

	/* create an arglist from the sargs */
	_NhlSArgToSetArgList(&args,args_in,nargs); 

	l = _NhlGetLayer(id);
	if(l == NULL){
		NhlPError(FATAL,E_UNKNOWN,
				"PID #%d can't be found in NhlALSetValues",id);
		return(FATAL);
	}
	ret = _NhlSetValues(l, args, nargs);

	(void)NhlFree(args);

	return(ret);
}

/*
 * Function:	SetValuesChild
 *
 * Description:	This is the actual function that impliments the functionality
 *		of the _NhlSetValuesChild call, but it is private - done
 *		after the AL and varargs global interfaces have done what
 *		they need to do.
 *
 * In Args:	
 *		int		pid,		pid
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
SetValuesChild
#if	__STDC__
(
	int		pid,		/* pid			*/
	Layer		parent,		/* parent of child	*/
	_NhlArgList	sargs,		/* resources to set	*/
	int		num_sargs	/* number of res to set	*/
)
#else
(pid,parent,sargs,num_sargs)
	int		pid;		/* pid			*/
	Layer		parent;		/* parent of child	*/
	_NhlArgList	sargs;		/* resources to set	*/
	int		num_sargs;	/* number of res to set	*/
#endif
{
	int			num_pargs=0;
	_NhlArgList		pargs = NULL;
	int			num_args=0;
	_NhlArgList		args = NULL;
	_NhlChildArgList	targnode=NULL;
	NhlErrorTypes		ret=NOERROR;
	_NhlChildList		tchldnode=NULL;
	Layer			child = _NhlGetLayer(pid);

	if(child == NULL){
		NHLPERROR((FATAL,E_UNKNOWN,"Unable to retrieve child PID#%d",
									pid));
		return FATAL;
	}

	/*
	 * Retrieve the child node from the parent
	 */
	tchldnode = parent->base.children;
	while(tchldnode != NULL){
		if(tchldnode->pid == pid)
			break;
		tchldnode = tchldnode->next;
	}

	if(tchldnode == NULL){
		NHLPERROR((FATAL,E_UNKNOWN,
			"can't find child record for PID#%d in parent",pid));
		return FATAL;
	}

	/*
	 * retrieve args passed by parent
	 */
	targnode = parent->base.child_args;

	while(targnode != NULL){
		if(targnode->class == tchldnode->class){
			pargs = targnode->args;
			num_pargs = targnode->nargs;
			break;
		}
		targnode = targnode->next;
	}

	if(targnode == NULL){
		NHLPERROR((FATAL,E_UNKNOWN,
			"can't find forwarded args to set resources for %s",
					_NhlClassName(tchldnode->class)));
		return FATAL;
	}

	/*
	 * merge the pargs and sargs into a single args list for _NhlSetValues
	 */
	_NhlMergeArgLists(&args,&num_args,sargs,num_sargs,pargs,num_pargs);



	/*
	 * SetValues of the child
	 */
	ret = _NhlSetValues(child,args,num_args);

	(void)NhlFree(args);

	/*
	 * fill in the child node infomation and add it into the children
	 * list of the parent
	 */
	tchldnode->svalscalled = True;

	return ret;
}

/*
 * Function:	_NhlSetValuesChild
 *
 * Description:	This function is used from within a layer's methode to
 *		SetValues of the child.  It takes the args that were
 *		passed to the parent - that apply to the child ( as
 *		determined by the _NhlRegisterChildClass call) and combines
 *		the args passed by this call to do the set values of the
 *		child.  This avoids a race condition for resources that
 *		are dependant on eachother - if the parent set's some
 *		of them, and the user try's to set others of them.  This
 *		function must be called by the parents setvalues methode
 *		for every child that is registered with autosetvals equal
 *		to False.  If it is not, an error message will occur in
 *		_NhlSetValues.
 *
 * In Args:	
 *		int		pid,	pid return
 *		Layer		parent,	parent of child
 *		...			args to set in child
 *
 * Out Args:	
 *
 * Scope:	Global Layer writer
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*VARARGS2*/
NhlErrorTypes
_NhlSetValuesChild
#if	NeedVarArgProto
(
	int		pid,	/* pid return		*/
	Layer		parent,	/* parent of child	*/
	...			/* args to set in child	*/
)
#else
(pid,parent,va_alist)
	int		pid;	/* pid return		*/
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

	ret = SetValuesChild(pid,parent,vargs,num_vargs);

	(void)NhlFree(vargs);

	return ret;
}

/*
 * Function:	_NhlALSetValuesChild
 *
 * Description:	This function is used from within a layer's methode to
 *		SetValues of the child.  It takes the args that were
 *		passed to the parent - that apply to the child ( as
 *		determined by the _NhlRegisterChildClass call) and combines
 *		the args passed by this call to do the set values of the
 *		child.  This avoids a race condition for resources that
 *		are dependant on eachother - if the parent set's some
 *		of them, and the user try's to set others of them.  This
 *		function must be called by the parents setvalues methode
 *		for every child that is registered with autosetvals equal
 *		to False.  If it is not, an error message will occur in
 *		_NhlSetValues.
 *
 * In Args:	
 *		int		pid,		pid return
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
_NhlALSetValuesChild
#if	NhlNeedProto
(
	int		pid,		/* pid return		*/
	Layer		parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args		*/
)
#else
(pid,parent,args_in,nargs)
	int		pid;		/* pid return		*/
	Layer		parent;		/* parent of child	*/
	NhlSArgList	args_in;	/* args in		*/
	int		nargs;		/* number args		*/
#endif
{
	_NhlArgList		args = NULL;
	NhlErrorTypes		ret;

	_NhlSArgToSetArgList(&args,args_in,nargs);

	ret = SetValuesChild(pid,parent,args,nargs);

	(void)NhlFree(args);

	return ret;
}
