/*
 *      $Id: SetValues.c,v 1.30 1999-05-22 00:43:13 dbrown Exp $
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
 *			a NhlLayer instance via the SetValues call. Documentation
 *			is NhlDOCREF(/design/hlu/SetValues.html,here).
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/AppI.h>


/*
 * Function:	CallSetValues
 *
 * Description:	This function is used to call the SetValues methods of the
 *		given layer.
 *
 * In Args:	
 *		NhlClass	class,		Class of NhlLayer being set
 *		NhlLayer	oldl,		NhlLayer w/ old values
 *		NhlLayer	reql,		NhlLayer w/ requested values
 *		NhlLayer	newl,		NhlLayer to update
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
#if	NhlNeedProto
(
	NhlClass	class,		/* Class of NhlLayer being set	*/
	NhlLayer	oldl,		/* NhlLayer w/ old values	*/
	NhlLayer	reql,		/* NhlLayer w/ requested values	*/
	NhlLayer	newl,		/* NhlLayer to update		*/
	_NhlArgList	args,		/* res names and values to set	*/
	int		num_args	/* num of resources to set	*/
)
#else
(class,oldl,reql,newl,args,num_args) 
	NhlClass	class;		/* Class of NhlLayer being set	*/
	NhlLayer	oldl;		/* NhlLayer w/ old values	*/
	NhlLayer	reql;		/* NhlLayer w/ requested values	*/
	NhlLayer	newl;		/* NhlLayer to update		*/
	_NhlArgList	args;		/* res names and values to set	*/
	int		num_args;	/* num of resources to set	*/
#endif
{
	NhlErrorTypes ansestorerr=NhlNOERROR, thisclasserr=NhlNOERROR;

	if(class->base_class.superclass != NULL)
		ansestorerr = CallSetValues(class->base_class.superclass,
						oldl,reql,newl,args,num_args);

	if(ansestorerr < NhlWARNING)
		return(ansestorerr);

	if(class->base_class.layer_set_values != NULL)
		thisclasserr = (*(class->base_class.layer_set_values))
						(oldl,reql,newl,args,num_args);

	return(MIN(ansestorerr,thisclasserr));
}

/*
 * Function:	CallSetValuesHook
 *
 * Description:	This function is used to call the SetValues methods of the
 *		given layer.
 *
 * In Args:	
 *		NhlClass	class,		Class of NhlLayer being set
 *		NhlLayer	oldl,		NhlLayer w/ old values
 *		NhlLayer	reql,		NhlLayer w/ requested values
 *		NhlLayer	newl,		NhlLayer to update
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
CallSetValuesHook
#if	NhlNeedProto
(
	NhlClass	class,		/* Class of NhlLayer being set	*/
	NhlLayer	oldl,		/* NhlLayer w/ old values	*/
	NhlLayer	reql,		/* NhlLayer w/ requested values	*/
	NhlLayer	newl,		/* NhlLayer to update		*/
	_NhlArgList	args,		/* res names and values to set	*/
	int		num_args	/* num of resources to set	*/
)
#else
(class,oldl,reql,newl,args,num_args) 
	NhlClass	class;		/* Class of NhlLayer being set	*/
	NhlLayer	oldl;		/* NhlLayer w/ old values	*/
	NhlLayer	reql;		/* NhlLayer w/ requested values	*/
	NhlLayer	newl;		/* NhlLayer to update		*/
	_NhlArgList	args;		/* res names and values to set	*/
	int		num_args;	/* num of resources to set	*/
#endif
{
	NhlErrorTypes ansestorerr=NhlNOERROR, thisclasserr=NhlNOERROR;

	if(class->base_class.layer_set_values_hook != NULL)
		thisclasserr = (*(class->base_class.layer_set_values_hook))
						(oldl,reql,newl,args,num_args);

	if(class->base_class.superclass != NULL)
		ansestorerr = CallSetValuesHook(class->base_class.superclass,
						oldl,reql,newl,args,num_args);


	return(MIN(ansestorerr,thisclasserr));
}

/*
 * Function:	_NhlSetValues
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
 * Scope:	
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes 
_NhlSetValues
#if	NhlNeedProto
(
	_NhlConvertContext	context,/* convert context for mem	*/
	char*			base,	/* base address to write values to*/
	NrmResourceList		resources,/* resource list with offsets	*/
	int			num_res,/* number of resources		*/
	_NhlArgList		args,	/* names and values of resources*/
	int			nargs	/* number of args		*/
)
#else
(context,base,resources,num_res,args,nargs)
	_NhlConvertContext	context;/* convert context for mem	*/
	char*			base;	/* base address to write values to*/
	NrmResourceList		resources;/* resource list with offsets	*/
	int			num_res;/* number of resources		*/
	_NhlArgList		args;	/* names and values of resources*/
	int			nargs;	/* number of args		*/
#endif
{
	char		func[] = "_NhlSetValues";
	register int	i,j;
	NhlBoolean	argfound[_NhlMAXARGLIST];
	NhlErrorTypes	ret = NhlNOERROR;

	/*
	 * all args could have been used in children
	 */
	if(nargs == 0)
		return NhlNOERROR;

	/* Mark each arg as not found */ 
	memset((char*)argfound,0,(nargs * sizeof(NhlBoolean))); 
		 
	for(i=0,j=0; i < nargs; i++){
		while(j < num_res){
			if(args[i].quark > resources[j].nrm_name){
				j++;
				continue;
			}
			if(args[i].quark < resources[j].nrm_name)
				break;

			if(resources[j].res_info & _NhlRES_NOSACCESS){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:%s does not have \"S\" access",
					func,NrmQuarkToString(args[i].quark));
				args[i].quark = NrmNULLQUARK;
			}
			else if((args[i].type == NrmNULLQUARK) &&
				!_NhlConvertArg(&args[i],resources[j].nrm_type,
							resources[j].nrm_size)){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:Unable to determine type of variable that set %s",
					func,NrmQuarkToString(args[i].quark));
				args[i].quark = NrmNULLQUARK;
			}
			else if(args[i].type==resources[j].nrm_type){
				_NhlCopyFromArgVal(args[i].value,
					(char*)(base + resources[j].nrm_offset),
					resources[j].nrm_size);
			}
			else{
				/* 
				 * call converter
				 */
				NrmValue	from, to;

				from.size = args[i].size;
				from.data = args[i].value;
				to.size = resources[j].nrm_size;
				to.data.ptrval = ((char*)base +
						resources[j].nrm_offset);

				if(NhlWARNING >_NhlConvertData(context,
							args[i].type,
							resources[j].nrm_type,
							&from, &to)){
					
					NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Error retrieving resource %s from args - Ignoring Arg",
					NrmNameToString(resources[j].nrm_name));
					ret = MIN(NhlWARNING,ret);
				}
			}
			argfound[i] = True;
			break;
		}

		if(!argfound[i]){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s is not a resource in the given object",
						NrmNameToString(args[i].quark));
			ret = MIN(ret,NhlWARNING);
		}
	}

	return(ret);
}

/*
 * Function:	_NhlSetLayerValues
 *
 * Description:	This function sets the resources specified by the args passed
 *		in the layer passed.
 *
 * In Args:	_NhlArgList	args;	resource names and values to change
 *		int		nargs;	length of arg array
 *
 * Out Args:	NhlLayer		l;	layer to set values in.
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	The layer is modified to set the requested values
 */
NhlDOCTAG(_NhlSetLayerValues)
NhlErrorTypes
_NhlSetLayerValues
#if	NhlNeedProto
(
	NhlLayer	l,		/* layer instance	*/
	_NhlArgList	args,		/* args to change	*/
	int		nargs		/* number of args	*/
)
#else
(l,args,nargs)
	NhlLayer	l;		/* layer instance	*/
	_NhlArgList	args;		/* args to change	*/
	int		nargs;		/* number of args	*/
#endif
{
	char			func[]="_NhlSetLayerValues";
	int			i;
	NhlLayer		oldl, reql;
	NhlClass		lc = _NhlClass(l);
	NhlErrorTypes		ret=NhlNOERROR, lret=NhlNOERROR;
	_NhlArg			stackargs[_NhlMAXARGLIST];
	_NhlArgList		largs=stackargs;
	int			nlargs;
	_NhlChildArgList	child_args=NULL;
	NhlBoolean		child_args_used[_NhlMAXARGLIST];
	_NhlChildArgList	targnode=NULL;
	_NhlChildList		tchldnode=NULL;
	_NhlConvertContext	context;
	NhlArgVal		cbdata;
	static NhlBoolean	first = True;
	static NrmQuark		qupdate_req;

	if(l == NULL){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:passed a NULL object",func));
		return NhlFATAL;
	}

	if(nargs == 0)
		return NhlNOERROR;

	/*
	 * Obj's don't support resource forwarding.
	 */
	if(_NhlIsObj(l)){
		largs = args;
		nlargs = nargs;
	}

	else{

		if(_NhlIsApp(l)){
			lret = _NhlSortAppArgs(l,args,nargs,&largs,&nlargs);
			if(lret < NhlWARNING){
				NhlPError(lret,NhlEUNKNOWN,
					"%s:Unable to sort Arg List",func);
				return lret;
			}
			l->base.child_args = NULL;
			for(i=0;i<nargs;i++)
				child_args_used[i] = True;
		}
		else{
			/*
			 * Sort the args into args for instance and for it's
			 * children. If there are no children it just copies
			 * the args to largs
			 */
			lret =_NhlSortChildArgs(l,args,nargs,&largs,&nlargs,
					&child_args,child_args_used,False);
			if(lret < NhlWARNING){
				NhlPError(lret,NhlEUNKNOWN,
				"Unable to Create Arg Lists - Can't SetValues");
				return lret;
			}
			ret = MIN(ret,lret);
			l->base.child_args = child_args;
		}

		/*
		 * If this layer has children forward args to them if
		 * autosetval = True.
		 */
		if(l->base.children != NULL){
			tchldnode = l->base.children;

			while(tchldnode != NULL){

				targnode = l->base.child_args;

				while((targnode != NULL) &&
					(tchldnode->theclass != targnode->theclass))
					targnode = targnode->next;
			
				if(targnode == NULL){
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"SetValues can't find args to set child's resources %s",
					_NhlClassName(tchldnode->theclass)));
					tchldnode = tchldnode->next;
					continue;
				}

				if(targnode->autosetval){
					lret = _NhlSetLayerValues(
						_NhlGetLayer(tchldnode->pid),
						targnode->args,targnode->nargs);
					if(lret < NhlWARNING){
						NHLPERROR((lret,NhlEUNKNOWN,
				"SetValues can't set values of hidden child %s",
						NhlName(tchldnode->pid)));
						tchldnode = tchldnode->next;
						continue;
					}
					ret = MIN(lret,ret);

					for(i=0;i<targnode->nargs;i++)
						*(targnode->args_used[i]) =True;

					tchldnode->svalscalled = True;
				}

				tchldnode = tchldnode->next;
			}
		}
	}

/*
 * context is a structure that remembers the memory that is allocated
 * by any converters on behalf of this object.  It needs to be free'd -
 * along with all that memory after the SetValues method has had a chance
 * to copy the memory.
 */
	context = _NhlCreateConvertContext(l);

	oldl = (NhlLayer)NhlMalloc((unsigned)lc->base_class.layer_size);
	reql = (NhlLayer)NhlMalloc((unsigned)lc->base_class.layer_size);

	if((oldl == NULL) || (reql == NULL) || (context == NULL)){
		NhlPError(NhlFATAL,ENOMEM,"Unable to set values of NhlLayer %s",
							NhlName(l->base.id));
		(void)NhlFree(oldl);
		(void)NhlFree(reql);
		_NhlFreeChildArgs(child_args);
		return NhlFATAL;
	}

	memcpy((char*)oldl,(char*)l,(int)lc->base_class.layer_size);

	lret = _NhlSetValues(context,(char*)l,
				(NrmResourceList)(lc->base_class.resources),
				lc->base_class.num_resources, largs,nlargs);

	if (lret < NhlWARNING) {
/*
 * When fatal error occurs destroy l - it is in an unpredictable state.
 */
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to set values of PID #%d",
							l->base.id);
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"PID #%d Destroyed to recover from errors",l->base.id);
		_NhlFreeChildArgs(child_args);
		_NhlFreeConvertContext(context);
		(void)NhlDestroy(l->base.id);
		(void)NhlFree(oldl);
		(void)NhlFree(reql);
	} 
	ret = MIN(ret,lret);

	memcpy((char*)reql,(char*)l,(int)lc->base_class.layer_size);

	lret = CallSetValues(lc,oldl,reql,l,largs,nlargs);
	ret = MIN(lret,ret);

	lret = CallSetValuesHook(lc,oldl,reql,l,largs,nlargs);
	ret = MIN(lret,ret);

	/*
	 * memory should have been copied in CallSetValues.
	 */
	_NhlFreeConvertContext(context);
/*
 * LOOP threw child_list and make sure they have all had setvalues called
 * on them - if not print out an error. - Also reset svalscalled to False
 * for next SetValues call.
 */

	if(!_NhlIsObj(l)){

		tchldnode = l->base.children;
		while(tchldnode != NULL){
			if(!tchldnode->svalscalled){
				NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"SetValuesChild never occurred on %s: Error in %s",
						NhlName(tchldnode->pid),
						_NhlClassName(_NhlClass(l))));
			}
			tchldnode->svalscalled = False;

			tchldnode = tchldnode->next;
		}

		_NhlFreeChildArgs(child_args);
		l->base.child_args = NULL;

		for(i=0;i<nargs;i++){
			if(!child_args_used[i]){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s is not a valid resource in %s at this time",
				NrmNameToString(args[i].quark),_NhlName(l));
				ret = MIN(ret,NhlWARNING);
			}
		}
	}

	/*
	 * This is a hack to eliminate a bad slow-down in the GUI, there's
	 * got to be a better way. But for now...
	 */

	if (first) {
		qupdate_req = NrmStringToQuark(".pmUpdateReq");
		first = False;
	}
	if (nargs > 1 || args[0].quark != qupdate_req) {
		NhlINITVAR(cbdata);
		cbdata.ptrval = (NhlPointer)oldl;
		_NhlIterateObjCallbacks(l,_NhlCBobjValueSet,_NhlcbCALL,cbdata);
	}

	(void)NhlFree(oldl);
	(void)NhlFree(reql);

	return ret;
}

/*
 * Function:	NhlVASetValues
 *
 * Description:	This function sets the resources specified by the name/value
 *		pairs passed in threw the varargs.
 *		Internal SetValues function is NhlDOCREF(#_NhlSetLayerValues,here).
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
NhlDOCTAG(NhlVASetValues)
/*VARARGS1*/
NhlErrorTypes
NhlVASetValues
#if	NhlNeedVarArgProto
(
	int		id,		/* plot id		*/
	...
)
#else
(id,va_alist)
	int		id;		/* plot id		*/
	va_dcl
#endif	/* NhlNeedVarArgProto */
{
        va_list         ap; 
	int             num_args; 
	_NhlArg		args[_NhlMAXARGLIST];
	NhlErrorTypes	ret;
	NhlLayer	l = NULL;

	/* create an arglist from the varargs */
	VA_START(ap,id); 
	num_args = _NhlVarToSetArgList(ap,args); 
	va_end(ap); 

	l = _NhlGetLayer(id);
	if(l == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"PID #%d can't be found in NhlVASetValues",id);
		return(NhlFATAL);
	}
	ret = _NhlSetLayerValues(l,args,num_args);

	return(ret);
}

/*
 * Function:	NhlALSetValues
 *
 * Description:	This function sets the resources specified by the SArgList
 *		passed to it.
 *		Internal SetValues function is NhlDOCREF(#_NhlSetLayerValues,here).
 *
 * In Args:	int		id;	Index into list of layers
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	The layer indexed by id, is modified to set the requested values
 */
NhlDOCTAG(NhlALSetValues)
/*VARARGS1*/
NhlErrorTypes
NhlALSetValues
#if	NhlNeedProto
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
	_NhlArg		args[_NhlMAXARGLIST];
	NhlErrorTypes	ret;
	NhlLayer		l = NULL;

	/* create an arglist from the sargs */
	_NhlSArgToSetArgList(args,args_in,nargs); 

	l = _NhlGetLayer(id);
	if(l == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"PID #%d can't be found in NhlALSetValues",id);
		return(NhlFATAL);
	}
	ret = _NhlSetLayerValues(l, args, nargs);

	return(ret);
}

/*
 * Function:	NhlSetValues
 *
 * Description:	This function sets the resources specified by the RL List
 *		passed to it.
 *		Internal SetValues function is NhlDOCREF(#_NhlSetLayerValues,here).
 *
 * In Args:
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
#if	NhlNeedProto
(
	int		id,		/* plot id		*/
	int		rlid		/* RL id		*/
)
#else
(id,rlid)
	int		id;		/* plot id		*/
	int		rlid;		/* RL id		*/
#endif
{
	_NhlArg		args[_NhlMAXARGLIST];
	int		nargs;
	NhlLayer	l = _NhlGetLayer(id);

	if(l == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"PID #%d can't be found in NhlSetValues",id);
		return(NhlFATAL);
	}

	/* create an arglist from the reslist */
	if(!_NhlRLToArgList(rlid,NhlSETRL,args,&nargs)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"NhlSetValues:Invalid RL id %d",rlid);
		return NhlFATAL;
	}

	return _NhlSetLayerValues(l, args, nargs);
}

/*
 * Function:	nhlpfsetvalues
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Fortran
 * Returns:	err_ret gets NhlErrorTypes.
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfsetvalues,NHLPFSETVALUES)
#if	NhlNeedProto
(
	int		*pid,		/* plot id <return>		*/
	int		*rlid,		/* RL list of resources		*/
	int		*err_ret	/* error return			*/
)
#else
(pid,rlid,err_ret)
	int		*pid;		/* plot id <return>		*/
	int		*rlid;		/* RL list of resources		*/
	int		*err_ret;	/* error return			*/
#endif
{
	*err_ret = NhlSetValues(*pid,*rlid);

	return;
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
 *		NhlLayer	parent,		parent of child
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
#if	NhlNeedProto
(
	int		pid,		/* pid			*/
	NhlLayer	parent,		/* parent of child	*/
	_NhlArgList	sargs,		/* resources to set	*/
	int		num_sargs	/* number of res to set	*/
)
#else
(pid,parent,sargs,num_sargs)
	int		pid;		/* pid			*/
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
	_NhlChildArgList	targnode=NULL;
	NhlErrorTypes		ret=NhlNOERROR;
	_NhlChildList		tchldnode=NULL;
	NhlLayer		child = _NhlGetLayer(pid);

	if(_NhlIsObj(parent)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"ChildSetValues:%s is a sub-class of Obj so it can't have children",
						NhlName(parent->base.id));
		return NhlFATAL;
	}

	if(child == NULL){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Unable to retrieve child PID#%d",
									pid));
		return NhlFATAL;
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
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"can't find child record for PID#%d in parent",pid));
		return NhlFATAL;
	}

	/*
	 * retrieve args passed by parent
	 */
	targnode = parent->base.child_args;

	while(targnode != NULL){
		if(targnode->theclass == tchldnode->theclass){
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

	if(targnode == NULL){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"can't find forwarded args to set resources for %s",
					_NhlClassName(tchldnode->theclass)));
		return NhlFATAL;
	}

	/*
	 * merge the pargs and sargs into a single args list for _NhlSetLayerValues
	 */
	_NhlMergeArgLists(args,&num_args,sargs,num_sargs,pargs,num_pargs);

	/*
	 * SetValues of the child
	 */
	ret = _NhlSetLayerValues(child,args,num_args);

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
 *		NhlLayer		parent,	parent of child
 *		...			args to set in child
 *
 * Out Args:	
 *
 * Scope:	Global NhlLayer writer
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(_NhlVASetValuesChild)
/*VARARGS2*/
NhlErrorTypes
_NhlVASetValuesChild
#if	NhlNeedVarArgProto
(
	int		pid,	/* pid return		*/
	NhlLayer	parent,	/* parent of child	*/
	...			/* args to set in child	*/
)
#else
(pid,parent,va_alist)
	int		pid;	/* pid return		*/
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

	ret = SetValuesChild(pid,parent,vargs,num_vargs);

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
NhlDOCTAG(_NhlALSetValuesChild)
NhlErrorTypes
_NhlALSetValuesChild
#if	NhlNeedProto
(
	int		pid,		/* pid return		*/
	NhlLayer	parent,		/* parent of child	*/
	NhlSArgList	args_in,	/* args in		*/
	int		nargs		/* number args		*/
)
#else
(pid,parent,args_in,nargs)
	int		pid;		/* pid return		*/
	NhlLayer	parent;		/* parent of child	*/
	NhlSArgList	args_in;	/* args in		*/
	int		nargs;		/* number args		*/
#endif
{
	_NhlArg			args[_NhlMAXARGLIST];
	NhlErrorTypes		ret;

	_NhlSArgToSetArgList(args,args_in,nargs);

	ret = SetValuesChild(pid,parent,args,nargs);

	return ret;
}

static NhlLayer
GetResLayer 
#if	NhlNeedProto
(
	NhlLayer	l,
	NrmQuark	resq,
	NrmResource	**res
)
#else
(l,resq,res)
	NhlLayer	l;
	NrmQuark	resq;
	NrmResource	**res;
#endif
{
	NhlClass		lc = _NhlClass(l);
	NrmResourceList 	reslist = 
		(NrmResourceList) lc->base_class.resources;
	int			num_res = lc->base_class.num_resources;
	_NhlChildResList	child_reslist;
	_NhlChildList		childlist;
	NhlClass		child_class = NULL;
	int			i;
	NhlBoolean		found = False;

	for (i = 0; i < num_res; i++) {
		if (resq == reslist[i].nrm_name) {
			found = True;
			*res = &reslist[i];
			return(l);
		}
	}
	if (_NhlIsObj(l))
		return NULL;

	child_reslist = lc->base_class.child_resources;
	while (child_reslist) {
		if (NrmQinQList(child_reslist->resources,resq)) {
			child_class = child_reslist->theclass;
			break;
		}
		child_reslist = child_reslist->next;
	}
	if (!child_class)
		return NULL;

	childlist = l->base.children;
	while (childlist) {
		if (child_class == childlist->theclass) {
			break;
		}
		childlist = childlist->next;
	}
	if (!childlist) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "Internal HLU lib error");
		return NULL;
	}
	return (GetResLayer(_NhlGetLayer(childlist->pid),resq,res));
}

void ForwardedValueSetCB(NhlArgVal cbdata, NhlArgVal udata)

{
	_NhlValSetCBInfo	vsinfo = (_NhlValSetCBInfo) udata.ptrval;

	vsinfo->forwarded_value_set = True;
	return;
}

static _NhlValSetCBInfo 
AddValueSetCB
#if	NhlNeedProto
(
	NhlLayer	l,
	NrmQuark	resq
)
#else
(l,resq)
	NhlLayer	l;
	NrmQuark	resq;
#endif
{
	_NhlValSetCBInfo	vsinfo;
	NhlLayer		resl;
	NrmResource		*res = NULL;
	_NhlCB			cb;
	NhlArgVal		sel;
	NhlArgVal		udata;

        if (resq == NrmNULLQUARK)
                resl = l;
        else {
                resl = GetResLayer(l,resq,&res);
                if (! resl)
                        return NULL;
        }

	vsinfo = NhlMalloc(sizeof(_NhlValSetCBInfoRec));
	
	if (!vsinfo) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}

	vsinfo->nameq = resq;
	vsinfo->resl = resl;
	vsinfo->resl_id = resl->base.id;
	vsinfo->forwarded_value_set = False;
	if (resl != l) {
		vsinfo->forwarded = True;
		sel.lngval = resq;
		udata.ptrval = vsinfo;
		cb =  _NhlAddObjCallback(resl,"CBobjValueSet",sel,
					 ForwardedValueSetCB,udata);
		if (! cb) {
			NhlFree(vsinfo);
			return NULL;
		}
		vsinfo->forwarded_cb = cb;
	}
	else if (! res) {
		vsinfo->forwarded = False;
		vsinfo->forwarded_cb = NULL;
		vsinfo->offset = 0;
		vsinfo->size = 0;
	}
        else {
		vsinfo->forwarded = False;
		vsinfo->forwarded_cb = NULL;
		vsinfo->offset = res->nrm_offset;
		vsinfo->size = res->nrm_size;
	}
	return vsinfo;
}
NhlErrorTypes _NhlResValueSetCBTask
#if	NhlNeedProto
(
	NhlPointer	procdata,
	_NhlCBTask	task,
	NhlArgVal	selector,
	NhlBoolean	*do_it,					
	NhlArgVal	*cbdata,				     
	NhlPointer	*cbnode_data
)
#else
(procdata,task,selector,do_it,cbdata,cbnode_data)
	NhlPointer	procdata;
	_NhlCBTask	task;
	NhlArgVal	selector;
	NhlBoolean	*do_it;					
	NhlArgVal	*cbdata;				     
	NhlPointer	*cbnode_data;
#endif
{
	NhlLayer l = (NhlLayer) procdata;
	NrmQuark resq = (NrmQuark)selector.lngval;
 	_NhlValSetCBInfo vsinfo;
	static _NhlValueSetCBDataRec vsdata;
	char *base, *oldbase;

	switch (task) {
	case _NhlcbCALL:
		*do_it = False;
		if (! cbnode_data || ! *cbnode_data) {
			return NhlNOERROR;
		}
		vsinfo = (_NhlValSetCBInfo) *cbnode_data;
		if (vsinfo->forwarded) {
			if (! vsinfo->forwarded_value_set)
				return NhlNOERROR;
			else
				vsinfo->forwarded_value_set = False;
		}
		else if (vsinfo->size > 0) {
			base = (char *) vsinfo->resl;
			oldbase = (char *) (*cbdata).ptrval;
			if (! memcmp(base+vsinfo->offset,
				     oldbase+vsinfo->offset,vsinfo->size)) {
				return NhlNOERROR;
			}
		}
		vsdata.id = l->base.id;
		vsdata.resq = resq;
		(*cbdata).ptrval = &vsdata;
		*do_it = True;
		return NhlNOERROR;
	case _NhlcbADD:	
		*cbnode_data = (NhlPointer)AddValueSetCB(l,resq);
		if (! *cbnode_data) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "SetValues CB not installed for %s",
				  NrmQuarkToString(resq));
			*do_it = False;
			return NhlWARNING;
		}
		*do_it = False;
		return NhlNOERROR;
	case _NhlcbDELETE:
		if (!cbnode_data || ! *cbnode_data)
			return NhlNOERROR;

		vsinfo = (_NhlValSetCBInfo) *cbnode_data;
		if (vsinfo->forwarded && 
		    (vsinfo->resl == _NhlGetLayer(vsinfo->resl_id)))
			_NhlCBDelete(vsinfo->forwarded_cb);
			
		NhlFree(*cbnode_data);
		*do_it = False;
		return NhlNOERROR;
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "Invalid enumeration value for callback task");
		break;
	}
	return NhlNOERROR;
}
