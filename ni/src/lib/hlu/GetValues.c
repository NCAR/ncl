/*
 *      $Id: GetValues.c,v 1.7 1994-03-23 15:27:25 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		GetValues.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 2 14:12:13 MDT 1992
 *
 *	Description:	This file contains all the functions neccessary to
 *			retrieve a resource value from a layer instance.
 *			Design docs for GetValues is
 *			NhlDOCREF(/design/hlu/SetValues.html,here).
 */
#include <string.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>

static	NrmQuark	stringQ;
static	NrmQuark	genQ;

/*
 * Function:	GetValues
 *
 * Description:	This function retrieves values addressed by base + resource
 *		offset.  It retrieves the resources specifed by the arg names
 *		into the address space provided in the arglist.
 *
 * In Args:	char*		base;		base address to copy vals from
 *		NrmResourceList	resources;	resource list with offsets
 *		int		num_res;	number of resources
 *		_NhlArgList	args;		names of resources to retrieve
 *		int		nargs;		number of args
 *
 * Out Args:	_NhlArgList	args;		resource vals copied into value
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes 
GetValues
#if	__STDC__
(
 	char*		base,		/* base address to copy vals from */
	NrmResourceList	resources,	/* resource list with offsets	*/
	int		num_res,	/* number of resources		*/
	_NhlArgList	args,		/* names of resources to retrieve*/
	int		nargs		/* number of args		*/
)
#else
(base,resources,num_res,args,nargs)
 	char*		base;		/* base address to copy vals from */
	NrmResourceList	resources;	/* resource list with offsets	*/
	int		num_res;	/* number of resources		*/
	_NhlArgList	args;		/* names of resources to retrieve*/
	int		nargs;		/* number of args		*/
#endif
{
	register int	i,j;
	NhlBoolean	argfound[_NhlMAXARGLIST];
	NhlErrorTypes	ret = NhlNOERROR;

	/*
	 * All args could have been taken by children
	 */
	if(nargs == 0)
		return NhlNOERROR;

	/* Mark each arg as not found */
	memset((char*)argfound,0,(nargs * sizeof(NhlBoolean)));

	for(i=0; i < nargs; i++){
		for(j=0; j < num_res; j++){
			if(args[i].quark == resources[j].nrm_name){
				if(args[i].type != NrmNULLQUARK){
					*args[i].type_ret=resources[j].nrm_type;
					*args[i].size_ret=resources[j].nrm_size;
					if(*args[i].type_ret == stringQ){
						*args[i].free_func =
							(_NhlFreeFunc)NhlFree;
					}
					else if(*args[i].type_ret == genQ){
						*args[i].free_func =
						(_NhlFreeFunc)NhlFreeGenArray;
					}
				}
				_NhlCopyToArg((NhlPointer)
					(base + resources[j].nrm_offset),
					&args[i].value,
					resources[j].nrm_size);
				argfound[i] = True;
				break;
			}
		}
		if(!argfound[i]){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s isn't a resource in this object",
						NrmNameToString(args[i].quark));
			ret = MIN(ret,NhlWARNING);
		}
	}

	return(ret);
}

/*
 * Function:	CallGetValues
 *
 * Description:	This function calls the GetValues methode of the class of the
 *		layer instance and of every superclass. This is needed for
 *		the instance to put a copy of the data requested, instead
 *		of a pointer into the internal data structure if the
 *		class wants to.
 *
 * In Args:	NhlLayerClass	lc;	class pointer
 *		NhlLayer	l;	instance pointer
 *		_NhlArgList	args;	resources retrieving as well as values
 *		int		nargs;	number of args
 *
 * Out Args:	_NhlArgList	args;	resources retrieving as well as values
 *
 * Scope:	static
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes 
CallGetValues
#if	__STDC__
(
	NhlLayerClass	lc,	/* class pointer	*/
	NhlLayer	l,	/* instance pointer	*/
	_NhlArgList	args,	/* resources retrieving	*/
	int		nargs	/* number of args	*/
)
#else
(lc,l,args,nargs)
	NhlLayerClass	lc;	/* class pointer	*/
	NhlLayer	l;	/* instance pointer	*/
	_NhlArgList	args;	/* resources retrieving	*/
	int		nargs;	/* number of args	*/
#endif
{
	NhlErrorTypes ansestorerr=NhlNOERROR, thisclasserr=NhlNOERROR;

	if(lc->base_class.superclass != NULL){
		ansestorerr = CallGetValues(lc->base_class.superclass,
								l,args,nargs);
		if(ansestorerr < NhlWARNING)
			return(ansestorerr);
	}

	if(lc->base_class.layer_get_values != NULL)
		thisclasserr = (*(lc->base_class.layer_get_values))
							(l, args, nargs);
	return(MIN(ansestorerr,thisclasserr));

}

/*
 * Function:	_NhlGetValues
 *
 * Description:	This function retrieves the resources specified by the
 *		args parameter from the layer instance into the value
 *		part of the args.
 *
 * In Args:	NhlLayer	l;	layer to get values from
 *		int		nargs;	length of arg array 
 *
 * Out Args:	_NhlArgList	args;	resource names and values to retrieve 
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(_NhlGetValues)
static NhlErrorTypes
_NhlGetValues
#if	__STDC__
(
	NhlLayer	l,		/* layer instance	*/
	_NhlArgList	args,		/* args to retrieve	*/
	int		nargs		/* number of args	*/
)
#else
(l,args,nargs)
	NhlLayer	l;		/* layer instance	*/
	_NhlArgList	args;		/* args to retrieve	*/
	int		nargs;		/* number of args	*/
#endif
{
	int			i;
	NhlLayerClass		lc = _NhlClass(l);
	_NhlArg		stackargs[_NhlMAXARGLIST];
	_NhlArgList		largs=stackargs;
	int			nlargs;
	_NhlChildArgList	targnode=NULL;
	_NhlChildArgList	chld_args=NULL;
	NhlBoolean		chld_args_used[_NhlMAXARGLIST];
	_NhlChildList		tchldnode=NULL;
	NhlErrorTypes		ret= NhlNOERROR, lret = NhlNOERROR;

	if(l == NULL){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"_NhlGetValues was passed a NULL layer"));
		return NhlFATAL;
	}

	if (nargs == 0) return(NhlNOERROR);

	if (args == NULL) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"GetValues called with NULL arglist"));
		return(NhlFATAL);
	}

	if(_NhlIsObj(l)){
		largs = args;
		nlargs = nargs;
	}

	else{
		/*
		 * Sort the args into args for this layer and it's children
		 * If there are no children it just copies the args to largs
		 */
		lret = _NhlSortChildArgs(l,args,nargs,&largs,&nlargs,&chld_args,
							chld_args_used,True);
		if(lret < NhlWARNING){
			NhlPError(lret,NhlEUNKNOWN,
				"Unable to Sort Arg Lists - Can't GetValues");
			return lret;
		}
		ret = MIN(ret,lret);

		/*
		 * If this layer has children forward args to them
		 */
		if(l->base.children != NULL){

			tchldnode = l->base.children;

			while(tchldnode != NULL){

				targnode = chld_args;

				while((targnode != NULL) &&
					(tchldnode->class != targnode->class))
					targnode = targnode->next;

				if(targnode == NULL){
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"GetValues can't find args to get child's resources"));
					return NhlFATAL;
				}

				lret = _NhlGetValues(
						_NhlGetLayer(tchldnode->pid),
						targnode->args,targnode->nargs);
				if(lret < NhlWARNING){
					NHLPERROR((lret,NhlEUNKNOWN,
				"GetValues can't get values of hidden child %s",
						NhlName(tchldnode->pid)));
					return lret;
				}
				ret = MIN(ret,lret);

				for(i=0;i<targnode->nargs;i++){
					*(targnode->args_used[i]) = True;
				}

				tchldnode = tchldnode->next;
			}
			_NhlFreeChildArgs(chld_args);
			for(i=0;i<nargs;i++){
				if(!chld_args_used[i]){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s is not a valid resource in %s at this time",
				NrmNameToString(args[i].quark),_NhlName(l));
					ret = MIN(ret,NhlWARNING);
				}
			}
		}
	}

	lret = GetValues((char*)l,(NrmResourceList)(lc->base_class.resources),
				lc->base_class.num_resources, largs, nlargs);

	if(lret != NhlFATAL) {
		ret = MIN(ret,lret);
		lret = CallGetValues(lc,l,args,nargs);
	}

	return MIN(ret,lret);
}

/*
 * Function:	NhlGetValues
 *
 * Description:	This function retrieves the resources specified by the
 *		resource name/addr pairs given in the RL.  It retrieves
 *		the given resource from the layer specified and puts the
 *		value in the space pointed to by the addr
 *
 * In Args:	int		pid;	id for layer to get values from
 *		...			name part of resource name/addr pairs
 *
 * Out Args:	...			*addr of resource name/addr pairs
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(NhlGetValues)
/*VARARGS1*/
NhlErrorTypes
NhlGetValues
#if	__STDC__
(
	int		pid,		/* id for layer instance	*/
	int		rlid		/* RL id			*/
)
#else
(pid,rlid)
	int		pid;		/* id for layer instance	*/
	int		rlid;		/* RL id			*/
#endif
{
	_NhlArg			args[_NhlMAXARGLIST];
	_NhlArg			gargs[_NhlMAXARGLIST];
	_NhlGArgExtra		gextra[_NhlMAXARGLIST];
	int			nargs,i;
	NhlLayer		l = _NhlGetLayer(pid);
	NhlErrorTypes		ret = NhlNOERROR;
	_NhlConvertContext	context=NULL;

	if(l == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"NhlGetValues:PID #%d is invalid",pid);
		return NhlFATAL;
	}

	if(!_NhlRLToArgList(rlid,NhlGETRL,args,&nargs)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NhlGetValues:Invalid RL id %d",
									rlid);
		return NhlFATAL;
	}

	/*
	 * Add indirection information for free_func and type_ret.
	 */
	for(i=0;i<nargs;i++){
		gargs[i].quark = args[i].quark;
		gargs[i].value.ptrval = &gextra[i].value_ret;
		gargs[i].type = args[i].type;
		gextra[i].type_ret = NrmNULLQUARK;
		gextra[i].free_func = NULL;
		gargs[i].type_ret = &gextra[i].type_ret;
		gargs[i].size_ret = &gextra[i].size_ret;
		gargs[i].free_func = &gextra[i].free_func;
	}

	ret = _NhlGetValues(l,gargs,nargs);

	if(ret < NhlWARNING)
		return ret;

	/*
	 * Convert data. Freeing objects data if necessary.
	 */
	for(i=0;i<nargs;i++){
		/*
		 * if type is NrmNULLQUARK then don't do conversion stuff.
		 */
		if((gargs[i].type == NrmNULLQUARK) ||
					(gargs[i].type == gextra[i].type_ret)){
			_NhlCopyFromArg(gextra[i].value_ret,
				args[i].value.ptrval,gextra[i].size_ret);
		}
		else if(_NhlConverterExists(gextra[i].type_ret,args[i].type)){
			/*
			 * Call Converter
			 */
			NrmValue	from, to;

			from.size = gextra[i].size_ret;
			from.data = gextra[i].value_ret;
			/*
			 * This size is kind of bogus - I really don't know
			 * how large the space is that the users pointer
			 * is addressing.  In practice it isn't so bad since
			 * the user told us the "Name" of the type they are
			 * pointing at, and the converter function should
			 * know what type it is pointing at.  _NhlArgVal
			 * should be the "largest" size supported - and it
			 * is since it's a union of all the supported types.
			 */
			to.size = sizeof(_NhlArgVal);
			to.data = args[i].value;

			if(context == NULL)
				context = _NhlCreateConvertContext();

			if(NhlNOERROR != _NhlConvertData(context,
				gextra[i].type_ret,args[i].type,&from,&to)){

				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"NhlGetValues:Error retrieving %s",
					NrmQuarkToString(args[i].quark));

				ret = MIN(ret,NhlWARNING);
			}

			/*
			 * if converted to a string - copy memory since we
			 * want the user to own it.
			 */
			if(args[i].type == stringQ){
				NhlString	tstr =
					*(NhlString *)args[i].value.ptrval;
				*(NhlString *)args[i].value.ptrval =
						NhlMalloc(strlen(tstr) + 1);
				if(*(NhlString *)args[i].value.ptrval == NULL){
					NhlPError(NhlWARNING,ENOMEM,
					"NhlGetValues:Unable to retrieve %s",
					NrmQuarkToString(args[i].quark));
					ret = MIN(ret,NhlWARNING);
				}
				else
					strcpy(
					*(NhlString*)args[i].value.ptrval,
									tstr);
			}

			if(gextra[i].free_func != NULL)
				(*(gextra[i].free_func))
					(gextra[i].value_ret.ptrval);
		}
		else{
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"NhlGetValues:%s unretrievable: Can't convert a \"%s\" to a \"%s\"",
					NrmQuarkToString(args[i].quark),
					NrmQuarkToString(gextra[i].type_ret),
						NrmQuarkToString(args[i].type));
			ret = MIN(ret,NhlWARNING);
		}
	}

	_NhlFreeConvertContext(context);

	return ret;
}

/*
 * Function:	NhlVAGetValues
 *
 * Description:	This function retrieves the resources specified by the
 *		resource name/addr pairs given in the varargs.  It retrieves
 *		the given resource from the layer specified and puts the
 *		value in the space pointed to by the addr
 *		Internal GetValues function is NhlDOCREF(#_NhlGetValues,here).
 *
 * In Args:	int		pid;	id for layer to get values from
 *		...			name part of resource name/addr pairs
 *
 * Out Args:	...			*addr of resource name/addr pairs
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(NhlVAGetValues)
/*VARARGS1*/
NhlErrorTypes
NhlVAGetValues
#if	NeedVarArgProto
(
	int		pid,		/* id for layer instance*/
	...				/* res/addr pairs	*/
)
#else
(pid,va_alist)
	int		pid;		/* id for layer instance*/
	va_dcl				/* res/addr pairs	*/
#endif	/* NeedVarArgProto */
{
	va_list         ap;
	NhlString	name;
	int             i = 0;
	_NhlArg		args[_NhlMAXARGLIST];
	NhlLayer	l = _NhlGetLayer(pid);
	NrmQuark	type_ret;
	_NhlFreeFunc	free_func;
	unsigned int	size_ret;

	if(l == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlVAGetValues:Unable to access object w/ID %d",pid);
		return(NhlFATAL);
	}

	VA_START(ap,pid); 
	for(name = va_arg(ap,NhlString); name != NULL;
						name = va_arg(ap,NhlString)){

		if(i >= _NhlMAXARGLIST){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlVAGetValues:Only %d args can be passed in-Ignoring rest",
								_NhlMAXARGLIST);
			break;
		}

		args[i].quark = NrmStringToQuark(name);
		args[i].value.ptrval = va_arg(ap,NhlPointer);
		args[i].type_ret = &type_ret;
		args[i].size_ret = &size_ret;
		args[i].free_func = &free_func;
		args[i].type = NrmNULLQUARK;
		i++;
	}
	va_end(ap); 

	return _NhlGetValues(l,args,i);
}

/*
 * Function:	NhlALGetValues
 *
 * Description:	This function retrieves the resources specified by the
 *		resource name/addr pairs given in the varargs.  It retrieves
 *		the given resource from the layer specified and puts the
 *		value in the space pointed to by the addr
 *
 * In Args:
 *		int		pid,		id for layer to get values from
 *		NhlGArgList	gargs,		GArg list
 *		int		nargs		num of GArg's
 *
 * Out Args:
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(NhlALGetValues)
NhlErrorTypes
NhlALGetValues
#if	__STDC__
(
	int		pid,		/* id for layer instance	*/
	NhlGArgList	gargs,		/* GArg list 			*/
	int		nargs		/* num of GArg's		*/
)
#else
(pid,gargs,nargs)
	int		pid;		/* id for layer instance	*/
	NhlGArgList	gargs;		/* GArg list 			*/
	int		nargs;		/* num of GArg's		*/
#endif
{
	_NhlArg		args[_NhlMAXARGLIST];
	NhlLayer	l = _NhlGetLayer(pid);
	int		i;
	NrmQuark	type_ret;
	unsigned int	size_ret;
	_NhlFreeFunc	free_func;

	if(l == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlALGetValues:Unable to access object w/ID %d",pid);
		return(NhlFATAL);
	}

	for(i=0;i<nargs;i++){
		args[i].quark = NrmStringToQuark(gargs[i].resname);
		args[i].value = gargs[i].value;
		args[i].type_ret = &type_ret;
		args[i].size_ret = &size_ret;
		args[i].free_func = &free_func;
		args[i].type = NrmNULLQUARK;
	}


	return _NhlGetValues(l,args,nargs);
}

/*
 * Function:	_NhlInitGetValues
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
_NhlInitGetValues
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	stringQ = NrmStringToQuark(NhlTString);
	genQ = NrmStringToQuark(NhlTGenArray);

	return;
}
