/*
 *      $Id: GetValues.c,v 1.4 1994-01-27 21:23:04 boote Exp $
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
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>

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
	_NhlExtArgList	args,		/* names of resources to retrieve*/
	int		nargs		/* number of args		*/
)
#else
(base,resources,num_res,args,nargs)
 	char*		base;		/* base address to copy vals from */
	NrmResourceList	resources;	/* resource list with offsets	*/
	int		num_res;	/* number of resources		*/
	_NhlExtArgList	args;		/* names of resources to retrieve*/
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
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
						"Unimplimented Feature"));
					break;
				}
				_NhlCopyToArg(
					(char*)(base + resources[j].nrm_offset),
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
	_NhlExtArgList	args,		/* args to retrieve	*/
	int		nargs		/* number of args	*/
)
#else
(l,args,nargs)
	NhlLayer	l;		/* layer instance	*/
	_NhlExtArgList	args;		/* args to retrieve	*/
	int		nargs;		/* number of args	*/
#endif
{
	int			i;
	NhlLayerClass		lc = _NhlClass(l);
	_NhlExtArg		stackargs[_NhlMAXARGLIST];
	_NhlExtArgList		largs=stackargs;
	int			nlargs;
	_NhlChildArgList	targnode=NULL;
	_NhlChildArgList	chld_args=NULL;
	NhlBoolean		chld_args_used[_NhlMAXARGLIST];
	_NhlChildList		tchldnode=NULL;
	_NhlArg			get_args[_NhlMAXARGLIST];
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
		int i;
		for(i=0;i<nargs;i++){
			get_args[i].quark = args[i].quark;
			get_args[i].value = args[i].value;
		}
		ret = MIN(ret,lret);
		lret = CallGetValues(lc,l,get_args,nargs);
	}

	return MIN(ret,lret);
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
	int             num_args; 
	_NhlExtArg	args[_NhlMAXARGLIST];
	NhlErrorTypes	ret;
	NhlLayer	l = NULL;

	/* count var args */
	VA_START(ap,pid); 
	num_args = _NhlCountGetVarList(ap); 
	va_end(ap); 

	/* create an arglist from varargs */
	VA_START(ap,pid); 
	_NhlVarToGetArgList(ap,args,num_args); 
	va_end(ap); 

	l = _NhlGetLayer(pid);
	if(l == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to access plot w/PID %d",pid);
		return(NhlFATAL);
	}
	ret = _NhlGetValues(l,args,num_args);

	return(ret);
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
	_NhlExtArg	args[_NhlMAXARGLIST];
	NhlErrorTypes	ret;
	NhlLayer	l = NULL;

	/* create an arglist from gargs */
	_NhlGArgToGetArgList(args,gargs,nargs); 

	l = _NhlGetLayer(pid);
	if(l == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to access plot w/PID %d",
									pid);
		return(NhlFATAL);
	}
	ret = _NhlGetValues(l,args,nargs);

	return(ret);
}
