/*
 *      $Id: GetValues.c,v 1.25 2009-07-10 19:54:04 huangwei Exp $
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
#include <ncarg/hlu/AppI.h>

static	NrmQuark	stringQ;
static	NrmQuark	genQ;

/*
 * Function:	_NhlGetValues
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
NhlErrorTypes 
_NhlGetValues
#if	NhlNeedProto
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
	char		func[] = "_NhlGetValues";
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

	for(i=0,j=0; i < nargs; i++){
		while(j < num_res){
			if(args[i].quark > resources[j].nrm_name){
				j++;
				continue;
			}
			if(args[i].quark < resources[j].nrm_name)
				break;

			if(resources[j].res_info & _NhlRES_NOGACCESS){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:%s does not have \"G\" access",
					func,NrmQuarkToString(args[i].quark));
					args[i].quark = NrmNULLQUARK;
			}
			else{
				if(args[i].type != NrmNULLQUARK){
					*args[i].type_ret=resources[j].nrm_type;
					*args[i].size_ret=resources[j].nrm_size;
					*args[i].free_func =
							resources[j].free_func;
				}
				_NhlCopyToArg((NhlPointer)
					(base + resources[j].nrm_offset),
					&args[i].value,
					resources[j].nrm_size);
			}
			argfound[i] = True;
			break;
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
 * In Args:	NhlClass	lc;	class pointer
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
#if	NhlNeedProto
(
	NhlClass	lc,	/* class pointer	*/
	NhlLayer	l,	/* instance pointer	*/
	_NhlArgList	args,	/* resources retrieving	*/
	int		nargs	/* number of args	*/
)
#else
(lc,l,args,nargs)
	NhlClass	lc;	/* class pointer	*/
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
 * Function:	_NhlGetLayerValues
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
NhlDOCTAG(_NhlGetLayerValues)
static NhlErrorTypes
_NhlGetLayerValues
#if	NhlNeedProto
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
	char			func[]="_NhlGetLayerValues";
	int			i;
	NhlClass		lc = _NhlClass(l);
	_NhlArg		stackargs[_NhlMAXARGLIST];
	_NhlArgList		largs=stackargs;
	int			nlargs;
	_NhlChildArgList	targnode=NULL;
	_NhlChildArgList	chld_args=NULL;
	NhlBoolean		chld_args_used[_NhlMAXARGLIST];
	_NhlChildList		tchldnode=NULL;
	NhlErrorTypes		ret= NhlNOERROR, lret = NhlNOERROR;

	if(l == NULL){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:passed a NULL object",
								func));
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
		if(_NhlIsApp(l)){
			lret = _NhlSortAppArgs(l,args,nargs,&largs,&nlargs);
			if(lret < NhlWARNING){
				NhlPError(lret,NhlEUNKNOWN,
					"%s:Unable to sort Arg List",func);
				return lret;
			}
			l->base.child_args = NULL;
			for(i=0;i<nargs;i++)
				chld_args_used[i] = True;
		}
		else{
			/*
			 * Sort the args into args for this layer and it's
			 * children. If there are no children it just copies
			 * the args to largs
			 */
			lret = _NhlSortChildArgs(l,args,nargs,&largs,&nlargs,
					&chld_args,chld_args_used,True);
			if(lret < NhlWARNING){
				NhlPError(lret,NhlEUNKNOWN,
				"Unable to Sort Arg Lists - Can't GetValues");
				return lret;
			}
			ret = MIN(ret,lret);
		}

		/*
		 * If this layer has children forward args to them
		 */
		if(l->base.children != NULL){

			tchldnode = l->base.children;

			while(tchldnode != NULL){

				targnode = chld_args;

				while((targnode != NULL) &&
					(tchldnode->theclass != targnode->theclass))
					targnode = targnode->next;

				if(targnode == NULL){
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"GetValues can't find args to get child's resources"));
					return NhlFATAL;
				}

				lret = _NhlGetLayerValues(
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
			for(i=0;i<nargs;i++){
				if(!chld_args_used[i]){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s is not a valid resource in %s at this time",
				NrmNameToString(args[i].quark),_NhlName(l));
					ret = MIN(ret,NhlWARNING);
				}
			}
		}
		_NhlFreeChildArgs(chld_args);
	}

	lret = _NhlGetValues((char*)l,
				(NrmResourceList)(lc->base_class.resources),
				lc->base_class.num_resources, largs, nlargs);

	if(lret != NhlFATAL) {
		ret = MIN(ret,lret);
		lret = CallGetValues(lc,l,largs,nlargs);
	}

	return MIN(ret,lret);
}

/*
 * Function:	CopyArgToArgptr
 *
 * Description:	This function takes memory of a given size and copies it into
 *		another location. It doesn't actually change dst - it sets
 *		the memory pointed to by dst. ie. *dst = the value.
 *
 * In Args:	_NhlArgVal	src;	source
 *		unsigned int	size;	size
 *
 * Out Args:	int		*dst;	destination
 *
 * Scope:	static
 * Returns:	
 * Side Effect:	
 */
static void
CopyArgToArgptr
#if	NhlNeedProto
(
	_NhlArgVal	src,	/* source	*/
	void *		dst,	/* destination	*/
	unsigned int	size	/* size		*/
) 
#else
(src,dst,size) 
	_NhlArgVal	src;	/* source	*/
	void*		dst;	/* destination	*/
	unsigned int	size;	/* size		*/
#endif
{

    if (size == 0)			return;
    else if (size == sizeof(long))	*(long *)dst = src.lngval;
    else if (size == sizeof(short))	*(short *)dst = src.shrtval;
    else if (size == sizeof(unsigned short))
					*(unsigned short *)dst = src.ushortval;
    else if (size == sizeof(NhlPointer))
					*(NhlPointer *)dst = src.ptrval;
    else if (size == sizeof(char))	*(char *)dst = src.charval;
    else if (size == sizeof(char*))	*(char **)dst = src.strval;
    else if (size == sizeof(_NhlArgVal))	*(_NhlArgVal *)dst = src;
    else
        memcpy((void*)dst,(void*)&src,size);
}

/*
 * Function:	InitGetValues
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
static void
InitGetValues
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
#if	NhlNeedProto
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
	static NhlBoolean	Initialized = False;
	_NhlArg			args[_NhlMAXARGLIST];
	_NhlArg			gargs[_NhlMAXARGLIST];
	_NhlGArgExtra		gextra[_NhlMAXARGLIST];
	int			nargs,i;
	NhlLayer		l = _NhlGetLayer(pid);
	NhlErrorTypes		ret = NhlNOERROR;
	_NhlConvertContext	context=NULL;

	if(!Initialized){
		InitGetValues();
		Initialized = True;
	}

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
		gextra[i].size_ret = 0;
		gextra[i].free_func = NULL;
		gextra[i].chld_class = NULL;
		gargs[i].type_ret = &gextra[i].type_ret;
		gargs[i].size_ret = &gextra[i].size_ret;
		gargs[i].free_func = &gextra[i].free_func;
		gargs[i].chld_class = &gextra[i].chld_class;
	}

	ret = _NhlGetLayerValues(l,gargs,nargs);

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
			CopyArgToArgptr(gextra[i].value_ret,
				args[i].value.ptrval,gextra[i].size_ret);
		}
		else if(gextra[i].type_ret == NrmNULLQUARK){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
					"NhlGetValues:Error retrieving %s",
					NrmQuarkToString(args[i].quark));
		}
		else{
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
			 * is, since it's a union of all the supported types.
			 */
			to.size = sizeof(_NhlArgVal);
			to.data = args[i].value;

			if(context == NULL)
				context = _NhlCreateConvertContext(NULL);

			if(gextra[i].chld_class){
				_NhlConvertContextClass(context,
							gextra[i].chld_class);
			}
			else{
				_NhlConvertContextClass(context,
							l->base.layer_class);
			}

			if(_NhlConvertData(context,gextra[i].type_ret,
					args[i].type,&from,&to) < NhlINFO){

				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"NhlGetValues:Error retrieving %s",
					NrmQuarkToString(args[i].quark));

				ret = MIN(ret,NhlWARNING);
				continue;
			}

			/*
			 * if converted to a string - copy memory since we
			 * want the user to own it.
			 */
			if(args[i].type == stringQ){
				NhlString	tstr =
					*(NhlString *)args[i].value.ptrval;
				if(tstr != NULL){
					*(NhlString *)args[i].value.ptrval =
						NhlMalloc(strlen(tstr) + 1);
					if(*(NhlString *)args[i].value.ptrval
								== NULL){
					NhlPError(NhlWARNING,ENOMEM,
					"NhlGetValues:Unable to retrieve %s",
					NrmQuarkToString(args[i].quark));
					ret = MIN(ret,NhlWARNING);
					}
					else
						strcpy(
							*(NhlString*)
							args[i].value.ptrval,
									tstr);
				}
			}
			else if(_NhlIsSubtypeQ(genQ,args[i].type)){
			/*
			 * This really shouldn't happen much.  All Array
			 * getvalues should be using one of the (Exp)export
			 * types.
			 * But just in case...
			 */
				NhlGenArray	tgen =
					*(NhlGenArray *)args[i].value.ptrval;
				if(tgen != NULL){
					*(NhlGenArray*)args[i].value.ptrval =
						_NhlCopyGenArray(tgen,True);
					if(*(NhlGenArray*)args[i].value.ptrval
								== NULL){
					NhlPError(NhlWARNING,ENOMEM,
					"NhlGetValues:Unable to retrieve %s",
					NrmQuarkToString(args[i].quark));
					ret = MIN(ret,NhlWARNING);
					}
				}
			}

			if((gextra[i].free_func != NULL) &&
						(from.data.ptrval != NULL))
				(*(gextra[i].free_func))
					(from.data.ptrval);
		}
	}

	_NhlFreeConvertContext(context);

	return ret;
}

/*
 * Function:	nhlpfgetvalues
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	err_ret gets NhlErrorTypes
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfgetvalues,NHLPFGETVALUES)
#if	NhlNeedProto
(
	int	*id,
	int	*rlid,
	int	*err_ret
)
#else
(id,rlid,err_ret)
	int	*id;
	int	*rlid;
	int	*err_ret;
#endif
{
	*err_ret = NhlGetValues(*id,*rlid);

	return;
}

/*
 * Function:	NhlVAGetValues
 *
 * Description:	This function retrieves the resources specified by the
 *		resource name/addr pairs given in the varargs.  It retrieves
 *		the given resource from the layer specified and puts the
 *		value in the space pointed to by the addr
 *		Internal GetValues function is NhlDOCREF(#_NhlGetLayerValues,here).
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
#if	NhlNeedVarArgProto
(
	int		pid,		/* id for layer instance*/
	...				/* res/addr pairs	*/
)
#else
(pid,va_alist)
	int		pid;		/* id for layer instance*/
	va_dcl				/* res/addr pairs	*/
#endif	/* NhlNeedVarArgProto */
{
	va_list         ap;
	NhlString	name;
	int             i = 0;
	_NhlArg		args[_NhlMAXARGLIST];
	NhlLayer	l = _NhlGetLayer(pid);
	NrmQuark	type_ret;
	_NhlFreeFunc	free_func;
	unsigned int	size_ret;
	NhlClass	chld_class;

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
		args[i].chld_class = &chld_class;
		args[i].type = NrmNULLQUARK;
		i++;
	}
	va_end(ap); 

	qsort(args,i,sizeof(_NhlArg),_NhlCompareArg);

	return _NhlGetLayerValues(l,args,i);
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
#if	NhlNeedProto
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
	NhlClass	chld_class;

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
		args[i].chld_class = &chld_class;
		args[i].type = NrmNULLQUARK;
	}

	qsort(args,i,sizeof(_NhlArg),_NhlCompareArg);

	return _NhlGetLayerValues(l,args,nargs);
}
