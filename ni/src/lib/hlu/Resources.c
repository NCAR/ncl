/*
 *      $Id: Resources.c,v 1.4 1994-01-27 21:25:37 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Resources.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Aug 28 08:47:47 MDT 1992
 *
 *	Description:	This file is part of the hlu library. It contains the
 *			functions that are used to set resources in layer
 *			instance classes.  It also will contain the functions
 *			for the app programer to have access to application
 *			resources that they define.
 *
 *	Machine Dependancies:
 *
 *		_NhlCopyFromArg:
 *			The function _NhlCopyFromArg could have problems on
 *			an architecture with strange sizes for the primative
 *			types since it is trying to copy information in a
 *			type independant fashion.
 *		_NhlCopyToArg:
 *			Same as _NhlCopyFromArg.
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/defs.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/NresDB.h>

/*
 * Static vars used to determine resoureces hard-coded defaults
 */

static NrmQuark QImmediate = NrmNULLQUARK;
static NrmQuark QProcedure = NrmNULLQUARK;
static NrmQuark QString = NrmNULLQUARK;
static NrmQuark QExtraLayer = NrmNULLQUARK;

/*
 * Resource Database
 */
static NrmDatabase NhlResDB = NULL;

/*
 * Function:	_NhlCopyFromArg
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
 * Scope:	Global Private
 * Returns:	
 * Side Effect:	
 */
void
_NhlCopyFromArg
#if __STDC__
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

    if      (size == sizeof(long))	*(long *)dst = (long)src;
    else if (size == sizeof(short))	*(short *)dst = (short)src;
    else if (size == sizeof(NhlPointer))
					*(NhlPointer *)dst = (NhlPointer)src;
    else if (size == sizeof(char))	*(char *)dst = (char)src;
    else if (size == sizeof(char*))	*(char **)dst = (char*)src;
    else if (size == sizeof(_NhlArgVal))	*(_NhlArgVal *)dst = src;
    else if (size > sizeof(_NhlArgVal))
        memcpy((void*)dst,(void*)src,size);
    else
        memcpy((void*)dst,(void*)&src,size);
}

/*
 * Function:	_NhlCopyToArg
 *
 * Description:	This function takes memory of a given size and copies it into
 *		another location. It doesn't actually change dst - it sets
 *		the memory pointed to by dst. ie. *dst is expected to contain
 *		the address of the memory that needs to have the value set
 *		in it.
 *
 * In Args:	NArgVal		src;	source
 *		unsigned int	size;	size
 *
 * Out Args:	void		*dst;	destination
 *
 * Scope:	Global Private
 * Returns:	
 * Side Effect:	
 */
void
_NhlCopyToArg
#if __STDC__
(
	void *		src,	/* source	*/
	_NhlArgVal *	dst,	/* destination	*/
	unsigned int	size	/* size		*/
) 
#else
(src,dst,size) 
	void *		src;	/* source	*/
	_NhlArgVal *	dst;	/* destination	*/
	unsigned int	size;	/* size		*/
#endif
{

    if      (size == sizeof(long)) *((long *)*dst) = *(long*)src;
    else if (size == sizeof(short)) *((short *)*dst) = *(short*)src;
    else if (size == sizeof(NhlPointer)) *((NhlPointer*)*dst) =
							*(NhlPointer*)src;
    else if (size == sizeof(char))	*((char *)*dst) = *(char*)src;
    else if (size == sizeof(char*))	*((char **)*dst) = *(char**)src;
    else if (size == sizeof(_NhlArgVal))
				*((_NhlArgVal *)*dst) = *(_NhlArgVal*)src;
    else if (size > sizeof(_NhlArgVal))
        memcpy((char*)dst,(char*)src,size);
    else
        memcpy((char*)dst,(char*)&src,size);
}

/*
 * Function:	GetNamesAndClasses
 *
 * Description:	This function creates a quark list of names and classes that
 *		give a unique description of the NhlLayer instance. If there isn't
 *		enough memory it returns -<amountneeded>
 * In Args:	l - the NhlLayer instance
 *		int length;	The length of the Qarrays provided
 * Out Args:	names and classes - each is an array of quarks
 * Scope:	static
 * Returns:	int : the number of quarks in each array
 *		If value is neg. then quark arrays not finished - not enough
 *		memory - returned the negative of what was needed.
 * Side Effect:	none
 */
static int
GetNamesAndClasses
#if	__STDC__
(
	NhlLayer	layer,		/* layer instance		*/
	NrmNameList	names,		/* Quarkarray of names OUT	*/
	NrmClassList	classes,	/* Quarkarray of classes OUT	*/
	int		length		/* length of Quarkarrays provided*/
)
#else
(layer,names,classes,length)
	NhlLayer	layer;		/* layer instance		*/
	NrmNameList	names;		/* Quarkarray of names OUT	*/
	NrmClassList	classes;	/* Quarkarray of classes OUT	*/
	int		length;		/* length of Quarkarrays provided*/
#endif
{
	register int i, len;
	register NrmQuark tquark;

	/*
	 * return null-terminated quark arrays with len the number of quarks
	 * not including NULL
	 */
	for(len=0; layer != NULL; layer = (NhlLayer)layer->base.parent,len++){

		if(len >= length-1) continue;

		names[len] = layer->base.nrm_name;
		classes[len] = (layer->base.layer_class)->base_class.nrm_class;
	}

	if(len > length) return (-len);

	/*
	 * reverse order
	 */
	for(i=0; i < len/2; i++){

		tquark = names[i];
		names[i] = names[len-i-1];
		names[len-i-1] = tquark;

		tquark = classes[i];
		classes[i] = classes[len-i-1];
		classes[len-i-1] = tquark;
	}

	names[len] = classes[len] = NrmNULLQUARK;
	return(len);
}

/*
 * Function:	GetResources
 *
 * Description: This function takes a list of resources a base addr and sets
 *		the addrs off of base using the given args and the Nrm. child
 *		is a pointer to the resources that should be forwarded from
 *		the parent's DB level or NULL if the base doesn't point to
 *		a child NhlLayer.
 *
 * In Args:	void *base;	Used as base addr to write data to from reslist
 *		NrmQuarkList nameQ;		list of names Quarked
 *		NrmQuarkList classQ;		list of classes Quarked
 *		NrmResourceList resources;	list of resources to be set
 *		int num_res;			number of resources
 *		Args args;			args to override Nrm
 *		int num_args;			number of args
 *		NrmQuarkList child;		look at level above as well
 *
 * Out Args:	addresses pointed to by base+resource[].offsets have changed
 *
 * Scope:	static
 * Returns:	none
 * Side Effect:	none
 */
static NhlErrorTypes
GetResources
#if	__STDC__
(
	_NhlConvertContext	context,/* context for converter allocs	*/
	char			*base,	/* addr to write res-values to	*/
	NrmQuarkList		nameQ,	/* Qlist of names in instance	*/
	NrmQuarkList		classQ,	/* Qlist of classes in instance	*/
	NrmResourceList		resources,/* list of resources		*/
	int			num_res,/* number of resources		*/
	_NhlExtArgList		args,	/* args to override defaults	*/
	int			num_args,/* number of args		*/
	NrmQuarkList		child	/* look at parent's DB level too*/
)
#else
(context,base,nameQ,classQ,resources,num_res,args,num_args,child)
	_NhlConvertContext	context;/* context for converter allocs	*/
	char			*base;	/* addr to write res-values to	*/
	NrmQuarkList		nameQ;	/* Qlist of names in instance	*/
	NrmQuarkList		classQ;	/* Qlist of classes in instance	*/
	NrmResourceList		resources;/* list of resources		*/
	int			num_res;/* number of resources		*/
	_NhlExtArgList		args;	/* args to override defaults	*/
	int			num_args;/* number of args		*/
	NrmQuarkList		child;	/* look at parent's DB level too*/
#endif
{
	register int	i,j;
	NhlBoolean	resfound[_NhlMAXRESLIST];
	NhlBoolean	argfound[_NhlMAXARGLIST];
	NhlErrorTypes	ret = NhlNOERROR;
	NhlErrorTypes	lret = NhlNOERROR;
	NrmHashTable	stackslist[_NhlMAXRESLIST];
	NrmHashTable	*slist = stackslist;
	int		slistlen = _NhlMAXRESLIST;
	NrmHashTable	Pstackslist[_NhlMAXRESLIST];
	NrmHashTable	*Pslist = Pstackslist;
	int		Pslistlen = _NhlMAXRESLIST;

	/* Mark each resource as not found */
	memset((char *)resfound,0, (int)(num_res * sizeof(NhlBoolean)));

	/* Mark each arg as not found */
	memset((char *) argfound,0, (int)(num_args * sizeof(NhlBoolean)));

	/* Set resources specified via args */

	for (i=0; i < num_args; i++){
		for(j=0; j < num_res; j++) {
			if(args[i].quark == resources[j].nrm_name) {
				if((args[i].type == NrmNULLQUARK) ||
					(args[i].type==resources[j].nrm_type)){

					_NhlCopyFromArg(args[i].value,
					(char*)(base + resources[j].nrm_offset),
					resources[j].nrm_size);
					resfound[j] = True;
				}
				else if(_NhlConverterExists(args[i].type,
					resources[j].nrm_type,NrmNULLQUARK)){
					/* 
					 * call converter
					 */
					NrmValue	from, to;

					from.size = sizeof(NhlPointer);
					from.addr = (void*)args[i].value;
					to.size = resources[j].nrm_size;
					to.addr =(void *)(base +
						resources[j].nrm_offset);

					lret = _NhlConvertData(context,
							args[i].type,
							resources[j].nrm_type,
							&from, &to);
					
					if(lret!=NhlNOERROR){
						NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Error retrieving resource %s from args - Ignoring Arg",
					NrmNameToString(resources[i].nrm_name));
						ret = MIN(NhlWARNING,ret);
					}
					else{
						resfound[j] = True;
					}
				}
				else{
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"The %s resource is not setable using a %s",
						NrmQuarkToString(args[i].quark),
						NrmQuarkToString(args[i].type));
				}
				argfound[i] = True;
				break;
			}	
		}
		if(!argfound[i]){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s is not a resource in the given object",
						NrmNameToString(args[i].quark));
			ret = MIN(ret,NhlWARNING);
		}
	}

/*
 * Retrieve the levels of the ResDB that we actually need
 */

	while(!NrmQGetSearchList(NhlResDB,nameQ,classQ,slist,slistlen)){

		if(slist == stackslist)
			slist = NULL;
		
		slistlen *= 2;
		slist = (NrmHashTable *)NhlRealloc(slist,
					sizeof(NrmHashTable) * slistlen);
	}
	if(child != NULL){
		NrmQuark *tquark;

		/*
		 * remove last quark from each list because the resources
		 * should look as if they are actually resources of the
		 * parent.
		 */
		/* SUPPRESS 570 */
		for(tquark = nameQ;*(tquark+1) != NrmNULLQUARK;tquark++);
		*tquark = NrmNULLQUARK;
		/* SUPPRESS 570 */
		for(tquark = classQ;*(tquark+1) != NrmNULLQUARK;tquark++);
		*tquark = NrmNULLQUARK;
		while(!NrmQGetSearchList(NhlResDB,nameQ,classQ,Pslist,
								Pslistlen)){
			if(Pslist == Pstackslist)
				Pslist = NULL;

			Pslistlen *= 2;
			Pslist = (NrmHashTable *)NhlRealloc(Pslist,
					sizeof(NrmHashTable) * Pslistlen);
		}
	}


	for (i=0; i < num_res; i++){

		if(resfound[i])		/* resource set via arg or datares*/
			continue;

		/* Set resources via Nrm databases */

		{
			NrmValue	from, to;
			NrmQuark	rdbtype;

			from.size = to.size = 0;
			from.addr = to.addr = (unsigned int)NULL;

			if(NrmGetQResFromList(slist,
					resources[i].nrm_name,
					resources[i].nrm_class,&rdbtype,&from)){

				if(rdbtype != resources[i].nrm_type){

					to.size = resources[i].nrm_size;
					to.addr =(void *)(base +
						resources[i].nrm_offset);

					lret = _NhlConvertData(context,rdbtype,
							resources[i].nrm_type,
							&from, &to);
					
					if(lret == NhlNOERROR){
						resfound[i] = True;
						continue;
					}
					/*
					 * unspecified error - let res-default
					 * fill value even though there was
					 * a db val for it - just notify caller
					 */

					NhlPError(NhlWARNING,NhlEUNKNOWN,
		"Error retrieving resource %s from DB - Using default value",
					NrmNameToString(resources[i].nrm_name));

					ret = MIN(NhlINFO,ret);
				}
				else{

				/* no type conversion needed */

					if(rdbtype == QString){
						*(unsigned int *)
						(base+resources[i].nrm_offset)=
						(unsigned int)from.addr;
					}
					else{
						memcpy(base +
							resources[i].nrm_offset,
							from.addr,
							resources[i].nrm_size);
					}
					resfound[i] = True;
					continue;
				}
			}
		}

		/*
		 * Set resources via Nrm database at parents level
		 * As long as the resource should be forwarded.
		 */
		if((child != NULL) && NrmQinQList(child,resources[i].nrm_name)){
			NrmValue	from, to;
			NrmQuark	rdbtype;

			from.size = to.size = 0;
			from.addr = to.addr = (unsigned int)NULL;

			if(NrmGetQResFromList(Pslist,
					resources[i].nrm_name,
					resources[i].nrm_class,&rdbtype,&from)){

				if(rdbtype != resources[i].nrm_type){

					to.size = resources[i].nrm_size;
					to.addr =(void*)(base +
						resources[i].nrm_offset);

					lret = _NhlConvertData(context,rdbtype,
							resources[i].nrm_type,
							&from, &to);
					
					if(lret == NhlNOERROR){
						resfound[i] = True;
						continue;
					}
					/*
					 * unspecified error - let res-default
					 * fill value even though there was
					 * a db val for it - just notify caller
					 */

					NhlPError(NhlINFO,NhlEUNKNOWN,
		"Error retrieving resource %s from DB - Using default value",
					NrmNameToString(resources[i].nrm_name));

					ret = MIN(NhlINFO,ret);
				}
				else{

				/* no type conversion needed */

					if(rdbtype == QString){
						*(NhlPointer *)
						(base+resources[i].nrm_offset)=
						from.addr;
					}
					else{
						memcpy(
						base + resources[i].nrm_offset,
						from.addr,
							resources[i].nrm_size);
					}
					resfound[i] = True;
					continue;
				}
			}
		}

		/*
		 * Set remaining resources via hard-coded resource
		 * default values
		 *
		 * There are a number of ways the resources can be hard-coded
		 *
		 * 1. NhlTImmediate default_type
		 *	This means the value of the default field is the value
		 *	and not the address. - Unless NhlTString is requested
		 *	since the value of a string is in fact an address.
		 *
		 * 2. NhlTProcedure default_type
		 *	This means the default addr field specifies a procedure
		 *	to be run. - If it run's successfully it is assumed
		 *	the procedure filled the value in correctly - if it
		 *	doesn't run successfully it fills the value to NULL
		 *	unless the procedure returned NhlFATAL in which case
		 *	it will exit.  The return value will propogate up
		 *	to the user call point.
		 *
		 * 3. nrm_type == nrm_default_type
		 *	This means no type conversion is neccessary - the
		 *	nrm_default_addr will be treated as a pointer and
		 *	the data it points to will be bcopied.
		 *
		 * 4. Type Conversion
		 *	The nrm_default_addr will be treated as an address
		 *	to data, and a request will be made to the type
		 *	conversion utility to convert the data into the
		 *	representation we require.
		 *
		 * 5. none of the above worked
		 *	The resource will be set to NULL and a NhlErrorTypes
		 *	value of NhlWARNING will be returned.
		 *	
		 */

		{
			NrmValue	from, to;

			from.size = to.size = 0;
			from.addr = to.addr = (unsigned int)NULL;

			if(resources[i].nrm_default_type == QImmediate){

				/* step #1 */

				if(resources[i].nrm_type == QString){
					to.addr = resources[i].nrm_default_addr;
					if(to.addr != NULL)
					to.size=strlen((Const char*)to.addr)+1;
				}
				else{
					to.addr =&resources[i].nrm_default_addr;
					to.size = sizeof(NhlPointer);
				}
			}

			else if(resources[i].nrm_default_type == QProcedure){

				NrmResourceDefaultProc	def_proc =
					(NrmResourceDefaultProc)
						resources[i].nrm_default_addr;

				/* step #2 */

				lret = (*def_proc)(resources[i].nrm_name,
						 resources[i].nrm_class,
						 base,resources[i].nrm_offset);
				if(lret > NhlWARNING){
					resfound[i] = True;
					continue; /* this resource is finished*/
				}

				/* unspecified error - set to NULL */
				NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"Unable to set %s to default value - Using NULL",
				NrmNameToString(resources[i].nrm_name)));
				to.addr = NULL;
				ret = MIN(ret,NhlWARNING);
			}

			else if(resources[i].nrm_default_type == 
				resources[i].nrm_type){

				/* step #3 */

				to.addr = resources[i].nrm_default_addr;
				to.size = resources[i].nrm_size;
			}

			else{
				/* step #4 */

				from.size = 0;
				from.addr = resources[i].nrm_default_addr;

				lret = _NhlConvertData(context,
					resources[i].nrm_default_type,
					resources[i].nrm_type,&from,&to);
				
				if(lret < NhlINFO){

				/* step #5 set return to NhlWARNING*/
				/* and set resource to NULL	*/

					/*ERROR*/
					NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"Unable to set %s to default value - Using NULL",
				NrmNameToString(resources[i].nrm_name)));
					to.addr = NULL;
					ret = MIN(ret,NhlWARNING);
				}

			}

			/* copy resulting "to" into resource */

			if(resources[i].nrm_type == QString)
				*(NhlPointer *)
				(base+resources[i].nrm_offset) = to.addr;
			else if(to.addr == NULL)
				memset(base + resources[i].nrm_offset,0,
							resources[i].nrm_size);
			else
				memcpy((base+resources[i].nrm_offset),
							to.addr,to.size);
			resfound[i] = True;
		}
	}

	if(slist != stackslist)
		(void)NhlFree(slist);
	if(Pslist != Pstackslist)
		(void)NhlFree(Pslist);
	return(ret);
}

/*
 * Function:	_NhlGetResources
 *
 * Description:	Initializes the given layer's resources using the Nrm and Args
 *
 * In Args:	NhlLayer l;	The layer to be inited
 *		Args args;	The args to override Nrm
 *		int nargs;	Number of args
 * Out Args:	NhlLayer l;	Inited l's resources
 *
 * Scope:	Global private
 * Returns:	nothing
 * Side Effect:	none
 */
NhlErrorTypes
_NhlGetResources
#if	__STDC__
(
	_NhlConvertContext	context,
	NhlLayer			l,	/* layer to set resources of	*/
	_NhlExtArgList		args,	/* args to override res defaults*/
	int			num_args,/* number of args		*/
	NrmQuarkList		child	/* layer is auto-managed chld	*/
)
#else
(context,l,args,num_args,child)
	_NhlConvertContext	context;
	NhlLayer			l;	/* layer to set resources of	*/
	_NhlExtArgList		args;	/* args to override res defaults*/
	int			num_args;/* number of args		*/
	NrmQuarkList		child;	/* layer is auto-managed chld	*/
#endif
{
	NrmQuark nameQ[_NhlMAXTREEDEPTH], classQ[_NhlMAXTREEDEPTH];
	NhlLayerClass lc = _NhlClass(l);

	/*
	 * Get quarks list of names and classes to use in querying the Nrm
	 */

	if(GetNamesAndClasses(l,nameQ,classQ,_NhlMAXTREEDEPTH) < 0){
		/* ERROR- DON'T DO ANY RESOURCES */
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Instance Tree depth exceeds _NhlMAXTREEDEPTH of %d",
								_NhlMAXTREEDEPTH);
		return(NhlFATAL);
	}

	return(GetResources(context,(char*)l, nameQ, classQ,
				     (NrmResourceList)lc->base_class.resources,
			     lc->base_class.num_resources,args,num_args,child));
}

/*
 * Function:	_NhlCompileResourceList
 *
 * Description: This function compiles the resource list into an
 *              NrmResourceList in Place.
 *
 * In Args:	ResourceList	resources;
 *		int		num_resources;
 *
 * Out Args:	ResourceList    resources;
 *
 * Scope:	Global Private
 * Returns:	none
 * Side Effect:	none
 */
void
_NhlCompileResourceList
#if	__STDC__
(
	NhlResourceList	resources,	/* resources		*/
	int		num_resources	/* number of resources	*/
)
#else
(resources, num_resources)
	NhlResourceList	resources;	/* resources            */ 
	int		num_resources;	/* number of resources	*/
#endif
{
	register int		i;
	register NrmResource	*nrmres=(NrmResource *)&resources[0];

#define PSToQ NrmPermStringToQuark
	for(i=0; i < num_resources; nrmres++,resources++,i++){

		nrmres->nrm_name	= PSToQ(resources->resource_name);
		nrmres->nrm_class	= PSToQ(resources->resource_class);
		nrmres->nrm_type	= PSToQ(resources->resource_type);
		nrmres->nrm_default_type= PSToQ(resources->default_type);
	}
#undef PSToQ
	return;
} /* _NhlCompileResourceList */

/*
 * Function:	_NhlGroupResources
 *
 * Description:	This function combines the resources of the current layer
 *		class with the resources of it's superclass. If the current
 *		NhlLayerClass over-rides any of the resources in the super-class
 *
 * In Args:	NhlLayerClass	lc;	layer class to create resource list for
 *
 * Out Args:	lc->base_class.resources is updated.
 *
 * Scope:	Global private
 * Returns:	NULL
 * Side Effect:	
 */
void
_NhlGroupResources
#if	__STDC__
(
	NhlLayerClass lc	/* NhlLayerClass to create full resource list for	*/
)
#else
(lc)
	NhlLayerClass lc;	/* NhlLayerClass to create full resource list for	*/
#endif
{
	NrmResourceList	rlist;
	int		num_rlist;
	int		i,j,next;
	NrmResourceList	classlist;
	NhlLayerClass	sc = lc->base_class.superclass;
	unsigned int	super_size;
	NhlBoolean	override;

	/*
	 * If superclass has no resources then the classes resource list
	 * is already complete.
	 */
	if((sc == NULL) || (sc->base_class.num_resources < 1))
		return;

	/*
	 * If class doesn't have any resources then point to the
	 * super-classes resources.
	 */
	if(lc->base_class.num_resources < 1){
		lc->base_class.resources = sc->base_class.resources;
		lc->base_class.num_resources = sc->base_class.num_resources;

		return;
	}

	num_rlist = lc->base_class.num_resources + sc->base_class.num_resources;
	super_size = sc->base_class.layer_size;

	rlist = (NrmResourceList)NhlMalloc(
				(unsigned)(sizeof(NrmResource) * num_rlist));

	memcpy((char*)rlist,(char*)(sc->base_class.resources),
			sc->base_class.num_resources * sizeof(NrmResource));

	classlist = (NrmResourceList)lc->base_class.resources;
	next = sc->base_class.num_resources;

	for(i=0;i < lc->base_class.num_resources;i++){

		override = False;

		if(classlist[i].nrm_offset < super_size){

			for(j=0;j < sc->base_class.num_resources;j++){

				if(classlist[i].nrm_offset ==
							rlist[j].nrm_offset){
					/*
					 * Over-ride superclass field
					 * size must be the same so GetResources
					 * will not overwrite the next field.
					 */
					if(classlist[i].nrm_size !=
							rlist[j].nrm_size){
						NhlPError(NhlWARNING,NhlEUNKNOWN,
			"GroupResources:%s is Wrong size in Resource list",
				NrmQuarkToString(classlist[i].nrm_name));

						classlist[i].nrm_size =
							rlist[j].nrm_size;
					}
					rlist[j] = classlist[i];
					num_rlist--;
					override = True;
					break;
				}
			}
		}

		if(!override)
			rlist[next++] = classlist[i];
	}

	lc->base_class.resources = (NhlResourceList)rlist;
	lc->base_class.num_resources = num_rlist;

	return;
}


/*
 * Function:	_NhlResourceListInitialize
 *
 * Description:	This function initializes static variables needed to compute
 *		resource values later. It uses the static variable ResInit
 *		to determine if it has been called more than once.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	
 * Side Effect:	
 */
static int ResInit = False;

void
_NhlResourceListInitialize
#if	__STDC__
(
	void
) 
#else
() 
#endif
{ 
	if(ResInit)
		return;

	ResInit = True;

	QImmediate = NrmStringToName(NhlTImmediate);
	QProcedure = NrmStringToName(NhlTProcedure);
	QString = NrmStringToName(NhlTString);
	QExtraLayer = NrmStringToName(NhlTExtraLayer);

	return; 
}

/*
 * Function:	_NhlInitResDatabase
 *
 * Description:	This function is used to initialize the default resource
 *		database that is made up from a system-resource file and
 *		a user-resource file.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	
 * Side Effect:	
 */
void
_NhlInitResDatabase
#if	__STDC__
(
	void
)
#else
()
#endif
{
	Const char *sysfile=NULL;
	Const char *usrfile=NULL;

	sysfile = _NhlGetSysResFile();
	usrfile = _NhlGetUsrResFile();

	if((void *)sysfile == (void *)NULL){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"Unable to Get System Resource File Name?");
	}
	else
		NhlResDB = NrmGetFileDB(sysfile);
	
	if((void *)NhlResDB == (void *)NULL){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Unable to load System Resource File %s",sysfile);
	}

	if((void *)usrfile == (void *)NULL){
		NhlPError(NhlINFO,NhlEUNKNOWN,
				"Unable to Get User Resource File Name?");
	}
	else
		NrmCombineFileDB(usrfile,&NhlResDB,True);

	if((void *)NhlResDB == (void *)NULL){
		NrmPutStringRes(&NhlResDB,"DBLoaded","False");
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	"Unable to Create a Resource DB - Hard coded defaults will be used");

	}
	else
		NrmPutStringRes(&NhlResDB,"DBLoaded","True");

	return;
}

/*
 * Function:	_NhlDestroyResDatabase
 *
 * Description:	This function is used to destroy the default resource
 *		database that is made up from a system-resource file and
 *		a user-resource file.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	
 * Side Effect:	
 */
void
_NhlDestroyResDatabase
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NrmDestroyDB(NhlResDB);
	NhlResDB = NULL;

	return;
}

/*
 * Function:	_NhlMergeArgLists
 *
 * Description:	This function takes two arg lists and merges them - removing
 *		any duplicate argnames with the oargs taking presedence over
 *		the args.
 *
 * In Args:	
 *		_NhlExtArgList	oargs,		over-ride args
 *		int		num_oargs,	num oargs
 *		_NhlExtArgList	args,		args
 *		int		num_args	num args
 *
 * Out Args:	
 *		_NhlExtArgList	*ret_args,	return args
 *		int		*num_ret_args,	num ret_args
 *
 * Scope:	Private Global
 * Returns:	void
 * Side Effect:	
 */
void
_NhlMergeArgLists
#if	__STDC__
(
	_NhlExtArgList	ret_args,	/* return args		*/
	int		*num_ret_args,	/* num ret_args		*/
	_NhlExtArgList	oargs,		/* over-ride args	*/
	int		num_oargs,	/* num oargs		*/
	_NhlExtArgList	args,		/* args			*/
	int		num_args	/* num args		*/
)
#else
(ret_args,num_ret_args,oargs,num_oargs,args,num_args)
	_NhlExtArgList	ret_args;	/* return args		*/
	int		*num_ret_args;	/* num ret_args		*/
	_NhlExtArgList	oargs;		/* over-ride args	*/
	int		num_oargs;	/* num oargs		*/
	_NhlExtArgList	args;		/* args			*/
	int		num_args;	/* num args		*/
#endif
{
	NhlBoolean	argfound;
	int		i,j;

	/*
	 * Initialize num_ret_args
	 */
	*num_ret_args = 0;

	/*
	 * take care of simplest case
	 */
	if((num_oargs + num_args) == 0)
		return;

	/*
	 * Add args from args list only if that argname doesn't exist in
	 * the override arg list
	 */
	for(i=0;i < num_args;i++){
		argfound = False;

		for(j=0;j < num_oargs; j++){
			if(args[i].quark == oargs[j].quark){
				argfound = True;
				break;
			}
		}

		if(!argfound){
			ret_args[*num_ret_args] = args[i];
			(*num_ret_args)++;
		}
	}

	/*
	 * Add all the oargs to the ret list
	 */
	for(i=0;i < num_oargs;i++){
		ret_args[*num_ret_args] = oargs[i];
		(*num_ret_args)++;
	}
}

/*
 * Function:	_NhlResInClass
 *
 * Description:	This functions returns true if the given quark specifies
 *		a resource of the given class.
 *
 * In Args:	
 *		NhlLayerClass	lc	class to check for res
 *		NrmQuark	res	resource to look for
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlBoolean
 * Side Effect:	
 */
NhlBoolean
_NhlResInClass
#if	__STDC__
(
	NhlLayerClass	lc,	/* class to check for res	*/
	NrmQuark	res	/* resource to look for		*/
)
#else
(lc,res)
	NhlLayerClass	lc;	/* class to check for res	*/
	NrmQuark	res;	/* resource to look for		*/
#endif
{
	int		i;
	NrmResourceList	nrmres = (NrmResourceList)lc->base_class.resources;

	for(i=0;i < lc->base_class.num_resources;i++)
		if(nrmres[i].nrm_name == res) return True;

	return False;
}

/*
 * Function:	_NhlSortChildArgs
 *
 * Description:	This function is used to sort the given args into arg lists
 *		for the registered children of the given layer and to return
 *		the args that apply to the given layer.  The linked list of
 *		children args should be free'd by calling _NhlFreeChildArgs.
 *
 * In Args:	
 *		NhlLayer			l,		layer
 *		_NhlArgList		args_in,	args to sort
 *		int			nargs_in,	number args to sort
 *		_NhlArgList		args_out,	args not forwarded
 *		int			*nargs_out,	num args_out
 *		_NhlChildArgList	*forw_list	list of args to frwd
 *		NhlBoolean		getvalues	called frm getvalues
 *
 * Out Args:	
 *		_NhlArgList		args_out,	args not forwarded
 *		int			*nargs_out,	num args_out
 *		_NhlChildArgList	*forw_list	list of args to frwd
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlSortChildArgs
#if	__STDC__
(
	NhlLayer			l,		/* layer		*/
	_NhlExtArgList		args_in,	/* args to sort		*/
	int			nargs_in,	/* number args to sort	*/
	_NhlExtArgList		*args_out,	/* args not forwarded	*/
	int			*nargs_out,	/* num args_out		*/
	_NhlChildArgList	*forw_list,	/* list of args to frwd	*/
	NhlBoolean		*args_used,	/* args used		*/
	NhlBoolean		getvalues	/* called frm getvalues	*/
)
#else
(l,args_in,nargs_in,args_out,nargs_out,forw_list,args_used,getvalues)
	NhlLayer			l;		/* layer		*/
	_NhlExtArgList		args_in;	/* args to sort		*/
	int			nargs_in;	/* number args to sort	*/
	_NhlExtArgList		*args_out;	/* args not forwarded	*/
	int			*nargs_out;	/* num args_out		*/
	_NhlChildArgList	*forw_list;	/* list of args to frwd	*/
	NhlBoolean		*args_used;	/* args used		*/
	NhlBoolean		getvalues;	/* called frm getvalues	*/
#endif
{
	NhlLayerClass		lc = _NhlClass(l);
	int			i;
	_NhlChildResList	reslist=NULL;
	_NhlChildArgList	arglist=NULL;
	NhlErrorTypes		ret = NhlNOERROR;

	/*
	 * Init nargs_out
	 */
	*nargs_out = 0;

	/*
	 * Deal with simplest case
	 */
	if(nargs_in == 0)
		return NhlNOERROR;

	/*
	 * Deal with no children case - Parent gets all args
	 */
	if(lc->base_class.child_resources == NULL){
		*args_out = args_in;
		*nargs_out = nargs_in;
		for(i=0;i<nargs_in;i++)
			args_used[i] = True;
		return NhlNOERROR;
	}

	/*
	 * Initialize arg lists
	 */

	reslist = lc->base_class.child_resources;

	*forw_list = (_NhlChildArgList)NhlMalloc(sizeof(_NhlChildArgNode));
	if(*forw_list == NULL)
		return NhlFATAL;

	arglist = *forw_list;

	while(reslist != NULL){

		arglist->class = reslist->class;
		arglist->autosetval = reslist->autosetval;
		arglist->next = NULL;
		arglist->nargs = 0;
		arglist->args = NhlMalloc(nargs_in * sizeof(_NhlExtArg));
		arglist->args_used = (NhlBoolean**)NhlMalloc(nargs_in *
							sizeof(NhlBoolean*));
		if((arglist->args == NULL) || (arglist->args_used == NULL)){
			(void)NhlFree(arglist->args);
			(void)NhlFree(arglist->args_used);
			_NhlFreeChildArgs(*forw_list);
			*forw_list = NULL;
			return NhlFATAL;
		}

		if(reslist->next != NULL){

			arglist->next = (_NhlChildArgList)NhlMalloc(
						sizeof(_NhlChildArgNode));
			if(arglist->next == NULL){
				_NhlFreeChildArgs(*forw_list);
				return NhlFATAL;
			}
		}

		arglist = arglist->next;
		reslist = reslist->next;
	}

	/*
	 * Sort arg list into it's various parts
	 */
	for(i=0;i<nargs_in;i++){

		NhlBoolean argfound;

		argfound = False;
		args_used[i] = False;

		/*
		 * if arg is a res in parent then arg is added to
		 * parents arg list and no other.
		 */
		if(_NhlResInClass(lc,args_in[i].quark)){
			(*args_out)[*nargs_out].quark = args_in[i].quark;
			(*args_out)[*nargs_out].value = args_in[i].value;
			(*args_out)[*nargs_out].type = args_in[i].type;
			(*nargs_out)++;
			args_used[i] = True;
			argfound = True;
		}
		/*
		 * otherwise add it to the children - If called from getvalues
		 * only add it to the first child found with that resource.
		 * otherwise add it to all the children that have that resource.
		 */
		else{

			arglist = *forw_list;
			reslist = lc->base_class.child_resources;

			while((arglist != NULL) && (reslist != NULL)){

				if(NrmQinQList(reslist->resources,
							args_in[i].quark)){
					arglist->args[arglist->nargs].quark =
							args_in[i].quark;
					arglist->args[arglist->nargs].value =
							args_in[i].value;
					arglist->args[arglist->nargs].type =
								args_in[i].type;
					arglist->args_used[arglist->nargs] =
								&args_used[i];
					(arglist->nargs)++;
					argfound = True;
					/*
					 * If called from get values break out
					 * so arg is only added to one of the
					 * layer's lists.
					 */
					if(getvalues) break;
				}

				arglist = arglist->next;
				reslist = reslist->next;
			}
		}
		/*
		 * Notify user of bad resource requests
		 */
		if(!argfound){
			NhlPError(NhlINFO,NhlEUNKNOWN,
				"The resource %s is not in objects of class %s",
					NrmNameToString(args_in[i].quark),
							_NhlClassName(lc));
			ret = NhlINFO;
		}
	}

	return ret;
}

/*
 * Function:	_NhlFreeChildArgs
 *
 * Description:	This function free's the memory associated with the child_args
 *		field of the given NhlLayer instance.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	void
 * Side Effect:	
 */
void
_NhlFreeChildArgs
#if	__STDC__
(
	_NhlChildArgList	list	/* child arg list to free	*/
)
#else
(list)
	_NhlChildArgList	list;	/* child arg list to free	*/
#endif
{
	if(list == NULL)
		return;
	
	_NhlFreeChildArgs(list->next);

	(void)NhlFree(list->args);
	(void)NhlFree(list->args_used);
	(void)NhlFree(list);

	return;
}
