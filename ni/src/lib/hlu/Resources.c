/*
 *      $Id: Resources.c,v 1.1 1993-04-30 17:23:53 boote Exp $
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

/* SUPPRESS 112 */

/*
 * Static vars used to determine resoureces hard-coded defaults
 */

static NrmQuark QImmediate = NrmNULLQUARK;
static NrmQuark QProcedure = NrmNULLQUARK;
static NrmQuark QString = NrmNULLQUARK;

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
        bcopy((char *)  src, (char *) dst, (int) size);
    else
        bcopy((char *) &src, (char *) dst, (int) size);
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
        bcopy((char *)  src, (char *) dst, (int) size);
    else
        bcopy((char *) &src, (char *) dst, (int) size);
}

/*
 * Function:	GetNamesAndClasses
 *
 * Description:	This function creates a quark list of names and classes that
 *		give a unique description of the Layer instance. If there isn't
 *		enough memory it returns -<amountneeded>
 * In Args:	l - the Layer instance
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
	Layer		layer,		/* layer instance		*/
	NrmNameList	names,		/* Quarkarray of names OUT	*/
	NrmClassList	classes,	/* Quarkarray of classes OUT	*/
	int		length		/* length of Quarkarrays provided*/
)
#else
(layer,names,classes,length)
	Layer		layer;		/* layer instance		*/
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
	for(len=0; layer != NULL; layer = (Layer)layer->base.parent,len++){

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
 *		a child Layer.
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
/*ARGSUSED*/
static NhlErrorTypes
GetResources
#if	__STDC__
(
	char*		base,		/* addr to write res-values to	*/
	NrmQuarkList	nameQ,		/* Qlist of names in instance	*/
	NrmQuarkList	classQ,		/* Qlist of classes in instance	*/
	NrmResourceList	resources,	/* list of resources		*/
	int		num_res,	/* number of resources		*/
	_NhlArgList	args,		/* args to override defaults	*/
	int		num_args,	/* number of args		*/
	NrmQuarkList	child		/* look at parent's DB level too*/
)
#else
(base,nameQ,classQ,resources,num_res,args,num_args,child)
	char*		base;		/* addr to write res-values to	*/
	NrmQuarkList	nameQ;		/* Qlist of names in instance	*/
	NrmQuarkList	classQ;		/* Qlist of classes in instance	*/
	NrmResourceList	resources;	/* list of resources		*/
	int		num_res;	/* number of resources		*/
	_NhlArgList	args;		/* args to override defaults	*/
	int		num_args;	/* number of args		*/
	NrmQuarkList	child;		/* look at parent's DB level too*/
#endif
{
	register int	i,j;
	NhlBoolean	resfound[MAXRESLIST];
	NhlBoolean	argfound[MAXARGLIST];
	NhlErrorTypes	ret = NOERROR;
	NrmHashTable	stackslist[MAXRESLIST];
	NrmHashTable	*slist = stackslist;
	int		slistlen = MAXRESLIST;
	NrmHashTable	Pstackslist[MAXRESLIST];
	NrmHashTable	*Pslist = Pstackslist;
	int		Pslistlen = MAXRESLIST;

	/* Mark each resource as not found */
	bzero((char *) resfound, (int)(num_res * sizeof(NhlBoolean)));

	/* Mark each arg as not found */
	bzero((char *) argfound, (int)(num_args * sizeof(NhlBoolean)));

	/* Set resources specified via args */

	for (i=0; i < num_args; i++){
		for(j=0; j < num_res; j++) {
			if(args[i].quark == resources[j].nrm_name) {
				_NhlCopyFromArg(args[i].value,
					(char*)(base + resources[j].nrm_offset),
					resources[j].nrm_size);
				resfound[j] = True;
				argfound[i] = True;
				break;
			}	
		}
		if(!argfound[i]){
			NhlPError(INFO,E_UNKNOWN,
				"%s is not a resource in the given object",
						NrmNameToString(args[i].quark));
			ret = MIN(ret,INFO);
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
		 * remove last quark from each list
		 */
		/* SUPPRESS 530 */
		for(tquark = nameQ;*(tquark+1) != NrmNULLQUARK;tquark++);
		*tquark = NrmNULLQUARK;
		/* SUPPRESS 530 */
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

		if(resfound[i])		/* resource set via arg	*/
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

					NhlErrorTypes lret = NOERROR;

					to.size = resources[i].nrm_size;
					to.addr =(void *)(base +
						resources[i].nrm_offset);

					lret = _NhlConvertData(rdbtype,
							resources[i].nrm_type,
							&from, &to);
					
					if(lret == NOERROR){
						resfound[i] = True;
						continue;
					}
					/*
					 * unspecified error - let res-default
					 * fill value even though there was
					 * a db val for it - just notify caller
					 */

					NhlPError(WARNING,E_UNKNOWN,
		"Error retrieving resource %s from DB - Using default value",
					NrmNameToString(resources[i].nrm_name));

					ret = MIN(INFO,ret);
				}
				else{

				/* no type conversion needed */

					if(rdbtype == QString){
						*(unsigned int *)
						(base+resources[i].nrm_offset)=
						(unsigned int)from.addr;
					}
					else{
						bcopy(from.addr,
						base + resources[i].nrm_offset,
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

					NhlErrorTypes lret = NOERROR;

					to.size = resources[i].nrm_size;
					to.addr =(void*)(base +
						resources[i].nrm_offset);

					lret = _NhlConvertData(rdbtype,
							resources[i].nrm_type,
							&from, &to);
					
					if(lret == NOERROR){
						resfound[i] = True;
						continue;
					}
					/*
					 * unspecified error - let res-default
					 * fill value even though there was
					 * a db val for it - just notify caller
					 */

					NhlPError(INFO,E_UNKNOWN,
		"Error retrieving resource %s from DB - Using default value",
					NrmNameToString(resources[i].nrm_name));

					ret = MIN(INFO,ret);
				}
				else{

				/* no type conversion needed */

					if(rdbtype == QString){
						*(NhlPointer *)
						(base+resources[i].nrm_offset)=
						from.addr;
					}
					else{
						bcopy(from.addr,
						base + resources[i].nrm_offset,
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
		 *	unless the procedure returned FATAL in which case
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
		 *	value of WARNING will be returned.
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
					to.size=strlen((Const char*)to.addr)+1;
				}
				else{
					to.addr =&resources[i].nrm_default_addr;
					to.size = sizeof(NhlPointer);
				}
			}

			else if(resources[i].nrm_default_type == QProcedure){
				NhlErrorTypes lret;

				/* step #2 */

				lret = (*(NrmResourceDefaultProc)
						resources[i].nrm_default_addr)
						(resources[i].nrm_name,
						 resources[i].nrm_class,
						 base, resources[i].nrm_offset);
				if(lret > WARNING){
					resfound[i] = True;
					continue; /* this resource is finished*/
				}

				/* unspecified error - set to NULL */
				NHLPERROR((WARNING,E_UNKNOWN,
			"Unable to set %s to default value - Using NULL",
				NrmNameToString(resources[i].nrm_name)));
				to.addr = NULL;
				ret = MIN(ret,WARNING);
			}

			else if(resources[i].nrm_default_type == 
				resources[i].nrm_type){

				/* step #3 */

				to.addr = resources[i].nrm_default_addr;
				to.size = resources[i].nrm_size;
			}

			else{
				/* step #4 */

				NhlErrorTypes lret = NOERROR;

				from.size = 0;
				from.addr = resources[i].nrm_default_addr;

				lret = _NhlConvertData(
					resources[i].nrm_default_type,
					resources[i].nrm_type,&from,&to);
				
				if(lret < INFO){

				/* step #5 set return to WARNING*/
				/* and set resource to NULL	*/

					/*ERROR*/
					NHLPERROR((WARNING,E_UNKNOWN,
			"Unable to set %s to default value - Using NULL",
				NrmNameToString(resources[i].nrm_name)));
					to.addr = NULL;
					ret = MIN(ret,WARNING);
				}

			}

			/* copy resulting "to" into resource */

			if(resources[i].nrm_type == QString)
				*(NhlPointer *)
				(base + resources[i].nrm_offset) = to.addr;
			else if(to.addr == NULL)
				bzero(base + resources[i].nrm_offset,
							resources[i].nrm_size);
			else
				bcopy(to.addr,(base + resources[i].nrm_offset),
								to.size);
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
 * In Args:	Layer l;	The layer to be inited
 *		Args args;	The args to override Nrm
 *		int nargs;	Number of args
 * Out Args:	Layer l;	Inited l's resources
 *
 * Scope:	Global private
 * Returns:	nothing
 * Side Effect:	none
 */
NhlErrorTypes
_NhlGetResources
#if	__STDC__
(
	Layer		l,		/* layer to set resources of	*/
	_NhlArgList	args,		/* args to override res defaults*/
	int		num_args,	/* number of args		*/
	NrmQuarkList	child		/* layer is auto-managed chld	*/
)
#else
(l,args,num_args,child)
	Layer		l;		/* layer to set resources of	*/
	_NhlArgList	args;		/* args to override res defaults*/
	int		num_args;	/* number of args		*/
	NrmQuarkList	child;		/* layer is auto-managed chld	*/
#endif
{
	NrmQuark nameQ[MAXTREEDEPTH], classQ[MAXTREEDEPTH];
	LayerClass lc = _NhlClass(l);

	/*
	 * Get quarks list of names and classes to use in querying the Nrm
	 */

	if(GetNamesAndClasses(l,nameQ,classQ,MAXTREEDEPTH) < 0){
		/* ERROR- DON'T DO ANY RESOURCES */
		NhlPError(FATAL,E_UNKNOWN,
			"Instance Tree depth exceeds MAXTREEDEPTH of %d",
								MAXTREEDEPTH);
		return(FATAL);
	}

	return(GetResources((char *)l, nameQ, classQ,
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
 *		class with the resources of it's superclass.
 *
 * In Args:	LayerClass	lc;	layer class to create resource list for
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
	LayerClass lc	/* LayerClass to create full resource list for	*/
)
#else
(lc)
	LayerClass lc;	/* LayerClass to create full resource list for	*/
#endif
{
	NrmResourceList	rlist;
	LayerClass sc = lc->base_class.superclass;

	if(sc != NULL) {
	rlist = (NrmResourceList)NhlMalloc((unsigned)(sizeof(NrmResource) *
		(lc->base_class.num_resources+sc->base_class.num_resources)));
	bcopy((char*)(lc->base_class.resources),
		(char*)rlist,
		(unsigned)(sizeof(NrmResource)*lc->base_class.num_resources));
	bcopy((char*)(sc->base_class.resources),
		(char*)(&rlist[lc->base_class.num_resources]),
		(unsigned)(sizeof(NrmResource)* sc->base_class.num_resources));
	lc->base_class.resources = (NhlResourceList)rlist;
	lc->base_class.num_resources += sc->base_class.num_resources;
	}
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
		NhlPError(WARNING,E_UNKNOWN,
				"Unable to Get System Resource File Name?");
	}
	else
		NhlResDB = NrmGetFileDB(sysfile);
	
	if((void *)NhlResDB == (void *)NULL){
		NhlPError(WARNING,E_UNKNOWN,
			"Unable to load System Resource File %s",sysfile);
	}

	if((void *)usrfile == (void *)NULL){
		NhlPError(INFO,E_UNKNOWN,
				"Unable to Get User Resource File Name?");
	}
	else
		NrmCombineFileDB(usrfile,&NhlResDB,True);

	if((void *)NhlResDB == (void *)NULL){
		NrmPutStringRes(&NhlResDB,"DBLoaded","False");
		NhlPError(WARNING,E_UNKNOWN,
	"Unable to Create a Resource DB - Hard coded defaults will be used");

	}
	else
		NrmPutStringRes(&NhlResDB,"DBLoaded","True");

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
 *		_NhlArgList	oargs,		over-ride args
 *		int		num_oargs,	num oargs
 *		_NhlArgList	args,		args
 *		int		num_args	num args
 *
 * Out Args:	
 *		_NhlArgList	*ret_args,	return args
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
	_NhlArgList	*ret_args,	/* return args		*/
	int		*num_ret_args,	/* num ret_args		*/
	_NhlArgList	oargs,		/* over-ride args	*/
	int		num_oargs,	/* num oargs		*/
	_NhlArgList	args,		/* args			*/
	int		num_args	/* num args		*/
)
#else
(ret_args,num_ret_args,oargs,num_oargs,args,num_args)
	_NhlArgList	*ret_args;	/* return args		*/
	int		*num_ret_args;	/* num ret_args		*/
	_NhlArgList	oargs;		/* over-ride args	*/
	int		num_oargs;	/* num oargs		*/
	_NhlArgList	args;		/* args			*/
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
	 * allocate memory for return arg list
	 */
	*ret_args = (_NhlArgList)NhlMalloc((num_oargs + num_args) *
							sizeof(_NhlArg));
	if(*ret_args == NULL){
		NhlPError(FATAL,12,
				"Unable to Create args list to create child");
		return;
	}

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
			(*ret_args)[*num_ret_args].quark = args[i].quark;
			(*ret_args)[*num_ret_args].value = args[i].value;
			(*num_ret_args)++;
		}
	}

	/*
	 * Add all the oargs to the ret list
	 */
	for(i=0;i < num_oargs;i++){
		(*ret_args)[*num_ret_args].quark = oargs[i].quark;
		(*ret_args)[*num_ret_args].value = oargs[i].value;
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
 *		LayerClass	lc	class to check for res
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
	LayerClass	lc,	/* class to check for res	*/
	NrmQuark	res	/* resource to look for		*/
)
#else
(lc,res)
	LayerClass	lc;	/* class to check for res	*/
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
 *		Layer			l,		layer
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
	Layer			l,		/* layer		*/
	_NhlArgList		args_in,	/* args to sort		*/
	int			nargs_in,	/* number args to sort	*/
	_NhlArgList		args_out,	/* args not forwarded	*/
	int			*nargs_out,	/* num args_out		*/
	_NhlChildArgList	*forw_list,	/* list of args to frwd	*/
	NhlBoolean		getvalues	/* called frm getvalues	*/
)
#else
(l,args_in,nargs_in,args_out,nargs_out,forw_list,getvalues)
	Layer			l;		/* layer		*/
	_NhlArgList		args_in;	/* args to sort		*/
	int			nargs_in;	/* number args to sort	*/
	_NhlArgList		args_out;	/* args not forwarded	*/
	int			*nargs_out;	/* num args_out		*/
	_NhlChildArgList	*forw_list;	/* list of args to frwd	*/
	NhlBoolean		getvalues;	/* called frm getvalues	*/
#endif
{
	LayerClass		lc = _NhlClass(l);
	int			i;
	_NhlChildResList	reslist=NULL;
	_NhlChildArgList	arglist=NULL;
	NhlErrorTypes		ret = NOERROR;

	/*
	 * Init nargs_out
	 */
	*nargs_out = 0;

	/*
	 * Deal with simplest case
	 */
	if(nargs_in == 0){
		*nargs_out = 0;
		return NOERROR;
	}

	/*
	 * Deal with no children case - Parent gets all args
	 */
	if(lc->base_class.child_resources == NULL){
		bcopy(args_in,args_out,(int)(nargs_in * sizeof(_NhlArg)));
		*nargs_out = nargs_in;
		return NOERROR;
	}

	/*
	 * Initialize arg lists
	 */

	reslist = lc->base_class.child_resources;

	*forw_list = (_NhlChildArgList)NhlMalloc(sizeof(_NhlChildArgNode));
	if(*forw_list == NULL)
		return FATAL;

	arglist = *forw_list;

	while(reslist != NULL){

		arglist->class = reslist->class;
		arglist->autosetval = reslist->autosetval;
		arglist->next = NULL;
		arglist->nargs = 0;
		arglist->args = (_NhlArgList)NhlMalloc(nargs_in *
							sizeof(_NhlArg));
		if(arglist->args == NULL){
			_NhlFreeChildArgs(*forw_list);
			*forw_list = NULL;
			return FATAL;
		}

		if(reslist->next != NULL){

			arglist->next = (_NhlChildArgList)NhlMalloc(
						sizeof(_NhlChildArgNode));
			if(arglist->next == NULL){
				_NhlFreeChildArgs(*forw_list);
				return FATAL;
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

		/*
		 * if arg is a res in parent then arg is added to
		 * parents arg list and no other.
		 */
		if(_NhlResInClass(lc,args_in[i].quark)){
			args_out[*nargs_out].quark = args_in[i].quark;
			args_out[*nargs_out].value = args_in[i].value;
			(*nargs_out)++;
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
			NhlPError(INFO,E_UNKNOWN,
				"The resource %s is not in objects of class %s",
					NrmNameToString(args_in[i].quark),
							_NhlClassName(lc));
			ret = INFO;
		}
	}

	return ret;
}

/*
 * Function:	_NhlFreeChildArgs
 *
 * Description:	This function free's the memory associated with the child_args
 *		field of the given Layer instance.
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
	(void)NhlFree(list);

	return;
}
