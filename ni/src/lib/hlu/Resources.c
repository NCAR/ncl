/*
 *      $Id: Resources.c,v 1.25 1995-05-09 22:17:55 boote Exp $
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
#include <ncarg/hlu/defs.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/AppI.h>

/*
 * Static vars used to determine resoureces hard-coded defaults
 */

static NrmQuark QImmediate = NrmNULLQUARK;
static NrmQuark QProcedure = NrmNULLQUARK;
static NrmQuark QString = NrmNULLQUARK;
static NrmQuark	byteQ = NrmNULLQUARK;
static NrmQuark	charQ = NrmNULLQUARK;
static NrmQuark	doubleQ = NrmNULLQUARK;
static NrmQuark	floatQ = NrmNULLQUARK;
static NrmQuark	shortQ = NrmNULLQUARK;
static NrmQuark	stringQ = NrmNULLQUARK;
static NrmQuark	intQ = NrmNULLQUARK;
static NrmQuark	genQ = NrmNULLQUARK;
static NrmQuark	pointerQ = NrmNULLQUARK;

/*
 * Function:	_NhlCopyFromArgVal
 *
 * Description:	This function takes memory of a given size and copies it into
 *		another location. It doesn't actually change dst - it sets
 *		the memory pointed to by dst. ie. *dst = the value.
 *		This differs from _NhlCopyFromArg in that it expects the
 *		src values to be "left" justified - correctly placed in the
 *		enum.
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
_NhlCopyFromArgVal
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
	if(size == sizeof(long))	*(long *)dst = src.lngval;
	else if (size == sizeof(short))	*(short *)dst = src.shrtval;
	else if (size == sizeof(NhlPointer))
					*(NhlPointer *)dst = src.ptrval;
	else if (size == sizeof(char))	*(char *)dst = src.charval;
	else if (size == sizeof(char*))	*(char **)dst = src.strval;
	else if (size == sizeof(_NhlArgVal))
					*(_NhlArgVal *)dst = src;
	else
		memcpy((void*)dst,(void*)&src,size);
}

/*
 * Function:	_NhlCopyFromArg
 *
 * Description:	This function takes memory of a given size and copies it into
 *		another location. It doesn't actually change dst - it sets
 *		the memory pointed to by dst. ie. *dst = the value.
 *		This differs from _NhlCopyFromArgVal in that it assumes the
 *		values to be "right" justified in the enum because they
 *		were all placed in as .lngval in the VA and AL interfaces.
 *		So they need to be retrieved as "lngval" here.
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
#if	NhlNeedProto
(
	_NhlArgVal	src,	/* source	*/
	void *		dst,	/* destination	*/
	NrmQuark	name,	/* name		*/
	unsigned int	size	/* size		*/
) 
#else
(src,dst,name,size) 
	_NhlArgVal	src;	/* source	*/
	void*		dst;	/* destination	*/
	NrmQuark	name;	/* name		*/
	unsigned int	size;	/* size		*/
#endif
{

	if(_NhlIsSubtypeQ(floatQ,name)) *(float *)dst = *(float*)&src.lngval;
	else if(_NhlIsSubtypeQ(doubleQ,name))
					*(double *)dst = *(float*)&src.lngval;
	else if(_NhlIsSubtypeQ(charQ,name)) *(char *)dst = (char)src.lngval;
	else if(_NhlIsSubtypeQ(byteQ,name)) *(char *)dst = (char)src.lngval;
	else if(_NhlIsSubtypeQ(shortQ,name)) *(short *)dst = (short)src.lngval;
	else if(_NhlIsSubtypeQ(intQ,name)) *(int *)dst = (int)src.lngval;
	else if(_NhlIsSubtypeQ(stringQ,name))
				*(NhlString *)dst = (NhlString)src.lngval;
	else if(_NhlIsSubtypeQ(genQ,name))
				*(NhlPointer *)dst = (NhlPointer)src.lngval;
	else if(_NhlIsSubtypeQ(pointerQ,name))
				*(NhlPointer *)dst = (NhlPointer)src.lngval;
	else if(size == sizeof(long))	*(long *)dst = src.lngval;
	else if (size == sizeof(_NhlArgVal))	*(_NhlArgVal *)dst = src;
	else memcpy((void*)dst,(void*)&src,size);

	return;
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
#if	NhlNeedProto
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

    if      (size == sizeof(long)) *(long *)dst->ptrval = *(long*)src;
    else if (size == sizeof(float)) *(float *)dst->ptrval = *(float*)src;
    else if (size == sizeof(double)) *(double *)dst->ptrval = *(double*)src;
    else if (size == sizeof(int)) *(int *)dst->ptrval = *(int*)src;
    else if (size == sizeof(short)) *(short *)dst->ptrval = *(short*)src;
    else if (size == sizeof(NhlPointer)) *(NhlPointer*)dst->ptrval =
							*(NhlPointer*)src;
    else if (size == sizeof(char))	*(char *)dst->ptrval = *(char*)src;
    else if (size == sizeof(char*))	*(char **)dst->ptrval = *(char**)src;
    else if (size == sizeof(_NhlArgVal))
			*(_NhlArgVal *)dst->ptrval = *(_NhlArgVal*)src;
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
#if	NhlNeedProto
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

	if(len >= length) return (-len);

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
 * Function:	_NhlGetResources
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
NhlErrorTypes
_NhlGetResources
#if	NhlNeedProto
(
	_NhlConvertContext	context,/* context for converter allocs	*/
	NrmDatabase		resdb,	/* db				*/
	char			*base,	/* addr to write res-values to	*/
	NrmQuarkList		nameQ,	/* Qlist of names in instance	*/
	NrmQuarkList		classQ,	/* Qlist of classes in instance	*/
	NrmResourceList		resources,/* list of resources		*/
	int			num_res,/* number of resources		*/
	_NhlArgList		args,	/* args to override defaults	*/
	int			num_args,/* number of args		*/
	NrmQuarkList		*child	/* look at parent's DB level too*/
)
#else
(context,resdb,base,nameQ,classQ,resources,num_res,args,num_args,child)
	_NhlConvertContext	context;/* context for converter allocs	*/
	NrmDatabase		resdb;	/* db				*/
	char			*base;	/* addr to write res-values to	*/
	NrmQuarkList		nameQ;	/* Qlist of names in instance	*/
	NrmQuarkList		classQ;	/* Qlist of classes in instance	*/
	NrmResourceList		resources;/* list of resources		*/
	int			num_res;/* number of resources		*/
	_NhlArgList		args;	/* args to override defaults	*/
	int			num_args;/* number of args		*/
	NrmQuarkList		*child;	/* look at parent's DB level too*/
#endif
{
	char		func[] = "_NhlGetResources";
	register int	i,j;
	NhlBoolean	resfound[_NhlMAXRESLIST];
	NhlBoolean	argfound[_NhlMAXARGLIST];
	NhlErrorTypes	ret = NhlNOERROR;
	NhlErrorTypes	lret = NhlNOERROR;
	NrmHashTable	stackslist[_NhlMAXRESLIST];
	NrmHashTable	*slist = stackslist;
	int		slistlen = _NhlMAXRESLIST;

	/* Mark each resource as not found */
	memset((char *)resfound,0, (int)(num_res * sizeof(NhlBoolean)));

	/* Mark each arg as not found */
	memset((char *) argfound,0, (int)(num_args * sizeof(NhlBoolean)));

	/* Set resources specified via args */

	for (i=0; i < num_args; i++){
		for(j=0; j < num_res; j++) {
			if(args[i].quark == resources[j].nrm_name) {
				if(resources[j].res_info & _NhlRES_NOCACCESS){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:%s does not have \"C\" access",
					func,NrmQuarkToString(args[i].quark));
					args[i].quark = NrmNULLQUARK;
				}
				else if(args[i].type == NrmNULLQUARK){
					_NhlCopyFromArg(args[i].value,
					(char*)(base + resources[j].nrm_offset),
					resources[j].nrm_name,
					resources[j].nrm_size);
					resfound[j] = True;
				}
				else if(args[i].type==resources[j].nrm_type){
					_NhlCopyFromArgVal(args[i].value,
					(char*)(base + resources[j].nrm_offset),
					resources[j].nrm_size);
					resfound[j] = True;
				}
				else{
					/* 
					 * call converter
					 */
					NrmValue	from, to;

					from.size = args[i].size;
					from.data = args[i].value;
					to.size = resources[j].nrm_size;
					to.data.ptrval =(NhlPointer)(base +
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

	while(!NrmQGetSearchList(resdb,nameQ,classQ,slist,slistlen)){

		if(slist == stackslist)
			slist = NULL;
		
		slistlen *= 2;
		slist = (NrmHashTable *)NhlRealloc(slist,
					sizeof(NrmHashTable) * slistlen);
		if(slist == NULL)
			return NhlFATAL;
	}

	for (i=0; i < num_res; i++){
		NhlBoolean	raccess;

		if(resfound[i])		/* resource set via arg or datares*/
			goto found;

		raccess = !(resources[i].res_info&_NhlRES_NORACCESS);

		/* Set resources via Nrm databases */
		if(raccess){
			NrmValue	from, to;
			NrmQuark	rdbtype;

			if(NrmGetQResFromList(slist,resources[i].nrm_name,
				resources[i].nrm_class,&rdbtype,&from)){

				if(rdbtype != resources[i].nrm_type){

					if(rdbtype == QString)
						from.size =
						sizeof(NhlString);
					to.size = resources[i].nrm_size;
					to.data.ptrval =(void *)(base +
						resources[i].nrm_offset);

					lret = _NhlConvertData(context,rdbtype,
							resources[i].nrm_type,
							&from, &to);
					
					if(lret == NhlNOERROR){
						resfound[i] = True;
						goto found;
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
						*(NhlString *)
						(base+resources[i].nrm_offset)=
							from.data.strval;
					}
					else{
						memcpy(base +
							resources[i].nrm_offset,
							from.data.ptrval,
							resources[i].nrm_size);
					}
					resfound[i] = True;
					goto found;
				}
			}
		}

		/*
		 * Set resources via Nrm database at parents level
		 * As long as the resource should be forwarded.
		 */
		if(raccess && child != NULL){
			NrmQuark	lname[_NhlMAXTREEDEPTH];
			NrmQuark	lclass[_NhlMAXTREEDEPTH];
			int		tint;
			int		tree_len;

			tree_len = 0;
			while(nameQ[tree_len]) tree_len++;

			memcpy((void*)lname,(void*)nameQ,
						(tree_len+1)*sizeof(NrmQuark));
			memcpy((void*)lclass,(void*)classQ,
						(tree_len+1)*sizeof(NrmQuark));
			for(tint=0;(tint < tree_len-1) && child[tint] &&
				NrmQinQList(child[tint],resources[i].nrm_name);
									tint++){

				NrmValue	from, to;
				NrmQuark	rdbtype;

				/*
				 * remove last Q from lname/lclass:
				 * Retrieve resource as if it belongs to the
				 * parent level.
				 * retrieve resouce using NrmQGetResource.
				 * 
				 */
				lname[tree_len-tint] = NrmNULLQUARK;
				lclass[tree_len-tint] = NrmNULLQUARK;
				lname[tree_len-1-tint] = resources[i].nrm_name;
				lclass[tree_len-1-tint] =resources[i].nrm_class;


				if(NrmQGetResource(resdb,lname,lclass,&rdbtype,
									&from)){

					if(rdbtype != resources[i].nrm_type){

					to.size = resources[i].nrm_size;
					to.data.ptrval =(NhlPointer)(base +
						resources[i].nrm_offset);

					lret = _NhlConvertData(context,rdbtype,
							resources[i].nrm_type,
							&from, &to);
					
					if(lret == NhlNOERROR){
						resfound[i] = True;
						goto found;
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
						*(NhlString *)
						(base+resources[i].nrm_offset)=
						from.data.strval;
					}
					else{
						memcpy(
						base + resources[i].nrm_offset,
						from.data.ptrval,
							resources[i].nrm_size);
					}
					resfound[i] = True;
					goto found;
					}
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

			to.size = 0;
			to.data.ptrval = NULL;

			if(resources[i].nrm_default_type == QImmediate){

				/* step #1 */

				if(resources[i].nrm_type == QString){
					to.data.strval =
					resources[i].nrm_default_val.strval;
					if(to.data.strval != NULL)
					to.size= strlen(to.data.strval) + 1;
				}
				else{
					to.data.ptrval =
						&resources[i].nrm_default_val;
					/*
					 * size is size of first element of
					 * _NhlArgVal union.  Should be
					 * NhlPointer so func's and strings
					 * work correctly.
					 */
					to.size = resources[i].nrm_size;
				}
			}

			else if(resources[i].nrm_default_type == QProcedure){

				NrmResourceDefaultProc	def_proc =
					(NrmResourceDefaultProc)
					resources[i].nrm_default_val.ptrval;

				/* step #2 */

				lret = (*def_proc)(resources[i].nrm_name,
						 resources[i].nrm_class,
						 base,resources[i].nrm_offset);
				if(lret > NhlWARNING){
					resfound[i] = True;
					goto found; /* this resource is finished*/
				}

				/* unspecified error - set to NULL */
				NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"Unable to set %s to default value - Using NULL",
				NrmNameToString(resources[i].nrm_name)));
				to.data.ptrval = NULL;
				ret = MIN(ret,NhlWARNING);
			}

			else if(resources[i].nrm_default_type == 
				resources[i].nrm_type){

				/* step #3 */

				to.data = resources[i].nrm_default_val;
				to.size = resources[i].nrm_size;
			}

			else{
				/* step #4 */

				/*
				 * size must be the size of the first member
				 * of the union. (ptrval)
				 */
				from.size = sizeof(NhlPointer);
				from.data = resources[i].nrm_default_val;

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
					to.data.ptrval = NULL;
					ret = MIN(ret,NhlWARNING);
				}

			}

			/* copy resulting "to" into resource */

			if(resources[i].nrm_type == QString)
				*(NhlString *)
				(base+resources[i].nrm_offset) = to.data.strval;
			else if(to.data.ptrval == NULL)
				memset(base + resources[i].nrm_offset,0,
							resources[i].nrm_size);
			else
				memcpy((base+resources[i].nrm_offset),
							to.data.ptrval,to.size);
			resfound[i] = True;
		}
		found:
		;
	}

	if(slist != stackslist)
		(void)NhlFree(slist);
	return(ret);
}

/*
 * Function:	_NhlGetLayerResources
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
_NhlGetLayerResources
#if	NhlNeedProto
(
	_NhlConvertContext	context,
	NhlLayer		l,	/* layer to set resources of	*/
	_NhlArgList		args,	/* args to override res defaults*/
	int			num_args,/* number of args		*/
	NrmQuarkList		*child	/* layer is auto-managed chld	*/
)
#else
(context,l,args,num_args,res_list,num_res,child)
	_NhlConvertContext	context;
	NhlLayer		l;	/* layer to set resources of	*/
	_NhlArgList		args;	/* args to override res defaults*/
	int			num_args;/* number of args		*/
	NrmQuarkList		*child;	/* layer is auto-managed chld	*/
#endif
{
	NrmQuark nameQ[_NhlMAXTREEDEPTH], classQ[_NhlMAXTREEDEPTH];
	NhlClass lc = _NhlClass(l);

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

	return(_NhlGetResources(context,_NhlGetResDB(l),(char*)l,nameQ,classQ,
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
#if	NhlNeedProto
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

		if(resources->resource_size > sizeof(_NhlArgVal)){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"resource %s has an invalid size",
						resources->resource_name);
			nrmres->nrm_size = sizeof(_NhlArgVal);
		}

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
 *		NhlClass over-rides any of the resources in the super-class
 *
 * In Args:	NhlClass	lc;	layer class to create resource list for
 *
 * Out Args:	lc->base_class.resources is updated.
 *
 * Scope:	Global private
 * Returns:	NULL
 * Side Effect:	
 */
void
_NhlGroupResources
#if	NhlNeedProto
(
	NhlClass lc	/* Class to create full reslist	for	*/
)
#else
(lc)
	NhlClass lc;	/* Class to create full reslist	for	*/
#endif
{
	NrmResourceList	rlist;
	int		num_rlist;
	int		i,j,next;
	NrmResourceList	classlist;
	NhlClass	sc = lc->base_class.superclass;
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
 *		resource values later.
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
_NhlResourceListInitialize
#if	NhlNeedProto
(
	void
) 
#else
() 
#endif
{ 
	QImmediate = NrmStringToName(NhlTImmediate);
	QProcedure = NrmStringToName(NhlTProcedure);
	QString = NrmStringToName(NhlTString);

	byteQ = NrmStringToQuark(NhlTByte);
	charQ = NrmStringToQuark(NhlTCharacter);
	doubleQ = NrmStringToQuark(NhlTDouble);
	floatQ = NrmStringToQuark(NhlTFloat);
	shortQ = NrmStringToQuark(NhlTShort);
	stringQ = NrmStringToQuark(NhlTString);
	intQ = NrmStringToQuark(NhlTInteger);
	genQ = NrmStringToQuark(NhlTGenArray);
	pointerQ = NrmStringToQuark(NhlTPointer);

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
#if	NhlNeedProto
(
	_NhlArgList	ret_args,	/* return args		*/
	int		*num_ret_args,	/* num ret_args		*/
	_NhlArgList	oargs,		/* over-ride args	*/
	int		num_oargs,	/* num oargs		*/
	_NhlArgList	args,		/* args			*/
	int		num_args	/* num args		*/
)
#else
(ret_args,num_ret_args,oargs,num_oargs,args,num_args)
	_NhlArgList	ret_args;	/* return args		*/
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
 *		NhlClass	lc	class to check for res
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
#if	NhlNeedProto
(
	NhlClass	lc,	/* class to check for res	*/
	NrmQuark	res	/* resource to look for		*/
)
#else
(lc,res)
	NhlClass	lc;	/* class to check for res	*/
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
#if	NhlNeedProto
(
	NhlLayer		l,		/* layer		*/
	_NhlArgList		args_in,	/* args to sort		*/
	int			nargs_in,	/* number args to sort	*/
	_NhlArgList		*args_out,	/* args not forwarded	*/
	int			*nargs_out,	/* num args_out		*/
	_NhlChildArgList	*forw_list,	/* list of args to frwd	*/
	NhlBoolean		*args_used,	/* args used		*/
	NhlBoolean		getvalues	/* called frm getvalues	*/
)
#else
(l,args_in,nargs_in,args_out,nargs_out,forw_list,args_used,getvalues)
	NhlLayer		l;		/* layer		*/
	_NhlArgList		args_in;	/* args to sort		*/
	int			nargs_in;	/* number args to sort	*/
	_NhlArgList		*args_out;	/* args not forwarded	*/
	int			*nargs_out;	/* num args_out		*/
	_NhlChildArgList	*forw_list;	/* list of args to frwd	*/
	NhlBoolean		*args_used;	/* args used		*/
	NhlBoolean		getvalues;	/* called frm getvalues	*/
#endif
{
	NhlClass		lc = _NhlClass(l);
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
		arglist->args = NhlMalloc(nargs_in * sizeof(_NhlArg));
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
			(*args_out)[*nargs_out] = args_in[i];
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
					/*
					 * If called from get_values make
					 * sure there is an instance of the
					 * class to retrieve from.
					 * If no instance, then continue on
					 * to next class type.
					 */
					if(getvalues){
						_NhlChildList	chld_node =
							l->base.children;

						while(chld_node != NULL){
				if(chld_node->class == reslist->class)
								break;
							chld_node =
								chld_node->next;
						}
						if(chld_node == NULL){
							arglist = arglist->next;
							reslist = reslist->next;
							continue;
						}
					}
					arglist->args[arglist->nargs] =
								args_in[i];
					arglist->args_used[arglist->nargs] =
								&args_used[i];
					(arglist->nargs)++;
					argfound = True;

					/*
					 * Only add resoure to one list for
					 * getvalues.
					 */
					if(getvalues)
						break;
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
#if	NhlNeedProto
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
