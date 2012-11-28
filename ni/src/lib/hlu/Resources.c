/*
 *      $Id: Resources.c,v 1.42 2005-02-09 21:46:44 dbrown Exp $
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
 *		_NhlConvertArg
 *
 */
#include <ncarg/hlu/defs.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/AppI.h>


extern int
_NhlInitAllResources(
#if	NhlNeedProto
	NhlClass	lc
#endif
        );

/*
 * Static vars used to determine resoureces hard-coded defaults
 */

static NrmQuark QImmediate = NrmNULLQUARK;
static NrmQuark QProcedure = NrmNULLQUARK;
static NrmQuark QString = NrmNULLQUARK;
static NrmQuark byteQ = NrmNULLQUARK;
static NrmQuark charQ = NrmNULLQUARK;
static NrmQuark doubleQ = NrmNULLQUARK;
static NrmQuark floatQ = NrmNULLQUARK;
static NrmQuark shortQ = NrmNULLQUARK;
static NrmQuark stringQ = NrmNULLQUARK;
static NrmQuark intQ = NrmNULLQUARK;
static NrmQuark longQ = NrmNULLQUARK;
static NrmQuark genQ = NrmNULLQUARK;
static NrmQuark pointerQ = NrmNULLQUARK;

/*
 * Function:	_NhlCopyFromArgVal
 *
 * Description:	This function takes memory of a given size and copies it into
 *		another location.
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
	else if (size == sizeof(int))	*(int*)dst = src.intval;
	else if (size == sizeof(short))	*(short *)dst = src.shrtval;
	else if (size == sizeof(NhlPointer))
					*(NhlPointer *)dst = src.ptrval;
	else if (size == sizeof(char))	*(char *)dst = src.charval;
	else if (size == sizeof(char*))	*(char **)dst = src.strval;
	else if (size == sizeof(float))*(float*)dst = src.fltval;
	else if (size == sizeof(double))*(double*)dst = src.dblval;
	else if (size == sizeof(_NhlArgVal))
					*(_NhlArgVal *)dst = src;
	else
		memcpy((void*)dst,(void*)&src,size);
}

/*
 * Function:	_NhlConvertArg
 *
 * Description:	This function takes an "untyped" arg, and the name of
 *		the type it is being set to, (The user should have used
 *		this type of value as the va_arg) and converts the
 *		"untyped" arg to a "typed" arg.
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
NhlBoolean
_NhlConvertArg
#if	NhlNeedProto
(
	_NhlArg		*arg,
	NrmQuark	name,	/* name of type	*/
	unsigned int	size
) 
#else
(arg,name,size)
	_NhlArg		*arg;
	NrmQuark	name;	/* name of type	*/
	unsigned int	size;
#endif
{
	_NhlArgVal	tmp;

#ifdef	DEBUG
	memset(&tmp,0,sizeof(_NhlArgVal));
#endif

	if(_NhlIsSubtypeQ(floatQ,name))
		tmp.fltval = arg->value.dblval;
	else if(_NhlIsSubtypeQ(doubleQ,name))
		tmp.dblval = arg->value.dblval;
#ifdef	NGLONG2XINT
#ifdef  ByteSwapped
	else if(_NhlIsSubtypeQ(charQ,name))
		tmp.charval = (char)((int*)&arg->value.lngval)[0];
	else if(_NhlIsSubtypeQ(byteQ,name))
		tmp.byteval = (unsigned char)((int*)&arg->value.lngval)[0];
	else if(_NhlIsSubtypeQ(shortQ,name))
		tmp.shrtval = (short)((int*)&arg->value.lngval)[0];
	else if(_NhlIsSubtypeQ(intQ,name))
		tmp.intval = ((int*)&arg->value.lngval)[0];
#else
    /*
     * On non-byte swapped systems where ints are 4 bytes and longs are
     * 8 bytes, this macro must be set.  From the AL/VA interface all
     * non-long integral types are promoted to int, but they are pulled
     * off the va_arg stack as a long, and put in the arg->value union
     * as a long.  So, to get to the actual value, I treat the long as
     * an array of two ints, and pull off the second int.
	 */
	else if(_NhlIsSubtypeQ(charQ,name))
		tmp.charval = (char)((int*)&arg->value.lngval)[1];
	else if(_NhlIsSubtypeQ(byteQ,name))
		tmp.byteval = (unsigned char)((int*)&arg->value.lngval)[1];
	else if(_NhlIsSubtypeQ(shortQ,name))
		tmp.shrtval = (short)((int*)&arg->value.lngval)[1];
	else if(_NhlIsSubtypeQ(intQ,name))
		tmp.intval = ((int*)&arg->value.lngval)[1];
#endif
#else
	/*
	 * If sizeof(int) == sizeof(long), then this should work.
	 * In fact, it looks like it even works for the byte-swaped
	 * 64 bit DEC Alpha.
	 */
	else if(_NhlIsSubtypeQ(charQ,name))
		tmp.charval = (char)arg->value.intval;
	else if (_NhlIsSubtypeQ(byteQ,name))
		tmp.byteval = (unsigned char)arg->value.intval;
	else if(_NhlIsSubtypeQ(shortQ,name))
		tmp.shrtval = (short)arg->value.intval;
	else if(_NhlIsSubtypeQ(intQ,name))
		tmp.intval = arg->value.intval;
#endif
	/*
	 * I'm pretty sure sizeof(long)==sizeof(long) on all systems, but
	 * I wouldn't be supprised if the Cray gives us fits, since
	 * (10.0/1.0 != 10.0) == TRUE on the Cray...
	 */
	else if(_NhlIsSubtypeQ(longQ,name))
		tmp.lngval = arg->value.lngval;

	/*
	 * If we find a system where sizeof(NhlPointer) != sizeof(long),
	 * then these next entries will need to change.
	 */
	else if(_NhlIsSubtypeQ(stringQ,name))
		tmp.strval = (NhlString)arg->value.lngval;
	else if(_NhlIsSubtypeQ(genQ,name) ||
			_NhlIsSubtypeQ(pointerQ,name))
		tmp.ptrval = (NhlPointer)arg->value.lngval;
	else
		return False;

	arg->value = tmp;
	arg->type = name;
	arg->size = size;

	return True;
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
 * Function:	_NhlGetNamesAndClasses
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
int
_NhlGetNamesAndClasses
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

	for (i=0,j=0; i < num_args; i++){
		while(j < num_res) {
			if(args[i].quark > resources[j].nrm_name){
				j++;
				continue;
			}

			if(args[i].quark < resources[j].nrm_name)
				break;

			if(resources[j].res_info & _NhlRES_NOCACCESS){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:%s does not have \"C\" access",
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

				lret = _NhlConvertData(context,args[i].type,
						resources[j].nrm_type,
						&from, &to);
					
				if(lret<NhlINFO){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Error retrieving resource %s from args - Ignoring Arg",
					NrmNameToString(resources[j].nrm_name));
						ret = MIN(NhlWARNING,ret);
				}
				else{
					resfound[j] = True;
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
		int		immediate;

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
					
					if(lret >= NhlINFO){
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
					
					if(lret >= NhlINFO){
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
					if(resources[i].nrm_size ==
							 sizeof(NhlPointer)){
						to.data.ptrval =
						 &resources[i].nrm_default_val;
						to.size = sizeof(NhlPointer);
					}
					else{
					immediate = (int)
					resources[i].nrm_default_val.lngval;
					to.data.ptrval = &immediate;
					to.size = sizeof(int);
					}
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

	if(_NhlGetNamesAndClasses(l,nameQ,classQ,_NhlMAXTREEDEPTH) < 0){
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
	NhlClass	lc,
	NhlResourceList	resources,	/* resources		*/
	int		num_resources	/* number of resources	*/
)
#else
(lc,resources, num_resources)
	NhlClass	lc;
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
		nrmres->nhlclass	= lc;
	}
#undef PSToQ

	return;
}

static int
CompareRes
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
	NrmResource	*one = (NrmResource*)ov;
	NrmResource	*two = (NrmResource*)tv;

	if(one->nrm_name < two->nrm_name)
		return -1;
	if(one->nrm_name > two->nrm_name)
		return 1;
	return 0;
}

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

	/*
	 * If superclass has no resources then the classes resource list
	 * is already complete.
	 */
	if((sc == NULL) || (sc->base_class.num_resources < 1)){
		num_rlist = lc->base_class.num_resources;
		rlist = (NrmResourceList)lc->base_class.resources;
		goto SORT;
	}

	/*
	 * If class doesn't have any resources then point to the
	 * super-classes resources.
	 */
	if(lc->base_class.num_resources < 1){
		lc->base_class.resources = sc->base_class.resources;
		lc->base_class.num_resources = sc->base_class.num_resources;

		return;
	}

	super_size = sc->base_class.layer_size;
	num_rlist = lc->base_class.num_resources + sc->base_class.num_resources;

	rlist = (NrmResourceList)NhlMalloc(
				(unsigned)(sizeof(NrmResource) * num_rlist));
	if(!rlist){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	memcpy((char*)rlist,(char*)(sc->base_class.resources),
			sc->base_class.num_resources * sizeof(NrmResource));

	classlist = (NrmResourceList)lc->base_class.resources;
	next = sc->base_class.num_resources;

	for(i=0;i < lc->base_class.num_resources;i++){

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
					/*
					 * If resource name is the same,
					 * preserve nhlclass - probably
					 * just over-riding a default value.
					 */
					if(classlist[i].nrm_name ==
							rlist[j].nrm_name){
						NhlClass tc = rlist[j].nhlclass;
						rlist[j] = classlist[i];
						rlist[j].nhlclass = tc;
					}
					else
						rlist[j] = classlist[i];
					num_rlist--;
					goto NEXTRES;
				}
			}
		}

		rlist[next++] = classlist[i];
NEXTRES:
		;
	}

SORT:
	/*
	 * Sort the resource list.
	 */
	qsort(rlist,num_rlist,sizeof(NrmResource),CompareRes);

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
	longQ = NrmStringToQuark(NhlTLong);
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
	int		i,j,k;
	int		total_args;

	/*
	 * Initialize num_ret_args
	 */
	*num_ret_args = 0;

	total_args = num_oargs + num_args;
	/*
	 * take care of simplest case
	 */
	if(total_args == 0)
		return;

	/*
	 * Add args from args list only if that argname doesn't exist in
	 * the override arg list
	 */
	i=0;j=0;k=0;
	while(i < num_args && j < num_oargs){

		if(args[i].quark < oargs[j].quark)
			ret_args[k++] = args[i++];
		else if(args[i].quark > oargs[j].quark)
			ret_args[k++] = oargs[j++];
		else
			i++;

	}
	while(i < num_args)
		ret_args[k++] = args[i++];
	while(j < num_oargs)
		ret_args[k++] = oargs[j++];

	*num_ret_args = k;
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
	NrmResourceList	nrmres = (NrmResourceList)lc->base_class.resources;
	NrmResource	key;

	key.nrm_name = res;

	nrmres = bsearch(&key,(NrmResourceList)lc->base_class.resources,
			lc->base_class.num_resources,sizeof(NrmResource),
			CompareRes);

	return (nrmres != NULL);
}

NrmResource *
_NhlGetResInfo
#if	NhlNeedProto
(
	NhlClass	lc,
	NrmQuark	res
)
#else
(lc,res)
	NhlClass	lc;
	NrmQuark	res;
#endif
{
	NrmResourceList		nrmres = (NrmResourceList)
						lc->base_class.resources;
	NrmResource		key;
	_NhlChildResList	chld;

	key.nrm_name = res;
	nrmres = bsearch(&key,(NrmResourceList)lc->base_class.resources,
			lc->base_class.num_resources,sizeof(NrmResource),
			CompareRes);

	if(nrmres)
		return nrmres;

        if (lc->base_class.class_inited & _NhlObjClassFlag)
                return NULL;
        
	chld = lc->base_class.child_resources;
	while(chld){

		if(NrmQinQList(chld->resources,res))
			return _NhlGetResInfo(chld->theclass,res);

		chld = chld->next;
	}

	return NULL;
}

/*
 * Function:	_NhlGetNativeResInfo
 *
 * Description:	For resources that are intercepted by a parent, this
 * function finds the class that ultimately defines the resource. It fills
 * an NrmResource * array with the res info from each class that defines a
 * resource for itself, rather than intercept it from a child. In most
 * cases only one child class defines a particular resource, but in a few
 * situations more than one child defines the resource. At present no more
 * than two children define any top level resource in the HLU library, but
 * that could change eventually.
 *
 * Storage for the NrmResource * array must be allocated by the caller.
 * 2 elements are enough for now but more would be safer.
 * Since the routine is recursive, the count parameter which returns the
 * number of children defining the resource must be pre-initialized to 0
 * by the caller.
 *
 * In Args:	
 *		NhlClass	lc	top level class with intercepted res.
 *		NrmQuark	res	intercepted resource
 *
 * Out Args:	
 *              int *           count   number of classes defining resource:
 *                                     	MUST BE PRE-INITIALIZED TO 0.
 *              NrmResource **  native_res  array of NrmResource *'s:
 *					MUST CONTAIN AEDEQUATE STORAGE.
 *              Note there are no checks to ensure that storage is
 *              aedequate or that count is properly intiialized.
 *
 * Scope:	Global Private
 * Returns:	NhlBoolean
 * Side Effect:	
 */

void _NhlGetNativeResInfo
(
        NhlClass class,
        NrmName  res,
        int	 *count,
        NrmResource **native_res
        )
{
        _NhlChildResList chld = class->base_class.child_resources;

        while (chld) {
                NrmResource *chld_res = _NhlGetResInfo(chld->theclass,res);
                if (chld_res) {
                        if (chld_res->res_info & _NhlRES_INTERCEPTED) {
                                _NhlGetNativeResInfo
                                        (chld->theclass,res,count,native_res);
                        }
                        else {
                                native_res[*count] = chld_res;
                                (*count)++;
                        }
                }
                chld = chld->next;
        }
        return;
}


static int
UserCompareRes
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
	NrmQuark	*one = (NrmQuark *)ov;
	NrmQuark	*two = (NrmQuark *)tv;

        return strcmp(NrmQuarkToString(*one),NrmQuarkToString(*two));
}

NrmNameList
_NhlGetUserResources
#if	NhlNeedProto
(
	NhlClass	lc,
        int		*res_count
)
#else
(lc,res_count)
	NhlClass	lc;
        int		*res_count;
#endif
{
        NhlErrorTypes ret=NhlNOERROR;
        int i,j,count;
        NrmNameList all_res,qnames;
                
        if(! lc->base_class.class_inited) {
                ret = _NhlInitializeClass(lc);
                if (ret < NhlWARNING) {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                   "Unable to initialize class %s",
                                   lc->base_class.class_name));
                        return NULL;
                }
        }
        
        if (! lc->base_class.all_resources) {
                count = _NhlInitAllResources(lc);
                if (count < 0) {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                             "Unable to initialize all resources for class %s",
                                   lc->base_class.class_name));
                        return NULL;
                }
        }
        else {
                qnames = lc->base_class.all_resources;
                for (i=0; qnames[i] != NrmNULLQUARK; i++)
                        ;
                count = i;
        }
                
        qnames = NhlMalloc((count+1) * sizeof(NrmName));
        if (! qnames) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "Dynamic memory allocation error"));
                return NULL;
        }
	all_res = lc->base_class.all_resources;

	for (i=0,j=0; i < count; i++) {
		NrmResource *res = _NhlGetResInfo(lc,all_res[i]);
		if (res->res_info & _NhlRES_PRIVATE)
			continue;
		qnames[j++] = all_res[i];
	}
        qnames[j] = NrmNULLQUARK;
	*res_count = j;

        qsort(qnames,*res_count,sizeof(NrmName),UserCompareRes);
        
        return qnames;
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

		arglist->theclass = reslist->theclass;
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
				if(chld_node->theclass == reslist->theclass)
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
					if(getvalues){
						*args_in[i].chld_class =
								reslist->theclass;
						break;
					}
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
