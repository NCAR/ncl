/*
 *      $Id: Convert.c,v 1.1 1993-04-30 17:21:24 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Convert.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Sep 11 13:46:25 MDT 1992
 *
 *	Description:	This file contains all the functions that allow the
 *			user and other entities to interact with the type
 *			conversion facility.  The type converter utilities
 *			use an internal table accessed via a hash function
 *			to get to the appropriate converter structure and
 *			a member of that structure contains all the cached
 *			values converted by that function.
 */

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ConvertP.h>


#define HASHFUNC(a,b)	((((a)*HASHMULT) + (b)) & HASHMASK)

static NhlConvertPtr HashTable[HASHSIZE] = { NULL};

/*
 * Function:	CreateConvArgs
 *
 * Description:	This function takes an NhlConvertArgList and returns an
 *		NrmValue* with the data in it for internal use.
 *
 * In Args:	NhlConvertArgList	args	list of args
 *		int			nargs	number of args
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NrmValue*
 * Side Effect:	
 */
static NrmValue*
CreateConvArgs
#if	__STDC__
(
	NhlConvertArgList	args,	/* list of args		*/
	int			nargs	/* number of args	*/
)
#else
(args,nargs)
	NhlConvertArgList	args;	/* list of args		*/
	int			nargs;	/* number of args	*/
#endif
{
	register int	i;
	NrmValue*	newargs=NULL;

	newargs = (NrmValue*)NhlMalloc(nargs * sizeof(NrmValue));
	if(newargs == NULL){
		NhlPError(FATAL,E_UNKNOWN,"Unable to copy convert Args");
		return NULL;
	}

	for(i=0;i < nargs; i++){

		switch(args[i].addressmode){

			case NHLIMMEDIATE:
				*(unsigned int*)&newargs[i].addr = args[i].addr;
				/* -size means don't call free on addr later */
				newargs[i].size = -args[i].size;

				break;

			case NHLADDR:
				newargs[i].addr = (void*)
							NhlMalloc(args[i].size);
				bcopy((char *)(args[i].addr),
					(char *)(newargs[i].addr),args[i].size);
				newargs[i].size = args[i].size;

				break;

			default:
				NhlPError(FATAL,E_UNKNOWN,
				"addressmode of convert arg[%d] not valid",i);
				(void)NhlFree(newargs);
				return(NULL);
		}
	}

	return(newargs);
}

/*
 * Function:	insertConverter
 *
 * Description:	This function takes a pointer to a converter record and
 *		inserts it into the table in the correct point.
 *
 * In Args:	NrmValue	*ptr	pointer to converter record
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
insertConverter
#if	__STDC__
(
	NhlConvertPtr	ptr	/* pointer to converter record	*/
)
#else
(ptr)
	NhlConvertPtr	ptr;	/* pointer to converter record	*/
#endif
{
	int entry;

	entry = HASHFUNC(ptr->fromtype,ptr->totype);

	if(HashTable[entry] != (NhlConvertPtr)NULL)
		ptr->next = HashTable[entry];

	HashTable[entry] = ptr;

	return(NOERROR);
}

/*
 * Function:	_NhlRegisterConverter
 *
 * Description:	This function is the private interface for registering a
 *		converter function.
 *
 * In Args:	NrmQuark		from	from type
 *		NrmQuark		to	to type
 *		NhlTypeConverter	conv	the converter function
 *		NhlCacheClosure		close	func for freeing cached data
 *		NhlBoolean		cache	Cache results?
 *		NhlConvertArgList	args	list of conversion args
 *		int			nargs	number of args
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlRegisterConverter
#if     __STDC__
( 
	NrmQuark		from,		/* from type		*/
	NrmQuark		to,		/* to type		*/
	NhlTypeConverter	convert,	/* the converter function*/ 
	NhlConvertArgList	args,		/* conversion args	*/ 
	int			nargs,		/* number of args	*/ 
	NhlBoolean		cache,		/* cache results?	*/ 
	NhlCacheClosure		close		/* for freeing cache data*/ 
)
#else
(from,to,convert,args,nargs,cache,close)
	NrmQuark		from;		/* from type		*/
	NrmQuark		to;		/* to type		*/
	NhlTypeConverter	convert;	/* the converter function*/ 
	NhlConvertArgList	args;		/* conversion args	*/ 
	int			nargs;		/* number of args	*/ 
	NhlBoolean		cache;		/* cache results?	*/ 
	NhlCacheClosure		close;		/* for freeing cache data*/ 
#endif 
{
	NhlConvertPtr		cvtrec = NULL;

	if(convert == NULL) return(WARNING);

	cvtrec = (NhlConvertPtr)NhlMalloc(sizeof(NhlConvertRec));
	if(cvtrec == NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"Unable to allocate memory for Converter %s to %s",
				NrmNameToString(from),NrmNameToString(to));
		return FATAL;
	}

	cvtrec->next = NULL;
	cvtrec->fromtype = from;
	cvtrec->totype = to;
	cvtrec->converter = convert;
	cvtrec->cacheit = cache;
	cvtrec->closure = (cache) ? close : (NhlCacheClosure)NULL;

	cvtrec->nargs = nargs;
	if(nargs > 0){
		cvtrec->args = CreateConvArgs(args,nargs);
		if(cvtrec->args == NULL){
			NhlPError(FATAL,E_UNKNOWN,
					"Unable to install Converter %s to %s",
							NrmNameToString(from),
							NrmNameToString(to));
			(void)NhlFree(cvtrec);
			return(WARNING);
		}
	}
	else
		cvtrec->args = NULL;


	return(insertConverter(cvtrec));
}

/*
 * Function:	NhlRegisterConverter
 *
 * Description:	This function is the public interface for registering a
 *		converter function.
 *
 * In Args:	NhlString		from	from type
 *		NhlString		to	to type
 *		NhlTypeConverter	conv	the converter function
 *		NhlCacheClosure		close	func for freeing cached data
 *		NhlBoolean		cache	Cache results?
 *		NhlConvertArgList	args	list of conversion args
 *		int			nargs	number of args
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRegisterConverter
#if     __STDC__
( 
	NhlString		from,		/* from type		*/
	NhlString		to,		/* to type		*/
	NhlTypeConverter	convert,	/* the converter function*/ 
	NhlConvertArgList	args,		/* conversion args	*/ 
	int			nargs,		/* number of args	*/ 
	NhlBoolean		cache,		/* cache results?	*/ 
	NhlCacheClosure		close		/* for freeing cache data*/ 
)
#else
(from,to,convert,args,nargs,cache,close)
	NhlString		from;		/* from type		*/
	NhlString		to;		/* to type		*/
	NhlTypeConverter	convert;	/* the converter function*/ 
	NhlConvertArgList	args;		/* conversion args	*/ 
	int			nargs;		/* number of args	*/ 
	NhlBoolean		cache;		/* cache results?	*/ 
	NhlCacheClosure		close;		/* for freeing cache data*/ 
#endif 
{
	return(_NhlRegisterConverter(NrmStringToName(from),NrmStringToName(to),
					convert, args, nargs, cache, close));
}

/*
 * Function:	FreeConverter
 *
 * Description:	This function takes a pointer to a NhlConvertRec that needs
 *		to be free'd
 *
 * In Args:	NhlConvertPtr	ptr;
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
FreeConverter
#if	__STDC__
(
	NhlConvertPtr	ptr	/* ptr to convert rec	*/
)
#else
(ptr)
	NhlConvertPtr	ptr;	/* ptr to convert rec	*/
#endif
{
	CachePtr	cache=NULL, last=NULL;
	int		i;

	/*
	 * free the cache list
	 */
	if(ptr->cache != NULL){
		cache = ptr->cache;
		while(cache != NULL){
			if(ptr->closure != NULL)
				(ptr->closure)(cache->from,cache->to);
			last = cache;
			cache = cache->next;
			(void)NhlFree(last);
		}
	}

	/*
	 * free the args
	 */
	for(i=0;i < ptr->nargs;i++)
		if(ptr->args[i].size > 0)
			(void)NhlFree((void *)(ptr->args[i].addr));

	(void)NhlFree(ptr->args);

	/*
	 * Free the Converter structure
	 */
	(void)NhlFree(ptr);

	return(NOERROR);
}

/*
 * Function:	_NhlDeleteConverter
 *
 * Description:	This function is the private function used to remove a
 *		converter from the internal converter table.  It uses the
 *		closure function that was provided at registration time to
 *		free any cached data.
 *
 * In Args:	NrmQuark		from	from type
 *		NrmQuark		to	to type
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlDeleteConverter
#if     __STDC__
( 
	NrmQuark		fromQ,		/* from type	*/
	NrmQuark		toQ		/* to type	*/
)
#else
(fromQ,toQ)
	NrmQuark		fromQ;		/* from type	*/
	NrmQuark		toQ;		/* to type	*/
#endif 
{
	NhlConvertPtr	*ptr = NULL, tmp = NULL;

	/*
	 * ptr becomes the record containing the converter to delete
	 * last becomes the node before
	 */
	ptr = &HashTable[HASHFUNC(fromQ,toQ)];

	while((*ptr != NULL) &&
		    (((*ptr)->fromtype != fromQ) || ((*ptr)->totype != toQ)))
		ptr = &((*ptr)->next);

	if(*ptr == NULL){
		NhlPError(FATAL,E_UNKNOWN,
				"Unable to Find Converter %s to %s to remove",
				NrmNameToString(fromQ),NrmNameToString(toQ));
		return(FATAL);
	}

	tmp = *ptr;
	*ptr = (*ptr)->next;

	return(FreeConverter(tmp));
}

/*
 * Function:	NhlDeleteConverter
 *
 * Description:	This function is used to remove a converter from the internal
 *		converter table.  It uses the closure function that was
 *		provided at registration time to free any cached data.
 *
 * In Args:	NhlString		from	from type
 *		NhlString		to	to type
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlDeleteConverter
#if     __STDC__
( 
	NhlString		from,		/* from type	*/
	NhlString		to		/* to type	*/
)
#else
(from,to)
	NhlString		from;		/* from type	*/
	NhlString		to;		/* to type	*/
#endif 
{
	return(_NhlDeleteConverter(NrmStringToName(from),NrmStringToName(to)));
}

/*
 * Function:	_NhlUnRegisterConverter
 *
 * Description:	This function is the private function used to remove a
 *		converter from the internal converter table. Unlike
 *		NhlDeleteConverter, this function doesn't free all the memory
 *		associated with the converter.  It returns a pointer to the
 *		converter structure so the user can re-register the converter
 *		at a latter time if they want to.
 *
 * In Args:	NrmQuark	from		from type
 *		NrmQuark	to		to type
 *
 * Out Args:	NhlConvertPtr	converter	pointer to converter
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlUnRegisterConverter
#if     __STDC__
( 
	NrmQuark	from,		/* from type		*/
	NrmQuark	to,		/* to type		*/
 	NhlConvertPtr	converter	/* pointer to converter	*/
)
#else
(from,to,converter)
	NrmQuark	from;		/* from type		*/
	NrmQuark	to;		/* to type		*/
 	NhlConvertPtr	converter;	/* pointer to converter	*/
#endif 
{
	NhlConvertPtr	*ptr = NULL;

	/*
	 * ptr becomes the record containing the converter to remove
	 */
	ptr = &HashTable[HASHFUNC(from,to)];

	while((*ptr != NULL) &&
		    (((*ptr)->fromtype != from) || ((*ptr)->totype != to)))
		ptr = &((*ptr)->next);

	if(*ptr == NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"Unable to Find Converter %s to %s to unregister",
				NrmNameToString(from),NrmNameToString(to));
		return(FATAL);
	}

	/*
	 * return the converter structure in "converter" ptr
	 */
	*converter = **ptr;

	/*
	 * remove ptr from the list by setting the pointer that points to
	 * it to the structure after it.
	 */
	*ptr = (*ptr)->next;

	return(NOERROR);
}

/*
 * Function:	NhlUnRegisterConverter
 *
 * Description:	This function is used to remove a converter from the internal
 *		converter table. Unlike NhlDeleteConverter, this function
 *		doesn't free all the memory associated with the converter.  It
 *		returns a pointer to the converter structure so the user can
 *		re-register the converter at a latter time if they want to.
 *
 * In Args:	NhlString	from		from type
 *		NhlString	to		to type
 *
 * Out Args:	NhlConvertPtr	converter	pointer to converter
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlUnRegisterConverter
#if     __STDC__
( 
	NhlString	from,		/* from type		*/
	NhlString	to,		/* to type		*/
 	NhlConvertPtr	converter	/* pointer to converter	*/
)
#else
(from,to,converter)
	NhlString	from;		/* from type		*/
	NhlString	to;		/* to type		*/
 	NhlConvertPtr	converter;	/* pointer to converter	*/
#endif 
{
	return(_NhlUnRegisterConverter(NrmStringToName(from),
					NrmStringToName(to),converter));
}

/*
 * Function:	NhlReRegisterConverter
 *
 * Description:	This function is used to re-register a converter that had
 *		previously been un-registered
 *
 * In Args:	NhlConvertPtr	converter	pointer to converter
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlReRegisterConverter
#if     __STDC__
( 
 	NhlConvertPtr	converter	/* pointer to converter	*/
)
#else
(converter)
 	NhlConvertPtr	converter;	/* pointer to converter	*/
#endif 
{
	NhlConvertPtr	*ptr = NULL;

	/*
	 * ptr becomes the record containing the converter to delete
	 * last becomes the node before
	 */
	ptr = &HashTable[HASHFUNC(converter->fromtype,converter->totype)];

	while((*ptr != NULL) &&
		    (((*ptr)->fromtype != converter->fromtype) ||
		     ((*ptr)->totype != converter->totype))		)
		ptr = &((*ptr)->next);

	if(*ptr != NULL){
		converter->next = (*ptr)->next;
		FreeConverter(*ptr);
	}
	else
		converter->next = NULL;

	*ptr = converter;

	return(NOERROR);
}

/*
 * Function:	_NhlConverterExists
 *
 * Description:	This function returns a boolean value indicating the existance
 *		of a converter of the requested type.
 *
 * In Args:	NrmQuark		from		from type
 *		NrmQuark		to		to type
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	True if Converter is present False otherwise
 * Side Effect:	
 */
NhlBoolean
_NhlConverterExists
#if     __STDC__
( 
	NrmQuark		from,		/* from type		*/
	NrmQuark		to		/* to type		*/
)
#else
(from,to)
	NrmQuark		from;		/* from type		*/
	NrmQuark		to;		/* to type		*/
#endif 
{
	NhlConvertPtr	ptr = NULL;

	ptr = HashTable[HASHFUNC(from,to)];

	while((ptr != NULL) &&
		    ((ptr->fromtype != from) || (ptr->totype != to)))
		ptr = ptr->next;

	if(ptr == NULL)
		return(False);
	else
		return(True);
}

/*
 * Function:	NhlConverterExists
 *
 * Description:	This function returns a boolean value indicating the existance
 *		of a converter of the requested type.
 *
 * In Args:	NhlString	from		from type
 *		NhlString	to		to type
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	True if Converter is present False otherwise
 * Side Effect:	
 */
NhlBoolean
NhlConverterExists
#if     __STDC__
( 
	NhlString	from,		/* from type		*/
	NhlString	to		/* to type		*/
)
#else
(from,to)
	NhlString	from;		/* from type		*/
	NhlString	to;		/* to type		*/
#endif 
{
	return(_NhlConverterExists(NrmStringToName(from),NrmStringToName(to)));
}

/*
 * Function:	RetrieveCache
 *
 * Description:	This function returns a pointer to a cache record that
 *		matches the fromdata specified.
 *
 * In Args:	CachePtr	head;	The head of the cache list
 *		NrmValue	*from;	from data to match in cache
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	CachePtr or NULL
 * Side Effect:	
 */
static CachePtr
RetrieveCache
#if	__STDC__
(
	CachePtr		head,	/* The head of the cache list	*/
	NrmValue		*from	/* from data to match in cache	*/
)
#else
(head,from)
	CachePtr		head;	/* The head of the cache list	*/
	NrmValue		*from;	/* from data to match in cache	*/
#endif
{
	CachePtr node = head;

	while(node != NULL){

		if(from->size != node->from.size)
			continue;
		if(bcmp((char *)(from->addr),(char *)(node->from.addr),
							from->size) == 0)
			return(node);
		node = node->next;
	}

	return(NULL);
}

/*
 * Function:	SetConvertVal
 *
 * Description:	This function is used to set the todata with the information
 *		in the fromdata.  If the caller provided space for the
 *		information to be placed they are allowed to do anything
 *		with that information they want to - however, if they
 *		didn't provide space for the information they are given
 *		a READ-ONLY copy of the data - it is not possible to inforce
 *		this so it is only policy - if they do modify or free the
 *		data returned to them, the program will most likely die at
 *		some other point in time.
 *
 * In Args:	NrmValue	from		data to copy
 *
 * Out Args:	NrmValue	*to		place to put data
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
SetConvertVal
#if	__STDC__
(
	NrmValue	from,	/* data to copy		*/
	NrmValue	*to	/* place to put data	*/
)
#else
(from,to)
	NrmValue	from;	/* data to copy		*/
	NrmValue	*to;	/* place to put data	*/
#endif
{
	if((to->size > 0) && (to->addr != NULL)){

		/* caller provided space */

		if(to->size < from.size){ 	/* not large enough */
			to->size = from.size;
			NhlPError(FATAL,E_UNKNOWN,
				"Not enough space provided for converted data");
			return(FATAL);
		}

		/* give caller copy */

		to->size = from.size;
		bcopy((char *)from.addr,(char *)to->addr, to->size);
		return(NOERROR);
	}

	/*
	 * caller didn't provide space - we give them a pointer into
	 * our private data - if they modify or free it, they may die.
	 */

	to->size = from.size;
	to->addr = from.addr;

	return(NOERROR);
}

/*
 * Function:	InsertInCache
 *
 * Description:	This function takes the fromdata and todata after conversion
 *		and places them in the converters cache.
 *
 * In Args:	NhlConvertPtr	conv	ptr to converter
 *		NrmValue	*from	from data
 *		NrmValue	*to	to data
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	conv->cache should end up with another node in it.
 */
static NhlErrorTypes
InsertInCache
#if	__STDC__
(
	NhlConvertPtr	conv,	/* ptr to converter	*/
	NrmValue	*from,	/* from data		*/
	NrmValue	*to	/* to data		*/
)
#else
(conv,from,to)
	NhlConvertPtr	conv;	/* ptr to converter	*/
	NrmValue	*from;	/* from data		*/
	NrmValue	*to;	/* to data		*/
#endif
{
	CachePtr	newnode = NULL;

	newnode = (CachePtr)NhlMalloc(sizeof(CacheRec));


	newnode->from.size = from->size;
	newnode->from.addr = NhlMalloc(from->size);
	(void)bcopy((char*)(from->addr),(char*)(newnode->from.addr),from->size);


	newnode->to.size = to->size;
	newnode->to.addr = NhlMalloc(to->size);
	(void)bcopy((char*)(from->addr),(char*)(newnode->from.addr),from->size);

	newnode->next = conv->cache;
	conv->cache = newnode;

	return(NOERROR);
}

/*
 * Function:	Convert
 *
 * Description:	This function calls a converter on a piece of data and
 *		returns it to the caller in the todata structure.  If
 *		data from the specified converter is supposed to be cached
 *		the data is also copied into the cache.
 *
 * In Args:	NhlConvertPtr	conv		ptr to converter
 *		NrmValue	*fromdata,	data to convert
 *
 * Out Args:
 *		NrmValue	*todata		return converted data
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 *
 * Side Effect:	The cache member of conv may get an additional node added
 *		to it if conv->cachit is True.
 */
static NhlErrorTypes
Convert
#if	__STDC__
(
	NhlConvertPtr	conv,		/* ptr to converter	*/
	NrmValue	*fromdata,	/* data to convert	*/
	NrmValue	*todata		/* return converted data*/
)
#else
(conv,fromdata,todata)
	NhlConvertPtr	conv;		/* ptr to converter	*/
	NrmValue	*fromdata;	/* data to convert	*/
	NrmValue	*todata;	/* return converted data*/
#endif
{
	NhlErrorTypes	ret = NOERROR, lret = NOERROR;

	ret = (*(conv->converter))(fromdata,todata,conv->args,conv->nargs);
	if(ret < WARNING)
		return(ret);

	if(conv->cacheit){
		lret = InsertInCache(conv,fromdata,todata);
		return MIN(lret,ret);
	}
	else
		return(ret);
}

/*
 * Function:	_NhlConvertData
 *
 * Description:	This function is the private interface that calls the requested
 *		converter on the "from" data and places the result in "to"
 *		data.  If the converter was registered with cache = True, then
 *		the "to data" will be copied into a cache.
 *
 * In Args:	NrmQuark	fromtype	from type
 *		NrmQuark	totype		to type
 * 		NrmValue	*fromdata	from type
 *
 * Out Args:
 *		NrmValue	*todata		to type
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlConvertData
#if     __STDC__
( 
	NrmQuark	fromQ,		/* from type		*/
	NrmQuark	toQ,		/* to type		*/
	NrmValue	*fromdata,	/* from type		*/
	NrmValue	*todata		/* to type		*/
)
#else
(fromQ,toQ,fromdata,todata)
	NrmQuark	fromQ;		/* from type		*/
	NrmQuark	toQ;		/* to type		*/
	NrmValue	*fromdata;	/* from type		*/
	NrmValue	*todata;	/* to type		*/
#endif 
{
	NhlConvertPtr	ptr = NULL;
	CachePtr	cache=NULL;

	ptr = HashTable[HASHFUNC(fromQ,toQ)];

	while((ptr != NULL) &&
		    ((ptr->fromtype != fromQ) || (ptr->totype != toQ)))
		ptr = ptr->next;

	if(ptr == NULL){
		NhlPError(WARNING,E_UNKNOWN,"No Converter for %s to %s",
				NrmNameToString(fromQ),NrmNameToString(toQ));
		return(FATAL);
	}

	if(ptr->cacheit)
		if((cache = RetrieveCache(ptr->cache, fromdata)) != NULL)
			return(SetConvertVal(cache->to, todata));

	return(Convert(ptr,fromdata,todata));
}

/*
 * Function:	NhlConvertData
 *
 * Description:	This function calls the requested converter on the "from"
 *		data and places the result in "to" data.  If the converter
 *		was registered with cache = True, then the "to data" will
 *		be copied into a cache.
 *
 * In Args:	NhlString	from		from type
 *		NhlString	to		to type
 * 		NrmValue	*fromdata	from data
 *
 * Out Args:	NrmValue	*todata		to data
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlConvertData
#if     __STDC__
( 
	NhlString	from,		/* from type		*/
	NhlString	to,		/* to type		*/
	NrmValue	*fromdata,	/* from data		*/
	NrmValue	*todata		/* to data		*/
)
#else
(from,to,fromdata,todata)
	NhlString	from;		/* from type		*/
	NhlString	to;		/* to type		*/
	NrmValue	*fromdata;	/* from data		*/
	NrmValue	*todata;	/* to data		*/
#endif 
{
	return(_NhlConvertData(NrmStringToName(from),NrmStringToName(to),
							fromdata,todata));
}
