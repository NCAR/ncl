/*
 *      $Id: Convert.c,v 1.11 1995-10-12 21:33:39 boote Exp $
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
#include <string.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/ConvertP.h>


#define _NhlHASHFUNC(a,b)	((((a)*_NhlHASHMULT)+(b)) & _NhlHASHMASK)

static NhlConvertPtr HashTable[_NhlHASHSIZE] = { NULL};
static	_NhlCtxtStack	 ctxt_stack = NULL;

/*
 * Type hierarchy dec's
 */

#define	_NhlTYPEHASH(a)	(a & _NhlHASHMASK)

typedef struct _NhlTrec_ _NhlTrec, *_NhlTptr;
typedef struct _NhlTsubrec_ _NhlTsubrec, *_NhlTsubptr;

struct _NhlTsubrec_ {
	int		num;
	_NhlTptr	sub[NHLCONVALLOCLISTLEN];
	_NhlTsubptr	more;
};

struct _NhlTrec_ {
	NrmQuark	typeQ;
	_NhlTptr	super;
	_NhlTsubrec	sub;
	_NhlTptr	next;
};

static _NhlTptr TypeHashTable[_NhlHASHSIZE] = {NULL};

/*
 * Function:	_NhlCreateConvertContext
 *
 * Description:	This function is used to allocate a Converter context to
 *		be used by each layer during it's initialization/reparenting
 *		so that memory allocated on behalf of converters can be
 *		free'd in an easy way later.
 *
 * In Args:	void
 *
 * Out Args:	void
 *
 * Scope:	Global Private
 * Returns:	NhlConvertContext
 * Side Effect:	
 */
_NhlConvertContext
_NhlCreateConvertContext
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlConvertContext	context = NULL;
	_NhlConvertContextRec	init = {0,{NULL},NULL};

	context = (_NhlConvertContext)NhlMalloc(sizeof(_NhlConvertContextRec));
	if(context == NULL){
		NhlPError(NhlFATAL,ENOMEM,"_NhlCreateConvertContext needs memory");
		return NULL;
	}

	*context = init;

	return context;
}

/*
 * Function:	_NhlFreeConvertContext
 *
 * Description:	This function is used to free a converter context and all
 *		the memory allocated on behalf of the context.
 *
 * In Args:	_NhlConvertContext
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	void
 * Side Effect:	
 */
void
_NhlFreeConvertContext
#if	NhlNeedProto
(
	_NhlConvertContext	context	/* context to free	*/
)
#else
(context)
	_NhlConvertContext	context;/* context to free	*/
#endif
{
	int	i;

	if(context == NULL)
		return;

	if(context->next != NULL)
		_NhlFreeConvertContext(context->next);

	for(i=0;i < context->num_alloced;i++)
		(void)NhlFree(context->alloc_list[i]);
	(void)NhlFree(context);

	return;
}

static _NhlTptr
GetTypeNode
#if	NhlNeedProto
(
	NrmQuark	typeQ
)
#else
(typeQ)
	NrmQuark	typeQ;
#endif
{
	_NhlTptr	ptr = TypeHashTable[_NhlTYPEHASH(typeQ)];
	_NhlTrec	init = {0};

	if(typeQ == NrmNULLQUARK)
		return NULL;

	while(ptr != NULL){
		if(ptr->typeQ == typeQ)
			break;
		ptr = ptr->next;
	}

	/*
	 * If the type isn't currently in the table, add it.
	 */
	if(ptr == NULL){
		ptr = (_NhlTptr)NhlMalloc(sizeof(_NhlTrec));
		if(ptr == NULL) return NULL;
		*ptr = init;
		ptr->typeQ = typeQ;
		ptr->next = TypeHashTable[_NhlTYPEHASH(typeQ)];
		TypeHashTable[_NhlTYPEHASH(typeQ)] = ptr;
	}

	return ptr;
}

static void
ReleaseSubFromSuper
#if	NhlNeedProto
(
	_NhlTptr	sub
)
#else
(sub)
	_NhlTptr	sub;
#endif
{
	_NhlTsubptr	ptr;

	if(sub->super == NULL)
		return;

	ptr = &sub->super->sub;

	while(ptr != NULL){
		int		i;
		NhlBoolean	found = False;

		for(i=0;i < ptr->num;i++){
			if(ptr->sub[i] == sub){
				ptr->sub[i] = ptr->sub[--ptr->num];
				found = True;
				break;
			}
		}

		if(found)
			break;

		ptr = ptr->more;
	}

	sub->super = NULL;

	return;
}

static NhlErrorTypes
AddSubToSuper
#if	NhlNeedProto
(
	_NhlTptr	sub,
	_NhlTptr	super
)
#else
(sub,super)
	_NhlTptr	sub;
	_NhlTptr	super;
#endif
{
	_NhlTptr	tptr = super;
	_NhlTsubptr	ptr;
	_NhlTsubrec	init = {0};

	/*
	 * Insure we are not creating an infinite loop - (registering a type
	 * as a subtype of itself).
	 */
	while(tptr != NULL){
		if(sub == tptr){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Trying to register %s as a sub-type of itself",
						NrmQuarkToString(sub->typeQ));
			return NhlFATAL;
		}
		tptr = tptr->super;
	}

	sub->super = super;
	ptr = &sub->super->sub;

	while(ptr->num >= NHLCONVALLOCLISTLEN){
		if(ptr->more == NULL){
			ptr->more = (_NhlTsubptr)NhlMalloc(sizeof(_NhlTsubrec));
			if(ptr->more == NULL)
				return NhlFATAL;
			*(ptr->more) = init;
		}
		ptr = ptr->more;
	}

	ptr->sub[ptr->num++] = sub;

	return NhlNOERROR;
}

/*
 * Function:	_NhlRegisterTypeQ
 *
 * Description:	this function is used to tell the converter mech. which
 *		types are specializations of other types.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlRegisterTypeQ
#if	NhlNeedProto
(
	NrmQuark	supertypeQ,
	NrmQuark	typeQ
)
#else
(supertypeQ,typeQ)
	NrmQuark	supertypeQ;
	NrmQuark	typeQ;
#endif
{
	_NhlTptr	sub,super;

	if(typeQ == NrmNULLQUARK)
		return NhlFATAL;

	sub = GetTypeNode(typeQ);
	if(sub == NULL)
		return NhlFATAL;

	/*
	 * do nothing... - already registered this way.
	 */
	if((sub->super != NULL) && (sub->super->typeQ == supertypeQ))
		return NhlNOERROR;

	ReleaseSubFromSuper(sub);

	if(supertypeQ == NrmNULLQUARK)
		return NhlNOERROR;

	super = GetTypeNode(supertypeQ);
	if(super == NULL)
		return NhlFATAL;

	return AddSubToSuper(sub,super);
}

/*
 * Function:	_NhlRegisterType
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
NhlErrorTypes
_NhlRegisterType
#if	NhlNeedProto
(
	NhlString	supertype,
	NhlString	type
)
#else
(supertype,type)
	NhlString	supertype;
	NhlString	type;
#endif
{
	return _NhlRegisterTypeQ(NrmStringToQuark(supertype),
							NrmStringToQuark(type));
}

/*
 * Function:	_NhlRegisterTypesQ
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
NhlErrorTypes
_NhlRegisterTypesQ
#if     NhlNeedVarArgProto
( 
	NrmQuark	superQ,
	...		/* subtypeQs - NrmNULLQUARK terminated */
)
#else
(supertype,va_alist)
	NrmQuark	superQ;
	va_dcl		/* subtypes - NULL terminated */
#endif 
{
	va_list		ap;
	char		func[] = "_NhlRegisterTypesQ";
	NrmQuark	nameQ;

	VA_START(ap,superQ);
	for(nameQ = va_arg(ap,NrmQuark); nameQ != NrmNULLQUARK;
						nameQ = va_arg(ap,NrmQuark)){
		if(_NhlRegisterTypeQ(superQ,nameQ) != NhlNOERROR){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:Registering of types Failed",func));
			return NhlFATAL;
		}
	}
	va_end(ap);

	return NhlNOERROR;
}

/*
 * Function:	_NhlRegisterTypes
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
NhlErrorTypes
_NhlRegisterTypes
#if     NhlNeedVarArgProto
( 
	NhlString	supertype,
	...		/* subtypes - NULL terminated */
)
#else
(supertype,va_alist)
	NhlString	supertype;
	va_dcl		/* subtypes - NULL terminated */
#endif 
{
	va_list		ap;
	char		func[] = "_NhlRegisterTypes";
	NhlString	name;
	NrmQuark	superQ = NrmStringToQuark(supertype);

	VA_START(ap,supertype);
	for(name = va_arg(ap,NhlString); name != NULL;
						name = va_arg(ap,NhlString)){
		if(_NhlRegisterTypeQ(superQ,NrmStringToQuark(name)) !=
								NhlNOERROR){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:Registering of types Failed",func));
			return NhlFATAL;
		}
	}
	va_end(ap);

	return NhlNOERROR;
}

/*
 * Function:	_NhlIsSubtypeQ
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
NhlBoolean
_NhlIsSubtypeQ
#if	NhlNeedProto
(
	NrmQuark	superQ,
	NrmQuark	subQ
)
#else
(superQ,subQ)
	NrmQuark	superQ;
	NrmQuark	subQ;
#endif
{
	_NhlTptr	sub,super;

	if((superQ == NrmNULLQUARK) || (subQ == NrmNULLQUARK))
		return False;

	super = GetTypeNode(superQ);
	sub = GetTypeNode(subQ);

	if((sub == super) || (sub->super == super))
		return True;

	if(sub->super == NULL)
		return False;

	return _NhlIsSubtypeQ(superQ,sub->super->typeQ);
}

/*
 * Function:	_NhlIsSubtype
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
NhlBoolean
_NhlIsSubtype
#if	NhlNeedProto
(
	NhlString	super,
	NhlString	sub
)
#else
(super,sub)
	NhlString	super;
	NhlString	sub;
#endif
{
	return _NhlIsSubtypeQ(NrmStringToQuark(super),NrmStringToQuark(sub));
}

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
static NhlConvertArgList
CreateConvArgs
#if	NhlNeedProto
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
	register int		i;
	NhlConvertArgList	newargs=NULL;

	newargs = (NhlConvertArgList)NhlMalloc(nargs * sizeof(NhlConvertArg));
	if(newargs == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to copy convert Args");
		return NULL;
	}

	for(i=0;i < nargs; i++){

		switch(args[i].addressmode){

			case NhlIMMEDIATE:
				newargs[i].data = args[i].data;
				newargs[i].size = args[i].size;

				break;

			case NhlADDR:
				newargs[i].data.ptrval= NhlMalloc(args[i].size);
				memcpy(newargs[i].data.ptrval,
					args[i].data.ptrval,args[i].size);
				newargs[i].size = args[i].size;

				break;

			case NhlSTRENUM:
				/*
				 * This is a hack - Since string to enumerated
				 * types are so frequent it made sense to put
				 * it in.  This addr type basically means
				 * the addr is a null terminated string and
				 * the size is being used for integer data.
				 * The addr should be malloc'ed as in NhlADDR
				 * but it should allocate the size needed by
				 * doing a strlen, and then just set the
				 * value of size to the size part.
				 */
				newargs[i].data.strval = NhlMalloc(
						strlen(args[i].data.strval)+1);

				strcpy(newargs[i].data.strval,
							args[i].data.strval);

				newargs[i].size = args[i].size;

				break;

			default:
				NhlPError(NhlFATAL,NhlEUNKNOWN,
				"addressmode of convert arg[%d] not valid",i);
				(void)NhlFree(newargs);
				return(NULL);
		}

		newargs[i].addressmode = args[i].addressmode;
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
#if	NhlNeedProto
(
	NhlConvertPtr	ptr	/* pointer to converter record	*/
)
#else
(ptr)
	NhlConvertPtr	ptr;	/* pointer to converter record	*/
#endif
{
	int entry;

	entry = _NhlHASHFUNC(ptr->fromtype,ptr->totype);

	if(HashTable[entry] != (NhlConvertPtr)NULL)
		ptr->next = HashTable[entry];

	HashTable[entry] = ptr;

	return(NhlNOERROR);
}

/*
 * Function:	SymConverter
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
/*ARGSUSED*/
static NhlErrorTypes
SymConverter
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,
"SymConverter:This function should never be called! Error in Converters");

	return NhlFATAL;
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
#if	NhlNeedProto
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
		if(ptr->args[i].addressmode == NhlADDR)
			(void)NhlFree(ptr->args[i].data.ptrval);

	(void)NhlFree(ptr->args);

	/*
	 * Free the Converter structure
	 */
	(void)NhlFree(ptr);

	return(NhlNOERROR);
}

/*
 * Function:	_NhlRegSymConv
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
NhlErrorTypes
_NhlRegSymConv
#if	NhlNeedProto
(
	NhlString	fromSym,
	NhlString	toSym,
	NhlString	from,
	NhlString	to
)
#else
(fromSym,toSym,from,to)
	NhlString	fromSym;
	NhlString	toSym;
	NhlString	from;
	NhlString	to;
#endif
{
	return _NhlRegSymConvQ(
			NrmStringToQuark(fromSym),NrmStringToQuark(toSym),
				NrmStringToQuark(from),NrmStringToQuark(to));
}

/*
 * Function:	_NhlRegSymConvQ
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
NhlErrorTypes
_NhlRegSymConvQ
#if	NhlNeedProto
(
	NrmQuark	fromSym,
	NrmQuark	toSym,
	NrmQuark	from,
	NrmQuark	to
)
#else
(fromSym,toSym,from,to)
	NrmQuark	fromSym;
	NrmQuark	toSym;
	NrmQuark	from;
	NrmQuark	to;
#endif
{
	char			*fname = "_NhlRegSymConv";
	NhlConvertArg		cvtargs[2] = {
				{NhlIMMEDIATE,sizeof(NrmQuark),(NhlPointer)0},
				{NhlIMMEDIATE,sizeof(NrmQuark),(NhlPointer)0}
					};
	NhlConvertPtr		cvtrec = NULL;
	NhlConvertPtr		tmp = NULL;

	tmp = HashTable[_NhlHASHFUNC(fromSym,toSym)];

	while((tmp != NULL) &&
	    ((tmp->fromtype != fromSym) || (tmp->totype != toSym)))
		tmp = tmp->next;

	if(tmp != NULL){

		if(tmp->record_type != _NhlReferenceConverter){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Real Cvter exists for %s to %s",fname,
						NrmQuarkToString(fromSym),
						NrmQuarkToString(toSym));
			return NhlFATAL;
		}
		(void)_NhlDeleteConverter(fromSym,toSym);
	}

	cvtrec = (NhlConvertPtr)NhlMalloc(sizeof(NhlConvertRec));

	if(cvtrec == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to allocate memory for Converter %s to %s",
			NrmNameToString(fromSym),NrmNameToString(toSym));
		return NhlFATAL;
	}

	cvtrec->next = NULL;
	cvtrec->record_type = _NhlReferenceConverter;
	cvtrec->fromtype = fromSym;
	cvtrec->totype = toSym;
	cvtrec->converter = SymConverter;
	cvtrec->cacheit = False;
	cvtrec->cache = (CachePtr)NULL;
	cvtrec->closure = (NhlCacheClosure)NULL;

	cvtargs[0].data.lngval = from;
	cvtargs[1].data.lngval = to;

	cvtrec->nargs = 2;
	cvtrec->args = CreateConvArgs(cvtargs,2);
	if(cvtrec->args == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Unable to install Converter %s to %s",
						NrmNameToString(fromSym),
						NrmNameToString(toSym));
		(void)FreeConverter(cvtrec);
		return(NhlFATAL);
	}

	return(insertConverter(cvtrec));
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
static NhlErrorTypes
_NhlRegisterConverter
#if     NhlNeedProto
( 
	NrmQuark		from,		/* from type		*/
	NrmQuark		to,		/* to type		*/
	_NhlCvtRecType		rec_type,	/* type	of converter	*/
	NhlTypeConverter	convert,	/* the converter function*/ 
	NhlConvertArgList	args,		/* conversion args	*/ 
	int			nargs,		/* number of args	*/ 
	NhlBoolean		cache,		/* cache results?	*/ 
	NhlCacheClosure		close		/* for freeing cache data*/ 
)
#else
(from,to,rec_type,convert,args,nargs,cache,close)
	NrmQuark		from;		/* from type		*/
	NrmQuark		to;		/* to type		*/
	_NhlCvtRecType		rec_type;	/* symname type		*/
	NhlTypeConverter	convert;	/* the converter function*/ 
	NhlConvertArgList	args;		/* conversion args	*/ 
	int			nargs;		/* number of args	*/ 
	NhlBoolean		cache;		/* cache results?	*/ 
	NhlCacheClosure		close;		/* for freeing cache data*/ 
#endif 
{
	NhlConvertPtr		cvtrec = NULL;

	if(convert == NULL) return(NhlWARNING);

	cvtrec = (NhlConvertPtr)NhlMalloc(sizeof(NhlConvertRec));
	if(cvtrec == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to allocate memory for Converter %s to %s",
				NrmNameToString(from),NrmNameToString(to));
		return NhlFATAL;
	}

	cvtrec->next = NULL;
	cvtrec->record_type = rec_type;
	cvtrec->fromtype = from;
	cvtrec->totype = to;
	cvtrec->converter = convert;
	cvtrec->cacheit = cache;
	cvtrec->cache = (CachePtr)NULL;
	cvtrec->closure = (cache) ? close : (NhlCacheClosure)NULL;

	cvtrec->nargs = nargs;
	if(nargs > 0){
		cvtrec->args = CreateConvArgs(args,nargs);
		if(cvtrec->args == NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Unable to install Converter %s to %s",
							NrmNameToString(from),
							NrmNameToString(to));
			(void)NhlFree(cvtrec);
			return(NhlWARNING);
		}
	}
	else
		cvtrec->args = NULL;

	/*
	 * If there is a current converter installed - delete it.
	 * ignore return value - we don't care if one was actually removed
	 * or not.
	 */
	(void)_NhlDeleteConverter(from,to);

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
#if     NhlNeedProto
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
		_NhlRegularConverter,convert, args, nargs, cache, close));
}

/*
 * Function:	_NhlExtRegisterConverter
 *
 * Description:	This function is the public interface for registering a
 *		converter function.
 *
 * In Args:
 *	NrmQuark		from,		from type
 *	NrmQuark		to,		to type
 *	NhlExtTypeConverter	convert,	the converter function
 *	NhlConvertArgList	args,		conversion args
 *	int			nargs		number of args
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlExtRegisterConverter
#if     NhlNeedVarArgProto
( 
	NhlString		from,		/* from type		*/
	NhlString		to,		/* to type		*/
	NhlTypeConverter	convert,	/* the converter function*/ 
	NhlConvertArgList	args,		/* conversion args	*/ 
	int			nargs,		/* number of args	*/ 
	NhlBoolean		cache,		/* cache results???	*/
	NhlCacheClosure		close,		/* free cached data	*/
	_NhlCvtRecType		rec_type,	/* symname type		*/
	...
)
#else
(from,to,convert,args,nargs,cache,close,rec_type,va_alist)
	NhlString		from;		/* from type		*/
	NhlString		to;		/* to type		*/
	NhlTypeConverter	convert;	/* the converter function*/ 
	NhlConvertArgList	args;		/* conversion args	*/ 
	int			nargs;		/* number of args	*/ 
	NhlBoolean		cache;		/* cache results???	*/
	NhlCacheClosure		close;		/* free cached data	*/
	_NhlCvtRecType		rec_type;	/* symname type		*/
	va_dcl
#endif 
{
	va_list		ap;
	NhlErrorTypes	ret = NhlNOERROR, lret = NhlNOERROR;
	NrmQuark	fromQ = NrmStringToQuark(from);
	NrmQuark	toQ = NrmStringToQuark(to);
	NhlString	name;
	char		func[] = "_NhlExtRegisterConverter";

	switch (rec_type){

		case _NhlExclusiveConverter:
			ret = _NhlRegisterConverter(fromQ,toQ,rec_type,convert,
							args,nargs,cache,close);

			break;

		case _NhlSymFrom:
		case _NhlSymTo:

			ret = _NhlRegisterConverter(fromQ,toQ,
					_NhlRegularConverter,convert,args,nargs,
								cache,close);

			if(ret != NhlNOERROR){
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		"%s:Error installing base converter:SymNames not done",func));
				return ret;
			}

			VA_START(ap,rec_type);
			for(name = va_arg(ap,NhlString); name != NULL;
						name = va_arg(ap,NhlString)){
				if(rec_type == _NhlSymFrom)
					lret = _NhlRegSymConvQ(
					NrmStringToQuark(name),toQ,fromQ,toQ);
				else
					lret = _NhlRegSymConvQ(fromQ,
					NrmStringToQuark(name),fromQ,toQ);

				if(lret < NhlWARNING){
					NhlPError(lret,NhlEUNKNOWN,
					"%s:Unable to register SymNames",func);
					return lret;
				}

				ret = MIN(ret,lret);
			}
			va_end(ap);
			break;

		default:
			NhlPError(NhlFATAL,ENOSYS,
					"%s:Unsupported Converter type \"%d\"",
								func);
			ret = NhlFATAL;
	}

	return ret;
}

/*
 * Function:	_NhlDeleteConverter
 *
 * Description:	This function is the private function used to remove a
 *		converter from the internal converter table.  It uses the
 *		closure function that was provided at registration time to
 *		free any cached data.
 *
 * In Args:	NrmQuark		from		from type
 *		NrmQuark		to		to type
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlDeleteConverter
#if	NhlNeedProto
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

	ptr = &HashTable[_NhlHASHFUNC(fromQ,toQ)];

	while((*ptr != NULL) &&
	    (((*ptr)->fromtype != fromQ) || ((*ptr)->totype != toQ)))
		ptr = &((*ptr)->next);

	if(*ptr == NULL){
		return(NhlFATAL);
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
#if	NhlNeedProto
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
	return _NhlDeleteConverter(NrmStringToName(from),NrmStringToName(to));
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
#if     NhlNeedProto
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
	ptr = &HashTable[_NhlHASHFUNC(from,to)];

	while((*ptr != NULL) &&
		    (((*ptr)->fromtype != from) || ((*ptr)->totype != to)))
		ptr = &((*ptr)->next);

	if(*ptr == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to Find Converter %s to %s to unregister",
				NrmNameToString(from),NrmNameToString(to));
		return(NhlFATAL);
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

	return(NhlNOERROR);
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
#if     NhlNeedProto
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
#if     NhlNeedProto
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
	ptr = &HashTable[_NhlHASHFUNC(converter->fromtype,converter->totype)];

	while((*ptr != NULL) &&
		    (((*ptr)->fromtype != converter->fromtype) ||
		     ((*ptr)->totype != converter->totype)))
		ptr = &((*ptr)->next);

	if(*ptr != NULL){
		converter->next = (*ptr)->next;
		FreeConverter(*ptr);
	}
	else
		converter->next = NULL;

	*ptr = converter;

	return(NhlNOERROR);
}

static NhlConvertPtr
GetCvtHash
#if	NhlNeedProto
(
	NrmQuark	from,
	NrmQuark	to
)
#else
(from,to)
	NrmQuark	from;
	NrmQuark	to;
#endif
{
	NhlConvertPtr	ptr;

	while(1){
		ptr = HashTable[_NhlHASHFUNC(from,to)];

		while((ptr != NULL) &&
			    ((ptr->fromtype != from) || (ptr->totype != to)))
			ptr = ptr->next;

		if((ptr == NULL)||(ptr->record_type != _NhlReferenceConverter))
			return ptr;

		/*
		 * ptr is actually pointing to a "ref" converter -
		 * need to get the actual one.
		 */
		from = ptr->args[0].data.lngval;
		to = ptr->args[1].data.lngval;
	}
}

static NhlConvertPtr
RecurseFrom
#if	NhlNeedProto
(
	_NhlTptr	fptr,
	_NhlTptr	tptr,
	NrmQuark	from,
	NrmQuark	to
)
#else
(fptr,tptr,from,to)
	_NhlTptr	fptr;
	_NhlTptr	tptr;
	NrmQuark	from;
	NrmQuark	to;
#endif
{
	NhlConvertPtr	ptr;

	/*
	 * terminate recursion at top of from tree.
	 */
	if(fptr == NULL)
		return NULL;
	
	ptr = GetCvtHash(fptr->typeQ,tptr->typeQ);

	/*
	 * if we havn't found it yet, keep going up from tree.
	 */
	if((ptr == NULL) ||
		((ptr->record_type == _NhlExclusiveConverter) &&
			((ptr->fromtype != from) || (ptr->totype != to))))
		return RecurseFrom(fptr->super,tptr,from,to);

	return ptr;
}

static NhlConvertPtr
RecurseGetCvtPtr
#if	NhlNeedProto
(
	_NhlTptr	fptr,
	_NhlTptr	tptr,
	NrmQuark	from,
	NrmQuark	to
)
#else
(fptr,tptr,from,to)
	_NhlTptr	fptr;
	_NhlTptr	tptr;
	NrmQuark	from;
	NrmQuark	to;
#endif
{
	NhlConvertPtr	ptr;
	_NhlTsubptr	sub;
	int		i;
	
	/*
	 * Terminate Recursion down this branch.
	 */
	if((fptr == NULL) || (tptr == NULL))
		return NULL;
		
	/*
	 * Traverse up the "from super-type" tree first since it is
	 * probably shorter.
	 */
	ptr = RecurseFrom(fptr,tptr,from,to);

	if(ptr != NULL)
		return ptr;

	/*
	 * Traverse down the "to sub-type" tree if didn't find a converter up
	 * the "from super-type" tree.
	 */
	sub = &tptr->sub;

	while(sub != NULL){
		for(i=0;i<sub->num;i++){
			ptr = RecurseGetCvtPtr(fptr,sub->sub[i],from,to);
			if(ptr != NULL)
				return ptr;
		}
		sub = sub->more;
	}

	/*
	 * Didn't find anything down this sub-tree
	 */
	return NULL;
}

static NhlConvertPtr
GetCvtPtr
#if	NhlNeedProto
(
	NrmQuark	from,
	NrmQuark	to
)
#else
(from,to)
	NrmQuark	from;
	NrmQuark	to;
#endif
{
	_NhlTptr	fptr = GetTypeNode(from);
	_NhlTptr	tptr = GetTypeNode(to);

	return RecurseGetCvtPtr(fptr,tptr,from,to);
}

/*
 * Function:	ConverterExists
 *
 * Description:	This function returns a boolean value indicating the existance
 *		of a converter of the requested type.
 *
 * In Args:	NrmQuark		from		from type
 *		NrmQuark		to		to type
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	True if Converter is present False otherwise
 * Side Effect:	
 */
static NhlBoolean
ConverterExists
#if     NhlNeedProto
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
	NhlConvertPtr	ptr = GetCvtPtr(from,to);

	if((ptr == NULL) || (ptr->fromtype != from) || (ptr->totype != to))
		return False;
	return True;
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
 * Scope:	static
 * Returns:	True if Converter is present False otherwise
 * Side Effect:	
 */
NhlBoolean
_NhlConverterExists
#if     NhlNeedProto
( 
	NrmQuark	from,		/* from type		*/
	NrmQuark	to		/* to type		*/
)
#else
(from,to)
	NrmQuark	from;		/* from type		*/
	NrmQuark	to;		/* to type		*/
#endif 
{
	return ConverterExists(from,to);
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
#if	NhlNeedProto
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
		if(memcmp((from->data.ptrval),(node->from.data.ptrval),
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
#if	NhlNeedProto
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
	if((to->size > 0) && (to->data.ptrval != NULL)){

		/* caller provided space */

		if(to->size < from.size){ 	/* not large enough */
			to->size = from.size;
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Not enough space provided for converted data");
			return(NhlFATAL);
		}

		/* give caller copy */

		to->size = from.size;
		memcpy(to->data.ptrval,from.data.ptrval,to->size);
		return(NhlNOERROR);
	}

	/*
	 * caller didn't provide space - we give them a pointer into
	 * our private data - if they modify or free it, they may die.
	 */

	to->size = from.size;
	to->data = from.data;

	return(NhlNOERROR);
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
#if	NhlNeedProto
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
	newnode->from.data.ptrval = NhlMalloc(from->size);
	memcpy(newnode->from.data.ptrval,from->data.ptrval,from->size);


	newnode->to.size = to->size;
	newnode->to.data.ptrval = NhlMalloc(to->size);
	memcpy(newnode->from.data.ptrval,from->data.ptrval,from->size);

	newnode->next = conv->cache;
	conv->cache = newnode;

	return(NhlNOERROR);
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
#if	NhlNeedProto
(
	NhlConvertPtr		conv,		/* ptr to converter	*/
	NrmValue		*fromdata,	/* data to convert	*/
	NrmValue		*todata		/* return converted data*/
)
#else
(conv,fromdata,todata)
	NhlConvertPtr		conv;		/* ptr to converter	*/
	NrmValue		*fromdata;	/* data to convert	*/
	NrmValue		*todata;	/* return converted data*/
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, lret = NhlNOERROR;

	ret = (*conv->converter)(fromdata,todata,conv->args,conv->nargs);

	if(ret < NhlWARNING)
		return(ret);

	if(conv->cacheit){
		lret = InsertInCache(conv,fromdata,todata);
		return MIN(lret,ret);
	}
	else
		return(ret);
}

/*
 * Function:	ConvertData
 *
 * Description:	
 *
 * In Args:
 *		NrmQuark		fromtype	from type
 *		NrmQuark		totype		to type
 * 		NrmValue		*fromdata	from type
 *
 * Out Args:
 *		NrmValue	*todata		to type
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
ConvertData
#if     NhlNeedProto
( 
	_NhlConvertContext	context,	/* context		*/
	NrmQuark		fromQ,		/* from type		*/
	NrmQuark		toQ,		/* to type		*/
	NrmValue		*fromdata,	/* from type		*/
	NrmValue		*todata		/* to type		*/
)
#else
(context,fromQ,toQ,fromdata,todata)
	_NhlConvertContext	context;	/* context		*/
	NrmQuark		fromQ;		/* from type		*/
	NrmQuark		toQ;		/* to type		*/
	NrmValue		*fromdata;	/* from type		*/
	NrmValue		*todata;	/* to type		*/
#endif 
{
	NhlConvertPtr		ptr = NULL;
	CachePtr		cache=NULL;
	_NhlCtxtStackRec	ctxt;
	NhlErrorTypes		ret=NhlNOERROR;
	NrmQuark		from,to;

	if(context == NULL){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"ConvertData:Invalid Convert Context value"));
		return NhlFATAL;
	}

	ctxt.context = context;
	ctxt.next = ctxt_stack;
	ctxt_stack = &ctxt;

	fromdata->typeQ = from = fromQ;
	todata->typeQ = to = toQ;

	ptr = GetCvtPtr(from,to);

	if(ptr == NULL){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"No Converter for %s to %s",
				NrmNameToString(from),NrmNameToString(to));
		return(NhlFATAL);
	}

	if(ptr->cacheit)
		if((cache = RetrieveCache(ptr->cache, fromdata)) != NULL)
			return(SetConvertVal(cache->to, todata));

	ret = Convert(ptr,fromdata,todata);

	ctxt_stack = ctxt.next;

	return ret;
}

/*
 * Function:	_NhlConvertData
 *
 * Description:	This function is the private interface that calls the requested
 *		converter on the "from" data and places the result in "to"
 *		data.  If the converter was registered with cache = True, then
 *		the "to data" will be copied into a cache.
 *
 * In Args:	_NhlConvertContext	context		context
 *		NrmQuark		fromtype	from type
 *		NrmQuark		totype		to type
 * 		NrmValue		*fromdata	from type
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
#if     NhlNeedProto
( 
	_NhlConvertContext	context,	/* context		*/
	NrmQuark		fromQ,		/* from type		*/
	NrmQuark		toQ,		/* to type		*/
	NrmValue		*fromdata,	/* from type		*/
	NrmValue		*todata		/* to type		*/
)
#else
(context,fromQ,toQ,fromdata,todata)
	_NhlConvertContext	context;	/* context		*/
	NrmQuark		fromQ;		/* from type		*/
	NrmQuark		toQ;		/* to type		*/
	NrmValue		*fromdata;	/* from type		*/
	NrmValue		*todata;	/* to type		*/
#endif 
{
	return ConvertData(context,fromQ,toQ,fromdata,todata);
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
#if     NhlNeedProto
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
	static _NhlCtxtStack	free_list = NULL;
	_NhlCtxtStack		tptr = NULL;
	_NhlConvertContext	context = _NhlCreateConvertContext();
	NhlErrorTypes		ret;

	/*
	 * Clean up memory that is no longer in use.
	 */
	while(free_list != NULL){
		_NhlFreeConvertContext(free_list->context);
		tptr = free_list;
		free_list = free_list->next;
		(void)NhlFree(tptr);
	}

	if(context == NULL){
		NhlPError(NhlFATAL,ENOMEM,"Unable to Create Context");
		return NhlFATAL;
	}

	tptr = (_NhlCtxtStack)NhlMalloc(sizeof(_NhlCtxtStackRec));
	if(tptr == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	tptr->context = context;

	ret = _NhlConvertData(context,NrmStringToName(from),
					NrmStringToName(to),fromdata,todata);

	tptr->next = free_list;
	free_list = tptr;

	return ret;
}

/*
 * Function:	_NhlReConvertData
 *
 * Description:	This function should be used by converters that need to
 *		call a converter.  This allows the lower level converter
 *		to use the same convert context.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public (only used in "Converter" functions)
 * Returns:	NhlPointer
 * Side Effect:	
 */
NhlErrorTypes
_NhlReConvertData
#if	NhlNeedProto
(
	NrmQuark		fname,	/* from type			*/
	NrmQuark		tname,	/* to type			*/
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to	/* ptr to to data		*/
)
#else
(fname,tname,from,to,args,nargs)
	NrmQuark		fname;	/* from type			*/
	NrmQuark		tname;	/* to type			*/
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
#endif
{
	if((ctxt_stack == NULL) || (ctxt_stack->context == NULL)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"_NhlReConvertData:Context not active");
		return NhlFATAL;
	}

	return ConvertData(ctxt_stack->context,fname,tname,from,to);
}

/*
 * Function:	NhlReConvertData
 *
 * Description:	This function should be used by converters that need to
 *		call a converter.  This allows the lower level converter
 *		to use the same convert context.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public (only used in "Converter" functions)
 * Returns:	NhlPointer
 * Side Effect:	
 */
NhlErrorTypes
NhlReConvertData
#if	NhlNeedProto
(
	NhlString		fname,	/* from type			*/
	NhlString		tname,	/* to type			*/
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to	/* ptr to to data		*/
)
#else
(fname,tname,from,to,args,nargs)
	NhlString		fname;	/* from type			*/
	NhlString		tname;	/* to type			*/
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
#endif
{
	return _NhlReConvertData(NrmStringToQuark(fname),
					NrmStringToQuark(tname),from,to);
}

/*
 * Function:	_NhlCvtCtxtMalloc
 *
 * Description:	This is a malloc function that actually takes the Context
 *		as an arguement.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private global
 * Returns:	NhlPointer
 * Side Effect:	
 */
NhlPointer
_NhlCvtCtxtMalloc
#if	NhlNeedProto
(
	unsigned int		size,
	_NhlConvertContext	ctxt
)
#else
(size,ctxt)
	unsigned int		size;
	_NhlConvertContext	ctxt;
#endif
{
	NhlPointer	ptr;

	while(ctxt->num_alloced >= NHLCONVALLOCLISTLEN){
		if(ctxt->next == NULL){
			ctxt->next = _NhlCreateConvertContext();
			if(ctxt->next == NULL){
				NhlPError(NhlFATAL, NhlEUNKNOWN,
				"NhlConvertMalloc:Unable to Grow Context");
				return NULL;
			}
		}
		ctxt = ctxt->next;
	}

	ptr = NhlMalloc(size);

	if(ptr != NULL)
		ctxt->alloc_list[ctxt->num_alloced++] = ptr;

	return ptr;
}

/*
 * Function:	NhlConvertMalloc
 *
 * Description:	This function should be used by converters that need to
 *		dynamically allocate the memory to return the converted
 *		data.  It should not be used for memory that will be free'd
 *		inside the converter function.  This memory is automatically
 *		free'd the next time the Convert utility is used.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public (only used in "Converter" functions)
 * Returns:	NhlPointer
 * Side Effect:	
 */
NhlPointer
NhlConvertMalloc
#if	NhlNeedProto
(
	unsigned int	size	/* size of memory requested	*/
)
#else
(size)
	unsigned int	size;	/* size of memory requested	*/
#endif
{
	_NhlConvertContext	context;
	
	if((ctxt_stack == NULL) || (ctxt_stack->context == NULL)){
		NhlPError(NhlFATAL, NhlEUNKNOWN,
					"NhlConvertMalloc:Context not active");
		return NULL;
	}

	context = ctxt_stack->context;

	return _NhlCvtCtxtMalloc(size,context);
}


NhlGenArray
_NhlConvertCreateGenArray
#if	NhlNeedProto
(
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	int		*len_dimensions	/* number of dimensions	*/
)
#else
(data,type,size,num_dimensions,len_dimensions)
	NhlPointer	data;		/* data array		*/
	NhlString	type;		/* type of each element	*/
	unsigned int	size;		/* size of each element	*/
	int		num_dimensions;	/* number of dimensions	*/
	int		*len_dimensions;/* number of dimensions	*/
#endif
{
	return _NhlAllocCreateGenArray(data,type,size,num_dimensions,
				len_dimensions,False,NhlConvertMalloc);
}

/*
 * Function:	_NhlConvertCopyGenArray
 *
 * Description:	This function copies an NhlGenArray and allocates an
 *		NhlGenArray. It copies the "data" part of the GenArray
 *		if copy_data is true - otherwise the new GenArray just
 *		references the same data pointer.
 *
 * In Args:	
 *		NhlGenArray	gen		generic array pointer
 *		NhlBoolean	copy_data	copy data?
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlGenArray
 * Side Effect:	
 */
NhlGenArray
_NhlConvertCopyGenArray
#if	NhlNeedProto
(
	NhlGenArray	gen		/* generic array pointer	*/
)
#else
(gen)
	NhlGenArray	gen;		/* generic array pointer	*/
#endif
{
	return _NhlConvertCreateGenArray(gen->data,NrmQuarkToString(gen->typeQ),
		gen->size,gen->num_dimensions,gen->len_dimensions);
}
