/*
 *      $Id: nioConvertP.h,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ConvertP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Sep 11 13:39:35 MDT 1992
 *
 *	Description:	This file contains all the declarations needed
 *			internally for the Converters.  Most of this
 *			could probably be put directly in Convert.c
 *			but I decided it may be usefull in other
 *			places later.
 */
#ifndef _NCONVERTP_H
#define _NCONVERTP_H

#include "niohluP.h"
#include "nioNresDB.h"
#include "nioConvert.h"

#define	NHLCONVALLOCLISTLEN	10
/*
 * Declaration for Converter Context - used to make a list of mallocs
 * done in the called converter.
 */
typedef struct _NhlConvertContext _NhlConvertContextRec, *_NhlConvertContext;

struct _NhlConvertContext {
	NhlLayer		ref;
	NhlClass		ref_class;
	int			num_alloced;
	NhlPointer		alloc_list[NHLCONVALLOCLISTLEN];
	_NhlConvertContext	next;
};

typedef struct _NhlCtxtStack _NhlCtxtStackRec, *_NhlCtxtStack;

struct _NhlCtxtStack{
	_NhlConvertContext	context;
	_NhlCtxtStack		next;
};

extern NhlConvertPtr	_NhlDefHashTable[];

extern _NhlConvertContext _NhlCreateConvertContext(
#if	NhlNeedProto
	NhlLayer	ref
#endif
);

extern void _NhlConvertContextClass(
#if	NhlNeedProto
	_NhlConvertContext	ctxt,
	NhlClass		ref_class
#endif
);

extern void	_NhlFreeConvertContext(
#if	NhlNeedProto
	_NhlConvertContext	context
#endif
);

extern NhlPointer _NhlCvtCtxtMalloc(
#if	NhlNeedProto
	unsigned int		size,
	_NhlConvertContext	context
#endif
);

extern NhlGenArray
_NhlConvertCreateGenArray(
#if	NhlNeedProto
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	ng_size_t	*len_dimensions	/* number of dimensions	*/
#endif
);

extern NhlGenArray
_NhlConvertCopyGenArray(
#if	NhlNeedProto
	NhlGenArray	gen		/* generic array pointer	*/
#endif
);

/*
 * declarations for the conversion cache
 */

typedef struct _CacheRec CacheRec, *CachePtr;

struct _CacheRec {
	NrmValue	from;
	NrmValue	to;
	CachePtr	next;
};

/*
 * declarations for the converter entries - actual typedef is in public file.
 */

typedef enum _NhlCvtRecType_ {
	_NhlRegularConverter,
	_NhlExclusiveConverter,
	_NhlSymFrom,
	_NhlSymTo,
	_NhlReferenceConverter
} _NhlCvtRecType;

struct _NhlConvertRec{
	NhlClass		ref_class;
	NhlConvertPtr		next;
	_NhlCvtRecType		record_type;
	NrmQuark		fromtype;
	NrmQuark		totype;
	NhlTypeConverter	converter;
	NhlBoolean		computed_args;
	NhlConvertArgList	args;
	int			nargs;
	NhlBoolean		cacheit;
	CachePtr		cache;
	NhlCacheClosure		closure;
};

extern NhlErrorTypes _NhlExtRegisterConverter(
#if	NhlNeedVarArgProto
	NhlClass		ref_class,
	NhlString		from,		/* from type		*/
	NhlString		to,		/* to type		*/
	NhlTypeConverter	convert,	/* the converter function*/ 
	NhlConvertArgList	args,		/* conversion args	*/ 
	int			nargs,		/* number of args	*/ 
	NhlBoolean		cache,		/* cache results???	*/
	NhlCacheClosure		close,		/* free cached data	*/
	_NhlCvtRecType		sym_type,	/* symname type		*/
	...
#endif
);

extern NhlErrorTypes _NhlRegSymConv(
#if	NhlNeedProto
	NhlClass	ref_class,
	NhlString	fromSym,
	NhlString	toSym,
	NhlString	from,
	NhlString	to
#endif
);

NhlErrorTypes _NhlRegSymConvQ(
#if	NhlNeedProto
	NhlClass	ref_class,
	NrmQuark	fromSym,
	NrmQuark	toSym,
	NrmQuark	from,
	NrmQuark	to
#endif
);

extern NhlErrorTypes _NhlDeleteConverter(
#if	NhlNeedProto
	NhlClass		ref_class,
	NrmQuark		fromQ,		/* from type	*/
	NrmQuark		toQ		/* to type	*/
#endif
);

extern NhlErrorTypes _NhlUnRegisterConverter(
#if	NhlNeedProto
	NhlClass	ref_class,
	NrmQuark	from,		/* from type		*/
	NrmQuark	to,		/* to type		*/
 	NhlConvertPtr	*converter	/* pointer to converter	*/
#endif
);

extern NhlBoolean _NhlConverterExists(
#if	NhlNeedProto
	NhlClass		ref_class,
	NrmQuark		from,		/* from type	*/
	NrmQuark		to		/* to type	*/
#endif
);

extern NhlErrorTypes _NhlReConvertData(
#if	NhlNeedProto
	NrmQuark		fromQ,		/* from type		*/
	NrmQuark		toQ,		/* to type		*/
	NrmValue		*fromdata,	/* from type		*/
	NrmValue		*todata		/* to type		*/
#endif
);

extern NhlErrorTypes _NhlConvertData(
#if	NhlNeedProto
	_NhlConvertContext	context,	/* context		*/
	NrmQuark		fromQ,		/* from type		*/
	NrmQuark		toQ,		/* to type		*/
	NrmValue		*fromdata,	/* from type		*/
	NrmValue		*todata		/* to type		*/
#endif
);

extern NhlErrorTypes _NhlConverterGetArgs(
#if	NhlNeedProto
	NhlClass		ref_class,
	NrmQuark		fromQ,
	NrmQuark		toQ,
	NhlConvertArgList	*args,
	int			*nargs
#endif
);

extern NhlErrorTypes _NhlRegisterType(
#if	NhlNeedProto
	NhlString	supertype,
	NhlString	type
#endif
);

extern NhlErrorTypes _NhlRegisterTypeQ(
#if	NhlNeedProto
	NrmQuark	supertypeQ,
	NrmQuark	typeQ
#endif
);

/*VARARGS1*/
extern NhlErrorTypes _NhlRegisterTypes(
#if	NhlNeedVarArgProto
	NhlString	supertype,
	...		/* subtypes - NULL terminated */
#endif
);

/*VARARGS1*/
extern NhlErrorTypes _NhlRegisterTypesQ(
#if	NhlNeedVarArgProto
	NrmQuark	supertypeQ,
	...		/* subtypesQ - NrmNULLQUARK terminated */
#endif
);

extern NhlBoolean _NhlIsSubtypeQ(
#if	NhlNeedProto
	NrmQuark	superQ,
	NrmQuark	subQ
#endif
);

extern NhlBoolean _NhlIsSubtype(
#if	NhlNeedProto
	NhlString	super,
	NhlString	sub
#endif
);

#endif /* _NCONVERTP_H */
