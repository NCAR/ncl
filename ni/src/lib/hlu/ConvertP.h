/*
 *      $Id: ConvertP.h,v 1.4 1994-05-12 23:50:41 boote Exp $
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

#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Convert.h>

#define	NHLCONVALLOCLISTLEN	10
/*
 * Declaration for Converter Context - used to make a list of mallocs
 * done in the called converter.
 */
typedef struct _NhlConvertContext _NhlConvertContextRec, *_NhlConvertContext;

struct _NhlConvertContext {
	int			num_alloced;
	NhlPointer		alloc_list[NHLCONVALLOCLISTLEN];
	_NhlConvertContext	next;
};

typedef struct _NhlCtxtStack _NhlCtxtStackRec, *_NhlCtxtStack;

struct _NhlCtxtStack{
	_NhlConvertContext	context;
	_NhlCtxtStack		next;
};

extern _NhlConvertContext _NhlCreateConvertContext(
#if	NhlNeedProto
	void
#endif
);

extern void	_NhlFreeConvertContext(
#if	NhlNeedProto
	_NhlConvertContext	context
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
	_NhlRealConverter,
	_NhlReferenceConverter
} _NhlCvtRecType;

struct _NhlConvertRec{
	NhlConvertPtr		next;
	_NhlCvtRecType		record_type;
	NrmQuark		fromtype;
	NrmQuark		totype;
	NhlTypeConverter	converter;
	NhlConvertArgList	args;
	int			nargs;
	NhlBoolean		cacheit;
	CachePtr		cache;
	NhlCacheClosure		closure;
};

typedef enum _NhlCvtSymNames_ {
	_NhlSYM_NONE,
	_NhlSYM_FROM,
	_NhlSYM_TO
} _NhlCvtSymNames;

extern NhlErrorTypes _NhlExtRegisterConverter(
#ifdef	NhlNeedVarArgProto
	NhlString		from,		/* from type		*/
	NhlString		to,		/* to type		*/
	NhlTypeConverter	convert,	/* the converter function*/ 
	NhlConvertArgList	args,		/* conversion args	*/ 
	int			nargs,		/* number of args	*/ 
	NhlBoolean		cache,		/* cache results???	*/
	NhlCacheClosure		close,		/* free cached data	*/
	_NhlCvtSymNames		sym_type,	/* symname type		*/
	...
#endif
);

extern NhlErrorTypes _NhlDeleteConverter(
#if	NhlNeedProto
	NrmQuark		fromQ,		/* from type	*/
	NrmQuark		toQ		/* to type	*/
#endif
);

extern NhlErrorTypes _NhlUnRegisterConverter(
#if	NhlNeedProto
	NrmQuark	from,		/* from type		*/
	NrmQuark	to,		/* to type		*/
 	NhlConvertPtr	converter	/* pointer to converter	*/
#endif
);

extern NhlBoolean _NhlConverterExists(
#if	NhlNeedProto
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

#endif /* _NCONVERTP_H */
