/*
 *      $Id: ConvertP.h,v 1.3 1994-02-18 02:53:49 boote Exp $
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

struct _NhlConvertRec{
	NhlConvertPtr		next;
	NrmQuark		fromtype;
	NrmQuark		totype;
	NrmQuark		converter_type;
	NhlTypeConverter	converter;
	NhlConvertArgList	args;
	int			nargs;
	NhlBoolean		cacheit;
	CachePtr		cache;
	NhlCacheClosure		closure;
};

extern NhlErrorTypes _NhlExtRegisterConverter(
#if	NhlNeedProto
	NhlString		from,		/* from type		*/
	NhlString		to,		/* to type		*/
	NhlString		converter_type,	/* type	of conversion	*/
	NhlTypeConverter	convert,	/* the converter function*/ 
	NhlConvertArgList	args,		/* conversion args	*/ 
	int			nargs		/* number of args	*/ 
#endif
);

extern NhlErrorTypes _NhlDeleteConverter(
#if	NhlNeedProto
	NrmQuark		fromQ,		/* from type	*/
	NrmQuark		toQ,		/* to type	*/
	NrmQuark		conv_typeQ	/* conv type	*/
#endif
);

extern NhlErrorTypes _NhlUnRegisterConverter(
#if	NhlNeedProto
	NrmQuark	from,		/* from type		*/
	NrmQuark	to,		/* to type		*/
	NrmQuark	conv_type,	/* conv type		*/
 	NhlConvertPtr	converter	/* pointer to converter	*/
#endif
);

extern NhlBoolean _NhlConverterExists(
#if	NhlNeedProto
	NrmQuark		from,		/* from type	*/
	NrmQuark		to		/* to type	*/
#endif
);

extern NhlBoolean _NhlExtConverterExists(
#if	NhlNeedProto
	NrmQuark		from,		/* from type	*/
	NrmQuark		to,		/* to type	*/
	NrmQuark		conv_type	/* conv type	*/
#endif
);

extern NhlErrorTypes _NhlExtConvertData(
#if	NhlNeedProto
	_NhlConvertContext	context,	/* context		*/
	NrmQuark		typeQ,		/* conv type		*/
	NrmQuark		fromQ,		/* from type		*/
	NrmQuark		toQ,		/* to type		*/
	NrmValue		*fromdata,	/* from type		*/
	NrmValue		*todata		/* to type		*/
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
