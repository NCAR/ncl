/*
 *      $Id: nioConvert.h,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Convert.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Sep 11 13:26:02 MDT 1992
 *
 *	Description:	This file contains all the public declarations
 *			neccessary to use type conversion.  It should be
 *			included from hlu.h so everything get's these
 *			declarations.
 */
#ifndef _NCONVERT_H
#define _NCONVERT_H

/* used to set args for converters during registration */
typedef enum _NhlConvertAddrModes{
	NhlIMMEDIATE,	/* values that fit in an NhlArgVal only! */
	NhlADDR,
	NhlSTRENUM,	/* a hack - the size parameter is data	*/
	NhlLAYEROFFSET
} NhlConvertAddrModes;

typedef struct _NhlConvertArg{
	NhlConvertAddrModes	addressmode;
	int			size;
	NhlArgVal		data;
} NhlConvertArg, *NhlConvertArgList;

/* opaque type for un-re registering */
typedef struct _NhlConvertRec NhlConvertRec, *NhlConvertPtr;

/* type for converter functions */

typedef NhlErrorTypes (*NhlTypeConverter)(
#if	NhlNeedProto
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
#endif
);

/* type for closure functions */

typedef void (*NhlCacheClosure)(
#if	NhlNeedProto
	NrmValue	from,
	NrmValue	to
#endif
);

/*
 * Conversion functions
 */
extern NhlErrorTypes NhlRegisterConverter(
#if	NhlNeedProto
	NhlClass		ref_class,
	NhlString		from,
	NhlString		to,
	NhlTypeConverter	convert,
	NhlConvertArgList	args,
	int			nargs,
	NhlBoolean		cache,
	NhlCacheClosure		close
#endif
);

extern NhlErrorTypes NhlDeleteConverter(
#if	NhlNeedProto
	NhlClass	ref_class,
	NhlString	from,
	NhlString	to
#endif
);

extern NhlErrorTypes NhlUnRegisterConverter(
#if	NhlNeedProto
	NhlClass	ref_class,
	NhlString	from,
	NhlString	to,
	NhlConvertPtr*	ptr
#endif
);

extern NhlErrorTypes NhlReRegisterConverter(
#if	NhlNeedProto
	NhlConvertPtr	ptr	/* identifies converter to re-install	*/
#endif
);

extern NhlErrorTypes NhlConvertData(
#if	NhlNeedProto
	int		ref_obj,
	NhlString	from,
	NhlString	to,
	NrmValue*	fptr,
	NrmValue*	tptr
#endif
);

/*
 * These functions are for use inside Converter functions.
 */

extern NhlPointer NhlConvertMalloc(
#if	NhlNeedProto
	unsigned int	size	/* size of memory requested	*/
#endif
);

extern NhlGenArray NhlAllocCreateGenArray(
#if	NhlNeedProto
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	ng_size_t	*len_dimensions	/* number of dimensions	*/
#endif
);

/*
 * This function is used as an indirection of the converter.  If two
 * types are equivalent (but have different names), this is one way to call
 * a previously installed converter to convert the data.
 */
extern NhlErrorTypes NhlReConvertData(
#if	NhlNeedProto
	NhlString		fname,	/* from type			*/
	NhlString		tname,	/* to type			*/
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to	/* ptr to to data		*/
#endif
);

#endif /* _NCONVERT_H */
