/*
 *      $Id: Convert.h,v 1.2 1993-10-19 17:49:55 boote Exp $
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
	NHLIMMEDIATE,	/* values that fit in an NhlPointer only! */
	NHLADDR,
	NHLSTRENUM	/* a hack - the size parameter is data	*/
} NhlConvertAddrModes;

typedef struct _NhlConvertArg{
	NhlConvertAddrModes	addressmode;
	int			size;
	NhlPointer		addr;
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

NhlErrorTypes NhlRegisterConverter(
#if	NhlNeedProto
	NhlString,		/* from type - usually a NHLT*** constant*/
	NhlString,		/* to type - usually a NHLT*** constant	*/
	NhlTypeConverter,	/* the converter function		*/
	NhlConvertArgList,	/* list of conversion args		*/
	int,			/* number of args			*/
	NhlBoolean,		/* cache results?			*/
	NhlCacheClosure		/* function for freeing cache data	*/
#endif
);

NhlErrorTypes NhlDeleteConverter(
#if	NhlNeedProto
	NhlString,		/* from type - usually a NHLT*** constant*/
	NhlString		/* to type - usually a NHLT*** constant	*/
#endif
);

NhlErrorTypes NhlUnRegisterConverter(
#if	NhlNeedProto
	NhlString,	/* from type - usually a NHLT*** constant*/
	NhlString,	/* to type - usually a NHLT*** constant	*/
	NhlConvertPtr	/* returns pointer to opaque type	*/
#endif
);

NhlErrorTypes NhlReRegisterConverter(
#if	NhlNeedProto
	NhlConvertPtr	/* identifies converter to re-install	*/
#endif
);

NhlBoolean NhlConverterExists(
#if	NhlNeedProto
	NhlString,		/* from type - usually a NHLT*** constant*/
	NhlString		/* to type - usually a NHLT*** constant	*/
#endif
);

NhlErrorTypes NhlConvertData(
#if	NhlNeedProto
	NhlString,		/* from type - usually a NHLT*** constant*/
	NhlString,		/* to type - usually a NHLT*** constant	*/
	NrmValue*,	/* from data				*/
	NrmValue*	/* to data				*/
#endif
);

/*
 * These functions are for use inside Converter functions.
 */

NhlPointer NhlConvertMalloc(
#ifdef	NhlNeedProto
	unsigned int	size	/* size of memory requested	*/
#endif
);

#endif /* _NCONVERT_H */
