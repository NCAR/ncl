/*
 *      $Id: ConvertP.h,v 1.1 1993-04-30 17:21:31 boote Exp $
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
	NhlTypeConverter	converter;
	NrmValue		*args;
	int			nargs;
	NhlBoolean		cacheit;
	CachePtr		cache;
	NhlCacheClosure		closure;
};

extern NhlErrorTypes _NhlRegisterConverter(
#if	NhlNeedProto
	NrmQuark		from,		/* from type		*/
	NrmQuark		to,		/* to type		*/
	NhlTypeConverter	convert,	/* the converter function*/ 
	NhlConvertArgList	args,		/* conversion args	*/ 
	int			nargs,		/* number of args	*/ 
	NhlBoolean		cache,		/* cache results?	*/ 
	NhlCacheClosure		close		/* for freeing cache data*/ 
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

extern NhlErrorTypes _NhlConvertData(
#if	NhlNeedProto
	NrmQuark	fromQ,		/* from type		*/
	NrmQuark	toQ,		/* to type		*/
	NrmValue	*fromdata,	/* from type		*/
	NrmValue	*todata		/* to type		*/
#endif
);

#endif /* _NCONVERTP_H */
