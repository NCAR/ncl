/*
 *      $Id: ConvertersP.h,v 1.3 1995-03-13 21:47:23 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ConvertersP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 4 01:37:44 MDT 1994
 *
 *	Description:	
 */

#ifndef	_CNVTRS_P_H
#define	_CNVTRS_P_H

#include <ncarg/hlu/Convert.h>

#define	_NhlSTACK_ARGS_SIZE	(100)

typedef struct _NhlEnumVals_ _NhlEnumVals;

struct _NhlEnumVals_ {
	int		value;
	NhlString	name;
};

extern NhlErrorTypes _NhlRegisterEnumType(
#if	NhlNeedProto
	NhlString	enum_name,
	_NhlEnumVals	*enum_vals,
	int		nvals
#endif
);

extern void _NhlConvertersInitialize(
#if	NhlNeedProto
	void
#endif
);

extern NhlErrorTypes
_NhlCvtScalarToIndex(
#if	NhlNeedProto
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
#endif
);

extern NhlErrorTypes
_NhlCvtGenArrayToIndexGenArray(
#if	NhlNeedProto
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
#endif
);

#endif	/* _CNVTRS_P_H */
