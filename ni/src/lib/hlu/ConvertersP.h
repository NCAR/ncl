/*
 *      $Id: ConvertersP.h,v 1.6 1998-05-27 22:50:13 dbrown Exp $
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
	NhlClass	ref_class,
	NhlString	enum_name,
	_NhlEnumVals	*enum_vals,
	int		nvals
#endif
);

extern NhlErrorTypes _NhlRegisterEnumSubtype(
#if	NhlNeedProto
	NhlClass	ref_class,
	NhlString	enum_name,
        NhlString	enum_supertype_name,
	_NhlEnumVals	*enum_vals,
	int		nvals
#endif
);

extern void _NhlConvertersInitialize(
#if	NhlNeedProto
	void
#endif
);

typedef enum _NhlIndxRng {
	_NhlRngMIN,
	_NhlRngMAX,
	_NhlRngMINMAX
} _NhlIndxRng;

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

extern int _NhlCmpString(
#if	NhlNeedProto
	char	*s1,
	char	*s2
#endif
);

extern NhlGenArray _NhlStringToStringGenArray(
#if	NhlNeedProto
	Const char*	s
#endif
);

/*
 * This macro is used because most of the converters end the same way.
 */
#define	_NhlSetVal(type,sz,value)				\
{								\
	if((to->size > 0) && (to->data.ptrval != NULL)){	\
								\
		/* caller provided space */			\
								\
		if(to->size < sz){				\
			/* Not large enough */			\
			to->size = (unsigned int)sz;		\
			return(NhlFATAL);			\
		}						\
								\
		/* give caller copy */				\
								\
		to->size = (unsigned int)sz;			\
		*((type *)(to->data.ptrval)) = value;		\
		return(ret);					\
	}							\
	else{							\
								\
	/* caller didn't provide space - give pointer	*/	\
	/* into static data - if they modify it they	*/	\
	/* may die.					*/	\
								\
		static type val;				\
								\
		to->size = sz;					\
		val = value;					\
		to->data.ptrval = &val;				\
		return(ret);					\
	}							\
}

#endif	/* _CNVTRS_P_H */
