/*
 *      $Id: FortranP.h,v 1.5 1995-04-07 10:41:49 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Fortran.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Mar 25 10:49:45 MST 1994
 *
 *	Description:	
 */
#ifndef	_NFortranP_H
#define	_NFortranP_H

#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#ifdef	UNICOS
#include <fortran.h>
typedef _fcd _NhlFString;
#define	_NhlFptrToCptr(fptr)	(_fcdtocp(fptr))
#define	_NhlCptrToFptr(cptr)	((cptr)?_cptofcd(cptr,strlen(cptr)):_cptofcd("",0))
#else
typedef NhlString _NhlFString;
#define	_NhlFptrToCptr(fptr)		(fptr)
#define	_NhlCptrToFptr(cptr)		(cptr)
#endif




/*
 * Fortran interface "types".
 */

#define	_NhlTFExpString		"_FExport_String_"
#define	_NhlTFExpStringArr	"_FExport_String_Arr"
#define	_NhlTFExpArray		"_FExport_Array_"

typedef NhlClass (*_NhlClassFunc)(
#if	NhlNeedProto
	void
#endif
);

typedef	struct _NhlFExportStringRec_ _NhlFExportStringRec, *_NhlFExportString;
struct _NhlFExportStringRec_ {
	_NhlFString	fstring;
	unsigned int	strlen;
	int		*arr_len;
};

typedef struct _NhlFExportArrayRec_ _NhlFExportArrayRec, *_NhlFExportArray;
struct _NhlFExportArrayRec_ {
	NhlPointer	data;
	NrmQuark	typeQ;
	unsigned int	size;
	int		*num_dim;	/* if num_dim = NULL - one dim req. */
	int		*len_dim;
};


extern NhlString _NhlFstrToCstr(
#if	NhlNeedProto
	NhlString		cstr,
	unsigned int		cstr_len,
	Const _NhlFString	fstr,
	unsigned int		fstr_len
#endif
);

extern NhlString* _NhlMDFstrToCstrtbl(
#if	NhlNeedProto
	Const _NhlFString	fstr,
	unsigned int		num_strings,
	unsigned int		len_strings
#endif
);

extern NrmQuark _NhlFstrToQuark(
#if	NhlNeedProto
	Const _NhlFString	fstr,
	unsigned int		fstr_len
#endif
);

/*
 * the Fptr func is different from the Fstr function in that we already
 * have a valid "C" pointer to the Fortran memory.  UNICOS annoyance
 */
extern NhlErrorTypes _NhlCstrToFptr(
#if	NhlNeedProto
	NhlString	fptr,
	unsigned int	fstr_len,
	Const char	*cstr
#endif
);

extern NhlErrorTypes _NhlCstrToFstr(
#if	NhlNeedProto
	_NhlFString	fstr,
	unsigned int	fstr_len,
	Const char	*cstr
#endif
);

extern NhlGenArray _NhlCreateFGenArray(
#if	NhlNeedProto
	NhlPointer	data,
	NhlString	type,
	unsigned int	size,
	int		num_dimensions,
	int		*len_dimensions,
	NhlBoolean	copy_data
#endif
);

#endif	/* _NFortranP_H */
