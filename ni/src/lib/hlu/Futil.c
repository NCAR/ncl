/*
 *      $Id: Futil.c,v 1.3.22.1 2008-03-28 20:37:35 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Futil.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 28 16:57:31 MST 1994
 *
 *	Description:	This file contains misc utility functions used
 *			in the Fortran->C interface.
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/FortranP.h>


/*
 * Function:	FptrToCstr
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
static NhlString
FptrToCstr
#if	NhlNeedProto
(
	NhlString		cstr,
	unsigned int		cstr_len,
	NhlString		fptr,
	unsigned int		fstr_len
)
#else
(cstr,cstr_len,fptr,fstr_len)
	NhlString		cstr;
	unsigned int		cstr_len;
	NhlString		fptr;
	unsigned int		fstr_len;
#endif
{
	char	*cptr = NULL;
	int	len = fstr_len;

	while((len > 0) && (fptr[len-1] == ' ')) len--;

	if(len == 0)
		return NULL;

	if(cstr){
		if(cstr_len <= len)
			len = cstr_len - 1;
		cptr = cstr;
	}
	else{
		cptr = NhlMalloc(sizeof(char) * (len + 1));
	}

	if(!cptr || (len < 0)){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}

	strncpy(cptr,fptr,len);
	cptr[len] = '\0';

	return cptr;
}

/*
 * Function:	_NhlFstrToCstr
 *
 * Description:	
 *		This function takes a "Fortran" string and it's length -
 *		A "C" string and it's length and copies the "Fortran" string
 *		to the "C" string removing the " "'s.  If a null c string
 *		is passed in, then this function should malloc the amount
 *		of memory needed.  If the "C" string is not long enough,
 *		or an error occurs - return NULL;
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	char *
 * Side Effect:	
 */
NhlString
_NhlFstrToCstr
#if	NhlNeedProto
(
	NhlString		cstr,
	unsigned int		cstr_len,
	Const _NhlFString	fstr,
	unsigned int		fstr_len
)
#else
(cstr,cstr_len,fstr,fstr_len)
	NhlString		cstr;
	unsigned int		cstr_len;
	Const _NhlFString	fstr;
	unsigned int		fstr_len;
#endif
{
	char	*fptr;

	fptr = _NhlFptrToCptr(fstr);

	return FptrToCstr(cstr,cstr_len,fptr,fstr_len);
}

/*
 * Function:	_NhlMDFstrToCstrtbl
 *
 * Description:	Create a C string table from a Fortran 2d char array.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	NhlString *
 * Side Effect:	
 */
NhlString *
_NhlMDFstrToCstrtbl
#if	NhlNeedProto
(
	Const _NhlFString	fstr,
	unsigned int		num_strings,
	unsigned int		len_strings
)
#else
(fstr,num_strings,len_strings)
	Const _NhlFString	fstr;
	unsigned int		num_strings;
	unsigned int		len_strings;
#endif
{
	NhlString	*tbl = NULL;
	int		i;
	char		*fptr;

	fptr = _NhlFptrToCptr(fstr);

	tbl = NhlMalloc(sizeof(NhlString) * num_strings);
	if(tbl == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}

	for(i=0;i<num_strings;i++)
		tbl[i] = FptrToCstr(NULL,0,fptr+(i*len_strings),len_strings);

	return tbl;
}

/*
 * Function:	_NhlCstrToFptr
 *
 * Description:	
 *		This function takes a C string and copies it into a Fortran
 *		pointer.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	char *
 * Side Effect:	
 */
NhlErrorTypes
_NhlCstrToFptr
#if	NhlNeedProto
(
	NhlString	fptr,
	unsigned int	fstr_len,
	Const char	*cstr
)
#else
(fptr,fstr_len,cstr)
	NhlString	fptr;
	unsigned int	fstr_len;
	Const char	*cstr;
#endif
{
	int	cstr_len;

	if(cstr)
		cstr_len = strlen(cstr);
	else
		cstr_len = 0;

	if(fstr_len >= cstr_len){
		strncpy(fptr,cstr,cstr_len);
		fptr = fptr + cstr_len;
		memset(fptr,' ',fstr_len - cstr_len);

		return NhlNOERROR;
	}

	/*
	 * Not enough memory - put what we can in, and then return WARNING
	 */
	strncpy(fptr,cstr,fstr_len);

	return NhlWARNING;
}

/*
 * Function:	_NhlCstrToFstr
 *
 * Description:	
 *		This function takes a C string and copies it into a Fortran
 *		string variable.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	char *
 * Side Effect:	
 */
NhlErrorTypes
_NhlCstrToFstr
#if	NhlNeedProto
(
	_NhlFString	fstr,
	unsigned int	fstr_len,
	Const char	*cstr
)
#else
(fstr,fstr_len,cstr)
	_NhlFString	fstr;
	unsigned int	fstr_len;
	Const char	*cstr;
#endif
{
	char	*fptr;

	fptr = _NhlFptrToCptr(fstr);

	return _NhlCstrToFptr(fptr,fstr_len,cstr);
}

/*
 * Function:	_NhlFstrToQuark
 *
 * Description:	
 *		This function takes a "Fortran" string and it's length -
 *		and returns a Quark.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	NrmQuark
 * Side Effect:	
 */
NrmQuark
_NhlFstrToQuark
#if	NhlNeedProto
(
	Const _NhlFString	fstr,
	unsigned int		fstr_len
)
#else
(fstr,fstr_len)
	Const _NhlFString	fstr;
	unsigned int		fstr_len;
#endif
{
	char		*fptr;
	char		stack_cptr[_NhlMAXRESNAMLEN];
	char		*cptr = stack_cptr;
	int		i = fstr_len;
	NrmQuark	q;

	fptr = _NhlFptrToCptr(fstr);

	while((i > 0) && (fptr[i-1] == ' ')) i--;

	if(i==0)
		return NrmNULLQUARK;

	if(NhlNumber(stack_cptr) < i+1){
		cptr = NhlMalloc(sizeof(char) * (i+1));
	}

	if(!cptr){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NrmNULLQUARK;
	}

	strncpy(cptr,fptr,i);
	cptr[i] = '\0';

	q = NrmStringToQuark(cptr);

	if(cptr != stack_cptr)
		NhlFree(cptr);

	return q;
}

/*
 * Function:	_NhlCreateFGenArray
 *
 * Description:	Create a GenArray from a Fortran Array.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	NhlGenArray
 * Side Effect:	
 */
NhlGenArray
_NhlCreateFGenArray
#if	NhlNeedProto
(
	NhlPointer	data,
	NhlString	type,
	unsigned int	size,
	int		num_dimensions,
	int		*len_dimensions,
	NhlBoolean	copy_data
)
#else
(data,type,size,num_dimensions,len_dimensions,copy_data)
	NhlPointer	data;
	NhlString	type;
	unsigned int	size;
	int		num_dimensions;
	int		*len_dimensions;
	NhlBoolean	copy_data;
#endif
{
	ng_size_t	stacklen[_NhlMAXRESNAMLEN];
	ng_size_t	*lenptr = stacklen;
	int		i;
	NhlGenArray	gen;

	if(num_dimensions > NhlNumber(stacklen)){
		lenptr = (ng_size_t*)NhlMalloc(sizeof(ng_size_t) * num_dimensions);
		if(lenptr == NULL){
			NhlPError(NhlFATAL,ENOMEM,NULL);
			return NULL;
		}
	}

	/* reverse dim order - from fortran */
	for(i=0;i < num_dimensions; i++)
		lenptr[i] = len_dimensions[(num_dimensions - 1) - i];

	gen = _NhlCreateGenArray(data,type,size,num_dimensions,lenptr,
								copy_data);

	if(lenptr != stacklen)
		NhlFree(lenptr);

	return gen;
}
