/*
 *      $Id: Fortran.c,v 1.1 1994-05-12 23:51:16 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Fortran.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Mar 24 16:09:22 MST 1994
 *
 *	Description:	This file contains the functions the hlu Fortran
 *			interface calls.  They remap the args and call
 *			the apropriate C functions.
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/Converters.h>

/*
 * most arrays will have fewer dimensions than this number.
 */
#define	NORMAL_DIM	(10)

static NrmQuark	intQ;
static NrmQuark	floatQ;
static NrmQuark	stringQ;
static NrmQuark	genQ;
static NrmQuark	FExpStrQ;
static NrmQuark	FExpStrArrQ;
static NrmQuark	FExpArrQ;

/*
 * Function:	nhl_frlcreate
 *
 * Description:	This is a rapper function to allow fortran to get a ResList.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlcreate,NHL_FRLCREATE)
#if	NhlNeedProto
(
	int			*listid,
	Const _NhlFString	ltype,
	int			*ltype_len
)
#else
(listid,ltype,ltype_len)
	int			*listid;
	Const _NhlFString	ltype;
	int			*ltype_len;
#endif
{
	char		tstring[_NhlMAXRESNAMLEN];
	NhlRLType	rltype;
	NrmValue	from, to;

	if(!_NhlFstrToCstr(tstring,NhlNumber(tstring),ltype,*ltype_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Can't convert Fortran string to C string");
		*listid = NhlFATAL;
		return;
	}

	from.size = strlen(tstring);
	from.data.strval = tstring;
	to.size = sizeof(rltype);
	to.data.ptrval = &rltype;

	if(NhlConvertData(NhlTString,NhlTRLType,&from,&to) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid list type \"%s\"",
								tstring);
		*listid = NhlFATAL;
		return;
	}

	*listid = NhlRLCreate(rltype);

	return;
}

/*
 * Function:	nhl_frldestroy
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private global fortran
 * Returns:	
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frldestroy,NHL_FRLDESTROY)
#if	NhlNeedProto
(
	int	*id
)
#else
(id)
	int	*id;
#endif
{
	NhlRLDestroy(*id);

	return;
}

/*
 * Function:	nhl_frlclear
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private global fortran
 * Returns:	
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlclear,NHL_FRLCLEAR)
#if	NhlNeedProto
(
	int	*id
)
#else
(id)
	int	*id;
#endif
{
	NhlRLClear(*id);

	return;
}

/*
 * Function:	nhl_frlunset
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private global fortran
 * Returns:	
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlunset,NHL_FRLUNSET)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len
)
#else
(id,fname,fname_len)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
#endif
{
	char	resname[_NhlMAXRESNAMLEN];

	if(!_NhlFstrToCstr(resname,NhlNumber(resname),fname,*fname_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Unable to convert Fortran string to C string");
		return;
	}

	NhlRLUnSet(*id,resname);

	return;
}

/*
 * Function:	nhl_frlisset
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private global fortran
 * Returns:	
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlisset,NHL_FRLISSET)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len
)
#else
(id,fname,fname_len)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
#endif
{
	char	resname[_NhlMAXRESNAMLEN];

	if(!_NhlFstrToCstr(resname,NhlNumber(resname),fname,*fname_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Unable to convert Fortran string to C string");
		return;
	}

	NhlRLIsSet(*id,resname);

	return;
}

/*
 * Function:	nhl_frlsetint
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlsetint,NHL_FRLSETINT)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	int		*ival,
	int		*err
)
#else
(id,fname,fname_len,ival,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*ival;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.intval = *ival;

	if(_NhlRLInsert(*id,NhlSETRL,_NhlFstrToQuark(fname,*fname_len),intQ,val,
									NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhl_frlsetfloat
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlsetfloat,NHL_FRLSETFLOAT)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	float		*fval,
	int		*err
)
#else
(id,fname,fname_len,fval,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*fval;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.fltval = *fval;

	if(_NhlRLInsert(*id,NhlSETRL,_NhlFstrToQuark(fname,*fname_len),floatQ,
								val,NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhl_frlsetstring
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlsetstring,NHL_FRLSETSTRING)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	_NhlFString	fstring,
	int		*fstring_len,
	int		*err
)
#else
(id,fname,fname_len,fstring,fstring_len,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	_NhlFString	fstring;
	int		*fstring_len;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.strval = _NhlFstrToCstr(NULL,0,fstring,*fstring_len);

	if(_NhlRLInsert(*id,NhlSETRL,_NhlFstrToQuark(fname,*fname_len),stringQ,
						val,(_NhlFreeFunc)NhlFree))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	NhlFSetMDArray
 *
 * Description:	Set a Multi-dim array resources with a Fortran array.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlBoolean
 * Side Effect:	
 */
static NhlErrorTypes
NhlFSetMDArray
#if	NhlNeedProto
(
	int		id,
	_NhlFString	fname,
	int		fname_len,
	NhlPointer	data,
	NhlString	type,
	unsigned int	size,
	int		num_dimensions,
	int		*len_dimensions
)
#else
(id,fname,fname_len,data,type,size,num_dimensions,len_dimensions)
	int		id;
	_NhlFString	fname;
	int		fname_len;
	NhlPointer	data;
	NhlString	type;
	unsigned int	size;
	int		num_dimensions;
	int		*len_dimensions;
#endif
{
	_NhlArgVal	gen;

	gen.ptrval = _NhlCreateFGenArray(data,type,size,num_dimensions,
							len_dimensions,False);

	if(gen.ptrval == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to add array to RL");
		return NhlFATAL;
	}

	if(_NhlRLInsert(id,NhlSETRL,_NhlFstrToQuark(fname,fname_len),genQ,gen,
						(_NhlFreeFunc)NhlFreeGenArray))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	nhl_frlsetmdintarray
 *
 * Description:	Set a multidimentional int array
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private fortran global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlsetmdintarray,NHL_FRLSETMDINTARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	int		*data,
	int		*numdim,
	int		*lendim,
	int		*err
)
#else
(id,fname,fname_len,data,numdim,lendim,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*data;
	int		*numdim;
	int		*lendim;
	int		*err;
#endif
{
	*err = NhlFSetMDArray(*id,fname,*fname_len,data,NhlTInteger,sizeof(int),
								*numdim,lendim);

	return;
}

/*
 * Function:	nhl_frlsetmdfloatarray
 *
 * Description:	Set a multidimentional int array
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private fortran global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlsetmdfloatarray,NHL_FRLSETMDFLOATARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	float		*data,
	int		*numdim,
	int		*lendim,
	int		*err
)
#else
(id,fname,fname_len,data,numdim,lendim,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	float		*data;
	int		*numdim;
	int		*lendim;
	int		*err;
#endif
{
	*err = NhlFSetMDArray(*id,fname,*fname_len,data,NhlTFloat,sizeof(float),
								*numdim,lendim);

	return;
}

/*
 * Function:	nhl_frlsetintarray
 *
 * Description:	Set a multidimentional int array
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private fortran global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlsetintarray,NHL_FRLSETINTARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	int		*data,
	int		*numelements,
	int		*err
)
#else
(id,fname,fname_len,data,numelements,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*data;
	int		*numelements;
	int		*err;
#endif
{
	*err = NhlFSetMDArray(*id,fname,*fname_len,data,NhlTInteger,sizeof(int),
								1,numelements);

	return;
}

/*
 * Function:	nhl_frlsetfloatarray
 *
 * Description:	Set a multidimentional int array
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private fortran global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlsetfloatarray,NHL_FRLSETFLOATARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	float		*data,
	int		*numelements,
	int		*err
)
#else
(id,fname,fname_len,data,numelements,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	float		*data;
	int		*numelements;
	int		*err;
#endif
{
	*err = NhlFSetMDArray(*id,fname,*fname_len,data,NhlTFloat,sizeof(float),
								1,numelements);

	return;
}

/*
 * Function:	nhl_frlsetstringarray
 *
 * Description:	Set a multidimentional int array
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private fortran global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlsetstringarray,NHL_FRLSETSTRINGARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	_NhlFString	data,
	int		*num_strings,
	int		*maxlen_strings,
	int		*err
)
#else
(id,fname,fname_len,data,num_strings,maxlen_strings,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	_NhlFString	data;
	int		*num_strings;
	int		*maxlen_strings;
	int		*err;
#endif
{
	_NhlArgVal	val;
	NhlGenArray	gen;
	NhlString	*table;

	table = _NhlMDFstrToCstrtbl(data,*num_strings,*maxlen_strings);

	if(table == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Unable to add string array to RL");
		*err = NhlFATAL;
		return;
	}

	gen = _NhlCreateGenArray(table,NhlTString,sizeof(NhlString),1,
							num_strings,False);
	if(gen == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Unable to add string array to RL");
		*err = NhlFATAL;
		return;
	}

	gen->my_data = True;
	val.ptrval = gen;

	if(_NhlRLInsert(*id,NhlSETRL,_NhlFstrToQuark(fname,*fname_len),genQ,val,
						(_NhlFreeFunc)NhlFreeGenArray))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhl_frlgetint
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private Fortran global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlgetint,NHL_FRLGETINT)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	int		*iptr,
	int		*err
)
#else
(id,fname,fname_len,iptr,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*iptr;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.ptrval = iptr;

	if(_NhlRLInsert(*id,NhlGETRL,_NhlFstrToQuark(fname,*fname_len),intQ,val,
									NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhl_frlgetfloat
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private Fortran global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlgetfloat,NHL_FRLGETFLOAT)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	float		*fptr,
	int		*err
)
#else
(id,fname,fname_len,fptr,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*fptr;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.ptrval = fptr;

	if(_NhlRLInsert(*id,NhlGETRL,_NhlFstrToQuark(fname,*fname_len),floatQ,
								val,NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	CvtStrToFStr
 *
 * Description:	copy a resource value into a users FString variable.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtStrToFStr
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	NhlString		cstr;
	_NhlFExportString	exp;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"CvtStrToFStr:Called w/improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	cstr = from->data.strval;
	exp = (_NhlFExportString)to->data.ptrval;

	return _NhlCstrToFstr(exp->fstring,exp->strlen,cstr);
}

/*
 * Function:	CvtIntToFStr
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
/*ARGSUSED*/
static NhlErrorTypes
CvtIntToFStr
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	char			buff[_NhlMAXLINELEN];
	int			tint;
	_NhlFExportString	exp;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"CvtIntToFStr:Called w/improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	exp = (_NhlFExportString)to->data.ptrval;
	tint = from->data.intval;
	sprintf(buff,"%d",tint);

	return _NhlCstrToFstr(exp->fstring,exp->strlen,buff);
}

/*
 * Function:	CvtFloatToFStr
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
/*ARGSUSED*/
static NhlErrorTypes
CvtFloatToFStr
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	char			buff[_NhlMAXLINELEN];
	float			tfloat;
	_NhlFExportString	exp;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"CvtFloatToFStr:Called w/improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	exp = (_NhlFExportString)to->data.ptrval;
	tfloat = from->data.fltval;
	sprintf(buff,"%g",tfloat);

	return _NhlCstrToFstr(exp->fstring,exp->strlen,buff);
}

/*
 * Function:	nhl_frlgetstring
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private Fortran global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlgetstring,NHL_FRLGETSTRING)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	_NhlFString	fstring,
	int		*fstring_len,
	int		*err
)
#else
(id,fname,fname_len,fstring,fstring_len,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	_NhlFString	fstring;
	int		*fstring_len;
	int		*err;
#endif
{
	_NhlArgVal		val;
	_NhlFExportString	exp = NhlMalloc(sizeof(_NhlFExportStringRec));

	if(exp == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		*err = NhlFATAL;
		return;
	}

	exp->fstring = fstring;
	exp->strlen = *fstring_len;
	exp->arr_len = NULL;

	val.ptrval = exp;

	if(_NhlRLInsert(*id,NhlGETRL,_NhlFstrToQuark(fname,*fname_len),FExpStrQ,
						val,(_NhlFreeFunc)NhlFree))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	CvtGenArrToFArr
 *
 * Description:	copy a resource value into a users FArray variable.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtGenArrToFArr
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
#define	StackAlloc(ptr,min_size)\
	if(min_size > NhlNumber(stack_##ptr)){				\
		ptr = NhlMalloc(sizeof(stack_##ptr[0]) * min_size);	\
		if(ptr == NULL){					\
			NHLPERROR((NhlFATAL,ENOMEM,NULL));		\
			to->size = 0;					\
			return NhlFATAL;				\
		}							\
	}								\
	else								\
		ptr = stack_##ptr;

#define StackFree(ptr)\
	if(ptr != stack_##ptr)\
		NhlFree(ptr);

	NhlGenArray		gen;
	_NhlFExportArray	exp;
	int			i;
	int			num_dim=0;
	int			stack_len_dim[NORMAL_DIM];
	int			*len_dim=stack_len_dim;
	int			num_elements=0;
	char			*err = "CvtGenArrToFArr";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlBoolean		contig = True;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called w/improper number of args",err);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;
	exp = (_NhlFExportArray)to->data.ptrval;

	/* first determine if a converter is needed, and if it exists */
	if((exp->typeQ != gen->typeQ) &&
				!_NhlConverterExists(gen->typeQ,exp->typeQ)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to convert \"%s\" to \"%s\"",
					err,
					NrmQuarkToString(gen->typeQ),
					NrmQuarkToString(exp->typeQ));
		to->size = 0;
		return NhlFATAL;
	}

	/* now determine if the FArray is large enough */
	if(exp->num_dim != NULL){
		if(*exp->num_dim < gen->num_dimensions){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:Array Resource has %d dimensions, only space for %d provied",
				err,gen->num_dimensions,*exp->num_dim);
			ret = MIN(ret,NhlWARNING);
			num_dim = *exp->num_dim;
		}
		else
			num_dim = gen->num_dimensions;

		StackAlloc(len_dim,num_dim);

		for(i=0;i < num_dim; i++){
			if(exp->len_dim[i] < gen->len_dimensions[
						(gen->num_dimensions -1 )-i]){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:Array Resources dimension %d has %d elements, only room for %d",
				err,
				i+1,
				gen->len_dimensions[(gen->num_dimensions-1)-i],
				exp->len_dim[i]);
				ret = MIN(ret,NhlWARNING);

				len_dim[i] = exp->len_dim[i];
			}
			else
				len_dim[i] = gen->len_dimensions
						[gen->num_dimensions-1-i];
		}
	}
	else{
		/* pack a one dim array if it is large enough */
		if(*exp->len_dim < gen->num_elements){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Resource array has %d elements, only room for %d",
					err,gen->num_elements,*exp->len_dim);
			ret = MIN(ret,NhlWARNING);
			num_elements = *exp->len_dim;
		}
		else
			num_elements = gen->num_elements;

		len_dim[0] = num_elements;
		num_dim = 1;
	}

	/* start copying... */


	/*
	 * Check for Contiguous memory.
	 * if num_dim == NULL then we are packing a one dim array
	 * otherwise we check to make sure the dimensionality is
	 * correct for contiguous space. Don't have to check last
	 * dimension - we set last dimension's space.
	 */
	if(exp->num_dim != NULL){
		for(i=0;i < num_dim - 1; i++){
			if(exp->len_dim[i] !=
				gen->len_dimensions[gen->num_dimensions-1-i]){

				contig = False;
				break;
			}
		}

		if(contig && (exp->len_dim[num_dim-1] > gen->len_dimensions[0]))
			exp->len_dim[gen->num_dimensions-1] =
							gen->len_dimensions[0];
	}
	else
		*exp->len_dim = num_elements;

	/*
	 * If converters are not needed, and we have contiguous memory,
	 * then we can just to a memcpy.  I wish everything were so
	 * simple.
	 */
	if(contig && (exp->typeQ == gen->typeQ)){
		memcpy(exp->data,gen->data,gen->size * num_elements);
		StackFree(len_dim)

		return NhlNOERROR;
	}


	/*
	 * Non-contiguous memory needs to have *indexing* array allocated.
	 * Doing converter contiguous copy here as well.
	 */
	{
		int	stack_iarr[NORMAL_DIM];
		int	*iarr = stack_iarr;
		int	cindx,findx;
		int	num_passes = 1;
		int	j;

		for(i=1;i<num_dim;i++)
			num_passes *= len_dim[i];

		StackAlloc(iarr,num_dim)
		/* start with 0 indices */
		memset(iarr,0,sizeof(int)*num_dim);
		cindx = findx = 0;

		for(j=0;j<num_passes;j++){

			char		*carr, *farr;

			carr = ((char*)gen->data)+(cindx * gen->size);
			farr = ((char*)exp->data)+(findx * exp->size);

			/*
			 * if memory is of same type, can use memcpy because
			 * 1st dimension is contiguous.
			 */
			if(exp->typeQ == gen->typeQ){
				memcpy(farr,carr,gen->size*len_dim[0]);
			}
			else{
				NhlErrorTypes	ret;
				NrmValue	fromval, toval;

				for(i=0;i < len_dim[0];i++){
					_NhlCopyToVal((NhlPointer)
						(carr+(gen->size*i)),
						&fromval.data,gen->size);
					fromval.size = gen->size;

					toval.data.ptrval = (NhlPointer)
							(farr+(exp->size * i));
					toval.size = exp->size;

					ret = _NhlReConvertData(gen->typeQ,
						exp->typeQ,&fromval,&toval);
					if(ret != NhlNOERROR){
						NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to convert \"%s\" to \"%s\"",
						err,
						NrmQuarkToString(gen->typeQ),
						NrmQuarkToString(exp->typeQ));
						StackFree(iarr)
						StackFree(len_dim)
						to->size = 0;
						return NhlFATAL;
					}
				}
			}

			/* compute new indexes */
			/*
			 * i starts with dim2 because dim1 is handled by
			 * interior loop.
			 */
			for(i=1; i < num_dim;i++){
				if(iarr[i] < (len_dim[i]-1)){
					iarr[i]++;
					break;
				}
				else
					iarr[i] = 0;
			}

			/*
			 * to compute findx.
			 *
			 * farr(A,B,C,D)
			 *	to compute linear offset of farr(a,b,c,d)
			 *	a + Ab + ABc + ABCd
			 *	I decomposed this to...
			 *
			 *	a	(computed in interior loop or memcpy)
			 *	A(b + B(c + Cd))
			 *
			 *	iarr[i] is index into the i dimention.
			 *	iarr is order such that the fastest dimension
			 *	is iarr[0] and slowest dimension is
			 *	iarr[num_dim-1].
			 */
			findx = 0;
			for(i = num_dim-1; i > 0; i--)
				findx = (iarr[i] + findx) * exp->len_dim[i-1];
			cindx = 0;
			for(i = num_dim-1; i > 0; i--)
				cindx = (iarr[i] + cindx) *
				gen->len_dimensions[gen->num_dimensions-i];
		}
		StackFree(iarr)
	}

	*exp->num_dim = num_dim;
	memcpy(exp->len_dim,len_dim,sizeof(int)*num_dim);

	StackFree(len_dim)

	return NhlNOERROR;
#undef	StackAlloc
#undef	StackFree
}

/*
 * Function:	CvtIntToFArr
 *
 * Description:	copy a resource value into a users FArray variable.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtIntToFArr
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	_NhlFExportArray	exp;
	int			tint;
	int			num_dim;
	int			i;
	char			*err = "CvtIntToFArr";

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called w/improper number of args",err);
		to->size = 0;
		return NhlFATAL;
	}

	exp = (_NhlFExportArray)to->data.ptrval;
	tint = from->data.intval;

	/* first determine if a converter is needed, and if it exists */
	if((intQ != exp->typeQ) && !_NhlConverterExists(intQ,exp->typeQ)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to convert \"%s\" to \"%s\"",
					err,
					NhlTInteger,
					NrmQuarkToString(exp->typeQ));
		to->size = 0;
		return NhlFATAL;
	}

	/*
	 * The length of the first dim must be at least 1.
	 */
	if(*exp->len_dim < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Zero length array not valid",err);
		to->size = 0;
		return NhlFATAL;
	}

	/*
	 * if num_dim == NULL - it is a one dim array and we just have to
	 * assign
	 */
	if(exp->num_dim == NULL)
		num_dim = 1;
	else
		num_dim = *exp->num_dim;

	exp->len_dim[0] = 1;
	for(i=1;i < num_dim;i++)
		exp->len_dim[i] = 0;

	if(intQ != exp->typeQ){
		NrmValue	fromval, toval;

		fromval.data.intval = tint;
		fromval.size = sizeof(int);

		toval.data.ptrval = exp->data;
		toval.size = exp->size;

		return _NhlReConvertData(intQ,exp->typeQ,&fromval,&toval);
	}

	*(int*)exp->data = tint;

	return NhlNOERROR;
}

/*
 * Function:	CvtFloatToFArr
 *
 * Description:	copy a resource value into a users FArray variable.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtFloatToFArr
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	_NhlFExportArray	exp;
	float			tfloat;
	int			num_dim;
	int			i;
	char			*err = "CvtFloatToFArr";

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called w/improper number of args",err);
		to->size = 0;
		return NhlFATAL;
	}

	exp = (_NhlFExportArray)to->data.ptrval;
	tfloat = from->data.fltval;

	/* first determine if a converter is needed, and if it exists */
	if((floatQ != exp->typeQ) && !_NhlConverterExists(floatQ,exp->typeQ)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to convert \"%s\" to \"%s\"",
					err,
					NhlTFloat,
					NrmQuarkToString(exp->typeQ));
		to->size = 0;
		return NhlFATAL;
	}

	/*
	 * The length of the first dim must be at least 1.
	 */
	if(*exp->len_dim < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Zero length array not valid",err);
		to->size = 0;
		return NhlFATAL;
	}

	/*
	 * if num_dim == NULL - it is a one dim array and we just have to
	 * assign
	 */
	if(exp->num_dim == NULL)
		num_dim = 1;
	else
		num_dim = *exp->num_dim;

	exp->len_dim[0] = 1;
	for(i=1;i < num_dim;i++)
		exp->len_dim[i] = 0;

	if(floatQ != exp->typeQ){
		NrmValue	fromval, toval;

		fromval.data.fltval = tfloat;
		fromval.size = sizeof(float);

		toval.data.ptrval = exp->data;
		toval.size = exp->size;

		return _NhlReConvertData(floatQ,exp->typeQ,&fromval,&toval);
	}

	*(float*)exp->data = tfloat;

	return NhlNOERROR;
}

/*
 * Function:	GetFArray
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
NhlBoolean
GetFArray
#if	NhlNeedProto
(
	int		id,
	_NhlFString	fname,
	int		fname_len,
	NhlPointer	data,
	NrmQuark	typeQ,
	unsigned int	size,
	int		*num_dim,
	int		*len_dim
)
#else
(id,fname,fname_len,data,typeQ,size,num_dim,len_dim)
	int		id;
	_NhlFString	fname;
	int		fname_len;
	NhlPointer	data;
	NrmQuark	typeQ;
	unsigned int	size;
	int		*num_dim;
	int		*len_dim;
#endif
{
	_NhlArgVal		val;
	_NhlFExportArray	exp = NhlMalloc(sizeof(_NhlFExportArrayRec));

	if(exp == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return False;
	}

	exp->data = data;
	exp->typeQ = typeQ;
	exp->size = size;
	exp->num_dim = num_dim;
	exp->len_dim = len_dim;

	val.ptrval = exp;

	if(_NhlRLInsert(id,NhlGETRL,_NhlFstrToQuark(fname,fname_len),FExpArrQ,
						val,(_NhlFreeFunc)NhlFree))
		return False;
	else
		return True;
}

/*
 * Function:	nhl_frlgetmdintarray
 *
 * Description:	fortran func
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlgetmdintarray,NHL_FRLGETMDINTARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	int		*arr,
	int		*num_dim,
	int		*len_dim,
	int		*err
)
#else
(id,fname,fname_len,arr,num_dim,len_dim,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*arr;
	int		*num_dim;
	int		*len_dim;
	int		*err;
#endif
{
	if(GetFArray(*id,fname,*fname_len,arr,intQ,sizeof(int),num_dim,len_dim))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhl_frlgetmdfloatarray
 *
 * Description:	fortran func
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlgetmdfloatarray,NHL_FRLGETMDFLOATARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	float		*arr,
	int		*num_dim,
	int		*len_dim,
	int		*err
)
#else
(id,fname,fname_len,arr,num_dim,len_dim,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	float		*arr;
	int		*num_dim;
	int		*len_dim;
	int		*err;
#endif
{
	if(GetFArray(*id,fname,*fname_len,arr,floatQ,sizeof(float),num_dim,
								len_dim))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhl_frlgetintarray
 *
 * Description:	fortran func
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlgetintarray,NHL_FRLGETINTARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	int		*arr,
	int		*num_elements,
	int		*err
)
#else
(id,fname,fname_len,arr,num_elements,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*arr;
	int		*num_elements;
	int		*err;
#endif
{
	if(GetFArray(*id,fname,*fname_len,arr,intQ,sizeof(int),NULL,
								num_elements))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhl_frlgetfloatarray
 *
 * Description:	fortran func
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlgetfloatarray,NHL_FRLGETFLOATARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	float		*arr,
	int		*num_elements,
	int		*err
)
#else
(id,fname,fname_len,arr,num_elements,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	float		*arr;
	int		*num_elements;
	int		*err;
#endif
{
	if(GetFArray(*id,fname,*fname_len,arr,floatQ,sizeof(float),NULL,
								num_elements))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	CvtGenToFStrArr
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
/*ARGSUSED*/
static NhlErrorTypes
CvtGenToFStrArr
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	_NhlFExportString	exp;
	NhlGenArray		gen;
	char			*name = "CvtGenToFStrArr";
	char			*fptr, *cptr;
	NhlErrorTypes		ret = NhlNOERROR, lret = NhlNOERROR;
	int			num_elements;
	int			i;
	NrmValue		fromval,toval;
	NhlString		tstr;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called w/improper number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	gen = (NhlGenArray)from->data.ptrval;
	exp = (_NhlFExportString)to->data.ptrval;

	if(exp->arr_len == NULL){
		/* 1 dim char string??? */
		num_elements = 1;
	}
	else if(gen->num_elements > *exp->arr_len){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:Array resource has %d elements:Only space for %d provided",
					name,gen->num_elements,*exp->arr_len);
		ret = MIN(ret,NhlWARNING);
		num_elements = *exp->arr_len;
	}
	else
		num_elements = *exp->arr_len = gen->num_elements;

	fptr = _NhlFptrToCptr(exp->fstring);

	/*
	 * if the gen array is an NhlTString array, then just copy each string
	 * to each element of the Fortran character*(strlen) array.
	 */

	if(gen->typeQ == stringQ){
		NhlString	*ctbl;

		ctbl = gen->data;

		for(i=0;i < num_elements;i++){
			lret = _NhlCstrToFptr(fptr+(i*exp->strlen),exp->strlen,
								ctbl[i]);
			ret = MIN(ret,lret);
		}

		return ret;
	}

	if(!_NhlConverterExists(gen->typeQ,stringQ)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),NhlTString));
		to->size = 0;
		return NhlFATAL;
	}

	toval.data.ptrval = &tstr;
	toval.size = sizeof(NhlString);

	cptr = gen->data;
	fromval.size = gen->size;

	for(i=0;i < num_elements;i++){

		_NhlCopyToVal((NhlPointer)(cptr+(gen->size*i)),&fromval.data,
								gen->size);

		lret = _NhlReConvertData(gen->typeQ,stringQ,&fromval,&toval);

		if(lret < NhlWARNING){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),NhlTString));
			return NhlFATAL;
		}
		ret = MIN(ret,lret);

		lret = _NhlCstrToFptr(fptr+(i*exp->strlen),exp->strlen,tstr);
		ret = MIN(ret,lret);
	}

	return ret;
}

/*
 * Function:	nhl_frlgetstringarray
 *
 * Description:	fortran func
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhl_frlgetstringarray,NHL_FRLGETSTRINGARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	_NhlFString	arr,
	int		*arr_width,
	int		*arr_len,
	int		*err
)
#else
(id,fname,fname_len,arr,arr_width,arr_len,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	_NhlFString	arr;
	int		*arr_width;
	int		*arr_len;
	int		*err;
#endif
{
	_NhlArgVal		val;
	_NhlFExportString	exp = NhlMalloc(sizeof(_NhlFExportStringRec));

	if(exp == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		*err = NhlFATAL;
		return;
	}

	exp->fstring = arr;
	exp->strlen = *arr_width;
	exp->arr_len = arr_len;

	val.ptrval = exp;

	if(_NhlRLInsert(*id,NhlGETRL,_NhlFstrToQuark(fname,*fname_len),
					FExpStrArrQ,val,(_NhlFreeFunc)NhlFree))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	_NhlFortranInit
 *
 * Description:	Do the initialization that is needed for the Fortran interface.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	void
 * Side Effect:	
 */
void
_NhlFortranInit
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	NhlConvertArg	RlListTypeEnum[] = {
		{NhlSTRENUM,	NhlSETRL,	"SETRL"},
		{NhlSTRENUM,	NhlGETRL,	"GETRL"}
	};

	intQ = NrmStringToQuark(NhlTInteger);
	floatQ = NrmStringToQuark(NhlTFloat);
	stringQ = NrmStringToQuark(NhlTString);
	genQ = NrmStringToQuark(NhlTGenArray);
	FExpStrQ = NrmStringToQuark(_NhlTFExpString);
	FExpStrArrQ = NrmStringToQuark(_NhlTFExpStringArr);
	FExpArrQ = NrmStringToQuark(_NhlTFExpArray);

	(void)NhlRegisterConverter(NhlTString,NhlTRLType,NhlCvtStringToEnum,
			RlListTypeEnum,NhlNumber(RlListTypeEnum),False,NULL);

	(void)NhlRegisterConverter(NhlTString,_NhlTFExpString,CvtStrToFStr,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,_NhlTFExpString,CvtIntToFStr,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,_NhlTFExpString,CvtFloatToFStr,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,_NhlTFExpArray,CvtGenArrToFArr,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,_NhlTFExpStringArr,
					CvtGenToFStrArr,NULL,0,False,NULL);

	(void)NhlRegisterConverter(NhlTInteger,_NhlTFExpArray,CvtIntToFArr,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,_NhlTFExpArray,CvtFloatToFArr,
							NULL,0,False,NULL);
}
