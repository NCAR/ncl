/*
 *      $Id: Fortran.c,v 1.16.4.1 2008-03-28 20:37:35 grubin Exp $
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
#include <ncarg/hlu/ConvertersP.h>

/*
 * most arrays will have fewer dimensions than this number.
 */
#define	NORMAL_DIM	(10)

static NrmQuark	intQ;
static NrmQuark	floatQ;
static NrmQuark	doubleQ;
static NrmQuark	stringQ;
static NrmQuark	genQ;
static NrmQuark	strgenQ;
static NrmQuark	FExpStrQ;
static NrmQuark	FExpStrArrQ;
static NrmQuark	FExpArrQ;

static void FortranInit(
#if	NhlNeedProto
	void
#endif
);

/*
 * Function:	nhlpfrlcreate
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
_NHLCALLF(nhlpfrlcreate,NHLPFRLCREATE)
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
	static NhlBoolean	initialized = False;
	char			tstring[_NhlMAXRESNAMLEN];
	NhlRLType		rltype;
	NrmValue		from, to;

	if(!initialized){
		FortranInit();
		initialized = True;
	}

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

	if(NhlConvertData(NhlDEFAULT_APP,NhlTString,NhlTRLType,&from,&to) <
								NhlINFO){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid list type \"%s\"",
								tstring);
		*listid = NhlFATAL;
		return;
	}

	*listid = NhlRLCreate(rltype);

	return;
}

/*
 * Function:	nhlpfrldestroy
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
_NHLCALLF(nhlpfrldestroy,NHLPFRLDESTROY)
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
 * Function:	nhlpfrlclear
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
_NHLCALLF(nhlpfrlclear,NHLPFRLCLEAR)
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
 * Function:	nhlpfrlunset
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
_NHLCALLF(nhlpfrlunset,NHLPFRLUNSET)
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
 * Function:	nhlpfrlisset
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
_NHLCALLF(nhlpfrlisset,NHLPFRLISSET)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	int		*ret_val
)
#else
(id,fname,fname_len,ret_val)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	int		*ret_val;
#endif
{
	char	resname[_NhlMAXRESNAMLEN];

	if(!_NhlFstrToCstr(resname,NhlNumber(resname),fname,*fname_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Unable to convert Fortran string to C string");
		return;
	}

	*ret_val = NhlRLIsSet(*id,resname);

	return;
}

/*
 * Function:	nhlpfrlsetinteger
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
_NHLCALLF(nhlpfrlsetinteger,NHLPFRLSETINTEGER)
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
							sizeof(int),NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhlpfrlsetfloat
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
_NHLCALLF(nhlpfrlsetfloat,NHLPFRLSETFLOAT)
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
	float		*fval;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.fltval = *fval;

	if(_NhlRLInsert(*id,NhlSETRL,_NhlFstrToQuark(fname,*fname_len),floatQ,
							val,sizeof(float),NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhlpfrlsetdouble
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
_NHLCALLF(nhlpfrlsetdouble,NHLPFRLSETDOUBLE)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	double		*dval,
	int		*err
)
#else
(id,fname,fname_len,dval,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	double		*dval;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.dblval = *dval;

	if(_NhlRLInsert(*id,NhlSETRL,_NhlFstrToQuark(fname,*fname_len),doubleQ,
						val,sizeof(double),NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhlpfrlsetstring
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
_NHLCALLF(nhlpfrlsetstring,NHLPFRLSETSTRING)
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
				val,sizeof(NhlString),(_NhlFreeFunc)NhlFree))
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
			sizeof(NhlGenArray),(_NhlFreeFunc)NhlFreeGenArray))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	nhlpfrlsetmdintegerarray
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
_NHLCALLF(nhlpfrlsetmdintegerarray,NHLPFRLSETMDINTEGERARRAY)
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
 * Function:	nhlpfrlsetmdfloatarray
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
_NHLCALLF(nhlpfrlsetmdfloatarray,NHLPFRLSETMDFLOATARRAY)
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
 * Function:	nhlpfrlsetmddoublearray
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
_NHLCALLF(nhlpfrlsetmddoublearray,NHLPFRLSETMDDOUBLEARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	double		*data,
	int		*numdim,
	int		*lendim,
	int		*err
)
#else
(id,fname,fname_len,data,numdim,lendim,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	double		*data;
	int		*numdim;
	int		*lendim;
	int		*err;
#endif
{
	*err = NhlFSetMDArray(*id,fname,*fname_len,data,NhlTDouble,
						sizeof(double),*numdim,lendim);

	return;
}

/*
 * Function:	nhlpfrlsetintegerarray
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
_NHLCALLF(nhlpfrlsetintegerarray,NHLPFRLSETINTEGERARRAY)
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
 * Function:	nhlpfrlsetfloatarray
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
_NHLCALLF(nhlpfrlsetfloatarray,NHLPFRLSETFLOATARRAY)
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
 * Function:	nhlpfrlsetdoublearray
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
_NHLCALLF(nhlpfrlsetdoublearray,NHLPFRLSETDOUBLEARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	double		*data,
	int		*numelements,
	int		*err
)
#else
(id,fname,fname_len,data,numelements,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	double		*data;
	int		*numelements;
	int		*err;
#endif
{
	*err = NhlFSetMDArray(*id,fname,*fname_len,data,NhlTDouble,
						sizeof(double),1,numelements);

	return;
}

/*
 * Function:	nhlpfrlsetstringarray
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
_NHLCALLF(nhlpfrlsetstringarray,NHLPFRLSETSTRINGARRAY)
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
	ng_size_t       nstrings;

	nstrings = *num_strings;
	table = _NhlMDFstrToCstrtbl(data,*num_strings,*maxlen_strings);

	if(table == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Unable to add string array to RL");
		*err = NhlFATAL;
		return;
	}
        /* this is wrong -- can't just cast to a different size pointer */
	gen = _NhlCreateGenArray(table,NhlTString,sizeof(NhlString),1,&nstrings,False);
	if(gen == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Unable to add string array to RL");
		*err = NhlFATAL;
		return;
	}

	gen->my_data = True;
	val.ptrval = gen;

	if(_NhlRLInsert(*id,NhlSETRL,_NhlFstrToQuark(fname,*fname_len),strgenQ,
			val,sizeof(NhlGenArray),(_NhlFreeFunc)NhlFreeGenArray))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhlpfrlgetinteger
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
_NHLCALLF(nhlpfrlgetinteger,NHLPFRLGETINTEGER)
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
						sizeof(NhlPointer),NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhlpfrlgetfloat
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
_NHLCALLF(nhlpfrlgetfloat,NHLPFRLGETFLOAT)
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
	float		*fptr;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.ptrval = fptr;

	if(_NhlRLInsert(*id,NhlGETRL,_NhlFstrToQuark(fname,*fname_len),floatQ,
						val,sizeof(NhlPointer),NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhlpfrlgetdouble
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
_NHLCALLF(nhlpfrlgetdouble,NHLPFRLGETDOUBLE)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	double		*dptr,
	int		*err
)
#else
(id,fname,fname_len,dptr,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	double		*dptr;
	int		*err;
#endif
{
	_NhlArgVal	val;

	val.ptrval = dptr;

	if(_NhlRLInsert(*id,NhlGETRL,_NhlFstrToQuark(fname,*fname_len),doubleQ,
						val,sizeof(NhlPointer),NULL))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhlpfrlgetstring
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
_NHLCALLF(nhlpfrlgetstring,NHLPFRLGETSTRING)
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
				val,sizeof(NhlPointer),(_NhlFreeFunc)NhlFree))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	NhlCvtScalarToFStr
 *
 * Description:	
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
NhlCvtScalarToFStr
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
	char			func[] = "NhlCvtScalartoFStr";
	NhlString		tstring = NULL;
	NrmValue		svalue;
	_NhlFExportString	exp;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	if(from->typeQ == stringQ){
		tstring = from->data.strval;
	}
	else{
		svalue.size = sizeof(NhlString);
		svalue.data.ptrval = &tstring;

		if((_NhlReConvertData(from->typeQ,stringQ,from,&svalue) <
					NhlWARNING) || (tstring == NULL)){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to convert %s to FORTRAN string",func,
						NrmQuarkToString(from->typeQ));
			return NhlFATAL;
		}
	}

	exp = (_NhlFExportString)to->data.ptrval;

	return _NhlCstrToFstr(exp->fstring,exp->strlen,tstring);
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

	/* determine if the FArray is large enough */
	if(exp->num_dim != NULL){
		if(*exp->num_dim < gen->num_dimensions){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:Array Resource has %d dimensions, only space for %d provided",
				err,gen->num_dimensions,*exp->num_dim);
			ret = MIN(ret,NhlWARNING);
			num_dim = *exp->num_dim;
		}
		else
			num_dim = gen->num_dimensions;

		StackAlloc(len_dim,num_dim);

		num_elements = 1;
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
			else{
				len_dim[i] = gen->len_dimensions
						[gen->num_dimensions-1-i];
			}

			num_elements *= len_dim[i];
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
	 * then we can just do a memcpy.  I wish everything were so
	 * simple.
	 */
	if(contig && _NhlIsSubtypeQ(exp->typeQ,gen->typeQ)){
		memcpy(exp->data,gen->data,gen->size * num_elements);
		StackFree(len_dim)

		return NhlNOERROR;
	}


	/*
	 * Non-contiguous memory needs to have *indexing* array allocated.
	 * Doing converter contiguous copy here as well.
	 */
	{
		NrmValue	fromval, toval;
		int		stack_iarr[NORMAL_DIM];
		int		*iarr = stack_iarr;
		int		cindx,findx;
		int		num_passes = 1;
		int		j;
		NhlGenArrayRec	cgen;
		NhlGenArray	tgen;
		char		cgentype[_NhlMAXRESNAMLEN];
		char		tgentype[_NhlMAXRESNAMLEN];

		/*
		 * Prepare "from" array.
		 */
		cgen.num_dimensions = 1;
		cgen.num_elements = len_dim[0];
		cgen.len_dimensions = &cgen.num_elements;
		cgen.typeQ = gen->typeQ;
		cgen.size = gen->size;
		cgen.my_data = False;

		/*
		 * from array name and to array name
		 */
		strcpy(cgentype,NrmQuarkToString(cgen.typeQ));
		strcat(cgentype,NhlTGenArray);
		strcpy(tgentype,NrmQuarkToString(exp->typeQ));
		strcat(tgentype,NhlTGenArray);

		/*
		 * from values and to values
		 */
		fromval.size = sizeof(NhlGenArray);
		fromval.data.ptrval = &cgen;
		toval.size = sizeof(NhlGenArray);
		toval.data.ptrval = &tgen;

		for(i=1;i<num_dim;i++)
			num_passes *= len_dim[i];

		StackAlloc(iarr,num_dim)
		/* start with 0 indices */
		memset((char*)iarr,0,sizeof(int)*num_dim);
		cindx = findx = 0;

		for(j=0;j<num_passes;j++){

			char		*carr, *farr;

			carr = ((char*)gen->data)+(cindx * gen->size);
			farr = ((char*)exp->data)+(findx * exp->size);

			/*
			 * if memory is of same type, can use memcpy because
			 * 1st dimension is contiguous.
			 */
			if(_NhlIsSubtypeQ(exp->typeQ,gen->typeQ)){
				memcpy(farr,carr,gen->size*len_dim[0]);
			}
			else{
				NhlErrorTypes	ret;

				cgen.data = carr;
				tgen = NULL;

				ret = NhlReConvertData(cgentype,tgentype,
							&fromval,&toval);
				if((ret < NhlWARNING) || (tgen == NULL)){
					NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to convert \"%s\" to \"%s\"",
						err,cgentype,tgentype);
					StackFree(iarr)
					StackFree(len_dim)
					to->size = 0;
					return NhlFATAL;
				}
				memcpy(farr,tgen->data,gen->size*len_dim[0]);
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

	if(exp->num_dim != NULL){
		*exp->num_dim = num_dim;
		memcpy((char*)exp->len_dim,(Const char*)len_dim,
							sizeof(int)*num_dim);
	}
	else{
		*exp->len_dim = len_dim[0];
	}

	StackFree(len_dim)

	return NhlNOERROR;
#undef	StackAlloc
#undef	StackFree
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

	return _NhlRLInsert(id,NhlGETRL,_NhlFstrToQuark(fname,fname_len),
			FExpArrQ,val,sizeof(NhlPointer),(_NhlFreeFunc)NhlFree);
}

/*
 * Function:	nhlpfrlgetmdintegerarray
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
_NHLCALLF(nhlpfrlgetmdintegerarray,NHLPFRLGETMDINTEGERARRAY)
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
 * Function:	nhlpfrlgetmdfloatarray
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
_NHLCALLF(nhlpfrlgetmdfloatarray,NHLPFRLGETMDFLOATARRAY)
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
 * Function:	nhlpfrlgetmddoublearray
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
_NHLCALLF(nhlpfrlgetmddoublearray,NHLPFRLGETMDDOUBLEARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	double		*arr,
	int		*num_dim,
	int		*len_dim,
	int		*err
)
#else
(id,fname,fname_len,arr,num_dim,len_dim,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	double		*arr;
	int		*num_dim;
	int		*len_dim;
	int		*err;
#endif
{
	if(GetFArray(*id,fname,*fname_len,arr,doubleQ,sizeof(double),num_dim,
								len_dim))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

/*
 * Function:	nhlpfrlgetintegerarray
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
_NHLCALLF(nhlpfrlgetintegerarray,NHLPFRLGETINTEGERARRAY)
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
 * Function:	nhlpfrlgetfloatarray
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
_NHLCALLF(nhlpfrlgetfloatarray,NHLPFRLGETFLOATARRAY)
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
 * Function:	nhlpfrlgetdoublearray
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
_NHLCALLF(nhlpfrlgetdoublearray,NHLPFRLGETDOUBLEARRAY)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	fname,
	int		*fname_len,
	double		*arr,
	int		*num_elements,
	int		*err
)
#else
(id,fname,fname_len,arr,num_elements,err)
	int		*id;
	_NhlFString	fname;
	int		*fname_len;
	double		*arr;
	int		*num_elements;
	int		*err;
#endif
{
	if(GetFArray(*id,fname,*fname_len,arr,doubleQ,sizeof(double),NULL,
								num_elements))
		*err = NhlNOERROR;
	else
		*err = NhlFATAL;

	return;
}

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
	NhlGenArray		gen,tgen,strgen;
	char			*name = "CvtGenToFStrArr";
	char			*fptr;
	NhlString		*ctbl;
	NhlErrorTypes		ret = NhlNOERROR, lret = NhlNOERROR;
	int			num_elements;
	int			i;
	NrmValue		fromval,toval;

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

	if(gen->typeQ == stringQ){
		strgen = gen;
	}
	else{
		tgen = _NhlConvertCopyGenArray(gen);
		if(tgen == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",name);
			return NhlFATAL;
		}
		tgen->num_elements = num_elements;

		fromval.size = sizeof(NhlGenArray);
		fromval.data.ptrval = tgen;

		strgen = NULL;
		toval.size = sizeof(NhlGenArray);
		toval.data.ptrval = &strgen;

		lret = NhlReConvertData(NhlTGenArray,NhlTStringGenArray,
							&fromval,&toval);

		if((lret < NhlWARNING) || (strgen == NULL)){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),NhlTString));
			return NhlFATAL;
		}
		ret = MIN(ret,lret);
	}

	/*
	 * now the gen array is an NhlTString array, just copy each string
	 * to each element of the Fortran character*(strlen) array.
	 */

	ctbl = strgen->data;

	for(i=0;i < num_elements;i++){
		lret = _NhlCstrToFptr(fptr+(i*exp->strlen),exp->strlen,ctbl[i]);
		ret = MIN(ret,lret);
	}

	return ret;
}

/*
 * Function:	nhlpfrlgetstringarray
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
_NHLCALLF(nhlpfrlgetstringarray,NHLPFRLGETSTRINGARRAY)
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
		FExpStrArrQ,val,sizeof(NhlPointer),(_NhlFreeFunc)NhlFree))
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
static void
FortranInit
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlEnumVals	RlListTypeEnum[] = {
		{NhlSETRL,	"SETRL"},
		{NhlGETRL,	"GETRL"}
	};

	intQ = NrmStringToQuark(NhlTInteger);
	floatQ = NrmStringToQuark(NhlTFloat);
	doubleQ = NrmStringToQuark(NhlTDouble);
	stringQ = NrmStringToQuark(NhlTString);
	genQ = NrmStringToQuark(NhlTGenArray);
	strgenQ = NrmStringToQuark(NhlTStringGenArray);
	FExpStrQ = NrmStringToQuark(_NhlTFExpString);
	FExpStrArrQ = NrmStringToQuark(_NhlTFExpStringArr);
	FExpArrQ = NrmStringToQuark(_NhlTFExpArray);

	_NhlRegisterEnumType(NULL,NhlTRLType,RlListTypeEnum,
						NhlNumber(RlListTypeEnum));

	/*
	 * convert to a fortran string.
	 */
	(void)NhlRegisterConverter(NULL,NhlTScalar,_NhlTFExpString,
					NhlCvtScalarToFStr,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,_NhlTFExpString,
						NhlTGenArray,NhlTScalar);
	/*
	 * convert to a fortran array.
	 */
	(void)NhlRegisterConverter(NULL,NhlTGenArray,_NhlTFExpArray,
		CvtGenArrToFArr,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTScalar,_NhlTFExpArray,NhlTScalar,
								NhlTGenArray);


	/*
	 * convert to a fortran string array.
	 */
	(void)NhlRegisterConverter(NULL,NhlTGenArray,_NhlTFExpStringArr,
					CvtGenToFStrArr,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTScalar,_NhlTFExpStringArr,
						NhlTScalar,NhlTGenArray);
}
