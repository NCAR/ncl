/*
 *      $Id: Converters.c,v 1.7 1994-02-08 20:15:18 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Converters.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Sep 18 10:38:18 MDT 1992
 *
 *	Description:	This file contains all the default type converters
 *			that are created and installed when the hlu library
 *			is initialized.
 */
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Converters.h>
#include <math.h>

#if	defined(SunOs) && (MAJOR == 4)
#include <floatingpoint.h>
#endif	/* sun hack- strtod should be in stdlib.h but it's not */

/*
 * This macro is used because most of the converters end the same way.
 */
#define	SetVal(type,sz,value)					\
	{							\
	if((to->size > 0) && (to->addr != NULL)){		\
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
		*((type *)(to->addr)) = value;			\
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
		to->addr = &val;				\
		return(ret);					\
	}							\
	}

/*
 * Function:	NhlCvtStringToFloat
 *
 * Description:	This is a type converter to convert string's to floats.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToFloat
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	float tmp;
	NhlErrorTypes ret = NhlNOERROR;

	if(nargs != 0){
		/*ERROR*/
		ret = NhlWARNING;
	}

	tmp = (float)strtod((char *)from->addr,(char**)NULL);

	SetVal(float,sizeof(float),tmp);
}

/*
 * Function:	NhlCvtStringToInteger
 *
 * Description:	This is a type converter to convert string's to int's.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToInteger
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	int tmp;
	NhlErrorTypes ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtStringToInteger Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	tmp = (int)strtol((char *)from->addr,(char**)NULL,10);

	SetVal(int,sizeof(int),tmp);
}

/*
 * Function:	comparestring
 *
 * Description:	This function compares two strings - It treats uppercase and
 *		lower case the same.  If the first string is lexically greater
 *		than the second it returns a pos num if it is less it returns
 *		a neg number.  If the strings a lexically equal it returns 0.
 *		The comparison is only done for the length of the first string.
 *		If the second string is longer then the first they are not
 *		equal, unless the only thing left in the second string is
 *		white space - in that case they will be considered equal.
 *		This makes it possible to have trailing white space in the
 *		resource file.
 *
 * In Args:	char	*s1	string one
 *		char	*s2	string two
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	int
 * Side Effect:	
 */
static int
comparestring
#if	__STDC__
(
	char	*s1,	/* string one	*/
	char	*s2	/* string two	*/
)
#else
(s1,s2)
	char	*s1;	/* string one	*/
	char	*s2;	/* string two	*/
#endif
{
	char	*ptr1 = s1;
	char	*ptr2 = s2;
	int	len1 = strlen(s1);
	int	len2 = strlen(s2);
	int	i;
	int	c1, c2;

	/*
	 * If the first string is longer than the second return 1 - the
	 * resource file string isn't the same.
	 */
	if(len1 > len2)
		return 1;

	/*
	 * If the second string is longer that the first, check to make sure
	 * the difference isn't just white space.  If it isn't white space
	 * return -1.
	 */
	if(len2 > len1){
		for(i=len1;i<len2;i++){
			if(isspace((int)s2[i]))
				continue;
			return -1;
		}
	}

	/*
	 * The strings are the same length - are they the same value?
	 * Check each charactor - if they are not the same return -
	 * if we get all the way threw the string then they are equal.
	 */

	for(i=0;i < len1; i++){
		c1 = tolower((int)*ptr1);
		c2 = tolower((int)*ptr2);

		if(c1 == c2){
			ptr1++;
			ptr2++;
		}
		else if(c1 < c2)
			return (-1);
		else
			return (1);
	}

	return 0;
}

/*
 * Function:	NhlCvtStringToEnum
 *
 * Description:	This is a type converter to convert string's to enumerations
 *		defined by the args. This function uses the NrmValue structure
 *		in the args, in a slightly non-standard way.  The size
 *		part actually indicates the value that should be used if the
 *		string pointed to by addr is the same as the string being
 *		converted.  The addr should be a null terminated string.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
NhlErrorTypes
NhlCvtStringToEnum
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	int		i, tmp=0;
	NhlBoolean	set = False;
	NhlString	s1 = (NhlString)from->addr;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtStringToEnum Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(comparestring((char*)args[i].addr,s1) == 0){
			tmp = args[i].size;
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"NhlCvtStringToEnum: Unable to convert string \"%s\" to requested type",
							(char*)from->addr);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),tmp);
}

/*
 * Function:	NhlCvtStringToChar
 *
 * Description:	This function is used to convert a string to a char.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToChar
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	char		tmp;
	NhlString	s1 = (char*)from->addr;
	int		len = strlen(s1);
	int		i;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtStringToChar Called with improper number of args");
		return NhlFATAL;
	}

	if(len > 1){
		for(i=len-1;i > 0;i--){
			if(isspace((int)s1[i]))
				continue;
			NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtStringToChar called with a string length unequal to 1");
			to->size = 0;
			return NhlFATAL;
		}
	}

	tmp = *(char *)(from->addr);

	SetVal(char,sizeof(char),tmp);
}

/*
 * Function:	NhlCvtIntToBool
 *
 * Description:	This function is used to convert an int to an NhlBoolean.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtIntToBool
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	int		tmp;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtIntToBool Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	tmp = (int)from->addr;

	if(tmp){
		SetVal(NhlBoolean,sizeof(NhlBoolean),True);
	}
	else{
		SetVal(NhlBoolean,sizeof(NhlBoolean),False);
	}
}

/*
 * Function:	NhlCvtFloatToBool
 *
 * Description:	This function is used to convert an float to an NhlBoolean.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtFloatToBool
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	float		tfloat;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtFloatToBool Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	tfloat = *(float*)&from->addr;

	if(tfloat == 0.0){
		SetVal(NhlBoolean,sizeof(NhlBoolean),False);
	}
	else{
		SetVal(NhlBoolean,sizeof(NhlBoolean),True);
	}
}

/*
 * Function:	NhlCvtFloatToInt
 *
 * Description:	This function is used to convert an float to an int.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtFloatToInt
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	float		tfloat;
	int		tint;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtFloatToInt Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	tfloat = *(float*)&(from->addr);
	tint = (int)tfloat;

	if(tfloat != (float)tint){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"NhlCvtFloatToInt:Float to Int conversion loosing information");
		to->size = 0;
	}

	SetVal(int,sizeof(int),tint);
}

/*
 * Function:	NhlCvtIntToFloat
 *
 * Description:	This function is used to convert an int to a float.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtIntToFloat
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	float		tfloat;
	int		tint;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtIntToFloat Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	tint = (int)from->addr;
	tfloat = (float)tint;

	SetVal(float,sizeof(float),tfloat);
}

/*
 * Function:	NhlCvtIntToGen
 *
 * Description:	This function is used to convert an int to a gen array.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtIntToGen
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	int		*iptr;
	NhlGenArray	newgen = NULL;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtIntToGen Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	newgen = NhlConvertMalloc(sizeof(NhlGenArrayRec));
	iptr = NhlConvertMalloc(sizeof(int));
	if((newgen == NULL) || (iptr == NULL)){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}

	newgen->num_dimensions = 1;
	newgen->len_dimensions = &newgen->num_elements;
	newgen->num_elements = 1;
	newgen->typeQ = NrmStringToQuark(NhlTInteger);
	newgen->size = sizeof(int);
	*iptr = (int)from->addr;
	newgen->data = iptr;
	newgen->my_data = False;	/* data belongs to convert context */

	SetVal(NhlGenArray,sizeof(NhlGenArray),newgen);
}

/*
 * Function:	NhlCvtFloatToGen
 *
 * Description:	This function is used to convert an int to a gen array.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtFloatToGen
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	float		*fptr;
	NhlGenArray	newgen = NULL;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtIntToGen Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	newgen = NhlConvertMalloc(sizeof(NhlGenArrayRec));
	fptr = NhlConvertMalloc(sizeof(float));
	if((newgen == NULL) || (fptr == NULL)){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}

	newgen->num_dimensions = 1;
	newgen->len_dimensions = &newgen->num_elements;
	newgen->num_elements = 1;
	newgen->typeQ = NrmStringToQuark(NhlTFloat);
	newgen->size = sizeof(float);
	*fptr = *(float*)&from->addr;
	newgen->data = fptr;
	newgen->my_data = False;	/* data belongs to convert context */

	SetVal(NhlGenArray,sizeof(NhlGenArray),newgen);
}

/*
 * Function:	NhlCvtIntToEnum
 *
 * Description:	This function is used to convert an int to an enum.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
NhlErrorTypes
NhlCvtIntToEnum
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	int		i, tmp = (int)from->addr;
	NhlBoolean	set = False;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtIntToEnum Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(tmp == (int)args->addr){
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"NhlCvtIntToEnum: Unable to convert int \"%d\" to requested type",tmp);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),tmp);
}

/*
 * Function:	NhlCvtFloatToEnum
 *
 * Description:	This function is used to convert a float to an enum.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	add'n args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
NhlErrorTypes
NhlCvtFloatToEnum
#if	__STDC__
(
	NrmValue		*from,	/* ptr to from data		*/
	NrmValue		*to,	/* ptr to to data		*/
 	NhlConvertArgList	args,	/* add'n args for conversion	*/
	int			nargs	/* number of args		*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data		*/
	NrmValue		*to;	/* ptr to to data		*/
 	NhlConvertArgList	args;	/* add'n args for conversion	*/
	int			nargs;	/* number of args		*/
#endif
{
	int		i, itmp;
	float		ftmp = *(float *)&from->addr;
	NhlBoolean	set = False;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtFloatToEnum Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	itmp = (int)ftmp;

	if(ftmp != (float)itmp){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"NhlCvtFloatToEnum:float \"%f\" not directly convertable to Enum",ftmp);
		to->size = 0;
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(itmp == (int)args->addr){
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"NhlCvtFloatToEnum:Unable to convert float \"%f\" to requested type",
							*(float*)&from->addr);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),itmp);
}

/*
 * Function:	_NhlConvertersInitialize
 *
 * Description:	This function is used to initialize the Quark's that will be
 *		used later, and to install the default type converters.
 *
 * In Args:	void
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	void
 * Side Effect:	
 */
void
_NhlConvertersInitialize
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlConvertArg	BoolEnumList[] = {
			{NhlSTRENUM,	True,	"true"},
			{NhlSTRENUM,	False,	"false"},
			{NhlSTRENUM,	True,	"yes"},
			{NhlSTRENUM,	False,	"no"},
			{NhlSTRENUM,	True,	"on"},
			{NhlSTRENUM,	False,	"off"},
			{NhlSTRENUM,	True,	"1"},
			{NhlSTRENUM,	False,	"0"}
			};

	NhlConvertArg	FontEnumList[] = {
			{NhlSTRENUM,	0,	"0"},
			{NhlSTRENUM,	0,	"pwritx"},
			{NhlSTRENUM,	1,	"1"},
			{NhlSTRENUM,	1,	"default"},
			{NhlSTRENUM,	2,	"2"},
			{NhlSTRENUM,	2,	"cartographic_roman"},
			{NhlSTRENUM,	3,	"3"},
			{NhlSTRENUM,	3,	"cartographic_greek"},
			{NhlSTRENUM,	4,	"4"},
			{NhlSTRENUM,	4,	"simplex_roman"},
			{NhlSTRENUM,	5,	"5"},
			{NhlSTRENUM,	5,	"simplex_greek"},
			{NhlSTRENUM,	6,	"6"},
			{NhlSTRENUM,	6,	"simplex_script"},
			{NhlSTRENUM,	7,	"7"},
			{NhlSTRENUM,	7,	"complex_roman"},
			{NhlSTRENUM,	8,	"8"},
			{NhlSTRENUM,	8,	"complex_greek"},
			{NhlSTRENUM,	9,	"9"},
			{NhlSTRENUM,	9,	"complex_script"},
			{NhlSTRENUM,	10,	"10"},
			{NhlSTRENUM,	10,	"complex_italic"},
			{NhlSTRENUM,	11,	"11"},
			{NhlSTRENUM,	11,	"complex_cyrillic"},
			{NhlSTRENUM,	12,	"12"},
			{NhlSTRENUM,	12,	"duplex_roman"},
			{NhlSTRENUM,	13,	"13"},
			{NhlSTRENUM,	13,	"triplex_roman"},
			{NhlSTRENUM,	14,	"14"},
			{NhlSTRENUM,	14,	"triplex_italic"},
			{NhlSTRENUM,	15,	"15"},
			{NhlSTRENUM,	15,	"gothic_german"},
			{NhlSTRENUM,	16,	"16"},
			{NhlSTRENUM,	16,	"gothic_english"},
			{NhlSTRENUM,	17,	"17"},
			{NhlSTRENUM,	17,	"gothic_italian"},
			{NhlSTRENUM,	18,	"18"},
			{NhlSTRENUM,	18,	"math_symbols"},
			{NhlSTRENUM,	19,	"19"},
			{NhlSTRENUM,	19,	"symbol_set1"},
			{NhlSTRENUM,	20,	"20"},
			{NhlSTRENUM,	20,	"symbol_set2"},
			{NhlSTRENUM,	21,	"21"},
			{NhlSTRENUM,	21,	"helvetica"},
			{NhlSTRENUM,	22,	"22"},
			{NhlSTRENUM,	22,	"helvetica-bold"},
			{NhlSTRENUM,	25,	"25"},
			{NhlSTRENUM,	25,	"times-roman"},
			{NhlSTRENUM,	26,	"26"},
			{NhlSTRENUM,	26,	"times-bold"},
			{NhlSTRENUM,	29,	"29"},
			{NhlSTRENUM,	29,	"courier"},
			{NhlSTRENUM,	30,	"30"},
			{NhlSTRENUM,	30,	"courier-bold"},
			{NhlSTRENUM,	33,	"33"},
			{NhlSTRENUM,	33,	"greek"},
			{NhlSTRENUM,	34,	"34"},
			{NhlSTRENUM,	34,	"math-symbols"},
			{NhlSTRENUM,	35,	"35"},
			{NhlSTRENUM,	35,	"text-symbols"},
			{NhlSTRENUM,	36,	"36"},
			{NhlSTRENUM,	36,	"weather1"},
			{NhlSTRENUM,	37,	"37"},
			{NhlSTRENUM,	37,	"weather2"}
			};

	NhlConvertArg	FontIntEnumList[] = {
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)0},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)1},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)2},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)3},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)4},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)5},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)6},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)7},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)8},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)9},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)10},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)11},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)12},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)13},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)14},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)15},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)16},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)17},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)18},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)19},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)20},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)21},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)22},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)25},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)26},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)29},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)30},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)33},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)34},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)35},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)36},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)37}
			};

	(void)NhlRegisterConverter(NhlTString,NhlTFloat,NhlCvtStringToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTInteger,NhlCvtStringToInteger,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTBoolean,NhlCvtStringToEnum,
			BoolEnumList,NhlNumber(BoolEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTCharacter,NhlCvtStringToChar,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTFont,NhlCvtStringToEnum,
			FontEnumList,NhlNumber(FontEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTFont,NhlCvtIntToEnum,
			FontIntEnumList,NhlNumber(FontIntEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTFont,NhlCvtFloatToEnum,
			FontIntEnumList,NhlNumber(FontIntEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTBoolean,NhlCvtIntToBool,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTBoolean,NhlCvtFloatToBool,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTInteger,NhlCvtFloatToInt,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTFloat,NhlCvtIntToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTGenArray,NhlCvtIntToGen,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTGenArray,NhlCvtFloatToGen,
							NULL,0,False,NULL);
	return;
}
