/*
 *      $Id: Converters.c,v 1.13 1994-05-27 20:21:04 ethan Exp $
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
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Converters.h>
#include <math.h>

#if	defined(SunOs) && (MAJOR == 4)
#include <floatingpoint.h>
#endif	/* sun hack- strtod should be in stdlib.h but it's not */

static NrmQuark	floatQ;
static NrmQuark	intQ;
static NrmQuark	stringQ;
static NrmQuark	fontQ;
static NrmQuark	booleanQ;
static NrmQuark	quarkQ;
static NrmQuark	doubleQ;
static NrmQuark	longQ;
static NrmQuark	shortQ;

/*
 * This macro is used because most of the converters end the same way.
 */
#define	SetVal(type,sz,value)					\
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

	tmp = (float)strtod(from->data.strval,(char**)NULL);

	SetVal(float,sizeof(float),tmp);
}

/*
 * Function:	NhlCvtFloatToString
 *
 * Description:	This is a type converter to convert floats to strings.
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
NhlCvtFloatToString
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
	char		buff[_NhlMAXLINELEN];
	NhlString	tstring;
	float		tfloat;
	NhlErrorTypes ret = NhlNOERROR;

	if(nargs != 0){
		/*ERROR*/
		ret = NhlWARNING;
	}

	tfloat = from->data.fltval;
	sprintf(buff,"%g",tfloat);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	SetVal(NhlString,sizeof(NhlString),tstring);
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

	tmp = (int)strtol(from->data.strval,(char**)NULL,10);

	SetVal(int,sizeof(int),tmp);
}

/*
 * Function:	NhlCvtIntToString
 *
 * Description:	This is a type converter to convert Ints to strings.
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
NhlCvtIntToString
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
	char		buff[_NhlMAXLINELEN];
	NhlString	tstring;
	int		tint;
	NhlErrorTypes ret = NhlNOERROR;

	if(nargs != 0){
		/*ERROR*/
		ret = NhlWARNING;
	}

	tint = from->data.intval;
	sprintf(buff,"%d",tint);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	SetVal(NhlString,sizeof(NhlString),tstring);
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
 *		string pointed to by data is the same as the string being
 *		converted.  The data string should be a null terminated string.
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
NhlCvtQuarkToEnum
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
	NhlString	s1 = NrmQuarkToString(from->data.intval);
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtStringToEnum Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(comparestring(args[i].data.strval,s1) == 0){
			tmp = args[i].size;
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"NhlCvtStringToEnum: Unable to convert string \"%s\" to requested type",
									s1);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),tmp);
}
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
	NhlString	s1 = from->data.strval;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtStringToEnum Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(comparestring(args[i].data.strval,s1) == 0){
			tmp = args[i].size;
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"NhlCvtStringToEnum: Unable to convert string \"%s\" to requested type",
									s1);
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
	NhlString	s1 = from->data.strval;
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

	tmp = *s1;

	SetVal(char,sizeof(char),tmp);
}

/*
 * Function:	NhlCvtCharToString
 *
 * Description:	This function is used to convert.
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
NhlCvtCharToString
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
	NhlString	tstr;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtCharToString Called with improper number of args");
		return NhlFATAL;
	}

	tstr = NhlConvertMalloc(sizeof(char) * 2);

	tstr[0] = from->data.charval;
	tstr[1] = '\0';

	SetVal(NhlString,sizeof(NhlString),tstr);
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

	tmp = from->data.intval;

	if(tmp){
		SetVal(NhlBoolean,sizeof(NhlBoolean),True);
	}
	else{
		SetVal(NhlBoolean,sizeof(NhlBoolean),False);
	}
}

/*
 * Function:	NhlCvtBoolToInt
 *
 * Description:	This function is used to convert an NhlBoolean to an int.
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
NhlCvtBoolToInt
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
	NhlBoolean	tmp;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtBoolToInt Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	tmp = (NhlBoolean)from->data.intval;

	if(tmp){
		SetVal(int,sizeof(int),True);
	}
	else{
		SetVal(int,sizeof(int),False);
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
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtFloatToBool Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	if(from->data.fltval == 0.0){
		SetVal(NhlBoolean,sizeof(NhlBoolean),False);
	}
	else{
		SetVal(NhlBoolean,sizeof(NhlBoolean),True);
	}
}

/*
 * Function:	NhlCvtBoolToFloat
 *
 * Description:	This function is used to convert.
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
NhlCvtBoolToFloat
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
	float		tmp;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtBoolToFloat Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	tmp = from->data.intval;

	SetVal(float,sizeof(float),tmp);
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

	tfloat = from->data.fltval;
	tint = (int)tfloat;

	if(tfloat != (float)tint){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"NhlCvtFloatToInt:Float to Int conversion loosing information");
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
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtIntToFloat Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	tfloat = (float)from->data.intval;

	SetVal(float,sizeof(float),tfloat);
}
static NhlErrorTypes NhlCvtGenTo1DStringGen
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
	NhlGenArray tgen;
	NhlGenArray gen;
	char *name  = "NhlCvtGenTo1DStringGen";
	NhlErrorTypes	ret = NhlNOERROR;
	char **to_data = NULL;
	char buffer[512];
	int i;


	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;
		
	if(gen->num_dimensions != 1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with incorrect number of dimensions, can only convert single dimension GenArrays",name);
		to->size = 0;
		return NhlFATAL;
	}

	if(gen->typeQ == stringQ) {
		memcpy((void*)to->data.ptrval,(void*)&gen,to->size);
		return(NhlNOERROR);
	} else {
		if(gen->typeQ == floatQ) {
			float *from_data;
			to_data = (char**)NhlConvertMalloc(
				gen->num_elements*sizeof(char*));
			from_data = (float*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				sprintf(buffer,"%g",(float)from_data[i]);
				to_data[i] = (char*)NhlConvertMalloc(
						strlen(buffer)+1);
				strcpy(to_data[i],buffer);
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = stringQ;
			tgen->size = sizeof(char*);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == longQ) {
			long *from_data;
			to_data = (char**)NhlConvertMalloc(
				gen->num_elements*sizeof(char*));
			from_data = (long*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				sprintf(buffer,"%ld",(long)from_data[i]);
				to_data[i] = (char*)NhlConvertMalloc(
						strlen(buffer)+1);
				strcpy(to_data[i],buffer);
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = stringQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == shortQ) {
			short *from_data;
			to_data = (char**)NhlConvertMalloc(
				gen->num_elements*sizeof(char*));
			from_data = (short*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				sprintf(buffer,"%d",(short)from_data[i]);
				to_data[i] = (char*)NhlConvertMalloc(
						strlen(buffer)+1);
				strcpy(to_data[i],buffer);
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = stringQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == doubleQ) {
			double *from_data;

			to_data = (char**)NhlConvertMalloc(
				gen->num_elements*sizeof(char*));
			from_data = (double*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				sprintf(buffer,"%lg",(double)from_data[i]);
				to_data[i] = (char*)NhlConvertMalloc(
						strlen(buffer)+1);
				strcpy(to_data[i],buffer);
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = stringQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == intQ) {
			int *from_data;
			
			to_data = (char**)NhlConvertMalloc(
				gen->num_elements*sizeof(char*));
			from_data = (int*)gen->data;
			for(i = 0; i< gen->num_elements; i++) {	
				sprintf(buffer,"%d",(int)from_data[i]);
				to_data[i] = (char*)NhlConvertMalloc(
						strlen(buffer)+1);
				strcpy(to_data[i],buffer);
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = stringQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == quarkQ) {
			int *from_data;
			
			to_data = (char**)NhlConvertMalloc(
				gen->num_elements*sizeof(char*));
			from_data = (int*)gen->data;
			for(i = 0; i< gen->num_elements; i++) {	
				strcpy(buffer,NrmQuarkToString(from_data[i]));
				to_data[i] = (char*)NhlConvertMalloc(strlen(buffer)+1);
				strcpy(to_data[i],buffer);
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = stringQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Conversion for (%s) type to 1DFloatGenArray not supported",name,NrmQuarkToString(gen->typeQ));
			return NhlFATAL;
		}
		memcpy((void*)to->data.ptrval,(void*)&tgen,to->size);
		return NhlNOERROR;
	}
}
static NhlErrorTypes NhlCvtGenTo1DIntGen
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
	NhlGenArray tgen;
	NhlGenArray gen;
	char *name  = "NhlCvtGenTo1DIntGen";
	NhlErrorTypes	ret = NhlNOERROR;
	int *to_data = NULL;
	int i;


	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;
		
	if(gen->num_dimensions != 1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with incorrect number of dimensions, can only convert single dimension GenArrays",name);
		to->size = 0;
		return NhlFATAL;
	}

	if(gen->typeQ == intQ) {
		memcpy((void*)to->data.ptrval,(void*)&gen,to->size);
		return(NhlNOERROR);
	} else {
		if(gen->typeQ == floatQ) {
			float *from_data;
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Conversion may cause loss of information",name);
			to_data = (int*)NhlConvertMalloc(
				gen->num_elements*sizeof(int));
			from_data = (float*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				to_data[i] = (int)from_data[i];
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = intQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == longQ) {
			long *from_data;
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Conversion may cause loss of information",name);
			to_data = (int*)NhlConvertMalloc(
				gen->num_elements*sizeof(int));
			from_data = (long*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				to_data[i] = (int)from_data[i];
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = intQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		}else if(gen->typeQ == shortQ) {
			short *from_data;
			to_data = (int *)NhlConvertMalloc(
				gen->num_elements*sizeof(int));
			from_data = (short*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				to_data[i] = (int)from_data[i];
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = intQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		}else if(gen->typeQ == doubleQ) {
			double *from_data;

			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Conversion may cause loss of information",name);
			to_data = (int*)NhlConvertMalloc(
				gen->num_elements*sizeof(int));
			from_data = (double*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				to_data[i] = (int)from_data[i];
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = intQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		}else if(gen->typeQ == stringQ) {
			char **from_data;
			
			to_data = (int*)NhlConvertMalloc(
				gen->num_elements*sizeof(int));
			from_data = (char**)gen->data;
			for(i = 0; i< gen->num_elements; i++) {	
				to_data[i] = (int)atof(from_data[i]);
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = intQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == quarkQ) {
			int *from_data;
			
			to_data = (int*)NhlConvertMalloc(
				gen->num_elements*sizeof(int));
			from_data = (int*)gen->data;
			for(i = 0; i< gen->num_elements; i++) {	
				to_data[i] = (int)atof(NrmQuarkToString(from_data[i]));
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = intQ;
			tgen->size = sizeof(int);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Conversion for (%s) type to 1DFloatGenArray not supported",name,NrmQuarkToString(gen->typeQ));
			return NhlFATAL;
		}
		memcpy((void*)to->data.ptrval,(void*)&tgen,to->size);
		return(NhlNOERROR);
	} 
}
static NhlErrorTypes NhlCvtGenTo1DFloatGen
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
	NhlGenArray tgen;
	NhlGenArray gen;
	char *name  = "NhlCvtGenTo1DFloatGen";
	NhlErrorTypes	ret = NhlNOERROR;
	float *to_data = NULL;
	int i;


	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;
		
	if(gen->num_dimensions != 1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with incorrect number of dimensions, can only convert single dimension GenArrays",name);
		to->size = 0;
		return NhlFATAL;
	}

	if(gen->typeQ == floatQ) {
		memcpy((void*)to->data.ptrval,(void*)&gen,to->size);
		return(NhlNOERROR);
	} else {
		if(gen->typeQ ==intQ) {
			int *from_data;
			to_data = (float*)NhlConvertMalloc(
				gen->num_elements*sizeof(float));
			from_data = (int*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				to_data[i] = (float)from_data[i];
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = floatQ;
			tgen->size = sizeof(float);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == longQ) {
			long *from_data;
			to_data = (float*)NhlConvertMalloc(
				gen->num_elements*sizeof(float));
			from_data = (long*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				to_data[i] = (float)from_data[i];
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = floatQ;
			tgen->size = sizeof(float);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == shortQ) {
			short *from_data;
			to_data = (float*)NhlConvertMalloc(
				gen->num_elements*sizeof(float));
			from_data = (short*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				to_data[i] = (float)from_data[i];
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = floatQ;
			tgen->size = sizeof(float);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == doubleQ) {
			double *from_data;

			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Conversion may cause loss of information",name);
			to_data = (float*)NhlConvertMalloc(
				gen->num_elements*sizeof(float));
			from_data = (double*)gen->data;
			for(i = 0; i < gen->num_elements; i++) {
				to_data[i] = (float)from_data[i];
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = floatQ;
			tgen->size = sizeof(float);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == stringQ) {
			char **from_data;
			
			to_data = (float*)NhlConvertMalloc(
				gen->num_elements*sizeof(float));
			from_data = (char**)gen->data;
			for(i = 0; i< gen->num_elements; i++) {	
				to_data[i] = (float)atof(from_data[i]);
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = floatQ;
			tgen->size = sizeof(float);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else if(gen->typeQ == quarkQ) {
			int *from_data;
			
			to_data = (float*)NhlConvertMalloc(
				gen->num_elements*sizeof(float));
			from_data = (int*)gen->data;
			for(i = 0; i< gen->num_elements; i++) {	
				to_data[i] = (float)atof(NrmQuarkToString(from_data[i]));
			}
			tgen = (NhlGenArray)NhlConvertMalloc(
				sizeof(NhlGenArrayRec));
			tgen->num_dimensions = 1;
			tgen->len_dimensions = (int*)NhlConvertMalloc(sizeof(int));
			tgen->len_dimensions[0] = gen->len_dimensions[0];
			tgen->num_elements = gen->num_elements;
			tgen->typeQ = floatQ;
			tgen->size = sizeof(float);
			tgen->data = (NhlPointer)to_data;
			tgen->my_data = True;
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Conversion for (%s) type to 1DFloatGenArray not supported",name,NrmQuarkToString(gen->typeQ));
			return NhlFATAL;
		}
		memcpy((void*)to->data.ptrval,(void*)&tgen,to->size);
		return(NhlNOERROR);
	} 
}

/*
 * Function:	NhlCvtGenToString
 *
 * Description:	This function is used to convert
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
NhlCvtGenToString
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
	NhlString	tstring;
	NhlGenArray	gen;
	char		*name = "NhlCvtGenToString";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;

	if((gen->typeQ != stringQ) && !_NhlConverterExists(gen->typeQ,stringQ)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),NhlTString);
		to->size = 0;
		return NhlFATAL;
	}

	if(gen->num_elements != 1){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Conversion loosing information",name);
		ret = NhlWARNING;
	}

	if(gen->typeQ == stringQ)
		tstring = *(NhlString*)(gen->data);
	else{
		NrmValue	fromval,toval;
		NhlErrorTypes	lret;


		toval.data.ptrval = &tstring;
		toval.size = sizeof(NhlString);
		_NhlCopyToVal((NhlPointer)gen->data,&fromval.data,gen->size);
		fromval.size = gen->size;

		lret = _NhlReConvertData(gen->typeQ,stringQ,&fromval,&toval);

		if(lret < NhlWARNING){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),NhlTString));
			return NhlFATAL;
		}
		ret = MIN(ret,lret);
	}

	SetVal(NhlString,sizeof(NhlString),tstring);
}



/*ARGSUSED*/
NhlErrorTypes
NhlCvtGenToEnum
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
	int toutput;
	NhlGenArray	gen;
	char		*name = "NhlCvtGenToEnum";
	NhlErrorTypes	ret = NhlNOERROR;
	NrmValue	fromval,toval;
	NhlErrorTypes	lret;
	NrmQuark typeQ;

	if(nargs != 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",name);
		to->size = 0;
		return NhlFATAL;
	}
	typeQ = NrmStringToQuark(args[0].data.strval);
	

	gen = from->data.ptrval;

	if((gen->typeQ != typeQ) && !_NhlConverterExists(gen->typeQ,typeQ)){
		if((gen->typeQ == quarkQ) && !_NhlConverterExists(stringQ,typeQ)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),args[0].data.strval);
			to->size = 0;
			return NhlFATAL;
		} 
	} 

	if(gen->num_elements != 1){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Conversion loosing information",name);
		ret = NhlWARNING;
	}

/*
* When it gets here either types are ident or a quark type was detected or
* some other type was detected
*/

	if(gen->typeQ == typeQ)
		toutput= *(int*)(gen->data);
	else if(gen->typeQ == quarkQ) {
		fromval.data.strval = NrmQuarkToString(*(int*)gen->data);
		fromval.size = sizeof(char*);
	} else {
		_NhlCopyToVal((NhlPointer)gen->data,&fromval.data,gen->size);
		fromval.size = gen->size;
	}

	if(gen->typeQ != typeQ) {
		if( gen->typeQ == quarkQ) {
			toval.data.ptrval = &toutput;
			toval.size = sizeof(int);

			lret = _NhlReConvertData(stringQ,typeQ,&fromval,&toval);

			if(lret < NhlWARNING){
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to convert \"%s\" to \"%s\"",name,
					NrmQuarkToString(gen->typeQ),NrmQuarkToString(typeQ)));
				return NhlFATAL;
			}
			ret = MIN(ret,lret);
		} else {
			toval.data.ptrval = &toutput;
			toval.size = sizeof(int);

			lret = _NhlReConvertData(gen->typeQ,typeQ,&fromval,&toval);

			if(lret < NhlWARNING){
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to convert \"%s\" to \"%s\"",name,
					NrmQuarkToString(gen->typeQ),NrmQuarkToString(typeQ)));
				return NhlFATAL;
			}
			ret = MIN(ret,lret);
		}
	}

	SetVal(int,sizeof(int),toutput);
}

/*
 * Function:	NhlCvtGenToInt
 *
 * Description:	This function is used to convert
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
NhlCvtGenToInt
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
	int		tint;
	NhlGenArray	gen;
	char		*name = "NhlCvtGenToInt";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;

	if((gen->typeQ != intQ) && !_NhlConverterExists(gen->typeQ,intQ)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),NhlTInteger);
		to->size = 0;
		return NhlFATAL;
	}

	if(gen->num_elements != 1){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Conversion loosing information",name);
		ret = NhlWARNING;
	}

	if(gen->typeQ == intQ)
		tint = *(int*)gen->data;
	else{
		NrmValue	fromval,toval;
		NhlErrorTypes	lret;


		toval.data.ptrval = &tint;
		toval.size = sizeof(int);
		_NhlCopyToVal((NhlPointer)gen->data,&fromval.data,gen->size);
		fromval.size = gen->size;

		lret = _NhlReConvertData(gen->typeQ,intQ,&fromval,&toval);

		if(lret < NhlWARNING){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),NhlTInteger));
			return NhlFATAL;
		}
		ret = MIN(ret,lret);
	}

	SetVal(int,sizeof(int),tint);
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
	*iptr = from->data.intval;
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
	*fptr = from->data.fltval;
	newgen->data = fptr;
	newgen->my_data = False;	/* data belongs to convert context */

	SetVal(NhlGenArray,sizeof(NhlGenArray),newgen);
}

/*
 * Function:	NhlCvtGenToFloat
 *
 * Description:	This function is used to convert
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
NhlCvtGenToFloat
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
	NhlGenArray	gen;
	char		*name = "NhlCvtGenToFloat";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;

	if((gen->typeQ != floatQ) && !_NhlConverterExists(gen->typeQ,floatQ)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
					NrmQuarkToString(gen->typeQ),NhlTFloat);
		to->size = 0;
		return NhlFATAL;
	}

	if(gen->num_elements != 1){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Conversion loosing information",name);
		ret = NhlWARNING;
	}

	if(gen->typeQ == floatQ)
		tfloat = *(float*)gen->data;
	else{
		NrmValue	fromval,toval;
		NhlErrorTypes	lret;


		toval.data.ptrval = &tfloat;
		toval.size = sizeof(float);
		_NhlCopyToVal((NhlPointer)gen->data,&fromval.data,gen->size);
		fromval.size = gen->size;

		lret = _NhlReConvertData(gen->typeQ,floatQ,&fromval,&toval);

		if(lret < NhlWARNING){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert \"%s\" to \"%s\"",name,
				NrmQuarkToString(gen->typeQ),NhlTFloat));
			return NhlFATAL;
		}
		ret = MIN(ret,lret);
	}

	SetVal(float,sizeof(float),tfloat);
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
	int		i;
	NhlBoolean	set = False;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtIntToEnum Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(from->data.intval == args[i].data.intval){
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"NhlCvtIntToEnum: Unable to convert int \"%d\" to requested type",
							from->data.intval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),from->data.intval);
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
	float		ftmp = from->data.fltval;
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
		if(itmp == args[i].data.intval){
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"NhlCvtFloatToEnum:Unable to convert float \"%f\" to requested type",
							from->data.fltval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),itmp);
}

/*
 * Function:	NhlCvtEnumToString
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
NhlCvtEnumToString
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
	int		i;
	NhlString	tstring = NULL;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlCvtEnumToString Called with improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(from->data.intval == args[i].size){
			tstring = args[i].data.strval;
			break;
		}
	}

	if(tstring == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"NhlCvtEnumToString: Invalid Enum \"%d\"",
							from->data.intval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*
 * Function:	NhlCvtEnumToInt
 *
 * Description:	This function is used to convert.
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
NhlCvtEnumToInt
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
	NhlErrorTypes	ret = NhlNOERROR;

	SetVal(int,sizeof(int),from->data.intval);
}

/*
 * Function:	NhlCvtEnumToFloat
 *
 * Description:	This function is used to convert.
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
NhlCvtEnumToFloat
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
	float		ftmp = (float)from->data.intval;
	NhlErrorTypes	ret = NhlNOERROR;

	SetVal(float,sizeof(float),ftmp);
}

/*
 * Function:	NhlCvtEnumToFStr
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
NhlErrorTypes
NhlCvtEnumToFStr
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
	int			i;
	NhlString		tstring = NULL;
	_NhlFExportString	exp;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlCvtEnumToFStr:Called w/improper number of args");
		to->size = 0;
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(from->data.intval == args[i].size){
			tstring = args[i].data.strval;
			break;
		}
	}

	if(tstring == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"NhlCvtEnumToFStr: Invalid Enum \"%d\"",
							from->data.intval);
		to->size = 0;
		return NhlFATAL;
	}

	exp = (_NhlFExportString)to->data.ptrval;

	return _NhlCstrToFstr(exp->fstring,exp->strlen,tstring);
}

/*
 * Function:	NhlCvtStringToQuark
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
NhlCvtStringToQuark
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
	NrmQuark	tmp;
	char		*name = "NhlCvtStringToQuark";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with wrong number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	tmp = NrmStringToQuark(from->data.strval);

	SetVal(NrmQuark,sizeof(NrmQuark),tmp);
}

/*
 * Function:	NhlCvtQuarkToString
 *
 * Description:	This is a type converter
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
NhlCvtQuarkToString
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
	NhlString	tstring;
	NhlErrorTypes	ret = NhlNOERROR;
	char		*name = "NhlCvtQuarkToString";

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with wrong number of args",name);
		to->size = 0;
		return NhlFATAL;
	}

	tstring = NrmQuarkToString(from->data.intval);

	SetVal(NhlString,sizeof(NhlString),tstring);
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
/*ARGSUSED*/
void
_NhlConvertersInitialize
#if	__STDC__
(
	_NhlC_OR_F	init_type
)
#else
(init_type)
	_NhlC_OR_F	init_type;
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
			{NhlSTRENUM,	0,	"pwritx"},
			{NhlSTRENUM,	0,	"0"},
			{NhlSTRENUM,	1,	"default"},
			{NhlSTRENUM,	1,	"1"},
			{NhlSTRENUM,	2,	"cartographic_roman"},
			{NhlSTRENUM,	2,	"2"},
			{NhlSTRENUM,	3,	"cartographic_greek"},
			{NhlSTRENUM,	3,	"3"},
			{NhlSTRENUM,	4,	"simplex_roman"},
			{NhlSTRENUM,	4,	"4"},
			{NhlSTRENUM,	5,	"simplex_greek"},
			{NhlSTRENUM,	5,	"5"},
			{NhlSTRENUM,	6,	"simplex_script"},
			{NhlSTRENUM,	6,	"6"},
			{NhlSTRENUM,	7,	"complex_roman"},
			{NhlSTRENUM,	7,	"7"},
			{NhlSTRENUM,	8,	"complex_greek"},
			{NhlSTRENUM,	8,	"8"},
			{NhlSTRENUM,	9,	"complex_script"},
			{NhlSTRENUM,	9,	"9"},
			{NhlSTRENUM,	10,	"complex_italic"},
			{NhlSTRENUM,	10,	"10"},
			{NhlSTRENUM,	11,	"complex_cyrillic"},
			{NhlSTRENUM,	11,	"11"},
			{NhlSTRENUM,	12,	"duplex_roman"},
			{NhlSTRENUM,	12,	"12"},
			{NhlSTRENUM,	13,	"triplex_roman"},
			{NhlSTRENUM,	13,	"13"},
			{NhlSTRENUM,	14,	"triplex_italic"},
			{NhlSTRENUM,	14,	"14"},
			{NhlSTRENUM,	15,	"gothic_german"},
			{NhlSTRENUM,	15,	"15"},
			{NhlSTRENUM,	16,	"gothic_english"},
			{NhlSTRENUM,	16,	"16"},
			{NhlSTRENUM,	17,	"gothic_italian"},
			{NhlSTRENUM,	17,	"17"},
			{NhlSTRENUM,	18,	"math_symbols"},
			{NhlSTRENUM,	18,	"18"},
			{NhlSTRENUM,	19,	"symbol_set1"},
			{NhlSTRENUM,	19,	"19"},
			{NhlSTRENUM,	20,	"symbol_set2"},
			{NhlSTRENUM,	20,	"20"},
			{NhlSTRENUM,	21,	"helvetica"},
			{NhlSTRENUM,	21,	"21"},
			{NhlSTRENUM,	22,	"helvetica-bold"},
			{NhlSTRENUM,	22,	"22"},
			{NhlSTRENUM,	25,	"times-roman"},
			{NhlSTRENUM,	25,	"25"},
			{NhlSTRENUM,	26,	"times-bold"},
			{NhlSTRENUM,	26,	"26"},
			{NhlSTRENUM,	29,	"courier"},
			{NhlSTRENUM,	29,	"29"},
			{NhlSTRENUM,	30,	"courier-bold"},
			{NhlSTRENUM,	30,	"30"},
			{NhlSTRENUM,	33,	"greek"},
			{NhlSTRENUM,	33,	"33"},
			{NhlSTRENUM,	34,	"math-symbols"},
			{NhlSTRENUM,	34,	"34"},
			{NhlSTRENUM,	35,	"text-symbols"},
			{NhlSTRENUM,	35,	"35"},
			{NhlSTRENUM,	36,	"weather1"},
			{NhlSTRENUM,	36,	"36"},
			{NhlSTRENUM,	37,	"weather2"},
			{NhlSTRENUM,	37,	"37"}
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

	NhlConvertArg   fontgentoenumdat[] = {
			{NhlIMMEDIATE, sizeof(char*),  _NhlUSET((NhlPointer)NhlTFont)}
			};
	NhlConvertArg   boolgentoenumdat[] = {
			{NhlIMMEDIATE, sizeof(char*),  _NhlUSET((NhlPointer)NhlTBoolean)}
			};

	floatQ = NrmStringToQuark(NhlTFloat);
	intQ = NrmStringToQuark(NhlTInteger);
	shortQ = NrmStringToQuark(NhlTShort);
	longQ = NrmStringToQuark(NhlTLong);
	doubleQ = NrmStringToQuark(NhlTDouble);
	stringQ = NrmStringToQuark(NhlTString);
	fontQ = NrmStringToQuark(NhlTFont);
	booleanQ = NrmStringToQuark(NhlTBoolean);
	quarkQ = NrmStringToQuark(NhlTQuark);

	(void)NhlRegisterConverter(NhlTString,NhlTFloat,NhlCvtStringToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTString,NhlCvtFloatToString,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTInteger,NhlCvtStringToInteger,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTString,NhlCvtIntToString,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTBoolean,NhlCvtStringToEnum,
			BoolEnumList,NhlNumber(BoolEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,NhlTBoolean,NhlCvtGenToEnum,
			boolgentoenumdat,1,False,NULL);

	(void)NhlRegisterConverter(NhlTBoolean,NhlTString,NhlCvtEnumToString,
			BoolEnumList,NhlNumber(BoolEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTCharacter,NhlCvtStringToChar,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTCharacter,NhlTString,NhlCvtCharToString,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTFont,NhlCvtStringToEnum,
			FontEnumList,NhlNumber(FontEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTFont,NhlTString,NhlCvtEnumToString,
			FontEnumList,NhlNumber(FontEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTFont,NhlCvtIntToEnum,
			FontIntEnumList,NhlNumber(FontIntEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTFont,NhlTInteger,NhlCvtEnumToInt,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTFont,NhlCvtFloatToEnum,
			FontIntEnumList,NhlNumber(FontIntEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTFont,NhlTFloat,NhlCvtEnumToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTBoolean,NhlCvtIntToBool,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTBoolean,NhlTInteger,NhlCvtBoolToInt,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTBoolean,NhlCvtFloatToBool,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTBoolean,NhlTFloat,NhlCvtBoolToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTInteger,NhlCvtFloatToInt,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTFloat,NhlCvtIntToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTInteger,NhlTGenArray,NhlCvtIntToGen,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTFloat,NhlTGenArray,NhlCvtFloatToGen,
							NULL,0,False,NULL);

	(void)NhlRegisterConverter(NhlTGenArray,NhlTInteger,NhlCvtGenToInt,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,NhlTFloat,NhlCvtGenToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,NhlTString,NhlCvtGenToString,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,NhlTFont,NhlCvtGenToEnum,
							fontgentoenumdat,1,False,NULL);

	(void)NhlRegisterConverter(NhlTQuark,NhlTString,NhlCvtQuarkToString,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTQuark,NhlCvtStringToQuark,
							NULL,0,False,NULL);

	(void)NhlRegisterConverter(NhlTBoolean,_NhlTFExpString,NhlCvtEnumToFStr,
			BoolEnumList,NhlNumber(BoolEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTFont,_NhlTFExpString,NhlCvtEnumToFStr,
			FontEnumList,NhlNumber(FontEnumList),False,NULL);

	(void)NhlRegisterConverter(NhlTGenArray,NhlT1DFloatGenArray,
		NhlCvtGenTo1DFloatGen,NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,NhlT1DIntGenArray,
		NhlCvtGenTo1DIntGen,NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,NhlT1DStringGenArray,
		NhlCvtGenTo1DStringGen,NULL,0,False,NULL);

	return;
}
