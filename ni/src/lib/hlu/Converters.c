/*
 *      $Id: Converters.c,v 1.2 1993-10-19 17:50:03 boote Exp $
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
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Converters.h>

#if	defined(__CENTERLINE__) && defined(sun)
#include <floatingpoint.h>
#endif	/* centerline/sun hack- strtod should be in stdlib.h but it's not */

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
			return(FATAL);				\
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
NhlErrorTypes
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
	NhlErrorTypes ret = NOERROR;

	if(nargs != 0){
		/*ERROR*/
		ret = WARNING;
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
NhlErrorTypes
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
	NhlErrorTypes ret = NOERROR;

	if(nargs != 0){
		NhlPError(FATAL,E_UNKNOWN,
		"NhlCvtStringToInteger Called with improper number of args");
		return FATAL;
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
	NhlErrorTypes	ret = NOERROR;

	if(nargs < 1){
		NhlPError(FATAL,E_UNKNOWN,
		"NhlCvtStringToEnum Called with improper number of args");
		return FATAL;
	}

	for(i=0;i<nargs;i++){
		if(comparestring((char*)args[i].addr,s1) == 0){
			tmp = args[i].size;
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(FATAL,E_UNKNOWN,
	"NhlCvtStringToEnum: Unable to convert string \"%s\" to requested type",
							(char*)from->addr);
		to->size = 0;
		return FATAL;
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
NhlErrorTypes
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
	NhlErrorTypes	ret = NOERROR;

	if(nargs != 0){
		NhlPError(FATAL,E_UNKNOWN,
		"NhlCvtStringToChar Called with improper number of args");
		return FATAL;
	}

	if(len > 1){
		for(i=len-1;i > 0;i--){
			if(isspace((int)s1[i]))
				continue;
			NhlPError(FATAL,E_UNKNOWN,
		"NhlCvtStringToChar called with a string length unequal to 1");
			return FATAL;
		}
	}

	tmp = *(char *)(from->addr);

	SetVal(char,sizeof(char),tmp);
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
			{NHLSTRENUM,	True,	"true"},
			{NHLSTRENUM,	False,	"false"},
			{NHLSTRENUM,	True,	"yes"},
			{NHLSTRENUM,	False,	"no"},
			{NHLSTRENUM,	True,	"on"},
			{NHLSTRENUM,	False,	"off"},
			{NHLSTRENUM,	True,	"1"},
			{NHLSTRENUM,	False,	"0"}
			};

	(void)NhlRegisterConverter(NhlTString,NhlTFloat,NhlCvtStringToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTInteger,NhlCvtStringToInteger,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTBoolean,NhlCvtStringToEnum,
			BoolEnumList,NhlNumber(BoolEnumList),False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTCharacter,NhlCvtStringToChar,
							NULL,0,False,NULL);
	return;
}
