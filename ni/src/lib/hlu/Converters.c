/*
 *      $Id: Converters.c,v 1.1 1993-04-30 17:21:33 boote Exp $
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
#include <stdlib.h>
#include <ctype.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Convert.h>

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
 * In Args:	NrmValue	*from	ptr to from data
 *		NrmValue	*args	add'n args for conversion
 *		int		nargs	number of args
 *		
 *
 * Out Args:	NrmValue	*to	ptr to to data
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToFloat
#if	__STDC__
(
	NrmValue	*from,	/* ptr to from data	*/
	NrmValue	*to,	/* ptr to to data	*/
	NrmValue	*args,	/* add'n args for conv	*/
	int		nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue	*from;	/* ptr to from data	*/
	NrmValue	*to;	/* ptr to to data	*/
	NrmValue	*args;	/* add'n args for conv	*/
	int		nargs;	/* number of args	*/
#endif
{
	float tmp;
	NhlErrorTypes ret = NOERROR;

	if(nargs != 0){
		/*ERROR*/
		ret = WARNING;
	}

	tmp = (float)atof((char *)from->addr);

	SetVal(float,sizeof(float),tmp);
}

/*
 * Function:	NhlCvtStringToInteger
 *
 * Description:	This is a type converter to convert string's to int's.
 *
 * In Args:	NrmValue	*from	ptr to from data
 *		NrmValue	*args	add'n args for conversion
 *		int		nargs	number of args
 *		
 *
 * Out Args:	NrmValue	*to	ptr to to data
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToInteger
#if	__STDC__
(
	NrmValue	*from,	/* ptr to from data	*/
	NrmValue	*to,	/* ptr to to data	*/
	NrmValue	*args,	/* add'n args for conv	*/
	int		nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue	*from;	/* ptr to from data	*/
	NrmValue	*to;	/* ptr to to data	*/
	NrmValue	*args;	/* add'n args for conv	*/
	int		nargs;	/* number of args	*/
#endif
{
	int tmp;
	NhlErrorTypes ret = NOERROR;

	if(nargs != 0){
		NhlPError(FATAL,E_UNKNOWN,
		"NhlCvtStringToInteger Called with improper number of args");
		return FATAL;
	}

	tmp = (int)atoi((char *)from->addr);

	SetVal(int,sizeof(int),tmp);
}

/*
 * Function:	comparestring
 *
 * Description:	This function compares two strings - It treats uppercase and
 *		lower case the same.  If the first string is lexically greater
 *		than the first it returns a pos num if it is less it returns
 *		a neg number.  If the strings a lexically equal it returns 0.
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
	char	*ptr1, *ptr2;
	int	c1, c2;

	ptr1 = s1;
	ptr2 = s2;

	while((*ptr1 != '\0') && (*ptr2 != '\0')){
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

	c1 = tolower((int)*ptr1);
	c2 = tolower((int)*ptr2);

	if(c1 == c2)
		return 0;
	else if(c1 < c2)
		return -1;
	else
		return 1;
}



/*
 * Function:	NhlCvtStringToBoolean
 *
 * Description:	This is a type converter to convert string's to Booleans.
 *
 * In Args:	NrmValue	*from	ptr to from data
 *		NrmValue	*args	add'n args for conversion
 *		int		nargs	number of args
 *		
 *
 * Out Args:	NrmValue	*to	ptr to to data
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToBoolean
#if	__STDC__
(
	NrmValue	*from,	/* ptr to from data	*/
	NrmValue	*to,	/* ptr to to data	*/
	NrmValue	*args,	/* add'n args for conv	*/
	int		nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue	*from;	/* ptr to from data	*/
	NrmValue	*to;	/* ptr to to data	*/
	NrmValue	*args;	/* add'n args for conv	*/
	int		nargs;	/* number of args	*/
#endif
{
	NhlBoolean	tmp;
	NhlString	s1 = (NhlString)from->addr;
	NhlErrorTypes	ret = NOERROR;

	if(nargs != 0){
		NhlPError(FATAL,E_UNKNOWN,
		"NhlCvtStringToBoolean Called with improper number of args");
		return FATAL;
	}

	if( (comparestring(s1, "true") == 0)
	  ||(comparestring(s1, "yes") == 0)
	  ||(comparestring(s1, "on") == 0)
	  ||(comparestring(s1, "1") == 0))
		tmp = True;
	else if( (comparestring(s1, "false") == 0)
	       ||(comparestring(s1, "no") == 0)
	       ||(comparestring(s1, "off") == 0)
	       ||(comparestring(s1, "0") == 0))
		tmp = False;
	else
		return FATAL;

	SetVal(NhlBoolean,sizeof(NhlBoolean),tmp);
}

/*
 * Function:	NhlCvtStringToChar
 *
 * Description:	This function is used to convert a string to a char.
 *
 * In Args:	NrmValue	*from	ptr to from data
 *		NrmValue	*args	add'n args for conversion
 *		int		nargs	number of args
 *		
 *
 * Out Args:	NrmValue	*to	ptr to to data
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToChar
#if	__STDC__
(
	NrmValue	*from,	/* ptr to from data	*/
	NrmValue	*to,	/* ptr to to data	*/
	NrmValue	*args,	/* add'n args for conv	*/
	int		nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue	*from;	/* ptr to from data	*/
	NrmValue	*to;	/* ptr to to data	*/
	NrmValue	*args;	/* add'n args for conv	*/
	int		nargs;	/* number of args	*/
#endif
{
	char tmp;
	NhlErrorTypes ret = NOERROR;

	if(nargs != 0){
		NhlPError(FATAL,E_UNKNOWN,
		"NhlCvtStringToChar Called with improper number of args");
		return FATAL;
	}

	if(strlen((char*)from->addr) != 1){
		NhlPError(FATAL,E_UNKNOWN,
		"NhlCvtStringToChar called with a string length unequal to 1");
		return FATAL;
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
	(void)NhlRegisterConverter(NhlTString,NhlTFloat,NhlCvtStringToFloat,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTInteger,NhlCvtStringToInteger,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTBoolean,NhlCvtStringToBoolean,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTString,NhlTCharacter,NhlCvtStringToChar,
							NULL,0,False,NULL);
	return;
}
