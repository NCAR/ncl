/*
 *      $Id: Converters.c,v 1.33 1995-03-20 19:58:38 boote Exp $
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
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/ConvertersP.h>
#include <math.h>

#if	defined(SunOs) && (MAJOR == 4)
#include <floatingpoint.h>
#endif	/* sun hack- strtod should be in stdlib.h but it's not */

#define MAX_DIMENSIONS 50
typedef enum { CHAR, LP, SPACE , RP , COMMA, ENDOFSTRING} _NhlTokens;

static NrmQuark intQ;
static NrmQuark stringQ;
static NrmQuark genQ;
static NrmQuark intgenQ;
static NrmQuark strgenQ;


static _NhlTokens
NextToken
#if	NhlNeedProto
(
	Const char	*strng,
	int		*index
) 
#else
(strng,index) 
	Const char	*strng;
	int		*index;
#endif
{
	(*index)++;
	switch(strng[*index-1]) {
	case '(':
		if(strng[*index] == '/') {
			(*index)++;
			return(LP);
		} else {
			return(CHAR);
		}
	case '/':
		if(strng[*index] == ')') {
			(*index)++;
			return(RP);
		} else {
			return(CHAR);
		}
	case ' ':
	case '\t':
		return(SPACE);
	case ',':
		return(COMMA);
	case '\0':
		return(ENDOFSTRING);
	default:
		return(CHAR);
	}
}


static NhlGenArray
_NhlStringToStringGenArray
#if	NhlNeedProto
(
	Const char*	strng
)
#else
(strng)
	Const char*	strng;
#endif
{
	int done = 0;
	int state = 1;
	int index = 0;
	int d_o_index = 0;
	int d_p_index = 0;
	_NhlTokens token;
	int i,j;
	char *data_out;
	char **data_ptr;
	char space_buffer[80];
	int s_p_index = 0;
	int level_count[MAX_DIMENSIONS];
	int dimsizes[MAX_DIMENSIONS];
	int level = -1;
	
	
	while(!done) {
		token = NextToken(strng,&index);
		switch(state) {
		case 0:
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"Syntax error parsing resource file array");
			done = 1;
			break;
		case 1:
			switch(token) {
			case LP:
				for(i = 0; i < MAX_DIMENSIONS; i++) {
					dimsizes[i] = -1;
					level_count[i] = 0;
				}
				data_out = NhlConvertMalloc(strlen(strng) + 1);
				data_ptr = NhlConvertMalloc(sizeof(char*) *
								strlen(strng));
				memset(data_ptr,0,sizeof(char*)*strlen(strng));
				level++;
				state = 2;
				if(level >= MAX_DIMENSIONS){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
	"Maximum dimensions of (%d) for resource file arrays was exceeded",
								MAX_DIMENSIONS);
					state = 0;
				}
				break;
			default:
				return(NULL);
				break;
			}
			break;
		case 2:
			switch(token) {
			case CHAR:
				data_ptr[d_p_index++] = &(data_out[d_o_index]);
				data_out[d_o_index++] = strng[index-1];
				s_p_index = 0;
				state = 4;
				break;
			case SPACE:
				break;
			case LP:
				s_p_index = 0;
				level++;
				if(level >= MAX_DIMENSIONS){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
	"Maximum dimensions of (%d) for resource file arrays was exceeded",
								MAX_DIMENSIONS);
					state = 0;
				}
				break;
			default:
				state = 0;
				break;
			}
			break;
		case 3:
			switch(token) {
			case CHAR:
				data_ptr[d_p_index++] = &(data_out[d_o_index]);
				data_out[d_o_index++] = strng[index-1];
				state = 4;
				break;
			case SPACE:
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 4:
			switch(token) {
			case CHAR:
				for(i = 0; i < s_p_index; i++ ) {
					data_out[d_o_index++] = space_buffer[i];
				}
				s_p_index = 0;
				data_out[d_o_index++] = strng[index-1];
				break;
			case SPACE:
				space_buffer[s_p_index++] = strng[index-1];
				break;
			case RP:
				if(level >= 0) {
					data_out[d_o_index++] = '\0';
					level_count[level]++;
					s_p_index = 0;
					state = 5;
					if(dimsizes[level] == -1) {
						dimsizes[level] =
							level_count[level];
						level_count[level] = 0;
					} else if(dimsizes[level] !=
							level_count[level]) {
						NhlPError(NhlWARNING,
							NhlEUNKNOWN,
			"Dimension sizes of resource file array do not match");
						state = 0;
					} else {
						level_count[level] = 0;
					}
					level--;
					if(level >= 0)
						level_count[level]++;
				} else {
					state = 0;
				}
				break;
			case COMMA:
				data_out[d_o_index++] = '\0';
				level_count[level]++;
				s_p_index = 0;
				state = 3;
				break;
			default:
                                state = 0;
                                break;
			}
			break;
		case 5:
			switch(token) {
			case SPACE:
				break;
			case COMMA:
				state = 6;
				break;
			case ENDOFSTRING:
				state = 7;
				break;
			case RP:
				if(level >=0 ) {
					if(dimsizes[level] == -1) {
                                        	dimsizes[level] =
							level_count[level];
						level_count[level] = 0;
                                	} else if(dimsizes[level] !=
							level_count[level]) {
						NhlPError(NhlWARNING,
							NhlEUNKNOWN,
			"Dimension sizes of resource file array do not match");
                                        	state = 0;
                                	} else {
						level_count[level] = 0;
					}
                              		level--;
					if(level >= 0)
						level_count[level]++;
				} else {
					state = 0;
				}
				break;
			default:
                                state = 0;
                                break;
			}
			break;
		case 6:
			switch(token) {
			case LP:
				level++;
				state = 2;
				break;
			case SPACE:
				break;
			default:
                                state = 0;
                                break;
			}
			break;
		case 7:
			done = 1;
			break;
		}
	}


	if((state == 7)&&(level == -1)) {
		i = 0;
		while(dimsizes[i] != -1) i++;
		return _NhlConvertCreateGenArray(data_ptr,NhlTString,
						sizeof(char*),i,dimsizes);
	} else if((state != 0)&&(level >= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"Syntax error parsing resource file array");
		return(NULL);
	} else {
		return(NULL);
	}
}

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
#if	NhlNeedProto
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
static NhlErrorTypes
NhlCvtStringToEnum
#if	NhlNeedProto
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
	char		func[] = "NhlCvtStringToEnum";
	int		i, tmp=0;
	NhlBoolean	set = False;
	NhlString	s1 = from->data.strval;
	NhlString	t2 = NULL;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	if(isdigit((int)*s1) || (*s1 == '-')){
		tmp = (int)strtol(s1,&t2,10);
		if(!tmp && (s1 == t2)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't Convert \"%s\"",func,s1);
			to->size = 0;
			return NhlFATAL;
		}
		val.size = sizeof(int);
		val.data.intval = tmp;

		return _NhlReConvertData(intQ,to->typeQ,&val,to);
	}

	sgen = _NhlStringToStringGenArray(s1);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;

		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
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
			"%s: Unable to convert string \"%s\" to requested type",
								func,s1);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),tmp);
}

/*
 * Function:	NhlCvtEnumToString
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
NhlCvtEnumToString
#if	NhlNeedProto
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
	char		func[] = "NhlCvtEnumToString";
	int		i;
	NhlString	tstring = NULL;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
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
			"%s: Invalid Enum \"%d\"",func,from->data.intval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*
 * Function:	NhlCvtScalarToEnum
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
NhlCvtScalarToEnum
#if	NhlNeedProto
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
	char		func[] = "NhlCvtScalarToEnum";
	int		i,tint;
	NhlBoolean	set = False;
	NrmValue	ival;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	ival.size = sizeof(int);
	ival.data.ptrval = &tint;
	if(_NhlReConvertData(from->typeQ,intQ,from,&ival) < NhlWARNING){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Unable to convert from %s to %s",func,
				NrmQuarkToString(from->typeQ),NhlTInteger);
		return NhlFATAL;
	}

	for(i=0;i<nargs;i++){
		if(tint == args[i].data.intval){
			set = True;
			break;
		}
	}

	if(!set){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Unable to convert from %s to %s,\n\t%d is not a valid value for %s",
			func,
			NrmQuarkToString(from->typeQ),
			NrmQuarkToString(to->typeQ),
			tint,
			NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),tint);
}

/*
 * Function:	NhlCvtStringGenArrayToEnumGenArray
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
static NhlErrorTypes
NhlCvtStringGenArrayToEnumGenArray
#if	NhlNeedProto
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
	char		func[] = "NhlCvtStringGenArrayToEnumGenArray";
	int		i,j,tmp=0;
	NhlBoolean	set = False;
	NhlString	*sdata;
	NhlString	s1,s2;
	NhlGenArray	sgen = from->data.ptrval;
	NhlGenArray	tgen;
	int		*tint;
	NhlErrorTypes	ret = NhlNOERROR,lret = NhlNOERROR;
	char		enum_name[_NhlMAXRESNAMLEN];
	NrmQuark	enumQ;
	NrmValue	ival,eval;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	if(!sgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),sgen);
	}
	sdata = sgen->data;

	/*
	 * What is the name for a single element of the to GenArray?
	 */
	s1 = NrmQuarkToString(to->typeQ);
	strcpy(enum_name,s1);
	s1 = strstr(enum_name,NhlTGenArray);
	if(!s1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid \"to\" type %s ???",
			func,NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}
	*s1 = '\0';
	enumQ = NrmStringToQuark(enum_name);

	tgen = _NhlConvertCreateGenArray(NULL,enum_name,sizeof(int),
				sgen->num_dimensions,sgen->len_dimensions);
	if(!tgen){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}
	tint = NhlConvertMalloc(sizeof(int) * sgen->num_elements);
	if(!tint){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}

	tgen->data = tint;

	for(i=0;i < sgen->num_elements;i++){
		s1 = sdata[i];
		set = False;

		if(isdigit((int)*s1) || (*s1 == '-')){
			tmp = (int)strtol(s1,&s2,10);
			ival.size = sizeof(int);
			ival.data.intval = tmp;
			eval.size = sizeof(int);
			eval.data.ptrval = &tint[i];

			if(tmp || (s1 != s2)){
				lret =_NhlReConvertData(intQ,enumQ,&ival,&eval);
				if(lret == NhlNOERROR)
					set = True;
			}
		}
		else{
			for(j=0;j<nargs;j++){
				if(comparestring(args[j].data.strval,s1) == 0){
					tint[i] = args[j].size;
					set = True;
					break;
				}
			}
		}

		if(!set){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s: Unable to convert string \"%s\" to requested type",
								func,s1);
			to->size = 0;
			return NhlFATAL;
		}
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
}

/*
 * Function:	NhlCvtGenArrayToEnumGenArray
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
NhlCvtGenArrayToEnumGenArray
#if	NhlNeedProto
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
	char		func[] = "NhlCvtGenArrayToEnumGenArray";
	int		i,j;
	NhlGenArray	tgen;
	int		*tdata;
	NhlBoolean	set = False;
	NrmValue	ival;
	char		buff[_NhlMAXRESNAMLEN];
	char		*enumgen_name;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	tgen = from->data.ptrval;
	if(!tgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
	}

	if(tgen->typeQ == stringQ){
		return _NhlReConvertData(strgenQ,to->typeQ,from,to);
	}

	ival.size = sizeof(NhlGenArray);
	ival.data.ptrval = &tgen;
	if(_NhlReConvertData(from->typeQ,intgenQ,from,&ival) < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to convert from %s to %s",func,
			NrmQuarkToString(from->typeQ),NhlTIntegerGenArray);
		return NhlFATAL;
	}

	tdata = (int*)tgen->data;

	for(i=0;i<tgen->num_elements;i++){
		set = False;

		for(j=0;j<nargs;j++){
			if(tdata[i] == args[j].data.intval){
				set = True;
				break;
			}
		}

		if(!set){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Invalid value converting from %s to %s",
				func,NrmQuarkToString(from->typeQ),
				NrmQuarkToString(to->typeQ));
			return NhlFATAL;
		}
	}

	enumgen_name = NrmQuarkToString(to->typeQ);
	strcpy(buff,enumgen_name);
	enumgen_name = strstr(buff,NhlTGenArray);
	if(!enumgen_name){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid \"to\" type %s ???",
			func,NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}
	*enumgen_name = '\0';
	tgen->typeQ = NrmStringToQuark(buff);

	SetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
}

/*
 * Function:	_NhlRegisterEnumType
 *
 * Description:	This function is used to register an enumeration type as
 *		part of the type hierarchy.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlErrorTypes
_NhlRegisterEnumType
#if	NhlNeedProto
(
	NhlString	enum_name,
	_NhlEnumVals	*enum_vals,
	int		nvals
)
#else
(enum_name,enum_vals,nvals)
	NhlString	enum_name;
	_NhlEnumVals	*enum_vals;
	int		nvals;
#endif
{
	char		func[] = "_NhlRegisterEnumType";
	NhlConvertArg	args[_NhlSTACK_ARGS_SIZE];
	int		i;
	char		enumgen_name[_NhlMAXRESNAMLEN];

	if(nvals > _NhlSTACK_ARGS_SIZE){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"%s:Unable to register enum, increase _NhlSTACK_ARGS_SIZE to %d",func,
									nvals);
		return NhlFATAL;
	}

	if(_NhlRegisterType(NhlTEnum,enum_name) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}

	strcpy(enumgen_name,enum_name);
	strcat(enumgen_name,NhlTGenArray);
	if(_NhlRegisterType(NhlTEnumGenArray,enumgen_name) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}

	for(i=0;i<nvals;i++){
		args[i].addressmode = NhlSTRENUM;
		args[i].size = enum_vals[i].value;
		args[i].data.strval = enum_vals[i].name;
	}
	if(NhlRegisterConverter(NhlTString,enum_name,NhlCvtStringToEnum,args,
					nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}
	if(NhlRegisterConverter(enum_name,NhlTString,NhlCvtEnumToString,args,
					nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}
	if(NhlRegisterConverter(NhlTStringGenArray,enumgen_name,
					NhlCvtStringGenArrayToEnumGenArray,args,
					nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}

	for(i=0;i<nvals;i++){
		args[i].addressmode = NhlIMMEDIATE;
		args[i].size = sizeof(int);
		args[i].data.intval = enum_vals[i].value;
	}
	if(NhlRegisterConverter(NhlTScalar,enum_name,NhlCvtScalarToEnum,args,
					nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}
	(void)_NhlRegSymConv(NhlTScalar,enumgen_name,NhlTScalar,NhlTGenArray);
	if(NhlRegisterConverter(NhlTGenArray,enumgen_name,
					NhlCvtGenArrayToEnumGenArray,args,
					nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}

	if(_NhlRegSymConv(NhlTGenArray,enum_name,NhlTGenArray,NhlTScalar) !=
								NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}

	if(_NhlRegSymConv(NhlTQuark,enum_name,NhlTQuark,NhlTScalar) !=
								NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}
	if(_NhlRegSymConv(NhlTQuarkGenArray,enumgen_name,
				NhlTQuarkGenArray,NhlTGenArray) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
								func,enum_name);
		return NhlFATAL;
	}

	return NhlNOERROR;
}

NhlErrorTypes
_NhlCvtScalarToIndex
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
	char		func[] = "_NhlCvtScalarToIndex";
	int		tint;
	NrmValue	ival;

	if(nargs != 2){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	ival.size = sizeof(int);
	ival.data.ptrval = &tint;
	if(_NhlReConvertData(from->typeQ,intQ,from,&ival) < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert from %s to %s",func,
				NrmQuarkToString(from->typeQ),NhlTInteger);
		return NhlFATAL;
	}

	if(tint < args[0].data.intval || tint > args[1].data.intval){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Value %d is not within index range %d - %d",func,
			tint,args[0].data.intval,args[1].data.intval);
		return NhlFATAL;
	}

	if((to->size > 0) && (to->data.ptrval != NULL)){
		/* caller provided space */

		if(to->size < sizeof(int)){
			/* not large enough */
			to->size = sizeof(int);
			return NhlFATAL;
		}

		to->size = sizeof(int);
		*((int *)(to->data.ptrval)) = tint;

		return NhlNOERROR;
	}
	else{
		static int val;

		to->size = sizeof(int);
		val = tint;
		to->data.ptrval = &val;
		return NhlNOERROR;
	}
}

NhlErrorTypes
_NhlCvtGenArrayToIndexGenArray
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
	char		func[] = "_NhlCvtGenArrayToIndexGenArray";
	char		buff[_NhlMAXRESNAMLEN];
	char		*indxgen_name;
	NhlGenArray	tgen;
	int		*tint,i;
	NrmValue	ival;

	if(nargs != 2){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	ival.size = sizeof(NhlGenArray);
	ival.data.ptrval = &tgen;
	if(_NhlReConvertData(from->typeQ,intgenQ,from,&ival) < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert from %s to %s",func,
				NrmQuarkToString(from->typeQ),
				NhlTIntegerGenArray);
		return NhlFATAL;
	}

	tint = (int*)tgen->data;

	for(i=0;i < tgen->num_elements;i++){
		if(tint[i] < args[0].data.intval ||
						tint[i] > args[1].data.intval){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Value %d is not within index range %d - %d",func,
			tint[i],args[0].data.intval,args[1].data.intval);
			return NhlFATAL;
		}
	}

	indxgen_name = NrmQuarkToString(to->typeQ);
	strcpy(buff,indxgen_name);
	indxgen_name = strstr(buff,NhlTGenArray);
	if(!indxgen_name){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid \"to\" type %s ???",
			func,NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}
	*indxgen_name = '\0';
	tgen->typeQ = NrmStringToQuark(buff);

	if((to->size > 0) && (to->data.ptrval != NULL)){
		/* caller provided space */

		if(to->size < sizeof(NhlGenArray)){
			/* not large enough */
			to->size = sizeof(NhlGenArray);
			return NhlFATAL;
		}

		to->size = sizeof(NhlGenArray);
		*((NhlGenArray *)(to->data.ptrval)) = tgen;

		return NhlNOERROR;
	}
	else{
		static NhlGenArray val;

		to->size = sizeof(NhlGenArray);
		val = tgen;
		to->data.ptrval = &val;
		return NhlNOERROR;
	}
}

/************************************************************************
*									*
* This section has all the converters from Scalar values to other	*
* Scalar values.  The values currently supported are:			*
*	char								*
*	double								*
*	float								*
*	long								*
*	short								*
*	string								*
*	int								*
*	quark**								*
*									*
*	** quark is only supported as a "from" type - there are no	*
*	converters to quark.						*
*									*
************************************************************************/

#if	NhlNeedProto
#define	CvtArgs \
(									\
	NrmValue		*from,					\
	NrmValue		*to,					\
 	NhlConvertArgList	args,					\
	int			nargs					\
)
#else
#define	CvtArgs \
(from,to,args,nargs)							\
	NrmValue		*from;					\
	NrmValue		*to;					\
 	NhlConvertArgList	args;					\
	int			nargs;
#endif

#define	_ToType(fromtype,FROMTYPE,fromext,totype,TOTYPE)\
/*ARGSUSED*/								\
static NhlErrorTypes							\
NhlCvt##FROMTYPE##To##TOTYPE						\
CvtArgs									\
{									\
	totype		tempval;					\
	fromtype	t##fromext;					\
	char		func[] = "NhlCvt" #FROMTYPE "To" #TOTYPE;	\
	NhlErrorTypes	ret = NhlNOERROR;				\
									\
	if(nargs != 0){							\
		NhlPError(NhlFATAL,NhlEUNKNOWN,				\
			"%s:Called with improper number of args",func);	\
		return NhlFATAL;					\
	}								\
									\
	t##fromext = from->data.fromext##val;				\
	tempval = (totype)t##fromext;					\
									\
	if(t##fromext != (fromtype)tempval){				\
		NhlPError(NhlWARNING,NhlEUNKNOWN,			\
			"%s:" #FROMTYPE " to " #TOTYPE			\
			" conversion loosing information",func);	\
		ret = NhlWARNING;					\
	}								\
	SetVal(totype,sizeof(totype),tempval);				\
}

#define	_FromType(ftype,FTYPE,fext)\
_ToType(ftype,FTYPE,fext,char,Byte)					\
_ToType(ftype,FTYPE,fext,char,Character)				\
_ToType(ftype,FTYPE,fext,double,Double)					\
_ToType(ftype,FTYPE,fext,float,Float)					\
_ToType(ftype,FTYPE,fext,int,Integer)					\
_ToType(ftype,FTYPE,fext,long,Long)					\
_ToType(ftype,FTYPE,fext,short,Short)

/*
 * These six line create all 36 converter functions that go from each
 * of these six types to each of these six types. These are all the
 * conversions that can be done using simple casts.
 */
_FromType(char,Byte,char)
_FromType(char,Character,char)
_FromType(double,Double,dbl)
_FromType(float,Float,flt)
_FromType(int,Integer,int)
_FromType(long,Long,lng)
_FromType(short,Short,shrt)

#undef _ToType
#undef _FromType

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtByteToString
CvtArgs
{
	char		func[] = "NhlCvtByteToString";
	char		buff[_NhlMAXLINELEN];
	NhlString	tstring;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sprintf(buff,"%d",from->data.charval);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtCharacterToString
CvtArgs
{
	char		*tstr;
	char		func[] = "NhlCvtCharacterToString";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	tstr = NhlConvertMalloc(sizeof(char) * 2);

	tstr[0] = from->data.charval;
	tstr[1] = '\0';

	SetVal(NhlString,sizeof(NhlString),tstr);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtDoubleToString
CvtArgs
{
	char		func[] = "NhlCvtDoubleToString";
	char		buff[_NhlMAXLINELEN];
	NhlString	tstring;
	double		tdbl;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	tdbl = from->data.dblval;
	sprintf(buff,"%g",tdbl);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtFloatToString
CvtArgs
{
	char		func[] = "NhlCvtFloatToString";
	char		buff[_NhlMAXLINELEN];
	NhlString	tstring;
	float		tflt;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	tflt = from->data.fltval;
	sprintf(buff,"%g",tflt);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtIntegerToString
CvtArgs
{
	char		func[] = "NhlCvtIntegerToString";
	char		buff[_NhlMAXLINELEN];
	NhlString	tstring;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sprintf(buff,"%d",from->data.intval);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtLongToString
CvtArgs
{
	char		func[] = "NhlCvtLongToString";
	char		buff[_NhlMAXLINELEN];
	NhlString	tstring;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sprintf(buff,"%d",from->data.lngval);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtShortToString
CvtArgs
{
	char		func[] = "NhlCvtShortToString";
	char		buff[_NhlMAXLINELEN];
	NhlString	tstring;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sprintf(buff,"%d",from->data.shrtval);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToByte
CvtArgs
{
	char		func[] = "NhlCvtStringToByte";
	char		tmp;
	NhlString	t2=NULL;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	tmp = (char)strtol(from->data.strval,&t2,10);
	if(!tmp && (from->data.strval == t2)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
			func,from->data.strval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(char,sizeof(char),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToCharacter
CvtArgs
{
	char		func[] = "NhlCvtStringToCharacter";
	char		tmp;
	NhlString	s1 = from->data.strval;
	int		len = strlen(s1);
	int		i;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	if(len > 1){
		for(i=len-1;i > 0;i--){
			if(isspace((int)s1[i]))
				continue;
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with a string length unequal to 1",
									func);
			to->size = 0;
			return NhlFATAL;
		}
	}

	tmp = *s1;

	SetVal(char,sizeof(char),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToDouble
CvtArgs
{
	char		func[] = "NhlCvtStringToDouble";
	double		tmp;
	char		*t2=NULL;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	tmp = (double)strtod(from->data.strval,&t2);
	if(!tmp && (from->data.strval == t2)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to Convert \"%s\"",
			func,from->data.strval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(double,sizeof(double),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToFloat
CvtArgs
{
	char		func[] = "NhlCvtStringToFloat";
	float		tmp;
	char		*t2 = NULL;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	tmp = (float)strtod(from->data.strval,&t2);
	if(!tmp && (from->data.strval == t2)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to Convert \"%s\"",
			func,from->data.strval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(float,sizeof(float),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToInteger
CvtArgs
{
	char		func[] = "NhlCvtStringToInteger";
	int		tmp;
	NhlString	t2=NULL;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	tmp = (int)strtol(from->data.strval,&t2,10);
	if(!tmp && (from->data.strval == t2)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
			func,from->data.strval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(int,sizeof(int),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToLong
CvtArgs
{
	char		func[] = "NhlCvtStringToLong";
	long		tmp;
	NhlString	t2=NULL;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	tmp = strtol(from->data.strval,&t2,10);
	if(!tmp && (from->data.strval == t2)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
			func,from->data.strval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(long,sizeof(long),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToShort
CvtArgs
{
	char		func[] = "NhlCvtStringToShort";
	short		tmp;
	NhlString	t2=NULL;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	tmp = (short)strtol(from->data.strval,&t2,10);
	if(!tmp && (from->data.strval == t2)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
			func,from->data.strval);
		to->size = 0;
		return NhlFATAL;
	}

	SetVal(short,sizeof(short),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToString
CvtArgs
{
	char		func[] = "NhlCvtStringToString";
	NhlString	tstring;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	tstring = NhlConvertMalloc(sizeof(char)*(strlen(from->data.strval)+1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,from->data.strval);

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToQuark
CvtArgs
{
	char		func[] = "NhlCvtStringToQuark";
	NrmQuark	tq;
	NrmValue	val;
	NhlGenArray	sgen;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	sgen = _NhlStringToStringGenArray(from->data.strval);
	if(sgen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = sgen;
		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	tq = NrmStringToQuark(from->data.strval);

	SetVal(NrmQuark,sizeof(NrmQuark),tq);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtQuarkToScalar
CvtArgs
{
	char		func[] = "NhlCvtQuarkToScalar";
	NrmValue	sval;
	NhlString	tstring;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with wrong number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	tstring = NrmQuarkToString(from->data.intval);
	if(tstring == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to get a valid string from Quark",func);
		return NhlFATAL;
	}

	sval.data.strval = tstring;
	sval.size = strlen(tstring);

	return _NhlReConvertData(stringQ,to->typeQ,&sval,to);
}

/*
 * This converter is used to convert from a GenArray of ANY type to
 * any Scalar value.
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtGenArrayToScalar
CvtArgs
{
	char		func[] = "NhlCvtGenArrayToScalar";
	NhlGenArray	gen;
	NrmValue	val;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with wrong number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;

	if(!gen){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:%s to %s with a NULL array",
						func,NhlTGenArray,NhlTScalar);
		return NhlFATAL;
	}

	if(gen->num_elements > 1){
		NhlPError(NhlINFO,NhlEUNKNOWN,"%s:%s to %s loosing information",
						func,NhlTGenArray,NhlTScalar);
	}
	else if(gen->num_elements < 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:%s to %s with a 0 size array",func,NhlTGenArray,
								NhlTScalar);
		return NhlFATAL;
	}

	if((gen->size > sizeof(NhlArgVal)) || (gen->size < 1)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
"%s:Unable to convert from a GenArray with elements of type %s or size %d",
				func,NrmQuarkToString(gen->typeQ),gen->size);
		return NhlFATAL;
	}

	memcpy((char*)&val.data,(char*)gen->data,gen->size);
	val.size = gen->size;

	return _NhlReConvertData(gen->typeQ,to->typeQ,&val,to);
}

/*
 * This converter is used to convert from ANY scalar value to ANY GenArray.
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtScalarToGenArray
CvtArgs
{
	char		func[] = "NhlCvtScalarToGenArray";
	NhlGenArray	sgen;
	NrmValue	val;
	NhlPointer	data;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with wrong number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	if(from->typeQ == stringQ){
		sgen = _NhlStringToStringGenArray(from->data.strval);
		if(sgen){
			val.size = sizeof(NhlGenArray);
			val.data.ptrval = sgen;
			return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
		}
	}

	data = NhlConvertMalloc(from->size);
	if(data == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	if((from->typeQ == stringQ) && from->data.strval){
		NhlString	*tptr = data;
		*tptr = NhlConvertMalloc(strlen(from->data.strval)+1);
		if(!*tptr){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		strcpy(*tptr,from->data.strval);
	}
	else
		memcpy(data,&from->data,from->size);
	sgen = _NhlConvertCreateGenArray(data,NrmQuarkToString(from->typeQ),
							from->size,1,NULL);

	if(!sgen){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}

	if(to->typeQ == genQ){
		SetVal(NhlGenArray,sizeof(NhlGenArray),sgen);
	}

	val.size = sizeof(NhlGenArray);
	val.data.ptrval = sgen;

	return _NhlReConvertData(genQ,to->typeQ,&val,to);
}

/*
 * This converter is used to convert from ANY GenArray value to ANY GenArray.
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtGenArrayToGenArray
CvtArgs
{
	NhlGenArray	gen;
	char		func[] = "NhlCvtGenArrayToGenArray";
	char		buff[_NhlMAXRESNAMLEN];
	NrmQuark	newfromQ;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with wrong number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;

	/*
	 * if to GenArray, then all specialized GenArrays are valid and
	 * no conversion is really necessary.
	 */
	if(to->typeQ == genQ){
		SetVal(NhlGenArray,sizeof(NhlGenArray),gen);
	}

	/*
	 * if the from gen array is null, then it is a valid (NULL) array
	 * of any of the specific types - so just set it.
	 */
	if(!gen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),gen);
	}

	/*
	 * if from is not a GenArray, then this converter was already called
	 * to get the more specific name.  This ends the recursion.
	 */
	if(from->typeQ != genQ){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Need a converter from %s to %s",func,
						NrmQuarkToString(from->typeQ),
						NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}

	/*
	 * We need a more specific name for the from GenArray so the
	 * specific converters can be called.
	 */
	strcpy(buff,NrmQuarkToString(gen->typeQ));
	strcat(buff,NhlTGenArray);
	newfromQ = NrmStringToQuark(buff);
	/*
	 * If they are now equal, then just set.
	 */
	if(newfromQ == to->typeQ){
		SetVal(NhlGenArray,sizeof(NhlGenArray),gen);
	}
	return _NhlReConvertData(newfromQ,to->typeQ,from,to);
}

/*
 * This converter is used to convert from ANY GenArray value to the scalar
 * "variable" type.
 */
/*ARGSUSED*/
static NhlErrorTypes
NhlCvtGenArrayToVariable
CvtArgs
{
	NhlGenArray	gen;
	char		func[] = "NhlCvtGenArrayToVariable";
	NhlPointer	data;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with wrong number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	gen = from->data.ptrval;

	/*
	 * if the from gen array is null, then it is valid as is.
	 */
	if(!gen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),gen);
	}

	if(gen->num_elements > 1){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s to %s conversion loosing information",func,
			NrmQuarkToString(from->typeQ),NhlTVariable);
		ret = NhlWARNING;

		data = NhlConvertMalloc(gen->size);
		if(data == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		memcpy(data,gen->data,gen->size);
		gen = _NhlConvertCreateGenArray(data,
			NrmQuarkToString(gen->typeQ),gen->size,1,NULL);
		if(!gen){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),gen);
}

/*
 * All the specific Array types to all the other specific Array types.
 * - otherwise known as Macro Hell.
 */
#define	_ToArrType(fromtype,FROMTYPE,fromext,totype,TOTYPE)\
/*ARGSUSED*/								\
static NhlErrorTypes							\
NhlCvt##FROMTYPE##GenArrayTo##TOTYPE##GenArray				\
CvtArgs									\
{									\
	NhlGenArray	togen,fromgen;					\
	totype		*toval;						\
	fromtype	*fromval;					\
	int		i;						\
	char		func[] =					\
		"NhlCvt" #FROMTYPE "GenArrayTo" #TOTYPE "GenArray";	\
	NhlBoolean	echeck = True;					\
	NhlErrorTypes	ret = NhlNOERROR;				\
									\
	if(nargs != 0){							\
		NhlPError(NhlFATAL,NhlEUNKNOWN,				\
			"%s:Called with improper number of args",func);	\
		return NhlFATAL;					\
	}								\
									\
	fromgen = from->data.ptrval;					\
									\
	if((from->typeQ == to->typeQ) || (fromgen == NULL)){		\
		togen = fromgen;					\
	}								\
	else{								\
		fromval = fromgen->data;				\
		toval = (totype *)NhlConvertMalloc(sizeof(totype) *	\
						fromgen->num_elements);	\
		if(toval == NULL){					\
			NhlPError(NhlFATAL,ENOMEM,"%s",func);		\
			return NhlFATAL;				\
		}							\
									\
		togen = _NhlConvertCreateGenArray(toval,#TOTYPE,	\
				sizeof(totype),fromgen->num_dimensions,	\
					fromgen->len_dimensions);	\
		if(togen == NULL){					\
			NhlPError(NhlFATAL,ENOMEM,"%s",func);		\
			return NhlFATAL;				\
		}							\
									\
		for(i=0;i < fromgen->num_elements;i++){			\
			toval[i] = (totype)fromval[i];			\
			if(echeck && (fromval[i]!=(fromtype)toval[i])){	\
				NhlPError(NhlWARNING,NhlEUNKNOWN,	\
				"%s:Conversion Loosing Information",	\
								func);	\
				ret = NhlWARNING;			\
				echeck = False;				\
			}						\
		}							\
	}								\
	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);			\
}

#define	_FromArrType(ftype,FTYPE,fext)\
_ToArrType(ftype,FTYPE,fext,char,Byte)					\
_ToArrType(ftype,FTYPE,fext,char,Character)				\
_ToArrType(ftype,FTYPE,fext,double,Double)				\
_ToArrType(ftype,FTYPE,fext,float,Float)				\
_ToArrType(ftype,FTYPE,fext,int,Integer)				\
_ToArrType(ftype,FTYPE,fext,long,Long)					\
_ToArrType(ftype,FTYPE,fext,short,Short)

/*
 * These six line create all 49 converter functions that go from each
 * of these six types to each of these six types. These are all the
 * conversions that can be done using simple casts.
 */
_FromArrType(char,Byte,char)
_FromArrType(char,Character,char)
_FromArrType(double,Double,dbl)
_FromArrType(float,Float,flt)
_FromArrType(int,Integer,int)
_FromArrType(long,Long,lng)
_FromArrType(short,Short,shrt)

#undef _ToArrType
#undef _FromArrType

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtByteGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	char		*fromval;
	NhlString	*toval;
	char		func[] = "NhlCvtByteGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	int		i;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		sprintf(buff,"%d",fromval[i]);
		toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
		if(toval[i] == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		strcpy(toval[i],buff);
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtCharacterGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*toval;
	char		*fromval;
	int		i;
	char		func[] = "NhlCvtCharacterGenArrayToStringGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		char	*tchar;
		tchar = NhlConvertMalloc(sizeof(char) * 2);
		if(tchar == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		tchar[0] = fromval[i];
		tchar[1] = '\0';
		toval[i] = tchar;
	}
	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtDoubleGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	double		*fromval;
	NhlString	*toval;
	int		i;
	char		func[] = "NhlCvtDoubleGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		sprintf(buff,"%g",fromval[i]);
		toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
		if(toval[i] == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		strcpy(toval[i],buff);
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtFloatGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	float		*fromval;
	NhlString	*toval;
	int		i;
	char		func[] = "NhlCvtFloatGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		sprintf(buff,"%g",fromval[i]);
		toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
		if(toval[i] == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		strcpy(toval[i],buff);
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtIntegerGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	int		*fromval;
	NhlString	*toval;
	int		i;
	char		func[] = "NhlCvtIntegerGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		sprintf(buff,"%d",fromval[i]);
		toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
		if(toval[i] == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		strcpy(toval[i],buff);
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtLongGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	long		*fromval;
	NhlString	*toval;
	int		i;
	char		func[] = "NhlCvtLongGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		sprintf(buff,"%d",fromval[i]);
		toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
		if(toval[i] == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		strcpy(toval[i],buff);
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtShortGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	short		*fromval;
	NhlString	*toval;
	int		i;
	char		func[] = "NhlCvtShortGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		sprintf(buff,"%d",fromval[i]);
		toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
		if(toval[i] == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		strcpy(toval[i],buff);
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToByteGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	char		*toval;
	int		i;
	char		func[] = "NhlCvtStringGenArrayToByteGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (char *)NhlConvertMalloc(sizeof(char) * fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTByte,sizeof(char),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		char	*t2;

		t2=NULL;
		toval[i] = (char)strtol(fromval[i],&t2,10);
		if(!toval[i] && (fromval[i] == t2)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't Convert \"%s\"",func,fromval[i]);
			to->size = 0;
			return NhlFATAL;
		}
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToCharacterGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	char		*toval;
	int		i;
	char		func[] = "NhlCvtStringGenArrayToCharacterGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (char *)NhlConvertMalloc(sizeof(char) * fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTCharacter,sizeof(char),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		NhlString	tstring;

		tstring = fromval[i];
		if(tstring == NULL)
			toval[i] = '\0';
		else
			toval[i] = *tstring;
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToDoubleGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	double		*toval;
	int		i;
	char		func[] = "NhlCvtStringGenArrayToDoubleGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (double *)NhlConvertMalloc(sizeof(double) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTDouble,sizeof(double),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		char	*t2;

		t2=NULL;
		toval[i] = (double)strtod(fromval[i],&t2);
		if(!toval[i] && (fromval[i] == t2)){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to Convert \"%s\"",func,fromval[i]);
			to->size = 0;
			return NhlFATAL;
		}
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToFloatGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	float		*toval;
	int		i;
	char		func[] = "NhlCvtStringGenArrayToFloatGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (float *)NhlConvertMalloc(sizeof(float)*fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTFloat,sizeof(float),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		char	*t2;

		t2=NULL;
		toval[i] = (float)strtod(fromval[i],&t2);
		if(!toval[i] && (fromval[i] == t2)){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to Convert \"%s\"",func,fromval[i]);
			to->size = 0;
			return NhlFATAL;
		}
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToIntegerGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	int		*toval;
	int		i;
	char		func[] = "NhlCvtStringGenArrayToIntegerGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (int *)NhlConvertMalloc(sizeof(int) * fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTInteger,sizeof(int),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		char	*t2;

		t2=NULL;
		toval[i] = (int)strtol(fromval[i],&t2,10);
		if(!toval[i] && (fromval[i] == t2)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't Convert \"%s\"",func,fromval[i]);
			to->size = 0;
			return NhlFATAL;
		}
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToLongGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	long		*toval;
	int		i;
	char		func[] = "NhlCvtStringGenArrayToLongGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (long *)NhlConvertMalloc(sizeof(long) * fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTLong,sizeof(long),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		char	*t2;

		t2=NULL;

		toval[i] = (long)strtol(fromval[i],&t2,10);
		if(!toval[i] && (fromval[i] == t2)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't Convert \"%s\"",func,fromval[i]);
			to->size = 0;
			return NhlFATAL;
		}
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToShortGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	short		*toval;
	int		i;
	char		func[] = "NhlCvtStringGenArrayToShortGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (short *)NhlConvertMalloc(sizeof(short) *fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTShort,sizeof(short),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		char	*t2;

		t2=NULL;
		toval[i] = (short)strtol(fromval[i],&t2,10);
		if(!toval[i] && (fromval[i] == t2)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't Convert \"%s\"",func,fromval[i]);
			to->size = 0;
			return NhlFATAL;
		}
	}

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen;
	char		func[] = "NhlCvtStringGenArrayToStringGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	togen = from->data.ptrval;

	SetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtQuarkGenArrayToGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NrmQuark	*fromval;
	NhlString	*toval;
	int		i;
	char		func[] = "NhlCvtQuarkGenArrayToGenArray";
	NrmValue	sval;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:called with wrong number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;
	fromval = fromgen->data;

	if(!fromgen){
		SetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions,fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		toval[i] = NrmQuarkToString(fromval[i]);
	}

	sval.data.ptrval = togen;
	sval.size = sizeof(NhlGenArray);

	return _NhlReConvertData(NrmStringToQuark(NhlTStringGenArray),to->typeQ,
								&sval,to);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtBooleanToString
CvtArgs
{
	static char	true[] = "true";
	static char	false[] = "false";
	char		func[] = "NhlCvtBooleanToString";
	NhlString	tstring;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	if(from->data.intval)
		tstring = true;
	else
		tstring = false;

	SetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtScalarToBoolean
CvtArgs
{
	char		func[] = "NhlCvtScalarToBoolean";
	int		tint;
	NrmValue	ival;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	ival.size = sizeof(int);
	ival.data.ptrval = &tint;
	if(_NhlReConvertData(from->typeQ,intQ,from,&ival) < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to convert from %s to %s",func,
				NrmQuarkToString(from->typeQ),NhlTInteger);
		return NhlFATAL;
	}

	if(tint)
		tint = True;
	else
		tint = False;

	SetVal(int,sizeof(int),tint);
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
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlEnumVals	BoolEnumList[] = {
			{True,	"true"},
			{False,	"false"},
			{True,	"yes"},
			{False,	"no"},
			{True,	"on"},
			{False,	"off"},
			};

	_NhlEnumVals	FontEnumList[] = {
			{0,	"pwritx"},
			{1,	"default"},
			{1,	"ugly"},
			{2,	"cartographic_roman"},
			{3,	"cartographic_greek"},
			{4,	"simplex_roman"},
			{5,	"simplex_greek"},
			{6,	"simplex_script"},
			{7,	"complex_roman"},
			{8,	"complex_greek"},
			{9,	"complex_script"},
			{10,	"complex_italic"},
			{11,	"complex_cyrillic"},
			{12,	"duplex_roman"},
			{13,	"triplex_roman"},
			{14,	"triplex_italic"},
			{15,	"gothic_german"},
			{16,	"gothic_english"},
			{17,	"gothic_italian"},
			{18,	"math_symbols"},
			{19,	"symbol_set1"},
			{20,	"symbol_set2"},
			{21,	"helvetica"},
			{22,	"helvetica-bold"},
			{25,	"times-roman"},
			{26,	"times-bold"},
			{29,	"courier"},
			{30,	"courier-bold"},
			{33,	"greek"},
			{34,	"math-symbols"},
			{35,	"text-symbols"},
			{36,	"weather1"},
			{37,	"weather2"},
			};


	intQ = NrmStringToQuark(NhlTInteger);
	stringQ = NrmStringToQuark(NhlTString);
	genQ = NrmStringToQuark(NhlTGenArray);
	intgenQ = NrmStringToQuark(NhlTIntegerGenArray);
	strgenQ = NrmStringToQuark(NhlTStringGenArray);

	/*
	 * Create hierarchy
	 */
	(void)_NhlRegisterTypes(NhlTScalar,NhlTByte,NhlTCharacter,NhlTShort,
		NhlTLong,NhlTFloat,NhlTDouble,NhlTInteger,NhlTString,NhlTQuark,
								NULL);
	(void)_NhlRegisterTypes(NhlTInteger,NhlTEnum,NULL);

	(void)_NhlRegisterTypes(NhlTGenArray,NhlTByteGenArray,
		NhlTCharacterGenArray,NhlTShortGenArray,NhlTLongGenArray,
		NhlTFloatGenArray,NhlTDoubleGenArray,NhlTIntegerGenArray,
		NhlTStringGenArray,NhlTQuarkGenArray,NhlTVariable,NULL);

	(void)_NhlRegisterTypes(NhlTIntegerGenArray,NhlTEnumGenArray,NULL);


	/*
	 * Register all the converters from Scalar types to other Scalar
	 * types.
	 */
#define	_Reg(FROM,TO)\
	(void)NhlRegisterConverter(NhlT##FROM,NhlT##TO,NhlCvt##FROM##To##TO,\
							NULL,0,False,NULL);
#define _RegToAll(FROM)\
	_Reg(FROM,Byte)		\
	_Reg(FROM,Character)	\
	_Reg(FROM,Double)	\
	_Reg(FROM,Float)	\
	_Reg(FROM,Integer)	\
	_Reg(FROM,Long)		\
	_Reg(FROM,Short)	\
	_Reg(FROM,String)

	/*
	 * These 8 lines end up installing 64 converter functions.
	 */
	_RegToAll(Byte)
	_RegToAll(Character)
	_RegToAll(Double)
	_RegToAll(Float)
	_RegToAll(Integer)
	_RegToAll(Long)
	_RegToAll(Short)
	_RegToAll(String)

	(void)NhlRegisterConverter(NhlTString,NhlTQuark,NhlCvtStringToQuark,
							NULL,0,False,NULL);

	/*
	 * take care of all Quark to Scalar conversions
	 */
	(void)NhlRegisterConverter(NhlTQuark,NhlTScalar,NhlCvtQuarkToScalar,
							NULL,0,False,NULL);
	(void)_NhlRegSymConv(NhlTQuark,NhlTByte,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NhlTQuark,NhlTCharacter,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NhlTQuark,NhlTDouble,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NhlTQuark,NhlTFloat,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NhlTQuark,NhlTInteger,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NhlTQuark,NhlTLong,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NhlTQuark,NhlTShort,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NhlTQuark,NhlTString,NhlTQuark,NhlTScalar);


	/*
	 * Take care of all GenArray to Scalar Conversions
	 */
	(void)NhlRegisterConverter(NhlTGenArray,NhlTScalar,
				NhlCvtGenArrayToScalar,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTByte,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTCharacter,NhlTGenArray,
								NhlTScalar);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTDouble,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTFloat,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTInteger,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTLong,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTShort,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTString,NhlTGenArray,NhlTScalar);

	/*
	 * Take care of all Scalar to all GenArray conversions
	 */
	(void)NhlRegisterConverter(NhlTScalar,NhlTGenArray,
				NhlCvtScalarToGenArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NhlTScalar,NhlTByteGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTScalar,NhlTCharacterGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTScalar,NhlTDoubleGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTScalar,NhlTFloatGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTScalar,NhlTLongGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTScalar,NhlTShortGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTScalar,NhlTStringGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTScalar,NhlTIntegerGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTScalar,NhlTVariable,NhlTScalar,NhlTGenArray);

	/*
	 * Register all the converters from Array types to other Array
	 * types.
	 */
	(void)NhlRegisterConverter(NhlTGenArray,NhlTGenArray,
				NhlCvtGenArrayToGenArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTByteGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTCharacterGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTDoubleGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTFloatGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTLongGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTShortGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTStringGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NhlTGenArray,NhlTIntegerGenArray,NhlTGenArray,
								NhlTGenArray);

	(void)NhlRegisterConverter(NhlTGenArray,NhlTVariable,
				NhlCvtGenArrayToVariable,NULL,0,False,NULL);

#define	_RegArr(FROM,TO)\
	(void)NhlRegisterConverter(NhlT##FROM##GenArray,NhlT##TO##GenArray,\
		NhlCvt##FROM##GenArrayTo##TO##GenArray,NULL,0,False,NULL);
#define _RegArrToAll(FROM)\
	_RegArr(FROM,Byte)	\
	_RegArr(FROM,Character)	\
	_RegArr(FROM,Double)	\
	_RegArr(FROM,Float)	\
	_RegArr(FROM,Integer)	\
	_RegArr(FROM,Long)	\
	_RegArr(FROM,Short)	\
	_RegArr(FROM,String)

	/*
	 * These 8 lines end up installing 64 converter functions.
	 */
	_RegArrToAll(Byte)
	_RegArrToAll(Character)
	_RegArrToAll(Double)
	_RegArrToAll(Float)
	_RegArrToAll(Integer)
	_RegArrToAll(Long)
	_RegArrToAll(Short)
	_RegArrToAll(String)

	/*
	 * take care of all QuarkGenArray to All GenArray conversions
	 */
	(void)NhlRegisterConverter(NhlTQuarkGenArray,NhlTGenArray,
			NhlCvtQuarkGenArrayToGenArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTByteGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTCharacterGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTDoubleGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTFloatGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTIntegerGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTLongGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTShortGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTStringGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NhlTQuarkGenArray,NhlTVariable,
						NhlTQuarkGenArray,NhlTGenArray);
	/*
	 * Register enumerations.
	 */
	(void)_NhlRegisterEnumType(NhlTBoolean,BoolEnumList,
						NhlNumber(BoolEnumList));
	(void)_NhlRegisterEnumType(NhlTFont,FontEnumList,
						NhlNumber(FontEnumList));
	/*
	 * Need to over-ride some of the default "enum" converters for Boolean
	 * since all values are valid.
	 */
	(void)NhlRegisterConverter(NhlTBoolean,NhlTString,NhlCvtBooleanToString,
							NULL,0,False,NULL);
	(void)NhlRegisterConverter(NhlTScalar,NhlTBoolean,NhlCvtScalarToBoolean,
							NULL,0,False,NULL);

	return;
}
