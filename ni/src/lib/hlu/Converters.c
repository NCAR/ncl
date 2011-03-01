/*
 *      $Id: Converters.c,v 1.58 2010-04-14 21:29:47 huangwei Exp $
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
#include <string.h>
#include <ctype.h>
#include <limits.h>
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
static NrmQuark longQ;
static NrmQuark stringQ;
static NrmQuark quarkQ;
static NrmQuark genQ;
static NrmQuark intgenQ;
static NrmQuark strgenQ;
static NrmQuark quarkgenQ;
static NrmQuark varQ;

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


NhlGenArray
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
	int i;
	char *data_out;
	char **data_ptr;
	char space_buffer[80];
	int s_p_index = 0;
	int level_count[MAX_DIMENSIONS];
	ng_size_t dimsizes[MAX_DIMENSIONS];
	int level = -1;
	

	if(strng == NULL) return(NULL);	
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
				memset((char*)data_ptr,0,
						sizeof(char*)*strlen(strng));
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
				done = 1;
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
						sizeof(char*),i, dimsizes);
	} else if((state != 0)&&(level >= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"Syntax error parsing resource file array");
		return(NULL);
	} else {
		return(NULL);
	}
}

/*
 * Function:	_NhlCmpString
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
int
_NhlCmpString
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
			NhlPError(NhlINFO,NhlEUNKNOWN,
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
		if(_NhlCmpString(args[i].data.strval,s1) == 0){
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

	_NhlSetVal(int,sizeof(int),tmp);
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
                char		buff[_NhlMAXLINELEN];
                
                sprintf(buff,"%d",from->data.intval);
                tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
                if(tstring == NULL){
                        NHLPERROR((NhlFATAL,ENOMEM,NULL));
                        to->size = 0;
                        return NhlFATAL;
                }
                strcpy(tstring,buff);
                
#if 0                
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s: Invalid Enum \"%d\"",func,from->data.intval);
		to->size = 0;
		return NhlFATAL;
#endif                
	}

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
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
		if(tint == args[i].data.lngval){
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

	_NhlSetVal(int,sizeof(int),tint);
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
	ng_size_t	i;
	int             j,tmp=0;
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
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),sgen);
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
				sgen->num_dimensions, sgen->len_dimensions);
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
				if(_NhlCmpString(args[j].data.strval,s1) == 0){
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
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
	ng_size_t	i;
	int		j;
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
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
	}

	if(tgen->typeQ == stringQ){
		return _NhlReConvertData(strgenQ,to->typeQ,from,to);
	}

	if(tgen->typeQ == quarkQ){
		return _NhlReConvertData(quarkgenQ,to->typeQ,from,to);
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
			if(tdata[i] == args[j].data.lngval){
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
}

/*
 * Function:	CompletEnumRegistration
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
NhlErrorTypes
CompleteEnumRegistration
#if	NhlNeedProto
(
	NhlClass	ref_class,
	NhlString	enum_name,
	_NhlEnumVals	*enum_vals,
	int		nvals,
        char		*func
)
#else
(ref_class,enum_name,enum_vals,nvals,func)
	NhlClass	ref_class;
	NhlString	enum_name;
	_NhlEnumVals	*enum_vals;
	int		nvals;
        char		*func;
#endif
{
	NhlConvertArg	args[_NhlSTACK_ARGS_SIZE];
	int		i;
	char		enumgen_name[_NhlMAXRESNAMLEN];
        
	strcpy(enumgen_name,enum_name);
	strcat(enumgen_name,NhlTGenArray);
	if(_NhlRegisterType(NhlTEnumGenArray,enumgen_name) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}

	for(i=0;i<nvals;i++){
		args[i].addressmode = NhlSTRENUM;
		args[i].size = enum_vals[i].value;
		args[i].data.strval = enum_vals[i].name;
	}
	if(NhlRegisterConverter(ref_class,NhlTString,enum_name,
		NhlCvtStringToEnum,args,nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}
	if(NhlRegisterConverter(ref_class,enum_name,NhlTString,
		NhlCvtEnumToString,args,nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}
	if(NhlRegisterConverter(ref_class,NhlTStringGenArray,enumgen_name,
                                NhlCvtStringGenArrayToEnumGenArray,args,
					nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}

	for(i=0;i<nvals;i++){
		args[i].addressmode = NhlIMMEDIATE;
		args[i].size = sizeof(int);
		args[i].data.lngval = enum_vals[i].value;
	}
	if(NhlRegisterConverter(ref_class,NhlTScalar,enum_name,
		NhlCvtScalarToEnum,args,nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}
	(void)_NhlRegSymConv(ref_class,NhlTScalar,enumgen_name,NhlTScalar,
								NhlTGenArray);
	if(NhlRegisterConverter(ref_class,NhlTGenArray,enumgen_name,
					NhlCvtGenArrayToEnumGenArray,args,
					nvals,False,NULL) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}

	if(_NhlRegSymConv(ref_class,NhlTGenArray,enum_name,NhlTGenArray,
                          NhlTScalar) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}

	if(_NhlRegSymConv(ref_class,NhlTQuark,
                          enum_name,NhlTQuark,NhlTScalar) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}
	if(_NhlRegSymConv(ref_class,NhlTQuarkGenArray,enumgen_name,
				NhlTQuarkGenArray,NhlTGenArray) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}

	return NhlNOERROR;
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
	NhlClass	ref_class,
	NhlString	enum_name,
	_NhlEnumVals	*enum_vals,
	int		nvals
)
#else
(ref_class,enum_name,enum_vals,nvals)
	NhlClass	ref_class;
	NhlString	enum_name;
	_NhlEnumVals	*enum_vals;
	int		nvals;
#endif
{
	char		func[] = "_NhlRegisterEnumType";

	if(nvals > _NhlSTACK_ARGS_SIZE){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"%s:Unable to register enum, increase _NhlSTACK_ARGS_SIZE to %d",
                          func,nvals);
		return NhlFATAL;
	}
        
	if(_NhlRegisterType(NhlTEnum,enum_name) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}
        return CompleteEnumRegistration
                (ref_class,enum_name,enum_vals,nvals,func);
}

/*
 * Function:	_NhlRegisterEnumSubtype
 *
 * Description:	This function is used to register an enumeration type as
 *		a subtype of another Enum.
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
_NhlRegisterEnumSubtype
#if	NhlNeedProto
(
	NhlClass	ref_class,
	NhlString	enum_name,
        NhlString	enum_supertype_name,
	_NhlEnumVals	*enum_vals,
	int		nvals
)
#else
(ref_class,enum_name,enum_supertype_name,enum_vals,nvals)
	NhlClass	ref_class;
	NhlString	enum_name;
        NhlString	enum_supertype_name;
	_NhlEnumVals	*enum_vals;
	int		nvals;
#endif
{
	char		func[] = "_NhlRegisterEnumType";

	if(nvals > _NhlSTACK_ARGS_SIZE){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"%s:Unable to register enum, increase _NhlSTACK_ARGS_SIZE to %d",
                          func,nvals);
		return NhlFATAL;
	}
        if (! _NhlIsSubtype(NhlTEnum,enum_supertype_name)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "%s: %s not a registered enum type",
                          func,enum_supertype_name);
		return NhlFATAL;
	}
                    
	if(_NhlRegisterType(enum_supertype_name,enum_name) != NhlNOERROR){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "%s:Unable to register enum %s",
                          func,enum_name);
		return NhlFATAL;
	}
        return CompleteEnumRegistration
                (ref_class,enum_name,enum_vals,nvals,func);
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
	NhlErrorTypes	ret = NhlNOERROR;

	if((nargs < 2) ||
		((args[0].data.lngval != _NhlRngMIN) &&
			(args[0].data.lngval != _NhlRngMAX) &&
			(args[0].data.lngval != _NhlRngMINMAX)) ||
		((args[0].data.lngval == _NhlRngMIN) && (nargs != 2)) ||
		((args[0].data.lngval == _NhlRngMAX) && (nargs != 2)) ||
		((args[0].data.lngval == _NhlRngMINMAX) && (nargs != 3))){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper args",func);
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

	if((args[0].data.lngval == _NhlRngMINMAX) &&
		(tint < args[1].data.lngval || tint > args[2].data.lngval)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Value %d is not within index range %d - %d",func,
			tint,args[1].data.lngval,args[2].data.lngval);
		return NhlFATAL;
	}
	else if((args[0].data.lngval == _NhlRngMIN) &&
		(tint < args[1].data.lngval)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Value %d is less than index min %d",func,tint,
			args[1].data.lngval);
		return NhlFATAL;
	}
	else if((args[0].data.lngval == _NhlRngMAX) &&
		(tint > args[1].data.lngval)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Value %d is more than index max %d",func,tint,
			args[1].data.lngval);
		return NhlFATAL;
	}

	_NhlSetVal(int,sizeof(int),tint);
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
	NhlErrorTypes	ret = NhlNOERROR;

	if((nargs < 2) ||
		((args[0].data.lngval != _NhlRngMIN) &&
			(args[0].data.lngval != _NhlRngMAX) &&
			(args[0].data.lngval != _NhlRngMINMAX)) ||
		((args[0].data.lngval == _NhlRngMIN) && (nargs != 2)) ||
		((args[0].data.lngval == _NhlRngMAX) && (nargs != 2)) ||
		((args[0].data.lngval == _NhlRngMINMAX) && (nargs != 3))){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper args",func);
		to->size = 0;
		return NhlFATAL;
	}

	tgen = from->data.ptrval;
	if(!tgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
	}

	if(tgen->typeQ == stringQ){
		return _NhlReConvertData(strgenQ,to->typeQ,from,to);
	}

	if(tgen->typeQ == quarkQ){
		return _NhlReConvertData(quarkgenQ,to->typeQ,from,to);
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

	if(args[0].data.lngval == _NhlRngMINMAX){
		for(i=0;i < tgen->num_elements;i++){
			if(tint[i] < args[1].data.lngval ||
					tint[i] > args[2].data.lngval){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Value %d is not within index range %d - %d",
				func,tint[i],args[1].data.lngval,
				args[2].data.lngval);
				return NhlFATAL;
			}
		}
	}
	else if(args[0].data.lngval == _NhlRngMIN){
		for(i=0;i < tgen->num_elements;i++){
			if(tint[i] < args[1].data.lngval){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Value %d is less than index min %d",					func,tint[i],args[1].data.lngval);
				return NhlFATAL;
			}
		}
	}
	else{
		for(i=0;i < tgen->num_elements;i++){
			if(tint[i] > args[1].data.lngval){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Value %d is more than index max %d",					func,tint[i],args[1].data.lngval);
				return NhlFATAL;
			}
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
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
		NhlPError(NhlINFO,NhlEUNKNOWN,			\
			"%s:" #FROMTYPE " to " #TOTYPE			\
			" conversion losing information",func);	\
		ret = NhlINFO;						\
	}								\
	_NhlSetVal(totype,sizeof(totype),tempval);			\
}

#define	_FromType(ftype,FTYPE,fext)\
_ToType(ftype,FTYPE,fext,unsigned char,Byte)				\
_ToType(ftype,FTYPE,fext,char,Character)				\
_ToType(ftype,FTYPE,fext,double,Double)					\
_ToType(ftype,FTYPE,fext,float,Float)					\
_ToType(ftype,FTYPE,fext,int,Integer)					\
_ToType(ftype,FTYPE,fext,long,Long)					\
_ToType(ftype,FTYPE,fext,unsigned int,Uint)				\
_ToType(ftype,FTYPE,fext,unsigned long,Ulong)				\
_ToType(ftype,FTYPE,fext,unsigned short,Ushort)				\
_ToType(ftype,FTYPE,fext,long long,Int64)				\
_ToType(ftype,FTYPE,fext,unsigned long long,Uint64)			\
_ToType(ftype,FTYPE,fext,unsigned char,Ubyte)				\
_ToType(ftype,FTYPE,fext,short,Short)

/*
 * These six line create all 36 converter functions that go from each
 * of these six types to each of these six types. These are all the
 * conversions that can be done using simple casts.
 */
_FromType(unsigned char,Byte,byte)
_FromType(char,Character,char)
_FromType(double,Double,dbl)
_FromType(float,Float,flt)
_FromType(int,Integer,int)
_FromType(long,Long,lng)
_FromType(short,Short,shrt)
_FromType(long long,Int64,int64)
_FromType(unsigned int,Uint,uint)
_FromType(unsigned long,Ulong,ulong)
_FromType(unsigned short,Ushort,ushort)
_FromType(unsigned long long,Uint64,uint64)
_FromType(unsigned char,Ubyte,ubyte)

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

	sprintf(buff,"%d",(unsigned char)from->data.byteval);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
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

	_NhlSetVal(NhlString,sizeof(NhlString),tstr);
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

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
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

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
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

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUintToString
CvtArgs
{
        char            func[] = "NhlCvtUintToString";
        char            buff[_NhlMAXLINELEN];
        NhlString       tstring;
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        sprintf(buff,"%u",from->data.uintval);
        tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
        if(tstring == NULL){
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                to->size = 0;
                return NhlFATAL;
        }
        strcpy(tstring,buff);

        _NhlSetVal(NhlString,sizeof(NhlString),tstring);
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

	sprintf(buff,"%ld",from->data.lngval);
	tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
	if(tstring == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		to->size = 0;
		return NhlFATAL;
	}
	strcpy(tstring,buff);

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUlongToString
CvtArgs
{
        char            func[] = "NhlCvtUlongToString";
        char            buff[_NhlMAXLINELEN];
        NhlString       tstring;
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        sprintf(buff,"%lu",from->data.ulongval);
        tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
        if(tstring == NULL){
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                to->size = 0;
                return NhlFATAL;
        }
        strcpy(tstring,buff);

        _NhlSetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtInt64ToString
CvtArgs
{
        char            func[] = "NhlCvtInt64ToString";
        char            buff[_NhlMAXLINELEN];
        NhlString       tstring;
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        sprintf(buff,"%lld",from->data.int64val);
        tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
        if(tstring == NULL){
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                to->size = 0;
                return NhlFATAL;
        }
        strcpy(tstring,buff);

        _NhlSetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUint64ToString
CvtArgs
{
        char            func[] = "NhlCvtUint64ToString";
        char            buff[_NhlMAXLINELEN];
        NhlString       tstring;
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        sprintf(buff,"%llu",from->data.uint64val);
        tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
        if(tstring == NULL){
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                to->size = 0;
                return NhlFATAL;
        }
        strcpy(tstring,buff);

        _NhlSetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUbyteToString
CvtArgs
{
        char            func[] = "NhlCvtUbyteToString";
        char            buff[_NhlMAXLINELEN];
        NhlString       tstring;
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        sprintf(buff,"%u",from->data.ubyteval);
        tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
        if(tstring == NULL){
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                to->size = 0;
                return NhlFATAL;
        }
        strcpy(tstring,buff);

        _NhlSetVal(NhlString,sizeof(NhlString),tstring);
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

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUshortToString
CvtArgs
{
        char            func[] = "NhlCvtUshortToString";
        char            buff[_NhlMAXLINELEN];
        NhlString       tstring;
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        sprintf(buff,"%u",from->data.ushortval);
        tstring = NhlConvertMalloc(sizeof(char) * (strlen(buff)+1));
        if(tstring == NULL){
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                to->size = 0;
                return NhlFATAL;
        }
        strcpy(tstring,buff);

        _NhlSetVal(NhlString,sizeof(NhlString),tstring);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToByte
CvtArgs
{
	char		func[] = "NhlCvtStringToByte";
	unsigned char	tmp;
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

	tmp = (unsigned char)strtol(from->data.strval,&t2,10);
	if(!tmp && (from->data.strval == t2)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
			func,from->data.strval);
		to->size = 0;
		return NhlFATAL;
	}

	_NhlSetVal(unsigned char,sizeof(unsigned char),tmp);
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

	_NhlSetVal(char,sizeof(char),tmp);
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

	_NhlSetVal(double,sizeof(double),tmp);
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

	_NhlSetVal(float,sizeof(float),tmp);
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

	_NhlSetVal(int,sizeof(int),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToUint
CvtArgs
{
        char            func[] = "NhlCvtStringToUint";
        unsigned int    tmp;
        NhlString       t2=NULL;
        NrmValue        val;
        NhlGenArray     sgen;
        NhlErrorTypes   ret = NhlNOERROR;

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

        tmp = (unsigned int)strtol(from->data.strval,&t2,10);
        if(!tmp && (from->data.strval == t2)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
                        func,from->data.strval);
                to->size = 0;
                return NhlFATAL;
        }

        _NhlSetVal(unsigned int,sizeof(unsigned int),tmp);
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

	_NhlSetVal(long,sizeof(long),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToUlong
CvtArgs
{
        char            func[] = "NhlCvtStringToUlong";
        unsigned long   tmp;
        NhlString       t2=NULL;
        NrmValue        val;
        NhlGenArray     sgen;
        NhlErrorTypes   ret = NhlNOERROR;

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

        tmp = strtoul(from->data.strval,&t2,10);
        if(!tmp && (from->data.strval == t2)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
                        func,from->data.strval);
                to->size = 0;
                return NhlFATAL;
        }

        _NhlSetVal(unsigned long,sizeof(unsigned long),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToInt64
CvtArgs
{
        char            func[] = "NhlCvtStringToInt64";
        long long       tmp;
        NhlString       t2=NULL;
        NrmValue        val;
        NhlGenArray     sgen;
        NhlErrorTypes   ret = NhlNOERROR;

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

        tmp = local_strtoll(from->data.strval,&t2,10);

        if(!tmp && (from->data.strval == t2)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
                        func,from->data.strval);
                to->size = 0;
                return NhlFATAL;
        }

        _NhlSetVal(long long,sizeof(long long),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToUint64
CvtArgs
{
        char            func[] = "NhlCvtStringToUint64";
        unsigned long long       tmp;
        NhlString       t2=NULL;
        NrmValue        val;
        NhlGenArray     sgen;
        NhlErrorTypes   ret = NhlNOERROR;

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

        tmp = (unsigned long long) strtoull(from->data.strval,&t2,10);

        if(!tmp && (from->data.strval == t2)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
                        func,from->data.strval);
                to->size = 0;
                return NhlFATAL;
        }

        _NhlSetVal(unsigned long long,sizeof(unsigned long long),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToUbyte
CvtArgs
{
        char            func[] = "NhlCvtStringToUbyte";
        unsigned long long       tmp;
        NhlString       t2=NULL;
        NrmValue        val;
        NhlGenArray     sgen;
        NhlErrorTypes   ret = NhlNOERROR;

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

        tmp = (unsigned long long) strtoull(from->data.strval,&t2,10);

        if(!tmp && (from->data.strval == t2)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
                        func,from->data.strval);
                to->size = 0;
                return NhlFATAL;
        }

        _NhlSetVal(unsigned char,sizeof(unsigned char),tmp);
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

	_NhlSetVal(short,sizeof(short),tmp);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringToUshort
CvtArgs
{
        char            func[] = "NhlCvtStringToUshort";
        unsigned short  tmp;
        NhlString       t2=NULL;
        NrmValue        val;
        NhlGenArray     sgen;
        NhlErrorTypes   ret = NhlNOERROR;

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

        tmp = (unsigned short)strtoul(from->data.strval,&t2,10);
        if(!tmp && (from->data.strval == t2)){
                NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Can't Convert \"%s\"",
                        func,from->data.strval);
                to->size = 0;
                return NhlFATAL;
        }

        _NhlSetVal(unsigned short,sizeof(unsigned short),tmp);
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

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
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

	_NhlSetVal(NrmQuark,sizeof(NrmQuark),tq);
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

	tstring = NrmQuarkToString(from->data.lngval);
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
		NhlPError(NhlINFO,NhlEUNKNOWN,"%s:%s to %s losing information",
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
	char		buff[_NhlMAXRESNAMLEN];
	NrmQuark	newfromQ;
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
		strcpy(buff,NhlTString);
		sgen = _NhlConvertCreateGenArray(data,NhlTString,
						sizeof(NhlString),1,NULL);
	}
	else{
		memcpy(data,(Const char *)&from->data,from->size);
		strcpy(buff,NrmQuarkToString(from->typeQ));
		sgen = _NhlConvertCreateGenArray(data,buff,from->size,1,NULL);
	}
	if(!sgen){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}

	/*
	 * We need a more specific name for the from GenArray so the
	 * specific converters can be called.
	 */
	strcat(buff,NhlTGenArray);
	newfromQ = NrmStringToQuark(buff);

	/*
	 * If they are now equal, then just set.
	 */
	if((newfromQ == to->typeQ) || (genQ == to->typeQ)){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),sgen);
	}

	val.size = sizeof(NhlGenArray);
	val.data.ptrval = sgen;

	return _NhlReConvertData(newfromQ,to->typeQ,&val,to);
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
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
	}

	/*
	 * if the from gen array is null, then it is a valid (NULL) array
	 * of any of the specific types - so just set it.
	 */
	if(!gen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
	}

	/*
	 * if from is not a GenArray, then this converter was already called
	 * to get the more specific name.  This ends the recursion.
	 */
	if((from->typeQ != genQ) && (from->typeQ != varQ)){
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
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
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
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
	}

	if(gen->num_elements > 1){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s to %s conversion losing information",func,
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
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
	ng_size_t	i;						\
	char		func[] =					\
		"NhlCvt" #FROMTYPE "GenArrayTo" #TOTYPE "GenArray";	\
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
			toval[i] = (totype)(fromval[i]);		\
			if((ret != NhlINFO)&&(fromval[i]!=(fromtype)(toval[i]))){		\
				NhlPError(NhlINFO,NhlEUNKNOWN,	\
				"%s:Conversion Losing Information",	\
								func);	\
				ret = NhlINFO;				\
			}						\
		}							\
	}								\
	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);		\
}

#define	_FromArrType(ftype,FTYPE,fext)\
_ToArrType(ftype,FTYPE,fext,unsigned char,Byte)				\
_ToArrType(ftype,FTYPE,fext,char,Character)				\
_ToArrType(ftype,FTYPE,fext,double,Double)				\
_ToArrType(ftype,FTYPE,fext,float,Float)				\
_ToArrType(ftype,FTYPE,fext,int,Integer)				\
_ToArrType(ftype,FTYPE,fext,long,Long)					\
_ToArrType(ftype,FTYPE,fext,long long,Int64)				\
_ToArrType(ftype,FTYPE,fext,unsigned long long,Uint64)			\
_ToArrType(ftype,FTYPE,fext,unsigned char,Ubyte)			\
_ToArrType(ftype,FTYPE,fext,unsigned long,Ulong)			\
_ToArrType(ftype,FTYPE,fext,unsigned int,Uint)				\
_ToArrType(ftype,FTYPE,fext,unsigned short,Ushort)			\
_ToArrType(ftype,FTYPE,fext,short,Short)

/*
 * These six line create all 49 converter functions that go from each
 * of these six types to each of these six types. These are all the
 * conversions that can be done using simple casts.
 */
_FromArrType(unsigned char,Byte,byte)
_FromArrType(char,Character,char)
_FromArrType(double,Double,dbl)
_FromArrType(float,Float,flt)
_FromArrType(int,Integer,int)
_FromArrType(long,Long,lng)
_FromArrType(short,Short,shrt)
_FromArrType(long long,Int64,int64)
_FromArrType(unsigned long long,Uint64,uint64)
_FromArrType(unsigned long,Ulong,ulong)
_FromArrType(unsigned int,Uint,uint)
_FromArrType(unsigned short,Ushort,ushort)
_FromArrType(unsigned char,Ubyte,ubyte)

#undef _ToArrType
#undef _FromArrType

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtByteGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	unsigned char	*fromval;
	NhlString	*toval;
	char		func[] = "NhlCvtByteGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	ng_size_t	i;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtCharacterGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*toval;
	char		*fromval;
	ng_size_t	i;
	char		func[] = "NhlCvtCharacterGenArrayToStringGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions, fromgen->len_dimensions);
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
	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtDoubleGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	double		*fromval;
	NhlString	*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtDoubleGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtFloatGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	float		*fromval;
	NhlString	*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtFloatGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtIntegerGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	int		*fromval;
	NhlString	*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtIntegerGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUintGenArrayToStringGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        unsigned int    *fromval;
        NhlString       *toval;
        int             i;
        char            func[] = "NhlCvtUintGenArrayToStringGenArray";
        char            buff[_NhlMAXLINELEN];
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

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

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtLongGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	long		*fromval;
	NhlString	*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtLongGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions, fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		sprintf(buff,"%ld",fromval[i]);
		toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
		if(toval[i] == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		strcpy(toval[i],buff);
	}

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUlongGenArrayToStringGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        unsigned long   *fromval;
        NhlString       *toval;
        int             i;
        char            func[] = "NhlCvtUlongGenArrayToStringGenArray";
        char            buff[_NhlMAXLINELEN];
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

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
                sprintf(buff,"%ld",fromval[i]);
                toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
                if(toval[i] == NULL){
                        NhlPError(NhlFATAL,ENOMEM,"%s",func);
                        return NhlFATAL;
                }
                strcpy(toval[i],buff);
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtInt64GenArrayToStringGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        long long       *fromval;
        NhlString       *toval;
        int             i;
        char            func[] = "NhlCvtInt64GenArrayToStringGenArray";
        char            buff[_NhlMAXLINELEN];
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

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
                sprintf(buff,"%lld",fromval[i]);
                toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
                if(toval[i] == NULL){
                        NhlPError(NhlFATAL,ENOMEM,"%s",func);
                        return NhlFATAL;
                }
                strcpy(toval[i],buff);
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUint64GenArrayToStringGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        unsigned long long       *fromval;
        NhlString       *toval;
        int             i;
        char            func[] = "NhlCvtUint64GenArrayToStringGenArray";
        char            buff[_NhlMAXLINELEN];
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

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
                sprintf(buff,"%llu",fromval[i]);
                toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
                if(toval[i] == NULL){
                        NhlPError(NhlFATAL,ENOMEM,"%s",func);
                        return NhlFATAL;
                }
                strcpy(toval[i],buff);
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUbyteGenArrayToStringGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        unsigned char   *fromval;
        NhlString       *toval;
        int             i;
        char            func[] = "NhlCvtUbyteGenArrayToStringGenArray";
        char            buff[_NhlMAXLINELEN];
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

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
                sprintf(buff,"%u",(unsigned int)fromval[i]);
                toval[i] = NhlConvertMalloc(sizeof(char) * (strlen(buff) + 1));
                if(toval[i] == NULL){
                        NhlPError(NhlFATAL,ENOMEM,"%s",func);
                        return NhlFATAL;
                }
                strcpy(toval[i],buff);
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtShortGenArrayToStringGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	short		*fromval;
	NhlString	*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtShortGenArrayToStringGenArray";
	char		buff[_NhlMAXLINELEN];
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtUshortGenArrayToStringGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        unsigned short  *fromval;
        NhlString       *toval;
        int             i;
        char            func[] = "NhlCvtUshortGenArrayToStringGenArray";
        char            buff[_NhlMAXLINELEN];
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

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

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToByteGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	unsigned char	*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtStringGenArrayToByteGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (unsigned char *)
		NhlConvertMalloc(sizeof(unsigned char)*fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTByte,sizeof(unsigned char),
			fromgen->num_dimensions, fromgen->len_dimensions);
	if(togen == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	for(i=0;i < fromgen->num_elements;i++){
		char	*t2;

		t2=NULL;
		toval[i] = (unsigned char)strtol(fromval[i],&t2,10);
		if(!toval[i] && (fromval[i] == t2)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't Convert \"%s\"",func,fromval[i]);
			to->size = 0;
			return NhlFATAL;
		}
	}

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToCharacterGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	char		*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtStringGenArrayToCharacterGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (char *)NhlConvertMalloc(sizeof(char) * fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTCharacter,sizeof(char),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToDoubleGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	double		*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtStringGenArrayToDoubleGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (double *)NhlConvertMalloc(sizeof(double) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTDouble,sizeof(double),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToFloatGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	float		*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtStringGenArrayToFloatGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (float *)NhlConvertMalloc(sizeof(float)*fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTFloat,sizeof(float),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToIntegerGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	int		*toval;
	ng_size_t		i;
	char		func[] = "NhlCvtStringGenArrayToIntegerGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (int *)NhlConvertMalloc(sizeof(int) * fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTInteger,sizeof(int),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToUintGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        NhlString       *fromval;
        unsigned int    *toval;
        int             i;
        char            func[] = "NhlCvtStringGenArrayToUintGenArray";
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

        toval = (unsigned int *)NhlConvertMalloc(sizeof(unsigned int) * fromgen->num_elements);
        if(toval == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        togen = _NhlConvertCreateGenArray(toval,NhlTUint,sizeof(unsigned int),
                        fromgen->num_dimensions,fromgen->len_dimensions);
        if(togen == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        for(i=0;i < fromgen->num_elements;i++){
                char    *t2;

                t2=NULL;
                toval[i] = (unsigned int)strtoul(fromval[i],&t2,10);
                if(!toval[i] && (fromval[i] == t2)){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "%s:Can't Convert \"%s\"",func,fromval[i]);
                        to->size = 0;
                        return NhlFATAL;
                }
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToLongGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	long		*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtStringGenArrayToLongGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (long *)NhlConvertMalloc(sizeof(long) * fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTLong,sizeof(long),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToUlongGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        NhlString       *fromval;
        unsigned long   *toval;
        int             i;
        char            func[] = "NhlCvtStringGenArrayToUlongGenArray";
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

        toval = (unsigned long *)NhlConvertMalloc(sizeof(unsigned long) * fromgen->num_elements);
        if(toval == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        togen = _NhlConvertCreateGenArray(toval,NhlTUlong,sizeof(unsigned long),
                        fromgen->num_dimensions,fromgen->len_dimensions);
        if(togen == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        for(i=0;i < fromgen->num_elements;i++){
                char    *t2;

                t2=NULL;
                toval[i] = strtoul(fromval[i],&t2,10);
                if(!toval[i] && (fromval[i] == t2)){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "%s:Can't Convert \"%s\"",func,fromval[i]);
                        to->size = 0;
                        return NhlFATAL;
                }
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToInt64GenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        NhlString       *fromval;
        long long       *toval;
        int             i;
        char            func[] = "NhlCvtStringGenArrayToInt64GenArray";
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

        toval = (long long *)NhlConvertMalloc(sizeof(long long) * fromgen->num_elements);
        if(toval == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        togen = _NhlConvertCreateGenArray(toval,NhlTInt64,sizeof(long long),
                        fromgen->num_dimensions,fromgen->len_dimensions);
        if(togen == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        for(i=0;i < fromgen->num_elements;i++){
                char    *t2;

                t2=NULL;

                toval[i] = local_strtoll(fromval[i],&t2,10);
                if(!toval[i] && (fromval[i] == t2)){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "%s:Can't Convert \"%s\"",func,fromval[i]);
                        to->size = 0;
                        return NhlFATAL;
                }
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToUint64GenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        NhlString       *fromval;
        unsigned long long *toval;
        int             i;
        char            func[] = "NhlCvtStringGenArrayToUint64GenArray";
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

        toval = (unsigned long long *)NhlConvertMalloc(sizeof(unsigned long long) * fromgen->num_elements);
        if(toval == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        togen = _NhlConvertCreateGenArray(toval,NhlTUint64,sizeof(unsigned long long),
                        fromgen->num_dimensions,fromgen->len_dimensions);
        if(togen == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        for(i=0;i < fromgen->num_elements;i++){
                char    *t2;

                t2=NULL;
                toval[i] = strtoull(fromval[i],&t2,10);
                if(!toval[i] && (fromval[i] == t2)){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "%s:Can't Convert \"%s\"",func,fromval[i]);
                        to->size = 0;
                        return NhlFATAL;
                }
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToUbyteGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        NhlString       *fromval;
        unsigned char   *toval;
        int             i;
        char            func[] = "NhlCvtStringGenArrayToUbyteGenArray";
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

        toval = (unsigned char *)NhlConvertMalloc(sizeof(unsigned char) * fromgen->num_elements);
        if(toval == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        togen = _NhlConvertCreateGenArray(toval,NhlTUbyte,sizeof(unsigned char),
                        fromgen->num_dimensions,fromgen->len_dimensions);
        if(togen == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        for(i=0;i < fromgen->num_elements;i++){
                char    *t2;

                t2=NULL;
                toval[i] = strtoull(fromval[i],&t2,10);
                if(!toval[i] && (fromval[i] == t2)){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "%s:Can't Convert \"%s\"",func,fromval[i]);
                        to->size = 0;
                        return NhlFATAL;
                }
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToShortGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NhlString	*fromval;
	short		*toval;
	ng_size_t	i;
	char		func[] = "NhlCvtStringGenArrayToShortGenArray";
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called with improper number of args",func);
		return NhlFATAL;
	}

	fromgen = from->data.ptrval;

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (short *)NhlConvertMalloc(sizeof(short) *fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTShort,sizeof(short),
			fromgen->num_dimensions, fromgen->len_dimensions);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtStringGenArrayToUshortGenArray
CvtArgs
{
        NhlGenArray     togen,fromgen;
        NhlString       *fromval;
        unsigned short  *toval;
        int             i;
        char            func[] = "NhlCvtStringGenArrayToUshortGenArray";
        NhlErrorTypes   ret = NhlNOERROR;

        if(nargs != 0){
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                        "%s:Called with improper number of args",func);
                return NhlFATAL;
        }

        fromgen = from->data.ptrval;

        if(!fromgen){
                _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
        }
        fromval = fromgen->data;

        toval = (unsigned short *)NhlConvertMalloc(sizeof(unsigned short) *fromgen->num_elements);
        if(toval == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        togen = _NhlConvertCreateGenArray(toval,NhlTUshort,sizeof(unsigned short),
                        fromgen->num_dimensions,fromgen->len_dimensions);
        if(togen == NULL){
                NhlPError(NhlFATAL,ENOMEM,"%s",func);
                return NhlFATAL;
        }

        for(i=0;i < fromgen->num_elements;i++){
                char    *t2;

                t2=NULL;
                toval[i] = (unsigned short)strtol(fromval[i],&t2,10);
                if(!toval[i] && (fromval[i] == t2)){
                        NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "%s:Can't Convert \"%s\"",func,fromval[i]);
                        to->size = 0;
                        return NhlFATAL;
                }
        }

        _NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
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

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),togen);
}

/*ARGSUSED*/
static NhlErrorTypes
NhlCvtQuarkGenArrayToGenArray
CvtArgs
{
	NhlGenArray	togen,fromgen;
	NrmQuark	*fromval;
	NhlString	*toval;
	ng_size_t	i;
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

	if(!fromgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fromgen);
	}
	fromval = fromgen->data;

	toval = (NhlString *)NhlConvertMalloc(sizeof(NhlString) *
							fromgen->num_elements);
	if(toval == NULL){
		NhlPError(NhlFATAL,ENOMEM,"%s",func);
		return NhlFATAL;
	}

	togen = _NhlConvertCreateGenArray(toval,NhlTString,sizeof(NhlString),
			fromgen->num_dimensions, fromgen->len_dimensions);
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
	static char	true[] = "True";
	static char	false[] = "False";
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

	_NhlSetVal(NhlString,sizeof(NhlString),tstring);
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

	_NhlSetVal(int,sizeof(int),tint);
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
			{True,	"True"},
			{False,	"False"},
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
			{121,	"o_helvetica"},
			{122,	"o_helvetica-bold"},
			{125,	"o_times-roman"},
			{126,	"o_times-bold"},
			{129,	"o_courier"},
			{130,	"o_courier-bold"},
			{133,	"o_greek"},
			{134,	"o_math-symbols"},
			{135,	"o_text-symbols"},
			{136,	"o_weather1"},
			{137,	"o_weather2"},
			};


	intQ = NrmStringToQuark(NhlTInteger);
	longQ = NrmStringToQuark(NhlTLong);
	stringQ = NrmStringToQuark(NhlTString);
	quarkQ = NrmStringToQuark(NhlTQuark);
	genQ = NrmStringToQuark(NhlTGenArray);
	intgenQ = NrmStringToQuark(NhlTIntegerGenArray);
	strgenQ = NrmStringToQuark(NhlTStringGenArray);
	quarkgenQ = NrmStringToQuark(NhlTQuarkGenArray);
	varQ = NrmStringToQuark(NhlTVariable);

	/*
	 * Create hierarchy
	 */
	(void)_NhlRegisterTypes(NhlTScalar,NhlTByte,NhlTCharacter,
		NhlTUbyte,NhlTShort,
		NhlTUshort, NhlTUint, NhlTUlong, NhlTInt64, NhlTUint64,
		NhlTLong,NhlTFloat,NhlTDouble,NhlTInteger,NhlTString,NhlTQuark,
								NULL);
	(void)_NhlRegisterTypes(NhlTInteger,NhlTEnum,NULL);

	(void)_NhlRegisterTypes(NhlTGenArray,NhlTByteGenArray,
		NhlTCharacterGenArray,NhlTUbyteGenArray,
		NhlTShortGenArray,NhlTLongGenArray,
                NhlTUshortGenArray, NhlTUintGenArray, NhlTUlongGenArray,
                NhlTInt64GenArray, NhlTUint64GenArray,
		NhlTFloatGenArray,NhlTDoubleGenArray,NhlTIntegerGenArray,
		NhlTStringGenArray,NhlTQuarkGenArray,NhlTVariable,NULL);

	(void)_NhlRegisterTypes(NhlTIntegerGenArray,NhlTEnumGenArray,NULL);


	/*
	 * Register all the converters from Scalar types to other Scalar
	 * types.
	 */
#define	_Reg(FROM,TO)\
	(void)NhlRegisterConverter(NULL,NhlT##FROM,NhlT##TO,\
				NhlCvt##FROM##To##TO,NULL,0,False,NULL);
#define _RegToAll(FROM)\
	_Reg(FROM,Byte)		\
	_Reg(FROM,Character)	\
	_Reg(FROM,Double)	\
	_Reg(FROM,Float)	\
	_Reg(FROM,Integer)	\
	_Reg(FROM,Long)		\
	_Reg(FROM,Short)	\
	_Reg(FROM,Ushort)	\
	_Reg(FROM,Uint)		\
	_Reg(FROM,Ulong)	\
	_Reg(FROM,Uint64)	\
	_Reg(FROM,Int64)	\
	_Reg(FROM,Ubyte)	\
	_Reg(FROM,String)

	/*
	 * These 8 lines end up installing 64 converter functions.
	 * 
	 * Now, we have 13 data type, there should be lots of converter functions.
	 */
	_RegToAll(Ushort)
	_RegToAll(Uint)
	_RegToAll(Ulong)
	_RegToAll(Uint64)
	_RegToAll(Int64)
	_RegToAll(Ubyte)
	_RegToAll(Byte)
	_RegToAll(Character)
	_RegToAll(Double)
	_RegToAll(Float)
	_RegToAll(Integer)
	_RegToAll(Long)
	_RegToAll(Short)
	_RegToAll(String)

	(void)NhlRegisterConverter(NULL,NhlTString,NhlTQuark,
				NhlCvtStringToQuark,NULL,0,False,NULL);

	/*
	 * take care of all Quark to Scalar conversions
	 */
	(void)NhlRegisterConverter(NULL,NhlTQuark,NhlTScalar,
			NhlCvtQuarkToScalar,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTByte,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTCharacter,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTDouble,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTFloat,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTInteger,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTUint,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTLong,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTUlong,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTShort,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTUshort,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTInt64,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTUint64,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTUbyte,NhlTQuark,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTQuark,NhlTString,NhlTQuark,NhlTScalar);


	/*
	 * Take care of all GenArray to Scalar Conversions
	 */
	(void)NhlRegisterConverter(NULL,NhlTGenArray,NhlTScalar,
				NhlCvtGenArrayToScalar,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTByte,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTCharacter,NhlTGenArray,
								NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTDouble,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTFloat,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTInteger,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUint,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTLong,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUlong,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTInt64,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUint64,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUbyte,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTShort,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUshort,NhlTGenArray,NhlTScalar);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTString,NhlTGenArray,NhlTScalar);

	/*
	 * Take care of all Scalar to all GenArray conversions
	 */
	(void)NhlRegisterConverter(NULL,NhlTScalar,NhlTGenArray,
				NhlCvtScalarToGenArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTByteGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTCharacterGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTDoubleGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTFloatGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTLongGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTInt64GenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTShortGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTStringGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTIntegerGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTUint64GenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTUbyteGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTUlongGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTUintGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTUshortGenArray,NhlTScalar,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTScalar,NhlTVariable,NhlTScalar,NhlTGenArray);

	/*
	 * Register all the converters from Array types to other Array
	 * types.
	 */
	(void)NhlRegisterConverter(NULL,NhlTGenArray,NhlTGenArray,
				NhlCvtGenArrayToGenArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTByteGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTCharacterGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTDoubleGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTFloatGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTLongGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTInt64GenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTShortGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTStringGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTIntegerGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUshortGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUintGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUlongGenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUint64GenArray,NhlTGenArray,
								NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTGenArray,NhlTUbyteGenArray,NhlTGenArray,
								NhlTGenArray);

	(void)NhlRegisterConverter(NULL,NhlTGenArray,NhlTVariable,
				NhlCvtGenArrayToVariable,NULL,0,False,NULL);

#define	_RegArr(FROM,TO)\
	(void)NhlRegisterConverter(NULL,\
		NhlT##FROM##GenArray,NhlT##TO##GenArray,\
		NhlCvt##FROM##GenArrayTo##TO##GenArray,NULL,0,False,NULL);
#define _RegArrToAll(FROM)\
	_RegArr(FROM,Byte)	\
	_RegArr(FROM,Character)	\
	_RegArr(FROM,Double)	\
	_RegArr(FROM,Float)	\
	_RegArr(FROM,Integer)	\
	_RegArr(FROM,Long)	\
	_RegArr(FROM,Short)	\
	_RegArr(FROM,Int64)	\
	_RegArr(FROM,Ushort)	\
	_RegArr(FROM,Uint)	\
	_RegArr(FROM,Ulong)	\
	_RegArr(FROM,Uint64)	\
	_RegArr(FROM,Ubyte)	\
	_RegArr(FROM,String)

	/*
	 * These 8 lines end up installing 64 converter functions.
	 * 
	 * Now, we have 13 data type, there should be lots of converter functions.
	 */
	_RegArrToAll(Ushort)
	_RegArrToAll(Int64)
	_RegArrToAll(Uint)
	_RegArrToAll(Ulong)
	_RegArrToAll(Uint64)
	_RegArrToAll(Ubyte)
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
	(void)NhlRegisterConverter(NULL,NhlTQuarkGenArray,NhlTGenArray,
			NhlCvtQuarkGenArrayToGenArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTByteGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTCharacterGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTDoubleGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTFloatGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTShortGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTIntegerGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTLongGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTInt64GenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTUshortGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTUintGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTUlongGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTUint64GenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTUbyteGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTStringGenArray,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)_NhlRegSymConv(NULL,NhlTQuarkGenArray,NhlTVariable,
						NhlTQuarkGenArray,NhlTGenArray);
	/*
	 * Register enumerations.
	 */
	(void)_NhlRegisterEnumType(NULL,NhlTBoolean,BoolEnumList,
						NhlNumber(BoolEnumList));
	(void)_NhlRegisterEnumType(NULL,NhlTFont,FontEnumList,
						NhlNumber(FontEnumList));
	/*
	 * Need to over-ride some of the default "enum" converters for Boolean
	 * since all values are valid.
	 */
	(void)NhlRegisterConverter(NULL,NhlTBoolean,NhlTString,
				NhlCvtBooleanToString,NULL,0,False,NULL);
	(void)NhlRegisterConverter(NULL,NhlTScalar,NhlTBoolean,
				NhlCvtScalarToBoolean,NULL,0,False,NULL);

	return;
}

/* $OpenBSD: strtoll.c,v 1.1 2006/09/18 21:12:57 mpf Exp $ */
/* Modified strtoll() from stdlib */
/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.

#include <sys/param.h>
#include "stand.h"
 */

/*
 * Convert a string to a long long.
 *
 * Ignores `locale' stuff.  Assumes that the upper and lower case
 * alphabets and digits are each contiguous.
 */
long long
local_strtoll(const char *nptr, char **endptr, int base)
{
	const char *s;
	long long acc, cutoff;
	int c;
	int neg, any, cutlim;

	/*
	 * Skip white space and pick up leading +/- sign if any.
	 * If base is 0, allow 0x for hex and 0 for octal, else
	 * assume decimal; if base is already 16, allow 0x.
	 */
	s = nptr;
	do {
		c = (unsigned char) *s++;
	} while (isspace(c));
	if (c == '-') {
		neg = 1;
		c = *s++;
	} else {
		neg = 0;
		if (c == '+')
			c = *s++;
	}
	if ((base == 0 || base == 16) &&
	    c == '0' && (*s == 'x' || *s == 'X')) {
		c = s[1];
		s += 2;
		base = 16;
	}
	if (base == 0)
		base = c == '0' ? 8 : 10;

	/*
	 * Compute the cutoff value between legal numbers and illegal
	 * numbers.  That is the largest legal value, divided by the
	 * base.  An input number that is greater than this value, if
	 * followed by a legal input character, is too big.  One that
	 * is equal to this value may be valid or not; the limit
	 * between valid and invalid numbers is then based on the last
	 * digit.  For instance, if the range for long longs is
	 * [-9223372036854775808..9223372036854775807] and the input base
	 * is 10, cutoff will be set to 922337203685477580 and cutlim to
	 * either 7 (neg==0) or 8 (neg==1), meaning that if we have
	 * accumulated a value > 922337203685477580, or equal but the
	 * next digit is > 7 (or 8), the number is too big, and we will
	 * return a range error.
	 *
	 * Set any if any `digits' consumed; make it negative to indicate
	 * overflow.
	 */
	cutoff = neg ? LLONG_MIN : LLONG_MAX;
	cutlim = cutoff % base;
	cutoff /= base;
	if (neg) {
		if (cutlim > 0) {
			cutlim -= base;
			cutoff += 1;
		}
		cutlim = -cutlim;
	}
	for (acc = 0, any = 0;; c = (unsigned char) *s++) {
		if (isdigit(c))
			c -= '0';
		else if (isalpha(c))
			c -= isupper(c) ? 'A' - 10 : 'a' - 10;
		else
			break;
		if (c >= base)
			break;
		if (any < 0)
			continue;
		if (neg) {
			if (acc < cutoff || (acc == cutoff && c > cutlim)) {
				any = -1;
				acc = LLONG_MIN;
			} else {
				any = 1;
				acc *= base;
				acc -= c;
			}
		} else {
			if (acc > cutoff || (acc == cutoff && c > cutlim)) {
				any = -1;
				acc = LLONG_MAX;
			} else {
				any = 1;
				acc *= base;
				acc += c;
			}
		}
	}
	if (endptr != 0)
		*endptr = (char *) (any ? s - 1 : nptr);
	return (acc);
}

