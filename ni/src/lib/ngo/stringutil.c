/*
 *      $Id: stringutil.c,v 1.3 1998-12-16 23:51:41 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		stringutil.c
 *
 *	Authors:	David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Apr 28 14:06:01 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/stringutil.h>
#include <float.h>

/*
 * Removes all but one of the zeroes that follow any non-zero digits to
 * right of a decimal point in the string representation of a floating
 * point value. Assumes that the representation contains a decimal point.
 */

void
NgRemoveZeros
#if	NhlNeedProto
(
	char		*fstr

)
#else
(fstr)
	char		*fstr;
#endif
{
	char *cp = fstr + strlen(fstr) - 1;

	/* assumes there must be a decimal point */

	if (*cp != '0' || cp - fstr < 4)
		return;
	while (*(cp-1) == '0') {
		*cp = '\0';
		cp--;
	}
	return;
}


/*
 * Ensures that floating point numbers always contain a decimal point, even
 * if represented in exponential format.
 */

void
NgFixFloat
(
	char		*fstr
        )
{
        char *cp,tcp;
                            
        if (!strchr(fstr,'.')) {
                strcat(fstr,".0");
                if ((cp=strchr(fstr,'e'))!= NULL) {
                        char *tcp;
                        for (tcp = &fstr[strlen(fstr)-1];
                             tcp > cp+1;tcp--) {
                                *tcp = *(tcp-2);
                        }
                        *cp = '.';
                        *(cp+1) = '0';
                }
        }
        return;
}

/*
 * returns string representation of an Ncl value in an internally
 * managed buffer. Each call destroys the results of a previous call.
 * The strlen is returned in the vlen parameter. If short_form is True,
 * superfluous zeros are removed from the end of floating point numbers.
 */

extern char *NgTypedValueToString
(
        NclExtValueRec	*val,
        int		index,
        NhlBoolean	short_form,
        int		*vlen
        )
{
        static char		buffer[256];
        static NhlBoolean	nclalloc = False;
        static char		*nclstring = NULL;
        char			*valp = ((char *) val->value) +
                				index * val->elem_size;
        if (nclalloc) {
                NclFree(nclstring);
                nclalloc = False;
        }
        
        switch (val->type) {
            case NCLAPI_float:
                    sprintf(buffer,"%.*g",FLT_DIG,*(float*)valp);
                    NgFixFloat(buffer);
                    if (short_form)
                            NgRemoveZeros(buffer);
                    *vlen = strlen(buffer);
                    return buffer;
            case NCLAPI_double:
                    sprintf(buffer,"%.*g",DBL_DIG,*(double*)valp);
                    NgFixFloat(buffer);
                    if (short_form)
                            NgRemoveZeros(buffer);
                    *vlen = strlen(buffer);
                    return buffer;
            case NCLAPI_byte:
                    sprintf(buffer,"0x%x",*(char*)valp);
                    *vlen = strlen(buffer);
                    return buffer;
            default:
                    nclstring = NclTypeToString(valp,val->type);
                    nclalloc = True;
                    *vlen = strlen(nclstring);
                    return nclstring;
        }
}


/*
 * returns string representation of an NCL type
 */

extern char *
NgTypeString
(
	int type
)
{
	switch (type) {
	case NCLAPI_none:
	default:
		return("none");
	case NCLAPI_short:
		return("short");
	case NCLAPI_int:
		return("integer");
	case NCLAPI_long:
		return("long");
	case NCLAPI_float:
		return("float");
	case NCLAPI_double:
		return("double");
	case NCLAPI_char:
		return("character");
	case NCLAPI_byte:
		return("byte");
	case NCLAPI_string:
		return("string");
#if 0
	case NCLAPI_numeric:
		return("numeric");
#endif
	case NCLAPI_logical:
		return("logical");
	case NCLAPI_obj:
		return("graphic");
	}
}


/*
 * returns HLU type string representation of an NCL type
 */

extern char *
NgHLUTypeString
(
	int type
)
{
	switch (type) {
	case NCLAPI_none:
	default:
		return("None");
	case NCLAPI_short:
		return(NhlTShort);
	case NCLAPI_int:
		return(NhlTInteger);
	case NCLAPI_long:
		return(NhlTLong);
	case NCLAPI_float:
		return(NhlTFloat);
	case NCLAPI_double:
		return(NhlTDouble);
	case NCLAPI_char:
		return(NhlTCharacter);
	case NCLAPI_byte:
		return(NhlTByte);
	case NCLAPI_string:
		return(NhlTString);
#if 0
	case NCLAPI_numeric:
		return("numeric");
#endif
	case NCLAPI_logical:
		return(NhlTBoolean);
	case NCLAPI_obj:
		return(NhlTObjId);
	}
}





