/*
 *      $Id: Format.c,v 1.18 2007-06-28 23:01:22 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Format.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 12 17:29:07 MDT 1994
 *
 *	Description:	Provides output formatting suitable for use with
 *			Plotchar. For now formatting of float values only
 *			is supported.
 */

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/Error.h>
#include <ncarg/hlu/FormatI.h>

#define MAX(a,b) (((a)>(b))?(a):(b))

#define _ffSIGDIGITCHAR		'.'
#define _ffLEFTSIGDIGITCHAR	';'
#define _ffEXPONENTCHAR		'^'
#define _ffPOINTPOSITIONCHAR	'~'
#define _ffEXPSWITCHLENGTHCHAR	'?'

static char *Init_Entry_Name = "Unknown_Caller";
static char *Entry_Name;

static char *Cp;

static NhlFormatRec Init_Format = { 
	NULL,		/* fstring  */
	False,		/* plus  */
	False,		/* minus  */
	False,		/* pound */
	False,		/* zero */
	False,		/* space */
	False,		/* exclamation */
	False,		/* comma */
	False,		/* at_sign */
	0,		/* fill_char */
	NhlffUNSPECED,  /* field_width_flag */
        0,		/* field_width */
	NhlffUNSPECED,  /* sig_digits_flag */
        6,		/* sig_digits */
	NhlffUNSPECED,  /* left_sig_digit_flag */
        -10000,		/* left_sig_digit */
	NhlffUNSPECED,  /* point_position_flag */
        0,		/* point_position */
	NhlffUNSPECED,  /* exp_switch_len_flag */
        5,		/* exp_switch_len */
	NhlffUNSPECED,  /* exp_field_width_flag */
        0,		/* exp_field_width */
        False,		/* exp_plus */
	NhlffUNSPECED,  /* exp_type_flag */
        NhlffELITTLE,	/* exp_type */
	NhlffGENERICFLD,/* field_type */
        NULL		/* next_char */
};

static char *Cp;

static NhlFormatRec Format;


/*
 * Function:	print_format
 *
 * Description:	for debugging: prints the current contents of the Format
 *		record
 * In Args:
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */


void print_format(void)
{
	char buf[4];

	printf("plus %s ",Format.plus ? "+" : "-");
	printf("minus %s ",Format.minus ? "+" : "-");
	printf("pound %s ",Format.pound ? "+" : "-");
	printf("zero %s ",Format.zero ? "+" : "-");
	printf("space %s ",Format.space ? "+" : "-");
	printf("exclamation %s ",Format.exclamation ? "+" : "-");
	printf("comma %s ",Format.comma ? "+" : "-");
	printf("at_sign %s\n",Format.at_sign ? "+" : "-");
	if (Format.fill_char)
		sprintf(buf,"%c",Format.fill_char);
	else
		sprintf(buf,"\\0");
	printf("fill_char		'%s'\n",buf);

	printf("field_width_flag	%d\n",Format.field_width_flag);
	printf("field_width		%d\n",Format.field_width);
	printf("sig_digits_flag		%d\n",Format.sig_digits_flag);
	printf("sig_digits		%d\n",Format.sig_digits);
	printf("left_sig_digit_flag	%d\n",Format.left_sig_digit_flag);
	printf("left_sig_digit		%d\n",Format.left_sig_digit);
	printf("point_position_flag	%d\n",Format.point_position_flag);
	printf("point_position		%d\n",Format.point_position);
	printf("exp_switch_flag		%d\n",Format.exp_switch_flag);
	printf("exp_switch_len		%d\n",Format.exp_switch_len);
	printf("exp_field_width_flag	%d\n",Format.exp_field_width_flag);
	printf("exp_field_width		%d\n",Format.exp_field_width);
	printf("exp_plus		%s\n",Format.exp_plus ? "+" : "-");
	printf("exp_type_flag		%d\n",Format.exp_type_flag);
	printf("exp_type		%d\n",Format.exp_type);
	printf("field_type		%d\n",Format.field_type);
        printf("next_char		%p\n",Format.next_char);
}

static NhlErrorTypes parse_flags(void)
{
	NhlBoolean done = False;
	NhlBoolean looking_for_fill_char = False;
        char *e_text;

	while (! done) {
		if (looking_for_fill_char) {
			if (! isprint(*Cp)) {
				e_text = "%s: invalid fill char specified";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,Entry_Name);
				return NhlFATAL;
			}
			Format.fill_char = *Cp;
			looking_for_fill_char = False;
			Cp++;
		}
		switch (*Cp) {
		case '+':
			Format.plus = True;
			break;
		case '-':
			Format.minus = True;
			break;
		case '#':
			Format.pound = True;
			break;
		case '0':
			Format.zero = True;
			break;
		case ' ':
			Format.space = True;
			break;
		case '!':
			Format.exclamation = True;
			break;
		case ',':
			Format.comma = True;
			break;
		case '@':
			Format.at_sign = True;
			break;
		case '&':
			looking_for_fill_char = True;
			break;
		case '\0':
                        e_text = "%s: premature end of format string";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,Entry_Name);
			return NhlFATAL;
		default:
			done = True;
			break;
		}
		if (! done)
			Cp++;
	}
	return NhlNOERROR;
}

static NhlErrorTypes parse_field_width(void)
{			
	NhlBoolean done = False;
	char field_width_str[16];
	int ndigits = 0;
        char *e_text;
/*
 * Note that specifying a plus following field width dynamic
 * causes all the other fields to default to dynamic mode
 */
	memset(field_width_str,0,16);

	while (! done) {
		switch (*Cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			field_width_str[ndigits++] = *Cp;
			Cp++;
			break;
		case '*':
			if (ndigits > 0 && *(Cp + 1) != '+') {
				e_text =
				      "%s: invalid field width specification";
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
                                          e_text,Entry_Name);
                                return NhlFATAL;
			}
			if (ndigits == 0)
				Format.field_width_flag = NhlffDYNAMIC;
			Cp++;
			if (*Cp == '+') {
				Format.sig_digits_flag = NhlffDYNAMIC;
				Format.left_sig_digit_flag = NhlffDYNAMIC;
				Format.point_position_flag = NhlffDYNAMIC;
				Format.exp_switch_flag = NhlffDYNAMIC;
				Format.exp_field_width_flag = NhlffDYNAMIC;
				Cp++;
			}
			done = True;
			break;
		case '\0':
			e_text = "%s: premature end of format string";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,Entry_Name);
			return NhlFATAL;
		default:
			done = True;
			break;
		}
	}

	if (ndigits > 0) {
		Format.field_width = strtol(field_width_str,NULL,10);
		Format.field_width_flag = NhlffEXPLICIT;
	}
	return NhlNOERROR;
}			


static NhlErrorTypes parse_sig_digits(void)
{			
	NhlBoolean done = False;
	char sig_digits_str[16];
	int ndigits = 0;
        char *e_text;

	memset(sig_digits_str,0,16);

	while (! done) {
		switch (*Cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			sig_digits_str[ndigits++] = *Cp;
			Cp++;
			break;
		case '*':
			if (ndigits > 0) {
				e_text = 
				        "%s: invalid sig digits specification";
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
                                          e_text,Entry_Name);
                                return NhlFATAL;
			}
			Format.sig_digits_flag = NhlffDYNAMIC;
			Cp++;
			done = True;
			break;
		case '\0':
			e_text = "%s: premature end of format string";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,Entry_Name);
			return NhlFATAL;
		default:
			done = True;
			break;
		}
	}

	if (ndigits > 0) {
		Format.sig_digits = strtol(sig_digits_str,NULL,10);
		Format.sig_digits_flag = NhlffEXPLICIT;
	}
	return NhlNOERROR;
}			

static NhlErrorTypes parse_left_sig_digit(void)
{			
	NhlBoolean done = False;
	char left_sig_digit_str[16];
	int ndigits = 0;
        char *e_text;

	memset(left_sig_digit_str,0,16);

	while (! done) {
		switch (*Cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			left_sig_digit_str[ndigits++] = *Cp;
			Cp++;
			break;
		case '-':
		case '+':
			if (ndigits > 0) {
				e_text = 
                                    "%s: invalid left sig digit specification";
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
                                          e_text,Entry_Name);
                                return NhlFATAL;
			}
			left_sig_digit_str[ndigits++] = *Cp;
			Cp++;
			break;
		case '*':
			if (ndigits > 0) {
				e_text = 
                                   "%s: invalid left sig digit specification";
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
                                          e_text,Entry_Name);
                                return NhlFATAL;
			}
			Format.left_sig_digit_flag = NhlffDYNAMIC;
			Cp++;
			done = True;
			break;
		case '\0':
			e_text = "%s: premature end of format string";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,Entry_Name);
			return NhlFATAL;
		default:
			done = True;
			break;
		}
	}

	if (ndigits > 0) {
		Format.left_sig_digit = strtol(left_sig_digit_str,NULL,10);
		Format.left_sig_digit_flag = NhlffEXPLICIT;
	}
	return NhlNOERROR;
}			


static NhlErrorTypes parse_exponent(void)
{
	NhlBoolean done = False;
	char field_width_str[16];
	int ndigits = 0;
        char *e_text;

        if (*Cp == 's') {
                Format.exp_type_flag = NhlffEXPLICIT;
		Format.exp_type = NhlffSUPERSCRIPT;
                Cp++;
        }
	else if (*Cp == 'a') {
                Format.exp_type_flag = NhlffEXPLICIT;
		Format.exp_type = NhlffASTERISK;
                Cp++;
	}
	memset(field_width_str,0,16);

	while (! done) {
		switch (*Cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			field_width_str[ndigits++] = *Cp;
			Cp++;
			break;
		case '+':
			if (ndigits > 0) {
				e_text =
				    "%s: invalid exponent width specification";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,Entry_Name);
				return NhlFATAL;
			}
			Format.exp_plus = True;
			Cp++;
			break;
		case '*':
			if (ndigits > 0) {
				e_text =
				   "%s: invalid exponent width specification";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,Entry_Name);
				return NhlFATAL;
			}
			Format.exp_field_width_flag = NhlffDYNAMIC;
			Cp++;
			done = True;
			break;
		case '\0':
			e_text = "%s: premature end of format string";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,Entry_Name);
			return NhlFATAL;
		default:
			done = True;
			break;
		}
	}

	if (ndigits > 0) {
		Format.exp_field_width = strtol(field_width_str,NULL,10);
		Format.exp_field_width_flag = NhlffEXPLICIT;
	}
	return NhlNOERROR;
}

NhlErrorTypes parse_point_position(void)
{			
	NhlBoolean done = False;
	char point_position_str[16];
	int ndigits = 0;
	char *e_text;

	memset(point_position_str,0,16);

	while (! done) {
		switch (*Cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			point_position_str[ndigits++] = *Cp;
			Cp++;
			break;
		case '*':
			if (ndigits > 0) {
				e_text =
				    "%s: invalid point position specification";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,Entry_Name);
				return NhlFATAL;
			}
			Format.point_position_flag = NhlffDYNAMIC;
			Cp++;
			done = True;
			break;
		case '\0':
			e_text = "%s: premature end of format string";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,Entry_Name);
			return NhlFATAL;
		default:
			done = True;
			break;
		}
	}

	if (ndigits > 0) {
		Format.point_position = strtol(point_position_str,NULL,10);
		Format.point_position_flag = NhlffEXPLICIT;
	}
	return NhlNOERROR;
}			


NhlErrorTypes parse_exp_switch_len(void)
{			
	NhlBoolean done = False;
	char exp_switch_len_str[16];
	int ndigits = 0;
	char *e_text;

	memset(exp_switch_len_str,0,16);

	while (! done) {
		switch (*Cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			exp_switch_len_str[ndigits++] = *Cp;
			Cp++;
			break;
		case '*':
			if (ndigits > 0) {
				e_text =
			    "%s: invalid exponent switch length specification";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,Entry_Name);
				return NhlFATAL;
			}
			Format.exp_switch_flag = NhlffDYNAMIC;
			Cp++;
			done = True;
			break;
		case '\0':
			e_text = "%s: premature end of format string";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,Entry_Name);
			return NhlFATAL;
		default:
			done = True;
			break;
		}
	}

	if (ndigits > 0) {
		Format.exp_switch_len = 
			strtol(exp_switch_len_str,NULL,10);
		Format.exp_switch_flag = NhlffEXPLICIT;
	}
	return NhlNOERROR;
}			

NhlErrorTypes parse_numerics(void)
{
	NhlBoolean done = False;
	NhlBoolean first = True;
	char *e_text;

	while (! done) {
		switch (*Cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		case '*':
			if (first) {
				if (parse_field_width() < NhlNOERROR)
					return NhlFATAL;
			}
			else {
				e_text = "%s: error parsing format string";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,Entry_Name);
				return NhlFATAL;
			}
			break;
		case _ffSIGDIGITCHAR:
			Cp++;
			if (parse_sig_digits() < NhlNOERROR)
				return NhlFATAL;
			break;
		case _ffLEFTSIGDIGITCHAR:
			Cp++;
			if (parse_left_sig_digit() < NhlNOERROR)
				return NhlFATAL;
			break;
		case _ffEXPONENTCHAR:
			Cp++;
			if (parse_exponent() < NhlNOERROR)
				return NhlFATAL;
			break;
		case _ffPOINTPOSITIONCHAR:
			Cp++;
			if (parse_point_position() < NhlNOERROR)
				return NhlFATAL;
			break;
		case _ffEXPSWITCHLENGTHCHAR:
			Cp++;
			if (parse_exp_switch_len() < NhlNOERROR)
				return NhlFATAL;
			break;
		default:
			done = True;
			break;
		}
		first = False;
	}
	return NhlNOERROR;
}

static NhlErrorTypes parse_field_type(void)
{
        char *e_text;

	switch (*Cp) {
	case 'f':
		Format.field_type = NhlffFLOATFLD;
		break;
	case 'g':
		Format.field_type = NhlffGENERICFLD;
		if (! Format.exp_type_flag)
			Format.exp_type = NhlffELITTLE;
		break;
	case 'G':
		Format.field_type = NhlffGENERICFLD;
		if (! Format.exp_type_flag)
			Format.exp_type = NhlffEBIG;
		break;
	case 'e':
		Format.field_type = NhlffEXPONENTIALFLD;
		if (! Format.exp_type_flag)
			Format.exp_type = NhlffELITTLE;
		break;
	case 'E':
		Format.field_type = NhlffEXPONENTIALFLD;
		if (! Format.exp_type_flag)
			Format.exp_type = NhlffEBIG;
		break;
	default:
		e_text = "%s: invalid field type specification";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,Entry_Name);
                return NhlFATAL;
	}
	Cp++;
        Format.next_char = Cp;
	return NhlNOERROR;
}

static char *find_exp
#if	NhlNeedProto
(
	NhlFormatRec *format,
	char *start,
	char *search1,
	char *search2,
	NhlBoolean *has_mantissa
)
#else
(format,start,search1,search2,has_mantissa)
	NhlFormatRec *format;
	char *start;
	char *search1;
	char *search2;
	NhlBoolean *has_mantissa;
#endif
{
        char *cp = NULL;

	*has_mantissa = True;
        switch (format->exp_type) {
        case NhlffELITTLE:
        case NhlffEBIG:
		cp = strstr((const char *)start,(const char *)search2);
		if (cp == start) 
			*has_mantissa = False;
                break;
        case NhlffASTERISK:
        case NhlffSUPERSCRIPT:
		cp = strstr((const char *)start,(const char *)search1);
		if (cp == NULL) {
			cp = strstr((const char *)start,(const char *)search2);
			*has_mantissa = False;
		}
                break;
        }
        return cp;
}


/*
 * Function:	_NhlScanFString
 *
 * Description:	Given an hlu-style format string, returns a pointer to
 * 		a static NhlFormatRec struct resulting from a parse of the
 *		string. This struct is reinitialized at each call, so the
 *		caller is responsible for copying it if the information
 *		needs to be preserved. Warning errors are produced if the
 *		string does not parse, and a NULL record pointer is 
 *		returned. If the input format string is NULL, the function
 *		returns the default NhlFormatRec struct. If entry_name is
 *		NULL error messages will be prefaced with the string
 *		"Unknown caller"
 *
 * In Args:	
 *		NhlString  fstring - the format string
 *		NhlString  entry_name - name of the caller
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

NhlFormatRec *_NhlScanFString
#if	NhlNeedProto
(
	NhlString	fstring,
	NhlString	entry_name
)
#else
(fstring,entry_name)
	NhlString	fstring;
	NhlString	entry_name;
#endif
{
        Entry_Name = entry_name == NULL ? Init_Entry_Name : entry_name;
	Cp = fstring;
	memcpy((void *)&Format,
	       (const void *)&Init_Format,sizeof(NhlFormatRec));
        if (Cp == NULL)
                return &Format;
	Format.fstring = fstring;

	if (parse_flags() < NhlNOERROR)
                return NULL;
	if (parse_numerics() < NhlNOERROR)
                return NULL;
	if (parse_field_type() < NhlNOERROR)
                return NULL;

#if 0 /* for debugging */

	print_format();
#endif
        return &Format;
}

/*
 * Function:	_NhlFormatFloat
 *
 * Description:	Given an NhlFormatRec struct and a floating point value
 *		returns a string representation appropriately formatted
 *		for output using Plotchar.
 *		If any of the 'dynamic' fields are set true in the format
 *		record (meaning that the field has been formatted with the
 *		'*' dynamic attribute) AND the corresponding pointer argument
 *		is not NULL, the argument's value is used to specify the
 *		field. If dynamic is specified but the value is NULL then
 *		the default is used.
 *		If successful the formatted string is returned in a static
 *		buffer. It must be copied if it needs to be retained by the
 *		caller.
 *		If entry_name is NULL error messages are prefaced by the
 *		string - "Unknown caller".
 *
 * In Args:	
 *		NhlFormatRec	format - the formatting record
 *		float		value - the value to be formatted
 *		int *		fwidth - -> to a dynamic field width value
 *		int *		sig_digits - -> to a dynamic sig digits val
 *		int *		left_sig_digit -> to a dynamic left sig dig val
 *		NhlString	entry_point - name of the caller
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	NhlString	the formatted value or NULL if error.
 * Side Effect:	
 */

NhlString _NhlFormatFloat
#if	NhlNeedProto
(
	NhlFormatRec	*format,
	float		value,
	int		*fwidth,
	int		*sig_digits,
	int		*left_sig_digit,
	int		*exp_fwidth,
	int		*exp_switch_len,
	int		*point_pos,
	char		func_code,
	NhlString	entry_point
)
#else
(format,value,fwidth,sig_digits,left_sig_digit,
 exp_fwidth,exp_switch_len,point_pos,func_code,entry_point)
	NhlFormatRec	*format;
	float		value;
	int		*fwidth;
	int		*sig_digits;
	int		*left_sig_digit;
	int		*exp_fwidth;
	int		*exp_switch_len;
	int		*point_pos;
	char		func_code;
	NhlString	entry_point;
#endif
{
        int ndgd,lmsd,iexp,lexp;
        char *c1[] = { " "," ","x",":F0L1:4:F:" };
        int  lex1[] = { 1,1,1,1 };
        char *c2[] = { "e","E","10**","10:S:" };
        int lex2[] = { 0,0,4,2 };
        char *c3[] = { " "," "," ",":N:" };
	char cex1[11],cex2[7],cex3[5];
        int lex3[] = { 0,0,0,0 };
        int len_diff[] = { 0,0,0,15 };
	int exp_len[] = {1,1,5,15 };
        int ioma,iodp,iotz;
        static char cbuf[256];
        int nbuf;
        int ndgs;
        int ieva;
        int ix, i,j;
        char *cp =NULL, *ppos = NULL;
        char fill_char;
        NhlBoolean zero_added = False;
        char *exp_pos = NULL;
        int field_width,apparent_fwidth;
	NhlBoolean left_justify = False;
	int set_ppos,move_cnt = 0;
	NhlBoolean has_mantissa,set_point_pos = False;
	NhlBoolean check_zero = True;
        char tbuf[64];
        long actual_leftmost = 0;

	lmsd = format->left_sig_digit;
        if (format->left_sig_digit_flag) {
		if (format->left_sig_digit_flag == NhlffDYNAMIC && 
		    left_sig_digit != NULL)
			lmsd = *left_sig_digit;
	}
        ndgd = format->sig_digits;
	if (format->sig_digits_flag) {
		if (format->sig_digits_flag == NhlffDYNAMIC && 
		    sig_digits != NULL)
			ndgd = *sig_digits;
	}
        if (lmsd > -10000) {
                float fval;
                sprintf(tbuf,"%14.7e",value);
                for (i = strlen(tbuf)-1; i >=0; i--) {
                        if (tbuf[i] == '+' || tbuf[i] == '-') {
                                cp = &tbuf[i];
                                actual_leftmost = strtol(cp,NULL,10);
                                break;
                        }
                }
                if (value != 0.0)
                        ndgd += MAX(0,actual_leftmost-lmsd);
		/* 
		   this is another attempt at fixing the CPNUMB rounding
		   problem, by "pre-rounding" using the C-library sprintf
		   routine. Values of .5 seem to be somewhat indeterminate.
		   */
                if (actual_leftmost < lmsd) {
                        int digits = MAX(1,actual_leftmost-lmsd+ndgd);
                        sprintf(tbuf,"%14.*g",digits,value);
                        sscanf(tbuf,"%f",&fval);
                        value = fval;
                }
        }

	field_width = format->field_width;
	if (format->field_width_flag) {
		if (format->field_width_flag == NhlffDYNAMIC && 
		    fwidth != NULL)
				field_width = *fwidth;
	}
        
        if (format->field_type == NhlffFLOATFLD)
                iexp = 256;
        else if (format->field_type == NhlffEXPONENTIALFLD)
                iexp = 0;
        else {
                iexp = format->exp_switch_len;
                if (format->exp_switch_flag) {
                        if (format->exp_switch_flag == NhlffDYNAMIC && 
                            exp_switch_len != NULL)
				iexp = *exp_switch_len;
                }
	}

        lexp = format->exp_field_width;
        if (format->exp_field_width_flag) {
                if (format->exp_field_width_flag == NhlffDYNAMIC && 
                    exp_fwidth != NULL)
                        lexp = *exp_fwidth;
        }

        ix = (int) format->exp_type;
	strcpy(cex1,c1[ix]);
	strcpy(cex2,c2[ix]);
	strcpy(cex3,c3[ix]);
	if (format->exp_type == NhlffSUPERSCRIPT && func_code != ':') {
		cex1[0] = cex1[5] = cex1[7] = cex1[9] =func_code;
		cex2[2] = cex2[4] = func_code;
		cex3[0] = cex3[2] = func_code;
	}

        ioma = (int) ! format->exclamation;
        iodp = (int) ! format->pound;
        iotz = (int) ! format->zero;

	if (value == 0.0) {
		check_zero = True;
		if (lmsd < -1000) {
			lmsd = 0;
			check_zero = False;
		}
		value = pow(10.0,lmsd - ndgd - 4);
	}

        NGCALLF(cpinrc,CPINRC)();

	{
		int len1,len2,len3,len4;
		NGstring cex1_f;
		NGstring cex2_f;
		NGstring cex3_f;
		NGstring cbuf_f;
		char *cbuf_c;
		len1 = NGSTRLEN(cex1);
		len2 = NGSTRLEN(cex2);
		len3 = NGSTRLEN(cex3);
		len4 = 128;
		cex1_f = NGCstrToFstr(cex1,len1);
		cex2_f = NGCstrToFstr(cex2,len2);
		cex3_f = NGCstrToFstr(cex3,len3);
		cbuf_f = NGCstrToFstr(cbuf,len4);
        	NGCALLF(cpnumb,CPNUMB)(&value,&ndgd,&lmsd,&iexp,&lexp,
			       cex1_f,cex2_f,cex3_f,
			       &lex1[ix],&lex2[ix],&lex3[ix],
			       &ioma,&iodp,&iotz,cbuf_f,&nbuf,&ndgs,&ieva,
			       len1,len2,len3,len4);
		cbuf_c = NGFstrToCstr(cbuf_f);
		if (cbuf_c != &cbuf[0])
			strcpy(cbuf,cbuf_c);
		cbuf[nbuf] = '\0';
	}
#if 0
        printf("value %f ndgd %d lmsd %d ndgs %d ieva %d cbuf %s\n",
               value,ndgd,lmsd,ndgs,ieva,cbuf);
#endif        

/*
 * Add a leading zero if appropriate.
 */		
        if (format->at_sign && (cbuf[0] == '.' || 
                            (cbuf[0] == '-' && cbuf[1] == '.'))) {
                for (i=nbuf; i > 0; i--) {
                        cbuf[i] = cbuf[i-1];
                }
                if (cbuf[0] == '.')
                        cbuf[0] = '0';
                else
                        cbuf[1] = '0';
                nbuf++;
                zero_added = True;
        }
/*
 * If the value was 0.0 CPNUMB will put extra zeros in the front.
 * Remove them. If the field is to be filled with a fill character,
 * they may be replaced later.
 */

	if (check_zero) {
		i = 0;
		if (cbuf[0] == '-') i++;
		if (cbuf[i] == '0') i++;
		j = i;
		while (cbuf[j] == '0' && cbuf[j] != '\0') j++;
		while (cbuf[j] != '\0')
			cbuf[i++] = cbuf[j++];
		nbuf -= (j - i);
		cbuf[i] = '\0';
	}
		
			
		
/*
 * Add a trailing zero as required
 */
        if (format->at_sign && cbuf[nbuf-1] == '.') {
                zero_added = True;
                cbuf[nbuf++] = '0';
        }
/*
 * Leading plus sign
 */
        if (format->plus && cbuf[0] != '-') {
                for (i=nbuf; i > 0; i--) {
                        cbuf[i] = cbuf[i-1];
                }
                cbuf[0] = '+';
                nbuf++;
        }
/*
 * Or alternatively a leading space
 */
        else if (format->space && cbuf[0] != '-') {
                for (i=nbuf; i > 0; i--) {
                        cbuf[i] = cbuf[i-1];
                }
                cbuf[0] = ' ';
                nbuf++;
        }

/*
 * If not a forced float field find the position of possible exponent.
 * (NULL means the exponent has been left out.)
 */
	if (format->field_type != NhlffFLOATFLD) {
                exp_pos = find_exp(format,cbuf,
				   cex1,cex2,&has_mantissa);
		if (! has_mantissa) {
			if (format->exp_type == NhlffASTERISK) {
				exp_len[ix] = 4;
			}
			else if (format->exp_type == NhlffSUPERSCRIPT) {
				exp_len[ix] = 5;
				len_diff[ix] = 6;
			}
		}
	}

/*
 * Find out whether the point is to be placed explicitly
 */
	
	if (field_width > 0) {
		if (format->point_position_flag == NhlffEXPLICIT) {
			set_ppos = format->point_position;
			set_point_pos = True;
		}
		else if (format->point_position_flag == NhlffDYNAMIC &&
			 point_pos != NULL) {
			set_ppos = *point_pos;
			set_point_pos = True;
		}
	}
	      
/*
 * If replacing the decimal or adding trailing zeros find the position of
 * the decimal point. Note that a trailing zero may need to be inserted
 * after the decimal point and before the exponent.
 */
        if (format->comma || set_point_pos ||
	    (format->at_sign && ! zero_added))
                ppos = strchr((const char *)cbuf,(int)'.');
	if (ppos != NULL) {
 		if (format->comma)
			*ppos = ',';
		if (format->at_sign && ! zero_added) {
			if (exp_pos == ppos + 1) {
				for (cp=&cbuf[nbuf]; cp>exp_pos; cp--)
					*cp = *(cp-1);
				*(exp_pos++) = '0';
				nbuf++;
				zero_added = True;
			}
                }
        }
        cbuf[nbuf] = '\0';
/*
 * Add or remove the exponential plus sign as required
 */
	if (exp_pos != NULL) {
		cp = exp_pos + exp_len[ix];
		if (! format->exp_plus && *cp == '+') {
			for ( ; *cp != '\0'; cp++)
				*cp = *(cp+1);
			nbuf--;
		}
		else if (format->exp_plus && *cp != '+') {
			for (cp=&cbuf[nbuf]; cp > exp_pos + exp_len[ix]; cp--) 
				*cp = *(cp-1);
			*(exp_pos + exp_len[ix]) = '+';
			nbuf++;
		}
	}

				 
/*
 * Figure out the fill character - either '0' or some other character -
 * (Zeros are handled differently in that preceding fill zeros follow the
 * sign character instead of preceding it).
 */
	if (format->fill_char == 0)
		fill_char = format->zero ? '0' : ' ';
	else
		fill_char = format->fill_char;

/*
 * The apparent field width is the real field width plus the length of
 * function code characters that will not actually be printed
 */
	if (exp_pos == NULL)
		apparent_fwidth = field_width;
	else 
		apparent_fwidth = field_width + len_diff[ix];

/*
 * If the point position is specified it overrides left or right 
 * justification. If the decimal cannot be placed exactly at the desired
 * position it is placed as near to it as possible.
 */

	if (set_point_pos) {
		move_cnt = ppos == NULL ? set_ppos - nbuf - 1:
			set_ppos - (ppos - cbuf) - 1;
		if (move_cnt + nbuf > apparent_fwidth) {
			move_cnt = apparent_fwidth - nbuf;
		}
		if (move_cnt > 0) {
			for (i = nbuf - 1; i >= 0; i--) {
				cbuf[i + move_cnt] = cbuf[i];
			}
			nbuf += move_cnt;
			if (exp_pos != NULL)
				exp_pos += move_cnt;
			for (i = 0; i < move_cnt; i++)
				cbuf[i] = fill_char;
			switch (cbuf[move_cnt]) {
			case '+':
			case '-':
			case ' ':
				cbuf[0] = cbuf[move_cnt];
				cbuf[move_cnt] = fill_char;
				break;
			default:
				break;
			}
		}
	}
	else if (format->minus) {
		left_justify = True;
	}
		
/*
 * If the field width is greater than the number of characters in the buffer,
 * adjust to field width, filling with the appropriate fill character.
 * If there is an exponent, it is necessary to add to the field width the
 * characters used for Plotchar formatting. If filling with zeros on the 
 * right and there is an exponent the zeros must be inserted between the
 * mantissa and the exponent.
 */
	if (nbuf >= apparent_fwidth) {
		cbuf[nbuf] = '\0';
	}
        else {
		if (left_justify || set_point_pos) {
			if (exp_pos != NULL && fill_char == '0') {
				int start = exp_pos - cbuf;
				int len = nbuf - start;
				for (i = 1; i <= nbuf - start; i++)
					cbuf[apparent_fwidth-i] = cbuf[nbuf-i];
				for (i = start; i < apparent_fwidth - len; i++)
					cbuf[i] = fill_char;
			}
			else {
				for (i = nbuf; i < apparent_fwidth; i++)
					cbuf[i] = fill_char;
			}
                }
                else if (fill_char == '0') {
			switch (cbuf[0]) {
			case '+':
			case '-':
			case ' ':
				for (i = 1; i <= nbuf; i++)
					cbuf[apparent_fwidth-i] = cbuf[nbuf-i];
				for (i = 1; i <= apparent_fwidth - nbuf; i++)
					cbuf[i] = fill_char;
				break;
			default:
				for (i = 1; i <= nbuf; i++)
					cbuf[apparent_fwidth-i] = cbuf[nbuf-i];
				for (i = 0; i < apparent_fwidth - nbuf; i++)
					cbuf[i] = fill_char;
				break;
			}
                }
		else {
			for (i = 1; i <= nbuf; i++)
				cbuf[apparent_fwidth-i] = cbuf[nbuf-i];
			for (i = 0; i < apparent_fwidth - nbuf; i++)
				cbuf[i] = fill_char;
		}
		cbuf[apparent_fwidth] = '\0';
        }

	return cbuf;
}


/*
 * Function:	_NhlGetScaleInfo
 *
 * Description:	Returns the power of ten that a floating point number
 *		must be divided by in order to put it into the range
 *		greater or equal to .1 and less than 1.0.
 *		Also returns the number of sig digits necessary to 
 *		express the number precisely (maximum value 8 - due to
 *		floating point precision limits).
 *
 * In Args:	
 *		value - 	float number to be evaluated
 *
 * Out Args:	
 *		int *		div_pwr - divisor power of 10
 *		int *		sig_digits - c sig digits val
 * Scope:	
 * Side Effect:	
 */

NhlErrorTypes _NhlGetScaleInfo
#if	NhlNeedProto
(
	float		value,
	int		*div_pwr,
	int		*sig_digits,
	NhlString	entry_point
)
#else
(value,div_pwr,sig_digits,entry_point)
	float		value;
	int		*div_pwr;
	int		*sig_digits;
	NhlString	entry_point;
#endif
{
        int ndgd,lmsd,iexp,lexp;
        char *cex1 = " ";
        int  lex1 = 0;
        char *cex2 = " ";
        int lex2 =  0;
        char *cex3 = " ";
        int lex3 = 0;
        int ioma,iodp,iotz;
        char cbuf[128];
        int nbuf;
        int ndgs;
        int ieva;
	float tmpf;
	char *cp;

        lmsd = -10000;
        ndgd = 10;
	iexp = 100;
	iexp = 6;
        lexp = 0;
        ioma = 0;
        iodp = 0;
        iotz = 0;

        NGCALLF(cpinrc,CPINRC)();

	{
		int len1,len2,len3,len4;
		NGstring cex1_f;
		NGstring cex2_f;
		NGstring cex3_f;
		NGstring cbuf_f;
		char *cbuf_c;
		len1 = NGSTRLEN(cex1);
		len2 = NGSTRLEN(cex2);
		len3 = NGSTRLEN(cex3);
		len4 = 128;
		cex1_f = NGCstrToFstr(cex1,len1);
		cex2_f = NGCstrToFstr(cex2,len2);
		cex3_f = NGCstrToFstr(cex3,len3);
		cbuf_f = NGCstrToFstr(cbuf,len4);
		NGCALLF(cpnumb,CPNUMB)(&value,&ndgd,&lmsd,&iexp,&lexp,
				       cex1_f,cex2_f,cex3_f,&lex1,&lex2,&lex3,
				       &ioma,&iodp,&iotz,cbuf_f,&nbuf,
				       &ndgs,&ieva,len1,len2,len3,len4);
		cbuf_c = NGFstrToCstr(cbuf_f);
		if (cbuf_c != &cbuf[0])
			strcpy(cbuf,cbuf_c);
		cbuf[nbuf] = '\0';
	}

	*div_pwr = ieva;

	tmpf = fabs(value);
	sprintf(cbuf,"%.6e",tmpf);
	*sig_digits = 0;

	cp = strrchr(cbuf,'e');
	cp--;
	while (*cp == '0' && cp > cbuf) {
		cp--;
	}
	*sig_digits = cp - cbuf;

	return NhlNOERROR;
}
