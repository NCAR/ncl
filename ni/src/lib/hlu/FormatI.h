/*
 *      $Id: FormatI.h,v 1.3 1996-01-19 18:06:29 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		FormatI.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 12 17:29:07 MDT 1994
 *
 *	Description:	
 */
#ifndef	_NFORMATI_H
#define	_NFORMATI_H

#include <ncarg/hlu/hlu.h>


typedef enum _NhlExponentType {
	NhlffELITTLE, NhlffEBIG, NhlffASTERISK, NhlffSUPERSCRIPT 
} NhlExponentType;

typedef enum _NhlFieldtype { 
	NhlffFLOATFLD, NhlffEXPONENTIALFLD, NhlffGENERICFLD } 
NhlFieldType;

typedef enum _NhlffStat {
	NhlffUNSPECED = 0, NhlffDYNAMIC, NhlffEXPLICIT
} NhlffStat;

typedef struct _NhlFormatRec {
	/* the format string */
	NhlString  fstring; 

	/* flags */
	NhlBoolean plus;	/* force leading plus sign */
	NhlBoolean minus;	/* left justify number in the field */
	NhlBoolean pound;	/* force trailing decimal */
	NhlBoolean zero;	/* 1) fill with zeros to account for all
				significant digits; 2) then if field width >
				current string len, fill to field width, left
				or right depending on justification. */
	NhlBoolean space;	/* force leading space for positive #'s if
				no plus */
	NhlBoolean exclamation; /* force initial mantissa's of 1(.0x) */
	NhlBoolean comma;	/* change decimal sign from dot to comma */
	NhlBoolean at_sign;     /* force initial or trailing 0 where decimal
				point is first or last character */

	char		fill_char;

	NhlffStat	field_width_flag;
	int		field_width;

	NhlffStat	sig_digits_flag;
	int		sig_digits;

	NhlffStat	left_sig_digit_flag;
	int		left_sig_digit;

	NhlffStat	point_position_flag;
	int		point_position;

	NhlffStat	exp_switch_flag;
	int	  	exp_switch_len;

	NhlffStat	exp_field_width_flag;
	int		exp_field_width;

        NhlBoolean	exp_plus;

	NhlffStat	exp_type_flag;
	NhlExponentType exp_type;

	NhlFieldType field_type;
	/* pointer to next character after format info in the input string */
        char	   *next_char;
} NhlFormatRec;

extern NhlFormatRec *_NhlScanFString(
#if  NhlNeedProto
	NhlString	fstring,
	NhlString	entry_name
#endif
);

extern NhlString _NhlFormatFloat(
#if  NhlNeedProto
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
#endif
);


extern NhlErrorTypes _NhlGetScaleInfo(
#if  NhlNeedProto
	float		value,
	int		*div_pwr,
	int		*sig_digits,
	NhlString	entry_point
#endif
);

#endif	/* _NFORMATI_H */



