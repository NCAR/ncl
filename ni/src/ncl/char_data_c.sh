#!/bin/sh
#
#      $Id: char_data_c.sh,v 1.7 2009-07-10 19:54:06 huangwei Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		int_data_c.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 17:21:13 MDT 1994
#
#	Description:	
#
#	Usage:
#
#	Environment:
#
#	Files:
#
#
#	Options:

sh op_funcs.sh  char NhlTCharacter NhlTCharacterGenArray \(char\)0 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%c\\n/' \
-e 's/DATATYPE/char/g' \
-e 's/LOCALTYPE/char/g' \
-e 's/HLUTYPEREP/NhlTCharacter/g' \
-e 's/HLUGENTYPEREP/NhlTCharacterGenArray/g' \
-e 's/DEFAULT_MISS/'\0'/g' \
-e 's/DEFAULT_FORMAT/%c/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypechar.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/Ncl_Type_char_mat_type/NULL/' \
-e 's/Ncl_Type_char_mat/NULL/' \
-e 's/Ncl_Type_char_mod_type/NULL/' \
-e 's/Ncl_Type_char_mod/NULL/' \
-e 's/Ncl_Type_char_divide_type/NULL/' \
-e 's/Ncl_Type_char_divide/NULL/' \
-e 's/Ncl_Type_char_multiply_type/NULL/' \
-e 's/Ncl_Type_char_multiply/NULL/' \
-e 's/Ncl_Type_char_minus_type/NULL/' \
-e 's/Ncl_Type_char_minus/NULL/' \
-e 's/Ncl_Type_char_exponent_type/NULL/' \
-e 's/Ncl_Type_char_exponent/NULL/' \
-e 's/Ncl_Type_char_neg_type/NULL/' \
-e 's/Ncl_Type_char_neg/NULL/' \
-e 's/Ncl_Type_char_not_type/NULL/' \
-e 's/Ncl_Type_char_not/NULL/' \
-e 's/Ncl_Type_char_and_type/NULL/' \
-e 's/Ncl_Type_char_and/NULL/' \
-e 's/Ncl_Type_char_or_type/NULL/' \
-e 's/Ncl_Type_char_or/NULL/' \
-e 's/Ncl_Type_char_xor_type/NULL/' \
-e 's/Ncl_Type_char_xor/NULL/' \
NclType.c.sed > NclTypechar.c

rm .tmp.$$

echo "created NclTypechar.c"
