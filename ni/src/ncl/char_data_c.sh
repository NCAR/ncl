#!/bin/sh
#
#      $Id: char_data_c.sh,v 1.1 1994-07-27 18:15:12 ethan Exp $
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

sh op_funcs.sh  char > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%c\\n/' \
-e 's/DATATYPE/char/g' \
-e 's/HLUTYPEREP/NhlTCharacter/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValcharData.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/MultiDVal_char_mdmd_Mod/NULL/' \
-e 's/MultiDVal_char_smd_Mod/NULL/' \
-e 's/MultiDVal_char_mds_Mod/NULL/' \
-e 's/MultiDVal_char_ss_Mod/NULL/' \
-e 's/MultiDVal_char_mdmd_Div/NULL/' \
-e 's/MultiDVal_char_smd_Div/NULL/' \
-e 's/MultiDVal_char_mds_Div/NULL/' \
-e 's/MultiDVal_char_ss_Div/NULL/' \
-e 's/MultiDVal_char_mdmd_Mul/NULL/' \
-e 's/MultiDVal_char_smd_Mul/NULL/' \
-e 's/MultiDVal_char_mds_Mul/NULL/' \
-e 's/MultiDVal_char_ss_Mul/NULL/' \
-e 's/MultiDVal_char_mdmd_Minus/NULL/' \
-e 's/MultiDVal_char_smd_Minus/NULL/' \
-e 's/MultiDVal_char_mds_Minus/NULL/' \
-e 's/MultiDVal_char_ss_Minus/NULL/' \
-e 's/MultiDVal_char_mdmd_Exp/NULL/' \
-e 's/MultiDVal_char_smd_Exp/NULL/' \
-e 's/MultiDVal_char_mds_Exp/NULL/' \
-e 's/MultiDVal_char_ss_Exp/NULL/' \
-e 's/MultiDVal_char_md_Neg/NULL/' \
-e 's/MultiDVal_char_s_Neg/NULL/' \
-e 's/MultiDVal_char_md_Not/NULL/' \
-e 's/MultiDVal_char_s_Not/NULL/' \
-e 's/MultiDVal_char_mdmd_And/NULL/' \
-e 's/MultiDVal_char_mds_And/NULL/' \
-e 's/MultiDVal_char_smd_And/NULL/' \
-e 's/MultiDVal_char_ss_And/NULL/' \
-e 's/MultiDVal_char_mdmd_Or/NULL/' \
-e 's/MultiDVal_char_mds_Or/NULL/' \
-e 's/MultiDVal_char_smd_Or/NULL/' \
-e 's/MultiDVal_char_ss_Or/NULL/' \
-e 's/MultiDVal_char_mdmd_Xor/NULL/' \
-e 's/MultiDVal_char_mds_Xor/NULL/' \
-e 's/MultiDVal_char_smd_Xor/NULL/' \
-e 's/MultiDVal_char_ss_Xor/NULL/' \
NclMultiDValData.c.sed > NclMultiDValcharData.c

rm .tmp.$$

echo "created NclMultiDValcharData.c"
