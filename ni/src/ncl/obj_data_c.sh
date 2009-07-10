#!/bin/sh

#
#      $Id: obj_data_c.sh,v 1.6 2009-07-10 19:54:06 huangwei Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1995				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		
#
#	Author:		Ethan Alpert
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Fri Jan 27 18:34:11 MST 1995
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
sh op_funcs.sh obj NULL NULL -1 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e '/PRINTFORMAT/d' \
-e 's/DATATYPE/obj/g' \
-e 's/LOCALTYPE/obj/g' \
-e 's/HLUTYPEREP/NULL/g' \
-e 's/HLUGENTYPEREP/NULL/g' \
-e 's/DEFAULT_MISS/-1/g' \
-e 's/DEFAULT_FORMAT/%d/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeobj.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/Ncl_Type_obj_not_type/NULL/' \
-e 's/Ncl_Type_obj_not/NULL/' \
-e 's/Ncl_Type_obj_xor_type/NULL/' \
-e 's/Ncl_Type_obj_xor/NULL/' \
-e 's/Ncl_Type_obj_or_type/NULL/' \
-e 's/Ncl_Type_obj_or/NULL/' \
-e 's/Ncl_Type_obj_and_type/NULL/' \
-e 's/Ncl_Type_obj_and/NULL/' \
-e 's/Ncl_Type_obj_mat_type/NULL/' \
-e 's/Ncl_Type_obj_mat/NULL/' \
-e 's/Ncl_Type_obj_mod_type/NULL/' \
-e 's/Ncl_Type_obj_mod/NULL/' \
-e 's/Ncl_Type_obj_divide_type/NULL/' \
-e 's/Ncl_Type_obj_divide/NULL/' \
-e 's/Ncl_Type_obj_multiply_type/NULL/' \
-e 's/Ncl_Type_obj_multiply/NULL/' \
-e 's/Ncl_Type_obj_plus_type/NULL/' \
-e 's/Ncl_Type_obj_plus/NULL/' \
-e 's/Ncl_Type_obj_minus_type/NULL/' \
-e 's/Ncl_Type_obj_minus/NULL/' \
-e 's/Ncl_Type_obj_exponent_type/NULL/' \
-e 's/Ncl_Type_obj_exponent/NULL/' \
-e 's/Ncl_Type_obj_sel_lt_type/NULL/' \
-e 's/Ncl_Type_obj_sel_lt/NULL/' \
-e 's/Ncl_Type_obj_sel_gt_type/NULL/' \
-e 's/Ncl_Type_obj_sel_gt/NULL/' \
-e 's/Ncl_Type_obj_neg_type/NULL/' \
-e 's/Ncl_Type_obj_neg/NULL/' \
-e 's/Ncl_Type_obj_gt_type/NULL/' \
-e 's/Ncl_Type_obj_gt/NULL/' \
-e 's/Ncl_Type_obj_lt_type/NULL/' \
-e 's/Ncl_Type_obj_lt/NULL/' \
-e 's/Ncl_Type_obj_ge_type/NULL/' \
-e 's/Ncl_Type_obj_ge/NULL/' \
-e 's/Ncl_Type_obj_le_type/NULL/' \
-e 's/Ncl_Type_obj_le/NULL/' \
-e 's/Ncl_Type_obj_is_mono/NULL/' \
-e 's/Ncl_Type_obj_coerce/NULL/' \
-e 's/Ncl_Type_obj_cmpf/NULL/' \
NclType.c.sed > NclTypeobj.c

rm .tmp.$$

echo "created NclTypeobj.c"

exit 0
