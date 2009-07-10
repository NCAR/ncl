#!/bin/sh

sh op_funcs.sh logical NhlTBoolean NhlTBooleanGenArray -1 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e '/PRINTFORMAT/d' \
-e 's/DATATYPE/logical/g' \
-e 's/LOCALTYPE/logical/g' \
-e 's/HLUTYPEREP/NhlTBoolean/g' \
-e 's/HLUGENTYPEREP/NhlTBooleanGenArray/g' \
-e 's/DEFAULT_MISS/-1/g' \
-e 's/DEFAULT_MISS/%d/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypelogical.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/Ncl_Type_logical_mat_type/NULL/' \
-e 's/Ncl_Type_logical_mat/NULL/' \
-e 's/Ncl_Type_logical_mod_type/NULL/' \
-e 's/Ncl_Type_logical_mod/NULL/' \
-e 's/Ncl_Type_logical_divide_type/NULL/' \
-e 's/Ncl_Type_logical_divide/NULL/' \
-e 's/Ncl_Type_logical_multiply_type/NULL/' \
-e 's/Ncl_Type_logical_multiply/NULL/' \
-e 's/Ncl_Type_logical_plus_type/NULL/' \
-e 's/Ncl_Type_logical_plus/NULL/' \
-e 's/Ncl_Type_logical_minus_type/NULL/' \
-e 's/Ncl_Type_logical_minus/NULL/' \
-e 's/Ncl_Type_logical_exponent_type/NULL/' \
-e 's/Ncl_Type_logical_exponent/NULL/' \
-e 's/Ncl_Type_logical_sel_lt_type/NULL/' \
-e 's/Ncl_Type_logical_sel_lt/NULL/' \
-e 's/Ncl_Type_logical_sel_gt_type/NULL/' \
-e 's/Ncl_Type_logical_sel_gt/NULL/' \
-e 's/Ncl_Type_logical_neg_type/NULL/' \
-e 's/Ncl_Type_logical_neg/NULL/' \
-e 's/Ncl_Type_logical_gt_type/NULL/' \
-e 's/Ncl_Type_logical_gt/NULL/' \
-e 's/Ncl_Type_logical_lt_type/NULL/' \
-e 's/Ncl_Type_logical_lt/NULL/' \
-e 's/Ncl_Type_logical_ge_type/NULL/' \
-e 's/Ncl_Type_logical_ge/NULL/' \
-e 's/Ncl_Type_logical_le_type/NULL/' \
-e 's/Ncl_Type_logical_le/NULL/' \
-e 's/Ncl_Type_logical_is_mono/NULL/' \
-e 's/Ncl_Type_logical_cmpf/NULL/' \
NclType.c.sed > NclTypelogical.c

rm .tmp.$$

echo "created NclTypelogical.c"

exit 0
