#!/bin/sh

sh op_funcs.sh float NhlTFloat NhlTFloatGenArray > .tmp.$$

if [  ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%g\\n/' \
-e 's/DATATYPE/float/g' \
-e 's/HLUTYPEREP/NhlTFloat/g' \
-e 's/HLUGENTYPEREP/NhlTFloatGenArray/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypefloat.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/Ncl_Type_float_mat_type/NULL/' \
-e 's/Ncl_Type_float_mat/NULL/' \
-e 's/Ncl_Type_float_mod_type/NULL/' \
-e 's/Ncl_Type_float_mod/NULL/' \
NclType.c.sed > NclTypefloat.c

rm .tmp.$$

echo "created NclTypefloat.c"

exit 0
