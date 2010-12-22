#!/bin/sh

sh op_funcs.sh float NhlTFloat NhlTFloatGenArray 9.9692099683868690e+36f > .tmp.$$

if [  ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%g\\n/' \
-e 's/DATATYPE/float/g' \
-e 's/LOCALTYPE/float/g' \
-e 's/HLUTYPEREP/NhlTFloat/g' \
-e 's/HLUGENTYPEREP/NhlTFloatGenArray/g' \
-e 's/DEFAULT_MISS/9.9692099683868690e+36f/g' \
-e 's/DEFAULT_FORMAT/%f/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypefloat.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/Ncl_Type_float_mod_type/NULL/' \
-e 's/Ncl_Type_float_mod/NULL/' \
NclType.c.sed > NclTypefloat.c

rm .tmp.$$

echo "created NclTypefloat.c"

exit 0
