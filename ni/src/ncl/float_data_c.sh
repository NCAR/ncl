#!/bin/sh

sh op_funcs.sh float NhlTFloat NhlTFloatGenArray -999.0 > .tmp.$$

if [  ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%g\\n/' \
-e 's/DATATYPE/float/g' \
-e 's/HLUTYPEREP/NhlTFloat/g' \
-e 's/HLUGENTYPEREP/NhlTFloatGenArray/g' \
-e 's/DEFAULT_MISS/-9999.0/g' \
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
