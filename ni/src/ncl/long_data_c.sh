#!/bin/sh

sh op_funcs.sh long NhlTLong NhlTLongGenArray -9999 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%ld\\n/' \
-e 's/DATATYPE/long/g' \
-e 's/HLUTYPEREP/NhlTLong/g' \
-e 's/HLUGENTYPEREP/NhlTLongGenArray/g' \
-e 's/DEFAULT_MISS/-9999/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypelong.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/Ncl_Type_long_mat_type/NULL/' \
-e 's/Ncl_Type_long_mat/NULL/' \
NclType.c.sed > NclTypelong.c

rm .tmp.$$

echo "created NclTypelong.c"

exit 0
