#!/bin/sh

sh op_funcs.sh int NhlTInteger NhlTIntegerGenArray -2147483647 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e 's/DATATYPE/int/g' \
-e 's/LOCALTYPE/int/g' \
-e 's/HLUTYPEREP/NhlTInteger/g' \
-e 's/HLUGENTYPEREP/NhlTIntegerGenArray/g' \
-e 's/DEFAULT_MISS/-2147483647/g' \
-e 's/DEFAULT_FORMAT/%d/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeint.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeint.c

rm .tmp.$$

echo "created NclTypeint.c"
