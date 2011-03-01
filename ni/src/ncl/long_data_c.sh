#!/bin/sh

sh op_funcs.sh long NhlTLong NhlTLongGenArray -2147483647 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%ld\\n/' \
-e 's/DATATYPE/long/g' \
-e 's/LOCALTYPE/long/g' \
-e 's/HLUTYPEREP/NhlTLong/g' \
-e 's/HLUGENTYPEREP/NhlTLongGenArray/g' \
-e 's/DEFAULT_MISS/-2147483647/g' \
-e 's/DEFAULT_FORMAT/%ld/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypelong.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypelong.c

rm .tmp.$$

echo "created NclTypelong.c"

exit 0
