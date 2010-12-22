#!/bin/sh

sh op_funcs.sh int8 NhlTInt8 NhlTInt8GenArray -127 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e 's/DATATYPE/int8/g' \
-e 's/LOCALTYPE/char/g' \
-e 's/HLUTYPEREP/NhlTInt8/g' \
-e 's/HLUGENTYPEREP/NhlTInt8GenArray/g' \
-e 's/DEFAULT_MISS/-127/g' \
-e 's/DEFAULT_FORMAT/%d/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeint8.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeint8.c

rm .tmp.$$

echo "created NclTypeint8.c"

exit 0
