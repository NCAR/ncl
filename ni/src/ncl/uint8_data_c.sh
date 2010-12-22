#!/bin/sh

sh op_funcs.sh uint8 NhlTUint8 NhlTUint8GenArray 255 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%u\\n/' \
-e 's/DATATYPE/uint8/g' \
-e 's/LOCALTYPE/unsigned char/g' \
-e 's/HLUTYPEREP/NhlTUint8/g' \
-e 's/HLUGENTYPEREP/NhlTUint8GenArray/g' \
-e 's/DEFAULT_MISS/255/g' \
-e 's/DEFAULT_FORMAT/%hhu/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeuint8.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeuint8.c

rm .tmp.$$

echo "created NclTypeuint8.c"

exit 0
