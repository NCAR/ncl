#!/bin/sh

sh op_funcs.sh ubyte NhlTUbyte NhlTUbyteGenArray 255 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%u\\n/' \
-e 's/DATATYPE/ubyte/g' \
-e 's/LOCALTYPE/unsigned char/g' \
-e 's/HLUTYPEREP/NhlTUbyte/g' \
-e 's/HLUGENTYPEREP/NhlTUbyteGenArray/g' \
-e 's/DEFAULT_MISS/255/g' \
-e 's/DEFAULT_FORMAT/%hhu/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeubyte.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeubyte.c

rm .tmp.$$

echo "created NclTypeubyte.c"

exit 0
