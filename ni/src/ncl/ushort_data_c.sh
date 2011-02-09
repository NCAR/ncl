#!/bin/sh

sh op_funcs.sh ushort NhlTUshort NhlTUshortGenArray 65535U > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e 's/DATATYPE/ushort/g' \
-e 's/LOCALTYPE/unsigned short/g' \
-e 's/HLUTYPEREP/NhlTUshort/g' \
-e 's/HLUGENTYPEREP/NhlTUshortGenArray/g' \
-e 's/DEFAULT_MISS/65535U/g' \
-e 's/DEFAULT_FORMAT/%hu/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeushort.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeushort.c

rm .tmp.$$

echo "created NclTypeushort.c"

exit 0
