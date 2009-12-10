#!/bin/sh

sh op_funcs.sh ushort NhlTUshort NhlTUshortGenArray 0 > .tmp.$$

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
-e 's/DEFAULT_MISS/0/g' \
-e 's/DEFAULT_FORMAT/%hd/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeushort.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeushort.c

rm .tmp.$$

echo "created NclTypeushort.c"

exit 0
