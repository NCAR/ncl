#!/bin/sh

sh op_funcs.sh short NhlTShort NhlTShortGenArray \(short\)-32767 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e 's/DATATYPE/short/g' \
-e 's/LOCALTYPE/short/g' \
-e 's/HLUTYPEREP/NhlTShort/g' \
-e 's/HLUGENTYPEREP/NhlTShortGenArray/g' \
-e 's/DEFAULT_MISS/-32767/g' \
-e 's/DEFAULT_FORMAT/%hd/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeshort.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeshort.c

rm .tmp.$$

echo "created NclTypeshort.c"

exit 0
