#!/bin/sh

sh op_funcs.sh ulong NhlTUlong NhlTUlongGenArray 4294967295U > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%lu\\n/' \
-e 's/DATATYPE/ulong/g' \
-e 's/LOCALTYPE/unsigned long/g' \
-e 's/HLUTYPEREP/NhlTUlong/g' \
-e 's/HLUGENTYPEREP/NhlTUlongGenArray/g' \
-e 's/DEFAULT_MISS/4294967295U/g' \
-e 's/DEFAULT_FORMAT/%lu/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeulong.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeulong.c

rm .tmp.$$

echo "created NclTypeulong.c"

exit 0
