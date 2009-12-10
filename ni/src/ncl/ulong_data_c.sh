#!/bin/sh

sh op_funcs.sh ulong NhlTUlong NhlTUlongGenArray 0 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%ld\\n/' \
-e 's/DATATYPE/ulong/g' \
-e 's/LOCALTYPE/unsigned long/g' \
-e 's/HLUTYPEREP/NhlTUlong/g' \
-e 's/HLUGENTYPEREP/NhlTUlongGenArray/g' \
-e 's/DEFAULT_MISS/0/g' \
-e 's/DEFAULT_FORMAT/%ld/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeulong.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeulong.c

rm .tmp.$$

echo "created NclTypeulong.c"

exit 0
