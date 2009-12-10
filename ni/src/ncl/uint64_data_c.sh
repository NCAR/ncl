#!/bin/sh

sh op_funcs.sh uint64 NhlTUint64 NhlTUint64GenArray 0 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%ld\\n/' \
-e 's/DATATYPE/uint64/g' \
-e 's/LOCALTYPE/unsigned long long/g' \
-e 's/HLUTYPEREP/NhlTUint64/g' \
-e 's/HLUGENTYPEREP/NhlTUint64GenArray/g' \
-e 's/DEFAULT_MISS/0/g' \
-e 's/DEFAULT_FORMAT/%lld/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeuint64.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeuint64.c

rm .tmp.$$

echo "created NclTypeuint64.c"

exit 0
