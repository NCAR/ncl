#!/bin/sh

sh op_funcs.sh int64 NhlTInt64 NhlTInt64GenArray '\(long long\)-9223372036854775806LL' > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%lld\\n/' \
-e 's/DATATYPE/int64/g' \
-e 's/LOCALTYPE/long long/g' \
-e 's/HLUTYPEREP/NhlTInt64/g' \
-e 's/HLUGENTYPEREP/NhlTInt64GenArray/g' \
-e 's/DEFAULT_MISS/(long long)-9223372036854775806LL/g' \
-e 's/DEFAULT_FORMAT/%lld/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeint64.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeint64.c

rm .tmp.$$

echo "created NclTypeint64.c"

exit 0
