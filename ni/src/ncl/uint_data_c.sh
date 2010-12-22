#!/bin/sh

sh op_funcs.sh uint NhlTUint NhlTUintGenArray 4294967295U > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e 's/DATATYPE/uint/g' \
-e 's/LOCALTYPE/unsigned int/g' \
-e 's/HLUTYPEREP/NhlTUint/g' \
-e 's/HLUGENTYPEREP/NhlTUintGenArray/g' \
-e 's/DEFAULT_MISS/4294967295U/g' \
-e 's/DEFAULT_FORMAT/%u/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeuint.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeuint.c

rm .tmp.$$

echo "created NclTypeuint.c"
