#!/bin/sh

sh op_funcs.sh uint NhlTUint NhlTUintGenArray 0 > .tmp.$$

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
-e 's/DEFAULT_MISS/0/g' \
-e 's/DEFAULT_FORMAT/%d/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypeuint.c.specific' \
-e '/DSPECIFIC/d' \
NclType.c.sed > NclTypeuint.c

rm .tmp.$$

echo "created NclTypeuint.c"
