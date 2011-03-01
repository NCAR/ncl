#!/bin/sh

sh op_funcs.sh byte NhlTByte NhlTByteGenArray \(char\)-127 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%c\\n/' \
-e 's/DATATYPE/byte/g' \
-e 's/LOCALTYPE/byte/g' \
-e 's/HLUTYPEREP/NhlTByte/g' \
-e 's/HLUGENTYPEREP/NhlTByteGenArray/g' \
-e "s/DEFAULT_MISS/-127/g" \
-e "s/DEFAULT_FORMAT/%hhd/g" \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypebyte.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/Ncl_Type_byte_mod_type/NULL/' \
-e 's/Ncl_Type_byte_mod/NULL/' \
NclType.c.sed > NclTypebyte.c

rm .tmp.$$

echo "created NclTypebyte.c"
