#!/bin/sh

sh op_funcs.sh byte NhlTByte NhlTByteGenArray > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%c\\n/' \
-e 's/DATATYPE/byte/g' \
-e 's/HLUTYPEREP/NhlTByte/g' \
-e 's/HLUGENTYPEREP/NhlTByteGenArray/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclTypebyte.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/Ncl_Type_byte_mat_type/NULL/' \
-e 's/Ncl_Type_byte_mat/NULL/' \
-e 's/Ncl_Type_byte_mod_type/NULL/' \
-e 's/Ncl_Type_byte_mod/NULL/' \
-e 's/Ncl_Type_byte_coerce/NULL/' \
NclType.c.sed > NclTypebyte.c

rm .tmp.$$

echo "created NclTypebyte.c"
