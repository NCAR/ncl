#!/bin/sh

rm -f NclTypestring.c

sh op_funcs.sh string NhlTString NhlTStringGenArray -1 > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

cat NclTypestring.c.specific >> .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e "/INSERTTMPSTRING/r .tmp.$$" \
-e '/INSERTTMPSTRING/d' \
-e 's/HLUTYPEREP/NhlTQuark/g' \
-e 's/HLUGENTYPEREP/NhlTQuarkGenArray/g' \
NclTypestring.c.sed > NclTypestring.c

if [ ! $? ]
then
	exit $?
fi

cp NclTypestring.c .tmp.$$
sed \
-e 's/(string)/(NclQuark)/g' \
-e 's/string\*/NclQuark \*/g' \
-e 's/string \*/NclQuark \*/g' \
.tmp.$$ > NclTypestring.c

rm .tmp.$$
rm -f NclTypestring.c.tmp

echo "created NclTypestring.c"

exit 0
