
#
#      $Id: type_support_c.sh,v 1.2 1997-09-02 20:27:38 ethan Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1995				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		
#
#	Author:		Ethan Alpert
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Fri Jan 27 18:33:47 MST 1995
#
#	Description:	
#
#	Usage:
#
#	Environment:
#
#	Files:
#
#
#	Options:
#!/bin/sh


sed \
-e 's/TFUNC/multiply/g' \
TypeSupportOpTemplate.c.sed > .tmp.$$

sed \
-e 's/TFUNC/divide/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/plus/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/minus/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/exponent/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/mod/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/sel_lt/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/sel_gt/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/gt/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/lt/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/ge/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/le/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/ne/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/eq/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/and/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/or/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/xor/g' \
TypeSupportOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/not/g' \
TypeSupportMonoOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/neg/g' \
TypeSupportMonoOpTemplate.c.sed >> .tmp.$$

sed \
-e 's/TFUNC/mat/g' \
TypeSupportMatTemplate.c.sed >> .tmp.$$


if [ ! $? ]
then
	exit $?
fi

sed \
-e "/INSERTHERE/r .tmp.$$" \
-e '/INSERTHERE/d' \
TypeSupport.c.sed > TypeSupport.c

rm .tmp.$$

echo "created TypeSupport.c"

exit 0
