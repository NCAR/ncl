
#
#      $Id: multid_data_c.sh,v 1.2 1997-09-02 20:27:26 ethan Exp $
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
#	Date:		Fri Jan 27 18:33:33 MST 1995
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
-e 's/FUNCNAME/Mul/g' \
-e 's/TFUNC/multiply/g' \
MultiDValOp.c.sed > .tmp.$$

sed \
-e 's/FUNCNAME/Div/g' \
-e 's/TFUNC/divide/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Plus/g' \
-e 's/TFUNC/plus/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Minus/g' \
-e 's/TFUNC/minus/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Exp/g' \
-e 's/TFUNC/exponent/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Mod/g' \
-e 's/TFUNC/mod/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/SelLt/g' \
-e 's/TFUNC/sel_lt/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/SelGt/g' \
-e 's/TFUNC/sel_gt/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Gt/g' \
-e 's/TFUNC/gt/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Lt/g' \
-e 's/TFUNC/lt/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Ge/g' \
-e 's/TFUNC/ge/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Le/g' \
-e 's/TFUNC/le/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Ne/g' \
-e 's/TFUNC/ne/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Eq/g' \
-e 's/TFUNC/eq/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/And/g' \
-e 's/TFUNC/and/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Or/g' \
-e 's/TFUNC/or/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Xor/g' \
-e 's/TFUNC/xor/g' \
MultiDValOp.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/Mat/g' \
MatMulDValOp.c.sed >> .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e "/INSERTHERE/r .tmp.$$" \
-e '/INSERTHERE/d' \
NclMultiDValData.c.sed > NclMultiDValData.c

rm .tmp.$$

echo "created NclMultiDValData.c"

exit 0
