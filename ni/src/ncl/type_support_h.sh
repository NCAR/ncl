#!/bin/sh
#
#      $Id: type_support_h.sh,v 1.2 1997-09-02 20:27:41 ethan Exp $
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
#	Date:		Fri Jan 27 18:33:52 MST 1995
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



sed \
-e 's/TFUNC/multiply/g' \
TypeSupportOpTemplate.h.sed > .tmp.$$

sed \
-e 's/TFUNC/divide/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/plus/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/minus/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/exponent/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/mod/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/sel_lt/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/sel_gt/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/gt/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/lt/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/ge/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/le/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/ne/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/eq/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/ncl_and/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/ncl_or/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/ncl_xor/g' \
TypeSupportOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/ncl_not/g' \
TypeSupportMonoOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/neg/g' \
TypeSupportMonoOpTemplate.h.sed >> .tmp.$$

sed \
-e 's/TFUNC/mat/g' \
TypeSupportMatTemplate.h.sed >> .tmp.$$



if [ ! $? ]
then
	exit $?
fi

sed \
-e "/INSERTHERE/r .tmp.$$" \
-e '/INSERTHERE/d' \
TypeSupport.h.sed > .tmp2.$$

sed \
-e 's/ncl_and_type/and_type/g' \
-e 's/ncl_not_type/not_type/g' \
-e 's/ncl_xor_type/xor_type/g' \
-e 's/ncl_or_type/or_type/g' \
.tmp2.$$ > TypeSupport.h

rm .tmp.$$ .tmp2.$$

echo "created TypeSupport.h"

exit 0
