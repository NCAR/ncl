#!/bin/sh
#
#      $Id: op_funcs.sh,v 1.2 1994-07-27 18:14:16 ethan Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		op_funcs.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 16:14:26 MDT 1994
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

#
# Begin "logical" section
#

if [ $1 = "logical" ]
then

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/THEOP/\&\&/g" \
-e "s/OPER/\.and\./g" \
-e "s/FUNCNAME/And/g" \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/THEOP/\!/g" \
-e "s/OPER/\.not\./g" \
-e "s/FUNCNAME/Not/g" \
MultiDValMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/THEOP/||/g" \
-e "s/OPER/\.or\./g" \
-e "s/FUNCNAME/Or/g" \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/OPER/\.xor\./g" \
-e "s/FUNCNAME/Xor/g" \
MultiDValXorOpTemplate.c.sed

sed \
-e "s/DATATYPE/logical/g" \
MultiDValSelectFuncsTemplate.c.sed

exit 0

fi

#
# Begin "string" section
#
if [ $1 = "string" ]
then

sed \
-e 's/SELFUNC/select_string_gt/g' \
-e 's/FUNCNAME/SelGt/' \
string_sel.c.sed

sed \
-e 's/SELFUNC/select_string_lt/g' \
-e 's/FUNCNAME/SelLt/' \
string_sel.c.sed

sed \
-e 's/CMPFUNC/cmp_string_lt/g' \
-e 's/FUNCNAME/Lt/' \
string_cmp.c.sed

sed \
-e 's/CMPFUNC/cmp_string_gt/g' \
-e 's/FUNCNAME/Gt/' \
string_cmp.c.sed

sed \
-e 's/CMPFUNC/cmp_string_ge/g' \
-e 's/FUNCNAME/Ge/' \
string_cmp.c.sed

sed \
-e 's/CMPFUNC/cmp_string_le/g' \
-e 's/FUNCNAME/Le/' \
string_cmp.c.sed

sed \
-e 's/DATATYPE/string/g' \
MultiDValSelectFuncsTemplate.c.sed

exit 0

fi

#
# Begin "nclfile" section
#
if [ $1 = "nclfile" ]
then

sed \
-e "s/OUTDATATYPE/nclfile/g" \
-e "s/DATATYPE/nclfile/g" \
-e "s/THEOP/==/g" \
-e "s/OPER/\.eq\./g" \
-e "s/FUNCNAME/Eq/g" \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/DATATYPE/nclfile/g" \
MultiDValSelectFuncsTemplate.c.sed

exit 0

fi

if [ $1 = "char" ]
then
sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/+/' \
-e 's/OPER/\+/' \
-e 's/FUNCNAME/Plus/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/</' \
-e 's/OPER/\.lt\./' \
-e 's/FUNCNAME/Lt/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/>/' \
-e 's/OPER/\.gt\./' \
-e 's/FUNCNAME/Gt/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/<=/' \
-e 's/OPER/\.le\./' \
-e 's/FUNCNAME/Le/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/>=/' \
-e 's/OPER/\.ge\./' \
-e 's/FUNCNAME/Ge/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/==/' \
-e 's/OPER/\.eq\./' \
-e 's/FUNCNAME/Eq/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/!=/' \
-e 's/OPER/\.ne\./' \
-e 's/FUNCNAME/Ne/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\</' \
-e 's/OPER/\</' \
-e 's/FUNCNAME/SelLt/' \
MultiDValSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\>/' \
-e 's/OPER/\>/' \
-e 's/FUNCNAME/SelGt/' \
MultiDValSelectOpTemplate.c.sed

sed \
-e "s/DATATYPE/$1/g" \
MultiDValSelectFuncsTemplate.c.sed

exit 0
fi

#
# Begin "numeric" section
#

sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/+/' \
-e 's/OPER/\+/' \
-e 's/FUNCNAME/Plus/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/-/' \
-e 's/OPER/\-/' \
-e 's/FUNCNAME/Minus/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/*/' \
-e 's/OPER/\*/' \
-e 's/FUNCNAME/Mul/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/</' \
-e 's/OPER/\.lt\./' \
-e 's/FUNCNAME/Lt/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/>/' \
-e 's/OPER/\.gt\./' \
-e 's/FUNCNAME/Gt/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/<=/' \
-e 's/OPER/\.le\./' \
-e 's/FUNCNAME/Le/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/>=/' \
-e 's/OPER/\.ge\./' \
-e 's/FUNCNAME/Ge/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/==/' \
-e 's/OPER/\.eq\./' \
-e 's/FUNCNAME/Eq/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/!=/' \
-e 's/OPER/\.ne\./' \
-e 's/FUNCNAME/Ne/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\&\&/' \
-e 's/OPER/\.and\./' \
-e 's/FUNCNAME/And/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/||/' \
-e 's/OPER/\.or\./' \
-e 's/FUNCNAME/Or/' \
MultiDValSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/FNAME/pow/' \
-e 's/OPER/\^/' \
-e 's/FUNCNAME/Exp/' \
-e 's/CAST/double/g' \
-e 's/INCLUDE/math/' \
MultiDValFunctionOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\!/' \
-e 's/OPER/\.not\./' \
-e 's/FUNCNAME/Not/' \
MultiDValMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\-/' \
-e 's/OPER/neg/' \
-e 's/FUNCNAME/Neg/' \
MultiDValMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\</' \
-e 's/OPER/\</' \
-e 's/FUNCNAME/SelLt/' \
MultiDValSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\>/' \
-e 's/OPER/\>/' \
-e 's/FUNCNAME/SelGt/' \
MultiDValSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/OPER/\.xor\./' \
-e 's/FUNCNAME/Xor/' \
MultiDValXorOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\//' \
-e 's/FUNCNAME/Div/' \
MultiDValDivOpTemplate.c.sed

sed \
-e "s/DATATYPE/$1/g" \
MultiDValSelectFuncsTemplate.c.sed

if [ \( $1 = "int" \) -o \( $1 = "long" \) -o \( $1 = "short" \) ]
then
sed \
-e "s/OUTDATATYPE/int/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\%/' \
-e 's/OPER/mod/' \
-e 's/FUNCNAME/Mod/' \
MultiDValDivOpTemplate.c.sed
fi

exit 0
