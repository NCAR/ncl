#!/bin/sh

#
# Begin "logical" section
#

sed \
-e "s/DATATYPE/$1/g" \
TypeResetMissing.c.sed

if [ $1 = "logical" ]
then

sed \
-e "s/HLUGENTYPEREP/$3/g" \
-e "s/HLUTYPEREP/$2/g" \
-e "s/DATATYPE/logical/g" \
-e "s/DEFAULT_MISS/$4/g" \
TypeInitClassTemplate.c.sed


sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/THEOP/\&\&/g" \
-e "s/OPER/\.and\./g" \
-e "s/FUNCNAME/and/g" \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/OUTMISSING/lhs_m->logicalval/g" \
-e "s/THEOP/\!/g" \
-e "s/OPER/\.not\./g" \
-e "s/FUNCNAME/not/g" \
TypeMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/THEOP/||/g" \
-e "s/OPER/\.or\./g" \
-e "s/FUNCNAME/or/g" \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/OPER/\.xor\./g" \
-e "s/FUNCNAME/xor/g" \
TypeXorOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/THEOP/==/g" \
-e "s/OPER/\.eq\./g" \
-e "s/FUNCNAME/eq/g" \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/THEOP/!=/g" \
-e "s/OPER/\.ne\./g" \
-e "s/FUNCNAME/ne/g" \
TypeSimpleOpTemplate.c.sed
exit 0

fi

#
# Begin "string" section
#
if [ $1 = "string" ]
then


sed \
-e 's/SELFUNC/select_string_gt/g' \
-e 's/FUNCNAME/sel_gt/' \
TypestringSelectOpTemplate.c.sed

sed \
-e 's/SELFUNC/select_string_lt/g' \
-e 's/FUNCNAME/sel_lt/' \
TypestringSelectOpTemplate.c.sed

sed \
-e 's/CMPFUNC/cmp_string_lt/g' \
-e 's/FUNCNAME/lt/' \
TypestringCompareOpTemplate.c.sed

sed \
-e 's/CMPFUNC/cmp_string_gt/g' \
-e 's/FUNCNAME/gt/' \
TypestringCompareOpTemplate.c.sed

sed \
-e 's/CMPFUNC/cmp_string_ge/g' \
-e 's/FUNCNAME/ge/' \
TypestringCompareOpTemplate.c.sed

sed \
-e 's/CMPFUNC/cmp_string_le/g' \
-e 's/FUNCNAME/le/' \
TypestringCompareOpTemplate.c.sed

exit 0

fi

#
# Begin "obj" section
#
if [ $1 = "obj" ]
then

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/obj/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/THEOP/==/g" \
-e "s/OPER/\.eq\./g" \
-e "s/FUNCNAME/eq/g" \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/obj/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/THEOP/!=/g" \
-e "s/OPER/\.ne\./g" \
-e "s/FUNCNAME/ne/g" \
TypeSimpleOpTemplate.c.sed

exit 0

fi

if [ $1 = "char" ]
then
sed \
-e "s/HLUGENTYPEREP/$3/g" \
-e "s/HLUTYPEREP/$2/g" \
-e "s/DATATYPE/char/g" \
-e "s/DEFAULT_MISS/$4/g" \
TypeInitClassTemplate.c.sed

sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/+/' \
-e 's/OPER/\+/' \
-e 's/FUNCNAME/plus/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/</' \
-e 's/OPER/\.lt\./' \
-e 's/FUNCNAME/lt/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/>/' \
-e 's/OPER/\.gt\./' \
-e 's/FUNCNAME/gt/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/<=/' \
-e 's/OPER/\.le\./' \
-e 's/FUNCNAME/le/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/>=/' \
-e 's/OPER/\.ge\./' \
-e 's/FUNCNAME/ge/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/==/' \
-e 's/OPER/\.eq\./' \
-e 's/FUNCNAME/eq/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/!=/' \
-e 's/OPER/\.ne\./' \
-e 's/FUNCNAME/ne/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\</' \
-e 's/OPER/\</' \
-e 's/FUNCNAME/sel_lt/' \
TypeSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\>/' \
-e 's/OPER/\>/' \
-e 's/FUNCNAME/sel_gt/' \
TypeSelectOpTemplate.c.sed

sed \
-e "s/DATATYPE/$1/g" \
TypeIsMonoFunc.c.sed


exit 0
fi

#
# Begin "numeric" section
#

sed \
-e "s/HLUGENTYPEREP/$3/g" \
-e "s/HLUTYPEREP/$2/g" \
-e "s/DATATYPE/$1/g" \
-e "s/DEFAULT_MISS/$4/g" \
TypeInitClassTemplate.c.sed


sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
TypeMatMulOpTemplate.c.sed

sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/+/' \
-e 's/OPER/\+/' \
-e 's/FUNCNAME/plus/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/-/' \
-e 's/OPER/\-/' \
-e 's/FUNCNAME/minus/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/*/' \
-e 's/OPER/\*/' \
-e 's/FUNCNAME/multiply/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/</' \
-e 's/OPER/\.lt\./' \
-e 's/FUNCNAME/lt/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/>/' \
-e 's/OPER/\.gt\./' \
-e 's/FUNCNAME/gt/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/<=/' \
-e 's/OPER/\.le\./' \
-e 's/FUNCNAME/le/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/>=/' \
-e 's/OPER/\.ge\./' \
-e 's/FUNCNAME/ge/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/==/' \
-e 's/OPER/\.eq\./' \
-e 's/FUNCNAME/eq/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/!=/' \
-e 's/OPER/\.ne\./' \
-e 's/FUNCNAME/ne/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/\&\&/' \
-e 's/OPER/\.and\./' \
-e 's/FUNCNAME/and/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/||/' \
-e 's/OPER/\.or\./' \
-e 's/FUNCNAME/or/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/DATATYPE/$1/g" \
TypeIsMonoFunc.c.sed

if [ \( $1 = "int" \) -o \( $1 = "long" \) -o \( $1 = "short" \) ]
then
sed \
-e "s/OUTDATATYPE/float/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/FNAME/pow/' \
-e 's/OPER/\^/' \
-e 's/FUNCNAME/exponent/' \
-e 's/CAST/double/g' \
-e 's/INCLUDE/math/' \
TypeFunctionOpTemplate.c.sed
else
sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/FNAME/pow/' \
-e 's/OPER/\^/' \
-e 's/FUNCNAME/exponent/' \
-e 's/CAST/double/g' \
-e 's/INCLUDE/math/' \
TypeFunctionOpTemplate.c.sed
fi


sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/OUTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/\!/' \
-e 's/OPER/\.not\./' \
-e 's/FUNCNAME/not/' \
TypeMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/OUTMISSING/lhs_m->$1val/g" \
-e 's/THEOP/\-/' \
-e 's/OPER/neg/' \
-e 's/FUNCNAME/neg/' \
TypeMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\</' \
-e 's/OPER/\</' \
-e 's/FUNCNAME/sel_lt/' \
TypeSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\>/' \
-e 's/OPER/\>/' \
-e 's/FUNCNAME/sel_gt/' \
TypeSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/OPER/\.xor\./' \
-e 's/FUNCNAME/xor/' \
TypeXorOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/\//' \
-e 's/FUNCNAME/divide/' \
TypeDivOpTemplate.c.sed


if [ \( $1 = "int" \) -o \( $1 = "long" \) -o \( $1 = "short" \) ]
then
sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/\%/' \
-e 's/OPER/mod/' \
-e 's/FUNCNAME/mod/' \
TypeDivOpTemplate.c.sed
fi

exit 0
