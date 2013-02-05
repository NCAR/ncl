#!/bin/sh


if [ $1 = "ushort" ]; then
    def_type="unsigned short"
elif [ $1 = "uint" ]; then
    def_type="unsigned int"
elif [ $1 = "ulong" ]; then
    def_type="unsigned long"
elif [ $1 = "int64" ]; then
    def_type="long long"
elif [ $1 = "uint64" ]; then
    def_type="unsigned long long"
elif [ $1 = "ubyte" ]; then
    def_type="unsigned char"
else
    def_type=$1
fi

export def_type

sed \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
TypeResetMissing.c.sed

#
# Begin "logical" section
#

if [ $1 = "logical" ]
then

sed \
-e "s/HLUGENTYPEREP/$3/g" \
-e "s/HLUTYPEREP/$2/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/DEFAULT_MISS/$4/g" \
TypeInitClassTemplate.c.sed


sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/THEOP/\&\&/g" \
-e "s/OPER/\.ncl_and\./g" \
-e "s/FUNCNAME/and/g" \
TypeAndOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/OUTMISSING/lhs_m->logicalval/g" \
-e "s/THEOP/\!/g" \
-e "s/OPER/\.ncl_not\./g" \
-e "s/FUNCNAME/not/g" \
TypeMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/THEOP/||/g" \
-e "s/OPER/\.ncl_or\./g" \
-e "s/FUNCNAME/or/g" \
TypeOrOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/OPER/\.ncl_xor\./g" \
-e "s/FUNCNAME/xor/g" \
TypeXorOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/lhs_m->logicalval/g" \
-e "s/RIGHTMISSING/rhs_m->logicalval/g" \
-e "s/THEOP/==/g" \
-e "s/OPER/\.eq\./g" \
-e "s/FUNCNAME/eq/g" \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/LOCALTYPE/logical/g" \
-e "s/LOCALOUTTYPE/logical/g" \
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
-e "s/LOCALTYPE/obj/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/THEOP/==/g" \
-e "s/OPER/\.eq\./g" \
-e "s/FUNCNAME/eq/g" \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/obj/g" \
-e "s/LOCALTYPE/obj/g" \
-e "s/LOCALOUTTYPE/logical/g" \
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
-e "s/LOCALTYPE/$def_type/g" \
-e "s/DEFAULT_MISS/$4/g" \
TypeInitClassTemplate.c.sed

sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$1/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/+/' \
-e 's/OPER/\+/' \
-e 's/FUNCNAME/plus/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$1/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/</' \
-e 's/OPER/\.lt\./' \
-e 's/FUNCNAME/lt/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$1/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/>/' \
-e 's/OPER/\.gt\./' \
-e 's/FUNCNAME/gt/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$1/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/<=/' \
-e 's/OPER/\.le\./' \
-e 's/FUNCNAME/le/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$1/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/>=/' \
-e 's/OPER/\.ge\./' \
-e 's/FUNCNAME/ge/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$1/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/==/' \
-e 's/OPER/\.eq\./' \
-e 's/FUNCNAME/eq/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$1/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/!=/' \
-e 's/OPER/\.ne\./' \
-e 's/FUNCNAME/ne/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e 's/THEOP/\</' \
-e 's/OPER/\</' \
-e 's/FUNCNAME/sel_lt/' \
TypeSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e 's/THEOP/\>/' \
-e 's/OPER/\>/' \
-e 's/FUNCNAME/sel_gt/' \
TypeSelectOpTemplate.c.sed

sed \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
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
-e "s/LOCALTYPE/$def_type/g" \
-e "s/DEFAULT_MISS/$4/g" \
TypeInitClassTemplate.c.sed

sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
TypeMatMulOpTemplate.c.sed

sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/+/' \
-e 's/OPER/\+/' \
-e 's/FUNCNAME/plus/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/-/' \
-e 's/OPER/\-/' \
-e 's/FUNCNAME/minus/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/*/' \
-e 's/OPER/\*/' \
-e 's/FUNCNAME/multiply/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/</' \
-e 's/OPER/\.lt\./' \
-e 's/FUNCNAME/lt/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/>/' \
-e 's/OPER/\.gt\./' \
-e 's/FUNCNAME/gt/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/<=/' \
-e 's/OPER/\.le\./' \
-e 's/FUNCNAME/le/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/>=/' \
-e 's/OPER/\.ge\./' \
-e 's/FUNCNAME/ge/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/==/' \
-e 's/OPER/\.eq\./' \
-e 's/FUNCNAME/eq/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/!=/' \
-e 's/OPER/\.ne\./' \
-e 's/FUNCNAME/ne/' \
TypeSimpleOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/\&\&/' \
-e 's/OPER/\.ncl_and\./' \
-e 's/FUNCNAME/and/' \
TypeAndOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/||/' \
-e 's/OPER/\.ncl_or\./' \
-e 's/FUNCNAME/or/' \
TypeOrOpTemplate.c.sed

sed \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
TypeIsMonoFunc.c.sed

if [ \( $1 = "int" \) -o \( $1 = "long" \) -o \( $1 = "short" \) -o \( $1 = "uint" \) -o \( $1 = "ulong" \) -o \( $1 = "ushort" \) -o \( $1 = "ubyte" \) ]
then
sed \
-e "s/OUTDATATYPE/float/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/FNAME/pow/' \
-e 's/OPER/\^/' \
-e 's/FUNCNAME/exponent/' \
-e 's/CAST/double/g' \
-e 's/INCLUDE/math/' \
TypeFunctionOpTemplate.c.sed
else
if [ \( $1 = "int64" \) -o \( $1 = "uint64" \) ]
then

sed \
-e "s/OUTDATATYPE/double/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
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
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/FNAME/pow/' \
-e 's/OPER/\^/' \
-e 's/FUNCNAME/exponent/' \
-e 's/CAST/double/g' \
-e 's/INCLUDE/math/' \
TypeFunctionOpTemplate.c.sed
fi
fi


sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/OUTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/THEOP/\!/' \
-e 's/OPER/\.ncl_not\./' \
-e 's/FUNCNAME/not/' \
TypeMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e "s/OUTMISSING/lhs_m->$1val/g" \
-e 's/THEOP/\-/' \
-e 's/OPER/neg/' \
-e 's/FUNCNAME/neg/' \
TypeMonoOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e 's/THEOP/\</' \
-e 's/OPER/\</' \
-e 's/FUNCNAME/sel_lt/' \
TypeSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e 's/THEOP/\>/' \
-e 's/OPER/\>/' \
-e 's/FUNCNAME/sel_gt/' \
TypeSelectOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/logical/g" \
-e "s/LEFTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e "s/RIGHTMISSING/((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval/g" \
-e 's/OPER/\.ncl_xor\./' \
-e 's/FUNCNAME/xor/' \
TypeXorOpTemplate.c.sed

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/\//' \
-e 's/FUNCNAME/divide/' \
TypeDivOpTemplate.c.sed

if [ \( $1 = "int" \) -o \( $1 = "long" \) -o \( $1 = "short" \) -o \( $1 = "int64" \) -o \( $1 = "uint" \) -o \( $1 = "ulong" \) -o \( $1 = "ushort" \) -o \( $1 = "uint64" \) -o \( $1 = "ubyte" \) ]
then
sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$def_type/g" \
-e "s/LOCALOUTTYPE/$def_type/g" \
-e "s/LEFTMISSING/lhs_m->$1val/g" \
-e "s/RIGHTMISSING/rhs_m->$1val/g" \
-e 's/THEOP/\%/' \
-e 's/OPER/mod/' \
-e 's/FUNCNAME/mod/' \
TypeDivOpTemplate.c.sed
fi

exit 0
