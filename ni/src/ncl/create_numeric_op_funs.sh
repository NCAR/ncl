#
#
#      $Id: create_numeric_op_funs.sh,v 1.1 1994-07-14 20:47:39 ethan Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
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
#	Date:		Thu Jan 13 15:09:34 MST 1994
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
sed  \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/+/' \
-e 's/OPER/\+/' \
-e 's/FUNCNAME/Plus/' \
MultiDValSimpleOpTemplate.c.sed  > tmp.$1

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/-/' \
-e 's/OPER/\-/' \
-e 's/FUNCNAME/Minus/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/*/' \
-e 's/OPER/\*/' \
-e 's/FUNCNAME/Mul/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1


sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/</' \
-e 's/OPER/\.lt\./' \
-e 's/FUNCNAME/Lt/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/>/' \
-e 's/OPER/\.gt\./' \
-e 's/FUNCNAME/Gt/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/<=/' \
-e 's/OPER/\.le\./' \
-e 's/FUNCNAME/Le/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1 

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/>=/' \
-e 's/OPER/\.ge\./' \
-e 's/FUNCNAME/Ge/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/==/' \
-e 's/OPER/\.eq\./' \
-e 's/FUNCNAME/Eq/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/!=/' \
-e 's/OPER/\.ne\./' \
-e 's/FUNCNAME/Ne/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\&\&/' \
-e 's/OPER/\.and\./' \
-e 's/FUNCNAME/And/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/||/' \
-e 's/OPER/\.or\./' \
-e 's/FUNCNAME/Or/' \
MultiDValSimpleOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/FNAME/pow/' \
-e 's/OPER/\^/' \
-e 's/FUNCNAME/Exp/' \
-e 's/CAST/double/g' \
-e 's/INCLUDE/math/' \
MultiDValFunctionOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\!/' \
-e 's/OPER/\.not\./' \
-e 's/FUNCNAME/Not/' \
MultiDValMonoOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\-/' \
-e 's/OPER/neg/' \
-e 's/FUNCNAME/Neg/' \
MultiDValMonoOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\</' \
-e 's/OPER/\</' \
-e 's/FUNCNAME/SelLt/' \
MultiDValSelectOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\>/' \
-e 's/OPER/\>/' \
-e 's/FUNCNAME/SelGt/' \
MultiDValSelectOpTemplate.c.sed >> tmp.$1

if(($1 == "long")||($1 == "int")||($1 == "short")) then
sed \
-e "s/OUTDATATYPE/int/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\%/' \
-e 's/OPER/mod/' \
-e 's/FUNCNAME/Mod/' \
MultiDValDivOpTemplate.c.sed >> tmp.$1
endif

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/$1/g" \
-e 's/OPER/\.xor\./' \
-e 's/FUNCNAME/Xor/' \
MultiDValXorOpTemplate.c.sed >> tmp.$1

sed \
-e "s/OUTDATATYPE/$1/g" \
-e "s/DATATYPE/$1/g" \
-e 's/THEOP/\//' \
-e 's/FUNCNAME/Div/' \
MultiDValDivOpTemplate.c.sed >> tmp.$1

sed \
-e "s/DATATYPE/$1/g" \
MultiDValSelectFuncsTemplate.c.sed >> tmp.$1
