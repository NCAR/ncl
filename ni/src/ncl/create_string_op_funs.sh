#
#
#      $Id: create_string_op_funs.sh,v 1.1 1994-07-14 20:47:42 ethan Exp $
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
#	Date:		Thu Jan 13 15:10:10 MST 1994
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
-e "s/SELFUNC/select_string_gt/g" \
-e 's/FUNCNAME/SelGt/'\
string_sel.c.sed  > tmp.string

sed \
-e "s/SELFUNC/select_string_lt/g" \
-e 's/FUNCNAME/SelLt/'\
string_sel.c.sed >> tmp.string

sed \
-e "s/CMPFUNC/cmp_string_lt/g" \
-e 's/FUNCNAME/Lt/'\
string_cmp.c.sed >> tmp.string

sed \
-e "s/CMPFUNC/cmp_string_gt/g" \
-e 's/FUNCNAME/Gt/'\
string_cmp.c.sed >> tmp.string

sed \
-e "s/CMPFUNC/cmp_string_ge/g" \
-e 's/FUNCNAME/Ge/'\
string_cmp.c.sed >> tmp.string

sed \
-e "s/CMPFUNC/cmp_string_le/g" \
-e 's/FUNCNAME/Le/'\
string_cmp.c.sed >> tmp.string

sed \
-e "s/DATATYPE/string/g" \
MultiDValSelectFuncsTemplate.c.sed >> tmp.string

cat NclMultiDValstringData.c.specific >> tmp.string

