#
#
#      $Id: create_string_dataclass.sh,v 1.1 1994-07-14 20:47:41 ethan Exp $
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
#	Date:		Thu Jan 13 15:09:43 MST 1994
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


create_string_op_funs.sh

sed \
-e '/INSERTTMPSTRING/r tmp.string' \
-e '/INSERTTMPSTRING/d' \
-e 's/HLUTYPEREP/NhlTQuark/g' \
string_ops.c.sed >! NclMultiDValstringData.c

rm tmp.string

sed \
-e 's/FIELDNAME/stringval/' \
-e 's/DATATYPE/string/g' \
-e '/DSPECIFIC/d' \
NclMultiDValData.h.sed >! NclMultiDValstringData.h


