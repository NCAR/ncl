#
#
#      $Id: create_numeric_dataclasses.sh,v 1.1 1994-07-14 20:47:37 ethan Exp $
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
#	Date:		Thu Jan 13 15:05:46 MST 1994
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
create_numeric_op_funs.sh double

echo "done with op_funs"

sed \
-e 's/PRINTFORMAT/\%lg\\n/' \
-e 's/DATATYPE/double/g' \
-e 's/HLUTYPEREP/NhlTDouble/g' \
-e '/REPLACE/r tmp.double' \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValdoubleData.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/MultiDVal_double_mdmd_Mod/NULL/' \
-e 's/MultiDVal_double_smd_Mod/NULL/' \
-e 's/MultiDVal_double_mds_Mod/NULL/' \
-e 's/MultiDVal_double_ss_Mod/NULL/' \
NclMultiDValData.c.sed >! NclMultiDValdoubleData.c

echo "done with class.c"

sed \
-e 's/FIELDNAME/doubleval/' \
-e 's/DATATYPE/double/g' \
-e '/DSPECIFIC/d' \
NclMultiDValData.h.sed >! NclMultiDValdoubleData.h

echo "done with class.h"
rm tmp.double

create_numeric_op_funs.sh float

sed \
-e 's/PRINTFORMAT/\%g\\n/' \
-e 's/DATATYPE/float/g' \
-e 's/HLUTYPEREP/NhlTFloat/g' \
-e '/REPLACE/r tmp.float' \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValfloatData.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/MultiDVal_float_mdmd_Mod/NULL/' \
-e 's/MultiDVal_float_smd_Mod/NULL/' \
-e 's/MultiDVal_float_mds_Mod/NULL/' \
-e 's/MultiDVal_float_ss_Mod/NULL/' \
NclMultiDValData.c.sed >! NclMultiDValfloatData.c

sed \
-e 's/FIELDNAME/floatval/' \
-e 's/DATATYPE/float/g' \
-e '/DSPECIFIC/d' \
NclMultiDValData.h.sed >! NclMultiDValfloatData.h

rm tmp.float

create_numeric_op_funs.sh int

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e 's/DATATYPE/int/g' \
-e 's/HLUTYPEREP/NhlTInteger/g' \
-e '/REPLACE/r tmp.int' \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValintData.c.specific' \
-e '/DSPECIFIC/d' \
NclMultiDValData.c.sed >! NclMultiDValintData.c

sed \
-e 's/FIELDNAME/intval/' \
-e 's/DATATYPE/int/g' \
-e '/DSPECIFIC/d' \
NclMultiDValData.h.sed >! NclMultiDValintData.h

rm tmp.int

create_numeric_op_funs.sh long

sed \
-e 's/PRINTFORMAT/\%ld\\n/' \
-e 's/DATATYPE/long/g' \
-e 's/HLUTYPEREP/NhlTLong/g' \
-e '/REPLACE/r tmp.long' \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDVallongData.c.specific' \
-e '/DSPECIFIC/d' \
NclMultiDValData.c.sed >! NclMultiDVallongData.c

sed \
-e 's/FIELDNAME/longval/' \
-e 's/DATATYPE/long/g' \
-e '/DSPECIFIC/d' \
NclMultiDValData.h.sed >! NclMultiDVallongData.h

rm tmp.long

create_numeric_op_funs.sh short 

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e 's/DATATYPE/short/g' \
-e 's/HLUTYPEREP/NhlTShort/g' \
-e '/REPLACE/r tmp.short' \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValshortData.c.specific' \
-e '/DSPECIFIC/d' \
NclMultiDValData.c.sed >! NclMultiDValshortData.c

sed \
-e 's/FIELDNAME/shortval/' \
-e 's/DATATYPE/short/g' \
-e '/DSPECIFIC/d' \
NclMultiDValData.h.sed >! NclMultiDValshortData.h

rm tmp.short
