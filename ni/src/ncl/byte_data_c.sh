#!/bin/sh
#
#      $Id: byte_data_c.sh,v 1.2 1994-08-25 18:01:10 ethan Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		int_data_c.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 17:21:13 MDT 1994
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

sh op_funcs.sh byte NhlTByte NhlTByteGenArray > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%c\\n/' \
-e 's/DATATYPE/byte/g' \
-e 's/HLUTYPEREP/NhlTByte/g' \
-e 's/HLUGENTYPEREP/NhlTByteGenArray/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValbyteData.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/MultiDVal_byte_mdmd_Mod/NULL/' \
-e 's/MultiDVal_byte_mds_Mod/NULL/' \
-e 's/MultiDVal_byte_smd_Mod/NULL/' \
-e 's/MultiDVal_byte_ss_Mod/NULL/' \
NclMultiDValData.c.sed > NclMultiDValbyteData.c

rm .tmp.$$

echo "created NclMultiDValbyteData.c"
