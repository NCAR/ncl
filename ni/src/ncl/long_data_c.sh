#!/bin/sh
#
#      $Id: long_data_c.sh,v 1.1 1994-07-21 23:16:27 boote Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		long_data_c.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 17:27:08 MDT 1994
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

sh op_funcs.sh long > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%ld\\n/' \
-e 's/DATATYPE/long/g' \
-e 's/HLUTYPEREP/NhlTLong/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDVallongData.c.specific' \
-e '/DSPECIFIC/d' \
NclMultiDValData.c.sed > NclMultiDVallongData.c

rm .tmp.$$

echo "created NclMultiDVallongData.c"

exit 0
