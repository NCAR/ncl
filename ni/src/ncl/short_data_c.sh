#!/bin/sh
#
#      $Id: short_data_c.sh,v 1.2 1994-08-25 18:01:28 ethan Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		short_data_c.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 17:34:05 MDT 1994
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

sh op_funcs.sh short NhlTShort NhlTShortGenArray > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e 's/DATATYPE/short/g' \
-e 's/HLUTYPEREP/NhlTShort/g' \
-e 's/HLUGENTYPEREP/NhlTShortGenArray/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValshortData.c.specific' \
-e '/DSPECIFIC/d' \
NclMultiDValData.c.sed > NclMultiDValshortData.c

rm .tmp.$$

echo "created NclMultiDValshortData.c"

exit 0
