#!/bin/sh
#
#      $Id: string_data_c.sh,v 1.1 1994-07-21 23:16:33 boote Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		string_data_c.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 17:48:54 MDT 1994
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

sh op_funcs.sh string > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

cat NclMultiDValstringData.c.specific >> .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e "/INSERTTMPSTRING/r .tmp.$$" \
-e '/INSERTTMPSTRING/d' \
-e 's/HLUTYPEREP/NhlTQuark/g' \
string_ops.c.sed > NclMultiDValstringData.c

if [ ! $? ]
then
	exit $?
fi

rm .tmp.$$

echo "created NclMultiDValstringData.c"

exit 0
