#!/bin/sh
#
#      $Id: double_data_c.sh,v 1.1 1994-07-21 23:16:20 boote Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		double_data.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 16:17:37 MDT 1994
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

sh op_funcs.sh double > .tmp.$$ 

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%lg\\n/' \
-e 's/DATATYPE/double/g' \
-e 's/HLUTYPEREP/NhlTDouble/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValdoubleData.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/MultiDVal_double_mdmd_Mod/NULL/' \
-e 's/MultiDVal_double_smd_Mod/NULL/' \
-e 's/MultiDVal_double_mds_Mod/NULL/' \
-e 's/MultiDVal_double_ss_Mod/NULL/' \
NclMultiDValData.c.sed > NclMultiDValdoubleData.c

rm .tmp.$$

echo "created NclMultiDValdoubleData.c"

exit 0
