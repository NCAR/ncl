#!/bin/sh
#
#      $Id: float_data_c.sh,v 1.1 1994-07-21 23:16:22 boote Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		float_data_c.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 16:49:38 MDT 1994
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

sh op_funcs.sh float > .tmp.$$

if [  ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%g\\n/' \
-e 's/DATATYPE/float/g' \
-e 's/HLUTYPEREP/NhlTFloat/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValfloatData.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/MultiDVal_float_mdmd_Mod/NULL/' \
-e 's/MultiDVal_float_smd_Mod/NULL/' \
-e 's/MultiDVal_float_mds_Mod/NULL/' \
-e 's/MultiDVal_float_ss_Mod/NULL/' \
NclMultiDValData.c.sed > NclMultiDValfloatData.c

rm .tmp.$$

echo "created NclMultiDValfloatData.c"

exit 0
