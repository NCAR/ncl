#!/bin/sh
#
#      $Id: logical_data_c.sh,v 1.1 1994-07-21 23:16:25 boote Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		logical_data_c.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 18:12:12 MDT 1994
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

sh op_funcs.sh logical > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e '/PRINTFORMAT/d' \
-e 's/DATATYPE/logical/g' \
-e 's/HLUTYPEREP/NhlTBoolean/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDVallogicalData.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/MultiDVal_logical_mdmd_Mod/NULL/' \
-e 's/MultiDVal_logical_smd_Mod/NULL/' \
-e 's/MultiDVal_logical_mds_Mod/NULL/' \
-e 's/MultiDVal_logical_ss_Mod/NULL/' \
-e 's/MultiDVal_logical_mdmd_Div/NULL/' \
-e 's/MultiDVal_logical_smd_Div/NULL/' \
-e 's/MultiDVal_logical_mds_Div/NULL/' \
-e 's/MultiDVal_logical_ss_Div/NULL/' \
-e 's/MultiDVal_logical_mdmd_Mul/NULL/' \
-e 's/MultiDVal_logical_smd_Mul/NULL/' \
-e 's/MultiDVal_logical_mds_Mul/NULL/' \
-e 's/MultiDVal_logical_ss_Mul/NULL/' \
-e 's/MultiDVal_logical_mdmd_Plus/NULL/' \
-e 's/MultiDVal_logical_smd_Plus/NULL/' \
-e 's/MultiDVal_logical_mds_Plus/NULL/' \
-e 's/MultiDVal_logical_ss_Plus/NULL/' \
-e 's/MultiDVal_logical_mdmd_Minus/NULL/' \
-e 's/MultiDVal_logical_smd_Minus/NULL/' \
-e 's/MultiDVal_logical_mds_Minus/NULL/' \
-e 's/MultiDVal_logical_ss_Minus/NULL/' \
-e 's/MultiDVal_logical_mdmd_Exp/NULL/' \
-e 's/MultiDVal_logical_smd_Exp/NULL/' \
-e 's/MultiDVal_logical_mds_Exp/NULL/' \
-e 's/MultiDVal_logical_ss_Exp/NULL/' \
-e 's/MultiDVal_logical_mdmd_SelLt/NULL/' \
-e 's/MultiDVal_logical_smd_SelLt/NULL/' \
-e 's/MultiDVal_logical_mds_SelLt/NULL/' \
-e 's/MultiDVal_logical_ss_SelLt/NULL/' \
-e 's/MultiDVal_logical_mdmd_SelGt/NULL/' \
-e 's/MultiDVal_logical_smd_SelGt/NULL/' \
-e 's/MultiDVal_logical_mds_SelGt/NULL/' \
-e 's/MultiDVal_logical_ss_SelGt/NULL/' \
-e 's/MultiDVal_logical_md_Neg/NULL/' \
-e 's/MultiDVal_logical_s_Neg/NULL/' \
-e 's/MultiDVal_logical_mdmd_Gt/NULL/' \
-e 's/MultiDVal_logical_smd_Gt/NULL/' \
-e 's/MultiDVal_logical_mds_Gt/NULL/' \
-e 's/MultiDVal_logical_ss_Gt/NULL/' \
-e 's/MultiDVal_logical_mdmd_Lt/NULL/' \
-e 's/MultiDVal_logical_smd_Lt/NULL/' \
-e 's/MultiDVal_logical_mds_Lt/NULL/' \
-e 's/MultiDVal_logical_ss_Lt/NULL/' \
-e 's/MultiDVal_logical_mdmd_Ge/NULL/' \
-e 's/MultiDVal_logical_smd_Ge/NULL/' \
-e 's/MultiDVal_logical_mds_Ge/NULL/' \
-e 's/MultiDVal_logical_ss_Ge/NULL/' \
-e 's/MultiDVal_logical_mdmd_Le/NULL/' \
-e 's/MultiDVal_logical_smd_Le/NULL/' \
-e 's/MultiDVal_logical_mds_Le/NULL/' \
-e 's/MultiDVal_logical_ss_Le/NULL/' \
-e 's/MultiDVal_logical_mdmd_Ne/NULL/' \
-e 's/MultiDVal_logical_smd_Ne/NULL/' \
-e 's/MultiDVal_logical_mds_Ne/NULL/' \
-e 's/MultiDVal_logical_ss_Ne/NULL/' \
-e 's/MultiDVal_logical_mdmd_Eq/NULL/' \
-e 's/MultiDVal_logical_smd_Eq/NULL/' \
-e 's/MultiDVal_logical_mds_Eq/NULL/' \
-e 's/MultiDVal_logical_ss_Eq/NULL/' \
NclMultiDValData.c.sed > NclMultiDVallogicalData.c

rm .tmp.$$

echo "created NclMultiDVallogicalData.c"

exit 0
