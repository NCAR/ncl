#!/bin/sh

sh op_funcs.sh nclfile NULL NULL > .tmp.$$

if [ ! $? ]
then
	exit $?
fi

sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e '/PRINTFORMAT/d' \
-e 's/DATATYPE/nclfile/g' \
-e 's/HLUTYPEREP/NULL/g' \
-e 's/HLUGENTYPEREP/NULL/g' \
-e 's/DEFAULT_FORMAT/%d/g' \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
-e '/DSPECIFIC/r NclMultiDValnclfileData.c.specific' \
-e '/DSPECIFIC/d' \
-e 's/MultiDVal_nclfile_InitClass/NULL/' \
-e 's/MultiDVal_nclfile_md_Not/NULL/' \
-e 's/MultiDVal_nclfile_s_Not/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_And/NULL/' \
-e 's/MultiDVal_nclfile_smd_And/NULL/' \
-e 's/MultiDVal_nclfile_mds_And/NULL/' \
-e 's/MultiDVal_nclfile_ss_And/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Or/NULL/' \
-e 's/MultiDVal_nclfile_smd_Or/NULL/' \
-e 's/MultiDVal_nclfile_mds_Or/NULL/' \
-e 's/MultiDVal_nclfile_ss_Or/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Xor/NULL/' \
-e 's/MultiDVal_nclfile_smd_Xor/NULL/' \
-e 's/MultiDVal_nclfile_mds_Xor/NULL/' \
-e 's/MultiDVal_nclfile_ss_Xor/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Mod/NULL/' \
-e 's/MultiDVal_nclfile_smd_Mod/NULL/' \
-e 's/MultiDVal_nclfile_mds_Mod/NULL/' \
-e 's/MultiDVal_nclfile_ss_Mod/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Div/NULL/' \
-e 's/MultiDVal_nclfile_smd_Div/NULL/' \
-e 's/MultiDVal_nclfile_mds_Div/NULL/' \
-e 's/MultiDVal_nclfile_ss_Div/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Mul/NULL/' \
-e 's/MultiDVal_nclfile_smd_Mul/NULL/' \
-e 's/MultiDVal_nclfile_mds_Mul/NULL/' \
-e 's/MultiDVal_nclfile_ss_Mul/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Plus/NULL/' \
-e 's/MultiDVal_nclfile_smd_Plus/NULL/' \
-e 's/MultiDVal_nclfile_mds_Plus/NULL/' \
-e 's/MultiDVal_nclfile_ss_Plus/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Minus/NULL/' \
-e 's/MultiDVal_nclfile_smd_Minus/NULL/' \
-e 's/MultiDVal_nclfile_mds_Minus/NULL/' \
-e 's/MultiDVal_nclfile_ss_Minus/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Exp/NULL/' \
-e 's/MultiDVal_nclfile_smd_Exp/NULL/' \
-e 's/MultiDVal_nclfile_mds_Exp/NULL/' \
-e 's/MultiDVal_nclfile_ss_Exp/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_SelLt/NULL/' \
-e 's/MultiDVal_nclfile_smd_SelLt/NULL/' \
-e 's/MultiDVal_nclfile_mds_SelLt/NULL/' \
-e 's/MultiDVal_nclfile_ss_SelLt/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_SelGt/NULL/' \
-e 's/MultiDVal_nclfile_smd_SelGt/NULL/' \
-e 's/MultiDVal_nclfile_mds_SelGt/NULL/' \
-e 's/MultiDVal_nclfile_ss_SelGt/NULL/' \
-e 's/MultiDVal_nclfile_md_Neg/NULL/' \
-e 's/MultiDVal_nclfile_s_Neg/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Gt/NULL/' \
-e 's/MultiDVal_nclfile_smd_Gt/NULL/' \
-e 's/MultiDVal_nclfile_mds_Gt/NULL/' \
-e 's/MultiDVal_nclfile_ss_Gt/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Lt/NULL/' \
-e 's/MultiDVal_nclfile_smd_Lt/NULL/' \
-e 's/MultiDVal_nclfile_mds_Lt/NULL/' \
-e 's/MultiDVal_nclfile_ss_Lt/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Ge/NULL/' \
-e 's/MultiDVal_nclfile_smd_Ge/NULL/' \
-e 's/MultiDVal_nclfile_mds_Ge/NULL/' \
-e 's/MultiDVal_nclfile_ss_Ge/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Le/NULL/' \
-e 's/MultiDVal_nclfile_smd_Le/NULL/' \
-e 's/MultiDVal_nclfile_mds_Le/NULL/' \
-e 's/MultiDVal_nclfile_ss_Le/NULL/' \
-e 's/MultiDVal_nclfile_mdmd_Ne/NULL/' \
-e 's/MultiDVal_nclfile_smd_Ne/NULL/' \
-e 's/MultiDVal_nclfile_mds_Ne/NULL/' \
-e 's/MultiDVal_nclfile_ss_Ne/NULL/' \
NclMultiDValData.c.sed > NclMultiDValnclfileData.c

rm .tmp.$$

echo "created NclMultiDValnclfileData.c"

exit 0
