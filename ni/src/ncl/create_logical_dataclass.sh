#
sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/THEOP/\&\&/g" \
-e "s/OPER/\.and\./g" \
-e "s/FUNCNAME/And/g" \
MultiDValSimpleOpTemplate.c.sed > tmp.logical

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/THEOP/\!/g" \
-e "s/OPER/\.not\./g" \
-e "s/FUNCNAME/Not/g" \
MultiDValMonoOpTemplate.c.sed >> tmp.logical

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/THEOP/||/g" \
-e "s/OPER/\.or\./g" \
-e "s/FUNCNAME/Or/g" \
MultiDValSimpleOpTemplate.c.sed >> tmp.logical

sed \
-e "s/OUTDATATYPE/logical/g" \
-e "s/DATATYPE/logical/g" \
-e "s/OPER/\.xor\./g" \
-e "s/FUNCNAME/Xor/g" \
MultiDValXorOpTemplate.c.sed >> tmp.logical

sed \
-e "s/DATATYPE/logical/g" \
MultiDValSelectFuncsTemplate.c.sed >> tmp.logical


sed \
-e 's/PRINTFORMAT/\%d\\n/' \
-e '/PRINTFORMAT/d' \
-e 's/DATATYPE/logical/g' \
-e 's/HLUTYPEREP/NhlTBoolean/g' \
-e '/REPLACE/r tmp.logical' \
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
NclMultiDValData.c.sed >! NclMultiDVallogicalData.c


sed \
-e 's/FIELDNAME/logicalval/' \
-e 's/DATATYPE/logical/g' \
-e '/DSPECIFIC/d' \
NclMultiDValData.h.sed >! NclMultiDVallogicalData.h

rm tmp.logical
