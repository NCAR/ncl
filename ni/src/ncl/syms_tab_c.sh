#!/bin/sh
#
#      $Id: syms_tab_c.sh,v 1.1 1994-07-21 23:16:37 boote Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		syms_tab_c.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Mon Jul 18 11:40:47 MDT 1994
#
#	Description:	Creates parser.c from y.tab.c changing symbols so
#			conflicts with other libraries go away.
#
#	Usage:
#
#	Environment:
#
#	Files:
#
#
#	Options:
sed \
-e "s/yy_i/ncl_i/g" \
-e "s/yy_len/ncl_len/g" \
-e "s/yy_n/ncl_n/g" \
-e "s/yy_newstate/ncl_newstate/g" \
-e "s/yy_ps/ncl_ps/g" \
-e "s/yy_pv/ncl_pv/g" \
-e "s/yy_stack/ncl_stack/g" \
-e "s/yy_state/ncl_state/g" \
-e "s/yyact/nclact/g" \
-e "s/yychar/nclchar/g" \
-e "s/yychk/nclchk/g" \
-e "s/yyclearin/nclclearin/g" \
-e "s/yydebug/ncldebug/g" \
-e "s/yydef/ncldef/g" \
-e "s/yydefault/ncldefault/g" \
-e "s/yyerrflag/nclerrflag/g" \
-e "s/yyerrlab/nclerrlab/g" \
-e "s/yyerrok/nclerrok/g" \
-e "s/yyerror/nclerror/g" \
-e "s/yyexca/nclexca/g" \
-e "s/yyin/nclin/g" \
-e "s/yylex/ncllex/g" \
-e "s/yylineno/ncllineno/g" \
-e "s/yylval/ncllval/g" \
-e "s/yymaxdepth/nclmaxdepth/g" \
-e "s/yynerrs/nclnerrs/g" \
-e "s/yynewstate/nclnewstate/g" \
-e "s/yypact/nclpact/g" \
-e "s/yyparse/nclparse/g" \
-e "s/yypgo/nclpgo/g" \
-e "s/yyps/nclps/g" \
-e "s/yyps_index/nclps_index/g" \
-e "s/yypv/nclpv/g" \
-e "s/yypv_index/nclpv_index/g" \
-e "s/yypvt_index/nclpvt_index/g" \
-e "s/yypvt/nclpvt/g" \
-e "s/yyr/nclr/g" \
-e "s/yyreds/nclreds/g" \
-e "s/yys/ncls/g" \
-e "s/yystack/nclstack/g" \
-e "s/yystate/nclstate/g" \
-e "s/yytmp/ncltmp/g" \
-e "s/yytoks/ncltoks/g" \
-e "s/yytoktype/ncltoktype/g" \
-e "s/yyv/nclv/g" \
-e "s/yyval/nclval/g" \
-e "s/yyxi/nclxi/g" \
y.tab.c > parser.c

exit 0
