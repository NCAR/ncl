#!/bin/sh
#
#      $Id: syms_lex.sh,v 1.1 1994-07-21 23:16:35 boote Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		syms_lex.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Mon Jul 18 11:32:03 MDT 1994
#
#	Description:	This script is used to create scanner.c from lex.yy.c.
#			It changes all the "yy" symbols it needs to change
#			to avoid conflicts with system libraries on brain
#			dead suns.
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
-e "s/yyback/nclback/g" \
-e "s/yybgin/nclbgin/g" \
-e "s/yych/nclch/g" \
-e "s/yycrank/nclcrank/g" \
-e "s/yyestate/nclestate/g" \
-e "s/yyextra/nclextra/g" \
-e "s/yyfirst/nclfirst/g" \
-e "s/yyfnd/nclfnd/g" \
-e "s/yyfussy/nclfussy/g" \
-e "s/yyin/nclin/g" \
-e "s/yyinput/nclinput/g" \
-e "s/yylastch/ncllastch/g" \
-e "s/yylex/ncllex/g" \
-e "s/yylineno/ncllineno/g" \
-e "s/yylook/ncllook/g" \
-e "s/yylsp/ncllsp/g" \
-e "s/yylstate/ncllstate/g" \
-e "s/yylval/ncllval/g" \
-e "s/yymatch/nclmatch/g" \
-e "s/yymore/nclmore/g" \
-e "s/yymorfg/nclmorfg/g" \
-e "s/yyolsp/nclolsp/g" \
-e "s/yyother/nclother/g" \
-e "s/yyout/nclout/g" \
-e "s/yyoutput/ncloutput/g" \
-e "s/yyr/nclr/g" \
-e "s/yyreject/nclreject/g" \
-e "s/yysbuf/nclsbuf/g" \
-e "s/yysptr/nclsptr/g" \
-e "s/yystate/nclstate/g" \
-e "s/yystoff/nclstoff/g" \
-e "s/yystops/nclstops/g" \
-e "s/yysvec/nclsvec/g" \
-e "s/yysvf/nclsvf/g" \
-e "s/yytchar/ncltchar/g" \
-e "s/yytop/ncltop/g" \
-e "s/yyvstop/nclvstop/g" \
-e "s/yywork/nclwork/g" \
-e "s/yywrap/nclwrap/g" \
-e "s/yyz/nclz/g" \
lex.yy.c > scanner.c

exit 0
