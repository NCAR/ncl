#!/bin/sh
#
#      $Id: syms_tab_h.sh,v 1.1 1994-07-21 23:16:38 boote Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		syms_tab_h.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Mon Jul 18 11:37:49 MDT 1994
#
#	Description:	This file creates parser.h from y.tab.h, changing
#			symbols to minimize conflicts with other libs.
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
-e "s/yylval/ncllval/g" \
y.tab.h > parser.h

exit 0
