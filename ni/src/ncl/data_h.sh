#!/bin/sh
#
#      $Id: data_h.sh,v 1.3 2009-07-10 19:54:06 huangwei Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1994				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		data_h.sh
#
#	Author:		Jeff W. Boote
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Wed Jul 20 16:22:39 MDT 1994
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

sed \
-e "s/FIELDNAME/${1}val/" \
-e "s/DATATYPE/$1/g" \
-e "s/LOCALTYPE/$1/g" \
-e "/DSPECIFIC/d" \
NclType.h.sed > NclType${1}.h

echo "created NclType${1}.h"

exit 0
