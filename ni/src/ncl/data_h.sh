#!/bin/sh
#
#      $Id: data_h.sh,v 1.1 1994-07-21 23:16:18 boote Exp $
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
-e "/DSPECIFIC/d" \
NclMultiDValData.h.sed > NclMultiDVal${1}Data.h

echo "created NclMultiDVal${1}Data.h"

exit 0
