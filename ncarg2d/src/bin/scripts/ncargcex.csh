#!/bin/csh -f
#
#	$Id: ncargcex.csh,v 1.29 1995-03-27 17:33:26 haley Exp $
#

if ($#argv < 1) goto usage

echo ""
echo "Invoking 'ncargex'..."
echo ""

ncargex $argv

exit

usage:

echo ""
echo "****************************************************"
echo "* ncargcex has been replaced by ncargex.           *"
echo "*                                                  *"
echo "* ncargcex will continue work until NCAR Graphics  *"
echo "* Version 4.1 when it will be removed.             *"
echo "*                                                  *"
echo "* Please see the man page for ncargex.             *"
echo "****************************************************"
echo ""


