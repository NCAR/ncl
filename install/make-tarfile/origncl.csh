# csh script to switch between versions of NCL
#
# To use: source /contrib/bin/origncl.csh
# To switch back to the test version: source /contrib/bin/testncl.csh

set NCARG_ROOT=/contrib/ncl-4.3.0

set sub=s:$NCARG_ROOT/bin\\:::g
set NEWPATH=`echo $PATH | sed -e $sub`

# Remove extra colons
# set sub=s/:://
# set NEWPATH=`echo $PATH | sed -e $sub`

setenv PATH $NEWPATH

setenv NCARG_ROOT /contrib

set version=`ncl -V`
echo "You are now using the current version of NCL: $version"

