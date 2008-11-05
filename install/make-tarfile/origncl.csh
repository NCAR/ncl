# csh script to switch between versions of NCL
#
# To use: source /contrib/bin/origncl.csh
# To switch back to the test version: source /contrib/bin/testncl.csh

set ngroot=/contrib/ncl-4.3.0

set sub=s:$ngroot/bin\\:::g
set newpath=`echo $PATH | sed -e $sub`

# Remove extra colons
# set sub=s/:://
# set newpath=`echo $PATH | sed -e $sub`

setenv PATH $newpath

setenv NCARG_ROOT /contrib

set version=`ncl -V`
echo "You are now using the current version of NCL: $version"

unset ngroot
unset newpath
