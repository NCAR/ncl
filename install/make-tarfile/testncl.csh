# csh script to switch between versions of NCL
#
# To use: source /contrib/bin/testncl.csh
# To switch back to the original version: source /contrib/bin/origncl.csh

setenv NCARG_ROOT /contrib/ncl-5.1.1-beta
setenv PATH $NCARG_ROOT/bin:$PATH

set version=`ncl -V`
echo "You are now using the test version of NCL: $version"

