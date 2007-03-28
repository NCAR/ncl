# sh script to switch between versions of NCL
#
# To use: . /contrib/bin/origncl.sh
# To switch back to test version: . /contrib/bin/testncl.sh

NCARG_ROOT=/contrib/ncl-4.3.0

sub=s:$NCARG_ROOT/bin\\:::g
NEWPATH=`echo $PATH | sed -e $sub`
export PATH=$NEWPATH

export NCARG_ROOT=/contrib

version=`ncl -V`
echo "You are now using the current version of NCL: $version"

