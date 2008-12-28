# sh script to switch between versions of NCL
#
# To use: . /contrib/bin/origncl.sh
# To switch back to test version: . /contrib/bin/testncl.sh

ngroot=/contrib/ncl-5.1.1-beta

sub=s:$ngroot/bin\\:::g
newpath=`echo $PATH | sed -e $sub`
export PATH=$newpath

export NCARG_ROOT=/contrib

version=`ncl -V`
echo "You are now using the current version of NCL: $version"

unset ngroot
unset newpath
