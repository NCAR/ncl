#!/bin/csh -f
#
#   $Id: nhlex.csh,v 1.1 1993-04-30 17:18:37 boote Exp $
#

if (SED_VERBOSE) then
    set echo verbose
endif

if ($#argv < 1) then
echo "usage: nhlex [-all,-A] [-allexamples,-E] [-clean] [-n] names	"
echo ""
echo "See <nhlex(1) man page>"
echo ""
exit
endif

if (SED_DEBUG) then
    set lib_dir = SED_LIBROOT
else
    set lib_dir=`ncargpath lib`
endif

if ($status != 0) then
        exit 1
endif

set example_dir=$lib_dir/ncarg/hlu/examples

if (! -d "$example_dir") then
  echo "Example directory <$example_dir> does not exist."
  exit 1
endif

set example_list=(SED_EXAMPLES)

set names

while ($#argv > 0)
    
    switch ($1)

        case "-all":
        case "-A":
            shift
            set names=($example_list)
            breaksw

        case "-clean":
            shift
            set CleanOption
            breaksw

        case "-n":
            shift
            set NoRunOption
            breaksw
        
        case "-*":
            echo "$0 : Unknown option <$1>"
            exit 1
            breaksw

        default:
            set names=($names $1)
            shift
            breaksw
    endsw
end

foreach name ($names)

set rmfiles

################################################################
#
# Code for handling examples
#
################################################################

echo ""
echo "NCAR Graphics <Pre-Alpha\!> High Level Utility Example <$name>"

set c_files = $name.c
set m_files = Makefile.$name

set copy_files = "$c_files $m_files"

if ( -e "$example_dir/$name.resfile") then
    set r_file = $name.resfile
    set copy_files = "$copy_files $r_file"
endif

if ( -e "$example_dir/$name.h") then
    set h_file = $name.h
    set copy_files = "$copy_files $h_file"
endif

if ( -e "$example_dir/$name.data") then
    set data_file = $name.data
    set copy_files = "$copy_files $data_file"
endif

set rmfiles = $name.o


if (! $?NoRunOption) then
    set copy_files="$copy_files $name"
endif

set rmfiles=($rmfiles $copy_files)

foreach file($copy_files)
    echo "  Copying $file"
    cp $example_dir/$file .
end

if (! $?NoRunOption) then
    echo ""
    echo ""
    if ($?r_file) then
	echo "*************************************************************************"
	echo "*									*"
        echo "* Setting SED_HLUUSRRESENVNAME to ./$r_file to pick up resources	*"
	echo "*									*"
	echo "*************************************************************************"
	setenv SED_HLUUSRRESENVNAME ./$r_file
    endif
    echo ""
    echo "Executing <$name>..."
    echo ""

    $name

    echo ""
    echo "Finished Executing <$name>..."

    if( -e $name.ncgm ) then
	echo ""
	echo "*************************************************"
        echo "*	Example $name produced a metafile.	*"
        echo "*	Metafile is named $name.ncgm		*"
	echo "*************************************************"
        echo ""
    endif
endif

# Clean out unwanted files.

if ($?CleanOption) then
    rm -f $rmfiles
endif

end
