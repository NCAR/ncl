#!/bin/csh -f
#
#   $Id: nhlex.csh,v 1.5 1994-12-10 00:21:06 haley Exp $
#

if (SED_VERBOSE) then
    set echo verbose
endif

if ($#argv < 1) then
echo "usage: nhlex [-A] [-clean] [-n] names	"
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

set example_dir="`ncargpath SED_NCARGDIR`/SED_HLUDIR/examples"

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
        
        case "-list":
            shift
            set List
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

#***********************************************#
#                                               #
# If you just want to see what list of examples #
# you have asked for, list them and exit.       #
#                                               #
#***********************************************#
if ($?List) then
   echo $names
   exit
endif

foreach name ($names)

#*******************************#
#                               #
# Make sure this example exists #
#                               #
#*******************************#
unset found
foreach name2($example_list)
  if ("$name" == "$name2") then
    set found
    break
  endif
end

if (! $?found) then
  echo ""
  echo "Example name $name not found"
  echo ""
  exit
endif

set rmfiles

################################################################
#
# Code for handling examples
#
################################################################

echo ""
echo "NCAR Graphics High Level Utility Example <$name>"

unset fortran

if ( -e "$example_dir/$name.c") then
  set src_files = $name.c
else
  set src_files = $name.f
  set fortran
endif

set copy_files = "$src_files"

if ( -e "$example_dir/$name.res") then
    set r_file = $name.res
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

set rmfiles = ($name $name.o $copy_files)

foreach file($copy_files)
    echo "  Copying $file"
    cp $example_dir/$file .
end

if (! $?NoRunOption) then
    echo ""
    echo "Compiling and linking..."
    if (! $?fortran) then
      nhlcc -o $name $src_files
    else
      nhlf77 -o $name $src_files
    endif
    if ($status != 0) then
        echo ""
        echo "The compile and link failed."
        echo ""
        exit
    endif
    echo "Executing <$name>..."
    echo ""

    ./$name

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
