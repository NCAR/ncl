#!/bin/csh -f
#
#	$Id: ncargCex.csh,v 1.6 1993-01-22 16:31:56 haley Exp $
#

set example_dir = `ncargpath SED_EXAMPLESDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$example_dir") then
  echo "Example directory <$example_dir> does not exist."
  exit 1
endif

set example_list=(\
c_agex07 c_coex01 c_colcon c_mpex05 c_eezmpa c_elblba c_epltch c_cbex01 \
c_slex01 c_sfex02)

if ($#argv < 1) then
echo "usage: ncargCex [-all] [-clean] [-n] [-onebyone] names               "
echo "                                                                     "
echo "See <man ncargCex>                                                   "
exit
endif

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
        
        case "-onebyone":
            shift
            set OneByOneOption
            breaksw

        case "-unique"
            shift
            set Unique
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

set type="Unknown"

foreach known ($example_list)
    if ("$name" == "$known") then
        set type="Example"
    endif
end

################################################################
#
# Code for handling examples
#
################################################################

echo ""
echo "NCAR Graphics C Example <$name>"

if ($?Unique && -f $name.ncgm) goto theend

set c_files = $name.c

set copy_files="$c_files"

set rmfiles=($rmfiles $copy_files)

foreach file($copy_files)
    echo "  Copying $file"
    cp $example_dir/$file .
end

if (! $?NoRunOption) then
    echo ""
    echo "Compiling and Linking..."
    ncargcc -o $name $c_files
    if ($status != 0) then
            echo ""
            echo "The compile and link failed"
            exit -1
    endif
    echo ""
    echo "Executing <$name>..."
    ncargrun -o $name.ncgm $name
    set rmfiles = ($rmfiles $name.o $name)
    echo "Metafile is named $name.ncgm"
endif

# Code for handling inappropriate requests

if ("$type" == "Unknown") then

echo "ncargCex: <$name> is not a known example or test"

endif

# Clean out unwanted files.

if ($?CleanOption) then
    rm -f $rmfiles
endif

if ($?OneByOneOption) then
    ctrans -d X11 -geometry 1142x865+10+0 $name.ncgm
    rm -f $name.ncgm $rmfiles
endif

theend:

end
