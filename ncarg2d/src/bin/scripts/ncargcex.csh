#!/bin/csh -f
#
#	$Id: ncargcex.csh,v 1.5 1993-03-01 23:59:17 haley Exp $
#

if ($#argv < 1) then
echo "usage: ncargcex [-all] [-clean] [-n] [-onebyone] names               "
echo "                                                                     "
echo "See <man ncargcex>                                                   "
exit
endif

set example_dir = `ncargpath SED_EXAMPLESDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$example_dir") then
  echo "Example directory <$example_dir> does not exist."
  exit 1
endif

set example_list=(\
c_agex07 c_colcon c_mpex05 c_eezmpa c_elblba c_epltch c_cbex01 \
c_slex01 c_sfex02 c_gtxpac)

set X11_option = ""

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

        case "-list"
            shift
            set List
            breaksw

        case "-noX11":
            shift
            set X11_option = "-noX11"
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

if ($?List) then
   echo $names
   exit
endif

################################################################
#
# Code for handling examples
#
################################################################

if ($?List) then
   echo $name
   goto theend
endif

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
    ncargcc $X11_option -o $name $c_files
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

if ("$name" == "c_slex01") then
    set rmfiles = ($rmfiles GNFB09)
endif

# Code for handling inappropriate requests

if ("$type" == "Unknown") then

echo "ncargcex: <$name> is not a known example or test"

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
