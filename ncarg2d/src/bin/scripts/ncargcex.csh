#!/bin/csh -f
#
#	$Id: ncargcex.csh,v 1.10 1994-05-11 16:23:55 haley Exp $
#

#********************#
#                    #
#   NCARGCEX USAGE   #
#                    #
#********************#
if ($#argv < 1) then
  echo "usage: ncargcex [-all] [-clean] [-n] [-onebyone] names               "
  echo "                                                                     "
  echo "See <man ncargcex>                                                   "
  exit
endif

#*********************************************#
#                                             #
# Make sure NCARG_ROOT is set for this script #
#                                             #
#*********************************************#
setenv NCARG_ROOT  `ncargpath root`

if ($status != 0) then
	exit 1
endif

#*********************************#
#                                 #
# Check for existing directories  #
#                                 #
#*********************************#
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

set intexample_list = (c_xwndws)

set X11_option = ""

#***************#
#               #
# Parse options #
#               #
#***************#
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

#*************************************#
#                                     #
# Find out what type of example it is #
#                                     #
#*************************************#
set type="Unknown"

foreach known ($example_list)
    if ("$name" == "$known") then
        set type="Example"
    endif
end

foreach known ($intexample_list)
    if ("$name" == "$known") then
        set type="Interactive_Example"
    endif
end

if ($?List) then
   echo $names
   exit
endif

#****************************#
#                            #
# Code for handling examples #
#                            #
#****************************#

if ($?List) then
   echo $name
   goto theend
endif

#**************************#
#                          #
# Find out what type it is #
#                          #
#**************************#
switch ($type)
    case Example:
        echo "NCAR Graphics C Example <$name>"
    breaksw

    case Interactive_Example:
        echo "NCAR Graphics Interactive C Example <$name>"
    breaksw

    case Unknown:
        echo "ncargcex: <$name> is not a known example"
        goto theend
    breaksw
endsw
echo ""

#**************************************************#
#                                                  #
# If the "-unique" option was selected and the     #
# example already exists, don't generate it again. #
#                                                  #
#**************************************************#

if ($?Unique && -f $name.ncgm) goto theend

set c_files = $name.c
set copy_files="$c_files"
set rmfiles=($rmfiles $copy_files)

#***********************#
#                       #
# Copy the needed files #
#                       #
#***********************#
   
foreach file($copy_files)
    echo "  Copying $file"
    cp $example_dir/$file .
end

#******************************#
#                              #
# Compile and link the example #
#                              #
#******************************#

if (! $?NoRunOption) then
    if ($type == "Interactive_Example") then
        echo ""
        echo "    This example is interactive and can only be executed if"
        echo "    you have X running and have your DISPLAY environment"
        echo "    variable set properly.  It will create an X11 window"
        echo "    that you must click on with your mouse to advance the"
        echo "    frame(s)."
        echo ""
        echo ""
        echo "Compiling and Linking..."
        ncargcc -o $name $c_files
    else
        echo ""
        echo "Compiling and Linking..."
        ncargcc $X11_option -o $name $c_files
    endif
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

#************************#
#                        #
# Remove unwanted files. #
#                        #
#************************#

if ($?CleanOption) then
    rm -f $rmfiles
endif

#************************#
#                        #
# Display NCGM on screen #
#                        #
#************************#
if ($?OneByOneOption) then
    ctrans -d X11 -geometry 1142x865+10+0 $name.ncgm
    rm -f $name.ncgm $rmfiles
endif

theend:

end
