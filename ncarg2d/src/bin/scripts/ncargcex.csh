#!/bin/csh -f
#
#	$Id: ncargcex.csh,v 1.11 1994-05-13 18:01:53 haley Exp $
#

#********************#
#                    #
#   NCARGCEX USAGE   #
#                    #
#********************#
if ($#argv < 1) then
  echo ""
  echo "usage: ncargcex [-all,-A] [-autograph] [-bivar] [-conpack] [-ezmap] "
  echo "                [-gks] [-labelbar] [-plotchar] [-scrolled_title]    "
  echo "                [-softfill] [-inter] [-clean] [-n] [-onebyone]      "
  echo "                names                                               "
  echo ""
  echo "See <man ncargcex>                                                  "
  echo ""
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

set ex_list

#**************************#
#                          #
#  Set autograph examples  #
#                          #
#**************************#
set autograph_list = (c_agex07)
set ex_list = ($ex_list $autograph_list)

#******************************#
#                              #
#  Set bivar/conpack examples  #
#                              #
#******************************#
set cbivar_list = (c_cbex01)
set ex_list = ($ex_list $cbivar_list)

#************************#
#                        #
#  Set conpack examples  #
#                        #
#************************#
set conpack_list = (c_colcon)
set ex_list = ($ex_list $conpack_list)

#**********************#
#                      #
#  Set ezmap examples  #
#                      #
#**********************#
set ezmap_list = (c_eezmpa c_mpex05)
set ex_list = ($ex_list $ezmap_list)

#******************#
#                  #
# set gks examples #
#                  #
#******************#
set gks_list   = (c_gtxpac)
set ex_list = ($ex_list $gks_list)

#***********************#
#                       #
# set labelbar examples #
#                       #
#***********************#
set labelbar_list   = (c_elblba)
set ex_list = ($ex_list $labelbar_list)

#***********************#
#                       #
# set plotchar examples #
#                       #
#***********************#
set plotchar_list   = (c_epltch)
set ex_list = ($ex_list $plotchar_list)

#*****************************#
#                             #
# set scrolled title examples #
#                             #
#*****************************#
set scrlld_title_list   = (c_slex01)
set ex_list = ($ex_list $scrlld_title_list)

#***********************#
#                       #
# set softfill examples #
#                       #
#***********************#
set softfill_list = (c_sfex02)
set ex_list = ($ex_list $softfill_list)

#**************************#
#                          #
# set interactive examples #
#                          #
#**************************#
set interactive_list = (c_xwndws)

#*********************************#
#                                 #
# Default is to load in X library #
#                                 #
#*********************************#
set X11_option

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
            set names=($ex_list)
            breaksw
        
        case "-autograph":
            shift
            set names=($names $autograph_list)
            breaksw

        case "-bivar":
            shift
            set names=($names $cbivar_list)
            breaksw

        case "-conpack":
            shift
            set names=($names $conpack_list)
            breaksw

        case "-ezmap":
            shift
            set names=($names $ezmap_list)
            breaksw

        case "-gks":
            shift
            set names=($names $gks_list)
            breaksw

        case "-labelbar":
            shift
            set names=($names $labelbar_list)
            breaksw

        case "-plotchar":
            shift
            set names=($names $plotchar_list)
            breaksw

        case "-scrolled_title":
            shift
            set names=($names ${scrlld_title_list})
            breaksw

        case "-softfill":
            shift
            set names=($names $softfill_list)
            breaksw

        case "-inter":
            shift
            set names=($names $interactive_list)
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

#*************************************#
#                                     #
# Find out what type of example it is #
#                                     #
#*************************************#
set type="Unknown"

foreach known ($ex_list)
    if ("$name" == "$known") then
        set type="Example"
    endif
end

foreach known ($interactive_list)
    if ("$name" == "$known") then
        set type="Interactive_Example"
    endif
end

if ($?List) then
   echo $names
   exit
endif

#***********************************************#
#                                               #
# If you just want to see what list of examples #
# you have asked for, list them and exit        #
#                                               #
#***********************************************#
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
        echo ""
        echo "NCAR Graphics C Example <$name>"
    breaksw

    case Interactive_Example:
        echo ""
        echo "NCAR Graphics Interactive C Example <$name>"
    breaksw

    case Unknown:
        echo ""
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
set rmfiles=($copy_files)

set ncargf77flags

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
        ncargcc $ncargf77flags -o $name $c_files
    else
        echo ""
        echo "Compiling and Linking..."
        ncargcc $ncargf77flags $X11_option -o $name $c_files
    endif
    if ($status != 0) then
            echo ""
            echo "The compile and link failed"
            exit -1
    endif
#*****************#
#                 #
# Run the example #
#                 #
#*****************#
    echo ""
    echo "Executing <$name>..."
    ncargrun -o $name.ncgm $name
    endsw

    set rmfiles = ($rmfiles $name.o $name)
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
