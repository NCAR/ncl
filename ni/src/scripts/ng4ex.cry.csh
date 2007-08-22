#!/bin/csh -f
#
#   $Id: ng4ex.cry.csh,v 1.2 2007-08-22 04:45:24 haley Exp $
#
#######################################################################
#                                                                     #
#              Copyright (C)  1995                                    #
#        University Corporation for Atmospheric Research              #
#              All Rights Reserved                                    #
#                                                                     #
#######################################################################
#
#   File:       Jeff Boote (and modified by Mary Haley)
#
#   Author:     
#           National Center for Atmospheric Research
#           PO 3000, Boulder, Colorado
#
#   Date:       Sun Jan 29 19:31:56 MST 1995
#
#   Description:    "ng4ex" provides the user with access to several
#                   C and Fortran examples illustrating the use
#                   of the NCAR Graphics HLUs (High Level Utilities).
# 
# The current "objects" that "ng4ex" demonstrates include the following:
# 
#     object name           abbrev. name
#     -----------           ------------
#     app                       ap
#     basic                     basic
#     contourplot               cn
#     labelbar                  lb
#     legend                    lg
#     primitives                pr
#     mapplot                   mp
#     ngmath                    nm
#     textitem                  tx
#     tickmark                  tm
#     title                     ti
#     streamlineplot            st
#     vectorplot                vc
#     xyplot                    xy
#
# How to add an example to this script:
#
#   First add it to appropriate variable below (c_xyplot, f_vector,
#   etc.).
#
#   If it is one of the "special" examples, then you'll also need to
#   add it to one or more of the following variables:
#
#    anno_list:  Annotation example
#    gui_list:   GUI example
#    cdf_list:   Reads or writes a netCDF file
#    x_list:     An example that always creates an X window (this
#                list was created so that if a user runs "ng4ex -W ncgm"
#                it won't try to generate examples that will always 
#                produce an X11 window).  GUI examples should be included
#                in this list.
#

#***********************#
#                       #
# ng4ex usage statement #
#                       #
#***********************#
if ($#argv < 1) then
  echo "usage: ng4ex [options] [example names]"
  echo ""
  echo " Options:"
  echo ""
  echo "   [-list] [-n] [-clean]"
  echo ""
  echo "   To invoke various types of examples:"
  echo "   [-A] [-C] [-Fortran] [-cdf] [-gui] [-app] [-basic]"
  echo ""
  echo "   To invoke various types of objects:"
  echo "   [-contourplot] [-labelbar] [-legend] [-mapplot] [-ngmath]"
  echo "   [-primitives] [-streamlineplot] [-textitem] [-tickmark]"
  echo "   [-title] [-vectorplot] [-xyplot]"
  echo ""
  echo "See <man ng4ex> for explanation of options." 
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
# Get directory names for various #
# examples and files.             #
#                                 #
#*********************************#

set hluex_dir="`ncargpath SED_HLUEXDIR`"
if (! -d "$hluex_dir") then
  echo "HLU directory <$hluex_dir> does not exist."
  exit 1
endif

set res_dir="`ncargpath SED_RESDIR`"
if (! -d "$res_dir") then
  echo "Resource directory <$res_dir> does not exist."
  exit 1
endif

set data_dir="`ncargpath SED_DATADIR`"
if (! -d "$data_dir") then
  echo "Data directory <$data_dir> does not exist."
  exit 1
endif

#**********************#
#                      #
# Initialize directory #
# names for objects    #
#                      #
#**********************#
set dir_ap = "app"
set dir_basic = "basic"
set dir_cn = "contourplot"
set dir_lb = "labelbar"
set dir_lg = "legend"
set dir_pr = "primitives"
set dir_mp = "mapplot"
set dir_nm = "ngmath"
set dir_st = "streamlineplot"
set dir_tx = "textitem"
set dir_tm = "tickmark"
set dir_ti = "title"
set dir_vc = "vectorplot"
set dir_xy = "xyplot"

#******************#
#                  #
# Initialize lists #
#                  #
#******************#
set c_list
set f_list

#***************************************#
#                                       #
# Will hold list of Annotation examples #
#                                       #
#***************************************#
set anno_list

#********************************#
#                                #
# Will hold list of GUI examples #
#                                #
#********************************#
set gui_list

#***********************************#
#                                   #
# Will hold list of netCDF examples #
#                                   #
#***********************************#
set cdf_list

#********************************************************#
#                                                        #
# Keep track of examples that will *always* produce an X #
# window, (these examples we don't want to generate if   #
# the "-W ncgm" option is specified.                     #
#                                                        #
#********************************************************#
set x_list

#********************#
#                    #
#  Set App examples  #
#                    #
#********************#
set f_app   = (ap01f)
set c_app   = (ap01c)
set app_list = ($c_app $f_app)
set c_list = ($c_list $c_app)
set f_list = ($f_list $f_app)

#**********************#
#                      #
#  Set Basic examples  #
#                      #
#**********************#
set c_basic   = (basic01c basic02c basic03c basic04c basic05c basic06c \
                 basic07c basic08c basic09c)
set f_basic   = (basic01f basic02f basic03f basic04f basic05f basic06f \
                 basic07f basic08f basic09f)
set basic_list = ($c_basic $f_basic)
set c_list = ($c_list $c_basic)
set f_list = ($f_list $f_basic)
set x_list = ($x_list basic03c basic03f basic04c basic04f basic07c basic07f)

#*************************#
#                         #
#  Set Contour examples   #
#                         #
#*************************#
set c_contour  = (cn01c cn02c cn03c cn04c cn05c cn06c cn07c cn08c cn09c cn10c \
                  cn11c cn12c cn13c cn14c cn15c cn16c cn17c)
set f_contour  = (cn01f cn02f cn03f cn04f cn05f cn06f cn07f cn08f cn09f cn10f \
                  cn11f cn12f cn13f cn14f cn15f cn16f cn17f)
set contour_list = ($c_contour $f_contour)
set c_list = ($c_list $c_contour)
set f_list = ($f_list $f_contour)
set anno_list = ($anno_list cn17c cn17f)
set cdf_list = ($cdf_list cn05c cn06c cn07c cn08c cn09c cn10c cn14c cn15c cn16c cn05f cn06f cn07f cn08f cn09f cn10f cn14f cn15f cn16f)

#*************************#
#                         #
#  Set LabelBar examples  #
#                         #
#*************************#
set c_labelbar = (lb01c lb02c)
set f_labelbar = (lb01f lb02f)
set labelbar_list = ($c_labelbar $f_labelbar)
set c_list = ($c_list $c_labelbar)
set f_list = ($f_list $f_labelbar)

#***********************#
#                       #
#  Set Legend examples  #
#                       #
#***********************#
set c_legend = (lg01c lg02c lg03c)
set f_legend = (lg01f lg02f lg03f)
set legend_list = ($c_legend $f_legend)
set c_list = ($c_list $c_legend)
set f_list = ($f_list $f_legend)

#*************************#
#                         #
#  Set Primitive examples #
#                         #
#*************************#
set c_prim   = (pr01c pr02c pr03c pr04c pr05c)
set f_prim   = (pr01f pr02f pr03f pr04f pr05f)
set prim_list = ($c_prim $f_prim)
set c_list = ($c_list $c_prim)
set f_list = ($f_list $f_prim)

#************************#
#                        #
#  Set MapPlot examples  #
#                        #
#************************#
set c_mapplot   = (mp01c mp02c mp03c mp04c mp05c mp06c)
set f_mapplot   = (mp01f mp02f mp03f mp04f mp05f mp06f)
set mapplot_list = ($c_mapplot $f_mapplot)
set c_list = ($c_list $c_mapplot)
set f_list = ($f_list $f_mapplot)
set anno_list = ($anno_list mp04c mp04f)

#***********************#
#                       #
#  Set Ngmath examples  #
#                       #
#***********************#
set c_ngmath = (nm01c nm02c nm03c nm04c nm05c nm06c)
set f_ngmath = (nm01f nm02f nm03f nm04f nm05f nm06f)
set ngmath_list = ($c_ngmath $f_ngmath)
set c_list = ($c_list $c_ngmath)
set f_list = ($f_list $f_ngmath)

#*******************************#
#                               #
#  Set Streamlineplot examples  #
#                               #
#*******************************#
set c_stream  = (st01c st02c st03c st04c)
set f_stream  = (st01f st02f st03f st04f)
set stream_list = ($c_stream $f_stream)
set c_list = ($c_list $c_stream)
set f_list = ($f_list $f_stream)
set cdf_list = ($cdf_list st03c st03f st04c st04f)

#*************************#
#                         #
#  Set TextItem examples  #
#                         #
#*************************#
set c_textitem = (tx01c tx02c tx03c tx04c tx05c tx06c tx07c tx08c)
set f_textitem = (tx01f tx02f tx03f tx04f tx05f tx06f tx07f tx08f)
set textitem_list  = ($c_textitem $f_textitem)
set c_list = ($c_list $c_textitem)
set f_list = ($f_list $f_textitem)

#***********************#
#                       #
# set TickMark examples #
#                       #
#***********************#
set c_tickmark = (tm01c tm02c tm03c)
set f_tickmark = (tm01f tm02f tm03f)
set tickmark_list  = ($c_tickmark $f_tickmark)
set c_list = ($c_list $c_tickmark)
set f_list = ($f_list $f_tickmark)

#**********************#
#                      #
#  Set Title examples  #
#                      #
#**********************#
set c_title = (ti01c ti02c ti03c)
set f_title = (ti01f ti02f ti03f)
set title_list  = ($c_title $f_title)
set c_list = ($c_list $c_title)
set f_list = ($f_list $f_title)

#***************************#
#                           #
#  Set Vectorplot examples  #
#                           #
#***************************#
set c_vector  = (vc01c vc02c vc03c vc04c vc05c vc06c vc07c vc08c vc09c)
set f_vector  = (vc01f vc02f vc03f vc04f vc05f vc06f vc07f vc08f vc09f)
set vector_list = ($c_vector $f_vector)
set c_list = ($c_list $c_vector)
set f_list = ($f_list $f_vector)
set cdf_list = ($cdf_list vc06c vc06f vc08c vc08f vc09c vc09f)

#***********************#
#                       #
#  Set XYPlot examples  #
#                       #
#***********************#
set c_xyplot  = (xy01c xy02c xy03c xy04c xy05c xy06c xy07c xy08c xy09c \
                 xy10c xy11c xy12c xy13c xy14c xy15c xy16c xy17c)
set f_xyplot  = (xy01f xy02f xy03f xy04f xy05f xy06f xy07f xy08f xy09f xy10f \
                 xy11f xy14f xy15f xy16f xy17f)
set xyplot_list = ($c_xyplot $f_xyplot)
set c_list = ($c_list $c_xyplot)
set f_list = ($f_list $f_xyplot)
set gui_list = ($gui_list xy12c xy13c)
set cdf_list = ($cdf_list xy06c xy10c xy06f xy10f)
set x_list = ($x_list xy11c xy11f xy12c xy13c)

#*******************************#
#                               #
# Parse options on command line #
#                               #
#*******************************#
set names
unset NCGM
unset X11
unset PS
set num_set = 0

while ($#argv > 0)

  switch ($1)
    case "-all":
    case "-A":
      shift
      set names=($names $c_list $f_list)
      breaksw

    case "-fortran":
    case "-Fortran":
      shift
      set names=($names $f_list)
      breaksw
        
    case "-C":
    case "-c":
      shift
      set names=($names $c_list)
      breaksw
        
    case "-GUI":
    case "-gui":
      shift
      set names=($names $gui_list)
      breaksw
        
    case "-anno":
    case "-annotation":
    case "-ANNOTATION":
      shift
      set names=($names $anno_list)
      breaksw
        
    case "-CDF":
    case "-cdf":
      shift
      set names=($names $cdf_list)
      breaksw
        
    case "-unique":
      shift
      set Unique
      breaksw

    case "-W":
      shift
      set ws_type = $1
      if ("$ws_type" == "NCGM" || "$ws_type" == "ncgm") then
        set NCGM
        @@ num_set += 1
      else if ("$ws_type" == "X11" || "$ws_type" == "x11") then
        set X11
        @@ num_set += 1
      else if ("$ws_type" == "PS" || "$ws_type" == "ps") then
        set PS
        @@ num_set += 1
      else
        echo ""
        echo "    '$ws_type' is not a valid workstation type."
        echo ""
        exit 1
      endif
      if ($num_set > 1) then
        echo ""
        echo "    Cannot specify more than one workstation type."
        echo ""
        exit 1
      endif
      shift
      breaksw
        
    case "-app":
      shift
      set names=($names $app_list)
      breaksw
        
    case "-basic":
      shift
      set names=($names $basic_list)
      breaksw
        
    case "-contourplot":
    case "-ContourPlot":
      shift
      set names=($names $contour_list)
      breaksw
        
    case "-labelbar":
    case "-LabelBar":
      shift
      set names=($names $labelbar_list)
      breaksw

    case "-Legend":
    case "-legend":
      shift
      set names=($names $legend_list)
      breaksw

    case "-primitives":
    case "-Primitives":
      shift
      set names=($names $prim_list)
      breaksw

    case "-mapplot":
    case "-MapPlot":
      shift
      set names=($names $mapplot_list)
      breaksw

    case "-ngmath":
    case "-NgMath":
    case "-Ngmath":
      shift
      set names=($names $ngmath_list)
      breaksw

    case "-streamlineplot":
    case "-StreamlinePlot":
      shift
      set names=($names $stream_list)
      breaksw

    case "-textitem":
    case "-TextItem":
      shift
      set names=($names $textitem_list)
      breaksw

    case "-tickmark":
    case "-TickMark":
      shift
      set names=($names $tickmark_list)
      breaksw

    case "-title":
    case "-Title":
      shift
      set names=($names $title_list)
      breaksw

    case "-vectorplot":
    case "-VectorPlot":
      shift
      set names=($names $vector_list)
      breaksw

    case "-xyplot":
    case "-XyPlot":
      shift
      set names=($names $xyplot_list)
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
      set ex_name = $1:r
      set names=($names $ex_name)
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

#***************************#
#                           #
# Loop through each example #
#                           #
#***************************#
foreach name ($names)

#**************************************************#
#                                                  #
# If the "-unique" option was selected and the     #
# NCGM/PS already exists, don't generate it again. #
#                                                  #
#**************************************************#
if ($?NCGM && $?Unique && -f $name.ncgm) goto theend
if ($?PS && $?Unique && -f $name.ps) goto theend

#*********************************************#
#                                             #
# If we've specified "-W ncgm", then we don't #
# want to generate any of the "X11" examples. #
#                                             #
#*********************************************#
if ($?NCGM) then
  set found = 0
  foreach file($x_list)
    if ("$file" == "$name") then
      set found = 1
      break
    endif
  end
endif

#*************************************#
#                                     #
# Find out what type of example it is #
#                                     #
#*************************************#
set ex_type="Unknown"

foreach known ($c_list)
  if ("$name" == "$known") then
    set ex_type = "C"
    break
  endif
end

foreach known ($f_list)
  if ("$name" == "$known") then
    set ex_type = "Fortran"
    break
  endif
end

#***************#
#               #
# Echo the type #
#               #
#***************#

switch($ex_type)
    case Unknown:
    echo ""
    echo "ng4ex: <$name> is not a known example"
    echo ""
    goto theend
    breaksw

    default:
    echo ""
    echo "NCAR Graphics High Level Utility $ex_type Example <$name>"
    echo ""
    breaksw
endsw

#******************************#
#                              #
# Determine the directory that #
# the example resides in.      #
#                              #
#******************************#
set obj = `expr $name : '\(.*\)[0-9][0-9].*'`
set obj_dir = `eval echo \$dir_$obj`

#******************************************#
#                                          #
# Determine if it's a C or Fortran example #
#                                          #
#******************************************#
unset fprog
unset cprog
set res_file
set inc_file
set ascdata_file
set bindata_file
set comp_flags
set generic_name = `expr $name : '\(.*[0-9][0-9]\).*'`
set prog_type = `expr $name : '.*[0-9][0-9]\(.\)'`
set resfile_dir = "$res_dir/$obj_dir"

if ("$prog_type" == "c") then
  set cprog
  set example_dir = "$hluex_dir/$obj_dir"
  set src_file = "$name.c"
else if ("$prog_type" == "f") then
  set fprog
  set example_dir = "$hluex_dir/$obj_dir"
  set src_file = "$name.f"
else
  echo ""
  echo "Cannot determine what type of example $name is."
  echo ""
  exit 1
endif

#***********************************#
#                                   #
# Get the names of needed resource, #
# include, and/or data files        #
#                                   #
#***********************************#

if ( -e "$resfile_dir/$name.res") then
    set res_file = $name.res
else if ( -e "$resfile_dir/$generic_name.res") then
    set res_file = $generic_name.res
endif

if ( -e "$example_dir/$name.h") then
    set inc_file = $name.h
endif

#*******************************#
#                               #
# Some examples need data files #
# or extra compiler stuff or    #
# some special message.         #
#                               #
#*******************************#
switch($name)
    case xy06c:
    case xy06f:
    case xy10c:
    case xy10f:
    case cn05c:
    case cn05f:
    case cn06c:
    case cn06f:
    case cn07c:
    case cn07f:
    case cn08c:
    case cn08f:
    case cn09c:
    case cn09f:
    case cn10c:
    case cn10f:
    case cn14c:
    case cn14f:
    case cn15c:
    case cn15f:
    case cn16c:
    case cn16f:
    case st03c:
    case st03f:
    case st04c:
    case st04f:
    case vc06c:
    case vc06f:
    case vc08c:
    case vc08f:
    case vc09c:
    case vc09f:
        echo ""
        echo "You must have the netCDF library installed on your system"
        echo "in order to run this example."
        echo ""
        set comp_flags = "-netcdf"
    breaksw

    case xy12c:
    case xy13c:
        echo ""
        echo "This example is a GUI example, and requires the X"
        echo "libraries Xm and Xt to be loaded during the link phase."
        echo ""
        echo "The output will be displayed to an X11 window, and an"
        echo "NCGM/PS file may or may not be produced."
        echo ""
        set comp_flags = "-XmXt"
        if ("$name" == "xy12c") set ascdata_file = "xy12c.asc"
    breaksw

    case cn03f:
    case cn11f:
        set ascdata_file = "$name.asc"
    breaksw

    case xy16c:
    case xy16f:
        set ascdata_file = "xy.asc"
    breaksw

    case xy17c:
    case xy17f:
        set ascdata_file = "xy.asc"
    breaksw

    case nm01c:
    case nm02c:
    case nm03c:
    case nm04c:
    case nm05c:
    case nm06c:
    case nm01f:
    case nm02f:
    case nm03f:
    case nm04f:
    case nm05f:
    case nm06f:
        echo ""
        echo "This example requires the math library, -lngmath, to be"
        echo "loaded during the link phase."
        echo ""
        set comp_flags = "-ngmath"
    breaksw
endsw

switch($generic_name)
    case basic04:
	case xy11:
	case xy12:
        echo ""
        echo "This example was set up to display the output to an"
        echo "X11 window and an NCGM."
        echo ""
    breaksw

	case cn15:
    case basic07:
        echo ""
        echo "This example was set up to display the output to an"
        echo "X11 window, an NCGM, and a PostScript workstation."
        echo ""
    breaksw

    case cn13:
        echo ""
        echo "This example may take awhile, so please be patient."
        echo ""
    breaksw

    case xy08:
        set ascdata_file = "$generic_name.asc"
    breaksw
endsw

#************#
#            #
# Copy files #
#            #
#************#
foreach file($src_file)
    echo "  Copying $file"
    cp $example_dir/$file .
end

#*****************************************#
#                                         #
# Modify example if we explicity want to  #
# output to an NCGM file, an XWorkstaion, #
# or a PS file.  If a type is explicitly  #
# requested, then turn the other 2 types  #
# off.                                    #
#                                         #
#*****************************************#
if ($?NCGM) then
  ed << EOF - ./$src_file >& /dev/null
g/NCGM=0/s//NCGM=1/g
w
q
EOF
else if ("$num_set" == "1") then
  ed << EOF - ./$src_file >& /dev/null
g/NCGM=1/s//NCGM=0/g
w
q
EOF
endif

if ($?X11) then
  ed << EOF - ./$src_file >& /dev/null
g/X11=0/s//X11=1/g
w
q
EOF
else if ("$num_set" == "1") then
  ed << EOF - ./$src_file >& /dev/null
g/X11=1/s//X11=0/g
w
q
EOF
endif

if ($?PS) then
  ed << EOF - ./$src_file >& /dev/null
g/PS=0/s//PS=1/g
w
q
EOF
else if ("$num_set" == "1") then
  ed << EOF - ./$src_file >& /dev/null
g/PS=1/s//PS=0/g
w
q
EOF
endif

foreach file($res_file)
    echo "  Copying $file"
    cp $resfile_dir/$file .
end

foreach file($inc_file)
    echo "  Copying $file"
    cp $example_dir/$file .
end

foreach file($ascdata_file)
    echo "  Copying $file"
    cp $data_dir/asc/$file .
end

foreach file($bindata_file)
    echo "  Copying $file"
    cp $data_dir/bin/$file .
end

#********************************#
#                                #
# Create list of files to remove #
#                                #
#********************************#
set rmfiles = ($src_file $res_file $inc_file $ascdata_file $bindata_file)
set rmfiles = ($name $name.o $rmfiles)

#******************************#
#                              #
# Compile and link the example #
#                              #
#******************************#
   
if (! $?NoRunOption) then
    echo ""
    echo "Compiling and linking..."
    if ($?cprog) then
      nhlcc -o $name $src_file $comp_flags
    else
      nhlf90 -o $name $src_file $comp_flags
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
    echo "Finished executing <$name>..."
    echo ""
    if( -e $name.ncgm ) then
      echo ""
      echo "    Example $name produced a metafile."
      echo "    Metafile is named $name.ncgm."
      echo ""
    endif
    if( -e $name.ps ) then
      echo ""
      echo "    Example $name produced a PostScript file."
      echo "    PS file is named $name.ps."
      echo ""
    endif
endif

#***********************#
#                       #
# Remove unwanted files #
#                       #
#***********************#
if ($?CleanOption) then
    /bin/rm $rmfiles >>& /dev/null
endif

theend:

end
