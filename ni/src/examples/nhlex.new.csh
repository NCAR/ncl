#!/bin/csh -f
#
#	$Id: nhlex.new.csh,v 1.4 1995-01-26 18:41:38 haley Exp $
#

#***********************#
#                       #
# nhlex usage statement #
#                       #
#***********************#
if ($#argv < 1) then
  echo "usage: nhlex.new [options] [example names]"
  echo ""
  echo " Options:"
  echo ""
  echo "   [-list] [-n] [-clean]"
  echo ""
  echo " To invoke various types of examples:"
  echo "   [-A] [-C] [-Fortran]"
  echo ""
  echo " To invoke various types of objects:"
  echo "   [-annotation] [-contour] [-labelbar] [-legend] [-mapplot]  "
  echo "   [-overlay] [-textitem] [-tickmark] [-title] [-xyplot]    "
  echo ""
  echo "See <man nhlex.new> for explanation of options." 
  echo ""
  exit
endif

if (SED_VERBOSE) then
    set echo verbose
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

#******************#
#                  #
# Initialize lists #
#                  #
#******************#
set c_list
set f_list

#***************************#
#                           #
#  Set Annotation examples  #
#                           #
#***************************#
set c_annotation   = (an01c)
set f_annotation   = (an01f)
set annotation_list = ($c_annotation $f_annotation)
set c_list = ($c_list $c_annotation)
set f_list = ($f_list $f_annotation)

#*************************#
#                         #
#  Set Contour examples   #
#                         #
#*************************#
set c_contour  = (cn01c cn02c cn03c cn04c)
set f_contour  = (cn01f cn02f cn03f cn04f)
set contour_list = ($c_contour $f_contour)
set c_list = ($c_list $c_contour)
set f_list = ($f_list $f_contour)

#*************************#
#                         #
#  Set LabelBar examples  #
#                         #
#*************************#
set c_labelbar = (lb01c lb02c)
set labelbar_list = ($c_labelbar)
set c_list = ($c_list $c_labelbar)

#***********************#
#                       #
#  Set Legend examples  #
#                       #
#***********************#
set c_legend = (lg01c lg02c lg03c)
set legend_list = ($c_legend)
set c_list = ($c_list $c_legend)

#************************#
#                        #
#  Set MapPlot examples  #
#                        #
#************************#
set c_mapplot   = (mp01c mp02c mp03c)
set f_mapplot   = (mp01f mp02f mp03f)
set mapplot_list = ($c_mapplot $f_mapplot)
set c_list = ($c_list $c_mapplot)
set f_list = ($f_list $f_mapplot)

#************************#
#                        #
#  Set Overlay examples  #
#                        #
#************************#
set overlay_list

#*************************#
#                         #
#  Set TextITem examples  #
#                         #
#*************************#
set c_textitem = (tx01c tx02c tx03c tx04c)
set f_textitem = (tx01f)
set textitem_list  = ($c_textitem $f_textitem)
set c_list = ($c_list $c_textitem)
set f_list = ($f_list $f_textitem)

#***********************#
#                       #
# set TickMark examples #
#                       #
#***********************#
set c_tickmark = (tm01c tm02c tm03c)
set tickmark_list  = ($c_tickmark)
set c_list = ($c_list $c_tickmark)

#**********************#
#                      #
#  Set Title examples  #
#                      #
#**********************#
set c_title = (ti01c ti02c ti03c)
set title_list  = ($c_title)
set c_list = ($c_list $c_title)

#***********************#
#                       #
#  Set XYPlot examples  #
#                       #
#***********************#
set c_xyplot  = (xy01c xy02c xy03c)
set f_xyplot  = (xy01f)
set xyplot_list = ($c_xyplot $f_xyplot)
set c_list = ($c_list $c_xyplot)
set f_list = ($f_list $f_xyplot)

#*******************************#
#                               #
# Parse options on command line #
#                               #
#*******************************#
set names

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
        
    case "-annotation":
    case "-Annotation":
      shift
      set names=($names $annotation_list)
      breaksw
        
    case "-contour":
    case "-Contour":
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

    case "-mapplot":
    case "-MapPlot":
      shift
      set names=($names $mapplot_list)
      breaksw

    case "-overlay":
    case "-Overlay":
      shift
      set names=($names $overlay_list)
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

    case "-xyplot":
    case "-XYPlot":
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

#***************************#
#                           #
# Loop through each example #
#                           #
#***************************#
foreach name ($names)

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

if ( "$ex_type" == "Unknown" ) then
  echo ""
  echo "nhlex.new: <$name> is not a known example"
  echo ""
  goto theend
else
  echo ""
  echo "NCAR Graphics High Level Utility $ex_type Example <$name>"
  echo ""
endif

set generic_name = `expr substr $name 1 4`

#******************************************#
#                                          #
# Determine if it's a C or Fortran example #
#                                          #
#******************************************#
unset fortran
if ( -e "$example_dir/$name.c") then
  set src_files = $name.c
else
  set src_files = $name.f
  set fortran
endif

#***************************#
#                           #
# Copy the needed resource, #
# .h, and/or data files     #
#                           #
#***************************#
set copy_files = "$src_files"

if ( -e "$example_dir/$name.res") then
    set r_file = $name.res
    set copy_files = "$copy_files $r_file"
endif

if ( -e "$example_dir/$generic_name.res") then
    set r_file = $generic_name.res
    set copy_files = "$copy_files $r_file"
endif

if ( -e "$example_dir/$name.h") then
    set h_file = $name.h
    set copy_files = "$copy_files $h_file"
endif

if ( -e "$example_dir/$name.asc") then
    set data_file = $name.asc
    set copy_files = "$copy_files $data_file"
endif

set rmfiles = ($name $name.o $copy_files)

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

#***********************#
#                       #
# Remove unwanted files #
#                       #
#***********************#
if ($?CleanOption) then
    /bin/rm $rmfiles >> /dev/null
endif

theend:

end
