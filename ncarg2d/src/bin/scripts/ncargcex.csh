#!/bin/csh -f
#
#	$Id: ncargcex.csh,v 1.27 1994-12-16 18:24:05 haley Exp $
#

#**************************#
#                          #
# ncargcex usage statement #
#                          #
#**************************#
if ($#argv < 1) then
  echo "usage: ncargcex [options] [example names]"
  echo ""
  echo " Options:"
  echo ""
  echo " To invoke various classes of examples:"
  echo "   [-A] [-E] [-F] [-P] [-T] [-U] [-ps] [-x11]"
  echo ""
  echo " To invoke various utilities:"
  echo "   [-areas] [-autograph] [-bivar] [-colconv] [-conpack]  "
  echo "   [-dashline] [dashpack] [-ezmap] [-field_flow]         "
  echo "   [-gflash] [-gks] [-gridall] [-histogram] [-isosurface]"
  echo "   [-labelbar] [-ngmisc] [-plotchar] [polypack]          "
  echo "   [-scrolled_title] [-softfill] [-spps] [-streamlines]  "
  echo "   [-surface] [-threed] [-vectors] [-wmap]"
  echo ""
  echo " Other options:"
  echo "   [-W workstation_type] [-n] [-clean] [-onebyone] names"
  echo ""
  echo "See <man ncargcex> for explanation of options." 
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

#****************************#
#                            #
# Check for directories that #
# contain the examples       #
#                            #
#****************************#
set example_dir=`ncargpath SED_EXAMPLESDIR`
if ($status != 0) then
  exit 1
endif

if (! -d "$example_dir") then
  echo "Example directory <$example_dir> does not exist."
  exit 1
endif
set fund_dir = $example_dir
set pdoc_dir = $example_dir

set test_dir=`ncargpath SED_TESTSDIR`
if ($status != 0) then
  exit 1
endif

if (! -d "$test_dir") then
  echo "Test directory <$test_dir> does not exist."
  exit 2
endif

set tutor_dir=`ncargpath SED_TUTORIALDIR`
if ($status != 0) then
  exit 1
endif

if (! -d "$tutor_dir") then
  echo "Test directory <$tutor_dir> does not exist."
  exit 2
endif

#*************************************************#
#                                                 #
# Initialize file types, orientation types, color #
# types, names of default output files, output    #
# messages, etc.                                  #
#                                                 #
# If new workstation types are added, this is     #
# where you specify the name for it               #
#                                                 #
#*************************************************#
set file_types     = (ncgm x11 text ps eps epsi)
set orient_types = (port land)
set color_types  = (color mono)
set ws_types = (\
                "ncgm.port.color" "" "" "" "" "" "" \
                "x11.port.color" "" "text.port.color" "" "" \
                "" "" "" "" "" "" "" \
                "ps.port.color" "eps.port.color" "epsi.port.color" \
                "ps.port.mono" "eps.port.mono" "epsi.port.mono" \
                "ps.land.color" "eps.land.color" "epsi.land.color" \
                "ps.land.mono" "eps.land.mono" "epsi.land.mono" )
set suffix_names = (\
                "ncgm" "" "" "" "" "" "" "" "" "txt" "" \
                "" "" "" "" "" "" "" "" \
                "ps" "eps" "epsi" "ps" "eps" "epsi" \
                "ps" "eps" "epsi" "ps" "eps" "epsi" )
set default_files = (\
                "gmeta" \
                "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" \
                "gmeta1.ps" "gmeta1.eps" "gmeta1.epsi" "gmeta1.ps" \
                "gmeta1.eps" "gmeta1.epsi" "gmeta1.ps" "gmeta1.eps" \
                "gmeta1.epsi" "gmeta1.ps" "gmeta1.eps" "gmeta1.epsi" )
set default_msgs = (\
	"Metafile file is named" \
	"" "" "" "" "" "" "" "" \
	"Text dump file is named" \
    "" "" "" "" "" "" "" "" "" \
	"Color portrait PostScript file is named" \
	"Color portrait encapsulated PostScript file is named" \
	"Color portrait interchange encapsulated PostScript file is named" \
	"Monochrome portrait PostScript file is named" \
	"Monochrome portrait encapsulated PostScript file is named" \
	"Monochrome portrait interchange encapsulated PostScript file is named" \
	"Color landscape PostScript file is named" \
	"Color landscape encapsulated PostScript file is named" \
	"Color landscape interchange encapsulated PostScript file is named" \
	"Monochrome landscape PostScript file is named" \
	"Monochrome landscape encapsulated PostScript file is named" \
	"Monochrome landscape interchange encapsulated PostScript file is named")

#**********************#
#                      #
#  Set areas examples  #
#                      #
#**********************#
set tst_areas  = (c_tareas)
set areas_list = ($tst_areas)

#**************************#
#                          #
#  Set autograph examples  #
#                          #
#**************************#
set ex_autograph   = (c_agex07)
set autograph_list = ($ex_autograph)

#**********************#
#                      #
#  Set bivar examples  #
#                      #
#**********************#
set ex_cbivar   = (c_cbex01)
set cbivar_list = ($ex_cbivar)

#************************#
#                        #
#  Set colconv examples  #
#                        #
#************************#
set ex_colconv   = (c_coex02)
set colconv_list = ($ex_colconv)

#************************#
#                        #
#  Set conpack examples  #
#                        #
#************************#
set ttr_conpack  = (c_colcon)
set ex_conpack   = (${ex_cbivar})
set conpack_list = ($ex_conpack $ttr_conpack)

#*************************#
#                         #
#  Set dashline examples  #
#                         #
#*************************#
set fnd_dashline  = (c_fdldashc)
set dashline_list = ($fnd_dashline)

#*************************#
#                         #
#  Set dashpack examples  #
#                         #
#*************************#
set tst_dashpack  = (c_tdshpk)
set dashpack_list = ($tst_dashpack)

#**********************#
#                      #
#  Set ezmap examples  #
#                      #
#**********************#
set ex_ezmap   = (c_mpex05 c_eezmpa)
set ezmap_list = ($ex_ezmap)

#***********************#
#                       #
#  Set gflash examples  #
#                       #
#***********************#
set tst_gflash  = (c_tgflas)
set gflash_list = ($tst_gflash)

#******************#
#                  #
# set gks examples #
#                  #
#******************#
set ex_gks     = (c_gtxpac)
set pdc_gks    = (c_pgkex21)
set gks_list   = ($ex_gks $pdc_gks)

#************************#
#                        #
#  Set gridall examples  #
#                        #
#************************#
set tst_gridall  = (c_tgrida)
set gridall_list = ($tst_gridall)

#**************************#
#                          #
#  Set histogram examples  #
#                          #
#**************************#
set tst_histogram  = (c_thstmv)
set histogram_list = ($tst_histogram)

#*************************#
#                         #
# set isosurface examples #
#                         #
#*************************#
set tst_isosurface  = (c_tisosr)
set isosurface_list = ($tst_isosurface)

#***********************#
#                       #
# set labelbar examples #
#                       #
#***********************#
set ex_labelbar   = (c_elblba)
set labelbar_list = ($ex_labelbar)

#*********************#
#                     #
# set ngmisc examples #
#                     #
#*********************#
set fnd_ngmisc  = (c_fngwsym)
set ngmisc_list = ($fnd_ngmisc)

#***********************#
#                       #
# set plotchar examples #
#                       #
#***********************#
set ex_plotchar   = (c_epltch)
set plotchar_list = ($ex_plotchar)

#***********************#
#                       #
# set polypack examples #
#                       #
#***********************#
set ex_polypack   = (c_ppex01)
set polypack_list = ($ex_polypack)

#*****************************#
#                             #
# set scrolled title examples #
#                             #
#*****************************#
set ex_scrlld_title   = (c_slex01)
set scrlld_title_list = (${ex_scrlld_title})

#***********************#
#                       #
# set softfill examples #
#                       #
#***********************#
set ex_softfill   = (c_sfex02)
set softfill_list = ($ex_softfill)

#*******************#
#                   #
# set spps examples #
#                   #
#*******************#
set fnd_spps  = (c_fcoord2)
set spps_list = ($fnd_spps)

#**************************#
#                          #
# set streamlines examples #
#                          #
#**************************#
set fnd_streamlines  = (c_ffex03)
set streamlines_list = ($fnd_streamlines)

#**********************#
#                      #
# set surface examples #
#                      #
#**********************#
set ex_surface   = (c_srex01)
set surface_list = ($ex_surface)

#*********************#
#                     #
# set threed examples #
#                     #
#*********************#
set fnd_threed  = (c_fthex01)
set threed_list = ($fnd_threed)

#**********************#
#                      #
# set vectors examples #
#                      #
#**********************#
set ex_vectors   = (c_vvex03)
set vectors_list = ($ex_vectors)

#*******************#
#                   #
# set wmap examples #
#                   #
#*******************#
set ex_wmap   = (c_wmex09)
set wmap_list = ($ex_wmap)

#****************************************#
#                                        #
#  Set field flow examples - consists of #
#  all streamlines and vectors examples  #
#                                        #
#****************************************#
set ex_field   = (c_vvex03)
set fnd_field  = (c_ffex03)
set field_list = ($ex_field $fnd_field)

#****************************************#
#                                        #
# Set lists of various types of examples #
#                                        #
#****************************************#
set x11_list = (c_xwndws)
set ex_list  = ($ex_autograph $ex_colconv $ex_conpack $ex_ezmap \
                $ex_field $ex_gks $ex_labelbar $ex_plotchar $ex_polypack \
                ${ex_scrlld_title} $ex_softfill $ex_surface \
                $ex_wmap)

set tst_list = ($tst_areas $tst_dashpack $tst_gflash $tst_gridall \
                $tst_histogram $tst_isosurface)

set ttr_list = ($ttr_conpack)

set fnd_list = ($fnd_dashline $fnd_field $fnd_ngmisc $fnd_spps $fnd_threed)
set pdc_list = ($pdc_gks)
set ps_list = (c_pgkex21)

#****************************************#
#                                        #
# Default is to load in the X11 library. #
# If the user specifies "-noX11" on the  #
# command line, then a stub will be      #
# linked instead of the X11 library      #
#                                        #
#****************************************#
set X11_option

#**********************************#
#                                  #
# Default workstation type is NCGM #
#                                  #
#**********************************#
set ws_type = "1"

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
      set names=($names $ex_list $tst_list $ttr_list $fnd_list $pdc_list)
      breaksw

    case "-allexamples":
    case "-E":
      shift
      set names=($names $ex_list)
      breaksw

    case "-alltests":
    case "-T":
      shift
      set names=($names $tst_list)
      breaksw
        
    case "-alltutorial":
    case "-U":
      shift
      set names=($names $ttr_list)
      breaksw
        
    case "-allfundamental":
    case "-F":
      shift
      set names=($names $fnd_list)
      breaksw
        
    case "-allpdocs":
    case "-P":
      shift
      set names=($names $pdc_list)
      breaksw
        
    case "-areas":
      shift
      set names=($names $areas_list)
      breaksw
        
    case "-autograph":
      shift
      set names=($names $autograph_list)
      breaksw

    case "-bivar":
      shift
      set names=($names $cbivar_list)
      breaksw

    case "-colconv":
      shift
      set names=($names $colconv_list)
      breaksw

    case "-conpack":
      shift
      set names=($names $conpack_list)
      breaksw

    case "-dashline":
      shift
      set names=($names $dashline_list)
      breaksw

    case "-dashpack":
      shift
      set names=($names $dashpack_list)
      breaksw

    case "-ezmap":
      shift
      set names=($names $ezmap_list)
      breaksw

    case "-field_flow":
      shift
      set names=($names $field_list)
      breaksw

    case "-gflash":
      shift
      set names=($names $gflash_list)
      breaksw

    case "-gks":
      shift
      set names=($names $gks_list)
      breaksw

    case "-gridall":
      shift
      set names=($names $gridall_list)
      breaksw

    case "-histogram":
      shift
      set names=($names $histogram_list)
      breaksw

    case "-isosurface":
      shift
      set names=($names $isosurface_list)
      breaksw

    case "-labelbar":
      shift
      set names=($names $labelbar_list)
      breaksw

    case "-ngmisc":
      shift
      set names=($names $ngmisc_list)
      breaksw

    case "-plotchar":
      shift
      set names=($names $plotchar_list)
      breaksw

    case "-polypack":
      shift
      set names=($names $polypack_list)
      breaksw

    case "-scrolled_title":
      shift
      set names=($names ${scrlld_title_list})
      breaksw

    case "-softfill":
      shift
      set names=($names $softfill_list)
      breaksw

    case "-spps":
      shift
      set names=($names $spps_list)
      breaksw

    case "-streamlines":
      shift
      set names=($names $streamlines_list)
      breaksw

    case "-surface":
      shift
      set names=($names $surface_list)
      breaksw

    case "-threed":
      shift
      set names=($names $threed_list)
      breaksw

    case "-vectors":
      shift
      set names=($names $vectors_list)
      breaksw

    case "-wmap":
      shift
      set names=($names $wmap_list)
      breaksw

    case "-ps":
      shift
      set names=($names $ps_list)
      breaksw

    case "-x11":
      shift
      set names=($names $x11_list)
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

    case "-unique":
      shift
      set Unique
      breaksw

    case "-W":
    case "-w":
      shift
      set ws_type = $1
      if ( !(`expr "$ws_type" : '[0-9]'`)) then
#***********************************************#
#                                               #
# The workstation type has been specified as a  #
# string and not a number, so we have to get    #
# the number by parsing the string.             #
#                                               #
# If the workstation is a PostScript file, then #
# it can have three different attributes:  file #
# type, orientation type, and color type.       #
#                                               #
#***********************************************#
        set file_type
        set orient_type
        set color_type
        set str1 = "$ws_type"
        set str2
        set str3
        set num = 1
#************************#
#                        #
# Parse the string.      #
#                        #
# String can be of form  #
# "xxx", "xxx.yyy", or   #
# "xxx.yyy.zzz"          #
#                        #
#************************#
        if ( `expr "$ws_type" : '.*\..*\..*'` ) then
#***********************#
#                       #
# String is xxx.yyy.zzz #
#                       #
#***********************#
          set str1 = `expr "$ws_type" : '\(.*\)\..*\..*'`
          set str2 = `expr "$ws_type" : '.*\.\(.*\)\..*'`
          set str3 = `expr "$ws_type" : '.*\..*\.\(.*\)'`
          set num = 3
        else
          if ( `expr "$ws_type" : '.*\..*'` ) then
#*******************#
#                   #
# String is xxx.yyy #
#                   #
#*******************#
            set str1 = `expr "$ws_type" : '\(.*\)\..*'`
            set str2 = `expr "$ws_type" : '.*\.\(.*\)'`
            set num = 2
          endif
        endif
        set found_strings = (1 1 1)  
        if ($str1 != "") set found_strings[1] = 0
        if ($str2 != "") set found_strings[2] = 0
        if ($str3 != "") set found_strings[3] = 0
        set strings = ($str1 $str2 $str3)
        set i = 1
        while ($i <= $num)
          set str = $strings[$i]
          foreach ftype($file_types)
            if ("$str" == "$ftype") then
              set found_strings[$i] = 1
              set file_type = "$str"
              break
            endif
          end
          foreach otype($orient_types)
            if ("$str" == "$otype") then
              set found_strings[$i] = 1
              set orient_type = "$str"
              break
            endif
          end
          foreach ctype($color_types)
            if ("$str" == "$ctype") then
              set found_strings[$i] = 1
              set color_type = "$str"
              break
            endif
          end
          @ i++
        end
#*********************************************#
#                                             #
# If the workstation type was not specified,  #
# or one of the attributes was not recognized #
# then this is an error.                      #
#                                             #
#*********************************************#
        if ("$file_type" == "")  then
          set not_valid
          goto invalid
        endif
        set i = 1
        while ($i <= $num)
          if ("$found_strings[$i]" == 0 ) then
            set not_valid
            goto invalid
          endif
          @ i++
        end  
        if ("$orient_type" == "") set orient_type = "port"
        if ("$color_type" == "") set color_type = "color"
        set str = "$file_type.$orient_type.$color_type"
#**************************************#
#                                      #
# Find the workstation type associated #
# with the string and assign the       #
# correct workstation number.          #
#                                      #
#**************************************#
        @ i = 1
        unset found
        while ($i <= $#ws_types)
          if ("$str" == "$ws_types[$i]" ) then
            set ws_type = "$i"
            set found
            break
          endif
          @ i++
        end
        if (! $?found) then
          set not_valid
          goto invalid
        endif        
      else
        if ("$ws_type" < 1 || "$ws_type" > $#ws_types ) then
          set not_valid
          goto invalid
        else
          if ("$ws_types[$ws_type]" == "" ) then
            set not_valid
            goto invalid
          endif
        endif
      endif
invalid:
      if ($?not_valid) then
          echo ""
          echo "    '$1' is not a valid workstation type"
          echo ""
          exit 1
      endif
      shift
      breaksw

    case "-noX11":
      shift
      set X11_option = "-noX11"
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

#********************************************#
#                                            #
# Cannot have both interactive and noX11 set #
#                                            #
#********************************************#

if ($X11_option == "-noX11" && "$ws_type" == "8") then
    echo ""
    echo "Warning:  You cannot use the '-noX11' option if you are"
    echo "          running an interactive example.  I will turn"
    echo "          the '-noX11' option off."
    echo ""
    set X11_option
endif

#***************************#
#                           #
# Loop through each example #
#                           #
#***************************#
foreach name ($names)

unset no_file
unset tmp_ws_type
unset tmp_msg
set input
set output

#*************************************#
#                                     #
# Find out what type of example it is #
#                                     #
#*************************************#
set ex_type="Unknown"

foreach known ($ex_list)
  if ("$name" == "$known") then
    set ex_type
    set temp_dir = "$example_dir"
    break
  endif
end

if ( $ex_type == "Unknown" ) then
  foreach known ($tst_list)
    if ("$name" == "$known") then
      set ex_type=" Test"
      set temp_dir = "$test_dir"
      break
    endif
  end
endif

if ( $ex_type == "Unknown" ) then
  foreach known ($ttr_list)
    if ("$name" == "$known") then
      set ex_type=" Tutorial"
      set temp_dir = "$tutor_dir"
      break
    endif
  end
endif

if ( $ex_type == "Unknown" ) then
  foreach known ($fnd_list $x11_list)
    if ("$name" == "$known") then
      set ex_type=" Fundamentals"
      set temp_dir = "$fund_dir"
      break
    endif
  end
endif

if ( $ex_type == "Unknown" ) then
  foreach known ($pdc_list)
    if ("$name" == "$known") then
      set ex_type=" Programmer"
      set temp_dir = "$pdoc_dir"
      break
    endif
  end
endif

#***************#
#               #
# Echo the type #
#               #
#***************#

if ( "$ex_type" == "Unknown" ) then
  echo ""
  echo "ncargcex: <$name> is not a known example"
  echo ""
  goto theend
else
  echo ""
  echo "NCAR Graphics C$ex_type Example <$name>"
  echo ""
endif

#**************************************#
#                                      #
# Check this particular example to see #
# if there's anything special about it #
#                                      #
#**************************************#
switch($name)
    case c_pgkex21:
      set tmp_ws_type = "20"
      echo ""
      echo "  This example was set up to demonstrate the Postscript"
      echo "  driver, so workstation type 20 is being used."
      echo ""
    breaksw
endsw

if ($?tmp_ws_type) then
  set the_ws_type = "$tmp_ws_type"
else
  set the_ws_type = "$ws_type"
endif

#***************************************#
#                                       #
# If the workstation type is "8" (X11)  #
# or "10" (text dump) then no file is   #
# created when the example is executed. #
#                                       #
#***************************************#
if ("$the_ws_type" == "8" || "$the_ws_type" == "10" ) set no_file

#**************************************#
#                                      #
# Initialize the name of the default   #
# output file and the name of the file #
# it is going to be renamed to.        #
#                                      #
#**************************************#
set suffix = "$suffix_names[$the_ws_type]"
set graphic_file = "$name.$suffix"
set default_file = $default_files[$the_ws_type]
set msg = "$default_msgs[$the_ws_type] $graphic_file."
set main = "$name.c"

if ($?tmp_msg) then
  set msg = "$tmp_msg"
endif

if ("$the_ws_type" == "8") then
  echo ""
  echo "NOTE: This example is being run interactively and can only"
  echo "      be executed if you have X running and have your     "
  echo "      DISPLAY environment variable set properly.  It will "
  echo "      create an X11 window that you must click on with your"
  echo "      mouse to advance the frame(s)."
  echo ""
endif

#**********************************#
#                                  #
# For workstation "10" (text dump) #
# the output goes to stdout        #
#                                  #
#**********************************#
if ("$the_ws_type" == "10") then
  set output = "$graphic_file"
endif


#***********************************************#
#                                               #
# If the "-unique" option was selected and the  #
# NCGM already exists, don't generate it again. #
#                                               #
#***********************************************#
if ($?Unique && -f $graphic_file) goto theend

#************************************************#
#                                                #
# If the workstation type is an X11 workstation, #
# then the X11 library must be linked.           #
#                                                #
#************************************************#
if ("$ws_type" == "8") then
  set ncargccflags = ($X11_option)
else
  set ncargccflags = ("")
endif

#**************************#
#                          #
# Some examples need extra #
# C files, data files, or  #
# special compiler options #
#                          #
#**************************#
set extra_c_files
set data_files

switch ($name)
    case c_ffex03:
        set data_files = (ffex02.dat)
    breaksw

    case c_srex01:
        set data_files = (srex01.dat)
    breaksw
endsw

#**********************************#
#                                  #
# Check if this particular example #
# needs a data file                #
#                                  #
#**********************************#
if ("$data_files" != "") then
  set input = "$data_files"
endif

#******************************#
#                              #
# Modify the main program to   #
# give it the workstation type #
#                              #
#******************************#
   
echo "  Copying $main"
echo ""
cp $temp_dir/$main ./$main
ed << EOF - ./$main >& /dev/null
g/SED_WSTYPE/s//$the_ws_type/g
w
q
EOF

set c_files = ($extra_c_files $main)

#***********************#
#                       #
# Copy the needed files #
#                       #
#***********************#
   
set copy_files = ($extra_c_files $data_files)

foreach file($copy_files)
    echo "  Copying $file"
    echo ""
    cp $temp_dir/$file .
end

#******************************#
#                              #
# Compile and link the example #
#                              #
#******************************#
   
if (! $?NoRunOption) then
    echo "Compiling and linking..."
    ncargcc $ncargccflags -o $name $c_files
    if ($status != 0) then
        echo ""
        echo "The compile and link failed."
        echo ""
        exit
    endif
#*****************#
#                 #
# Run the example #
#                 #
#*****************#
    set rename_option = "-o $graphic_file"
    if ($?no_file) set rename_option
    echo ""
    echo "Executing <$name>..."
    if ("$input" != "" ) then
      if ("$output" != "") then
        ncargrun $rename_option ./$name < $input > $output
      else 
        ncargrun $rename_option ./$name < $input
      endif
    else
      if ("$output" != "") then
        ncargrun $rename_option ./$name > $output
      else 
        ncargrun $rename_option ./$name
      endif
    endif
    if ($status != 0) then
        echo ""
        echo "The execution of ./$name failed"
        echo ""
        exit
    endif
    echo ""
    echo "$msg"
    echo ""
endif

#***********************#
#                       #
# Remove unwanted files #
#                       #
#***********************#
set rmfiles = ($data_files $c_files $name)
foreach file($c_files)
  set obj_file = {$file:r}.o
  set rmfiles = ($rmfiles $obj_file)
end

if ($?CleanOption) /bin/rm $rmfiles >& /dev/null

#************************#
#                        #
# Display NCGM on screen #
# as it is generated     #
#                        #
#************************#
if ($?OneByOneOption) then
  if ( -f $graphic_file) then
    if ($suffix == "ncgm") then
      ctrans -d X11 -geometry 1142x865+10+0 $graphic_file
    else if ("$suffix" == "ps" || "$suffix" == "eps" || \
             "$suffix" == "epsi" ) then
      gs $graphic_file
    endif
    /bin/rm -f $graphic_file $rmfiles >& /dev/null
  endif
endif

theend:

end
