#!/bin/csh -f
#
#	$Id: ncargcex.csh,v 1.17 1994-10-26 13:41:33 haley Exp $
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
  echo "   [-A] [-x11]"
  echo ""
  echo " To invoke various utilities:"
  echo "   [-autograph] [-bivar] [-conpack] [-ezmap] [-gks]     "
  echo "   [-labelbar] [-plotchar] [-scrolled_title] [-softfill]"
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
#  Set conpack examples  #
#                        #
#************************#
set ex_conpack  = (c_colcon)
set conpack_list = ($ex_conpack)

#**********************#
#                      #
#  Set ezmap examples  #
#                      #
#**********************#
set ex_ezmap   = (c_mpex05 c_eezmpa)
set ezmap_list = ($ex_ezmap)

#******************#
#                  #
# set gks examples #
#                  #
#******************#
set ex_gks     = (c_gtxpac)
set gks_list   = ($ex_gks)

#***********************#
#                       #
# set labelbar examples #
#                       #
#***********************#
set ex_labelbar   = (c_elblba)
set labelbar_list = ($ex_labelbar)

#***********************#
#                       #
# set plotchar examples #
#                       #
#***********************#
set ex_plotchar   = (c_epltch)
set plotchar_list = ($ex_plotchar) 

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

#****************************************#
#                                        #
# Set lists of various types of examples #
#                                        #
#****************************************#
set x11_list = (c_xwndws)
set ex_list  = ($ex_autograph $ex_conpack $ex_ezmap $ex_gks $ex_labelbar \
                $ex_plotchar ${ex_scrlld_title} $ex_softfill)

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
      set names=($names $ex_list)
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

    case "-unique"
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

unset tmp_ws_type
unset tmp_msg
unset no_file

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
    case c_pgkex19:
    case c_pgkex20:
    case c_pgkex21:
    case c_pgkex22:
    case c_pgkex23:
      set tmp_ws_type = "20"
      echo ""
      echo "  This example was set up to demonstrate the Postscript"
      echo "  driver, so workstation type 20 is being used."
      echo ""
    breaksw

    case c_wmex01:
    case c_wmex02:
    case c_wmex04:
      if ($ws_type >= 20 && $ws_type < 31) then
        set tmp_ws_type = "26"
        echo ""
        echo "  This example was set up to use the entire"
        echo "  page when going to PostScript, so workstation"
        echo "  type 26 is being used."
        echo ""
      endif
    breaksw

    case c_pgkex26:
    case c_fgke03:
      set tmp_ws_type = "1"
      echo ""
      echo "  This example was set up to demonstrate how to change"
      echo "  the name of the metafile from within the program."
      echo ""
      set tmp_msg = "Metafiles META01 and META02 produced."
      set no_file
    breaksw

    case c_fgke01:
    case c_fgke04:
      echo ""
      echo "  This example was set up to demonstrate the X11"
      echo "  driver.  It also generates a graphic file."
      echo ""
    breaksw

    case c_ccpcff:
    case c_tcolcv:
    case c_fcce02:
      unset tmp_ws_type
      set no_file
      set tmp_msg = "   "
      echo ""
      echo "  No graphics file will be produced by this example."
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
  set output = "> $graphic_file"
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

#**********************************#
#                                  #
# Some examples need extra Fortran #
# files, data files, or special    #
# compiler options                 #
#                                  #
#**********************************#
set extra_c_files
set data_files

#******************************#
#                              #
# Modify the main program to   #
# give it the workstation type #
#                              #
#******************************#
   
echo "  Copying $name.c"
echo ""
ed << EOF - $temp_dir/$name.c >& /dev/null
g/SED_WSTYPE/s//$the_ws_type/g
w ./$name.c
q
EOF

set c_files = ($extra_c_files $name.c)

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
    echo ""
    echo "Executing <$name>..."
    eval "./$name $input $output"
    if ($status != 0) then
        echo ""
        echo "The execution of ./$name failed"
        echo ""
        /bin/rm ./$default_file >& /dev/null
        exit
    endif
    if ( ! $?no_file ) then
        mv ./$default_file $graphic_file
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
