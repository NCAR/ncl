#!/bin/csh -f
#
#	$Id: ncargcex_local.csh,v 1.1 1994-10-04 17:32:48 haley Exp $
#

#********************#
#                    #
#   NCARGCEX_LOCAL USAGE   #
#                    #
#********************#
if ($#argv < 1) then
  echo "usage: ncargcex_local [-all,-A] [-areas] [-autograph] [-bivar]      "
  echo "                [-colconv] [-conpack] [-dashline] [dashpack]        "
  echo "                [-ezmap] [-field_flow] [-gflash] [-gks] [-gridall]  "
  echo "                [-histogram] [-isosurface] [-labelbar] [-misc]      "
  echo "                [-ngmisc] [-plotchar] [polypack] [-scrolled_title]  "
  echo "                [-seter] [-softfill] [-spps] [-streamlines]         "
  echo "                [-surface] [-threed] [-vectors] [-wmap] [-inter]    "
  echo "                [-clean] [-n] [-onebyone] [-W n] names              "
  echo ""
  echo "See <man ncargcex_local>                                            "
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

set tutorial_dir = `ncargpath SED_TUTORIALDIR`

if ($status != 0) then
        exit 1
endif

if (! -d "$tutorial_dir") then
  echo "Tutorial directory <$tutorial_dir> does not exist."
  exit 1
endif


#**********************#
#                      #
#  Set areas examples  #
#                      #
#**********************#
set areas_list = (c_arex01 c_arex02 c_cardb1 c_cardb2 c_caredg c_carfill \
                  c_carline c_carmap c_tareas)
set ex_list = ($areas_list)

#**************************#
#                          #
#  Set autograph examples  #
#                          #
#**************************#
set autograph_list = (c_agex01 c_agex02 c_agex03 c_agex04 c_agex05 c_agex06 \
                      c_agex07 c_agex08 c_agex09 c_agex10 c_agex11 c_agex12 \
                      c_agex13 c_class2 c_class3 c_fagaxclr c_fagaxlbl \
                      c_fagaxmax c_fagcuclr c_fagcudsh c_fagezmxy c_fagezmy \
                      c_fagezxy c_fagezy c_fagilclr c_fagovrvw c_tagupw \
                      c_tautog)
set ex_list = ($ex_list $autograph_list)

#******************************#
#                              #
#  Set bivar/conpack examples  #
#                              #
#******************************#
set cbivar_list = (c_cbex01 c_cidsfft)
set ex_list = ($ex_list $cbivar_list)

#************************#
#                        #
#  Set colconv examples  #
#                        #
#************************#
set colconv_list   = (c_coex01 c_coex02 c_coex03 c_fcce01 c_fcce02 c_tcolcv)
set ex_list = ($ex_list $colconv_list)

#************************#
#                        #
#  Set conpack examples  #
#                        #
#************************#
set conpack_list = (c_ccpback c_ccpcff c_ccpcfx c_ccpcica c_ccpcir c_ccpcis \
                    c_ccpcit c_ccpclc c_ccpcld c_ccpcldm c_ccpcldr c_ccpcll \
                    c_ccpclu c_ccpcnrc c_ccpdflt c_ccpezct c_ccpfil c_ccpga \
                    c_ccphand c_ccphcf c_ccphl c_ccphlt c_ccpila c_ccpils \
                    c_ccpilt c_ccpklb c_ccplbam c_ccplbdr c_ccpline c_ccpllb \
                    c_ccpllc c_ccplll c_ccpllo c_ccpllp c_ccpllt c_ccpllw \
                    c_ccpmap c_ccpmovi c_ccpmpxy c_ccpncls c_ccpnet c_ccpnof \
                    c_ccpnsd c_ccppc c_ccppc1 c_ccppc2 c_ccppc3 c_ccppc4 \
                    c_ccppkcl c_ccppole c_ccprc c_ccprect c_ccprwc c_ccprwu \
                    c_ccpscam c_ccpset c_ccpsps1 c_ccpsps2 c_ccpspv c_ccpt2d \
                    c_ccptitle c_ccpvp c_ccpvs c_colcon c_cpex01 c_cpex02 \
                    c_cpex03 c_cpex04 c_cpex05 c_cpex06 c_cpex07 c_cpex08 \
                    c_cpex09 c_cpex10 c_cpex11 c_cpex12 c_tconpa)
set ex_list = ($ex_list $conpack_list)

#*************************#
#                         #
#  Set dashline examples  #
#                         #
#*************************#
set dashline_list  = (c_fdlcurvd c_fdldashc c_fdldashd c_fdlsmth c_tdashc \
                      c_tdashl c_tdashp c_tdashs)
set ex_list = ($ex_list $dashline_list)

#*************************#
#                         #
#  Set dashpack examples  #
#                         #
#*************************#
set dashpack_list  = (c_tdshpk)
set ex_list = ($ex_list $dashpack_list)

#**********************#
#                      #
#  Set ezmap examples  #
#                      #
#**********************#
set ezmap_list = (c_cezmap1 c_cezmap2 c_cezmap3 c_cmpclr c_cmpdd c_cmpdrw \
                  c_cmpel c_cmpfil c_cmpgci c_cmpgrd c_cmpgrp c_cmpita \
                  c_cmpitm c_cmplab c_cmplbl c_cmplot c_cmpmsk c_cmpou \
                  c_cmppos c_cmpsat c_cmpsup c_cmptit c_cmptra c_cmpusr \
                  c_eezmpa c_mpex01 c_mpex02 c_mpex03 c_mpex04 c_mpex05 \
                  c_mpex06 c_mpex07 c_mpex08 c_mpex09 c_mpex10 c_mpexfi \
                  c_tezmap c_tezmpa)
set ex_list = ($ex_list $ezmap_list)

#***********************#
#                       #
#  Set gflash examples  #
#                       #
#***********************#
set gflash_list  = (c_tgflas)
set ex_list = ($ex_list $gflash_list)

#******************#
#                  #
# set gks examples #
#                  #
#******************#

set gks_list = (c_class1 c_gtxpac c_fcell c_fcell0 c_fcirc c_fgke02 c_fgke03 \
                c_fgkgtx c_fgklnclr c_fgklnwth c_fgkgpl c_fgkgpm c_fgpm01 \
                c_pgkex01 c_pgkex02 c_pgkex03 c_pgkex04 c_pgkex05 c_pgkex06 \
                c_pgkex07 c_pgkex08 c_pgkex09 c_pgkex10 c_pgkex11 c_pgkex12 \
                c_pgkex13 c_pgkex14 c_pgkex15 c_pgkex16 c_pgkex17 c_pgkex18 \
                c_pgkex19 c_pgkex20 c_pgkex21 c_pgkex22 c_pgkex23 c_pgkex24 \
                c_pgkex25 c_pgkex26 c_test09 c_tgesc)
set ex_list = ($ex_list $gks_list)

#************************#
#                        #
#  Set gridall examples  #
#                        #
#************************#
set gridall_list  = (c_tgrida)
set ex_list = ($ex_list $gridall_list)

#**************************#
#                          #
#  Set histogram examples  #
#                          #
#**************************#
set histogram_list  = (c_thstgr c_thstmv)
set ex_list = ($ex_list $histogram_list)

#*************************#
#                         #
# set isosurface examples #
#                         #
#*************************#
set isosurface_list  = (c_fisissrf c_fispwrzi c_tisosr c_tpwrzi)
set ex_list = ($ex_list $isosurface_list)

#***********************#
#                       #
# set labelbar examples #
#                       #
#***********************#
set labelbar_list   = (c_clbbar c_clbfil c_clblbr c_elblba c_tlblba)
set ex_list = ($ex_list $labelbar_list)

#****************************#
#                            #
# set miscellaneous examples #
#                            #
#****************************#
set misc_list   = (c_animat c_bnchmk c_example c_plcmnt )
set ex_list = ($ex_list $misc_list)

#*********************#
#                     #
# set ngmisc examples #
#                     #
#*********************#
set ngmisc_list  = (c_fngngdts c_fngwsym)
set ex_list = ($ex_list $ngmisc_list)

#***********************#
#                       #
# set plotchar examples #
#                       #
#***********************#
set plotchar_list   = (c_epltch c_fpcfonts c_fpchiqu c_fpcloqu c_tpltch)
set ex_list = ($ex_list $plotchar_list)

#***********************#
#                       #
# set polypack examples #
#                       #
#***********************#
set polypack_list   = (c_ppex01 c_tppack)
set ex_list = ($ex_list $polypack_list)

#*****************************#
#                             #
# set scrolled title examples #
#                             #
#*****************************#
set scrlld_title_list   = (c_fslfont c_slex01 c_tstitl)
set ex_list = ($ex_list $scrlld_title_list)

#********************#
#                    #
# set seter examples #
#                    #
#********************#
set seter_list  = (c_tseter)
set ex_list = ($ex_list $seter_list)

#***********************#
#                       #
# set softfill examples #
#                       #
#***********************#
set softfill_list   = (c_fsfsgfa c_fsfwrld c_sfex01 c_sfex02 c_tsoftf)
set ex_list = ($ex_list $softfill_list)

#*******************#
#                   #
# set spps examples #
#                   #
#*******************#

set spps_list   = (c_fcoord c_fcoord1 c_fcoord2 c_fspcurve c_fspline \
                   c_fsppoint c_fspponts c_splogy c_spps c_sprevx)
set ex_list = ($ex_list $spps_list)

#**************************#
#                          #
# set streamlines examples #
#                          #
#**************************#
set streamlines_list   = (c_ffex00 c_ffex01 c_ffex03 c_ffex04 c_fstream \
                          c_stex01 c_stex02 c_stex03 c_tstrml c_tstrm2)
set ex_list = ($ex_list $streamlines_list)

#**********************#
#                      #
# set surface examples #
#                      #
#**********************#
set surface_list   = (c_fsrezsrf c_fsrpwrzs c_fsrsrfac c_srex01 c_tpwrzs \
                      c_tsrfac )
set ex_list = ($ex_list $surface_list)

#*********************#
#                     #
# set threed examples #
#                     #
#*********************#
set threed_list  = (c_fthex01 c_fthex02 c_fthex03 c_fthex04 c_fthex05 \
                    c_tpwrzt c_tthre2 c_tthree)
set ex_list = ($ex_list $threed_list)

#**********************#
#                      #
# set vectors examples #
#                      #
#**********************#
set vectors_list = (c_fcover c_ffex00 c_ffex01 c_ffex02 c_ffex05 c_tvelvc \
                    c_vvex01 c_vvex02 c_vvex03)
set ex_list = ($ex_list $vectors_list)

#*******************#
#                   #
# set wmap examples #
#                   #
#*******************#
set wmap_list = (c_wmex01 c_wmex02 c_wmex03 c_wmex04)
set ex_list = ($ex_list $wmap_list)

#****************************************#
#                                        #
#  Set field flow examples - consists of #
#  all streamlines and vectors examples  #
#                                        #
#****************************************#
set field_list   = (c_fcover c_ffex00 c_ffex01 c_ffex02 c_ffex03 c_ffex04 \
                    c_ffex05 c_fstream c_stex01 c_stex02 c_stex03 c_tstrm2 \
                    c_tstrml c_tvelvc c_vvex01 c_vvex02 c_vvex03)
set ex_list = ($ex_list $field_list)

#**************************#
#                          #
# set interactive examples #
#                          #
#**************************#
set interactive_list = (c_nlines c_nmrkpos c_pgkex06 c_pgkex07 c_pgkex08 \
                        c_pgkex12 c_pgkex18 c_test12 c_test28 c_test30 \
                        c_test38 c_test41 c_xwndws)
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

#*********************************#
#                                 #
# Default workstation type is "1" #
# Default graphic is NCGM         #
#                                 #
#*********************************#
set ncgmfile
set ws_type = "1"

while ($#argv > 0)
    
    switch ($1)

        case "-all":
        case "-A":
            shift
            set names=($ex_list)
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

        case "-misc":
            shift
            set names=($names $misc_list)
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

        case "-seter":
            shift
            set names=($names $seter_list)
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

        case "-inter":
            shift
            set interfile
            set ws_type = "8"
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

        case "-W":
            shift
            set ws_type = "$1"

            switch ($ws_type)
            
            case  "1":
            case "10":
                set ncgmfile
            breaksw

            case "8":
                set interfile
            breaksw


            case "20":
            case "23":
            case "26":
            case "29":
                unset ncgmfile
                set psfile
            breaksw

            case "21":
            case "24":
            case "27":
            case "30":
                unset ncgmfile
                set epsfile
            breaksw

            case "22":
            case "25":
            case "28":
            case "31":
                unset ncgmfile
                set epsifile
            breaksw

            default:
                echo ""
                echo "    ncargcex_local:  $ws_type is an invalid workstation type."
                echo ""
                exit 1
            endsw
            shift
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

#********************************************#
#                                            #
# Cannot have both interactive and noX11 set #
#                                            #
#********************************************#

if ($X11_option == "-noX11" && $?interfile) then
    echo ""
    echo "Warning:  You cannot use the '-noX11' option if you are"
    echo "          running an interactive example.  I will turn"
    echo "          the '-noX11' option off."
    echo ""
    set X11_option
endif

#***********************************************#
#                                               #
# Cannot have both interactive and ws_type != 8 #
#                                               #
#***********************************************#
if ($?interfile && $ws_type != "8") then
    echo ""
    echo "Warning:  You must have a workstation type of '8' if you"
    echo "          are running an interactive example.  I will force"
    echo "          ws_type to be '8'.  If you specify the '-inter'"
    echo "          option, ws_type will be set to 8 automatically."
    echo ""
    set ws_type = "8"
endif

foreach name ($names)

switch($name)
    case c_pgkex19:
    case c_pgkex20:
    case c_pgkex21:
    case c_pgkex22:
    case c_pgkex23:
        unset ncgmfile
        set graphic_type = "ps"
        set default_file = "gmeta1.ps"
        set message = "PostScript file is named"
    breaksw

    case c_pgkex26:
    case c_fgke03:
        unset ncgmfile
        set graphic_type = "ncgm"
    breaksw

    default:
        if ($?psfile) then
            set default_file = "gmeta1.ps"
            set graphic_type = "ps"
            set message = "PostScript file is named"
        else if ($?epsfile) then
            set default_file = "gmeta1.eps"
            set graphic_type = "eps"
            set message = "Encapsulated PostScript file is named"
        else if ($?epsifile) then
            set default_file = "gmeta1.epsi"
            set graphic_type = "epsi"
            set message = "Interchange Encapsulated PostScript file is named"
        else 
            set default_file = "gmeta"
            set graphic_type = "ncgm"
            set message = "Metafile file is named"
        endif
    breaksw
endsw

set graphic_file = $name.$graphic_type

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
        echo "NCAR Graphics C Example <$name>"
    breaksw

    case Interactive_Example:
        echo "NCAR Graphics Interactive C Example <$name>"
    breaksw

    case Unknown:
        echo "ncargcex_local: <$name> is not a known example"
        goto theend
    breaksw
endsw
echo ""

#***********************************************#
#                                               #
# If the "-unique" option was selected and the  #
# NCGM already exists, don't generate it again. #
#                                               #
#***********************************************#

if ($?Unique && -f $graphic_file) goto theend

set c_files
set ncargf77flags
set rmfiles = "$name.c"
set copy_files

#**********************************#
#                                  #
# Some examples need extra C files #
#                                  #
#**********************************#

switch ($name)
    case c_cpex01:
    case c_cpex02:
    case c_cpex03:
    case c_cpex04:
    case c_cpex05:
    case c_cpex06:
    case c_cpex07:
    case c_cpex08:
    case c_cpex09:
        set c_files = (c_cpexcc.c)
        set copy_files = ($example_dir/c_cpexcc.c)
        set rmfiles = ($rmfiles c_cpexcc.c c_cpexcc.o)
    breaksw

    case c_ccpcica:
    case c_ccpcir:
    case c_ccpcnrc:
    case c_ccpezct:
    case c_ccphl:
    case c_ccpmap:
    case c_ccpmovi:
    case c_ccpvp:
        set c_files = (c_ggdini.c)
        set copy_files = ($example_dir/c_ggdini.c)
        set rmfiles = ($rmfiles c_ggdini.c c_ggdini.o)
    breaksw
endsw

#*************************************#
#                                     #
# Some examples need extra data files #
#                                     #
#*************************************#

switch ($name)
    case c_ccpils:
    case c_ccpilt:
	case c_ccplbdr:
	case c_ccptitle:
        set copy_files = ($copy_files $tutorial_dir/ccpex.dat)
        set rmfiles=($rmfiles ccpex.dat)
    breaksw

    case c_ccpila:
    case c_ccpt2d:
        set copy_files = ($copy_files $tutorial_dir/ccpila.dat)
        set rmfiles=($rmfiles ccpila.dat)
    breaksw

    case c_ccpmpxy:
        set copy_files = ($copy_files $tutorial_dir/cpmpxy1.dat $tutorial_dir/cpmpxy2.dat)
        set rmfiles=($rmfiles cpmpxy1.dat cpmpxy2.dat)
    breaksw

    case c_mpexfi:
        set copy_files = ($copy_files $example_dir/mpexfi.dat)
        set rmfiles=($rmfiles mpexfi.dat)
    breaksw

    case c_ffex02:
    case c_ffex03:
        set copy_files = ($copy_files $example_dir/ffex02.dat)
        set rmfiles=($rmfiles ffex02.dat)
    breaksw

    case c_ffex05:
        set copy_files = ($copy_files $example_dir/ffex05.dat)
        set rmfiles=($rmfiles ffex05.dat)
    breaksw

    case c_class1:
        set copy_files = ($copy_files $tutorial_dir/class1.dat)
        set rmfiles=($rmfiles class1.dat)
    breaksw

    case c_fcover:
        set copy_files = ($copy_files $example_dir/fcover.dat)
        set rmfiles=($rmfiles fcover.dat)
    breaksw

    case c_srex01:
        set copy_files = ($copy_files $example_dir/srex01.dat)
        set rmfiles=($rmfiles srex01.dat)
    breaksw

    case c_agex13:
        set copy_files = ($copy_files $example_dir/agda13.dat)
        set rmfiles=($rmfiles agda13.dat)
    breaksw

#**********************************************************#
#                                                          #
# Set special ncargf77 flags for some of the test examples #
#                                                          #
#**********************************************************#
# quick routines
    case c_tdashl:
        set ncargf77flags = "-quick"
    breaksw

# smooth routines (default)
    case c_tdashs:
        set ncargf77flags = "-smooth"
    breaksw

# super routines
    case c_tdashp:
    case c_fdlsmth:
        set ncargf77flags = "-super"
    breaksw

# autograph with pwritx for character generation
    case c_tagupw:
        set ncargf77flags = "-agupwrtx"
    breaksw
endsw

#***********************#
#                       #
# Copy the needed files #
#                       #
#***********************#
   
echo "  Copying $name.c"
ed << EOF - $example_dir/$name.c >& /dev/null
g/SED_WSTYPE/s//$ws_type/g
w ./$name.c
q
EOF

set c_files = ($c_files $name.c)

foreach file($copy_files)
    echo "  Copying $file:t"
    cp $file $file:t
end


#******************************#
#                              #
# Compile and link the example #
#                              #
#******************************#

unset not_valid_metafile
unset no_file

if (! $?NoRunOption) then
    if ($type == "Interactive_Example" && $?interfile) then
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
    echo ""
    echo "Executing <$name>..."
#*****************#
#                 #
# Run the example #
#                 #
#*****************#
   
    if ($?ncgmfile) setenv NCARG_GKS_OUTPUT $name.ncgm
    switch( $name )
        case c_mpexfi:
            ./$name < mpexfi.dat
        breaksw
        case c_class1:
            ./$name < class1.dat
        breaksw
        case c_ffex02:
        case c_ffex03:
            ./$name < ffex02.dat
        breaksw
        case c_ffex05:
            ./$name < ffex05.dat
        breaksw
        case c_fcover:
            ./$name < fcover.dat
        breaksw
        case c_srex01:
            ./$name < srex01.dat
        breaksw
        case c_agex13:
            ./$name < agda13.dat
        breaksw
        case c_fgke03:
        case c_pgkex26:
            ./$name
            echo ""
            echo "Metafiles META01 and META02 produced."
            echo ""
            set no_file
        breaksw
        case c_tcolcv:
        case c_fcce02:
        case c_ccpcff:
        case c_tgesc:
            ./$name
            set not_valid_metafile
            echo ""
            echo "NOTE: No valid metafile is produced by this example."
            echo ""
        breaksw
        default:
            ./$name
    endsw
    if ( ! $?not_valid_metafile && ! $?no_file ) then
        if (! $?ncgmfile ) then
            mv ./$default_file $graphic_file
        endif
        echo ""
        echo "$message $graphic_file"
        echo ""
    endif

    set rmfiles = ($rmfiles $name.o $name)
endif

#******************************#
#                              #
# Keep track of unwanted files #
#                              #
#******************************#
switch ($name)
    case c_slex01:
    case c_tstitl:
    case c_fslfont:
        set rmfiles = ($rmfiles GNFB09)
    breaksw

    case c_ccpmovi:
        set rmfiles = ($rmfiles GNFB00)
    breaksw

    case c_tgflas:
        set rmfiles = ($rmfiles GNFB01 GNFB02 GNFB03 GNFB04)
    breaksw

    case c_test09:
        set rmfiles = ($rmfiles GNFB01 GNFB02 GNFB03 GNFB04 GNFB05 GNFB06 \
                                GNFB07 GNFB08 GNFB09 GNFB10 GNFB11)
    breaksw

endsw


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

