#!/bin/csh -f
#
#   $Id: ncargex.csh,v 1.79 1994-11-29 15:34:48 haley Exp $
#

#*************************#
#                         #
# ncargex usage statement #
#                         #
#*************************#
if ($#argv < 1) then
  echo "usage: ncargex [options] [example names]"
  echo ""
  echo " Options:"
  echo ""
  echo " To invoke various classes of examples:"
  echo "   [-A] [-E] [-F] [-P] [-T] [-U] [-class] [-ps] [-x11]"
  echo ""
  echo " To invoke various utilities:"
  echo "   [-areas] [-autograph] [-bivar] [-colconv] [-conpack]"
  echo "   [-conran_family] [-conrec_family] [-dashline]       "
  echo "   [-dashpack] [-ezmap] [-field_flow] [-gflash] [-gks] "
  echo "   [-gridall] [-halftone] [-histogram] [-isosrfhr]     "
  echo "   [-isosurface] [-labelbar] [-ngmisc] [-plotchar]     "
  echo "   [-polypack] [-pwrite_family] [-scrolled_title]      "
  echo "   [-seter] [-softfill] [-spps] [-streamlines]         "
  echo "   [-surface] [-threed] [-vectors] [-wmap] [-misc]     "
  echo ""
  echo " Other options:"
  echo "   [-W workstation_type] [-n] [-clean] [-onebyone] names"
  echo ""
  echo "See <man ncargex> for explanation of options." 
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
set ex_areas   = (arex01 arex02)
set tst_areas  = (tareas)
set ttr_areas  = (cardb1 caredg carline cardb2 carfill carmap)
set areas_list = ($ex_areas $tst_areas $ttr_areas)

#**************************#
#                          #
#  Set autograph examples  #
#                          #
#**************************#
set ex_autograph   = (agex01 agex02 agex03 agex04 agex05 agex06 agex07 \
                      agex08 agex09 agex10 agex11 agex12 agex13)
set tst_autograph  = (tautog tagupw)
set fnd_autograph  = (fagaxclr fagaxlbl fagaxmax fagcuclr fagcudsh fagezmxy \
                      fagezmy fagezxy fagezy fagilclr fagovrvw)
set autograph_list = ($ex_autograph $tst_autograph $fnd_autograph)

#**********************#
#                      #
#  Set bivar examples  #
#                      #
#**********************#
set ex_cbivar   = (cbex01)
set ttr_cbivar  = (cidsfft)
set cbivar_list = ($ex_cbivar $ttr_cbivar)

#************************#
#                        #
#  Set colconv examples  #
#                        #
#************************#
set ex_colconv   = (coex01 coex02 coex03)
set tst_colconv  = (tcolcv)
set fnd_colconv  = (fcce01 fcce02)
set colconv_list = ($ex_colconv $tst_colconv $fnd_colconv)

#************************#
#                        #
#  Set conpack examples  #
#                        #
#************************#
set ex_conpack   = (cpex01 cpex02 cpex03 cpex04 cpex05 cpex06 cpex07 \
                    cpex08 cpex09 cpex10 cpex11 cpex12 ${ex_cbivar})
set tst_conpack  = (tconpa)
set ttr_conpack  = (ccpback ccpcff ccpcfx ccpcica ccpcir ccpcis ccpcit ccpclc \
                    ccpcld ccpcldm ccpcldr ccpcll ccpclu ccpcnrc ccpdflt \
                    ccpezct ccpfil ccpga ccphand ccphcf ccphl ccphlt ccpila \
                    ccpils ccpilt ccpklb ccplbam ccplbdr ccpline ccpllb \
                    ccpllc ccplll ccpllo ccpllp ccpllt ccpllw ccpmap \
                    ccpmovi ccpmpxy ccpncls ccpnet ccpnof ccpnsd ccppc ccppc1 \
                    ccppc2 ccppc3 ccppc4 ccppkcl ccppole ccpt2d ccprc ccprect \
                    ccprwc ccprwu ccpscam ccpset ccpsps1 ccpsps2 ccpspv \
                    ccptitle ccpvp ccpvs colcon ${ttr_cbivar})
set conpack_list = ($ex_conpack $tst_conpack $ttr_conpack)

#******************************#
#                              #
#  Set conran_family examples  #
#                              #
#******************************#
set tst_cnrn_family  = (tconan tconaq tconas)
set cnrn_family_list = (${tst_cnrn_family})

#******************************#
#                              #
#  Set conrec_family examples  #
#                              #
#******************************#
set tst_cnrc_family  = (tconre tcnqck tcnsmt tcnsup)
set cnrc_family_list = (${tst_cnrc_family})

#*************************#
#                         #
#  Set dashline examples  #
#                         #
#*************************#
set tst_dashline  = (tdashc tdashl tdashp tdashs)
set fnd_dashline  = (fdlcurvd fdldashc fdldashd fdlsmth)
set dashline_list = ($tst_dashline $fnd_dashline)

#***********************#
#                       #
# set dashpack examples #
#                       #
#***********************#
set tst_dashpack  = (tdshpk)
set dashpack_list = ($tst_dashpack)

#**********************#
#                      #
#  Set ezmap examples  #
#                      #
#**********************#
set ex_ezmap   = (mpex01 mpex02 mpex03 mpex04 mpex05 mpex06 mpex07 mpex08 \
                  mpex09 mpex10 mpexfi eezmpa)
set tst_ezmap  = (tezmap tezmpa)
set ttr_ezmap  = (cezmap1 cezmap2 cezmap3 cmpclr cmpdd cmpdrw cmpel \
                  cmpfil cmpgci cmpgrd cmpgrp cmpita cmpitm cmplab cmplbl \
                  cmplot cmpmsk cmpou cmppos cmpsat cmpsup cmptit cmptra \
                  cmpusr)
set ezmap_list = ($ex_ezmap $tst_ezmap $ttr_ezmap)

#***********************#
#                       #
#  Set gflash examples  #
#                       #
#***********************#
set tst_gflash  = (tgflas)
set gflash_list = ($tst_gflash)

#******************#
#                  #
# set gks examples #
#                  #
#******************#
set fnd_gks    = (fgke02 fgke03 fcell fcell0 fgpm01 fgkgpl fgkgpm fgkgtx \
                  fgklnclr fgklnwth fcirc)
set pdc_gks    = (pgkex01 pgkex02 pgkex03 pgkex04 pgkex05 pgkex06 pgkex07 \
                  pgkex08 pgkex09 pgkex10 pgkex11 pgkex12 pgkex13 pgkex14 \
                  pgkex15 pgkex16 pgkex17 pgkex18 pgkex19 pgkex20 pgkex21 \
                  pgkex22 pgkex23 pgkex24 pgkex25 pgkex26)
set gks_list   = ($fnd_gks $pdc_gks)

#************************#
#                        #
#  Set gridall examples  #
#                        #
#************************#
set tst_gridall  = (tgrida)
set gridall_list = ($tst_gridall)

#*************************#
#                         #
#  Set halftone examples  #
#                         #
#*************************#
set tst_halftone  = (thafto)
set halftone_list = ($tst_halftone)

#**************************#
#                          #
#  Set histogram examples  #
#                          #
#**************************#
set tst_histogram  = (thstgr thstmv)
set histogram_list = ($tst_histogram)

#***********************#
#                       #
# set isosrfhr examples #
#                       #
#***********************#
set tst_isosrfhr  = (tisohr)
set isosrfhr_list = ($tst_isosrfhr)

#*************************#
#                         #
# set isosurface examples #
#                         #
#*************************#
set tst_isosurface  = (tisosr tpwrzi)
set fnd_isosurface  = (fisissrf fispwrzi)
set isosurface_list = ($tst_isosurface $fnd_isosurface)

#***********************#
#                       #
# set labelbar examples #
#                       #
#***********************#
set ex_labelbar   = (elblba)
set tst_labelbar  = (tlblba)
set fnd_labelbar  = (clbfil clbbar clblbr)
set labelbar_list = ($ex_labelbar $tst_labelbar $fnd_labelbar)

#*********************#
#                     #
# set ngmisc examples #
#                     #
#*********************#
set fnd_ngmisc  = (fngngdts fngwsym)
set ngmisc_list = ($fnd_ngmisc)

#***********************#
#                       #
# set plotchar examples #
#                       #
#***********************#
set ex_plotchar   = (epltch)
set tst_plotchar  = (tpltch)
set fnd_plotchar  = (fpchiqu fpcloqu fpcfonts)
set plotchar_list = ($ex_plotchar $tst_plotchar $fnd_plotchar)

#***********************#
#                       #
# set polypack examples #
#                       #
#***********************#
set ex_polypack   = (ppex01)
set tst_polypack  = (tppack)
set polypack_list = ($ex_polypack $tst_polypack)

#****************************#
#                            #
# set pwrite_family examples #
#                            #
#****************************#
set tst_pwrite  = (tpwrtx tpwry)
set pwrite_list = ($tst_pwrite)

#*****************************#
#                             #
# set scrolled title examples #
#                             #
#*****************************#
set ex_scrlld_title   = (slex01)
set tst_scrlld_title  = (tstitl)
set fnd_scrlld_title  = (fslfont)
set scrlld_title_list = (${ex_scrlld_title} ${tst_scrlld_title} \
                         ${fnd_scrlld_title})

#********************#
#                    #
# set seter examples #
#                    #
#********************#
set tst_seter  = (tseter)
set seter_list = ($tst_seter)

#***********************#
#                       #
# set softfill examples #
#                       #
#***********************#
set ex_softfill   = (sfex01 sfex02)
set tst_softfill  = (tsoftf)
set fnd_softfill  = (fsfwrld fsfsgfa)
set softfill_list = ($ex_softfill $tst_softfill $fnd_softfill)

#*******************#
#                   #
# set spps examples #
#                   #
#*******************#
set ex_spps   = (splogy sprevx)
set fnd_spps  = (fspcurve fspline fsppoint fspponts fcoord fcoord1 fcoord2)
set spps_list = ($ex_spps $fnd_spps)

#**************************#
#                          #
# set streamlines examples #
#                          #
#**************************#
set ex_streamlines   = (stex01 stex02 stex03)
set tst_streamlines  = (tstrml)
set fnd_streamlines  = (fstream ffex00 ffex01 ffex03 ffex04)
set streamlines_list = ($ex_streamlines $tst_streamlines $fnd_streamlines)

#**********************#
#                      #
# set surface examples #
#                      #
#**********************#
set ex_surface   = (srex01)
set tst_surface  = (tsrfac tpwrzs)
set fnd_surface  = (fsrezsrf fsrpwrzs fsrsrfac)
set surface_list = ($ex_surface $tst_surface $fnd_surface)

#*********************#
#                     #
# set threed examples #
#                     #
#*********************#
set tst_threed  = (tthree tpwrzt)
set fnd_threed  = (fthex01 fthex02 fthex03 fthex04 fthex05)
set threed_list = ($tst_threed $fnd_threed)

#**********************#
#                      #
# set vectors examples #
#                      #
#**********************#
set ex_vectors   = (vvex01 vvex02 vvex03)
set tst_vectors  = (tvelvc)
set fnd_vectors  = (ffex00 ffex01 ffex02 ffex05 fcover)
set vectors_list = ($ex_vectors $tst_vectors $fnd_vectors)

#*******************#
#                   #
# set wmap examples #
#                   #
#*******************#
set ex_wmap   = (wmex01 wmex02 wmex03 wmex04)
set wmap_list = ($ex_wmap)

#****************************************#
#                                        #
#  Set field flow examples - consists of #
#  all streamlines and vectors examples  #
#                                        #
#****************************************#
set ex_field   = (stex01 stex02 stex03 vvex01 vvex02 vvex03)
set tst_field  = (tstrml tvelvc)
set fnd_field  = (fstream ffex00 ffex01 ffex02 ffex03 ffex04 ffex05 fcover)
set field_list = ($ex_field $tst_field $fnd_field)

#****************************#
#                            #
# set miscellaneous examples #
#                            #
#****************************#
set ex_misc   = (example bnchmk)
set misc_list = ($ex_misc)

#********************#
#                    #
# set class examples #
#                    #
#*******************#
set ttr_class   = (class1 class2 class3)
set class_list  = ($ttr_class)

#*************************************************************#
#                                                             #
# Some of the other examples are considered tutorial examples #
#                                                             #
#*************************************************************#
set ttr_overlap = (mpex03 mpex05 arex01 sfex01 tsoftf)

#****************************************#
#                                        #
# Set lists of various types of examples #
#                                        #
#****************************************#
set x11_list = (fgke01 fgke04)
set ex_list  = ($ex_areas $ex_autograph $ex_colconv $ex_conpack $ex_ezmap \
                $ex_field $ex_labelbar $ex_plotchar $ex_polypack \
                ${ex_scrlld_title} $ex_softfill $ex_spps $ex_surface \
                $ex_wmap $ex_misc)

set tst_list = ($tst_areas $tst_autograph $tst_colconv $tst_conpack \
                ${tst_cnrn_family} ${tst_cnrc_family} $tst_dashline \
                $tst_dashpack \
                $tst_ezmap $tst_field $tst_gflash $tst_gridall $tst_halftone \
                $tst_histogram $tst_isosrfhr $tst_isosurface $tst_labelbar \
                $tst_plotchar $tst_polypack $tst_pwrite ${tst_scrlld_title} \
                $tst_seter $tst_softfill $tst_surface $tst_threed)

set ttr_list = ($ttr_areas $ttr_conpack $ttr_ezmap $ttr_class)

set fnd_list = ($fnd_autograph $fnd_colconv $fnd_dashline $fnd_field $fnd_gks \
                $fnd_isosurface $fnd_labelbar $fnd_ngmisc $fnd_plotchar \
                ${fnd_scrlld_title} $fnd_softfill $fnd_spps $fnd_surface \
                $fnd_threed)

set pdc_list = ($pdc_gks)
set ps_list = (pgkex19 pgkex20 pgkex21 pgkex22 pgkex23)

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
      set names=($names $ttr_list $ttr_overlap)
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

    case "-conran_family":
      shift
      set names=($names ${cnrn_family_list})
      breaksw

    case "-conrec_family":
      shift
      set names=($names ${cnrc_family_list})
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

    case "-halftone":
      shift
      set names=($names $halftone_list)
      breaksw

    case "-histogram":
      shift
      set names=($names $histogram_list)
      breaksw

    case "-isosrfhr":
      shift
      set names=($names $isosrfhr_list)
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

    case "-pwrite_family":
      shift
      set names=($names $pwrite_list)
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

    case "-misc":
      shift
      set names=($names $misc_list)
      breaksw

    case "-class":
      shift
      set names=($names $class_list)
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
  echo "ncargex: <$name> is not a known example"
  echo ""
  goto theend
else
  echo ""
  echo "NCAR Graphics Fortran$ex_type Example <$name>"
  echo ""
endif

#**************************************#
#                                      #
# Check this particular example to see #
# if there's anything special about it #
#                                      #
#**************************************#
switch($name)
    case pgkex19:
    case pgkex20:
    case pgkex21:
    case pgkex22:
    case pgkex23:
      set tmp_ws_type = "20"
      echo ""
      echo "  This example was set up to demonstrate the Postscript"
      echo "  driver, so workstation type 20 is being used."
      echo ""
    breaksw

    case wmex01:
    case wmex02:
    case wmex04:
      if ($ws_type >= 20 && $ws_type < 31) then
        set tmp_ws_type = "26"
        echo ""
        echo "  This example was set up to use the entire"
        echo "  page when going to PostScript, so workstation"
        echo "  type 26 is being used."
        echo ""
      endif
    breaksw

    case pgkex26:
    case fgke03:
      unset tmp_ws_type
      echo ""
      echo "  This example was set up to demonstrate how to change"
      echo "  the name of the metafile from within the program."
      echo ""
      set tmp_msg = "Metafiles META01 and META02 produced."
      set no_file
    breaksw

    case fgke01:
    case fgke04:
      echo ""
      echo "  This example was set up to demonstrate the X11"
      echo "  driver.  It also generates a graphic file."
      echo ""
    breaksw

    case ccpcff:
    case tcolcv:
    case fcce02:
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
set main = "$name.f"

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
  set ncargf77flags = ($X11_option)
else
  set ncargf77flags = ("")
endif

#**********************************#
#                                  #
# Some examples need extra Fortran #
# files, data files, or special    #
# compiler options                 #
#                                  #
#**********************************#
set extra_fort_files
set data_files

switch ($name)
    case agex13:
        set data_files = (agda13.dat)
    breaksw

    case ccpcica:
    case ccpcir:
    case ccpcnrc:
    case ccpezct:
    case ccphl:
    case ccpmap:
    case ccpmovi:
    case ccpvp:
        set extra_fort_files = (ggdini.f)
    breaksw

    case ccpila:
    case ccpt2d:
        set data_files = (ccpila.dat)
    breaksw

    case ccpils:
    case ccpilt:
    case ccplbdr:
    case ccptitle:
        set data_files = (ccpex.dat)
    breaksw

    case ccpmpxy:
        set data_files = (cpmpxy1.dat)
    breaksw

    case class1:
        set data_files = (class1.dat)
    breaksw

    case cpex01:
    case cpex02:
    case cpex03:
    case cpex04:
    case cpex05:
    case cpex06:
    case cpex07:
    case cpex08:
    case cpex09:
        set extra_fort_files = (cpexcc.f)
    breaksw

    case fcover:
        set data_files = (fcover.dat)
    breaksw

    case ffex02:
    case ffex03:
        set data_files = (ffex02.dat)
    breaksw

    case ffex05:
        set data_files = (ffex05.dat)
    breaksw

    case mpex01:
    case mpex02:
    case mpex03:
    case mpex04:
    case mpex05:
    case mpex06:
    case mpex07:
    case mpex08:
    case mpex09:
    case mpex10:
        set extra_fort_files = (mpexcc.f)
    breaksw

    case mpexfi:
        set data_files = (mpexfi.dat)
        set extra_fort_files = (mpexcc.f)
    breaksw

    case srex01:
        set data_files = (srex01.dat)
    breaksw
#************************************************#           
#                                                #
# autograph with pwritx for character generation #
#                                                #
#************************************************#           
    case tagupw:
        set ncargf77flags = ($ncargf77flags "-agupwrtx")
    breaksw

#***************************#           
#                           #
# smooth routines (default) #
#                           #
#***************************#           
    case tdashs:
    case tcnsmt:
    case tconan:
        set ncargf77flags = ($ncargf77flags "-smooth")
    breaksw
#****************#           
#                #
# quick routines #
#                #
#****************#           
    case tdashl:
    case tcnqck:
    case tconaq:
        set ncargf77flags = ($ncargf77flags "-quick")
    breaksw
#****************#
#                #
# super routines #
#                #
#****************#
    case tdashp:
    case tcnsup:
    case tconas:
    case fdlsmth:
        set ncargf77flags = ($ncargf77flags "-super")
    breaksw

    case vvex01:
    case vvex02:
        set extra_fort_files = (vvexcc.f)
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

set fort_files = ($extra_fort_files $main)

#***********************#
#                       #
# Copy the needed files #
#                       #
#***********************#
   
set copy_files = ($extra_fort_files $data_files)

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
    ncargf77 $ncargf77flags -o $name $fort_files
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
set rmfiles = ($data_files $fort_files $name)
foreach file($fort_files)
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
