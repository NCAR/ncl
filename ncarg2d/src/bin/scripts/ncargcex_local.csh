#!/bin/csh -f
#
#	$Id: ncargcex_local.csh,v 1.8 1994-12-19 18:45:37 stautler Exp $
#

#********************************#
#                                #
# ncargcex_local usage statement #
#                                #
#********************************#
if ($#argv < 1) then
  echo "usage: ncargcex_local [options] [example names]"
  echo ""
  echo " Options:"
  echo ""
  echo " To invoke various classes of examples:"
  echo "   [-A] [-ps] [-x11]"
  echo ""
  echo " To invoke various utilities:"
  echo "   [-areas] [-autograph] [-bivar] [-colconv] [-conpack]  "
  echo "   [-dashline] [dashpack] [-ezmap] [-field_flow]         "
  echo "   [-gflash] [-gks] [-gridall] [-histogram] [-isosurface]"
  echo "   [-labelbar] [-misc] [-ngmisc] [-plotchar] [polypack]  "
  echo "   [-scrolled_title] [-seter] [-softfill] [-spps]        "
  echo "   [-streamlines] [-surface] [-threed] [-vectors] [-wmap]"
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
set ex_areas   = (c_arex01 c_arex02)
set tst_areas  = (c_tareas)
set ttr_areas  = (c_cardb1 c_caredg c_carline c_cardb2 c_carfill c_carmap)
set areas_list = ($ex_areas $tst_areas $ttr_areas)

#**************************#
#                          #
#  Set autograph examples  #
#                          #
#**************************#
set ex_autograph   = (c_agex01 c_agex02 c_agex03 c_agex04 c_agex05 c_agex06 \
                      c_agex07 c_agex08 c_agex09 c_agex10 c_agex11 c_agex12 \
                      c_agex13)
set tst_autograph  = (c_tautog c_tagupw)
set fnd_autograph  = (c_fagaxclr c_fagaxlbl c_fagaxmax c_fagcuclr \
                      c_fagcudsh c_fagezmxy c_fagezmy c_fagezxy c_fagezy \
                      c_fagilclr c_fagovrvw)
set autograph_list = ($ex_autograph $tst_autograph $fnd_autograph)

#**********************#
#                      #
#  Set bivar examples  #
#                      #
#**********************#
set ex_cbivar   = (c_cbex01)
set ttr_cbivar  = (c_cidsfft)
set cbivar_list = ($ex_cbivar $ttr_cbivar)

#************************#
#                        #
#  Set colconv examples  #
#                        #
#************************#
set ex_colconv   = (c_coex01 c_coex02 c_coex03)
set tst_colconv  = (c_tcolcv)
set fnd_colconv  = (c_fcce01 c_fcce02)
set colconv_list = ($ex_colconv $tst_colconv $fnd_colconv)

#************************#
#                        #
#  Set conpack examples  #
#                        #
#************************#
set ex_conpack   = (c_cpex01 c_cpex02 c_cpex03 c_cpex04 c_cpex05 c_cpex06 \
                    c_cpex07 c_cpex08 c_cpex09 c_cpex10 c_cpex11 c_cpex12 \
                    ${ex_cbivar})
set tst_conpack  = (c_tconpa)
set ttr_conpack  = (\
   c_ccpback c_ccpcff c_ccpcfx c_ccpcica c_ccpcir c_ccpcis c_ccpcit c_ccpclc \
   c_ccpcld c_ccpcldm c_ccpcldr c_ccpcll c_ccpclu c_ccpcnrc c_ccpdflt \
   c_ccpezct c_ccpfil c_ccpga c_ccphand c_ccphcf c_ccphl c_ccphlt c_ccpila \
   c_ccpils c_ccpilt c_ccpklb c_ccplbam c_ccplbdr c_ccpline c_ccpllb \
   c_ccpllc c_ccplll c_ccpllo c_ccpllp c_ccpllt c_ccpllw c_ccpmap \
   c_ccpmovi c_ccpmpxy c_ccpncls c_ccpnet c_ccpnof c_ccpnsd c_ccppc c_ccppc1 \
   c_ccppc2 c_ccppc3 c_ccppc4 c_ccppkcl c_ccppole c_ccpt2d c_ccprc c_ccprect \
   c_ccprwc c_ccprwu c_ccpscam c_ccpset c_ccpsps1 c_ccpsps2 c_ccpspv \
   c_ccptitle c_ccpvp c_ccpvs c_colcon ${ttr_cbivar})
set conpack_list = ($ex_conpack $tst_conpack $ttr_conpack)

#*************************#
#                         #
#  Set dashline examples  #
#                         #
#*************************#
set tst_dashline  = (c_tdashc c_tdashl c_tdashp c_tdashs)
set fnd_dashline  = (c_fdlcurvd c_fdldashc c_fdldashd c_fdlsmth)
set dashline_list = ($tst_dashline $fnd_dashline)

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
set ex_ezmap   = (\
     c_mpex01 c_mpex02 c_mpex03 c_mpex04 c_mpex05 c_mpex06 c_mpex07 c_mpex08 \
     c_mpex09 c_mpex10 c_mpexfi c_eezmpa)
set tst_ezmap  = (c_tezmap c_tezmpa)
set ttr_ezmap  = (\
     c_cezmap1 c_cezmap2 c_cezmap3 c_cmpclr c_cmpdd c_cmpdrw c_cmpel \
     c_cmpfil c_cmpgci c_cmpgrd c_cmpgrp c_cmpita c_cmpitm c_cmplab c_cmplbl \
     c_cmplot c_cmpmsk c_cmpou c_cmppos c_cmpsat c_cmpsup c_cmptit c_cmptra \
     c_cmpusr)
set ezmap_list = ($ex_ezmap $tst_ezmap $ttr_ezmap)

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
set ex_gks     = (c_gtxpac c_test09 c_tgesc)
set fnd_gks    = (c_fgke02 c_fgke03 c_fcell c_fcell0 c_fgpm01 c_fgkgpl \
                  c_fgkgpm c_fgkgtx c_fgklnclr c_fgklnwth c_fcirc)
set pdc_gks    = (\
    c_pgkex01 c_pgkex02 c_pgkex03 c_pgkex04 c_pgkex05 c_pgkex06 c_pgkex07 \
    c_pgkex08 c_pgkex09 c_pgkex10 c_pgkex11 c_pgkex12 c_pgkex13 c_pgkex14 \
    c_pgkex15 c_pgkex16 c_pgkex17 c_pgkex18 c_pgkex19 c_pgkex20 c_pgkex21 \
    c_pgkex22 c_pgkex23 c_pgkex24 c_pgkex25 c_pgkex26)
set gks_list   = ($fnd_gks $pdc_gks $ex_gks)

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
set tst_histogram  = (c_thstgr c_thstmv)
set histogram_list = ($tst_histogram)

#*************************#
#                         #
# set isosurface examples #
#                         #
#*************************#
set tst_isosurface  = (c_tisosr c_tpwrzi)
set fnd_isosurface  = (c_fisissrf c_fispwrzi)
set isosurface_list = ($tst_isosurface $fnd_isosurface)

#***********************#
#                       #
# set labelbar examples #
#                       #
#***********************#
set ex_labelbar   = (c_elblba)
set tst_labelbar  = (c_tlblba)
set fnd_labelbar  = (c_clbfil c_clbbar c_clblbr)
set labelbar_list = ($ex_labelbar $tst_labelbar $fnd_labelbar)

#****************************#
#                            #
# set miscellaneous examples #
#                            #
#****************************#
set ex_misc   = (c_animat c_bnchmk c_example c_plcmnt)
set misc_list = ($ex_misc)

#*********************#
#                     #
# set ngmisc examples #
#                     #
#*********************#
set fnd_ngmisc  = (c_fngngdts c_fngwsym)
set ngmisc_list = ($fnd_ngmisc)

#***********************#
#                       #
# set plotchar examples #
#                       #
#***********************#
set ex_plotchar   = (c_epltch)
set tst_plotchar  = (c_tpltch)
set fnd_plotchar  = (c_fpchiqu c_fpcloqu c_fpcfonts)
set plotchar_list = ($ex_plotchar $tst_plotchar $fnd_plotchar)

#***********************#
#                       #
# set polypack examples #
#                       #
#***********************#
set ex_polypack   = (c_ppex01)
set tst_polypack  = (c_tppack)
set polypack_list = ($ex_polypack $tst_polypack)

#*****************************#
#                             #
# set scrolled title examples #
#                             #
#*****************************#
set ex_scrlld_title   = (c_slex01)
set tst_scrlld_title  = (c_tstitl)
set fnd_scrlld_title  = (c_fslfont)
set scrlld_title_list = (${ex_scrlld_title} ${tst_scrlld_title} \
                         ${fnd_scrlld_title})

#********************#
#                    #
# set seter examples #
#                    #
#********************#
set tst_seter  = (c_tseter)
set seter_list = ($tst_seter)

#***********************#
#                       #
# set softfill examples #
#                       #
#***********************#
set ex_softfill   = (c_sfex01 c_sfex02)
set tst_softfill  = (c_tsoftf)
set fnd_softfill  = (c_fsfwrld c_fsfsgfa)
set softfill_list = ($ex_softfill $tst_softfill $fnd_softfill)

#*******************#
#                   #
# set spps examples #
#                   #
#*******************#
set ex_spps   = (c_splogy c_sprevx)
set fnd_spps  = (c_fspcurve c_fspline c_fsppoint c_fspponts \
                 c_fcoord c_fcoord1 c_fcoord2)
set spps_list = ($ex_spps $fnd_spps)

#**************************#
#                          #
# set streamlines examples #
#                          #
#**************************#
set ex_streamlines   = (c_stex01 c_stex02 c_stex03)
set tst_streamlines  = (c_tstrml)
set fnd_streamlines  = (c_fstream c_ffex00 c_ffex01 c_ffex03 c_ffex04)
set streamlines_list = ($ex_streamlines $tst_streamlines $fnd_streamlines)

#**********************#
#                      #
# set surface examples #
#                      #
#**********************#
set ex_surface   = (c_srex01)
set tst_surface  = (c_tsrfac c_tpwrzs)
set fnd_surface  = (c_fsrezsrf c_fsrpwrzs c_fsrsrfac)
set surface_list = ($ex_surface $tst_surface $fnd_surface)

#*********************#
#                     #
# set threed examples #
#                     #
#*********************#
set tst_threed  = (c_tthree c_tpwrzt)
set fnd_threed  = (c_fthex01 c_fthex02 c_fthex03 c_fthex04 c_fthex05)
set threed_list = ($tst_threed $fnd_threed)

#**********************#
#                      #
# set vectors examples #
#                      #
#**********************#
set ex_vectors   = (c_vvex01 c_vvex02 c_vvex03)
set tst_vectors  = (c_tvelvc)
set fnd_vectors  = (c_ffex00 c_ffex01 c_ffex02 c_ffex05 c_fcover)
set vectors_list = ($ex_vectors $tst_vectors $fnd_vectors)

#*******************#
#                   #
# set wmap examples #
#                   #
#*******************#
set ex_wmap   = (c_wmex01 c_wmex02 c_wmex03 c_wmex04 c_wmex05 c_wmex06 \
                 c_wmex07 c_wmex08 c_wmex09 c_wmex10 c_wmex11 c_wmex12 \
                 c_wmex13 c_wmex14)
set wmap_list = ($ex_wmap)

#****************************************#
#                                        #
#  Set field flow examples - consists of #
#  all streamlines and vectors examples  #
#                                        #
#****************************************#
set ex_field   = (c_stex01 c_stex02 c_stex03 c_vvex01 c_vvex02 c_vvex03)
set tst_field  = (c_tstrml c_tvelvc)
set fnd_field  = (c_fstream c_ffex00 c_ffex01 c_ffex02 c_ffex03 c_ffex04 \
                  c_ffex05 c_fcover)
set field_list = ($ex_field $tst_field $fnd_field)

#********************#
#                    #
# set class examples #
#                    #
#*******************#
set ttr_class   = (c_class1 c_class2 c_class3)
set class_list  = ($ttr_class)

#****************************************#
#                                        #
# Set lists of various types of examples #
#                                        #
#****************************************#
set x11_list = (c_nlines c_nmrkpos c_pgkex06 c_pgkex07 c_pgkex08 \
                c_pgkex12 c_pgkex18 c_test12 c_test28 c_test30 \
                c_test38 c_test41 c_xwndws c_fgke01 c_fgke04)
set ex_list  = ($ex_areas $ex_autograph $ex_colconv $ex_conpack $ex_ezmap \
                $ex_field $ex_labelbar $ex_plotchar $ex_polypack \
                ${ex_scrlld_title} $ex_softfill $ex_spps $ex_surface \
                $ex_wmap $ex_misc)

set tst_list = ($tst_areas $tst_autograph $tst_colconv $tst_conpack \
                $tst_dashline $tst_dashpack $tst_ezmap $tst_field \
                $tst_gflash $tst_gridall $tst_histogram $tst_isosurface \
                $tst_labelbar $tst_plotchar $tst_polypack ${tst_scrlld_title} \
                $tst_seter $tst_softfill $tst_surface $tst_threed)

set ttr_list = ($ttr_areas $ttr_conpack $ttr_ezmap $ttr_class)

set fnd_list = ($fnd_autograph $fnd_colconv $fnd_dashline $fnd_field $fnd_gks \
                $fnd_isosurface $fnd_labelbar $fnd_ngmisc $fnd_plotchar \
                ${fnd_scrlld_title} $fnd_softfill $fnd_spps $fnd_surface \
                $fnd_threed)

set pdc_list = ($pdc_gks)
set ps_list = (c_pgkex19 c_pgkex20 c_pgkex21 c_pgkex22 c_pgkex23)

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
  echo "ncargcex_local: <$name> is not a known example"
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

    case c_wmex12:
    case c_wmex13:
    case c_wmex14:
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
    case c_ccpcica:
    case c_ccpcir:
    case c_ccpcnrc:
    case c_ccpezct:
    case c_ccphl:
    case c_ccpmap:
    case c_ccpmovi:
    case c_ccpvp:
        set extra_c_files = (c_ggdini.c)
    breaksw

    case c_ccpila:
    case c_ccpt2d:
        set data_files = (ccpila.dat)
    breaksw

    case c_ccpils:
    case c_ccpilt:
    case c_ccplbdr:
    case c_ccptitle:
        set data_files = (ccpex.dat)
    breaksw

    case c_ccpmpxy:
        set data_files = (cpmpxy1.dat cpmpxy2.dat)
    breaksw

    case c_class1:
        set data_files = (class1.dat)
    breaksw

    case c_cpex01:
    case c_cpex02:
    case c_cpex03:
    case c_cpex04:
    case c_cpex05:
    case c_cpex06:
    case c_cpex07:
    case c_cpex08:
    case c_cpex09:
        set extra_c_files = (c_cpexcc.c)
    breaksw

    case c_fcover:
        set data_files = (fcover.dat)
    breaksw

    case c_ffex02:
    case c_ffex03:
        set data_files = (ffex02.dat)
    breaksw

    case c_ffex05:
        set data_files = (ffex05.dat)
    breaksw

    case c_mpexfi:
        set data_files = (mpexfi.dat)
    breaksw

    case c_srex01:
        set data_files = (srex01.dat)
    breaksw
#************************************************#           
#                                                #
# autograph with pwritx for character generation #
#                                                #
#************************************************#           
    case c_tagupw:
        set ncargccflags = ($ncargccflags "-agupwrtx")
    breaksw

#***************************#           
#                           #
# smooth routines (default) #
#                           #
#***************************#           
    case c_tdashs:
        set ncargccflags = ($ncargccflags "-smooth")
    breaksw
#****************#           
#                #
# quick routines #
#                #
#****************#           
    case c_tdashl:
        set ncargccflags = ($ncargccflags "-quick")
    breaksw
#****************#
#                #
# super routines #
#                #
#****************#
    case c_tdashp: 
    case c_fdlsmth:
        set ncargccflags = ($ncargccflags "-super")
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
