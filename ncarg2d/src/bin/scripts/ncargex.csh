#!/bin/csh -f
#
#   $Id: ncargex.csh,v 1.175 2010-05-07 16:15:39 haley Exp $
#                                                                      
#                Copyright (C)  2000
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# The use of this Software is governed by a License Agreement.
#

if ($#argv < 1) goto usage

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
# where you specify the name for it.  The         #
# position (index, starting at "1") of the        # 
# workstation description in the following        #
# lists corresponds to its associated integer     #
# value.                                          #
#                                                 #
#*************************************************#
set file_types     = (ncgm x11 text oldps eps epsi oldpdf ps png pdf)
set orient_types = (port land)
set color_types  = (color mono)
set ws_types = (                                                   \
                "ncgm.port.color"                                  \
                "" "" "" "" "" ""                                  \
                "x11.port.color" "" "text.port.color"              \
                "oldpdf.port.color" "oldpdf.land.color"            \
                "" "" "" "" "" "" ""                               \
                "oldps.port.color" "eps.port.color" "epsi.port.color" \
                "oldps.port.mono"  "eps.port.mono"  "epsi.port.mono"  \
                "oldps.land.color" "eps.land.color" "epsi.land.color" \
                "oldps.land.mono"  "eps.land.mono"  "epsi.land.mono"  \
                "" "" "" "" "" "" "" ""                            \
                "ps.port.color" "png.port.color" "pdf.port.color" \
               )
set suffix_names = (                                               \
                "ncgm"                                             \
                "" "" "" "" "" ""                                  \
                "" "" "txt"                                        \
                "pdf" "pdf"                                        \
                "" "" "" "" "" "" ""                               \
                "ps" "eps" "epsi"                                  \
                "ps" "eps" "epsi"                                  \
                "ps" "eps" "epsi"                                  \
                "ps" "eps" "epsi"                                  \
                "" "" "" "" "" "" "" ""                            \
                "ps" "png" "pdf"                                   \
                    )
set default_msgs = (                                                       \
  "Metafile file is named"                                                 \
  "" "" "" "" "" ""                                                        \
  "" "" "Text dump file is named"                                          \
  "Older-style color portrait PDF file is named"                                       \
  "Older-style Color landscape PDF file is named"                                      \
  "" "" "" "" "" "" ""                                                     \
  "Older-style color portrait PostScript file is named"                                \
  "Color portrait encapsulated PostScript file is named"                   \
  "Color portrait interchange encapsulated PostScript file is named"       \
  "Older-style monochrome portrait PostScript file is named"                           \
  "Monochrome portrait encapsulated PostScript file is named"              \
  "Monochrome portrait interchange encapsulated PostScript file is named"  \
  "Older-style color landscape PostScript file is named"                               \
  "Color landscape encapsulated PostScript file is named"                  \
  "Color landscape interchange encapsulated PostScript file is named"      \
  "Older-style monochrome landscape PostScript file is named"                          \
  "Monochrome landscape encapsulated PostScript file is named"             \
  "Monochrome landscape interchange encapsulated PostScript file is named" \
  "" "" "" "" "" "" "" ""                                                  \
  "cairo Postscript file is named"                                         \
  ""                                                                       \
  "cairo PDF file is named"                                                \
)

set f_list
set c_list
#**********************#
#                      #
#  Set areas examples  #
#                      #
#**********************#
set areas_fex   = (arex01 arex02 arex03)
set areas_ftst  = (tareas)
set areas_fttr  = (cardb1 caredg carline cardb2 carfill carmap)
set areas_flist = ($areas_fex $areas_ftst $areas_fttr)

set areas_ctst  = (c_tareas)
set areas_clist = ($areas_ctst)
set f_list = ($f_list $areas_flist)
set c_list = ($c_list $areas_clist)

#**************************#
#                          #
#  Set autograph examples  #
#                          #
#**************************#
set autograph_fex   = (agex01 agex02 agex03 agex04 agex05 agex06 agex07 \
		      agex08 agex09 agex10 agex11 agex12 agex13 agdp01)
set autograph_ftst  = (tautog tagupw)
set autograph_ffnd  = (fagaxclr fagaxlbl fagaxmax fagcuclr fagcudsh fagezmxy \
                      fagezmy fagezxy fagezy fagilclr fagovrvw)
set autograph_flist = ($autograph_fex $autograph_ftst $autograph_ffnd)

set autograph_cex   = (c_agex07)
set autograph_clist = ($autograph_cex)

set f_list = ($f_list $autograph_flist)
set c_list = ($c_list $autograph_clist)

#**********************#
#                      #
#  Set bivar examples  #
#                      #
#**********************#
set cbivar_fex   = (cbex01)
set cbivar_fttr  = (cidsfft)
set cbivar_flist = ($cbivar_fex $cbivar_fttr)

set cbivar_cex   = (c_cbex01)
set cbivar_clist = ($cbivar_cex)

set f_list = ($f_list $cbivar_flist)
set c_list = ($c_list $cbivar_clist)

#************************#
#                        #
#  Set colconv examples  #
#                        #
#************************#
set colconv_fex   = (coex01 coex02 coex03)
set colconv_ftst  = (tcolcv)
set colconv_ffnd  = (fcce01 fcce02)
set colconv_flist = ($colconv_fex $colconv_ftst $colconv_ffnd)

set colconv_cex   = (c_coex02)
set colconv_clist = ($colconv_cex)

set f_list = ($f_list $colconv_flist)
set c_list = ($c_list $colconv_clist)

#************************#
#                        #
#  Set conpack examples  #
#                        #
#************************#
set conpack_fex  = (cpex01 cpex02 cpex03 cpex04 cpex05 cpex06 cpex07 \
                    cpex08 cpex09 cpex10 cpex11 cpex12 cpex13 cpex14 \
                    cpex15 cpex16 ${cbivar_fex})
set conpack_ftst = (tconpa)
set conpack_fttr = (ccpback ccpcff ccpcfx ccpcica ccpcir ccpcis ccpcit ccpclc \
                    ccpcld ccpcldm ccpcldr ccpcll ccpclu ccpcnrc ccpdflt \
                    ccpezct ccpfil ccpga ccphand ccphcf ccphl ccphlt ccpila \
                    ccpils ccpilt ccpklb ccplbam ccplbdr ccpline ccpllb \
                    ccpllc ccplll ccpllo ccpllp ccpllt ccpllw ccpmap \
                    ccpmovi ccpmpxy ccpncls ccpnet ccpnof ccpnsd ccppc ccppc1 \
                    ccppc2 ccppc3 ccppc4 ccppkcl ccppole ccpt2d ccprc ccprect \
                    ccprwc ccprwu ccpscam ccpset ccpsps1 ccpsps2 ccpspv \
                    ccptitle ccpvp ccpvs colcon ${cbivar_fttr})
set conpack_flist = ($conpack_fex $conpack_ftst $conpack_fttr)

set conpack_cttr  = (c_colcon)
set conpack_cex   = (${cbivar_cex})
set conpack_clist = ($conpack_cex $conpack_cttr)

set f_list = ($f_list $conpack_flist)
set c_list = ($c_list $conpack_clist)

#*************************#
#                         #
#  Set conpackt examples  #
#                         #
#*************************#
set conpackt_fex   = (ctcbay ctex01 ctex02 ctfite ctgaus ctgc23 ctgeo1 ctgeo2 \
		      ctgeo3 ctiscp ctisc2 ctllg1 ctllg2 ctllg3 ctnccl ctorca \
		      ctpopg ctswth ctwng1 ctwng2 cttd01 cttd02 ctterr)
set conpackt_flist = ($conpackt_fex)

set conpackt_cex   = (c_ctllg3)
set conpackt_clist = ($conpackt_cex)

set f_list = ($f_list $conpackt_flist)
set c_list = ($c_list $conpackt_clist)

#******************************#
#                              #
#  Set conran_family examples  #
#                              #
#******************************#
set cnrn_family_ftst  = (tconan tconaq tconas)
set cnrn_family_flist = (${cnrn_family_ftst})

set f_list = ($f_list ${cnrn_family_flist})

#******************************#
#                              #
#  Set conrec_family examples  #
#                              #
#******************************#
set cnrc_family_ftst  = (tconre tcnqck tcnsmt tcnsup)
set cnrc_family_flist = (${cnrc_family_ftst})

set f_list = ($f_list ${cnrc_family_flist})

#*************************#
#                         #
#  Set dashline examples  #
#                         #
#*************************#
set dashline_ftst  = (tdashc tdashl tdashp tdashs)
set dashline_ffnd  = (fdlcurvd fdldashc fdldashd fdlsmth)
set dashline_flist = ($dashline_ftst $dashline_ffnd)

set dashline_cfnd  = (c_fdldashc)
set dashline_clist = ($dashline_cfnd)

set f_list = ($f_list $dashline_flist)
set c_list = ($c_list $dashline_clist)

#***********************#
#                       #
# set dashpack examples #
#                       #
#***********************#
set dashpack_ftst  = (tdshpk)
set dashpack_flist = ($dashpack_ftst)

set dashpack_ctst  = (c_tdshpk)
set dashpack_clist = ($dashpack_ctst)

set f_list = ($f_list $dashpack_flist)
set c_list = ($c_list $dashpack_clist)

#**********************#
#                      #
#  Set ezmap examples  #
#                      #
#**********************#
set ezmap_fex   = (mpex01 mpex02 mpex03 mpex04 mpex05 mpex06 mpex07 mpex08 \
		  mpex09 mpex10 mpex11 mpex12 mpex13 mpex14 mpex15 mpexfi \
		  eezmpa)
set ezmap_ftst  = (tezmap tezmpa tezmpb)
set ezmap_fttr  = (cezmap1 cezmap2 cezmap3 cmpclr cmpdd cmpdrw cmpel \
                  cmpfil cmpgci cmpgrd cmpgrp cmpita cmpitm cmplab cmplbl \
                  cmplot cmpmsk cmpou cmppos cmpsat cmpsup cmptit cmptra \
                  cmpusr)
set ezmap_flist = ($ezmap_fex $ezmap_ftst $ezmap_fttr)

set ezmap_cex   = (c_mpex05 c_eezmpa c_eezmpb)
set ezmap_clist = ($ezmap_cex)

set f_list = ($f_list $ezmap_flist)
set c_list = ($c_list $ezmap_clist)

#***********************#
#                       #
#  Set gflash examples  #
#                       #
#***********************#
set gflash_ftst  = (tgflas)
set gflash_flist = ($gflash_ftst)

set gflash_ctst  = (c_tgflas)
set gflash_clist = ($gflash_ctst)

set f_list = ($f_list $gflash_flist)
set c_list = ($c_list $gflash_clist)

#******************#
#                  #
# set gks examples #
#                  #
#******************#
set gks_ffnd    = (fgke02 fgke03 fcell fcell0 fgpm01 fgkgpl fgkgpm fgkgtx \
                  fgklnclr fgklnwth fcirc)
set gks_fpdc    = (pgkex01 pgkex02 pgkex03 pgkex04 pgkex05 pgkex06 pgkex07 \
                  pgkex08 pgkex09 pgkex10 pgkex11 pgkex12 pgkex13 pgkex14 \
                  pgkex15 pgkex16 pgkex17 pgkex18 pgkex19 pgkex20 pgkex21 \
                  pgkex22 pgkex23 pgkex24 pgkex25 pgkex26 pgkex27)
set gks_flist   = ($gks_ffnd $gks_fpdc)

set gks_cex     = (c_gtxpac c_gcell)
set gks_cpdc    = (c_pgkex21)
set gks_clist   = ($gks_cex $gks_cpdc)

set f_list = ($f_list $gks_flist)
set c_list = ($c_list $gks_clist)

#************************#
#                        #
#  Set gridall examples  #
#                        #
#************************#
set gridall_ftst  = (tgrida)
set gridall_ffnd  = (ccpga)
set gridall_flist = ($gridall_ffnd $gridall_ftst)

set gridall_ctst  = (c_tgrida)
set gridall_clist = ($gridall_ctst)

set f_list = ($f_list $gridall_flist)
set c_list = ($c_list $gridall_clist)

#*************************#
#                         #
#  Set halftone examples  #
#                         #
#*************************#
set halftone_ftst  = (thafto)
set halftone_flist = ($halftone_ftst)

set f_list = ($f_list $halftone_flist)

#**************************#
#                          #
#  Set histogram examples  #
#                          #
#**************************#
set histogram_ftst  = (thstgr thstmv)
set histogram_flist = ($histogram_ftst)

set histogram_ctst  = (c_thstmv)
set histogram_clist = ($histogram_ctst)

set f_list = ($f_list $histogram_flist)
set c_list = ($c_list $histogram_clist)

#***********************#
#                       #
# set isosrfhr examples #
#                       #
#***********************#
set isosrfhr_ftst  = (tisohr)
set isosrfhr_flist = ($isosrfhr_ftst)

set f_list = ($f_list $isosrfhr_flist)

#*************************#
#                         #
# set isosurface examples #
#                         #
#*************************#
set isosurface_ftst  = (tisosr tpwrzi)
set isosurface_ffnd  = (fisissrf fispwrzi)
set isosurface_flist = ($isosurface_ftst $isosurface_ffnd)

set isosurface_ctst  = (c_tisosr)
set isosurface_clist = ($isosurface_ctst)

set f_list = ($f_list $isosurface_flist)
set c_list = ($c_list $isosurface_clist)

#***********************#
#                       #
# set labelbar examples #
#                       #
#***********************#
set labelbar_fex   = (elblba)
set labelbar_ftst  = (tlblba)
set labelbar_ffnd  = (clbfil clbbar clblbr)
set labelbar_flist = ($labelbar_fex $labelbar_ftst $labelbar_ffnd)

set labelbar_cex   = (c_elblba)
set labelbar_clist = ($labelbar_cex)

set f_list = ($f_list $labelbar_flist)
set c_list = ($c_list $labelbar_clist)

#*********************#
#                     #
# set ngmath examples #
#                     #
#*********************#
set csagrid_flist = (csex01 csex02 csex03 csex04 csex05 csex06 csex07)
set cssgrid_flist = (cssex01 cssex02 cssex03)
set dsgrid_flist  = (dsex01 dsex01d dsex02 dsex03 dsex04 dsex05 dsex06)
set fitgrid_flist = (ftex01  ftex02  ftex03  ftex04  ftex05  ftex06  ftex07 \
                     ftex01d ftex02d ftex03d ftex04d ftex05d ftex06d ftex07d)
set natgrid_flist = (nnex01 nnex01d nnex02 nnex03 nnex04 nnex05 nnex06 \
                     nnex07 nnex08 nnex09 nnex10)
set shgrid_flist  = (shex01 shex02 shex03)

set ngmath_flist  = ($csagrid_flist $cssgrid_flist $dsgrid_flist \
                     $fitgrid_flist $natgrid_flist $shgrid_flist)
                   
set csagrid_clist = (c_csex01 c_csex02 c_csex03 c_csex04 c_csex05 \
                     c_csex06 c_csex07)
set cssgrid_clist = (c_cssex01 c_cssex02 c_cssex03)
set dsgrid_clist  = (c_dsex01 c_dsex01d c_dsex02 c_dsex03 c_dsex04 \
                     c_dsex05 c_dsex06)
set fitgrid_clist = (c_ftex01  c_ftex02  c_ftex03  c_ftex04  c_ftex05 \
                     c_ftex06  c_ftex07 \
                     c_ftex01d c_ftex02d c_ftex03d c_ftex04d c_ftex05d \
                     c_ftex06d c_ftex07d)
set natgrid_clist = (c_nnex01 c_nnex01d c_nnex02 c_nnex03 c_nnex04 c_nnex06)
set shgrid_clist  = (c_shex01 c_shex02 c_shex03)

set ngmath_clist  = ($csagrid_clist $cssgrid_clist $dsgrid_clist \
                     $fitgrid_clist $natgrid_clist $shgrid_clist)
                      
set ngmath_fex    = ($ngmath_flist)
set ngmath_cex    = ($ngmath_clist)
set f_list = ($f_list $ngmath_flist)
set c_list = ($c_list $ngmath_clist)

#*********************#
#                     #
# set ngmisc examples #
#                     #
#*********************#
set ngmisc_ffnd  = (fngngdts fngwsym miex01)
set ngmisc_flist = ($ngmisc_ffnd)

set ngmisc_cfnd  = (c_fngwsym)
set ngmisc_clist = ($ngmisc_cfnd)

set f_list = ($f_list $ngmisc_flist)
set c_list = ($c_list $ngmisc_clist)

#***********************#
#                       #
# set plotchar examples #
#                       #
#***********************#
set plotchar_fex   = (epltch)
set plotchar_ftst  = (tpltch)
set plotchar_ffnd  = (fpchiqu fpcloqu fpcfonts)
set plotchar_flist = ($plotchar_fex $plotchar_ftst $plotchar_ffnd)

set plotchar_cex   = (c_epltch)
set plotchar_clist = ($plotchar_cex)

set f_list = ($f_list $plotchar_flist)
set c_list = ($c_list $plotchar_clist)

#***********************#
#                       #
# set polypack examples #
#                       #
#***********************#
set polypack_fex   = (ppex01)
set polypack_ftst  = (tppack)
set polypack_flist = ($polypack_fex $polypack_ftst)

set polypack_cex   = (c_ppex01)
set polypack_clist = ($polypack_cex)

set f_list = ($f_list $polypack_flist)
set c_list = ($c_list $polypack_clist)

#****************************#
#                            #
# set pwrite_family examples #
#                            #
#****************************#
set pwrite_ftst  = (tpwrtx tpwry)
set pwrite_flist = ($pwrite_ftst)

set f_list = ($f_list $pwrite_flist)

#*****************************#
#                             #
# set scrolled title examples #
#                             #
#*****************************#
set scrlld_title_fex   = (slex01 slex02)
set scrlld_title_ftst  = (tstitl)
set scrlld_title_ffnd  = (fslfont)
set scrlld_title_flist = (${scrlld_title_fex} ${scrlld_title_ftst} \
                         ${scrlld_title_ffnd})

set scrlld_title_cex   = (c_slex01)
set scrlld_title_clist = (${scrlld_title_cex})

set f_list = ($f_list ${scrlld_title_flist})
set c_list = ($c_list ${scrlld_title_clist})

#********************#
#                    #
# set seter examples #
#                    #
#********************#
set seter_ftst  = (tseter)
set seter_flist = ($seter_ftst)

set f_list = ($f_list $seter_flist)

#***********************#
#                       #
# set softfill examples #
#                       #
#***********************#
set softfill_fex   = (sfex01 sfex02)
set softfill_ftst  = (tsoftf)
set softfill_ffnd  = (fsfwrld fsfsgfa)
set softfill_flist = ($softfill_fex $softfill_ftst $softfill_ffnd)

set softfill_cex   = (c_sfex02)
set softfill_clist = ($softfill_cex)

set f_list = ($f_list $softfill_flist)
set c_list = ($c_list $softfill_clist)

#*******************#
#                   #
# set spps examples #
#                   #
#*******************#
set spps_fex   = (splogy sprevx)
set spps_ffnd  = (fspcurve fspline fsppoint fspponts fcoord fcoord1 fcoord2)
set spps_flist = ($spps_fex $spps_ffnd)

set spps_cfnd  = (c_fcoord2)
set spps_clist = ($spps_cfnd)

set f_list = ($f_list $spps_flist)
set c_list = ($c_list $spps_clist)

#**************************#
#                          #
# set streamlines examples #
#                          #
#**************************#
set streamlines_fex   = (stex01 stex02 stex03)
set streamlines_ftst  = (tstrml)
set streamlines_ffnd  = (fstream ffex00 ffex01 ffex03 ffex04)
set streamlines_flist = ($streamlines_fex $streamlines_ftst $streamlines_ffnd)

set streamlines_cfnd  = (c_ffex03)
set streamlines_clist = ($streamlines_cfnd)

set f_list = ($f_list $streamlines_flist)
set c_list = ($c_list $streamlines_clist)

#**********************#
#                      #
# set surface examples #
#                      #
#**********************#
set surface_fex   = (srex01)
set surface_ftst  = (tsrfac tpwrzs)
set surface_ffnd  = (fsrezsrf fsrpwrzs fsrsrfac)
set surface_flist = ($surface_fex $surface_ftst $surface_ffnd)

set surface_cex   = (c_srex01)
set surface_clist = ($surface_cex)

set f_list = ($f_list $surface_flist)
set c_list = ($c_list $surface_clist)

#*********************#
#                     #
# set tdpack examples #
#                     #
#*********************#
set tdpack_fex   = (tdex01 tdex02 tdex03 tdex04 tdex05 tdex06 tdex07 tdex08)
set tdpack_ftst  = ()
set tdpack_flist = ($tdpack_fex $tdpack_ftst)

set tdpack_cex   = (c_tdex01 c_tdex03)
set tdpack_clist = ($tdpack_cex)

set f_list = ($f_list $tdpack_flist)
set c_list = ($c_list $tdpack_clist)

#*********************#
#                     #
# set threed examples #
#                     #
#*********************#
set threed_ftst  = (tthree tpwrzt)
set threed_ffnd  = (fthex01 fthex02 fthex03 fthex04 fthex05)
set threed_flist = ($threed_ftst $threed_ffnd)

set threed_cfnd  = (c_fthex01)
set threed_clist = ($threed_cfnd)

set f_list = ($f_list $threed_flist)
set c_list = ($c_list $threed_clist)

#*************************#
#                         #
#  Set vaspackt examples  #
#                         #
#*************************#
set vaspackt_fex   = (vtex01 vtex02 vtex03)
set vaspackt_flist = ($vaspackt_fex)

set vaspackt_cex   = ()
set vaspackt_clist = ($vaspackt_cex)

set f_list = ($f_list $vaspackt_flist)
set c_list = ($c_list $vaspackt_clist)

#**********************#
#                      #
# set vectors examples #
#                      #
#**********************#
set vectors_fex   = (vvex01 vvex02 vvex03)
set vectors_ftst  = (tvelvc)
set vectors_ffnd  = (ffex00 ffex01 ffex02 ffex05 fcover)
set vectors_flist = ($vectors_fex $vectors_ftst $vectors_ffnd)

set vectors_cex   = (c_vvex03)
set vectors_clist = ($vectors_cex)

set f_list = ($f_list $vectors_flist)
set c_list = ($c_list $vectors_clist)

#*******************#
#                   #
# set wmap examples #
#                   #
#*******************#
set wmap_fex   = (wmex01 wmex02 wmex03 wmex04 wmex05 wmex06 wmex07 wmex08\
                 wmex09 wmex10 wmex11 wmex12 wmex13 wmex14 wmex15 wmex16 \
                 wmex17)
set wmap_flist = ($wmap_fex)

set wmap_cex   = (c_wmex09)
set wmap_clist = ($wmap_cex)

set f_list = ($f_list $wmap_flist)
set c_list = ($c_list $wmap_clist)

#****************************************#
#                                        #
#  Set field flow examples - consists of #
#  all streamlines and vectors examples  #
#                                        #
#****************************************#
set field_fex   = (stex01 stex02 stex03 vvex01 vvex02 vvex03)
set field_ftst  = (tstrml tvelvc)
set field_ffnd  = (fstream ffex00 ffex01 ffex02 ffex03 ffex04 ffex05 fcover)
set field_flist = ($field_fex $field_ftst $field_ffnd)

set field_cex   = (c_vvex03)
set field_cfnd  = (c_ffex03)
set field_clist = ($field_cex $field_cfnd)

#****************************#
#                            #
# set miscellaneous examples #
#                            #
#****************************#
set misc_fex   = (example bnchmk ncargworld)
set misc_flist = ($misc_fex)

set f_list = ($f_list $misc_flist)

#********************#
#                    #
# set class examples #
#                    #
#*******************#
set class_fttr   = (class1 class2 class3)
set class_flist  = ($class_fttr)

set f_list = ($f_list $class_flist)

#*************************************************************#
#                                                             #
# Some of the other examples are considered tutorial examples #
#                                                             #
#*************************************************************#
set overlap_fttr = (mpex03 mpex05 arex01 sfex01 tsoftf)

#****************************************#
#                                        #
# Set lists of various types of examples #
#                                        #
#****************************************#
set list_fx11 = (fgke01 fgke04)
set list_cx11 = (c_xwndws)
set list_fex = ($areas_fex $autograph_fex $colconv_fex $conpack_fex \
		$conpackt_fex $ezmap_fex $field_fex $labelbar_fex $ngmath_fex \
		$plotchar_fex $polypack_fex ${scrlld_title_fex} $softfill_fex \
		$spps_fex $surface_fex $tdpack_fex $wmap_fex $misc_fex \
		$vaspackt_fex)

set list_cex  = ($autograph_cex $colconv_cex $conpack_cex $conpackt_cex \
		$ezmap_cex $field_cex $gks_cex $labelbar_cex $ngmath_cex \
		$plotchar_cex $polypack_cex ${scrlld_title_cex} $softfill_cex \
		$surface_cex $tdpack_cex $wmap_cex $vaspackt_cex)

set list_ftst = ($areas_ftst $autograph_ftst $colconv_ftst $conpack_ftst \
                ${cnrn_family_ftst} ${cnrc_family_ftst} $dashline_ftst \
                $dashpack_ftst $ezmap_ftst $field_ftst $gflash_ftst \
                $gridall_ftst $halftone_ftst $histogram_ftst $isosrfhr_ftst \
                $isosurface_ftst $labelbar_ftst $plotchar_ftst $polypack_ftst \
                $pwrite_ftst ${scrlld_title_ftst} $seter_ftst $softfill_ftst \
                $surface_ftst $tdpack_ftst $threed_ftst)

set list_ctst = ($areas_ctst $dashpack_ctst $gflash_ctst $gridall_ctst \
                $histogram_ctst $isosurface_ctst)

set list_fttr = ($areas_fttr $conpack_fttr $ezmap_fttr $class_fttr)

set list_cttr = ($conpack_cttr)

set list_ffnd = ($autograph_ffnd $colconv_ffnd $dashline_ffnd $field_ffnd \
                $gks_ffnd $gridall_ffnd $isosurface_ffnd $labelbar_ffnd \
                $ngmisc_ffnd $plotchar_ffnd ${scrlld_title_ffnd} \
                $softfill_ffnd $spps_ffnd $surface_ffnd $threed_ffnd)

set list_cfnd = ($dashline_cfnd $field_cfnd $ngmisc_cfnd $spps_cfnd \
                 $threed_cfnd)

set list_fpdc = ($gks_fpdc)
set list_cpdc = ($gks_cpdc)

set list_fps = (pgkex19 pgkex20 pgkex21 pgkex22 pgkex23 wmex12 wmex13 wmex14)
set list_cps = (c_pgkex21)

#****************************************************#
#                                                    #
# Default is to load in the X11 library. If the      #
# user specifies "-noX11" or "-NCGMonly on the       #
# command line, then a stub will be linked instead   #
# of the X11 library                                 #
#                                                    #
#****************************************************#
set X11_option
set ncarbd_flag
set ngmathbd_flag

#*******************************#
#                               #
# Parse options on command line #
#                               #
#*******************************#
set names

#*******************************#
#                               #
# change_ws_type is used to keep#
# track of whether the user     #
# explicitly changed the ws type#
#                               #
#*******************************#
unset change_ws_type
set ws_type      = 1

while ($#argv > 0)

  switch ($1)
    case "-all":
    case "-A":
      shift
      set names=($names $list_fex $list_ftst $list_fttr $list_ffnd $list_fpdc \
                 $list_cex $list_ctst $list_cttr $list_cfnd $list_cpdc)
      breaksw

    case "-c":
    case "-C":
      shift
      set names=($names $c_list)
      breaksw

    case "-fortran":
    case "-Fortran":
      shift
      set names=($names $f_list)
      breaksw

    case "-allexamples":
    case "-E":
      shift
      set names=($names $list_fex $list_cex)
      breaksw

    case "-alltests":
    case "-T":
      shift
      set names=($names $list_ftst $list_ctst)
      breaksw
        
    case "-alltutorial":
    case "-U":
      shift
      set names=($names $list_fttr $overlap_fttr $list_cttr)
      breaksw
        
    case "-allfundamental":
    case "-F":
      shift
      set names=($names $list_ffnd $list_cfnd)
      breaksw
        
    case "-allpdocs":
    case "-P":
      shift
      set names=($names $list_fpdc $list_cpdc)
      breaksw
        
    case "-areas":
      shift
      set names=($names $areas_flist $areas_clist)
      breaksw
        
    case "-autograph":
      shift
      set names=($names $autograph_flist $autograph_clist)
      breaksw

    case "-bivar":
      shift
      set names=($names $cbivar_flist $cbivar_clist)
      breaksw

    case "-colconv":
      shift
      set names=($names $colconv_flist $colconv_clist)
      breaksw

    case "-conpack":
      shift
      set names=($names $conpack_flist $conpack_clist)
      breaksw

    case "-conpackt":
      shift
      set names=($names $conpackt_flist $conpackt_clist)
      breaksw

    case "-conran_family":
      shift
      set names=($names ${cnrn_family_flist})
      breaksw

    case "-conrec_family":
      shift
      set names=($names ${cnrc_family_flist})
      breaksw

    case "-csagrid":
      shift
      set names=($names $csagrid_flist $csagrid_clist)
      breaksw

    case "-cssgrid":
      shift
      set names=($names $cssgrid_flist $cssgrid_clist)
      breaksw

    case "-dashline":
      shift
      set names=($names $dashline_flist $dashline_clist)
      breaksw

    case "-dashpack":
      shift
      set names=($names $dashpack_flist $dashpack_clist)
      breaksw

    case "-dsgrid":
      shift
      set names=($names $dsgrid_flist $dsgrid_clist)
      breaksw

    case "-ezmap":
      shift
      set names=($names $ezmap_flist $ezmap_clist)
      breaksw

    case "-field_flow":
      shift
      set names=($names $field_flist $field_clist)
      breaksw

    case "-fitgrid":
      shift
      set names=($names $fitgrid_flist $fitgrid_clist)
      breaksw

    case "-gflash":
      shift
      set names=($names $gflash_flist $gflash_clist)
      breaksw

    case "-gks":
      shift
      set names=($names $gks_flist $gks_clist)
      breaksw

    case "-gridall":
      shift
      set names=($names $gridall_flist $gridall_clist)
      breaksw

    case "-halftone":
      shift
      set names=($names $halftone_flist)
      breaksw

    case "-histogram":
      shift
      set names=($names $histogram_flist $histogram_clist)
      breaksw

    case "-isosrfhr":
      shift
      set names=($names $isosrfhr_flist)
      breaksw

    case "-isosurface":
      shift
      set names=($names $isosurface_flist $isosurface_clist)
      breaksw

    case "-labelbar":
      shift
      set names=($names $labelbar_flist $labelbar_clist)
      breaksw

    case "-natgrid":
      shift
      set names=($names $natgrid_flist $natgrid_clist)
      breaksw

    case "-ngmath":
      shift
      set names=($names $ngmath_flist $ngmath_clist)
      breaksw

    case "-ngmisc":
      shift
      set names=($names $ngmisc_flist $ngmisc_clist)
      breaksw

    case "-plotchar":
      shift
      set names=($names $plotchar_flist $plotchar_clist)
      breaksw

    case "-polypack":
      shift
      set names=($names $polypack_flist $polypack_clist)
      breaksw

    case "-pwrite_family":
      shift
      set names=($names $pwrite_flist)
      breaksw

    case "-scrolled_title":
      shift
      set names=($names ${scrlld_title_flist} ${scrlld_title_clist})
      breaksw

    case "-seter":
      shift
      set names=($names $seter_flist)
      breaksw

    case "-shgrid":
      shift
      set names=($names $shgrid_flist $shgrid_clist)
      breaksw

    case "-softfill":
      shift
      set names=($names $softfill_flist $softfill_clist)
      breaksw

    case "-spps":
      shift
      set names=($names $spps_flist $spps_clist)
      breaksw

    case "-streamlines":
      shift
      set names=($names $streamlines_flist $streamlines_clist)
      breaksw

    case "-surface":
      shift
      set names=($names $surface_flist $surface_clist)
      breaksw

    case "-tdpack":
      shift
      set names=($names $tdpack_flist $tdpack_clist)
      breaksw

    case "-threed":
      shift
      set names=($names $threed_flist $threed_clist)
      breaksw

    case "-vaspackt":
      shift
      set names=($names $vaspackt_flist $vaspackt_clist)
      breaksw

    case "-vectors":
      shift
      set names=($names $vectors_flist $vectors_clist)
      breaksw

    case "-wmap":
      shift
      set names=($names $wmap_flist $wmap_clist)
      breaksw

    case "-misc":
      shift
      set names=($names $misc_flist)
      breaksw

    case "-class":
      shift
      set names=($names $class_flist)
      breaksw

    case "-ps":
      shift
      set names=($names $list_fps $list_cps)
      breaksw

    case "-x11":
      shift
      set names=($names $list_fx11 $list_cx11)
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
      set new_ws_type = $1
      if ( !(`expr "$new_ws_type" : '[0-9]'`)) then
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
        set str = ("$new_ws_type" "" "")
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
        if ( `expr "$new_ws_type" : '.*\..*\..*'` ) then
#***********************#
#                       #
# String is xxx.yyy.zzz #
#                       #
#***********************#
          set str[1] = `expr "$new_ws_type" : '\(.*\)\..*\..*'`
          set str[2] = `expr "$new_ws_type" : '.*\.\(.*\)\..*'`
          set str[3] = `expr "$new_ws_type" : '.*\..*\.\(.*\)'`
          set num = 3
        else
          if ( `expr "$new_ws_type" : '.*\..*'` ) then
#*******************#
#                   #
# String is xxx.yyy #
#                   #
#*******************#
            set str[1] = `expr "$new_ws_type" : '\(.*\)\..*'`
            set str[2] = `expr "$new_ws_type" : '.*\.\(.*\)'`
            set num = 2
          endif
        endif
        set i = 1
        while ($i <= $num)
          foreach ftype($file_types)
            if ("$str[$i]" == "$ftype") then
              set file_type = "$str[$i]"
              break
            endif
          end
          foreach otype($orient_types)
            if ("$str[$i]" == "$otype") then
              set orient_type = "$str[$i]"
              break
            endif
          end
          foreach ctype($color_types)
            if ("$str[$i]" == "$ctype") then
              set color_type = "$str[$i]"
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
        if ("$file_type" == "" || \
           ($num == 2 && ("$orient_type" == "" && "$color_type" == "")) || \
           ($num == 3 && ("$orient_type" == "" || "$color_type" == ""))) then
          set not_valid
          goto invalid
        endif
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
            set new_ws_type = "$i"
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
        if ("$new_ws_type" < 1 || "$new_ws_type" > $#ws_types ) then
          set not_valid
          goto invalid
        else
          if ("$ws_types[$new_ws_type]" == "" ) then
            set not_valid
            goto invalid
          endif
        endif
      endif
invalid:
      if ($?not_valid) then
          echo ""
          echo "    '$1' is not a valid workstation type."
          echo ""
          exit 1
      endif
      set change_ws_type
      shift
      breaksw

    case "-ncarbd":
      shift
      set ncarbd_flag = "-ncarbd"
      breaksw

    case "-ngmathbd":
      shift
      set ngmathbd_flag = "-ngmathbd"
      breaksw

    case "-noX11":
    case "-nox11":
    case "-NCGMonly":
    case "-ncgmonly":
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

#***************************#
#                           #
# Loop through each example #
#                           #
#***************************#
foreach name ($names)

unset tmp_msg
set input
set output

#*************************************#
#                                     #
# Find out what type of example it is #
#                                     #
#*************************************#
set ex_type = "Unknown"

foreach known ($list_fex $list_cex $list_cx11)
  if ("$name" == "$known") then
    set ex_type
    set temp_dir = "$example_dir"
    break
  endif
end

if ( "$ex_type" == "Unknown" ) then
  foreach known ($list_ftst $list_ctst)
    if ("$name" == "$known") then
      set ex_type = "Test"
      set temp_dir = "$test_dir"
      break
    endif
  end
endif

if ( "$ex_type" == "Unknown" ) then
  foreach known ($list_fttr $list_cttr)
    if ("$name" == "$known") then
      set ex_type = "Tutorial"
      set temp_dir = "$tutor_dir"
      break
    endif
  end
endif

if ( "$ex_type" == "Unknown" ) then
  foreach known ($list_ffnd $list_cfnd $list_fx11)
    if ("$name" == "$known") then
      set ex_type = "Fundamentals"
      set temp_dir = "$fund_dir"
      break
    endif
  end
endif

if ( "$ex_type" == "Unknown" ) then
  foreach known ($list_fpdc $list_cpdc)
    if ("$name" == "$known") then
      set ex_type = "Programmer"
      set temp_dir = "$pdoc_dir"
      break
    endif
  end
endif

# 
# There are four types of examples:
# 
#   1. One that creates an NCGM by default, but can be changed
#      via the "-W" option. This is how most examples are currently.
#      They must have an IWTYPE parameter in order for this to work.
# 
#   2. One that creates something other than an NCGM, but can
#      be changed via the "-W" option. Again, IWTYPE must be present.
# 
# Set the "modifiable" for types of both #1 and #2.
#
#   3. One that creates an output file that cannot be changed
#      via the "-W" option.
# 
# Set std_file for types of #1-#3. This just means a "standard"
# file of the form "example_name.xxxx" is created.
#
#   4. One that doesn't create a file of the form example_name.xxx,
#      or creates no file at all.
# 
# Unset "modifiable" for types of both #3 and #4.
# Unset std_file for types of #4.
#
# The variable "orig_ws_type" represents the workstation type.  In most
# cases, this value will be equal to 1 (case #1). If a Fortran example
# has the IWTYPE parameter, and it is set to 1, then leave orig_ws_type
# alone. Otherwise, if IWTYPE exists and is set to something other than
# 1, then set orig_ws_type to this value.
# 
# If the example does not have an IWTYPE parameter, but it supposed to
# create a file of the style example_name.xxx, then unset modifiable,
# but leave std_file set.
# 
# If the example is either not meant to create an output file, or else
# it doesn't create one of the form example_ncgm.xxxx, then unset 
# modifiable AND std_file.
# 

set orig_ws_type = "1"
set modifiable
set std_file

switch($name)
    case pgkex19:
    case pgkex20:
    case pgkex21:
    case pgkex22:
    case pgkex23:
    case c_pgkex21:
      unset modifiable
      set orig_ws_type = "20"
      if ("$name" == "pgkex20") then
        set orig_ws_type = "26"
      endif
      echo ""
      echo "  This example was set up to demonstrate how to use"
      echo "  the Ngmisc routines to define the Postscript output."
      echo ""
    breaksw

    case wmex12:
    case wmex13:
    case wmex14:
      set orig_ws_type = "26"
      if (! $?change_ws_type) then
        echo ""
        echo "  This example was set up to use the entire"
        echo "  page when going to PostScript, so workstation"
        echo "  type 26 is being used."
        echo ""
      endif
    breaksw

    case pgkex26:
    case fgke03:
      unset modifiable
      unset std_file
      echo ""
      echo "  This example was set up to demonstrate how to change"
      echo "  the name of the metafile from within the program."
      echo ""
      set tmp_msg = "Metafiles META01 and META02 produced."
    breaksw

    case pgkex27:
      unset modifiable
      unset std_file
      echo ""
      echo "  This example was set up to demonstrate how to write"
      echo "  to more than one metafile at a time."
      echo ""
      set tmp_msg = "Metafiles gmeta1 and gmeta2 produced."
    breaksw

    case fgke01:
    case fgke04:
      set orig_ws_type = 8
      unset modifiable
      unset std_file
      echo ""
      echo "  This example was set up to demonstrate the X11"
      echo "  driver.  It also generates a graphic file."
      echo ""
      set tmp_msg = "   "
    breaksw

    case nnex08:
    case nnex09:
    case nnex10:
    case mpex12:
      unset modifiable
      unset std_file
      set tmp_msg = "   "
      echo ""
      echo "  This example does not produce a metafile."
      echo ""
    breaksw

    case ccpcff:
    case tcolcv:
    case fcce02:
    case ftex01d:
    case ftex02d:
    case ftex03d:
    case ftex04d:
    case ftex05d:
    case ftex06d:
    case ftex07d:
    case c_ftex01d:
    case c_ftex02d:
    case c_ftex03d:
    case c_ftex04d:
    case c_ftex05d:
    case c_ftex06d:
    case c_ftex07d:
      unset modifiable
      unset std_file
      set tmp_msg = "   "
      echo ""
      echo "  No graphics file will be produced by this example."
      echo ""
    breaksw
endsw

#
# If the user changed the workstation type with the -W option,
# then only use it if this particular example is a modifiable one.
#
if ($?change_ws_type && $?modifiable) then
  set ws_type = $new_ws_type
else
  set ws_type = $orig_ws_type
endif

#******************************************#
#                                          #
# Determine if it's a C or Fortran example #
#                                          #
#******************************************#
unset fprog
unset cprog
if ( `expr "$name" : "c_.*"`) then 
  set cprog
  set prog_type = "C"
  set comp_script = "ncargcc"
else
  set fprog
  set prog_type = "Fortran"
  set comp_script = "ncargf77"
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
  echo "NCAR Graphics $prog_type $ex_type Example <$name>"
  echo ""
endif

#********************************************#
#                                            #
# "-noX11" only works if going to an NCGM    #
# file.                                      #
#                                            #
#********************************************#
if ("$X11_option" == "-noX11" && "$ws_type" != "1") then
    echo ""
    echo "Warning:  You can only use the '-noX11' option if you are"
    echo "          sending your output to an NCGM file. The '-noX11'"
    echo "          option will be turned off for this example."
    echo ""
    set xoption = ""
else
    set xoption = $X11_option
endif


#***************************************#
#                                       #
# If the workstation type is "8" (X11)  #
# or "10" (text dump) then no file is   #
# created when the example is executed. #
#                                       #
#***************************************#
if ("$ws_type" == "8" || "$ws_type" == "10" ) unset std_file

#**************************************#
#                                      #
# Initialize the name of the default   #
# output file and the name of the file #
# it is going to be renamed to.        #
#                                      #
#**************************************#
set suffix = "$suffix_names[$ws_type]"
set graphic_file = "$name.$suffix"

if ($ws_type == 41) then
  set msg = "cairo PNG files are named $name.xxxxxx.png (xxxxxx is the frame #)"
else
  set msg = "$default_msgs[$ws_type] $graphic_file."
endif
if ($?fprog) then
  set main = "$name.f"
else
  set main = "$name.c"
endif

if ($?tmp_msg) then
  set msg = "$tmp_msg"
endif

if ("$ws_type" == "8") then
  echo ""
  echo "NOTE: This example is being run interactively and can only"
  echo "      be executed if you have X running and have your     "
  echo "      DISPLAY environment variable set properly.  It will "
  echo "      create an X11 window that you must click on with your"
  echo "      mouse to advance the frame(s)."
  echo ""
  set xoption = ""
endif

#**********************************#
#                                  #
# For workstation "10" (text dump) #
# the output goes to stdout        #
#                                  #
#**********************************#
if ("$ws_type" == "10") then
  set output = "$graphic_file"
endif

#***********************************************#
#                                               #
# If the "-unique" option was selected and the  #
# NCGM already exists, don't generate it again. #
#                                               #
#***********************************************#
if ($?Unique && -f $graphic_file) goto theend

#****************************#
#                            #
# Set initial compiler flags #
#                            #
#****************************#
set comp_flags = ($ncarbd_flag $ngmathbd_flag $xoption)

#**********************************#
#                                  #
# Some examples need extra Fortran #
# files, data files, or special    #
# compiler options                 #
#                                  #
#**********************************#
set extra_src_files
set stdin_data_file
set other_data_files

switch ($name)
    case agex13:
	set stdin_data_file = (agda13.dat)
    breaksw

    case ccpcica:
    case ccpcir:
    case ccpcnrc:
    case ccpezct:
    case ccphl:
    case ccpmap:
    case ccpmovi:
    case ccpvp:
        set extra_src_files = (ggdini.f)
    breaksw

    case ccpila:
    case ccpt2d:
	set stdin_data_file = (ccpila.dat)
    breaksw

    case ccpils:
    case ccpilt:
    case ccplbdr:
    case ccptitle:
	set stdin_data_file = (ccpex.dat)
    breaksw

    case ccpmpxy:
	set stdin_data_file = (cpmpxy1.dat)
    breaksw

    case class1:
	set stdin_data_file = (class1.dat)
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
        set extra_src_files = (cpexcc.f)
    breaksw

    case cpex14:
	set other_data_files = (cpex14.dat)
    breaksw

    case cpex16:
	set other_data_files = (cpex16.dat)
    breaksw

    case ctcbay:
	set other_data_files = (ctcbay.dat)
    breaksw

    case ctfite:
	set other_data_files = (ctfite.dat)
    breaksw

    case ctgaus:
	set other_data_files = (ctgaus.dat)
    breaksw

    case ctgeo3:
    case ctgc23:
	set other_data_files = (ctgeo3.dat)
    breaksw

    case ctiscp:
    case ctisc2:
	set other_data_files = (ctiscp.dat)
    breaksw

    case ctnccl:
	set other_data_files = (ctnccl.dat)
    breaksw

    case ctorca:
	set other_data_files = (ctorca.dat)
    breaksw

    case ctpopg:
	set other_data_files = (ctpopg.dat)
    breaksw

    case ctswth:
	set other_data_files = (ctswth.dat)
    breaksw

    case ctterr:
	set other_data_files = (ctterr.dat)
    breaksw

    case ctwng1:
	set other_data_files = (ctwng1.dat)
    breaksw

    case ctwng2:
	set other_data_files = (ctwng2.dat)
    breaksw

    case fcover:
	set stdin_data_file = (fcover.dat)
    breaksw

    case ffex02:
    case ffex03:
    case c_ffex03:
	set stdin_data_file = (ffex02.dat)
    breaksw

    case ffex05:
	set stdin_data_file = (ffex05.dat)
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
        set extra_src_files = (mpexcc.f)
    breaksw

    case mpex15:
	set other_data_files = (Europe.png Europe.pngi)
        set comp_flags = ($comp_flags "-lpng -lz")
    breaksw

    case mpexfi:
	set stdin_data_file = (mpexfi.dat)
        set extra_src_files = (mpexcc.f)
    breaksw

    case srex01:
    case c_srex01:
	set stdin_data_file = (srex01.dat)
    breaksw

    case csex01:
    case csex02:
    case csex03:
    case csex04:
    case csex05:
    case csex06:
    case csex07:
    case cssex01:
    case cssex02:
    case cssex03:
    case dsex01:
    case dsex01d:
    case dsex02:
    case dsex03:
    case dsex04:
    case dsex05:
    case dsex06:
    case ftex01:
    case ftex02:
    case ftex03:
    case ftex04:
    case ftex05:
    case ftex06:
    case ftex07:
    case ftex01d:
    case ftex02d:
    case ftex03d:
    case ftex04d:
    case ftex05d:
    case ftex06d:
    case ftex07d:
    case shex01:
    case shex02:
    case shex03:
    case wmex01:
    case wmex02:
    case wmex03:
    case wmex04:
    case wmex13:
    case wmex14:
    case c_csex01:
    case c_csex02:
    case c_csex03:
    case c_csex04:
    case c_csex05:
    case c_csex06:
    case c_csex07:
    case c_cssex01:
    case c_cssex02:
    case c_cssex03:
    case c_dsex01:
    case c_dsex01d:
    case c_dsex02:
    case c_dsex03:
    case c_dsex04:
    case c_dsex05:
    case c_dsex06:
    case c_ftex01:
    case c_ftex02:
    case c_ftex03:
    case c_ftex04:
    case c_ftex05:
    case c_ftex06:
    case c_ftex07:
    case c_ftex01d:
    case c_ftex02d:
    case c_ftex03d:
    case c_ftex04d:
    case c_ftex05d:
    case c_ftex06d:
    case c_ftex07d:
    case c_shex01:
    case c_shex02:
    case c_shex03
       set comp_flags = ($comp_flags "-ngmath")
    breaksw

    case nnex01:
    case nnex02:
    case nnex03:
    case nnex04:
    case nnex05:
    case nnex06:
    case nnex07:
    case nnex08:
    case nnex09:
    case nnex10:
    case nnex01d:
        set extra_src_files = (nnplotf.f)
        set comp_flags = ($comp_flags "-ngmath")
    breaksw

    case c_nnex01:
    case c_nnex02:
    case c_nnex03:
    case c_nnex04:
    case c_nnex06:
    case c_nnex01d:
        set extra_src_files = (nnplotc.c)
        set comp_flags = ($comp_flags "-ngmath")
    breaksw

#************************************************#           
#                                                #
# autograph with pwritx for character generation #
#                                                #
#************************************************#           
    case tagupw:
        set comp_flags = ($comp_flags "-agupwrtx")
    breaksw

#***************************#           
#                           #
# smooth routines (default) #
#                           #
#***************************#           
    case tdashs:
    case tcnsmt:
    case tconan:
        set comp_flags = ($comp_flags "-smooth")
    breaksw
#****************#           
#                #
# quick routines #
#                #
#****************#           
    case tdashl:
    case tcnqck:
    case tconaq:
        set comp_flags = ($comp_flags "-quick")
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
        set comp_flags = ($comp_flags "-super")
    breaksw

    case vvex01:
    case vvex02:
        set extra_src_files = (vvexcc.f)
    breaksw
endsw

#**********************************#
#                                  #
# Check if this particular example #
# needs a data file on "stdin"     #
#                                  #
#**********************************#
if ("$stdin_data_file" != "") then
  set input = "$stdin_data_file"
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
if ($?fprog && $?change_ws_type && $?modifiable) then
if($orig_ws_type != $ws_type) then
ed << EOF - ./$main >& /dev/null
g/^ *PARAMETER/s/IWTYPE=[0-9][0-9]*/IWTYPE=$ws_type/g
.
w
q
endif
endif

if ($?cprog && $?change_ws_type && $?modifiable) then
if($orig_ws_type != $ws_type) then
ed << EOF - ./$main >& /dev/null
/define IWTYPE
c
#define IWTYPE $ws_type
.
w
q
EOF
endif
endif

set src_files = ($extra_src_files $main)

#***********************#
#                       #
# Copy the needed files #
#                       #
#***********************#
   
set copy_files = ($extra_src_files $stdin_data_file $other_data_files)

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
    $comp_script $comp_flags -o $name $src_files
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
    if (! $?std_file) set rename_option
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

#*******************************************#
#                                           #
# Remove files created after the execution. #
#                                           #
#*******************************************#
switch ($name)
    case slex02:
	set other_data_files = ($other_data_files slex02.input)
    breaksw
endsw


#***********************#
#                       #
# Remove unwanted files #
#                       #
#***********************#
set rmfiles = ($stdin_data_file $other_data_files $src_files $nameSED_EXE_SUFFIX)
foreach file($src_files)
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
    if ("$suffix" == "ncgm") then
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

exit

usage:
#*************************#
#                         #
# ncargex usage statement #
#                         #
#*************************#
echo "usage: ncargex [options] [example names]"
echo ""
echo " Options:"
echo ""
echo " To invoke various classes of examples:"
echo "   [-A] [-E] [-F] [-P] [-T] [-U] [-ngmath] [-class] [-ps] [-x11]"
echo ""
echo " To invoke various utilities:"
echo "   [-areas] [-autograph] [-bivar] [-colconv] [-conpack]     "
echo "   [-conpackt] [-conran_family] [-conrec_family] [-csagrid] "
echo "   [-cssgrid] [-dashline] [-dashpack] [-dsgrid] [-ezmap]    "
echo "   [-field_flow] [-fitgrid] [-gflash] [-gks] [-gridall]     "
echo "   [-halftone] [-histogram] [-isosrfhr] [-isosurface]       "
echo "   [-labelbar] [-natgrid] [-ngmisc] [-plotchar]             "
echo "   [-polypack] [-pwrite_family] [-scrolled_title]           "
echo "   [-seter] [-shgrid] [-softfill] [-spps] [-streamlines]    "
echo "   [-surface] [-threed] [-vaspackt][-vectors] [-wmap]       "
echo "   [-misc]                                                  "
echo ""
echo " Other options:"
echo "   [-W workstation_type] [-n] [-clean] [-onebyone] names"
echo ""
echo "See <man ncargex> for explanation of options." 
echo ""
exit
