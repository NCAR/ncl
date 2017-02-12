#!/bin/csh -f
#
#   $Id: ng4ex.csh,v 1.15 2010-04-02 17:29:43 haley Exp $
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
#                   C, Fortran, and NCL examples illustrating the use
#                   of the NCAR Graphics HLUs (High Level Utilities)
#                   and NCL.
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
#     GSUN examples             gsun
#
#     NCL User Guide examples   nug   ( added in NCL V6.4.0 )
#
# How to add an example to this script:
#
#   First add it to appropriate variable below (c_xyplot, f_vector,
#   n_textitem, etc.).
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
  echo "   [-A] [-C] [-Fortran] [-NCL] [-cdf] [-gui] [-app] [-basic] [-nug] [-gsun]"
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

set hluex_dir = "`ncargpath SED_HLUEXDIR`"
set nclex_dir = "`ncargpath SED_NCLEXDIR`"
set res_dir   = "`ncargpath SED_RESDIR`"

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
set dir_nug = "nug"
set dir_gsun = "gsun"

#******************#
#                  #
# Initialize lists #
#                  #
#******************#
set c_list
set f_list
set n_list

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
set n_app   = (ap01n)
set app_list = ($c_app $f_app $n_app)
set c_list = ($c_list $c_app)
set f_list = ($f_list $f_app)
set n_list = ($n_list $n_app)

#**********************#
#                      #
#  Set Basic examples  #
#                      #
#**********************#
set c_basic   = (basic01c basic02c basic03c basic04c basic05c basic06c \
                 basic07c basic08c basic09c)
set f_basic   = (basic01f basic02f basic03f basic04f basic05f basic06f \
                 basic07f basic08f basic09f)
set n_basic   = (basic01n basic02n basic03n basic04n basic05n basic06n \
                 basic07n basic08n basic09n)
set basic_list = ($c_basic $f_basic $n_basic)
set c_list = ($c_list $c_basic)
set f_list = ($f_list $f_basic)
set n_list = ($n_list $n_basic)
set x_list = ($x_list basic03c basic03f basic03n basic04c basic04f basic04n \
              basic07c basic07f basic07n)


#*************************#
#                         #
#  Set Contour examples   #
#                         #
#*************************#
set c_contour  = (cn01c cn02c cn03c cn04c cn05c cn06c cn07c cn08c cn09c cn10c \
                  cn11c cn12c cn13c cn14c cn15c cn16c cn17c)
set f_contour  = (cn01f cn02f cn03f cn04f cn05f cn06f cn07f cn08f cn09f cn10f \
                  cn11f cn12f cn13f cn14f cn15f cn16f cn17f)
set n_contour  = (cn01n cn02n cn03n cn04n cn05n cn06n cn07n cn08n cn09n cn10n \
                  cn12n cn13n cn14n cn15n cn16n cn17n)
set contour_list = ($c_contour $f_contour $n_contour)
set c_list = ($c_list $c_contour)
set f_list = ($f_list $f_contour)
set n_list = ($n_list $n_contour)
set anno_list = ($anno_list cn17c cn17f cn17n)
set cdf_list = ($cdf_list cn05c cn06c cn07c cn08c cn09c cn10c cn14c cn15c cn16c cn05f cn06f cn07f cn08f cn09f cn10f cn14f cn15f cn16f cn05n cn06n cn07n cn08n cn09n cn10n cn14n cn15n cn16n cn17n)

#*************************#
#                         #
#  Set LabelBar examples  #
#                         #
#*************************#
set c_labelbar = (lb01c lb02c)
set f_labelbar = (lb01f lb02f)
set n_labelbar = (lb01n lb02n)
set labelbar_list = ($c_labelbar $f_labelbar $n_labelbar)
set c_list = ($c_list $c_labelbar)
set f_list = ($f_list $f_labelbar)
set n_list = ($n_list $n_labelbar)

#***********************#
#                       #
#  Set Legend examples  #
#                       #
#***********************#
set c_legend = (lg01c lg02c lg03c)
set f_legend = (lg01f lg02f lg03f)
set n_legend = (lg01n lg02n lg03n)
set legend_list = ($c_legend $f_legend $n_legend)
set c_list = ($c_list $c_legend)
set f_list = ($f_list $f_legend)
set n_list = ($n_list $n_legend)

#*************************#
#                         #
#  Set Primitive examples #
#                         #
#*************************#
set c_prim   = (pr01c pr02c pr03c pr04c pr05c)
set f_prim   = (pr01f pr02f pr03f pr04f pr05f)
set n_prim   = (pr01n pr02n pr03n pr04n pr05n)
set prim_list = ($c_prim $f_prim $n_prim)
set c_list = ($c_list $c_prim)
set f_list = ($f_list $f_prim)
set n_list = ($n_list $n_prim)

#************************#
#                        #
#  Set MapPlot examples  #
#                        #
#************************#
set c_mapplot   = (mp01c mp02c mp03c mp04c mp05c mp06c mp07c)
set f_mapplot   = (mp01f mp02f mp03f mp04f mp05f mp06f mp07f)
set n_mapplot   = (mp01n mp02n mp03n mp04n mp05n mp06n mp07n)
set mapplot_list = ($c_mapplot $f_mapplot $n_mapplot)
set c_list = ($c_list $c_mapplot)
set f_list = ($f_list $f_mapplot)
set n_list = ($n_list $n_mapplot)
set anno_list = ($anno_list mp04c mp04f mp04n)

#***********************#
#                       #
#  Set Ngmath examples  #
#                       #
#***********************#

set c_natgrid = (nm01c nm02c nm03c)
set c_dsgrid  = (nm04c nm05c nm06c)

set f_natgrid = (nm01f nm02f nm03f)
set f_dsgrid  = (nm04f nm05f nm06f)

set n_csagrid = (nm13n nm14n nm15n nm16n nm17n nm18n nm19n)
set n_cssgrid = (nm20n nm21n)
set n_dsgrid  = (nm04n nm05n nm06n)
set n_fitgrid = (nm07n nm08n nm09n nm10n nm11n nm12n)
set n_natgrid = (nm01n nm02n nm03n)
set n_shgrid  = (nm22n)

set c_ngmath = ($c_natgrid $c_dsgrid)
set f_ngmath = ($f_natgrid $f_dsgrid)
set n_ngmath = ($n_natgrid $n_dsgrid $n_fitgrid $n_csagrid $n_cssgrid \
                $n_shgrid)

set csagrid_list = ($n_csagrid)
set cssgrid_list = ($n_cssgrid)
set dsgrid_list  = ($c_dsgrid $f_dsgrid $n_dsgrid)
set fitgrid_list = ($n_fitgrid)
set natgrid_list = ($c_natgrid $f_natgrid $n_natgrid)
set shgrid_list  = ($n_shgrid)

set ngmath_list  = ($c_ngmath $f_ngmath $n_ngmath)
set c_list = ($c_list $c_ngmath)
set f_list = ($f_list $f_ngmath)
set n_list = ($n_list $n_ngmath)

#*******************************#
#                               #
#  Set Streamlineplot examples  #
#                               #
#*******************************#
set c_stream  = (st01c st02c st03c st04c)
set f_stream  = (st01f st02f st03f st04f)
set n_stream  = (st01n st02n st03n st04n)
set stream_list = ($c_stream $f_stream $n_stream)
set c_list = ($c_list $c_stream)
set n_list = ($n_list $n_stream)
set f_list = ($f_list $f_stream)
set cdf_list = ($cdf_list st03c st03f st03n st04c st04f st04n)

#*************************#
#                         #
#  Set TextItem examples  #
#                         #
#*************************#
set c_textitem = (tx01c tx02c tx03c tx04c tx05c tx06c tx07c tx08c)
set f_textitem = (tx01f tx02f tx03f tx04f tx05f tx06f tx07f tx08f)
set n_textitem = (tx01n tx02n tx03n tx04n tx05n tx06n tx07n tx08n)
set textitem_list  = ($c_textitem $f_textitem $n_textitem)
set c_list = ($c_list $c_textitem)
set f_list = ($f_list $f_textitem)
set n_list = ($n_list $n_textitem)

#***********************#
#                       #
# set TickMark examples #
#                       #
#***********************#
set c_tickmark = (tm01c tm02c tm03c)
set f_tickmark = (tm01f tm02f tm03f)
set n_tickmark = (tm01n tm02n tm03n tm04n)
set tickmark_list  = ($c_tickmark $f_tickmark $n_tickmark)
set c_list = ($c_list $c_tickmark)
set f_list = ($f_list $f_tickmark)
set n_list = ($n_list $n_tickmark)

#**********************#
#                      #
#  Set Title examples  #
#                      #
#**********************#
set c_title = (ti01c ti02c ti03c)
set f_title = (ti01f ti02f ti03f)
set n_title = (ti01n ti02n ti03n)
set title_list  = ($c_title $f_title $n_title)
set c_list = ($c_list $c_title)
set f_list = ($f_list $f_title)
set n_list = ($n_list $n_title)

#***************************#
#                           #
#  Set Vectorplot examples  #
#                           #
#***************************#
set c_vector  = (vc01c vc02c vc03c vc04c vc05c vc06c vc07c vc08c vc09c)
set f_vector  = (vc01f vc02f vc03f vc04f vc05f vc06f vc07f vc08f vc09f)
set n_vector  = (vc01n vc02n vc03n vc04n vc05n vc06n vc07n vc08n vc09n)
set vector_list = ($c_vector $f_vector $n_vector)
set c_list = ($c_list $c_vector)
set f_list = ($f_list $f_vector)
set n_list = ($n_list $n_vector)
set cdf_list = ($cdf_list vc06c vc06f vc06n vc08c vc08f vc08n vc09c vc09f vc09n)

#***********************#
#                       #
#  Set XYPlot examples  #
#                       #
#***********************#
set c_xyplot  = (xy01c xy02c xy03c xy04c xy05c xy06c xy07c xy08c xy09c \
                 xy10c xy11c xy12c xy13c xy14c xy15c xy16c xy17c)
set f_xyplot  = (xy01f xy02f xy03f xy04f xy05f xy06f xy07f xy08f xy09f xy10f \
                 xy11f xy14f xy15f xy16f xy17f)
set n_xyplot  = (xy01n xy02n xy03n xy04n xy05n xy06n xy07n xy08n xy09n xy10n \
                 xy11n xy14n xy15n xy16n xy17n)
set xyplot_list = ($c_xyplot $f_xyplot $n_xyplot)
set c_list = ($c_list $c_xyplot)
set f_list = ($f_list $f_xyplot)
set n_list = ($n_list $n_xyplot)
set gui_list = ($gui_list xy12c xy13c)
set cdf_list = ($cdf_list xy06c xy10c xy06f xy10f xy06n xy10n)
set x_list = ($x_list xy11c xy11f xy11n xy12c xy13c)


#*********************#
#                     #
#  Set NUG examples   #
#                     #
#*********************#
set n_nug  = (NUG_GrADS_binary_data NUG_ICON_triangles_colored \
              NUG_axis_annotations NUG_bar_chart \
              NUG_bar_chart_col_above_below NUG_bar_chart_multi \
              NUG_bipolar_grid_MPI-ESM \
              NUG_bipolar_grid_MPI-ESM_subregion \
              NUG_color_Land_Ocean NUG_color_country_user \
              NUG_colormaps NUG_contour_dashpattern \
              NUG_contour_filled_map NUG_contour_fillpattern \
              NUG_contour_labels NUG_contour_map \
              NUG_contour_map_add_zonal_average \
              NUG_curvilinear_basic NUG_curvilinear_grid \
              NUG_date_format NUG_function_procedure \
              NUG_globe_orography_grid_resolution \
              NUG_grid_resolution_comparison \
              NUG_histograms-transparent NUG_histograms \
              NUG_insert_logo NUG_labelbar_title \
              NUG_labelbars NUG_legends NUG_map_countries \
              NUG_map_default NUG_map_grid_and_tickmark_settings \
              NUG_map_land_ocean_settings NUG_map_resolutions \
              NUG_map_selected_countries NUG_map_settings \
              NUG_masking NUG_multi_timeseries \
              NUG_multiple_plots_along_y NUG_panel_plot_3x2 \
              NUG_panel_control NUG_panel_plot NUG_panel_vp \
              NUG_plot_in_5_steps NUG_plot_rotated_grid \
              NUG_polar_NH NUG_polyline_polygon_polymarker \
              NUG_primitives \
              NUG_projections NUG_projections_mollweide \
              NUG_projections_robinson NUG_read_ASCII_1 \
              NUG_read_ASCII_2 NUG_read_ASCII_3 \
              NUG_read_Binary_1 NUG_read_Binary_GrADS \
              NUG_read_CSV_1 NUG_read_CSV_2 \
              NUG_regrid_bilinear_CMIP5_grid_to_1x1deg_grid \
              NUG_regrid_curvilinear_to_rectilinear_bilinear_weights_ESMF \
              NUG_regrid_curvilinear_to_rectilinear_bilinear_wgts_destgrid_ESMF \
              NUG_regrid_rectilinear_to_curvilinear_bilinear_wgts_destgrid_ESMF \
              NUG_regrid_unstructured_to_rectilinear_bilinear_wgts_ESMF \
              NUG_regrid_unstructured_to_rectilinear_bilinear_wgts_destgrid_ESMF \
              NUG_shapefile_plot NUG_shapefile_plot_data \
              NUG_slice_plot NUG_statistics_linear_regression \
              NUG_statistics_running_mean NUG_strings \
              NUG_system_calls NUG_template_script \
              NUG_text_settings NUG_title_strings \
              NUG_transparent_filled_contour \
              NUG_transparent_land_sea_mask_Africa \
              NUG_triangular_grid_ICON NUG_triangular_grid_ICON_640 \
              NUG_tripolar_grid_STORM \
              NUG_unstructured_grid NUG_unstructured_grid_640 \
              NUG_unstructured_grid_ICON \
              NUG_use_Fortran_subroutines NUG_vector_curly \
              NUG_vector_default NUG_vector_plot_colorized \
              NUG_vector_plot_overlay NUG_wind_mag \
              NUG_write_ASCII_1 NUG_write_ASCII_2 \
              NUG_write_ASCII_3 NUG_write_ASCII_4 \
              NUG_write_ASCII_5 NUG_write_Binary_1 \
              NUG_write_Binary_2 NUG_write_netCDF_1 \
              NUG_write_netCDF_2 NUG_xy_plot NUG_xy_plot_res \
              NUG_xy_plot_timeseries NUG_xy_plot_timeseries_2 \
              )
set nug_list = ($n_nug)

#*********************#
#                     #
#  Set GSUN examples  #
#                     #
#*********************#
set n_gsun  = (gsun01n gsun02n gsun03n gsun04n gsun05n gsun06n gsun07n \
               gsun08n gsun09n gsun10n gsun11n)
set gsun_list = ($n_gsun)
set n_list = ($n_list $n_gsun)
set cdf_list = ($cdf_list gsun02n gsun03n gsun04n gsun05n gsun06n gsun09n)
set x_list = ($x_list gsun01n gsun02n gsun03n gsun04n gsun05n gsun06n gsun07n \
              gsun08n gsun10n gsun11n)

#*******************************#
#                               #
# Parse options on command line #
#                               #
#*******************************#
set names
unset NCGM
unset X11
unset PS
unset PDF
unset OLDPS
unset PNG
unset OLDPDF

set ncarbd_flag
set ngmathbd_flag
set num_set = 0

set ws_type = "x11"

while ($#argv > 0)

  switch ($1)
    case "-all":
    case "-A":
      shift
      set names=($names $c_list $f_list $n_list)
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
        
    case "-NCL":
    case "-ncl":
      shift
      set names=($names $n_list)
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
      set change_ws_type
      set ws_type = $1
      if ("$ws_type" == "NCGM" || "$ws_type" == "ncgm") then
        set NCGM
        @ num_set += 1
      else if ("$ws_type" == "X11" || "$ws_type" == "x11") then
        set X11
        @ num_set += 1
      else if ("$ws_type" == "OLDPS" || "$ws_type" == "oldps") then
        set OLDPS
        @ num_set += 1
      else if ("$ws_type" == "oldpdf" || "$ws_type" == "OLDPDF") then
        set OLDPDF
        @ num_set += 1
      else if ("$ws_type" == "ps" || "$ws_type" == "PS") then
        set PS
        @ num_set += 1
      else if ("$ws_type" == "png"    || "$ws_type" == "PNG") then
        set PNG
        @ num_set += 1
      else if ("$ws_type" == "pdf" || "$ws_type" == "PDF") then
        set PDF
        @ num_set += 1
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
        
    case "-ncarbd":
      shift
      set ncarbd_flag = "-ncarbd"
      breaksw

    case "-ngmathbd":
      shift
      set ngmathbd_flag = "-ngmathbd"
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
        
    case "-csagrid":
    case "-Csagrid":
      shift
      set names=($names $csagrid_list)
      breaksw

    case "-cssgrid":
    case "-Cssgrid":
      shift
      set names=($names $cssgrid_list)
      breaksw

    case "-dsgrid":
    case "-Dsgrid":
      shift
      set names=($names $dsgrid_list)
      breaksw

    case "-fitgrid":
    case "-Fitgrid":
      shift
      set names=($names $fitgrid_list)
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

    case "-natgrid":
    case "-Natgrid":
      shift
      set names=($names $natgrid_list)
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

    case "-shgrid":
    case "-Shgrid":
      shift
      set names=($names $shgrid_list)
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

    case "-gsun":
    case "-GSUN":
      shift
      set names=($names $gsun_list)
      breaksw

    case "-nug":
    case "-NUG":
      shift
      set names=($names $nug_list)
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
# NCGM/PS/PDF already exists, don't generate       #
# it again.                                        #
#**************************************************#
if ($?NCGM && $?Unique && -f $name.ncgm) goto theend
if ($?OLDPS && $?Unique && -f $name.ps) goto theend
if ($?OLDPDF && $?Unique && -f $name.pdf) goto theend
if ($?PS && $?Unique && -f $name.ps) goto theend
if ($?PDF && $?Unique && -f $name.pdf) goto theend
if ($?PNG && $?Unique && (-f $name.000001.png || -f $name.png)) goto theend

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

foreach known ($n_list)
  if ("$name" == "$known") then
    set ex_type = "NCL"
    break
  endif
end

foreach known ($nug_list)
  if ("$name" == "$known") then
    set ex_type = "NUG"
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

    case NCL:
    echo ""
    echo "NCAR Graphics NCL Example <$name>"
    echo ""
    breaksw 
 
    case NUG:
    echo ""
    echo "NCL User Guide Example <$name.ncl>"
    echo ""
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
if($ex_type != "NUG") then
  set obj = `expr $name : '\(.*\)[0-9][0-9].*'`
  set obj_dir = `eval echo \$dir_$obj`
else
  set obj = `expr $name : '\(.*\)[0-9][0-9].*'`
  set obj_dir = $dir_nug
endif

#************************************************#
#                                                #
# Determine if it's a C, Fortran, or NCL example #
#                                                #
#************************************************#
unset fprog
unset cprog
unset nprog
set res_file
set inc_file
set ascdata_file
set bindata_file
set ncl_file
set comp_flags = ($ncarbd_flag $ngmathbd_flag)
set generic_name = `expr $name : '\(.*[0-9][0-9]\).*'`

if($ex_type != "NUG") then
  set prog_type = `expr $name : '.*[0-9][0-9]\(.\)'`
else
  set prog_type = "nug"
endif
set resfile_dir = "$res_dir/$obj_dir"

if ("$prog_type" == "c") then
  if (! -d "$hluex_dir") then
    echo "Warning: HLU directory <$hluex_dir> doesn't exist."
    echo "         Won't generate $name example."
    continue
  endif
  set cprog
  set example_dir = "$hluex_dir/$obj_dir"
  set src_file = "$name.c"
else if ("$prog_type" == "f") then
  set fprog
  set example_dir = "$hluex_dir/$obj_dir"
  set src_file = "$name.f"
else if ("$prog_type" == "n" || "$prog_type" == "nug") then
  if (! -d "$nclex_dir") then
    echo "Warning: NCL directory <$nclex_dir> doesn't exist."
    echo "         Won't generate $name example."
    continue
  endif
  if ("$prog_type" == "n") then
    set example_dir = "$nclex_dir/$obj_dir"
  else
    set example_dir = "$nclex_dir/$dir_nug"
  endif
  set nprog
  set src_file = "$name.ncl"
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
        echo "in order to compile this example."
        echo ""
        set comp_flags = ($comp_flags "-netcdf")
    breaksw

    case xy12c:
    case xy13c:
        echo ""
        echo "This example is a GUI example, and requires the X"
        echo "libraries Xm and Xt to be loaded during the link phase."
        echo ""
        echo "The output will be displayed to an X11 window, and an"
        echo "NCGM/PS/PDF/PNG file may or may not be produced."
        echo ""
        set comp_flags = ($comp_flags "-XmXt")
        if ("$name" == "xy12c") set ascdata_file = "xy12c.asc"
    breaksw

    case cn03f:
    case cn04n:
    case cn11f:
    case cn12n:
    case mp03n:
        set ascdata_file = "$name.asc"
    breaksw

    case xy16c:
    case xy16f:
    case xy16n:
        set ascdata_file = "xy.asc"
    breaksw

    case gsun11n:
        set ascdata_file = "u.cocos"
    breaksw

    case xy17c:
    case xy17f:
    case xy17n:
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
        set comp_flags = ($comp_flags "-ngmath")
    breaksw
endsw

switch($generic_name)
    case gsun01:
    case gsun02:
    case gsun03:
    case gsun04:
    case gsun05:
    case gsun06:
    case gsun07:
    case gsun10:
    case gsun11:
    case basic03:
        echo ""
        echo "This example was set up to display the output to an"
        echo "X11 window."
        echo ""
    breaksw

    case gsun09:
        echo ""
        echo "This example was set up to display the output to an"
        echo "NCGM file only."
        echo ""
    breaksw

    case basic04:
	case xy11:
	case xy12:
        echo ""
        echo "This example was set up to display the output to an"
        echo "X11 window and an NCGM."
        echo ""
    breaksw

	case cn15:
	case gsun08:
    case basic07:
        echo ""
        echo "This example was set up to display the output to an"
        echo "X11 window, an NCGM, a PDF, and a PostScript workstation."
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
# output to an NCGM file, an XWorkstation,#
# a PS file, a PDF file, or PNG file.     #
# If a type is explicitly requested, then #
# turn the other types off.               #
#                                         #
#*****************************************#
if ($?cprog && $?change_ws_type) then
ed << EOF - ./$src_file >& /dev/null
/wks_type =/d
i
  char const *wks_type = "$ws_type";
.
w
q
EOF
endif

if ($?fprog && $?change_ws_type) then
ed << EOF - ./$src_file >& /dev/null
/wks_type =/d
i
      wks_type = "$ws_type"
.
w
q
EOF
endif

if ($?nprog && $?change_ws_type) then
ed << EOF - ./$src_file >& /dev/null
/wks_type = "
c
wks_type = "$ws_type"
.
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
if (! $?nprog) then
  set rmfiles = ($name.o ${name}SED_EXE_SUFFIX $rmfiles)
endif

#******************************#
#                              #
# Compile and link the example #
#                              #
#******************************#
   
if (! $?NoRunOption) then
    echo ""
    if ($?fprog || $?cprog) then   
      echo "Compiling and linking..."
      if ($?cprog) then
        nhlcc -o $name $src_file $comp_flags
      else
        nhlf77 -o $name $src_file $comp_flags
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
    else 
      echo "Running NCL..."
      ncl < $src_file
      if ($status != 0) then
          echo ""
          echo "ncl failed."
          echo ""
          exit
      endif
      echo ""
      echo "Finished running 'ncl < $src_file'..."
      echo ""
    endif
    if( -e $name.ncgm ) then
      echo ""
      echo "    Example $name produced a metafile."
      echo "    Metafile is named $name.ncgm."
      echo ""
    endif
    if( -e $name.ps  && ("$ws_type" == "oldps" || "$ws_type" == "OLDPS")) then
      echo ""
      echo "    Example $name produced an older version PostScript file."
      echo "    PS file is named $name.ps."
      echo ""
    endif
    if( -e $name.ps  && ("$ws_type" == "ps" || "$ws_type" == "PS")) then
      echo ""
      echo "    Example $name produced a cairo PostScript file."
      echo "    PS file is named $name.ps."
      echo ""
    endif
    if( -e $name.pdf  && ("$ws_type" == "oldpdf" || "$ws_type" == "OLDPDF")) then
      echo ""
      echo "    Example $name produced an older version PDF file."
      echo "    PDF file is named $name.pdf."
      echo ""
    endif
    if( -e $name.pdf  && ("$ws_type" == "pdf" || "$ws_type" == "PDF")) then
      echo ""
      echo "    Example $name produced a cairo PDF file."
      echo "    PDF file is named $name.pdf."
      echo ""
    endif

    if($ex_type == "NUG") then
      echo ""
      echo "    Example $src_file produced PNG file."
      echo ""
    endif

    if( (-e $name.png || -e $name.000001.png)  && ("$ws_type" == "png" || "$ws_type" == "PNG")) then
      echo ""
      echo "    Example $name produced one or more cairo PNG files."
      echo "    If single PNG file, it is named $name.png"
      echo "    If multiple PNG files, first PNG file is named $name.000001.png, and so on."
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

