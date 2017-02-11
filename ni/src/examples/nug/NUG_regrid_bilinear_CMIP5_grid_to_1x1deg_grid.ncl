;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_regrid_bilinear_CMIP5_grid_to_1x1deg_grid.ncl
; 
;
;     Regrid CMIP5 grid to world grid 1x1 degrees - without plotting
;     Write regridded data to file: regrid_bilinear_CMIP5_thetao.nc
;
;     CMIP5_SCRIP_bilinear.nc         will be generated if not existing
;                                     contains source grid_center_lat, grid_center_lon,
;                                     grid_mask, grid_corner_lat, grid_corner_lon
;
;     World1deg_SCRIP_bilinear.nc     will be generated if not existing
;                                     contains destination grid_center_lat, grid_center_lon,
;                                     grid_mask, grid_corner_lat, grid_corner_lon
;
;     CMIP5toWORLD_1x1_bilinear.nc    will be generated if not existing
;                                     weights file
; KMF 30.10.14
;----------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

begin
  start_time = get_cpu_time()               ;-- get cpu time

;-- name of output file
  outfile = "regridded_CMIP5_to_rectilinear_bilinear_wgts_ESMF.nc"
  
;-- read data
  diri  = "./"
  fili  = "thetao_curvilinear_ocean.nc"

  if (.not. fileexists(diri+fili)) then
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     exit
  end if
  
  sfile        =  addfile(diri+fili,"r")
  thetao       =  sfile->thetao(0,0,:,:)
  thetao@lat2d =  sfile->lat
  thetao@lon2d =  sfile->lon

;-- set resources to generate the weights and grid files
  Opt                 =  True
  Opt@InterpMethod    = "bilinear"
  Opt@SrcFileName     = "CMIP5_SCRIP_bilinear.nc"         ;-- source grid file name
  Opt@DstFileName     = "World1deg_SCRIP_bilinear.nc"     ;-- destination grid file name
  Opt@WgtFileName     = "CMIP5toWORLD_1x1_bilinear.nc"    ;-- name of weights file, which will be generated
  Opt@ForceOverwrite  =  True
  Opt@SrcMask2D       =  where(.not.ismissing(thetao),1,0)
  Opt@DstGridType     = "1x1"                             ;-- destination grid type
  Opt@DstTitle        = "World Grid 1x1-degree Resolution bilinear"
  Opt@DstLLCorner     =  (/ -89.75d,   0.00d /)
  Opt@DstURCorner     =  (/  89.75d, 359.75d /)

;-- interpolate data from CMIP5 to World 1x1 degree grid using ESMF
  print("---------------------------------------------------------------------")
  print("Generating interpolation weights from CMIP5 to World 1x1 degree grid.")
  print("   Method:  bilinear")
  print("---------------------------------------------------------------------")

  thetao_regrid = ESMF_regrid(thetao,Opt)
  printVarSummary(thetao_regrid)

;-- write regridded data to file
  system("rm -rf "+outfile)
  fout = addfile(outfile, "c")
  fout->thetao = thetao_regrid

;-- get the resulting CPU time
  end_time =  get_cpu_time()
  cpu_time =  end_time - start_time
  print("Elapsed time:  "+ cpu_time + "s")

;-- open a workstation
  wks_type          = "png"
  wks_type@wkWidth  =  1024
  wks_type@wkHeight =  1024
  wks = gsn_open_wks(wks_type,"NUG_regrid_bilinear_CMIP5_grid_to_1x1deg_grid")

;-- set resources for contour plots
  res                      =  True
  res@gsnDraw              =  False
  res@gsnFrame             =  False
  res@gsnLeftString        = "thetao (original)"
  res@gsnCenterString      = "rectilinear"
  res@gsnAddCyclic         =  True
  res@cnFillOn             =  True
  res@cnFillPalette        = "BlueWhiteOrangeRed"
  res@cnLineLabelsOn       = False
  res@cnLevelSelectionMode = "ManualLevels" ;-- manual contour levels
  res@cnMinLevelValF       =  230.          ;-- minimum level
  res@cnMaxLevelValF       =  310.          ;-- maximum level
  res@cnLevelSpacingF      =    5.          ;-- contour spacing
   
  res@lbLabelBarOn         =  False                   ;-- no single label bar

;-- create the plot with original data on rectilinear grid
  plot0 = gsn_csm_contour_map(wks, thetao, res)

;-- create the plot with regridded data on curvilinear grid
  res2                     =  res
  res2@gsnLeftString       = "thetao (regridded)"
  res2@gsnCenterString     = "curvilinear (MPIOM)"
  res2@gsnAddCyclic        =  True

  plot1 = gsn_csm_contour_map(wks, thetao_regrid, res2) ;-- from output netCDF file
  
;-- create the panel plot
  pres = True
  pres@gsnPanelMainString = "Regridding"     ;-- panel title string (resource added in 6.4.0)
; pres@txString           = "Regridding"     ;-- panel title string
  pres@gsnPanelLabelBar   =  True            ;-- turn on a common labelbar for the entire panel plot

  gsn_panel(wks,(/plot0,plot1/),(/2,1/),pres)

end
