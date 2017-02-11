;----------------------------------------------------------
;-- NCL User Guide Example:  NUG_regrid_unstructured_to_rectilinear_bilinear_wgts_ESMF.ncl
;--
;-- Description:      regrid unstructured to 1x1 deg grid
;--
;-- Interpolation:    ESMF - bilinear
;--
;-- Data:             ICON (unstructured)
;--
;-- 2015-02-23 kmf
;----------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

begin
  start_time = get_cpu_time()                    ;-- get cpu time

  rad2deg =  45./atan(1.)                              ;-- radians to degrees

  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "triangular_grid_ICON.nc"
  
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
  
;-- read data
  f       =  addfile(diri+fili,"r")
  var     =  f->S(time|0,depth|0,ncells|:)  ;-- set variable with dims: (time,depth,ncells)
  printVarSummary(var)

  x       =  f->clon * rad2deg              ;-- cell center, lon
  y       =  f->clat * rad2deg              ;-- cell center, lat
  x!0     = "lon"                           ;-- set named dimension lon
  y!0     = "lat"                           ;-- set named dimension lat
  x@units = "degrees_east"                  ;-- set lon units
  y@units = "degrees_north"                 ;-- set lat units

  vlon    =  f->clon_vertices * rad2deg     ;-- cell longitude vertices
  vlon    =  where(vlon.lt.0, vlon + 360, vlon) ;-- longitude: 0-360
  vlat    =  f->clat_vertices * rad2deg     ;-- cell latitude vertices
  nv      =  dimsizes(vlon(0,:))            ;-- number of points in polygon

;-- set resources
  Opt                     =  True
  Opt@InterpMethod        = "bilinear"                         ;-- interpolation method
  Opt@ForceOverwrite      =  True                              ;-- force overwrite 
;  Opt@Debug               =  True                              ;-- print debug information
;  Opt@PrintTimings        =  True

  Opt@SrcFileName         = "CMIP5_SCRIP_bilinear.nc"          ;-- source file name
  Opt@SrcInputFileName    =  diri+fili                         ;-- optional, but good idea
  Opt@SrcRegional         =  False
  Opt@SrcGridLat          =  y
  Opt@SrcGridLon          =  x
  Opt@WgtFileName         = "ICONtoWORLD_1x1_bilinear.nc"      ;-- name of weights file, which will be generated

  Opt@DstFileName         = "World1deg_SCRIP_bilinear.nc"      ;-- destination file name
  Opt@DstGridType         = "rectilinear"                      ;-- destination grid
  Opt@DstTitle            = "World Grid 1x1-degree Resolution bilinear" ;-- destination title
  Opt@DstRegional         =  False
  Opt@DstGridLon          =  fspan(-180.,180.,360)
  Opt@DstGridLat          =  fspan(-90.,90.,180)
    
  print("---------------------------------------------------")
  print("Generating interpolation weights from ICON to")
  print("World 1x1 degree grid.")
  print("")
  print("Method: bilinear")
  print("---------------------------------------------------")
  
;-- call ESMF_regrid
  var_regrid = ESMF_regrid(var,Opt)                  ;-- do the regridding
  printVarSummary(var_regrid)
  nlon = dimsizes(var_regrid&lon)                    ;-- dim size new lon
  nlat = dimsizes(var_regrid&lat)                    ;-- dim size new lat
  
;-- assign a output netcdf file for the new regridded data (npoints = 180x360)
  system("rm -rf regridded_rectilinear_bilinear_ICON_S_ESMF.nc")
  fout =  addfile("regridded_rectilinear_bilinear_ICON_S_ESMF.nc", "c")

;-- start to define output file settings
  setfileoption(fout,"DefineMode",True)              ;-- explicitly declare file definition mode

;-- create global attributes of the file
  fAtt                  =  True                      ;-- assign file attributes
  fAtt@Conventions      = "CF-1.4"   
  fAtt@comment          = "Regrid unstructured mesh to 1x1 rectilinear grid using ESMF"       
  fAtt@title            = "Regrid to 1x1 deg rectilinear grid"
  fAtt@project_id       = "NCL User Guide" 
  fAtt@source_file      =  fili
  fAtt@creation_date    =  systemfunc ("date")        
  fAtt@history          = "NUG_regrid_ICON_bilinear_with_weights.ncl: "+fili+" to 1x1 deg rectilinear grid" 
  fileattdef(fout,fAtt)                              ;-- copy file attributes    

;-- predefine the coordinate variables and their dimensionality
  dimNames = (/"lat", "lon"/)  
  dimSizes = (/nlat,  nlon/) 
  dimUnlim = (/False, False/)   
  filedimdef(fout,dimNames,dimSizes,dimUnlim)

;-- predefine the the dimensionality of the variables to be written out
  filevardef(fout, "lat",   typeof(var_regrid&lat), getvardims(var_regrid&lat))                          
  filevardef(fout, "lon",   typeof(var_regrid&lon), getvardims(var_regrid&lon))                          
  filevardef(fout, "S",     typeof(var_regrid),     getvardims(var_regrid))

;-- copy attributes associated with each variable to the file
  filevarattdef(fout,"lat",   var_regrid&lat)        ;-- copy lat attributes
  filevarattdef(fout,"lon",   var_regrid&lon)        ;-- copy lon attributes
  filevarattdef(fout,"S",     var_regrid)            ;-- copy var_regrid attributes

;-- explicitly exit file definition mode (not required)
  setfileoption(fout,"DefineMode",False)

;-- output only the data values since the dimensionality and such have been predefined.
;-- The "(/", "/)" syntax tells NCL to only output the data values to the predefined 
;-- locations on the file.
  fout->lat =  (/var_regrid&lat/)                    ;-- write lat to new netCDF file
  fout->lon =  (/var_regrid&lon/)                    ;-- write lon to new netCDF file
  fout->S   =  (/var_regrid/)                        ;-- write variable to new netCDF file

;-- get the resulting CPU time
  end_time =  get_cpu_time()
  cpu_time =  end_time - start_time
  print("Elapsed time:  "+ cpu_time + "s")
  
;-- open a PNG file
  wks_type          = "png"
  wks_type@wkWidth  =  1024
  wks_type@wkHeight =  1024
  wks = gsn_open_wks(wks_type,"NUG_regrid_unstructured_to_rectilinear_bilinear_wgts_ESMF")

;-- set resources for contour plots
  res                    =  True
  res@gsnDraw            =  False                   ;-- don't draw plot yet
  res@gsnFrame           =  False                   ;-- don't advance the frame
  res@gsnCenterString    = "unstructured"
  res@gsnAddCyclic       =  False
  res@lbLabelBarOn       =  False                   ;-- no single label bar

  res@cnFillOn           =  True                    ;-- turn color fill on
  res@cnFillPalette      = "BlueWhiteOrangeRed"     ;-- choose color map 

  res@cnLinesOn          =  False                   ;-- turn lines off
  res@cnLineLabelsOn     =  False                   ;-- turn labels off
  res@cnLevelSelectionMode = "ManualLevels"         ;-- use manual contour line levels
  res@cnMinLevelValF     =  20.                     ;-- contour min. value
  res@cnMaxLevelValF     =  38.                     ;-- contour max. value
  res@cnLevelSpacingF    =   0.5                    ;-- contour interval

  res2                   =  res
  res2@gsnCenterString   = "rectilinear"
  
  res@cnFillMode         = "CellFill"               ;-- set fill mode
  res@sfXArray           =  x                       ;-- transform x to mesh scalar field
  res@sfYArray           =  y                       ;-- transform y to mesh scalar field
  res@sfXCellBounds      =  vlon                    ;-- needed if set cnFillMode = "CellFill"
  res@sfYCellBounds      =  vlat                    ;-- needed if set cnFillMode = "CellFill"

;-- create the plots
  plot0 = gsn_csm_contour_map(wks, var, res)        ;-- original data

  plot1 = gsn_csm_contour_map(wks, var_regrid, res2) ;-- regridded data
  
;-- create the panel plot
  pres                   =  True
  pres@txString          = "Regridding"             ;-- panel title string
  pres@gsnPanelLabelBar  =  True                    ;-- turn on a common labelbar for the entire panel plot

  gsn_panel(wks,(/plot0,plot1/),(/2,1/),pres)
  
end
