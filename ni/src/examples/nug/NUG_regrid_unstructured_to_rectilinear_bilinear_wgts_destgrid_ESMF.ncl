;----------------------------------------------------------
;-- NCL Doc example:  NUG_regrid_unstructured_to_rectilinear_bilinear_wgts_destgrid_ESMF.ncl
;--
;-- Description:      regrid unstructured to dest grid 192x96 grid
;--
;-- Interpolation:    ESMF - bilinear
;--
;-- Data:             ICON (unstructured)
;--
;-- 2015-02-23 kmf
;----------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

;-------------------------------------------
;-- Check for existence of file and open it
;-------------------------------------------
undef("open_file")
function open_file(dir[1]:string,name[1]:string)
begin
   if (fileexists(dir+name)) then
      f = addfile(dir+name, "r")
      return(f)
   end if
   print("")
   print("You don't have the necessary data for this script. You can download it from:​")
   print("")
   print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+name)
   print("")
   print("or use the wget command:")
   print("")
   print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+name)
   print("")
   exit
end
  
begin
  start_time = get_cpu_time()               ;-- get cpu time

  outputfile = "regridded_rectilinear_bilinear_ICON_S_ESMF_destgrid.nc"
  
  rad2deg =  45./atan(1.)                   ;-- radians to degrees

  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "triangular_grid_ICON.nc"
  grid = "tas_rectilinear_grid_2D.nc"       ;-- use grid from file

;-- read destination grid data
  g       =  open_file(diri,grid)
  dst_lat =  g->lat
  dst_lon =  g->lon

;-- read data
  f       =  open_file(diri,fili)
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
  Opt@InterpMethod        = "bilinear"      ;-- interpolation method
  Opt@ForceOverwrite      =  True           ;-- force overwrite 
;  Opt@Debug               =  True
;  Opt@PrintTimings        =  True

  Opt@SrcFileName         = "CMIP5_SCRIP_bilinear.nc" ;-- source file name
  Opt@SrcInputFileName    =  diri+fili      ;-- optional, but good idea
  Opt@SrcRegional         =  False
  Opt@SrcGridLat          =  y
  Opt@SrcGridLon          =  x
  Opt@WgtFileName         = "ICONtoWORLD_bilinear_192x96.nc" ;-- name of weights file, which will be generated

  Opt@DstFileName         = "World1deg_SCRIP_bilinear.nc"  ;-- destination file name
  Opt@DstGridType         = "rectilinear"   ;-- destination grid
  Opt@DstTitle            = "World Grid 1x1-degree Resolution bilinear" ;-- destination title
  Opt@DstRegional         =  False
  Opt@DstGridLon          =  dst_lon
  Opt@DstGridLat          =  dst_lat

  print("---------------------------------------------------")
  print("Generating interpolation weights from ICON to")
  print("World destination 192x96 degree grid.")
  print("")
  print("Method: bilinear")
  print("---------------------------------------------------")
  
;-- call ESMF_regrid
  var_regrid = ESMF_regrid(var,Opt)         ;-- do the regridding
  printVarSummary(var_regrid)
  nlon = dimsizes(var_regrid&lon)           ;-- dims new lon
  nlat = dimsizes(var_regrid&lat)           ;-- dims new lat
  
;-- assign a output netcdf file for the new regridded data (npoints = 180x360)
  system("rm -rf "+outputfile)
  fout =  addfile(outputfile, "c")

;-- start to define output file settings
  setfileoption(fout,"DefineMode",True)     ;-- explicitly declare file definition mode

;-- create global attributes of the file
  fAtt                  =  True             ;-- assign file attributes
  fAtt@Conventions      = "CF-1.4"   
  fAtt@comment          = "Regrid unstructured mesh to 192x96 rectilinear grid using ESMF"       
  fAtt@title            = "Regrid to 192x96 rectilinear grid"
  fAtt@project_id       = "NCL User Guide" 
  fAtt@source_file      =  fili
  fAtt@creation_date    =  systemfunc ("date")        
  fAtt@history          = "NUG_regrid_ICON_bilinear_with_weights.ncl: "+fili+\
                          " to 1x1 deg rectilinear grid" 
  fileattdef(fout,fAtt)                     ;-- copy file attributes    

;-- predefine the coordinate variables and their dimensionality
  dimNames = (/"lat", "lon"/)  
  dimSizes = (/nlat,  nlon/) 
  dimUnlim = (/False, False/)   
  filedimdef(fout,dimNames,dimSizes,dimUnlim)

;-- predefine the the dimensionality of the variables to be written out
  filevardef(fout, "lat", typeof(var_regrid&lat), getvardims(var_regrid&lat))                          
  filevardef(fout, "lon", typeof(var_regrid&lon), getvardims(var_regrid&lon))                          
  filevardef(fout, "S",   typeof(var_regrid),     getvardims(var_regrid))

;-- copy attributes associated with each variable to the file
  filevarattdef(fout,"lat", var_regrid&lat) ;-- copy lat attributes
  filevarattdef(fout,"lon", var_regrid&lon) ;-- copy lon attributes
  filevarattdef(fout,"S",   var_regrid)     ;-- copy var_regrid attributes

;-- explicitly exit file definition mode (not required)
  setfileoption(fout,"DefineMode",False)

;-- output only the data values since the dimensionality and such have been predefined.
;-- The "(/", "/)" syntax tells NCL to only output the data values to the predefined 
;-- locations on the file.
  fout->lat =  (/var_regrid&lat/)           ;-- write lat to new netCDF file
  fout->lon =  (/var_regrid&lon/)           ;-- write lon to new netCDF file
  fout->S   =  (/var_regrid/)               ;-- write variable to new netCDF file

;-- get the resulting CPU time
  end_time =  get_cpu_time()
  cpu_time =  end_time - start_time
  print("Elapsed time:  "+ cpu_time + "s")

;----------------------------------------------------------
;-- control the netCDF output file; open file and read variable
;----------------------------------------------------------
  p = addfile(outputfile,"r")
  new_var = p->S
  
;-- open a PNG file
  wks_type          = "png"
  wks_type@wkWidth  =  1024
  wks_type@wkHeight =  1024
  wks = gsn_open_wks(wks_type,"NUG_regrid_unstructured_to_rectilinear_bilinear_wgts_destgrid_ESMF")

;-- set resources for contour plots
  res                    =  True
  res@gsnDraw            =  False
  res@gsnFrame           =  False
  res@gsnCenterString    = "unstructured"
  res@gsnAddCyclic       =  False
  res@lbLabelBarOn       =  False           ;-- no single label bar

  res@cnFillOn           =  True
  res@cnFillPalette      = "BlueWhiteOrangeRed"
  res@cnLinesOn          =  False           ;-- turn lines off
  res@cnLineLabelsOn     =  False           ;-- turn labels off
  res@cnLevelSelectionMode = "ManualLevels" ;-- manual contour levels
  res@cnMinLevelValF     =  20.             ;-- minimum level
  res@cnMaxLevelValF     =  38.             ;-- maximum level
  res@cnLevelSpacingF    =   0.5            ;-- contour spacing

  res2                   =  res

  res@cnFillMode         = "CellFill"       ;-- set fill mode
  
  res@sfXArray           =  x               ;-- transform x to mesh scalar field
  res@sfYArray           =  y               ;-- transform y to mesh scalar field
  res@sfXCellBounds      =  vlon            ;-- needed if set cnFillMode = "CellFill"
  res@sfYCellBounds      =  vlat            ;-- needed if set cnFillMode = "CellFill"

;-- create the plot of the original data
  plot0 = gsn_csm_contour_map(wks, var, res)

;-- create the plot of the regridded data
  res2@gsnCenterString   = "rectilinear"
  res2@gsnAddCyclic      =  True

  plot1 = gsn_csm_contour_map(wks, new_var, res2)
  
;-- create the panel plot
  pres = True
  pres@gsnPanelMainString = "Regridding"     ;-- panel title string (resource added in 6.4.0)
; pres@txString           = "Regridding"     ;-- panel title string
  pres@gsnPanelLabelBar   =  True            ;-- turn on a common labelbar for the entire panel plot

  gsn_panel(wks,(/plot0,plot1/),(/2,1/),pres)
  
end
