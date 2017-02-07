;----------------------------------------------------------
;-- NCL User Guide example:  
;--   NUG_regrid_rectilinear_to_curvilinear_bilinear_wgts_destgrid_ESMF.ncl
;--
;-- Description:      regrid to curvilinear grid
;--
;-- Interpolation:    ESMF - bilinear
;--
;-- Data:             ECHAM5 (rectilinear 192x96)
;--
;-- 2015-04-01 kmf
;----------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

begin
  start_time = get_cpu_time()               ;-- get cpu time

;-- read data
  diri    = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili    = "tas_rectilinear_grid_2D.nc"
  dird    = "./"
  dstfili = "thetao_curvilinear_ocean.nc"   ;-- get destination grid from file
    
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
  
  if (.not. fileexists(dird+dstfili)) then
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+dstfili)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+dstfili)
     print("")
     exit
  end if
  
  f       =  addfile(diri+fili,"r")
  var     =  f->tas(0,:,:) 

;-- read the destination grid lat/lon arrays
  d       =  addfile(dird+dstfili,"r")
  dst_lat =  d->lat
  dst_lon =  d->lon
  dvar    =  d->thetao(0,0,:,:)
  dims    =  dimsizes(dst_lat)
  nlat    =  dims(0)
  nlon    =  dims(1)
  printVarSummary(dvar)
  print("")
  print("----> Destination dims:  lat "+nlat+"  lon "+nlon)
  print("")
  
;-- name of output file
  outputfile = "regridded_rectilinear_to_curvilinear_bilinear_wgts_destgrid_ESMF.nc"

;-- set resources
  Opt                 =  True
  Opt@InterpMethod    = "bilinear"                  ;-- interpolation method
  Opt@SrcFileName     = "ECHAM5_SCRIP_bilinear.nc"  ;-- source file name
  Opt@DstFileName     = "WorldCurvilinear_SCRIP_bilinear.nc" ;-- destination file name
  Opt@WgtFileName     = "ECHAM5toWorldCurvilinear_bilinear.nc";-- name of weights file, which will be generated
  Opt@ForceOverwrite  =  True                       ;-- force overwrite 
  Opt@DstMask2D       =  where(ismissing(dvar),0,1) ;-- create mask from destination
  Opt@DstGridType     = "curvilinear"               ;-- Destination grid
  Opt@DstTitle        = "World Grid Curvilinear Resolution bilinear" ;-- destination title
  Opt@DstGridLon      =  dst_lon
  Opt@DstGridLat      =  dst_lat
  
  print("---------------------------------------------------")
  print("Generating interpolation weights from ECHAM5 to")
  print("World destination curvilinear grid.")
  print("")
  print("Method: bilinear")
  print("---------------------------------------------------")
    
;-- call ESMF_regrid
  var_regrid   =  ESMF_regrid(var,Opt)
  var_regrid!0 = "y"                        ;-- named coordinate
  var_regrid!1 = "x"                        ;-- named coordinate
  printVarSummary(var_regrid)

  delete(var_regrid@lat2d)                  ;-- delete attribute array lat2d
  delete(var_regrid@lon2d)                  ;-- delete attribute array lon2d
  
;-- assign a output netcdf file for the new regridded data
  system("rm -rf "+outputfile)
  fout =  addfile(outputfile, "c")

;-- start to define output file settings
  setfileoption(fout,"DefineMode",True) ;-- explicitly declare file definition mode

;-- create global attributes of output file
  fAtt                  =  True             ;-- assign file attributes
  fAtt@Conventions      = "CF-1.4"   
  fAtt@comment          = "Regrid rectilinear grid to curvilinear grid using ESMF"       
  fAtt@title            = "Regrid to curvilinear grid"
  fAtt@project_id       = "NCL User Guide" 
  fAtt@source_file      =  fili
  fAtt@creation_date    =  systemfunc ("date")        
  fAtt@history          = "NUG_regrid_rectilinear_to_curvilinear_bilinear_wgts_destgrid_ESMF.ncl: "+fili+" to curvilinear grid" 
  fileattdef(fout,fAtt)                     ;-- copy file attributes    

;-- predefine the coordinate variables and their dimensionality
  dimNames = (/"y", "x"/)                   ;-- curvilinear grid: dimensions not lat/lon 
  dimSizes = (/nlat,  nlon/)                ;-- dimension size of destination y/x
  dimUnlim = (/False, False/)   
  filedimdef(fout,dimNames,dimSizes,dimUnlim)

;-- predefine the the dimensionality of the variables to be written out
  filevardef(fout, "lat",  typeof(dst_lat),    getvardims(dst_lat)) ;-- variable lat not dimension                    
  filevardef(fout, "lon",  typeof(dst_lon),    getvardims(dst_lon)) ;-- variable lon not dimension                  
  filevardef(fout, "var",  typeof(var_regrid), getvardims(var_regrid))

;-- copy attributes associated with each variable to output file
  filevarattdef(fout,"lat",  dst_lat)       ;-- copy attributes from destination lat
  filevarattdef(fout,"lon",  dst_lon)       ;-- copy attributes from destination lon
  filevarattdef(fout,"var",  var_regrid)    ;-- copy var_regrid attributes

;-- explicitly exit file definition mode (not required)
  setfileoption(fout,"DefineMode",False)

;-- output only the data values since the dimensionality and such have been predefined;
;-- the "(/", "/)" syntax tells NCL to only output the data values to the predefined 
;-- locations on the file
  fout->lat =  (/dst_lat/)                  ;-- write lat to new netCDF file
  fout->lon =  (/dst_lon/)                  ;-- write lon to new netCDF file
  fout->var =  (/var_regrid/)               ;-- write variable to new netCDF file

;-- get the resulting CPU time
  end_time =  get_cpu_time()
  cpu_time =  end_time - start_time
  print("Elapsed time:  "+ cpu_time + "s")

;----------------------------------------------------------
;-- control the netCDF output file; open file and read variable
;----------------------------------------------------------
  p = addfile(outputfile,"r")
  new_var = p->var
  new_var@lat2d   =  p->lat
  new_var@lon2d   =  p->lon  
  
;-- open a workstation
  wks_type          = "png"
  wks_type@wkWidth  =  1024
  wks_type@wkHeight =  1024
  wks = gsn_open_wks(wks_type,"NUG_regrid_rectilinear_to_curvilinear_bilinear_wgts_destgrid_ESMF")

;-- set resources for contour plots
  res                      =  True
  res@gsnDraw              =  False
  res@gsnFrame             =  False
  res@gsnLeftString        = "tas (original)"
  res@gsnCenterString      = "rectilinear"
  res@gsnAddCyclic         =  True
  res@cnFillOn             =  True
  res@cnFillPalette        = "BlueWhiteOrangeRed" ;-- choose color map
  res@cnLevelSelectionMode = "ManualLevels" ;-- manual contour levels
  res@cnMinLevelValF       =  230.          ;-- minimum level
  res@cnMaxLevelValF       =  310.          ;-- maximum level
  res@cnLevelSpacingF      =    5.          ;-- contour spacing
   
  res@lbLabelBarOn         =  False                   ;-- no single label bar

;-- create the plot with original data on rectilinear grid
  plot0 = gsn_csm_contour_map(wks, var, res)

;-- create the plot with regridded data on curvilinear grid
  res2                     =  res
  res2@gsnLeftString       = "tas (regridded)"
  res2@gsnCenterString     = "curvilinear (MPIOM)"
  res2@gsnAddCyclic        =  True

  plot1 = gsn_csm_contour_map(wks, new_var, res2) ;-- from output netCDF file
  
;-- create the panel plot
  pres = True
  pres@gsnPanelMainString = "Regridding"     ;-- panel title string (resource added in 6.4.0)
; pres@txString           = "Regridding"     ;-- panel title string
  pres@gsnPanelLabelBar   =  True            ;-- turn on a common labelbar for the entire panel plot

  gsn_panel(wks,(/plot0,plot1/),(/2,1/),pres)
  
end
