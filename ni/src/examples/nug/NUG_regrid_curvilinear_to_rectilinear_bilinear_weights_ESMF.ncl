;---------------------------------------------------
;  NCL User Guide Example:   NUG_regrid_curvilinear_to_rectilinear_bilinear_weights_ESMF.ncl
;
;  KMF
;---------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

begin
  start_time = get_cpu_time()                    ;-- get cpu time

  diri = "./"
  fili = "thetao_curvilinear_ocean.nc"

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
  sfile               =  addfile(diri+fili,"r")
  thetao              =  sfile->thetao(0,0,:,:)
  thetao@lat2d        =  sfile->lat
  thetao@lon2d        =  sfile->lon
  printVarSummary(thetao)

;-- set resources
  Opt                 =  True
  Opt@InterpMethod    = "bilinear"                ;-- interpolation method
  Opt@SrcFileName     = "CMIP5_SCRIP_bilinear.nc" ;-- source file name
  Opt@DstFileName     = "World1deg_SCRIP_bilinear.nc"  
                                                  ;-- destination file name
  Opt@WgtFileName     = "CMIP5toWORLD_1x1_bilinear.nc"     
                        ;-- name of weights file, which will be generated
  Opt@ForceOverwrite  =  True                     ;-- force overwrite 
  Opt@SrcMask2D       =  where(.not.ismissing(thetao),1,0) 
                                                  ;-- what to mask
  Opt@DstGridType     = "1x1"                     ;-- Destination grid
  Opt@DstTitle        = "World Grid 1x1-degree Resolution bilinear" 
                                                  ;-- destination title
  Opt@DstLLCorner     = (/-89.75d,   0.00d /)
                                         ;-- destination lower left corner
  Opt@DstURCorner     = (/ 89.75d, 359.75d /) 
                                         ;-- destination upper right corner
  print("---------------------------------------------------")
  print("Generating interpolation weights from CMIP5 to")
  print("World 1x1 degree grid.")
  print("")
  print("Method: bilinear")
  print("---------------------------------------------------")
    
;-- call ESMF_regrid
  thetao_regrid = ESMF_regrid(thetao,Opt)
  printVarSummary(thetao_regrid)
  nlon = dimsizes(thetao_regrid&lon)
  nlat = dimsizes(thetao_regrid&lat)
  
;-- assign a output netcdf file for the new regridded data 
;-- (npoints = 180x360)
  system("rm -rf regridded_curvilinear_bilinear_CMIP5_thetao_ESMF.nc")
  fout = addfile("regridded_curvilinear_bilinear_CMIP5_thetao_ESMF.nc","c")

;-- start to define output file settings
  setfileoption(fout,"DefineMode",True) 
                                ;-- explicitly declare file definition mode
;-- create global attributes of the file
  fAtt             =  True         ;-- assign file attributes
  fAtt@Conventions = "CF-1.4"   
  fAtt@comment     = "Regrid curvilinear to 1x1 rectilinear grid using ESMF"       
  fAtt@title       = "Regrid to 1x1 deg rectilinear grid"
  fAtt@project_id  = "NCL User Guide" 
  fAtt@source_file =  fili
  fAtt@creation_date =  systemfunc ("date")        
  fAtt@history = "NUG_regrid_CMIP5_bilinear_with_weights.ncl: "+fili+" to 1x1 deg rectilinear grid" 
  fileattdef(fout,fAtt)                          ;-- copy file attributes    

;-- predefine the coordinate variables and their dimensionality
  dimNames = (/"lat", "lon"/)  
  dimSizes = (/nlat,  nlon/) 
  dimUnlim = (/False, False/)   
  filedimdef(fout,dimNames,dimSizes,dimUnlim)

;-- predefine the the dimensionality of the variables to be written out
  filevardef(fout,"lat",typeof(thetao_regrid&lat), \
                                       getvardims(thetao_regrid&lat))                          
  filevardef(fout,"lon",typeof(thetao_regrid&lon), \
                                       getvardims(thetao_regrid&lon))                          
  filevardef(fout,"thetao",typeof(thetao_regrid), \
                                       getvardims(thetao_regrid))

;-- copy attributes associated with each variable to the file
  filevarattdef(fout,"lat",thetao_regrid&lat)     ;-- copy lat attributes
  filevarattdef(fout,"lon",thetao_regrid&lon)     ;-- copy lon attributes
  filevarattdef(fout,"thetao",thetao_regrid)
                                        ;-- copy thetao_regrid attributes

;-- explicitly exit file definition mode (not required)
  setfileoption(fout,"DefineMode",False)

;-- output only the data values since the dimensionality and such 
;-- have been predefined; the "(/", "/)" syntax tells NCL to only 
;-- output the data values to the predefined locations on the file
  fout->lat    =  (/thetao_regrid&lat/)    ;-- write lat to new netCDF file
  fout->lon    =  (/thetao_regrid&lon/)    ;-- write lon to new netCDF file
  fout->thetao =  (/thetao_regrid/)   ;-- write variable to new netCDF file

;-- get the resulting CPU time
  end_time   =  get_cpu_time()
  cpu_time   =  end_time - start_time
  print("Elapsed time:  "+ cpu_time + "s")
  
end
