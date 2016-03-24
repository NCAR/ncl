;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_write_netCDF_2.ncl
;
;  KMF
;-----------------------------------------------------------------
begin

  diri    = "./"
  fili    = "rectilinear_grid_2D.nc"
  outfile = "t_in_Celsius_2.nc"

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
  
  if (isfilepresent(outfile)) then
    system("rm -rf "+outfile)      ;-- make sure that file does not exist
  end if

;-- open data file
  fin  = addfile(diri+fili,"r")    ;-- open data file

;-- get variable t and its dimensions and dimension sizes
  tK    =  fin->tsurf              ;-- get variable
  
  time  =  fin->time               ;-- get dimension time
  lat   =  fin->lat                ;-- get dimension lat
  lon   =  fin->lon                ;-- get dimension lon
  
  ntim  =  dimsizes(time)          ;-- get dimension sizes of time
  nlat  =  dimsizes(lat)           ;-- get dimension sizes of lat
  nlon  =  dimsizes(lon)           ;-- get dimension sizes of lon

  printVarSummary (tK)             ;-- print variable information
  
;-- convert variable t: Kelvin to Celsius
  tC           =  tK               ;-- copy variable and its dimensions 
                                      ;and attributes
  tC           =  tK - 273.15      ;-- convert from Kelvin to Celsius
  tC@units     = "degC"            ;-- define new units

;-- create new netCDF file
  fout = addfile(outfile,"c")

;-- begin output file settings
  setfileoption(fout,"DefineMode",True) ;-- explicitly declare file
                                           ;definition mode
;-- create global attributes of the file
  fAtt               =  True       ;-- assign file attributes
  fAtt@title         = "NCL Efficient Approach to netCDF Creation"  
  fAtt@source_file   =  fili
  fAtt@Conventions   = "CF"   
  fAtt@creation_date =  systemfunc ("date")        
  fAtt@history       = "NCL script: NUG_write_netCDF_2.ncl" 
  fAtt@comment = "Convert variable tsurf from degrees Kelvin to degrees Celsius"       
  fileattdef(fout,fAtt)            ;-- copy file attributes    

;-- predefine the coordinate variables and their dimensionality
  dimNames = (/"time", "lat", "lon"/)  
  dimSizes = (/ -1   ,  nlat,  nlon/) 
  dimUnlim = (/ True , False, False/)   
  filedimdef(fout,dimNames,dimSizes,dimUnlim)

;-- predefine the the dimensionality of the variables to be written out
  filevardef(fout, "time" ,typeof(time),getvardims(time)) 
  filevardef(fout, "lat"  ,typeof(lat), getvardims(lat))                          
  filevardef(fout, "lon"  ,typeof(lon), getvardims(lon))                          
  filevardef(fout, "tC"   ,typeof(tK),  getvardims(tK))

;-- copy attributes associated with each variable to the file
  filevarattdef(fout,"time" ,time)   ;-- copy time attributes
  filevarattdef(fout,"lat"  ,lat)    ;-- copy lat attributes
  filevarattdef(fout,"lon"  ,lon)    ;-- copy lon attributes
  filevarattdef(fout,"tC",   tC)     ;-- copy tC attributes

;-- explicitly exit file definition mode (not required)
  setfileoption(fout,"DefineMode",False)

;-- output only the data values since the dimensionality and such 
;-- have been predefined. The "(/.../)" syntax tells NCL to only 
;-- output the data values to the predefined locations on the file.
  fout->time   =  (/time/)           ;-- write time to new netCDF file
  fout->lat    =  (/lat/)            ;-- write lat to new netCDF file
  fout->lon    =  (/lon/)            ;-- write lon to new netCDF file
  fout->tC     =  (/tC/)             ;-- write variable to new netCDF file
  
end
