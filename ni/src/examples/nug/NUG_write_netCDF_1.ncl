;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_write_netCDF_1.ncl
;
;  KMF
;-----------------------------------------------------------------
begin
  diri    = "./"
  fili    = "rectilinear_grid_2D.nc"
  outfile = "t_in_Celsius_1.nc"
  
  if (isfilepresent(outfile)) then
    system("rm -rf "+outfile)      ;-- make sure that file does not exist
  end if
  
  fin  = addfile(diri+fili,"r")    ;-- open data file
  fout = addfile(outfile,"c")      ;-- create new file (netcdf 3)

  filedimdef(fout,"time",-1,True)  ;-- make time and UNLIMITED dimension

  tK           =  fin->tsurf       ;-- get variable
  tC           =  tK               ;-- copy variable and its related
                                      ;dimensions and attributes
  tC           =  tK - 273.15      ;-- convert from Kelvin to Celsius
  tC@units     = "degC"            ;-- define new units

  fout->tC     =  tC               ;-- write variable to new netCDF file
  
end
