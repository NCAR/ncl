;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_function_procedure.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
;======================================
;== define the function wind_mag(u,v)
;======================================
undef("wind_mag")
function wind_mag(u,v)
local ma
begin
  ma = sqrt(u^2 + v^2)
  return(m)
end

;======================================
;== define the function my_pi()
;======================================
undef("my_pi")
function my_pi()
local lpi
begin
  lpi = 4*atan(1)
  return(lpi)
end

;======================================
;== define function average_data(v,t)
;======================================
undef("average_data")
function average_data(v,atype:integer)
local my_avg
begin
;-- variable with 3 dimensions (time,lat,lon)
   if (atype .eq. 0) then
      my_avg = dim_avg_n_Wrap(v,0)
   else if (atype .eq. 1) then
           my_avg = dim_avg_n_Wrap(v,1)
        end if
   end if
   return(my_avg)
end

;=========================================================================
;==   MAIN script
;=========================================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
  t1 = systemfunc("date")
  t2 = systemfunc("date +%s")

;--------------------------------------------------------------------------------
;-- use function my_pi()
;--------------------------------------------------------------------------------
  x = my_pi()
  print(x)
  print("--------------------------------------")

;--------------------------------------------------------------------------------
;-- use function average_data()
;--------------------------------------------------------------------------------
  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "tas_rectilinear_grid_2D.nc"
  
  if (fileexists(diri+fili)) then
     file1 = addfile(diri+fili, "r")
  else
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
  
  variable =  file1->tas(0,:,:)
  wks      =  gsn_open_wks("png","NUG_function_procedure")

  res                   =  True
  res@tiMainString      = "Function: average_data~C~~C~   dim_avg_n_Wrap"
  res@tiMainFontHeightF =  0.02

;-- avg_type = 0 (time average at each lat grid points)
;-- avg_type = 1 (time average at each lon grid points)
  avg_type = 1
  
;-- calculate the averages
  dummy = average_data(variable,avg_type)
  printVarSummary(dummy)
  
;-- the dimension name of rlat and rlon will be changed within NCL to lat and lon
;-- set x-coordinate to lat/lon array
  if (avg_type .eq.0) then
     x := variable&lon
  else
     if (avg_type .eq.1) then
        x := variable&lat
     end if
  end if
  
;-- draw the contour map
  plot = gsn_csm_xy(wks, x, dummy, res)

end
