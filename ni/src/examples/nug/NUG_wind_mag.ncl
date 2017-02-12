;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_wind_mag.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------

;==========================================
;== Check for existence of file and open it
;==========================================
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

  
;======================================
;== define the function wind_mag(u,v)
;======================================
undef("wind_mag")
function wind_mag(u,v)
local ma
begin
  ma = u                             ; Trick for copy metadata from u to ma
  ma@long_name = "wind magnitude"
  ma@standard_name = "magnitude_of_wind_velocity"
  if(isatt(ma,"cell_methods")) then
    delete(ma@cell_methods)
  end if
  if(isatt(ma,"original_name")) then
    delete(ma@original_name)
  end if
  ma = sqrt(u^2 + v^2)
  return(ma)
end
;=========================================================================
;==   MAIN script
;=========================================================================
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fil1 = "uas_rectilinear_grid_2D.nc"
  fil2 = "vas_rectilinear_grid_2D.nc"

  f1 = open_file(diri,fil1)
  f2 = open_file(diri,fil2)
  u  = f1->uas(0,:,:)
  v  = f2->vas(0,:,:)

  wks  =  gsn_open_wks("png","NUG_wind_mag")

  res                   =  True
  res@gsnAddCyclic      =  True
  res@tiMainString      = "NCL Doc Example: Function wind_mag()"
  res@tiMainFontHeightF =  0.02
 
  mag             = wind_mag(u,v)
  mag&lat@units  = "degrees_north"
  mag&lon@units  = "degrees_east"

  printVarSummary(mag)

  ;-- draw the contour map

  plot = gsn_csm_contour_map(wks,mag,res)

  print("--------------------------------------")

;------------------------------------------------------------------
;-- if you don't copy ma=u within the function wind_mag, there are
;-- no dimension names or coordinate arrays at all. The user has to
;-- define the named dimensions and attributes by himself.
;------------------------------------------------------------------
  show_flag = False
  if(show_flag) then
    mm               = sqrt(u^2+v^2)
    mm!0             = "lat"
    mm!1             = "lon"
    mm&lat           = u&lat
    mm&lon           = u&lon
    mm&lat@units     = "degrees_north"
    mm&lon@units     = "degrees_east"
    mm@standard_name = "magnitude_of_wind_velocity"
    mm@long_name     = "magnitude (sqrt(u^2+v^2)"
    mm@units         = "m s-1"
    printVarSummary(mm)

    plot = gsn_csm_contour_map(wks,mm,res)
  end if

end
