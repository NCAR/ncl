;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_contour_map.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---- read the data and define variable reference var

  diri = "./"
  fili = "rectilinear_grid_2D.nc"
   
  if (fileexists(diri+fili)) then
     f = addfile(diri+fili, "r")
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

  var  =  f->tsurf(0,:,:)

;---- define the workstation (plot output type and name)
  wks = gsn_open_wks("png","NUG_contour_map")

;---- set resources
  res                       =  True
  res@gsnMaximize           =  True
  res@tiMainString          = "NCL Doc Example:  contour map"   ;-- title string
  res@tiMainFontHeightF     =  0.02

;---- draw the contour map
  plot = gsn_csm_contour_map(wks, var, res)

end
