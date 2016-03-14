;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_contour_map.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---- read the data and define variable reference var

  diri = "./"
  fili = "rectilinear_grid_2D.nc"
   
  f    =  addfile(diri+fili, "r")
  var  =  f->tsurf(0,:,:)

;---- define the workstation (plot output type and name)
  wks = gsn_open_wks("png","plot_contour_map")

;---- set resources
  res                       =  True
  res@gsnMaximize           =  True
  res@tiMainString          = "NCL UG Example:  contour map"   ;-- title string
  res@tiMainFontHeightF     =  0.02

;---- draw the contour map
  plot = gsn_csm_contour_map(wks, var, res)

end
