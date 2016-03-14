;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_curvilinear_basic.ncl
; 
;  KMF 
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
   diri = "./"
   fili = "tos_ocean_bipolar_grid.nc"
   
   f           =  addfile(diri+fili, "r")
   var         =  f->tos(0,:,:)
   var@lat2d   =  f->lat
   var@lon2d   =  f->lon

   wks = gsn_open_wks("png","plot_curvilinear_basic")  ;-- open a workstation

   res               =  True                  ;-- set resources
   res@gsnAddCyclic  =  False                 ;-- lon < 360 degrees
   res@cnFillOn      =  True                  ;-- turn on contour fill
   res@cnFillPalette = "BlueWhiteOrangeRed"   ;-- change color map
   res@tiMainString  = "Curvilinear grid:  MPI-ESM  (2D lat/lon arrays)"

   plot = gsn_csm_contour_map(wks,var,res)           ;-- create the plot

end
