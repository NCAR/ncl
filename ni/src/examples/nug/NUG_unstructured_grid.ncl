;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_unstructured_grid.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

   diri  = "./"
   fili  = "camse_unstructured_grid.nc"
   
   f     =  addfile(diri+fili, "r")
   var   =  f->T850
   lat1d =  f->lat
   lon1d =  f->lon

  
;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","plot_unstructured_grid_camse")

;-- set resources
   res                       =  True
   res@gsnMaximize           =  True

   res@cnFillOn              =  True                   ;-- turn on contour fill
   res@cnFillPalette         = "BlueWhiteOrangeRed"    ;-- choose color map 

   res@tiMainString          = "NCL UG Example: Unstructured grid (CAM-SE)"  ;-- title string
   res@tiMainFontHeightF     =  0.02

;---Lat/lon arrays of curvilinear grid for overlaying on map
   res@sfXArray              =  lon1d
   res@sfYArray              =  lat1d

;-- draw the contour map
   plot = gsn_csm_contour_map(wks,var,res)

end
