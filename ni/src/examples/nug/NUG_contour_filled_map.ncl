;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_contour_filled_map.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

  diri = "./"
  fili = "rectilinear_grid_2D.nc"
   
  f    =  addfile(diri+fili, "r")
  var  =  f->tsurf(0,:,:)

;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","plot_contour_filled_map")

;-- set resources
  res                       =  True
  res@gsnMaximize           =  True

  res@cnFillOn              =  True            ;-- turn on contour fill
  res@cnFillPalette         = "BlueWhiteOrangeRed" ;-- choose color map
  res@cnLinesOn             =  False           ;-- turn off contour lines
  res@cnLineLabelsOn        =  False           ;-- turn off line labels
  res@cnLevelSelectionMode  = "ManualLevels"   ;-- set contour levels manually
  res@cnMinLevelValF        =  250.            ;-- minimum contour level
  res@cnMaxLevelValF        =  310.            ;-- maximum contour level
  res@cnLevelSpacingF       =  1               ;-- contour level spacing
  res@lbLabelStride         =  4
  res@lbBoxMinorExtentF     =  0.15            ;-- decrease the height of the labelbar
  res@tiMainString          = "NCL UG Example: filled contour map"     ;-- title string
  res@tiMainFontHeightF     =  0.02

;-- draw the contour map
  plot = gsn_csm_contour_map(wks, var, res)

end
