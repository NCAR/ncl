;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_map_settings.ncl
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
  wks = gsn_open_wks("png","plot_map_settings")

;-- set resources
  res                       =  True
  res@gsnMaximize           =  True
  res@lbLabelStride         =  2
  res@lbBoxMinorExtentF     =  0.15            ;-- decrease the height of the labelbar
  res@cnFillOn              =  True            ;-- turn on contour fill
  res@cnLinesOn             =  False           ;-- turn off contour lines
  res@cnLineLabelsOn        =  False           ;-- turn off line labels
  res@cnLevelSelectionMode  = "ManualLevels"   ;-- set contour levels manually
  res@cnMinLevelValF        =  250.            ;-- minimum contour level
  res@cnMaxLevelValF        =  310.            ;-- maximum contour level
  res@cnLevelSpacingF       =  1               ;-- contour level spacing

  res@mpProjection          = "CylindricalEquidistant"  ;-- change projection
  res@mpLimitMode           = "Corners"
  res@mpLeftCornerLonF      = -20.0
  res@mpRightCornerLonF     =  60.0
  res@mpLeftCornerLatF      = -37.0
  res@mpRightCornerLatF     =  40.0

  res@mpDataBaseVersion     = "MediumRes"

  res@mpGridAndLimbOn       =  True            ;-- plot grid lines
  res@mpGridLineColor       = "grey30"         ;-- set grid line color

  res@tmXBMinorOn           =  False           ;-- no minor x-tickmarks
  res@tmYLMinorOn           =  False           ;-- no minor y-tickmarks

  res@tiMainString          = "NCL UG Example: map settings"   ;-- title string
  res@tiMainFontHeightF     =  0.02

;-- draw the contour map
  plot = gsn_csm_contour_map(wks, var, res)

end
