;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_masking.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---- read the data and define variable reference var

  diri  = "./"
  fil1  = "tas_rectilinear_grid_2D.nc"
  fil2  = "sftlf_mod1_rectilinear_grid_2D.nc"
  
  file1          = addfile(diri+fil1,"r")
  var            =  file1->tas(0,:,:)
  var&lat@units  = "degrees_north"
  var&lon@units  = "degrees_east"

;---- read the land /sea mask data
  mask1          =  addfile(diri+fil2,"r")
  lsm1           =  mask1->sftlf(:,:)
  lsm1&lat@units = "degrees_north"
  lsm1&lon@units = "degrees_east"

  land_only      =  var                                        ;-- copy attributes, too.
  land_only      =  where(lsm1 .ge. 50, var, var@_FillValue)   ;-- mask less than 50%

;---- define the workstation (plot output type and name)
  wks = gsn_open_wks("png","plot_masking")

;---- set resources
  res                       =  True
  res@gsnSpreadColors       =  True
  res@gsnMaximize           =  True
  res@lbLabelStride         =  2
  res@lbBoxMinorExtentF     =  0.15                                   ;-- decrease the height of the labelbar
  res@cnFillOn              =  True                                   ;-- turn on contour fill
  res@cnLinesOn             =  False                                  ;-- turn off contour lines
  res@cnLineLabelsOn        =  False                                  ;-- turn off line labels
  res@cnLevelSelectionMode  = "ManualLevels"                          ;-- set contour levels manually
  res@cnMinLevelValF        =  250.                                   ;-- minimum contour level
  res@cnMaxLevelValF        =  310.                                   ;-- maximum contour level
  res@cnLevelSpacingF       =  1                                      ;-- contour level spacing
  res@tiMainString          = "NCL UG Example:  masking"   ;-- title string
  res@tiMainFontHeightF     =  0.02

;---- draw the contour map
  plot = gsn_csm_contour_map(wks, land_only, res)

end
