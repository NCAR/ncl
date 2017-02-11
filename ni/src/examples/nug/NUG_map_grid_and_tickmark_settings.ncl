;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_map_grid_and_tickmark_settings.ncl
;
;  KMF
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 

begin
;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","NUG_map_grid_and_tickmark_settings")

;-- set resources
  res                         =  True
;-- grid line settings
  res@mpGridAndLimbOn       =  True    ;-- draw grid lines on the plot
  res@mpGridLatSpacingF     =  20      ;-- grid line latitude spacing
  res@mpGridLonSpacingF     =  45      ;-- grid line longitude spacing
  res@mpGridLineColor       = "gray"   ;-- grid line color
  res@mpGridLineThicknessF  =  2       ;-- grid line thickness
  res@mpGridLineDashPattern =  2       ;-- grid line dash pattern 
                                       ;-- (2: dotted)
;-- latitude settings 
  res@gsnMajorLatSpacing    =  10     ;-- change major lat tickmark spacing
  res@gsnMinorLatSpacing    =  2.5    ;-- change major lat tickmark spacing

  res@tmYLLabelStride       =  3      ;-- write only every 3rd label
  res@tmYLLabelFontHeightF  =  0.016  ;-- change major lat tickmark spacing
  res@tmYLMajorLengthF      =  0.02   ;-- change the tickmark length
  res@tmYLMinorLengthF      =  0.01   ;-- change the tickmark length
  res@tmYLMajorLineColor    = "blue"  ;-- change major tickmarks color
  res@tmYLMinorLineColor    = "grey20" ;-- change major tickmarks color
  res@tmYLLabelFontColor    = "blue"  ;-- change label color

;-- longitude settings
  res@gsnMajorLonSpacing    =  10     ;-- change major lon tickmark spacing
  res@gsnMinorLonSpacing    =  2.5    ;-- change major lon tickmark spacing

  res@tmXBLabelStride       =  4      ;-- write only every 4th label
  res@tmXBLabelFontHeightF  =  0.014  ;-- change major lat tickmark spacing
  res@tmXBMajorLengthF      =  0.02   ;-- change the tickmark length
  res@tmXBMinorLengthF      =  0.01   ;-- change the tickmark length
  res@tmXBMajorLineColor    = "red"   ;-- change major tickmarks color
  res@tmXBMinorLineColor    = "grey20";-- change major tickmarks color     
  res@tmXBLabelFontColor    = "red"   ;-- change label color

;-- draw the map
  map = gsn_csm_map(wks, res)

end
