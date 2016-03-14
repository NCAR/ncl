;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_map_land_ocean_settings.ncl
;
;  KMF
;-----------------------------------------------------------------
begin
;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","plot_map_land_ocean_settings")

;-- set resources
  res                      =  True
  res@mpFillOn             =  True       ;-- use land fill (default: True)
  res@mpOutlineOn          =  True       ;-- outline land  (default: False)
  
  res@mpOceanFillColor     = "lightblue" ;-- color to fill ocean
  res@mpInlandWaterFillColor = "lightblue"   ;-- color to fill inland water
  res@mpLandFillColor      = "navajowhite1"  ;-- color to fill land

  res@mpGeophysicalLineColor = "blue"        ;-- outline color
  res@mpGeophysicalLineThicknessF = 1.2      ;-- thickness of continental
                                             ;-- outlines
  res@mpDataBaseVersion    = "MediumRes"     ;-- map resolution

;-- draw the map
  map = gsn_csm_map(wks, res)

end
